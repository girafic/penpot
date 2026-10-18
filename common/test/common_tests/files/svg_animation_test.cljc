;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.files.svg-animation-test
  (:require
   #?(:clj [app.common.animation.svg :as asvg])
   #?(:clj [clojure.xml :as xml])
   [app.common.files.changes-builder :as pcb]
   [app.common.files.svg-animation :as svga]
   [app.common.geom.rect :as grc]
   [app.common.math :as mth]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.types.animation :as cta]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.shape :as cts]
   [clojure.string :as cstr]
   [clojure.test :as t]))

(t/use-fixtures :each thi/test-fixture)

;; --- SVG documents as the parser gives them

(defn- el
  [tag attrs & content]
  {:tag tag :attrs attrs :content (vec content)})

(defn- svg
  [& content]
  (assoc (apply el :svg {:viewBox "0 0 100 100" :width "100" :height "100"} content)
         :name "Test.svg"))

(defn- import-svg
  [doc]
  (svga/svg->shapes doc {:x 0 :y 0}))

(defn- named
  [{:keys [shapes]} name]
  (some #(when (= name (:name %)) %) shapes))

(defn- frames
  "The `[time value]` of the keyframes of `property` of `shape`, of its
  first fill or stroke for a paint."
  [{:keys [timeline]} shape property]
  (mapv (juxt :time :value)
        (cta/property-keyframes timeline (:id shape) property
                                (when (contains? #{:fill-color :stroke-color} property) 0))))

(defn- close-frames?
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map (fn [[t0 v0] [t1 v1]]
                            (and (= t0 t1)
                                 (if (number? v0) (mth/close? v0 v1 0.01) (= v0 v1))))
                          expected actual))))

;; --- Tests

(t/deftest an-svg-without-animations-is-not-animated
  (t/is (false? (svga/animated? (svg (el :rect {:width "10" :height "10"})))))
  (t/is (true? (svga/animated? (svg (el :rect {:width "10" :height "10"}
                                        (el :animate {:attributeName "x" :to "5" :dur "1s"}))))))
  (t/is (true? (svga/animated? (svg (el :style {} "@keyframes a { to { opacity: 0 } }"))))))

(t/deftest a-turn-plays-on-a-board-around-its-centre
  (let [result (import-svg (svg (el :rect {:id "box" :x "40" :y "40" :width "20" :height "20" :fill "#ff0000"}
                                    (el :animateTransform {:attributeName "transform" :type "rotate"
                                                           :from "0 50 50" :to "360 50 50"
                                                           :dur "2s" :repeatCount "indefinite"}))))
        board  (first (:shapes result))
        node   (named result "box motion")
        box    (named result "box")]
    (t/is (= :frame (:type board)))
    (t/is (= [100 100] (map mth/round ((juxt :width :height) board))))
    (t/is (= :frame (:type node)))
    (t/is (= (:id node) (:parent-id box)))
    (t/is (= [40 40 20 20] (mapv mth/round ((juxt :x :y :width :height) (:selrect node))))
          "centred on the centre of the turn")
    (t/is (close-frames? [[0 0] [2000 360]] (frames result node :rotation)))
    (t/is (empty? (frames result node :x)) "it stays where it is")
    (t/is (= :loop (get-in result [:timeline :playback])))
    (t/is (= 2000 (get-in result [:timeline :duration])))
    (t/is (empty? (:skipped result)))))

(t/deftest css-keyframes-move-and-fade-back-and-forth
  (let [result (import-svg (svg (el :style {} (str "@keyframes slide {"
                                                   "  from { transform: translateX(0); opacity: 0 }"
                                                   "  to { transform: translateX(50px); opacity: 1 } }"
                                                   ".box { animation: slide 1s ease-in-out infinite alternate; }"))
                                (el :rect {:class "box" :id "box" :x "0" :y "40" :width "20" :height "20"})))
        node   (named result "box motion")
        [first-kf] (cta/property-keyframes (:timeline result) (:id node) :x)]
    (t/is (some? node))
    (t/is (close-frames? [[0 0] [1000 50] [2000 0]] (frames result node :x)))
    (t/is (= {:type :bezier :curve [0.42 0 0.58 1]} (:easing first-kf)) "ease-in-out")
    (t/is (close-frames? [[0 0] [1000 1] [2000 0]] (frames result node :opacity)))
    (t/is (= 2000 (get-in result [:timeline :duration])) "a cycle there and back")))

(t/deftest set-holds-and-colours-blend
  (let [result (import-svg (svg (el :rect {:id "box" :width "20" :height "20" :fill "#ff0000"}
                                    (el :set {:attributeName "opacity" :to "0" :begin "1s" :dur "1s"})
                                    (el :animate {:attributeName "fill" :from "#ff0000" :to "#0000ff"
                                                  :dur "2s" :fill "freeze"}))))
        node   (named result "box motion")
        box    (named result "box")]
    (t/is (close-frames? [[0 1] [1000 0] [2000 1]] (frames result node :opacity)))
    (t/is (every? #(= :step (:interpolation %))
                  (butlast (cta/property-keyframes (:timeline result) (:id node) :opacity))))
    (t/is (close-frames? [[0 "#ff0000"] [2000 "#0000ff"]] (frames result box :fill-color)))
    (t/is (= :once (get-in result [:timeline :playback])))))

(t/deftest a-dash-that-moves-draws-the-line
  (let [result (import-svg (svg (el :path {:id "line" :d "M0 50 L100 50" :fill "none" :stroke "#000000"
                                           :stroke-dasharray "100" :stroke-dashoffset "100"}
                                    (el :animate {:attributeName "stroke-dashoffset" :from "100" :to "0"
                                                  :dur "1s" :fill "freeze"}))))
        line   (named result "line")]
    (t/is (close-frames? [[0 0] [1000 1]] (frames result line :trim-end)))
    (t/is (= :solid (get-in line [:strokes 0 :stroke-style])) "no dashes left")
    (t/is (not-any? #(contains? (:svg-attrs line) %) [:strokeDasharray :strokeDashoffset]) "nor in its SVG")))

(t/deftest a-motion-follows-its-path
  (let [result (import-svg (svg (el :circle {:id "dot" :cx "0" :cy "0" :r "5"}
                                    (el :animateMotion {:path "M10 10 L90 10" :dur "1s" :fill "freeze"}))))
        node   (named result "dot motion")]
    (t/is (close-frames? [[0 5] [1000 85]] (frames result node :x))
          "from the start of its path to its end")
    (t/is (close-frames? [[0 5] [1000 5]] (frames result node :y)))))

(t/deftest the-last-rule-wins
  (let [result (import-svg (svg (el :style {} ".dot { fill: #ff0000; } .dot.two { fill: #00ff00; }")
                                (el :rect {:id "box" :class "dot two" :width "20" :height "20" :fill "#0000ff"})))]
    (t/is (= "#00ff00" (get-in (named result "box") [:fills 0 :fill-color]))
          "over the rules before it and the attribute")))

(t/deftest steps-hold-each-value
  (let [result (import-svg (svg (el :style {} (str "@keyframes blink { from { opacity: 0 } to { opacity: 1 } }"
                                                   "#box { animation: blink 1s steps(4) forwards; }"))
                                (el :rect {:id "box" :width "20" :height "20"})))
        node   (named result "box motion")]
    (t/is (close-frames? [[0 0] [250 0.25] [500 0.5] [750 0.75] [1000 1]] (frames result node :opacity)))))

(t/deftest what-a-board-cannot-play-is-counted
  (let [result (import-svg (svg (el :path {:id "shape" :d "M0 0 L10 10"}
                                    (el :animate {:attributeName "x" :to "20" :dur "1s" :begin "click"}))))]
    (t/is (= {:interactive 1} (:skipped result)))))

(defn- outline-frames
  "The `[time path]` of the outline keyframes of `shape`, whole numbers
  written whole."
  [result shape]
  (mapv (juxt :time #(cstr/replace (str (:value %)) #"\.0(?=\D|$)" ""))
        (cta/property-keyframes (:timeline result) (:id shape) :content)))

(t/deftest an-animated-path-morphs
  (let [result (import-svg (svg (el :path {:id "shape" :d "M10 10 L50 10 L50 50 Z" :fill "#ff0000"}
                                    (el :animate {:attributeName "d" :values "M10 10 L50 10 L50 50 Z;M10 10 L90 10 L50 90 Z"
                                                  :dur "1s" :fill "freeze"}))))
        shape  (named result "shape")
        frames (outline-frames result shape)]
    (t/is (empty? (:skipped result)))
    (t/is (= [0 1000] (mapv first frames)))
    (t/is (= "M0,0L40,0L40,40Z" (second (first frames))) "from its outline, in its own frame")
    (t/is (= "M0,0L80,0L40,80Z" (second (peek frames))) "to the other one")
    (t/is (= (ctao/outline-box shape) (get-in (:timeline result) [:tracks (:id shape) :outline-box])))))

(def ^:private tri "M10 10 L50 10 L50 50 Z")
(def ^:private sq "M10 10 L90 10 L90 90 L10 90 Z")
(def ^:private hexa "M10 10 L50 10 L70 30 L50 50 L10 50 L0 30 Z")

(defn- held-outlines
  "`[time interpolation path]` of the outline keyframes of the element
  `shape` of the SVG `result`, whole numbers written whole."
  [result]
  (mapv (juxt :time :interpolation #(cstr/replace (str (:value %)) #"\.0(?=\D|$)" ""))
        (cta/property-keyframes (:timeline result) (:id (named result "shape")) :content)))

(t/deftest a-path-of-other-outlines-holds-each
  ;; a browser plays outlines of other structures as jumps, at the middle
  ;; of each interval
  (let [result (import-svg (svg (el :path {:id "shape" :d tri :fill "#ff0000"}
                                    (el :animate {:attributeName "d" :values (str tri ";" sq)
                                                  :dur "1s" :fill "freeze"}))))
        frames (held-outlines result)]
    (t/is (empty? (:skipped result)) "nothing left out")
    (t/is (= [0 500] (mapv first (take 2 frames))) "the square from 500 ms")
    (t/is (= :step (second (first frames))) "the triangle held until it")
    (t/is (= "M0,0L40,0L40,40Z" (nth (first frames) 2)))
    (t/is (= "M0,0L80,0L80,80L0,80Z" (nth (second frames) 2)))))

(t/deftest outlines-of-other-structures-loop-as-browsers-play-them
  (doseq [[label svg-data]
          [["SMIL, repeating"
            (svg (el :path {:id "shape" :d tri :fill "#ff0000"}
                     (el :animate {:attributeName "d" :values (str tri ";" sq) :dur "1s" :repeatCount "indefinite"})))]
           ["CSS, infinite"
            (svg (el :style {} (str "@keyframes m { from { d: path('" tri "') } to { d: path('" sq "') } }"
                                    " #shape { animation: m 1s linear infinite; }"))
                 (el :path {:id "shape" :d tri :fill "#ff0000"}))]]]
    (let [result (import-svg svg-data)
          frames (held-outlines result)]
      (t/is (empty? (:skipped result)) label)
      (t/is (some #(and (= 500 (first %)) (= "M0,0L80,0L80,80L0,80Z" (nth % 2))) frames)
            (str label ": the square from 500 ms"))))
  (t/testing "an eased CSS one where its easing reaches half way"
    (doseq [[tf from to] [["" 280 305] ["ease-in" 640 680]]]
      (let [result (import-svg (svg (el :style {} (str "@keyframes m { from { d: path('" tri "') } to { d: path('" sq "') } }"
                                                       " #shape { animation: m 1s " tf " infinite; }"))
                                    (el :path {:id "shape" :d tri :fill "#ff0000"})))
            switch (some #(when (= "M0,0L80,0L80,80L0,80Z" (nth % 2)) (first %)) (held-outlines result))]
        (t/is (and (some? switch) (<= from switch to)) (str "timing " (pr-str tf) ": the square from " switch " ms")))))
  (t/testing "three: the last from the middle of its interval"
    (let [result (import-svg (svg (el :path {:id "shape" :d tri :fill "#ff0000"}
                                      (el :animate {:attributeName "d" :values (str tri ";" sq ";" hexa)
                                                    :dur "1s" :repeatCount "indefinite"}))))]
      (t/is (some #(and (= 750 (first %)) (cstr/starts-with? (nth % 2) "M0,0L40,0L60,20")) (held-outlines result))
            "the hexagon from 750 ms")))
  (t/testing "an explicit discrete one as it says"
    (let [result (import-svg (svg (el :path {:id "shape" :d tri :fill "#ff0000"}
                                      (el :animate {:attributeName "d" :values (str tri ";" sq) :calcMode "discrete"
                                                    :dur "1s" :repeatCount "indefinite"}))))]
      (t/is (= [0 500] (mapv first (take 2 (held-outlines result))))))))

(defn- spline-half
  "The progress where the cubic bezier easing `[x1 y1 x2 y2]` reaches 0.5."
  [[x1 y1 x2 y2]]
  (let [b (fn [p1 p2 t] (+ (* 3 p1 t (- 1 t) (- 1 t)) (* 3 p2 t t (- 1 t)) (* t t t)))]
    (loop [lo 0.0 hi 1.0 n 0]
      (let [m (/ (+ lo hi) 2)]
        (cond
          (= n 50) (b x1 x2 m)
          (< (b y1 y2 m) 0.5) (recur m hi (inc n))
          :else (recur lo m (inc n)))))))

(defn- switch-time
  "When the outline of the element `shape` of `result` becomes `outline`."
  [result outline]
  (some #(when (= outline (nth % 2)) (first %)) (held-outlines result)))

(t/deftest outlines-switch-where-splines-and-steps-say
  (t/testing "a SMIL spline where it reaches half way"
    (let [spline [0.42 0 1 1]
          result (import-svg (svg (el :path {:id "shape" :d tri :fill "#ff0000"}
                                      (el :animate {:attributeName "d" :values (str tri ";" sq)
                                                    :calcMode "spline" :keyTimes "0;1" :keySplines "0.42 0 1 1"
                                                    :dur "1s" :fill "freeze"}))))
          switch (switch-time result "M0,0L80,0L80,80L0,80Z")]
      (t/is (some? switch))
      (t/is (<= (abs (- switch (* 1000 (spline-half spline)))) 1) (str "the square from " switch " ms"))
      (t/is (= [:step :step nil] (mapv second (held-outlines result)))
            "a clean jump there, no morph into it nor keyframes after it")))
  (t/testing "CSS steps by keyframe"
    (let [anim   (fn [first-tf]
                   (import-svg (svg (el :style {} (str "@keyframes m {"
                                                       " 0% { d: path('" tri "'); animation-timing-function: " first-tf "; }"
                                                       " 50% { d: path('" sq "'); animation-timing-function: ease-in; }"
                                                       " 100% { d: path('" hexa "'); } }"
                                                       " #shape { animation: m 1s forwards; }"))
                                    (el :path {:id "shape" :d tri :fill "#ff0000"}))))
          ends   (anim "step-end")
          starts (anim "step-start")
          hexa?  #(cstr/starts-with? (nth % 2) "M0,0L40,0L60,20")]
      (t/is (= 500 (switch-time ends "M0,0L80,0L80,80L0,80Z")) "step-end: the square at the end of its interval")
      (let [switch (some #(when (hexa? %) (first %)) (held-outlines ends))]
        (t/is (and (some? switch) (<= (abs (- switch (+ 500 (* 500 (spline-half [0.42 0 1 1]))))) 1))
              (str "ease-in after it: the hexagon from " switch " ms")))
      (t/is (= 0 (switch-time starts "M0,0L80,0L80,80L0,80Z")) "step-start: the square from the start of its interval"))))

(t/deftest a-path-past-what-a-row-holds-is-counted
  (let [big    (fn [k] (str "M0 0" (apply str (map #(str " L" % " " (mod (+ % k) 7)) (range 1 5000)))))
        result (import-svg (svg (el :path {:id "shape" :d (big 0) :fill "#ff0000"}
                                    (el :animate {:attributeName "d" :values (str (big 0) ";" (big 1))
                                                  :dur "1s" :fill "freeze"}))))]
    (t/is (= {:path-morph 1} (:skipped result)))
    (t/is (empty? (cta/property-keyframes (:timeline result) (:id (named result "shape")) :content)))))

(t/deftest a-css-path-and-points-morph
  (let [css    (import-svg (svg (el :style {} (str "@keyframes grow { to { d: path('M10 10 L90 10 L50 90 Z') } }"
                                                   "#shape { animation: grow 1s forwards; }"))
                                (el :path {:id "shape" :d "M10 10 L50 10 L50 50 Z" :fill "#ff0000"})))
        points (import-svg (svg (el :polygon {:id "shape" :points "10,10 50,10 50,50" :fill "#ff0000"}
                                    (el :animate {:attributeName "points" :to "10,10 90,10 50,90" :dur "1s" :fill "freeze"}))))]
    (t/is (= 2 (count (outline-frames css (named css "shape")))) "d: path() of CSS")
    (t/is (= 2 (count (outline-frames points (named points "shape")))) "points of a polygon")))

;; --- What an animation does not say starts from the value at rest

(defn- css-anim
  "An SVG whose element `#box` plays the CSS `keyframes` (`a`) for 1 s,
  linear, kept at its end."
  [keyframes & content]
  (apply svg (el :style {} (str "@keyframes a { " keyframes " }"
                                " #box { animation: a 1s linear forwards; }"))
         content))

(t/deftest one-sided-keyframes-start-from-the-value-at-rest
  (doseq [[keyframes expected] [["to { opacity: 0 }" [[0 1] [1000 0]]]
                                ["from { opacity: 0 }" [[0 0] [1000 1]]]
                                ["50% { opacity: .5 }" [[0 1] [500 0.5] [1000 1]]]]]
    (let [result (import-svg (css-anim keyframes (el :rect {:id "box" :width "20" :height "20"})))]
      (t/is (close-frames? expected (frames result (named result "box motion") :opacity)) keyframes)
      (t/is (empty? (:skipped result)) keyframes))))

(t/deftest a-line-draws-itself-from-its-dash-attributes
  (let [result (import-svg (css-anim "to { stroke-dashoffset: 0 }"
                                     (el :path {:id "box" :d "M0 50 L100 50" :fill "none" :stroke "#000000"
                                                :stroke-dasharray "100" :stroke-dashoffset "100"})))]
    (t/is (close-frames? [[0 0] [1000 1]] (frames result (named result "box") :trim-end)))))

(t/deftest smil-to-and-by-start-from-the-value-at-rest
  (let [result (import-svg (svg (el :rect {:id "fade" :width "10" :height "10"}
                                    (el :animate {:attributeName "opacity" :to "0" :dur "1s" :fill "freeze"}))
                                (el :rect {:id "turn" :x "40" :y "40" :width "20" :height "20"}
                                    (el :animateTransform {:attributeName "transform" :type "rotate" :by "360"
                                                           :dur "1s" :fill "freeze"}))
                                ;; a by-animation is `values="0;by"` added: from scale(0)
                                (el :rect {:id "grow" :x "20" :width "10" :height "10"}
                                    (el :animateTransform {:attributeName "transform" :type "scale" :by "1"
                                                           :dur "1s" :fill "freeze"}))))]
    (t/is (close-frames? [[0 1] [1000 0]] (frames result (named result "fade motion") :opacity)))
    (t/is (close-frames? [[0 0] [1000 360]] (frames result (named result "turn motion") :rotation)))
    (t/is (close-frames? [[0 0] [1000 1]] (frames result (named result "grow motion") :scale-x)))
    (t/is (empty? (:skipped result)))))

(t/deftest a-colour-blends-from-the-one-at-rest
  (doseq [[label dot] [["its own" (el :circle {:id "box" :cx "50" :cy "50" :r "10" :fill "#0000ff"})]
                       ["of its group" (el :g {:fill "#0000ff"} (el :circle {:id "box" :cx "50" :cy "50" :r "10"}))]]]
    (let [result (import-svg (css-anim "50% { fill: #ff0000 }" dot))]
      (t/is (close-frames? [[0 "#0000ff"] [500 "#ff0000"] [1000 "#0000ff"]]
                           (frames result (named result "box") :fill-color))
            label))))

(t/deftest a-css-transform-starts-from-the-transform-attribute
  ;; in SVG syntax, its centre kept; to other functions through their
  ;; matrices, as CSS interpolates them, with no jump
  (doseq [[label transform turn] [["a turn" "rotate(30 50 50)" 60]
                                  ["a move" "translate(50 50)" 90]]]
    (let [result (import-svg (css-anim "to { transform: rotate(90deg) }"
                                       (el :rect {:id "box" :x "40" :y "40" :width "20" :height "20"
                                                  :transform transform})))
          node   (named result "box motion")
          xs     (cta/property-keyframes (:timeline result) (:id node) :x)]
      (t/is (close-frames? [[0 0] [1000 turn]] (frames result node :rotation)) label)
      (t/is (mth/close? (:x node) (:value (first xs)) 0.5) (str label ": where it is at rest"))
      (t/is (not-any? #(= :step (:interpolation %)) xs) label))))

(t/deftest a-layer-shows-when-its-display-turns-on
  (let [result (import-svg (svg (el :g {:id "box" :display "none"}
                                    (el :rect {:id "kid" :width "10" :height "10"})
                                    (el :set {:attributeName "display" :to "inline"
                                              :begin "1s" :dur "1s" :fill "freeze"}))))
        box    (named result "box")]
    (t/is (not (:hidden box)))
    (t/is (nil? (get-in box [:svg-attrs :display])) "nor hidden by its SVG")
    (t/is (close-frames? [[0 0] [1000 1] [2000 1]] (frames result (named result "box motion") :opacity)))))

(t/deftest a-layer-shows-when-it-turns-visible
  (let [result (import-svg (svg (el :g {:id "box" :visibility "hidden"}
                                    (el :rect {:id "kid" :width "10" :height "10"})
                                    (el :set {:attributeName "visibility" :to "visible"
                                              :begin "1s" :dur "1s" :fill "freeze"}))))]
    (t/is (nil? (get-in (named result "box") [:svg-attrs :visibility])))
    (t/is (nil? (get-in (named result "kid") [:svg-attrs :visibility])) "nor what it holds")
    (t/is (close-frames? [[0 0] [1000 1] [2000 1]] (frames result (named result "box motion") :opacity)))))

(t/deftest a-pop-in-from-nothing-is-built-whole
  ;; at rest it is scaled to nothing: built at its full size, its board
  ;; plays the scale
  (doseq [[label css centre] [["around its corner" "" [0 0]]
                              ["around its centre" "transform-box: fill-box; transform-origin: center;" [50 50]]]]
    (let [result (import-svg (svg (el :style {} (str "@keyframes pop { to { transform: scale(1) } }"
                                                     "#box { transform: scale(0); " css
                                                     " animation: pop 1s linear forwards; }"))
                                  (el :rect {:id "box" :x "40" :y "40" :width "20" :height "20"})))
          node   (named result "box motion")]
      (t/is (= [20 20] (mapv mth/round ((juxt :width :height) (:selrect (named result "box"))))) label)
      (t/is (close-frames? [[0 0] [1000 1]] (frames result node :scale-x)) label)
      (t/is (= centre (mapv mth/round [(+ (:x node) (/ (:width node) 2)) (+ (:y node) (/ (:height node) 2))]))
            (str label ": the board centred on where it grows from")))))

;; --- How the animations play, as Chrome plays them (values sampled in
;; Chrome at the times asked)

(defn- value-at
  "The value of `property` of the shape named `name` at `t` ms, as the
  timeline of the import plays it."
  [{:keys [timeline] :as result} name property t]
  (cta/property-value-at (cta/property-keyframes timeline (:id (named result name)) property) t))

(defn- plays?
  "Whether `property` of the shape named `name` has the values `expected`
  (`[t value]`) as Chrome plays them."
  [result name property expected]
  (every? (fn [[t v]] (mth/close? v (value-at result name property t) 0.01)) expected))

(defn- moved-x
  "How far the board of the element `id` has moved in x at `t` ms."
  [result id t]
  (- (value-at result (str id " motion") :x t) (:x (named result (str id " motion")))))

(defn- one
  "An SVG of the element `#x` with the SMIL animation of `attrs` (`tag`)."
  [tag attrs & [elem-attrs]]
  (svg (el :rect (merge {:id "x" :width "10" :height "10"} elem-attrs) (el tag attrs))))

(t/deftest an-animation-begins-at-the-end-of-another
  (let [result (import-svg (svg (el :rect {:id "a-el" :width "10" :height "10"}
                                    (el :animate {:id "a" :attributeName "opacity" :from "1" :to "0"
                                                  :dur "1s" :fill "freeze"}))
                                (el :rect {:id "x" :x "20" :width "10" :height "10"}
                                    (el :animate {:attributeName "opacity" :from "1" :to "0"
                                                  :begin "a.end+0.5s" :dur "1s" :fill "freeze"}))))]
    (t/is (plays? result "x motion" :opacity [[1400 1] [2000 0.5] [2500 0]]))
    (t/is (= 2500 (get-in result [:timeline :duration])))))

(t/deftest end-and-repeat-dur-stop-a-repeat
  (let [ended  (import-svg (one :animate {:attributeName "opacity" :values "1;0" :dur "1s"
                                          :repeatCount "indefinite" :end "2.5s"}))
        frozen (import-svg (one :animate {:attributeName "opacity" :values "1;0" :dur "1s"
                                          :repeatDur "1.5s" :fill "freeze"}))]
    (t/is (plays? ended "x motion" :opacity [[500 0.5] [1500 0.5] [2400 0.6] [2600 1]]))
    (t/is (= 2500 (get-in ended [:timeline :duration])))
    (t/is (plays? frozen "x motion" :opacity [[1200 0.8] [1500 0.5] [1800 0.5]]))))

(t/deftest key-splines-ease-a-segment
  (t/is (plays? (import-svg (one :animate {:attributeName "opacity" :values "1;0" :keyTimes "0;1"
                                           :calcMode "spline" :keySplines "0.42 0 0.58 1"
                                           :dur "1s" :fill "freeze"}))
                "x motion" :opacity [[250 0.8708] [500 0.5] [750 0.1292]])))

(t/deftest discrete-values-hold
  (t/is (plays? (import-svg (one :animate {:attributeName "opacity" :values "1;0.5;0" :calcMode "discrete"
                                           :dur "1.5s" :fill "freeze"}))
                "x motion" :opacity [[250 1] [750 0.5] [1250 0] [1600 0]])))

(t/deftest a-motion-goes-at-an-even-pace
  ;; paced: as far along its path as the time is along the cycle
  (let [result (import-svg (one :animateMotion {:path "M0 0 L10 0 L100 0" :dur "1s" :fill "freeze"}))]
    (doseq [[t x] [[250 25] [500 50] [750 75]]]
      (t/is (mth/close? x (moved-x result "x" t) 0.5) (str t " ms")))))

(t/deftest a-negative-delay-starts-half-way
  (let [result (import-svg (svg (el :style {} (str "@keyframes f { from { opacity: 0 } to { opacity: 1 } }"
                                                   "#x { animation: f 2s linear -1s both; }"))
                                (el :rect {:id "x" :width "10" :height "10"})))]
    (t/is (plays? result "x motion" :opacity [[0 0.5] [500 0.75] [1000 1]]))))

(t/deftest an-additive-animation-goes-on-top-of-the-transform
  (let [result (import-svg (one :animateTransform {:attributeName "transform" :type "translate"
                                                   :from "0 0" :to "50 0" :additive "sum"
                                                   :dur "1s" :fill "freeze"}
                                {:transform "translate(10 0)"}))]
    ;; Chrome: 10, 35 and 60 with the translate it has at rest
    (doseq [[t x] [[0 0] [500 25] [1000 50]]]
      (t/is (mth/close? x (moved-x result "x" t) 0.5) (str t " ms")))))

(t/deftest a-turn-goes-around-its-transform-origin
  (doseq [[label css centre] [["of the view box" "transform-origin: right bottom;" [100 100]]
                              ["of the box of the element" "transform-box: fill-box; transform-origin: left top;" [40 40]]]]
    (let [result (import-svg (svg (el :style {} (str "@keyframes spin { to { transform: rotate(90deg) } }"
                                                     "#x { " css " animation: spin 1s linear forwards; }"))
                                  (el :rect {:id "x" :x "40" :y "40" :width "20" :height "20"})))
          node   (named result "x motion")]
      (t/is (= centre (mapv mth/round [(+ (:x node) (/ (:width node) 2)) (+ (:y node) (/ (:height node) 2))])) label)
      (t/is (plays? result "x motion" :rotation [[500 45]]) label))))

(t/deftest loops-that-do-not-fit-together-play-the-longest
  ;; 7, 11 and 13 s meet again only after 1001 s
  (let [result (import-svg (apply svg (for [[i dur] (map-indexed vector ["7s" "11s" "13s"])]
                                        (el :rect {:id (str "r" i) :x (str (* 20 i)) :width "10" :height "10"}
                                            (el :animate {:attributeName "opacity" :values "1;0;1"
                                                          :dur dur :repeatCount "indefinite"})))))]
    (t/is (= {:loop-length 1} (:skipped result)))
    (t/is (= 13000 (get-in result [:timeline :duration])))))

;; --- Static CSS in an animated SVG

(t/deftest a-static-css-turn-goes-around-its-origin
  ;; the box of `transform-origin` is the view box from its origin, not
  ;; where it starts, or the box of the element: where Chrome draws it
  (doseq [[label view-box css centre]
          [["the view box" "0 0 100 100" "transform-origin: center;" [50 7.57]]
           ["a view box elsewhere" "-50 -50 100 100" "transform-origin: center;" [100 57.57]]
           ["the box of the element" "0 0 100 100" "transform-box: fill-box; transform-origin: center;" [20 20]]]]
    (let [result (import-svg (assoc (el :svg {:viewBox view-box :width "100" :height "100"}
                                        (el :style {} (str "#b { " css " transform: rotate(45deg); }"
                                                           "@keyframes a { to { opacity: 0 } } #o { animation: a 1s; }"))
                                        (el :rect {:id "b" :x "10" :y "10" :width "20" :height "20"})
                                        (el :rect {:id "o" :width "5" :height "5"}))
                                    :name "Test.svg"))
          b      (named result "b")
          c      (grc/rect->center (:selrect b))]
      (t/is (every? true? (map #(mth/close? %1 %2 0.05) centre [(:x c) (:y c)])) label)
      (t/is (mth/close? 28.28 (:width (grc/points->rect (:points b))) 0.05) (str label ": turned")))))

(t/deftest a-pop-in-around-its-centre-waits-at-nothing
  (let [result (import-svg (svg (el :style {} (str "@keyframes pop { to { transform: scale(1) } }"
                                                   "#box { transform-box: fill-box; transform-origin: center;"
                                                   " transform: scale(0); animation: pop 1s linear 0.5s forwards; }"))
                                (el :rect {:id "box" :x "40" :y "40" :width "20" :height "20"})))]
    (t/is (plays? result "box motion" :scale-x [[0 0] [250 0] [1000 0.5] [1500 1]]))))

(t/deftest an-unknown-blend-mode-is-left-out
  (let [result (import-svg (svg (el :style {} (str ".glow { mix-blend-mode: plus-lighter; }"
                                                   "@keyframes a { to { opacity: 0 } } #box { animation: a 1s; }"))
                                (el :rect {:id "box" :class "glow" :width "10" :height "10"})))]
    (t/is (some? (named result "box")))
    (t/is (= {:attribute 1} (:skipped result)))))

;; --- What an SVG asks for is bounded: it comes from anyone

(defn- keyframe-count
  [{:keys [timeline]}]
  (count (mapcat :keyframes (vals (:tracks timeline)))))

(t/deftest a-zero-duration-holds-the-first-value
  ;; SMIL: a `dur` of 0 or less is no duration, as if indefinite
  (let [result (import-svg (svg (el :rect {:id "loop" :width "10" :height "10"}
                                    (el :animate {:attributeName "opacity" :values "0;1"
                                                  :dur "2s" :repeatCount "indefinite"}))
                                (el :rect {:id "box" :x "20" :width "10" :height "10"}
                                    (el :animate {:attributeName "opacity" :values "0;1" :dur "0s"
                                                  :begin "1s" :repeatCount "indefinite"}))))
        node   (named result "box motion")]
    (t/is (= 2000 (get-in result [:timeline :duration])))
    (t/is (close-frames? [[0 1] [1000 0] [2000 0]] (frames result node :opacity)))
    (t/is (empty? (:skipped result)))))

(t/deftest too-short-animations-are-left-out
  (doseq [[label doc]
          [["SMIL" (svg (el :rect {:id "box" :width "10" :height "10"}
                            (el :animate {:attributeName "opacity" :values "0;1"
                                          :dur "1e-9s" :repeatCount "indefinite"})))]
           ["CSS" (svg (el :style {} (str "@keyframes spin { to { transform: rotate(360deg) } }"
                                          "#box { animation: spin 0.000001s linear infinite; }"))
                       (el :rect {:id "box" :width "10" :height "10"}))]]]
    (let [result (import-svg doc)]
      (t/is (= {:timing 1} (:skipped result)) label)
      (t/is (nil? (named result "box motion")) label))))

(t/deftest a-loop-longer-than-ten-minutes-is-cut
  (let [result   (import-svg (svg (el :rect {:id "box" :width "10" :height "10"}
                                      (el :animate {:attributeName "opacity" :values "0;1"
                                                    :dur "1e9s" :repeatCount "indefinite"}))))
        duration (get-in result [:timeline :duration])]
    (t/is (< 0 duration 600001))
    (t/is (= {:loop-length 1} (:skipped result)))))

(t/deftest a-billion-steps-play-as-a-line
  (let [result (import-svg (svg (el :style {} (str "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }"
                                                   "#box { animation: fade 1s steps(1000000000) forwards; }"))
                                (el :rect {:id "box" :width "20" :height "20"})))
        node   (named result "box motion")]
    (t/is (close-frames? [[0 0] [1000 1]] (frames result node :opacity)))))

(t/deftest a-fast-loop-on-a-long-board-is-left-out
  ;; a 10 ms spinner turns 60 000 times in a board of 10 minutes
  (let [result (import-svg (svg (el :rect {:id "spinner" :width "10" :height "10"}
                                    (el :animateTransform {:attributeName "transform" :type "rotate"
                                                           :from "0 5 5" :to "360 5 5"
                                                           :dur "10ms" :repeatCount "indefinite"}))
                                (el :rect {:id "slow" :x "20" :width "10" :height "10"}
                                    (el :animate {:attributeName "opacity" :values "0;1"
                                                  :dur "600s" :repeatCount "indefinite"}))))]
    (t/is (= {:timing 1} (:skipped result)))
    (t/is (nil? (named result "spinner motion")))
    (t/is (some? (named result "slow motion")))
    (t/is (< (keyframe-count result) 5000))))

(t/deftest raw-elements-are-no-frames
  ;; an element the import keeps as SVG, with elements in it
  (let [file    (thf/sample-file :file1)
        page    (thf/current-page file)
        result  (import-svg (svg (el :pattern {:id "dots" :width "10" :height "10"}
                                     (el :g {} (el :rect {:width "5" :height "5"})))
                                 (el :rect {:id "box" :width "20" :height "20"}
                                     (el :animate {:attributeName "opacity" :to "0" :dur "1s"}))))
        board   (first (:shapes result))
        changes (as-> (-> (pcb/empty-changes nil (:id page))
                          (pcb/with-page page)
                          (pcb/with-objects (:objects page))) changes
                  (reduce pcb/add-object changes (:shapes result))
                  (pcb/change-timeline changes (:id board) (:timeline result)))]
    (t/is (some? (thf/apply-changes file changes)) "a valid file")))

(t/deftest nested-defs-keep-their-definitions
  (let [result (import-svg (svg (el :defs {}
                                    (el :mask {:id "m"}
                                        (el :g {}
                                            (el :defs {} (el :linearGradient {:id "g" :x2 "1"}
                                                             (el :stop {:offset "0" :stop-color "#ffffff"})
                                                             (el :stop {:offset "1" :stop-color "#000000"})))
                                            (el :rect {:id "band" :width "100" :height "100" :fill "url(#g)"}))))
                                (el :rect {:id "box" :width "100" :height "100" :fill "#ff0000" :mask "url(#m)"}
                                    (el :animate {:attributeName "opacity" :to "0" :dur "1s"}))))]
    (t/is (= :linear (get-in (named result "band") [:fills 0 :fill-color-gradient :type]))
          "the gradient of the mask, in defs in defs")))

(t/deftest the-board-goes-into-a-file
  (let [file    (thf/sample-file :file1)
        page    (thf/current-page file)
        result  (import-svg (svg (el :g {:id "group"}
                                     (el :rect {:id "box" :x "40" :y "40" :width "20" :height "20" :fill "#ff0000"}
                                         (el :animateTransform {:attributeName "transform" :type "rotate"
                                                                :from "0 50 50" :to "360 50 50"
                                                                :dur "2s" :repeatCount "indefinite"})))))
        board   (first (:shapes result))
        changes (as-> (-> (pcb/empty-changes nil (:id page))
                          (pcb/with-page page)
                          (pcb/with-objects (:objects page))) changes
                  (reduce pcb/add-object changes (:shapes result))
                  (pcb/change-timeline changes (:id board) (:timeline result)))
        page'   (thf/current-page (thf/apply-changes file changes))]
    (t/is (cta/valid-timeline? (get-in page' [:timelines (:id board)])))
    (t/is (every? cts/valid-shape? (vals (:objects page'))))
    (t/is (= (:id (named result "box motion"))
             (get-in page' [:objects (:id (named result "box")) :frame-id]))
          "the box is in its board, through the group")))

#?(:clj
   (t/deftest an-svg-export-comes-back
     ;; as the CSS of the export plays it: its curves sampled
     (let [board  (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300 :name "Board"})
           shape  (cts/setup-shape {:type :rect :x 100 :y 100 :width 200 :height 100 :name "Box"
                                    :fills [{:fill-color "#336699" :fill-opacity 1}]})
           [bid sid] (map :id [board shape])
           objects {bid (assoc board :shapes [sid]) sid (assoc shape :parent-id bid :frame-id bid)}
           tl      (-> (cta/make-timeline {:board-id bid :duration 1000 :playback :loop})
                       (cta/add-keyframe sid {:time 0 :property :x :value 100 :easing :ease-in-out})
                       (cta/add-keyframe sid {:time 1000 :property :x :value 300})
                       (cta/add-keyframe sid {:time 0 :property :rotation :value 0})
                       (cta/add-keyframe sid {:time 1000 :property :rotation :value 90}))
           images  {bid {:markup "<rect width=\"400\" height=\"300\" fill=\"#ffffff\"/>"}
                    sid {:markup "<rect x=\"100\" y=\"100\" width=\"200\" height=\"100\" fill=\"#336699\"/>"}}
           text    (asvg/timeline->svg tl objects images)
           doc     (-> (xml/parse (java.io.ByteArrayInputStream. (.getBytes ^String text "UTF-8")))
                       (assoc :name "Board.svg"))
           result  (svga/svg->shapes doc {:x 0 :y 0})
           node    (some #(when (and (= :frame (:type %)) (seq (frames result % :rotation))) %) (:shapes result))
           xs      (frames result node :x)]
       (t/is (some? node))
       (t/is (= [100 100 200 100] (mapv mth/round ((juxt :x :y :width :height) (:selrect node))))
             "centred on the box, the origin of its turn")
       (t/is (close-frames? [[0 0] [1000 90]] (frames result node :rotation)))
       (t/is (mth/close? 100 (second (first xs)) 0.5))
       (t/is (mth/close? 300 (second (peek xs)) 0.5))
       (t/is (mth/close? 200 (let [[[t0 v0] [t1 v1]] (->> (partition 2 1 xs) (filter (fn [[[a] [b]]] (<= a 500 b))) (first))]
                               (+ v0 (* (- v1 v0) (/ (- 500 t0) (- t1 t0)))))
                         1)
             "halfway at the half")
       (t/is (= 1000 (get-in result [:timeline :duration]))))))
