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
   [app.common.math :as mth]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.types.animation :as cta]
   [app.common.types.shape :as cts]
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

(defn- import
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
  (let [result (import (svg (el :rect {:id "box" :x "40" :y "40" :width "20" :height "20" :fill "#ff0000"}
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
  (let [result (import (svg (el :style {} (str "@keyframes slide {"
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
  (let [result (import (svg (el :rect {:id "box" :width "20" :height "20" :fill "#ff0000"}
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
  (let [result (import (svg (el :path {:id "line" :d "M0 50 L100 50" :fill "none" :stroke "#000000"
                                       :stroke-dasharray "100" :stroke-dashoffset "100"}
                                (el :animate {:attributeName "stroke-dashoffset" :from "100" :to "0"
                                              :dur "1s" :fill "freeze"}))))
        line   (named result "line")]
    (t/is (close-frames? [[0 0] [1000 1]] (frames result line :trim-end)))
    (t/is (= :solid (get-in line [:strokes 0 :stroke-style])) "no dashes left")))

(t/deftest a-motion-follows-its-path
  (let [result (import (svg (el :circle {:id "dot" :cx "0" :cy "0" :r "5"}
                                (el :animateMotion {:path "M10 10 L90 10" :dur "1s" :fill "freeze"}))))
        node   (named result "dot motion")]
    (t/is (close-frames? [[0 5] [1000 85]] (frames result node :x))
          "from the start of its path to its end")
    (t/is (close-frames? [[0 5] [1000 5]] (frames result node :y)))))

(t/deftest the-last-rule-wins
  (let [result (import (svg (el :style {} ".dot { fill: #ff0000; } .dot.two { fill: #00ff00; }")
                            (el :rect {:id "box" :class "dot two" :width "20" :height "20" :fill "#0000ff"})))]
    (t/is (= "#00ff00" (get-in (named result "box") [:fills 0 :fill-color]))
          "over the rules before it and the attribute")))

(t/deftest steps-hold-each-value
  (let [result (import (svg (el :style {} (str "@keyframes blink { from { opacity: 0 } to { opacity: 1 } }"
                                               "#box { animation: blink 1s steps(4) forwards; }"))
                            (el :rect {:id "box" :width "20" :height "20"})))
        node   (named result "box motion")]
    (t/is (close-frames? [[0 0] [250 0.25] [500 0.5] [750 0.75] [1000 1]] (frames result node :opacity)))))

(t/deftest what-a-board-cannot-play-is-counted
  (let [result (import (svg (el :path {:id "shape" :d "M0 0 L10 10"}
                                (el :animate {:attributeName "d" :to "M0 0 L20 20" :dur "1s"})
                                (el :animate {:attributeName "x" :to "20" :dur "1s" :begin "click"}))))]
    (t/is (= {:path-morph 1 :interactive 1} (:skipped result)))))

(t/deftest raw-elements-are-no-frames
  ;; an element the import keeps as SVG, with elements in it
  (let [file    (thf/sample-file :file1)
        page    (thf/current-page file)
        result  (import (svg (el :pattern {:id "dots" :width "10" :height "10"}
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
  (let [result (import (svg (el :defs {}
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
        result  (import (svg (el :g {:id "group"}
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
