;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.svg-vectors-test
  (:require
   ["jsdom" :refer [JSDOM]]
   [app.common.animation.export :as aexp]
   [app.common.animation.svg :as asvg]
   [app.common.types.animation :as cta]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.exports.svg-vectors :as svgv]
   [app.main.ui.shapes.text.fontfaces :as ff]
   [app.util.http :as http]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]
   [frontend-tests.helpers.mock :as mock]))

(defn- board-with
  "A board holding `shapes`."
  [& shapes]
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100
                                :fills [{:fill-color "#112233" :fill-opacity 1}]})
        bid   (:id board)]
    {:bid bid
     :objects (into {bid (assoc board :shapes (mapv :id shapes))}
                    (map (fn [shape] [(:id shape) (assoc shape :parent-id bid :frame-id bid)]))
                    shapes)}))

(defn- track-of
  "A track of keyframes of `properties`."
  [& properties]
  {:keyframes (mapv #(hash-map :time 0 :property % :value 0) properties)})

(t/deftest a-layer-is-drawn-without-the-layers-in-it
  (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20})
        b (cts/setup-shape {:type :circle :x 40 :y 10 :width 20 :height 20})
        {:keys [bid objects]} (board-with a b)
        drawable (svgv/without-layers objects #{(:id a)})]
    (t/is (= [(:id b)] (get-in drawable [bid :shapes])) "a layer of its own")
    (let [markup (svgv/layer-markup drawable bid nil)]
      (t/is (not (str/includes? markup "<svg")) "only what is in the svg it is drawn in")
      (t/is (str/includes? markup "<ellipse") "what is no layer of its own")
      (t/is (not (str/includes? markup (str (:id a)))) "nor the layer"))))

(t/deftest a-layer-draws-as-vectors-at-rest
  (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20 :opacity 0.5
                            :fills [{:fill-color "#ff0000" :fill-opacity 1}]
                            :blur {:id (random-uuid) :type :layer-blur :value 4 :hidden false}})
        {:keys [objects]} (board-with a)
        markup (svgv/layer-markup objects (:id a) nil)]
    (t/is (str/includes? markup "<rect"))
    (t/is (str/includes? markup "#ff0000"))
    (t/is (not (str/includes? markup "<image")) "no picture")
    (t/is (not (re-find #"opacity=\"0.5\"|opacity:0.5" markup)) "its group has the opacity")
    (t/is (not (str/includes? markup "feGaussianBlur"))
          "its blur on a filter of its own, as strong as on the canvas (see `asvg/svg-filter-blur?`)")
    (t/is (not (str/includes? (svgv/layer-markup objects (:id a) (track-of :blur)) "feGaussianBlur"))
          "one that plays too")
    (t/is (str/includes? (svgv/layer-markup (update objects (:id a) assoc-in [:blur :hidden] true) (:id a) nil)
                         "<rect")
          "a hidden one, none")))

(t/deftest the-strokes-of-a-trimmed-layer-are-one-long
  (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20
                            :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                       :stroke-style :solid :stroke-alignment :center}]})
        {:keys [objects]} (board-with a)
        plain   (svgv/layer-markup objects (:id a) nil)
        trimmed (svgv/layer-markup objects (:id a) (track-of :trim-end))]
    (t/is (not (str/includes? plain "pathLength")))
    (t/is (= 1 (count (re-seq #"pathLength=\"1\"" trimmed))) "its stroke, not its fill")))

(t/deftest the-stroke-of-an-outline-of-several-subpaths-is-drawn-by-pieces
  ;; the canvas trims it along them all: each its share of the length
  ;; two closed outlines, of 3 and 1 times 10 · (2 + √2) px, as an inner or
  ;; outer stroke takes only them
  (doseq [alignment [:center :inner :outer]]
    (let [a       (cts/setup-shape {:type :path :content (path/from-string "M 0 0 L 30 0 L 30 30 Z M 40 0 L 50 0 L 50 10 Z")
                                    :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                               :stroke-style :solid :stroke-alignment alignment}]})
          {:keys [objects]} (board-with a)
          trimmed (svgv/layer-markup objects (:id a) (track-of :trim-end))
          plain   (svgv/layer-markup objects (:id a) nil)
          pieces  (re-seq #"<path pathLength=\"([^\"]*)\" class=\"trim-piece-(\d)\" d=\"([^\"]*)\"" trimmed)]
      (t/is (= ["0" "1"] (mapv #(nth % 2) pieces)) (name alignment))
      (t/is (every? true? (map (fn [[_ length] share] (< (js/Math.abs (- (js/parseFloat length) share)) 1e-9))
                               pieces [0.75 0.25])))
      (t/is (every? #(= 1 (count (re-seq #"m" (nth % 3)))) pieces) "one subpath each")
      (t/is (not (str/includes? plain "trim-piece")) "not trimmed, whole")
      (when (not= :center alignment)
        (t/is (re-find #"<(?:clipPath|mask)[^>]*><path pathLength=\"1\" d=\"m[^\"]*m" trimmed)
              "its clip or mask whole"))))
  (t/testing "an outline of one subpath, as before"
    (let [a (cts/setup-shape {:type :path :content (path/from-string "M 0 0 L 30 0")
                              :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                         :stroke-style :solid :stroke-alignment :center}]})
          {:keys [objects]} (board-with a)]
      (t/is (not (str/includes? (svgv/layer-markup objects (:id a) (track-of :trim-end)) "trim-piece"))))))

(defn- trimmed-markup
  "The markup of a layer drawing `content` with `stroke`, trimmed."
  [content stroke]
  (let [a (cts/setup-shape {:type :path :content (path/from-string content)
                            :strokes [(merge {:stroke-opacity 1 :stroke-width 2 :stroke-style :solid
                                              :stroke-alignment :center}
                                             stroke)]})
        {:keys [objects]} (board-with a)]
    (svgv/layer-markup objects (:id a) (track-of :trim-end))))

(def ^:private two-triangles
  "Two closed outlines, 50 wide and 30 high together."
  "M 0 0 L 30 0 L 30 30 Z M 40 0 L 50 0 L 50 10 Z")

(t/deftest the-pieces-of-a-stroke-paint-with-its-gradient-on-the-whole-outline
  (doseq [type [:linear :radial]]
    (let [gradient {:type type :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5 :width 1
                    :stops [{:color "#ff0000" :opacity 1 :offset 0} {:color "#0000ff" :opacity 1 :offset 1}]}
          markup   (trimmed-markup two-triangles {:stroke-color-gradient gradient})
          tag      (if (= :linear type) "linearGradient" "radialGradient")
          [copy]   (re-find (re-pattern (str "<" tag "[^>]*id=\"([^\"]*-whole)\"[^>]*>")) markup)
          whole-id (second (re-find #"id=\"([^\"]*-whole)\"" (or copy "")))]
      (t/is (some? copy) (str "a copy of its " (name type) " gradient"))
      (t/is (str/includes? copy "gradientUnits=\"userSpaceOnUse\""))
      (t/is (str/includes? copy "matrix(50 0 0 30 0 0") "laid on the box of the whole outline")
      (t/is (= 2 (count (re-seq (re-pattern (str "class=\"trim-piece-\\d\"[^>]*url\\(#" whole-id "\\)")) markup)))
            "both pieces paint with it"))))

(t/deftest the-pieces-of-a-stroke-paint-with-its-image-on-the-whole-outline
  (let [markup (trimmed-markup two-triangles {:stroke-image {:id (uuid/next) :width 10 :height 10 :mtype "image/png"}})
        copy   (re-find #"<pattern[^>]*id=\"[^\"]*-whole\"[^>]*>" markup)]
    (t/is (some? copy) "a copy of its pattern")
    (t/is (str/includes? copy "patternUnits=\"userSpaceOnUse\""))
    (t/is (re-find #"\swidth=\"5\d" copy) "as wide as the whole outline and the stroke past it")
    (t/is (= 2 (count (re-seq #"class=\"trim-piece-\d\"[^>]*-whole" markup))))))

(t/deftest the-caps-of-a-stroke-stay-on-the-ends-of-its-whole-outline
  (let [markup (trimmed-markup "M 0 0 L 30 0 M 0 10 L 10 10"
                               {:stroke-color "#000000" :stroke-cap-start :triangle-arrow :stroke-cap-end :circle-marker})
        piece  #(re-find (re-pattern (str "<path[^>]*class=\"trim-piece-" % "\"[^>]*>")) markup)]
    (t/is (str/includes? (piece 0) "marker-start"))
    (t/is (not (str/includes? (piece 0) "marker-end")))
    (t/is (str/includes? (piece 1) "marker-end"))
    (t/is (not (str/includes? (piece 1) "marker-start")))))

(t/deftest a-stroke-whose-gradient-is-missing-stays-whole
  (let [split  #'svgv/split-trim-pieces
        markup (str "<g id=\"strokes-1-abc\" class=\"strokes\"><g class=\"stroke-shape\"><defs></defs>"
                    "<path d=\"M0,0L30,0M0,10L10,10\" style=\"stroke:url(#nowhere)\"></path></g></g>")]
    (t/is (= markup (split markup "abc" [[0 0.75] [0.75 0.25]])))))

(t/deftest the-effects-of-a-layer-are-not-drawn-in-it
  ;; A filter of its own draws them, as soft as the canvas does (see
  ;; `asvg/svg-filter-shadows?`).
  (let [shadow {:id (random-uuid) :offset-x 4 :offset-y 4 :blur 8 :spread 0 :hidden false
                :color {:color "#000000" :opacity 0.3}}
        a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20
                            :shadow [(assoc shadow :style :drop-shadow) (assoc shadow :style :inner-shadow)]
                            :blur {:id (random-uuid) :type :layer-blur :value 4 :hidden false}})
        {:keys [objects]} (board-with a)
        rest   (svgv/layer-markup objects (:id a) nil)
        played (svgv/layer-markup objects (:id a) (track-of :shadow-offset-x))]
    (t/is (not (str/includes? rest "feOffset")) "its shadows at rest")
    (t/is (not (str/includes? played "feOffset")) "and played, inner ones too")
    (t/is (not (str/includes? played "feGaussianBlur")) "the blur with them, on top")
    (t/is (str/includes? (svgv/layer-markup (update objects (:id a) assoc :shadow [(assoc shadow :hidden true)]
                                                    :blur nil)
                                            (:id a) nil)
                         "<rect")
          "hidden ones, none")))

(t/deftest corners-smil-plays-are-a-marked-path
  (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20 :r1 0 :r2 0 :r3 0 :r4 0
                            :fills [{:fill-color "#ff0000" :fill-opacity 1}]
                            :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                       :stroke-style :solid :stroke-alignment :inner}]})
        {:keys [objects]} (board-with a)
        track  {:keyframes [{:time 0 :property :r1 :value 0} {:time 1000 :property :r1 :value 8}]}
        markup (svgv/layer-markup objects (:id a) track)]
    (t/is (str/includes? (svgv/layer-markup objects (:id a) nil) "<rect") "alike, a rect")
    (t/is (not (str/includes? markup "<rect")) "not alike as they play, a path")
    (t/is (<= 3 (count (re-seq #"<path [^>]*data-penpot-outline=\"\"" markup)))
          "its fill, its stroke and what cuts it, marked")))

(t/deftest a-path-resized-is-marked-for-smil
  ;; SMIL plays its size on its outline: its fill and its stroke, and what
  ;; cuts an inner one, marked.
  (let [p (cts/setup-shape {:type :path
                            :content (path/content [{:command :move-to :params {:x 10 :y 10}}
                                                    {:command :line-to :params {:x 50 :y 10}}
                                                    {:command :line-to :params {:x 50 :y 40}}
                                                    {:command :close-path :params {}}])
                            :fills [{:fill-color "#ff0000" :fill-opacity 1}]
                            :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                       :stroke-style :solid :stroke-alignment :inner}]})
        {:keys [objects]} (board-with p)
        track  {:keyframes [{:time 0 :property :width :value 40} {:time 1000 :property :width :value 80}]}
        markup (svgv/layer-markup objects (:id p) track)]
    (t/is (<= 3 (count (re-seq #"<path [^>]*data-penpot-outline=\"\"" markup))))
    (t/is (not (str/includes? (svgv/layer-markup objects (:id p) nil) "data-penpot-outline")) "not at rest")))

(t/deftest a-morphing-path-is-marked-for-smil
  ;; SMIL plays its outline on the elements of its outline.
  (let [p (cts/setup-shape {:type :path
                            :content (path/content [{:command :move-to :params {:x 10 :y 10}}
                                                    {:command :line-to :params {:x 50 :y 10}}
                                                    {:command :line-to :params {:x 50 :y 40}}
                                                    {:command :close-path :params {}}])
                            :fills [{:fill-color "#ff0000" :fill-opacity 1}]
                            :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                       :stroke-style :solid :stroke-alignment :center}]})
        {:keys [objects]} (board-with p)
        track  {:keyframes [{:time 0 :property :content :value (:content p)}
                            {:time 1000 :property :content :value (:content p)}]}
        markup (svgv/layer-markup objects (:id p) track)]
    (t/is (<= 2 (count (re-seq #"<path [^>]*data-penpot-outline=\"\"" markup))) "its fill and its stroke")))

(t/deftest a-turned-rect-resized-draws-as-a-marked-path
  (let [a (assoc (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20 :r1 0 :r2 0 :r3 0 :r4 0
                                   :fills [{:fill-color "#ff0000" :fill-opacity 1}]})
                 :rotation 30)
        {:keys [objects]} (board-with a)
        track  {:keyframes [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}]}
        markup (svgv/layer-markup objects (:id a) track)]
    (t/is (not (str/includes? markup "<rect")) "a path")
    (t/is (re-find #"<path [^>]*data-penpot-outline=\"\"" markup) "marked")))

(t/deftest the-uses-of-a-layer-are-copies
  ;; CSS plays on the elements, not on the copies a `<use>` draws.
  (t/is (= "<g><defs><clipPath id=\"c\"><rect x=\"1\" style=\"stroke:red\"></rect></clipPath></defs><rect x=\"1\" clip-path=\"url(#c)\" style=\"fill:none;stroke:red\"></rect></g>"
           (svgv/inline-uses (str "<g><defs><clipPath id=\"c\"><use href=\"#s\"></use></clipPath>"
                                  "<rect x=\"1\" style=\"stroke:red\" id=\"s\"></rect></defs>"
                                  "<use href=\"#s\" clip-path=\"url(#c)\" style=\"fill:none\"></use></g>")))
        "the style of the element over the one of the use")
  (doseq [alignment [:inner :outer]]
    (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20
                              :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                         :stroke-style :solid :stroke-alignment alignment}]})
          {:keys [objects]} (board-with a)
          markup (svgv/layer-markup objects (:id a) nil)]
      (t/is (not (str/includes? markup "<use")) (name alignment))
      (t/is (not (str/includes? markup "id=\"stroke-shape")) "nor what they used"))))

(defn- text-of
  "A text of `runs`, `[text fills]` each, in one paragraph."
  [& runs]
  (-> (cts/setup-shape {:type :text :x 10 :y 10 :width 200 :height 20})
      (assoc :content {:type "root"
                       :children [{:type "paragraph-set"
                                   :children [{:type "paragraph"
                                               :children (mapv (fn [[text fills]] {:text text :fills fills}) runs)}]}]})))

(t/deftest a-text-outlines-in-the-paint-of-its-text
  (let [red   [{:fill-color "#ff0000" :fill-opacity 1}]
        green [{:fill-color "#00ff00" :fill-opacity 1}]]
    (t/is (= red (svgv/text-fills (text-of ["Hi " red] ["you" red]))))
    (t/is (= red (svgv/text-fills (text-of ["Hi" red] [" " green]))) "blank text paints nothing")
    (t/is (nil? (svgv/text-fills (text-of ["Hi " red] ["you" green]))) "one outline has one paint")
    (let [text    (text-of ["Hi" red])
          content (path/from-plain [{:command :move-to :params {:x 20 :y 12}}
                                    {:command :line-to :params {:x 60 :y 12}}
                                    {:command :line-to :params {:x 60 :y 26}}
                                    {:command :close-path :params {}}])
          drawn   (svgv/outlined text content red)
          view    (svgv/outlined-view text content red)]
      (t/is (= [:path red] ((juxt :type :fills) drawn)))
      (t/is (= [20 12 40 14] (mapv #(js/Math.round (get-in drawn [:selrect %])) [:x :y :width :height]))
            "drawn where its glyphs are")
      (t/is (= :path (:type view)))
      (t/is (= (:selrect text) (:selrect view)) "played on the box of the text")
      (let [{:keys [objects]} (board-with drawn)
            markup (svgv/layer-markup objects (:id drawn) nil)]
        (t/is (str/includes? markup "d=\"m20,12l40,0l0,14z\"") "its glyphs, compact")
        (t/is (not (str/includes? markup "<text")))
        (t/is (empty? (ff/shape->fonts drawn objects)) "nor fonts")))))

(t/deftest path-data-is-written-compact
  ;; rounded to 0.01 before it is made relative, so nothing drifts
  (t/is (= "m10,20l20,.5c1,.5,2,1.5,3.33,2.5zm-5,-15l1,1"
           (svgv/compact-path "M10.004,20L30,20.5C31,21,32,22,33.333,23Z M5,5L6,6")))
  (t/is (= "m.1,-.2l1,1" (svgv/compact-path "M0.1,-0.2 1.1,0.8")) "numbers after a move are lines")
  (t/is (= "M0,0A5,5,0,0,1,10,10" (svgv/compact-path "M0,0A5,5,0,0,1,10,10")) "other commands as they are"))

(t/deftest a-span-paints-the-outlines-of-its-glyphs
  (let [red   [{:fill-color "#ff0000" :fill-opacity 1}]
        green [{:fill-color "#00ff00" :fill-opacity 1}]
        text  (text-of ["Hi " red] ["you" green])
        empty (update-in text [:content :children 0 :children] #(into [{:type "paragraph" :children []}] %))]
    (t/is (= green (svgv/span-fills text 0 1)))
    (t/is (= green (svgv/span-fills empty 0 1)) "as render-wasm counts paragraphs: those with spans")
    (let [content (path/from-plain [{:command :move-to :params {:x 20 :y 12}}
                                    {:command :line-to :params {:x 30 :y 12}}
                                    {:command :close-path :params {}}])
          {:keys [objects]} (board-with text)
          markup (svgv/unit-markup (get objects (:id text)) [{:paragraph 0 :span 0 :content content}
                                                             {:paragraph 0 :span 1 :content content}]
                                   nil)]
      (t/is (= 2 (count (re-seq #"<path" markup))) "a path by run")
      (t/is (str/includes? markup "#ff0000"))
      (t/is (str/includes? markup "#00ff00"))
      (t/is (not (str/includes? markup "<text"))))))

(t/deftest media-loads-for-the-board-alone-a-few-at-a-time
  ;; Not every image of the page, all at once, for one board.
  (let [image   #(cts/setup-shape {:type :rect :x % :y 10 :width 20 :height 20 :hidden %2
                                   :fills [{:fill-opacity 1
                                            :fill-image {:id (uuid/next) :width 10 :height 10
                                                         :mtype "image/png"}}]})
        [b1 b2] (repeatedly 2 #(cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100}))
        hidden  (image 0 true)
        shown   (mapv #(image (* 20 %) false) (range 6))
        other   (image 0 false)
        in      (fn [board kids] (map (fn [k] [(:id k) (assoc k :parent-id (:id board) :frame-id (:id board))]) kids))
        objects (-> {(:id b1) (assoc b1 :shapes (mapv :id (cons hidden shown)))
                     (:id b2) (assoc b2 :shapes [(:id other)])}
                    (into (in b1 (cons hidden shown)))
                    (into (in b2 [other])))
        id-of   #(str (get-in % [:fills 0 :fill-image :id]))
        fetched (atom [])]
    ;; fetches that never end, counted as they start
    (with-redefs [http/fetch-data-uri (mock/stub (fn [uri] (rx/create (fn [_] (swap! fetched conj uri) nil))))]
      (svgv/load-media objects (:id b1)))
    (t/is (= 4 (count @fetched)) "four at a time, the others when those are done")
    (t/is (every? (fn [uri] (some #(str/includes? uri (id-of %)) shown)) @fetched)
          "of the shapes it shows, not of the hidden one nor of the other board")))

(t/deftest media-loads-but-what-a-hidden-group-holds
  ;; It draws none of it.
  (let [image   #(cts/setup-shape {:type :rect :x % :y 10 :width 20 :height 20
                                   :fills [{:fill-opacity 1
                                            :fill-image {:id (uuid/next) :width 10 :height 10
                                                         :mtype "image/png"}}]})
        board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100})
        group   (cts/setup-shape {:type :group :x 0 :y 10 :width 20 :height 20 :hidden true})
        inside  (image 0)
        shown   (image 40)
        [bid gid iid sid] (map :id [board group inside shown])
        objects {bid (assoc board :shapes [gid sid])
                 gid (assoc group :parent-id bid :frame-id bid :shapes [iid])
                 iid (assoc inside :parent-id gid :frame-id bid)
                 sid (assoc shown :parent-id bid :frame-id bid)}
        fetched (atom [])]
    (with-redefs [http/fetch-data-uri (mock/stub (fn [uri] (rx/create (fn [_] (swap! fetched conj uri) nil))))]
      (svgv/load-media objects bid))
    (t/is (= 1 (count @fetched)))
    (t/is (str/includes? (first @fetched) (str (get-in shown [:fills 0 :fill-image :id]))))))

(defn- contract-board
  "A board holding a rect with two fills and an inner, an outer and a
  center stroke, an ellipse, and a board that clips, their fills,
  strokes, trim, corners and sizes playing."
  []
  (let [stroke  (fn [alignment color]
                  {:stroke-color color :stroke-opacity 1 :stroke-width 4
                   :stroke-style :solid :stroke-alignment alignment})
        board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300
                                  :fills [{:fill-color "#ffffff" :fill-opacity 1}]})
        rect    (cts/setup-shape {:type :rect :x 20 :y 20 :width 120 :height 80 :r1 8 :r2 8 :r3 8 :r4 8
                                  :fills [{:fill-color "#ff0000" :fill-opacity 1}
                                          {:fill-color "#00ff00" :fill-opacity 0.5}]
                                  :strokes [(stroke :inner "#000000") (stroke :outer "#0000ff")
                                            (stroke :center "#ff00ff")]})
        ellipse (cts/setup-shape {:type :circle :x 180 :y 20 :width 80 :height 80
                                  :fills [{:fill-color "#ffcc00" :fill-opacity 1}]
                                  :strokes [(stroke :center "#333333")]})
        card    (cts/setup-shape {:type :frame :x 20 :y 140 :width 160 :height 100 :r1 12 :r2 12 :r3 12 :r4 12
                                  :show-content false
                                  :fills [{:fill-color "#34c759" :fill-opacity 1}]})
        kid     (cts/setup-shape {:type :rect :x 30 :y 150 :width 40 :height 40
                                  :fills [{:fill-color "#ffffff" :fill-opacity 1}]})
        [bid rid eid cid kid-id] (map :id [board rect ellipse card kid])
        kf      (fn [tl id property index from to]
                  (-> tl
                      (cta/add-keyframe id (cond-> {:time 0 :property property :value from}
                                             (some? index) (assoc :index index)))
                      (cta/add-keyframe id (cond-> {:time 1000 :property property :value to}
                                             (some? index) (assoc :index index)))))]
    {:bid bid
     :objects {bid (assoc board :shapes [rid eid cid])
               rid (assoc rect :parent-id bid :frame-id bid)
               eid (assoc ellipse :parent-id bid :frame-id bid)
               cid (assoc card :parent-id bid :frame-id bid :shapes [kid-id])
               kid-id (assoc kid :parent-id cid :frame-id cid)}
     :tl (-> (cta/make-timeline {:board-id bid :duration 1000})
             (kf rid :fill-color 0 "#ff0000" "#880000")
             (kf rid :fill-color 1 "#00ff00" "#008800")
             (kf rid :stroke-color 0 "#000000" "#444444")
             (kf rid :stroke-width 1 4 8)
             (kf rid :stroke-color 2 "#ff00ff" "#880088")
             (kf rid :trim-end nil 1 0.5)
             ;; corners alike play as CSS
             (kf rid :r1 nil 8 20) (kf rid :r2 nil 8 20) (kf rid :r3 nil 8 20) (kf rid :r4 nil 8 20)
             (kf eid :fill-opacity 0 1 0.5)
             (kf eid :width nil 80 120)
             (kf cid :r1 nil 12 24) (kf cid :r2 nil 12 24) (kf cid :r3 nil 12 24) (kf cid :r4 nil 12 24)
             (kf cid :width nil 160 200))}))

(defn- vector-svg
  "The SVG of vectors of the board `bid` of `objects` playing `tl`: its
  document, its style and the selectors the style animates."
  [bid objects tl]
  (let [resolved (cta/resolve-animations tl objects)
        layers   (aexp/export-layer-ids objects resolved)
        drawable (svgv/without-layers objects (set layers))
        images   (into {}
                       (map (fn [id] [id {:markup (svgv/layer-markup drawable id (get-in resolved [:tracks id]))}]))
                       (cons bid layers))
        svg      (asvg/timeline->svg tl objects images {})
        document (.. (JSDOM. svg #js {:contentType "image/svg+xml"}) -window -document)
        style    (.-textContent (.querySelector document "style"))]
    {:document  document
     :layers    layers
     :style     style
     :selectors (map second (re-seq #"(?m)^([^@\s{}][^{}\n]*) \{\n  animation: penpot-svg-" style))}))

(t/deftest the-css-of-an-svg-finds-what-the-svg-renderer-draws
  ;; The CSS that plays the fills, strokes, trim, corners and sizes of an
  ;; SVG of vectors picks their elements by how Penpot's SVG renderer
  ;; draws them: every selector it writes finds one in the SVG it makes.
  (let [{:keys [bid objects tl]} (contract-board)
        {:keys [document layers selectors]} (vector-svg bid objects tl)]
    (doseq [[part what] [["> defs > pattern > g > :nth-child(" "the fills in a pattern"]
                         ["[id=\"fills-" "a fill"]
                         [".inner-stroke-shape > :not(defs)" "an inner stroke"]
                         [".inner-stroke-shape > defs > clipPath > *" "the clip of an inner stroke"]
                         [".outer-stroke-shape > [mask]" "an outer stroke"]
                         [".outer-stroke-shape > defs > mask > :first-child" "the mask of an outer stroke"]
                         [".outer-stroke-shape > :not(defs):not([mask])" "the outline of an outer stroke"]
                         [".stroke-shape > :not(defs)" "a center stroke"]
                         ["> defs > clipPath > rect" "the clip of a board"]
                         ["[id=\"penpot-clip-" "the clip of the layers in a board"]]]
      (t/is (some #(str/includes? % part) selectors) (str what " plays")))
    (doseq [selector selectors]
      (t/is (pos? (.-length (.querySelectorAll document selector))) selector))
    (t/testing "the outlines of trimmed strokes are of length 1"
      (let [painted (.querySelectorAll document (str ".strokes[id$=\"-" (first layers) "\"] path, "
                                                     ".strokes[id$=\"-" (first layers) "\"] rect"))]
        (t/is (pos? (.-length painted)))
        (t/is (every? #(= "1" (.getAttribute % "pathLength")) (array-seq painted)))))))

(t/deftest the-opacity-of-a-gradient-or-an-image-fill-plays-in-its-pattern
  ;; The renderer paints a gradient or an image fill in a pattern of the
  ;; shape, its opacity on the element there: the opacity plays on that
  ;; element, as `opacity` on an image, which takes no fill opacity.
  (let [board    (cts/setup-shape {:type :frame :x 0 :y 0 :width 300 :height 200
                                   :fills [{:fill-color "#ffffff" :fill-opacity 1}]})
        gradient {:type :linear :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5 :width 1
                  :stops [{:color "#ff0000" :opacity 1 :offset 0} {:color "#0000ff" :opacity 1 :offset 1}]}
        grect    (cts/setup-shape {:type :rect :x 20 :y 20 :width 100 :height 60
                                   :fills [{:fill-color-gradient gradient :fill-opacity 0.5}]})
        irect    (cts/setup-shape {:type :rect :x 150 :y 20 :width 100 :height 60
                                   :fills [{:fill-color "#00ff00" :fill-opacity 1}
                                           {:fill-image {:id (uuid/next) :width 10 :height 10 :mtype "image/png"}
                                            :fill-opacity 1}]})
        [bid gid iid] (map :id [board grect irect])
        objects  {bid (assoc board :shapes [gid iid])
                  gid (assoc grect :parent-id bid :frame-id bid)
                  iid (assoc irect :parent-id bid :frame-id bid)}
        kf       (fn [tl id index from to]
                   (-> tl
                       (cta/add-keyframe id {:time 0 :property :fill-opacity :index index :value from})
                       (cta/add-keyframe id {:time 1000 :property :fill-opacity :index index :value to})))
        tl       (-> (cta/make-timeline {:board-id bid :duration 1000})
                     (kf gid 0 0.5 1)
                     (kf iid 1 1 0.2))
        {:keys [document style selectors]} (vector-svg bid objects tl)
        in-pattern (fn [id] (str "[id=\"shape-" id "\"] > defs > pattern > g > :nth-child(1)"))]
    (t/testing "a single gradient fill"
      (t/is (some #{(in-pattern gid)} selectors) "its opacity plays in the pattern")
      (t/is (pos? (.-length (.querySelectorAll document (in-pattern gid)))))
      (t/is (not-any? #(str/includes? % (str "fills-" gid)) selectors)
            "not on the shape, which paints with the pattern"))
    (t/testing "an image fill among two"
      (t/is (some #{(in-pattern iid)} selectors))
      (t/is (= "image" (some-> (.querySelector document (in-pattern iid)) .-localName))
            "the element of the image fill")
      (t/is (re-find #"(?<!fill-)opacity: 0\.2" style) "an image plays opacity"))
    (doseq [selector selectors]
      (t/is (pos? (.-length (.querySelectorAll document selector))) selector))))

(t/deftest a-shape-resized-on-its-elements-paints-over-its-box
  ;; As the SVG renderer draws it: its fills (a gradient, more than one)
  ;; in a pattern over the box of what they fill, the mask of its outer
  ;; stroke over all it covers as it plays (see `asvg/svg-sized-paint`).
  (let [rect     (cts/setup-shape {:type :rect :x 20 :y 20 :width 120 :height 80
                                   :fills [{:fill-color-gradient {:type :linear :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5
                                                                  :width 1
                                                                  :stops [{:color "#ff0000" :opacity 1 :offset 0}
                                                                          {:color "#0000ff" :opacity 1 :offset 1}]}}
                                           {:fill-color "#00ff00" :fill-opacity 0.5}]
                                   :strokes [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 4
                                              :stroke-style :solid :stroke-alignment :outer}]})
        rid      (:id rect)
        {:keys [bid objects]} (board-with rect)
        tl       (-> (cta/make-timeline {:board-id bid :duration 1000})
                     (cta/add-keyframe rid {:time 0 :property :width :value 120})
                     (cta/add-keyframe rid {:time 1000 :property :width :value 240}))
        resolved (cta/resolve-animations tl objects)
        layers   (aexp/export-layer-ids objects resolved)
        drawable (svgv/without-layers objects (set layers))
        images   (into {}
                       (map (fn [id] [id {:markup (svgv/layer-markup drawable id (get-in resolved [:tracks id]))}]))
                       (cons bid layers))
        document (.. (JSDOM. (asvg/timeline->svg tl objects images {}) #js {:contentType "image/svg+xml"})
                     -window -document)
        pattern  (.querySelector document (str "[id=\"shape-" rid "\"] > defs > pattern"))
        mask     (.querySelector document (str "mask[id$=\"-" rid "-0\"]"))
        attrs    (fn [node names] (mapv #(.getAttribute node %) names))]
    (t/is (aexp/svg-box-sized? rect (get-in resolved [:tracks rid])))
    (t/is (= ["objectBoundingBox" "objectBoundingBox" "0" "0" "1" "1"]
             (attrs pattern ["patternUnits" "patternContentUnits" "x" "y" "width" "height"])))
    (t/is (= [["1" "1"] ["1" "1"]] (map #(attrs % ["width" "height"]) (array-seq (.querySelectorAll pattern "rect"))))
          "each fill over all of it")
    ;; from 20 to 140 it grows to -40 to 200 around its middle, its
    ;; stroke 4 wide, a miter 4 times
    (t/is (= ["-56" "4" "272" "112"] (attrs mask ["x" "y" "width" "height"])))))
