;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.svg-vectors-test
  (:require
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
          "its blur on a filter of its own, as strong as on the canvas (see `cta/svg-filter-blur?`)")
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

(t/deftest the-effects-of-a-layer-are-not-drawn-in-it
  ;; A filter of its own draws them, as soft as the canvas does (see
  ;; `cta/svg-filter-shadows?`).
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
