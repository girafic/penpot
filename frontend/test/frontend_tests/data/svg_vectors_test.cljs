;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.svg-vectors-test
  (:require
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.main.data.exports.svg-vectors :as svgv]
   [app.main.ui.shapes.text.fontfaces :as ff]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]))

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

(t/deftest a-layer-is-drawn-without-the-layers-in-it
  (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20})
        b (cts/setup-shape {:type :circle :x 40 :y 10 :width 20 :height 20})
        {:keys [bid objects]} (board-with a b)
        drawable (svgv/without-layers objects #{(:id a)})]
    (t/is (= [(:id b)] (get-in drawable [bid :shapes])) "a layer of its own")
    (let [markup (svgv/layer-markup drawable bid #{})]
      (t/is (not (str/includes? markup "<svg")) "only what is in the svg it is drawn in")
      (t/is (str/includes? markup "<ellipse") "what is no layer of its own")
      (t/is (not (str/includes? markup (str (:id a)))) "nor the layer"))))

(t/deftest a-layer-draws-as-vectors-at-rest
  (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20 :opacity 0.5
                            :fills [{:fill-color "#ff0000" :fill-opacity 1}]
                            :blur {:id (random-uuid) :type :layer-blur :value 4 :hidden false}})
        {:keys [objects]} (board-with a)
        markup (svgv/layer-markup objects (:id a) #{})]
    (t/is (str/includes? markup "<rect"))
    (t/is (str/includes? markup "#ff0000"))
    (t/is (not (str/includes? markup "<image")) "no picture")
    (t/is (not (re-find #"opacity=\"0.5\"|opacity:0.5" markup)) "its group has the opacity")
    (t/is (str/includes? markup "feGaussianBlur") "its blur, at rest")
    (t/is (not (str/includes? (svgv/layer-markup objects (:id a) #{:blur}) "feGaussianBlur"))
          "but the one CSS plays")))

(t/deftest the-strokes-of-a-trimmed-layer-are-one-long
  (let [a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20
                            :strokes [{:stroke-color "#00ff00" :stroke-opacity 1 :stroke-width 2
                                       :stroke-style :solid :stroke-alignment :center}]})
        {:keys [objects]} (board-with a)
        plain   (svgv/layer-markup objects (:id a) #{})
        trimmed (svgv/layer-markup objects (:id a) #{:trim-end})]
    (t/is (not (str/includes? plain "pathLength")))
    (t/is (= 1 (count (re-seq #"pathLength=\"1\"" trimmed))) "its stroke, not its fill")))

(t/deftest drop-shadows-css-plays-are-not-drawn
  (let [shadow {:id (random-uuid) :offset-x 4 :offset-y 4 :blur 8 :spread 0 :hidden false
                :color {:color "#000000" :opacity 0.3}}
        a (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20
                            :shadow [(assoc shadow :style :drop-shadow)]})
        {:keys [objects]} (board-with a)]
    (t/is (str/includes? (svgv/layer-markup objects (:id a) #{}) "feOffset") "at rest")
    (t/is (not (str/includes? (svgv/layer-markup objects (:id a) #{:shadow-offset-x}) "feOffset"))
          "played by CSS")))

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
          markup (svgv/layer-markup objects (:id a) #{})]
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
            markup (svgv/layer-markup objects (:id drawn) #{})]
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
                                   #{})]
      (t/is (= 2 (count (re-seq #"<path" markup))) "a path by run")
      (t/is (str/includes? markup "#ff0000"))
      (t/is (str/includes? markup "#00ff00"))
      (t/is (not (str/includes? markup "<text"))))))
