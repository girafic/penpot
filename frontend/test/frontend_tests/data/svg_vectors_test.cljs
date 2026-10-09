;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.svg-vectors-test
  (:require
   [app.common.types.shape :as cts]
   [app.main.data.exports.svg-vectors :as svgv]
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
