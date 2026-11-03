;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-backdrop-test
  "The background blur of a layer fixed when scrolling in the view mode:
  a box under it blurs the layers that scroll, which draw on another
  canvas (Penpot Motion)."
  (:require
   [app.common.geom.matrix :as gmt]
   [app.common.types.shape :as cts]
   [app.main.ui.viewer.viewport-common :as vpc]
   [cljs.test :as t :include-macros true]))

(defn- shape
  [type x y w h & {:as attrs}]
  (merge (cts/setup-shape {:type type :x x :y y :width w :height h}) attrs))

(def ^:private blur
  {:id (random-uuid) :type :background-blur :value 20 :hidden false})

(t/deftest a-layer-blurs-what-is-under-it-while-it-shows
  (t/is (vpc/background-blur? (shape :rect 0 0 10 10 :background-blur blur)))
  (t/is (not (vpc/background-blur? (shape :rect 0 0 10 10))) "none")
  (t/is (not (vpc/background-blur? (shape :rect 0 0 10 10 :background-blur (assoc blur :hidden true))))
        "hidden in the design tab")
  (t/is (not (vpc/background-blur? (shape :rect 0 0 10 10 :background-blur blur :hidden true)))
        "the layer hidden")
  (t/is (not (vpc/background-blur? (shape :rect 0 0 10 10 :background-blur (assoc blur :value 0))))
        "as much as nothing"))

(t/deftest the-box-covers-the-layer-and-blurs-as-the-canvas
  (let [style (vpc/backdrop-style (shape :rect 10 20 100 50 :background-blur blur
                                         :r1 8 :r2 8 :r3 0 :r4 0)
                                  2)]
    (t/is (= ["20px" "40px" "200px" "100px"]
             [(.-left style) (.-top style) (.-width style) (.-height style)])
          "where the layer is, at the zoom of the view mode")
    (t/is (= "16px 16px 0px 0px" (.-borderRadius style)) "round where it is")
    (t/is (= "blur(24.09px)" (.-backdropFilter style))
          "as Skia blurs a radius of 20: its sigma, at the zoom")
    (t/is (= (.-backdropFilter style) (.-WebkitBackdropFilter style)))
    (t/is (undefined? (.-transform style)) "unturned"))
  (t/is (= "50%" (.-borderRadius (vpc/backdrop-style (shape :circle 0 0 40 40 :background-blur blur) 1)))
        "an ellipse, oval")
  (t/is (= "matrix(0, 1, -1, 0, 0, 0)"
           (.-transform (vpc/backdrop-style (shape :rect 0 0 40 20 :background-blur blur
                                                   :transform (gmt/matrix 0 1 -1 0 0 0))
                                            1)))
        "turned as the layer is"))
