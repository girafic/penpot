;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-background-test
  "The background of the prototype of a page, round its boards shown
  without a device in the view mode (Penpot Motion)."
  (:require
   [app.main.data.viewer.devices :as dvd]
   [app.main.ui.viewer.background :as vbg]
   [cljs.test :as t :include-macros true]))

(def ^:private top-to-bottom
  {:type :linear :start-x 0.5 :start-y 0 :end-x 0.5 :end-y 1 :width 1
   :stops [{:color "#000000" :opacity 1 :offset 0}
           {:color "#ffffff" :opacity 1 :offset 1}]})

(t/deftest the-background-of-the-prototype-shows-without-a-device
  (let [page {:prototype-background "#ffffff"}]
    (t/is (= "#ffffff" (vbg/background page nil)))
    (t/is (nil? (vbg/background page (dvd/resolve-device "iphone-island" 402 874)))
          "round a device, the view mode keeps its own")
    (t/is (nil? (vbg/background {} nil)) "unset, its own")
    (t/is (= top-to-bottom (vbg/background {:prototype-background top-to-bottom} nil))
          "a gradient as well")))

(t/deftest a-color-fills-the-box
  (let [style (vbg/background-style "#1e1e1e" 800 600)]
    (t/is (= "#1e1e1e" (.-backgroundColor style)))
    (t/is (undefined? (.-backgroundImage style))))
  (t/is (nil? (vbg/background-style nil 800 600)) "none, its own"))

(t/deftest a-gradient-runs-across-the-box-as-on-the-canvas
  (t/is (= "linear-gradient(180deg, #000000 0%, #ffffff 100%)"
           (.-backgroundImage (vbg/background-style top-to-bottom 800 600))))
  (t/is (= "linear-gradient(90deg, #000000 0%, #ffffff 100%)"
           (.-backgroundImage (vbg/background-style (assoc top-to-bottom :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5)
                                                    800 600)))
        "turned, from left to right")
  (t/is (= "radial-gradient(300px 300px at 400px 300px, #000000 0%, #ffffff 100%)"
           (.-backgroundImage (vbg/background-style (assoc top-to-bottom :type :radial :start-y 0.5)
                                                    800 600)))
        "a radial one round the middle, as wide as it reaches")
  (t/is (= "linear-gradient(180deg, #000000 0%, #ffffff 100%)"
           (.-backgroundImage (vbg/background-style top-to-bottom nil nil)))
        "before its box is measured, still a gradient"))
