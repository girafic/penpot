;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.device-frame-test
  "The device round the screen of the view mode: its body below the
  screen, its cutouts over it, and the box of the screen."
  (:require
   [app.main.data.viewer.devices :as dvd]
   [app.main.ui.viewer.device-frame :as dfr :refer [device-drawing*]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(defn- layout-of
  "The layout of the device `choice` for a board `bw` × `bh` in a stage
  1000 × 800."
  [choice bw bh]
  (let [device (dvd/resolve-device choice bw bh)]
    [device (dvd/layout device 1000 800 24)]))

(defn- render-drawing!
  [root choice bw bh finish cutouts & [label]]
  (let [[device layout] (layout-of choice bw bh)]
    (thd/render! root (mf/element device-drawing*
                                  #js {:layout layout
                                       :kind (:kind device)
                                       :finish finish
                                       :cutouts cutouts
                                       :label label}))
    layout))

(t/deftest the-body-and-the-cutouts-are-drawn-apart
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [{:keys [x y w h geometry]} (render-drawing! root "macbook" 1512 982 "light" false)
            svg   (.querySelector document "svg")
            style (.-style svg)]
        (t/is (= "light" (.getAttribute svg "data-finish")))
        (t/is (= "macbook" (.getAttribute svg "data-kind")))
        (t/is (= "true" (.getAttribute svg "aria-hidden")) "decoration")
        (t/is (= [x y w h] (mapv #(js/parseFloat (unchecked-get style %)) ["left" "top" "width" "height"])))
        (t/is (= (count (:parts geometry)) (.-length (.querySelectorAll svg "rect,circle")))
              "the lid and the base"))
      (let [{:keys [geometry]} (render-drawing! root "macbook" 1512 982 "light" true)]
        (t/is (= (count (:cutouts geometry))
                 (.-length (.querySelectorAll document "svg rect,svg circle")))
              "the notch")))))

(t/deftest a-device-on-its-side-turns-its-drawing
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [layout (render-drawing! root "android" 915 412 "dark" false)
            group  (.querySelector document "svg g")]
        (t/is (true? (:landscape? layout)))
        (t/is (re-find #"rotate\(-90\)" (.getAttribute group "transform")))))))

(t/deftest the-screen-takes-the-box-of-the-layout
  (let [[_ layout] (layout-of "iphone-island" 402 874)
        {:keys [x y w h radii]} (:screen layout)
        style (dfr/screen-style layout)]
    (t/is (= (mapv #(str % "px") [x y w h])
             (mapv #(unchecked-get style %) ["marginLeft" "marginTop" "width" "height"])))
    (t/is (= (str (first radii) "px " (second radii) "px " (nth radii 2) "px " (nth radii 3) "px")
             (unchecked-get style "borderRadius")))))

(t/deftest a-browser-names-the-board-in-its-field
  (thd/with-dom
    (fn [{:keys [root document]}]
      (render-drawing! root "browser" 1440 900 "dark" false "Landing page")
      (t/is (= "Landing page" (.-textContent (.querySelector document "svg text"))))
      (render-drawing! root "browser" 390 244 "dark" false
                       "A board with a name far longer than its field")
      (let [text (.-textContent (.querySelector document "svg text"))]
        (t/is (< (count text) 30) "cut to its field")
        (t/is (re-find #"…$" text))))))
