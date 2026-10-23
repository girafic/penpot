;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.prototype-preview-test
  "The sizes the menu of the preview of the prototype gives its window."
  (:require
   [app.common.math :as mth]
   [app.main.ui.workspace.prototype-preview :as pp]
   [cljs.test :as t :include-macros true]))

(def ^:private frame {:x 400 :y 100 :w 600 :h 600})

(t/deftest the-window-shows-the-board-at-its-own-size
  (let [{:keys [x y w h]} (pp/actual-frame frame 1600 1000 320 180)]
    (t/is (= [322 (+ 180 pp/bar-height 2)] [w h]) "the board, the bar and the border")
    (t/is (= [700 400] [(+ x (/ w 2)) (+ y (/ h 2))]) "about its middle"))
  (t/is (<= (:w (pp/actual-frame frame 1600 1000 4000 3000)) 1600)
        "never larger than the browser window"))

(t/deftest the-window-fits-its-height-to-the-board
  (let [{:keys [x y w h]} (pp/fitted-frame frame 1600 1000 1600 900)]
    (t/is (= [400 100 600] [x y w]) "its top left and width kept")
    (t/is (mth/close? (+ (* 598 (/ 900 1600)) pp/bar-height 2) h) "as high as the board then is"))
  (let [{:keys [w h]} (pp/fitted-frame frame 1600 1000 400 1600)]
    (t/is (< w 600) "narrower for a tall board the browser window cannot hold")
    (t/is (mth/close? (/ 400 1600) (/ (- w 2) (- h pp/bar-height 2))) "as wide as the board then is")))
