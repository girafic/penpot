;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.canvas-window-test
  (:require
   [app.main.ui.workspace.canvas-window :as cw]
   [cljs.test :as t :include-macros true]))

(def ^:private frame {:x 1000 :y 600 :w 400 :h 252})

(t/deftest the-canvas-window-starts-at-the-bottom-right
  (t/is (= {:x 1184 :y 732 :w 400 :h 252} (cw/start-frame 1600 1000))))

(t/deftest it-moves
  (t/is (= {:x 900 :y 650 :w 400 :h 252} (cw/moved frame -100 50))))

(t/deftest its-corners-resize-it-keeping-the-opposite-one
  (t/is (= {:x 950 :y 580 :w 450 :h 272} (cw/resized frame :top-left -50 -20)))
  (t/is (= {:x 1000 :y 580 :w 450 :h 272} (cw/resized frame :top-right 50 -20)))
  (t/is (= {:x 950 :y 600 :w 450 :h 272} (cw/resized frame :bottom-left -50 20)))
  (t/is (= {:x 1000 :y 600 :w 450 :h 272} (cw/resized frame :bottom-right 50 20)))
  (t/is (= {:x 1200 :y 726 :w cw/min-width :h cw/min-height} (cw/resized frame :top-left 500 500))
        "no smaller than the smallest window"))

(t/deftest it-stays-in-the-browser-window
  (t/is (= {:x 1200 :y 748 :w 400 :h 252} (cw/clamp-frame (cw/moved frame 400 400) 1600 1000))
        "dragged past the edge")
  (t/is (= {:x 0 :y 0 :w 400 :h 252} (cw/clamp-frame (cw/moved frame -2000 -2000) 1600 1000)))
  (t/is (= {:x 10 :y 10 :w 568 :h 368} (cw/clamp-frame {:x 10 :y 10 :w 900 :h 900} 600 400))
        "no larger than the browser window"))
