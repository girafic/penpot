;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-canvas-test
  "The canvas of a phone: when the view mode starts on it, when it shows,
  and the routes in and out of it."
  (:require
   [app.main.data.viewer.canvas :as dvc]
   [cljs.test :as t :include-macros true]))

(def ^:private file-params
  {:file-id "f" :page-id "p"})

(t/deftest a-link-to-no-board-starts-on-the-canvas
  (t/is (dvc/starts-on-canvas? {:file-id "f"}) "a tap in the dashboard: no page yet")
  (t/is (dvc/starts-on-canvas? file-params))
  (t/is (dvc/starts-on-canvas? (assoc file-params :section "interactions")))
  (t/is (dvc/starts-on-canvas? (assoc file-params :share-id "s" :index "0"))
        "a share link: its dialog always writes the first board")
  (t/is (dvc/starts-on-canvas? (assoc file-params :share-id "s" :index ["2" "0"]))
        "a repeated key counts by its last value"))

(t/deftest a-link-to-a-board-starts-on-the-prototype
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :frame-id "b"))))
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :index "0")))
        "an index without a share link was chosen")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :share-id "s" :index "3"))))
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :follow "true"))) "live")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :preview "true")))
        "the preview of the workspace")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :section "comments"))))
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :section :inspect)))))

(t/deftest a-choice-made-on-the-phone-stays
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :canvas "false" :share-id "s" :index "0")))
        "the prototype chosen: a reload stays on its board")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :canvas "true")))
        "the canvas already in the route"))

(t/deftest the-canvas-shows-where-it-can-with-no-board
  (let [params (assoc file-params :canvas "true")]
    (t/is (dvc/canvas-shown? true params))
    (t/is (dvc/canvas-shown? true (assoc params :section "interactions")))
    (t/is (not (dvc/canvas-shown? false params)) "not where it cannot: a computer, no WASM")
    (t/is (not (dvc/canvas-shown? nil params)))
    (t/is (not (dvc/canvas-shown? true (assoc params :index "2"))) "a board in the route")
    (t/is (not (dvc/canvas-shown? true (assoc params :section "inspect"))))
    (t/is (not (dvc/canvas-shown? true (assoc file-params :canvas "false"))))
    (t/is (not (dvc/canvas-shown? true file-params)))))

(t/deftest from-the-canvas-to-a-board-and-back
  (let [canvas (dvc/canvas-params (assoc file-params :share-id "s" :index "0" :frame-id "b"
                                         :section "interactions" :zoom "fit"))
        board  (dvc/board-params canvas 3)]
    (t/is (= {:file-id "f" :page-id "p" :share-id "s" :zoom "fit" :canvas "true"} canvas))
    (t/is (dvc/canvas-shown? true canvas))
    (t/is (= (assoc canvas :index 3 :canvas "false") board) "one step, the canvas left")
    (t/is (not (dvc/canvas-shown? true board)))
    (t/is (not (dvc/starts-on-canvas? (update board :index str))) "a reload stays on the board")
    (t/is (= canvas (dvc/canvas-params board)) "back to the canvas")))

(t/deftest a-board-off-the-canvas-keeps-the-route
  (t/is (= (assoc file-params :index 2) (dvc/board-params file-params 2))
        "no canvas in the route: none added")
  (t/is (= (assoc file-params :index 2 :canvas "false")
           (dvc/board-params (assoc file-params :canvas "false") 2))))
