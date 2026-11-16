;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.frame-titles-test
  "Which boards draw their title over the canvas: on a page of thousands
  of boards, drawing all of them again at each step of a zoom made it
  stutter."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape :as cts]
   [app.main.ui.workspace.viewport.widgets :as widgets]
   [cljs.test :as t :include-macros true]))

(defn- board
  [x y width height & {:as attrs}]
  (merge (cts/setup-shape {:type :frame :x x :y y :width width :height height}) attrs))

(defn- turned
  "The `shape` turned `angle` degrees round its middle, its corners and
  its selrect as a turn leaves them."
  [shape angle]
  (gsh/transform-shape shape (ctm/rotation-modifiers shape (gsh/shape->center shape) angle)))

(def ^:private vbox
  {:x 0 :y 0 :width 1000 :height 800})

(def ^:private view
  (widgets/title-view vbox))

(t/deftest the-titles-draw-in-the-view-and-half-of-it-round
  (t/is (nil? (widgets/title-view nil)) "no view yet: everywhere")
  (t/is (= [-500 -400 2000 1600]
           ((juxt :x :y :width :height) view))))

(t/deftest a-board-in-view-draws-its-title
  (t/is (widgets/title-shown? (board 100 100 400 300) view 1 false))
  (t/is (widgets/title-shown? (board 100 100 400 300) nil 1 false) "with no view yet"))

(t/deftest a-board-far-out-of-view-draws-none
  (t/is (widgets/title-shown? (board 1200 100 400 300) view 1 false)
        "just beyond the view: there as a pan brings it in")
  (t/is (not (widgets/title-shown? (board 3000 100 400 300) view 1 false)) "beyond half the view round it")
  (t/is (not (widgets/title-shown? (board 100 -2000 400 300) view 1 false)) "above it"))

(t/deftest a-turned-board-is-where-its-corners-are
  (let [diamond (turned (board -1000 -1000 2000 2000) 45)
        ;; 400 x 250 at zoom 4, on its top corner at (0, -1414)
        corner  (widgets/title-view {:x -200 :y -1539 :width 400 :height 250})]
    (t/is (< (gpt/distance (gpt/point 0 -1414.2) (apply min-key :y (:points diamond))) 1)
          "its top corner, out of its unturned selrect")
    (t/is (> (:y1 (:selrect diamond)) -1100) "which starts below it")
    (t/is (widgets/title-shown? diamond corner 4 false)
          "the view on that corner draws its title")))

(t/deftest a-board-too-narrow-on-the-screen-draws-none
  (t/is (not (widgets/title-shown? (board 100 100 400 300) view 0.03 false)) "12 px wide")
  (t/is (widgets/title-shown? (board 100 100 400 300) view 0.04 false) "16 px wide")
  (t/is (widgets/title-shown? (turned (board 100 100 300 400) 90) view 0.04 false)
        "turned, its title runs along its height"))

(t/deftest the-board-selected-always-draws-its-title
  (t/is (widgets/title-shown? (board 100 100 400 300) view 0.01 true))
  (t/is (widgets/title-shown? (board 5000 5000 400 300) view 1 true)))
