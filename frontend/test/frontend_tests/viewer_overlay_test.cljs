;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-overlay-test
  "Where an overlay with a shadow shows: its box where its position says,
  the margin of the shadow round it, at any zoom."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.main.ui.viewer :as viewer]
   [cljs.test :as t :include-macros true]))

(defn- overlay
  [snap-to]
  {:frame {:selrect (grc/make-rect 500 0 402 500)}
   :position (gpt/point 0 374)
   :snap-to snap-to})

(def ^:private size
  "The bounds of a box 402 × 500 with a shadow: 116 px round it, 148 more
  below (its offset)."
  {:base-width 634 :base-height 764})

(t/deftest an-overlay-element-holds-its-box-at-its-snap-side
  (t/is (= (gpt/point 0 374) (viewer/overlay-element-position (overlay [:top :left]) size))
        "at the top left the box is where the element starts")
  (t/is (= (gpt/point -116 110) (viewer/overlay-element-position (overlay [:bottom :center]) size))
        "at the bottom the element ends where the box does, in the middle across")
  (t/is (= (gpt/point -232 242) (viewer/overlay-element-position (overlay [:center :right]) size))))

(defn- sized
  "What `calculate-size` gives for the bounds of that box at `zoom`: its
  start in px of the board, its size in px of the screen too."
  [zoom]
  (assoc size :x 384 :y -116 :width (* 634 zoom) :height (* 764 zoom)))

(t/deftest an-overlay-shifts-its-box-by-px-of-the-board-at-any-zoom
  (let [selrect (grc/make-rect 500 0 402 500)]
    (doseq [zoom [1 0.6418 2]]
      (t/is (gpt/close? (gpt/point 0 -148) (viewer/calculate-delta (sized zoom) selrect [:bottom :center] zoom))
            (str "at the bottom it moves up by the shadow below, at zoom " zoom))
      (t/is (gpt/close? (gpt/point 116 116) (viewer/calculate-delta (sized zoom) selrect [:top :left] zoom))
            (str "at the top left by the margin above and before, at zoom " zoom)))))
