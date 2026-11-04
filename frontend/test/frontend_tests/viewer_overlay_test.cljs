;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-overlay-test
  "Where an overlay with a shadow shows: its box where its position says,
  the margin of the shadow round it, at any zoom; on the screen as the
  page scrolls, but with a layer that scrolls when that placed it."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.ui.viewer :as viewer]
   [app.main.ui.viewer.shapes :as shapes]
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

(def ^:private screen-id (uuid/custom 11 1))
(def ^:private row (uuid/custom 11 2))
(def ^:private header (uuid/custom 11 3))
(def ^:private back (uuid/custom 11 4))
(def ^:private sheet-id (uuid/custom 11 5))
(def ^:private option (uuid/custom 11 6))

(defn- shape
  [id type parent-id frame-id x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type type :x x :y y :width w :height h})
      (merge {:id id :parent-id parent-id :frame-id frame-id} attrs)))

(def ^:private objects
  (->> [(shape screen-id :frame uuid/zero uuid/zero 0 0 402 874 :scroll-overflow :vertical
               :shapes [row header])
        (shape row :rect screen-id screen-id 24 600 354 68)
        (shape header :group screen-id screen-id 0 0 402 54 :fixed-scroll true :shapes [back])
        (shape back :rect header screen-id 24 10 44 44)
        (shape sheet-id :frame uuid/zero uuid/zero 500 0 402 560 :shapes [option])
        (shape option :rect sheet-id sheet-id 524 100 354 52)]
       (into {} (map (juxt :id identity)))))

(t/deftest an-overlay-scrolls-with-the-page-by-a-layer-that-does
  (let [screen (get objects screen-id)]
    (t/is (true? (shapes/in-page? objects row screen)) "by a row of the screen")
    (t/is (false? (shapes/in-page? objects nil screen)) "not by the screen (no layer)")
    (t/is (false? (shapes/in-page? objects screen-id screen)) "nor by the screen itself")
    (t/is (false? (shapes/in-page? objects back screen)) "nor by a layer fixed when scrolling")
    (t/is (false? (shapes/in-page? objects option screen)) "nor by a layer of an overlay")))
