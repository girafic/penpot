;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-overlay-test
  "Where an overlay with a shadow shows: its box where its position says,
  the margin of the shadow round it, at any zoom; on the screen as the
  page scrolls, but with a layer that scrolls when that placed it, and up
  on a screen lower than the board."
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
(def ^:private heading (uuid/custom 11 7))
(def ^:private more (uuid/custom 11 8))
(def ^:private sheet-title (uuid/custom 11 9))
(def ^:private cta (uuid/custom 11 10))
(def ^:private buy (uuid/custom 11 11))

(defn- shape
  [id type parent-id frame-id x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type type :x x :y y :width w :height h})
      (merge {:id id :parent-id parent-id :frame-id frame-id} attrs)))

(def ^:private objects
  (->> [(shape screen-id :frame uuid/zero uuid/zero 0 0 402 874 :scroll-overflow :vertical
               :shapes [row heading header cta])
        (shape row :rect screen-id screen-id 24 600 354 68)
        (shape heading :frame screen-id screen-id 0 300 402 44 :sticky-scroll true :shapes [more])
        (shape more :rect heading heading 350 310 24 24)
        (shape header :group screen-id screen-id 0 0 402 54 :fixed-scroll true :shapes [back])
        (shape back :rect header screen-id 24 10 44 44)
        (shape cta :frame screen-id screen-id 0 1000 402 60 :sticky-scroll true :sticky-edge :bottom
               :shapes [buy])
        (shape buy :rect cta cta 24 1010 354 40)
        (shape sheet-id :frame uuid/zero uuid/zero 500 0 402 560 :shapes [sheet-title option])
        (shape sheet-title :frame sheet-id sheet-id 500 20 402 44 :sticky-scroll true)
        (shape option :rect sheet-id sheet-id 524 100 354 52)]
       (into {} (map (juxt :id identity)))))

(t/deftest an-overlay-holds-to-what-placed-it-as-the-page-scrolls
  (let [screen (get objects screen-id)]
    (t/is (= :page (shapes/overlay-anchor objects row screen [])) "a row of the screen: the page")
    (t/is (= :screen (shapes/overlay-anchor objects nil screen [])) "no layer: the screen")
    (t/is (= :screen (shapes/overlay-anchor objects screen-id screen [])) "the screen itself")
    (t/is (= :layer (shapes/overlay-anchor objects back screen [])) "a layer fixed when scrolling")
    (t/is (= :layer (shapes/overlay-anchor objects option screen [{:id sheet-id :anchor :screen}]))
          "a layer of an overlay on the screen")
    (t/is (= :page (shapes/overlay-anchor objects option screen [{:id sheet-id :anchor :page}]))
          "a layer of an overlay that scrolls with the page")))

(t/deftest an-overlay-goes-where-the-sticky-layer-that-placed-it-shows
  (let [screen (get objects screen-id)
        top    (fn [stuck id] (some-> stuck :objects (get id) :selrect :y1))]
    (t/is (nil? (shapes/stuck-place objects more screen 100 874 true)) "not stuck yet: where it is")
    (let [stuck (shapes/stuck-place objects more screen 400 874 true)]
      (t/is (= :layer (:anchor stuck)) "on a screen it stays with the stuck layer")
      (t/is (= 10 (top stuck more)) "at its place on the first screen, the top")
      (t/is (= 0 (top stuck heading)) "with the layer that sticks"))
    (let [stuck (shapes/stuck-place objects more screen 400 874 false)]
      (t/is (= :page (:anchor stuck)) "on a page it scrolls with it")
      (t/is (= 410 (top stuck more)) "at the top of what the page shows"))
    (t/is (nil? (shapes/stuck-place objects sheet-title screen 400 874 true))
          "not by the sticky layer of an overlay on the screen, which the page leaves where it is")
    (let [stuck (shapes/stuck-place objects buy screen 0 874 true)]
      (t/is (= :layer (:anchor stuck)) "by a layer stuck at the bottom it stays with it")
      (t/is (= 824 (top stuck buy)) "where it shows on the first screen: 1010 - (1000 - 814)"))
    (t/is (= 924 (top (shapes/stuck-place objects buy screen 0 974 true) buy)) "lower on a higher screen")))

(t/deftest an-overlay-placed-by-the-screen-goes-up-on-a-lower-screen
  (let [placed (fn [anchor snap-v]
                 {:anchor anchor :snap-to [snap-v :center] :position (gpt/point 0 314)})
        y      #(:y (:position (viewer/overlay-on-screen %1 %2)))]
    (t/is (= 107 (y (placed :screen :bottom) 207)) "at the bottom as far")
    (t/is (= 210.5 (y (placed :screen :center) 207)) "in the middle half as far")
    (t/is (= 314 (y (placed :screen :top) 207)) "not at the top")
    (t/is (= 314 (y (placed :layer :bottom) 207)) "not by a layer, which sits where it is")
    (t/is (= 314 (y (placed :screen :bottom) nil)) "not without a lower screen")))

(t/deftest the-box-of-a-board-reaches-below-the-screen-of-a-device
  (let [board (fn [h & {:as attrs}] (merge {:selrect (grc/make-rect 0 0 402 h)} attrs))]
    (t/is (= 207 (viewer/overlay-drop (board 1450 :app.main.data.viewer.scroll/screen-h 874) {:screen-h 667}))
          "of a board that scrolls through its content, its first screen")
    (t/is (= 926 (viewer/overlay-drop (board 1800) {:screen-h 874})) "of a long board, all of it")
    (t/is (nil? (viewer/overlay-drop (board 874) {:screen-h 932})) "nothing on a higher screen")
    (t/is (nil? (viewer/overlay-drop (board 1800) nil)) "nothing without a device")))
