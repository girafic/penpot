;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-sticky-boxes-test
  "The layers that stick at the top of a screen as it scrolls: which ones
  get a box of their own, and the span it sticks in (Penpot Motion)."
  (:require
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.ui.viewer.viewport-common :as vpc]
   [cljs.test :as t :include-macros true]))

(def ^:private board-id (uuid/custom 11 1))
(def ^:private status-bar (uuid/custom 11 2))
(def ^:private heading-1 (uuid/custom 11 3))
(def ^:private section-1 (uuid/custom 11 4))
(def ^:private section-head (uuid/custom 11 5))
(def ^:private section-2 (uuid/custom 11 6))
(def ^:private heading-2 (uuid/custom 11 7))
(def ^:private chips (uuid/custom 11 8))
(def ^:private chip (uuid/custom 11 9))
(def ^:private card (uuid/custom 11 10))
(def ^:private card-title (uuid/custom 11 11))
(def ^:private holder (uuid/custom 11 12))
(def ^:private held-bar (uuid/custom 11 13))
(def ^:private above (uuid/custom 11 14))
(def ^:private sheet-id (uuid/custom 11 15))
(def ^:private sheet-title (uuid/custom 11 16))
(def ^:private sheet-more (uuid/custom 11 17))
(def ^:private ghost (uuid/custom 11 18))

(defn- shape
  [id type parent-id x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type type :x x :y y :width w :height h})
      (merge {:id id :parent-id parent-id :frame-id parent-id} attrs)))

(def ^:private objects
  (->> [(shape board-id :frame uuid/zero 0 0 402 1814 :frame-id uuid/zero
               :shapes [heading-1 section-1 section-2 ghost heading-2 chips card holder above status-bar])
        (shape status-bar :rect board-id 0 0 402 54 :fixed-scroll true)
        (shape heading-1 :rect board-id 0 100 402 40 :sticky-scroll true)
        (shape section-1 :frame board-id 0 200 402 700 :shapes [section-head])
        (shape section-head :rect section-1 0 200 402 50 :sticky-scroll true)
        (shape section-2 :frame board-id 0 900 402 700)
        (shape ghost :rect board-id 0 1200 402 40 :sticky-scroll true :hidden true)
        (shape heading-2 :rect board-id 0 1650 402 40 :sticky-scroll true)
        (shape chips :frame board-id 0 1700 402 40 :scroll-overflow :horizontal :shapes [chip])
        (shape chip :rect chips 0 1700 80 40 :sticky-scroll true)
        (shape card :frame board-id 20 1740 200 60 :sticky-scroll true :shapes [card-title])
        (shape card-title :rect card 20 1740 200 20 :sticky-scroll true)
        (shape holder :frame board-id 220 1740 160 60 :sticky-scroll true :shapes [held-bar])
        (shape held-bar :rect holder 220 1740 160 20 :fixed-scroll true)
        (shape above :rect board-id 0 -20 402 40 :sticky-scroll true)
        (shape sheet-id :frame uuid/zero 500 0 402 600 :frame-id uuid/zero :scroll-overflow :vertical
               :shapes [sheet-title])
        (shape sheet-title :frame sheet-id 500 20 402 50 :sticky-scroll true :shapes [sheet-more])
        (shape sheet-more :rect sheet-title 850 30 40 30)]
       (into {} (map (juxt :id identity)))))

(t/deftest the-sticky-layers-of-a-screen-get-a-box-of-their-own
  (t/is (= [heading-1 section-head heading-2 card] (vpc/screen-sticky-ids objects board-id))
        "in the order they draw: not one in a container, in another one, holding a fixed one, above the board, nor a hidden one")
  (t/is (not (vpc/screen-sticky-layer? objects (get objects chip))) "it sticks in its container")
  (t/is (not (vpc/screen-sticky-layer? objects (get objects holder))) "it holds a layer that stays on screen")
  (t/is (not (vpc/screen-sticky-layer? objects (get objects above))) "it would stick at once")
  (t/is (empty? (vpc/screen-sticky-ids (dissoc objects heading-1 section-1 heading-2 card) board-id))
        "none without them"))

(t/deftest a-sticky-box-sticks-in-the-span-of-its-board
  (t/is (= {:side :top :inset 0 :edge 200 :height 80 :top 0 :bottom 3300}
           (vpc/sticky-span objects board-id heading-1 2))
        "on the screen itself: until the next sticky layer pushes it on, not a hidden one")
  (t/is (= {:side :top :inset 0 :edge 400 :height 100 :top 400 :bottom 1800}
           (vpc/sticky-span objects board-id section-head 2))
        "in a section: it leaves with it")
  (t/is (= {:side :top :inset 0 :edge 1650 :height 40 :top 0 :bottom 1740}
           (vpc/sticky-span objects board-id heading-2 1))
        "the next one need not be in a box of its own")
  (t/is (= {:side :top :inset 0 :edge 1740 :height 60 :top 0 :bottom 1814}
           (vpc/sticky-span objects board-id card 1))
        "the last one: to the end of the screen"))

(t/deftest an-overlay-goes-where-a-stuck-layer-shows
  (t/is (nil? (vpc/screen-stuck-move objects board-id heading-1 50 874)) "not stuck yet: at its place")
  (t/is (= {:id heading-1 :dy 200} (vpc/screen-stuck-move objects board-id heading-1 300 874))
        "stuck at the top of the screen: as far down as the page scrolled past it")
  (t/is (= {:id card :dy 60} (vpc/screen-stuck-move objects board-id card-title 1800 874))
        "a layer in one goes with it")
  (t/is (= {:id section-head :dy 650} (vpc/screen-stuck-move objects board-id section-head 880 874))
        "pushed on by the end of its section")
  (t/is (nil? (vpc/screen-stuck-move objects board-id chip 1800 874)) "one of a container sticks there")
  (t/is (nil? (vpc/screen-stuck-move objects board-id (uuid/custom 11 99) 300 874)) "none without it")
  (t/is (nil? (vpc/screen-stuck-move objects board-id sheet-more 300 874))
        "one of an overlay over the screen: the page of the screen scrolls not under it")
  (t/is (= {:id sheet-title :dy 280} (vpc/screen-stuck-move objects sheet-id sheet-more 300 874))
        "as the screen itself it sticks"))

(def ^:private cta (uuid/custom 12 1))
(def ^:private form (uuid/custom 12 2))
(def ^:private total (uuid/custom 12 3))
(def ^:private nav-head (uuid/custom 12 4))
(def ^:private early-head (uuid/custom 12 5))

(def ^:private edge-objects
  (->> [(shape board-id :frame uuid/zero 0 0 402 1814 :frame-id uuid/zero
               :shapes [early-head nav-head form cta])
        (shape early-head :rect board-id 0 20 402 40 :sticky-scroll true :sticky-offset 44)
        (shape nav-head :rect board-id 0 100 402 40 :sticky-scroll true :sticky-offset 44)
        (shape form :frame board-id 0 900 402 500 :shapes [total])
        (shape total :rect form 0 1340 402 60 :sticky-scroll true :sticky-edge :bottom)
        (shape cta :rect board-id 0 1500 402 60 :sticky-scroll true :sticky-edge :bottom :sticky-offset 16)]
       (into {} (map (juxt :id identity)))))

(t/deftest a-layer-sticks-at-the-bottom-of-the-screen-or-from-its-top
  (t/is (= [nav-head total cta] (vpc/screen-sticky-ids edge-objects board-id))
        "not one that starts above its line, the top of the board plus its offset")
  (t/is (= {:side :top :inset 88 :edge 200 :height 80 :top 0 :bottom 3628}
           (vpc/sticky-span edge-objects board-id nav-head 2))
        "under a bar 44 high: held 44 from the top")
  (t/is (= {:side :bottom :inset 16 :edge 1500 :height 60 :top 0 :bottom 1560}
           (vpc/sticky-span edge-objects board-id cta 1))
        "at the bottom: from the top of the board to its own end")
  (t/is (= {:side :bottom :inset 0 :edge 1340 :height 60 :top 900 :bottom 1400}
           (vpc/sticky-span edge-objects board-id total 1))
        "in a board: it rises no higher than its top"))

(t/deftest an-overlay-goes-where-a-layer-stuck-at-the-bottom-shows
  (t/is (= {:id cta :dy -702} (vpc/screen-stuck-move edge-objects board-id cta 0 874))
        "at rest it shows 16 above the bottom of the screen: 874 - 16 - 60 - 1500")
  (t/is (= {:id cta :dy -402} (vpc/screen-stuck-move edge-objects board-id cta 300 874))
        "and stays there as the page scrolls")
  (t/is (nil? (vpc/screen-stuck-move edge-objects board-id cta 800 874)) "until its place comes")
  (t/is (= {:id cta :dy -802} (vpc/screen-stuck-move edge-objects board-id cta 0 774))
        "on a lower screen higher")
  (t/is (= {:id nav-head :dy 244} (vpc/screen-stuck-move edge-objects board-id nav-head 300 874))
        "one stuck under a bar: 44 from the top"))
