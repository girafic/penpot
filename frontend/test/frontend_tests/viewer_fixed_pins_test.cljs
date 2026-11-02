;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-fixed-pins-test
  "The layers fixed when scrolling on a screen lower than the first
  screen of their board: they stay where their vertical constraint
  holds them (Penpot Motion)."
  (:require
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.viewer.devices :as dvd]
   [app.main.ui.viewer :as viewer]
   [app.main.ui.viewer.viewport-common :as vpc]
   [cljs.test :as t :include-macros true]))

(def ^:private board-id (uuid/custom 10 1))
(def ^:private tab-bar (uuid/custom 10 2))
(def ^:private tab (uuid/custom 10 3))
(def ^:private header (uuid/custom 10 4))
(def ^:private row (uuid/custom 10 5))
(def ^:private button (uuid/custom 10 6))
(def ^:private label (uuid/custom 10 7))

(defn- shape
  [id type parent-id x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type type :x x :y y :width w :height h})
      (merge {:id id :parent-id parent-id :frame-id board-id} attrs)))

(def ^:private objects
  (->> [(shape board-id :frame uuid/zero 0 0 402 874 :frame-id uuid/zero
               :shapes [header row tab-bar button])
        (shape header :rect board-id 0 0 402 54 :fixed-scroll true :constraints-v :top)
        (shape row :rect board-id 0 300 402 68 :constraints-v :bottom)
        (shape tab-bar :group board-id 0 786 402 88 :fixed-scroll true :constraints-v :bottom
               :shapes [tab])
        (shape tab :rect tab-bar 20 800 60 40)
        (shape button :rect board-id 300 400 60 40 :fixed-scroll true :constraints-v :center
               :shapes [label])
        (shape label :text button 310 410 40 20)]
       (into {} (map (juxt :id identity)))))

(t/deftest the-fixed-layers-at-the-bottom-go-up-on-a-lower-screen
  (t/is (= {tab-bar 159 button 79.5} (vpc/fixed-pins objects board-id 159))
        "at the bottom as far, in the middle half as far; not at the top, nor what scrolls")
  (t/is (nil? (vpc/fixed-pins objects board-id 0)) "none on a screen as high")
  (t/is (nil? (vpc/fixed-pins objects board-id nil)) "none without a device"))

(t/deftest a-fixed-layer-goes-up-with-what-it-holds
  (let [pinned (vpc/pin-objects objects {tab-bar 159})]
    (t/is (= 627 (get-in pinned [tab-bar :selrect :y])) "the tab bar")
    (t/is (= 641 (get-in pinned [tab :selrect :y])) "a tab in it")
    (t/is (= 0 (get-in pinned [header :selrect :y])) "not the others")))

(t/deftest the-fixed-layers-go-up-after-the-animation
  (let [tree (vpc/pin-modifiers {tab-bar {:modifiers (ctm/move-modifiers 10 0)}} {tab-bar 159})]
    (t/is (= [10 -159] (let [m (ctm/modifiers->transform (get-in tree [tab-bar :modifiers]))]
                         [(.-e m) (.-f m)]))
          "the move of the animation, then up")
    (t/is (some? (get-in (vpc/pin-modifiers nil {button 5}) [button :modifiers])) "without one")))

(t/deftest the-first-screen-of-a-board-is-what-goes-below-a-lower-screen
  (let [se    (dvd/resolve-device "iphone-home" 402 874)
        board {:selrect {:width 402 :height 874}}
        long  {:selrect {:width 402 :height 1814}}]
    (t/is (= 715 (:screen-h se)) "the iPhone SE is lower")
    (t/is (= 159 (viewer/fixed-drop board se)) "a board as high as a phone")
    (t/is (= 159 (viewer/fixed-drop long se)) "a long board: its first screen, not all of it")
    (t/is (nil? (viewer/fixed-drop long (dvd/resolve-device "iphone-island" 402 1814)))
          "nothing below in the phone of its size")
    (t/is (nil? (viewer/fixed-drop board nil)) "nothing without a device")
    (t/is (= 874 (dvd/first-screen-h 402 1814)) "the first screen of a long board")
    (t/is (= 500 (dvd/first-screen-h 500 500)) "all of a board no device takes")))
