;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-turn-test
  "A phone or a tablet of the view mode turned: the board it shows then,
  the counterpart of the board or the board turned by its constraints."
  (:require
   [app.common.test-helpers.compositions :as cthc]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.common.uuid :as uuid]
   [app.main.data.viewer.devices :as dvd]
   [app.main.data.viewer.turn :as dvt]
   [cljs.test :as t :include-macros true]))

(defn- board
  [name w h]
  {:id (uuid/next) :name name :type :frame :frame-id uuid/zero :parent-id uuid/zero
   :selrect {:x 0 :y 0 :width w :height h}})

(t/deftest a-name-without-the-way-it-stands
  (t/is (= "menu" (dvt/base-name "Menu")))
  (t/is (= "menu" (dvt/base-name "Menu (landscape)")))
  (t/is (= "menu" (dvt/base-name "Menu – Landscape")))
  (t/is (= "menu" (dvt/base-name "Portrait / Menu")))
  (t/is (= "full menu" (dvt/base-name "Full menu_horizontal")))
  (t/is (= "menu 2" (dvt/base-name "Menu landscape 2")))
  (t/is (= "landscapes" (dvt/base-name "Landscapes")) "a word of its own only"))

(t/deftest a-board-of-the-same-name-the-other-way-is-its-counterpart
  (let [menu   (board "Menu" 402 874)
        wide   (board "Menu (landscape)" 874 402)
        other  (board "Detail (landscape)" 874 402)
        frames [menu other wide]]
    (t/is (= wide (dvt/counterpart frames menu :landscape)))
    (t/is (= menu (dvt/counterpart frames wide :portrait)))
    (t/is (nil? (dvt/counterpart frames menu :portrait)) "standing as it does: none")
    (t/is (nil? (dvt/counterpart frames other :portrait)))))

(t/deftest held-as-a-board-stands-it-shows-as-it-is
  (let [menu (board "Menu" 402 874)]
    (doseq [hold [nil :portrait]]
      (let [{:keys [frame-id device turned?]} (dvt/shown [menu] menu "iphone-island" hold)]
        (t/is (= (:id menu) frame-id))
        (t/is (= [402 874 false] ((juxt :screen-w :screen-h :landscape?) device)))
        (t/is (not turned?))))
    (t/is (= {:frame-id (:id menu) :device nil} (dvt/shown [menu] menu "none" :landscape))
          "no device")
    (let [watch (board "Watch" 416 496)]
      (t/is (not (:landscape? (:device (dvt/shown [watch] watch "watch" :landscape))))
            "a watch does not turn"))))

(t/deftest held-the-other-way-it-shows-the-counterpart
  (let [menu (board "Menu" 402 874)
        wide (board "Menu (landscape)" 874 402)
        {:keys [frame-id device turned?]} (dvt/shown [menu wide] menu "auto" :landscape)]
    (t/is (= (:id wide) frame-id))
    (t/is (= [:iphone-island true 874 402]
             ((juxt :kind :landscape? :screen-w :screen-h) device))
          "in the device of the board, on its side")
    (t/is (not turned?))))

(t/deftest held-the-other-way-without-one-it-shows-the-board-turned
  (let [menu (board "Menu" 402 874)
        {:keys [frame-id device turned?]} (dvt/shown [menu] menu "iphone-island" :landscape)]
    (t/is (= (:id menu) frame-id))
    (t/is turned?)
    (t/is (= [true 874 402] ((juxt :landscape? :screen-w :screen-h) device))))
  (let [wide (board "Player" 874 402)
        {:keys [device turned?]} (dvt/shown [wide] wide "iphone-island" :portrait)]
    (t/is turned?)
    (t/is (= [false 402 874] ((juxt :landscape? :screen-w :screen-h) device)))))

(t/deftest two-turns-give-the-prototype-back-as-drawn
  (let [menu  (board "Menu" 402 874)
        stood (dvd/resolve-device "iphone-island" 402 874)]
    (t/is (= :landscape (dvt/next-hold menu "iphone-island" stood)))
    (t/is (nil? (dvt/next-hold menu "iphone-island" (dvd/turn stood)))))
  (let [wide (board "Player" 874 402)]
    (t/is (= :portrait (dvt/next-hold wide "iphone-island"
                                      (dvd/resolve-device "iphone-island" 874 402))))))

(defn- phone-objects
  "A page with the board Menu 402 × `h`: a card across it, a bar at its
  bottom and, fixed when scrolling, tabs at the bottom of the screen."
  [h]
  (-> (cthf/sample-file :file1)
      (cthc/add-frame :menu :name "Menu" :x 0 :y 0 :width 402 :height h)
      (cths/add-sample-shape :card :parent-label :menu :x 24 :y 100 :width 354 :height 200
                             :constraints-h :leftright :constraints-v :top)
      (cths/add-sample-shape :end :parent-label :menu :x 0 :y (- h 90) :width 402 :height 90
                             :constraints-h :leftright :constraints-v :bottom)
      (cths/add-sample-shape :tabs :parent-label :menu :x 0 :y 786 :width 402 :height 88
                             :constraints-h :leftright :constraints-v :bottom :fixed-scroll true)
      (cthf/current-page)
      :objects))

(defn- box
  [objects label]
  (let [{:keys [x y width height]} (get-in objects [(cthi/id label) :selrect])]
    (mapv #(js/Math.round %) [x y width height])))

(t/deftest a-board-turns-by-its-constraints
  (let [objects (phone-objects 874)
        frames  [(get objects (cthi/id :menu))]
        turned  (dvt/turn-objects objects frames "iphone-island" :landscape)]
    (t/is (identical? objects (dvt/turn-objects objects frames "iphone-island" nil)))
    (t/is (= [0 0 874 874] (box turned :menu))
          "as wide as the screen on its side, as high as it was: it scrolls")
    (t/is (= [24 100 826 200] (box turned :card)) "stretched across")
    (t/is (= [0 784 874 90] (box turned :end)) "at the bottom of the board")
    (t/is (= [0 314 874 88] (box turned :tabs))
          "fixed when scrolling, at the bottom of the screen")))

(t/deftest a-long-board-turns-as-its-screen
  (let [objects (phone-objects 1814)
        frames  [(get objects (cthi/id :menu))]
        turned  (dvt/turn-objects objects frames "iphone-island" :landscape)]
    (t/is (= [0 0 874 1814] (box turned :menu)))
    (t/is (= [0 1724 874 90] (box turned :end)))
    (t/is (= [0 314 874 88] (box turned :tabs))
          "at the bottom of the screen it scrolls in")))

(t/deftest a-board-turns-from-its-first-screen-in-a-lower-phone
  (let [objects (phone-objects 874)
        frames  [(get objects (cthi/id :menu))]
        turned  (dvt/turn-objects objects frames "iphone-home" :landscape)]
    (t/is (= [0 0 715 874] (box turned :menu)) "as wide as the screen of the iPhone SE on its side")
    (t/is (= [0 314 715 88] (box turned :tabs))
          "fixed when scrolling, from the bottom of the board's own screen to the bottom of this one")))

(t/deftest a-board-with-a-counterpart-stays-as-it-is
  (let [objects (phone-objects 874)
        wide    (board "Menu (landscape)" 874 402)
        frames  [(get objects (cthi/id :menu)) wide]]
    (t/is (= objects (dvt/turn-objects objects frames "iphone-island" :landscape)))))
