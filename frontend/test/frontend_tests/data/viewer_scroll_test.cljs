;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-scroll-test
  "A top board that scrolls through its content in the prototype: the
  view mode shows it as high as its content goes, its box its first
  screen."
  (:require
   [app.common.test-helpers.compositions :as cthc]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.main.data.viewer.devices :as dvd]
   [app.main.data.viewer.scroll :as dvs]
   [app.main.data.viewer.turn :as dvt]
   [cljs.test :as t :include-macros true]))

(t/use-fixtures :each {:before cthi/reset-idmap!})

(defn- menu-file
  "A page with the board Menu 402 × `h` (`board` its other attributes): a
  card at its top, a row whose bottom is at `bottom`, tabs fixed when
  scrolling at the bottom of the board and a hidden layer far below."
  [h bottom & {:as board}]
  (-> (cthf/sample-file :file1)
      (cthc/add-frame :menu (merge {:name "Menu" :x 0 :y 0 :width 402 :height h} board))
      (cths/add-sample-shape :card :parent-label :menu :x 24 :y 100 :width 354 :height 200
                             :constraints-h :leftright :constraints-v :top)
      (cths/add-sample-shape :row :parent-label :menu :x 24 :y (- bottom 80) :width 354 :height 80
                             :constraints-h :leftright :constraints-v :top)
      (cths/add-sample-shape :tabs :parent-label :menu :x 0 :y (- h 88) :width 402 :height 88
                             :constraints-h :leftright :constraints-v :bottom :fixed-scroll true)
      (cths/add-sample-shape :ghost :parent-label :menu :x 0 :y 4000 :width 402 :height 80
                             :hidden true)))

(defn- menu-objects
  [& args]
  (:objects (cthf/current-page (apply menu-file args))))

(defn- box
  [objects label]
  (let [{:keys [x y width height]} (get-in objects [(cthi/id label) :selrect])]
    (mapv #(js/Math.round %) [x y width height])))

(t/deftest a-board-that-scrolls-is-as-high-as-its-content
  (let [objects  (menu-objects 874 1300 :scroll-overflow :vertical)
        scrolled (dvs/scroll-objects objects)
        menu     (get scrolled (cthi/id :menu))]
    (t/is (= 1300 (dvs/scroll-height objects (get objects (cthi/id :menu))))
          "down to its lowest row: not the tabs fixed when scrolling, nor the hidden layer")
    (t/is (= [0 0 402 1300] (box scrolled :menu)))
    (t/is (= 874 (dvs/screen-h menu)) "its box, its first screen")
    (t/is (= 874 (dvd/board-first-screen-h menu)))
    (t/is (= (map #(box objects %) [:card :row :tabs])
             (map #(box scrolled %) [:card :row :tabs]))
          "its layers stay where they are")))

(t/deftest the-bottom-padding-of-its-layout-goes-below-its-content
  (let [objects (menu-objects 874 1300 :scroll-overflow :vertical
                              :layout :flex :layout-flex-dir :column
                              :layout-padding-type :multiple
                              :layout-padding {:p1 0 :p2 0 :p3 48 :p4 0})]
    (t/is (= [0 0 402 1348] (box (dvs/scroll-objects objects) :menu)))))

(t/deftest a-board-that-does-not-scroll-or-whose-content-fits-stays-as-it-is
  (let [plain (menu-objects 874 1300)
        fits  (menu-objects 874 700 :scroll-overflow :vertical)]
    (t/is (identical? plain (dvs/scroll-objects plain)))
    (t/is (nil? (dvs/scroll-height fits (get fits (cthi/id :menu)))))
    (t/is (identical? fits (dvs/scroll-objects fits)))
    (t/is (nil? (dvs/screen-h (get plain (cthi/id :menu)))))))

(t/deftest only-a-top-board-scrolls
  (let [objects (-> (menu-file 874 700)
                    (cthc/add-frame :list :parent-label :menu :x 24 :y 320 :width 354 :height 200
                                    :scroll-overflow :vertical)
                    (cths/add-sample-shape :item :parent-label :list :x 24 :y 320 :width 354 :height 600)
                    (cthf/current-page)
                    :objects)]
    (t/is (identical? objects (dvs/scroll-objects objects)) "a board in a board scrolls in stage 2")))

(t/deftest the-page-of-the-view-mode-lists-its-boards-as-high
  (let [objects (menu-objects 874 1300 :scroll-overflow :vertical)
        menu    (get objects (cthi/id :menu))
        page    {:objects objects :frames [menu] :all-frames [menu]}
        page'   (dvs/scroll-page page)]
    (t/is (= 1300 (get-in page' [:frames 0 :selrect :height])))
    (t/is (= 1300 (get-in page' [:all-frames 0 :selrect :height])))
    (t/is (= (get-in page' [:objects (:id menu)]) (get-in page' [:frames 0])))
    (let [plain (menu-objects 874 1300)
          still {:objects plain :frames [(get plain (cthi/id :menu))]}]
      (t/is (identical? still (dvs/scroll-page still))))))

(t/deftest a-board-that-scrolls-turns-from-its-own-first-screen
  ;; 800 high in the design: a phone of the view mode 874 high would put
  ;; its first screen lower, and the tabs, fixed at its bottom, 74 px up
  (let [objects  (dvs/scroll-objects (menu-objects 800 1600 :scroll-overflow :vertical))
        frames   [(get objects (cthi/id :menu))]
        turned   (dvt/turn-objects objects frames "iphone-island" :landscape)]
    (t/is (= 800 (dvd/board-first-screen-h (first frames))))
    (t/is (= [0 0 874 1600] (box turned :menu)) "as wide as the screen on its side, as high as its content")
    (t/is (= [0 314 874 88] (box turned :tabs)) "at the bottom of the screen it scrolls in")))
