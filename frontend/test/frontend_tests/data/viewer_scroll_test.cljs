;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-scroll-test
  "Boards that scroll through their content in the prototype: a top
  board the view mode shows as high as its content goes, its box its
  first screen; a container whose content moves inside its box, a move
  given to WASM after it laid the shapes out."
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
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

;; --- Containers

(defn- screen-file
  "A page with the top board Screen 402 × 874 holding the container List
  (24, 0, 354 × 400, `list` its other attributes) with rows down to
  `bottom` and a layer fixed when scrolling, and the chip row Chips
  (24, 600, 354 × 48) with chips across 800 px."
  [bottom & {:as list}]
  (-> (cthf/sample-file :file1)
      (cthc/add-frame :screen :name "Screen" :x 0 :y 0 :width 402 :height 874)
      (cthc/add-frame :list (merge {:name "List" :parent-label :screen :x 24 :y 0 :width 354 :height 400
                                    :scroll-overflow :vertical}
                                   list))
      (cths/add-sample-shape :row1 :parent-label :list :x 24 :y 0 :width 354 :height 80)
      (cths/add-sample-shape :row2 :parent-label :list :x 24 :y (- bottom 80) :width 354 :height 80)
      (cths/add-sample-shape :pin :parent-label :list :x 24 :y 360 :width 354 :height 40 :fixed-scroll true)
      (cthc/add-frame :chips {:name "Chips" :parent-label :screen :x 24 :y 600 :width 354 :height 48
                              :scroll-overflow :horizontal})
      (cths/add-sample-shape :chip1 :parent-label :chips :x 24 :y 604 :width 120 :height 40)
      (cths/add-sample-shape :chip2 :parent-label :chips :x 704 :y 604 :width 120 :height 40)))

(defn- screen-objects
  [& args]
  (:objects (cthf/current-page (apply screen-file args))))

(defn- shape
  [objects label]
  (get objects (cthi/id label)))

(defn- move-of
  "The translation of `matrix`, `[x y]` rounded."
  [matrix]
  [(js/Math.round (dm/get-prop matrix :e)) (js/Math.round (dm/get-prop matrix :f))])

(t/deftest a-container-scrolls-as-far-as-its-content-goes-past-its-box
  (let [objects (screen-objects 1890)]
    (t/is (= #{:y} (dvs/container-axes objects (shape objects :list))))
    (t/is (= #{:x} (dvs/container-axes objects (shape objects :chips))))
    (t/is (= [0 1490] (dvs/scroll-range objects (shape objects :list) #{:y}))
          "rows down to 1890 in a box 400 high, the fixed layer left out")
    (t/is (= [446 0] (dvs/scroll-range objects (shape objects :chips) #{:x}))
          "chips across 800 px in a box 354 wide")
    (t/is (= [0 0] (dvs/scroll-range (screen-objects 300) (shape (screen-objects 300) :list) #{:y}))
          "content that fits")
    (let [padded (screen-objects 1890 :layout :flex :layout-flex-dir :column :layout-padding-type :multiple
                                 :layout-padding {:p1 0 :p2 0 :p3 48 :p4 0})]
      (t/is (= [0 1538] (dvs/scroll-range padded (shape padded :list) #{:y})) "its bottom padding counts"))
    (t/is (= [0 0] (dvs/clamp-offset [0 0] [10 -5])))
    (t/is (= [0 1490] (dvs/clamp-offset [0 1490] [-3 2000])))))

(t/deftest a-top-board-is-a-container-only-as-the-root-of-an-overlay
  (let [objects (-> (cthf/sample-file :file1)
                    (cthc/add-frame :sheet :name "Sheet" :x 0 :y 0 :width 402 :height 500
                                    :scroll-overflow :vertical)
                    (cths/add-sample-shape :item :parent-label :sheet :x 0 :y 1120 :width 402 :height 80)
                    (cthf/current-page)
                    :objects
                    (dvs/scroll-objects))
        sheet   (shape objects :sheet)]
    (t/is (nil? (dvs/container-axes objects sheet)) "as a screen it scrolls as a page does")
    (t/is (= #{:y} (dvs/container-axes objects sheet :overlay-root? true)))
    (t/is (= #{:y} (dvs/container-axes objects (assoc sheet :scroll-overflow :both) :overlay-root? true)))
    (t/is (nil? (dvs/container-axes objects (assoc sheet :scroll-overflow :horizontal) :overlay-root? true)))
    (t/is (= [0 700] (dvs/scroll-range objects sheet #{:y}))
          "grown to 1200 by the view mode, it scrolls from its box of 500")))

(t/deftest a-turned-or-tilted-board-does-not-scroll-its-content
  (let [objects (screen-objects 1890)
        tilted  (-> (screen-file 1890)
                    (cthc/add-group :group :parent-label :screen :rotate-x 30)
                    (cthc/add-frame :inner :parent-label :group :x 24 :y 700 :width 100 :height 100
                                    :scroll-overflow :vertical)
                    (cthf/current-page)
                    :objects)]
    (t/is (nil? (dvs/container-axes objects (assoc (shape objects :list) :transform (gmt/rotate-matrix 30)))))
    (t/is (nil? (dvs/container-axes tilted (shape tilted :inner))) "drawn in 3D")))

(t/deftest the-layers-of-a-container-move-back-by-its-offset
  (let [objects (-> (screen-file 1890)
                    (cthc/add-frame :inner :parent-label :list :x 24 :y 100 :width 354 :height 200
                                    :scroll-overflow :vertical)
                    (cths/add-sample-shape :item :parent-label :inner :x 24 :y 100 :width 354 :height 400)
                    (cthc/add-group :group :parent-label :list)
                    (cths/add-sample-shape :deep :parent-label :group :x 24 :y 500 :width 50 :height 50)
                    (cthf/current-page)
                    :objects)
        shifts  (dvs/scroll-shifts objects {(cthi/id :list) [0 100] (cthi/id :inner) [0 30] (cthi/id :chips) [0 0]})
        at      #(some-> (get shifts (cthi/id %)) ((juxt :x :y)))]
    (t/is (= [0 -100] (at :row1)))
    (t/is (= [0 -100] (at :inner)))
    (t/is (= [0 -130] (at :item)) "in two containers")
    (t/is (= [0 -100] (at :deep)) "and all within a layer")
    (t/is (nil? (at :pin)) "fixed when scrolling, it stays")
    (t/is (nil? (at :chip1)) "an offset of 0 moves nothing")))

(t/deftest the-scroll-goes-on-top-of-the-entries-wasm-laid-out
  (let [objects (-> (screen-file 1890)
                    (cthc/add-group :group :parent-label :list)
                    (cths/add-sample-shape :deep :parent-label :group :x 24 :y 500 :width 50 :height 50)
                    (cthf/current-page)
                    :objects)
        shifts  (dvs/scroll-shifts objects {(cthi/id :list) [0 100]})
        board   (gmt/translate-matrix (gpt/point 5 0))
        row     (gmt/translate-matrix (gpt/point 40 0))
        entries [[(cthi/id :screen) board] [(cthi/id :row1) row]]
        result  (into {} (dvs/scroll-entries objects entries shifts))]
    (t/is (identical? entries (dvs/scroll-entries objects entries {})) "nothing scrolls: the same entries")
    (t/is (= [5 0] (move-of (get result (cthi/id :screen)))) "the board stays")
    (t/is (= [40 -100] (move-of (get result (cthi/id :row1)))) "its own move, then the scroll")
    (t/is (= [5 -100] (move-of (get result (cthi/id :row2))))
          "without an entry: what it took from the board, then the scroll")
    (t/is (= [5 -100] (move-of (get result (cthi/id :group)))))
    (t/is (not (contains? result (cthi/id :deep))) "within a layer it moves with it")
    (t/is (not (contains? result (cthi/id :pin))) "fixed when scrolling, it takes the board's")
    (t/is (not (contains? result (cthi/id :list))) "the container stays")))

(t/deftest a-sheet-root-draws-back-at-its-box
  (let [objects (-> (cthf/sample-file :file1)
                    (cthc/add-frame :sheet :name "Sheet" :x 0 :y 0 :width 402 :height 500
                                    :scroll-overflow :vertical)
                    (cths/add-sample-shape :item :parent-label :sheet :x 0 :y 1120 :width 402 :height 80)
                    (cths/add-sample-shape :top :parent-label :sheet :x 0 :y 0 :width 402 :height 80)
                    (cthf/current-page)
                    :objects
                    (dvs/scroll-objects))
        shifts  (dvs/scroll-shifts objects {(cthi/id :sheet) [0 200]})
        result  (into {} (dvs/scroll-entries objects [] shifts :sheet-root (cthi/id :sheet)))
        root    (get result (cthi/id :sheet))]
    (t/is (= 500 (js/Math.round (* 1200 (dm/get-prop root :d)))) "shrunk from 1200 to its box")
    (t/is (= [0 -200] (move-of (get result (cthi/id :item)))) "its layers scroll, not shrunk")
    (t/is (= 1 (dm/get-prop (get result (cthi/id :top)) :d)))
    (t/is (= result (into {} (dvs/scroll-entries objects [] shifts :sheet-root (cthi/id :sheet))))
          "the same each frame")))

(t/deftest a-top-board-with-both-scrolls-vertically-as-a-screen
  (t/is (= [0 0 402 1300] (box (dvs/scroll-objects (menu-objects 874 1300 :scroll-overflow :both)) :menu)))
  (let [side (menu-objects 874 1300 :scroll-overflow :horizontal)]
    (t/is (identical? side (dvs/scroll-objects side)) "sideways it does not scroll as a screen")))
