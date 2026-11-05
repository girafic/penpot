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

(t/deftest the-view-mode-reads-the-scroll-of-its-containers-clamped
  (let [objects (-> (screen-file 1890)
                    (cthc/add-frame :inner :parent-label :list :x 24 :y 100 :width 354 :height 200
                                    :scroll-overflow :vertical)
                    (cthf/current-page)
                    :objects)
        id      #(cthi/id %)]
    (t/is (= #{(id :list) (id :chips) (id :inner)} (dvs/container-ids objects (id :screen))))
    (t/is (= #{(id :list) (id :inner)} (dvs/container-ids objects (id :list)))
          "a board inside another is a container of its own")
    (t/is (= {(id :list) [0 1520] (id :chips) [446 0]}
             (dvs/view-offsets objects {(id :list)   {:offset [0 2000] :past [0 30]}
                                        (id :chips)  {:offset [999 5]}
                                        (id :screen) {:offset [0 40]}}))
          "within its range, the rubber band past it; a screen scrolls as a page does")))

(t/deftest a-container-clips-its-content-in-the-view-mode
  (let [objects (screen-objects 1890 :show-content true)
        clipped (dvs/scroll-objects objects)]
    (t/is (false? (get-in clipped [(cthi/id :list) :show-content])))
    (t/is (= (get objects (cthi/id :chips)) (get clipped (cthi/id :chips))) "already clipped")
    (let [plain (screen-objects 1890)]
      (t/is (identical? plain (dvs/scroll-objects plain)) "nothing to grow or clip: the same objects"))))

(t/deftest a-wheel-gesture-goes-along-the-axis-it-moves-more
  (t/is (= :y (dvs/gesture-axis [3 40])) "a trackpad swiped down")
  (t/is (= :x (dvs/gesture-axis [-30 4])))
  (t/is (dvs/takes-gesture? #{:x} :x))
  (t/is (not (dvs/takes-gesture? #{:x} :y)) "a chip row lets a swipe down go on to the screen")
  (t/is (dvs/takes-gesture? #{:x :y} :y))
  (t/is (dvs/can-scroll? [0 1490] [0 100] #{:y} [0 40]))
  (t/is (dvs/can-scroll? [0 1490] [0 100] #{:y} [0 -40]))
  (t/is (not (dvs/can-scroll? [0 1490] [0 1490] #{:y} [0 40])) "at its end")
  (t/is (not (dvs/can-scroll? [0 1490] [0 0] #{:y} [0 -40])) "at its start")
  (t/is (not (dvs/can-scroll? [446 0] [0 0] #{:x} [0 40])) "not along its axis"))

(t/deftest the-wheel-finds-the-innermost-container-under-the-pointer
  (let [objects (-> (screen-file 1890)
                    (cthc/add-frame :inner :parent-label :list :x 24 :y 100 :width 354 :height 200
                                    :scroll-overflow :vertical)
                    (cths/add-sample-shape :item :parent-label :inner :x 24 :y 100 :width 354 :height 400)
                    (cthf/current-page)
                    :objects)
        id      #(cthi/id %)
        ids     (dvs/container-ids objects (id :screen))
        at      (fn [x y & {:keys [offsets extra]}]
                  (dvs/container-at objects ids (or offsets {}) (gpt/point x y) :extra extra))]
    (t/is (= [(id :inner) (id :list)] (at 100 150)) "the innermost first")
    (t/is (= [(id :list)] (at 100 350)) "below the inner one")
    (t/is (= [(id :chips)] (at 100 620)))
    (t/is (= [] (at 390 620)) "beside it")
    (t/is (= [(id :list)] (at 100 160 :offsets {(id :list) [0 150]}))
          "the inner one scrolled up off the pointer")
    (t/is (= [(id :inner) (id :list)] (at 100 50 :offsets {(id :list) [0 80]}))
          "scrolled up into the pointer")
    (t/is (= [(id :chips)] (at 100 560 :extra {(id :chips) (gpt/point 0 -50)}))
          "where a pin holds it")))

(t/deftest a-sheet-root-places-and-takes-the-wheel-by-its-box
  (let [objects (-> (cthf/sample-file :file1)
                    (cthc/add-frame :sheet :name "Sheet" :x 0 :y 0 :width 402 :height 500
                                    :scroll-overflow :vertical)
                    (cths/add-sample-shape :item :parent-label :sheet :x 0 :y 1120 :width 402 :height 80)
                    (cthf/current-page)
                    :objects
                    (dvs/scroll-objects))
        sheet   (get objects (cthi/id :sheet))
        ids     #{(cthi/id :sheet)}]
    (t/is (= 1200 (get-in sheet [:selrect :height])) "grown by the view mode")
    (t/is (= 500 (get-in (dvs/design-box sheet) [:selrect :height])) "placed by its box")
    (t/is (= [(cthi/id :sheet)] (dvs/container-at objects ids {} (gpt/point 10 400))))
    (t/is (= [] (dvs/container-at objects ids {} (gpt/point 10 600))) "below its box, nothing")
    (t/is (= (get objects (cthi/id :item)) (dvs/design-box (get objects (cthi/id :item))))
          "any other layer as it is")))

;; --- Sticky layers

(t/deftest a-sticky-layer-moves-sticks-and-is-pushed-on
  (t/is (= 200 (dvs/sticky-edge 300 50 100 0 nil)) "it moves with what scrolls")
  (t/is (= 0 (dvs/sticky-edge 300 50 400 0 nil)) "and stops at the edge")
  (t/is (= 0 (dvs/sticky-edge 300 50 400 0 600)) "until what comes next")
  (t/is (= -30 (dvs/sticky-edge 300 50 580 0 600)) "pushes it on")
  (t/is (= -110 (dvs/sticky-edge -10 50 100 0 nil)) "one above the edge moves as any other"))

(defn- sticky-list
  "A page with the container List (24, 100, 354 × 400) scrolling down
  through three sticky headers at 100, 500 and 900, a board with a label
  in it the second, and rows between down to 1500; the section Section
  of the container Sections beside it, a header in it."
  []
  (-> (cthf/sample-file :file1)
      (cthc/add-frame :screen :name "Screen" :x 0 :y 0 :width 402 :height 874)
      (cthc/add-frame :list :name "List" :parent-label :screen :x 24 :y 100 :width 354 :height 400
                      :scroll-overflow :vertical)
      (cths/add-sample-shape :h1 :parent-label :list :x 24 :y 100 :width 354 :height 40 :sticky-scroll true)
      (cths/add-sample-shape :row1 :parent-label :list :x 24 :y 140 :width 354 :height 360)
      (cthc/add-frame :h2 :parent-label :list :x 24 :y 500 :width 354 :height 40 :sticky-scroll true)
      (cths/add-sample-shape :label :parent-label :h2 :x 30 :y 505 :width 100 :height 30)
      (cths/add-sample-shape :row2 :parent-label :list :x 24 :y 540 :width 354 :height 360)
      (cths/add-sample-shape :h3 :parent-label :list :x 24 :y 900 :width 354 :height 40 :sticky-scroll true)
      (cths/add-sample-shape :row3 :parent-label :list :x 24 :y 940 :width 354 :height 560)
      (cthc/add-frame :sections :name "Sections" :parent-label :screen :x 24 :y 520 :width 354 :height 300
                      :scroll-overflow :vertical)
      (cthc/add-frame :section :parent-label :sections :x 24 :y 520 :width 354 :height 500)
      (cths/add-sample-shape :header :parent-label :section :x 24 :y 520 :width 354 :height 40 :sticky-scroll true)
      (cths/add-sample-shape :after :parent-label :sections :x 24 :y 1020 :width 354 :height 600)
      (cthf/current-page)
      :objects))

(t/deftest a-sticky-layer-of-a-container-stops-at-its-top
  (let [objects (sticky-list)
        at      (fn [offset label]
                  (some-> (get (dvs/scroll-shifts objects {(cthi/id :list) [0 offset]}) (cthi/id label))
                          :y js/Math.round))]
    (t/is (= -50 (at 50 :row1)) "the rows scroll")
    (t/is (= 0 (at 50 :h1)) "the first header, at the top of the list, stays")
    (t/is (= -300 (at 300 :h2)) "the second one scrolls up to the top")
    (t/is (= -20 (at 380 :h1)) "and pushes the first one on")
    (t/is (= -400 (at 450 :h2)) "then stays at the top itself")
    (t/is (= -400 (at 450 :label)) "with what is in it")
    (t/is (= -90 (at 450 :h1)))))

(t/deftest a-sticky-layer-leaves-with-its-board
  (let [objects (sticky-list)
        at      (fn [offset label]
                  (some-> (get (dvs/scroll-shifts objects {(cthi/id :sections) [0 offset]}) (cthi/id label))
                          :y js/Math.round))]
    (t/is (= -100 (at 100 :section)))
    (t/is (= 0 (at 100 :header)) "the header of the section stays at the top")
    (t/is (= -60 (at 520 :header)) "until the end of its section takes it on")))

(t/deftest a-sticky-layer-of-a-row-stops-at-its-left
  (let [objects (-> (cthf/sample-file :file1)
                    (cthc/add-frame :screen :name "Screen" :x 0 :y 0 :width 402 :height 874)
                    (cthc/add-frame :chips :name "Chips" :parent-label :screen :x 24 :y 600 :width 354 :height 48
                                    :scroll-overflow :horizontal)
                    (cths/add-sample-shape :tag :parent-label :chips :x 24 :y 604 :width 80 :height 40
                                           :sticky-scroll true)
                    (cths/add-sample-shape :chip :parent-label :chips :x 110 :y 604 :width 700 :height 40)
                    (cthf/current-page)
                    :objects)
        shifts  (dvs/scroll-shifts objects {(cthi/id :chips) [200 0]})]
    (t/is (= -200 (:x (get shifts (cthi/id :chip)))))
    (t/is (= 0 (:x (get shifts (cthi/id :tag)))) "the tag stays at the left edge")))

(t/deftest a-stuck-layer-gives-with-the-rubber-band
  (let [objects (sticky-list)
        id      (cthi/id :list)
        shifts  (dvs/view-shifts objects {id {:offset [0 2000] :past [0 30]}})]
    (t/is (= -1030 (js/Math.round (:y (get shifts (cthi/id :row3))))) "past its end the content gives")
    (t/is (= -830 (js/Math.round (:y (get shifts (cthi/id :h3)))))
          "the header stuck at the top goes with it: 100 - 900 - 30")))
