;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.canvas-window-test
  (:require
   [app.main.ui.workspace.canvas-window :as cw]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(def ^:private frame {:x 1000 :y 600 :w 400 :h 252})

(t/deftest the-canvas-window-starts-at-the-bottom-right
  (t/is (= {:x 1184 :y 732 :w 400 :h 252} (cw/start-frame 1600 1000))))

(t/deftest it-moves
  (t/is (= {:x 900 :y 650 :w 400 :h 252} (cw/moved frame -100 50))))

(t/deftest its-corners-resize-it-keeping-the-opposite-one
  (t/is (= {:x 950 :y 580 :w 450 :h 272} (cw/resized frame :top-left -50 -20)))
  (t/is (= {:x 1000 :y 580 :w 450 :h 272} (cw/resized frame :top-right 50 -20)))
  (t/is (= {:x 950 :y 600 :w 450 :h 272} (cw/resized frame :bottom-left -50 20)))
  (t/is (= {:x 1000 :y 600 :w 450 :h 272} (cw/resized frame :bottom-right 50 20)))
  (t/is (= {:x 1200 :y 726 :w cw/min-width :h cw/min-height} (cw/resized frame :top-left 500 500))
        "no smaller than the smallest window"))

(t/deftest it-stays-in-the-browser-window
  (t/is (= {:x 1200 :y 748 :w 400 :h 252} (cw/clamp-frame (cw/moved frame 400 400) 1600 1000))
        "dragged past the edge")
  (t/is (= {:x 0 :y 0 :w 400 :h 252} (cw/clamp-frame (cw/moved frame -2000 -2000) 1600 1000)))
  (t/is (= {:x 10 :y 10 :w 568 :h 368} (cw/clamp-frame {:x 10 :y 10 :w 900 :h 900} 600 400))
        "no larger than the browser window"))

(t/deftest a-click-on-its-bar-commits-nothing
  ;; A window where it starts stays there, following the corner of the
  ;; browser window.
  (t/is (nil? (cw/drag-commit {:mode :move :frame frame :last frame})))
  (t/is (= [(cw/moved frame 10 0) false]
           (cw/drag-commit {:mode :move :frame frame :last (cw/moved frame 10 0)}))
        "moved")
  (t/is (= [(cw/resized frame :top-left -10 -10) true]
           (cw/drag-commit {:mode :top-left :frame frame :last (cw/resized frame :top-left -10 -10)}))
        "resized"))

(t/deftest put-back-it-fits-the-board-again-when-its-size-changes
  (t/is (nil? (cw/reset-refit frame 1600 1000)) "only moved, the board fits as it did")
  (t/is (= :after-resize (cw/reset-refit (assoc frame :w 600) 1600 1000)))
  (t/is (nil? (cw/reset-refit {:x 0 :y 0 :w 368 :h 252} 400 1000))
        "as large as it starts in a narrow browser window"))

(t/deftest the-canvas-is-under-the-bar-of-its-window
  ;; The bar covers none of the board fitted in it.
  (t/is (= {:x 1001 :y 625 :w 398 :h 226} (cw/canvas-box frame)))
  (t/is (= (+ (:y frame) 1 cw/bar-height) (:y (cw/canvas-box frame)))))

(defn- drag!
  "Press the element `node` of the DOM of `window` at `from`, move the
  pointer to `to` and let go, as the browser tells a drag."
  [window node [x0 y0] [x1 y1]]
  (let [pointer (fn [type x y]
                  (thd/act #(.dispatchEvent node (new (.-MouseEvent window) type
                                                      #js {:bubbles true :clientX x :clientY y}))))]
    (pointer "pointerdown" x0 y0)
    (pointer "pointermove" x1 y1)
    (thd/act #(.dispatchEvent node (new (.-Event window) "lostpointercapture" #js {:bubbles true})))))

(t/deftest a-drag-that-goes-nowhere-leaves-no-frame-behind
  ;; Where the window showed while dragged is dropped, so it shows where
  ;; it was left, where a double click puts it back and where it follows
  ;; the browser window.
  (thd/with-dom
    (fn [{:keys [root document window]}]
      ;; jsdom captures no pointer
      (unchecked-set (.. window -HTMLElement -prototype) "setPointerCapture" (fn [_]))
      (let [changes (atom [])
            commits (atom [])
            render! (fn [frame]
                      (reset! changes [])
                      (thd/render! root (mf/element cw/canvas-window*
                                                    #js {:frame frame
                                                         :onChange #(swap! changes conj %)
                                                         :onCommit #(swap! commits conj [%1 %2])})))
            small   (assoc frame :w cw/min-width :h cw/min-height)]
        (render! frame)
        (drag! window (.querySelector document "[data-mode=move]") [1100 605] [1100 605])
        (t/is (= [frame nil] @changes) "its bar let go where it was pressed")
        (render! small)
        (drag! window (.querySelector document "[data-mode=top-left]") [1000 600] [1010 610])
        (t/is (= [small nil] @changes) "a corner of the smallest window pushed in")
        (t/is (empty? @commits) "nothing kept")
        (render! frame)
        (drag! window (.querySelector document "[data-mode=move]") [1100 605] [1080 600])
        (t/is (= [[(cw/moved frame -20 -5) false]] @commits) "a drag that goes somewhere kept")))))
