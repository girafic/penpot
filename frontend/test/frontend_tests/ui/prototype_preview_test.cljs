;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.prototype-preview-test
  "The sizes the menu of the preview of the prototype gives its window."
  (:require
   [app.common.math :as mth]
   [app.main.data.viewer.devices :as dvd]
   [app.main.ui.workspace.canvas-window :as cw]
   [app.main.ui.workspace.prototype-preview :as pp]
   [cljs.test :as t :include-macros true]))

(def ^:private frame {:x 400 :y 100 :w 600 :h 600})

(t/deftest the-window-shows-the-board-at-its-own-size
  (let [{:keys [x y w h]} (pp/actual-frame frame 1600 1000 320 180)]
    (t/is (= [322 (+ 180 pp/bar-height 2)] [w h]) "the board, the bar and the border")
    (t/is (= [700 400] [(+ x (/ w 2)) (+ y (/ h 2))]) "about its middle"))
  (t/is (<= (:w (pp/actual-frame frame 1600 1000 4000 3000)) 1600)
        "never larger than the browser window"))

(t/deftest the-window-fits-its-height-to-the-board
  (let [{:keys [x y w h]} (pp/fitted-frame frame 1600 1000 1600 900)]
    (t/is (= [400 100 600] [x y w]) "its top left and width kept")
    (t/is (mth/close? (+ (* 598 (/ 900 1600)) pp/bar-height 2) h) "as high as the board then is"))
  (let [{:keys [w h]} (pp/fitted-frame frame 1600 1000 400 1600)]
    (t/is (< w 600) "narrower for a tall board the browser window cannot hold")
    (t/is (mth/close? (/ 400 1600) (/ (- w 2) (- h pp/bar-height 2))) "as wide as the board then is")))

(def ^:private iphone (dvd/resolve-device "iphone-island" 402 874))

(defn- layout-in
  "Where the view mode in the window at `frame` places `device`."
  [device {:keys [w h]}]
  (dvd/layout device (- w 2) (- h pp/bar-height 2) dvd/stage-padding dvd/preview-top))

(defn- hugs?
  "Whether `device` fills the window at `frame` but for the room round it."
  [device {:keys [h] :as frame}]
  (let [layout (layout-in device frame)]
    (and (<= 24 (:x layout) 26)
         (<= (- h pp/bar-height 2 24 4) (+ (:y layout) (:h layout)) (- h pp/bar-height 2 24)))))

(t/deftest the-window-shows-the-board-in-its-device
  (t/is (= [402 874] (pp/shown-size nil 402 874)) "no device: the board")
  (t/is (nil? (pp/shown-device "auto" 1920 1080 nil)) "none for a slide")
  (let [[w h]   (pp/shown-size iphone 402 874)
        [ow oh] (dvd/outer-size iphone)]
    (t/is (= [(+ ow 48) (+ oh 24 8)] [w h]) "the device and the room round it, little above it")
    (t/is (< 402 w) "wider than the board")
    (let [layout (layout-in iphone (pp/actual-frame {:x 0 :y 0 :w 600 :h 600} 3000 3000 w h))]
      (t/is (= 1 (:scale layout)) "at its own size in the window as large as it")
      (t/is (= 8 (:y layout)) "right under the bar"))))

(t/deftest the-window-shows-the-device-as-the-view-mode-holds-it
  (t/is (= iphone (pp/shown-device "iphone-island" 402 874 nil)) "as it stands, until it says")
  (t/is (= iphone (pp/shown-device "iphone-island" 402 874 false)) "standing")
  (t/is (= (dvd/turn iphone) (pp/shown-device "iphone-island" 402 874 true)) "on its side")
  (let [macbook (dvd/resolve-device "macbook" 1280 720)]
    (t/is (= macbook (pp/shown-device "macbook" 1280 720 true)) "a laptop does not turn")))

(t/deftest the-window-holds-no-more-than-its-device
  (let [outer (dvd/outer-size iphone)]
    (let [{:keys [x y w h] :as frame} (pp/hugged-frame {:x 0 :y 50 :w 1600 :h 900} 2000 1200 outer)]
      (t/is (= [50 900] [y h]) "its top and height kept")
      (t/is (< w 600) "as narrow as the device then is")
      (t/is (mth/close? 800 (+ x (/ w 2)) 1) "about its middle")
      (t/is (hugs? iphone frame) "the room round the device and no more")
      (t/is (= frame (pp/hugged-frame frame 2000 1200 outer)) "as it stays"))
    (let [{:keys [x y w h] :as frame} (pp/hugged-frame {:x 400 :y 50 :w 300 :h 1100} 2000 1200 outer)]
      (t/is (= [50 1100] [y h]) "as high as it was")
      (t/is (< 300 w) "wider for the device as high")
      (t/is (mth/close? 550 (+ x (/ w 2))) "about its middle")
      (t/is (hugs? iphone frame) "the room round the device"))
    (let [macbook (dvd/resolve-device "macbook" 1280 720)
          {:keys [w h] :as frame} (pp/hugged-frame {:x 0 :y 0 :w 400 :h 1100} 1200 1200
                                                   (dvd/outer-size macbook))]
      (t/is (<= w (- 1200 32)) "no wider than the browser window")
      (t/is (< h 1100) "lower, as wide as it can be")
      (t/is (hugs? macbook frame) "the room round the laptop"))
    (t/is (= {:x 0 :y 0 :w 200 :h 126}
             (pp/hugged-frame {:x 0 :y 0 :w 200 :h 126} 2000 1200 outer))
          "no smaller than the smallest window")))

(t/deftest the-device-turns-in-a-square-frame
  (let [frame  (pp/hugged-frame {:x 400 :y 50 :w 600 :h 900} 2000 1200 (dvd/outer-size iphone))
        layout (layout-in iphone frame)
        {:keys [x y size clip]} (pp/frame-box iphone frame)
        inside (dvd/layout iphone size size dvd/stage-padding nil true)]
    (t/is (mth/close? (+ (max (:w layout) (:h layout)) 48) size)
          "as large as the device is long and the room round it")
    (t/is (mth/close? (+ x (/ size 2)) (+ (:x layout) (/ (:w layout) 2)) 0.01) "about the middle of the device")
    (t/is (mth/close? (+ y (/ size 2)) (+ (:y layout) (/ (:h layout) 2)) 0.01))
    (t/is (mth/close? (:scale layout) (:scale inside) 0.001) "the view mode shows the device in it as large")
    (t/is (mth/close? (+ x (:x inside)) (:x layout) 0.5) "where the window has it")
    (t/is (mth/close? (+ y (:y inside)) (:y layout) 0.5))
    (t/is (neg? x) "wider than the window round a standing phone")
    (t/is (every? #(>= % 0) clip))
    (t/is (mth/close? (- x) (nth clip 3) 0.01) "cut to the window at its left")
    (t/is (pos? (nth clip 1)) "and its right")))

(t/deftest the-window-turns-with-its-device
  (let [outer  (dvd/outer-size iphone)
        before (pp/hugged-frame {:x 400 :y 50 :w 600 :h 900} 2000 1200 outer)
        after  (pp/turned-frame before 2000 1200 outer)
        scale  #(:scale (layout-in %1 %2))
        ;; the middle of the frame of the view mode, px of the browser
        ;; window, and its size (as long on its side but for a px or two:
        ;; the window holds it while it turns)
        box    (fn [device f]
                 (let [{:keys [x y size]} (pp/frame-box device f)]
                   [(+ (:x f) 1 x (/ size 2)) (+ (:y f) pp/bar-height 1 y (/ size 2)) size]))]
    (t/is (> (:w after) (:h after)) "wider than high")
    (t/is (< (:y before) (:y after)) "lower, its bar right above the device on its side")
    (t/is (mth/close? (+ (:x before) (/ (:w before) 2)) (+ (:x after) (/ (:w after) 2)) 1)
          "its middle kept")
    (t/is (every? true? (map #(mth/close? %1 %2 %3) (box iphone before) (box (dvd/turn iphone) after) [1 1 3]))
          "the frame of the view mode where it was: the device turns in it about its middle")
    (t/is (mth/close? (scale iphone before) (scale (dvd/turn iphone) after) 0.01) "the device as large")
    (t/is (hugs? (dvd/turn iphone) after) "the room round it on its side")
    (let [back (pp/turned-frame after 2000 1200 (dvd/outer-size (dvd/turn iphone)))]
      (t/is (mth/close? (:h before) (:h back) 2) "standing up again, as high")
      (t/is (mth/close? (:y before) (:y back) 2) "where it was"))
    (t/is (pp/turned? outer (dvd/outer-size (dvd/turn iphone))) "on its side")
    (t/is (pp/turned? (dvd/outer-size (dvd/turn iphone)) outer) "and up again")
    (t/is (not (pp/turned? outer outer)) "as it stood")
    (t/is (not (pp/turned? nil outer)) "the first it shows")
    (t/is (not (pp/turned? outer nil)) "without a device")))

(t/deftest a-corner-scales-the-device
  (let [outer (dvd/outer-size iphone)
        start (pp/hugged-frame {:x 400 :y 50 :w 600 :h 700} 2000 1200 outer)
        right (+ (:x start) (:w start))
        scale #(:scale (layout-in iphone %))]
    (let [frame (pp/scaled-frame (assoc start :w (+ (:w start) 100)) :bottom-right start outer)]
      (t/is (mth/close? (+ (:w start) 100) (:w frame) 1) "as wide as dragged")
      (t/is (> (:h frame) (:h start)) "higher with it")
      (t/is (= [(:x start) (:y start)] [(:x frame) (:y frame)]) "its top left kept")
      (t/is (hugs? iphone frame) "the room round the device"))
    (let [frame (pp/scaled-frame (assoc start :h (- (:h start) 200)) :bottom-right start outer)]
      (t/is (< (scale frame) (scale start)) "smaller as the drag goes up")
      (t/is (mth/close? (- (:h start) 200) (:h frame) 1) "as high as dragged"))
    (let [frame (pp/scaled-frame (-> start
                                     (update :x - 60)
                                     (update :w + 60)
                                     (update :h + 10))
                                 :bottom-left start outer)]
      (t/is (mth/close? (+ (:w start) 60) (:w frame) 1) "the larger change wins")
      (t/is (mth/close? right (+ (:x frame) (:w frame)) 1) "its right side kept"))
    (let [moved (assoc start :x 10 :y 20)]
      (t/is (= moved (pp/scaled-frame moved :move start outer)) "moved, as it is"))))

(t/deftest a-side-scales-the-device
  (let [outer  (dvd/outer-size iphone)
        start  (pp/hugged-frame {:x 400 :y 50 :w 600 :h 700} 2000 1200 outer)
        right  (+ (:x start) (:w start))
        bottom (+ (:y start) (:h start))
        middle (+ (:x start) (/ (:w start) 2))
        drag   (fn [mode dx dy] (pp/scaled-frame (cw/resized start mode dx dy) mode start outer))]
    (let [frame (drag :right 100 40)]
      (t/is (mth/close? (+ (:w start) 100) (:w frame) 1) "as wide as dragged, however far down")
      (t/is (= [(:x start) (:y start)] [(:x frame) (:y frame)]) "its top left kept")
      (t/is (hugs? iphone frame) "the room round the device"))
    (let [frame (drag :left 60 0)]
      (t/is (mth/close? (- (:w start) 60) (:w frame) 1) "narrower as the drag goes in")
      (t/is (mth/close? right (+ (:x frame) (:w frame)) 1) "its right side kept"))
    (let [frame (drag :top 30 -100)]
      (t/is (mth/close? (+ (:h start) 100) (:h frame) 1) "as high as dragged, however far across")
      (t/is (mth/close? bottom (+ (:y frame) (:h frame)) 1) "its bottom kept")
      (t/is (mth/close? middle (+ (:x frame) (/ (:w frame) 2)) 1) "its middle kept")
      (t/is (hugs? iphone frame) "the room round the device"))
    (let [frame (drag :top-left -100 -10)]
      (t/is (mth/close? right (+ (:x frame) (:w frame)) 1) "from a top corner, the right side kept")
      (t/is (mth/close? bottom (+ (:y frame) (:h frame)) 1) "and the bottom"))))

(defn- overlaps?
  "Whether the boxes `a` and `b` share more than an edge."
  [a b]
  (let [e 0.001]
    (and (< (+ (:x a) e) (+ (:x b) (:w b))) (< (+ (:x b) e) (+ (:x a) (:w a)))
         (< (+ (:y a) e) (+ (:y b) (:h b))) (< (+ (:y b) e) (+ (:y a) (:h a))))))

(t/deftest the-frame-of-the-device-scales-it-from-round-its-screen
  (doseq [[device frame] [[iphone (pp/hugged-frame {:x 400 :y 50 :w 600 :h 700} 2000 1200
                                                   (dvd/outer-size iphone))]
                          [(dvd/turn iphone) (pp/hugged-frame {:x 400 :y 50 :w 600 :h 400} 2000 1200
                                                              (dvd/outer-size (dvd/turn iphone)))]
                          [(dvd/resolve-device "macbook" 1280 720)
                           (pp/hugged-frame {:x 0 :y 0 :w 400 :h 700} 2000 1200
                                            (dvd/outer-size (dvd/resolve-device "macbook" 1280 720)))]]]
    (let [{:keys [x y w h screen]} (layout-in device frame)
          grips (into {} (pp/grips device frame))
          sides (map grips [:top :right :bottom :left])]
      (t/is (= #{:top :right :bottom :left :top-left :top-right :bottom-left :bottom-right}
               (set (keys grips)))
            "each side and each corner")
      (t/is (not-any? #(overlaps? screen %) sides) "none of its screen")
      (t/is (not-any? #(overlaps? (grips :left) %) [(grips :top) (grips :bottom)]) "one side at a time")
      (t/is (mth/close? (:x screen) (+ (:x (grips :left)) (:w (grips :left)))) "up to its screen")
      (t/is (mth/close? (+ (:y screen) (:h screen)) (:y (grips :bottom))) "below it")
      (t/is (= [(- x 8) (- y 8)] [(:x (grips :top-left)) (:y (grips :top-left))]) "a little out of it")
      (t/is (mth/close? (+ x w 8) (+ (:x (grips :bottom-right)) (:w (grips :bottom-right)))))
      (t/is (mth/close? (+ y h 8) (+ (:y (grips :bottom-right)) (:h (grips :bottom-right)))))
      (t/is (every? (fn [{gx :x gy :y gw :w gh :h}]
                      (and (<= 0 gx) (<= 0 gy)
                           (<= (+ gx gw) (- (:w frame) 2))
                           (<= (+ gy gh) (- (:h frame) pp/bar-height 2))))
                    (vals grips))
            "in the window"))))
