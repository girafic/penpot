;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-devices-test
  "The devices of the view mode: the one a choice gives a board, its
  screen, and where it goes in the view mode."
  (:require
   [app.common.math :as mth]
   [app.main.constants :as cst]
   [app.main.data.viewer.devices :as dvd]
   [cljs.test :as t :include-macros true]))

(defn- presets
  "The size presets of the group `group` of the workspace."
  [group]
  (->> cst/size-presets
       (drop-while #(not= group (:name %)))
       (rest)
       (take-while :width)))

(t/deftest automatic-frames-the-device-sizes-of-the-presets
  (doseq [{:keys [name width height]} (presets "APPLE")]
    (t/is (some? (dvd/resolve-device "auto" width height)) name))
  (doseq [{:keys [name width height]} (presets "ANDROID")
          :when (not (#{"Expanded" "Medium"} name))]
    (t/is (some? (dvd/resolve-device "auto" width height)) name))
  (t/is (= :iphone-island (:kind (dvd/resolve-device "auto" 402 874))))
  (t/is (= :iphone-notch (:kind (dvd/resolve-device "auto" 390 844))))
  (t/is (= :iphone-home (:kind (dvd/resolve-device "auto" 320 568))))
  (t/is (= :ipad (:kind (dvd/resolve-device "auto" 768 1024))) "the Android tablet preset too")
  (t/is (= :watch (:kind (dvd/resolve-device "auto" 416 496))))
  (t/is (= :macbook (:kind (dvd/resolve-device "auto" 1512 982))))
  (t/is (= :android (:kind (dvd/resolve-device "auto" 412 915))))
  (t/is (= :iphone-notch (:kind (dvd/resolve-device "auto" 360 780)))
        "an iPhone before an Android phone of the same size"))

(t/deftest automatic-leaves-other-boards-alone
  (doseq [[w h] [[1920 1080] [1027 768] [1280 720] [1080 1350] [400 400] [320 320]
                 [700 840] [568 400]]]
    (t/is (nil? (dvd/resolve-device "auto" w h)) (str w "x" h)))
  (t/is (nil? (dvd/resolve-device "auto" 1920 1080)) "never the iMac, nor a browser for a slide"))

(t/deftest automatic-shows-a-web-board-in-a-browser
  (doseq [[w h] [[1280 800] [1366 768] [1440 1024]]]
    (t/is (= {:kind :browser :landscape? false :screen-w w :screen-h h}
             (dvd/resolve-device "auto" w h))
          (str w "x" h)))
  (t/is (= {:kind :browser :landscape? false :screen-w 1440 :screen-h 1024}
           (dvd/resolve-device "auto" 1440 6000))
        "a long page scrolls in it")
  (t/is (= 1080 (:screen-h (dvd/resolve-device "auto" 1920 4000))) "as wide as a slide too")
  (t/is (= :browser (:kind (dvd/resolve-device "auto" 1024 3000))) "before an iPad")
  (t/is (= :ipad (:kind (dvd/resolve-device "auto" 1024 1366))) "an iPad of its size"))

(t/deftest a-board-matches-its-device-exactly
  (t/is (= {:kind :iphone-island :landscape? false :screen-w 402 :screen-h 874}
           (dvd/resolve-device "auto" 402 874)))
  (t/is (= {:kind :iphone-island :landscape? true :screen-w 874 :screen-h 402}
           (dvd/resolve-device "auto" 874 402))
        "turned")
  (t/is (= {:kind :iphone-island :landscape? false :screen-w 402 :screen-h 874}
           (dvd/resolve-device "auto" 402.2 873.8))
        "a board a fraction off"))

(t/deftest a-long-phone-board-scrolls-in-its-phone
  (t/is (= {:kind :iphone-island :landscape? false :screen-w 393 :screen-h 852}
           (dvd/resolve-device "auto" 393 2400)))
  (t/is (= {:kind :ipad :landscape? false :screen-w 834 :screen-h 1112}
           (dvd/resolve-device "auto" 834 3000))
        "the first size of that width")
  (t/is (nil? (dvd/resolve-device "auto" 2400 393)) "not wide ones"))

(t/deftest a-device-picked-for-any-board
  (t/is (= {:kind :iphone-island :landscape? false :screen-w 375 :screen-h 812}
           (dvd/resolve-device "iphone-island" 375 812))
        "a board of its proportions fills it")
  (t/is (= {:kind :iphone-island :landscape? false :screen-w 375 :screen-h 815}
           (dvd/resolve-device "iphone-island" 375 2000))
        "a taller one scrolls in it")
  (t/is (= {:kind :iphone-island :landscape? false :screen-w 375 :screen-h 815}
           (dvd/resolve-device "iphone-island" 375 500))
        "a lower one sits at its top")
  (t/is (= {:kind :ipad :landscape? true :screen-w 1194 :screen-h 834}
           (dvd/resolve-device "ipad" 1194 834))
        "turned for a wide board")
  (t/is (false? (:landscape? (dvd/resolve-device "watch" 496 416))) "a watch never turns")
  (t/is (= {:kind :imac :landscape? false :screen-w 1920 :screen-h 1080}
           (dvd/resolve-device "imac" 1920 1080)))
  (t/is (= 1080 (:screen-h (dvd/resolve-device "imac" 1920 1200))) "16:9")
  (t/is (= 800 (:screen-h (dvd/resolve-device "android" 360 2000)))
        "the size as wide nearest its proportions")
  (t/is (nil? (dvd/resolve-device "none" 402 874)))
  (t/is (nil? (dvd/resolve-device "toaster" 402 874)))
  (t/is (nil? (dvd/resolve-device "auto" 0 874)))
  (t/is (nil? (dvd/resolve-device "auto" nil nil))))

(t/deftest the-phones-and-tablets-scroll-when-dragged
  (doseq [kind [:iphone-island :iphone-notch :iphone-home :iphone-duo :android :ipad :watch]]
    (t/is (true? (dvd/touch? kind)) (name kind)))
  (doseq [kind [:macbook :imac :browser :toaster]]
    (t/is (false? (dvd/touch? kind)) (name kind))))

(t/deftest the-choices-parse
  (t/is (= "ipad" (dvd/parse-choice "ipad")))
  (t/is (= "none" (dvd/parse-choice "toaster")))
  (t/is (= "none" (dvd/parse-choice nil)))
  (t/is (= "light" (dvd/parse-finish "light")))
  (t/is (= "dark" (dvd/parse-finish nil))))

(defn- inside?
  "Whether the rect `a` is inside the rect `b`."
  [a b]
  (and (<= (:x b) (+ (:x a) 0.001))
       (<= (:y b) (+ (:y a) 0.001))
       (<= (+ (:x a) (:w a)) (+ (:x b) (:w b) 0.001))
       (<= (+ (:y a) (:h a)) (+ (:y b) (:h b) 0.001))))

(t/deftest every-device-holds-its-screen
  (doseq [kind dvd/kinds
          [w h] [[402 874] [874 402] [1512 982] [416 496] [1920 1080]]
          :let [device (dvd/resolve-device (name kind) w h)
                {:keys [screen body] :as g}
                (apply dvd/geometry kind (if (:landscape? device)
                                           [(:screen-h device) (:screen-w device)]
                                           [(:screen-w device) (:screen-h device)]))]]
    (t/is (inside? screen body) (str kind " " w "x" h ": the screen in its body"))
    (t/is (inside? body {:x 0 :y 0 :w (:w g) :h (:h g)}) (str kind ": the body in the drawing"))
    (t/is (seq (:parts g)) (str kind ": something drawn"))))

(t/deftest a-device-fits-its-stage
  (let [device (dvd/resolve-device "iphone-island" 402 874)
        {:keys [x y w h scale screen view-w view-h]} (dvd/layout device 800 600 24)]
    (t/is (mth/close? (- 600 48) h 2) "as high as the stage less its padding")
    (t/is (<= h (- 600 48)))
    (t/is (<= w (- 800 48)))
    (t/is (mth/close? (/ (- 800 w) 2) x) "in the middle")
    (t/is (mth/close? (/ (- 600 h) 2) y))
    (t/is (mth/close? (* view-w scale) w))
    (t/is (mth/close? (* view-h scale) h))
    (t/is (= (mth/floor (:w screen)) (:w screen)) "a whole number of px wide")
    (t/is (mth/close? (* 402 scale) (:w screen)) "the zoom of the board is the scale")
    (t/is (mth/close? (* 874 scale) (:h screen)))
    (t/is (inside? screen {:x x :y y :w w :h h}) "the screen in the device")))

(t/deftest a-device-at-the-top-of-its-stage
  (let [device (dvd/resolve-device "iphone-island" 402 874)
        middle (dvd/layout device 600 2000 24)
        top    (dvd/layout device 600 2000 24 8)]
    (t/is (= 8 (:y top)) "the room left below it")
    (t/is (= (:scale middle) (:scale top)) "as large where the stage is as narrow")
    (t/is (= (:x middle) (:x top)) "in the middle across")
    (t/is (mth/close? (- (get-in top [:screen :y]) 8) (- (get-in middle [:screen :y]) (:y middle)))
          "its screen with it"))
  (let [device (dvd/resolve-device "iphone-island" 402 874)
        {:keys [y h]} (dvd/layout device 2000 600 24 8)]
    (t/is (= 8 y))
    (t/is (<= (+ y h) (- 600 24)) "as high as fits above the padding below it")
    (t/is (< (- 600 24 8 4) h) "nearly")))

(t/deftest a-device-on-its-side
  (let [device (dvd/resolve-device "iphone-island" 874 402)
        {:keys [x y w h scale screen landscape? view-w view-h]} (dvd/layout device 1200 800 24)
        [ow oh] (dvd/outer-size device)]
    (t/is (true? landscape?))
    (t/is (> view-w view-h) "wider than high")
    (t/is (= [ow oh] [view-w view-h]) "its size at its own size")
    (t/is (mth/close? (* 874 scale) (:w screen)) "the screen as wide as the board")
    (t/is (mth/close? (* 402 scale) (:h screen)))
    (t/is (inside? screen {:x x :y y :w w :h h}))))

(t/deftest the-corners-turn-with-the-device
  (let [device (dvd/resolve-device "macbook" 1512 982)
        radii  (get-in (dvd/layout device 1600 1100 0) [:screen :radii])]
    (t/is (= [0 0] (subvec radii 2)) "a MacBook's screen is square below")
    (t/is (every? pos? (subvec radii 0 2))))
  (let [g       (dvd/geometry :macbook 982 1512)
        turned  (dvd/layout {:kind :macbook :landscape? true :screen-w 1512 :screen-h 982} 1600 1100 0)
        [_ tr br _] (get-in g [:screen :radii])]
    (t/is (= [(pos? tr) (pos? br)]
             (mapv pos? (subvec (get-in turned [:screen :radii]) 0 2)))
          "turned a quarter to the left, its top right corner goes to the top left")))

(t/deftest a-browser-shows-a-board-up-to-its-window-whole
  (t/is (= {:kind :browser :landscape? false :screen-w 1600 :screen-h 900}
           (dvd/resolve-device "browser" 1600 900)))
  (t/is (= 1600 (:screen-h (dvd/resolve-device "browser" 1600 1600))) "as high as wide")
  (t/is (= 874 (:screen-h (dvd/resolve-device "browser" 402 874))) "a phone board")
  (t/is (= 1024 (:screen-h (dvd/resolve-device "browser" 1440 5000)))
        "a long page in the height of its preset")
  (t/is (= 1024 (:screen-h (dvd/resolve-device "browser" 402 3000))) "else at least 1024")
  (t/is (= 1600 (:screen-h (dvd/resolve-device "browser" 2560 8000))) "or 16:10")
  (let [toolbar #(get-in (dvd/geometry :browser %1 %2) [:screen :y])]
    (t/is (= 52 (toolbar 1920 1080) (toolbar 390 244)) "its toolbar as large at any size"))
  (t/is (some #(= :label (:type %)) (:parts (dvd/geometry :browser 1440 900)))
        "the name of the board in its field"))

(t/deftest the-iphone-duo-opens-for-a-wide-board
  (t/is (= {:kind :iphone-duo :landscape? false :screen-w 270 :screen-h 396}
           (dvd/resolve-device "iphone-duo" 270 396))
        "closed, the cover")
  (t/is (= {:kind :iphone-duo :landscape? false :screen-w 568 :screen-h 400}
           (dvd/resolve-device "iphone-duo" 568 400))
        "open")
  (t/is (= 901 (:screen-h (dvd/resolve-device "iphone-duo" 1280 720))) "open, a slide at its top")
  (t/is (= 440 (:screen-h (dvd/resolve-device "iphone-duo" 300 2000))) "a long board scrolls")
  (let [roles   #(set (map :role (:cutouts %)))
        open    (dvd/geometry :iphone-duo 568 400)
        closed  (dvd/geometry :iphone-duo 270 396)]
    (t/is (contains? (roles open) :crease) "the crease over the open screen")
    (t/is (not (contains? (roles closed) :crease)))
    (t/is (> (:w open) (* 1.9 (:w closed))) "two halves")
    (t/is (true? (dvd/open? :iphone-duo 568 400)))
    (t/is (false? (dvd/open? :ipad 1194 834)))))
