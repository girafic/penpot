;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.devices
  "The devices the view mode can show a board in (see
  `app.main.ui.viewer.device-frame`): the device a choice gives a board,
  its screen, what is drawn round it and where it goes in the view mode.
  A screen is as wide as the board and the device is scaled as a whole,
  so the zoom of the board is the scale of the device. Sizes are px of
  the board; a device that turns lies on its side for a board wider than
  high, the iPhone Duo opens for one."
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]))

(def kinds
  "The devices, in the order they claim a size they share (an iPhone
  before an Android phone of the same size, a browser before an iPad for
  a long page 1024 px wide)."
  [:iphone-island :iphone-notch :iphone-home :browser :ipad :watch :macbook :android
   :imac :iphone-duo])

(def choices
  "What a menu of the view mode picks, in its order: no device, the one
  the size of the board says, or one of `kinds`."
  ["none" "auto" "iphone-island" "iphone-notch" "iphone-home" "iphone-duo"
   "android" "ipad" "macbook" "watch" "imac" "browser"])

(def finishes
  "The colors of a device."
  ["dark" "light"])

(def stage-padding
  "Px the view mode leaves round a device, in its stage (see `layout`)."
  24)

(def preview-top
  "Px the preview of the workspace leaves above a device, under its bar:
  the device alone shows there, the bar right over it (see `layout`)."
  8)

(defn parse-choice
  "`choice` when it is one of `choices`, else \"none\"."
  [choice]
  (if (some #{choice} choices) choice "none"))

(defn parse-finish
  "`finish` when it is one of `finishes`, else \"dark\"."
  [finish]
  (if (some #{finish} finishes) finish "dark"))

(def ^:private specs
  "Each of `kinds`: its screen's height over its width as it stands
  (`:ratio`), whether it turns for a board wider than high, whether
  \"auto\" picks it for a board of one of its `:sizes`, whether a taller
  board as wide as one of its sizes, or of its `:pages`, scrolls in it
  then (`:tall`), and the short side of the screen its drawing is
  measured on (`:ref`). The iPhone Duo opens for a board wider than
  high, to a screen of `:open-ratio` measured on `:open-ref`. A browser
  shows a board whole up to as high as wide or `:window` px, a longer
  one in a window at least that high, its toolbar as large at any size
  (no `:ref`). One with a `:touch` screen scrolls when dragged."
  {:iphone-island {:ratio (/ 874 402) :turns true :auto true :tall true :ref 402 :touch true
                   :sizes [[393 852] [402 874] [430 932] [440 956]]}
   :iphone-notch  {:ratio (/ 844 390) :turns true :auto true :tall true :ref 390 :touch true
                   :sizes [[360 780] [375 812] [390 844] [414 896] [428 926]]}
   :iphone-home   {:ratio (/ 667 375) :turns true :auto true :tall true :ref 375 :touch true
                   :sizes [[320 568] [375 667] [414 736]]}
   :ipad          {:ratio (/ 1194 834) :turns true :auto true :tall true :ref 834 :touch true
                   :sizes [[744 1133] [768 1024] [820 1180] [834 1112] [834 1194]
                           [1024 1366] [1027 1366]]}
   :watch         {:ratio (/ 496 416) :turns false :auto true :ref 416 :touch true
                   :sizes [[272 340] [312 390] [324 394] [352 430] [368 448]
                           [396 484] [416 496]]}
   :macbook       {:ratio (/ 982 1512) :turns false :auto true :ref 982
                   :sizes [[1280 832] [1512 982] [1728 1117]]}
   :android       {:ratio (/ 915 412) :turns true :auto true :tall true :ref 412 :touch true
                   :sizes [[360 640] [360 780] [360 800] [384 854] [393 851]
                           [412 892] [412 914] [412 915] [412 917]]}
   :imac          {:ratio (/ 9 16) :turns false :auto false :ref 1260
                   :sizes []}
   ;; the foldable of the demo: a cover 270 × 396, open 568 × 400
   :iphone-duo    {:ratio (/ 396 270) :turns false :auto false :ref 270 :touch true
                   :open-ratio (/ 400 568) :open-ref 400
                   :sizes []}
   ;; the web presets of the workspace, less the size of a slide
   :browser       {:ratio (/ 800 1280) :turns false :auto true :tall true :window 1024
                   :sizes [[1280 800] [1366 768] [1440 1024]]
                   :pages [[1024 768] [1280 800] [1366 768] [1440 1024] [1920 1080]]}})

(defn touch?
  "Whether the device `kind` has a touch screen: one that scrolls when
  dragged, as a finger does."
  [kind]
  (true? (get-in specs [kind :touch])))

(defn turns?
  "Whether the device `kind` turns on its side: a phone or a tablet."
  [kind]
  (true? (get-in specs [kind :turns])))

(defn- pages
  "The sizes a board taller than one as wide scrolls in, of `spec`."
  [spec]
  (or (:pages spec) (:sizes spec)))

(defn- size-of?
  "Whether a board `bw` × `bh` is the size `[w h]`, or that size turned
  for a device that `turns`."
  [[w h] bw bh turns]
  (or (and (= w bw) (= h bh))
      (and turns (= h bw) (= w bh))))

(defn- auto-device
  "The device a board `bw` × `bh` is the size of, else the phone,
  tablet or browser as wide as it that it scrolls in, else nil."
  [bw bh]
  (or (some (fn [kind]
              (let [{:keys [auto turns sizes]} (get specs kind)]
                (when (and auto (some #(size-of? % bw bh turns) sizes))
                  {:kind kind
                   :landscape? (and turns (> bw bh))
                   :screen-w bw
                   :screen-h bh})))
            kinds)
      (when (> bh bw)
        (some (fn [kind]
                (let [spec (get specs kind)]
                  (when-let [[_ h] (and (:tall spec)
                                        (d/seek (fn [[w h]] (and (= w bw) (< h bh))) (pages spec)))]
                    {:kind kind :landscape? false :screen-w bw :screen-h h})))
              kinds))))

(defn open?
  "Whether the device `kind` opens for a screen `sw` × `sh`: the iPhone
  Duo, for one wider than high."
  [kind sw sh]
  (and (some? (get-in specs [kind :open-ratio])) (> sw sh)))

(defn- device-of
  "The device `kind` round a board `bw` × `bh`: as high as the board
  when the board is of its proportions (one of its sizes, or within 8 %;
  for a browser, no higher than its window), else as high as the size of
  it as wide as the board nearest its proportions, else as high as the
  device then is."
  [kind bw bh]
  (let [{:keys [ratio open-ratio turns sizes window] :as spec} (get specs kind)
        landscape? (and turns (> bw bh))
        ratio      (cond
                     landscape?          (/ 1 ratio)
                     (open? kind bw bh)  open-ratio
                     :else               ratio)
        fits?      (or (some #(size-of? % bw bh turns) sizes)
                       (if (some? window)
                         (<= bh (max bw window))
                         (<= (mth/abs (- (/ bh bw) ratio)) (* 0.08 ratio))))
        page       (when-not landscape?
                     (when-let [same-w (seq (filter #(= bw (first %)) (pages spec)))]
                       (apply min-key #(mth/abs (- (second %) (* bw ratio))) same-w)))]
    {:kind kind
     :landscape? landscape?
     :screen-w bw
     :screen-h (cond
                 fits?       bh
                 (some? page) (second page)
                 :else       (cond-> (mth/round (* bw ratio))
                               (some? window) (max window)))}))

(defn resolve-device
  "The device `choice` (one of `choices`) shows a board `bw` × `bh` in:
  `{:kind :landscape? :screen-w :screen-h}`, a screen as wide as the
  board; nil for none. The board scrolls in a screen lower than it."
  [choice bw bh]
  (let [bw (some-> bw mth/round)
        bh (some-> bh mth/round)]
    (when (and (number? bw) (number? bh) (pos? bw) (pos? bh))
      (case choice
        "auto" (auto-device bw bh)
        (when-let [kind (some #{(keyword choice)} kinds)]
          (device-of kind bw bh))))))

(defn first-screen-h
  "How high the first screen of a board `bw` × `bh` is, where its layers
  fixed when scrolling sit: the screen of the device \"auto\" gives it,
  else all of it."
  [bw bh]
  (min bh (or (:screen-h (resolve-device "auto" bw bh)) bh)))

(defn turn
  "`device` (see `resolve-device`) turned a quarter: on its side, or
  standing up again, its screen as wide as it was high."
  [{:keys [landscape? screen-w screen-h] :as device}]
  (assoc device
         :landscape? (not landscape?)
         :screen-w screen-h
         :screen-h screen-w))

;; --- What is drawn

(defn- rect
  [x y w h r role]
  {:type :rect :x x :y y :w w :h h :r r :role role})

(defn- circle
  [cx cy r role]
  {:type :circle :cx cx :cy cy :r r :role role})

(defn- label
  "A line of text centered on `cx` `cy`, `size` px high, at most `max-w`
  wide: the name of the board, filled in where it is drawn."
  [cx cy size max-w role]
  {:type :label :x cx :y cy :size size :max-w max-w :role role})

(defn- slab
  "A phone, tablet or watch round a screen `sw` × `sh`: a band of
  `band` round a glass face, the screen `insets` from the band's edge
  `[top right bottom left]`, buttons out of its sides as `[side at
  length]` and `margin` round it for them; px of the reference device
  times `k`."
  [sw sh k {:keys [insets band radius body-radius face margin buttons]
            :or {face :glass margin 3}}]
  (let [[top right bottom left] (mapv #(* % k) insets)
        m      (* margin k)
        band   (* band k)
        radius (* radius k)
        body-r (if (some? body-radius) (* body-radius k) (+ radius top))
        bw     (+ sw left right)
        bh     (+ sh top bottom)
        out    (* 3 k)
        button (fn [[side at length]]
                 (let [at (* at k) length (* length k)]
                   (case side
                     :left  (rect (- m out) (+ m at) (+ out k) length k :button)
                     :right  (rect (+ m bw (- k)) (+ m at) (+ out k) length k :button)
                     :top    (rect (+ m at) (- m out) length (+ out k) k :button)
                     :bottom (rect (+ m at) (+ m bh (- k)) length (+ out k) k :button))))]
    {:w (+ bw (* 2 m))
     :h (+ bh (* 2 m))
     :body {:x m :y m :w bw :h bh}
     :screen {:x (+ m left) :y (+ m top) :w sw :h sh :radii [radius radius radius radius]}
     :parts (-> (mapv button buttons)
                (conj (rect m m bw bh body-r :band)
                      (rect (+ m band) (+ m band) (- bw (* 2 band)) (- bh (* 2 band))
                            (- body-r band) face)))
     :cutouts []}))

(defn- notch
  "A notch `w` × `h` hanging from the top of `screen`, round below."
  [{:keys [x y] sw :w} w h r]
  (let [nx (+ x (/ (- sw w) 2))]
    [(rect nx (dec y) w (inc h) r :cutout)
     (rect nx (dec y) w (/ h 2) 0 :cutout)]))

(defn- laptop
  "A MacBook round a screen `sw` × `sh`: the lid with its notch, the base
  below, wider."
  [sw sh k]
  (let [[top side bottom] (mapv #(* % k) [14 14 24])
        band   (* 4 k)
        lid-w  (+ sw (* 2 side))
        lid-h  (+ sh top bottom)
        lid-r  (* 22 k)
        base-w (* lid-w 1.14)
        base-h (* 20 k)
        lap    (* 2 k)
        w      base-w
        lx     (/ (- w lid-w) 2)
        screen {:x (+ lx side) :y top :w sw :h sh
                :radii (mapv #(* % k) [12 12 0 0])}]
    {:w w
     :h (+ lid-h base-h (- lap))
     :body {:x lx :y 0 :w lid-w :h lid-h}
     :screen screen
     :parts [(rect lx 0 lid-w lid-h lid-r :band)
             (rect (+ lx band) band (- lid-w (* 2 band)) (- lid-h (* 2 band)) (- lid-r band) :glass)
             (rect 0 (- lid-h lap) base-w base-h (* 10 k) :base)
             (rect (/ (- w (* 150 k)) 2) (- lid-h lap) (* 150 k) (* 7 k) (* 4 k) :base-dark)]
     :cutouts (notch screen (* 190 k) (* 32 k) (* 10 k))}))

(defn- desktop
  "An iMac round a screen `sw` × `sh`: the front round the screen, the
  chin below it, on a stand."
  [sw sh k]
  (let [[top side bottom chin] (mapv #(* % k) [36 36 36 234])
        r       (* 24 k)
        body-w  (+ sw (* 2 side))
        front-h (+ top sh bottom)
        body-h  (+ front-h chin)
        neck-w  (* body-w 0.2)
        neck-h  (* 300 k)
        foot-w  (* body-w 0.32)
        foot-h  (* 30 k)]
    {:w body-w
     :h (+ body-h neck-h foot-h)
     :body {:x 0 :y 0 :w body-w :h body-h}
     :screen {:x side :y top :w sw :h sh :radii [0 0 0 0]}
     :parts [(rect (/ (- body-w neck-w) 2) (- body-h (* 4 k)) neck-w (+ neck-h (* 4 k)) 0 :stand)
             (rect (/ (- body-w foot-w) 2) (+ body-h neck-h) foot-w foot-h (* 10 k) :stand)
             (rect 0 0 body-w body-h r :chin)
             (rect 0 0 body-w front-h r :front)
             (rect 0 r body-w (- front-h r) 0 :front)]
     :cutouts []}))

(defn- foldable
  "The iPhone Duo round a screen `sw` × `sh`: closed, its cover with the
  island; open, two halves round one screen with the crease between
  them and a camera in the right one."
  [sw sh k]
  (if (> sw sh)
    (let [g (slab sw sh k {:insets [10 10 10 10] :band 3 :radius 28
                           :buttons [[:top 287 14] [:bottom 287 14] [:top 470 36]
                                     [:top 512 36] [:right 104 58]]})
          {:keys [x y w]} (:screen g)
          mid (+ x (/ w 2))]
      (update g :cutouts conj
              (rect (- mid k) y (* 2 k) sh 0 :crease)
              (circle (+ x w (* -16 k)) (+ y (* 12 k)) (* 4.5 k) :cutout)))
    (let [g (slab sw sh k {:insets [12 12 12 12] :band 3 :radius 26
                           :buttons [[:top 176 36] [:top 218 36] [:right 104 58]]})
          {:keys [x y w]} (:screen g)
          iw (* 84 k)
          ih (* 25 k)]
      (update g :cutouts conj
              (rect (+ x (/ (- w iw) 2)) (+ y (* 7 k)) iw ih (/ ih 2) :cutout)))))

(defn- window
  "A browser window round a page `sw` × `sh`: its toolbar above with the
  three lights and the address field, which shows the name of the board;
  as large at any size, as on a screen."
  [sw sh]
  (let [bar     52
        border  1
        r       10
        w       (+ sw (* 2 border))
        h       (+ sh bar border)
        mid     (/ bar 2)
        field-w (-> (* w 0.42) (max 120) (min 640))
        field-h 30
        field-x (/ (- w field-w) 2)]
    {:w w
     :h h
     :body {:x 0 :y 0 :w w :h h}
     :screen {:x border :y bar :w sw :h sh :radii [0 0 (- r border) (- r border)]}
     :parts [(rect 0 0 w h r :toolbar)
             (rect border (dec bar) sw 1 0 :rule)
             (circle 20 mid 6 :close)
             (circle 40 mid 6 :minimize)
             (circle 60 mid 6 :zoom)
             (rect field-x (- mid (/ field-h 2)) field-w field-h 8 :field)
             (label (/ w 2) mid 13 (- field-w 24) :title)]
     :cutouts []}))

(defn geometry
  "What is drawn of the device `kind` round a screen `sw` × `sh` as it
  stands (not turned): `{:w :h :body :screen :parts :cutouts}`, px of
  the board from its top left. `:parts` are drawn below the screen,
  `:cutouts` over it; each is a `:rect`, a `:circle` or a `:label` with
  a `:role`."
  [kind sw sh]
  (let [{:keys [ref open-ref]} (get specs kind)
        ref (if (open? kind sw sh) open-ref ref)
        k   (if (some? ref) (/ (min sw sh) ref) 1)
        cx  (fn [{:keys [screen]}] (+ (:x screen) (/ (:w screen) 2)))]
    (case kind
      :iphone-island
      (let [g (slab sw sh k {:insets [14 14 14 14] :band 4 :radius 55
                             :buttons [[:left 120 32] [:left 190 62] [:left 268 62]
                                       [:right 220 100]]})
            w (* 126 k)
            h (* 37 k)]
        (update g :cutouts conj
                (rect (- (cx g) (/ w 2)) (+ (get-in g [:screen :y]) (* 11 k)) w h (/ h 2) :cutout)))

      :iphone-notch
      (let [g (slab sw sh k {:insets [14 14 14 14] :band 4 :radius 47
                             :buttons [[:left 110 26] [:left 170 56] [:left 240 56]
                                       [:right 200 90]]})]
        (assoc g :cutouts (notch (:screen g) (* 162 k) (* 32 k) (* 20 k))))

      :iphone-home
      (let [g   (slab sw sh k {:insets [85 12 85 12] :band 4 :radius 0 :body-radius 56
                               :face :face
                               :buttons [[:left 90 26] [:left 150 50] [:left 210 50]
                                         [:right 150 60]]})
            mid (+ (get-in g [:body :y]) (* 42.5 k))
            low (+ (get-in g [:screen :y]) sh (* 42.5 k))]
        (update g :parts conj
                (rect (- (cx g) (* 30 k)) (- mid (* 3 k)) (* 60 k) (* 6 k) (* 3 k) :lens)
                (circle (- (cx g) (* 50 k)) mid (* 5 k) :lens)
                (circle (cx g) low (* 26 k) :ring)))

      :android
      (let [g (slab sw sh k {:insets [10 10 10 10] :band 3 :radius 36
                             :buttons [[:right 220 60] [:right 320 110]]})]
        (update g :cutouts conj
                (circle (cx g) (+ (get-in g [:screen :y]) (* 28 k)) (* 12 k) :cutout)))

      :ipad
      (let [g (slab sw sh k {:insets [24 24 24 24] :band 4 :radius 18
                             :buttons [[:top 640 50] [:right 90 50] [:right 150 50]]})]
        (update g :parts conj
                (circle (cx g) (+ (get-in g [:body :y]) (* 12 k)) (* 4 k) :lens)))

      :watch
      (let [g  (slab sw sh k {:insets [22 22 22 22] :band 10 :radius 92 :margin 14})
            {:keys [x y w h]} (:body g)]
        (update g :parts (fn [parts]
                           (into [(rect (+ x w (* -3 k)) (+ y (* 0.24 h)) (* 15 k) (* 52 k) (* 6 k) :button)
                                  (rect (+ x w (* -3 k)) (+ y (* 0.55 h)) (* 9 k) (* 84 k) (* 4 k) :button)]
                                 parts))))

      :macbook    (laptop sw sh k)
      :imac       (desktop sw sh k)
      :iphone-duo (foldable sw sh k)
      :browser    (window sw sh))))

(defn- standing
  "`device` as it stands, the screen `[w h]` of its drawing."
  [{:keys [landscape? screen-w screen-h]}]
  (if landscape? [screen-h screen-w] [screen-w screen-h]))

(defn outer-size
  "The size `[w h]` of `device` (see `resolve-device`) at its own size,
  turned when it lies on its side: what a window shows of it."
  [{:keys [kind landscape?] :as device}]
  (let [[sw sh] (standing device)
        {:keys [w h]} (geometry kind sw sh)]
    (if landscape? [h w] [w h])))

(defn layout
  "Where `device` (see `resolve-device`) goes in a stage `stage-w` ×
  `stage-h` px with `padding` round it, as large as fits, in the middle;
  with `top`, that many px from the top of the stage instead, the room
  left below it: `{:x :y :w :h :scale :view-w :view-h :landscape?
  :geometry :screen}`, the device drawn from `:geometry` as it stands
  in a box `:view-w` × `:view-h` (turned a quarter to the left when
  `:landscape?`), and its screen `{:x :y :w :h :radii}` in px of the
  stage. The screen is a whole number of px wide: the zoom of the board
  is `:scale`."
  ([device stage-w stage-h padding]
   (layout device stage-w stage-h padding nil))
  ([{:keys [kind landscape?] :as device} stage-w stage-h padding top]
   (let [[nw nh]  (standing device)
         g        (geometry kind nw nh)
         [vw vh]  (if landscape? [(:h g) (:w g)] [(:w g) (:h g)])
         {:keys [x y w h radii]} (:screen g)
         ;; turned a quarter to the left, (x, y) goes to (y, width - x)
         [sx sy sw sh] (if landscape? [y (- (:w g) x w) h w] [x y w h])
         radii    (if landscape?
                    (let [[tl tr br bl] radii] [tr br bl tl])
                    radii)
         fit      (min (/ (max 1 (- stage-w (* 2 padding))) vw)
                       (/ (max 1 (- stage-h padding (or top padding))) vh))
         screen-w (max 1 (mth/floor (* sw fit)))
         scale    (/ screen-w sw)
         ox       (/ (- stage-w (* vw scale)) 2)
         oy       (if (some? top) top (/ (- stage-h (* vh scale)) 2))]
     {:x ox
      :y oy
      :w (* vw scale)
      :h (* vh scale)
      :scale scale
      :view-w vw
      :view-h vh
      :landscape? (boolean landscape?)
      :geometry g
      :screen {:x (+ ox (* sx scale))
               :y (+ oy (* sy scale))
               :w screen-w
               :h (* sh scale)
               :radii (mapv #(* % scale) radii)}})))
