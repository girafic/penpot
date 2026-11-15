;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.phone-sheet
  "On a phone the view mode shows its board alone, no bars over it: two
  fingers held on it open a sheet from the bottom with what the bars hold
  elsewhere, the boards, the animation, the pages, the flows, how the
  interactions show, the sections and share; and the page as a whole
  instead of a board, its canvas (see `app.main.ui.viewer.canvas`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.types.page :as ctp]
   [app.main.data.comments :as dcm]
   [app.main.data.modal :as modal]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.canvas :as dvc]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.ds.controls.radio-buttons :refer [radio-buttons*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.main.ui.ds.layout.menu :refer [menu* menu-item* menu-separator*]]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.icons :as deprecated-icon]
   [app.main.ui.viewer.drag-scroll :as dds]
   [app.main.ui.viewer.header :as header]
   [app.util.dom :as dom]
   [app.util.globals :as g]
   [app.util.i18n :refer [tr]]
   [app.util.timers :as ts]
   [rumext.v2 :as mf]))

(def hold-ms
  "How long two fingers rest on the board to open the sheet."
  500)

(def ^:private hold-slop-px
  "How far a finger of a hold may move: further, it pinches or scrolls."
  10)

(def ^:private close-drag-px
  "How far down its lowest height the sheet is dragged to close."
  80)

(def ^:private flick-speed
  "How fast, px/ms, a drag of the grip let go goes on to the next height
  of the sheet the way it went."
  0.5)

(def ^:private most-of-screen
  "How high a sheet grows, of the screen: as `.full` and `.grown`."
  0.9)

(def ^:private little-of-screen
  "How low a sheet shrinks, of the screen, about as high as its grip and
  the top of what it holds, unless it marks that top (see `head-height`):
  as `.small`."
  0.1)

(def ^:private docked-of-screen
  "How high a docked sheet starts, of the screen: as `.start`."
  0.3)

(defn sheet-heights
  "The heights, px, a sheet rests at on a screen `vh` px high, lowest
  first, `[[rest px]...]`: `:small`, `:low` and `:high`, a little of the
  screen, half of it and most of it for a sheet as high as `:half`; else
  as high as what it holds, `natural` px, up to them, which leaves out
  those it is not as high as. A `docked` sheet rests where it starts
  too, `:start`, between the little and the half. `small`, px, the
  little one: as high as its head (see `head-height`)."
  [height vh natural & {:keys [docked small]}]
  (let [small (or small (* little-of-screen vh))
        start (* docked-of-screen vh)
        half  (* 0.5 vh)
        most  (* most-of-screen vh)
        low   (if (= height :half) half (min half natural))
        high  (if (= height :half) most (min most natural))]
    (cond-> []
      (< small low)                     (conj [:small small])
      (and (true? docked) (< start low)) (conj [:start start])
      :always                           (conj [:low low])
      (> high low)                      (conj [:high high]))))

(defn sheet-rest
  "Where a sheet let go rests, dragged by its grip to `h` px high going
  `v` px/ms (down more than 0), among its `heights` (see
  `sheet-heights`): the nearest, or the next one it was flicked to, or
  `:closed` once dragged further than `close-drag-px` below the lowest,
  or flicked down below it."
  [h v heights]
  (let [[_ lowest] (first heights)
        above      (filter (fn [[_ px]] (> px h)) heights)
        below      (filter (fn [[_ px]] (< px h)) heights)
        nearest    (apply min-key (fn [[_ px]] (mth/abs (- px h))) heights)]
    (first
     (cond
       (or (< h (- lowest close-drag-px))
           (and (< h lowest) (> v flick-speed)))
       [:closed]

       (< v (- flick-speed)) (or (first above) (last heights))
       (> v flick-speed)     (or (last below) (first heights))
       :else                 nearest))))

(defn- head-height
  "How high the sheet `node` is down to the end of its head, the last of
  what it holds marked `data-sheet-head`, and the room below what it
  holds; nil for none marked. Its position in the sheet, whether the sheet
  is dragged or what it holds scrolled."
  [^js node]
  (when-let [^js body (some-> node .-lastChild)]
    (let [heads (.querySelectorAll body "[data-sheet-head]")
          n     (.-length heads)]
      (when (pos? n)
        (let [^js head (.item heads (dec n))]
          (mth/ceil (+ (.-offsetTop head)
                       (.-offsetHeight head)
                       (js/parseFloat (.-paddingBottom (.. body -ownerDocument -defaultView
                                                           (getComputedStyle body)))))))))))

(defn- drag-style
  "How a sheet dragged to `h` px high shows among its `heights`: as high,
  or, below the lowest, as that and moved down."
  [h heights]
  (let [[_ lowest]  (first heights)
        [_ highest] (peek heights)]
    {:block-size (dm/str (mth/clamp h lowest highest) "px")
     :transform  (when (< h lowest) (dm/str "translateY(" (- lowest h) "px)"))}))

(def hint-ms
  "How long the note of how the sheet opens shows."
  5000)

(defn- touch-points
  "`{identifier [x y]}` of the touches of `touches`."
  [^js touches]
  (into {} (map (fn [^js t] [(.-identifier t) [(.-clientX t) (.-clientY t)]])) (array-seq touches)))

(defn held-still?
  "Whether the touches `now` are the two fingers of `start`, each no
  further from where it came down than `hold-slop-px`."
  [start now]
  (and (= 2 (count now))
       (= (set (keys start)) (set (keys now)))
       (every? (fn [[id [x0 y0]]]
                 (let [[x y] (get now id)]
                   (<= (js/Math.hypot (- x x0) (- y y0)) hold-slop-px)))
               start)))

(defn use-two-finger-hold!
  "Calls `on-hold` once two fingers rested `hold-ms` on the page, while
  `enabled?`: in any section of the view mode, which on a phone has no
  bars to go to another by."
  [enabled? on-hold]
  (let [on-hold-ref (mf/use-ref on-hold)]
    (mf/set-ref-val! on-hold-ref on-hold)
    (mf/with-effect [enabled?]
      (when enabled?
        (let [target g/window
              hold*  (volatile! nil)

              cancel!
              (fn []
                (some-> @hold* :timer js/clearTimeout)
                (vreset! hold* nil))

              on-start
              (fn [^js event]
                (cancel!)
                (when (= 2 (.. event -touches -length))
                  (let [start (touch-points (.-touches event))]
                    (vreset! hold* {:start start
                                    :timer (js/setTimeout
                                            ;; on the next frame: the moves the
                                            ;; page was too busy to take come
                                            ;; before it, and a pinch cancels
                                            (fn []
                                              (ts/raf
                                               (fn []
                                                 (when (= start (:start @hold*))
                                                   (vreset! hold* nil)
                                                   ((mf/ref-val on-hold-ref))))))
                                            hold-ms)}))))

              on-move
              (fn [^js event]
                (when-let [{:keys [start]} @hold*]
                  (when-not (held-still? start (touch-points (.-touches event)))
                    (cancel!))))]

          (.addEventListener target "touchstart" on-start #js {:passive true})
          (.addEventListener target "touchmove" on-move #js {:passive true})
          (.addEventListener target "touchend" cancel! #js {:passive true})
          (.addEventListener target "touchcancel" cancel! #js {:passive true})
          (fn []
            (.removeEventListener target "touchstart" on-start #js {:passive true})
            (.removeEventListener target "touchmove" on-move #js {:passive true})
            (.removeEventListener target "touchend" cancel! #js {:passive true})
            (.removeEventListener target "touchcancel" cancel! #js {:passive true})
            (cancel!)))))))

(defn sheet-key?
  "Whether the key `event` opens or closes the sheet on a desktop that
  shows the view mode of a phone (the device mode of the developer tools
  of a browser), which has no two fingers to hold: Alt+O, not in a field."
  [^js event]
  (let [target (.-target event)]
    (and (.-altKey event)
         (not (.-metaKey event))
         (not (.-ctrlKey event))
         (= "KeyO" (.-code event))
         (not (contains? #{"INPUT" "TEXTAREA" "SELECT"} (some-> target .-tagName)))
         (not (some-> target .-isContentEditable)))))

(defn use-sheet-key!
  "Calls `on-key` at the key that opens or closes the sheet (see
  `sheet-key?`) while `enabled?`."
  [enabled? on-key]
  (let [on-key-ref (mf/use-ref on-key)]
    (mf/set-ref-val! on-key-ref on-key)
    (mf/with-effect [enabled?]
      (when enabled?
        (let [on-keydown
              (fn [^js event]
                (when (sheet-key? event)
                  (.preventDefault event)
                  ((mf/ref-val on-key-ref))))]
          (.addEventListener g/window "keydown" on-keydown)
          (fn []
            (.removeEventListener g/window "keydown" on-keydown)))))))

(defn use-keyboard-inset!
  "Keeps on the page, while `enabled?`, how high the keyboard of a phone
  covers its bottom, as `--keyboard-inset`, and how high the part of the
  screen above it is, as `--keyboard-room`: what docks at the bottom of
  the screen stays above it, and no higher than that part. The page keeps
  its height as the keyboard comes, the part of it that shows shrinks
  (`visualViewport`)."
  [enabled?]
  (mf/with-effect [enabled?]
    (when-let [^js view (when enabled? (unchecked-get g/window "visualViewport"))]
      (let [root (.-documentElement g/document)

            update!
            (fn []
              (let [inset (max 0 (- (.-clientHeight root) (.-height view) (.-offsetTop view)))]
                (.setProperty (.-style root) "--keyboard-inset" (dm/str inset "px"))
                (.setProperty (.-style root) "--keyboard-room" (dm/str (.-height view) "px"))))]

        (update!)
        (.addEventListener view "resize" update!)
        (.addEventListener view "scroll" update!)
        (fn []
          (.removeEventListener view "resize" update!)
          (.removeEventListener view "scroll" update!)
          (.removeProperty (.-style root) "--keyboard-inset")
          (.removeProperty (.-style root) "--keyboard-room"))))))

(mf/defc sheet*
  "A sheet from the bottom of a phone named `label`: `children` scroll
  under its grip. It reaches at most half the screen, or always as high
  as `height` says, `:half`. Its grip drags it higher, up to most of the
  screen, and lower, down to a little of it: let go, it rests at the
  nearest of these heights, or the next one it was flicked to, or it
  calls `on-close` (see `sheet-rest`). A tap beside it closes it too,
  which dims what it covers, unless `open-beside`: then what shows above
  it takes taps. `docked`, it starts at a third of the screen or so and
  rests at a little of it instead of closing. `kept-height`, px, it
  keeps that height, whatever it holds, until its grip drags it, which
  calls `on-drag`. Down to a little of it, it shows its head, what it
  marks `data-sheet-head` (see `head-height`). Each time `lowered`, a
  number, grows, it rests where it starts again, so that what it opened
  shows above it."
  [{:keys [label height open-beside docked on-close children kept-height on-drag lowered]}]
  (let [resting*   (mf/use-state (if (true? docked) :start :low))
        resting    (deref resting*)
        drag*      (mf/use-state nil)
        drag       (deref drag*)
        sheet-ref  (mf/use-ref nil)
        ;; how high it is down to its head, px (see `head-height`)
        small-ref  (mf/use-ref nil)
        ;; the drag going on: where it started, its heights, its speed
        gesture-ref (mf/use-ref nil)

        on-drag-start
        (mf/use-fn
         (mf/deps height resting docked on-drag)
         (fn [^js event]
           (when (fn? on-drag) (on-drag))
           (let [y       (.. event -touches (item 0) -clientY)
                 sheet   (mf/ref-val sheet-ref)
                 natural (+ (some-> sheet .-firstChild .-offsetHeight)
                            (some-> sheet .-lastChild .-scrollHeight))
                 heights (sheet-heights height (.-innerHeight g/window) natural
                                        :docked docked :small (mf/ref-val small-ref))
                 shown   (some-> sheet .-offsetHeight)]
             (mf/set-ref-val! gesture-ref {:y0 y
                                           :h0 (if (pos? shown)
                                                 shown
                                                 (or (some (fn [[k px]] (when (= k resting) px)) heights)
                                                     (second (first heights))))
                                           :heights heights
                                           :speed (dds/start y 0 (.-timeStamp event))}))))

        on-drag-move
        (mf/use-fn
         (fn [^js event]
           (when-let [{:keys [y0 h0] :as gesture} (mf/ref-val gesture-ref)]
             (let [y (.. event -touches (item 0) -clientY)]
               (mf/set-ref-val! gesture-ref (update gesture :speed dds/move y (.-timeStamp event)))
               (reset! drag* (- h0 (- y y0)))))))

        on-drag-end
        (mf/use-fn
         (mf/deps drag docked on-close)
         (fn [^js event]
           (let [{:keys [heights speed]} (mf/ref-val gesture-ref)]
             (mf/set-ref-val! gesture-ref nil)
             (reset! drag* nil)
             (when (some? drag)
               ;; the screen's speed up: the finger's down
               (let [to (sheet-rest drag (- (dds/speed speed (.-timeStamp event))) heights)]
                 (cond
                   (and (= to :closed) (true? docked)) (reset! resting* (ffirst heights))
                   (= to :closed)                      (on-close)
                   :else                               (reset! resting* to)))))))

        {:keys [block-size transform]}
        (when (some? drag)
          (drag-style drag (:heights (mf/ref-val gesture-ref))))]

    (mf/with-effect [lowered]
      (when (and (number? lowered) (pos? lowered))
        (reset! resting* (if (true? docked) :start :low))))

    ;; what it holds changes its head: `.small` as high as it, on the
    ;; node, which React leaves be
    (mf/with-layout-effect [children]
      (when-let [^js node (mf/ref-val sheet-ref)]
        (let [px (head-height node)]
          (when (not= px (mf/ref-val small-ref))
            (mf/set-ref-val! small-ref px)
            (if (some? px)
              (.setProperty (.-style node) "--sheet-small" (dm/str px "px"))
              (.removeProperty (.-style node) "--sheet-small"))))))

    [:*
     (when-not open-beside
       [:div {:class (stl/css :backdrop) :on-click on-close}])
     [:div {:class (stl/css-case :sheet true
                                 :floating (not= resting :high)
                                 ;; not while dragged: what follows its head shows
                                 :small (and (= resting :small) (nil? drag))
                                 :start (= resting :start)
                                 :half (and (= height :half) (= resting :low))
                                 :full (and (= height :half) (= resting :high))
                                 :grown (and (nil? height) (= resting :high)))
            :ref sheet-ref
            :role "dialog"
            :aria-label label
            ;; a map literal: rumext passes only these to React as styles
            :style {:block-size (or block-size
                                    (when (some? kept-height) (dm/str kept-height "px")))
                    :max-block-size (when (or (some? drag) (some? kept-height)) "none")
                    :transform transform
                    :transition (when (some? drag) "none")}}
      [:div {:class (stl/css :grip-zone)
             :on-touch-start on-drag-start
             :on-touch-move on-drag-move
             :on-touch-end on-drag-end
             :on-touch-cancel on-drag-end}
       [:span {:class (stl/css :grip)}]]
      [:div {:class (stl/css :body)} children]]]))

(def controls-key
  "Where a phone keeps whether the controls of the prototype show on the
  screen (see `screen-controls*`)."
  ::screen-controls)

(mf/defc board-nav*
  "The buttons that go to the first board, the one before and the one
  after the `index`th of `total`."
  {::mf/private true}
  [{:keys [index total]}]
  [:*
   [:> icon-button* {:variant "ghost"
                     :aria-label (tr "viewer.go-to-first-board")
                     :icon i/reload
                     :disabled (zero? index)
                     :on-click #(st/emit! dv/select-first-frame)}]
   [:> icon-button* {:variant "ghost"
                     :aria-label (tr "labels.previous")
                     :icon i/arrow-left
                     :disabled (zero? index)
                     :on-click #(st/emit! dv/select-prev-frame)}]
   [:> icon-button* {:variant "ghost"
                     :aria-label (tr "labels.next")
                     :icon i/arrow-right
                     :disabled (>= (inc index) total)
                     :on-click #(st/emit! dv/select-next-frame)}]])

(def ^:private place-key
  "Where a phone keeps where its controls on the screen were dragged to."
  ::controls-place)

(def ^:private drag-slop-px
  "How far a finger on the controls on the screen moves before it drags
  them: less, it taps the button under it."
  8)

(def ^:private screen-gap-px
  "How far the controls on the screen keep from its edges."
  8)

(defn controls-place
  "Where the controls on a `screen` (`{:width :height}`, px) go, dragged
  `dx dy` px from the box `rect` (`{:left :top :width :height}`): the
  middle of their box, as parts of the screen (`{:x :y}`, 0 to 1), so
  that it holds as the phone turns, the box whole on the screen, clear of
  its `insets` too (`{:top :right :bottom :left}`, px: the room it keeps
  for its sensor housing and its home bar)."
  ([rect dx dy screen]
   (controls-place rect dx dy screen nil))
  ([rect dx dy screen insets]
   (let [{:keys [left top width height]} rect
         sw     (:width screen)
         sh     (:height screen)
         half-w (/ width 2)
         half-h (/ height 2)
         gap    #(+ screen-gap-px (get insets % 0))
         cx     (mth/clamp (+ left half-w dx) (+ half-w (gap :left)) (- sw half-w (gap :right)))
         cy     (mth/clamp (+ top half-h dy) (+ half-h (gap :top)) (- sh half-h (gap :bottom)))]
     {:x (/ cx (max 1 sw)) :y (/ cy (max 1 sh))})))

(defn kept-place
  "Where the controls dropped at `place` (see `controls-place`) show,
  `width` by `height` px now, on a `screen` with `insets`: from there,
  whole on it. Turned back, or narrower again, they show where they were
  dropped."
  [place width height screen insets]
  (controls-place {:left   (- (* (:x place) (:width screen)) (/ width 2))
                   :top    (- (* (:y place) (:height screen)) (/ height 2))
                   :width  width
                   :height height}
                  0 0 screen insets))

(defn- screen-of
  "The size of the screen the `node` shows on, px."
  [^js node]
  (let [^js window (.. node -ownerDocument -defaultView)]
    {:width (.-innerWidth window) :height (.-innerHeight window)}))

(defn- probe-insets
  "The room the screen keeps at its edges (`env(safe-area-inset-*)`), px,
  as the padding of the `probe` resolves it (see `.safe-area-probe`)."
  [^js probe]
  (if (some? probe)
    (let [^js style (.getComputedStyle (.. probe -ownerDocument -defaultView) probe)
          px        #(let [v (js/parseFloat %)] (if (js/isNaN v) 0 v))]
      {:top    (px (.-paddingTop style))
       :right  (px (.-paddingRight style))
       :bottom (px (.-paddingBottom style))
       :left   (px (.-paddingLeft style))})
    {}))

(defn- show-place!
  "Show the controls `node` at `to` (see `controls-place`), by its style:
  React not at all."
  [^js node to]
  (.setProperty (.-style node) "--controls-x" (:x to))
  (.setProperty (.-style node) "--controls-y" (:y to)))

(defn- place-style
  [{:keys [x y]}]
  #js {"--controls-x" x "--controls-y" y})

(mf/defc screen-controls*
  "On a phone, over the board at the bottom of the screen, for one who
  chose them in the options: the controls of the prototype, which the
  options hold too, at hand. Where the `index`th of `total` boards is,
  the buttons to the first, the one before and the one after, and those
  of its animation, `timeline`, when it has one. A finger drags them
  elsewhere (see `controls-place`), where they stay on this phone; one
  that moves less taps the button under it."
  [{:keys [index total timeline]}]
  (let [place*    (hooks/use-persisted-state place-key nil)
        place     (deref place*)
        node-ref  (mf/use-ref nil)
        probe-ref (mf/use-ref nil)
        ;; the finger down: where, the box then, whether it drags
        drag-ref  (mf/use-ref nil)

        on-pointer-down
        (mf/use-fn
         (fn [^js event]
           (let [^js node (mf/ref-val node-ref)
                 rect     (.getBoundingClientRect node)]
             (mf/set-ref-val! drag-ref {:id (.-pointerId event)
                                        :x0 (.-clientX event)
                                        :y0 (.-clientY event)
                                        :rect {:left (.-left rect) :top (.-top rect)
                                               :width (.-width rect) :height (.-height rect)}}))))

        on-pointer-move
        (mf/use-fn
         (fn [^js event]
           (when-let [{:keys [id x0 y0 rect dragging]} (mf/ref-val drag-ref)]
             (when (= id (.-pointerId event))
               (let [dx (- (.-clientX event) x0)
                     dy (- (.-clientY event) y0)]
                 (when (or dragging (> (mth/hypot dx dy) drag-slop-px))
                   (let [^js node (mf/ref-val node-ref)
                         to       (controls-place rect dx dy (screen-of node)
                                                  (probe-insets (mf/ref-val probe-ref)))]
                     (when-not dragging
                       (.setPointerCapture node id)
                       (mf/set-ref-val! drag-ref (assoc (mf/ref-val drag-ref) :dragging true)))
                     (.setAttribute node "data-placed" "true")
                     (show-place! node to)
                     (mf/set-ref-val! drag-ref (assoc (mf/ref-val drag-ref) :to to)))))))))

        on-pointer-up
        (mf/use-fn
         (fn [^js event]
           (when-let [{:keys [id dragging to]} (mf/ref-val drag-ref)]
             (when (= id (.-pointerId event))
               (mf/set-ref-val! drag-ref nil)
               (when (and dragging (some? to))
                 ;; the click it may end in taps nothing
                 (dds/swallow-click!)
                 (reset! place* to))))))]

    ;; whole on the screen as they grow (the buttons of an animation) or
    ;; the screen turns: from where they were dropped, which stays the
    ;; choice, so that they go back there
    (mf/with-layout-effect [place]
      (when-let [^js node (when (some? place) (mf/ref-val node-ref))]
        (let [^js window (.. node -ownerDocument -defaultView)
              keep!      (fn []
                           (when-not (:dragging (mf/ref-val drag-ref))
                             (let [rect (.getBoundingClientRect node)]
                               (show-place! node (kept-place place (.-width rect) (.-height rect)
                                                             (screen-of node)
                                                             (probe-insets (mf/ref-val probe-ref)))))))
              observer   (when-let [observer-class (.-ResizeObserver window)]
                           (new observer-class keep!))]
          (keep!)
          (some-> observer (.observe node))
          (.addEventListener window "resize" keep!)
          (fn []
            (some-> observer (.disconnect))
            (.removeEventListener window "resize" keep!)))))

    [:div {:ref node-ref
           :class (stl/css :screen-controls)
           :role "group"
           :aria-label (tr "viewer.phone.controls")
           :data-placed (some? place)
           :style (when (some? place) (place-style place))
           :on-pointer-down on-pointer-down
           :on-pointer-move on-pointer-move
           :on-pointer-up on-pointer-up
           :on-pointer-cancel on-pointer-up}
     [:span {:ref probe-ref
             :class (stl/css :safe-area-probe)
             :aria-hidden true}]
     [:span {:class (stl/css :counter)} (dm/str (inc index) " / " total)]
     [:> board-nav* {:index index :total total}]
     (when (some? timeline)
       [:*
        [:span {:class (stl/css :divider)}]
        timeline])]))

(defn use-hold-hint
  "The note of how the sheet of a phone opens (see `hold-hint*`): whether
  it shows, `[hint? hide! open-sheet! toggle-sheet!]`. It shows each time
  the prototype shows, again after the canvas (`canvas?`), until it goes
  (`hide!`) or the sheet `sheet*` opens: by two fingers (`open-sheet!`)
  or by its key (`toggle-sheet!`, Alt+O on a desktop, which closes it
  too), both alike. The fns stay the same from render to render."
  [canvas? sheet*]
  (let [hint* (mf/use-state true)
        ;; no deps: a state given before swaps as the one of now does
        hide!
        (mf/use-fn #(reset! hint* false))

        open-sheet!
        (mf/use-fn (fn []
                     (reset! hint* false)
                     (reset! sheet* true)))

        toggle-sheet!
        (mf/use-fn (fn []
                     (reset! hint* false)
                     (swap! sheet* not)))]

    ;; the prototype again, after the canvas: the note again
    (mf/with-effect [canvas?]
      (when-not canvas?
        (reset! hint* true)))

    [(deref hint*) hide! open-sheet! toggle-sheet!]))

(mf/defc hold-hint*
  "At the bottom of a phone, where the sheet comes from, `raised` above
  the controls on the screen: how it opens. It calls `on-hide` after
  `hint-ms` and at a tap; when it shows is the view mode's (each time it
  shows the prototype, see `viewer-content*`)."
  [{:keys [on-hide raised]}]
  (mf/with-effect [on-hide]
    (let [timer (js/setTimeout on-hide hint-ms)]
      #(js/clearTimeout timer)))

  [:div {:class (stl/css-case :hint true :raised (true? raised))
         :role "status"
         :on-click on-hide}
   (tr "viewer.phone.hold-hint")])

(defn- comments-events
  "The events of the entry `id` of the menu of the comments of the
  options of a phone, where they show the resolved ones too (`resolved?`):
  which comments show, or their list, which opens above the options."
  [id resolved?]
  (case id
    "resolved" [(dcm/update-filters {:show (if resolved? :pending :all)})]
    "list"     [(dcm/update-options {:show-sidebar? true})]
    [(dcm/update-filters {:mode (keyword id)})]))

(defn- comments-items
  "The entries of the menu of the comments of a phone: which show (`cmode`,
  and the resolved ones unless `resolved?` is false), and their list."
  [cmode resolved?]
  [{:id "all" :label (tr "labels.show-all-comments") :ticked (= cmode :all)}
   {:id "yours" :label (tr "labels.show-your-comments") :ticked (= cmode :yours)}
   {:id "mentions" :label (tr "labels.show-mentions") :ticked (= cmode :mentions)}
   :separator
   {:id "resolved" :label (tr "labels.hide-resolved-comments") :ticked (not resolved?)}
   {:id "list" :label (tr "labels.show-comments-list") :icon i/comments}])

(mf/defc sheet-menu*
  "A menu of the options of a phone beside share, as the header holds it:
  a button (as a select of the design system) named `label` with an
  arrow, its `items` below it, or above where they have no room,
  `{:id :label :ticked :icon}` or `:separator`, and `on-action` with the
  id of the one tapped."
  {::mf/private true}
  [{:keys [label items on-action]}]
  (let [open* (mf/use-state false)
        open? (deref open*)]
    [:> menu* {:is-open open?
               :on-open-change #(reset! open* %)
               ;; down over what the sheet holds, up where it has no room
               :placement "bottom end"
               :on-action on-action
               :trigger (mf/html
                         [:> button* {:variant "secondary"
                                      :class (stl/css :menu-button)
                                      :aria-expanded open?
                                      :on-click #(swap! open* not)}
                          label
                          [:> icon* {:icon-id i/arrow-down :size "s"}]])}
     (for [[n item] (map-indexed vector items)]
       (if (= item :separator)
         [:> menu-separator* {:key (dm/str "separator-" n)}]
         (let [{:keys [id label ticked icon]} item]
           [:> menu-item* {:key id :id id :text-value label}
            (when icon [:> icon* {:icon-id icon :size "s"}])
            [:span {:class (stl/css :menu-label)} label]
            (when ticked [:> icon* {:icon-id i/tick :size "s"}])])))]))

(mf/defc phone-sheet*
  "The options of the view mode of a phone over the board `frame`, the
  `index`th of `page` of `file` of `project`, or over the canvas of the
  page (`is-canvas`). First the logo back to the dashboard with the
  project and the file, then the switch between the canvas and the
  prototype where it `has-canvas`. In the prototype then the sections
  `permissions` allow, a menu of what the header holds in the `section`
  shown (how the interactions show, `interactions-mode`, or which
  comments show and their list) and share; its boards and its animation
  (`timeline`, the controls of the view mode); its flows. Last the pages
  of the file. It stays open as it goes to another section, page, board
  or flow, changes how the interactions or the comments show and opens
  the list of the comments or sharing, which show above it; it closes as
  it leaves the view mode or opens what shows instead (log in).
  `is-docked`, it stays at the bottom of the
  canvas (see `sheet*`). The switch keeps it open, as high as it is: it
  calls `on-switch` first, which keeps it shown in the other mode. On the
  canvas, after the pages, the whole page and its top-level boards: a tap
  moves the canvas there (see `dv/focus-canvas`) and lowers the sheet to
  where it starts."
  [{:keys [page file project frame index permissions section interactions-mode timeline on-close
           is-canvas has-canvas is-docked on-switch show-controls on-show-controls]}]
  (let [frames     (:frames page)
        total      (count frames)
        flows      (not-empty (:flows page))
        page-ids   (dm/get-in file [:data :pages])
        flow       (ctp/get-frame-flow flows (:id frame))
        comments   (mf/deref refs/comments-local)
        cmode      (or (:mode comments) :all)
        resolved?  (not= :pending (:show comments))

        act!
        (fn [& events]
          (on-close)
          (apply st/emit! events))

        ;; the sheet stays, with what the section holds
        go-to-section
        (fn [section]
          (if (or (= section :interactions) (:is-logged permissions))
            (st/emit! dv/close-thumbnails-panel (dv/go-to-section section))
            (do (on-close) (header/open-login-dialog))))

        ;; sharing shows as a second sheet above this one, which stays
        open-share
        (fn []
          (modal/show! :share-link {:page page :file file})
          (modal/disallow-click-outside!))

        ;; the height the sheet keeps going to the other mode, px
        kept*      (mf/use-state nil)

        ;; what the canvas draws, its top-level boards in the order of
        ;; the view mode, and the comment threads on it
        cobjects   (mf/with-memo [page is-canvas]
                     (when (true? is-canvas)
                       (dvc/canvas-objects (:objects page))))
        boards     (mf/with-memo [cobjects frames]
                     (when (some? cobjects)
                       (dvc/top-boards cobjects frames)))
        threads    (mf/deref refs/comment-threads)
        nthreads   (mf/with-memo [cobjects threads]
                     (count (dvc/canvas-threads (vals threads) nil cobjects)))
        comment?   (and (true? is-canvas) (dv/can-comment? permissions))
        shown?     (true? (:canvas-shown? comments))

        ;; the comments on the canvas, or none (see
        ;; `app.main.ui.viewer.canvas-comments`)
        show-comments
        (fn [shown?]
          (if shown?
            (st/emit! (dcm/update-options {:canvas-shown? true}))
            (st/emit! (dcm/close-thread) (dcm/update-options {:canvas-shown? false}))))

        ;; how often a board of the list was tapped (see `sheet*`)
        lowered*   (mf/use-state 0)

        focus-canvas
        (fn [target]
          (reset! kept* nil)
          (swap! lowered* inc)
          (st/emit! (dv/focus-canvas target)))

        switch-mode
        (fn [value event]
          (reset! kept* (some-> (dom/get-target event) (.closest "[role=dialog]") (.-offsetHeight)))
          (when (fn? on-switch) (on-switch))
          ;; a thread open on the canvas stays there
          (st/emit! (dcm/close-thread)
                    (if (= "canvas" value) dv/show-canvas (dv/go-to-frame-auto))))

        ;; the page as a whole, or its boards one by one
        view-switch
        (when (true? has-canvas)
          (mf/html
           [:> radio-buttons* {:name "phone-view"
                               :extended true
                               :selected (if (true? is-canvas) "canvas" "prototype")
                               :on-change switch-mode
                               :options [{:id "phone-view-canvas"
                                          :value "canvas"
                                          :label (tr "viewer.phone.canvas")}
                                         {:id "phone-view-prototype"
                                          :value "prototype"
                                          :label (tr "viewer.phone.prototype")}]}]))]

    [:> sheet* {:label (tr "viewer.phone.options")
                :docked (true? is-docked)
                :open-beside (true? is-docked)
                :kept-height (deref kept*)
                :lowered (deref lowered*)
                :on-drag #(reset! kept* nil)
                :on-close on-close}
     ;; the logo, back to the dashboard for the team, the project and the
     ;; file, as the header of the workspace shows them, which a phone
     ;; shows no header for
     [:div {:class (stl/css :row :file-row) :data-sheet-head true}
      (if (:in-team permissions)
        [:button {:type "button"
                  :class (stl/css :logo)
                  :aria-label (tr "labels.dashboard")
                  :on-click #(act! (dv/go-to-dashboard))}
         deprecated-icon/logo-icon]
        [:span {:class (stl/css :logo)} deprecated-icon/logo-icon])
      [:div {:class (stl/css :names)}
       (when (some? project)
         [:span {:class (stl/css :project-name)} (:name project)])
       [:span {:class (stl/css :file-name)} (:name file)]]]

     ;; then the switch, in the prototype as on the canvas, where the
     ;; docked sheet shows both at its little height: its head
     (when (some? view-switch)
       [:div {:data-sheet-head true} view-switch])

     ;; the comments of the canvas, for one who may comment: shown or not,
     ;; which, and their list; and how to add one
     (when comment?
       [:div {:class (stl/css :row)}
        [:> switch* {:class (stl/css :comments-switch)
                     :label (tr "viewer.phone.show-comments" nthreads)
                     :default-checked shown?
                     :on-change show-comments}]
        [:span {:class (stl/css :gap)}]
        (when shown?
          [:> sheet-menu* {:label (tr "labels.comments")
                           :on-action #(apply st/emit! (comments-events % resolved?))
                           :items (comments-items cmode resolved?)}])])
     (when (and comment? shown?)
       [:p {:class (stl/css :comments-hint)} (tr "viewer.phone.comment-hint")])

     ;; the sections and share, of the prototype only: the canvas shows the
     ;; page as it is. A phone edits not: no button for the workspace
     (when-not (true? is-canvas)
       [:div {:class (stl/css :row)}
        [:> icon-button* {:variant "ghost"
                          :aria-pressed (= section :interactions)
                          :aria-label (tr "viewer.header.interactions-section" (sc/get-tooltip :open-interactions))
                          :icon i/play
                          :on-click #(go-to-section :interactions)}]
        (when (or (:in-team permissions) (= (:who-comment permissions) "all"))
          [:> icon-button* {:variant "ghost"
                            :aria-pressed (= section :comments)
                            :aria-label (tr "viewer.header.comments-section" (sc/get-tooltip :open-comments))
                            :icon i/comments
                            :on-click #(go-to-section :comments)}])
        (when (or (:in-team permissions)
                  (and (= (:type permissions) :share-link)
                       (= (:who-inspect permissions) "all")))
          [:> icon-button* {:variant "ghost"
                            :aria-pressed (= section :inspect)
                            :aria-label (tr "viewer.header.inspect-section" (sc/get-tooltip :open-inspect))
                            :icon i/code
                            :on-click #(go-to-section :inspect)}])
        [:span {:class (stl/css :gap)}]
        ;; how the interactions show, the one shown ticked
        (when (= section :interactions)
          (let [shown (or interactions-mode :show-on-click)]
            [:> sheet-menu* {:label (tr "viewer.header.interactions")
                             :on-action #(st/emit! (dv/set-interactions-mode (keyword %)))
                             :items [{:id "hide" :label (tr "viewer.header.dont-show-interactions")
                                      :ticked (= shown :hide)}
                                     {:id "show" :label (tr "viewer.header.show-interactions")
                                      :ticked (= shown :show)}
                                     {:id "show-on-click" :label (tr "viewer.header.show-interactions-on-click")
                                      :ticked (= shown :show-on-click)}]}]))
        ;; which comments show, and their list, above the options
        (when (= section :comments)
          [:> sheet-menu* {:label (tr "labels.comments")
                           :on-action #(apply st/emit! (comments-events % resolved?))
                           :items (comments-items cmode resolved?)}])
        (when (:in-team permissions)
          [:> button* {:variant "primary" :on-click open-share}
           (tr "labels.share")])
        (when-not (:is-logged permissions)
          [:> button* {:variant "ghost"
                       :on-click #(do (on-close) (header/open-login-dialog))}
           (tr "labels.log-or-sign")])])

     ;; the boards
     (when-not (true? is-canvas)
       [:div {:class (stl/css :row)}
        [:span {:class (stl/css :board-name)} (:name frame)]
        [:span {:class (stl/css :counter)} (dm/str (inc index) " / " total)]
        [:> board-nav* {:index index :total total}]])

     ;; its animation
     (when (and (some? timeline) (not (true? is-canvas)))
       [:div {:class (stl/css :row)} timeline])

     ;; the controls of the boards and of the animation on the screen
     ;; too, at hand (see `screen-controls*`)
     (when (and (not (true? is-canvas)) (fn? on-show-controls))
       [:div {:class (stl/css :row)}
        [:> checkbox* {:id "phone-screen-controls"
                       :label (tr "viewer.phone.show-controls")
                       :checked (true? show-controls)
                       :on-change #(on-show-controls (not (true? show-controls)))}]])

     ;; the pages, the one shown in the mode it shows (see `dv/go-to-page`),
     ;; a lone one too: which page shows
     (when (seq page-ids)
       [:ul {:class (stl/css :list)}
        (for [id page-ids]
          [:li {:key (dm/str id)}
           [:button {:type "button"
                     :class (stl/css :item)
                     :aria-pressed (= id (:id page))
                     ;; the sheet stays, over the other page
                     :on-click #(st/emit! (dv/go-to-page id))}
            [:> icon* {:icon-id i/document :size "s"}]
            [:span {:class (stl/css :label)} (dm/get-in file [:data :pages-index id :name])]
            (when (= id (:id page))
              [:> icon* {:icon-id i/tick :size "s"}])]])])

     ;; on the canvas, the whole page and its boards
     (when (seq boards)
       [:ul {:class (stl/css :list)
             :aria-label (tr "viewer.phone.boards")}
        [:li
         [:button {:type "button"
                   :class (stl/css :item)
                   :on-click #(focus-canvas :page)}
          [:> icon* {:icon-id i/fit-content :size "s"}]
          [:span {:class (stl/css :label)} (tr "viewer.phone.whole-page")]]]
        (for [{:keys [id name]} boards]
          [:li {:key (dm/str id)}
           [:button {:type "button"
                     :class (stl/css :item)
                     :on-click #(focus-canvas {:board id})}
            [:> icon* {:icon-id i/board :size "s"}]
            [:span {:class (stl/css :label)} name]]])])

     ;; the flows
     (when (and flows (= section :interactions))
       [:ul {:class (stl/css :list)}
        (for [[flow-id item] flows]
          [:li {:key (dm/str flow-id)}
           [:button {:type "button"
                     :class (stl/css :item)
                     :aria-pressed (= flow-id (:id flow))
                     :on-click #(st/emit! (dv/go-to-frame (:starting-frame item)))}
            [:> icon* {:icon-id i/play :size "s"}]
            [:span {:class (stl/css :label)} (:name item)]
            (when (= flow-id (:id flow))
              [:> icon* {:icon-id i/tick :size "s"}])]])])]))
