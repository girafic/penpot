;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline.lanes
  "The time axis of the timeline dock, drawn on a canvas like Figma's: the
  ruler and, under it, the lane of each row with its bar, preset animation
  or keyframes. Only the rows in view are drawn, so scrubbing and playing
  cost the same with a hundred layers as with a few. The labels left of the
  lanes and the markers stay DOM (see `timeline*`).

  Positions are canvas pixels: `x` from the left edge of the lanes, right
  of the labels, and `y` from the top of the ruler. `geo` places the lanes:
  the `:width` and `:height` of the canvas, the time axis (`:span` ms over
  `:axis-width` px, from `:origin`, 0 or before when something lies
  there) and how far the view is scrolled (`:scroll-x`, `:scroll-y`)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.main.ui.workspace.timeline.easing :as easing]))

;; METRICS, in step with timeline.scss

(def label-width
  "Width of the labels left of the lanes (`$track-label-width`)."
  300)

(def start-gap
  "Room before 0 s, so a keyframe and the playhead pin there stay clear of
  the labels (`$axis-start-gap`)."
  12)

(def end-gap
  "Room after the end of the time axis (`$axis-end-gap`)."
  8)

(def ruler-height 24)

(def header-height
  "The ruler and the row of the markers, above the rows (`$header-height`)."
  52)

(def default-row-height
  "Height of a row as it starts (`$row-height`), and of the row of the
  markers, which stays so (see `header-height`)."
  28)

(def max-row-height
  "The tallest the rows get (see `row-height`)."
  72)

(def ^:private bar-inset
  "Room above and below a bar or a block in its row."
  5)

(def ^:private min-bar-width 12)

(def ^:private fade-grip
  "The radius of the grip of a fade of an audio clip, on the top edge of
  its bar where the fade ends (see `fade-grips`); a drag on it, from
  `fade-reach` px around it, changes the fade."
  4)

(def ^:private fade-reach 6)

(def ^:private volume-reach
  "How far, in px, from the volume line of an audio clip a drag takes it
  (see `volume-y`)."
  3)

(defn- volume-y
  "Where the volume line of `clip` is in its bar, from `top`, `height`
  px tall: near its top at the volume of its file and louder (when the
  line turns orange, see `draw-clip!`), near its bottom when silent."
  [top height clip]
  (+ top 3 (* (- 1 (min 1 (or (:volume clip) 1))) (- height 6))))

(def ^:private keyframe-radius
  "Half the diagonal of a keyframe, a 10px square turned 45°."
  (* 5 (mth/sqrt 2)))

;; GEOMETRY

(defn row-height
  "Height of the rows of `geo`, which the user makes taller up to
  `max-row-height` (see `dwa/row-height`): their bars and blocks grow
  with them, their keyframes stay in their middle."
  [geo]
  (get geo :row-height default-row-height))

(defn row-anchored-scroll
  "Where rows scrolled to `scroll-top` scroll to once they are `height`
  tall instead of `old-height`, so that what is at `y` of the view (the
  pointer, say) stays there; above the rows, in the header, the top row
  stays."
  [scroll-top y old-height height]
  (let [offset (max 0 (- y header-height))]
    (- (* (+ scroll-top offset) (/ height old-height)) offset)))

(defn axis-width
  "Pixels the time axis takes in a view `width` wide, labels included, at
  `zoom` (1 fits it in the view)."
  [width zoom]
  (* (max 0 (- width label-width start-gap end-gap)) zoom))

(defn content-width
  "Width of what the view scrolls over, for a time axis `axis-width` wide."
  [axis-width]
  (+ label-width start-gap axis-width end-gap))

(defn- axis-x
  "Where `time` is on what the view scrolls over."
  [{:keys [span axis-width origin]} time]
  (+ start-gap (* axis-width (/ (- time (or origin 0)) (max 1 span)))))

(defn time->x
  "Where `time` is on the canvas."
  [{:keys [scroll-x] :as geo} time]
  (- (axis-x geo time) scroll-x))

(defn x->time
  "The time at `x` of the canvas, on the time axis."
  [{:keys [span axis-width scroll-x origin]} x]
  (+ (or origin 0)
     (* span (mth/clamp (/ (- (+ x scroll-x) start-gap) (max 1 axis-width)) 0 1))))

(defn row-y
  "Top of the row at `index` on the canvas."
  [{:keys [scroll-y] :as geo} index]
  (- (+ header-height (* index (row-height geo))) scroll-y))

(defn row-index
  "Index of the row at `y` of the canvas (past the last one under the
  rows), nil above the rows."
  [{:keys [scroll-y] :as geo} y]
  (when (>= y header-height)
    (int (mth/floor (/ (+ (- y header-height) scroll-y) (row-height geo))))))

(defn visible-rows
  "`[start end]` indices of the rows in view, with `overscan` more on each
  side. `end` may be past the last row."
  [{:keys [height scroll-y] :as geo} overscan]
  (let [row-height (row-height geo)]
    [(int (max 0 (- (mth/floor (/ scroll-y row-height)) overscan)))
     (int (max 0 (+ (mth/ceil (/ (+ scroll-y (- height header-height)) row-height))
                    overscan)))]))

(defn scroll-to-rows
  "Where the rows scroll to (a `:scroll-y`) so that the first of the rows
  at `indices` is in the middle of the view, or nil while one of them is
  in view."
  [{:keys [height scroll-y] :as geo} indices]
  (let [row-height  (row-height geo)
        rows-height (- height header-height)
        in-view?    (fn [index]
                      (let [top (- (* index row-height) scroll-y)]
                        (and (>= top 0) (<= (+ top row-height) rows-height))))]
    (when (and (seq indices) (not-any? in-view? indices))
      (max 0 (mth/round (- (* (apply min indices) row-height)
                           (/ (- rows-height row-height) 2)))))))

(defn content-point
  "Canvas point `x`, `y`, kept on the rows, where it is on what the view
  scrolls over, so it stays the same as the view scrolls: `x` from the left
  edge of the lanes, `y` from the top of the rows."
  [{:keys [width height scroll-x scroll-y]} x y]
  [(+ (mth/clamp x 0 width) scroll-x)
   (+ (- (mth/clamp y header-height height) header-height) scroll-y)])

(defn- property-lane
  "The keyframes of the property of `row`, and whether they are locked or
  hidden."
  [timeline {:keys [id property index]}]
  {:keyframes (cta/property-keyframes timeline id property index)
   :locked?   (cta/slot-flag? timeline id :locked property index)
   :hidden?   (cta/slot-flag? timeline id :hidden property index)})

(defn- segment-ends
  "`[x0 x1]` of the line from keyframe `from` to `to`, clear of both."
  [geo from to]
  [(+ (time->x geo (:time from)) 7)
   (- (time->x geo (:time to)) 7)])

(defn- easing-button
  "Canvas rect of the easing button in the middle of the segment from `from`
  to `to`, in the row with its top at `y`."
  [geo from to y]
  (let [[x0 x1] (segment-ends geo from to)
        cx      (+ x0 (/ (max 0 (- x1 x0)) 2))]
    {:x (- cx 10) :y (+ y (/ (- (row-height geo) 20) 2)) :width 20 :height 20}))

(defn in-rect?
  [{:keys [x y width height]} px py]
  (and (<= x px (+ x width))
       (<= y py (+ y height))))

;; HIT TESTING

(defn- bar-part
  "The part of a bar or block from `x0` to `x1` at `x`: its `:start` or
  `:end`, grabbed from just outside its edge to past its grip (unless
  `ends?` is false), the rest of it (`:move`), or nil."
  [x0 x1 x ends?]
  (let [x1 (max x1 (+ x0 min-bar-width))]
    (cond
      (and ends? (<= (- x1 10) x (+ x1 4))) :end
      (and ends? (<= (- x0 4) x (+ x0 10))) :start
      (<= x0 x x1)                          :move)))

(defn- keyframe-at
  "The keyframe of `keyframes` under `x`, `y` (the last drawn, on top), in
  a row with its middle at `cy`."
  [geo keyframes x y cy]
  (let [reach (inc keyframe-radius)]
    (d/seek #(<= (+ (mth/abs (- x (time->x geo (:time %))))
                    (mth/abs (- y cy)))
                 reach)
            (rseq keyframes))))

(defn- property-hit
  [geo timeline row top x y base]
  (let [{:keys [keyframes locked?]} (property-lane timeline row)
        base (assoc base
                    :property (:property row)
                    :index (:index row)
                    :locked? locked?)]
    (if-let [keyframe (keyframe-at geo keyframes x y (+ top (/ (row-height geo) 2)))]
      (assoc base :type :keyframe :keyframe keyframe)
      (or (some (fn [[from to]]
                  (let [button  (easing-button geo from to top)
                        [x0 x1] (segment-ends geo from to)]
                    (cond
                      (in-rect? button x y) (assoc base :type :easing :from from :rect button)
                      (<= x0 x x1)          (assoc base :type :segment :from from))))
                (partition 2 1 keyframes))
          (assoc base :type :lane)))))

(def ^:private end-grip
  "How far (px) from the end of the timeline a press on the background of
  the rows drags it (see `hit`)."
  4)

(declare row-hit)

(defn- fade-grips
  "`[in-x out-x]`, where the grips of the fades of the audio `clip` are
  on the top edge of its bar: where each fade ends, but at least a grip
  inside the bar. Nil when the bar is too narrow for them."
  [geo clip]
  (let [x0    (time->x geo (:start clip))
        x1    (max (time->x geo (ctau/clip-end clip)) (+ x0 min-bar-width))
        inset (+ fade-grip 3)]
    (when (>= (- x1 x0) (* 4 inset))
      [(max (+ x0 inset) (time->x geo (+ (:start clip) (or (:fade-in clip) 0))))
       (min (- x1 inset) (time->x geo (- (ctau/clip-end clip) (or (:fade-out clip) 0))))])))

(defn- fade-hit
  "The fade of the audio `clip` whose grip is at `x` (see `fade-grips`),
  `:fade-in` or `:fade-out`; nil for none, and on a locked clip."
  [geo clip x]
  (when-not (:locked clip)
    (when-let [[in-x out-x] (fade-grips geo clip)]
      (cond
        (<= (mth/abs (- x in-x)) fade-reach)  :fade-in
        (<= (mth/abs (- x out-x)) fade-reach) :fade-out))))

(defn hit
  "What is at `x`, `y` of the canvas, a map with its `:type`:
  - on the ruler, the `:ruler`;
  - in a row (`:row` its index, `:shape-id` its layer), a layer `:bar` or
    an `:animation` block, `:mode` telling which part (see `bar-part`),
    the bar of a component `:copy`, or else the rest of the `:row`; in
    the row of a property (`:locked?`
    when it is), a `:keyframe`, the `:easing` button of a segment (with
    its `:rect`) or the rest of a `:segment` (both with the keyframe it
    starts `:from`), or else the rest of its `:lane`;
  - `:empty` under the rows;
  - on the background of the rows (a `:row`, a `:lane` or `:empty`),
    close to the end of the timeline, the `:duration`, which a drag
    there changes, like the end of the timeline in Figma.
  Nil over the markers and off the canvas. `scene` gives the `:rows` (see
  `timeline-rows`), their `:timeline` and its `:duration`."
  [geo {:keys [duration] :as scene} x y]
  (cond
    (or (neg? x) (neg? y) (> x (:width geo)) (> y (:height geo)))
    nil

    (< y ruler-height)
    {:type :ruler}

    (< y header-height)
    nil

    :else
    (let [target (row-hit geo scene x y)]
      (if (and (contains? #{:row :lane :empty} (:type target))
               (<= (mth/abs (- x (time->x geo duration))) end-grip))
        {:type :duration}
        target))))

(defn- row-hit
  "What is at `x`, `y` of the rows (see `hit`)."
  [geo {:keys [rows timeline]} x y]
  (let [index (row-index geo y)]
    (if-let [row (get rows index)]
      (let [top  (row-y geo index)
            bar? (<= bar-inset (- y top) (- (row-height geo) bar-inset))
            base {:row index :shape-id (:id row)}]
        (case (:type row)
          :layer
          (let [[start end] (:range row)
                [_ copy-end] (:copy row)
                part        (when (and bar? (some? start))
                              (bar-part (time->x geo start) (time->x geo end) x true))]
            (cond
              (some? part)
              (assoc base :type :bar :mode part)

              ;; the bar of a component copy: it selects the copy
              (and bar? (some? copy-end)
                   (<= (time->x geo 0) x (max (time->x geo copy-end)
                                              (+ (time->x geo 0) min-bar-width))))
              (assoc base :type :copy)

              :else
              (assoc base :type :row)))

          :animation
          (let [animation (:animation row)
                part      (when bar?
                            (bar-part (time->x geo (:start animation))
                                      (time->x geo (cta/animation-end animation))
                                      x
                                      (not (:locked animation))))]
            (if (some? part)
              (assoc base :type :animation :animation animation :mode part)
              (assoc base :type :row)))

          :property
          (property-hit geo timeline row top x y base)

          ;; the group of the audio lanes: nothing to drag on it
          :audio-group
          (assoc base :type :row)

          ;; the top-most clip there, the one drawn last; on the top edge
          ;; of a clip, the grips of its fades first
          :audio
          (let [fade   (when (<= (- y top) (+ bar-inset fade-reach))
                         (some (fn [clip]
                                 (when-let [mode (fade-hit geo clip x)]
                                   [clip mode]))
                               (rseq (:clips row))))
                ;; the volume line of a clip, away from its ends
                volume (when (nil? fade)
                         (d/seek (fn [clip]
                                   (and (not (:locked clip))
                                        (< (+ (time->x geo (:start clip)) 10) x
                                           (- (time->x geo (ctau/clip-end clip)) 10))
                                        (<= (mth/abs (- y (volume-y (+ top bar-inset)
                                                                    (- (row-height geo) (* 2 bar-inset))
                                                                    clip)))
                                            volume-reach)))
                                 (rseq (:clips row))))
                clip (when bar?
                       (d/seek #(bar-part (time->x geo (:start %)) (time->x geo (ctau/clip-end %)) x true)
                               (rseq (:clips row))))
                part (when (some? clip)
                       (bar-part (time->x geo (:start clip)) (time->x geo (ctau/clip-end clip)) x
                                 (not (:locked clip))))]
            (cond
              (some? fade)
              (assoc base :type :audio :clip (first fade) :lane (:lane row) :mode (second fade))

              (some? volume)
              (assoc base :type :audio :clip volume :lane (:lane row) :mode :volume)

              (some? part)
              (assoc base :type :audio :clip clip :lane (:lane row) :mode part)

              :else
              (assoc base :type :row :lane (:lane row))))))
      {:type :empty})))

(defn animation-boxes
  "The boxes of the blocks of the preset animations that can be selected
  (not locked), with their `:shape-id` and `:animation-id`, where they are
  on what the view scrolls over (see `content-point`), for a box
  selection."
  [geo {:keys [rows]}]
  (let [x #(axis-x geo %)]
    (into []
          (comp (map-indexed vector)
                (filter #(= :animation (:type (second %))))
                (keep (fn [[index {:keys [id animation]}]]
                        (when-not (:locked animation)
                          (let [top (+ (* index (row-height geo)) bar-inset)]
                            {:shape-id     id
                             :animation-id (:id animation)
                             :left         (x (:start animation))
                             :right        (x (cta/animation-end animation))
                             :top          top
                             :bottom       (- (+ top (row-height geo)) (* 2 bar-inset))})))))
          rows)))

(defn bar-boxes
  "The boxes of the bars of the layer tracks, with their `:shape-id`, where
  they are on what the view scrolls over (see `content-point`), for a box
  selection: one touched picks all of its track."
  [geo {:keys [rows]}]
  (let [x #(axis-x geo %)]
    (into []
          (comp (map-indexed vector)
                (keep (fn [[index {:keys [type id range]}]]
                        (when (and (= :layer type) (some? range))
                          (let [top (+ (* index (row-height geo)) bar-inset)]
                            {:shape-id id
                             :left     (x (first range))
                             :right    (x (second range))
                             :top      top
                             :bottom   (- (+ top (row-height geo)) (* 2 bar-inset))})))))
          rows)))

(defn clip-boxes
  "The boxes of the audio clips that can be selected (not locked), with
  their `:clip-id`, where they are on what the view scrolls over (see
  `content-point`), for a box selection."
  [geo {:keys [rows]}]
  (let [x #(axis-x geo %)]
    (into []
          (comp (map-indexed vector)
                (filter #(= :audio (:type (second %))))
                (mapcat (fn [[index row]]
                          (let [top (+ (* index (row-height geo)) bar-inset)]
                            (keep (fn [clip]
                                    (when-not (:locked clip)
                                      {:clip-id (:id clip)
                                       :left    (x (:start clip))
                                       :right   (x (ctau/clip-end clip))
                                       :top     top
                                       :bottom  (- (+ top (row-height geo)) (* 2 bar-inset))}))
                                  (:clips row))))))
          rows)))

(defn keyframe-boxes
  "The boxes of the keyframes that can be selected (not locked), with their
  `:shape-id` and `:keyframe-id`, where they are on what the view scrolls
  over (see `content-point`), for a box selection."
  [geo {:keys [rows timeline]}]
  (into []
        (comp (map-indexed vector)
              (filter #(= :property (:type (second %))))
              (mapcat (fn [[index row]]
                        (let [{:keys [keyframes locked?]} (property-lane timeline row)
                              row-height (row-height geo)
                              cy (+ (* index row-height) (/ row-height 2))]
                          (when-not locked?
                            (map (fn [keyframe]
                                   (let [cx (axis-x geo (:time keyframe))]
                                     {:shape-id    (:id row)
                                      :keyframe-id (:id keyframe)
                                      :left        (- cx keyframe-radius)
                                      :right       (+ cx keyframe-radius)
                                      :top         (- cy keyframe-radius)
                                      :bottom      (+ cy keyframe-radius)}))
                                 keyframes))))))
        rows))

;; DRAWING

(def palette
  "The colours the lanes are drawn with, from the theme (see `read-palette`)."
  {:bg           "var(--color-background-primary)"
   :bg-hover     "var(--color-background-secondary)"
   :bg-selected  "var(--color-accent-primary-muted)"
   :shade        "var(--color-background-tertiary)"
   :border       "var(--panel-border-color)"
   :fg           "var(--color-foreground-secondary)"
   :fg-strong    "var(--color-foreground-primary)"
   :accent       "var(--color-accent-primary)"
   :accent-muted "var(--color-accent-primary-muted)"
   :bar          "var(--timeline-bar-color)"
   ;; text and grips on the accent, as on a primary button: dark on the
   ;; light accent of the dark theme, light on the dark one of the light
   :on-accent    "var(--color-background-secondary)"
   :playhead     "var(--timeline-playhead-color)"
   :marker       "var(--color-accent-info)"
   :snap         "var(--color-accent-warning)"
   ;; the selection box of the canvas (see `selection-rect*`)
   :select       "var(--color-accent-tertiary)"
   :select-fill  "var(--color-accent-tertiary-muted)"
   ;; a component copy playing the animation of its main
   :component    "var(--color-accent-secondary)"
   :component-muted "color-mix(in srgb, var(--color-accent-secondary) 30%, transparent)"
   ;; an audio clip and its waveform; one that stays silent
   :audio        "color-mix(in srgb, var(--color-accent-success) 28%, transparent)"
   :wave         "var(--color-accent-success)"
   :error        "var(--color-accent-error)"})

(defn read-palette
  "The colours of `palette` as the browser resolves them on the children of
  `node`: one per colour, named in `data-color`, with it as its `color`."
  [^js node]
  (let [children (.-children node)]
    (loop [i 0 colors {}]
      (if (< i (.-length children))
        (let [child (.item children i)]
          (recur (inc i)
                 (assoc colors
                        (keyword (.getAttribute child "data-color"))
                        (.-color (js/getComputedStyle child)))))
        colors))))

(defn- crisp
  "`x` on a device pixel, so a line from it is sharp."
  [x dpr]
  (/ (mth/round (* x dpr)) dpr))

(defn- fill-rect!
  [^js ctx color x y width height]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx x y width height))

(defn- fill-round-rect!
  [^js ctx color x y width height radius]
  (set! (.-fillStyle ctx) color)
  (.beginPath ctx)
  (.roundRect ctx x y width height radius)
  (.fill ctx))

(defn- dashed-line!
  "A 1px dashed line down from `y0` to `y1`, from `x`."
  [^js ctx color x y0 y1]
  (.save ctx)
  (set! (.-strokeStyle ctx) color)
  (set! (.-lineWidth ctx) 1)
  (.setLineDash ctx #js [3 3])
  (.beginPath ctx)
  (.moveTo ctx (+ x 0.5) y0)
  (.lineTo ctx (+ x 0.5) y1)
  (.stroke ctx)
  (.restore ctx))

(defn- diamond!
  "Path of a diamond centred on `x`, `y`, `r` from its centre to its
  corners."
  [^js ctx x y r]
  (.beginPath ctx)
  (.moveTo ctx x (- y r))
  (.lineTo ctx (+ x r) y)
  (.lineTo ctx x (+ y r))
  (.lineTo ctx (- x r) y)
  (.closePath ctx))

(defn- faded!
  "Draw with `draw-fn` at `alpha` of the opacity it has."
  [^js ctx alpha draw-fn]
  (if (== alpha 1)
    (draw-fn)
    (do (.save ctx)
        (set! (.-globalAlpha ctx) (* alpha (.-globalAlpha ctx)))
        (draw-fn)
        (.restore ctx))))

(defn- fit-text
  "`text`, cut short with an ellipsis to fit in `width`; nil when not even
  that fits."
  [^js ctx text width]
  (if (<= (.-width (.measureText ctx text)) width)
    text
    (loop [n (dec (count text))]
      (when (pos? n)
        (let [cut (dm/str (subs text 0 n) "…")]
          (if (<= (.-width (.measureText ctx cut)) width)
            cut
            (recur (dec n))))))))

(defn- draw-bar!
  "A layer bar or an animation block from `start` to `end` in the row with
  its top at `y`, in `fill`. The grips at its ends, in `ink`, brighten while
  it is `hovered?`; a locked block has none (`grips?`). Its `[x0 x1]`."
  [ctx geo y start end fill ink hovered? grips?]
  (let [x0     (time->x geo start)
        x1     (max (time->x geo end) (+ x0 min-bar-width))
        top    (+ y bar-inset)
        height (- (row-height geo) (* 2 bar-inset))]
    (fill-round-rect! ctx fill x0 top (- x1 x0) height 6)
    (when grips?
      (faded! ctx (if hovered? 1 0.6)
              (fn []
                (let [grip-y (+ top (* 0.3 height))
                      grip-h (* 0.4 height)]
                  (fill-round-rect! ctx ink (+ x0 6) grip-y 2 grip-h 1)
                  (fill-round-rect! ctx ink (- x1 8) grip-y 2 grip-h 1)))))
    [x0 x1]))

(defn- outline-bar!
  "An outline around the bar from `start` to `end` in the row at `y`, in
  the colour of the box selection: its keyframes or block are selected in
  the dock (see `dwaa/selection-of`)."
  [^js ctx geo y start end color]
  (let [x0     (time->x geo start)
        x1     (max (time->x geo end) (+ x0 min-bar-width))
        top    (+ y bar-inset)
        height (- (row-height geo) (* 2 bar-inset))]
    (.beginPath ctx)
    (.roundRect ctx (- x0 1) (- top 1) (+ (- x1 x0) 2) (+ height 2) 7)
    (set! (.-strokeStyle ctx) color)
    (set! (.-lineWidth ctx) 2)
    (.stroke ctx)))

(defn- draw-block-label!
  "The name of an animation, centred in its block from `x0` to `x1`."
  [^js ctx text color font x0 x1 cy]
  (let [room (- x1 x0 32)]
    (when (pos? room)
      (set! (.-font ctx) (dm/str "12px " font))
      (when-let [text (fit-text ctx text room)]
        (set! (.-fillStyle ctx) color)
        (set! (.-textAlign ctx) "center")
        (set! (.-textBaseline ctx) "middle")
        (.fillText ctx text (/ (+ x0 x1) 2) cy)))))

(defn- draw-easing-button!
  "The button with the easing of the segment starting at keyframe `from`,
  which opens the easing editor, drawn like `curve-icon*` in a box."
  [^js ctx palette color {:keys [x y width height]} from]
  (fill-round-rect! ctx (:bg palette) x y width height 4)
  (set! (.-strokeStyle ctx) color)
  (set! (.-lineWidth ctx) 1)
  (.beginPath ctx)
  (.roundRect ctx (+ x 0.5) (+ y 0.5) (dec width) (dec height) 3.5)
  (.stroke ctx)
  (.beginPath ctx)
  (doseq [[i [px py]] (map-indexed vector (easing/icon-points (:easing from)
                                                              (= :step (:interpolation from))))]
    (if (zero? i)
      (.moveTo ctx (+ x 2 px) (+ y 2 py))
      (.lineTo ctx (+ x 2 px) (+ y 2 py))))
  (set! (.-lineWidth ctx) 1.25)
  (set! (.-lineCap ctx) "round")
  (set! (.-lineJoin ctx) "round")
  (.stroke ctx))

(defn- draw-property!
  "The keyframes of the property of `row`, with its top at `y`, joined by
  lines; the easing of the one under the pointer (`hover`, see `hit`) or
  open in the editor shows on it."
  [^js ctx geo {:keys [timeline selected-kfs easing]} palette row y color hover]
  (let [{:keys [keyframes locked? hidden?]} (property-lane timeline row)
        shape-id (:id row)
        cy       (+ y (/ (row-height geo) 2))
        width    (:width geo)
        segments (partition 2 1 keyframes)

        button?
        (fn [from]
          (or (and (contains? #{:segment :easing} (:type hover))
                   (= (:id from) (:id (:from hover))))
              (and (= shape-id (:shape-id easing))
                   (= (:id from) (:keyframe-id easing)))))]

    (faded! ctx (if hidden? 0.5 1)
            (fn []
              (doseq [[from to] segments
                      :let [[x0 x1] (segment-ends geo from to)]
                      :when (and (< x0 x1) (< x0 width) (pos? x1))]
                (fill-rect! ctx color x0 cy (- x1 x0) 1))

              (doseq [[from to] segments
                      :when (button? from)]
                (draw-easing-button! ctx palette color (easing-button geo from to y) from))

              (doseq [keyframe keyframes
                      :let [x (time->x geo (:time keyframe))]
                      :when (<= -12 x (+ width 12))
                      :let [selected? (contains? selected-kfs {:shape-id shape-id :keyframe-id (:id keyframe)})
                            ;; a selected one in the accent, whatever the colour of its lane
                            color     (if selected? (:accent palette) color)]]
                (when (and (not locked?)
                           (= :keyframe (:type hover))
                           (= (:id keyframe) (:id (:keyframe hover))))
                  (set! (.-strokeStyle ctx) (:accent-muted palette))
                  (set! (.-lineWidth ctx) 2)
                  (diamond! ctx x cy (+ keyframe-radius (mth/sqrt 2)))
                  (.stroke ctx))
                (when selected?
                  (set! (.-fillStyle ctx) color)
                  (diamond! ctx x cy keyframe-radius)
                  (.fill ctx))
                (set! (.-strokeStyle ctx) color)
                (set! (.-lineWidth ctx) 1)
                (diamond! ctx x cy (- keyframe-radius (/ (mth/sqrt 2) 2)))
                (.stroke ctx))))))

(defn- draw-hatching!
  "Diagonal lines over the box from `x`, `y`, `width` × `height`: a clip
  whose file is not decoded (yet)."
  [^js ctx color x y width height]
  (.save ctx)
  (.beginPath ctx)
  (.rect ctx x y width height)
  (.clip ctx)
  (set! (.-strokeStyle ctx) color)
  (set! (.-lineWidth ctx) 1)
  (.beginPath ctx)
  (doseq [i (range (- x height) (+ x width) 6)]
    (.moveTo ctx i (+ y height))
    (.lineTo ctx (+ i height) y))
  (.stroke ctx)
  (.restore ctx))

(defn- draw-waveform!
  "The waveform `peaks` (see `app.util.audio/peaks`) of `clip`, one line
  a pixel wide for each column of it in view, between `top` and `bottom`:
  as loud as its file is at most, but not a near silence, at the volume
  of the clip."
  [^js ctx {:keys [span axis-width scroll-x width origin]} peaks clip x0 x1 top bottom color]
  (let [{:keys [bucket-ms channels peak]} peaks
        buckets  (/ (.-length ^js (first channels)) 2)
        ms-of-px (/ span (max 1 axis-width))
        speed    (ctau/clip-speed clip)
        ;; the ms of the file at the left edge of the column at `x`
        file-ms  (fn [x] (+ (:offset clip)
                            (* speed (- (+ (or origin 0) (* (- (+ x scroll-x) start-gap) ms-of-px))
                                        (:start clip)))))
        mid      (/ (+ top bottom) 2)
        ;; as loud as the clip plays it
        amp      (* (or (:volume clip) 1) (/ (- bottom top) 2 (max 0.1 (or peak 1))))]
    (set! (.-fillStyle ctx) color)
    (doseq [x (range (mth/floor (max x0 0)) (mth/ceil (min x1 width)))]
      (let [b0 (mth/floor (/ (file-ms x) bucket-ms))
            b1 (max (inc b0) (mth/floor (/ (file-ms (inc x)) bucket-ms)))]
        (when (and (< b0 buckets) (pos? b1))
          (let [[lo hi] (reduce (fn [[lo hi] ^js data]
                                  (loop [b (max 0 b0) lo lo hi hi]
                                    (if (< b (min b1 buckets))
                                      (recur (inc b)
                                             (min lo (aget data (* 2 b)))
                                             (max hi (aget data (inc (* 2 b)))))
                                      [lo hi])))
                                [1 -1]
                                channels)]
            (when (<= lo hi)
              (let [y0 (- mid (* hi amp))
                    y1 (- mid (* lo amp))]
                (.fillRect ctx x y0 1 (max 1 (- y1 y0)))))))))))

(def ^:private fade-shade
  "What shades the quiet part of a fade: dark in both themes, so the name
  of the clip over it stays as readable as on the rest of it."
  "rgb(0, 0, 0)")

(defn- draw-fade!
  "The fade over the box from `x0` to `x1`, between `top` and `bottom`, of
  a clip: the part above the ramp from silence at `x0` to full volume at
  `x1` (back, when `x1` is left of `x0`) shaded, the ramp drawn."
  [^js ctx ink x0 x1 top bottom]
  (.beginPath ctx)
  (.moveTo ctx x0 bottom)
  (.lineTo ctx x1 top)
  (.lineTo ctx x0 top)
  (.closePath ctx)
  (set! (.-fillStyle ctx) fade-shade)
  (faded! ctx 0.3 #(.fill ctx))
  (.beginPath ctx)
  (.moveTo ctx x0 bottom)
  (.lineTo ctx x1 top)
  (set! (.-strokeStyle ctx) ink)
  (set! (.-lineWidth ctx) 1)
  (faded! ctx 0.8 #(.stroke ctx)))

(defn- draw-clip!
  "The audio `clip` in the row with its top at `y`: a bar over its span
  with its waveform (diagonal lines until its file is decoded, in the
  colour of errors when it stays silent, see `problem`), its volume line
  (see `volume-y`, shaded above) with its fades ramping to it, the part
  past the end of the timeline dimmed, faded while muted, its name, and
  grips to trim it but while locked; while it is selected or under the
  pointer, the grips of its fades too (see `fade-grips`)."
  [^js ctx geo {:keys [duration waveform problem selected-clips hover]} palette font y clip]
  (let [x0      (time->x geo (:start clip))
        x1      (max (time->x geo (ctau/clip-end clip)) (+ x0 min-bar-width))
        top     (+ y bar-inset)
        height  (- (row-height geo) (* 2 bar-inset))
        active? (contains? selected-clips (:id clip))
        ink     (if active? (:on-accent palette) (:fg-strong palette))
        peaks   (waveform (:audio-id clip))]
    (when (and (< x0 (:width geo)) (pos? x1))
      (faded! ctx (if (:muted clip) 0.4 1)
              (fn []
                (fill-round-rect! ctx (if active? (:accent palette) (:audio palette)) x0 top (- x1 x0) height 6)
                (.save ctx)
                (.beginPath ctx)
                (.roundRect ctx x0 top (- x1 x0) height 6)
                (.clip ctx)
                (if (some? peaks)
                  (faded! ctx 0.8 #(draw-waveform! ctx geo peaks clip x0 x1 (+ top 3) (- (+ top height) 3)
                                                   (if active? (:on-accent palette) (:wave palette))))
                  (let [silent? (and (some? problem) (some? (problem clip)))]
                    (faded! ctx (if silent? 0.8 0.5)
                            #(draw-hatching! ctx (if silent? (:error palette) (:wave palette))
                                             x0 top (- x1 x0) height))))
                (let [vy  (volume-y top height clip)
                      xi  (time->x geo (+ (:start clip) (or (:fade-in clip) 0)))
                      xo  (time->x geo (- (ctau/clip-end clip) (or (:fade-out clip) 0)))
                      on? (and (= :volume (:mode hover)) (= (:id clip) (:id (:clip hover))))]
                  ;; quieter than full: the room above the line shaded
                  (when (>= vy (+ top 3.5))
                    (faded! ctx 0.3 #(fill-rect! ctx fade-shade x0 top (- x1 x0) (- vy top))))
                  (when (:fade-in clip)
                    (draw-fade! ctx ink x0 xi vy (+ top height)))
                  (when (:fade-out clip)
                    (draw-fade! ctx ink x1 xo vy (+ top height)))
                  ;; the volume line between the fades, bolder under the pointer
                  (.beginPath ctx)
                  (.moveTo ctx (if (:fade-in clip) xi x0) vy)
                  (.lineTo ctx (if (:fade-out clip) xo x1) vy)
                  ;; louder than its file: orange
                  (set! (.-strokeStyle ctx) (if (> (or (:volume clip) 1) 1) (:snap palette) ink))
                  (set! (.-lineWidth ctx) (if on? 2 1))
                  (faded! ctx (if (or on? active?) 0.9 0.5) #(.stroke ctx)))
                ;; the end of the timeline cuts it: what is past it plays not
                (let [xd (time->x geo duration)]
                  (when (< xd x1)
                    (faded! ctx 0.6 #(fill-rect! ctx (:bg palette) (max x0 xd) top (- x1 (max x0 xd)) height))))
                (.restore ctx)
                (when-not (:locked clip)
                  (let [hovered? (and (= :audio (:type hover)) (= (:id clip) (:id (:clip hover))))]
                    (faded! ctx (if hovered? 1 0.6)
                            (fn []
                              (let [grip-y (+ top (* 0.3 height))
                                    grip-h (* 0.4 height)]
                                (fill-round-rect! ctx ink (+ x0 6) grip-y 2 grip-h 1)
                                (fill-round-rect! ctx ink (- x1 8) grip-y 2 grip-h 1))))
                    ;; in the ink on the accent with the outline of the
                    ;; theme's strongest ink, so they show on the bar and on
                    ;; the row in both themes
                    (when (or active? hovered?)
                      (doseq [grip-x (fade-grips geo clip)]
                        (.beginPath ctx)
                        (.arc ctx grip-x (+ top 1) fade-grip 0 (* 2 mth/PI))
                        (set! (.-fillStyle ctx) (:on-accent palette))
                        (.fill ctx)
                        (set! (.-strokeStyle ctx) (:fg-strong palette))
                        (set! (.-lineWidth ctx) 1.5)
                        (.stroke ctx)))))
                (let [room (- x1 x0 32)
                      ;; its speed when it is not the one of its file, as a factor
                      name (if (= 1 (ctau/clip-speed clip))
                             (:name clip)
                             (dm/str (:name clip) " · " (/ (mth/round (* 100 (ctau/clip-speed clip))) 100) "×"))]
                  (when (pos? room)
                    (set! (.-font ctx) (dm/str "11px " font))
                    (when-let [text (fit-text ctx name room)]
                      (set! (.-fillStyle ctx) ink)
                      (set! (.-textAlign ctx) "left")
                      (set! (.-textBaseline ctx) "top")
                      (.fillText ctx text (+ x0 14) (+ top 2))))))))))

(defn- draw-lane!
  [ctx geo scene palette font index row]
  (let [y       (row-y geo index)
        hover   (let [hover (:hover scene)]
                  (when (= index (:row hover)) hover))
        active? (contains? (:selected scene) (:id row))]
    (case (:type row)
      :layer
      (do
        ;; the animation of the main of a component copy, which the
        ;; copy plays as it is
        (when-let [[start end] (:copy row)]
          (draw-bar! ctx geo y start end
                     (if active? (:component palette) (:component-muted palette))
                     nil false false))
        (when-let [[start end] (:range row)]
          (draw-bar! ctx geo y start end
                     (if active? (:accent palette) (:bar palette))
                     (if active? (:on-accent palette) (:fg palette))
                     (= :bar (:type hover))
                     true)
          (when (contains? (:selected-tracks scene) (:id row))
            (outline-bar! ctx geo y start end (:select palette)))))

      :animation
      (let [animation (:animation row)
            ink       (if active? (:on-accent palette) (:fg-strong palette))]
        (faded! ctx (if (:hidden animation) 0.5 1)
                (fn []
                  (let [[x0 x1] (draw-bar! ctx geo y (:start animation) (cta/animation-end animation)
                                           (if active? (:accent palette) (:bar palette))
                                           ink
                                           (= :animation (:type hover))
                                           (not (:locked animation)))]
                    (draw-block-label! ctx ((:animation-label scene) animation) ink font
                                       x0 x1 (+ y (/ (row-height geo) 2)))
                    (when (contains? (:selected-anims scene)
                                     {:shape-id (:id row) :animation-id (:id animation)})
                      (outline-bar! ctx geo y (:start animation) (cta/animation-end animation)
                                    (:select palette)))))))

      :property
      (draw-property! ctx geo scene palette row y
                      (if active? (:accent palette) (:fg palette))
                      hover)

      :audio
      (doseq [clip (:clips row)]
        (draw-clip! ctx geo scene palette font y clip))

      ;; collapsed, an overview of all the clips, each a slim bar
      :audio-group
      (when (:collapsed? row)
        (let [mid (+ y (/ (row-height geo) 2))]
          (doseq [clip (:clips row)
                  :let [x0 (time->x geo (:start clip))
                        x1 (max (time->x geo (ctau/clip-end clip)) (+ x0 2))]
                  :when (and (< x0 (:width geo)) (pos? x1))]
            (faded! ctx (if (:muted clip) 0.3 0.7)
                    #(fill-round-rect! ctx (:wave palette) x0 (- mid 2) (- x1 x0) 4 2))))))))

(defn- draw-rows!
  [^js ctx geo {:keys [rows selected duration span markers playhead snap-line
                       hover hover-row marquee] :as scene}
   palette font dpr]
  (let [{:keys [width height scroll-x scroll-y]} geo
        [start end] (visible-rows geo 0)
        end         (min end (count rows))
        rows-height (- height header-height)]
    (.save ctx)
    (.beginPath ctx)
    (.rect ctx 0 header-height width rows-height)
    (.clip ctx)

    (fill-rect! ctx (:bg palette) 0 header-height width rows-height)
    (doseq [index (range start end)
            :let [row   (get rows index)
                  color (cond
                          (and (= :layer (:type row)) (contains? selected (:id row)))
                          (:bg-selected palette)

                          (= index hover-row)
                          (:bg-hover palette))]
            :when (some? color)]
      (fill-rect! ctx color 0 (row-y geo index) width (row-height geo)))

    ;; Under the bars and keyframes: before the start, where something
    ;; lies there, and past the duration, up to the right edge so no row
    ;; shows its background after the end of the axis, the end, which a
    ;; drag on it moves (see `hit`), and the markers.
    (when (neg? (:origin geo 0))
      (fill-rect! ctx (:shade palette) 0 header-height (max 0 (time->x geo 0)) rows-height))
    (when (> (+ (:origin geo 0) span) duration)
      (let [x (time->x geo duration)]
        (fill-rect! ctx (:shade palette) x header-height (max 0 (- width x)) rows-height)))
    (let [x (crisp (time->x geo duration) dpr)]
      (if (= :duration (:type hover))
        (fill-rect! ctx (:accent palette) (dec x) header-height 2 rows-height)
        (fill-rect! ctx (:border palette) x header-height 1 rows-height)))
    (faded! ctx 0.6
            (fn []
              (doseq [{:keys [time]} markers]
                (dashed-line! ctx (:marker palette) (crisp (time->x geo time) dpr) header-height height))))

    (doseq [index (range start end)]
      (draw-lane! ctx geo scene palette font index (get rows index)))

    ;; Over them: where a drag snaps to, a box selection and, on top of
    ;; all, the playhead.
    (when (some? snap-line)
      (dashed-line! ctx (:snap palette) (crisp (time->x geo snap-line) dpr) header-height height))

    (when-let [{:keys [left top right bottom]} marquee]
      (let [x (- left scroll-x)
            y (- (+ header-height top) scroll-y)
            w (- right left)
            h (- bottom top)]
        (fill-rect! ctx (:select-fill palette) x y w h)
        (set! (.-strokeStyle ctx) (:select palette))
        (set! (.-lineWidth ctx) 1)
        (.strokeRect ctx (+ x 0.5) (+ y 0.5) (max 0 (dec w)) (max 0 (dec h)))))

    ;; a rim of the background keeps it apart from a bar of its colour
    (let [x (crisp (time->x geo playhead) dpr)]
      (faded! ctx 0.8 #(fill-rect! ctx (:bg palette) (dec x) header-height 3 rows-height))
      (fill-rect! ctx (:playhead palette) x header-height 1 rows-height))
    (.restore ctx)))

(defn- draw-pin!
  "The pin of the playhead on top of its line, from `x`."
  [^js ctx color x]
  (let [left  (- x 6)
        right (+ x 7)]
    (set! (.-fillStyle ctx) color)
    (.beginPath ctx)
    (.moveTo ctx left 7.8)
    (.lineTo ctx left 3)
    (.arcTo ctx left 0 (+ left 3) 0 3)
    (.lineTo ctx (- right 3) 0)
    (.arcTo ctx right 0 right 3 3)
    (.lineTo ctx right 7.8)
    (.lineTo ctx (+ left 6.5) 13)
    (.closePath ctx)
    (.fill ctx)))

(defn- draw-ruler!
  "The ruler: a tick every half `tick` ms, labelled every `tick`, the time
  before the start and past the duration shaded and the playhead."
  [^js ctx geo {:keys [span duration tick tick-label playhead]} palette font dpr]
  (let [width  (:width geo)
        half   (/ tick 2)
        origin (:origin geo 0)
        end    (+ origin span)]
    (.save ctx)
    (.beginPath ctx)
    (.rect ctx 0 0 width ruler-height)
    (.clip ctx)

    (fill-rect! ctx (:bg palette) 0 0 width ruler-height)
    (when (neg? origin)
      (let [x (time->x geo origin)]
        (fill-rect! ctx (:shade palette) x 0 (- (time->x geo 0) x) (dec ruler-height))))
    (when (> end duration)
      (let [x (time->x geo duration)]
        (fill-rect! ctx (:shade palette) x 0 (- (time->x geo end) x) (dec ruler-height))))

    (set! (.-font ctx) (dm/str "10px " font))
    (set! (.-textAlign ctx) "left")
    (set! (.-textBaseline ctx) "top")
    ;; From a tick before the view, whose label may reach into it.
    (let [from (max (* half (mth/ceil (/ origin half)))
                    (- (* half (mth/floor (/ (x->time geo 0) half))) tick))
          to   (min end (+ (x->time geo width) half))]
      (doseq [time (range from (+ to (/ half 2)) half)]
        (let [major? (zero? (mod time tick))
              x      (crisp (time->x geo time) dpr)]
          (fill-rect! ctx (if major? (:fg palette) (:border palette)) x 0 1 (if major? 7 4))
          (when (and major? (< time end))
            (set! (.-fillStyle ctx) (:fg palette))
            (.fillText ctx (tick-label time) (+ x 5) 8)))))

    (fill-rect! ctx (:border palette) 0 (dec ruler-height) width 1)
    (let [x (crisp (time->x geo playhead) dpr)]
      (fill-rect! ctx (:playhead palette) x 0 1 (dec ruler-height))
      (draw-pin! ctx (:playhead palette) x))
    (.restore ctx)))

(defn- fit-canvas!
  "Size `canvas` to `width` × `height` px, of `dpr` device pixels each."
  [^js canvas width height dpr]
  (let [w     (mth/round (* width dpr))
        h     (mth/round (* height dpr))
        style (.-style canvas)
        css-w (dm/str width "px")
        css-h (dm/str height "px")]
    (when (not= w (.-width canvas))
      (set! (.-width canvas) w))
    (when (not= h (.-height canvas))
      (set! (.-height canvas) h))
    (when (not= css-w (.-width style))
      (set! (.-width style) css-w))
    (when (not= css-h (.-height style))
      (set! (.-height style) css-h))))

(defn draw!
  "Draw the ruler and the rows in view on `canvas`, placed by `geo` (see the
  namespace doc). `scene` is what they show: the `:rows` (see
  `timeline-rows`) of the `:timeline`, the `:selected` layers and
  `:selected-kfs` keyframes, the `:duration` and the `:span` of the time
  axis, its `:tick` spacing and `:tick-label`s, the `:animation-label`s,
  the `:markers`, the `:playhead`, the `:snap-line`, the open `:easing`
  editor, the row the pointer is over (`:hover-row`) and what is under it
  (`:hover`, see `hit`), and a box selection (`:marquee`, see
  `content-point`). `palette` has the colours (see `read-palette`), `font`
  the font family of the labels."
  [^js canvas geo scene palette font]
  (let [dpr     (or (.-devicePixelRatio js/window) 1)
        width   (:width geo)
        height  (:height geo)
        ^js ctx (.getContext canvas "2d")]
    (fit-canvas! canvas width height dpr)
    (.setTransform ctx dpr 0 0 dpr 0 0)
    (.clearRect ctx 0 0 width height)
    (draw-rows! ctx geo scene palette font dpr)
    (draw-ruler! ctx geo scene palette font dpr)))
