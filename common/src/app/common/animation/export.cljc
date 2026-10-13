;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.animation.export
  "What the markup exports share (see `app.common.animation.svg` and
  `app.common.animation.lottie`): which shapes are layers and what holds
  them, the sizes they play on their outlines and the boards they follow
  as those resize, with the geometry both draw them by."
  (:require
   [app.common.animation.css :as acss]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.uuid :as uuid]))

(def kappa
  "How far the control points of a cubic curve of a quarter of a circle
  are from its ends, by its radius."
  0.5522847498)

(declare svg-sized? svg-sized-ids svg-holder-ids board-follow svg-stroke-alignment svg-even-corners? unturned?)

(defn board-inner-shadows?
  "Whether the board `shape` casts an inner shadow, which the canvas
  casts inside its fills, under the layers in it."
  [shape]
  (and (= :frame (:type shape))
       (some #(and (= :inner-shadow (:style %)) (not (:hidden %))) (cta/as-vec (:shadow shape)))
       (some (complement :hidden) (cta/as-vec (:fills shape)))))

(defn export-holders
  "The shapes the layers in which are layers of an SVG or Lottie export of
  `timeline` (see `export-layer-ids`): the boards whose size it plays on
  their outlines, which they follow (see `svg-sized?`); the boards of
  `objects` in its board, it too, that cast an inner shadow (see
  `board-inner-shadows?`) and what holds them, so the shadow goes between
  a board and what is in it; and the masked groups in it with what holds
  them, up to the board, so that a masked group is a layer and what it
  holds too, which its mask keeps to it (an SVG `<mask>` or a Lottie
  matte)."
  [objects timeline]
  (let [board-id (:board-id timeline)
        tree     (tree-seq #(seq (dm/get-in objects [% :shapes]))
                           #(dm/get-in objects [% :shapes])
                           board-id)
        boards   (filterv #(board-inner-shadows? (get objects %)) tree)
        masked   (filterv #(true? (dm/get-in objects [% :masked-group])) tree)
        up       (fn [id]
                   (->> (iterate #(dm/get-in objects [% :parent-id]) id)
                        (take-while #(and (some? %) (contains? objects %)))
                        (reduce (fn [ids id]
                                  (cond-> (conj ids id) (= id board-id) (reduced)))
                                [])))]
    (-> (svg-holder-ids objects (svg-sized-ids objects timeline))
        (into boards)
        (into (comp (remove #{board-id}) (keep #(dm/get-in objects [% :parent-id]))) boards)
        (into (mapcat up) masked))))

(defn export-layer-ids
  "The layers of an SVG or Lottie export of `timeline` (with its
  keyframes, see `cta/resolve-animations`): its tracked shapes and their
  siblings (see `cta/export-layer-ids`), and the layers in the shapes of
  `export-holders`."
  [objects timeline]
  (cta/export-layer-ids objects (:board-id timeline) (:tracks timeline)
                        (export-holders objects timeline)))

(def blur-sigma-scale
  "How render-wasm turns the value of a layer blur into the standard
  deviation of its Gaussian (`radius_to_sigma`): 1/√3 of it, plus 0.5."
  0.57735027)

(defn blur-sigma
  "The standard deviation of the Gaussian of a layer blur of `value` on
  the canvas (render-wasm's `radius_to_sigma`): none without a value."
  [value]
  (if (pos? value)
    (+ (* blur-sigma-scale value) 0.5)
    0))

(defn pivot-offset
  "Where the track origin `origin` (0..1 on the box of `shape`, see
  `track-origin`) is from the top left of the box, turned as the shape
  is around its center: what the pivot is in the picture of the shape."
  [shape origin]
  (let [sr    (:selrect shape)
        sw    (double (or (:width sr) 0))
        sh    (double (or (:height sr) 0))
        dx    (* (- (or (:x origin) 0.5) 0.5) sw)
        dy    (* (- (or (:y origin) 0.5) 0.5) sh)
        angle (mth/radians (or (:rotation shape) 0))
        cos   (mth/cos angle)
        sin   (mth/sin angle)]
    [(+ (/ sw 2) (- (* dx cos) (* dy sin)))
     (+ (/ sh 2) (+ (* dx sin) (* dy cos)))]))

(defn lottie-pivot
  "Layer-space point of the track origin on `shape` as it is, turned with
  it (see `pivot-offset`)."
  [shape ox oy origin]
  (let [sr      (:selrect shape)
        [dx dy] (pivot-offset shape origin)]
    [(+ (- (or (:x sr) 0) ox) dx)
     (+ (- (or (:y sr) 0) oy) dy)]))

(defn canvas-outline
  "The outline of `shape` where it is on the canvas, as path content."
  [shape]
  (if (contains? #{:path :bool} (:type shape))
    (:content shape)
    (:content (path/convert-to-path (cond-> shape (= :frame (:type shape)) (assoc :type :rect))))))

(defn clipping-board?
  [shape]
  (and (= :frame (:type shape)) (not (true? (:show-content shape)))))

(defn layer-nodes
  "The layers inside `id` as the Lottie and SVG exports nest them, the
  bottom-most first: a layer is `{:id id}`, with `:inner` nodes when
  layers sit inside it, so they follow it; a board that clips layers but
  is none itself is `{:id id :clip? true :inner nodes}`; any other shape
  gives the layers inside it in its place."
  [objects layer-ids id]
  (into []
        (mapcat (fn [cid]
                  (let [inner (layer-nodes objects layer-ids cid)]
                    (cond
                      (contains? layer-ids cid)
                      [(cond-> {:id cid} (seq inner) (assoc :inner inner))]

                      (and (seq inner) (clipping-board? (get objects cid)))
                      [{:id cid :clip? true :inner inner}]

                      :else inner))))
        (cta/painted-children objects id)))

(defn unit-text-ids
  "The texts `images` has pictures of the units of (`[text-id k]`, see
  `timeline->svg`)."
  [images]
  (into #{}
        (keep (fn [key] (when (vector? key) (first key))))
        (keys images)))

(defn turned-moves
  "The timeline of the units of a text (see `unit-timelines`) turned
  `angle` degrees with it: the offsets of its units, in the text, turned
  as they move in the picture of the turned text."
  [units angle]
  (if (mth/almost-zero? (mod (or angle 0) 360))
    units
    (let [radians (mth/radians angle)
          cos     (mth/cos radians)
          sin     (mth/sin radians)]
      (update units :tracks d/update-vals
              (fn [{:keys [keyframes] :as track}]
                (let [xs (cta/sort-keyframes (filter #(= :x (:property %)) keyframes))
                      ys (cta/sort-keyframes (filter #(= :y (:property %)) keyframes))]
                  (if (and (empty? xs) (empty? ys))
                    track
                    (let [at     (fn [keyframes time] (or (cta/property-value-at keyframes time) 0))
                          easing (fn [time]
                                   (-> (or (d/seek #(= time (:time %)) xs) (d/seek #(= time (:time %)) ys))
                                       (select-keys [:easing :interpolation])))
                          turned (mapcat (fn [time]
                                           (let [x (at xs time)
                                                 y (at ys time)]
                                             [(cta/make-keyframe (merge (easing time) {:time time :property :x
                                                                                       :value (- (* x cos) (* y sin))}))
                                              (cta/make-keyframe (merge (easing time) {:time time :property :y
                                                                                       :value (+ (* x sin) (* y cos))}))]))
                                         (into (sorted-set) (map :time) (concat xs ys)))]
                      (assoc track :keyframes
                             (cta/sort-keyframes (into (filterv #(not (#{:x :y} (:property %))) keyframes) turned)))))))))))
(def svg-radius-properties #{:r1 :r2 :r3 :r4})

(defn svg-even-corners?
  "Whether an SVG of vectors plays the corners `track` animates on
  `shape` (see `svg-appearance-css`): a rectangle or board whose four
  corners are alike at rest and at each keyframe, which the SVG renderer
  draws as a `rect`."
  [shape track]
  (let [corners (fn [values] (map #(get values % (get shape % 0)) [:r1 :r2 :r3 :r4]))
        even?   #(apply = (corners %))]
    (and (contains? #{:rect :frame} (:type shape))
         (even? {})
         (every? (fn [time] (even? (into {} (keep (fn [kf] (when (= time (:time kf)) [(:property kf) (:value kf)])))
                                         (:keyframes track))))
                 (into #{} (comp (filter #(svg-radius-properties (:property %))) (map :time)) (:keyframes track))))))

(def svg-size-properties #{:width :height})

(defn unturned?
  "Whether `shape` is neither turned nor flipped: its box is its selrect."
  [shape]
  (let [turn (mod (or (:rotation shape) 0) 360)]
    (and (or (mth/almost-zero? turn) (mth/close? turn 360))
         (or (nil? (:transform shape)) (gmt/unit? (:transform shape))))))

(defn- plain-paint?
  "Whether a fill or stroke paints a plain colour: gradients and images are
  laid out over the shape at rest."
  [paint]
  (and (nil? (:fill-color-gradient paint)) (nil? (:fill-image paint))
       (nil? (:stroke-color-gradient paint)) (nil? (:stroke-image paint))))

(defn svg-stroke-alignment
  "Where the SVG renderer draws `stroke` of `shape`: centered when it
  draws nothing or the shape is an open path."
  [shape stroke]
  (if (and (pos? (or (:stroke-width stroke) 0))
           (not= :none (:stroke-style stroke :none))
           (not (and (= :path (:type shape)) (path/shape-with-open-path? shape))))
    (:stroke-alignment stroke :center)
    :center))

(defn svg-sized?
  "Whether an SVG of vectors plays the width and height `track` animates
  on `shape` on its elements, as the canvas resizes it (see
  `svg-appearance-css`): an unturned rectangle, ellipse or board, its
  corners alike (see `svg-even-corners?`), of one fill at most, in plain
  colours and without an outer stroke, whose mask the SVG renderer
  bounds. The layers in such a board follow it as their constraints and
  layouts say (see `timeline->svg`). Otherwise width and height play as
  a scale of its group, which stretches it."
  [shape track]
  (let [sr (:selrect shape)]
    (boolean
     (and (some #(svg-size-properties (:property %)) (:keyframes track))
          (contains? #{:rect :circle :frame} (:type shape))
          (unturned? shape)
          (pos? (or (:width sr) 0))
          (pos? (or (:height sr) 0))
          (<= (count (:fills shape)) 1)
          (every? plain-paint? (:fills shape))
          (every? #(and (plain-paint? %) (not= :outer (svg-stroke-alignment shape %)))
                  (:strokes shape))
          (or (= :circle (:type shape)) (svg-even-corners? shape track))))))

(defn- unflipped?
  "Whether `shape` is turned at most, not flipped (turned by half a turn
  when flipped both ways)."
  [shape]
  (let [{:keys [a b c d]} (or (:transform shape) (gmt/matrix))]
    (pos? (- (* a d) (* b c)))))

(defn- box-paints?
  "Whether the fills of `shape` paint over its box as its width and height
  play in an SVG of vectors: but images, which keep the size they rest
  at, the pattern of more than one or of a gradient laid over the box of
  the element it fills (see `asvg/svg-sized-paint`); of a turned path, a
  plain colour at most, as the pattern of a path turns with it."
  [shape]
  (let [fills (:fills shape)]
    (and (not-any? :fill-image fills)
         (or (not (contains? #{:path :bool} (:type shape)))
             (unturned? shape)
             (and (<= (count fills) 1) (every? plain-paint? fills))))))

(defn svg-box-sized?
  "Whether an SVG of vectors plays the width and height `track` animates
  on `shape` on its elements as `svg-sized?` does, which Lottie shares,
  where only an SVG can: a rectangle (unturned, its corners alike) or an
  ellipse (turned too, in its own space) whose fills paint over its box
  (see `box-paints?`) or with an outer stroke, the masks of which an SVG
  lays over what it covers as it plays (see `asvg/svg-sized-paint`)."
  [shape track]
  (let [sr (:selrect shape)]
    (boolean
     (and (some #(svg-size-properties (:property %)) (:keyframes track))
          (not (svg-sized? shape track))
          (case (:type shape)
            :rect   (and (unturned? shape) (svg-even-corners? shape track))
            :circle (unflipped? shape)
            false)
          (pos? (or (:width sr) 0))
          (pos? (or (:height sr) 0))
          (box-paints? shape)
          (every? plain-paint? (:strokes shape))))))

(defn svg-outline-sized?
  "Whether an SVG of vectors plays the width and height `track` animates
  on `shape` on the path of its outline, by SMIL, as the canvas resizes it
  (see `asvg/svg-smil`), its strokes as wide, when it can not on its
  elements (see `svg-sized?`, `svg-box-sized?`): a path, a boolean with
  its outline, or a rectangle (turned, or its corners not alike as they
  play, drawn as a path), whose fills paint over its box (see
  `box-paints?`), its strokes in plain colours, the masks of outer ones
  laid over what it covers as it plays (see `asvg/svg-sized-paint`).
  Otherwise width and height play as a scale of its group, which
  stretches it, its strokes too."
  [shape track]
  (let [sr (:selrect shape)]
    (boolean
     (and (some #(svg-size-properties (:property %)) (:keyframes track))
          (not (svg-sized? shape track))
          (not (svg-box-sized? shape track))
          (case (:type shape)
            :path true
            :bool (some? (:content shape))
            :rect true
            false)
          (pos? (or (:width sr) 0))
          (pos? (or (:height sr) 0))
          (box-paints? shape)
          (every? plain-paint? (:strokes shape))))))

(def ^:private geometry-properties
  "The properties that move, turn or resize a layer."
  #{:x :y :width :height :scale-x :scale-y :rotation})

(defn- expand-group-size
  "`timeline` with the width and height `timeline` animates on the group
  `gid` also on its layers, as the canvas resizes them with it: each
  takes them scaled to its own size, from the same keyframes (times and
  easings) and around the pivot of the group, which its own becomes (a
  share of its box, past it too). Nil when it can not: the group is no
  plain one (a masked group, a turned one, one that scales), or a layer
  of it moves, turns or resizes on its own, is turned (the canvas skews
  it) or has no size along an axis that plays."
  [timeline objects gid]
  (let [group    (get objects gid)
        track    (dm/get-in timeline [:tracks gid])
        kfs      (:keyframes track)
        sizes    (filterv #(svg-size-properties (:property %)) kfs)
        animated (into #{} (map :property) sizes)
        {:keys [x y width height]} (:selrect group)
        origin   (cta/track-origin track)
        px       (+ x (* width (:x origin)))
        py       (+ y (* height (:y origin)))
        kids     (mapv #(get objects %) (:shapes group))
        sized?   (fn [sr]
                   (and (or (not (animated :width)) (pos? (or (:width sr) 0)))
                        (or (not (animated :height)) (pos? (or (:height sr) 0)))))]
    (when (and (= :group (:type group))
               (not (:masked-group group))
               (seq sizes)
               (not-any? #(contains? #{:scale-x :scale-y} (:property %)) kfs)
               (unturned? group)
               (sized? (:selrect group))
               (seq kids)
               (every? (fn [kid]
                         (and (some? kid)
                              (unturned? kid)
                              (sized? (:selrect kid))
                              (not-any? #(geometry-properties (:property %))
                                        (dm/get-in timeline [:tracks (:id kid) :keyframes]))))
                       kids))
      (reduce (fn [timeline kid]
                (let [sr     (:selrect kid)
                      share  (fn [pivot from size]
                               (if (pos? (or size 0)) (/ (- pivot from) size) 0.5))
                      scaled (mapv (fn [kf]
                                     (let [[base own] (if (= :width (:property kf))
                                                        [width (:width sr)]
                                                        [height (:height sr)])]
                                       (assoc kf :id (uuid/next) :value (* own (/ (:value kf) base)))))
                                   sizes)]
                  (update-in timeline [:tracks (:id kid)]
                             (fn [kt]
                               (-> (or kt {:shape-id (:id kid) :keyframes []})
                                   (update :keyframes #(cta/sort-keyframes (into (or % []) scaled)))
                                   (assoc :origin {:x (share px (:x sr) (:width sr))
                                                   :y (share py (:y sr) (:height sr))}))))))
              timeline
              kids))))

(defn expand-group-sizes
  "`timeline` (its animations resolved into keyframes, see
  `cta/resolve-animations`) with the width and height it plays on groups
  also on their layers, as the canvas resizes them with the group (see
  `expand-group-size`), the layers in groups inside them too: an SVG of
  vectors or a Lottie then resizes each as it can, its strokes as wide,
  where a scale of the group would stretch them all. Those groups keep
  their keyframes (they are layers still) but play no size, their tracks
  marked (see `sized-on-layers?`, `sized-groups`). Once only: a timeline
  so expanded stays."
  [timeline objects]
  (if (contains? timeline ::sized-groups)
    timeline
    (let [;; looping sizes unrolled first, so the layers repeat them too
          timeline (-> timeline (cta/resolve-animations objects) cta/expand-loops)
          board-id (:board-id timeline)
          ;; the outer groups first: they give those inside them a size
          ids      (rest (tree-seq #(seq (dm/get-in objects [% :shapes]))
                                   #(dm/get-in objects [% :shapes])
                                   board-id))]
      (reduce (fn [timeline id]
                (if-let [expanded (when (some #(svg-size-properties (:property %))
                                              (dm/get-in timeline [:tracks id :keyframes]))
                                    (expand-group-size timeline objects id))]
                  (-> expanded
                      (update ::sized-groups conj id)
                      (assoc-in [:tracks id ::sized-on-layers] true))
                  timeline))
              (assoc timeline ::sized-groups #{})
              ids))))

(defn sized-on-layers?
  "Whether the group of `track` plays its width and height on its layers,
  not on itself (see `expand-group-sizes`)."
  [track]
  (true? (::sized-on-layers track)))

(defn sized-groups
  "The groups whose size `timeline` plays on their layers (see
  `expand-group-sizes`), not on themselves."
  [timeline]
  (into #{}
        (keep (fn [[id track]] (when (sized-on-layers? track) id)))
        (:tracks timeline)))

(defn svg-sized-ids
  "The shapes whose width and height `timeline` (with its keyframes, see
  `resolve-animations`) plays on their elements in an SVG of vectors
  (see `svg-sized?`)."
  [objects timeline]
  (into #{}
        (keep (fn [[id track]]
                (when (some-> (get objects id) (svg-sized? track)) id)))
        (:tracks timeline)))

(defn svg-holder-ids
  "The boards among the shapes `sized` (see `svg-sized-ids`): the layers
  in them follow them (see `svg-follow-css`)."
  [objects sized]
  (filterv #(= :frame (dm/get-in objects [% :type])) sized))

(defn- svg-matrix
  "The transform of `modifiers` (on the page; none, nil) in the SVG, whose
  origin is `ox`/`oy`: `[a b c d e f]`."
  [modifiers ox oy]
  (let [{:keys [a b c d e f]} (if (some? modifiers)
                                (ctm/modifiers->transform modifiers)
                                (gmt/matrix))]
    [a b c d (- (+ (* a ox) (* c oy) e) ox) (- (+ (* b ox) (* d oy) f) oy)]))

(defn board-follow
  "How the layers among `ids` in the boards `holders`, whose size plays
  on their outlines (see `svg-sized?`), move with them, as the canvas
  moves them as their constraints and layouts say: `propagate` gives a
  modif-tree with the modifiers of what is in the shapes it modifies
  (`app.common.geom.modifiers/set-objects-modifiers`), whose transforms
  each layer goes through at the stops of the size of its board (see
  `eased-stops`), in a space whose origin is `ox`/`oy` (see
  `svg-matrix`). `[[layer-id [[time keyframe matrix]...]]...]`, the
  layers that move."
  [timeline objects holders ids propagate ox oy]
  (let [duration  (max 1 (:duration timeline))
        layer?    (set ids)
        identity? (fn [matrix] (every? true? (map mth/close? matrix [1 0 0 1 0 0])))]
    (for [hid   holders
          :let  [holder   (get objects hid)
                 track    (dm/get-in timeline [:tracks hid])
                 kfs      (filterv #(svg-size-properties (:property %)) (:keyframes track))
                 children (filterv layer? (:shapes holder))]
          :when (and (some? holder) (seq kfs) (seq children))
          :let  [{:keys [width height]} (:selrect holder)
                 pivot  (cta/origin-point holder (cta/track-origin track))
                 values (acss/shape-values timeline hid)
                 size   #(let [v (values %)] [(get v :width width) (get v :height height)])
                 poses  (for [[t kf] (acss/eased-stops #(mapv double (size %))
                                                       kfs
                                                       (vec (into (sorted-set 0 duration)
                                                                  (comp (map :time) (filter #(<= 0 % duration)))
                                                                  kfs)))]
                          (let [[w h] (size t)]
                            [t kf (propagate {hid {:modifiers (ctm/resize (ctm/empty)
                                                                          (gpt/point (/ w width) (/ h height))
                                                                          pivot)}}
                                             objects)]))]
          cid   children
          :let  [stops (mapv (fn [[t kf tree]] [t kf (svg-matrix (dm/get-in tree [cid :modifiers]) ox oy)])
                             poses)]
          :when (not-every? (comp identity? peek) stops)]
      [cid stops])))
