;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.animation.svg
  "The animated SVG export of a timeline (see `timeline->svg`): the
  vectors Penpot's SVG renderer draws, played by CSS and, where CSS can
  not, by SMIL."
  (:require
   [app.common.animation.css :as acss]
   [app.common.animation.export :as aexp]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes.bounds :as gsb]
   [app.common.geom.shapes.corners :as gco]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.color :as clr]
   [app.common.types.path :as path]
   [clojure.string :as str]))

(defn- svg-escape
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "\"" "&quot;")
      (str/replace "<" "&lt;")))

(defn- svg-clip-id
  [id]
  (str "penpot-clip-" id))

(defn- unit-image-indexes
  "The indexes of the units of the text `text-id` that `images` has a
  picture of, in order: a unit of no ink has none."
  [images text-id]
  (sort (keep (fn [key]
                (when (and (vector? key) (= text-id (first key)) (int? (second key)))
                  (second key)))
              (keys images))))

(defn unit-css-class
  "The CSS class of the unit `k` of the text `text-id` in the SVG export
  (see `timeline->svg`)."
  [text-id k]
  (str "penpot-unit-" text-id "-" k))

(defn- svg-vectors
  "The vectors of a layer (`:markup`, see `timeline->svg`), drawn where
  they are on the page, moved to where they are in the SVG, whose origin
  is `ox`/`oy`; nothing draws without a fill of its own."
  [markup ox oy]
  (str "<g transform=\"translate(" (acss/fmt (- ox)) " " (acss/fmt (- oy)) ")\" fill=\"none\">" markup "</g>"))

(defn- svg-image
  [img]
  (str "<image href=\"" (svg-escape (or (:href img) ""))
       "\" x=\"" (acss/fmt (:x img)) "\" y=\"" (acss/fmt (:y img))
       "\" width=\"" (acss/fmt (:width img)) "\" height=\"" (acss/fmt (:height img)) "\"/>"))

(defn- svg-drawing
  "What an entry of `images` draws (see `timeline->svg`): its vectors,
  where they are in the SVG, whose origin is `ox`/`oy` (see
  `svg-vectors`), or its picture."
  [img ox oy]
  (if-let [markup (:markup img)]
    (svg-vectors markup ox oy)
    (svg-image img)))

(def ^:private svg-name-junk
  "What an XML name (an `id`) cannot hold."
  #"[\s\"'<>&#%/\\:;,()\[\]{}=+*!?@$^|~`]")

(defn svg-layer-ids
  "The `id`s of the layers `ids` (shape ids) in an SVG export, by shape
  id, as Figma writes them: their names, what an XML name cannot hold as
  `_`, with `_` before a name that cannot start one, the second layer of
  a name `<name>_2` and so on."
  [objects ids]
  (loop [ids  (seq ids)
         used {}
         out  {}]
    (if-let [id (first ids)]
      (let [base  (let [name (str/replace (str (or (not-empty (:name (get objects id))) "layer")) svg-name-junk "_")]
                    (if (re-find #"^[0-9.-]" name) (str "_" name) name))
            n     (inc (get used base 0))
            unique (if (= 1 n) base (str base "_" n))]
        (recur (next ids) (assoc used base n) (assoc out id unique)))
      out)))

(def ^:private svg-fill-properties #{:fill-color :fill-opacity})
(def ^:private svg-stroke-properties #{:stroke-color :stroke-opacity :stroke-width})
(def ^:private svg-trim-properties #{:trim-start :trim-end :trim-offset})

(defn svg-export-layer-ids
  "The layers of an SVG of vectors of `timeline` (with its keyframes, see
  `resolve-animations`): its export layers (see `export-layer-ids`) and
  the layers in the boards whose size it plays on their elements (see
  `svg-sized?`) or that cast inner shadows (see `export-holders`)."
  [objects timeline]
  (cta/export-layer-ids objects (:board-id timeline) (:tracks timeline) (aexp/export-holders objects timeline)))

(defn- svg-clip-rest-id
  [id]
  (str "penpot-clip-rest-" id))

(defn- svg-clipped-class
  [id]
  (str "penpot-clipped-" id))

(defn- svg-clip-animated?
  "Whether SMIL plays the corners of the clip of the board `id` (see
  `svg-clip-paths`): an unturned one whose outline it plays (see
  `svg-smil`)."
  [objects smil id]
  (and (some? (dm/get-in smil [id :outline]))
       (aexp/unturned? (get objects id))))

(defn- svg-clip-paths
  "The clip paths of the boards `ids` that clip layers, where they are in
  the SVG, whose origin is `ox`/`oy`: a rectangle when the board is one
  (see `svg-even-corners?`, its track in `tracks`), whose size and
  corners its CSS plays (see `svg-outline-elements`), else its outline,
  whose corners SMIL plays when it is unturned (the `:outline` of `smil`,
  see `svg-smil`), with one at rest for reduced motion then."
  [objects tracks smil ox oy ids]
  (->> ids
       (map (fn [id]
              (let [shape (get objects id)
                    move  (str "translate(" (acss/fmt (- ox)) " " (acss/fmt (- oy)) ")")]
                (str "<clipPath id=\"" (svg-escape (svg-clip-id id)) "\">"
                     (if (and (aexp/unturned? shape) (aexp/svg-even-corners? shape (get tracks id)))
                       (let [{:keys [x y width height]} (:selrect shape)
                             r (acss/fmt (or (:r1 shape) 0))]
                         (str "<rect transform=\"" move "\" x=\"" (acss/fmt x) "\" y=\"" (acss/fmt y)
                              "\" width=\"" (acss/fmt width) "\" height=\"" (acss/fmt height)
                              "\" rx=\"" r "\" ry=\"" r "\"/>"))
                       (let [animate (when (aexp/unturned? shape) (dm/get-in smil [id :outline]))]
                         (str "<path transform=\"" move "\" d=\""
                              (path/content (aexp/canvas-outline shape)) "\""
                              (if (some? animate) (str ">" animate "</path>") "/>"))))
                     "</clipPath>"
                     ;; at rest, with reduced motion asked for (see `svg-clip-group`)
                     (when (svg-clip-animated? objects smil id)
                       (str "<clipPath id=\"" (svg-escape (svg-clip-rest-id id)) "\"><path transform=\"" move
                            "\" d=\"" (path/content (aexp/canvas-outline shape)) "\"/></clipPath>"))))))
       (str/join "")))

(defn- svg-clip-group
  "`svg` kept to the outline of the board `id` (see `svg-clip-paths`), to
  the one at rest with reduced motion asked for when SMIL plays its
  corners (see `svg-smil-css`)."
  [objects smil id svg]
  (str "<g" (when (svg-clip-animated? objects smil id)
              (str " class=\"" (svg-escape (svg-clipped-class id)) "\""))
       " clip-path=\"url(#" (svg-escape (svg-clip-id id)) ")\">" svg "</g>"))

(defn- indexed-value
  "The value of a property at `index` in `values` (see `values-at`): by
  index for fills, strokes and shadows."
  [v index]
  (if (map? v) (get v index) v))

(defn- svg-trim-declarations
  "The dash of the strokes of a layer drawn from `:trim-start` to
  `:trim-end` moved along by `:trim-offset` of `values`, on outlines of
  length 1 (the vectors of the layer have a `pathLength` of 1)."
  [values]
  (when (some #(contains? values %) svg-trim-properties)
    (let [{:keys [trim-start trim-end trim-offset]} (merge cta/trim-properties (select-keys values svg-trim-properties))
          length (mth/clamp (- trim-end trim-start) 0 1)]
      [(str "stroke-dasharray: " (acss/fmt length) " " (acss/fmt (- 1 length)) ";")
       (str "stroke-dashoffset: " (acss/fmt (- (+ trim-start trim-offset))) ";")])))

(defn- svg-stroke-elements
  "The elements of the stroke `index` of `shape` as the SVG renderer
  draws its strokes, in reverse order, an inner or outer one twice as
  wide and cut by a copy of the shape (its `<use>`s made copies, see
  `app.main.data.exports.svg-vectors/inline-uses`): `:paint`, the one
  painted, `:mask`, the stroke of the mask of an outer one, and
  `:outline`, all that draw its outline."
  [shape index]
  (let [strokes (cta/as-vec (:strokes shape))
        group   (str ".strokes[id$=\"-" (:id shape) "\"] > :nth-child(" (- (count strokes) index) ")")]
    (case (aexp/svg-stroke-alignment shape (get strokes index))
      :inner
      (let [paint (str group ".inner-stroke-shape > :not(defs)")]
        {:paint paint
         :outline [paint (str group ".inner-stroke-shape > defs > clipPath > *")]})

      :outer
      (let [outer (str group ".outer-stroke-shape")
            paint (str outer " > [mask]")
            mask  (str outer " > defs > mask > :first-child")]
        {:paint paint
         :mask mask
         :outline [paint (str outer " > :not(defs):not([mask])") mask
                   (str outer " > defs > mask > :last-child")]})

      (let [paint (str group ".stroke-shape > :not(defs)")]
        {:paint paint :outline [paint]}))))

(defn- svg-outline-elements
  "The elements that draw the outline of `shape` as the SVG renderer
  draws it: its fill, its strokes (see `svg-stroke-elements`) and, of a
  board, its clip and the one of the layers in it (see
  `svg-clip-paths`)."
  [shape]
  (let [sid (:id shape)]
    (-> [(str "[id=\"fills-" sid "\"] > :not(defs)")]
        (cond-> (= :frame (:type shape))
          (conj (str "[id=\"shape-" sid "\"] > defs > clipPath > rect")
                (str "[id=\"" (svg-clip-id sid) "\"] > rect")))
        (into (mapcat #(:outline (svg-stroke-elements shape %)))
              (range (count (cta/as-vec (:strokes shape))))))))

(defn- svg-size-declarations
  "The geometry of `shape` of the width and height in `values`, resized
  around the pivot of `track` as the canvas resizes it, as an ellipse or
  a rectangle has it; flipped, its box (its paint is plain, see
  `svg-sized?`)."
  [shape track values]
  (when (some #(contains? values %) aexp/svg-size-properties)
    (let [{:keys [x y width height]} (:selrect shape)
          pivot   (cta/origin-point shape (cta/track-origin track))
          span    (fn [from size pivot new-size]
                    (let [start (- pivot (* (- pivot from) (/ new-size size)))]
                      [(min start (+ start new-size)) (mth/abs new-size)]))
          [nx nw] (span x width (:x pivot) (get values :width width))
          [ny nh] (span y height (:y pivot) (get values :height height))
          px      #(str (acss/fmt %) "px")]
      (if (= :circle (:type shape))
        [(str "cx: " (px (+ nx (/ nw 2))) ";") (str "cy: " (px (+ ny (/ nh 2))) ";")
         (str "rx: " (px (/ nw 2)) ";") (str "ry: " (px (/ nh 2)) ";")]
        [(str "x: " (px nx) ";") (str "y: " (px ny) ";")
         (str "width: " (px nw) ";") (str "height: " (px nh) ";")]))))

(defn- svg-targets
  "What plays the appearance `props` animates on `shape` in an SVG of
  vectors: `[selectors declarations properties]` each, the selectors of
  its elements as the SVG renderer draws them, one each (see
  `svg-appearance-css`), the declarations of the `values` of a stop and
  the properties they read."
  [shape track props]
  (let [sid       (:id shape)
        text?     (= :text (:type shape))
        fills     (cta/as-vec (:fills shape))
        strokes   (cta/as-vec (:strokes shape))
        kf-index  (fn [pset]
                    (into (sorted-set)
                          (comp (filter #(pset (:property %))) (map #(or (:index %) 0)))
                          (:keyframes track)))
        trim?     (some svg-trim-properties props)
        fill-targets
        (for [index (kf-index svg-fill-properties)
              :when (or text? (< index (count fills)))]
          [[(cond
              ;; its `text`, or the outlines of its units (see
              ;; `timeline->svg`)
              (or text? (<= (count fills) 1)) (str "[id=\"fills-" sid "\"] > :not(defs)")
              :else               (str "[id=\"shape-" sid "\"] > defs > pattern > g > :nth-child("
                                       (- (count fills) index) ")"))]
           (fn [values]
             (let [color   (indexed-value (:fill-color values) index)
                   opacity (indexed-value (:fill-opacity values) index)]
               (cond-> []
                 (some? color)   (conj (str "fill: " color ";"))
                 (some? opacity) (conj (str "fill-opacity: " (acss/fmt opacity) ";")))))
           svg-fill-properties])
        stroke-targets
        (when-not text?
          (mapcat
           (fn [index]
             (let [;; inner and outer strokes are drawn twice as wide, then cut
                   factor (if (= :center (aexp/svg-stroke-alignment shape (get strokes index))) 1 2)
                   {:keys [paint mask]} (svg-stroke-elements shape index)
                   width  #(when-let [w (indexed-value (:stroke-width %) index)]
                             (str "stroke-width: " (acss/fmt (* factor w)) ";"))]
               (cond-> [[[paint]
                         (fn [values]
                           (let [color   (indexed-value (:stroke-color values) index)
                                 opacity (indexed-value (:stroke-opacity values) index)]
                             (cond-> (vec (svg-trim-declarations values))
                               (some? color)   (conj (str "stroke: " color ";"))
                               (some? opacity) (conj (str "stroke-opacity: " (acss/fmt opacity) ";"))
                               (some? (width values)) (conj (width values)))))
                         (into svg-stroke-properties svg-trim-properties)]]
                 ;; the mask of an outer stroke is as wide
                 (and (some? mask) (contains? props :stroke-width))
                 (conj [[mask]
                        (fn [values] (keep identity [(width values)]))
                        #{:stroke-width}]))))
           (into (kf-index svg-stroke-properties)
                 (when trim? (range (count strokes))))))
        radius-targets
        (when (and (some aexp/svg-radius-properties props) (aexp/svg-even-corners? shape track))
          [[(svg-outline-elements shape)
            (fn [values]
              (when-let [r (some #(get values %) [:r1 :r2 :r3 :r4])]
                [(str "rx: " (acss/fmt r) "px;") (str "ry: " (acss/fmt r) "px;")]))
            aexp/svg-radius-properties]])
        size-targets
        (when (aexp/svg-sized? shape track)
          [[(svg-outline-elements shape)
            (fn [values] (svg-size-declarations shape track values))
            aexp/svg-size-properties]])]
    (concat fill-targets stroke-targets radius-targets size-targets)))

(defn- svg-iterations
  "The CSS `animation-iteration-count` (and direction) of `timeline`."
  [timeline]
  (case (cta/playback-mode timeline)
    :loop      "infinite"
    :ping-pong "infinite alternate"
    "1 both"))

(defn- svg-stop-css
  "A stop at `time` of `duration` of a `@keyframes`: `decls`, and the
  timing of the segment it starts, when it starts one."
  [time duration decls timing]
  (let [decls (cond-> (vec decls)
                (and timing (< time duration))
                (conj (str "animation-timing-function: " timing ";")))]
    (str "  " (acss/fmt (* 100.0 (/ (double time) duration))) "% { " (str/join " " decls) " }")))

(defn- svg-animations-css
  "The CSS of `animations`, `[selectors name keyframes]` each: their
  `@keyframes`, then a rule of each element listing all its animations,
  as a rule of its own would drop those of the others, and none with
  reduced motion asked for."
  [animations duration iterations]
  (when (seq animations)
    (let [elements (into [] (comp (mapcat first) (distinct)) animations)
          names    (reduce (fn [names [selectors name]]
                             (reduce #(update %1 %2 (fnil conj []) name) names selectors))
                           {}
                           animations)]
      (str (str/join "\n\n" (map #(nth % 2) animations))
           "\n\n"
           (str/join "\n\n"
                     (map (fn [selector]
                            (str selector " {\n  animation: "
                                 (str/join ", " (map #(str % " " duration "ms linear " iterations)
                                                     (get names selector)))
                                 ";\n}"))
                          elements))
           "\n\n@media (prefers-reduced-motion: reduce) {\n  "
           (str/join ",\n  " elements)
           " {\n    animation: none;\n  }\n}"))))

(defn svg-appearance-css
  "The CSS that plays the fills, strokes, corners, trim and sizes of the
  layers of an SVG of vectors on their elements (see `timeline->svg`),
  as Penpot's SVG renderer draws them: `fills-<id>` holds the fills
  (more than one in a pattern, in reverse order), `strokes-…-<id>` the
  strokes, in reverse order, an inner or outer one painted twice as
  wide. Corners play when they are alike (see `svg-even-corners?`),
  sizes as `svg-sized?` says. `timeline` is the one the exports play,
  cut to its duration (see `cut-timeline`). Nil without any. A frontend
  test matches its selectors against what the SVG renderer draws
  (`svg-vectors-test`), which may change upstream."
  [timeline objects]
  (let [duration   (max 1 (:duration timeline))
        appearance (some-fn svg-fill-properties svg-stroke-properties aexp/svg-radius-properties
                            svg-trim-properties aexp/svg-size-properties)]
    (svg-animations-css
     (for [[sid track] (:tracks timeline)
           :let  [shape (get objects sid)
                  props (into #{} (map :property) (:keyframes track))]
           :when (and (some? shape) (some appearance props))
           [n [selectors declarations reads]] (map-indexed vector (svg-targets shape track props))
           :let  [kfs (filterv #(reads (:property %)) (:keyframes track))]
           :when (seq kfs)]
       (let [name   (str "penpot-svg-" sid "-" n)
             times  (vec (into (sorted-set 0 duration)
                               (comp (map :time) (filter #(<= 0 % duration)))
                               kfs))
             values (acss/shape-values timeline sid)
             stops  (for [[t kf] (acss/eased-stops #(acss/value-numbers (select-keys (values %) reads)) kfs times)]
                      (svg-stop-css t duration (declarations (values t)) (some-> kf acss/keyframe-timing)))]
         [selectors name (str "@keyframes " name " {\n" (str/join "\n" stops) "\n}")]))
     duration
     (svg-iterations timeline))))

;; What CSS can not play on the vectors of an SVG, SMIL plays: inner
;; shadows and the spread of shadows (on a filter of the shadows), and
;; corners not alike (on the path of the outline).

(def ^:private svg-shadow-properties
  #{:shadow-offset-x :shadow-offset-y :shadow-blur :shadow-spread :shadow-color :shadow-opacity})

(defn- track-properties
  [track]
  (into #{} (map :property) (:keyframes track)))

(defn svg-filter-shadows?
  "Whether an SVG of vectors draws the shadows of `shape` on a filter of
  their own around it and the layers in it, as the canvas shadows a
  group or board with what it holds, and as soft (see
  `svg-effects-filter`), SMIL playing those `track` animates, its
  vectors drawn without them: when it has shadows to show."
  [shape _track]
  (boolean (some (complement :hidden) (cta/as-vec (:shadow shape)))))

(defn- blur-at
  "The radius of the layer blur `blur` of a shape when its animation
  gives it the value `v` (nil when it plays none): 0 when hidden or
  none, as the canvas draws it (see `apply-blur`)."
  [blur v]
  (let [blur (if (some? v) (cta/apply-blur blur v) blur)]
    (if (and (some? blur) (not (:hidden blur)))
      (or (:value blur) 0)
      0)))

(defn svg-filter-blur?
  "Whether an SVG of vectors draws the layer blur of `shape` on a filter
  of its own around it and the layers in it, as strong as the canvas
  blurs (see `svg-effects-filter`), SMIL playing it as `track` animates
  it (nil, none): a blur it shows or one that plays."
  [shape track]
  (boolean
   (or (and (contains? (track-properties track) :blur) (not (:hidden (:blur shape))))
       (pos? (blur-at (:blur shape) nil)))))

(defn svg-smil-corners?
  "Whether an SVG of vectors plays the corners `track` animates on
  `shape` by SMIL, on the path of its outline (see `svg-corner-path`): a
  rectangle or board whose corners are not alike (see
  `svg-even-corners?`), drawn as a path (the elements of its outline
  marked `data-penpot-outline`, see `svg-smil-outline`)."
  [shape track]
  (boolean
   (and (contains? #{:rect :frame} (:type shape))
        (some aexp/svg-radius-properties (track-properties track))
        (not (aexp/svg-even-corners? shape track)))))

(def ^:private smil-linear [0 0 1 1])

(defn- smil-curve
  "The curve (SMIL `keySplines`) of the stretch of `kfs` from `from`: the
  easing of the keyframe there when SMIL takes its control points (within
  0 and 1), linear without one; nil for a spring, a curve out of them or
  a hold."
  [kfs from]
  (if-let [kf (d/seek #(= from (:time %)) kfs)]
    (when-not (= :step (:interpolation kf))
      (let [curve (cta/easing-curve (:easing kf))]
        (when (and (some? curve) (every? #(<= 0 % 1) curve))
          curve)))
    smil-linear))

(defn- smil-stretch
  "The points SMIL plays `value` (a time to a vector of numbers) by from
  `t0` to `t1` of the keyframes `kfs`: `[[time numbers curve]...]`, a point
  and the curve to the next. One when the values between follow the curve
  of the keyframe at `t0` (see `smil-curve`), two that hold the first value
  for a hold, else samples (see `sample-times`)."
  [value kfs t0 t1]
  (let [v0    (value t0)
        curve (smil-curve kfs t0)]
    (cond
      (and (some? curve) (acss/follows? value t0 t1 #(cta/cubic-bezier curve %)))
      [[t0 v0 curve]]

      (acss/follows? value t0 t1 (constantly 0))
      (if (= v0 (value t1))
        [[t0 v0 smil-linear]]
        [[t0 v0 smil-linear] [(max (/ (+ t0 t1) 2) (- t1 0.5)) v0 smil-linear]])

      :else
      (mapv (fn [t] [t (value t) smil-linear]) (acss/sample-times t0 t1)))))

(defn- smil-points
  "The points SMIL plays `value` (a time to a vector of numbers) of the
  keyframes `kfs` by over `duration` (see `smil-stretch`): the stretches
  between their times within it, 0 and `duration`, then the end."
  [value kfs duration]
  (let [value #(mapv double (value %))
        times (vec (into (sorted-set 0 duration)
                         (comp (map :time) (filter #(<= 0 % duration)))
                         kfs))]
    (conj (into [] (mapcat (fn [[t0 t1]] (smil-stretch value kfs t0 t1))) (partition 2 1 times))
          [duration (value duration) nil])))

(defn- smil-mirrored
  "`points` (see `smil-points`) played forth and back over twice
  `duration`, as CSS alternates a ping-pong: each stretch back by its
  curve reversed."
  [points duration]
  (let [n     (dec (count points))
        rev   (fn [[x1 y1 x2 y2]] [(- 1 x2) (- 1 y2) (- 1 x1) (- 1 y1)])
        curve #(nth (nth points %) 2)]
    (into (conj (pop points) (assoc (peek points) 2 (rev (curve (dec n)))))
          (map (fn [i]
                 (let [[t v] (nth points i)]
                   [(- (* 2 duration) t) v (when (pos? i) (rev (curve (dec i))))])))
          (range (dec n) -1 -1))))

(defn- smil-fraction
  [v]
  (let [r (mth/precision v 6)]
    (if (== r (mth/round r)) (str (long r)) (str r))))

(defn- smil-animate
  "The SMIL `<animate>` of the attribute `attr` playing `points` (see
  `smil-points`) as `timeline` plays (its duration and playback, as its
  CSS, see `svg-iterations`), `render` writing the numbers of a value;
  nil when it keeps its first one."
  [timeline attr points render]
  (when (not-every? #(= (second (first points)) (second %)) points)
    (let [duration (max 1 (:duration timeline))
          mode     (cta/playback-mode timeline)
          points   (if (= :ping-pong mode) (smil-mirrored points duration) points)
          total    (if (= :ping-pong mode) (* 2 duration) duration)]
      (str "<animate attributeName=\"" attr "\" dur=\"" (acss/fmt total) "ms\""
           (if (= :once mode) " fill=\"freeze\"" " repeatCount=\"indefinite\"")
           " calcMode=\"spline\" keyTimes=\""
           (str/join ";" (map #(smil-fraction (/ (first %) total)) points))
           "\" keySplines=\""
           (str/join ";" (map #(str/join " " (map acss/fmt (nth % 2))) (butlast points)))
           "\" values=\"" (str/join ";" (map #(render (second %)) points)) "\"/>"))))

(def ^:private svg-hard-alpha
  "The color matrix of the alpha of the source, opaque wherever it shows."
  "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 127 0")

(defn- svg-shadow-color
  [shadow]
  (let [{:keys [color opacity]} (:color shadow)
        [r g b] (clr/hex->rgb (or color "#000000"))]
    [(/ r 255.0) (/ g 255.0) (/ b 255.0) (double (or opacity 1))]))

(defn- render-color-matrix
  [[r g b a]]
  (str "0 0 0 0 " (acss/fmt r) " 0 0 0 0 " (acss/fmt g) " 0 0 0 0 " (acss/fmt b) " 0 0 0 " (acss/fmt a) " 0"))

(defn- render-number
  [[v]]
  (acss/fmt v))

(defn svg-effects-id
  "The id of the filter of the effects of the layer `id` (see
  `svg-effects-filter`); `rest?`, of the one at rest."
  [id rest?]
  (str (if rest? "penpot-effects-rest-" "penpot-effects-") id))

(defn- smil-attr
  "The attribute `name` of a filter primitive, of `value` (a time to a
  vector of numbers, `render` writing one) at 0, and its SMIL
  `<animate>` when the keyframes `kfs` of `timeline` (cut to its
  duration) play it: `{:attr :animate :points}`."
  [timeline name value kfs render]
  (let [points (when (and (some? timeline) (seq kfs))
                 (smil-points value kfs (max 1 (:duration timeline))))]
    {:attr    (str " " name "=\"" (render (value 0)) "\"")
     :animate (when points (smil-animate timeline name points render))
     :points  points}))

(defn- smil-element
  "The filter primitive `tag` of the attributes `attrs` and `props` (see
  `smil-attr`), holding the `<animate>`s of those."
  [tag attrs & props]
  (let [animates (keep :animate props)]
    (str "<" tag attrs (str/join "" (map :attr props))
         (if (seq animates)
           (str ">" (str/join "" animates) "</" tag ">")
           "/>"))))

(defn- svg-blur-primitive
  "The `feGaussianBlur` of a layer blur of `radius` (a time to it) as
  strong as the canvas blurs (see `blur-sigma`), SMIL playing the
  keyframes `kfs` of `timeline`."
  [timeline radius kfs]
  (smil-element "feGaussianBlur" ""
                (smil-attr timeline "stdDeviation" #(vector (aexp/blur-sigma (radius %))) kfs render-number)))

(defn- svg-effects-filter
  "The filter `id` of the effects of `shape` an SVG of vectors draws on
  a filter of their own (see `svg-smil`): with `shadows?`, its shadows
  as the SVG renderer draws them (see `app.main.ui.shapes.filters`), as
  soft as on the canvas (see `blur-sigma`), its drop shadows under it,
  its inner ones over it, the last first; then,
  with `blur?`, its layer blur, as strong as on the canvas (see
  `blur-sigma`). With `timeline` (cut to its duration), SMIL plays their
  keyframes on it; without, they rest. Its region, on the page, holds
  them as they play, and `area` (a rect on the page), when given: what
  the layers in it may cover. Of its shadows, those of `styles` (both
  kinds by default)."
  [id shape timeline {:keys [shadows? blur? area styles] :or {styles #{:drop-shadow :inner-shadow}}}]
  (let [sid      (:id shape)
        track    (dm/get-in timeline [:tracks sid])
        duration (max 1 (:duration timeline 1))
        values   (if (some? track) (acss/shape-values timeline sid) (constantly {}))
        kfs      (filterv #(svg-shadow-properties (:property %)) (:keyframes track))
        blur-kfs (filterv #(= :blur (:property %)) (:keyframes track))
        shadows  #(cta/apply-shadows (:shadow shape) (values %))
        radius   #(blur-at (:blur shape) (:blur (values %)))
        attr     (fn [index name props value render]
                   (smil-attr timeline name #(value (get (shadows %) index))
                              (filterv #(and (props (:property %)) (= index (or (:index %) 0))) kfs)
                              render))
        el       smil-element
        spread   (fn [index sign]
                   (attr index "radius" #{:shadow-spread}
                         (fn [s] [(max 0 (* sign (or (:spread s) 0)))])
                         render-number))
        spreads? (fn [{:keys [points attr]}]
                   (or (some #(pos? (first (second %))) points)
                       (not= attr " radius=\"0\"")))
        common   (fn [index]
                   (str (el "feOffset" ""
                            (attr index "dx" #{:shadow-offset-x} (fn [s] [(or (:offset-x s) 0)]) render-number)
                            (attr index "dy" #{:shadow-offset-y} (fn [s] [(or (:offset-y s) 0)]) render-number))
                        (el "feGaussianBlur" ""
                            (attr index "stdDeviation" #{:shadow-blur} (fn [s] [(aexp/blur-sigma (or (:blur s) 0))])
                                  render-number))))
        color    (fn [index]
                   (el "feColorMatrix" " type=\"matrix\""
                       (attr index "values" #{:shadow-color :shadow-opacity} svg-shadow-color render-color-matrix)))
        drop     (fn [prev [k index]]
                   (let [alpha  (str "a" k)
                         dilate (spread index 1)
                         erode  (spread index -1)]
                     (str (el "feColorMatrix" (str " in=\"SourceAlpha\" type=\"matrix\" values=\"" svg-hard-alpha
                                                   "\" result=\"" alpha "\""))
                          (when (spreads? dilate)
                            (el "feMorphology" (str " in=\"" alpha "\" operator=\"dilate\"") dilate))
                          (when (spreads? erode)
                            (el "feMorphology" (str (when-not (spreads? dilate) (str " in=\"" alpha "\""))
                                                    " operator=\"erode\"")
                                erode))
                          (common index)
                          (color index)
                          (el "feBlend" (str " mode=\"normal\" in2=\"" prev "\" result=\"s" k "\"")))))
        inner    (fn [prev [k index]]
                   (let [alpha (str "a" k)
                         erode (spread index 1)]
                     (str (el "feColorMatrix" (str " in=\"SourceAlpha\" type=\"matrix\" values=\"" svg-hard-alpha
                                                   "\" result=\"" alpha "\""))
                          (when (spreads? erode)
                            (el "feMorphology" (str " in=\"" alpha "\" operator=\"erode\"") erode))
                          (common index)
                          (el "feComposite" (str " in2=\"" alpha "\" operator=\"arithmetic\" k2=\"-1\" k3=\"1\""))
                          (color index)
                          (el "feBlend" (str " mode=\"normal\" in2=\"" prev "\" result=\"s" k "\"")))))
        ;; each over the one before: `[result markup]`
        chain    (fn [prev items f]
                   (reduce (fn [[prev markup] [k :as item]]
                             [(str "s" k) (str markup (f prev item))])
                           [prev ""]
                           items))
        ;; as the renderer: its last shadow first
        shown    (->> (cta/as-vec (:shadow shape))
                      (map-indexed vector)
                      (remove (comp :hidden second))
                      reverse
                      (map-indexed (fn [k [index shadow]] [k index (:style shadow :drop-shadow)]))
                      (filter #(contains? styles (nth % 2))))
        [prev drops] (chain "BackgroundImageFix" (filter #(= :drop-shadow (nth % 2)) shown) drop)
        [_ inners]   (chain "shape" (filter #(= :inner-shadow (nth % 2)) shown) inner)
        times    (into #{0 duration} (map :time) (concat kfs blur-kfs))
        bounds   (grc/join-rects
                  (cond-> (mapv #(gsb/get-shape-filter-bounds
                                  (cond-> (assoc shape :shadow (shadows %))
                                    blur? (assoc :blur {:type :layer-blur :value (radius %) :hidden false})))
                                times)
                    (some? area) (conj area)))
        ;; its strokes; its shadows and blur are in its bounds
        padding  (gsb/calculate-padding shape false true)
        px       (+ (:horizontal padding 0) 5)
        py       (+ (:vertical padding 0) 5)]
    (str "<filter id=\"" (svg-escape id)
         "\" x=\"" (acss/fmt (- (:x bounds) px)) "\" y=\"" (acss/fmt (- (:y bounds) py))
         "\" width=\"" (acss/fmt (+ (:width bounds) (* 2 px))) "\" height=\"" (acss/fmt (+ (:height bounds) (* 2 py)))
         "\" filterUnits=\"userSpaceOnUse\" color-interpolation-filters=\"sRGB\">"
         (when shadows?
           (str "<feFlood flood-opacity=\"0\" result=\"BackgroundImageFix\"/>"
                drops
                "<feBlend mode=\"normal\" in=\"SourceGraphic\" in2=\"" prev "\" result=\"shape\"/>"
                inners))
         (when blur?
           (svg-blur-primitive timeline radius blur-kfs))
         "</filter>")))

(defn svg-unit-blur-id
  "The id of the filter of the blur of the unit `k` of the text `text-id`
  (see `svg-unit-blurs`)."
  [text-id k]
  (str "penpot-unit-blur-" text-id "-" k))

(defn- svg-unit-blurs
  "The filters of the blur of the units of the split texts `texts` (ids)
  as `timeline` (cut to its duration) plays them (see `unit-timelines`),
  as strong as on the canvas (see `blur-sigma`), each over the box of
  its vectors in `images` (`:box`, in the SVG), else over `area`: `{[text-id
  k] filter}`."
  [timeline objects images texts area]
  (into {}
        (for [[text-id {units-tl :timeline ids :ids}] (select-keys (cta/unit-timelines timeline objects) texts)
              :let  [units-tl (cta/cut-timeline units-tl)]
              [k unit-id] (map-indexed vector ids)
              :let  [kfs (filterv #(= :blur (:property %)) (dm/get-in units-tl [:tracks unit-id :keyframes]))
                     img (get images [text-id k])]
              :when (and (some? img) (some #(pos? (or (:value %) 0)) kfs))]
          (let [values (acss/shape-values units-tl unit-id)
                margin (+ 2 (* 3 (aexp/blur-sigma (reduce max 0 (map :value kfs)))))
                {:keys [x y width height]} (or (:box img) area)]
            [[text-id k]
             (str "<filter id=\"" (svg-escape (svg-unit-blur-id text-id k))
                  "\" x=\"" (acss/fmt (- x margin)) "\" y=\"" (acss/fmt (- y margin))
                  "\" width=\"" (acss/fmt (+ width (* 2 margin))) "\" height=\"" (acss/fmt (+ height (* 2 margin)))
                  "\" filterUnits=\"userSpaceOnUse\" color-interpolation-filters=\"sRGB\">"
                  (svg-blur-primitive units-tl #(or (:blur (values %)) 0) kfs)
                  "</filter>")]))))

(defn- render-corner-path
  "The outline of a rectangle `[x y width height r1 r2 r3 r4]` (its
  corners clamped, see `svg-corner-path`) as lines and curves: no arcs,
  whose radii WebKit does not tween."
  [[x y w h r1 r2 r3 r4]]
  (let [k     (- 1 aexp/kappa)
        right (+ x w)
        bottom (+ y h)
        pt    #(str (acss/fmt %1) "," (acss/fmt %2))]
    (str "M" (pt (+ x r1) y)
         "L" (pt (- right r2) y)
         "C" (pt (- right (* k r2)) y) " " (pt right (+ y (* k r2))) " " (pt right (+ y r2))
         "L" (pt right (- bottom r3))
         "C" (pt right (- bottom (* k r3))) " " (pt (- right (* k r3)) bottom) " " (pt (- right r3) bottom)
         "L" (pt (+ x r4) bottom)
         "C" (pt (+ x (* k r4)) bottom) " " (pt x (- bottom (* k r4))) " " (pt x (- bottom r4))
         "L" (pt x (+ y r1))
         "C" (pt x (+ y (* k r1))) " " (pt (+ x (* k r1)) y) " " (pt (+ x r1) y)
         "Z")))

(defn- svg-corner-path
  "The `<animate>` of the path of the outline of `shape` whose corners
  `timeline` (cut to its duration) plays (see `svg-smil-corners?`), in
  its own space (its turn is the `transform` of the path): its box at
  rest, its corners clamped as the canvas clamps them."
  [shape timeline]
  (let [sid      (:id shape)
        {:keys [x y width height]} (:selrect shape)
        values   (acss/shape-values timeline sid)
        kfs      (filterv #(aexp/svg-radius-properties (:property %)) (dm/get-in timeline [:tracks sid :keyframes]))
        value    (fn [t]
                   (let [vs (values t)]
                     (into [x y width height]
                           (gco/shape-corners-4 {:width width :height height
                                                 :r1 (get vs :r1 (or (:r1 shape) 0))
                                                 :r2 (get vs :r2 (or (:r2 shape) 0))
                                                 :r3 (get vs :r3 (or (:r3 shape) 0))
                                                 :r4 (get vs :r4 (or (:r4 shape) 0))}))))]
    (smil-animate timeline "d" (smil-points value kfs (max 1 (:duration timeline))) render-corner-path)))

(defn- svg-smil-outline
  "`markup` with the `<animate>` `animate` in the elements of the outline
  of its shape, as the frontend marked them (`data-penpot-outline`)."
  [markup animate]
  (str/replace markup #"<path ([^>]*?) ?data-penpot-outline=\"\"([^>]*)></path>"
               (str "<path $1$2>" animate "</path>")))

(defn- svg-smil-class
  [id]
  (str "penpot-smil-" id))

(defn- svg-inner-id
  "The id of the filter of the inner shadows of the board `id` (see
  `svg-smil`); `rest?`, of the one at rest."
  [id rest?]
  (str (if rest? "penpot-inner-rest-" "penpot-inner-") id))

(defn- svg-smil-inner-class
  [id]
  (str "penpot-smil-inner-" id))

(defn- svg-smil-anim-class
  [id]
  (str "penpot-smil-anim-" id))

(defn- svg-smil-rest-class
  [id]
  (str "penpot-smil-rest-" id))

(defn- svg-smil
  "What SMIL plays on the vectors of the layers of `timeline` (cut to its
  duration), and of the board, that `images` has the vectors of (see
  `timeline->svg`), `{id {:outline :filters}}`: the `<animate>` of the
  path of the outline of each whose corners it plays (see
  `svg-smil-corners?`), the filters of the effects of each that has
  shadows it plays or a blur (see `svg-filter-shadows?`,
  `svg-filter-blur?`), as they play and at rest, which reduced motion
  shows (see `svg-smil-css`). Those of `wide` hold layers, which may go
  over `area` (on the page)."
  [timeline objects images wide area]
  (reduce
   (fn [result sid]
     (let [track    (dm/get-in timeline [:tracks sid])
           vectors? (or (some? (dm/get-in images [sid :markup]))
                        (some #(some? (:markup (get images [sid %]))) (unit-image-indexes images sid)))
           shape    (when vectors? (get objects sid))
           ;; a board casts its inner shadows inside its fills, under what
           ;; it holds: on a filter of its vectors (see `svg-smil-vectors`)
           board?   (and (some? shape) (aexp/board-inner-shadows? shape))
           drops    (if board? #{:drop-shadow} #{:drop-shadow :inner-shadow})
           shadows? (and (some? shape)
                         (svg-filter-shadows? shape track)
                         (some #(and (not (:hidden %)) (contains? drops (:style % :drop-shadow)))
                               (cta/as-vec (:shadow shape))))
           blur?    (and (some? shape) (svg-filter-blur? shape track))
           outline  (when (and (some? shape) (svg-smil-corners? shape track))
                      (svg-corner-path shape timeline))
           filters  (when (or shadows? blur?)
                      (let [flags {:shadows? shadows? :blur? blur? :styles drops
                                   :area (when (contains? wide sid) area)}]
                        (str (svg-effects-filter (svg-effects-id sid false) shape timeline flags)
                             (svg-effects-filter (svg-effects-id sid true) shape nil flags))))
           inner    (when board?
                      (let [flags {:shadows? true :styles #{:inner-shadow}}]
                        (str (svg-effects-filter (svg-inner-id sid false) shape timeline flags)
                             (svg-effects-filter (svg-inner-id sid true) shape nil flags))))]
       (cond-> result
         (or (some? outline) (some? filters) (some? inner))
         (assoc sid (cond-> {}
                      (some? outline) (assoc :outline outline)
                      (some? filters) (assoc :filters filters)
                      (some? inner)   (assoc :inner inner))))))
   {}
   (into (set (keys (:tracks timeline))) (filter uuid?) (keys images))))

(defn- svg-smil-css
  "The CSS that shows what SMIL plays (see `svg-smil`) at rest with
  reduced motion asked for, which SMIL does not heed: the effects of
  `smil`, the copies at rest of the vectors whose corners it plays (see
  `svg-smil-vectors`) and the clips of the boards `clipped` (see
  `svg-clip-group`), and the units of `unit-blurs` (see
  `svg-unit-blurs`) unblurred, as they rest; nil without any."
  [smil unit-blurs clipped]
  (let [ids   (into [] (keep (fn [[id {:keys [filters]}]] (when filters id))) smil)
        rules (concat (map (fn [id]
                             (str "  ." (svg-smil-class id) " { filter: url(#"
                                  (svg-effects-id id true) "); }"))
                           ids)
                      (keep (fn [[id {:keys [inner]}]]
                              (when (some? inner)
                                (str "  ." (svg-smil-inner-class id) " { filter: url(#"
                                     (svg-inner-id id true) "); }")))
                            smil)
                      (mapcat (fn [[id {:keys [outline]}]]
                                (when (some? outline)
                                  [(str "  ." (svg-smil-anim-class id) " { display: none; }")
                                   (str "  ." (svg-smil-rest-class id) " { display: inline; }")]))
                              smil)
                      (map (fn [id]
                             (str "  ." (svg-clipped-class id) " { clip-path: url(#"
                                  (svg-clip-rest-id id) "); }"))
                           clipped)
                      (map (fn [[text-id k]]
                             (str "  ." (unit-css-class text-id k) " { filter: none; }"))
                           (keys unit-blurs)))]
    (when (seq rules)
      (str "@media (prefers-reduced-motion: reduce) {\n" (str/join "\n" rules) "\n}"))))

(defn- svg-rest-copy
  "`markup` without the marks of its outline (see `svg-smil-outline`),
  its ids, and what refers to them, ending in `-rest`, so it draws beside
  itself."
  [markup]
  (reduce (fn [markup id]
            (-> markup
                (str/replace (str "id=\"" id "\"") (str "id=\"" id "-rest\""))
                (str/replace (str "#" id ")") (str "#" id "-rest)"))
                (str/replace (str "#" id "\"") (str "#" id "-rest\""))))
          (str/replace markup " data-penpot-outline=\"\"" "")
          (distinct (map second (re-seq #" id=\"([^\"]+)\"" markup)))))

(defn- svg-smil-vectors
  "The vectors `markup` of the layer `id` (see `svg-vectors`) with the
  path of their outline SMIL plays (see `svg-smil`), and beside them, for
  reduced motion, which SMIL does not heed, a copy at rest (see
  `svg-rest-copy`, `svg-smil-css`)."
  [{:keys [smil ox oy]} id markup]
  (let [{:keys [outline inner]} (get smil id)
        ;; the inner shadows of a board on a filter around its vectors
        inside (fn [markup defs?]
                 (if (some? inner)
                   (str (when defs? (str "<defs>" inner "</defs>"))
                        "<g class=\"" (svg-escape (svg-smil-inner-class id))
                        "\" filter=\"url(#" (svg-escape (svg-inner-id id false)) ")\">" markup "</g>")
                   markup))]
    (if (some? outline)
      (str "<g class=\"" (svg-escape (svg-smil-anim-class id)) "\">"
           (svg-vectors (inside (svg-smil-outline markup outline) true) ox oy) "</g>"
           "<g class=\"" (svg-escape (svg-smil-rest-class id)) "\" display=\"none\">"
           (svg-vectors (inside (svg-rest-copy markup) false) ox oy) "</g>")
      (svg-vectors (inside markup true) ox oy))))

(defn- svg-effects-around
  "`svg`, in the SVG, whose origin is `ox`/`oy`, on the filter of the
  effects of the layer `id` (see `svg-smil`), if any: its vectors and the
  layers in it, which it shadows and blurs as the canvas does."
  [{:keys [smil ox oy]} id svg]
  (if-let [filters (dm/get-in smil [id :filters])]
    (str "<g transform=\"translate(" (acss/fmt (- ox)) " " (acss/fmt (- oy)) ")\"><defs>" filters "</defs>"
         "<g class=\"" (svg-escape (svg-smil-class id))
         "\" filter=\"url(#" (svg-escape (svg-effects-id id false)) ")\">"
         "<g transform=\"translate(" (acss/fmt ox) " " (acss/fmt oy) ")\">" svg "</g></g></g>")
    svg))

(defn- svg-follow-class
  "The CSS class of the group that moves the layer `id` with the board it
  is in (see `svg-follow-css`)."
  [id]
  (str "penpot-follow-" id))

(defn- svg-follow-css
  "How the layers among `ids` in the boards `holders` move with them (see
  `board-follow`), each in a group of its own (see `svg-follow-class`).
  `timeline` is cut to its duration; the SVG starts at `ox`/`oy`. `{:css
  :ids}`, the layers that move."
  [timeline objects holders ids propagate ox oy]
  (let [duration   (max 1 (:duration timeline))
        iterations (svg-iterations timeline)
        moves
        (for [[cid stops] (aexp/board-follow timeline objects holders ids propagate ox oy)]
          (let [name     (str "penpot-follow-" cid)
                selector (str "." (svg-follow-class cid))
                css      (for [[t kf matrix] stops]
                           (svg-stop-css t duration
                                         [(str "transform: matrix(" (str/join ", " (map acss/fmt matrix)) ");")]
                                         (some-> kf acss/keyframe-timing)))]
            [cid selector
             (str "@keyframes " name " {\n" (str/join "\n" css) "\n}\n\n"
                  selector " {\n  transform-origin: 0 0;\n  animation: " name " " duration "ms linear "
                  iterations ";\n}")]))]
    {:ids (into #{} (map first) moves)
     :css (when (seq moves)
            (str (str/join "\n\n" (map #(nth % 2) moves))
                 "\n\n@media (prefers-reduced-motion: reduce) {\n  "
                 (str/join ",\n  " (map second moves))
                 " {\n    animation: none;\n  }\n}"))}))

(defn- svg-id-attr
  [svg-ids id]
  (when-let [name (get svg-ids id)]
    (str " id=\"" (svg-escape name) "\"")))

(defn- emit-svg-node
  "The SVG of a node of `layer-nodes`: a layer is a group its CSS class
  animates, holding its image and the layers inside it, which so follow
  it; a board that clips keeps those to its outline. A text whose units
  `images` has pictures of (see `timeline->svg`) holds, instead of its
  image, the picture of what is no unit and a group of each unit, which
  its CSS class animates. All images sit where they are in the SVG,
  whose origin is `ox`/`oy`. A layer of `svg-ids` has that `id`; one of
  `follow` sits in a group that moves it with the board it is in (see
  `svg-follow-css`). SMIL plays what CSS can not on vectors (see
  `svg-smil-vectors`), the effects of a layer around it and the layers
  in it (see `svg-effects-around`) and the blur of the units of a text
  (`unit-blurs`, see `svg-unit-blurs`)."
  [{:keys [objects images svg-ids ox oy follow unit-blurs] :as ctx} {:keys [id inner clip?]}]
  (when-let [shape (get objects id)]
    (let [inner   (str/join "" (keep #(emit-svg-node ctx %) inner))
          clipped #(if (and (seq inner) (aexp/clipping-board? shape))
                     (svg-clip-group objects (:smil ctx) id %)
                     %)
          svg
          (cond
            clip?
            (clipped inner)

            (seq (unit-image-indexes images id))
            (let [opacity (or (:opacity shape) 1)]
              (str "<g class=\"" (svg-escape (acss/shape-css-class id)) "\""
                   (svg-id-attr svg-ids id)
                   (when (< opacity 1) (str " opacity=\"" (acss/fmt opacity) "\""))
                   ">"
                   (svg-effects-around
                    ctx id
                    (str (when-let [blurs (seq (keep #(get unit-blurs [id %]) (unit-image-indexes images id)))]
                           (str "<defs>" (str/join "" blurs) "</defs>"))
                         (some-> (get images [id :rest]) (svg-drawing ox oy))
                         (str/join "" (map (fn [k]
                                             (str "<g class=\"" (svg-escape (unit-css-class id k)) "\""
                                                  (when (contains? unit-blurs [id k])
                                                    (str " filter=\"url(#" (svg-escape (svg-unit-blur-id id k)) ")\""))
                                                  ">"
                                                  (svg-drawing (get images [id k]) ox oy)
                                                  "</g>"))
                                           (unit-image-indexes images id)))
                         inner))
                   "</g>"))

            :else
            (let [img     (get images id)
                  sr      (:selrect shape)
                  ;; the image is opaque; an animated opacity overrides this
                  opacity (or (:opacity shape) 1)]
              (str "<g class=\"" (svg-escape (acss/shape-css-class id)) "\""
                   (svg-id-attr svg-ids id)
                   (when (< opacity 1) (str " opacity=\"" (acss/fmt opacity) "\""))
                   ">"
                   (svg-effects-around
                    ctx id
                    (clipped
                     (str (if-let [markup (:markup img)]
                            ;; its vectors
                            (svg-smil-vectors ctx id markup)
                            ;; the picture of the shape as it is, turned, over its
                            ;; box with shadows and blur (`:x :y` of the image), or
                            ;; its box
                            (str "<image href=\"" (svg-escape (or (:href img) ""))
                                 "\" x=\"" (acss/fmt (or (:x img) (- (or (:x sr) 0) ox)))
                                 "\" y=\"" (acss/fmt (or (:y img) (- (or (:y sr) 0) oy)))
                                 "\" width=\"" (acss/fmt (or (:width img) (:width sr) 0))
                                 "\" height=\"" (acss/fmt (or (:height img) (:height sr) 0)) "\"/>"))
                          inner)))
                   "</g>")))]
      (if (contains? follow id)
        (str "<g class=\"" (svg-escape (svg-follow-class id)) "\">" svg "</g>")
        svg))))

(defn- svg-style
  "An SVG `<style>` of `css`, as character data: an SVG is XML, where a
  name with `&` or `<` in a comment, or a font family, would end it."
  [css]
  (str "<style><![CDATA[" (str/replace css "]]>" "]]]]><![CDATA[>") "]]></style>"))

(defn timeline->svg
  "Build an animated SVG: rest-pose `<image>` per export layer plus the
  CSS from `timeline->css`. The layers nest as `layer-nodes` says, so a
  layer follows the one it sits in and a board clips them; each turns
  and scales around its pivot in the SVG. `images` is `{shape-id {:href
  :x :y :width :height}}`; the board's own image, when there, goes below
  them, all in a group that plays the board's own animation and has its
  opacity, and the board clips them. A split text plays its units one by
  one when `images` has pictures of them, `[text-id k]` of the unit `k`
  (see `unit-timelines`) and, when it has ink (an underline), `[text-id
  :rest]` of what is no unit, each over the box it covers (with the text
  turned), or their vectors (`:markup`, see `svg-vectors`), with the
  middle of the box of a unit as `:x :y` of no size and what its glyphs
  cover as `:box`, where its blur plays (see `svg-unit-blurs`); otherwise its split
  animations play as animations of the whole text (see `flatten-splits`). An entry of `images` with `:markup`
  instead holds the vectors of its layer, the SVG of the shape at rest as
  it is on the page (see `svg-vectors`), and `:fonts` (an option) the
  `@font-face` CSS of their texts; the layers are then those of
  `svg-export-layer-ids`. The width and height of a layer play on its
  vectors when they can (see `svg-sized?`), and the layers in a board so
  resized move with it as `:propagate` says (an option, see
  `svg-follow-css`), else they keep their place. Options, as Figma's:
  `:ids?`, the names of the layers and of the board as their `id`s (see
  `svg-layer-ids`), and `:bbox?`, a bounding box, an empty rectangle over
  the board, first."
  ([timeline objects images]
   (timeline->svg timeline objects images nil))
  ([timeline objects images {:keys [ids? bbox? fonts propagate]}]
   (let [unit-texts (aexp/unit-text-ids images)
         timeline (-> timeline
                      (cta/flatten-splits #{:char :word :line} unit-texts)
                      (cta/resolve-animations objects)
                      cta/expand-loops)
         board-id (:board-id timeline)
         board    (get objects board-id)
         bsr      (:selrect board)
         width    (mth/round (or (:width bsr) 100))
         height   (mth/round (or (:height bsr) 100))
         ox       (or (:x bsr) 0)
         oy       (or (:y bsr) 0)
         sized    (aexp/svg-sized-ids objects timeline)
         holders  (aexp/svg-holder-ids objects sized)
         ;; SVG takes a share of `transform-origin` of the whole drawing
         css      (acss/timeline->css timeline objects
                                      {:svg? true
                                       :sized sized
                                       :origin (fn [shape track]
                                                 (let [[x y] (aexp/lottie-pivot shape ox oy (cta/track-origin track))]
                                                   (str (acss/fmt x) "px " (acss/fmt y) "px")))
                                       :units (when (seq unit-texts)
                                                {:kinds #{:char :word :line}
                                                 :texts unit-texts
                                                 :selector (fn [text k] (str "." (unit-css-class (:id text) k)))
                                                 ;; each unit turns around the middle of its picture
                                                 :origin (fn [text k]
                                                           (let [{:keys [x y width height]} (get images [(:id text) k])]
                                                             (str (acss/fmt (+ (or x 0) (/ (or width 0) 2))) "px "
                                                                  (acss/fmt (+ (or y 0) (/ (or height 0) 2))) "px")))
                                                 :adjust (fn [text units] (aexp/turned-moves units (:rotation text)))})})
         played   (-> timeline cta/expand-paths cta/cut-timeline)
         ids      (cta/export-layer-ids objects board-id (:tracks timeline) (aexp/export-holders objects timeline))
         nodes    (aexp/layer-nodes objects (set ids) board-id)
         ;; the layers holding layers, which may go anywhere in the view
         wide     (into #{board-id}
                        (comp (filter (comp seq :inner)) (map :id))
                        (tree-seq :inner :inner {:inner nodes}))
         smil     (svg-smil played objects images wide
                            {:x (- ox (/ width 2)) :y (- oy (/ height 2)) :width (* 2 width) :height (* 2 height)})
         unit-blurs (svg-unit-blurs played objects images unit-texts {:x 0 :y 0 :width width :height height})
         follow   (when (some? propagate)
                    (svg-follow-css played objects holders ids propagate ox oy))
         clip?    (and (seq nodes) (aexp/clipping-board? board))
         clip-ids (cond-> (into []
                                (comp (filter #(and (seq (:inner %))
                                                    (aexp/clipping-board? (get objects (:id %)))))
                                      (map :id))
                                (rest (tree-seq :inner :inner {:inner nodes})))
                    clip? (conj board-id))
         clips    (svg-clip-paths objects (:tracks timeline) smil ox oy clip-ids)
         svg-ids  (when ids? (svg-layer-ids objects (cons board-id ids)))
         backdrop (when-let [img (get images board-id)]
                    (if-let [markup (:markup img)]
                      (str "<g" (svg-id-attr svg-ids board-id) ">"
                           (svg-smil-vectors {:smil smil :ox ox :oy oy} board-id markup) "</g>")
                      (str "<image" (svg-id-attr svg-ids board-id)
                           " href=\"" (svg-escape (:href img))
                           "\" x=\"0\" y=\"0\" width=\"" width "\" height=\"" height "\"/>")))
         layers   (str/join "" (keep #(emit-svg-node {:objects objects :images images :svg-ids svg-ids
                                                      :ox ox :oy oy :follow (:ids follow) :smil smil
                                                      :unit-blurs unit-blurs}
                                                     %)
                                     nodes))
         opacity  (or (:opacity board) 1)
         content  (svg-effects-around
                   {:smil smil :ox ox :oy oy} board-id
                   (str backdrop
                        (if clip?
                          (svg-clip-group objects smil board-id layers)
                          layers)))]
     (str "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" width
          "\" height=\"" height "\" viewBox=\"0 0 " width " " height
          "\" overflow=\"hidden\">"
          (when (seq clips) (str "<defs>" clips "</defs>"))
          (svg-style
           (str "g { transform-box: view-box; }\n" (some-> fonts (str "\n")) css
                (some->> (svg-appearance-css played objects) (str "\n\n"))
                (some->> (:css follow) (str "\n\n"))
                (some->> (svg-smil-css smil unit-blurs
                                       (filterv #(svg-clip-animated? objects smil %) clip-ids))
                         (str "\n\n"))))
          (when bbox?
            (str "<rect width=\"" width "\" height=\"" height "\" fill=\"none\"/>"))
          ;; the board's own animation and opacity, of all in it
          (if (or (seq (dm/get-in timeline [:tracks board-id :keyframes])) (< opacity 1))
            (str "<g class=\"" (svg-escape (acss/shape-css-class board-id)) "\""
                 (when (< opacity 1) (str " opacity=\"" (acss/fmt opacity) "\""))
                 ">" content "</g>")
            content)
          "</svg>"))))
