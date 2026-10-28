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
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes.bounds :as gsb]
   [app.common.geom.shapes.corners :as gco]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.animation-paint :as ctap]
   [app.common.types.color :as clr]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [clojure.string :as str]))

(declare trim-pieces-of)

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

(defn- svg-name
  "`name` as an XML name (an `id`): but letters, marks, digits, `_`, `.`,
  `-` and `·`, what it holds as `_`, and `_` before it when it starts
  with a digit, `.` or `-`, which can not start one."
  [name]
  (let [name #?(:clj  (str/replace name #"[^\p{L}\p{M}\p{Nd}_.\-\u00B7]" "_")
                :cljs (.replace name (js/RegExp. "[^\\p{L}\\p{M}\\p{Nd}_.\\-\\u00B7]" "gu") "_"))]
    (if (re-find #"^[0-9.-]" name) (str "_" name) name)))

(defn svg-layer-ids
  "The `id`s of the layers `ids` (shape ids) in an SVG export, by shape
  id, as Figma writes them: their names as XML names (see `svg-name`),
  the second layer of a name `<name>_2` and so on, each unlike the ids
  before it."
  [objects ids]
  (loop [ids   (seq ids)
         taken #{}
         out   {}]
    (if-let [id (first ids)]
      (let [base   (svg-name (str (or (not-empty (:name (get objects id))) "layer")))
            unique (d/seek #(not (contains? taken %))
                           (cons base (map #(str base "_" %) (iterate inc 2))))]
        (recur (next ids) (conj taken unique) (assoc out id unique)))
      out)))

(def ^:private svg-fill-properties #{:fill-color :fill-opacity})
(def ^:private svg-stroke-properties
  #{:stroke-color :stroke-opacity :stroke-width :stroke-dash :stroke-gap :stroke-dash-offset})
(def ^:private svg-trim-properties #{:trim-start :trim-end :trim-offset})

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

(defn- svg-dash-declarations
  "The dash pattern of the dashed `stroke` its dash, gap and offset in
  `values` play (at `index`), the others as they rest (see
  `ctap/stroke-dash`); none when the stroke trims (`trimmed?`), whose trim
  takes the dash of its vectors (see `svg-trim-declarations`)."
  [stroke index values trimmed?]
  (when (and (ctap/dashed-stroke? stroke) (not trimmed?)
             (some #(contains? values %) [:stroke-dash :stroke-gap :stroke-dash-offset]))
    (let [[dash gap] (ctap/stroke-dash stroke)
          dash   (or (indexed-value (:stroke-dash values) index) dash)
          gap    (or (indexed-value (:stroke-gap values) index) gap)
          offset (or (indexed-value (:stroke-dash-offset values) index) (:stroke-dash-offset stroke) 0)]
      [(str "stroke-dasharray: " (acss/fmt dash) " " (acss/fmt gap) ";")
       (str "stroke-dashoffset: " (acss/fmt offset) ";")])))

(defn trim-piece-class
  "The class of the element drawing the subpath `k` of a trimmed stroke
  (see `aexp/trim-pieces`), whose trim plays from where it begins on
  them all (see `svg-trim-declarations`)."
  [k]
  (str "trim-piece-" k))

(defn- svg-trim-declarations
  "The dash of the strokes of a layer drawn from `:trim-start` to
  `:trim-end` moved along by `:trim-offset` of `values`, on outlines of
  length 1 (the vectors of the layer have a `pathLength` of 1), or on
  a piece of them that begins at `from` of that length (its `pathLength`
  its share of it, see `trim-piece-class`)."
  [values & [from]]
  (when (some #(contains? values %) svg-trim-properties)
    (let [{:keys [trim-start trim-end trim-offset]} (merge cta/trim-properties (select-keys values svg-trim-properties))
          length (mth/clamp (- trim-end trim-start) 0 1)]
      [(str "stroke-dasharray: " (acss/fmt length) " " (acss/fmt (- 1 length)) ";")
       (str "stroke-dashoffset: " (acss/fmt (- (or from 0) (+ trim-start trim-offset))) ";")])))

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
          pivot   (cta/box-point shape (cta/track-origin track))
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
        ;; an outline of several subpaths trims along them all, each
        ;; drawn by an element of its own (see `trim-piece-class`); the
        ;; rule of the whole stays for one drawn whole
        pieces    (trim-pieces-of shape track)
        ;; the renderer paints the fills in a pattern of the shape, each
        ;; with its opacity there, when one is not a flat colour or there
        ;; are several (as `url-fill?` of `app.main.ui.shapes.attrs`)
        pattern?  (or (> (count fills) 1)
                      (some :fill-color-gradient fills)
                      (some :fill-image fills))
        fill-targets
        (for [index (kf-index svg-fill-properties)
              :when (or text? (< index (count fills)))
              :let  [image? (and (not text?) (some? (:fill-image (get fills index))))]]
          [[(cond
              ;; its `text`, or the outlines of its units (see
              ;; `timeline->svg`)
              (or text? (not pattern?)) (str "[id=\"fills-" sid "\"] > :not(defs)")
              :else                     (str "[id=\"shape-" sid "\"] > defs > pattern > g > :nth-child("
                                             (- (count fills) index) ")"))]
           (fn [values]
             (let [color   (indexed-value (:fill-color values) index)
                   opacity (indexed-value (:fill-opacity values) index)]
               (cond-> []
                 (some? color)   (conj (str "fill: " color ";"))
                 ;; an `image` takes no fill opacity
                 (some? opacity) (conj (str (if image? "opacity: " "fill-opacity: ")
                                            (acss/fmt opacity) ";")))))
           svg-fill-properties])
        ;; a text draws its strokes along its glyphs, each line or run in a
        ;; group of its own: its trim stays as it rests, as a `text` takes
        ;; no length of its path
        stroke-targets
        (mapcat
         (fn [index]
           (let [;; inner and outer strokes are drawn twice as wide, then cut
                 factor (if (= :center (aexp/svg-stroke-alignment shape (get strokes index))) 1 2)
                 {:keys [paint mask]} (svg-stroke-elements shape index)
                 width  #(when-let [w (indexed-value (:stroke-width %) index)]
                           (str "stroke-width: " (acss/fmt (* factor w)) ";"))
                 decls  (fn [values from]
                          (let [color   (indexed-value (:stroke-color values) index)
                                opacity (indexed-value (:stroke-opacity values) index)
                                trims   (when-not text? (svg-trim-declarations values from))]
                            (cond-> (vec trims)
                              (some? color)   (conj (str "stroke: " color ";"))
                              (some? opacity) (conj (str "stroke-opacity: " (acss/fmt opacity) ";"))
                              (some? (width values)) (conj (width values))
                              :always (into (svg-dash-declarations (get strokes index) index values
                                                                   (or (seq trims) (seq pieces)))))))
                 reads  (cond-> svg-stroke-properties (not text?) (into svg-trim-properties))]
             (cond-> (into [[[paint] #(decls % 0) reads]]
                           (map-indexed (fn [k [from _]]
                                          [[(str paint "." (trim-piece-class k))] #(decls % from) reads]))
                           pieces)
               ;; the mask of an outer stroke is as wide
               (and (some? mask) (contains? props :stroke-width))
               (conj [[mask]
                      (fn [values] (keep identity [(width values)]))
                      #{:stroke-width}]))))
         (filter #(< % (count strokes))
                 (into (kf-index svg-stroke-properties)
                       (when (and trim? (not text?)) (range (count strokes))))))
        radius-targets
        (when (and (some aexp/svg-radius-properties props) (aexp/svg-even-corners? shape track))
          [[(svg-outline-elements shape)
            (fn [values]
              (when-let [r (some #(get values %) [:r1 :r2 :r3 :r4])]
                [(str "rx: " (acss/fmt r) "px;") (str "ry: " (acss/fmt r) "px;")]))
            aexp/svg-radius-properties]])
        size-targets
        (when (or (aexp/svg-sized? shape track) (aexp/svg-box-sized? shape track))
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
  sizes as `svg-sized?` and `svg-box-sized?` say. `timeline` is the one the exports play,
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
           ;; a hidden layer, or one in a hidden one, has no elements
           :when (and (some? shape) (some appearance props)
                      (not (aexp/hidden-in? objects (:board-id timeline) sid)))
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
  `svg-effects-filter`), SMIL playing those its track animates, its
  vectors drawn without them: when it has shadows to show."
  [shape]
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
  `shape` by SMIL, on the path of its outline (see `svg-outline-path`): a
  rectangle or board whose corners are not alike (see
  `svg-even-corners?`), drawn as a path (the elements of its outline
  marked `data-penpot-outline`, see `svg-smil-outline`)."
  [shape track]
  (boolean
   (and (contains? #{:rect :frame} (:type shape))
        (some aexp/svg-radius-properties (track-properties track))
        (not (aexp/svg-even-corners? shape track)))))

(defn svg-smil-morph?
  "Whether an SVG of vectors plays the outline `track` animates on the
  path `shape` (see `ctao/outline-of`) by SMIL, on the path of its outline
  (see `svg-outline-path`)."
  [shape track]
  (boolean
   (and (= :path (:type shape))
        (some #(= :content (:property %)) (:keyframes track)))))

(defn smil-outline?
  "Whether SMIL plays on the outline of `shape` what `track` animates:
  its corners (see `svg-smil-corners?`), its size (see
  `aexp/svg-outline-sized?`) or its outline (see `svg-smil-morph?`)."
  [shape track]
  (boolean (and (some? shape)
                (or (svg-smil-corners? shape track)
                    (aexp/svg-outline-sized? shape track)
                    (svg-smil-morph? shape track)))))

(defn trim-pieces-of
  "The pieces (see `aexp/trim-pieces`) the stroke of `shape` is drawn by,
  one element each, when `track` trims it: an outline of several
  subpaths, but of a text (its trim stays as it rests) or of an outline
  SMIL plays (see `smil-outline?`), which stays whole. Nil when it is
  drawn whole."
  [shape track]
  (when (and (some svg-trim-properties (track-properties track))
             (not= :text (:type shape))
             (not (smil-outline? shape track)))
    (aexp/trim-pieces shape)))

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
  nil when it keeps its first one, but `always?`."
  [timeline attr points render & {:keys [always?]}]
  (when (or always? (not-every? #(= (second (first points)) (second %)) points))
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

(defn- render-hex
  [rgb]
  (clr/rgb->hex (mapv #(mth/clamp (mth/round %) 0 255) rgb)))

(defn- unwrap-turns
  "`points` (see `smil-points`) of a turn in degrees, each the way round
  nearest the one before, so SMIL turns the short way."
  [points]
  (reduce (fn [out [t [a] curve]]
            (let [prev (first (second (peek out)))
                  a    (if (some? prev)
                         (let [delta (mod (- a prev) 360)]
                           (+ prev (if (> delta 180) (- delta 360) delta)))
                         a)]
              (conj out [t [a] curve])))
          []
          points))

(def ^:private gradient-paints
  "Where the gradients the keyframes of a gradient property play are: the
  paints of `shape` that hold them, the key of the gradient in one, and
  the id the SVG renderer gives it, but its render id and index."
  {:fill-gradient   {:paints :fills :paint? cta/gradient-fill?
                     :key :fill-color-gradient :id "fill-color-gradient"}
   :stroke-gradient {:paints :strokes :paint? cta/gradient-stroke?
                     :key :stroke-color-gradient :id "stroke-color-gradient"}})

(defn- rest-gradient
  "The gradient of the paint `index` of `shape` that `property` (see
  `gradient-paints`) plays, as it rests."
  [shape property index]
  (let [{:keys [paints key]} (get gradient-paints property)]
    (get (get (cta/as-vec (get shape paints)) index) key)))

(defn- gradient-tracks
  "The keyframes `track` plays on the gradients of the fills and strokes
  of `shape`, by `[property index]` of the paint: of paints that are
  gradients, of a layer that is no text (whose paints the SVG renderer
  draws once a line)."
  [shape track]
  (when (not= :text (:type shape))
    (->> (:keyframes track)
         (filter (fn [{:keys [property index]}]
                   (when-let [{:keys [paints paint?]} (get gradient-paints property)]
                     (paint? (get (cta/as-vec (get shape paints)) index)))))
         (group-by (juxt :property :index))
         (not-empty))))

(def ^:private gradient-geometry [:start-x :start-y :end-x :end-y :width])

(defn svg-gradient-static?
  "Whether `track` plays on the gradients of the fills and strokes of
  `shape` what an SVG of vectors keeps as it rests (see
  `svg-gradient-smil`): the gradients of a text, the points of a radial
  one of a path, and the type of one that changes it, which keeps the one
  it rests in."
  [shape track]
  (let [kfs (filter #(ctap/gradient-property? (:property %)) (:keyframes track))]
    (boolean
     (and (seq kfs)
          (or (= :text (:type shape))
              (some (fn [[[property index] kfs]]
                      (let [rest (rest-gradient shape property index)]
                        (some (fn [{:keys [value]}]
                                (or (not= (:type value) (:type rest))
                                    (and (= :radial (:type rest))
                                         (= :path (:type shape))
                                         (not= (select-keys value gradient-geometry)
                                               (select-keys rest gradient-geometry)))))
                              kfs)))
                    (group-by (juxt :property :index) kfs)))))))

(defn- svg-gradient-smil
  "What SMIL plays in the gradient of the fill or stroke `index` of
  `shape` that `property` plays (see `gradient-paints`), its keyframes
  `kfs` of `timeline` (cut to its duration), drawn as the SVG
  renderer draws it: the `<animate>`s of its points (`x1` to `y2` of a
  linear one; the centre and radius of a radial one, and its turn and
  width on its `gradientTransform`, but on a path, whose transform the
  renderer bakes in) and its stops, as many as the gradient with most
  has (see `ctap/pad-stops`), each with its own. Nil without any."
  [timeline shape [property index] kfs]
  (let [rest     (rest-gradient shape property index)
        values   (acss/shape-values timeline (:id shape))
        at       (memoize #(or (indexed-value (get (values %) property) index) rest))
        most     (apply max-key #(count (:stops %)) rest (map :value kfs))
        n        (count (:stops most))
        stops-at (memoize (fn [t]
                            (let [g (at t)]
                              (if (= n (count (:stops g)))
                                (ctap/gradient-stops g)
                                (ctap/pad-stops g most)))))
        points   #(smil-points % kfs (max 1 (:duration timeline)))
        animate  (fn [attr value render] (smil-animate timeline attr (points value) render))
        vec-of   (fn [g] (gpt/point (- (:end-x g) (:start-x g)) (- (:end-y g) (:start-y g))))
        geometry
        (case (:type rest)
          :linear
          (keep (fn [[attr k]] (animate attr #(vector (get (at %) k)) render-number))
                [["x1" :start-x] ["y1" :start-y] ["x2" :end-x] ["y2" :end-y]])

          :radial
          (when-not (= :path (:type shape))
            ;; translate(start) rotate(turn) scale(width 1) translate(-start)
            (let [moves [["translate" "replace" (points #(let [g (at %)] [(:start-x g) (:start-y g)]))]
                         ["rotate" "sum" (unwrap-turns (points #(vector (+ 90 (gpt/angle (vec-of (at %)))))))]
                         ["scale" "sum" (points #(vector (:width (at %)) 1))]
                         ["translate" "sum" (points #(let [g (at %)] [(- (:start-x g)) (- (:start-y g))]))]]
                  moves? (some (fn [[_ _ points]]
                                 (not-every? #(= (second (first points)) (second %)) points))
                               moves)]
              (concat
               (keep (fn [[attr value]] (animate attr value render-number))
                     [["cx" #(vector (:start-x (at %)))]
                      ["cy" #(vector (:start-y (at %)))]
                      ["r" #(vector (gpt/length (vec-of (at %))))]])
               (when moves?
                 (map (fn [[type additive points]]
                        (-> (smil-animate timeline "gradientTransform" points
                                          #(str/join " " (map acss/fmt %)) :always? true)
                            (str/replace-first "<animate " (str "<animateTransform type=\"" type
                                                                "\" additive=\"" additive "\" "))))
                      moves)))))

          nil)
        stops
        (map (fn [k]
               (let [stop #(nth (stops-at %) k)
                     s0   (stop 0)]
                 (str "<stop offset=\"" (acss/fmt (:offset s0)) "\" stop-color=\"" (:color s0)
                      "\" stop-opacity=\"" (acss/fmt (:opacity s0)) "\">"
                      (animate "offset" #(vector (:offset (stop %))) render-number)
                      (animate "stop-color" #(clr/hex->rgb (:color (stop %))) render-hex)
                      (animate "stop-opacity" #(vector (:opacity (stop %))) render-number)
                      "</stop>")))
             (range n))]
    (when (some? rest)
      (str (str/join "" geometry) (str/join "" stops)))))

(defn- svg-smil-gradients
  "`markup` with what SMIL plays in the gradients of its fills and
  strokes (see `svg-gradient-smil`), by `[property index]` of the paint,
  in place of their stops."
  [markup gradients]
  (reduce-kv (fn [markup [property index] inner]
               (str/replace markup
                            (re-pattern (str "(?s)(<(linearGradient|radialGradient) [^>]*?id=\""
                                             (:id (get gradient-paints property)) "-[^\"]*-"
                                             index "\"[^>]*>).*?(</\\2>)"))
                            (fn [[_ open _ close]] (str open inner close))))
             markup
             gradients))

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

(defn- outline-size
  "The box `[x y width height]` of `shape` (in its own space, its turn
  aside) its width and height `values` give, resized around its pivot
  `origin` (a share of its box), as the canvas resizes it (see
  `cta/shape->modifiers`)."
  [shape origin values]
  (let [{:keys [x y width height]} (:selrect shape)
        w (double (get values :width width))
        h (double (get values :height height))
        px (+ x (* width (:x origin)))
        py (+ y (* height (:y origin)))]
    [(- px (* w (:x origin))) (- py (* h (:y origin))) w h]))

(defn- svg-drawn-path
  "The function of the values of the path `shape` at a time (see
  `acss/shape-values`) to its outline then, on the page, as an SVG of
  vectors plays it on its path (see `svg-outline-path`): its points
  resized along its own sides around its pivot, as the canvas resizes
  it, and the outline the row `track` plays when it morphs (see
  `svg-smil-morph?`)."
  [shape track]
  (let [{:keys [x y width height]} (:selrect shape)
        origin (cta/track-origin track)
        morph? (svg-smil-morph? shape track)
        center (gpt/point (+ x (/ width 2)) (+ y (/ height 2)))
        pivot  (gpt/point (+ x (* width (:x origin))) (+ y (* height (:y origin))))
        turn   (:transform shape)
        unturn (:transform-inverse shape)]
    (fn [vs]
      (let [sx (/ (double (get vs :width width)) width)
            sy (/ (double (get vs :height height)) height)
            m  (cond->> (gmt/scale-matrix (gpt/point sx sy) pivot)
                 (and (some? turn) (some? unturn))
                 (#(gmt/multiply (gmt/transform-in center turn) %
                                 (gmt/transform-in center unturn))))]
        (path/transform-content (if (and morph? (some? (:content vs)))
                                  (ctao/outline->content shape track (:content vs))
                                  (:content shape))
                                m)))))

(defn- svg-played-box
  "How the box of `shape` plays as `timeline` (cut to its duration) plays
  the size of its box or outline on its elements (see
  `aexp/svg-box-sized?`, `aexp/svg-outline-sized?`) or its outline (see
  `svg-smil-morph?`): `{:times :corners}`, the times to take it at
  (between its keyframes too, as its easings may go past them) and the
  function of a time to the corners of its box then: of the outline it
  draws for a path that morphs, on the page; else of its box in its own
  space and, turned by its `transform`, on the page. Nil when none of
  them plays."
  [shape timeline]
  (let [sid    (:id shape)
        track  (dm/get-in timeline [:tracks sid])
        sized? (or (aexp/svg-box-sized? shape track) (aexp/svg-outline-sized? shape track))
        morph? (svg-smil-morph? shape track)]
    (when (or sized? morph?)
      (let [values   (acss/shape-values timeline sid)
            origin   (cta/track-origin track)
            duration (max 1 (:duration timeline))
            {:keys [x y width height]} (:selrect shape)
            turn     (when-let [transform (:transform shape)]
                       (gmt/transform-in (gpt/point (+ x (/ width 2)) (+ y (/ height 2))) transform))
            drawn    (when morph? (svg-drawn-path shape track))]
        {:times   (into (sorted-set duration)
                        (concat (map #(* duration (/ % 120)) (range 120))
                                (keep #(when (<= 0 (:time %) duration) (:time %)) (:keyframes track))))
         :corners (if morph?
                    (fn [t] (grc/rect->points (path/calc-selrect (drawn (values t)))))
                    (fn [t]
                      (let [[bx by bw bh] (outline-size shape origin (values t))
                            own (for [px [bx (+ bx bw)] py [by (+ by bh)]] (gpt/point px py))]
                        (cond-> own (some? turn) (concat (map #(gpt/transform % turn) own))))))}))))

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
  the layers in it may cover, and `played`, its box as it plays (see
  `svg-played-box`), when it does. Of its shadows, those of `styles`
  (both kinds by default)."
  [id shape timeline {:keys [shadows? blur? area styles played] :or {styles #{:drop-shadow :inner-shadow}}}]
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
        ;; the box as it plays its size or its outline (`played`, see
        ;; `svg-played-box`), with its effects then
        played   (when (some? timeline) played)
        times    (-> #{0 duration}
                     (into (map :time) (concat kfs blur-kfs))
                     (into (:times played)))
        bounds   (grc/join-rects
                  (cond-> (mapv #(gsb/get-shape-filter-bounds
                                  (cond-> (assoc shape :shadow (shadows %))
                                    (some? played) (assoc :points ((:corners played) %))
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
  corners clamped, see `svg-outline-path`) as lines and curves: no arcs,
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

(def ^:private path-number
  #"-?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?")

(defn- render-path-numbers
  "The path data `d` with its numbers, in order, those of `numbers`."
  [d numbers]
  (let [numbers (volatile! (seq numbers))]
    (str/replace d path-number (fn [_]
                                 (let [n (first @numbers)]
                                   (vswap! numbers next)
                                   (acss/fmt n))))))

(defn- svg-outline-path
  "The `<animate>` of the path of the outline of `shape` whose corners,
  size or outline `timeline` (cut to its duration) plays (see
  `svg-smil-corners?`, `aexp/svg-outline-sized?`, `svg-smil-morph?`): a
  rectangle in its own space (its turn is the `transform` of the path),
  its corners clamped as the canvas clamps them; a path resized along its
  own sides around its pivot, as the canvas resizes it, its strokes as
  wide, and morphing, its outlines of one structure (see
  `cta/match-outlines`)."
  [shape timeline]
  (let [sid    (:id shape)
        track  (dm/get-in timeline [:tracks sid])
        values (acss/shape-values timeline sid)
        origin (cta/track-origin track)
        sized? (aexp/svg-outline-sized? shape track)
        morph? (svg-smil-morph? shape track)
        kfs    (filterv #(or (aexp/svg-radius-properties (:property %))
                             (and sized? (aexp/svg-size-properties (:property %)))
                             (and morph? (= :content (:property %))))
                        (:keyframes track))
        points #(smil-points % kfs (max 1 (:duration timeline)))]
    (if (contains? #{:rect :frame} (:type shape))
      (smil-animate timeline "d"
                    (points (fn [t]
                              (let [vs (values t)
                                    [x y w h] (if sized? (outline-size shape origin vs) (outline-size shape origin {}))]
                                (into [x y w h]
                                      (gco/shape-corners-4 {:width w :height h
                                                            :r1 (get vs :r1 (or (:r1 shape) 0))
                                                            :r2 (get vs :r2 (or (:r2 shape) 0))
                                                            :r3 (get vs :r3 (or (:r3 shape) 0))
                                                            :r4 (get vs :r4 (or (:r4 shape) 0))})))))
                    render-corner-path)
      ;; a path: its points resized as the canvas resizes it, along its own
      ;; sides when it is turned, and its outline as it morphs
      (let [drawn   (svg-drawn-path shape track)
            ;; the numbers of every value go into the commands of the
            ;; first, which they all have
            d       (if morph? (str (drawn (values 0))) (str (:content shape)))]
        (smil-animate timeline "d"
                      (points (fn [t]
                                (mapv d/parse-double (re-seq path-number (str (drawn (values t)))))))
                      #(render-path-numbers d %))))))

(defn- svg-sized-region
  "The region `[x y width height]` the masks of the outer strokes of
  `shape` take as `timeline` (cut to its duration) plays its width and
  height on its elements (see `aexp/svg-box-sized?`,
  `aexp/svg-outline-sized?`) or its outline (see `svg-smil-morph?`):
  what its box covers as it plays (`played`, see `svg-played-box`), and
  around it as far as its strokes reach (their miters, four times as
  wide as they are at their widest)."
  [shape timeline played]
  (let [values  (acss/shape-values timeline (:id shape))
        {:keys [times corners]} played
        strokes (cta/as-vec (:strokes shape))
        widest  (reduce max 0
                        (for [t times index (range (count strokes))]
                          (or (indexed-value (:stroke-width (values t)) index)
                              (:stroke-width (get strokes index))
                              0)))
        pad     (* 4 widest)
        box     (grc/points->rect (mapcat corners times))]
    [(- (:x box) pad) (- (:y box) pad) (+ (:width box) (* 2 pad)) (+ (:height box) (* 2 pad))]))

(defn- svg-box-pattern
  "`markup` with the pattern `pattern-id` laid over the box of the element
  it fills (in units of that box), so it stays over it as its width and
  height play: the pattern of the fills of a shape, of rectangles over
  its box at rest, each a fill (a gradient in units of its rectangle),
  as the SVG renderer draws more than one fill or a gradient."
  [markup pattern-id]
  (let [at    (str/index-of markup (str " id=\"" pattern-id "\""))
        start (some->> at (str/last-index-of markup "<pattern "))
        end   (some->> at (str/index-of markup "</pattern>"))
        box   #"patternUnits=\"userSpaceOnUse\" x=\"[^\"]*\" y=\"[^\"]*\" width=\"[^\"]*\" height=\"[^\"]*\""
        whole (when (and start end) (subs markup start end))]
    (if (and (some? whole) (re-find box whole))
      (str (subs markup 0 start)
           (-> whole
               (str/replace box (str "patternUnits=\"objectBoundingBox\" patternContentUnits=\"objectBoundingBox\""
                                     " x=\"0\" y=\"0\" width=\"1\" height=\"1\""))
               (str/replace #"<rect width=\"[^\"]*\" height=\"[^\"]*\"" "<rect width=\"1\" height=\"1\""))
           (subs markup end))
      markup)))

(defn- svg-sized-paint
  "The vectors `markup` of the layer `sid`, whose width and height or
  outline play on its elements (see `aexp/svg-box-sized?`,
  `aexp/svg-outline-sized?`, `svg-smil-morph?`): with `pattern?` (a box
  that sizes), the pattern of its fills over its box as it plays (see
  `svg-box-pattern`), a morph keeping its fills at rest as the canvas
  does; and the masks of its outer strokes, bounded to its box at rest by
  the SVG renderer, over `region` (see `svg-sized-region`), when it has
  any."
  [markup sid region pattern?]
  (let [pattern-id (when pattern?
                     (second (re-find (re-pattern (str "id=\"fills-" sid "\"><\\w+ [^>]*?fill=\"url\\(#([^)\"]+)\\)\""))
                                      markup)))
        [x y w h]  region]
    (cond-> markup
      (some? pattern-id)
      (svg-box-pattern pattern-id)

      (some? region)
      (str/replace (re-pattern (str "<mask id=\"(outer-stroke-[^\"]*-" sid "(?:-\\d+)?)\" x=\"[^\"]*\" y=\"[^\"]*\""
                                    " width=\"[^\"]*\" height=\"[^\"]*\""))
                   (fn [[_ id]]
                     (str "<mask id=\"" id "\" x=\"" (acss/fmt x) "\" y=\"" (acss/fmt y)
                          "\" width=\"" (acss/fmt w) "\" height=\"" (acss/fmt h) "\""))))))

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
  shows (see `svg-smil-css`), and what it plays in the gradients of the
  fills and strokes of each (`:gradients`, see `svg-gradient-smil`). Those of `wide`
  hold layers, which may go over `area` (on the page)."
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
                         (svg-filter-shadows? shape)
                         (some #(and (not (:hidden %)) (contains? drops (:style % :drop-shadow)))
                               (cta/as-vec (:shadow shape))))
           blur?    (and (some? shape) (svg-filter-blur? shape track))
           outline  (when (and (some? shape)
                               (or (svg-smil-corners? shape track)
                                   (aexp/svg-outline-sized? shape track)
                                   (svg-smil-morph? shape track)))
                      (svg-outline-path shape timeline))
           ;; its box as its size or its outline plays (see
           ;; `svg-played-box`), once for its paint and its filters
           played   (when (some? shape) (svg-played-box shape timeline))
           ;; its fills and the masks of its outer strokes over its box
           ;; as its size or its outline plays on its elements (see
           ;; `svg-sized-paint`)
           sized?   (and (some? shape)
                         (or (aexp/svg-box-sized? shape track) (aexp/svg-outline-sized? shape track)))
           paint    (when (and (some? shape) (or sized? (svg-smil-morph? shape track)))
                      {:region (when (some #(= :outer (aexp/svg-stroke-alignment shape %))
                                           (cta/as-vec (:strokes shape)))
                                 (svg-sized-region shape timeline played))
                       :pattern? sized?})
           filters  (when (or shadows? blur?)
                      (let [flags {:shadows? shadows? :blur? blur? :styles drops
                                   :area (when (contains? wide sid) area)
                                   :played played}]
                        (str (svg-effects-filter (svg-effects-id sid false) shape timeline flags)
                             (svg-effects-filter (svg-effects-id sid true) shape nil flags))))
           inner    (when board?
                      (let [flags {:shadows? true :styles #{:inner-shadow} :played played}]
                        (str (svg-effects-filter (svg-inner-id sid false) shape timeline flags)
                             (svg-effects-filter (svg-inner-id sid true) shape nil flags))))
           gradients (when (some? shape)
                       (not-empty
                        (into {}
                              (keep (fn [[slot kfs]]
                                      (some->> (svg-gradient-smil timeline shape slot kfs)
                                               (vector slot))))
                              (gradient-tracks shape track))))]
       (cond-> result
         (or (some? outline) (some? filters) (some? inner) (some? paint) (some? gradients))
         (assoc sid (cond-> {}
                      (some? outline) (assoc :outline outline)
                      (some? gradients) (assoc :gradients gradients)
                      (some? filters) (assoc :filters filters)
                      (some? inner)   (assoc :inner inner)
                      (some? paint)   (assoc :paint paint))))))
   {}
   (into (set (keys (:tracks timeline))) (filter uuid?) (keys images))))

(defn- svg-smil-css
  "The CSS that shows what SMIL plays (see `svg-smil`) at rest with
  reduced motion asked for, which SMIL does not heed: the effects of
  `smil`, the copies at rest of the vectors whose corners or gradients it
  plays (see `svg-smil-vectors`) and the clips of the boards `clipped` (see
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
                      (mapcat (fn [[id {:keys [outline gradients]}]]
                                (when (or (some? outline) (some? gradients))
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
  path of their outline and the gradients of their paints SMIL plays (see
  `svg-smil`), and beside them, for reduced motion, which SMIL does not
  heed, a copy at rest (see `svg-rest-copy`, `svg-smil-css`)."
  [{:keys [smil ox oy]} id markup]
  (let [{:keys [outline inner paint gradients]} (get smil id)
        markup (cond-> markup
                 (some? paint) (svg-sized-paint id (:region paint) (:pattern? paint)))
        ;; the inner shadows of a board on a filter around its vectors
        inside (fn [markup defs?]
                 (if (some? inner)
                   (str (when defs? (str "<defs>" inner "</defs>"))
                        "<g class=\"" (svg-escape (svg-smil-inner-class id))
                        "\" filter=\"url(#" (svg-escape (svg-inner-id id false)) ")\">" markup "</g>")
                   markup))]
    (if (or (some? outline) (some? gradients))
      (str "<g class=\"" (svg-escape (svg-smil-anim-class id)) "\">"
           (svg-vectors (inside (cond-> (svg-smil-outline markup outline)
                                  (some? gradients) (svg-smil-gradients gradients))
                                true)
                        ox oy) "</g>"
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

(defn- svg-mask-id
  [id]
  (str "penpot-mask-" id))

(declare emit-svg-node)

(def ^:private svg-mask-filters
  "The filters that turn the mask of an inverted mask mode (see
  `cts/mask-modes`) into what keeps over a white region: black where it
  is, or dark where it was bright."
  {:alpha-inverted (str "<feFlood flood-color=\"black\" result=\"flood\"/>"
                        "<feComposite in=\"flood\" in2=\"SourceGraphic\" operator=\"in\"/>")
   :luma-inverted  (str "<feColorMatrix type=\"matrix\""
                        " values=\"-1 0 0 0 1  0 -1 0 0 1  0 0 -1 0 1  0 0 0 1 0\"/>")})

(defn- svg-masked
  "What the masked group `shape` holds, the layers `inner` (see
  `layer-nodes`) kept to its mask, the bottom-most of them, drawn
  without its shadows and blur, as the canvas draws it: a `<mask>` by
  its alpha or by its brightness, as its mask mode says (see
  `cts/mask-modes`); an inverted one over a white region, which the
  mask, filtered (see `svg-mask-filters`), takes from. One region
  around the board (`width` by `height` of `ctx`) as large as the
  layers may move. The layers it holds are layers (see
  `aexp/export-holders`)."
  [{:keys [width height] :as ctx} shape inner]
  (let [mask-id (first (:shapes shape))
        node    (d/seek #(= mask-id (:id %)) inner)
        content (str/join "" (keep #(emit-svg-node ctx %) (remove #(= mask-id (:id %)) inner)))]
    (if (nil? node)
      content
      (let [plain  (-> ctx
                       (update-in [:objects mask-id] dissoc :shadow :blur)
                       ;; nor the filters of its effects (see `svg-smil`)
                       (update-in [:smil mask-id] dissoc :filters :inner))
            mode   (cts/mask-mode shape)
            filter (get svg-mask-filters mode)
            type   (if (= :alpha mode) "alpha" "luminance")
            mid    (svg-escape (svg-mask-id (:id shape)))
            fid    (str mid "-filter")
            [x y w h] (map acss/fmt [(- width) (- height) (* 3 width) (* 3 height)])
            drawn  (emit-svg-node plain node)]
        (str "<defs>"
             (when (some? filter)
               (str "<filter id=\"" fid "\" color-interpolation-filters=\"sRGB\">" filter "</filter>"))
             "<mask id=\"" mid "\" mask-type=\"" type "\" style=\"mask-type: " type "\""
             " maskUnits=\"userSpaceOnUse\" x=\"" x "\" y=\"" y "\" width=\"" w "\" height=\"" h "\">"
             (if (some? filter)
               (str "<rect x=\"" x "\" y=\"" y "\" width=\"" w "\" height=\"" h "\" fill=\"white\"/>"
                    "<g filter=\"url(#" fid ")\">" drawn "</g>")
               drawn)
             "</mask></defs>"
             "<g mask=\"url(#" mid ")\">" content "</g>")))))

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
  (`unit-blurs`, see `svg-unit-blurs`). A masked group keeps the layers
  it holds to its mask (see `svg-masked`)."
  [{:keys [objects images svg-ids ox oy follow unit-blurs] :as ctx} {:keys [id clip?] :as node}]
  (when-let [shape (get objects id)]
    (let [masked? (and (not clip?) (true? (:masked-group shape)))
          inner   (if masked?
                    (svg-masked ctx shape (:inner node))
                    (str/join "" (keep #(emit-svg-node ctx %) (:inner node))))
          clipped #(if (and (seq (:inner node)) (aexp/clipping-board? shape))
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
                     (str (cond
                            ;; what it holds draws it
                            masked? nil

                            (some? (:markup img))
                            ;; its vectors
                            (svg-smil-vectors ctx id (:markup img))

                            ;; the picture of the shape as it is, turned, over its
                            ;; box with shadows and blur (`:x :y` of the image), or
                            ;; its box
                            :else
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
  `aexp/export-layer-ids`. The width and height of a layer play on its
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
                      ;; the size of a group on its layers (see `aexp/expand-group-sizes`)
                      (aexp/expand-group-sizes objects)
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
         ;; the layers SMIL resizes on their outlines, and those CSS
         ;; resizes on their elements where only an SVG can (vectors
         ;; only): no scale of their groups either
         outlined (into #{}
                        (keep (fn [[id track]]
                                (when-let [shape (when (some? (dm/get-in images [id :markup])) (get objects id))]
                                  (when (or (aexp/svg-outline-sized? shape track) (aexp/svg-box-sized? shape track))
                                    id))))
                        (:tracks timeline))
         ids      (aexp/export-layer-ids objects timeline)
         css      (acss/timeline->css timeline objects
                                      {:svg? true
                                       ;; what it draws: no rules of hidden layers
                                       :ids (conj (set ids) board-id)
                                       :sized (-> sized
                                                  (into outlined)
                                                  ;; groups resize on their layers
                                                  (into (aexp/sized-groups timeline)))
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
                      ;; the picture over the box it covers, as a layer's: a
                      ;; board's shadow or what reaches past it widen it
                      (str "<image" (svg-id-attr svg-ids board-id)
                           " href=\"" (svg-escape (:href img))
                           "\" x=\"" (acss/fmt (or (:x img) 0)) "\" y=\"" (acss/fmt (or (:y img) 0))
                           "\" width=\"" (acss/fmt (or (:width img) width))
                           "\" height=\"" (acss/fmt (or (:height img) height)) "\"/>")))
         layers   (str/join "" (keep #(emit-svg-node {:objects objects :images images :svg-ids svg-ids
                                                      :ox ox :oy oy :follow (:ids follow) :smil smil
                                                      :unit-blurs unit-blurs :width width :height height}
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
