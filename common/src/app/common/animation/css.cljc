;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.animation.css
  "The CSS export of a timeline (see `timeline->css`): the keyframes of
  the layers of its board as CSS animations, which an SVG of vectors
  plays too (see `app.common.animation.svg`)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.modifiers :as cgm]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.animation-follow :as ctaf]
   [app.common.types.animation-layout :as ctal]
   [app.common.types.animation-paint :as ctap]
   [app.common.types.color :as clr]
   [clojure.string :as str]))

(defn fmt
  "Format a number for CSS: rounded to 3 decimals, integers without a
  trailing `.0`."
  [v]
  (let [r (mth/precision v 3)]
    (if (== r (mth/round r))
      (str (long r))
      (str r))))

(defn format-number
  "A number as the code of an animation writes it (see `timeline->css`):
  rounded to 3 decimals, a whole one without a trailing `.0`, the same on
  the JVM and in JS."
  [v]
  (fmt v))

(defn origin-css
  "CSS `transform-origin` value for a normalized `{ :x :y }` origin."
  [origin]
  (str (fmt (* 100.0 (:x origin))) "% "
       (fmt (* 100.0 (:y origin))) "%"))

(defn easing->css
  "Render a keyframe easing as a CSS timing-function string. A spring
  becomes a `linear()` function through points of its curve."
  [easing]
  (cond
    (= :spring (:type easing))
    (let [stops (cta/spring-samples easing 40)]
      (str "linear("
           (->> (range (inc stops))
                (map (fn [i]
                       (let [t (/ i stops)]
                         (str (fmt (cta/spring-progress easing t)) " " (fmt (* 100.0 t)) "%"))))
                (str/join ", "))
           ")"))

    (map? easing)
    (let [[a b c d] (:curve easing)]
      (str "cubic-bezier(" (fmt a) ", " (fmt b) ", " (fmt c) ", " (fmt d) ")"))

    (keyword? easing)
    (case easing
      :linear "linear"
      :ease "ease"
      :ease-in "ease-in"
      :ease-out "ease-out"
      :ease-in-out "ease-in-out"
      "linear")

    :else "linear"))

(defn shape-css-class
  "CSS class used by `timeline->css` for `id`. The whole id: the ids of
  the shapes of a file begin alike."
  [id]
  (str "penpot-shape-" id))

(defn- css-color
  [hex opacity]
  (if (or (nil? hex) (nil? opacity) (= opacity 1))
    (or hex "transparent")
    (let [[r g b] (clr/hex->rgb hex)]
      (str "rgba(" (int r) ", " (int g) ", " (int b) ", " (fmt opacity) ")"))))

(defn- css-gradient
  "The gradient `gradient` of a box `width` by `height` as CSS draws it,
  as the canvas does, in pixels: a linear one `{:type :angle :stops}`, its
  angle (degrees) and each stop `[colour share]`, the share of the line
  of the gradient CSS draws through the middle of the box; a radial one
  `{:type :rx :ry :cx :cy :stops}`, an ellipse (px) along the axes of the
  box nearest its own (CSS turns none), each stop a share of its radius."
  [{:keys [type start-x start-y end-x end-y width] :as gradient} box-width box-height]
  (let [w     (double (or box-width 0))
        h     (double (or box-height 0))
        sx    (* start-x w)
        sy    (* start-y h)
        dx    (- (* end-x w) sx)
        dy    (- (* end-y h) sy)
        stops (ctap/gradient-stops gradient)
        color #(css-color (:color %) (:opacity %))]
    (if (= :radial type)
      (let [r     (mth/hypot dx dy)
            other (* r (or width 1))
            [rx ry] (if (>= (mth/abs dx) (mth/abs dy)) [r other] [other r])]
        {:type :radial :rx rx :ry ry :cx sx :cy sy
         :stops (mapv #(vector (color %) (* 100 (:offset %))) stops)})
      (let [angle (if (and (mth/almost-zero? dx) (mth/almost-zero? dy))
                    180
                    (mth/degrees (mth/atan2 dx (- dy))))
            rad   (mth/radians angle)
            ux    (mth/sin rad)
            uy    (- (mth/cos rad))
            line  (max 1e-6 (+ (mth/abs (* w ux)) (mth/abs (* h uy))))
            share (fn [offset]
                    (let [qx (- (+ sx (* offset dx)) (/ w 2))
                          qy (- (+ sy (* offset dy)) (/ h 2))]
                      (* 100 (+ 0.5 (/ (+ (* qx ux) (* qy uy)) line)))))]
        {:type :linear :angle angle
         :stops (mapv #(vector (color %) (share (:offset %))) stops)}))))

(defn css-gradient-image
  "The CSS `background-image` of a gradient (see `css-gradient`)."
  [{:keys [type angle rx ry cx cy stops]}]
  (let [stops (str/join ", " (map (fn [[color share]] (str color " " (fmt share) "%")) stops))]
    (if (= :radial type)
      (str "radial-gradient(" (fmt rx) "px " (fmt ry) "px at " (fmt cx) "px " (fmt cy) "px, " stops ")")
      (str "linear-gradient(" (fmt angle) "deg, " stops ")"))))

(defn- css-gradient-vars
  "The declarations of the custom properties that draw a gradient (see
  `css-gradient`), which CSS plays as it can not a `background-image`
  (see `gradient-css`)."
  [{:keys [type angle rx ry cx cy stops]}]
  (into (if (= :radial type)
          [(str "--penpot-gradient-rx: " (fmt rx) "px;") (str "--penpot-gradient-ry: " (fmt ry) "px;")
           (str "--penpot-gradient-cx: " (fmt cx) "px;") (str "--penpot-gradient-cy: " (fmt cy) "px;")]
          [(str "--penpot-gradient-angle: " (fmt angle) "deg;")])
        (mapcat (fn [k [color share]]
                  [(str "--penpot-gradient-color-" k ": " color ";")
                   (str "--penpot-gradient-at-" k ": " (fmt share) "%;")])
                (range)
                stops)))

(defn- css-box-shadow
  [shadow]
  (str (when (= :inner-shadow (:style shadow)) "inset ")
       (fmt (:offset-x shadow)) "px "
       (fmt (:offset-y shadow)) "px "
       (fmt (:blur shadow)) "px "
       (fmt (:spread shadow)) "px "
       (css-color (get-in shadow [:color :color])
                  (get-in shadow [:color :opacity]))))

(def ^:private css-transform-properties
  "The properties the CSS of an animation plays through `transform`."
  #{:x :y :width :height :scale-x :scale-y :rotation :rotate-x :rotate-y :translate-z})

(declare frame-values* frame-values**)

(defn frame-values
  "What `shape` shows at one frame of its animation, `values` (its
  values then, see `values-at`), as CSS plays it: `:translate-x`,
  `:translate-y` (px), `:rotate` (degrees) and `:scale-x`, `:scale-y`, all
  from its rest pose, when its position, size, scale or rotation plays;
  then, for the properties `values` holds, `:opacity`, `:border-radius`
  (the four corners in px), `:background-color` and `:border-color` (CSS
  colours of the first solid fill and stroke), `:gradient` (the first
  gradient fill, see `css-gradient`), `:border-width` (px; of a stroke
  with sides, the width of each side, top first) (of a
  text, `:color`, `:text-stroke-color` and `:text-stroke-width`: it paints
  its glyphs in the fills of its spans, see `cta/paint-fills`),
  `:box-shadow` (the CSS of its visible shadows), `:blur` and
  `:backdrop-blur` (px), and of a text `:font-size`, `:letter-spacing`
  (px), `:line-height` and `:font-weight`; of an auto layout `:gap` (row,
  column) and `:padding`, of a layer in one `:margin` (both top first) and
  `:min-width`, `:max-width`, `:min-height`, `:max-height` (px). `origin` is where its positions count from (see
  `position-origin`). A tilted shape (see `app.common.geom.tilt`) has its
  `:rotate-x` and `:rotate-y` (degrees) with its transform, and, turning
  around `pivot` (fractions of its box) off its middle with a tilt at
  rest, `:lift` (`[dx dy dz]` px, see `gtl/lift`); one that comes out of
  the plane its `:translate-z` (px, towards the viewer). A board whose camera
  plays (see `cta/camera-properties`) has `:perspective` (px) and
  `:perspective-origin` (CSS, shares of its box). A layer the light or the
  depth cues of its board show (`:scene` of `values`, see `scene-layers`)
  has its `:opacity` faded, its `:blur` with theirs and its `:brightness`."
  ([shape values origin]
   (frame-values shape values origin nil))
  ([shape values origin pivot]
   (let [frame (frame-values* shape values origin)]
     (if (and (contains? frame :translate-x)
              (or (gtl/three-d? shape) (cta/has? values :rotate-x :rotate-y :translate-z)))
       (let [rest-x (or (:rotate-x shape) 0)
             rest-y (or (:rotate-y shape) 0)
             rest-z (or (:translate-z shape) 0)
             {px :x py :y} (or pivot cta/default-transform-origin)
             lift   (gtl/lift shape px py rest-x rest-y)
             z      (get values :translate-z rest-z)]
         (cond-> (assoc frame
                        :rotate-x (get values :rotate-x rest-x)
                        :rotate-y (get values :rotate-y rest-y))
           (some #(not (mth/almost-zero? %)) lift)
           (assoc :lift lift)

           (or (contains? values :translate-z) (not (mth/almost-zero? z)))
           (assoc :translate-z z)))
       frame))))

(defn- with-scene
  "`frame` of `shape` with the light and the depth cues of its board (the
  `:scene` of `values`, `{:shade :fade :blur}`, see `scene-layers`): its
  opacity faded, its blur and theirs (their variances add) and its
  `:brightness`, written all along, as they change."
  [frame shape values]
  (if-let [{:keys [shade fade blur]} (:scene values)]
    (let [own  (get values :blur (if (get-in shape [:blur :hidden]) 0 (get-in shape [:blur :value] 0)))
          cued (or blur 0)]
      (assoc frame
             :opacity (* (get values :opacity (or (:opacity shape) 1)) (or fade 1))
             :blur (mth/sqrt (+ (* own own) (* cued cued)))
             :brightness (or shade 1)))
    frame))

(defn- frame-values*
  [shape values origin]
  (with-scene (frame-values** shape values origin) shape values))

(defn- frame-values**
  [shape values origin]
  (let [selrect (:selrect shape)
        base-x  (:x selrect)
        base-y  (:y selrect)
        base-r  (or (:rotation shape) 0)
        text?   (= :text (:type shape))
        fills   (cta/apply-fills (cta/paint-fills shape) values)
        strokes (cta/apply-strokes (:strokes shape) values)
        shadows (cta/apply-shadows (:shadow shape) values)
        fill    (d/seek cta/solid-color-fill? fills)
        stroke  (d/seek cta/solid-color-stroke? strokes)
        ;; a text paints its glyphs, which CSS paints in a colour
        grad    (when (and (not text?) (contains? values :fill-gradient))
                  (d/seek cta/gradient-fill? fills))]
    (cond-> {}
      (some #(contains? values %) css-transform-properties)
      (assoc :translate-x (if (contains? values :x) (- (+ (:x origin) (:x values)) base-x) 0)
             :translate-y (if (contains? values :y) (- (+ (:y origin) (:y values)) base-y) 0)
             :rotate      (- (get values :rotation base-r) base-r)
             :scale-x     (cta/axis-scale (or (:width selrect) 0) values :width :scale-x)
             :scale-y     (cta/axis-scale (or (:height selrect) 0) values :height :scale-y))

      (contains? values :opacity)
      (assoc :opacity (:opacity values))

      (cta/has? values :r1 :r2 :r3 :r4)
      (assoc :border-radius [(get values :r1 (:r1 shape)) (get values :r2 (:r2 shape))
                             (get values :r3 (:r3 shape)) (get values :r4 (:r4 shape))])

      (and fill (cta/has? values :fill-color :fill-opacity))
      (assoc (if text? :color :background-color) (css-color (:fill-color fill) (or (:fill-opacity fill) 1)))

      (some? grad)
      (assoc :gradient (css-gradient (:fill-color-gradient grad) (:width selrect) (:height selrect)))

      (and stroke (cta/has? values :stroke-color :stroke-opacity))
      (assoc (if text? :text-stroke-color :border-color) (css-color (:stroke-color stroke) (or (:stroke-opacity stroke) 1)))

      (and stroke (contains? values :stroke-width) (not (ctap/sided-stroke? stroke)))
      (assoc (if text? :text-stroke-width :border-width) (or (:stroke-width stroke) 0))

      ;; a stroke with sides: each as wide as it plays, top first
      (and stroke (not text?) (ctap/sided-stroke? stroke)
           (apply cta/has? values :stroke-width ctap/stroke-side-properties))
      (assoc :border-width (mapv #(ctap/stroke-side-width stroke %) ctap/stroke-side-properties))

      (cta/has? values :shadow-offset-x :shadow-offset-y :shadow-blur
                :shadow-spread :shadow-color :shadow-opacity)
      (assoc :box-shadow (->> shadows
                              (remove :hidden)
                              (map css-box-shadow)
                              (str/join ", ")))

      (contains? values :blur)
      (assoc :blur (:blur values))

      (contains? values :background-blur)
      (assoc :backdrop-blur (:background-blur values))

      ;; the type style of a text, on the element of the text
      (and text? (contains? values :font-size))
      (assoc :font-size (:font-size values))

      (and text? (contains? values :letter-spacing))
      (assoc :letter-spacing (:letter-spacing values))

      (and text? (contains? values :line-height))
      (assoc :line-height (:line-height values))

      (and text? (contains? values :font-weight))
      (assoc :font-weight (mth/round (:font-weight values)))

      ;; the spacing of an auto layout, and of a layer in one
      (apply cta/has? values [:layout-row-gap :layout-column-gap])
      (assoc :gap (mapv #(get values % (ctal/layout-rest shape %)) [:layout-row-gap :layout-column-gap]))

      (apply cta/has? values [:layout-padding-top :layout-padding-right :layout-padding-bottom :layout-padding-left])
      (assoc :padding (mapv #(get values % (ctal/layout-rest shape %))
                            [:layout-padding-top :layout-padding-right :layout-padding-bottom :layout-padding-left]))

      (apply cta/has? values [:layout-margin-top :layout-margin-right :layout-margin-bottom :layout-margin-left])
      (assoc :margin (mapv #(get values % (ctal/layout-rest shape %))
                           [:layout-margin-top :layout-margin-right :layout-margin-bottom :layout-margin-left]))

      (contains? values :layout-min-width)  (assoc :min-width (:layout-min-width values))
      (contains? values :layout-max-width)  (assoc :max-width (:layout-max-width values))
      (contains? values :layout-min-height) (assoc :min-height (:layout-min-height values))
      (contains? values :layout-max-height) (assoc :max-height (:layout-max-height values))

      ;; the camera of a board: CSS sees the tilted layers in it with its
      ;; perspective (see `app.common.geom.tilt`); an orthographic one has
      ;; none to play
      (and (cta/has? values :perspective :perspective-x :perspective-y) (= :frame (:type shape))
           (not (true? (:orthographic shape))))
      (assoc :perspective (get values :perspective (cta/camera-rest shape :perspective))
             :perspective-origin (str (fmt (* 100 (get values :perspective-x (cta/camera-rest shape :perspective-x)))) "% "
                                      (fmt (* 100 (get values :perspective-y (cta/camera-rest shape :perspective-y)))) "%")))))

(declare svg-group-declarations css-declarations)

(defn- values->declarations
  "Build the CSS declarations (transform, opacity, fill, stroke, shadow,
  blur) for `shape` given the interpolated `values` at one keyframe stop
  (see `frame-values`). A `transform` only when the position, size, scale
  or rotation of the shape plays, as it replaces the one the shape has at
  rest: with `rest-transform`, that one of the code it plays in, kept
  between its turn and its scale. For an SVG (`svg?`) the group of a
  layer plays its transform, opacity and blur, as a CSS `filter`; its
  fills, strokes, corners and trim play on its elements (see
  `svg-appearance-css`), its shadows and the corners CSS can not play by
  SMIL (see `svg-smil`)."
  ([shape values origin rest-transform]
   (values->declarations shape values origin rest-transform false nil))
  ([shape values origin rest-transform svg? pivot]
   (if svg?
     (svg-group-declarations shape values origin)
     (css-declarations shape values origin rest-transform pivot))))

(defn- svg-group-declarations
  "The declarations of the group of a layer of an SVG: a turned shape
  scales along its own sides, as the canvas scales it. Its blur plays on
  a filter (see `svg-effects-filter`)."
  [shape values origin]
  (let [{:keys [translate-x translate-y rotate scale-x scale-y opacity] :as frame}
        (frame-values shape values origin)
        turn (or (:rotation shape) 0)]
    (cond-> []
      (contains? frame :translate-x)
      (conj (str "transform: translate(" (fmt translate-x) "px, " (fmt translate-y) "px) "
                 "rotate(" (fmt rotate) "deg) "
                 (if (mth/almost-zero? (mod turn 360))
                   (str "scale(" (fmt scale-x) ", " (fmt scale-y) ");")
                   (str "rotate(" (fmt turn) "deg) scale(" (fmt scale-x) ", " (fmt scale-y) ") "
                        "rotate(" (fmt (- turn)) "deg);"))))

      (contains? frame :opacity)
      (conj (str "opacity: " (fmt opacity) ";")))))

(defn filter-css
  "The CSS `filter` of a frame (see `frame-values`): its `:blur` (px) and
  its `:brightness`."
  [{:keys [blur brightness]}]
  (str/join " " (cond-> []
                  (some? blur)       (conj (str "blur(" (fmt blur) "px)"))
                  (some? brightness) (conj (str "brightness(" (fmt brightness) ")")))))

(defn- css-declarations
  "The CSS declarations of `shape` at a frame: the tilt in its own axes,
  after the turn it rests at (`rest-transform`, flat) and before its
  scale, lifted to its pivot (see `frame-values`); the unit of a text
  that turns in 3D seen from its `:unit-perspective` (px) in front of its
  middle, as render-wasm draws it (see `tilt::unit_matrix`)."
  [shape values origin rest-transform pivot]
  (let [{:keys [translate-x translate-y rotate scale-x scale-y opacity border-radius
                background-color border-color border-width box-shadow backdrop-blur
                color text-stroke-color text-stroke-width lift perspective perspective-origin]
         :as frame}
        (frame-values shape values origin pivot)]
    (cond-> []
      (contains? frame :translate-x)
      (conj (str "transform: "
                 (when-let [distance (and (contains? frame :rotate-x) (:unit-perspective shape))]
                   (str "perspective(" (fmt distance) "px) "))
                 "translate(" (fmt translate-x) "px, " (fmt translate-y) "px) "
                 "rotate(" (fmt rotate) "deg) "
                 (when-not (str/blank? rest-transform)
                   (str rest-transform " "))
                 (when (contains? frame :translate-z)
                   (str "translateZ(" (fmt (:translate-z frame)) "px) "))
                 (when-let [[dx dy dz] lift]
                   (str "translate3d(" (fmt dx) "px, " (fmt dy) "px, " (fmt dz) "px) "))
                 (when (contains? frame :rotate-x)
                   (str "rotateX(" (fmt (:rotate-x frame)) "deg) rotateY(" (fmt (:rotate-y frame)) "deg) "))
                 "scale(" (fmt scale-x) ", " (fmt scale-y) ");"))

      (contains? frame :opacity)
      (conj (str "opacity: " (fmt opacity) ";"))

      (contains? frame :border-radius)
      (conj (str "border-radius: "
                 (str/join " " (map #(str (fmt %) "px") border-radius)) ";"))

      (contains? frame :background-color)
      (conj (str "background-color: " background-color ";"))

      (contains? frame :gradient)
      (into (css-gradient-vars (:gradient frame)))

      (contains? frame :border-color)
      (conj (str "border-color: " border-color ";"))

      (contains? frame :border-width)
      (conj (str "border-width: "
                 (if (sequential? border-width)
                   (str/join " " (map #(str (fmt %) "px") border-width))
                   (str (fmt border-width) "px"))
                 ";"))

      (contains? frame :color)
      (conj (str "color: " color ";"))

      (contains? frame :text-stroke-color)
      (conj (str "-webkit-text-stroke-color: " text-stroke-color ";"))

      (contains? frame :text-stroke-width)
      (conj (str "-webkit-text-stroke-width: " (fmt text-stroke-width) "px;"))

      (contains? frame :box-shadow)
      (conj (str "box-shadow: " box-shadow ";"))

      (or (contains? frame :blur) (contains? frame :brightness))
      (conj (str "filter: " (filter-css frame) ";"))

      (contains? frame :backdrop-blur)
      (conj (str "backdrop-filter: blur(" (fmt backdrop-blur) "px);"))

      (contains? frame :font-size)
      (conj (str "font-size: " (fmt (:font-size frame)) "px;"))

      (contains? frame :letter-spacing)
      (conj (str "letter-spacing: " (fmt (:letter-spacing frame)) "px;"))

      (contains? frame :line-height)
      (conj (str "line-height: " (fmt (:line-height frame)) ";"))

      (contains? frame :font-weight)
      (conj (str "font-weight: " (:font-weight frame) ";"))

      (contains? frame :gap)
      (conj (str "gap: " (str/join " " (map #(str (fmt %) "px") (:gap frame))) ";"))

      (contains? frame :padding)
      (conj (str "padding: " (str/join " " (map #(str (fmt %) "px") (:padding frame))) ";"))

      (contains? frame :margin)
      (conj (str "margin: " (str/join " " (map #(str (fmt %) "px") (:margin frame))) ";"))

      (contains? frame :min-width)  (conj (str "min-width: " (fmt (:min-width frame)) "px;"))
      (contains? frame :max-width)  (conj (str "max-width: " (fmt (:max-width frame)) "px;"))
      (contains? frame :min-height) (conj (str "min-height: " (fmt (:min-height frame)) "px;"))
      (contains? frame :max-height) (conj (str "max-height: " (fmt (:max-height frame)) "px;"))

      (contains? frame :perspective)
      (conj (str "perspective: " (fmt perspective) "px;")
            (str "perspective-origin: " perspective-origin ";")))))

(defn unit-perspective
  "How far in front of its middle a unit of the text `text` of `objects`
  that turns in 3D is seen from, px: the perspective of its board, nil
  for an orthographic one (see `gtl/perspective-of`)."
  [objects text]
  (let [{:keys [distance orthographic?]} (gtl/perspective-of objects text)]
    (when-not orthographic? distance)))

(def ^:private scene-properties
  "The properties whose play moves a layer in 3D: the light and the depth
  cues of its board show it anew (see `scene-layers`)."
  #{:x :y :width :height :rotation :scale-x :scale-y :rotate-x :rotate-y :translate-z})

(defn- posed
  "`objects` posed as `timeline` plays them at `t` (see
  `ctaf/timeline->modif-tree`), what a layer holds moving with it (see
  `cgm/set-objects-modifiers`), as the canvas poses them."
  [timeline objects t]
  (let [tree (ctaf/timeline->modif-tree timeline objects t)]
    (if (empty? tree)
      objects
      (gsh/apply-objects-modifiers objects (cgm/set-objects-modifiers tree objects)))))

(def ^:private cue-properties
  "The properties of the light and the depth cues of a board."
  (into (set (keys cta/light-properties)) cta/depth-cue-properties))

(defn scene-layers
  "`{id scene}` of the layers of `objects` the light or the depth cues of
  their board show (see `gtl/scene-effects`) as `timeline` (as designed)
  plays: `scene` their effects at rest (`{:shade :fade :blur}`, those that
  change them), or, of a layer that moves in 3D as it plays (it or a 3D
  space group it is in, see `gtl/space-ids`), a function of a time to
  them (`{}` for none then), as for one whose board's light plays. Of the
  layers in the planes of their boards, none."
  [timeline objects]
  (let [played (cta/resolve-animations timeline objects)
        ;; the boards with a light or depth cues at rest, or depth cues
        ;; that play from none (keyframes of styles too)
        cued   (fn [board]
                 (and (cta/depth-cued? board)
                      (some #(contains? (set cta/depth-cue-properties) (:property %))
                            (dm/get-in played [:tracks (:id board) :keyframes]))))
        lit    (into #{}
                     (comp (filter #(and (= :frame (:type %)) (or (some? (gtl/scene %)) (cued %))))
                           (map :id))
                     (vals objects))]
    (if (empty? lit)
      {}
      (let [;; the page as it plays at a time, which every layer that
            ;; shows anew reads
            poses  (volatile! {})
            pose   (fn [t]
                     (or (get @poses t)
                         (let [objs (posed timeline objects t)]
                           (vswap! poses assoc t objs)
                           objs)))]
        (into {}
              (keep
               (fn [shape]
                 (when (and (contains? lit (:frame-id shape))
                            (not (:hidden shape))
                            (not (gtl/three-d-space? shape)))
                   (let [id     (:id shape)
                         ids    (gtl/space-ids objects shape)
                         tracks (keep #(get-in played [:tracks %]) ids)
                         ;; the light or the depth cues of its board play
                         board  (:frame-id shape)
                         lights (some #(contains? cue-properties (:property %))
                                      (dm/get-in played [:tracks board :keyframes]))]
                     (if (or lights
                             (some (fn [track]
                                     (or (some? (:follow track))
                                         (some #(contains? scene-properties (:property %)) (:keyframes track))))
                                   tracks))
                       (let [cache (volatile! {})]
                         [id (fn [t]
                               (or (get @cache t)
                                   (let [objs    (pose t)
                                         effects (or (gtl/scene-effects objs (get objs id)) {})]
                                     (vswap! cache assoc t effects)
                                     effects)))])
                       (when-let [effects (gtl/scene-effects objects shape)]
                         [id effects]))))))
              (vals objects))))))

(defn keyframe-timing
  "The CSS timing function of the segment starting at `keyframe`, or nil
  when it has no easing: a hold jumps at its end."
  [keyframe]
  (cond
    (= :step (:interpolation keyframe)) "steps(1, end)"
    (some? (:easing keyframe))          (easing->css (:easing keyframe))))

(defn shape-values
  "A function of a time to the values of the shape `sid` then as
  `timeline` plays it (see `values-at`), keeping those it gave."
  [timeline sid]
  (let [timeline (update timeline :tracks select-keys [sid])
        cache    (volatile! {})]
    (fn [t]
      (or (get @cache t)
          (let [v (get (cta/values-at timeline t) sid {})]
            (vswap! cache assoc t v)
            v)))))

(defn- gradient-numbers
  "The numbers of a gradient: its points, its width, and the offset, the
  channels and the opacity of each stop."
  [{:keys [start-x start-y end-x end-y width] :as gradient}]
  (into (mapv double [start-x start-y end-x end-y width])
        (mapcat (fn [{:keys [offset color opacity]}]
                  (into [(double offset)] (conj (mapv double (clr/hex->rgb color)) (double opacity)))))
        (ctap/gradient-stops gradient)))

(defn value-numbers
  "The numbers of `values` (see `values-at`), in an order of their own: a
  colour as its channels, a gradient as `gradient-numbers`, an indexed
  value by index."
  [values]
  (into []
        (mapcat (fn [[_ v]]
                  (cond
                    (number? v)                                [(double v)]
                    (and (string? v) (str/starts-with? v "#")) (mapv double (clr/hex->rgb v))
                    (and (map? v) (contains? v :stops))        (gradient-numbers v)
                    (map? v)                                   (value-numbers v)
                    :else                                      [])))
        (sort-by (comp str key) values)))

(def ^:private eased-samples-per-second
  "How often an export takes the values of a stretch it can not ease as
  the canvas does (see `eased-stops`)."
  30)

(defn follows?
  "Whether `value` (a time to a vector of numbers) goes from `t0` to `t1`
  as `progress` (of the share of the way) eases it: within 1% of its
  change a quarter, half and three quarters of the way."
  [value t0 t1 progress]
  (let [v0 (value t0)
        v1 (value t1)]
    (every? (fn [p]
              (let [e (progress p)]
                (every? true? (map (fn [a b actual]
                                     (<= (mth/abs (- actual (+ a (* e (- b a)))))
                                         (max 0.01 (* 0.01 (mth/abs (- b a))))))
                                   v0 v1 (value (+ t0 (* p (- t1 t0))))))))
            [0.25 0.5 0.75])))

(defn sample-times
  "The times from `t0` to `t1` (but it) an export takes values at to play
  the stretch between linearly, 30 a second (see
  `eased-samples-per-second`), 4 at least."
  [t0 t1]
  (let [n (max 4 (mth/ceil (* (- t1 t0) 0.001 eased-samples-per-second)))]
    (map #(+ t0 (* % (/ (- t1 t0) n))) (range n))))

(defn- keyframe-progress
  "The eased progress, of the share of the way, of the stretch from
  `keyframe` as its CSS timing plays it (see `keyframe-timing`); linear
  without one."
  [keyframe]
  (cond
    (nil? keyframe)                     identity
    (= :step (:interpolation keyframe)) #(if (< % 1) 0 1)
    :else                               #(cta/easing-progress (:easing keyframe) %)))

(defn eased-stops
  "The stops of an animation of `value` (a time to a vector of numbers,
  see `value-numbers`) at `times` (sorted) of the keyframes `kfs`, `[time
  keyframe]` each: the keyframe there that eases the stretch to the next
  stop (see `keyframe-timing`) when its values follow it, else stops to
  it played linearly (see `sample-times`), of no keyframe. A stretch cut
  by the keyframes of other properties is a part of the curves of those
  that go on through it, which no timing of its own eases."
  [value kfs times]
  (conj (into []
              (mapcat (fn [[t0 t1]]
                        (let [kf (d/seek #(and (= t0 (:time %)) (some? (keyframe-timing %))) kfs)]
                          (if (follows? value t0 t1 (keyframe-progress kf))
                            [[t0 kf]]
                            (map #(vector % nil) (sample-times t0 t1))))))
              (partition 2 1 times))
        [(peek times) nil]))

(defn- track->keyframes-css
  "The `@keyframes` of `track` on `shape`. With `sized?`, an SVG plays its
  width and height on its elements (see `svg-sized?`), not as a scale.
  With `eased?`, as the canvas eases each property (see `eased-stops`);
  else a stop of each keyframe, which code reads best, its timing that
  of the stretch to the next. `scene`, the light and the depth cues of
  its board (see `scene-layers`), plays with it."
  [timeline shape track kf-name origin rest-transform & {:keys [svg? sized? eased? scene]}]
  (let [duration (max 1 (:duration timeline))
        sid      (:shape-id track)
        kfs      (:keyframes track)
        ;; Keyframes past the end do not play: the value at the end is
        ;; the one on the way to them.
        times    (-> (into (sorted-set 0 duration)
                           (comp (map :time) (filter #(<= % duration)))
                           kfs)
                     vec)
        values   (let [values (shape-values timeline sid)
                       scene  (if (map? scene) (constantly scene) scene)]
                   #(cond-> (values %)
                      sized?       (dissoc :width :height)
                      ;; all three, so that the stops see each change
                      (some? scene) (assoc :scene (merge {:shade 1 :fade 1 :blur 0} (scene %)))))
        stops    (for [[t seg-timing] (if eased?
                                        (map (fn [[t kf]] [t (some-> kf keyframe-timing)])
                                             (eased-stops (comp value-numbers values) kfs times))
                                        (map (fn [t] [t (some #(when (= (:time %) t) (keyframe-timing %)) kfs)])
                                             times))]
                   (let [pct   (fmt (* 100.0 (/ (double t) duration)))
                         decls (cond-> (values->declarations shape (values t) origin rest-transform svg?
                                                             (cta/track-origin track))
                                 (and seg-timing (< t duration))
                                 (conj (str "animation-timing-function: " seg-timing ";")))]
                     (str "  " pct "% { " (str/join " " decls) " }")))]
    (str "@keyframes " kf-name " {\n" (str/join "\n" stops) "\n}")))

(defn- svg-group-plays?
  "Whether the group of a layer of an SVG plays any of `track` (see
  `svg-group-declarations`): its place, scale, turn or opacity, or its
  size unless it plays on its elements (`sized?`)."
  [track sized?]
  (boolean
   (some (fn [{:keys [property]}]
           (or (contains? #{:x :y :scale-x :scale-y :rotation :opacity} property)
               (and (not sized?) (contains? #{:width :height} property))))
         (:keyframes track))))

(defn- css-comment
  "A CSS comment of `text`, which can not end it early (a name with `*/`
  would go on as CSS)."
  [text]
  (str "/* " (str/replace (str text) "*/" "* /") " */"))

(defn- gradient-rule
  "What the rule of `shape` adds when `track` of `timeline` plays its
  first gradient fill (see `css-gradient`): `[declarations count type]`,
  its `background-image` of the custom properties its `@keyframes` play
  (see `css-gradient-vars`) and their values at rest, which reduced
  motion shows, with as many stops as it plays; nil else."
  [timeline shape track]
  (let [fills (cta/as-vec (:fills shape))
        index (d/index-of-pred fills cta/gradient-fill?)]
    (when (and (some? index)
               (not= :text (:type shape))
               (some #(and (= :fill-gradient (:property %)) (= index (:index %))) (:keyframes track)))
      (let [played (get-in ((shape-values timeline (:id shape)) 0) [:fill-gradient index])
            rest   (:fill-color-gradient (get fills index))
            rest   (cond-> rest
                     (some? played) (assoc :stops (ctap/pad-stops rest played)))
            {:keys [type stops] :as css} (css-gradient rest (dm/get-in shape [:selrect :width])
                                                       (dm/get-in shape [:selrect :height]))
            stops  (str/join ", " (map #(str "var(--penpot-gradient-color-" % ") var(--penpot-gradient-at-" % ")")
                                       (range (count stops))))]
        [(conj (css-gradient-vars css)
               (if (= :radial type)
                 (str "background-image: radial-gradient(var(--penpot-gradient-rx) var(--penpot-gradient-ry)"
                      " at var(--penpot-gradient-cx) var(--penpot-gradient-cy), " stops ");")
                 (str "background-image: linear-gradient(var(--penpot-gradient-angle), " stops ");")))
         (count (:stops css))
         type]))))

(defn- gradient-properties
  "The `@property` rules of the custom properties that draw the gradients
  of `gradients` (see `gradient-rule`), so CSS plays them: they hold a
  colour, a share, an angle or a length, not a string."
  [gradients]
  (let [most  (reduce max 0 (map second gradients))
        types (into #{} (map #(nth % 2)) gradients)
        prop  (fn [name syntax initial]
                (str "@property --penpot-gradient-" name " {\n  syntax: \"" syntax "\";\n"
                     "  inherits: false;\n  initial-value: " initial ";\n}"))]
    (str/join "\n\n"
              (concat
               (when (contains? types :linear) [(prop "angle" "<angle>" "180deg")])
               (when (contains? types :radial)
                 (map #(prop % "<length>" "0px") ["rx" "ry" "cx" "cy"]))
               (mapcat (fn [k]
                         [(prop (str "color-" k) "<color>" "transparent")
                          (prop (str "at-" k) "<percentage>" "0%")])
                       (range most))))))

(defn timeline->css
  "Generate a CSS string (`@keyframes` blocks + per-shape `animation`
  rules) for `timeline`, using the base geometry from `objects`. Pure.
  Played once, a shape keeps the last frame. With reduced motion asked
  for, the shapes stay as designed. A layer the light or the depth cues
  of its board show (see `scene-layers`) plays them with its animation,
  and one they show anew as it moves in 3D plays them on its own. Split animations play as animations
  of the whole text (see `flatten-splits`) unless `:units` says
  otherwise. Options: `:ids`, the shapes to write (all by default),
  `:selector`, the CSS selector of a shape (`shape-css-class` by
  default), `:origin`, the `transform-origin` of a shape and its track
  (`origin-css` of the track origin, a share of the shape's box, by
  default), `:transform`, the `transform` a shape has at rest in the code
  its animation plays in (none by default), which the animation keeps,
  and `:units`, to play the units of split texts one by one (see
  `unit-timelines`): `{:kinds :texts :selector :origin :display}`, the
  kinds of units (the others play whole), whether a text plays by them
  (a function of its id, all by default), the selector of a unit (of the
  text and its index), its `transform-origin` (of the same), its CSS
  `display`, if any (an inline element of a page takes no transform),
  and `:adjust`, if any, a function of a text and the timeline of its
  units (see `unit-timelines`) giving the one to play. With `:svg?`, the
  declarations of the groups of an SVG (see `values->declarations`), of
  those that play any (see `svg-group-plays?`), but the width and height
  of the shapes of `:sized`, which play on their elements (see
  `svg-sized?`), each property eased as the canvas eases it (see
  `eased-stops`)."
  ([timeline objects]
   (timeline->css timeline objects nil))
  ([timeline objects {:keys [ids selector origin transform units svg? sized]}]
   (let [;; an SVG draws 3D flat, in no light
         scenes   (if svg? {} (scene-layers timeline objects))
         timeline (-> timeline
                      (cta/flatten-splits (or (:kinds units) #{}) (or (:texts units) (constantly true)))
                      (cta/resolve-animations objects)
                      ;; no outline of a path, which no CSS of a box plays
                      ;; (an SVG plays them by SMIL), nor a track that plays
                      ;; only that
                      (update :tracks
                              (fn [tracks]
                                (into {}
                                      (keep (fn [[id {:keys [keyframes animations] :as track}]]
                                              (let [kept (filterv #(not= :content (:property %)) keyframes)]
                                                (when (or (seq kept) (seq animations) (empty? keyframes))
                                                  [id (assoc track :keyframes kept)]))))
                                      tracks)))
                      cta/expand-loops
                      cta/expand-paths
                      cta/cut-timeline
                      ;; a layer the light or the depth cues of its board
                      ;; show anew as it plays plays them on its own
                      (update :tracks
                              (fn [tracks]
                                (reduce-kv (fn [tracks id scene]
                                             (cond-> tracks
                                               (and (fn? scene) (not (contains? tracks id)))
                                               (assoc id {:shape-id id :keyframes []})))
                                           tracks
                                           scenes))))
         duration (max 1 (:duration timeline))
         iter     (case (cta/playback-mode timeline)
                    :loop      "infinite"
                    :ping-pong "infinite alternate"
                    "1 both")
         selector (or selector #(str "." (shape-css-class (:id %))))
         origin   (or origin (fn [_ track] (origin-css (cta/track-origin track))))
         ;; each unit of a split text, a layer in it (see `unit-timelines`)
         unit-rules
         (when (some? units)
           (for [[text-id {units-tl :timeline unit-ids :ids}] (cta/unit-timelines timeline objects)
                 :let  [text     (get objects text-id)
                        units-tl (cta/cut-timeline (if-let [adjust (:adjust units)] (adjust text units-tl) units-tl))]
                 :when (and (some? text) (or (nil? ids) (contains? ids text-id)))
                 [k unit-id] (map-indexed vector unit-ids)]
             (let [kf-name  (str "penpot-unit-" text-id "-" k)
                   selector ((:selector units) text k)
                   ;; an SVG draws a turn in 3D flat
                   seen     (when-not svg? (unit-perspective objects text))
                   unit     (cond-> (cta/unit-shape units-tl objects unit-id)
                              (some? seen) (assoc :unit-perspective seen))]
               [selector
                (str (css-comment (str (or (:name text) text-id) ", unit " (inc k))) "\n"
                     (track->keyframes-css units-tl unit (get-in units-tl [:tracks unit-id]) kf-name
                                           (cta/position-origin units-tl objects unit-id) nil
                                           :svg? svg?
                                           :eased? svg?)
                     "\n\n"
                     selector " {\n"
                     (when-let [display (:display units)]
                       (str "  display: " display ";\n"))
                     "  animation: " kf-name " " duration "ms linear " iter ";\n"
                     "  transform-origin: " ((:origin units) text k) ";\n}")])))
         rules    (into []
                        (keep (fn [[sid track]]
                                (when-let [shape (and (or (nil? ids) (contains? ids sid))
                                                      (or (not svg?) (svg-group-plays? track (contains? sized sid)))
                                                      (get objects sid))]
                                  (let [kf-name  (str "penpot-anim-" sid)
                                        selector (selector shape)
                                        comment  (css-comment (or (:name shape) sid))
                                        ;; an SVG plays its gradients by SMIL
                                        gradient (when-not svg? (gradient-rule timeline shape track))]
                                    [selector
                                     (str comment "\n"
                                          (track->keyframes-css timeline shape track kf-name
                                                                (cta/position-origin timeline objects sid)
                                                                (when transform (transform shape))
                                                                :svg? svg?
                                                                :eased? (or svg? (fn? (get scenes sid)))
                                                                :sized? (contains? sized sid)
                                                                :scene (get scenes sid)) "\n\n"
                                          selector " {\n  animation: " kf-name " "
                                          duration "ms linear " iter ";\n"
                                          "  transform-origin: "
                                          (origin shape track) ";\n"
                                          (str/join "" (map #(str "  " % "\n") (first gradient)))
                                          "}")
                                     gradient]))))
                        (:tracks timeline))
         gradients (keep #(nth % 2) rules)
         rules    (into rules unit-rules)]
     (if (empty? rules)
       ""
       (str (when (seq gradients)
              (str (gradient-properties gradients) "\n\n"))
            (str/join "\n\n" (map second rules))
            "\n\n@media (prefers-reduced-motion: reduce) {\n  "
            (str/join ",\n  " (map first rules))
            " {\n    animation: none;\n  }\n}")))))
