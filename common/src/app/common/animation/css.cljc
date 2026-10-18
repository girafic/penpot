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
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
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
  #{:x :y :width :height :scale-x :scale-y :rotation :rotate-x :rotate-y})

(declare frame-values*)

(defn frame-values
  "What `shape` shows at one frame of its animation, `values` (its
  values then, see `values-at`), as CSS plays it: `:translate-x`,
  `:translate-y` (px), `:rotate` (degrees) and `:scale-x`, `:scale-y`, all
  from its rest pose, when its position, size, scale or rotation plays;
  then, for the properties `values` holds, `:opacity`, `:border-radius`
  (the four corners in px), `:background-color` and `:border-color` (CSS
  colours of the first solid fill and stroke), `:border-width` (px) (of a
  text, `:color`, `:text-stroke-color` and `:text-stroke-width`: it paints
  its glyphs in the fills of its spans, see `cta/paint-fills`),
  `:box-shadow` (the CSS of its visible shadows), `:blur` and
  `:backdrop-blur` (px). `origin` is where its positions count from (see
  `position-origin`). A tilted shape (see `app.common.geom.tilt`) has its
  `:rotate-x` and `:rotate-y` (degrees) with its transform, and, turning
  around `pivot` (fractions of its box) off its middle with a tilt at
  rest, `:lift` (`[dx dy dz]` px, see `gtl/lift`). A board whose camera
  plays (see `cta/camera-properties`) has `:perspective` (px) and
  `:perspective-origin` (CSS, shares of its box)."
  ([shape values origin]
   (frame-values shape values origin nil))
  ([shape values origin pivot]
   (let [frame (frame-values* shape values origin)]
     (if (and (contains? frame :translate-x)
              (or (gtl/tilted? shape) (cta/has? values :rotate-x :rotate-y)))
       (let [rest-x (or (:rotate-x shape) 0)
             rest-y (or (:rotate-y shape) 0)
             {px :x py :y} (or pivot cta/default-transform-origin)
             lift   (gtl/lift shape px py rest-x rest-y)]
         (cond-> (assoc frame
                        :rotate-x (get values :rotate-x rest-x)
                        :rotate-y (get values :rotate-y rest-y))
           (some #(not (mth/almost-zero? %)) lift)
           (assoc :lift lift)))
       frame))))

(defn- frame-values*
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
        stroke  (d/seek cta/solid-color-stroke? strokes)]
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

      (and stroke (cta/has? values :stroke-color :stroke-opacity))
      (assoc (if text? :text-stroke-color :border-color) (css-color (:stroke-color stroke) (or (:stroke-opacity stroke) 1)))

      (and stroke (contains? values :stroke-width))
      (assoc (if text? :text-stroke-width :border-width) (or (:stroke-width stroke) 0))

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

      ;; the camera of a board: CSS sees the tilted layers in it with its
      ;; perspective (see `app.common.geom.tilt`)
      (and (cta/has? values :perspective :perspective-x :perspective-y) (= :frame (:type shape)))
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

(defn- css-declarations
  "The CSS declarations of `shape` at a frame: the tilt in its own axes,
  after the turn it rests at (`rest-transform`, flat) and before its
  scale, lifted to its pivot (see `frame-values`)."
  [shape values origin rest-transform pivot]
  (let [{:keys [translate-x translate-y rotate scale-x scale-y opacity border-radius
                background-color border-color border-width box-shadow blur backdrop-blur
                color text-stroke-color text-stroke-width lift perspective perspective-origin]
         :as frame}
        (frame-values shape values origin pivot)]
    (cond-> []
      (contains? frame :translate-x)
      (conj (str "transform: translate(" (fmt translate-x) "px, " (fmt translate-y) "px) "
                 "rotate(" (fmt rotate) "deg) "
                 (when-not (str/blank? rest-transform)
                   (str rest-transform " "))
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

      (contains? frame :border-color)
      (conj (str "border-color: " border-color ";"))

      (contains? frame :border-width)
      (conj (str "border-width: " (fmt border-width) "px;"))

      (contains? frame :color)
      (conj (str "color: " color ";"))

      (contains? frame :text-stroke-color)
      (conj (str "-webkit-text-stroke-color: " text-stroke-color ";"))

      (contains? frame :text-stroke-width)
      (conj (str "-webkit-text-stroke-width: " (fmt text-stroke-width) "px;"))

      (contains? frame :box-shadow)
      (conj (str "box-shadow: " box-shadow ";"))

      (contains? frame :blur)
      (conj (str "filter: blur(" (fmt blur) "px);"))

      (contains? frame :backdrop-blur)
      (conj (str "backdrop-filter: blur(" (fmt backdrop-blur) "px);"))

      (contains? frame :perspective)
      (conj (str "perspective: " (fmt perspective) "px;")
            (str "perspective-origin: " perspective-origin ";")))))

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

(defn value-numbers
  "The numbers of `values` (see `values-at`), in an order of their own: a
  colour as its channels, an indexed value by index."
  [values]
  (into []
        (mapcat (fn [[_ v]]
                  (cond
                    (number? v)                                [(double v)]
                    (and (string? v) (str/starts-with? v "#")) (mapv double (clr/hex->rgb v))
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
  of the stretch to the next."
  [timeline shape track kf-name origin rest-transform & {:keys [svg? sized? eased?]}]
  (let [duration (max 1 (:duration timeline))
        sid      (:shape-id track)
        kfs      (:keyframes track)
        ;; Keyframes past the end do not play: the value at the end is
        ;; the one on the way to them.
        times    (-> (into (sorted-set 0 duration)
                           (comp (map :time) (filter #(<= % duration)))
                           kfs)
                     vec)
        values   (let [values (shape-values timeline sid)]
                   #(cond-> (values %) sized? (dissoc :width :height)))
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

(defn timeline->css
  "Generate a CSS string (`@keyframes` blocks + per-shape `animation`
  rules) for `timeline`, using the base geometry from `objects`. Pure.
  Played once, a shape keeps the last frame. With reduced motion asked
  for, the shapes stay as designed. Split animations play as animations
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
   (let [timeline (-> timeline
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
                      cta/cut-timeline)
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
                   unit     (cta/unit-shape units-tl objects unit-id)]
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
                                        comment  (css-comment (or (:name shape) sid))]
                                    [selector
                                     (str comment "\n"
                                          (track->keyframes-css timeline shape track kf-name
                                                                (cta/position-origin timeline objects sid)
                                                                (when transform (transform shape))
                                                                :svg? svg?
                                                                :eased? svg?
                                                                :sized? (contains? sized sid)) "\n\n"
                                          selector " {\n  animation: " kf-name " "
                                          duration "ms linear " iter ";\n"
                                          "  transform-origin: "
                                          (origin shape track) ";\n}")]))))
                        (:tracks timeline))
         rules    (into rules unit-rules)]
     (if (empty? rules)
       ""
       (str (str/join "\n\n" (map second rules))
            "\n\n@media (prefers-reduced-motion: reduce) {\n  "
            (str/join ",\n  " (map first rules))
            " {\n    animation: none;\n  }\n}")))))
