;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation
  "Keyframe based timeline animations (a.k.a. Penpot Motion).

  A `timeline` is a page level, uuid-keyed object (stored on the page
  under `:timelines`, mirroring `:flows`). Each timeline holds a map of
  `tracks` keyed by the target shape-id, and each track holds a vector of
  `keyframes`. A keyframe animates a single property of a shape at a given
  time.

  This namespace also contains the pure interpolation engine
  (`values-at`, `timeline->modif-tree`, `timeline->opacity`) shared by the
  editor preview, the viewer playback and the exporter, so the animation
  is computed identically everywhere."
  (:require
   [app.common.data :as d]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes.common :as gco]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape.interactions :as cti]
   [app.common.uuid :as uuid]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCHEMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Properties that a keyframe can animate. Reuses the existing easing
;; presets from interactions plus a custom cubic-bezier curve.
(def animatable-properties
  #{:x :y :opacity :rotation :scale-x :scale-y})

(def interpolation-types
  #{:linear :bezier :step})

;; A custom cubic-bezier easing curve, expressed as the two control
;; points (x1 y1 x2 y2) like the CSS `cubic-bezier()` function.
(def schema:bezier-easing
  [:map {:title "BezierEasing"}
   [:type [:= :bezier]]
   [:curve [:tuple ::sm/safe-number ::sm/safe-number ::sm/safe-number ::sm/safe-number]]])

(def schema:easing
  [:multi {:dispatch (fn [v] (if (map? v) :bezier :preset))
           :title "KeyframeEasing"}
   [:preset [::sm/one-of cti/easing-types]]
   [:bezier schema:bezier-easing]])

(def schema:keyframe
  [:map {:title "Keyframe"}
   [:id ::sm/uuid]
   [:time ::sm/safe-int]
   [:property [::sm/one-of animatable-properties]]
   [:value ::sm/safe-number]
   [:easing {:optional true} schema:easing]
   [:interpolation {:optional true} [::sm/one-of interpolation-types]]])

(def schema:track
  [:map {:title "AnimationTrack"}
   [:shape-id ::sm/uuid]
   [:keyframes [:vector {:gen/max 8} schema:keyframe]]])

;; A timeline is scoped to a board (top-level frame). The page-level
;; `:timelines` map is keyed by `:board-id`, so each board owns at most
;; one timeline (like Figma Motion).
(def schema:timeline
  [:map {:title "AnimationTimeline"}
   [:board-id ::sm/uuid]
   [:name :string]
   [:duration ::sm/safe-int]
   [:loop {:optional true} :boolean]
   [:loop-count {:optional true} [:maybe ::sm/safe-int]]
   [:tracks [:map-of {:gen/max 3} ::sm/uuid schema:track]]])

(def schema:timelines
  [:map-of {:gen/max 2} ::sm/uuid schema:timeline])

(sm/register! ::keyframe schema:keyframe)
(sm/register! ::track schema:track)
(sm/register! ::timeline schema:timeline)

(def check-timeline
  (sm/check-fn schema:timeline))

(def valid-timeline?
  (sm/lazy-validator schema:timeline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-duration 1000)

(defn make-timeline
  "Create a timeline for `board-id` (a top-level frame). The page-level
  `:timelines` map is keyed by this same board-id."
  [{:keys [board-id name duration loop loop-count]}]
  (cond-> {:board-id board-id
           :name (or name "Animation")
           :duration (or duration default-duration)
           :tracks {}}
    (some? loop) (assoc :loop loop)
    (some? loop-count) (assoc :loop-count loop-count)))

(defn make-keyframe
  [{:keys [id time property value easing interpolation]}]
  (cond-> {:id (or id (uuid/next))
           :time (or time 0)
           :property property
           :value value}
    (some? easing) (assoc :easing easing)
    (some? interpolation) (assoc :interpolation interpolation)))

(defn sort-keyframes
  "Return keyframes sorted by `:time` and then by `:property` for a
  stable ordering."
  [keyframes]
  (vec (sort-by (juxt :time #(name (:property %))) keyframes)))

(defn get-track
  [timeline shape-id]
  (dm/get-in timeline [:tracks shape-id]))

(defn add-keyframe
  "Add a keyframe to the track of `shape-id`, creating the track if
  needed. If a keyframe for the same property/time already exists it is
  replaced (so re-recording a value at the same instant updates it)."
  [timeline shape-id keyframe]
  (let [keyframe (make-keyframe keyframe)
        track    (or (get-track timeline shape-id) {:shape-id shape-id :keyframes []})
        kept     (->> (:keyframes track)
                      (remove #(and (= (:time %) (:time keyframe))
                                    (= (:property %) (:property keyframe)))))
        track    (assoc track :keyframes (sort-keyframes (conj (vec kept) keyframe)))]
    (assoc-in timeline [:tracks shape-id] track)))

(defn update-keyframe
  [timeline shape-id keyframe-id f]
  (d/update-in-when timeline [:tracks shape-id :keyframes]
                    (fn [keyframes]
                      (sort-keyframes
                       (mapv #(if (= (:id %) keyframe-id) (f %) %) keyframes)))))

(defn remove-keyframe
  "Remove a keyframe. If it leaves the track empty, drop the track too."
  [timeline shape-id keyframe-id]
  (let [timeline (d/update-in-when timeline [:tracks shape-id :keyframes]
                                   (fn [keyframes]
                                     (filterv #(not= (:id %) keyframe-id) keyframes)))]
    (if (empty? (dm/get-in timeline [:tracks shape-id :keyframes]))
      (update timeline :tracks dissoc shape-id)
      timeline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERPOLATION ENGINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cubic-bezier presets matching the CSS easing keywords, so a preset
;; and a custom curve are evaluated by the same solver.
(def ^:private preset-curves
  {:linear      [0.0 0.0 1.0 1.0]
   :ease        [0.25 0.1 0.25 1.0]
   :ease-in     [0.42 0.0 1.0 1.0]
   :ease-out    [0.0 0.0 0.58 1.0]
   :ease-in-out [0.42 0.0 0.58 1.0]})

(defn- cubic-bezier-component
  [t p1 p2]
  (let [mt (- 1.0 t)]
    (+ (* 3 mt mt t p1)
       (* 3 mt t t p2)
       (* t t t))))

(defn cubic-bezier
  "Evaluate a cubic-bezier easing `[x1 y1 x2 y2]` at progress `t`
  (0..1), returning the eased progress. Solves x(s)=t with the
  bisection method (no derivatives needed, robust for any monotonic
  curve) and returns y(s)."
  [[x1 y1 x2 y2] t]
  (cond
    (<= t 0.0) 0.0
    (>= t 1.0) 1.0
    :else
    (loop [lo 0.0 hi 1.0 i 0]
      (let [s  (/ (+ lo hi) 2.0)
            x  (cubic-bezier-component s x1 x2)]
        (if (or (> i 24) (< (mth/abs (- x t)) 0.0005))
          (cubic-bezier-component s y1 y2)
          (if (< x t)
            (recur s hi (inc i))
            (recur lo s (inc i))))))))

(defn- segment-progress
  "Eased progress in [0,1] for the segment that STARTS at `from-keyframe`,
  given the linear progress `t` in [0,1]. Linear/no easing is returned
  exactly (no solver error)."
  [from-keyframe t]
  (let [easing (:easing from-keyframe)]
    (cond
      (or (nil? easing) (= easing :linear)) t
      (keyword? easing) (cubic-bezier (preset-curves easing (preset-curves :linear)) t)
      (map? easing)     (cubic-bezier (:curve easing) t)
      :else             t)))

(defn- lerp
  [a b t]
  (+ a (* (- b a) t)))

(defn- property-value-at
  "Compute the value of a single property from its (time-sorted)
  keyframes at `time`, or nil when there are no keyframes for it."
  [keyframes time]
  (when (seq keyframes)
    (let [first-kf (first keyframes)
          last-kf  (last keyframes)]
      (cond
        (<= time (:time first-kf)) (:value first-kf)
        (>= time (:time last-kf))  (:value last-kf)
        :else
        (let [[from to] (->> (partition 2 1 keyframes)
                             (some (fn [[a b]]
                                     (when (and (<= (:time a) time) (< time (:time b)))
                                       [a b]))))]
          ;; `:step` interpolation holds the source value (exact, keeps type)
          (if (= :step (:interpolation from :linear))
            (:value from)
            (let [span (- (:time to) (:time from))
                  t    (if (zero? span) 0.0 (/ (double (- time (:time from))) span))]
              (lerp (:value from) (:value to) (segment-progress from t)))))))))

(defn values-at
  "Return `{shape-id {property value}}` with the interpolated value of
  every animated property of every track of `timeline` at `time` (ms)."
  [timeline time]
  (reduce-kv
   (fn [acc shape-id track]
     (let [by-prop (group-by :property (:keyframes track))
           values  (reduce-kv
                    (fn [m property keyframes]
                      (let [v (property-value-at (sort-keyframes keyframes) time)]
                        (cond-> m (some? v) (assoc property v))))
                    {}
                    by-prop)]
       (cond-> acc (seq values) (assoc shape-id values))))
   {}
   (:tracks timeline)))

(defn- has? [values & props]
  (some #(contains? values %) props))

(defn- shape->modifiers
  "Build a single modifiers record for `shape` from the interpolated
  absolute `values`. Position/scale/rotation become geometry modifiers;
  opacity becomes a `:change-property` structure modifier so it rides the
  same modifier pipeline in every renderer (SVG via `transform-shape`,
  WASM via `set-shape-opacity`) and in export."
  [shape values]
  (let [selrect (:selrect shape)
        base-x  (:x selrect)
        base-y  (:y selrect)
        base-r  (or (:rotation shape) 0)
        target-x (get values :x base-x)
        target-y (get values :y base-y)
        sx       (get values :scale-x 1)
        sy       (get values :scale-y 1)
        target-r (get values :rotation base-r)
        origin   (gpt/point base-x base-y)
        center   (gco/shape->center shape)]
    (cond-> (ctm/empty)
      (has? values :scale-x :scale-y)
      (ctm/resize (gpt/point sx sy) origin)

      (has? values :x :y)
      (ctm/move (gpt/point (- target-x base-x) (- target-y base-y)))

      (has? values :rotation)
      (ctm/rotation center (- target-r base-r))

      (has? values :opacity)
      (ctm/change-property :opacity (get values :opacity)))))

(defn timeline->modif-tree
  "Compute a transient modif-tree `{shape-id {:modifiers ...}}` for
  `timeline` at `time`, covering transforms (x/y/scale/rotation) and
  opacity (as a change-property modifier). Designed to be fed to
  `set-modifiers` / `set-wasm-modifiers` (editor preview) or applied
  through `transform-shape` (viewer/export)."
  [timeline objects time]
  (reduce-kv
   (fn [tree shape-id values]
     (if-let [shape (get objects shape-id)]
       (let [modifiers (shape->modifiers shape values)]
         (if (ctm/empty? modifiers)
           tree
           (assoc tree shape-id {:modifiers modifiers})))
       tree))
   {}
   (values-at timeline time)))

(defn timeline->opacity
  "Compute `{shape-id opacity}` for every shape that animates opacity at
  `time` (opacity is applied as a transient style override, not a
  modifier)."
  [timeline time]
  (reduce-kv
   (fn [acc shape-id values]
     (if (contains? values :opacity)
       (assoc acc shape-id (:opacity values))
       acc))
   {}
   (values-at timeline time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAINTENANCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-shapes
  "Drop the tracks targeting any of `shape-ids` (used when shapes are
  deleted). Returns the updated timeline."
  [timeline shape-ids]
  (update timeline :tracks #(apply dissoc % shape-ids)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS EXPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- fmt
  "Format a number for CSS: rounded to 3 decimals, integers without a
  trailing `.0`."
  [v]
  (let [r (mth/precision v 3)]
    (if (== r (mth/round r))
      (str (long r))
      (str r))))

(defn easing->css
  "Render a keyframe easing as a CSS timing-function string."
  [easing]
  (cond
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

(defn- short-id
  [id]
  (subs (str id) 0 8))

(defn- values->declarations
  "Build the CSS declarations (transform + opacity) for `shape` given the
  interpolated absolute `values` at one keyframe stop."
  [shape values]
  (let [selrect (:selrect shape)
        base-x  (:x selrect)
        base-y  (:y selrect)
        base-r  (or (:rotation shape) 0)
        dx      (- (get values :x base-x) base-x)
        dy      (- (get values :y base-y) base-y)
        sx      (get values :scale-x 1)
        sy      (get values :scale-y 1)
        rot     (- (get values :rotation base-r) base-r)
        transform (str "translate(" (fmt dx) "px, " (fmt dy) "px) "
                       "rotate(" (fmt rot) "deg) "
                       "scale(" (fmt sx) ", " (fmt sy) ")")
        decls [(str "transform: " transform ";")]]
    (cond-> decls
      (contains? values :opacity)
      (conj (str "opacity: " (fmt (:opacity values)) ";")))))

(defn- track->keyframes-css
  [timeline shape track kf-name]
  (let [duration (max 1 (:duration timeline))
        sid      (:shape-id track)
        kfs      (:keyframes track)
        times    (-> (into (sorted-set 0 duration) (map :time kfs)) vec)
        stops    (for [t times]
                   (let [values     (get (values-at timeline t) sid {})
                         pct        (fmt (* 100.0 (/ (double t) duration)))
                         seg-easing (some #(when (= (:time %) t) (:easing %)) kfs)
                         decls      (cond-> (values->declarations shape values)
                                      (and seg-easing (< t duration))
                                      (conj (str "animation-timing-function: "
                                                 (easing->css seg-easing) ";")))]
                     (str "  " pct "% { " (str/join " " decls) " }")))]
    (str "@keyframes " kf-name " {\n" (str/join "\n" stops) "\n}")))

(defn timeline->css
  "Generate a CSS string (`@keyframes` blocks + per-shape `animation`
  rules) for `timeline`, using the base geometry from `objects`. Pure."
  [timeline objects]
  (let [duration (max 1 (:duration timeline))
        iter     (if (:loop timeline) "infinite" "1")]
    (->> (:tracks timeline)
         (keep (fn [[sid track]]
                 (when-let [shape (get objects sid)]
                   (let [kf-name  (str "penpot-anim-" (short-id sid))
                         selector (str ".penpot-shape-" (short-id sid))
                         comment  (str "/* " (or (:name shape) (str sid)) " */")]
                     (str comment "\n"
                          (track->keyframes-css timeline shape track kf-name) "\n\n"
                          selector " {\n  animation: " kf-name " "
                          duration "ms linear " iter ";\n}")))))
         (str/join "\n\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOTTIE (bodymovin JSON) EXPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Builds a Lottie animation document (a plain map ready to be
;; JSON-encoded). The board (the timeline's `:board-id`) is the
;; composition; each tracked shape becomes a shape layer whose transform
;; (position/scale/rotation/opacity) is animated with native Lottie
;; keyframes + cubic-bezier easing handles. Shape geometry is exported as
;; a placeholder rectangle of the shape's bounds and first fill colour
;; (full vector fidelity -- paths/strokes/text -- is a follow-up).

(def ^:private lottie-fps 60)

(defn- ms->frames
  [ms]
  (mth/precision (* (/ (double ms) 1000.0) lottie-fps) 3))

(defn- easing->bezier-curve
  [easing]
  (cond
    (map? easing)     (:curve easing)
    (keyword? easing) (preset-curves easing (preset-curves :linear))
    :else             (preset-curves :linear)))

(defn- hex->rgb01
  "Parse a `#rrggbb` colour into normalized [r g b] (0..1). Falls back to a
  neutral grey."
  [hex]
  (let [h (cond
            (and (string? hex) (str/starts-with? hex "#")) (subs hex 1)
            (string? hex) hex
            :else nil)]
    (if (and h (= 6 (count h)))
      (let [pp (fn [s]
                 (/ (double #?(:clj (Integer/parseInt s 16)
                               :cljs (js/parseInt s 16)))
                    255.0))]
        [(pp (subs h 0 2)) (pp (subs h 2 4)) (pp (subs h 4 6))])
      [0.694 0.698 0.710])))

(defn- shape-fill-rgb
  [shape]
  (hex->rgb01 (-> shape :fills first :fill-color)))

(defn- lottie-keyframes
  "Build a Lottie keyframe vector for a single-property keyframe list.
  `val-fn` maps a keyframe value to its Lottie `s` array. The segment
  easing is carried on the starting keyframe via `:o`/`:i` (or `:h 1` for
  a stepped hold)."
  [kfs val-fn]
  (let [kfs (sort-keyframes kfs)
        n   (count kfs)]
    (vec (map-indexed
          (fn [i kf]
            (let [base {:t (ms->frames (:time kf)) :s (val-fn (:value kf))}]
              (cond
                (= i (dec n))
                base

                (= :step (:interpolation kf :linear))
                (assoc base :h 1)

                :else
                (let [[x1 y1 x2 y2] (easing->bezier-curve (:easing kf))]
                  (assoc base :o {:x [x1] :y [y1]} :i {:x [x2] :y [y2]})))))
          kfs))))

(defn- lottie-prop-1d
  "A 1D Lottie property (split position x/y, rotation, opacity). Static
  values are scalars; animated values use keyframes with `s` of `[v]`."
  [kfs scalar-fn default-scalar]
  (if (empty? kfs)
    {:a 0 :k default-scalar}
    {:a 1 :k (lottie-keyframes kfs (fn [v] [(scalar-fn v)]))}))

(defn- lottie-scale-prop
  "Combine the (separate) scale-x and scale-y tracks into one 2D Lottie
  scale property (percentages)."
  [sx-kfs sy-kfs]
  (if (and (empty? sx-kfs) (empty? sy-kfs))
    {:a 0 :k [100 100 100]}
    (let [sx    (sort-keyframes sx-kfs)
          sy    (sort-keyframes sy-kfs)
          times (->> (concat sx sy) (map :time) distinct sort vec)
          n     (count times)
          at    (fn [kfs t] (some #(when (= (:time %) t) %) kfs))]
      {:a 1
       :k (vec (map-indexed
                (fn [i t]
                  (let [vx   (or (property-value-at sx t) 1)
                        vy   (or (property-value-at sy t) 1)
                        kf   (or (at sx t) (at sy t))
                        base {:t (ms->frames t) :s [(* 100.0 vx) (* 100.0 vy) 100]}]
                    (cond
                      (= i (dec n))
                      base

                      (= :step (:interpolation kf :linear))
                      (assoc base :h 1)

                      :else
                      (let [[x1 y1 x2 y2] (easing->bezier-curve (:easing kf))]
                        (assoc base :o {:x [x1] :y [y1]} :i {:x [x2] :y [y2]})))))
                times))})))

(defn- shape->lottie-layer
  "Build a Lottie shape layer for `shape`/`track`. `ox`/`oy` is the board
  origin so coordinates are relative to the composition."
  [ind shape track ox oy]
  (let [by-prop (group-by :property (:keyframes track))
        selrect (:selrect shape)
        sw      (double (:width selrect))
        sh      (double (:height selrect))
        cx      (+ (- (:x selrect) ox) (/ sw 2.0))
        cy      (+ (- (:y selrect) oy) (/ sh 2.0))
        base-r  (or (:rotation shape) 0)
        [r g b] (shape-fill-rgb shape)]
    {:ddd 0
     :ind (inc ind)
     :ty 4
     :nm (or (:name shape) (str "layer-" (inc ind)))
     :sr 1
     :ks {:o (lottie-prop-1d (by-prop :opacity) (fn [v] (* 100.0 v)) 100)
          :r (lottie-prop-1d (by-prop :rotation) identity base-r)
          :p {:s true
              :x (lottie-prop-1d (by-prop :x) (fn [v] (+ (- v ox) (/ sw 2.0))) cx)
              :y (lottie-prop-1d (by-prop :y) (fn [v] (+ (- v oy) (/ sh 2.0))) cy)}
          :a {:a 0 :k [cx cy 0]}
          :s (lottie-scale-prop (by-prop :scale-x) (by-prop :scale-y))}
     :ao 0
     :shapes [{:ty "gr"
               :nm "shape"
               :np 3
               :it [{:ty "rc" :d 1 :nm "rect"
                     :s {:a 0 :k [sw sh]}
                     :p {:a 0 :k [cx cy]}
                     :r {:a 0 :k 0}}
                    {:ty "fl" :nm "fill" :r 1
                     :c {:a 0 :k [r g b 1]}
                     :o {:a 0 :k 100}}
                    {:ty "tr" :nm "transform"
                     :p {:a 0 :k [0 0]}
                     :a {:a 0 :k [0 0]}
                     :s {:a 0 :k [100 100]}
                     :r {:a 0 :k 0}
                     :o {:a 0 :k 100}}]}]
     :ip 0
     :op (ms->frames (max 1 (:duration track 0)))
     :st 0
     :bm 0}))

(defn timeline->lottie
  "Generate a Lottie (bodymovin) animation document (a plain map ready for
  JSON encoding) for `timeline`, using `objects` for geometry. The board
  (`:board-id`) is the composition. Pure."
  [timeline objects]
  (let [duration  (max 1 (:duration timeline))
        op-frames (ms->frames duration)
        board     (get objects (:board-id timeline))
        bsr       (:selrect board)
        ox        (or (:x bsr) 0)
        oy        (or (:y bsr) 0)
        width     (or (:width bsr) 100)
        height    (or (:height bsr) 100)
        layers    (->> (:tracks timeline)
                       (keep (fn [[sid track]]
                               (when-let [shape (get objects sid)]
                                 [(assoc track :duration duration) shape])))
                       (map-indexed (fn [i [track shape]]
                                      (shape->lottie-layer i shape track ox oy)))
                       vec)]
    {:v "5.7.0"
     :fr lottie-fps
     :ip 0
     :op op-frames
     :w (mth/round width)
     :h (mth/round height)
     :nm (or (:name timeline) "Penpot Animation")
     :ddd 0
     :assets []
     :layers layers}))
