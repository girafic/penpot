;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.animation.lottie
  "The Lottie (bodymovin JSON) export of a timeline (see
  `timeline->lottie`): a Lottie animation document, a plain map ready to
  be JSON-encoded. The board (the timeline's `:board-id`) is the
  composition; each tracked shape becomes a shape layer whose transform
  (position/scale/rotation/opacity) is animated with native Lottie
  keyframes and cubic-bezier easing handles. Rectangles, ellipses, paths
  and booleans, and groups and boards of them, are exported as vectors,
  with their fills, strokes, trim, shadows and blur (see
  `lottie-vector-layer?`); other shapes as images or, without them, as a
  rectangle of their bounds in their first fill colour."
  (:require
   [app.common.animation.css :as acss]
   [app.common.animation.export :as aexp]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.shapes.corners :as gco]
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.animation-follow :as ctaf]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [clojure.string :as str]))

(def ^:private lottie-fps 60)

(defn- ms->frames
  [ms]
  (mth/precision (* (/ (double ms) 1000.0) lottie-fps) 3))

(defn- easing->bezier-curve
  "Control points of a keyframe easing; springs are unrolled before (see
  `expand-springs`), anything else is linear."
  [easing]
  (or (cta/easing-curve easing) (cta/preset-curves :linear)))

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

(defn- lottie-keyframes
  "Build a Lottie keyframe vector for a single-property keyframe list.
  `val-fn` maps a keyframe value to its Lottie `s` array. The segment
  easing is carried on the starting keyframe via `:o`/`:i` (or `:h 1` for
  a stepped hold)."
  [kfs val-fn]
  (let [kfs (cta/sort-keyframes kfs)
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
  values are scalars, as is a single keyframe: Lottie players draw no
  layer with an animated value of one keyframe. Animated values use
  keyframes with `s` of `[v]`."
  [kfs scalar-fn default-scalar]
  (case (count kfs)
    0 {:a 0 :k default-scalar}
    1 {:a 0 :k (scalar-fn (:value (first kfs)))}
    {:a 1 :k (lottie-keyframes kfs (fn [v] [(scalar-fn v)]))}))

(defn- lottie-stops-prop
  "A Lottie property playing `stops` (`[time keyframe value]` each, see
  `eased-stops`), `render` giving the Lottie value (`s`) of a value and
  `static` the one of the property when it keeps its first: each
  keyframe eased by the curve of its keyframe, held for a hold, linear
  without one."
  [stops render static]
  (let [n  (count stops)
        v0 (render (nth (first stops) 2))]
    (if (or (< n 2) (every? #(= v0 (render (nth % 2))) stops))
      {:a 0 :k (static v0)}
      {:a 1
       :k (into []
                (map-indexed
                 (fn [i [t kf v]]
                   (let [base {:t (ms->frames t) :s (render v)}]
                     (cond
                       (= i (dec n))                         base
                       (= :step (:interpolation kf :linear)) (assoc base :h 1)
                       :else
                       (let [[x1 y1 x2 y2] (easing->bezier-curve (:easing kf))]
                         (assoc base :o {:x [x1] :y [y1]} :i {:x [x2] :y [y2]}))))))
                stops)})))

(defn- lottie-pair-prop
  "One 2D Lottie property from two (separate) 1D tracks, `a-kfs` and
  `b-kfs`; `val-fn` gives the Lottie value of their values, `a` and `b`
  when a track has none. Each eased by its own curve (see
  `eased-stops`): a stretch where one curve can not ease both is taken
  as it goes."
  [a-kfs b-kfs a b val-fn]
  (let [ak     (cta/sort-keyframes a-kfs)
        bk     (cta/sort-keyframes b-kfs)
        times  (->> (concat ak bk) (map :time) distinct sort vec)
        values (fn [t] [(double (or (cta/property-value-at ak t) a))
                        (double (or (cta/property-value-at bk t) b))])]
    (if (< (count times) 2)
      ;; static for no keyframe or one instant (see `lottie-prop-1d`)
      {:a 0 :k (val-fn (or (:value (first ak)) a) (or (:value (first bk)) b))}
      (lottie-stops-prop (map (fn [[t kf]] [t kf (values t)])
                              (acss/eased-stops values (concat ak bk) times))
                         #(apply val-fn %)
                         identity))))

(defn- lottie-scale-prop
  "Combine the (separate) scale-x and scale-y tracks into one 2D Lottie
  scale property (percentages)."
  [sx-kfs sy-kfs]
  (lottie-pair-prop sx-kfs sy-kfs 1 1 (fn [x y] [(* 100.0 x) (* 100.0 y) 100])))

(defn- lottie-eased-prop
  "A Lottie property of `value` (a time to a vector of numbers, as the
  canvas plays it) at the times of the keyframes `kfs`, eased as the
  canvas eases each property (see `eased-stops`, `lottie-stops-prop`)."
  [value kfs render static]
  (let [times (vec (into (sorted-set) (map :time) kfs))]
    (lottie-stops-prop (if (empty? times)
                         [[0 nil (value 0)]]
                         (map (fn [[t kf]] [t kf (value t)]) (acss/eased-stops value kfs times)))
                       render
                       static)))

(defn lottie-sized?
  "Whether a Lottie export plays the width and height `track` animates on
  `shape` on its outline, as the canvas resizes it, the layers in a board
  following it, as an SVG does (see `svg-sized?`); else they stretch its
  layer (see `lottie-stretched?`)."
  [shape track]
  (aexp/svg-sized? shape track))

(defn- lottie-stretched?
  "Whether the width and height `track` animates on `shape` stretch its
  Lottie layer (see `lottie-sized?`): not those of a group that plays
  them on its layers (see `aexp/expand-group-sizes`)."
  [shape track]
  (boolean
   (and (some #(#{:width :height} (:property %)) (:keyframes track))
        (not (aexp/sized-on-layers? track))
        (not (lottie-sized? shape track)))))

(defn- lottie-box
  "A function of a time to the box of `shape` then, resized by the width
  and height `track` plays around its pivot, as the canvas resizes it,
  in the board at `ox`, `oy`: `[left top width height radius]`, its
  first radius clamped as Lottie clamps it."
  [shape track ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        pivot  (cta/box-point shape (cta/track-origin track))
        values (acss/shape-values {:tracks {(:id shape) track}} (:id shape))]
    (fn [t]
      (let [v (values t)
            w (double (get v :width width))
            h (double (get v :height height))
            r (double (get v :r1 (or (:r1 shape) 0)))]
        [(- (:x pivot) (* (- (:x pivot) x) (/ w width)) ox)
         (- (:y pivot) (* (- (:y pivot) y) (/ h height)) oy)
         w
         h
         (min r (/ (min w h) 2))]))))

(defn- lottie-size-props
  "The `:size` and `:center` Lottie properties of the outline of `shape`
  whose width and height `track` plays on it (see `lottie-sized?`), in
  the board at `ox`, `oy`: resized around its pivot (see `lottie-box`)."
  [shape track ox oy]
  (let [box (lottie-box shape track ox oy)
        kfs (filterv #(#{:width :height} (:property %)) (:keyframes track))]
    {:size   (lottie-eased-prop #(subvec (box %) 2 4) kfs identity identity)
     :center (lottie-eased-prop (fn [t]
                                  (let [[l tp w h] (box t)]
                                    [(+ l (/ w 2)) (+ tp (/ h 2))]))
                                kfs identity identity)}))

(defn- lottie-rect-shape4
  "A Lottie path of a rectangle `[left top width height r1 r2 r3 r4]`
  (top left, top right, bottom right, bottom left): eight points, its
  corners curves (lines when square), so it tweens with any other."
  [[l t w h r1 r2 r3 r4]]
  (let [k      #(* aexp/kappa %)
        right  (+ l w)
        bottom (+ t h)]
    {:c true
     :v [[(+ l r1) t] [(- right r2) t] [right (+ t r2)] [right (- bottom r3)]
         [(- right r3) bottom] [(+ l r4) bottom] [l (- bottom r4)] [l (+ t r1)]]
     :i [[(- (k r1)) 0] [0 0] [0 (- (k r2))] [0 0] [(k r3) 0] [0 0] [0 (k r4)] [0 0]]
     :o [[0 0] [(k r2) 0] [0 0] [0 (k r3)] [0 0] [(- (k r4)) 0] [0 0] [0 (- (k r1))]]}))

(defn- lottie-rect-shape
  "A Lottie path of a rectangle `[left top width height radius]`, its
  corners alike (see `lottie-rect-shape4`)."
  [[l t w h r]]
  (lottie-rect-shape4 [l t w h r r r r]))

(defn- lottie-corners-path
  "The Lottie path (`sh`) of the outline of the rectangle or board
  `shape`, in the board at `ox`, `oy`, before it turns, whose corners not
  alike the keyframes `kfs` play, as the canvas clamps them."
  [shape kfs ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        sid    (:id shape)
        values (acss/shape-values {:tracks {sid {:shape-id sid :keyframes kfs}}} sid)
        value  (fn [t]
                 (let [v (values t)]
                   (into [(- x ox) (- y oy) width height]
                         (gco/shape-corners-4 {:width width :height height
                                               :r1 (get v :r1 (or (:r1 shape) 0))
                                               :r2 (get v :r2 (or (:r2 shape) 0))
                                               :r3 (get v :r3 (or (:r3 shape) 0))
                                               :r4 (get v :r4 (or (:r4 shape) 0))}))))]
    {:ty "sh" :nm "outline"
     :ks (lottie-eased-prop #(mapv double (value %))
                            (filterv #(#{:r1 :r2 :r3 :r4} (:property %)) kfs)
                            #(vector (lottie-rect-shape4 %))
                            first)}))

(defn- lottie-corners-play?
  "Whether the keyframes `kfs` play corners of the rectangle or board
  `shape` that are not alike (see `svg-even-corners?`)."
  [shape kfs]
  (and (contains? #{:rect :frame} (:type shape))
       (some #(#{:r1 :r2 :r3 :r4} (:property %)) kfs)
       (not (aexp/svg-even-corners? shape {:keyframes kfs}))))

(defn- slot-kfs
  "Keyframes of `property` at `index` (nil = the unindexed property)."
  [by-prop property index]
  (filterv #(= (:index %) index) (by-prop property)))

(defn- lottie-color-prop
  "A Lottie colour: static for no keyframe or one (see `lottie-prop-1d`)."
  [kfs default-rgb]
  (let [rgba (fn [v]
               (let [[r g b] (hex->rgb01 v)]
                 [r g b 1]))]
    (case (count kfs)
      0 {:a 0 :k (conj (vec default-rgb) 1)}
      1 {:a 0 :k (rgba (:value (first kfs)))}
      {:a 1 :k (lottie-keyframes kfs rgba)})))

(defn- lottie-dashes
  "The dash pattern of a stroke of `style` and `width`, as the canvas
  draws it."
  [{:keys [stroke-style stroke-width stroke-dash stroke-gap]}]
  (let [w       (or stroke-width 1)
        pattern (case stroke-style
                  :dotted [0 (+ w 5)]
                  :dashed [(or stroke-dash (+ w 10)) (or stroke-gap (+ w 10))]
                  :mixed  [(+ w 5) (+ w 5) (+ w 1) (+ w 5)]
                  nil)]
    (when (seq pattern)
      (conj (vec (map-indexed (fn [i length]
                                (if (even? i)
                                  {:n "d" :nm "dash" :v {:a 0 :k length}}
                                  {:n "g" :nm "gap" :v {:a 0 :k length}}))
                              pattern))
            {:n "o" :nm "offset" :v {:a 0 :k 0}}))))

(defn- lottie-stroke-items
  "The Lottie strokes of `shape`, top-most first, each with the keyframes
  of its index, but the hidden ones. Lottie draws them centered on the
  outline."
  [shape by-prop]
  (->> (cta/as-vec (:strokes shape))
       (map-indexed
        (fn [index stroke]
          (let [color-kfs (slot-kfs by-prop :stroke-color index)
                opac-kfs  (slot-kfs by-prop :stroke-opacity index)
                width-kfs (slot-kfs by-prop :stroke-width index)]
            ;; a hidden one stays hidden as it plays
            (when (and (not (:hidden stroke))
                       (or (cta/solid-color-stroke? stroke) (seq color-kfs)))
              (let [cap    (:stroke-cap-start stroke)
                    dashes (lottie-dashes stroke)]
                (cond-> {:ty "st" :nm (str "stroke " index)
                         :c (lottie-color-prop color-kfs (hex->rgb01 (or (:stroke-color stroke)
                                                                         (:value (first color-kfs))
                                                                         "#000000")))
                         :o (lottie-prop-1d opac-kfs (fn [v] (* 100.0 v))
                                            (* 100.0 (or (:stroke-opacity stroke) 1)))
                         :w (lottie-prop-1d width-kfs identity (or (:stroke-width stroke) 1))
                         :lc (cond
                               (= :dotted (:stroke-style stroke))           2
                               (not= cap (:stroke-cap-end stroke))          1
                               :else (case cap :round 2 :square 3 1))
                         :lj 1 :ml 4}
                  (seq dashes) (assoc :d dashes)))))))
       (remove nil?)
       (vec)))

(defn- fill-rule
  "Lottie fill rule of `shape`: 2 (even-odd) when it says so, else 1."
  [shape]
  (if (= "evenodd" (or (dm/get-in shape [:svg-attrs :fill-rule])
                       (dm/get-in shape [:svg-attrs :fillRule])))
    2
    1))

(defn- lottie-gradient-fill
  "A Lottie gradient fill of `gradient` over the box `[x y width height]`,
  where the gradient points are fractions of the box."
  [{:keys [type start-x start-y end-x end-y stops]} opacity rule [x y width height]]
  (let [stops (sort-by :offset stops)
        point (fn [px py] [(+ x (* px width)) (+ y (* py height))])]
    {:ty "gf" :nm "gradient" :r rule
     :t (if (= type :radial) 2 1)
     :s {:a 0 :k (point start-x start-y)}
     :e {:a 0 :k (point end-x end-y)}
     :g {:p (count stops)
         :k {:a 0 :k (vec (concat (mapcat (fn [{:keys [offset color]}]
                                            (cons offset (hex->rgb01 color)))
                                          stops)
                                  (mapcat (fn [{:keys [offset opacity]}]
                                            [offset (or opacity 1)])
                                          stops)))}}
     :o {:a 0 :k (* 100.0 (or opacity 1))}}))

(defn- lottie-fill-items
  "The Lottie fills of `shape`, top-most first: flat colours with the
  keyframes of their index, and gradients over `box` (see
  `lottie-gradient-fill`)."
  [shape by-prop box]
  (let [rule (fill-rule shape)]
    (->> (cta/as-vec (:fills shape))
         (map-indexed
          (fn [index fill]
            (cond
              (cta/solid-color-fill? fill)
              {:ty "fl" :nm (str "fill " index) :r rule
               :c (lottie-color-prop (slot-kfs by-prop :fill-color index) (hex->rgb01 (:fill-color fill)))
               :o (lottie-prop-1d (slot-kfs by-prop :fill-opacity index) (fn [v] (* 100.0 v))
                                  (* 100.0 (or (:fill-opacity fill) 1)))}

              (some? (:fill-color-gradient fill))
              (lottie-gradient-fill (:fill-color-gradient fill) (:fill-opacity fill) rule box))))
         (remove nil?)
         (vec))))

(defn- lottie-trim-item
  "Lottie trim paths of the trim keyframes, or nil without any: start and
  end in percent, the offset in degrees (a whole turn is the outline).
  Of an outline of `paths` Lottie paths more than one, along them all one
  after another (`m` 2), as the canvas trims them, not each by itself."
  [by-prop paths]
  (when (some #(seq (by-prop %)) (keys cta/trim-properties))
    {:ty "tm" :nm "trim" :m (if (> paths 1) 2 1)
     :s (lottie-prop-1d (by-prop :trim-start) (fn [v] (* 100.0 v)) 0)
     :e (lottie-prop-1d (by-prop :trim-end) (fn [v] (* 100.0 v)) 100)
     :o (lottie-prop-1d (by-prop :trim-offset) (fn [v] (* 360.0 v)) 0)}))

(defn lottie-blurriness
  "The blurriness of a Lottie Gaussian blur as strong as the layer blur
  `value` on the canvas: lottie-web blurs by 0.3 of it as a standard
  deviation."
  [value]
  (/ (aexp/blur-sigma value) 0.3))

(defn lottie-blur-value
  "The value of a layer blur as strong as a Lottie Gaussian blur of
  `blurriness` (see `lottie-blurriness`)."
  [blurriness]
  (max 0 (/ (- (* 0.3 blurriness) 0.5) aexp/blur-sigma-scale)))

(defn- lottie-gaussian-blur
  "A Lottie Gaussian blur as strong as a layer blur of `value` and the
  keyframes `kfs` of it on the canvas, as After Effects and lottie-web
  take it: its blurriness (see `lottie-blurriness`) in both dimensions,
  edges not repeated."
  [kfs value]
  {:ty 29
   :nm "Gaussian Blur"
   :mn "ADBE Gaussian Blur 2"
   :np 5
   :en 1
   :ef [{:ty 0 :nm "Blurriness" :mn "ADBE Gaussian Blur 2-0001"
         :v (lottie-prop-1d kfs lottie-blurriness (lottie-blurriness (or value 0)))}
        {:ty 7 :nm "Blur Dimensions" :mn "ADBE Gaussian Blur 2-0002" :v {:a 0 :k 1}}
        {:ty 7 :nm "Repeat Edge Pixels" :mn "ADBE Gaussian Blur 2-0003" :v {:a 0 :k 0}}]})

(defn- lottie-blur-effect
  "A Lottie Gaussian blur of the layer blur of `shape` and its keyframes
  (see `lottie-gaussian-blur`). None of a hidden blur, which stays hidden
  as it plays."
  [shape by-prop]
  (let [blur (:blur shape)
        kfs  (by-prop :blur)]
    (when (and (not (:hidden blur)) (or (seq kfs) (some? blur)))
      [(lottie-gaussian-blur kfs (:value blur))])))

(defn- unwrap-angles
  "`stops` (`[time keyframe [degrees]]` each) turning the short way from
  one to the next, by whole turns."
  [stops]
  (reduce (fn [out [t kf [v]]]
            (let [prev (some-> (peek out) (nth 2) first)
                  v    (if (nil? prev) v (+ v (* 360 (mth/round (/ (- prev v) 360)))))]
              (conj out [t kf [v]])))
          []
          stops))

(defn- lottie-pair-1d
  "A 1D Lottie property of two 1D tracks, `val-fn` giving its value of
  theirs, `a` and `b` when a track has none: taken as it goes where it
  is no straight function of theirs (see `eased-stops`), as the direction
  and distance of a shadow of its offsets, which so moves straight;
  with `angle?`, degrees, turning the short way."
  [a-kfs b-kfs a b val-fn & {:keys [angle?]}]
  (let [ak    (cta/sort-keyframes a-kfs)
        bk    (cta/sort-keyframes b-kfs)
        times (->> (concat ak bk) (map :time) distinct sort vec)
        value (fn [t] [(double (val-fn (or (cta/property-value-at ak t) a) (or (cta/property-value-at bk t) b)))])]
    (if (< (count times) 2)
      {:a 0 :k (val-fn (or (:value (first ak)) a) (or (:value (first bk)) b))}
      (let [stops (map (fn [[t kf]] [t kf (value t)]) (acss/eased-stops value (concat ak bk) times))
            prop  (lottie-stops-prop (cond-> (vec stops) angle? unwrap-angles) identity identity)]
        (cond-> prop
          (zero? (:a prop)) (update :k first))))))

(defn- drop-shadow?
  [shadow]
  (and (= :drop-shadow (:style shadow)) (not (:hidden shadow))))

(defn- lottie-filter-size
  "The region of the filter of a Lottie drop shadow of `shadow` on
  `shape` and the keyframes `kfs` of it (a function of a property), as
  lottie-web takes it (`fs`): around the box of the shape as far as the
  shadow reaches, moved and blurred. Without it lottie-web keeps the
  shadow to the box of the shape."
  [shape shadow kfs]
  (let [{:keys [width height]} (:selrect shape)
        values (fn [prop rest] (cons (or rest 0) (map :value (kfs prop))))
        blur   (* 3 (aexp/blur-sigma (reduce max (values :shadow-blur (:blur shadow)))))
        reach  (fn [prop rest] (+ 2 blur (reduce max (map mth/abs (values prop rest)))))
        pct    (fn [reach size] (* 100.0 (/ reach (max 1 (or size 0)))))
        mx     (pct (reach :shadow-offset-x (:offset-x shadow)) width)
        my     (pct (reach :shadow-offset-y (:offset-y shadow)) height)]
    {:x (str (- mx) "%") :y (str (- my) "%")
     :width (str (+ 100 (* 2 mx)) "%") :height (str (+ 100 (* 2 my)) "%")}))

(defn- lottie-shadow-effects
  "Lottie drop shadows of the drop shadows of `shape`, each with the
  keyframes of its index, as soft as on the canvas in lottie-web (see
  `blur-sigma`), in a region it does not cut (see `lottie-filter-size`).
  Lottie has no spread: a layer whose shadows spread casts them as
  layers of its own (see `lottie-shadow-layers?`,
  `lottie-container-shadows`)."
  [shape by-prop]
  (->> (cta/as-vec (:shadow shape))
       (map-indexed
        (fn [index shadow]
          (when (drop-shadow? shadow)
            (let [kfs      #(slot-kfs by-prop % index)
                  x        (or (:offset-x shadow) 0)
                  y        (or (:offset-y shadow) 0)
                  offset   #(apply lottie-pair-1d (kfs :shadow-offset-x) (kfs :shadow-offset-y) x y %&)]
              {:ty 25 :nm "Drop Shadow" :en 1 :np 8
               :fs (lottie-filter-size shape shadow kfs)
               :ef [{:ty 2 :nm "Shadow Color"
                     :v (lottie-color-prop (kfs :shadow-color)
                                           (hex->rgb01 (or (dm/get-in shadow [:color :color]) "#000000")))}
                    {:ty 0 :nm "Opacity"
                     :v (lottie-prop-1d (kfs :shadow-opacity) (fn [v] (* 255.0 v))
                                        (* 255.0 (or (dm/get-in shadow [:color :opacity]) 1)))}
                    ;; 0 is up, 90 to the right
                    {:ty 0 :nm "Direction" :v (offset (fn [dx dy] (mth/degrees (mth/atan2 dx (- dy)))) :angle? true)}
                    {:ty 0 :nm "Distance" :v (offset mth/hypot)}
                    ;; lottie-web blurs by a quarter of it, as a deviation
                    {:ty 0 :nm "Softness"
                     :v (lottie-prop-1d (kfs :shadow-blur) #(* 4.0 (aexp/blur-sigma %)) (* 4.0 (aexp/blur-sigma (or (:blur shadow) 0))))}
                    {:ty 7 :nm "Shadow Only" :v {:a 0 :k 0}}]}))))
       (remove nil?)
       (vec)))

(defn- lottie-anchor
  "Layer-space point of the track origin on `shape`, before it turns."
  [shape ox oy origin]
  (let [sr (:selrect shape)
        sw (double (or (:width sr) 0))
        sh (double (or (:height sr) 0))
        nx (or (:x origin) 0.5)
        ny (or (:y origin) 0.5)]
    [(+ (- (or (:x sr) 0) ox) (* sw nx))
     (+ (- (or (:y sr) 0) oy) (* sh ny))]))

(defn- lottie-ks
  "The transform of the Lottie layer of `shape`: it turns its picture,
  drawn as it is before it turns, around its pivot (`lottie-anchor`),
  which sits where the pivot of the turned shape is (`lottie-pivot`)."
  [shape track ox oy origin]
  (let [by-prop (group-by :property (:keyframes track))
        pivot   (cta/track-origin track)
        [ax ay] (lottie-anchor shape ox oy pivot)
        [px py] (aexp/lottie-pivot shape ox oy pivot)
        [dx dy] (aexp/pivot-offset shape pivot)
        base-r  (or (:rotation shape) 0)]
    {:o (lottie-prop-1d (by-prop :opacity) (fn [v] (* 100.0 v)) (* 100.0 (or (:opacity shape) 1)))
     :r (lottie-prop-1d (by-prop :rotation) identity base-r)
     :p {:s true
         :x (lottie-prop-1d (by-prop :x) (fn [v] (+ (- (+ (:x origin) v) ox) dx)) px)
         :y (lottie-prop-1d (by-prop :y) (fn [v] (+ (- (+ (:y origin) v) oy) dy)) py)}
     :a {:a 0 :k [ax ay 0]}
     :s (if (lottie-stretched? shape track)
          ;; its width and height stretch it (see `lottie-stretched?`)
          (let [{:keys [width height]} (:selrect shape)
                values (acss/shape-values {:tracks {(:id shape) track}} (:id shape))]
            (lottie-eased-prop (fn [t]
                                 (let [v (values t)]
                                   [(* 100.0 (cta/axis-scale (or width 0) v :width :scale-x))
                                    (* 100.0 (cta/axis-scale (or height 0) v :height :scale-y))
                                    100.0]))
                               (filterv #(#{:width :height :scale-x :scale-y} (:property %)) (:keyframes track))
                               identity identity))
          (lottie-scale-prop (by-prop :scale-x) (by-prop :scale-y)))}))

(defn- lottie-holder-ks
  "The transform of a precomp layer holding `shape` drawn as it is,
  turned, and the layers in it where they are: it turns them only as
  much as the animation turns the shape, and moves and scales them,
  around its pivot as it is (see `lottie-pivot`)."
  [shape track ox oy origin]
  (let [by-prop (group-by :property (:keyframes track))
        [px py] (aexp/lottie-pivot shape ox oy (cta/track-origin track))
        base-r  (or (:rotation shape) 0)]
    (assoc (lottie-ks shape track ox oy origin)
           :r (lottie-prop-1d (by-prop :rotation) #(- % base-r) 0)
           :a {:a 0 :k [px py 0]})))

(defn- shape->lottie-image-layer
  "An image layer of `shape`, whose image is rasterized opaque: the layer
  has its opacity. The image is the shape as it is, turned, over the box
  `asset` gives (`:x :y` in the board, the box of the shape when not
  there): the anchor is the pivot on it, and the layer turns it only as
  much as the animation turns the shape."
  [ind shape track ox oy origin asset duration]
  {:ddd 0
   :ind (inc ind)
   :ty 2
   :nm (or (:name shape) (str "layer-" (inc ind)))
   :refId (:id asset)
   :sr 1
   :ks (let [by-prop (group-by :property (:keyframes track))
             pivot   (cta/track-origin track)
             [px py] (aexp/lottie-pivot shape ox oy pivot)
             sr      (:selrect shape)
             ix      (or (:x asset) (- (or (:x sr) 0) ox))
             iy      (or (:y asset) (- (or (:y sr) 0) oy))
             base-r  (or (:rotation shape) 0)]
         (assoc (lottie-ks shape track ox oy origin)
                :r (lottie-prop-1d (by-prop :rotation) #(- % base-r) 0)
                :a {:a 0 :k [(- px ix) (- py iy) 0]}))
   :ao 0
   :ip 0
   :op (ms->frames (max 1 duration))
   :st 0
   :bm 0})

(def ^:private lottie-group-transform
  {:ty "tr" :nm "transform"
   :p {:a 0 :k [0 0]}
   :a {:a 0 :k [0 0]}
   :s {:a 0 :k [100 100]}
   :r {:a 0 :k 0}
   :o {:a 0 :k 100}})

(defn- lottie-group
  [name items]
  (let [items (conj (vec items) lottie-group-transform)]
    {:ty "gr" :nm name :np (count items) :it items}))

(defn- lottie-paints?
  "Whether the fills and strokes of `shape` are Lottie paints: flat or
  gradient fills, flat strokes (a hidden one draws nothing)."
  [shape]
  (and (every? #(or (cta/solid-color-fill? %) (some? (:fill-color-gradient %)))
               (cta/as-vec (:fills shape)))
       (every? #(or (:hidden %) (cta/solid-color-stroke? %)) (cta/as-vec (:strokes shape)))))

(declare lottie-content?)

(defn- lottie-outline?
  "Whether the kind of `shape` draws as Lottie vectors, the layers of a
  group or board as `lottie-content?` says, but for those in `layer-ids`,
  which draw on their own."
  [objects layer-ids shape]
  (case (:type shape)
    (:rect :circle) true
    (:path :bool)   (some? (:content shape))
    ;; a masked group is a precomp of its layers (see
    ;; `lottie-masked-layers`); a hidden one is none, as it draws nothing
    ;; (see `aexp/export-layer-ids`)
    :group          (if (:masked-group shape)
                      (every? #(or (contains? layer-ids %) (true? (dm/get-in objects [% :hidden])))
                              (:shapes shape))
                      (every? #(or (contains? layer-ids %)
                                   (lottie-content? objects layer-ids (get objects %)))
                              (:shapes shape)))
    :frame          (every? #(or (contains? layer-ids %)
                                 (lottie-content? objects layer-ids (get objects %)))
                            (:shapes shape))
    false))

(defn- lottie-content?
  "Whether `shape`, drawn inside a layer, draws as Lottie vectors (a hidden
  one draws nothing). Effects go with a whole layer, so no shadow nor blur,
  no board clipping its content and no mask."
  [objects layer-ids shape]
  (or (true? (:hidden shape))
      (and (some? shape)
           (not (:masked-group shape))
           (lottie-paints? shape)
           (every? :hidden (cta/as-vec (:shadow shape)))
           (or (nil? (:blur shape)) (:hidden (:blur shape)))
           (or (not= :frame (:type shape)) (true? (:show-content shape)))
           (lottie-outline? objects layer-ids shape))))

(def ^:private lottie-leaf-types
  "The kinds of layers whose shadows Lottie can cast as layers of their
  own (see `lottie-shadow-layers?`): what has an outline of its own."
  #{:rect :circle :path :bool})

(defn lottie-vector-layer?
  "Whether the layer `id` of a Lottie export draws as vectors (`layer-ids`
  are all the layers of the export): a rectangle, ellipse, path or
  boolean, or a group or board of such, with flat or gradient fills, flat
  strokes, and shadows and blur of its own at most (see
  `lottie-shadow-layers?`, `lottie-inner-shadow-mattes`), or a masked
  group of layers (see `lottie-masked-layers`). Anything else draws as
  an image (see `timeline->lottie`)."
  [objects layer-ids id]
  (let [shape (get objects id)]
    (and (some? shape)
         (lottie-paints? shape)
         (lottie-outline? objects layer-ids shape))))

(defn- lottie-path-shapes
  "Lottie paths (`sh`) of path `content`, one per subpath, moved by
  `dx`, `dy`. Tangents are relative to their vertex. A subpath back on
  its first vertex closes on it, unless not `fold?` (outlines that morph
  keep their vertices, as many in each)."
  [content dx dy & {:keys [fold?] :or {fold? true}}]
  (let [point  (fn [x y] [(+ x dx) (+ y dy)])
        vertex (fn [[x y]] {:v [x y] :i [0 0] :o [0 0]})
        item   (fn [vertices closed?]
                 (let [first-v  (first vertices)
                       last-v   (peek vertices)
                       back?    (and fold?
                                     (> (count vertices) 2)
                                     (mth/close? ((:v first-v) 0) ((:v last-v) 0))
                                     (mth/close? ((:v first-v) 1) ((:v last-v) 1)))
                       ;; an outline back on its first vertex closes on it,
                       ;; as the ones `path/convert-to-path` gives do
                       closed?  (or closed? back?)
                       vertices (if back?
                                  (assoc (pop vertices) 0 (assoc first-v :i (:i last-v)))
                                  vertices)]
                   {:ty "sh" :nm "path"
                    :ks {:a 0 :k {:c closed?
                                  :v (mapv :v vertices)
                                  :i (mapv :i vertices)
                                  :o (mapv :o vertices)}}}))
        done   (fn [items vertices closed?]
                 (cond-> items (> (count vertices) 1) (conj (item vertices closed?))))]
    (loop [segments (seq (vec content))
           vertices []
           items    []]
      (if-let [{:keys [command params]} (first segments)]
        (let [{:keys [x y c1x c1y c2x c2y]} params
              p (when (some? x) (point x y))]
          (case command
            :move-to
            (recur (rest segments) [(vertex p)] (done items vertices false))

            :line-to
            (recur (rest segments) (conj vertices (vertex p)) items)

            :curve-to
            (let [[px py] (:v (or (peek vertices) (vertex p)))
                  [ax ay] (point c1x c1y)
                  [bx by] (point c2x c2y)
                  prev    (if (seq vertices)
                            (assoc-in vertices [(dec (count vertices)) :o] [(- ax px) (- ay py)])
                            [(vertex [px py])])]
              (recur (rest segments)
                     (conj prev {:v p :i [(- bx (p 0)) (- by (p 1))] :o [0 0]})
                     items))

            :close-path
            ;; what follows without a move starts where this one did
            (recur (rest segments)
                   (if (seq vertices) [(vertex (:v (first vertices)))] [])
                   (done items vertices true))

            (recur (rest segments) vertices items)))
        (done items vertices false)))))

(defn- lottie-morph-paths
  "The Lottie paths (`sh`) of the outlines the keyframes `kfs` of a path
  play (see `cta/match-outlines`; their values its contents where it
  rests), turned back by `turn-back` and in the board at `ox`, `oy`: one
  per subpath, each playing its vertices, eased as the canvas eases
  them (see `eased-stops`)."
  [kfs turn-back ox oy]
  (let [kfs     (cta/sort-keyframes kfs)
        items   (fn [t]
                  (lottie-path-shapes (turn-back (cta/property-value-at kfs t)) (- ox) (- oy) :fold? false))
        numbers (fn [t]
                  (into [] (mapcat (fn [item]
                                     (let [{:keys [v i o]} (get-in item [:ks :k])]
                                       (flatten [v i o]))))
                        (items t)))
        times   (vec (into (sorted-set) (map :time) kfs))
        stops   (mapv (fn [[t kf]] [t kf (items t)]) (acss/eased-stops numbers kfs times))]
    (mapv (fn [index]
            {:ty "sh" :nm "path"
             :ks (lottie-stops-prop (map (fn [[t kf its]] [t kf (get-in its [index :ks :k])]) stops)
                                    vector
                                    first)})
          (range (count (nth (first stops) 2))))))

(defn- lottie-outline
  "The outline of `shape` as Lottie items, in board coordinates (the
  board at `ox`, `oy`) and before it turns: the layer turns it. An
  ellipse and a rectangle with one radius take their radius from the
  keyframes and their size from `sized` (see `lottie-size-props`), as
  the canvas resizes them; without, the size they have (their layer
  stretches, see `lottie-stretched?`). A rectangle whose corners play,
  not alike, is a path of them (see `lottie-corners-path`)."
  [shape by-prop ox oy sized]
  (let [kfs    (into [] cat (vals by-prop))
        {:keys [x y width height]} (:selrect shape)
        left   (- x ox)
        top    (- y oy)
        size   (or (:size sized) {:a 0 :k [width height]})
        center (or (:center sized) {:a 0 :k [(+ left (/ width 2)) (+ top (/ height 2))]})
        rotation (or (:rotation shape) 0)]
    (cond
      (= :circle (:type shape))
      [{:ty "el" :nm "ellipse" :d 1 :s size :p center}]

      (and (contains? #{:rect :frame} (:type shape))
           (aexp/svg-even-corners? shape {:keyframes kfs}))
      [{:ty "rc" :nm "rect" :d 1 :s size :p center
        :r (lottie-prop-1d (by-prop :r1) identity (or (:r1 shape) 0))}]

      (lottie-corners-play? shape kfs)
      [(lottie-corners-path shape kfs ox oy)]

      (contains? #{:rect :frame} (:type shape))
      (-> (assoc shape :type :rect)
          (dissoc :transform :transform-inverse :flip-x :flip-y)
          (path/convert-to-path)
          (:content)
          (lottie-path-shapes (- ox) (- oy)))

      :else
      (let [turn-back #(cond-> %
                         (not (mth/almost-zero? rotation))
                         (path/transform-content (gmt/rotate-matrix (- rotation)
                                                                    (gpt/point (+ x (/ width 2)) (+ y (/ height 2))))))]
        ;; a path that morphs plays its outlines (see `lottie-track`)
        (if (and (= :path (:type shape)) (seq (by-prop :content)))
          (lottie-morph-paths (by-prop :content) turn-back ox oy)
          (lottie-path-shapes (turn-back (:content shape)) (- ox) (- oy)))))))

(defn- lottie-vector-groups
  "The Lottie groups drawing `shape` as vectors: its outline (resized as
  `sized` says, see `lottie-outline`) with its strokes over its fills. A
  trim trims the strokes only, as the canvas does: they get a trimmed
  copy of the outline, the fills, when it has some, one of their own."
  [shape by-prop ox oy & [sized]]
  (let [{:keys [x y width height]} (:selrect shape)
        outline (lottie-outline shape by-prop ox oy sized)
        strokes (lottie-stroke-items shape by-prop)
        fills   (lottie-fill-items shape by-prop [(- x ox) (- y oy) width height])
        trim    (lottie-trim-item by-prop (count (filter #(= "sh" (:ty %)) outline)))]
    (if (and (some? trim) (seq strokes))
      (cond-> [(lottie-group "strokes" (concat outline [trim] strokes))]
        (seq fills) (conj (lottie-group "fills" (concat outline fills))))
      [(lottie-group "shape" (concat outline strokes fills))])))

(defn- lottie-turned-back
  "`content`, the outline of a shape where it is on the canvas, turned
  back by `rotation` around `center` (the layer turns it) and as Lottie
  paths in the board at `ox`, `oy`."
  [content rotation center ox oy]
  (-> (cond-> content
        (not (mth/almost-zero? rotation))
        (path/transform-content (gmt/rotate-matrix (- rotation) center)))
      (lottie-path-shapes (- ox) (- oy))))

(defn- lottie-content-group
  "A Lottie group drawing `shape`, a layer of a group or board drawn in
  one layer that turns by `rotation` around `center`: its outline turned
  back with its strokes over its fills or, for a group or board, the ones
  of its layers (but for those in `layer-ids`, which draw on their own),
  the top-most first, over the board's own. None for a hidden one."
  [objects layer-ids shape rotation center ox oy]
  (when-not (true? (:hidden shape))
    (let [children (when (contains? #{:group :frame} (:type shape))
                     (->> (rseq (cta/painted-children objects (:id shape)))
                          (remove #(contains? layer-ids %))
                          (keep #(lottie-content-group objects layer-ids (get objects %)
                                                       rotation center ox oy))))
          {:keys [x y width height]} (:selrect shape)
          paint    (when (not= :group (:type shape))
                     (concat (lottie-stroke-items shape {})
                             (lottie-fill-items shape {} [(- x ox) (- y oy) width height])))
          items    (cond-> (vec children)
                     (seq paint)
                     (conj (lottie-group "shape" (concat (lottie-turned-back (aexp/canvas-outline shape)
                                                                             rotation center ox oy)
                                                         paint))))]
      (when (seq items)
        (-> (lottie-group (or (:name shape) "layer") items)
            (update :it (fn [it]
                          (assoc-in it [(dec (count it)) :o :k] (* 100.0 (or (:opacity shape) 1))))))))))

(defn- lottie-clip-mask
  "A Lottie mask keeping a layer to the outline of the board `shape`,
  turned back by `rotation`, as much as the layer turns it, resized as
  the width and height `track` plays on it when it does (see
  `lottie-sized?`)."
  [shape ox oy rotation & [track]]
  (let [{:keys [x y width height]} (:selrect shape)
        center (gpt/point (+ x (/ width 2)) (+ y (/ height 2)))
        path   (first (lottie-turned-back (aexp/canvas-outline shape) rotation center ox oy))]
    {:inv false :mode "a" :nm "clip"
     :pt (cond
           (and (some? track) (lottie-sized? shape track))
           (lottie-eased-prop (lottie-box shape track ox oy)
                              (filterv #(#{:width :height :r1} (:property %)) (:keyframes track))
                              #(vector (lottie-rect-shape %))
                              first)

           ;; corners not alike that play, where it is unturned
           (and (some? track)
                (lottie-corners-play? shape (:keyframes track))
                (or (aexp/unturned? shape) (mth/close? rotation (or (:rotation shape) 0))))
           (:ks (lottie-corners-path shape (:keyframes track) ox oy))

           :else
           (:ks path))
     :o {:a 0 :k 100} :x {:a 0 :k 0}}))

(defn- lottie-layer-groups
  "The Lottie groups drawing the layer `shape` as vectors: its own shape
  with the keyframes of its track (see `lottie-vector-groups`, `sized`
  its size) or, for a group, its layers, and for a board, its layers
  over its own shape."
  [objects layer-ids shape by-prop ox oy & [sized]]
  (let [{:keys [x y width height]} (:selrect shape)
        center   (gpt/point (+ x (/ width 2)) (+ y (/ height 2)))
        rotation (or (:rotation shape) 0)
        children #(->> (rseq (cta/painted-children objects (:id shape)))
                       (remove (fn [id] (contains? layer-ids id)))
                       (keep (fn [id] (lottie-content-group objects layer-ids (get objects id)
                                                            rotation center ox oy))))]
    (case (:type shape)
      :group (vec (children))
      :frame (into (vec (children)) (lottie-vector-groups shape by-prop ox oy sized))
      (lottie-vector-groups shape by-prop ox oy sized))))

(defn- lottie-placeholder-group
  "A rectangle of the bounds of `shape` in its first fill colour, for a
  shape drawn neither as vectors nor as an image."
  [shape by-prop ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        fill (d/seek cta/solid-color-fill? (cta/as-vec (:fills shape)))]
    (lottie-group
     "shape"
     (concat [{:ty "rc" :d 1 :nm "rect"
               :s {:a 0 :k [width height]}
               :p {:a 0 :k [(+ (- x ox) (/ width 2.0)) (+ (- y oy) (/ height 2.0))]}
               :r {:a 0 :k 0}}]
             (lottie-stroke-items shape by-prop)
             [{:ty "fl" :nm "fill" :r 1
               :c (lottie-color-prop (slot-kfs by-prop :fill-color 0)
                                     (hex->rgb01 (or (:fill-color fill)
                                                     (-> shape :fills first :fill-color))))
               :o (lottie-prop-1d (slot-kfs by-prop :fill-opacity 0)
                                  (fn [v] (* 100.0 v))
                                  (* 100.0 (or (:fill-opacity fill) 1)))}]))))

(defn- shape->lottie-layer
  "Build a Lottie shape layer for `shape`/`track`, as vectors when it can
  be (see `lottie-vector-layer?`, `layer-ids` all the layers), with its
  drop shadows and blur as effects and a board kept to its outline.
  `ox`/`oy` is the board origin so coordinates are relative to the
  composition; keyframe positions are relative to `origin`. Without
  `effects?` or `mask?`, those are left to the precomp holding it."
  [ind objects layer-ids shape track ox oy origin & {:keys [effects? mask?] :or {effects? true mask? true}}]
  (let [by-prop (group-by :property (:keyframes track))
        vector? (lottie-vector-layer? objects layer-ids (:id shape))
        sized   (when (lottie-sized? shape track) (lottie-size-props shape track ox oy))
        effects (when effects?
                  (concat (when vector? (lottie-shadow-effects shape by-prop))
                          (lottie-blur-effect shape by-prop)))]
    (cond-> {:ddd 0
             :ind (inc ind)
             :ty 4
             :nm (or (:name shape) (str "layer-" (inc ind)))
             :sr 1
             :ks (lottie-ks shape track ox oy origin)
             :ao 0
             :shapes (if vector?
                       (lottie-layer-groups objects layer-ids shape by-prop ox oy sized)
                       [(lottie-placeholder-group shape by-prop ox oy)])
             :ip 0
             :op (ms->frames (max 1 (:duration track 0)))
             :st 0
             :bm 0}
      (seq effects)
      (assoc :ef (vec effects))

      (and mask? vector? (= :frame (:type shape)) (not (true? (:show-content shape))))
      (assoc :hasMask true :masksProperties [(lottie-clip-mask shape ox oy (or (:rotation shape) 0) track)]))))

(defn- lottie-markers
  "The Lottie markers of the markers of `timeline`: each names the stretch
  up to the next one, or to the end, so a player plays it by its name."
  [timeline]
  (let [markers (sort-by :time (:markers timeline))
        ends    (concat (map :time (rest markers)) [(:duration timeline)])]
    (mapv (fn [{:keys [time name]} end]
            {:tm (ms->frames time)
             :cm name
             :dr (ms->frames (max 0 (- end time)))})
          markers
          ends)))

(defn- lottie-document
  [timeline objects layers assets]
  (let [board  (get objects (:board-id timeline))
        bsr    (:selrect board)
        width  (or (:width bsr) 100)
        height (or (:height bsr) 100)]
    {:v "5.7.0"
     :fr lottie-fps
     :ip 0
     :op (ms->frames (max 1 (:duration timeline)))
     :w (mth/round width)
     :h (mth/round height)
     :nm (or (:name timeline) "Penpot Animation")
     ;; 3D when a layer tilts (see `lottie-tilted-layers`)
     :ddd (if (some #(= 1 (:ddd %)) (concat layers (mapcat :layers assets))) 1 0)
     :assets (vec assets)
     :layers (vec layers)}))

(def ^:private lottie-layer-properties
  "What a layer does as a whole: a precomp does it to all its layers."
  #{:x :y :rotation :scale-x :scale-y :opacity})

(defn- lottie-track
  "The track of `id` in the timeline of `ctx`, its outlines (see
  `ctao/outline->content`) the contents they draw where its path rests."
  [{:keys [timeline duration objects]} id]
  (let [track (or (get-in timeline [:tracks id]) {:shape-id id :keyframes []})
        shape (get objects id)]
    (cond-> (assoc track :duration duration)
      (and (= :path (:type shape)) (some #(= :content (:property %)) (:keyframes track)))
      (update :keyframes (partial mapv #(cond-> %
                                          (= :content (:property %))
                                          (update :value (fn [v] (ctao/outline->content shape track v)))))))))

(defn- lottie-numbered
  "`layers` numbered (`:ind`), each that follows the board it is in
  (`::follow`) a child of the null layer that moves it (`::null`, see
  `lottie-follow-null`), a tilted one (`::tilt-parent`) of the null that
  turns it in the plane (`::tilt-null`, see `lottie-tilted-layers`)."
  [layers]
  (let [numbered (into [] (map-indexed (fn [i layer] (assoc layer :ind (inc i)))) layers)
        nulls    (into {} (keep #(when-let [id (::null %)] [id (:ind %)])) numbered)
        turns    (into {} (keep #(when-let [id (::tilt-null %)] [id (:ind %)])) numbered)]
    (mapv (fn [layer]
            (cond-> (dissoc layer ::null ::follow ::tilt-null ::tilt-parent)
              (contains? nulls (::follow layer)) (assoc :parent (get nulls (::follow layer)))
              (contains? turns (::tilt-parent layer)) (assoc :parent (get turns (::tilt-parent layer)))))
          numbered)))

(defn- lottie-follow-null
  "A null layer that moves the layer of `shape` with the board it is in
  as it resizes, by the matrices of `stops` (see `board-follow`): its
  layer a child of it (see `lottie-numbered`)."
  [{:keys [duration]} shape stops]
  {:ddd 0
   :ty 3
   :nm (str (or (:name shape) (:id shape)) " follows")
   :sr 1
   :ks {:o {:a 0 :k 100}
        :r {:a 0 :k 0}
        :p (lottie-stops-prop stops (fn [[_ _ _ _ e f]] [e f 0]) identity)
        :a {:a 0 :k [0 0 0]}
        :s (lottie-stops-prop stops (fn [[a _ _ d]] [(* 100.0 a) (* 100.0 d) 100]) identity)}
   :ao 0
   :ip 0
   :op (ms->frames duration)
   :st 0
   :bm 0
   ::null (:id shape)})

(defn- lottie-shape-layer
  "A layer drawing `shape` with `track`: as vectors when it can be, else
  as its image or a placeholder (see `shape->lottie-layer`)."
  [{:keys [objects layer-ids assets timeline duration ox oy]} shape track
   & {:keys [effects? mask?] :or {effects? true mask? true}}]
  (let [id     (:id shape)
        origin (cta/position-origin timeline objects id)
        asset  (get assets id)]
    (if (and (some? asset) (not (lottie-vector-layer? objects layer-ids id)))
      (shape->lottie-image-layer 0 shape track ox oy origin asset duration)
      (shape->lottie-layer 0 objects layer-ids shape track ox oy origin
                           :effects? effects? :mask? mask?))))

(defn- lottie-content-track
  "The track of the drawing of `shape` in the precomp (or the
  composition) that moves, fades and clips it: but what that does."
  [ctx shape]
  (let [track (lottie-track ctx (:id shape))
        ;; its holder stretches it, else it is resized as it plays (see
        ;; `lottie-sized?`)
        drop  (cond-> lottie-layer-properties
                (not (lottie-sized? shape track)) (into #{:width :height}))]
    (update track :keyframes #(into [] (remove (comp drop :property)) %))))

(defn- lottie-content-layer
  "The layer drawing `shape` itself and the shapes inside it that are no
  layers, for the precomp (or the composition) that moves, fades and
  clips it."
  [ctx shape]
  (lottie-shape-layer ctx (assoc shape :opacity 1) (lottie-content-track ctx shape)
                      :effects? false :mask? false))

(defn lottie-shadow-layers?
  "Whether a Lottie export casts the shadows of the layer `shape`, which
  `track` animates, as layers of their own, around its drawing in a
  precomp (see `lottie-effects-layer`): a rectangle, ellipse, path or
  boolean with an inner shadow, for which Lottie has no effect, or a
  drop shadow that spreads, out or in, which its effect does not."
  [shape track]
  (boolean
   (and (contains? lottie-leaf-types (:type shape))
        (let [shown (into [] (remove :hidden) (cta/as-vec (:shadow shape)))]
          (or (some #(= :inner-shadow (:style %)) shown)
              (some #(not (zero? (or (:spread %) 0))) shown)
              (and (seq shown)
                   (some #(and (= :shadow-spread (:property %)) (not (zero? (or (:value %) 0))))
                         (:keyframes track))))))))

(defn- stroke-reach
  "How far the strokes of `shape` reach out of its outline."
  [shape]
  (transduce (map (fn [stroke]
                    (case (aexp/svg-stroke-alignment shape stroke)
                      :outer  (or (:stroke-width stroke) 0)
                      :inner  0
                      (/ (or (:stroke-width stroke) 0) 2))))
             max 0
             (remove :hidden (cta/as-vec (:strokes shape)))))

(def ^:private lottie-identity-ks
  {:o {:a 0 :k 100} :r {:a 0 :k 0} :p {:a 0 :k [0 0 0]} :a {:a 0 :k [0 0 0]} :s {:a 0 :k [100 100 100]}})

(defn- add-comp!
  "Add the precomp `comp` to `comps`, its id: once its layers are made,
  which add the precomps inside it first, as a function evaluates its
  arguments before (`vswap!` would read `comps` before them)."
  [comps comp]
  (vswap! comps conj comp)
  (:id comp))

(defn- lottie-shadow-paint
  "The colour and opacity (`:c :o`) of the paint of a shadow `shadow` of
  the keyframes `kfs` (a function of a property)."
  [shadow kfs]
  {:c (lottie-color-prop (kfs :shadow-color) (hex->rgb01 (or (dm/get-in shadow [:color :color]) "#000000")))
   :o (lottie-prop-1d (kfs :shadow-opacity) (fn [v] (* 100.0 v))
                      (* 100.0 (or (dm/get-in shadow [:color :opacity]) 1)))})

(defn- lottie-moved-group
  "A Lottie group of `items` that `offset` (a 2D property) moves."
  [name items offset]
  (update (lottie-group name items) :it
          (fn [it] (assoc-in it [(dec (count it)) :p] offset))))

(defn- lottie-shadow-layer
  "A layer of the shadow `index` of the drawing of `shape` in the precomp
  of its effects (see `lottie-effects-layer`), with the transform `ks` of
  that drawing, its outline `outline` (see `lottie-outline`) and the
  keyframes `by-prop`, moved by its offset and blurred as soft as on the
  canvas (see `lottie-gaussian-blur`). A drop shadow casts its silhouette
  (its outline, filled when it has fills, and as far as its strokes
  reach), grown by its spread; an inner shadow is what lies outside the
  outline, grown in by its spread, under a matte of the outline (the
  layer before it, see `lottie-matte-layer`), which keeps it inside.
  A drop shadow that spreads in is a precomp of its silhouette cut by an
  inverted matte of the band its spread takes off along the outline,
  which it blurs once cut, as the canvas erodes it before it blurs (its
  precomp goes to `comps`). The board is at `ox`, `oy`, `width` by
  `height`."
  [{:keys [comps width height]} shape by-prop outline ks ox oy duration index shadow]
  (let [kfs     #(slot-kfs by-prop % index)
        ;; opaque: its fill and spread overlap; the layer has its opacity
        paint   (assoc (lottie-shadow-paint shadow kfs) :o {:a 0 :k 100})
        opacity (:o (lottie-shadow-paint shadow kfs))
        inner?  (= :inner-shadow (:style shadow))
        reach   (if inner? 0 (stroke-reach shape))
        spreads (cons (or (:spread shadow) 0) (map :value (kfs :shadow-spread)))
        fills?  (seq (remove :hidden (cta/as-vec (:fills shape))))
        grows?  (some #(pos? (+ reach %)) spreads)
        erodes? (and (not inner?) fills? (some #(neg? (+ reach %)) spreads))
        grow    #(* 2.0 (max 0 (+ reach %)))
        band    #(* 2.0 (max 0 (- (+ reach %))))
        offset  (lottie-pair-prop (kfs :shadow-offset-x) (kfs :shadow-offset-y)
                                  (or (:offset-x shadow) 0) (or (:offset-y shadow) 0)
                                  (fn [x y] [x y]))
        spread  (when grows?
                  (lottie-group "spread"
                                (concat outline
                                        ;; mitred, as the canvas strokes it
                                        [(merge {:ty "st" :nm "spread" :lc 1 :lj 1 :ml 4
                                                 :w (lottie-prop-1d (kfs :shadow-spread) grow
                                                                    (grow (or (:spread shadow) 0)))}
                                                paint)])))
        cast    (if inner?
                  ;; around the outline, as far as it may show moved and blurred
                  (let [{:keys [x y width height]} (:selrect shape)
                        far    (+ 20
                                  (* 3 (aexp/blur-sigma (reduce max (or (:blur shadow) 0) (map :value (kfs :shadow-blur)))))
                                  (reduce max 0 (map mth/abs (concat [(or (:offset-x shadow) 0) (or (:offset-y shadow) 0)]
                                                                     (map :value (kfs :shadow-offset-x))
                                                                     (map :value (kfs :shadow-offset-y)))))
                                  (reduce max 0 spreads))]
                    (lottie-group "outside"
                                  (concat [{:ty "rc" :nm "around" :d 1
                                            :s {:a 0 :k [(+ width (* 2 far)) (+ height (* 2 far))]}
                                            :p {:a 0 :k [(+ (- x ox) (/ width 2)) (+ (- y oy) (/ height 2))]}
                                            :r {:a 0 :k 0}}]
                                          outline
                                          [(merge {:ty "fl" :nm "outside" :r 2} paint)])))
                  (when fills?
                    (lottie-group "silhouette" (concat outline [(merge {:ty "fl" :nm "fill" :r (fill-rule shape)} paint)]))))
        name    (str (or (:name shape) "layer") (if inner? " inner shadow " " drop shadow ") (inc index))
        blur    (lottie-gaussian-blur (kfs :shadow-blur) (or (:blur shadow) 0))
        layer   (cond-> {:ddd 0
                         :ty 4
                         :nm name
                         :sr 1
                         :ks (assoc ks :o opacity)
                         :ao 0
                         :shapes [(lottie-moved-group "shadow" (keep identity [spread cast]) offset)]
                         :ip 0
                         :op (ms->frames duration)
                         :st 0
                         :bm 0}
                  (not erodes?) (assoc :ef [blur])
                  inner? (assoc :tt 1))]
    (if-not erodes?
      (assoc layer ::effect true)
      (let [ref  (str "shadow_" (:id shape) "_" index)
            cut  {:ddd 0 :ty 4 :nm (str name " spread") :sr 1 :ks ks :ao 0 :td 1
                  :shapes [(lottie-moved-group
                            "spread"
                            [(lottie-group "band"
                                           (concat outline
                                                   [{:ty "st" :nm "band" :lc 2 :lj 2 :ml 4
                                                     :c {:a 0 :k [0 0 0 1]} :o {:a 0 :k 100}
                                                     :w (lottie-prop-1d (kfs :shadow-spread) band
                                                                        (band (or (:spread shadow) 0)))}]))]
                            offset)]
                  :ip 0 :op (ms->frames duration) :st 0 :bm 0}]
        (add-comp! comps {:id ref :nm name :layers (lottie-numbered [cut (assoc layer :tt 2)])})
        {:ddd 0 :ty 0 :nm name :refId ref :sr 1
         :ks lottie-identity-ks
         :ao 0 :w width :h height
         :ef [blur]
         :ip 0 :op (ms->frames duration) :st 0 :bm 0
         ::effect true}))))

(defn- lottie-matte-layer
  "The alpha matte (`td`) of the outline `outline` of `shape`, with the
  transform `ks`, which keeps the layer after it inside (`tt`)."
  [shape outline ks duration]
  {:ddd 0
   :ty 4
   :nm (str (or (:name shape) "layer") " outline")
   :sr 1
   :ks ks
   :ao 0
   :td 1
   :shapes [(lottie-group "outline" (concat outline [{:ty "fl" :nm "matte" :r (fill-rule shape)
                                                      :c {:a 0 :k [0 0 0 1]} :o {:a 0 :k 100}}]))]
   :ip 0
   :op (ms->frames duration)
   :st 0
   :bm 0
   ::effect true})

(def ^:private lottie-outline-types
  "The kinds of Lottie items that are outlines (or trim them), which the
  paints after them in their group draw."
  #{"sh" "rc" "el" "sr" "tm"})

(defn- lottie-rest-1d
  "The value of the 1D Lottie property `prop` at rest: its first
  keyframe when it plays."
  [prop]
  (if (= 1 (:a prop))
    (let [s (:s (first (:k prop)))] (if (sequential? s) (first s) s))
    (:k prop)))

(defn- lottie-silhouette-group
  "The Lottie group `group` as a silhouette in `paint`, grown by `spread`
  (`{:rest :kfs}`, its value and keyframes), as `lottie-shadow-layer`
  grows a shape: each group of outlines filled when it has fills, with a
  stroke as wide as its strokes reach and the spread. With `cut?`, the
  band a spread that erodes takes off its fills instead, nil where none
  does."
  [group paint {:keys [rest kfs] :as spread} cut?]
  (let [items (:it group)
        tr    (d/seek #(= "tr" (:ty %)) items)]
    (if-not (some #(contains? lottie-outline-types (:ty %)) items)
      (let [kids (into []
                       (keep #(if (= "gr" (:ty %)) (lottie-silhouette-group % paint spread cut?) %))
                       items)]
        (when (or (not cut?) (some #(= "gr" (:ty %)) kids))
          (assoc group :it kids :np (count kids))))
      (let [fill    (d/seek #(contains? #{"fl" "gf"} (:ty %)) items)
            strokes (filter #(contains? #{"st" "gs"} (:ty %)) items)
            reach   (reduce max 0 (map #(/ (or (lottie-rest-1d (:w %)) 0) 2) strokes))
            spreads (cons rest (map :value kfs))
            grow    #(* 2.0 (max 0 (+ reach %)))
            band    #(* 2.0 (max 0 (- (+ reach %))))
            stroke  (fn [width]
                      ;; mitred, as the canvas strokes it
                      (cond-> (merge {:ty "st" :nm "spread" :lc 1 :lj 1 :ml 4
                                      :w (lottie-prop-1d kfs width (width rest))}
                                     paint)
                        ;; a dashed outline with no fill casts dashes
                        (and (nil? fill) (some? (:d (first strokes))))
                        (assoc :d (:d (first strokes)))))
            outline (filterv #(contains? lottie-outline-types (:ty %)) items)
            it      (if cut?
                      (when (and (some? fill) (some #(neg? (+ reach %)) spreads))
                        (conj outline (stroke band)))
                      (cond-> outline
                        (some? fill)
                        (conj (merge {:ty "fl" :nm "fill" :r (or (:r fill) 1)} paint))

                        (some #(pos? (+ reach %)) spreads)
                        (conj (stroke grow))))]
        (when (some? it)
          (let [it (cond-> it (some? tr) (conj tr))]
            (assoc group :it it :np (count it))))))))

(defn- lottie-outline-groups
  "The Lottie groups `items` once for each group of outlines in them, in
  the groups around it (and their transforms), each alone."
  [items]
  (into []
        (mapcat (fn [item]
                  (cond
                    (not= "gr" (:ty item)) nil

                    (some #(contains? lottie-outline-types (:ty %)) (:it item)) [item]

                    :else
                    (let [others (filterv #(not= "gr" (:ty %)) (:it item))]
                      (map (fn [kid] (assoc item :it (into [kid] others) :np (inc (count others))))
                           (lottie-outline-groups (:it item)))))))
        items))

(defn- lottie-silhouette-shape-layer
  "The shape layer `layer` as a silhouette (see `lottie-silhouette-group`):
  one layer, and, for each of its groups of outlines a spread erodes, a
  layer of it alone under an inverted matte (`td`, `tt` 2) of the band
  that spread takes off it, so that it does not cut the others, as the
  canvas erodes each shape; but for a matte or a layer kept to one,
  which only grows."
  [layer paint spread]
  (let [plain  (dissoc layer :ef)
        grown  #(into [] (keep (fn [group] (lottie-silhouette-group group paint spread false))) %)
        groups (lottie-outline-groups (:shapes layer))
        cut?   #(some? (lottie-silhouette-group % paint spread true))]
    (if (or (some? (:tt layer)) (some? (:td layer)) (not-any? cut? groups))
      [(assoc plain :shapes (grown (:shapes layer)))]
      (into [(assoc plain :shapes (grown (remove cut? groups)))]
            (mapcat (fn [group]
                      [(-> plain
                           (dissoc :hasMask :masksProperties)
                           (assoc :shapes [(lottie-silhouette-group group paint spread true)]
                                  :td 1 :nm (str (:nm layer) " spread") ::extra true))
                       (assoc plain :shapes (grown [group]) :tt 2 ::extra true)]))
            (filter cut? groups)))))

(declare lottie-silhouette-comp)

(defn- lottie-silhouette
  "The layers `layers` of a precomp as the silhouette a shadow of what
  they draw casts, in `paint` (opaque) and grown or eroded by `spread`
  (`{:rest :kfs}`): their vectors as `lottie-silhouette-group` grows
  them, the pictures in `paint` where they are opaque (an alpha matte
  over its colour), the precomps as silhouettes of theirs too (under ids
  of `key`, see `lottie-silhouette-comp`), as they move and fade. The
  shadows and blur of the layers are left out, as the canvas casts a
  shadow of what is plain (the layers of effects, `::effect`). The
  matte of a mask grows too, as the canvas spreads what a mask keeps,
  past the mask (an approximation where the two outlines cross); a
  picture that is one keeps its alpha."
  [{:keys [width height] :as ctx} layers paint spread key]
  (let [convert (fn [layer]
                  (cond
                    (::effect layer)  []
                    (and (some? (:td layer)) (not (contains? #{0 4} (:ty layer))))
                    [(dissoc layer :ef)]
                    (= 0 (:ty layer)) [(-> (dissoc layer :ef)
                                           (assoc :refId (lottie-silhouette-comp ctx (:refId layer) paint spread key)))]
                    (= 4 (:ty layer)) (lottie-silhouette-shape-layer layer paint spread)
                    (and (= 2 (:ty layer)) (nil? (:tt layer)))
                    [(assoc (dissoc layer :ef) :td 1)
                     {:ddd 0 :ty 4 :nm (str (:nm layer) " colour") :sr 1 :ks lottie-identity-ks :ao 0 :tt 1
                      :shapes [(lottie-group "colour"
                                             [{:ty "rc" :nm "all" :d 1
                                               :s {:a 0 :k [(* 3 width) (* 3 height)]}
                                               :p {:a 0 :k [(/ width 2) (/ height 2)]}
                                               :r {:a 0 :k 0}}
                                              (merge {:ty "fl" :nm "fill" :r 1} paint)])]
                      :ip (:ip layer) :op (:op layer) :st 0 :bm 0
                      ::extra true}]
                    :else [(dissoc layer :ef)]))
        outs    (into [] (mapcat (fn [layer] (map #(vector layer %) (convert layer)))) layers)
        ;; what the layers were numbered, now: the parents they follow
        ind-of  (into {}
                      (keep-indexed (fn [i [layer out]]
                                      (when (and (some? (:ind layer)) (not (::extra out)))
                                        [(:ind layer) (inc i)])))
                      outs)]
    (into []
          (map-indexed (fn [i [_ out]]
                         (let [parent (get ind-of (:parent out))]
                           (cond-> (assoc (dissoc out ::extra :parent) :ind (inc i))
                             (some? parent) (assoc :parent parent)))))
          outs)))

(defn- lottie-silhouette-comp
  "The id of a precomp of the silhouette of the precomp `ref` (see
  `lottie-silhouette`), made once for `key`: it goes to `comps`. `ref`
  itself when there is no such precomp."
  [{:keys [comps] :as ctx} ref paint spread key]
  (let [id (str "sil_" key "_" ref)]
    (cond
      (some #(= id (:id %)) @comps) id

      :else
      (if-let [comp (d/seek #(= ref (:id %)) @comps)]
        (let [layers (lottie-silhouette ctx (:layers comp) paint spread key)]
          (add-comp! comps {:id id :nm (str (:nm comp) " silhouette") :layers layers})
          id)
        ref))))

(defn- lottie-spreads?
  "Whether `shadow` spreads, at rest or as the keyframes `kfs` of its
  spread play (out only, with `out?`)."
  [shadow kfs & {:keys [out?]}]
  (boolean (some #(if out? (pos? (or % 0)) (not (zero? (or % 0))))
                 (cons (:spread shadow) (map :value kfs)))))

(defn- lottie-inner-shadow-layers
  "The layers of the inner shadows of `shape` over its drawing (see
  `lottie-content-layer`), the first on top: each a matte of its outline
  and what lies outside it, moved and blurred (see
  `lottie-shadow-layer`)."
  [{:keys [objects timeline duration ox oy] :as ctx} shape]
  (let [ctrack  (lottie-content-track ctx shape)
        cby     (group-by :property (:keyframes ctrack))
        origin  (cta/position-origin timeline objects (:id shape))
        ks      (lottie-ks (assoc shape :opacity 1) ctrack ox oy origin)
        sized   (when (lottie-sized? shape ctrack) (lottie-size-props shape ctrack ox oy))
        outline (lottie-outline shape cby ox oy sized)]
    (into []
          (mapcat (fn [[index s]]
                    [(lottie-matte-layer shape outline ks duration)
                     (lottie-shadow-layer ctx shape cby outline ks ox oy duration index s)]))
          (->> (cta/as-vec (:shadow shape))
               (map-indexed vector)
               (filter #(and (= :inner-shadow (:style (second %))) (not (:hidden (second %)))))))))

(defn- lottie-effects-layer
  "A precomp layer of the layer `shape` that casts its shadows as layers
  of their own (see `lottie-shadow-layers?`): its drawing (see
  `lottie-content-layer`), its inner shadows over it and its drop
  shadows under it, the first of each on top (see `lottie-shadow-layer`),
  which it moves, turns, fades and blurs as its track says, as the
  canvas blurs a shape with its shadows. The precomp goes to `comps`."
  [{:keys [objects timeline duration width height ox oy comps] :as ctx} shape]
  (let [id      (:id shape)
        name    (or (:name shape) (str id))
        track   (lottie-track ctx id)
        ctrack  (lottie-content-track ctx shape)
        cby     (group-by :property (:keyframes ctrack))
        origin  (cta/position-origin timeline objects id)
        ks      (lottie-ks (assoc shape :opacity 1) ctrack ox oy origin)
        sized   (when (lottie-sized? shape ctrack) (lottie-size-props shape ctrack ox oy))
        outline (lottie-outline shape cby ox oy sized)
        shown   (->> (cta/as-vec (:shadow shape))
                     (map-indexed vector)
                     (remove (comp :hidden second)))
        shadow  #(lottie-shadow-layer ctx shape cby outline ks ox oy duration %1 %2)
        inners  (lottie-inner-shadow-layers ctx shape)
        drops   (into [] (comp (filter #(= :drop-shadow (:style (second %) :drop-shadow)))
                               (map (fn [[index s]] (shadow index s))))
                      shown)
        ref     (str "comp_" id)
        by-prop (group-by :property (:keyframes track))
        blur    (lottie-blur-effect shape by-prop)
        layers  (lottie-numbered (concat inners [(lottie-content-layer ctx shape)] drops))]
    (add-comp! comps {:id ref :nm name :layers layers})
    (cond-> {:ddd 0
             :ty 0
             :nm name
             :refId ref
             :sr 1
             :ks (lottie-holder-ks shape track ox oy origin)
             :ao 0
             :w width
             :h height
             :ip 0
             :op (ms->frames duration)
             :st 0
             :bm 0}
      (seq blur) (assoc :ef (vec blur)))))

(defn- lottie-draws?
  "Whether the content layer of `shape` draws anything: paint of its own
  or shapes inside it that are no layers."
  [objects layer-ids shape]
  (boolean
   (or (seq (:fills shape))
       (seq (:strokes shape))
       (some #(and (not (contains? layer-ids %))
                   (not (true? (dm/get-in objects [% :hidden]))))
             (:shapes shape)))))

(declare lottie-node-layers)

(defn- lottie-holds-inner-shadows?
  "Whether the group `shape` has an inner shadow to show, which a Lottie
  export casts inside what it holds (see `lottie-inner-shadow-mattes`), as
  the SVG renderer does. A board casts it inside its fills, under what it
  holds (see `board-inner-shadows?`)."
  [shape]
  (and (= :group (:type shape))
       (some #(and (= :inner-shadow (:style %)) (not (:hidden %))) (cta/as-vec (:shadow shape)))))

(defn- lottie-inner-shadow-mattes
  "The layers of the inner shadows of the group `shape` over what it
  holds, the precomp `content` (see `lottie-precomp-layer`), the first
  on top: each a precomp of the colour of the shadow cut by an inverted
  matte of `content` moved by its offset, which it blurs as soft as on
  the canvas (see `lottie-gaussian-blur`), under a matte of `content`,
  which keeps it inside: as the canvas casts it inside the silhouette of
  what the shape holds. Its spread erodes the matte it is cut by, as a
  silhouette (see `lottie-silhouette`), so it reaches in further; the
  canvas spreads it in only. Their precomps go to `comps`."
  [{:keys [comps duration width height] :as ctx} shape content]
  (let [by-prop (group-by :property (:keyframes (lottie-track ctx (:id shape))))
        op      (ms->frames duration)
        layer   (fn [name ref ks]
                  {:ddd 0 :ty 0 :nm name :refId ref :sr 1 :ks ks :ao 0
                   :w width :h height :ip 0 :op op :st 0 :bm 0})]
    (into []
          (mapcat
           (fn [[index shadow]]
             (let [kfs    #(slot-kfs by-prop % index)
                   name   (str (or (:name shape) "layer") " inner shadow " (inc index))
                   ref    (str "inner_" (:id shape) "_" index)
                   ;; what it is cut by, shrunk by its spread
                   in     #(- (max 0 (or % 0)))
                   cut    (if (lottie-spreads? shadow (kfs :shadow-spread) :out? true)
                            (lottie-silhouette-comp ctx content {:c {:a 0 :k [0 0 0 1]} :o {:a 0 :k 100}}
                                                    {:rest (in (:spread shadow))
                                                     :kfs (mapv #(update % :value in) (kfs :shadow-spread))}
                                                    ref)
                            content)
                   offset (lottie-pair-prop (kfs :shadow-offset-x) (kfs :shadow-offset-y)
                                            (or (:offset-x shadow) 0) (or (:offset-y shadow) 0)
                                            (fn [x y] [x y 0]))
                   colour {:ddd 0 :ty 4 :nm (str name " colour") :sr 1 :ks lottie-identity-ks :ao 0 :tt 2
                           :shapes [(lottie-group "colour"
                                                  [{:ty "rc" :nm "all" :d 1
                                                    :s {:a 0 :k [(* 3 width) (* 3 height)]}
                                                    :p {:a 0 :k [(/ width 2) (/ height 2)]}
                                                    :r {:a 0 :k 0}}
                                                   (merge {:ty "fl" :nm "fill" :r 1} (lottie-shadow-paint shadow kfs))])]
                           :ip 0 :op op :st 0 :bm 0}]
               (add-comp! comps {:id ref :nm name
                                 :layers (lottie-numbered
                                          [(assoc (layer (str name " moved") cut (assoc lottie-identity-ks :p offset))
                                                  :td 1)
                                           colour])})
               [(assoc (layer (str name " inside") content lottie-identity-ks) :td 1 ::effect true)
                (assoc (layer name ref lottie-identity-ks)
                       :tt 1
                       :ef [(lottie-gaussian-blur (kfs :shadow-blur) (or (:blur shadow) 0))]
                       ::effect true)])))
          (->> (cta/as-vec (:shadow shape))
               (map-indexed vector)
               (filter #(and (= :inner-shadow (:style (second %))) (not (:hidden (second %)))))))))

(def lottie-track-mattes
  "The track matte (`tt`) of each mask mode (see `cts/mask-modes`)."
  {:alpha 1 :alpha-inverted 2 :luma 3 :luma-inverted 4})

(defn- lottie-masked-layers
  "The layers of the masked group `shape` in its precomp, the top-most
  first: its mask, the bottom-most of what it holds, a matte (`td`)
  over a precomp of the others, `inner` but the mask (see
  `layer-nodes`), which keeps them to it (`tt`, by alpha or luma,
  inverted or not, as its mask mode says), as the canvas does. The
  mask draws without its shadows and blur, as the canvas draws it; the
  layers it holds are layers (see `aexp/export-holders`). The precomp goes to
  `comps`."
  [{:keys [objects comps duration width height] :as ctx} shape inner]
  (let [mask-id (first (:shapes shape))
        node    (d/seek #(= mask-id (:id %)) inner)
        plain   (update-in ctx [:objects mask-id] dissoc :shadow :blur)
        drawn   (cond
                  (some? node)                (lottie-node-layers plain [node])
                  (contains? objects mask-id) [(lottie-shape-layer plain (get-in plain [:objects mask-id])
                                                                   (lottie-track ctx mask-id))])
        matte   (d/seek (complement ::null) drawn)
        ref     (str "masked_" (:id shape))
        name    (or (:name shape) ref)]
    (let [layers (lottie-numbered (lottie-node-layers ctx (filterv #(not= mask-id (:id %)) inner)))]
      (add-comp! comps {:id ref :nm (str name " content") :layers layers}))
    (cond-> []
      (some? matte) (conj (-> matte (dissoc :ef) (assoc :td 1)))
      :always       (conj (cond-> {:ddd 0 :ty 0 :nm (str name " content") :refId ref :sr 1
                                   :ks lottie-identity-ks :ao 0 :w width :h height
                                   :ip 0 :op (ms->frames duration) :st 0 :bm 0}
                            (some? matte) (assoc :tt (get lottie-track-mattes (cts/mask-mode shape) 1))))
      :always       (into (filter ::null) drawn))))

(defn- lottie-precomp-layer
  "A precomp layer of the node `id` (see `layer-nodes`): it moves, turns,
  fades and clips its layers, with the shape's own drawing at the
  bottom, all drawn as they are, turned (see `lottie-holder-ks`): the
  inner shadows of a board over its drawing (see
  `lottie-inner-shadow-layers`), those of a group over all (in a precomp
  around theirs, see `lottie-inner-shadow-mattes`); a masked group keeps
  its layers to its mask (see `lottie-masked-layers`). The precomps go
  to `comps`."
  [{:keys [objects layer-ids timeline duration width height ox oy comps] :as ctx}
   {:keys [id inner clip?]}]
  (let [shape   (get objects id)
        masked? (and (not clip?) (true? (:masked-group shape)))
        track   (if clip?
                  {:shape-id id :keyframes [] :duration duration}
                  (lottie-track ctx id))
        content (when (and (not clip?)
                           (not masked?)
                           (or (contains? (:assets ctx) id)
                               (lottie-draws? objects layer-ids shape)))
                  (lottie-content-layer ctx shape))
        ref     (str "comp_" id)
        ;; a board casts its inner shadows over its drawing, under what it
        ;; holds
        layers  (lottie-numbered (if masked?
                                   (lottie-masked-layers ctx shape inner)
                                   (cond-> (lottie-node-layers ctx inner)
                                     (and (some? content) (not clip?) (aexp/board-inner-shadows? shape))
                                     (into (lottie-inner-shadow-layers ctx shape))

                                     (some? content) (conj content))))
        by-prop (group-by :property (:keyframes track))
        effects (when-not clip?
                  (concat (when (lottie-vector-layer? objects layer-ids id)
                            (lottie-shadow-effects shape by-prop))
                          (lottie-blur-effect shape by-prop)))]
    (add-comp! comps {:id ref :nm (or (:name shape) ref) :layers layers})
    (cond-> {:ddd 0
             :ty 0
             :nm (or (:name shape) ref)
             :refId (if (and (not clip?) (lottie-holds-inner-shadows? shape))
                      (let [fx     (str "fx_" id)
                            layers (lottie-numbered
                                    (conj (lottie-inner-shadow-mattes ctx shape ref)
                                          {:ddd 0 :ty 0 :nm (or (:name shape) ref) :refId ref :sr 1
                                           :ks lottie-identity-ks :ao 0 :w width :h height
                                           :ip 0 :op (ms->frames duration) :st 0 :bm 0}))]
                        (add-comp! comps {:id fx :nm (str (or (:name shape) ref) " effects") :layers layers})
                        fx)
                      ref)
             :sr 1
             :ks (lottie-holder-ks shape track ox oy (cta/position-origin timeline objects id))
             :ao 0
             :w width
             :h height
             :ip 0
             :op (ms->frames duration)
             :st 0
             :bm 0}
      (seq effects)
      (assoc :ef (vec effects))

      ;; its outline as it is, turned, as what it holds, resized as it
      ;; plays
      (aexp/clipping-board? shape)
      (assoc :hasMask true :masksProperties [(lottie-clip-mask shape ox oy 0 track)]))))

(defn- lottie-unit-layer
  "The image layer of the unit `k` of a text, whose picture is `asset`
  (over its box in the board, see `timeline->lottie`), playing the
  keyframes of the unit, `track` (see `unit-timelines`): it moves by them
  from where it rests, turns and scales around its middle and blurs as
  a layer blur of their radius."
  [name k asset track duration]
  (let [by-prop (group-by :property (:keyframes track))
        w       (double (or (:w asset) 0))
        h       (double (or (:h asset) 0))
        cx      (+ (or (:x asset) 0) (/ w 2))
        cy      (+ (or (:y asset) 0) (/ h 2))
        blur    (lottie-blur-effect {} by-prop)]
    (cond-> {:ddd 0
             :ty 2
             :nm (str name " " (inc k))
             :refId (:id asset)
             :sr 1
             :ks {:o (lottie-prop-1d (by-prop :opacity) (fn [v] (* 100.0 v)) 100.0)
                  :r (lottie-prop-1d (by-prop :rotation) identity 0)
                  :p {:s true
                      :x (lottie-prop-1d (by-prop :x) (fn [v] (+ cx v)) cx)
                      :y (lottie-prop-1d (by-prop :y) (fn [v] (+ cy v)) cy)}
                  :a {:a 0 :k [(/ w 2) (/ h 2) 0]}
                  :s (lottie-scale-prop (by-prop :scale-x) (by-prop :scale-y))}
             :ao 0
             :ip 0
             :op (ms->frames duration)
             :st 0
             :bm 0}
      (some? blur) (assoc :ef blur))))

(defn- lottie-run-groups
  "The Lottie groups drawing the outlines of the glyphs of the text `text`,
  its `runs` (`[{:content :fills}]`, on the page as they are, see
  `timeline->lottie`), in the board at `ox`, `oy`: a path of each in its
  fills, with the strokes of the text."
  [text runs ox oy]
  (into []
        (mapcat (fn [{:keys [content fills]}]
                  (lottie-vector-groups (path/update-geometry {:type :path :fills fills :strokes (:strokes text)}
                                                              content)
                                        {} ox oy)))
        runs))

(defn- lottie-unit-vector-layer
  "The shape layer of the unit `k` of the text `text`, the outlines of its
  glyphs `asset` (`{:runs :center}`, see `timeline->lottie`), playing the
  keyframes of the unit, `track`: it moves by them from where it rests,
  turns and scales around the centre of its box and blurs as a layer
  blur of their radius."
  [text k asset track duration ox oy]
  (let [by-prop (group-by :property (:keyframes track))
        cx      (- (get-in asset [:center :x] 0) ox)
        cy      (- (get-in asset [:center :y] 0) oy)
        blur    (lottie-blur-effect {} by-prop)]
    (cond-> {:ddd 0
             :ty 4
             :nm (str (or (:name text) (:id text)) " " (inc k))
             :sr 1
             :ks {:o (lottie-prop-1d (by-prop :opacity) (fn [v] (* 100.0 v)) 100.0)
                  :r (lottie-prop-1d (by-prop :rotation) identity 0)
                  :p {:s true
                      :x (lottie-prop-1d (by-prop :x) (fn [v] (+ cx v)) cx)
                      :y (lottie-prop-1d (by-prop :y) (fn [v] (+ cy v)) cy)}
                  :a {:a 0 :k [cx cy 0]}
                  :s (lottie-scale-prop (by-prop :scale-x) (by-prop :scale-y))}
             :ao 0
             :shapes (lottie-run-groups text (:runs asset) ox oy)
             :ip 0
             :op (ms->frames duration)
             :st 0
             :bm 0}
      (some? blur) (assoc :ef blur))))

(defn- lottie-units-layer
  "A precomp layer of the text `shape` whose units play one by one (see
  `unit-timelines`), with the picture of each unit and of what is no unit
  as assets (`[text-id k]`, `[text-id :rest]`), or the outlines of their
  glyphs (see `lottie-unit-vector-layer`): it moves, turns and fades
  the text as its track says (its picture turned already, it turns by
  the animation only), and the units inside it play their own
  keyframes, their moves turned with the text. The precomp goes to
  `comps`."
  [{:keys [objects timeline duration width height ox oy comps assets units] :as ctx} shape]
  (let [id       (:id shape)
        name     (or (:name shape) (str id))
        track    (lottie-track ctx id)
        units    (get units id)
        units-tl (aexp/turned-moves (:timeline units) (:rotation shape))
        rest     (get assets [id :rest])
        layers   (cond-> (into []
                               (keep-indexed (fn [k unit-id]
                                               (when-let [asset (get assets [id k])]
                                                 (let [track (get-in units-tl [:tracks unit-id])]
                                                   (if (some? (:runs asset))
                                                     (lottie-unit-vector-layer shape k asset track duration ox oy)
                                                     (lottie-unit-layer name k asset track duration))))))
                               (:ids units))
                   (some? (:runs rest))
                   (conj {:ddd 0 :ty 4 :nm (str name " text") :sr 1
                          :ks {:o {:a 0 :k 100}
                               :r {:a 0 :k 0}
                               :p {:a 0 :k [0 0 0]}
                               :a {:a 0 :k [0 0 0]}
                               :s {:a 0 :k [100 100 100]}}
                          :ao 0 :shapes (lottie-run-groups shape (:runs rest) ox oy)
                          :ip 0 :op (ms->frames duration) :st 0 :bm 0})

                   (and (some? rest) (nil? (:runs rest)))
                   (conj {:ddd 0 :ty 2 :nm (str name " text") :refId (:id rest) :sr 1
                          :ks {:o {:a 0 :k 100}
                               :r {:a 0 :k 0}
                               :p {:a 0 :k [(or (:x rest) 0) (or (:y rest) 0) 0]}
                               :a {:a 0 :k [0 0 0]}
                               :s {:a 0 :k [100 100 100]}}
                          :ao 0 :ip 0 :op (ms->frames duration) :st 0 :bm 0}))
        ref      (str "comp_" id)]
    (add-comp! comps {:id ref :nm name :layers (lottie-numbered layers)})
    {:ddd 0
     :ty 0
     :nm name
     :refId ref
     :sr 1
     :ks (lottie-holder-ks shape track ox oy (cta/position-origin timeline objects id))
     :ao 0
     :w width
     :h height
     :ip 0
     :op (ms->frames duration)
     :st 0
     :bm 0}))

(defn- lottie-container-shadows
  "The layers of the drop shadows of the group or board `shape` when one
  spreads, which the drop shadow effect of Lottie does not (nil when
  none does), under its layer `layer`, the first on top: each the
  silhouette of what `layer` draws (see `lottie-silhouette`) in its
  colour, moved by its offset, as opaque as it is and blurred as soft
  as on the canvas, with the blur of `shape`, which `layer` moves, turns
  and fades as its own; kept to the outline of a board that clips. As
  the canvas grows each shape in it by its spread, and its own fills.
  Their precomps go to `comps`."
  [{:keys [comps width height duration] :as ctx} shape layer]
  (let [id      (:id shape)
        by-prop (group-by :property (:keyframes (lottie-track ctx id)))
        shown   (->> (cta/as-vec (:shadow shape))
                     (map-indexed vector)
                     (filter (comp drop-shadow? second)))]
    (when (some (fn [[index shadow]] (lottie-spreads? shadow (slot-kfs by-prop :shadow-spread index))) shown)
      (let [op      (ms->frames duration)
            name    (or (:name shape) (str id))
            drawn   (if (= 0 (:ty layer))
                      (:layers (d/seek #(= (str "comp_" id) (:id %)) @comps))
                      [(-> layer
                           (dissoc :ef :hasMask :masksProperties)
                           (assoc :ks lottie-identity-ks :ind 1))])
            blur    (lottie-blur-effect shape by-prop)
            precomp (fn [nm ref ks]
                      {:ddd 0 :ty 0 :nm nm :refId ref :sr 1 :ks ks :ao 0 :w width :h height
                       :ip 0 :op op :st 0 :bm 0})]
        (mapv (fn [[index shadow]]
                (let [kfs    #(slot-kfs by-prop % index)
                      key    (str id "_" index)
                      sname  (str name " drop shadow " (inc index))
                      {:keys [o] :as paint} (lottie-shadow-paint shadow kfs)
                      offset (lottie-pair-prop (kfs :shadow-offset-x) (kfs :shadow-offset-y)
                                               (or (:offset-x shadow) 0) (or (:offset-y shadow) 0)
                                               (fn [x y] [x y 0]))
                      sref   (str "spread_" key)
                      oref   (str "drop_" key)]
                  (let [layers (lottie-silhouette ctx drawn (assoc paint :o {:a 0 :k 100})
                                                  {:rest (or (:spread shadow) 0) :kfs (kfs :shadow-spread)}
                                                  key)]
                    (add-comp! comps {:id sref :nm (str sname " silhouette") :layers layers}))
                  (add-comp! comps
                             {:id oref :nm sname
                              :layers [(cond-> (assoc (precomp (str sname " moved") sref
                                                               (assoc lottie-identity-ks :p offset :o o))
                                                      :ind 1)
                                         (:hasMask layer)
                                         (assoc :hasMask true :masksProperties (:masksProperties layer)))]})
                  (assoc (precomp sname oref (:ks layer))
                         :ef (into [(lottie-gaussian-blur (kfs :shadow-blur) (or (:blur shadow) 0))] blur)
                         ::effect true)))
              shown)))))

(defn- lottie-see-through?
  "Whether the layer `layer` is less than opaque, at rest or as it plays."
  [layer]
  (let [o (get-in layer [:ks :o])]
    (or (= 1 (:a o)) (< (or (:k o) 100) 100))))

(defn- lottie-faded-layers
  "A precomp layer of the layer `layer` of `shape` over the layers of its
  drop shadows `shadows` (see `lottie-container-shadows`), which fades
  and blends them together as `layer` would: in Lottie a layer that fades
  over the layers of its shadows lets them show through it, which the
  canvas does not. The precomp goes to `comps`."
  [{:keys [comps width height duration]} shape layer shadows]
  (let [name  (str (or (:name shape) (:id shape)) " with shadows")
        ref   (str "faded_" (:id shape))
        solid #(-> % (assoc-in [:ks :o] {:a 0 :k 100}) (assoc :bm 0))]
    (add-comp! comps {:id ref :nm name :layers (lottie-numbered (mapv solid (cons layer shadows)))})
    {:ddd 0 :ty 0 :nm name :refId ref :sr 1
     :ks (assoc lottie-identity-ks :o (get-in layer [:ks :o] {:a 0 :k 100}))
     :ao 0 :w width :h height :ip 0 :op (ms->frames duration) :st 0 :bm (or (:bm layer) 0)}))

(defn- lottie-rest-ks
  "The transform of the layer of `shape` (see `lottie-shape-layer`) with
  nothing playing: where it is at rest, in the composition."
  [{:keys [objects layer-ids assets timeline ox oy]} shape]
  (let [id     (:id shape)
        origin (cta/position-origin timeline objects id)
        rest   (cond-> {:shape-id id :keyframes []}
                 (some? (dm/get-in timeline [:tracks id :origin]))
                 (assoc :origin (dm/get-in timeline [:tracks id :origin])))
        asset  (get assets id)]
    (if (and (some? asset) (not (lottie-vector-layer? objects layer-ids id)))
      (:ks (shape->lottie-image-layer 0 shape rest ox oy origin asset 1))
      (lottie-ks shape rest ox oy origin))))

(defn- lottie-tagged
  "`layer`, the one layer that moves `shape` as its transform says: with
  the shape (`::shape`) and, when it leads a layer that can be its child
  (one of the `:leaders` of `ctx`), its transform at rest (`::rest-ks`,
  see `lottie-parented`)."
  [ctx shape layer]
  (cond-> (assoc layer ::shape (:id shape))
    (contains? (:leaders ctx) (:id shape))
    (assoc ::rest-ks (lottie-rest-ks ctx shape))))

;; 3D (Penpot Motion, see `app.common.geom.tilt`)

(declare lottie-shifted)

(defn- lottie-tilted?
  "Whether the layer of `shape` playing `track` tilts out of the plane,
  comes out of it (`:translate-z`) or hides its back face, at rest or as
  it plays."
  [shape track]
  (or (gtl/three-d? shape)
      (true? (:backface-hidden shape))
      (some #(contains? #{:rotate-x :rotate-y :translate-z} (:property %)) (:keyframes track))))

(defn- lottie-posed
  "`objects` of `ctx` at `t` ms, the shapes of `ids` posed as its
  timeline plays them (see `ctaf/timeline->modif-tree`)."
  [{:keys [timeline objects]} ids t]
  (let [tree (ctaf/timeline->modif-tree (update timeline :tracks select-keys ids) objects t)]
    (reduce (fn [objects id]
              (if-let [mods (dm/get-in tree [id :modifiers])]
                (update objects id #(-> % (gsh/transform-shape mods) (ctm/apply-structure-modifiers mods)))
                objects))
            objects
            ids)))

(defn- lottie-backface-opacity
  "The opacity `o` of the layer of `shape` playing `track`, none while it
  shows its back if it hides it (see `gtl/back-facing?`, which Lottie can
  not), at each frame it turns: held."
  [{:keys [objects duration] :as ctx} shape track o]
  (if-not (true? (:backface-hidden shape))
    o
    (let [id     (:id shape)
          ids    (cons id (cfh/get-parent-ids objects id))
          step   (/ 1000.0 lottie-fps)
          frames (conj (vec (range 0 duration step)) duration)
          back   (mapv (fn [t]
                         (let [posed (lottie-posed ctx ids t)]
                           (gtl/back-facing? posed (get posed id))))
                       frames)]
      (if (not-any? true? back)
        o
        (let [values (acss/shape-values {:tracks {id track}} id)
              rest   (or (:opacity shape) 1)
              at     (fn [t back?] (if back? 0 (* 100.0 (get (values t) :opacity rest))))
              stops  (map (fn [t back?] [t (at t back?)]) frames back)
              ;; each frame while its opacity plays, else where it turns
              stops  (if (= 1 (:a o))
                       stops
                       (keep-indexed (fn [i [_ v :as stop]]
                                       (when (or (zero? i) (not= v (second (nth stops (dec i)))))
                                         stop))
                                     stops))]
          {:a 1 :k (mapv (fn [[t v]] {:t (ms->frames t) :s [v] :h 1}) stops)})))))

(defn- lottie-tilted-layers
  "The layers of the layer `layer` of `shape` playing `track`, which tilts
  (see `lottie-tilted?`): the layer in 3D, tilted in its own axes (`rx`,
  `ry`, the other way round from CSS, and again along an axis its drawing
  is flipped on), its pivot lifted to where its tilt at rest takes it
  (see `gtl/lift`); and, when it turns in the plane, a 3D null layer
  turning it (`rz`), whose child it is: Lottie turns a layer in the plane
  before it tilts it. A layer whose drawing holds its turn at rest (an
  image or a precomp, see `lottie-holder-ks`) turns that back, so its
  tilt is in its own axes. Its depth (`:translate-z`, towards the viewer)
  is the z of its position (or of the null's), away from the viewer in
  Lottie. Its back face hidden by its opacity (see
  `lottie-backface-opacity`). `[layer]` or `[layer null]`, the null with
  what the layer follows (`::follow`)."
  [ctx shape track layer]
  (let [ks       (:ks layer)
        by-prop  (group-by :property (:keyframes track))
        base-r   (or (:rotation shape) 0)
        baked    (if (= 4 (:ty layer)) 0 base-r)
        fx       (if (:flip-x shape) -1 1)
        fy       (if (:flip-y shape) -1 1)
        rest-x   (or (:rotate-x shape) 0)
        rest-y   (or (:rotate-y shape) 0)
        {px :x py :y} (cta/track-origin track)
        [lx ly lz]    (gtl/lift shape px py rest-x rest-y)
        r        (or (:r ks) {:a 0 :k 0})
        turns?   (or (= 1 (:a r)) (not (mth/almost-zero? (:k r))) (not (mth/almost-zero? baked)))
        tilt     {:rx (lottie-prop-1d (by-prop :rotate-x) #(* -1 fy %) (* -1 fy rest-x))
                  :ry (lottie-prop-1d (by-prop :rotate-y) #(* -1 fx %) (* -1 fx rest-y))
                  :rz {:a 0 :k (- baked)}
                  :or {:a 0 :k [0 0 0]}}
        depth    (lottie-prop-1d (by-prop :translate-z) #(- %) (- (or (:translate-z shape) 0)))
        deep?    (or (= 1 (:a depth)) (not (mth/almost-zero? (:k depth))))
        lift?    (or deep? (not-every? mth/almost-zero? [lx ly lz]))
        o        (lottie-backface-opacity ctx shape track (:o ks))]
    (if turns?
      (let [null  {:ddd 1
                   :ty 3
                   :nm (str (or (:name shape) "layer") " turn")
                   :sr 1
                   :ks {:o {:a 0 :k 100}
                        :p (cond-> (:p ks) deep? (assoc :z depth))
                        :a {:a 0 :k [0 0 0]}
                        :s {:a 0 :k [100 100 100]}
                        :rx {:a 0 :k 0}
                        :ry {:a 0 :k 0}
                        :rz (lottie-shifted r baked)
                        :or {:a 0 :k [0 0 0]}}
                   :ao 0
                   :ip 0
                   :op (ms->frames (:duration ctx))
                   :st 0
                   :bm 0
                   ::tilt-null (:id shape)}
            child (-> layer
                      (assoc :ddd 1 ::tilt-parent (:id shape))
                      (update :ks #(-> (dissoc % :r)
                                       (merge tilt)
                                       (assoc :o o
                                              :p {:a 0 :k [(* fx lx) (* fy ly) (- lz)]}))))]
        [child null])
      [(-> layer
           (assoc :ddd 1)
           (update :ks #(-> (dissoc % :r)
                            (merge tilt)
                            (assoc :o o)
                            (cond-> lift?
                              (update :p (fn [p]
                                           (-> p
                                               (update :x lottie-shifted (* fx lx))
                                               (update :y lottie-shifted (* fy ly))
                                               (assoc :z (lottie-shifted depth (- lz))))))))))])))

(defn- lottie-node-layers
  "The Lottie layers of `nodes` (see `layer-nodes`), the top-most first,
  each that follows the board it is in as it resizes (`:follow` of
  `ctx`, see `board-follow`) with the null layer that moves it (see
  `lottie-follow-null`); a group or board whose drop shadows spread
  over the layers of those (see `lottie-container-shadows`)."
  [ctx nodes]
  (into []
        (mapcat (fn [{:keys [id inner] :as node}]
                  (when-let [shape (get (:objects ctx) id)]
                    (let [track   (lottie-track ctx id)
                          vector? (lottie-vector-layer? (:objects ctx) (:layer-ids ctx) id)
                          layer   (cond
                                    (contains? (:units ctx) id) (lottie-units-layer ctx shape)
                                    (or (seq inner)
                                        (and (or (lottie-holds-inner-shadows? shape) (aexp/board-inner-shadows? shape))
                                             vector?))
                                    (lottie-precomp-layer ctx node)
                                    (and vector? (lottie-shadow-layers? shape track))
                                    (lottie-effects-layer ctx shape)
                                    :else                       (lottie-tagged ctx shape (lottie-shape-layer ctx shape track)))
                          shadows (when (and vector?
                                             (contains? #{:group :frame} (:type shape))
                                             (not (contains? (:units ctx) id)))
                                    (lottie-container-shadows ctx shape layer))
                          ;; its drop shadows are those layers
                          layer   (if (seq shadows)
                                    (let [ef (filterv #(not= 25 (:ty %)) (:ef layer))]
                                      (if (seq ef) (assoc layer :ef ef) (dissoc layer :ef)))
                                    layer)
                          layers  (cond
                                    (and (seq shadows) (lottie-see-through? layer))
                                    [(lottie-faded-layers ctx shape layer shadows)]

                                    (lottie-tilted? shape track)
                                    (into (lottie-tilted-layers ctx shape track layer) shadows)

                                    :else
                                    (into [layer] shadows))]
                      (if-let [stops (get (:follow ctx) id)]
                        (conj (mapv #(cond-> % (nil? (::tilt-parent %)) (assoc ::follow id)) layers)
                              (lottie-follow-null ctx shape stops))
                        layers)))))
        (rseq (vec nodes))))

(defn- lottie-shifted
  "The 1D Lottie property `prop` with `d` added to its values."
  [prop d]
  (if (= 1 (:a prop))
    (update prop :k (fn [kfs] (mapv #(update % :s (fn [[v]] [(+ v d)])) kfs)))
    (update prop :k + d)))

(defn- lottie-parent-ks
  "The transform `ks` of a layer turned into the space of a parent layer
  whose transform at rest is `rest` (see `lottie-rest-ks`): moved and
  turned back as much, its anchor and scale kept. Nil when it can not be
  as the canvas plays it: the parent scales at rest, or turns and the
  layer's x and y play at other times or easings, which a turn mixes."
  [ks rest]
  (let [px    (get-in rest [:p :x :k])
        py    (get-in rest [:p :y :k])
        r     (get-in rest [:r :k])
        [ax ay] (get-in rest [:a :k])
        [sx sy] (get-in rest [:s :k])
        x     (get-in ks [:p :x])
        y     (get-in ks [:p :y])]
    (when (and (number? px) (number? py) (number? r) (number? ax) (number? ay)
               (mth/close? 100 sx) (mth/close? 100 sy)
               (some? x) (some? y))
      (let [rad  (mth/radians (- r))
            cos  (mth/cos rad)
            sin  (mth/sin rad)
            to   (fn [x y]
                   [(+ ax (- (* cos (- x px)) (* sin (- y py))))
                    (+ ay (+ (* sin (- x px)) (* cos (- y py))))])
            same #(= (mapv (fn [kf] (dissoc kf :s)) (:k x)) (mapv (fn [kf] (dissoc kf :s)) (:k y)))
            [x y] (cond
                    (mth/close? r 0)
                    [(lottie-shifted x (- ax px)) (lottie-shifted y (- ay py))]

                    (and (= 0 (:a x)) (= 0 (:a y)))
                    (let [[x' y'] (to (:k x) (:k y))]
                      [{:a 0 :k x'} {:a 0 :k y'}])

                    (and (= 1 (:a x)) (= 1 (:a y)) (same))
                    (let [pairs (mapv (fn [kx ky] (to (first (:s kx)) (first (:s ky)))) (:k x) (:k y))]
                      [(assoc x :k (mapv (fn [kf [v _]] (assoc kf :s [v])) (:k x) pairs))
                       (assoc y :k (mapv (fn [kf [_ v]] (assoc kf :s [v])) (:k y) pairs))]))]
        (when (some? x)
          (-> ks
              (assoc-in [:p :x] x)
              (assoc-in [:p :y] y)
              (update :r lottie-shifted (- r))))))))

(defn- lottie-parented
  "`layers`, of one composition, with the layer of each follower in
  `parents` (its id to its leader's) a child (`:parent`) of the layer of
  its leader when both are there, one each (see `lottie-tagged`), its
  transform turned into its leader's space (see `lottie-parent-ks`):
  Lottie plays the move, turn and scale of the leader on it as the
  canvas does. `[layers parented failed]`: the followers made children,
  and those whose layer is there but could not be (in a board whose size
  plays, which has its own parent, or with a leader elsewhere)."
  [layers parents]
  (let [tagged (group-by ::shape (filter ::shape layers))
        one    #(let [found (get tagged %)] (when (= 1 (count found)) (first found)))]
    (reduce (fn [[layers parented failed] [i layer]]
              (let [id   (::shape layer)
                    lead (some-> (get parents id) one)
                    ks   (when (and (some? lead)
                                    (= layer (one id))
                                    (nil? (:parent layer))
                                    (nil? (:parent lead))
                                    (some? (:ind lead)))
                           (lottie-parent-ks (:ks layer) (::rest-ks lead)))]
                (cond
                  (not (contains? parents id)) [layers parented failed]
                  (some? ks)                   [(assoc layers i (assoc layer :ks ks :parent (:ind lead)))
                                                (conj parented id) failed]
                  :else                        [layers parented (conj failed id)])))
            [(vec layers) #{} #{}]
            (map-indexed vector layers))))

(defn- lottie-untagged
  [layers]
  (mapv #(dissoc % ::shape ::rest-ks) layers))

(defn parent-candidates
  "The layers of `timeline` (of `objects`) whose follows (see
  `cta/follow-of`) a Lottie export can play by parenting (see
  `lottie-parented`): at once (no delay), taking where the pivot of the
  leader goes, how it turns and scales and not its opacity, from where
  it rests, or from a first frame where it plays at rest; a
  leader that scales evenly, never flips nor stretches as it plays (its
  hidden rows left out, its presets and styles as their keyframes, see
  `cta/resolve-animations`), and is no helper layer (see `cta/guide?`);
  no link of a chain that reaches (see `ctaf/ik-chains`). Where their
  layers land decides the rest."
  [timeline objects]
  (let [follows  (ctaf/followers timeline objects)
        chained  (into #{} (mapcat (juxt :a :b :e)) (vals (ctaf/ik-chains timeline objects follows)))
        resolved (delay (cta/resolve-animations timeline objects))
        even?    (fn [leader]
                   (let [kfs   (dm/get-in @resolved [:tracks leader :keyframes])
                         rows  #(into #{} (comp (filter (fn [kf] (= % (:property kf))))
                                                (map (juxt :time :value :easing :interpolation)))
                                      kfs)
                         sx    (rows :scale-x)
                         shape (get objects leader)]
                     (and (= sx (rows :scale-y))
                          (every? #(pos? (second %)) sx)
                          (not-any? #(contains? #{:width :height} (:property %)) kfs)
                          (not (:flip-x shape))
                          (not (:flip-y shape)))))
        at-zero  (delay (ctaf/timeline->modif-tree timeline objects 0))
        at-rest? (fn [leader]
                   (let [mods (dm/get-in @at-zero [leader :modifiers])
                         {:keys [a b c d e f]} (if (some? mods) (ctm/modifiers->transform mods) (gmt/matrix))]
                     (every? (fn [[v w]] (mth/close? v w)) [[a 1] [b 0] [c 0] [d 1] [e 0] [f 0]])))]
    (into #{}
          (filter (fn [id]
                    (let [{:keys [leader from position rotation scale opacity delay]} (cta/follow-of timeline id)]
                      (and (zero? delay) position rotation scale (not opacity)
                           (not (contains? chained id))
                           (not (cta/guide? timeline leader))
                           (even? leader)
                           (or (= :rest from) (at-rest? leader))))))
          follows)))

(defn- lottie-layers
  "The layers of the composition of `timeline`, the top-most first, and
  the precomps they use: a layer with layers inside it, or a board that
  clips some, becomes a precomp (see `lottie-precomp-layer`). Layers not
  in the tree of the board go below the others, and the board draws
  itself, and what is no layer, at the bottom. The layers in a board
  whose size plays on its outline follow it as `follow` says (see
  `board-follow`). The layers of `leaders` keep their transform at rest
  for the layers of their followers (see `lottie-tagged`)."
  [timeline objects layer-ids assets units follow leaders]
  (let [board-id (:board-id timeline)
        board    (get objects board-id)
        bsr      (:selrect board)
        ctx      {:objects objects
                  :follow follow
                  :leaders leaders
                  :layer-ids layer-ids
                  :assets assets
                  :units units
                  :timeline timeline
                  :duration (max 1 (:duration timeline))
                  :ox (or (:x bsr) 0)
                  :oy (or (:y bsr) 0)
                  :width (mth/round (or (:width bsr) 100))
                  :height (mth/round (or (:height bsr) 100))
                  :comps (volatile! [])}
        tree     (aexp/layer-nodes objects layer-ids board-id)
        treed    (into #{} (comp (mapcat #(tree-seq :inner :inner %)) (map :id)) tree)
        untreed  (into [] (comp (remove treed)
                                (remove #{board-id})
                                (filter #(contains? objects %))
                                (map (fn [id] {:id id})))
                       layer-ids)
        layers   (lottie-node-layers ctx (into untreed tree))
        board    (cond-> board (true? (:hide-fill-on-export board)) (assoc :fills []))
        content  (when (and (some? board)
                            (or (contains? assets board-id)
                                (lottie-draws? objects layer-ids board)))
                   (lottie-content-layer ctx board))
        ;; its inner shadows over its drawing, under the layers
        inner    (when (and (some? content) (aexp/board-inner-shadows? board))
                   (lottie-inner-shadow-layers ctx board))]
    ;; the layers of effects are none to tell (see `lottie-silhouette`)
    {:layers (mapv #(dissoc % ::effect)
                   (lottie-numbered (cond-> (into layers inner) (some? content) (conj content))))
     :comps  (mapv (fn [comp] (update comp :layers (partial mapv #(dissoc % ::effect)))) @(:comps ctx))}))

(defn- lottie-doc
  "The Lottie document of `timeline` (see `timeline->lottie`), the layers
  of the followers in `parents` (their ids to their leaders') children of
  their leaders' where they can be (see `lottie-parented`): `{:doc
  :parented :failed}`."
  [timeline objects assets propagate parents]
  (let [markers    (lottie-markers timeline)
        unit-texts (aexp/unit-text-ids assets)
        resolved   (-> timeline
                       (cta/flatten-splits #{:char :word :line} unit-texts)
                       (cta/resolve-animations objects)
                       ;; the size of a group on its layers (see `aexp/expand-group-sizes`)
                       (aexp/expand-group-sizes objects))
        ;; Lottie has no ping-pong, so the way back is part of the document.
        expand     #(-> % cta/expand-paths cta/expand-springs cta/cut-timeline cta/expand-ping-pong)
        units      (into {}
                         (map (fn [[id units]] [id (update units :timeline expand)]))
                         (select-keys (cta/unit-timelines resolved objects) unit-texts))
        timeline   (expand (cta/expand-loops resolved))
        board-id   (:board-id timeline)
        ;; the layers the pictures were taken of, even when all draw as
        ;; vectors: a shape among them that is no Lottie content (with
        ;; effects, see `lottie-content?`) draws only as one
        holders    (aexp/svg-holder-ids objects (aexp/svg-sized-ids objects timeline))
        layer-ids  (if (some? assets)
                     (set (aexp/export-layer-ids objects timeline))
                     (into #{}
                           (remove #(aexp/hidden-in? objects board-id %))
                           (-> (set (keys (:tracks timeline)))
                               (disj board-id)
                               (into (mapcat #(dm/get-in objects [% :shapes])) (aexp/export-holders objects timeline)))))
        bsr        (dm/get-in objects [board-id :selrect])
        follow     (when (and (some? propagate) (seq holders))
                     (into {} (aexp/board-follow timeline objects holders layer-ids propagate
                                                 (or (:x bsr) 0) (or (:y bsr) 0))))
        {:keys [layers comps]} (lottie-layers timeline objects layer-ids assets units follow
                                              (set (vals parents)))
        [layers parented failed] (lottie-parented layers parents)
        comps     (mapv (fn [comp] (update comp :layers lottie-parented parents)) comps)
        parented  (into parented (mapcat #(second (:layers %))) comps)
        failed    (into failed (mapcat #(nth (:layers %) 2)) comps)
        layers    (lottie-untagged layers)
        comps     (mapv (fn [comp] (update comp :layers #(lottie-untagged (first %)))) comps)
        ;; the pictures among them (the units of texts may be outlines)
        images    (map (fn [[_ asset]]
                         {:id (:id asset)
                          :w (:w asset)
                          :h (:h asset)
                          :u (or (:u asset) "")
                          :p (:p asset)
                          :e (or (:e asset) 1)})
                       (remove (comp :runs val) assets))]
    {:doc      (cond-> (lottie-document timeline objects layers (concat images comps))
                 (seq markers) (assoc :markers markers))
     :parented parented
     :failed   failed}))

(defn- lottie-parenting
  "The Lottie document of `timeline` (see `timeline->lottie`), the layers
  of the followers in `kept` children of their leaders' (see
  `lottie-doc`), the other follows playing by keyframes (see
  `ctaf/bake-follows`). Those it could not parent where their layers
  landed play by keyframes the next time."
  [timeline objects assets propagate kept]
  (let [baked   (ctaf/bake-follows timeline objects :keep? kept)
        parents (into {}
                      (keep (fn [id] (when-let [leader (:leader (cta/follow-of baked id))] [id leader])))
                      kept)
        {:keys [doc parented failed]} (lottie-doc baked objects assets propagate parents)
        left    (into #{} (remove #(and (contains? parented %) (not (contains? failed %)))) kept)]
    (if (seq left)
      (recur timeline objects assets propagate (into #{} (remove left) kept))
      doc)))

(defn timeline->lottie
  "Generate a Lottie (bodymovin) animation document (a plain map ready for
  JSON encoding) for `timeline`, using `objects` for geometry. The board
  (`:board-id`) is the composition. Pure.

  When `assets` is a map of `{shape-id {:id :w :h :u :p :e :x :y}}` (an
  image embedded as a data URI, `:e` 1, or a file `:p` in the folder `:u`
  of a dotLottie archive, `:e` 0; `:x :y` where it begins in the board,
  the box of the shape when not there), empty when all draw as vectors,
  the export layers (see `export-layer-ids`) are those shapes, which
  become image layers (`ty` 2) unless they export as vectors (see
  `lottie-vector-layer?`); the board's own asset draws it. Without
  assets (nil) the animated shapes are the layers, as vectors or
  rectangles of their bounds. Layers holding layers become precomps (see
  `lottie-layers`). The markers become Lottie markers (see
  `lottie-markers`). Split animations play as animations of the whole
  text (see `flatten-splits`), but when `assets` has pictures of its
  units, `[text-id k]` (and `[text-id :rest]` of what is no unit), or
  the outlines of their glyphs instead, `{:runs [{:content :fills}]
  :center}` on the page, which they turn and scale around (see
  `lottie-units-layer`). The width and height of a layer play on its
  outline when they can (see `lottie-sized?`), the layers in such a
  board moving with it as `:propagate` says (an option, see
  `board-follow`), those of a group on its layers (see
  `aexp/expand-group-sizes`), else they stretch it. A layer that follows
  another (see `cta/follow-of`) is a child (`parent`) of its leader's
  layer where Lottie plays the follow as the canvas does (see
  `parent-candidates`, `lottie-parented`), else it plays it by keyframes
  (see `ctaf/bake-follows`)."
  ([timeline objects]
   (timeline->lottie timeline objects nil))
  ([timeline objects assets]
   (timeline->lottie timeline objects assets nil))
  ([timeline objects assets {:keys [propagate]}]
   (lottie-parenting timeline objects assets propagate (parent-candidates timeline objects))))
