;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.geom.path-walk
  "Walking the outline of a shape, as a layer rides it (Penpot Motion):
  its length, and the point and the direction a share of the way along
  it. The outline is the exact one, where the shape is on the canvas:
  the content of a path or a boolean, the rounded corners of a rect, the
  curves of an ellipse, turned and flipped with the shape. Its curves are
  cut finely and measured, so a share of the way is a share of the
  length; their direction is the one of the curve, not of the cut. The
  pieces of an outline of several are walked one after the other, in
  their order, with no way between them."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.common.types.path :as path]
   [app.common.types.path.helpers :as path.helpers]))

(def ^:private curve-steps
  "Into how many pieces a curve of the outline is cut to measure it."
  32)

(def ^:private outlined-types
  #{:path :bool :rect :circle :frame})

(defn outline-content
  "The outline of `shape` where it is on the canvas, as path content; nil
  for a shape without one (a text, an image, a group)."
  [shape]
  (case (:type shape)
    (:path :bool) (:content shape)
    (when (contains? outlined-types (:type shape))
      (:content (path/convert-to-path (cond-> shape (= :frame (:type shape)) (assoc :type :rect)))))))

(defn- angle-of
  "The direction of the vector `v`, degrees clockwise from the x axis (y
  goes down); nil for none."
  [v]
  (when (and (some? v) (not (and (mth/almost-zero? (:x v)) (mth/almost-zero? (:y v)))))
    (mth/degrees (mth/atan2 (:y v) (:x v)))))

(defn- entry-samples
  "The points along the segment `entry` (see `path/segment-entries`),
  from its start, with the direction there: `[[point angle] ...]`."
  [entry]
  (let [[start end :as bezier] (path.helpers/entry->bezier entry)
        chord (or (angle-of (gpt/subtract end start)) 0)]
    (if (= :curve-to (get-in entry [:segment :command]))
      (mapv (fn [i]
              (let [t (/ i curve-steps)]
                [(path.helpers/curve-values bezier t)
                 (or (angle-of (path.helpers/curve-tangent bezier t)) chord)]))
            (range (inc curve-steps)))
      [[start chord] [end chord]])))

(defn- unwrapped
  "`angle` turned by whole turns to be the nearest to `previous`."
  [previous angle]
  (if (nil? previous)
    angle
    (+ angle (* 360 (mth/round (/ (- previous angle) 360))))))

;; the points of a walk, `[length point angle]`: the length of the way
;; to the point, and the direction there

(defn- table
  "The walk along the segment `entries`, its points in order: each
  segment starts with no way to it, at the end of the one before (where
  its direction may turn at once) or where a new piece starts; the
  angles turn on, never back by whole turns."
  [entries]
  (reduce (fn [rows entry]
            (reduce (fn [rows [i [point angle]]]
                      (let [[length previous previous-angle] (peek rows)
                            step (if (or (zero? i) (nil? previous)) 0 (gpt/distance previous point))]
                        (conj rows [(+ (or length 0) step) point (unwrapped previous-angle angle)])))
                    rows
                    (map-indexed vector (entry-samples entry))))
          []
          entries))

(defn- closed?
  "Whether the segment `entries` of an outline make one closed loop: a
  single piece that ends where it starts."
  [entries]
  (let [pieces (inc (count (filter (fn [[a b]] (> (gpt/distance (:to a) (:from b)) 0.01))
                                   (partition 2 1 entries))))]
    (and (= 1 pieces)
         (< (gpt/distance (:from (first entries)) (:to (peek entries))) 0.01))))

(defn- walk-at
  "The point and the direction `share` (0 to 1) of the way along the walk
  `rows`, `total` long: `{:point :angle}`."
  [rows total share]
  (let [target (* (mth/clamp share 0 1) total)
        last-i (dec (count rows))
        ;; the last point of the walk at or before the target
        i      (loop [lo 0 hi last-i]
                 (if (>= lo hi)
                   lo
                   (let [mid (quot (+ lo hi 1) 2)]
                     (if (<= (first (nth rows mid)) target) (recur mid hi) (recur lo (dec mid))))))
        [l0 p0 a0] (nth rows i)
        [l1 p1 a1] (nth rows (min (inc i) last-i))
        f      (if (> l1 l0) (/ (- target l0) (- l1 l0)) 0)]
    {:point (gpt/lerp p0 p1 f)
     :angle (+ a0 (* f (- a1 a0)))}))

(defn- walker*
  [shape]
  (when-let [content (outline-content shape)]
    (let [entries (vec (path/segment-entries content))]
      (when (seq entries)
        (let [rows  (table entries)
              total (first (peek rows))]
          (when (pos? total)
            {:length total
             :closed? (closed? entries)
             :at #(walk-at rows total %)}))))))

;; The walks of the shapes measured, gone with a shape no longer read: a
;; layer riding a path asks at every frame.
#?(:cljs
   (defonce ^:private walkers
     (js/WeakMap.)))

(defn walker
  "The outline of `shape` to walk (see `outline-content`): `{:length
  :closed? :at}`, `:at` a fn of a share of the way, 0 to 1, to `{:point
  :angle}`, the point there and the direction the outline goes on in,
  degrees clockwise from the x axis; `:closed?` when it is one loop. Nil
  without an outline, or one with no length."
  [shape]
  #?(:cljs (let [walk (.get walkers shape)]
             (if (some? walk)
               (when-not (= ::none walk) walk)
               (let [walk (walker* shape)]
                 (.set walkers shape (or walk ::none))
                 walk)))
     :clj (walker* shape)))
