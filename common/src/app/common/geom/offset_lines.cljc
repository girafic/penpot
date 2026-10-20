;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.geom.offset-lines
  "Offset lines (Penpot Motion): parallel copies of the outline of a
  shape, `:count` of them `:spacing` px apart across it, from one side to
  the other, their colour and opacity going from those of the inner side
  to those of the outer one (the side the outline turns away from), as a
  light painting or an engraving does. A group holding them keeps how
  they are made (`:offset-lines`, see `cts/schema:offset-lines`) and its
  source, hidden; each layer says what it is (`:offset-line`, see
  `layers-of`), which a copy keeps. They are paths like any other, which
  every renderer and export draws and a trim of the group draws together
  or in turn."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.point :as gpt]
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.color :as clr]
   [app.common.types.path :as path]))

(def defaults
  "How offset lines are made at first: a band of 24 lines, 4 px apart,
  dim inside and bright outside."
  {:count 24 :spacing 4 :width 2.5
   :inner-color "#5a667e" :inner-opacity 0.3
   :outer-color "#ffffff" :outer-opacity 1})

(defn layers-of
  "The layers of the group of offset lines `group` of `objects`, `{:source
  id :lines [id...]}`: its child that is `:offset-line :source` and those
  that are `:line`, whatever else it holds; of a group made before its
  layers had their roles, its `:source` and every other child. Nil
  without a source among its children."
  [objects group]
  (let [children (:shapes group)
        role     #(dm/get-in objects [% :offset-line])
        source   (d/seek #(= :source (role %)) children)
        legacy   (dm/get-in group [:offset-lines :source])]
    (cond
      (some? source)
      {:source source :lines (filterv #(= :line (role %)) children)}

      (and (some? legacy) (some #{legacy} children) (not-any? role children))
      {:source legacy :lines (filterv #(not= legacy %) children)})))

(def ^:private smoothness
  "How far, px, the lines may leave the points they are fitted through."
  0.25)

(def ^:private max-miter
  "The most a corner lengthens the offset of a point by."
  3)

(defn offsets
  "The offsets of the lines of `spec`, px from the outline, outwards
  positive: centred on it."
  [{:keys [count spacing]}]
  (mapv #(* spacing (- % (/ (dec count) 2))) (range count)))

(defn- normal
  "The normal of the segment from `a` to `b`, of length 1, to its left as
  y goes down: outwards for an outline going clockwise."
  [a b]
  (let [dx  (- (:x b) (:x a))
        dy  (- (:y b) (:y a))
        len (mth/hypot dx dy)]
    (when (> len 1e-9)
      (gpt/point (/ (- dy) len) (/ dx len)))))

(defn- dedupe-points
  [points]
  (reduce (fn [acc p]
            (if (and (seq acc) (< (gpt/distance (peek acc) p) 1e-6)) acc (conj acc p)))
          []
          points))

(defn offset-polyline
  "The points of the polyline `points` (closed when `closed?`) moved `d`
  px along their normals, the normals of their two segments averaged (a
  corner lengthens it, up to `max-miter`)."
  [points closed? d]
  (let [pts (cond-> (dedupe-points points)
              (and closed? (> (count points) 2) (< (gpt/distance (first points) (peek (vec points))) 1e-6))
              pop)
        n   (count pts)]
    (if (< n 2)
      pts
      (mapv (fn [i]
              (let [p      (nth pts i)
                    prev   (cond (pos? i) (nth pts (dec i)) closed? (nth pts (dec n)))
                    next   (cond (< i (dec n)) (nth pts (inc i)) closed? (nth pts 0))
                    n1     (some-> prev (normal p))
                    n2     (some->> next (normal p))
                    [nx ny] (cond
                              (and n1 n2) [(+ (:x n1) (:x n2)) (+ (:y n1) (:y n2))]
                              n1          [(:x n1) (:y n1)]
                              n2          [(:x n2) (:y n2)]
                              :else       [0 0])
                    len    (mth/hypot nx ny)
                    ;; along the bisector, as far as the two segments
                    ;; are apart, at most `max-miter`
                    miter  (if (and n1 n2 (> len 1e-9))
                             (min max-miter (/ 2 len))
                             1)
                    k      (if (> len 1e-9) (/ (* d miter) len) 0)]
                (gpt/point (+ (:x p) (* nx k)) (+ (:y p) (* ny k)))))
            (range n)))))

(defn- signed-area
  "Twice the area of the polygon `pts`, its sign the way it goes round."
  [pts]
  (let [pts (vec pts)]
    (if (< (count pts) 3)
      0
      (reduce + (map (fn [a b] (- (* (:x a) (:y b)) (* (:x b) (:y a))))
                     pts (conj (subvec pts 1) (first pts)))))))

(defn- inside?
  "Whether the point `p` is inside the polygon `pts` (even-odd)."
  [p pts]
  (let [pts (vec pts)
        n   (count pts)]
    (loop [i 0 j (dec n) in? false]
      (if (= i n)
        in?
        (let [a (nth pts i)
              b (nth pts j)
              crosses? (and (not= (> (:y a) (:y p)) (> (:y b) (:y p)))
                            (< (:x p) (+ (:x a) (/ (* (- (:x b) (:x a)) (- (:y p) (:y a)))
                                                   (- (:y b) (:y a))))))]
          (recur (inc i) i (if crosses? (not in?) in?)))))))

(defn- closed-polyline?
  [{:keys [points closed?]}]
  (and closed? (> (count points) 2)))

(defn- holes
  "For each of `polylines`, the outline of a shape, whether it is a hole:
  closed, inside an odd number of the other closed ones."
  [polylines]
  (let [closed (filterv closed-polyline? polylines)]
    (mapv (fn [{:keys [points] :as line}]
            (and (closed-polyline? line)
                 (odd? (count (filter #(and (not (identical? % line)) (inside? (first points) (:points %)))
                                      closed)))))
          polylines)))

(defn- outward-sign
  "1 when the left (as y goes down) of `line`, a polyline of the outline of
  a shape, is outside the shape, -1 otherwise: a closed one by the way it
  goes round, turned for a `hole?`, whose inside is outside the shape; an
  open one 1."
  [{:keys [points] :as line} hole?]
  (if (closed-polyline? line)
    (cond-> (if (pos? (signed-area points)) -1 1) hole? -)
    1))

(defn- rings-kept
  "Of the rings `rings` of the closed polyline `points` at the offsets
  `ds` (ascending, outwards positive), which to keep: towards its own
  middle (inwards, outwards for a `hole?`) each must be smaller than the
  one before it, from the outline on; one that is not went past the
  middle (turned inside out, as a point reflection of a circle does), and
  so do all further in. The other way, all."
  [points hole? ds rings]
  (let [area   #(mth/abs (signed-area %))
        source (area points)
        inward (->> (map-indexed vector ds)
                    (filter (fn [[_ d]] (if hole? (pos? d) (neg? d))))
                    (sort-by (comp mth/abs second))
                    (map first))
        lost   (loop [prev source, [i & more] inward]
                 (if (nil? i)
                   #{}
                   (let [a (area (nth rings i))]
                     (if (and (< a (* prev (- 1 1e-6))) (> a (* 1e-3 source)))
                       (recur a more)
                       (set (cons i more))))))]
    (mapv #(not (contains? lost %)) (range (count ds)))))

(defn- mix
  "The colour `t` of the way from `a` to `b` (hex)."
  [a b t]
  (let [[r1 g1 b1] (clr/hex->rgb a)
        [r2 g2 b2] (clr/hex->rgb b)
        lerp       (fn [x y] (mth/round (+ x (* t (- y x)))))]
    (clr/rgb->hex [(lerp r1 r2) (lerp g1 g2) (lerp b1 b2)])))

(defn lines
  "The lines `spec` makes from the outline of `source`: `{:offset :content
  :color :opacity :width}` each, from the inner side to the outer one, its
  path content fitted smoothly through its points, closed where the
  outline is. The rings past the middle of a closed outline are left out
  (see `rings-kept`; a line of none left out too); a concave outline can
  still loop."
  [source spec]
  (let [spec      (merge defaults spec)
        polylines (gtl/outline-polylines source)
        ds        (offsets spec)
        n         (count ds)
        ;; for each polyline, its ring at each offset, or nil
        rings     (mapv (fn [{:keys [points closed?] :as line} hole?]
                          (let [sign  (outward-sign line hole?)
                                rings (mapv #(offset-polyline points closed? (* sign %)) ds)
                                kept  (if (closed-polyline? line)
                                        (rings-kept points hole? ds rings)
                                        (repeat true))]
                            (mapv (fn [ring kept?] (when (and kept? (> (count ring) 1)) ring)) rings kept)))
                        polylines (holes polylines))]
    (into []
          (keep-indexed
           (fn [i d]
             (let [t        (if (> n 1) (/ i (dec n)) 1)
                   subpaths (keep (fn [[line line-rings]]
                                    (when-let [pts (nth line-rings i)]
                                      (let [closed? (closed-polyline? line)]
                                        (cond-> (vec (path/smooth-points->content (cond-> pts closed? (conj (first pts)))
                                                                                  smoothness))
                                          closed? (conj {:command :close-path :params {}})))))
                                  (map vector polylines rings))]
               (when (seq subpaths)
                 {:offset  d
                  :content (path/content (into [] cat subpaths))
                  :color   (mix (:inner-color spec) (:outer-color spec) t)
                  :opacity (+ (:inner-opacity spec) (* t (- (:outer-opacity spec) (:inner-opacity spec))))
                  :width   (:width spec)}))))
          ds)))
