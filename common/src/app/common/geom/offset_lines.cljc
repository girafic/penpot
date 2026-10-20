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

(defn- clean-points
  "The points of the polyline `points` (closed when `closed?`) its offsets
  move, point for point (see `offset-polyline`): without repeats, a closed
  one without its last point back on its first."
  [points closed?]
  (cond-> (dedupe-points points)
    (and closed? (> (count points) 2) (< (gpt/distance (first points) (peek (vec points))) 1e-6))
    pop))

(defn offset-polyline
  "The points of the polyline `points` (closed when `closed?`) moved `d`
  px along their normals, the normals of their two segments averaged (a
  corner lengthens it, up to `max-miter`)."
  [points closed? d]
  (let [pts (clean-points points closed?)
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

(def ^:private cell
  "The size, px, of the cells `segments` sorts the segments of an outline
  into."
  32)

(defn- cell-key
  [col row]
  (+ (* col 1000003) row))

(defn- segments
  "The segments of `polylines`, `[ax ay bx by a-end? b-end?]` each (an
  end of an open polyline at `a` or `b`), sorted into the `cell`s of the
  page their boxes reach (by `cell-key`), for `near?`."
  [polylines]
  (let [cell-of #(long (mth/floor (/ % cell)))]
    (persistent!
     (reduce (fn [grid [ax ay bx by :as seg]]
               (reduce (fn [grid key] (assoc! grid key (conj (get grid key []) seg)))
                       grid
                       (for [cx (range (cell-of (min ax bx)) (inc (cell-of (max ax bx))))
                             cy (range (cell-of (min ay by)) (inc (cell-of (max ay by))))]
                         (cell-key cx cy))))
             (transient {})
             (mapcat (fn [{:keys [points closed?]}]
                       (let [pts  (vec points)
                             segs (vec (partition 2 1 (cond-> pts closed? (conj (first pts)))))
                             last (dec (count segs))]
                         (map-indexed (fn [i [a b]]
                                        [(double (:x a)) (double (:y a)) (double (:x b)) (double (:y b))
                                         (and (not closed?) (= i 0)) (and (not closed?) (= i last))])
                                      segs)))
                     polylines)))))

(defn- near-segment?
  "Whether `px` `py` is nearer than the root of `l2` to the segment `seg`
  (see `segments`), but to an end of an open polyline, which a line may
  pass: the end of the bowl of a P, on the lines of its stem."
  [px py l2 [ax ay bx by a-end? b-end?]]
  (let [dx (- bx ax)
        dy (- by ay)
        sl (+ (* dx dx) (* dy dy))
        t  (if (zero? sl) 0 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) sl))]
    (if (or (and a-end? (<= t 0)) (and b-end? (>= t 1)))
      false
      (let [t  (mth/clamp t 0 1)
            ex (- px (+ ax (* t dx)))
            ey (- py (+ ay (* t dy)))]
        (< (+ (* ex ex) (* ey ey)) l2)))))

(defn- near?
  "Whether the point `p` is nearer than `limit` to one of the segments of
  `grid` (see `segments`): those of the cells it reaches that far."
  [p grid limit]
  (let [px (double (:x p))
        py (double (:y p))
        l2 (* limit limit)
        c0 (long (mth/floor (/ (- px limit) cell)))
        c1 (long (mth/floor (/ (+ px limit) cell)))
        r0 (long (mth/floor (/ (- py limit) cell)))
        r1 (long (mth/floor (/ (+ py limit) cell)))]
    (loop [cx c0 cy r0]
      (cond
        (> cx c1) false
        (> cy r1) (recur (inc cx) r0)
        (some #(near-segment? px py l2 %) (get grid (cell-key cx cy))) true
        :else (recur cx (inc cy))))))

(def ^:private kept-distance
  "The share of its offset a point of a line must keep from the outline:
  one that comes nearer runs into a part of the shape narrower than the
  lines go (a notch, a waist, a curve tighter than them), and is left
  out."
  0.9)

(defn- lerp-point
  [a b t]
  (gpt/point (+ (:x a) (* t (- (:x b) (:x a)))) (+ (:y a) (* t (- (:y b) (:y a))))))

(defn- dense-pairs
  "`[[point source-point own?]...]` along `ring`, the offset of the
  polyline `source` point for point (see `clean-points`), closed when
  `closed?`: its points (`own?`) and points between them, no more than
  `step` apart, each with the point of `source` it comes from."
  [ring source closed? step]
  (let [n    (count ring)
        segs (if closed? n (dec n))]
    (cond-> (into []
                  (mapcat (fn [k]
                            (let [k1 (mod (inc k) n)
                                  a  (nth ring k)
                                  b  (nth ring k1)
                                  m  (max 1 (mth/ceil (/ (gpt/distance a b) step)))]
                              (map (fn [i]
                                     (let [t (/ i m)]
                                       [(lerp-point a b t) (lerp-point (nth source k) (nth source k1) t) (zero? i)]))
                                   (range m)))))
                  (range segs))
      (not closed?) (conj [(peek ring) (peek source) true]))))

(defn- back?
  "Whether the step from the pair `[p s]` to `[q t]` (see `dense-pairs`)
  runs back against the step of their source from `s` to `t`."
  [[p s] [q t]]
  (neg? (+ (* (- (:x q) (:x p)) (- (:x t) (:x s))) (* (- (:y q) (:y p)) (- (:y t) (:y s))))))

(defn- halfway
  [[p s] [q t]]
  [(lerp-point p q 0.5) (lerp-point s t 0.5) true])

(defn- own-points
  "The points of `pairs` (see `dense-pairs`) a line is fitted through:
  those of its ring, and where it was cut or folded; all of them, `cut?`,
  near a cut, which keep the curve fitted through them on the straight
  steps across it."
  [pairs cut?]
  (let [n (count pairs)]
    (if cut?
      (mapv first pairs)
      (into [] (keep-indexed (fn [i [p _ own?]] (when (or own? (= i 0) (= i (dec n))) p))) pairs))))

(defn- bridged
  "`points`, a ring closed when `closed?`, with points every `step` px on
  the steps longer than that, where it was cut and joined again."
  [points closed? step]
  (let [steps (cond-> (vec (partition 2 1 points)) closed? (conj [(peek points) (first points)]))]
    (cond-> (into []
                  (mapcat (fn [[a b]]
                            (let [m (max 1 (mth/ceil (/ (gpt/distance a b) step)))]
                              (map #(lerp-point a b (/ % m)) (range m)))))
                  steps)
      (not closed?) (conj (peek points)))))

(defn- untwisted
  "`pairs` (see `dense-pairs`) without the steps that run back against
  their source, as a line does across a notch it bridges: each such step
  folded into one point halfway; when `closed?`, the step from the last
  back to the first too."
  [pairs closed?]
  (let [out (reduce (fn [out pair]
                      (if (and (seq out) (back? (peek out) pair))
                        (conj (pop out) (halfway (peek out) pair))
                        (conj out pair)))
                    []
                    pairs)]
    (if-not closed?
      out
      ;; around its start: the step from the last, then on to the second
      (loop [out out]
        (cond
          (<= (count out) 2)                 out
          (back? (peek out) (first out))     (recur (assoc (pop out) 0 (halfway (peek out) (first out))))
          (back? (first out) (second out))   (recur (into [(halfway (first out) (second out))] (subvec out 2)))
          :else                              out)))))

(defn- chained
  "`pieces` (each `pairs`, see `dense-pairs`), what is left of a closed
  ring, in its order, joined into rings: from the end of a piece on to the
  nearest start, of a piece left or of the ring it is on, which closes
  it. Across a notch the ring goes on with the next piece; around each
  side of a waist a piece closes on itself."
  [pieces]
  (let [dist #(gpt/distance (first (peek %1)) (first (first %2)))]
    (loop [rings [] ring (first pieces) left (vec (rest pieces))]
      (if (nil? ring)
        rings
        (let [own  (dist ring ring)
              [j d] (when (seq left)
                      (apply min-key second (map-indexed (fn [i piece] [i (dist ring piece)]) left)))]
          (if (and (some? j) (< d own))
            (recur rings (into ring (nth left j)) (into (subvec left 0 j) (subvec left (inc j))))
            (recur (conj rings ring) (first left) (vec (rest left)))))))))

(defn- kept-pieces
  "The pieces of `ring`, the offset `d` of the polyline `source` of an
  outline of `segments` (closed when `closed?`), that keep their distance
  from it (see `kept-distance`): `{:points :closed?}` each, the ring cut
  where it comes nearer, the pieces of a closed one joined into rings
  (see `chained`), without the steps that run back (see `untwisted`)."
  [ring source closed? d segments]
  (let [limit  (* kept-distance (mth/abs d))
        step   (max 2 (/ (mth/abs d) 3))
        pairs  (dense-pairs ring source closed? step)
        far?   (mapv (fn [[p]] (not (near? p segments limit))) pairs)
        cut?   (not-every? true? far?)
        n      (count pairs)
        ;; a closed ring from after a point left out, so that no piece wraps
        start  (if (and closed? cut?) (inc (d/index-of-pred far? false?)) 0)
        pieces (into []
                     (comp (partition-by #(nth far? %))
                           (filter #(nth far? (first %)))
                           (map #(mapv (partial nth pairs) %)))
                     (map #(mod (+ start %) n) (range n)))]
    (into []
          (comp (map #(own-points (untwisted % closed?) cut?))
                (filter #(> (count %) 1))
                (map #(cond-> % cut? (bridged closed? step)))
                (map (fn [points] {:points points :closed? closed?})))
          (if closed? (chained pieces) pieces))))

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
  (see `rings-kept`; a line of none left out too), and the parts of a line
  that come nearer the outline than it goes (see `kept-pieces`)."
  [source spec]
  (let [spec      (merge defaults spec)
        polylines (gtl/outline-polylines source)
        ds        (offsets spec)
        n         (count ds)
        ;; for each polyline, its ring at each offset, or nil
        sources   (mapv (fn [{:keys [points closed?]}] (clean-points points closed?)) polylines)
        segs      (segments polylines)
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
                   subpaths (for [[line source line-rings] (map vector polylines sources rings)
                                  :let  [ring (nth line-rings i)]
                                  :when (some? ring)
                                  {pts :points closed? :closed?} (kept-pieces ring source (closed-polyline? line) d segs)]
                              (cond-> (vec (path/smooth-points->content (cond-> pts closed? (conj (first pts)))
                                                                        smoothness))
                                closed? (conj {:command :close-path :params {}})))]
               (when (seq subpaths)
                 {:offset  d
                  :content (path/content (into [] cat subpaths))
                  :color   (mix (:inner-color spec) (:outer-color spec) t)
                  :opacity (+ (:inner-opacity spec) (* t (- (:outer-opacity spec) (:inner-opacity spec))))
                  :width   (:width spec)}))))
          ds)))
