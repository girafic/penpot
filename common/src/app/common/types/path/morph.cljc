;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.path.morph
  "Outlines that morph into each other (the `:content` keyframes of
  `app.common.types.animation`): `match` gives a row of path contents one
  structure, the same subpaths with the same segments and commands, so
  that `impl/lerp` takes each one into the next point by point."
  (:require
   [app.common.math :as mth]
   [app.common.types.path.impl :as impl]))

(def ^:private max-aligned-segments
  "The most segments of a subpath whose start `match` turns to its
  partner's: the search tries every start."
  1024)

(def ^:private max-even-segments
  "The most curves a subpath gets so that each of its outlines splits
  evenly (see `target-count`)."
  64)

(defn same-structure?
  "Whether the contents `a` and `b` have the same segments, with the same
  command each."
  [a b]
  (and (= (count a) (count b))
       (every? true? (map #(= (:command %1) (:command %2)) a b))))

(defn- seg-x [segment] (double (get-in segment [:params :x] 0)))
(defn- seg-y [segment] (double (get-in segment [:params :y] 0)))

(defn- point-of
  [segment]
  [(seg-x segment) (seg-y segment)])

(defn- subpaths
  "The subpaths of `content` that draw something: `{:start [x y]
  :segments [...] :closed? bool}`, each segment a `:line-to` or a
  `:curve-to` to its point. A segment after a close path starts from the
  start of the subpath it closed, as SVG draws it."
  [content]
  (let [done (fn [acc sub] (cond-> acc (seq (:segments sub)) (conj sub)))]
    (loop [segments (seq content) sub nil start [0.0 0.0] acc []]
      (if-let [segment (first segments)]
        (case (:command segment)
          :move-to
          (let [p (point-of segment)]
            (recur (rest segments) {:start p :segments [] :closed? false} p (done acc sub)))

          :close-path
          (recur (rest segments) nil start (done acc (some-> sub (assoc :closed? true))))

          (:line-to :curve-to)
          (recur (rest segments)
                 (update (or sub {:start start :segments [] :closed? false}) :segments conj segment)
                 start
                 acc)

          (recur (rest segments) sub start acc))
        (done acc sub)))))

(defn- to-content
  "Path content of the subpaths `subs`."
  [subs]
  (impl/from-plain
   (into []
         (mapcat (fn [{:keys [start segments closed?]}]
                   (cond-> (into [{:command :move-to :params {:x (start 0) :y (start 1)}}] segments)
                     closed? (conj {:command :close-path :params {}}))))
         subs)))

(defn- curve-from
  "The segment `segment`, from the point `p`, as a curve: a line keeps its
  course, its handles a third of the way from each end."
  [[x0 y0] segment]
  (if (= :curve-to (:command segment))
    segment
    (let [x (seg-x segment) y (seg-y segment)]
      {:command :curve-to
       :params {:x x :y y
                :c1x (+ x0 (/ (- x x0) 3.0)) :c1y (+ y0 (/ (- y y0) 3.0))
                :c2x (+ x0 (* 2 (/ (- x x0) 3.0))) :c2y (+ y0 (* 2 (/ (- y y0) 3.0)))}})))

(defn- curves
  "The segments of the subpath `sub` as curves `[c1 c2 to]`, a closed one
  with a curve back to its start when its last point is elsewhere."
  [{:keys [start segments closed?]}]
  (let [[out last-point]
        (reduce (fn [[out p] segment]
                  (let [{:keys [c1x c1y c2x c2y x y]} (:params (curve-from p segment))]
                    [(conj out [[c1x c1y] [c2x c2y] [x y]]) [x y]]))
                [[] start]
                segments)
        away? (> (+ (mth/abs (- (last-point 0) (start 0))) (mth/abs (- (last-point 1) (start 1)))) 1e-6)]
    (if (and closed? away?)
      (let [{:keys [c1x c1y c2x c2y]} (:params (curve-from last-point {:command :line-to
                                                                       :params {:x (start 0) :y (start 1)}}))]
        (conj out [[c1x c1y] [c2x c2y] start]))
      out)))

(defn- mix
  [[x0 y0] [x1 y1] t]
  [(+ x0 (* (- x1 x0) t)) (+ y0 (* (- y1 y0) t))])

(defn- split-curve
  "The curve `[c1 c2 to]` from `p` split at `t`: the two curves."
  [p [c1 c2 to] t]
  (let [p01  (mix p c1 t)
        p12  (mix c1 c2 t)
        p23  (mix c2 to t)
        p012 (mix p01 p12 t)
        p123 (mix p12 p23 t)
        m    (mix p012 p123 t)]
    [[p01 p012 m] [p123 p23 to]]))

(defn- split-into
  "The curve `curve` from `p` as `n` curves of equal steps along it."
  [p curve n]
  (loop [p p curve curve n n out []]
    (if (<= n 1)
      (conj out curve)
      (let [[a b] (split-curve p curve (/ 1.0 n))]
        (recur (nth a 2) b (dec n) (conj out a))))))

(defn- distance
  [[x0 y0] [x1 y1]]
  (mth/hypot (- x1 x0) (- y1 y0)))

(defn- curve-length
  "About how long the curve `[c1 c2 to]` from `p` is: between its chord
  and its handles."
  [p [c1 c2 to]]
  (/ (+ (distance p to) (distance p c1) (distance c1 c2) (distance c2 to)) 2))

(defn- with-count
  "The curves `cs` of a subpath starting at `start` as `n` curves (at
  least as many as they are): the longer ones split into more, by their
  share of its length."
  [start cs n]
  (let [k (count cs)]
    (if (>= k n)
      cs
      (let [starts  (into [start] (map #(nth % 2)) (butlast cs))
            lengths (mapv curve-length starts cs)
            total   (reduce + lengths)
            extra   (- n k)
            shares  (mapv #(if (pos? total) (* extra (/ % total)) (/ extra (double k))) lengths)
            pieces  (mapv #(inc (long (mth/floor %))) shares)
            left    (- n (reduce + pieces))
            ;; the rest to those with the largest remainders, spread
            ;; evenly among those of one remainder (the sides of a star)
            groups  (->> (range k)
                         (group-by #(long (mth/round (* 1e6 (- (nth shares %) (mth/floor (nth shares %)))))))
                         (sort-by key >)
                         (map val))
            picks   (loop [groups groups left left out []]
                      (if (or (<= left 0) (empty? groups))
                        out
                        (let [g (first groups) m (count g) p (min left m)]
                          (recur (rest groups) (- left p) (into out (map #(nth g (quot (* % m) p))) (range p))))))
            pieces  (reduce #(update %1 %2 inc) pieces picks)]
        (into [] (mapcat (fn [p c m] (split-into p c m)) starts cs pieces))))))

(defn- target-count
  "How many curves a subpath of as many curves as `counts` (one for each
  outline, none where it lacks the subpath) gets in all: their least
  common multiple while it is small, so that each splits evenly (a circle
  of four and a star of ten as twenty each), or else the most."
  [counts]
  (let [counts (filter pos? counts)
        most   (reduce max 1 counts)
        gcd    (fn [a b] (if (zero? b) a (recur b (mod a b))))
        lcm    (reduce (fn [a b] (quot (* a b) (gcd a b))) 1 counts)]
    (if (<= lcm max-even-segments) lcm most)))

(defn- vertices
  "The start of each of the curves `cs` of a subpath starting at `start`."
  [start cs]
  (into [start] (map #(nth % 2)) (butlast cs)))

(defn- turned
  "The closed subpath of curves `cs` from `start` starting at its `r`th
  vertex, gone backwards when `back?`: `[start cs]`."
  [start cs r back?]
  (let [cs    (if back?
                (let [vs (vertices start cs)]
                  (into [] (map (fn [i] (let [[c1 c2 _] (nth cs i)] [c2 c1 (nth vs i)])))
                        (range (dec (count cs)) -1 -1)))
                cs)
        k     (count cs)
        r     (mod r k)
        cs    (into (subvec cs r) (subvec cs 0 r))
        start (if (zero? r) start (nth (nth cs (dec k)) 2))]
    [start cs]))

(defn- aligned
  "The closed subpath of curves `cs` from `start`, turned (and gone
  backwards when that fits better) to start where it is closest to the
  subpath of curves `ref-cs` from `ref-start`, of as many curves: the
  least squared distance between their vertices."
  [start cs ref-start ref-cs]
  (let [k (count cs)]
    (if (or (> k max-aligned-segments) (not= k (count ref-cs)) (< k 2))
      [start cs]
      (let [ref  (vertices ref-start ref-cs)
            cost (fn [[s c]]
                   (reduce + (map (fn [a b] (let [d (distance a b)] (* d d))) (vertices s c) ref)))]
        (->> (for [back? [false true] r (range k)] (turned start cs r back?))
             (apply min-key cost))))))

(defn- centre
  [{:keys [start segments]}]
  (let [ps (cons start (map point-of segments))
        n  (count ps)]
    [(/ (reduce + (map first ps)) n) (/ (reduce + (map second ps)) n)]))

(defn- curve-content
  "Path content of subpaths given as `{:start :curves :closed?}`."
  [subs]
  (to-content (mapv (fn [{:keys [start curves closed?]}]
                      {:start start
                       :closed? closed?
                       :segments (mapv (fn [[[c1x c1y] [c2x c2y] [x y]]]
                                         {:command :curve-to
                                          :params {:c1x c1x :c1y c1y :c2x c2x :c2y c2y :x x :y y}})
                                       curves)})
                    subs)))

(defn match
  "`contents`, the outlines of a row of keyframes, all of one structure
  (see `same-structure?`), so that each one morphs into the next:
  - contents of one structure stay as they are, each point going to the
    point at its place (as Lottie and SMIL play them);
  - otherwise every segment becomes a curve; a subpath one of them lacks
    is a point at the middle of the one it has; a subpath open in one is
    open in all, a closed one going back to its start by a curve; each
    subpath has its longer curves split until it has as many as the
    others (see `target-count`); and a closed subpath that was not of one structure starts,
    forwards or backwards, where it is closest to the one before it (a
    square from a corner into a heart from its point does not twist)."
  [contents]
  (let [contents (vec contents)]
    (if (or (< (count contents) 2)
            (every? #(same-structure? (first contents) %) (rest contents)))
      contents
      (let [rows    (mapv subpaths contents)
            n       (apply max (map count rows))
            ;; the subpaths not of one structure in every content
            mixed   (into #{}
                          (filter (fn [i]
                                    (not (apply = (map #(some-> (get % i) ((juxt :closed? (comp (partial mapv :command) :segments))))
                                                       rows)))))
                          (range n))
            partner (fn [i] (some #(get % i) rows))
            open?   (fn [i] (some #(false? (:closed? (get % i))) rows))
            ;; each subpath as curves, a missing one as a point
            rows    (mapv (fn [subs]
                            (mapv (fn [i]
                                    (if-let [sub (get subs i)]
                                      {:start (:start sub) :curves (curves sub)
                                       :closed? (and (:closed? sub) (not (open? i)))}
                                      {:start (centre (partner i)) :curves []
                                       :closed? (and (:closed? (partner i)) (not (open? i)))}))
                                  (range n)))
                          rows)
            counts  (mapv (fn [i] (map #(count (:curves (nth % i))) rows)) (range n))
            ;; as many curves in each
            rows    (mapv (fn [subs]
                            (mapv (fn [i {:keys [start curves] :as sub}]
                                    (let [m (target-count (nth counts i))]
                                      (assoc sub :curves
                                             (if (empty? curves)
                                               (vec (repeat m [start start start]))
                                               (with-count start curves m)))))
                                  (range)
                                  subs))
                          rows)
            ;; the closed ones not of one structure start where they are
            ;; closest to the one before
            rows    (reduce (fn [out subs]
                              (let [prev (peek out)]
                                (conj out
                                      (mapv (fn [i {:keys [start curves closed?] :as sub}]
                                              (if (and closed? (some? prev) (contains? mixed i))
                                                (let [ref (nth prev i)
                                                      [s c] (aligned start curves (:start ref) (:curves ref))]
                                                  (assoc sub :start s :curves c))
                                                sub))
                                            (range)
                                            subs))))
                            []
                            rows)]
        (mapv curve-content rows)))))
