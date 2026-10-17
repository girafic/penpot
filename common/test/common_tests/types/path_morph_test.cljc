;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.path-morph-test
  (:require
   [app.common.math :as mth]
   [app.common.types.path :as path]
   [app.common.types.path.morph :as morph]
   [clojure.test :as t]))

(defn- polygon
  "Closed path content of the corners `points`."
  [points & {:keys [open?]}]
  (path/from-plain
   (cond-> (into [{:command :move-to :params (zipmap [:x :y] (first points))}]
                 (map (fn [p] {:command :line-to :params (zipmap [:x :y] p)}))
                 (rest points))
     (not open?) (conj {:command :close-path :params {}}))))

(defn- circle
  "A circle of radius `r` around `cx`, `cy`, as four curves."
  [cx cy r]
  (let [k (* r 0.5523)]
    (path/from-plain
     [{:command :move-to :params {:x (+ cx r) :y cy}}
      {:command :curve-to :params {:c1x (+ cx r) :c1y (+ cy k) :c2x (+ cx k) :c2y (+ cy r) :x cx :y (+ cy r)}}
      {:command :curve-to :params {:c1x (- cx k) :c1y (+ cy r) :c2x (- cx r) :c2y (+ cy k) :x (- cx r) :y cy}}
      {:command :curve-to :params {:c1x (- cx r) :c1y (- cy k) :c2x (- cx k) :c2y (- cy r) :x cx :y (- cy r)}}
      {:command :curve-to :params {:c1x (+ cx k) :c1y (- cy r) :c2x (+ cx r) :c2y (- cy k) :x (+ cx r) :y cy}}
      {:command :close-path :params {}}])))

(defn- star
  "A star of five points around `cx`, `cy`, as ten lines."
  [cx cy]
  (polygon (mapv (fn [i]
                   (let [a (mth/radians (* i 36))
                         r (if (even? i) 50 20)]
                     [(+ cx (* r (mth/cos a))) (+ cy (* r (mth/sin a)))]))
                 (range 10))))

(defn- points
  [content]
  (keep #(when-let [x (get-in % [:params :x])] [x (get-in % [:params :y])]) (vec content)))

(defn- commands
  [content]
  (mapv :command (vec content)))

(t/deftest outlines-of-one-structure-lerp-point-by-point
  (let [a (polygon [[0 0] [10 0] [0 10]])
        b (polygon [[10 10] [30 10] [10 50]])]
    (t/is (= [[5.0 5.0] [20.0 5.0] [5.0 30.0]] (vec (take 3 (points (path/lerp a b 0.5))))))
    (t/is (nil? (path/lerp a (polygon [[0 0] [10 0] [0 10] [5 5]]) 0.5)) "of other structures, none")))

(t/deftest outlines-of-one-structure-stay-as-they-are
  (let [row [(polygon [[0 0] [10 0] [0 10]]) (polygon [[0 10] [0 0] [10 0]])]]
    (t/is (every? true? (map identical? row (morph/match row))) "each point still goes to the one at its place")))

(t/deftest a-line-and-a-curve-at-one-place-become-curves
  (let [a (polygon [[0 0] [10 0] [0 10]])
        b (path/from-plain [{:command :move-to :params {:x 0 :y 0}}
                            {:command :curve-to :params {:c1x 3 :c1y -3 :c2x 7 :c2y -3 :x 10 :y 0}}
                            {:command :line-to :params {:x 0 :y 10}}
                            {:command :close-path :params {}}])
        [a' b'] (morph/match [a b])]
    (t/is (morph/same-structure? a' b'))
    (t/is (every? #{:move-to :curve-to :close-path} (commands a')) "all curves")
    (t/is (= (set (points a)) (set (points a'))) "the lines keep their course")))

(t/deftest a-square-into-a-heart-does-not-twist
  ;; as many segments, but lines from a corner into curves from the point
  ;; of the heart: the heart starts where it is closest to the square
  (let [square (polygon [[0 0] [100 0] [100 100] [0 100]])
        heart  (path/from-plain [{:command :move-to :params {:x 50 :y 100}}
                                 {:command :curve-to :params {:c1x 30 :c1y 80 :c2x 0 :c2y 60 :x 0 :y 30}}
                                 {:command :curve-to :params {:c1x 0 :c1y 0 :c2x 40 :c2y 0 :x 50 :y 20}}
                                 {:command :curve-to :params {:c1x 60 :c1y 0 :c2x 100 :c2y 0 :x 100 :y 30}}
                                 {:command :curve-to :params {:c1x 100 :c1y 60 :c2x 70 :c2y 80 :x 50 :y 100}}
                                 {:command :close-path :params {}}])
        [s' h'] (morph/match [square heart])
        d       (fn [[x0 y0] [x1 y1]] (mth/hypot (- x1 x0) (- y1 y0)))]
    (t/is (morph/same-structure? s' h'))
    (t/is (< (reduce + (map d (points s') (points h')))
             (reduce + (map d (points square) (points heart))))
          "closer than point by point from where each starts")))

(t/deftest a-circle-and-a-star-match
  (let [c       (circle 100 100 50)
        s       (star 100 100)
        [c' s'] (morph/match [c s])]
    (t/is (morph/same-structure? c' s'))
    (t/is (= 20 (count (filter #{:curve-to} (commands c'))))
          "four and ten as twenty, so that each splits evenly")
    (t/is (every? (fn [[x y]] (mth/close? 50 (mth/hypot (- x 100) (- y 100)) 0.5)) (points c'))
          "the circle split along itself")
    (t/is (every? (set (points s')) (points s))
          "the star keeps its points")
    (t/is (some? (path/lerp c' s' 0.5)))))

(t/deftest the-points-added-to-equal-sides-spread-evenly
  ;; a line of seven equal steps against one of eleven (no small multiple
  ;; of both): the four new points go to steps spread along it, not to
  ;; the first four
  (let [seven   (polygon (mapv (fn [i] [(* 10 i) 0]) (range 8)) :open? true)
        eleven  (polygon (mapv (fn [i] [(* 7 i) 20]) (range 12)) :open? true)
        [s' _]  (morph/match [seven eleven])]
    (t/is (= [0 5 10 15 20 30 35 40 50 55 60 70]
             (mapv (fn [[x _]] (mth/round x)) (points s'))))))

(t/deftest a-star-spinning-by-its-points-keeps-spinning-past-another-structure
  ;; a circle, then a star, then the star with its points shifted by two:
  ;; the two stars are of one structure, so each point of the first goes
  ;; to the point of the second at its place, as without the circle
  (let [s       (star 100 100)
        spun    (path/from-plain (let [pts   (mapv :params (subvec (vec s) 0 10))
                                       moved (vec (concat (drop 2 pts) (take 2 pts)))]
                                   (-> [{:command :move-to :params (first moved)}]
                                       (into (map (fn [p] {:command :line-to :params p})) (rest moved))
                                       (conj {:command :close-path :params {}}))))
        [_ s' t'] (morph/match [(circle 100 100 50) s spun])
        at      (fn [content p] (first (keep-indexed (fn [j q] (when (and (mth/close? (first p) (first q) 1e-6)
                                                                          (mth/close? (second p) (second q) 1e-6))
                                                                 j))
                                                     (points content))))]
    (t/is (morph/same-structure? s' t'))
    (doseq [v (range 10)]
      (let [j       (at s' (nth (points s) v))
            [x y]   (nth (points spun) v)
            [x' y'] (when (some? j) (nth (points t') j))]
        (t/is (some? j))
        (t/is (and (some? x') (mth/close? x x' 1e-6) (mth/close? y y' 1e-6))
              (str "the point " v " of the star goes to the point " v " of the spun one"))))))

(defn- subpath-areas
  "The signed area of each subpath of `content` (of the polygon of its
  points and handles)."
  [content]
  (let [subs (reduce (fn [subs {:keys [command params]}]
                       (case command
                         :move-to    (conj subs [[(:x params) (:y params)]])
                         :close-path subs
                         (update subs (dec (count subs)) into
                                 (cond-> []
                                   (some? (:c1x params)) (conj [(:c1x params) (:c1y params)] [(:c2x params) (:c2y params)])
                                   :always (conj [(:x params) (:y params)])))))
                     []
                     (vec content))]
    (mapv (fn [ps]
            (/ (reduce + (map (fn [[x0 y0] [x1 y1]] (- (* x0 y1) (* x1 y0))) ps (concat (rest ps) [(first ps)]))) 2))
          subs)))

(defn- with-hole
  "A square with a square hole, the hole going round the other way."
  []
  (path/from-plain (into (vec (polygon [[0 0] [100 0] [100 100] [0 100]]))
                         (vec (polygon [[30 30] [30 70] [70 70] [70 30]])))))

(t/deftest a-hole-stays-a-hole-as-it-grows
  ;; the hole of the square goes round against its outline (nonzero),
  ;; through the morph and whichever way the shape before it goes
  (let [hole? (fn [content]
                (let [[outer inner] (subpath-areas content)]
                  (or (mth/close? 0 inner 1e-6) (neg? (* outer inner)))))]
    (doseq [[label row] [["a triangle into it" [(polygon [[0 0] [100 0] [50 100]]) (with-hole)]]
                         ["a triangle the other way into it" [(polygon [[0 0] [50 100] [100 0]]) (with-hole)]]
                         ["it, the square, it" [(with-hole) (polygon [[0 0] [100 0] [100 100] [0 100]]) (with-hole)]]]]
      (let [matched (morph/match row)]
        (t/is (every? hole? matched) label)
        (t/is (every? true? (map #(some? (path/lerp %1 %2 0.5)) matched (rest matched))) label)))))

(t/deftest outlines-of-one-structure-pair-up-with-unequal-sides
  ;; two rectangles of one structure, one wide and one tall, after a
  ;; triangle: split alike, each corner of one still goes to the corner
  ;; of the other at its place
  (let [wide    (polygon [[0 0] [100 0] [100 20] [0 20]])
        tall    (polygon [[0 0] [20 0] [20 100] [0 100]])
        [_ w' t'] (morph/match [(polygon [[0 0] [60 0] [30 60]]) wide tall])
        at      (fn [content [x y]]
                  (first (keep-indexed (fn [j [qx qy]] (when (and (mth/close? x qx 1e-6) (mth/close? y qy 1e-6)) j))
                                       (points content))))]
    (doseq [v (range 4)]
      (let [j        (at w' (nth (points wide) v))
            [x y]    (nth (points tall) v)
            [x' y'] (when (some? j) (nth (points t') j))]
        (t/is (some? j))
        (t/is (and (some? x') (mth/close? x x' 1e-6) (mth/close? y y' 1e-6))
              (str "the corner " v " of the wide one goes to the corner " v " of the tall one"))))))

(defn- corners-in-order
  "The corners `corners` (`[x y]`) in the order the matched `content`
  passes them."
  [content corners]
  (into []
        (comp (map (fn [[x y]] (some (fn [[cx cy :as c]] (when (and (mth/close? x cx 1e-6) (mth/close? y cy 1e-6)) c)) corners)))
              (remove nil?)
              (dedupe))
        (points content)))

(t/deftest a-fixed-keyframe-pairs-first-point-to-first-point
  ;; a triangle into a square drawn from its far corner: matched on its
  ;; own the square starts near the triangle's start, fixed it starts
  ;; where it was drawn from
  (let [tri      (polygon [[0 0] [100 0] [0 100]])
        from-c   (polygon [[100 100] [0 100] [0 0] [100 0]])
        corners  [[0 0] [100 0] [100 100] [0 100]]
        [_ auto] (morph/match [tri from-c])
        [_ fixed] (morph/match [tri from-c] [false true])]
    (t/is (not= [100.0 100.0] (mapv double (first (points auto)))) "on its own, turned to fit")
    (t/is (= [100.0 100.0] (mapv double (first (points fixed)))) "fixed, from where it was drawn")
    (t/is (= [[100 100] [0 100] [0 0] [100 0]] (take 4 (corners-in-order fixed corners))) "and the way it goes round"))
  (let [tri      (polygon [[0 0] [100 0] [0 100]])
        back     (polygon [[100 100] [100 0] [0 0] [0 100]])
        [_ fixed] (morph/match [tri back] [false true])]
    (t/is (= [[100 100] [100 0] [0 0] [0 100]] (take 4 (corners-in-order fixed [[0 0] [100 0] [100 100] [0 100]])))
          "the other way round, as it was drawn"))
  (let [a (polygon [[0 0] [10 0] [0 10]])
        b (polygon [[0 10] [0 0] [10 0]])]
    (t/is (every? true? (map identical? [a b] (morph/match [a b] [false true])))
          "of one structure, as they are")))

(t/deftest close-contents-are-of-one-structure-within-a-tolerance
  (let [a (polygon [[0 0] [10 0] [0 10]])]
    (t/is (morph/close? a (polygon [[0 0.004] [10 0] [0 10]]) 0.01))
    (t/is (not (morph/close? a (polygon [[0 0.5] [10 0] [0 10]]) 0.01)))
    (t/is (not (morph/close? a (polygon [[0 0] [10 0] [0 10] [0 5]]) 0.01)) "not of one structure")))

(t/deftest an-open-line-and-a-circle-match
  (let [line    (polygon [[50 100] [150 100]] :open? true)
        [l' c'] (morph/match [line (circle 100 100 50)])]
    (t/is (morph/same-structure? l' c'))
    (t/is (not (some #{:close-path} (commands c'))) "open in one, open in both")))

(t/deftest a-missing-subpath-grows-from-a-point
  (let [two     (path/from-plain (into (vec (polygon [[0 0] [10 0] [0 10]])) (vec (polygon [[50 50] [60 50] [50 60]]))))
        one     (polygon [[0 0] [10 0] [0 10]])
        [t' o'] (morph/match [two one])]
    (t/is (morph/same-structure? t' o'))
    (let [second-sub (drop 4 (points o'))]
      (t/is (apply = second-sub) "a point")
      (t/is (every? (fn [[x y]] (and (mth/close? x (/ 160 3.0) 0.01) (mth/close? y (/ 160 3.0) 0.01))) second-sub)
            "at the middle of the one it has"))))

(t/deftest a-split-closed-outline-starts-where-it-is-closest
  (let [square  (polygon [[0 0] [100 0] [100 100] [0 100]])
        ;; the same square from another corner, with a point more
        other   (polygon [[100 100] [0 100] [0 0] [50 0] [100 0]])
        [s' o'] (morph/match [square other])
        d       (fn [[x0 y0] [x1 y1]] (mth/hypot (- x1 x0) (- y1 y0)))]
    (t/is (morph/same-structure? s' o'))
    (t/is (< (/ (reduce + (map d (points s') (points o'))) (count (points s'))) 10)
          "the points go the short way")))
