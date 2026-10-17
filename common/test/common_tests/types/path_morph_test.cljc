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
    (t/is (= [:move-to :curve-to :line-to :close-path] (commands a')))
    (t/is (= (points a) (points a')) "the line keeps its course")))

(t/deftest a-circle-and-a-star-match
  (let [c       (circle 100 100 50)
        s       (star 100 100)
        [c' s'] (morph/match [c s])]
    (t/is (morph/same-structure? c' s'))
    (t/is (= 10 (count (filter #{:curve-to} (commands c')))))
    (t/is (every? (fn [[x y]] (mth/close? 50 (mth/hypot (- x 100) (- y 100)) 0.5)) (points c'))
          "the circle split along itself")
    (t/is (= (set (points s)) (set (map (fn [[x y]] [x y]) (points s'))))
          "the star keeps its points")
    (t/is (some? (path/lerp c' s' 0.5)))))

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
    (t/is (< (reduce + (map d (points s') (points o'))) 60)
          "the points go the short way")))
