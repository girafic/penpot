;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.math-noise-test
  (:require
   [app.common.math :as mth]
   [app.common.math.noise :as noise]
   [clojure.test :as t]))

(def ^:private xs
  (map #(/ % 37.0) (range 0 2000)))

(t/deftest the-noise-of-a-seed-is-always-the-same
  (t/is (= (map #(noise/value 12345 %) xs) (map #(noise/value 12345 %) xs)))
  (t/is (not= (map #(noise/value 12345 %) xs) (map #(noise/value 12346 %) xs)))
  ;; the same values on every runtime: a few, by heart
  (t/is (every? true? (map #(mth/close? %1 %2 1e-12)
                           [0.07951916313718987 -0.45313056865779927 -0.9646847190405964
                            -0.11521289603615026 -0.4321475144457338]
                           (map #(noise/value 12345 %) [0 0.5 1.25 7.75 123.4]))))
  (t/is (every? true? (map #(mth/close? %1 %2 1e-12)
                           [-0.2789900883761145 -0.36197942572292785 -0.32255709479422906]
                           (map #(noise/fractal 99 % 3 8) [0.1 2.6 7.9])))))

(t/deftest the-noise-stays-within-one-and-moves-smoothly
  (let [values (map #(noise/fractal 77 % 3) xs)]
    (t/is (every? #(<= -1 % 1) values))
    (t/is (< 0.5 (- (reduce max values) (reduce min values))) "it spreads out")
    ;; no jump from a value to the next, 1/37 apart
    (t/is (every? #(< (mth/abs %) 0.35) (map - (rest values) values)))))

(t/deftest the-noise-of-a-period-repeats
  (doseq [x [0 0.3 1.7 4.2]]
    (t/is (mth/close? (noise/value 9 x 5) (noise/value 9 (+ x 5) 5) 1e-9))
    (t/is (mth/close? (noise/fractal 9 x 2 5) (noise/fractal 9 (+ x 5) 2 5) 1e-9))))

(defn- correlation
  "How much the numbers `a` and `b` go together, -1 to 1."
  [a b]
  (let [n      (count a)
        mean-a (/ (reduce + a) n)
        mean-b (/ (reduce + b) n)
        da     (map #(- % mean-a) a)
        db     (map #(- % mean-b) b)]
    (/ (reduce + (map * da db))
       (mth/sqrt (* (reduce + (map * da da)) (reduce + (map * db db)))))))

(defn- whole-values
  "The noise of `seed` at the whole numbers 0 to 999: its random values."
  [seed]
  (mapv #(noise/value seed %) (range 1000)))

(t/deftest the-noise-is-random
  (doseq [seed [42 12345 7]]
    (let [values (whole-values seed)]
      (t/testing (str "seed " seed)
        ;; a hash linear in its input steps by the same two amounts only
        (t/is (< 2 (count (into #{} (map #(mth/round (* 1e9 (- %2 %1))) values (rest values)))))
              "its steps differ")
        (t/is (< (mth/abs (correlation (butlast values) (rest values))) 0.1)
              "a value tells nothing of the next")
        (t/is (< (mth/abs (correlation (drop-last 2 values) (drop 2 values))) 0.1)
              "nor of the one after")
        ;; the two of a position (see `cta/play-wiggles`), and an octave
        (t/is (< (mth/abs (correlation values (whole-values (inc seed)))) 0.2)
              "the next seed goes its own way")
        (t/is (< 1 (count (into #{} (map #(mth/round (* 1e9 (- %2 %1))) values (whole-values (inc seed))))))
              "not the same, shifted")
        (t/is (< (mth/abs (correlation values (whole-values (+ seed 7919)))) 0.2)
              "nor an octave")))))
