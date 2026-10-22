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
                           [-0.33359081484413355 0.14846894811857803 0.3850426690473119
                            0.7813117821716286 -0.03709335087579624]
                           (map #(noise/value 12345 %) [0 0.5 1.25 7.75 123.4]))))
  (t/is (every? true? (map #(mth/close? %1 %2 1e-12)
                           [-0.2305934528646234 0.1419604228999178 -0.31534610898169185]
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
