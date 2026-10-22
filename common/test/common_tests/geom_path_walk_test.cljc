;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.geom-path-walk-test
  (:require
   [app.common.geom.path-walk :as gpw]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.path.helpers :as path.helpers]
   [app.common.types.shape :as cts]
   [clojure.test :as t]))

(defn- path-shape
  [d]
  (cts/setup-shape {:type :path :content (path/from-string d)}))

(defn- near?
  ([expected actual] (near? expected actual 0.01))
  ([expected actual tol] (mth/close? (double expected) (double actual) tol)))

(defn- at
  [walk share]
  ((:at walk) share))

(t/deftest a-line-is-walked-evenly
  (let [walk (gpw/walker (path-shape "M 0 0 L 100 0 L 100 50"))]
    (t/is (near? 150 (:length walk)))
    (t/is (false? (:closed? walk)))
    (t/is (near? 50 (get-in (at walk (/ 1 3)) [:point :x])))
    (t/is (near? 0 (:angle (at walk 0.2))) "to the right")
    (t/is (near? 90 (:angle (at walk 0.9))) "then down")
    (t/is (= (at walk 1) (at walk 3)) "kept within the way")))

(t/deftest a-rounded-rect-is-walked-round-its-corners
  (let [rect (cts/setup-shape {:type :rect :x 0 :y 0 :width 200 :height 100 :r1 20 :r2 20 :r3 20 :r4 20})
        walk (gpw/walker rect)
        ;; four sides less the corners, and four quarter circles
        perimeter (+ (* 2 (- 200 40)) (* 2 (- 100 40)) (* 2 mth/PI 20))]
    (t/is (near? perimeter (:length walk) (* 0.005 perimeter)))
    (t/is (true? (:closed? walk)))
    (t/testing "turned with the shape"
      (let [turned (gsh/transform-shape rect (ctm/rotation (ctm/empty) (gpt/point 100 50) 90))
            twalk  (gpw/walker turned)]
        (t/is (near? (:length walk) (:length twalk) 0.01))
        (t/is (near? (mod (+ 90 (:angle (at walk 0.1))) 360) (mod (:angle (at twalk 0.1)) 360) 0.5))))))

(t/deftest an-ellipse-is-walked-smoothly
  (let [walk   (gpw/walker (cts/setup-shape {:type :circle :x 0 :y 0 :width 300 :height 100}))
        a 150 b 50
        ;; Ramanujan's perimeter
        perimeter (* mth/PI (- (* 3 (+ a b)) (mth/sqrt (* (+ (* 3 a) b) (+ a (* 3 b))))))
        angles (map #(:angle (at walk (/ % 1000))) (range 1001))]
    (t/is (near? perimeter (:length walk) (* 0.005 perimeter)))
    ;; at its narrow ends it turns 2.3 degrees in a thousandth of the way
    (t/is (every? #(< (mth/abs %) 3) (map - (rest angles) angles)) "no jump of the direction")
    (t/is (near? 360 (mth/abs (- (last angles) (first angles))) 1) "once round, turning on")))

(t/deftest a-curve-goes-as-its-tangent
  (let [shape  (path-shape "M 0 0 C 0 100 100 100 100 0")
        walk   (gpw/walker shape)
        bezier [(gpt/point 0 0) (gpt/point 100 0) (gpt/point 0 100) (gpt/point 100 100)]
        start  (path.helpers/curve-tangent bezier 0)]
    (t/is (near? 90 (:angle (at walk 0)) 0.5) "down at its start")
    (t/is (near? (mth/degrees (mth/atan2 (:y start) (:x start))) (:angle (at walk 0)) 0.5))
    (t/is (near? 0 (:angle (at walk 0.5)) 1) "across at its middle")
    (t/is (near? 50 (get-in (at walk 0.5) [:point :x]) 0.5))))

(t/deftest only-outlines-are-walked
  (t/is (nil? (gpw/walker (cts/setup-shape {:type :text :x 0 :y 0 :width 10 :height 10}))))
  (t/is (nil? (gpw/walker (cts/setup-shape {:type :group :x 0 :y 0 :width 10 :height 10}))))
  (t/is (some? (gpw/walker (cts/setup-shape {:type :frame :x 0 :y 0 :width 10 :height 10}))) "a board, as a rect"))
