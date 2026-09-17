;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.ui.glass-options-test
  (:require
   [app.common.types.shape.glass :as ctsg]
   [app.main.ui.workspace.sidebar.options.menus.glass :as glass]
   [cljs.test :refer [deftest is testing] :include-macros true]))

(deftest normalize-angle-wraps-into-range
  (is (= 0 (glass/normalize-angle 0)))
  (is (= -45 (glass/normalize-angle -45)))
  (is (= -170 (glass/normalize-angle 190)))
  (is (= 170 (glass/normalize-angle -190)))
  (is (= -180 (glass/normalize-angle 180)))
  (is (= 0 (glass/normalize-angle 720))))

(deftest point->angle-matches-the-renderer
  (testing "0 is up and angles turn clockwise"
    (is (= 0 (glass/point->angle 0 -10)))
    (is (= 90 (glass/point->angle 10 0)))
    (is (= -90 (glass/point->angle -10 0)))
    (is (= -180 (glass/point->angle 0 10))))

  (testing "the top-left corner is -45 degrees"
    (is (= -45 (glass/point->angle -10 -10)))))

(deftest angle->position-places-the-light-marker
  (testing "the style is a JS object, so React applies it"
    (is (object? (glass/angle->position 0))))

  (testing "0 degrees is above the center"
    (let [style (glass/angle->position 0)]
      (is (= "50%" (.-left style)))
      (is (= "14%" (.-top style)))))

  (testing "90 degrees is right of the center"
    (let [style (glass/angle->position 90)]
      (is (= "86%" (.-left style)))
      (is (< (abs (- 50 (js/parseFloat (.-top style)))) 0.001))))

  (testing "-45 degrees is top-left"
    (let [style (glass/angle->position -45)
          left  (js/parseFloat (.-left style))
          top   (js/parseFloat (.-top style))]
      (is (< 24 left 25))
      (is (< 24 top 25)))))

(deftest slider-fill-spans-from-the-origin
  (testing "without origin the fill starts at the minimum"
    (is (= [0 50] (glass/slider-fill 50 0 100 nil))))
  (testing "the fill goes from the origin to the value, in either direction"
    (is (= [50 75] (glass/slider-fill 150 0 200 100)))
    (is (= [25 50] (glass/slider-fill 50 0 200 100))))
  (testing "values outside the range are clamped"
    (is (= [0 100] (glass/slider-fill 400 0 100 nil)))))

(deftest advanced-block-opens-for-modified-values
  (is (false? (glass/advanced-modified? ctsg/default-attrs)))
  (is (false? (glass/advanced-modified?
               (apply dissoc ctsg/default-attrs (keys ctsg/optional-defaults)))))
  (is (true? (glass/advanced-modified? (assoc ctsg/default-attrs :saturation 120))))
  (is (true? (glass/advanced-modified? (assoc ctsg/default-attrs :texture :reeded))))
  (is (true? (glass/advanced-modified? (assoc ctsg/default-attrs :dispersion 0)))))

(deftest create-glass-uses-the-defaults
  (let [a (glass/create-glass)
        b (glass/create-glass)]
    (is (ctsg/valid-glass? a))
    (is (not= (:id a) (:id b)))
    (is (= (dissoc a :id) ctsg/default-attrs))))
