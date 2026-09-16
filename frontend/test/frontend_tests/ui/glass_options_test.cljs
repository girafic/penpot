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

(deftest create-glass-uses-the-defaults
  (let [a (glass/create-glass)
        b (glass/create-glass)]
    (is (ctsg/valid-glass? a))
    (is (not= (:id a) (:id b)))
    (is (= (dissoc a :id) ctsg/default-attrs))))
