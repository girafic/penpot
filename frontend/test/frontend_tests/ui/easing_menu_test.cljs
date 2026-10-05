;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.easing-menu-test
  "The entry of the easing menu of preset animations and style entries
  (`easing/easing-entry`)."
  (:require
   [app.common.types.animation :as cta]
   [app.main.ui.workspace.timeline.easing :as easing]
   [cljs.test :as t :include-macros true]))

(t/deftest the-menu-names-an-easing-by-its-curve-or-spring
  (t/testing "the easing of the type and direction"
    (t/is (= :auto (easing/easing-entry nil))))
  (t/testing "named curves, as keywords or as their curves"
    (t/is (= :ease (easing/easing-entry :ease)))
    (t/is (= :linear (easing/easing-entry {:type :bezier :curve [0.0 0.0 1.0 1.0]})))
    (t/is (= :ease-out-back (easing/easing-entry cta/overshoot-easing))
          "the overshoot of the old menu is the ease out back"))
  (t/testing "named springs"
    (t/is (= [:gentle :quick :bouncy :slow]
             (map (comp easing/easing-entry :easing) cta/spring-presets))))
  (t/testing "what no name has is custom"
    (t/is (= :custom-bezier (easing/easing-entry {:type :bezier :curve [0.2 0.7 0.3 1.0]})))
    (t/is (= :custom-spring (easing/easing-entry {:type :spring :stiffness 250 :damping 12 :mass 1})))))
