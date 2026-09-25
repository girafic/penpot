;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.ui.dynamic-modifiers-test
  (:require
   [app.main.ui.workspace.shapes.frame.dynamic-modifiers :as fdm]
   [cljs.test :as t :include-macros true]))

(t/deftest playback-frames-coalesce-onto-one-paint
  (let [gone         {:id :gone}
        returned     {:id :a}
        first-frame  {:base-node :node
                      :shapes [returned]
                      :removed [gone]
                      :transforms {:a 1}
                      :modifiers {:a :m1}}
        second-frame {:base-node :node
                      :shapes [{:id :b}]
                      :removed [returned]
                      :transforms {:b 2}
                      :modifiers {:b :m2}}
        back         {:base-node :node
                      :shapes [returned]
                      :removed []
                      :transforms {:a 3}
                      :modifiers {:a :m3}}
        pending      (-> nil
                         (fdm/merge-pending-transform first-frame)
                         (fdm/merge-pending-transform second-frame))]
    (t/is (= [{:id :b}] (:shapes pending)))
    (t/is (= {:b 2} (:transforms pending)))
    (t/is (= [gone returned] (:removed pending)))
    (let [pending (fdm/merge-pending-transform pending back)]
      (t/is (= [returned] (:shapes pending)))
      (t/is (= {:a 3} (:transforms pending)))
      (t/is (= [gone] (:removed pending))
            "a shape that returns before the paint keeps its new transform"))))
