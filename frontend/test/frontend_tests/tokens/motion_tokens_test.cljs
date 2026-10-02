;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.tokens.motion-tokens-test
  "The motion token types (duration, cubic bezier) in the tokens sidebar:
  tokens no shape takes, which the entries of animation styles bind."
  (:require
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.shapes :as ths]
   [app.main.data.workspace.tokens.application :as dwta]
   [app.main.ui.workspace.tokens.management.context-menu :as wtcm]
   [app.main.ui.workspace.tokens.management.forms.controls.utils :as csu]
   [cljs.test :as t :include-macros true]))

(def ^:private duration
  {:name "motion.fast" :type :duration :value "150ms"})

(def ^:private bezier
  {:name "ease.out" :type :cubic-bezier :value [0 0 0.58 1]})

(t/deftest motion-tokens-apply-to-no-shape
  (let [file   (-> (thf/sample-file :file-1)
                   (tho/add-group :group1)
                   (tho/add-rect :rect1)
                   (tho/add-frame :frame1)
                   (tho/add-text :text1 "Hello World!"))
        shapes (map #(ths/get-shape file %) [:frame1 :rect1 :group1 :text1])]
    (doseq [token [duration bezier]]
      (t/is (empty? (wtcm/menu-actions {:token token :selected-shapes shapes}))
            (:name token))
      (let [props (dwta/get-token-properties token)]
        (t/is (some? (get-in props [:modal :key])) "they have a form")
        (t/is (nil? (:on-update-shape props)) "and update no shape")))))

(t/deftest bezier-tokens-show-their-numbers-as-text
  (let [token   (assoc bezier :id (random-uuid) :resolved-value [0 0 0.58 1])
        options @(csu/get-token-dropdown-options {:cubic-bezier [token]} nil)
        option  (first (filter #(= :token (:type %)) options))]
    (t/is (= "0, 0, 0.58, 1" (:value option)))
    (t/is (= "0, 0, 0.58, 1" (:resolved-value option)))))
