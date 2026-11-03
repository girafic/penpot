;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.tokens.motion-tokens-test
  "The motion token types (duration, cubic bezier, spring) in the tokens
  sidebar: tokens no shape takes, which presets, keyframes and the
  entries of animation styles bind."
  (:require
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.tokens-lib :as ctob]
   [app.main.data.tokenscript :as ts]
   [app.main.data.workspace.tokens.application :as dwta]
   [app.main.ui.workspace.timeline.easing :as easing]
   [app.main.ui.workspace.tokens.management.context-menu :as wtcm]
   [app.main.ui.workspace.tokens.management.forms.controls.utils :as csu]
   [app.main.ui.workspace.tokens.management.forms.spring :as spring-form]
   [cljs.test :as t :include-macros true]))

(def ^:private duration
  {:name "motion.fast" :type :duration :value "150ms"})

(def ^:private bezier
  {:name "ease.out" :type :cubic-bezier :value [0 0 0.58 1]})

(def ^:private spring
  {:name "spring.soft" :type :spring :value {:stiffness 120 :damping 14 :mass 1}})

(t/deftest motion-tokens-apply-to-no-shape
  (let [file   (-> (thf/sample-file :file-1)
                   (tho/add-group :group1)
                   (tho/add-rect :rect1)
                   (tho/add-frame :frame1)
                   (tho/add-text :text1 "Hello World!"))
        shapes (map #(ths/get-shape file %) [:frame1 :rect1 :group1 :text1])]
    (doseq [token [duration bezier spring]]
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

(t/deftest spring-tokens-show-their-numbers-as-text
  (let [token   (assoc spring :id (random-uuid) :resolved-value (:value spring))
        options @(csu/get-token-dropdown-options {:spring [token]} nil)
        option  (first (filter #(= :token (:type %)) options))]
    (t/is (= "120, 14, 1" (:value option)))
    (t/is (= "120, 14, 1" (:resolved-value option)))))

(t/deftest the-spring-form-reads-its-text
  (t/is (= {:stiffness 170 :damping 26 :mass 1} (spring-form/text->value "170, 26, 1")))
  (t/is (= {:stiffness 170 :damping 26 :mass 1} (spring-form/text->value " 170 26 1 ")))
  (t/is (= "{spring.soft}" (spring-form/text->value "{spring.soft}")) "a reference stays")
  (t/is (= "170, 26" (spring-form/text->value "170, 26")) "the rest, for the validation to refuse")
  (t/is (= "120, 14, 1" (spring-form/value->text (:value spring)))))

(t/deftest the-easing-of-a-keyframe-offers-curve-and-spring-tokens
  (let [tokens [{:name "ease.out"} {:name "spring.soft"}]]
    (t/is (= ["none" "ease.out" "spring.soft"] (map :value (easing/easing-token-options tokens nil))))
    (t/is (= ["none" "ease.out" "spring.soft" "ease.gone"]
             (map :value (easing/easing-token-options tokens "ease.gone")))
          "a bound token of no active set shows too")
    (t/is (= 3 (count (easing/easing-token-options tokens "ease.out"))))))

(defn- resolved
  "The resolved values of `tokens` (token params), by name, as tokenscript
  (the `:tokenscript` flag) resolves them."
  [& tokens]
  (-> (into {} (map (fn [token] [(:name token) (ctob/make-token token)])) tokens)
      (ts/resolve-tokens)
      (update-vals :resolved-value)))

(t/deftest durations-resolve-to-milliseconds-under-tokenscript
  (let [values (resolved duration
                         {:name "motion.ms" :type :duration :value "300ms"}
                         {:name "motion.s" :type :duration :value "0.2s"}
                         {:name "motion.ref" :type :duration :value "{motion.fast}"}
                         {:name "motion.twice" :type :duration :value "{motion.fast} * 2"}
                         {:name "motion.mixed" :type :duration :value "0.1s + 50ms"})]
    (t/is (= 150 (get values "motion.fast")))
    (t/is (= 300 (get values "motion.ms")))
    (t/is (= 200 (get values "motion.s")))
    (t/is (= 150 (get values "motion.ref")) "through a reference")
    (t/is (= 300 (some-> (get values "motion.twice") js/Math.round)) "an expression keeps the unit")
    (t/is (= 150 (some-> (get values "motion.mixed") js/Math.round)) "seconds and milliseconds together")))

(t/deftest a-cubic-bezier-resolves-to-its-four-numbers-under-tokenscript
  (let [values (resolved bezier {:name "ease.ui" :type :cubic-bezier :value "{ease.out}"})]
    (t/is (= [0 0 0.58 1] (get values "ease.out")))
    (t/is (= [0 0 0.58 1] (get values "ease.ui")) "through a reference")))
