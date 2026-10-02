;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.files.tokens-test
  (:require
   [app.common.files.tokens :as cfo]
   [app.common.schema :as sm]
   [clojure.test :as t]))

(t/deftest test-parse-token-value
  (t/testing "parses double from a token value"
    (t/is (= {:value 100.1 :unit nil} (cfo/parse-token-value "100.1")))
    (t/is (= {:value -9.0 :unit nil} (cfo/parse-token-value "-9"))))
  (t/testing "trims white-space"
    (t/is (= {:value -1.3 :unit nil} (cfo/parse-token-value "     -1.3   "))))
  (t/testing "parses unit: px"
    (t/is (= {:value 70.3 :unit "px"} (cfo/parse-token-value "     70.3px   "))))
  (t/testing "parses unit: %"
    (t/is (= {:value -10.0 :unit "%"} (cfo/parse-token-value "-10%"))))
  (t/testing "parses unit: px")
  (t/testing "returns nil for any invalid characters"
    (t/is (nil? (cfo/parse-token-value "     -1.3a   "))))
  (t/testing "doesnt accept invalid double"
    (t/is (nil? (cfo/parse-token-value ".3")))))

(t/deftest convert-dtcg-token-test
  (t/testing "keeps string scalar values untouched"
    (t/is (= {:name "spacing.16" :type :spacing :value "16"}
             (cfo/convert-dtcg-token {"name" "spacing.16" "type" "spacing" "value" "16"})))
    (t/is (= {:name "spacing.16" :type :spacing :value "16px"}
             (cfo/convert-dtcg-token {"name" "spacing.16" "type" "spacing" "value" "16px"}))))
  (t/testing "coerces numeric scalar values to strings"
    (t/is (= {:name "spacing.16" :type :spacing :value "16"}
             (cfo/convert-dtcg-token {"name" "spacing.16" "type" "spacing" "value" 16})))
    (t/is (= {:name "radius" :type :border-radius :value "4"}
             (cfo/convert-dtcg-token {"name" "radius" "type" "borderRadius" "value" 4})))))

(t/deftest token-applied-test
  (t/testing "matches passed token with `:token-attributes`"
    (t/is (true? (cfo/token-applied? {:name "a"} {:applied-tokens {:x "a"}} #{:x}))))
  (t/testing "doesn't match empty token"
    (t/is (nil? (cfo/token-applied? {} {:applied-tokens {:x "a"}} #{:x}))))
  (t/testing "does't match passed token `:id`"
    (t/is (nil? (cfo/token-applied? {:name "b"} {:applied-tokens {:x "a"}} #{:x}))))
  (t/testing "doesn't match passed `:token-attributes`"
    (t/is (nil? (cfo/token-applied? {:name "a"} {:applied-tokens {:x "a"}} #{:y})))))

(t/deftest shapes-ids-by-applied-attributes
  (t/testing "Returns set of matched attributes that fit the applied token"
    (let [attributes #{:x :y :z}
          shape-applied-x {:id "shape-applied-x"
                           :applied-tokens {:x "1"}}
          shape-applied-y {:id "shape-applied-y"
                           :applied-tokens {:y "1"}}
          shape-applied-x-y {:id "shape-applied-x-y"
                             :applied-tokens {:x "1" :y "1"}}
          shape-applied-none {:id "shape-applied-none"
                              :applied-tokens {}}
          shape-applied-all {:id "shape-applied-all"
                             :applied-tokens {:x "1" :y "1" :z "1"}}
          shape-ids (fn [& xs] (into #{} (map :id xs)))
          shapes [shape-applied-x
                  shape-applied-y
                  shape-applied-x-y
                  shape-applied-all
                  shape-applied-none]
          expected (cfo/shapes-ids-by-applied-attributes {:name "1"} shapes attributes)]
      (t/is (= (:x expected) (shape-ids shape-applied-x
                                        shape-applied-x-y
                                        shape-applied-all)))
      (t/is (= (:y expected) (shape-ids shape-applied-y
                                        shape-applied-x-y
                                        shape-applied-all)))
      (t/is (= (:z expected) (shape-ids shape-applied-all)))
      (t/is (true? (cfo/shapes-applied-all? expected (shape-ids shape-applied-all) attributes)))
      (t/is (false? (cfo/shapes-applied-all? expected (apply shape-ids shapes) attributes)))
      (shape-ids shape-applied-x
                 shape-applied-x-y
                 shape-applied-all))))

(t/deftest tokens-applied-test
  (t/testing "is true when single shape matches the token and attributes"
    (t/is (true? (cfo/shapes-token-applied? {:name "a"} [{:applied-tokens {:x "a"}}
                                                         {:applied-tokens {:x "b"}}]
                                            #{:x}))))
  (t/testing "is false when no shape matches the token or attributes"
    (t/is (nil? (cfo/shapes-token-applied? {:name "a"} [{:applied-tokens {:x "b"}}
                                                        {:applied-tokens {:x "b"}}]
                                           #{:x})))
    (t/is (nil? (cfo/shapes-token-applied? {:name "a"} [{:applied-tokens {:x "a"}}
                                                        {:applied-tokens {:x "a"}}]
                                           #{:y})))))

(t/deftest parse-duration-value-test
  (t/testing "a number of ms or s, ms without a unit"
    (t/is (= 150.0 (cfo/parse-duration-value "150ms")))
    (t/is (= 200.0 (cfo/parse-duration-value " 0.2s ")))
    (t/is (= 150.0 (cfo/parse-duration-value "150")))
    (t/is (= 150 (cfo/parse-duration-value 150))))
  (t/testing "nothing else"
    (t/is (nil? (cfo/parse-duration-value "-5ms")))
    (t/is (nil? (cfo/parse-duration-value -1)))
    (t/is (nil? (cfo/parse-duration-value "fast")))
    (t/is (nil? (cfo/parse-duration-value "150px")))))

(t/deftest parse-cubic-bezier-value-test
  (t/testing "four numbers, as numbers or texts"
    (t/is (= [0.42 0 0.58 1] (cfo/parse-cubic-bezier-value [0.42 0 0.58 1])))
    (t/is (= [0.42 0.0 0.58 1.0] (cfo/parse-cubic-bezier-value "0.42, 0, 0.58, 1")))
    (t/is (= [0.42 0.0 0.58 1.0] (cfo/parse-cubic-bezier-value ["0.42" "0" "0.58" "1"])))
    (t/is (= [0.3 -0.5 0.7 1.5] (cfo/parse-cubic-bezier-value [0.3 -0.5 0.7 1.5]))
          "the progress (y) may leave 0..1"))
  (t/testing "nothing else"
    (t/is (nil? (cfo/parse-cubic-bezier-value [0.42 0 0.58])))
    (t/is (nil? (cfo/parse-cubic-bezier-value [1.5 0 0.58 1])) "a time past 1")
    (t/is (nil? (cfo/parse-cubic-bezier-value "{ease.out}")))))

(t/deftest motion-token-value-schemas
  (let [valid? (fn [type value] (sm/validate (cfo/make-token-value-schema type) value))]
    (t/is (valid? :duration "150ms"))
    (t/is (valid? :duration "{motion.fast} * 2") "the resolver checks formulas")
    (t/is (not (valid? :duration "-150ms")))
    (t/is (not (valid? :duration "150px")))
    (t/is (valid? :cubic-bezier [0.42 0 0.58 1]))
    (t/is (valid? :cubic-bezier "{ease.out}"))
    (t/is (not (valid? :cubic-bezier [0.42 0 0.58])))
    (t/is (not (valid? :cubic-bezier "0.42, 0, 0.58, 1")) "kept as numbers")))

(t/deftest convert-dtcg-motion-token-test
  (t/testing "a duration of the 2025 format becomes a text"
    (t/is (= {:name "motion.fast" :type :duration :value "150ms"}
             (cfo/convert-dtcg-token {"name" "motion.fast" "type" "duration"
                                      "value" {"value" 150 "unit" "ms"}})))
    (t/is (= {:name "motion.slow" :type :duration :value "0.5s"}
             (cfo/convert-dtcg-token {"name" "motion.slow" "type" "duration"
                                      "value" {"value" 0.5 "unit" "s"}}))))
  (t/testing "a cubic bezier keeps its numbers"
    (t/is (= {:name "ease.out" :type :cubic-bezier :value [0 0 0.58 1]}
             (cfo/convert-dtcg-token {"name" "ease.out" "type" "cubicBezier"
                                      "value" [0 0 0.58 1]})))))
