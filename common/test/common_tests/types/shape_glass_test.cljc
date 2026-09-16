;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.types.shape-glass-test
  (:require
   [app.common.types.shape :as cts]
   [app.common.types.shape.glass :as ctsg]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- glass
  [& {:as attrs}]
  (merge ctsg/default-attrs {:id (uuid/next)} attrs))

(t/deftest glass-schema-test
  (t/testing "The default glass is valid"
    (t/is (ctsg/valid-glass? (glass))))

  (t/testing "A glass needs every parameter"
    (t/is (not (ctsg/valid-glass? (dissoc (glass) :refraction)))))

  (t/testing "Only the :glass type is accepted"
    (t/is (not (ctsg/valid-glass? (glass :type :background-blur))))))

(t/deftest shape-with-glass-test
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 68 :height 64})]
    (t/testing "A shape accepts a glass effect"
      (t/is (cts/valid-shape? (assoc shape :glass (glass)))))

    (t/testing "A shape rejects a malformed glass effect"
      (t/is (not (cts/valid-shape? (assoc shape :glass (glass :frost "4"))))))))
