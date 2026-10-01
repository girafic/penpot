;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.render-wasm.text-units-serializer-test
  (:require
   [app.common.render-wasm.serializers.text-units :as sr-tu]
   [app.render-wasm.api :as api]
   [app.render-wasm.shape :as shape]
   [cljs.test :as t :include-macros true]))

(defn- view
  [^js bytes]
  (js/DataView. (.-buffer bytes)))

(t/deftest units-are-written-as-render-wasm-reads-them
  (let [bytes (sr-tu/entries->bytes
               [{:kind :word :paragraph 2 :start 6 :end 11
                 :dx 0 :dy 12.5 :rotation 0 :scale-x 1 :scale-y 1 :opacity 0.4}
                {:kind :line :index 1 :line-count 3
                 :dx -3 :dy 0 :rotation 45 :scale-x 2 :scale-y 0.5 :opacity 1}])
        ^js v (view bytes)
        f32   #(.getFloat32 v % true)
        u32   #(.getUint32 v % true)]
    (t/is (= 88 (.-length bytes)) "a header of 8 and 40 bytes a unit")
    (t/is (= [2 3] [(u32 0) (u32 4)]) "the count and the line count")

    (t/testing "a word unit: its paragraph and UTF-16 range"
      (t/is (= 1 (.getUint8 v 8)))
      (t/is (= [2 6 11] [(u32 12) (u32 16) (u32 20)]))
      (t/is (= [0 12.5 0 1 1 (js/Math.fround 0.4)]
               (mapv f32 [24 28 32 36 40 44]))))

    (t/testing "a line unit: its line"
      (t/is (= 2 (.getUint8 v 48)))
      (t/is (= [1 0 0] [(u32 52) (u32 56) (u32 60)]))
      (t/is (= [-3 0 45 2 0.5 1] (mapv f32 [64 68 72 76 80 84]))))))

(t/deftest no-units-clear-them
  (doseq [entries [nil []]]
    (let [bytes (sr-tu/entries->bytes entries)]
      (t/is (= 8 (.-length bytes)))
      (t/is (= 0 (.getUint32 (view bytes) 0 true))))))

(t/deftest units-left-out-rest
  (let [^js v (view (sr-tu/entries->bytes [{:kind :char :paragraph 0 :start 0 :end 1}]))]
    (t/is (= [0 0 0 1 1 1] (mapv #(.getFloat32 v % true) [24 28 32 36 40 44])))))

(t/deftest the-text-units-of-a-shape-reach-render-wasm
  (let [sent (atom ::none)]
    (with-redefs [api/initialized?          (constantly true)
                  api/set-shape-text-units #(reset! sent %)]
      (let [entries [{:kind :char :paragraph 0 :start 0 :end 1 :opacity 0.5}]]
        (#'shape/set-wasm-attr! {:id (random-uuid) :type :text :text-units entries} :text-units)
        (t/is (= entries @sent))
        (#'shape/set-wasm-attr! {:id (random-uuid) :type :text :text-units []} :text-units)
        (t/is (= [] @sent) "no units clear them")))))
