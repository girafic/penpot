;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.render-wasm.text-units-serializer-test
  (:require
   [app.common.render-wasm.serializers.text-units :as sr-tu]
   [app.common.types.animation :as cta]
   [app.common.types.shape :as cts]
   [app.render-wasm.api :as api]
   [app.render-wasm.shape :as shape]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.mock :as mock]))

(defn- view
  [^js bytes]
  (js/DataView. (.-buffer bytes)))

(t/deftest units-are-written-as-render-wasm-reads-them
  (let [bytes (sr-tu/entries->bytes
               [{:kind :word :paragraph 2 :start 6 :end 11
                 :dx 0 :dy 12.5 :rotation 0 :scale-x 1 :scale-y 1 :opacity 0.4 :blur 0}
                {:kind :line :index 1 :line-count 3
                 :dx -3 :dy 0 :rotation 45 :scale-x 2 :scale-y 0.5 :opacity 1 :blur 6
                 :rotate-x 30 :rotate-y -60}])
        ^js v (view bytes)
        f32   #(.getFloat32 v % true)
        u32   #(.getUint32 v % true)]
    (t/is (= 112 (.-length bytes)) "a header of 8 and 52 bytes a unit")
    (t/is (= [2 3] [(u32 0) (u32 4)]) "the count and the line count")

    (t/testing "a word unit: its paragraph and UTF-16 range"
      (t/is (= 1 (.getUint8 v 8)))
      (t/is (= [2 6 11] [(u32 12) (u32 16) (u32 20)]))
      (t/is (= [0 12.5 0 1 1 (js/Math.fround 0.4) 0 0 0]
               (mapv f32 [24 28 32 36 40 44 48 52 56]))))

    (t/testing "a line unit: its line"
      (t/is (= 2 (.getUint8 v 60)))
      (t/is (= [1 0 0] [(u32 64) (u32 68) (u32 72)]))
      (t/is (= [-3 0 45 2 0.5 1 6 30 -60] (mapv f32 [76 80 84 88 92 96 100 104 108]))
            "the last two the 3D turn"))))

(t/deftest no-units-clear-them
  (doseq [entries [nil []]]
    (let [bytes (sr-tu/entries->bytes entries)]
      (t/is (= 8 (.-length bytes)))
      (t/is (= 0 (.getUint32 (view bytes) 0 true))))))

(t/deftest units-left-out-rest
  (let [^js v (view (sr-tu/entries->bytes [{:kind :char :paragraph 0 :start 0 :end 1}]))]
    (t/is (= [0 0 0 1 1 1 0 0 0] (mapv #(.getFloat32 v % true) [24 28 32 36 40 44 48 52 56])))))

(t/deftest the-text-units-of-a-shape-reach-render-wasm
  (let [sent (atom ::none)]
    (with-redefs [api/initialized?          (constantly true)
                  api/set-shape-text-units #(reset! sent %)]
      (let [entries [{:kind :char :paragraph 0 :start 0 :end 1 :opacity 0.5}]]
        (#'shape/set-wasm-attr! {:id (random-uuid) :type :text :text-units entries} :text-units)
        (t/is (= entries @sent))
        (#'shape/set-wasm-attr! {:id (random-uuid) :type :text :text-units []} :text-units)
        (t/is (= [] @sent) "no units clear them")))))

(t/deftest the-units-a-split-animation-poses-serialize
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 300 :height 100})
        text    (-> (cts/setup-shape {:type :text :x 10 :y 10 :width 200 :height 40})
                    (assoc :content {:type "root"
                                     :children [{:type "paragraph-set"
                                                 :children [{:type "paragraph" :children [{:text "Hi you"}]}
                                                            {:type "paragraph" :children [{:text "there"}]}]}]}))
        bid     (:id board)
        tid     (:id text)
        objects {bid (assoc board :shapes [tid])
                 tid (assoc text :parent-id bid :frame-id bid)}
        entries #(get (cta/text-unit-entries
                       (cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid
                                          {:type :move :split % :start 100 :offset-x 0 :offset-y 30})
                       objects 0)
                      tid)]
    (t/testing "words: their paragraphs and ranges, moved 30 down"
      (let [^js v (view (sr-tu/entries->bytes (entries :word)))
            u32   #(.getUint32 v % true)]
        (t/is (= [3 0] [(u32 0) (u32 4)]))
        (t/is (= [[1 0 0 2] [1 0 3 6] [1 1 0 5]]
                 (mapv (fn [offset] [(.getUint8 v offset) (u32 (+ offset 4)) (u32 (+ offset 8)) (u32 (+ offset 12))])
                       [8 60 112])))
        (t/is (= [0 30 0 1 1 1 0 0 0] (mapv #(.getFloat32 v % true) [24 28 32 36 40 44 48 52 56])))))
    (t/testing "a line: its index, and the line count in the header"
      (let [^js v (view (sr-tu/entries->bytes (entries :line)))]
        (t/is (= [1 1] [(.getUint32 v 0 true) (.getUint32 v 4 true)]))
        (t/is (= [2 0] [(.getUint8 v 8) (.getUint32 v 12 true)]))))))

(t/deftest the-animated-fills-of-a-text-paint-its-spans
  ;; A text paints its glyphs in the fills of its spans: those its
  ;; animation gives paint them all, its rest (no fills of its own) their
  ;; own again.
  (let [override (atom ::none)
        red      [{:fill-color "#ff0000" :fill-opacity 1}]]
    (with-redefs [api/initialized?                  (constantly true)
                  api/set-shape-fills               (mock/stub (fn [& _] nil))
                  api/set-shape-text-fills-override #(reset! override %)]
      (#'shape/set-wasm-attr! {:id (random-uuid) :type :text :fills red} :fills)
      (t/is (= red @override))
      (#'shape/set-wasm-attr! {:id (random-uuid) :type :text :fills nil} :fills)
      (t/is (empty? @override) "at rest, none: its spans in their own")
      (reset! override ::none)
      (#'shape/set-wasm-attr! {:id (random-uuid) :type :rect :fills red} :fills)
      (t/is (= ::none @override) "a shape paints its own fills"))))
