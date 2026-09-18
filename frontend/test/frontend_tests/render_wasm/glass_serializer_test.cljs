;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.render-wasm.glass-serializer-test
  (:require
   [app.common.render-wasm.serializers.glass :as sr-glass]
   [app.common.render-wasm.wasm :as wasm]
   [app.common.types.shape.glass :as ctsg]
   [cljs.test :refer [deftest is testing] :include-macros true]))

(def ^:private serializers
  #js {"glass-texture" #js {"none" 0 "reeded" 1 "wavy" 2 "prismatic" 3
                            "cross-reeded" 4 "hammered" 5}})

(defn- write
  "Writes `glass` at offset 4 of a fresh buffer and returns the DataView
  and the returned offset."
  [glass]
  (with-redefs [wasm/serializers serializers]
    (let [buffer (js/ArrayBuffer. (+ 8 sr-glass/GLASS-U8-SIZE))
          dview  (js/DataView. buffer)
          end    (sr-glass/write-glass! dview 4 glass)]
      [dview end])))

(defn- f32 [dview index]
  (.getFloat32 dview (+ 4 4 (* 4 index)) true))

(deftest writes-the-60-byte-layout
  (let [glass (assoc ctsg/default-attrs
                     :hidden true
                     :texture :reeded
                     :saturation 150
                     :brightness 60
                     :highlight-width 3
                     :texture-amount 40
                     :texture-scale 12
                     :texture-angle 30
                     :light-color {:color "#00ff00" :opacity 0.2})
        [dview end] (write glass)]
    (is (= (+ 4 60) end))
    (is (= 1 (.getUint8 dview 4)))
    (is (= 1 (.getUint8 dview 5)))
    (is (= -45 (f32 dview 0)))
    (is (= 80 (f32 dview 1)))
    (is (= 0 (f32 dview 6)))
    (is (= 150 (f32 dview 7)))
    (is (= 60 (f32 dview 8)))
    (is (= 3 (f32 dview 9)))
    (is (= 40 (f32 dview 10)))
    (is (= 12 (f32 dview 11)))
    (is (= 30 (f32 dview 12)))
    (testing "the light is opaque, whatever the color opacity"
      (is (= 0xFF00FF00 (.getUint32 dview (+ 4 56) true))))))

(deftest missing-keys-use-neutral-defaults
  (let [old-glass (apply dissoc ctsg/default-attrs (keys ctsg/optional-defaults))
        [dview _] (write old-glass)]
    (is (= 0 (.getUint8 dview 5)) "no texture")
    (is (= 100 (f32 dview 7)) "saturation")
    (is (= 100 (f32 dview 8)) "brightness")
    (is (= 2 (f32 dview 9)) "highlight width")
    (is (= 0xFFFFFFFF (.getUint32 dview (+ 4 56) true)) "white light")))

(deftest every-texture-has-its-own-byte
  (doseq [[texture expected] {:none 0 :reeded 1 :wavy 2 :prismatic 3
                              :cross-reeded 4 :hammered 5}]
    (let [[dview _] (write (assoc ctsg/default-attrs :texture texture))]
      (is (= expected (.getUint8 dview 5)) (str texture)))))

(deftest short-hex-light-colors-are-expanded
  (let [[dview _] (write (assoc ctsg/default-attrs :light-color {:color "#f0a"}))]
    (is (= 0xFFFF00AA (.getUint32 dview (+ 4 56) true)))))
