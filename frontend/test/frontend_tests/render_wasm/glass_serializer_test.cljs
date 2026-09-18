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

(defn- light-kind
  "Fill kind of the light: 0 solid, 1 linear, 2 radial."
  [dview]
  (.getUint8 dview (+ 4 sr-glass/LIGHT-U8-OFFSET)))

(defn- light-u32
  "Word `index` of the light record, counted from its own start."
  [dview index]
  (.getUint32 dview (+ 4 sr-glass/LIGHT-U8-OFFSET (* 4 index)) true))

(deftest writes-the-glass-layout
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
    (is (= (+ 4 216) end))
    (is (= 216 sr-glass/GLASS-U8-SIZE))
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
    (testing "the light is a solid fill, opaque whatever the color opacity"
      (is (= 0 (light-kind dview)))
      (is (= 0xFF00FF00 (light-u32 dview 1))))))

(deftest writes-a-gradient-light
  (let [gradient {:type :linear
                  :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5
                  :width 0
                  :stops [{:color "#ff0000" :opacity 1 :offset 0}
                          {:color "#0000ff" :opacity 0.5 :offset 1}]}
        [dview _] (write (assoc ctsg/default-attrs :light-color {:gradient gradient}))]
    (is (= 1 (light-kind dview)) "linear gradient")
    (is (= 0.5 (.getFloat32 dview (+ 4 sr-glass/LIGHT-U8-OFFSET 8) true)) "start y")
    (is (= 1.0 (.getFloat32 dview (+ 4 sr-glass/LIGHT-U8-OFFSET 12) true)) "end x")
    (is (= 2 (.getUint8 dview (+ 4 sr-glass/LIGHT-U8-OFFSET 28))) "stop count")
    (is (= 0xFFFF0000 (light-u32 dview 8)) "first stop")
    (is (= 0x7F0000FF (light-u32 dview 10)) "second stop")))

(deftest missing-keys-use-neutral-defaults
  (let [old-glass (apply dissoc ctsg/default-attrs (keys ctsg/optional-defaults))
        [dview _] (write old-glass)]
    (is (= 0 (.getUint8 dview 5)) "no texture")
    (is (= 100 (f32 dview 7)) "saturation")
    (is (= 100 (f32 dview 8)) "brightness")
    (is (= 2 (f32 dview 9)) "highlight width")
    (is (= 0 (light-kind dview)) "solid light")
    (is (= 0xFFFFFFFF (light-u32 dview 1)) "white light")))

(deftest every-texture-has-its-own-byte
  (doseq [[texture expected] {:none 0 :reeded 1 :wavy 2 :prismatic 3
                              :cross-reeded 4 :hammered 5}]
    (let [[dview _] (write (assoc ctsg/default-attrs :texture texture))]
      (is (= expected (.getUint8 dview 5)) (str texture)))))

(deftest short-hex-light-colors-are-expanded
  (let [[dview _] (write (assoc ctsg/default-attrs :light-color {:color "#f0a"}))]
    (is (= 0xFFFF00AA (light-u32 dview 1)))))
