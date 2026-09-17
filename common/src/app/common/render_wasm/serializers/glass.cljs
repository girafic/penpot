;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.render-wasm.serializers.glass
  "Byte layout of a glass effect, shared by the single-shape setter and
  the batch upload (must match `render-wasm/src/wasm/glass.rs`):

    [u8 hidden][u8 texture][2 pad][13 x f32][u32 light ARGB] = 60 bytes"
  (:require
   [app.common.buffer :as buf]
   [app.common.render-wasm.serializers :as sr]
   [app.common.render-wasm.serializers.color :as sr-clr]
   [app.common.types.color :as ctc]
   [app.common.types.shape.glass :as ctsg]))

(def ^:const GLASS-U8-SIZE 60)

(def ^:private float-attrs
  [:light-angle :light-intensity :refraction :depth :dispersion :frost :splay
   :saturation :brightness :highlight-width
   :texture-amount :texture-scale :texture-angle])

(def ^:const WHITE-ARGB 0xFFFFFFFF)

(defn- light-color->u32
  "Opaque ARGB of the light color; the light strength comes from the
  light intensity only."
  [light-color]
  (if-let [hex (:color light-color)]
    (let [hex (-> hex ctc/remove-hash ctc/expand-hex ctc/prepend-hash)]
      (sr-clr/hex->u32argb hex 1))
    WHITE-ARGB))

(defn write-glass!
  "Writes `glass` at `offset` and returns the offset after it. Missing
  optional keys are written with their neutral defaults, never 0."
  [dview offset glass]
  (buf/write-u8 dview offset (if (get glass :hidden) 1 0))
  (buf/write-u8 dview (+ offset 1) (sr/translate-glass-texture (ctsg/get-value glass :texture)))
  (buf/write-u8 dview (+ offset 2) 0)
  (buf/write-u8 dview (+ offset 3) 0)
  (reduce (fn [o attr]
            (buf/write-f32 dview o (or (ctsg/get-value glass attr) 0))
            (+ o 4))
          (+ offset 4)
          float-attrs)
  (buf/write-u32 dview (+ offset 56) (light-color->u32 (get glass :light-color)))
  (+ offset GLASS-U8-SIZE))
