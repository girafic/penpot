;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.render-wasm.serializers.glass
  "Byte layout of a glass effect, shared by the single-shape setter and
  the batch upload (must match `render-wasm/src/wasm/glass.rs`):

    [u8 hidden][u8 texture][2 pad][13 x f32][fill light] = 216 bytes

  The light paint uses the same record as a fill, so it can be a solid
  color or a gradient."
  (:require
   [app.common.buffer :as buf]
   [app.common.render-wasm.serializers :as sr]
   [app.common.types.color :as ctc]
   [app.common.types.fills.impl :as fills]
   [app.common.types.shape.glass :as ctsg]))

(def ^:const LIGHT-U8-OFFSET 56)
(def ^:const GLASS-U8-SIZE (+ LIGHT-U8-OFFSET fills/FILL-U8-SIZE))

(def ^:private float-attrs
  [:light-angle :light-intensity :refraction :depth :dispersion :frost :splay
   :saturation :brightness :highlight-width
   :texture-amount :texture-scale :texture-angle])

(defn- write-light!
  "Writes the light paint as a fill record. A solid color is written
  opaque: its strength comes from the light intensity."
  [dview offset light-color]
  (if-let [gradient (:gradient light-color)]
    (fills/write-gradient-fill offset dview (get light-color :opacity 1) gradient)
    (let [hex (-> (get light-color :color ctc/white)
                  ctc/remove-hash
                  ctc/expand-hex
                  ctc/prepend-hash)]
      (fills/write-solid-fill offset dview 1 hex))))

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
  (write-light! dview (+ offset LIGHT-U8-OFFSET) (get glass :light-color))
  (+ offset GLASS-U8-SIZE))
