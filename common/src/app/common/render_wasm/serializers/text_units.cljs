;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.render-wasm.serializers.text-units
  "The units of a text (characters, words or lines) that a split animation
  of Penpot Motion poses one by one, as render-wasm reads them with
  `_set_shape_text_units`: a header (the count and the line count) and 40
  bytes per unit, little endian (the byte contract is documented in
  `render-wasm/src/shapes/text_units.rs`)."
  (:require
   [app.common.render-wasm.mem :as mem]))

(def ^:private header-size 8)
(def ^:private entry-size 40)

(def ^:private kinds
  {:char 0 :word 1 :line 2})

(defn entries->bytes
  "The bytes of the text units `entries`, each one
  `{:kind :paragraph :start :end :dx :dy :rotation :scale-x :scale-y :opacity}`
  for a `:char` or `:word` unit (UTF-16 offsets in the source text of the
  paragraph), or `{:kind :line :index :line-count ...}` for a line unit.
  The header is always written: nil or no entries give a count of 0, which
  clears the units. The line count is the largest `:line-count` of them."
  [entries]
  (let [buffer (js/ArrayBuffer. (+ header-size (* entry-size (count entries))))
        view   (js/DataView. buffer)]
    (.setUint32 view 0 (count entries) true)
    (.setUint32 view 4 (reduce max 0 (keep :line-count entries)) true)
    (loop [entries (seq entries)
           offset  header-size]
      (when-let [{:keys [kind paragraph index start end dx dy rotation scale-x scale-y opacity]}
                 (first entries)]
        (.setUint8 view offset (get kinds kind 0))
        (.setUint32 view (+ offset 4) (or (if (= kind :line) index paragraph) 0) true)
        (.setUint32 view (+ offset 8) (or start 0) true)
        (.setUint32 view (+ offset 12) (or end 0) true)
        (.setFloat32 view (+ offset 16) (or dx 0) true)
        (.setFloat32 view (+ offset 20) (or dy 0) true)
        (.setFloat32 view (+ offset 24) (or rotation 0) true)
        (.setFloat32 view (+ offset 28) (or scale-x 1) true)
        (.setFloat32 view (+ offset 32) (or scale-y 1) true)
        (.setFloat32 view (+ offset 36) (or opacity 1) true)
        (recur (next entries) (+ offset entry-size))))
    (js/Uint8Array. buffer)))

(defn write!
  "Copy the bytes of the text units `entries` (see `entries->bytes`) into
  the WASM heap, where `_set_shape_text_units` takes them."
  [entries]
  (let [bytes  (entries->bytes entries)
        offset (mem/alloc (.-length bytes))]
    (.set (mem/get-heap-u8) bytes offset)))
