;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.render-wasm.api.props
  "Browser-free WASM shape property setters, shared by the workspace render
  orchestrator (`app.render-wasm.api`) and the headless exporter
  (`app.wasm.serialize`).

  These only touch the WASM FFI (`app.render-wasm.{helpers,mem,serializers,
  wasm}`) and the shared `common` byte layouts — no store/DOM/React — so they
  run identically in the browser and under Node. Setters that need host-specific
  data sources (fonts, image bytes, SVG static markup) stay in `app.render-wasm.api`."
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.render-wasm.helpers :as h]
   [app.common.render-wasm.mem :as mem]
   [app.common.render-wasm.serializers :as sr]
   [app.common.render-wasm.serializers.color :as sr-clr]
   [app.common.render-wasm.serializers.text-units :as sr-tu]
   [app.common.render-wasm.wasm :as wasm]
   [app.common.svg :as csvg]
   [app.common.types.fills :as types.fills]
   [app.common.types.fills.impl :as types.fills.impl]
   [app.common.types.path :as path]))

(def ^:const MAX_BUFFER_CHUNK_SIZE (* 256 1024))

(defn set-shape-bool-type
  [bool-type]
  (h/call wasm/internal-module "_set_shape_bool_type" (sr/translate-bool-type bool-type)))

(defn set-shape-grow-type
  [grow-type]
  (h/call wasm/internal-module "_set_shape_grow_type" (sr/translate-grow-type grow-type)))

(defn write-shape-fills!
  "Serializes a fill vector into WASM using the shared `common` byte layout.
  Only writes the fill *records*; image fill BYTES are host-specific (the browser
  fetches WebGL textures, the exporter provisions decoded bytes), so callers load
  images after. Returns the coerced `Fills` (for image-id extraction) or nil when
  empty."
  [fills]
  (if (empty? fills)
    (do (h/call wasm/internal-module "_clear_shape_fills") nil)
    (let [fills  (types.fills/coerce fills)
          offset (mem/alloc->offset-32 (types.fills/get-byte-size fills))
          heap   (mem/get-heap-u32)]
      (types.fills/write-to fills heap offset)
      (h/call wasm/internal-module "_set_shape_fills")
      fills)))

(defn write-shape-text-fills-override!
  "Serializes `fills` (records, as `write-shape-fills!`) as the ones that
  paint every span of the current text in place of its own while an
  animation of its fills plays; none paints the spans with their own
  again."
  [fills]
  (if (empty? fills)
    (h/call wasm/internal-module "_clear_shape_text_fills_override")
    (let [fills  (types.fills/coerce fills)
          offset (mem/alloc->offset-32 (types.fills/get-byte-size fills))
          heap   (mem/get-heap-u32)]
      (types.fills/write-to fills heap offset)
      (h/call wasm/internal-module "_set_shape_text_fills_override"))))

(defn write-shape-strokes!
  "Serializes the stroke vector (records only, like `write-shape-fills!`);
  image stroke BYTES are host-specific, so callers load images after. Returns
  the image ids referenced by the strokes' image fills."
  [strokes]
  (h/call wasm/internal-module "_clear_shape_strokes")
  (into []
        (keep
         (fn [stroke]
           (when-not (:hidden stroke)
             (let [opacity   (or (:stroke-opacity stroke) 1.0)
                   color     (:stroke-color stroke)
                   gradient  (:stroke-color-gradient stroke)
                   image     (:stroke-image stroke)
                   width     (:stroke-width stroke)
                   align     (:stroke-alignment stroke)
                   style     (-> stroke :stroke-style sr/translate-stroke-style)
                   cap-start (-> stroke :stroke-cap-start sr/translate-stroke-cap)
                   cap-end   (-> stroke :stroke-cap-end sr/translate-stroke-cap)
                   ;; Sentinel -1 means "unset" on the Rust side — keeps the
                   ;; FFI signature flat while letting the renderer fall back
                   ;; to its default dash pattern when no override is stored.
                   dash      (or (:stroke-dash stroke) -1)
                   gap       (or (:stroke-gap stroke) -1)
                   offset    (mem/alloc types.fills.impl/FILL-U8-SIZE)
                   heap      (mem/get-heap-u8)
                   dview     (js/DataView. (.-buffer heap))]
               (case align
                 :inner (h/call wasm/internal-module "_add_shape_inner_stroke" width style cap-start cap-end dash gap)
                 :outer (h/call wasm/internal-module "_add_shape_outer_stroke" width style cap-start cap-end dash gap)
                 (h/call wasm/internal-module "_add_shape_center_stroke" width style cap-start cap-end dash gap))

               ;; Per-side widths (rects/frames). Sides falling back to the
               ;; uniform width; skipped when all sides end up equal, so the
               ;; renderer keeps the uniform path (with dash/dot support).
               (let [top    (or (:stroke-width-top stroke) width)
                     right  (or (:stroke-width-right stroke) width)
                     bottom (or (:stroke-width-bottom stroke) width)
                     left   (or (:stroke-width-left stroke) width)]
                 (when-not (= top right bottom left)
                   (h/call wasm/internal-module "_set_shape_stroke_sides" top right bottom left)))

               (cond
                 (some? gradient)
                 (do (types.fills.impl/write-gradient-fill offset dview opacity gradient)
                     (h/call wasm/internal-module "_add_shape_stroke_fill")
                     nil)

                 (some? image)
                 (do (types.fills.impl/write-image-fill offset dview opacity image)
                     (h/call wasm/internal-module "_add_shape_stroke_fill")
                     (get image :id))

                 (some? color)
                 (do (types.fills.impl/write-solid-fill offset dview opacity color)
                     (h/call wasm/internal-module "_add_shape_stroke_fill")
                     nil))))))
        strokes))

(defn- get-string-length
  [string]
  (+ (count string) 1))

(defn set-shape-blur
  [blur]
  (let [type (sr/translate-blur-type :layer-blur)]
    (if (some? blur)
      (h/call wasm/internal-module "_set_shape_blur" type (boolean (:hidden blur)) (:value blur))
      (h/call wasm/internal-module "_clear_shape_blur" type))))

(defn set-shape-background-blur
  [background-blur]
  (let [type (sr/translate-blur-type :background-blur)]
    (if (some? background-blur)
      (h/call wasm/internal-module "_set_shape_blur" type (boolean (:hidden background-blur)) (:value background-blur))
      (h/call wasm/internal-module "_clear_shape_blur" type))))

(defn set-shape-shadows
  [shadows]
  (h/call wasm/internal-module "_clear_shape_shadows")
  (run! (fn [shadow]
          (let [color  (get shadow :color)
                blur   (get shadow :blur)
                rgba   (sr-clr/hex->u32argb (get color :color)
                                            (get color :opacity))
                hidden (get shadow :hidden)
                x      (get shadow :offset-x)
                y      (get shadow :offset-y)
                spread (get shadow :spread)
                style  (get shadow :style)]
            (h/call wasm/internal-module "_add_shape_shadow"
                    rgba
                    blur
                    spread
                    x
                    y
                    (sr/translate-shadow-style style)
                    hidden)))
        shadows))

(defn mask-value
  "How the renderer reads whether the group `shape` is masked and how
  (see `cts/mask-modes`): 0 unmasked, 1 by the alpha of the mask, 2 by
  where it is not, 3 by its brightness, 4 by its darkness."
  [shape]
  (if (true? (:masked-group shape))
    (case (:mask-mode shape)
      :alpha-inverted 2
      :luma           3
      :luma-inverted  4
      1)
    0))

(defn set-masked
  "Mask the current shape, a group, as `mask-value` says (0: not)."
  [value]
  (h/call wasm/internal-module "_set_shape_masked_group" value))

(defn set-shape-svg-attrs
  [attrs]
  (let [style (:style attrs)
        fill-rule       (-> (or (:fillRule style) (:fillRule attrs)) sr/translate-fill-rule)
        stroke-linecap  (-> (or (:strokeLinecap style) (:strokeLinecap attrs)) sr/translate-stroke-linecap)
        stroke-linejoin (-> (or (:strokeLinejoin style) (:strokeLinejoin attrs)) sr/translate-stroke-linejoin)
        fill-none       (= "none" (or (:fill style) (:fill attrs)))
        ;; dashes no stroke style draws, kept as the SVG has them (see
        ;; `sb/setup-stroke`)
        dasharray       (csvg/parse-numbers (str (or (:strokeDasharray style) (:strokeDasharray attrs))))
        dashoffset      (d/parse-double (or (:strokeDashoffset style) (:strokeDashoffset attrs)) 0)]
    (h/call wasm/internal-module "_set_shape_svg_attrs" fill-rule stroke-linecap stroke-linejoin fill-none)
    (when (seq dasharray)
      (let [offset (mem/alloc->offset-32 (* 4 (count dasharray)))
            heap   (mem/get-heap-f32)]
        (doseq [[i length] (map-indexed vector dasharray)]
          (aset heap (+ offset i) length))
        (h/call wasm/internal-module "_set_shape_svg_stroke_dash" dashoffset)))))

(defn set-shape-svg-raw-content
  [content]
  (let [size (get-string-length content)
        offset (mem/alloc size)]
    (h/call wasm/internal-module "stringToUTF8" content offset size)
    (h/call wasm/internal-module "_set_shape_svg_raw_content")))

(defn set-shape-text-units
  "Pose the units of the current text one by one, as a split animation of
  Penpot Motion moves them (see `app.common.render-wasm.serializers.text-units`);
  nil or no `entries` draw it at rest."
  [entries]
  (sr-tu/write! entries)
  (h/call wasm/internal-module "_set_shape_text_units")
  nil)

(defn set-shape-path-content
  "Upload path content in chunks to WASM."
  [content]
  (let [chunk-size (quot MAX_BUFFER_CHUNK_SIZE 4)
        buffer-size (path/get-byte-size content)
        padded-size (* 4 (mth/ceil (/ buffer-size 4)))
        buffer (js/Uint8Array. padded-size)]
    (path/write-to content (.-buffer buffer) 0)
    (h/call wasm/internal-module "_start_shape_path_buffer")
    (let [heapu32 (mem/get-heap-u32)]
      (loop [offset 0]
        (when (< offset padded-size)
          (let [end (min padded-size (+ offset (* chunk-size 4)))
                chunk (.subarray buffer offset end)
                chunk-u32 (js/Uint32Array. chunk.buffer chunk.byteOffset (quot (.-length chunk) 4))
                offset-size (.-length chunk-u32)
                heap-offset (mem/alloc->offset-32 (* 4 offset-size))]
            (.set heapu32 chunk-u32 heap-offset)
            (h/call wasm/internal-module "_set_shape_path_chunk_buffer")
            (recur end)))))
    (h/call wasm/internal-module "_set_shape_path_buffer")))
