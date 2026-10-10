;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.animation
  (:require
   ["./avif.js" :as avif]
   ["gifenc" :as gifenc]
   ["mediabunny" :as mb]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.modifiers :as cgm]
   [app.common.geom.rect :as grc]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.color :as clr]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.stroke :as ctst]
   [app.main.data.event :as ev]
   [app.main.data.exports.svg-vectors :as svgv]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.animation :as dwa]
   [app.main.store :as st]
   [app.render-wasm.api :as wasm.api]
   [app.render-wasm.gesture :as wasm-gesture]
   [app.render-wasm.modifiers :as wasm.modifiers]
   [app.render-wasm.shape :as wasm.shape]
   [app.util.audio :as audio]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.webapi :as wapi]
   [app.util.zip :as uz]
   [beicon.v2.core :as rx]
   [clojure.string :as str]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(defonce ^:private cancel-fn (atom nil))

(defn default-bridge
  []
  {:set-modifiers wasm.api/set-modifiers
   :propagate-modifiers wasm.api/propagate-modifiers
   :set-structure-modifiers wasm.api/set-structure-modifiers
   :clean-modifiers wasm.api/clean-modifiers
   :use-shape wasm.api/use-shape
   :set-shape-hidden wasm.api/set-shape-hidden
   :render-shape-pixels wasm.api/render-shape-pixels
   :get-shape-extrect wasm.api/get-shape-extrect
   :shape-to-path wasm.api/shape-to-path
   :text-unit-outlines wasm.api/text-unit-outlines
   :apply-shape-properties wasm.shape/apply-shape-properties!
   :set-modifiers-end wasm.api/set-modifiers-end
   :interactive? wasm-gesture/active?
   :end-interactive wasm-gesture/try-end-interactive-transform!})

(defn- property-ops
  [modif-tree]
  (->> modif-tree
       (mapcat (fn [[id {:keys [modifiers]}]]
                 (->> (:structure-parent modifiers)
                      (filter #(= :change-property (:type %)))
                      (map #(vector id %)))))
       (group-by first)))

(defn apply-pose!
  "Write `modif-tree` to WASM without scheduling a viewport render, as the
  view mode shows a frame (see `render-viewer-wasm/show-animation!`).
  The modifiers of the frame before go first, as the workspace drops
  them (see `dwm/set-wasm-modifiers`): WASM keeps the copies of the shapes
  they modified, and a shape no longer modified would show the copy
  instead of the properties set now. The properties go before the new
  modifiers, as these copy the shapes they modify: a shape both moved and
  faded would show the opacity of the frame before, and the first frame
  that of the canvas. The transforms go through `propagate-modifiers`,
  which composes those of a shape and of its parents: sent as they are,
  a layer moving in a moving board took the move of its board on some
  frames only, and jittered."
  [objects modif-tree {:keys [bridge] :or {bridge (default-bridge)}}]
  (let [{:keys [set-modifiers clean-modifiers apply-shape-properties
                propagate-modifiers set-structure-modifiers]} bridge
        geometry  (wasm.modifiers/parse-geometry-modifiers modif-tree)
        structure (wasm.modifiers/parse-structure-modifiers modif-tree)]
    (when clean-modifiers
      (clean-modifiers))
    (doseq [[id ops] (property-ops modif-tree)]
      (when-let [shape (get objects id)]
        (let [ops   (map second ops)
              shape (reduce ctm/apply-modifier shape ops)]
          (apply-shape-properties shape (map :property ops)))))
    (when (and set-structure-modifiers (seq structure))
      (set-structure-modifiers structure))
    (when (seq geometry)
      (let [propagated (when propagate-modifiers
                         (propagate-modifiers geometry false nil))
            roots      (into [] (map (fn [[id data]] [id (:transform data)])) geometry)]
        (set-modifiers (if (seq propagated) propagated roots) :request-render? false)))))

(defn- begin-full-quality!
  [bridge]
  (let [was-fast? (boolean (when-let [pred (:interactive? bridge)]
                             (pred)))]
    (when was-fast?
      (when-let [end (:end-interactive bridge)] (end))
      (when-let [end-mod (:set-modifiers-end bridge)] (end-mod)))
    was-fast?))

(defn render-shape-at-time
  "Apply the timeline pose at `time` silently, rasterize `shape-id`, then
  leave WASM on that pose. The caller restores the playhead."
  [objects timeline shape-id time {:keys [bridge scale] :or {bridge (default-bridge) scale 1}}]
  (apply-pose! objects (cta/timeline->modif-tree timeline objects time) {:bridge bridge})
  ((:render-shape-pixels bridge) shape-id scale :png))

(defn shoot-shape-at-rest
  "Rasterize `shape-id` in rest pose, hiding `hide-ids` for the shot:
  `{:bytes :rect}`, `:rect` the box the picture covers on the canvas (the
  shape as it is, turned, with its shadows and blur, see
  `wasm.api/get-shape-extrect`), nil without a way to know it. It is
  rasterized opaque: the layer of the export has its opacity, which its
  animation may change. A text shows its units at rest, whatever the
  canvas posed (see `cta/text-unit-entries`)."
  [objects shape-id hide-ids {:keys [bridge] :or {bridge (default-bridge)}}]
  (let [{:keys [use-shape set-shape-hidden clean-modifiers get-shape-extrect
                render-shape-pixels apply-shape-properties]} bridge
        shape (get objects shape-id)]
    (try
      (when clean-modifiers
        (clean-modifiers))
      (doseq [id hide-ids]
        (use-shape id)
        (set-shape-hidden true))
      ;; after hiding its layers: a change of the shape itself before
      ;; would keep them in the picture of it
      (when (and apply-shape-properties shape)
        (if (= :text (:type shape))
          (apply-shape-properties (assoc shape :opacity 1 :text-units []) [:opacity :text-units])
          (apply-shape-properties (assoc shape :opacity 1) [:opacity])))
      (let [bytes (render-shape-pixels shape-id 1 :png)]
        {:bytes bytes
         :rect  (when get-shape-extrect
                  (get-shape-extrect shape-id))})
      (finally
        (when (and apply-shape-properties shape)
          (apply-shape-properties shape [:opacity]))
        (doseq [id hide-ids]
          (let [shape (get objects id)]
            (if apply-shape-properties
              (apply-shape-properties shape [:hidden])
              (do (use-shape id)
                  (set-shape-hidden (boolean (:hidden shape)))))))))))

(defn render-shape-at-rest
  "The picture of `shape-id` in rest pose (see `shoot-shape-at-rest`)."
  [objects shape-id hide-ids opts]
  (:bytes (shoot-shape-at-rest objects shape-id hide-ids opts)))

(defn flatten-rgba
  "Composite `data` (RGBA) onto `[r g b]`. Returns a map with opaque pixels."
  [data width height [r g b]]
  (let [len (alength data)
        out (js/Uint8ClampedArray. len)]
    (loop [i 0]
      (when (< i len)
        (let [a  (/ (aget data (+ i 3)) 255.0)
              ia (- 1.0 a)]
          (aset out i       (js/Math.round (+ (* (aget data i) a) (* r ia))))
          (aset out (+ i 1) (js/Math.round (+ (* (aget data (+ i 1)) a) (* g ia))))
          (aset out (+ i 2) (js/Math.round (+ (* (aget data (+ i 2)) a) (* b ia))))
          (aset out (+ i 3) 255)
          (recur (+ i 4)))))
    {:data out :width width :height height}))

(defn- even-dim
  [n]
  (let [n (int n)]
    (if (even? n) n (inc n))))

(defn- clamp8
  [n]
  (cond
    (< n 0) 0
    (> n 255) 255
    :else n))

(def ^:private video-color-space
  "The color space of the frames `rgba->i420` makes: the sRGB of the
  board, as video. Its primaries are those of BT.709, its transfer the
  sRGB curve (the one of the pixels; tagged BT.709, color-managed players
  as QuickTime show them lighter than the board), the BT.709 matrix and
  the limited (TV) range of video."
  #js {:primaries "bt709" :transfer "iec61966-2-1" :matrix "bt709" :fullRange false})

(defn rgba->i420
  "Convert interleaved RGBA/RGBX to I420 in BT.709 limited range (black
  at 16, white at 235), as the videos are tagged (see
  `video-color-space`): full range BT.601 data in a stream tagged so
  played back darker and bluer than the board. Pads to even width/height
  so H.264 4:2:0 is valid for QuickTime."
  [data width height]
  (let [src-w (int width)
        src-h (int height)
        w     (even-dim src-w)
        h     (even-dim src-h)
        y-sz  (* w h)
        cw    (quot w 2)
        ch    (quot h 2)
        u-sz  (* cw ch)
        out   (js/Uint8Array. (+ y-sz u-sz u-sz))]
    (dotimes [y h]
      (dotimes [x w]
        (let [sx (min x (dec src-w))
              sy (min y (dec src-h))
              i  (* 4 (+ (* sy src-w) sx))
              r  (aget data i)
              g  (aget data (+ i 1))
              b  (aget data (+ i 2))
              Y  (+ 16 (unsigned-bit-shift-right (+ (* 11966 r) (* 40254 g) (* 4064 b) 32768) 16))]
          (aset out (+ (* y w) x) (clamp8 Y)))))
    (dotimes [cy ch]
      (dotimes [cx cw]
        (let [sx (min (* cx 2) (dec src-w))
              sy (min (* cy 2) (dec src-h))
              i  (* 4 (+ (* sy src-w) sx))
              r  (aget data i)
              g  (aget data (+ i 1))
              b  (aget data (+ i 2))
              U  (+ 128 (bit-shift-right (+ (* -6597 r) (* -22187 g) (* 28784 b) 32768) 16))
              V  (+ 128 (bit-shift-right (+ (* 28784 r) (* -26147 g) (* -2637 b) 32768) 16))
              oi (+ (* cy cw) cx)]
          (aset out (+ y-sz oi) (clamp8 U))
          (aset out (+ y-sz u-sz oi) (clamp8 V)))))
    {:data out :width w :height h :format "I420"}))

(defn alpha->i420
  "The alpha of interleaved RGBA as an I420 picture for an AVIF alpha
  track: the opacity as full range luma (0 transparent, 255 opaque) and
  neutral chroma, padded as `rgba->i420`."
  [data width height]
  (let [src-w (int width)
        src-h (int height)
        w     (even-dim src-w)
        h     (even-dim src-h)
        y-sz  (* w h)
        out   (js/Uint8Array. (+ y-sz (* 2 (quot w 2) (quot h 2))))]
    (.fill out 128 y-sz)
    (dotimes [y h]
      (dotimes [x w]
        (let [sx (min x (dec src-w))
              sy (min y (dec src-h))]
          (aset out (+ (* y w) x) (aget data (+ 3 (* 4 (+ (* sy src-w) sx))))))))
    {:data out :width w :height h :format "I420"}))

(defn rgba->i420a
  "`rgba->i420` with the opacity after the chroma, as `I420A`: the alpha
  plane as it is, padded the same."
  [data width height]
  (let [yuv   (rgba->i420 data width height)
        ^js color (:data yuv)
        y-sz  (* (:width yuv) (:height yuv))
        ^js alpha (:data (alpha->i420 data width height))
        out   (js/Uint8Array. (+ (.-length color) y-sz))]
    (.set out color)
    (.set out (.subarray alpha 0 y-sz) (.-length color))
    (assoc yuv :data out :format "I420A")))

(defn- transparent?
  "Whether a pixel of the RGBA `data` is not fully opaque."
  [data]
  (let [len (alength data)]
    (loop [i 3]
      (cond
        (>= i len) false
        (< (aget data i) 255) true
        :else (recur (+ i 4))))))

(defn board-backdrop
  [board]
  (clr/hex->rgb (or (-> board :fills first :fill-color) "#ffffff")))

(defn- image-parts
  [image]
  (if (map? image)
    [(:data image) (:width image) (:height image)]
    [(.-data image) (.-width image) (.-height image)]))

(defn yield-to-ui
  "A promise resolved by a task of a `MessageChannel` (`channel`, the
  browser's by default): the page draws and handles input before it,
  and a hidden tab does not slow it down as it slows timers (an export
  tab in the background goes on at its pace). A timer without one."
  ([]
   (yield-to-ui (unchecked-get js/globalThis "MessageChannel")))
  ([channel]
   (js/Promise.
    (fn [resolve]
      (if (some? channel)
        (let [^js ch (new channel)]
          (set! (.. ch -port1 -onmessage)
                (fn [_]
                  (.close (.-port1 ch))
                  (resolve nil)))
          (.postMessage (.-port2 ch) nil))
        (js/setTimeout resolve 0))))))

(defn- default-yield
  []
  (yield-to-ui))

(defn- throw-if-cancelled
  [cancelled?]
  (when (and cancelled? (cancelled?))
    (throw (ex-info "cancelled" {:type :export-cancelled}))))

(defn- find-transparency
  "Promise of whether a pixel of `frames` is not fully opaque. Gives the
  page its turn (`yield`) after each frame it looks through."
  [frames yield cancelled?]
  (reduce (fn [p image]
            (.then p (fn [found?]
                       (or found?
                           (do (throw-if-cancelled cancelled?)
                               (let [found? (transparent? (first (image-parts image)))]
                                 (.then (yield) (constantly found?))))))))
          (js/Promise.resolve false)
          frames))

(defn encode-gif
  "Encode decoded frames with gifenc: a promise of `{:status :ok :blob
  ...}`. `frames` is a seq of ImageData or `{:data :width :height}`
  maps. It gives the page its turn (`yield`) after each frame, and
  stops there when `cancelled?`.

  With `keep-alpha?`, frames with a pixel that is not fully opaque keep
  their transparency as a GIF does: a pixel is transparent or opaque
  (half opaque or more), each frame clearing the one before."
  [{:keys [frames fps loop? colors cancelled? on-progress yield keep-alpha?
           gif-encoder quantize apply-palette]
    :or {colors 128 fps 30}}]
  (let [encoder      (or gif-encoder gifenc/GIFEncoder)
        quantize*    (or quantize gifenc/quantize)
        apply-pal*   (or apply-palette gifenc/applyPalette)
        yield        (or yield default-yield)
        ^js gif      (encoder)
        delay        (max 1 (mth/round (/ 1000.0 fps)))
        n            (count frames)]
    (-> (if keep-alpha?
          (find-transparency frames yield cancelled?)
          (js/Promise.resolve false))
        (.then
         (fn [alpha?]
           (reduce (fn [p [i image]]
                     (-> p
                         (.then
                          (fn [_]
                            (throw-if-cancelled cancelled?)
                            (let [[data w h] (image-parts image)
                                  ^js palette (if alpha?
                                                (quantize* data colors #js {:format "rgba4444"
                                                                            :oneBitAlpha true})
                                                (quantize* data colors))
                                  index      (if alpha?
                                               (apply-pal* data palette "rgba4444")
                                               (apply-pal* data palette))
                                  ;; the color of the transparent pixels
                                  clear      (if alpha?
                                               (.findIndex palette #(zero? (aget % 3)))
                                               -1)]
                              (.writeFrame gif index w h
                                           (js/Object.assign
                                            #js {:palette palette
                                                 :delay delay
                                                 :repeat (if loop? 0 -1)}
                                            (when alpha? #js {:dispose 2})
                                            (when (>= clear 0)
                                              #js {:transparent true :transparentIndex clear}))))
                            (when on-progress (on-progress (inc i) n))
                            (yield)))))
                   (js/Promise.resolve nil)
                   (map-indexed vector frames))))
        (.then
         (fn [_]
           (throw-if-cancelled cancelled?)
           (.finish gif)
           (let [bytes (.bytes gif)]
             {:status :ok
              :blob (js/Blob. #js [bytes] #js {:type "image/gif"})
              :mtype "image/gif"
              :ext ".gif"}))))))

(defn- default-media
  []
  {:output        mb/Output
   :mp4-format    mb/Mp4OutputFormat
   :webm-format   mb/WebMOutputFormat
   :target        mb/BufferTarget
   :sample-source mb/VideoSampleSource
   :video-sample  mb/VideoSample
   :quality       mb/Quality
   :first-codec   mb/getFirstEncodableVideoCodec
   :can-encode    mb/canEncodeVideo
   :audio-source  mb/AudioBufferSource
   :first-audio-codec mb/getFirstEncodableAudioCodec})

(defn- output-format-ctor
  [format media]
  (case format
    :mp4  (:mp4-format media)
    :webm (:webm-format media)
    nil))

(defn- frame-pixels
  "Return `{ :data :width :height :format }` for Mediabunny's BufferSource
  constructor. `ImageData` is not accepted."
  [image flatten-bg]
  (let [[data w h] (image-parts image)
        flat       (if flatten-bg
                     (flatten-rgba data w h flatten-bg)
                     {:data data :width w :height h})]
    (assoc flat :format (if flatten-bg "RGBX" "RGBA"))))

(def ^:private avif-bitrate-factor
  "AV1 needs about half the bitrate of H.264 for the same quality."
  0.5)

(defn avif-bitrate
  [width height quality]
  (max 1 (mth/round (* avif-bitrate-factor (cta/video-bitrate width height quality)))))

(defn- av1-codec
  "AV1 codec string (main profile, 8 bit) with a level that allows a
  `width`×`height` frame: 5.1 up to 8192×4352, 6.0 above."
  [width height]
  (if (and (<= width 8192) (<= height 4352) (<= (* width height) 8912896))
    "av01.0.13M.08"
    "av01.0.16M.08"))

(def ^:private av1-color-space
  "The color space of `rgba->i420` output (see `video-color-space`)."
  video-color-space)

(def ^:private av1-cicp
  "`av1-color-space` as CICP codes, for the AVIF `colr` box: BT.709
  primaries, the sRGB transfer, the BT.709 matrix, limited range."
  #js {:primaries 1 :transfer 13 :matrix 1 :fullRange false})

(def ^:private av1-alpha-color-space
  "The color space of `alpha->i420` output: full range, so the luma is
  the opacity as it is."
  #js {:primaries "bt709" :transfer "iec61966-2-1" :matrix "bt709" :fullRange true})

(def ^:private alpha-quantizer
  "The AV1 quantizer (0-63) of the alpha plane: low, so the edges stay
  sharp and the opaque parts opaque. The plane is mostly flat, which
  costs little."
  4)

(defn- alpha-config
  "Promise of the encoder config of the alpha plane: `config` with a
  fixed quantizer (`alpha-quantizer`) where the encoder takes one, else
  as it is."
  [^js video-encoder config]
  (let [quantized (js/Object.assign #js {} config #js {:bitrateMode "quantizer"})]
    (-> (.isConfigSupported video-encoder quantized)
        (.then (fn [^js support]
                 (if (.-supported support) quantized config))))))

(defn- default-av1
  []
  {:video-encoder (when (exists? js/VideoEncoder) js/VideoEncoder)
   :video-frame   (when (exists? js/VideoFrame) js/VideoFrame)
   :write-avif    avif/writeSequence})

(defn codec-supported?
  ([format]
   (codec-supported? format nil))
  ([format media]
   (case format
     (:gif :svg :lottie :dotlottie)
     (js/Promise.resolve true)

     :avif
     (let [encoder (:video-encoder (merge (default-av1) media))]
       (if (nil? encoder)
         (js/Promise.resolve false)
         (-> (.isConfigSupported ^js encoder #js {:codec (av1-codec 64 64) :width 64 :height 64})
             (.then #(boolean (.-supported ^js %)))
             (.catch (fn [_] false)))))

     (:mp4 :webm)
     (let [media  (merge (default-media) media)
           ctor   (output-format-ctor format media)
           first* (:first-codec media)]
       (if (or (nil? ctor) (nil? first*))
         (js/Promise.resolve false)
         (-> (first* (.getSupportedVideoCodecs (new ctor))
                     #js {:width 64 :height 64})
             (.then some?)
             (.catch (fn [_] false)))))

     (js/Promise.resolve false))))

(defn export-formats
  "The formats `timeline` can be exported as, in menu order. Without the
  WASM renderer only Lottie (as JSON or dotLottie), and only when every
  layer draws as vectors: the other formats rasterize through it."
  [objects timeline render-wasm?]
  (cond
    (nil? timeline) []
    render-wasm?    [:mp4 :webm :gif :avif :svg :lottie :dotlottie]
    :else
    (let [ids   (cta/export-layer-ids objects (:board-id timeline) (:tracks timeline))
          idset (set ids)]
      (if (every? #(cta/lottie-vector-layer? objects idset %) ids)
        [:lottie :dotlottie]
        []))))

(defn- decorated?
  "Whether the text `shape` underlines or strikes some of its text: lines
  that cross the spaces between its units, which belong to none."
  [shape]
  (boolean
   (some #(contains? #{"underline" "line-through"} (:text-decoration %))
         (tree-seq :children :children (:content shape)))))

(def ^:private whole-text-formats
  "The formats that play the split animations of a text that underlines
  or strikes some of its text as animations of the whole text (see
  `unit-texts`)."
  #{:svg :lottie :dotlottie})

(def audio-formats
  "The formats that carry the sound of the audio clips."
  #{:mp4 :webm})

(def markup-formats
  "The formats written as markup over pictures of the layers (see
  `cta/timeline->svg` and `cta/timeline->lottie`)."
  #{:svg :lottie :dotlottie})

(def ^:private lottie-formats
  #{:lottie :dotlottie})

(defn- svg-static?
  "Whether `track` animates on `shape` what an SVG keeps as it rests: the
  strokes and trim of a text not `outlined?` (see `outline-texts`),
  which the SVG renderer draws as text. CSS plays the rest on its
  vectors, SMIL its shadows and the corners CSS can not (see
  `cta/svg-filter-shadows?`, `cta/svg-smil-corners?`)."
  [shape track outlined?]
  (boolean
   (and (= :text (:type shape))
        (not outlined?)
        (some #(#{:stroke-color :stroke-opacity :stroke-width :trim-start :trim-end :trim-offset} (:property %))
              (:keyframes track)))))

(defn- shown
  "The items of `items` (a shadow, a stroke, a blur, or a vector of them)
  that are not hidden."
  [items]
  (remove :hidden (if (map? items) [items] items)))

(defn- off-center-stroke?
  "Whether `stroke` draws otherwise than centered on the outline with
  plain ends, as Lottie draws a stroke."
  [stroke]
  (or (not= :center (:stroke-alignment stroke :inner))
      (contains? ctst/stroke-caps-marker (:stroke-cap-start stroke))
      (contains? ctst/stroke-caps-marker (:stroke-cap-end stroke))))

(defn- see-through?
  "Whether `board` shows what is under it where nothing covers it: no fill
  paints it opaque, or it hides its fill in exports."
  [board]
  (or (true? (:hide-fill-on-export board))
      (not-any? (fn [{:keys [fill-color fill-opacity fill-color-gradient]}]
                  (and (mth/close? 1 (or fill-opacity 1))
                       (or (some? fill-color)
                           (and (some? fill-color-gradient)
                                (every? #(mth/close? 1 (or (:opacity %) 1)) (:stops fill-color-gradient))))))
                (:fills board))))

(defn export-issues
  "What the export of `timeline` (of the shapes of `objects`) plays
  otherwise than the canvas, by format, as the Animated export panel lists
  them: a vector of kinds each, the most telling first.

  - `:lottie/images`, layers that draw as pictures for their effects,
    masks or paints (see `cta/lottie-vector-layer?`; texts are the
    outlines of their glyphs, see `outline-texts`, and an SVG has its
    layers as vectors, see `app.main.data.exports.svg-vectors`);
  - `:svg/appearance`, what an SVG keeps as it rests (see
    `svg-static?`; its CSS plays the rest on its vectors; its texts
    outlined with `:outline` of its export settings, see
    `outline-texts`);
  - `:svg/size` and `:lottie/size`, an animated width or height they
    play as a stretch (see `cta/svg-sized?`, `cta/lottie-sized?`);
  - `:lottie/blur`, a blur, which players draw at their own strength (an
    SVG blurs as the canvas, see `cta/svg-filter-blur?`);
  - `:lottie/shadow`, drop shadows (Lottie effects, without spread);
  - `:lottie/stroke`, a stroke Lottie draws otherwise (see
    `off-center-stroke?`);
  - `:background-blur` and `:blend-mode`, which no markup format plays
    (in SVG a layer blends only within itself);
  - `:split-text`, a split text played whole (one that underlines or
    strikes some of its text, see `unit-texts`), and `:no-audio`, for the
    formats without the sound of its audio clips;
  - `:copy-clock`, a component copy looping on its own clock (see
    `cltl/with-instance-tracks`), which the formats that loop or rest at
    their end bind to the board's: it stops at the end of a board that
    plays once and starts over with one that loops, unless its cycles
    fit the board's;
  - `:mask`, a layer in a masked group, which the markup formats draw
    on its own, unmasked (a mask that animates as a shape);
  - `:gif/alpha` and `:mp4/alpha`, a board that shows through (see
    `see-through?`): GIF has no partial transparency, MP4 none at all."
  [objects timeline]
  (if (nil? timeline)
    {}
    (let [board-id  (:board-id timeline)
          resolved  (cta/resolve-animations timeline objects)
          shapes    (->> (cfh/get-children-with-self objects board-id)
                         (filter some?)
                         (remove :hidden))
          animated  (into #{} (comp (mapcat :keyframes) (map :property)) (vals (:tracks resolved)))
          layer-ids (cta/export-layer-ids objects board-id (:tracks resolved))
          idset     (set layer-ids)
          some?*    (fn [pred] (boolean (some pred shapes)))
          ;; SVG and Lottie play some on the outlines
          stretch?  (boolean
                     (some (fn [[id track]]
                             (let [shape (get objects id)]
                               (and (some? shape)
                                    (some #(contains? #{:width :height} (:property %)) (:keyframes track))
                                    (not (cta/svg-sized? shape track)))))
                           (:tracks resolved)))
          units     (cta/unit-timelines resolved objects)
          ;; the texts an SVG outlines, but those whose units play as
          ;; pictures (see `outline-texts`, `unit-texts`)
          outline?  (true? (get-in timeline [:export :outline]))
          pictured  (into #{} (comp (map key) (remove #(decorated? (get objects %)))) units)
          text?     #(and (= :text (:type %)) (not (contains? pictured (:id %))))
          outlined? #(and outline? (text? %))
          ;; copies looping on their own clock (see `cltl/with-instance-tracks`)
          duration  (max 1 (:duration timeline 0))
          bound?    (some (fn [cycle]
                            (or (= :once (cta/playback-mode timeline))
                                (let [n (/ duration (max 1 cycle))]
                                  (> (mth/abs (- n (mth/round n))) 0.01))))
                          (:copy-loops timeline))
          masked?   (some (fn [id]
                            (some #(true? (dm/get-in objects [% :masked-group]))
                                  (cfh/get-parent-ids objects id)))
                          layer-ids)
          clear?    (some-> (get objects board-id) see-through?)
          table
          [[:lottie/images lottie-formats
            (boolean (some (fn [id]
                             (let [shape (get objects id)]
                               (and (some? shape)
                                    (not (:hidden shape))
                                    (not (contains? #{:text :image} (:type shape)))
                                    (not (cta/lottie-vector-layer? objects idset id)))))
                           (cons board-id layer-ids)))]
           [:svg/appearance #{:svg}
            (boolean (some (fn [[id track]]
                             (when-let [shape (get objects id)]
                               (svg-static? shape track (outlined? shape))))
                           (:tracks resolved)))]
           [:svg/size #{:svg} stretch?]
           [:lottie/size lottie-formats stretch?]
           [:lottie/shadow lottie-formats
            (some?* (fn [shape] (some #(= :drop-shadow (:style %)) (shown (:shadow shape)))))]
           [:lottie/blur lottie-formats
            (or (contains? animated :blur)
                (some?* (fn [shape] (some #(pos? (:value % 0)) (shown (:blur shape))))))]
           [:lottie/stroke lottie-formats
            (some?* (fn [shape]
                      (and (not= :text (:type shape))
                           (some off-center-stroke? (shown (:strokes shape))))))]
           [:background-blur markup-formats
            (or (contains? animated :background-blur)
                (some?* (fn [shape] (some #(pos? (:value % 0)) (shown (:background-blur shape))))))]
           [:blend-mode markup-formats
            (some?* #(not (contains? #{nil :normal} (:blend-mode %))))]
           [:split-text whole-text-formats
            (boolean (some (fn [[id _]] (decorated? (get objects id))) units))]
           [:no-audio #{:gif :avif :svg :lottie :dotlottie}
            (boolean (seq (:audio-clips timeline)))]
           [:copy-clock #{:gif :avif :svg :lottie :dotlottie} (boolean bound?)]
           [:mask markup-formats (boolean masked?)]
           [:gif/alpha #{:gif} (boolean clear?)]
           [:mp4/alpha #{:mp4} (boolean clear?)]]]
      (reduce (fn [issues [kind formats on?]]
                (if on?
                  (reduce #(update %1 %2 (fnil conj []) kind) issues formats)
                  issues))
              {}
              table))))

(def ^:private audio-bitrate
  "Bits per second of the sound of a video, stereo."
  128000)

(def ^:private aac-priming
  "The samples of silence an AAC encoder puts before the sound: 2112 for
  the one of macOS, which Chrome and Safari use there (see
  `audio-start`)."
  2112)

(defn- audio-start
  "Where, in seconds, the encoded sound of `codec` starts, so that it
  plays in time with the picture: the priming of AAC earlier than 0, which
  the edit list Mediabunny writes for a negative start cuts off. Opus
  carries its own (its pre-skip)."
  [codec]
  (if (= codec "aac")
    (- (/ aac-priming audio/sample-rate))
    0))

(defn- box-type
  [^js view offset]
  (apply str (map #(char (.getUint8 view (+ offset %))) (range 4))))

(defn- mp4-boxes
  "The boxes of the MP4 `view` (a DataView) between `start` and `end`, as
  `[type offset size]`. It stops at a box with a 64-bit size, which the
  boxes of a movie never have."
  [^js view start end]
  (loop [offset start
         boxes  []]
    (let [size (when (<= (+ offset 8) end) (.getUint32 view offset))]
      (if (or (nil? size) (< size 8))
        boxes
        (recur (+ offset size) (conj boxes [(box-type view (+ offset 4)) offset size]))))))

(defn- child-box
  [view [_ offset size] type]
  (when (some? offset)
    (d/seek #(= type (first %)) (mp4-boxes view (+ offset 8) (+ offset size)))))

(defn- sample-table
  "The path `[trak mdia minf stbl]` of the boxes of the track of `trak`
  that hold its sample table."
  [view trak]
  (let [mdia (child-box view trak "mdia")
        minf (child-box view mdia "minf")]
    [trak mdia minf (child-box view minf "stbl")]))

(defn- sound-track?
  [^js view trak]
  (let [hdlr (child-box view (child-box view trak "mdia") "hdlr")]
    (and (some? hdlr) (= "soun" (box-type view (+ (second hdlr) 16))))))

(defn- roll-group
  "The boxes of a 'roll' sample group of `n` samples, each needing the
  one before it to decode, as ffmpeg writes them for AAC."
  [n]
  (let [bytes (js/Uint8Array. 54)
        view  (js/DataView. (.-buffer bytes))
        type! (fn [offset type] (dotimes [i 4] (.setUint8 view (+ offset i) (.charCodeAt type i))))]
    ;; sgpd, version 1: one entry of 2 bytes, a roll distance of -1
    (.setUint32 view 0 26)
    (type! 4 "sgpd")
    (.setUint32 view 8 0x01000000)
    (type! 12 "roll")
    (.setUint32 view 16 2)
    (.setUint32 view 20 1)
    (.setInt16 view 24 -1)
    ;; sbgp: all the samples in that group
    (.setUint32 view 26 28)
    (type! 30 "sbgp")
    (.setUint32 view 34 0)
    (type! 38 "roll")
    (.setUint32 view 42 1)
    (.setUint32 view 46 n)
    (.setUint32 view 50 1)
    bytes))

(defn add-aac-roll-group
  "The MP4 `buffer` (an ArrayBuffer, its movie box before its media data)
  with a 'roll' sample group in its sound track: each AAC frame needs the
  one before it to decode. Without one, macOS players take the priming
  off a second time after the edit list (see `audio-start`) and the sound
  plays 44 ms early. The same `buffer` when it is no such MP4."
  [buffer]
  (let [view     (js/DataView. buffer)
        top      (mp4-boxes view 0 (.-byteLength buffer))
        moov     (d/seek #(= "moov" (first %)) top)
        mdat     (d/seek #(= "mdat" (first %)) top)
        traks    (when moov (filter #(= "trak" (first %)) (mp4-boxes view (+ (second moov) 8) (+ (second moov) (nth moov 2)))))
        path     (some->> (d/seek (partial sound-track? view) traks) (sample-table view))
        stbl     (last path)
        stsz     (child-box view stbl "stsz")]
    (if (or (nil? mdat) (< (second mdat) (second moov)) (nil? stsz)
            (some? (child-box view stbl "sgpd"))
            (some #(some? (child-box view (last (sample-table view %)) "co64")) traks))
      buffer
      (let [group  (roll-group (.getUint32 view (+ (second stsz) 16)))
            n      (.-length group)
            at     (+ (second stbl) (nth stbl 2))
            out    (js/Uint8Array. (+ (.-byteLength buffer) n))
            out-v  (js/DataView. (.-buffer out))
            src    (js/Uint8Array. buffer)]
        (.set out (.subarray src 0 at) 0)
        (.set out group at)
        (.set out (.subarray src at) (+ at n))
        ;; the boxes around the group grow, and the media data moves
        (doseq [[_ offset size] (cons moov path)]
          (.setUint32 out-v offset (+ size n)))
        (doseq [trak (filter #(= "trak" (first %)) (mp4-boxes out-v (+ (second moov) 8) (+ (second moov) (nth moov 2) n)))
                :let [[_ offset] (child-box out-v (last (sample-table out-v trak)) "stco")]
                :when (some? offset)
                i (range (.getUint32 out-v (+ offset 12)))
                :let [entry (+ offset 16 (* 4 i))]]
          (.setUint32 out-v entry (+ n (.getUint32 out-v entry))))
        (.-buffer out)))))

(defn- audio-codecs
  "The audio codecs a video `format` carries, in order of preference: AAC
  plays wherever MP4 does, Opus wherever WebM does."
  [format]
  (case format
    :mp4  #js ["aac" "opus"]
    :webm #js ["opus" "vorbis"]
    #js []))

(defn audio-codec
  "A promise of the audio codec a video `format` (`:mp4` or `:webm`) gets
  in this browser, or of nil when it encodes none of them: the video then
  goes without sound."
  ([format]
   (audio-codec format nil))
  ([format media]
   (let [media   (merge (default-media) media)
         first*  (:first-audio-codec media)
         quality (:quality media)
         codecs  (audio-codecs format)]
     (if (or (nil? first*) (zero? (alength codecs)))
       (js/Promise.resolve nil)
       (-> (first* codecs #js {:numberOfChannels 2
                               :sampleRate audio/sample-rate
                               :quality (new quality #js {:bitrate audio-bitrate})})
           (.catch (fn [_] nil)))))))

(defn export-audio
  "A promise of the sound of a video of `timeline` `n` frames long at
  `fps`, an `AudioBuffer` exactly as long as the frames, or of nil when
  no clip sounds in it: the clips as they play once from the start of
  the timeline (see `cta/audio-plan`), so silent on the way back of
  ping-pong. `:load` and `:mix` replace `audio/load!` and `audio/mix!` in
  tests."
  [timeline n fps & {:keys [load mix]}]
  (let [length (/ (* 1000 n) fps)
        plan   (cta/audio-plan timeline 0 (min length (cta/cycle-duration timeline)))
        load   (or load #(audio/load! (ctau/get-audio-clip timeline (:clip-id %))))]
    (if (empty? plan)
      (js/Promise.resolve nil)
      ((or mix audio/mix!) plan length {:load load}))))

(defn- prefetch-audio!
  "Start loading the files of the clips of `timeline` that sound, while
  the frames are captured (see `audio/load!`, which keeps them)."
  [timeline]
  (doseq [part (cta/audio-plan timeline 0 (cta/cycle-duration timeline))]
    (p/catch (audio/load! (ctau/get-audio-clip timeline (:clip-id part)))
             (constantly nil))))

(def ^:private alpha-video-codecs
  "The codecs of a WebM that keep the transparency of its frames, in the
  order they are tried (WebCodecs encodes no alpha in AV1)."
  ["vp9" "vp8"])

(defn- alpha-video-codec
  "A promise of the first of `alpha-video-codecs` the browser encodes
  with alpha (`can-encode`, see `mb/canEncodeVideo`) for `options`, nil
  when none."
  [can-encode options]
  (reduce (fn [p codec]
            (.then p (fn [found]
                       (or found
                           (.then (can-encode codec (js/Object.assign #js {} options #js {:alpha "keep"}))
                                  #(when % codec))))))
          (js/Promise.resolve nil)
          alpha-video-codecs))

(defn encode-video
  "Encode decoded frames with Mediabunny, with the sound `audio` (an
  `AudioBuffer` as long as the frames) when given. Returns a promise of
  `{:status :ok|:unavailable :blob ...}`, with `:audio-dropped true` when
  the browser encodes no audio codec the format carries.

  Frames are composited onto `flatten-bg`. With `keep-alpha?`, the
  frames of a WebM with a pixel that is not fully opaque keep their
  transparency instead, when the browser encodes it (see
  `alpha-video-codecs`)."
  [{:keys [format frames fps bitrate cancelled? on-progress flatten-bg keep-alpha? audio yield]
    :as params}]
  (let [yield           (or yield default-yield)
        media           (merge (default-media)
                               (select-keys params [:output :mp4-format :webm-format
                                                    :target :sample-source :video-sample
                                                    :quality :first-codec :can-encode
                                                    :audio-source :first-audio-codec]))
        output-ctor     (:output media)
        format-ctor     (output-format-ctor format media)
        target-ctor     (:target media)
        source-ctor     (:sample-source media)
        sample-ctor     (:video-sample media)
        quality-ctor    (:quality media)
        first-codec     (:first-codec media)
        audio-ctor      (:audio-source media)
        [_ raw-w raw-h] (when (seq frames) (image-parts (first frames)))
        w               (when raw-w (even-dim raw-w))
        h               (when raw-h (even-dim raw-h))]
    (if (or (nil? output-ctor) (nil? format-ctor) (nil? target-ctor)
            (nil? source-ctor) (nil? sample-ctor) (nil? first-codec)
            (nil? w))
      (js/Promise.resolve {:status :unavailable})
      (let [fmt    (if (= format :mp4)
                     (new format-ctor #js {:fastStart "in-memory"})
                     (new format-ctor))
            target (new target-ctor)
            output (new output-ctor #js {:format fmt :target target})
            n      (count frames)
            dur    (/ 1.0 fps)
            closed (atom 0)
            codecs (if (= format :mp4)
                     #js ["avc"]
                     (.getSupportedVideoCodecs fmt))
            probe  #js {:width w
                        :height h
                        :quality (new quality-ctor #js {:bitrate bitrate})
                        :frameRate fps}
            can-encode (:can-encode media)]
        (-> (if (and keep-alpha? (= format :webm) (some? can-encode))
              (-> (find-transparency frames yield cancelled?)
                  (.then #(when % (alpha-video-codec can-encode probe))))
              (js/Promise.resolve nil))
            (.then
             (fn [alpha-codec]
               (js/Promise.all
                #js [(or alpha-codec (first-codec codecs probe))
                     (if (some? audio)
                       (audio-codec format media)
                       (js/Promise.resolve nil))
                     alpha-codec])))
            (.then
             (fn [[codec acodec alpha-codec]]
               (if (nil? codec)
                 {:status :unavailable}
                 (let [alpha?     (some? alpha-codec)
                       flatten-bg (when-not alpha? flatten-bg)
                       source  (new source-ctor
                                    #js {:codec codec
                                         :quality (new quality-ctor #js {:bitrate bitrate})
                                         :alpha (if alpha? "keep" "discard")})
                       asource (when (some? acodec)
                                 (new audio-ctor
                                      #js {:codec acodec
                                           :quality (new quality-ctor #js {:bitrate audio-bitrate})}
                                      #js {:startTimestamp (audio-start acodec)}))]
                   (.addVideoTrack output source #js {:frameRate fps})
                   (when (some? asource)
                     (.addAudioTrack output asource))
                   (-> (.start output)
                       (.then
                        (fn [_]
                          (reduce
                           (fn [p [i image]]
                             (-> p
                                 (.then
                                  (fn [_]
                                    (throw-if-cancelled cancelled?)
                                    (let [pixels (frame-pixels image flatten-bg)
                                          yuv    ((if alpha? rgba->i420a rgba->i420)
                                                  (:data pixels)
                                                  (:width pixels)
                                                  (:height pixels))
                                          sample (new sample-ctor
                                                      (:data yuv)
                                                      #js {:timestamp (* i dur)
                                                           :duration dur
                                                           :colorSpace video-color-space
                                                           :format (:format yuv)
                                                           :codedWidth (:width yuv)
                                                           :codedHeight (:height yuv)})]
                                      (-> (.add source sample
                                                #js {:keyFrame (zero? (mod i fps))})
                                          (.finally
                                           (fn []
                                             (.close sample)
                                             (swap! closed inc)
                                             (when on-progress
                                               (on-progress (inc i) n))))
                                          ;; the page gets its turn
                                          (.then (fn [_] (yield)))))))))
                           (js/Promise.resolve nil)
                           (map-indexed vector frames))))
                       (.then
                        (fn [_]
                          (.close source)
                          (when (some? asource)
                            (-> (.add asource audio)
                                (.then #(.close asource))))))
                       (.then
                        (fn [_]
                          (.finalize output)))
                       (.then
                        (fn [_]
                          (let [buf   (.-buffer target)
                                buf   (cond-> buf
                                        (and (some? buf) (= acodec "aac")) (add-aac-roll-group))
                                mtype (if (= format :mp4) "video/mp4" "video/webm")
                                ext   (if (= format :mp4) ".mp4" ".webm")]
                            (if (or (nil? buf) (zero? (.-byteLength buf)))
                              {:status :unavailable}
                              (cond-> {:status :ok
                                       :blob (js/Blob. #js [buf] #js {:type mtype})
                                       :mtype mtype
                                       :ext ext
                                       :closed @closed}
                                (and (some? audio) (nil? acodec))
                                (assoc :audio-dropped true))))))))))))))))

(defn fit-video-size
  "A promise of the factor, at most 1, that scales a video of `w` × `h`
  px to the largest size the encoder takes, keeping its proportions:
  `encodable?` gives a promise of whether it takes a size (`w` `h`). 1
  when it takes it as it is, nil when not even a tiny one; else found
  to a thousandth, with 2 px to spare for the rounding of the frames."
  [w h encodable?]
  (let [fits?  (fn [f] (encodable? (even-dim (+ 2 (* w f))) (even-dim (+ 2 (* h f)))))
        search (fn search [lo hi steps]
                 (if (zero? steps)
                   (js/Promise.resolve (when (pos? lo) lo))
                   (let [mid (/ (+ lo hi) 2)]
                     (.then (fits? mid)
                            #(if % (search mid hi (dec steps)) (search lo mid (dec steps)))))))]
    (.then (encodable? (even-dim w) (even-dim h))
           #(if % 1 (search 0 1 10)))))

(defn- mp4-encodable?
  "Whether the browser encodes an MP4 (H.264) of `w` × `h` px at `fps`
  and `quality`: a promise, for `fit-video-size`. Chrome stops at 4096 ×
  2304, where WebM (VP9) goes on."
  [fps quality]
  (let [{first-codec :first-codec quality-ctor :quality} (default-media)]
    (fn [w h]
      (-> (first-codec #js ["avc"]
                       #js {:width w
                            :height h
                            :quality (new quality-ctor #js {:bitrate (cta/video-bitrate w h quality)})
                            :frameRate fps})
          (.then some?)))))

(defn- encoder-ready
  "Resolve once `encoder` has room for another frame, so frames are not
  queued faster than they are encoded. Closing the encoder (also on an
  error) empties the queue and resolves it."
  [^js encoder]
  (if (< (.-encodeQueueSize encoder) 4)
    (js/Promise.resolve nil)
    (js/Promise.
     (fn [resolve]
       (.addEventListener encoder "dequeue"
                          (fn [] (resolve (encoder-ready encoder)))
                          #js {:once true})))))

(defn- av1-encoder
  "A VideoEncoder for `config` that collects its temporal units as AVIF
  samples in `chunks` and keeps an error in `failure`."
  [video-encoder config chunks failure]
  (let [^js encoder
        (new video-encoder
             #js {:output (fn [^js chunk _]
                            (let [data (js/Uint8Array. (.-byteLength chunk))]
                              (.copyTo chunk data)
                              (swap! chunks conj #js {:data data
                                                      :key (= "key" (.-type chunk))
                                                      :duration 1})))
                  :error (fn [cause] (reset! failure cause))})]
    (.configure encoder config)
    encoder))

(defn- encode-i420!
  "Encode the I420 picture `yuv` as frame `i`, a key frame every second,
  with the AV1 `quantizer` if given."
  [video-frame ^js encoder yuv color-space i fps quantizer]
  (let [step (/ 1000000.0 fps)
        ^js frame
        (new video-frame (:data yuv)
             #js {:format (:format yuv)
                  :codedWidth (:width yuv)
                  :codedHeight (:height yuv)
                  :timestamp (mth/round (* i step))
                  :duration (mth/round step)
                  :colorSpace color-space})]
    (try
      (.encode encoder frame (let [key? (zero? (mod i fps))]
                               (if (some? quantizer)
                                 #js {:keyFrame key? :av1 #js {:quantizer quantizer}}
                                 #js {:keyFrame key?})))
      (finally
        (.close frame)))))

(defn encode-avif
  "Encode decoded frames as an animated AVIF: AV1 through WebCodecs, in
  the container `avif.js` writes. Returns a promise of `{:status
  :ok|:unavailable :blob ...}`.

  Frames are composited onto `flatten-bg`. With `keep-alpha?`, frames
  with a pixel that is not fully opaque keep their transparency instead:
  a second encoder writes their alpha plane to an alpha track."
  [{:keys [frames fps bitrate loop? cancelled? on-progress flatten-bg keep-alpha? yield]
    :as params}]
  (let [yield (or yield default-yield)
        {:keys [video-encoder video-frame write-avif]}
        (merge (default-av1)
               (select-keys params [:video-encoder :video-frame :write-avif]))

        [_ raw-w raw-h] (when (seq frames) (image-parts (first frames)))
        w               (when raw-w (even-dim raw-w))
        h               (when raw-h (even-dim raw-h))]
    (if (or (nil? video-encoder) (nil? video-frame) (nil? w))
      (js/Promise.resolve {:status :unavailable})
      (let [config #js {:codec (av1-codec w h)
                        :width w
                        :height h
                        :bitrate bitrate
                        :framerate fps
                        :latencyMode "quality"}]
        (-> (.isConfigSupported ^js video-encoder config)
            (.then
             (fn [^js support]
               (if-not (.-supported support)
                 {:status :unavailable}
                 (-> (if keep-alpha?
                       (find-transparency frames yield cancelled?)
                       (js/Promise.resolve false))
                     (.then
                      (fn [alpha?]
                        (if alpha?
                          (.then (alpha-config video-encoder config) #(vector true %))
                          [false nil])))
                     (.then
                      (fn [[alpha? ^js alpha-settings]]
                        (let [chunks       (atom [])
                              alpha-chunks (atom [])
                              failure      (atom nil)
                              n            (count frames)
                              flatten-bg   (when-not alpha? flatten-bg)

                              ^js encoder
                              (av1-encoder video-encoder config chunks failure)

                              ^js alpha-encoder
                              (when alpha?
                                (av1-encoder video-encoder alpha-settings alpha-chunks failure))

                              quantizer
                              (when (= "quantizer" (some-> alpha-settings .-bitrateMode))
                                alpha-quantizer)

                              encoders (filterv some? [encoder alpha-encoder])

                              check!
                              (fn []
                                (throw-if-cancelled cancelled?)
                                (when-let [cause @failure]
                                  (throw cause)))]
                          (-> (reduce
                               (fn [p [i image]]
                                 (-> p
                                     (.then (fn [_]
                                              (check!)
                                              (js/Promise.all (into-array (map encoder-ready encoders)))))
                                     (.then
                                      (fn [_]
                                        (check!)
                                        (let [pixels (frame-pixels image flatten-bg)
                                              [data pw ph] ((juxt :data :width :height) pixels)]
                                          (encode-i420! video-frame encoder (rgba->i420 data pw ph)
                                                        av1-color-space i fps nil)
                                          (when alpha-encoder
                                            (encode-i420! video-frame alpha-encoder (alpha->i420 data pw ph)
                                                          av1-alpha-color-space i fps quantizer)))
                                        (when on-progress
                                          (on-progress (inc i) n))
                                        (yield)))))
                               (js/Promise.resolve nil)
                               (map-indexed vector frames))
                              (.then (fn [_] (js/Promise.all (into-array (map #(.flush ^js %) encoders)))))
                              (.then
                               (fn [_]
                                 (check!)
                                 (let [bytes (write-avif #js {:width w
                                                              :height h
                                                              :timescale fps
                                                              :loop (boolean loop?)
                                                              :color av1-cicp
                                                              :samples (into-array @chunks)
                                                              :alphaSamples (when alpha?
                                                                              (into-array @alpha-chunks))})]
                                   {:status :ok
                                    :blob (js/Blob. #js [bytes] #js {:type "image/avif"})
                                    :mtype "image/avif"
                                    :ext ".avif"})))
                              (.finally
                               (fn []
                                 (doseq [^js e encoders]
                                   (when (not= "closed" (.-state e))
                                     (.close e))))))))))))))))))

(defn- safe-filename
  [name ext]
  (str (-> (or name "animation")
           (str/replace #"[^a-zA-Z0-9_-]+" "-"))
       ext))

(defn approx-size-label
  [bytes]
  (cond
    (< bytes 1024)
    (str "~" bytes " B")

    (< bytes (* 1024 1024))
    (str "~" (mth/round (/ bytes 1024.0)) " KB")

    :else
    (str "~" (mth/precision (/ bytes (* 1024.0 1024.0)) 1) " MB")))

(defn estimated-size
  [format width height quality duration fps]
  (let [times (cta/frame-times duration fps)]
    (case format
      (:mp4 :webm) (cta/estimated-video-bytes width height quality duration)
      :avif (mth/round (* avif-bitrate-factor
                          (cta/estimated-video-bytes width height quality duration)))
      :gif (cta/estimated-gif-bytes width height (count times))
      0)))

(defn- u8->base64
  [bytes]
  (let [u8 (if (instance? js/Uint8Array bytes) bytes (js/Uint8Array. bytes))]
    (loop [i 0 acc ""]
      (if (>= i (.-length u8))
        (js/btoa acc)
        (let [end (js/Math.min (.-length u8) (+ i 0x8000))]
          (recur end
                 (str acc (.apply js/String.fromCharCode nil (.subarray u8 i end)))))))))

(defn- child-layer-ids
  [objects layer-id layer-set]
  (into []
        (filter layer-set)
        (rest (tree-seq (fn [id] (seq (get-in objects [id :shapes])))
                        (fn [id] (get-in objects [id :shapes] []))
                        layer-id))))

(defn- restore-rest-properties!
  "Give the shapes whose properties the animation preview at `time`
  changed their own ones back."
  [objects timeline time {:keys [apply-shape-properties]}]
  (doseq [[id ops] (property-ops (cta/timeline->modif-tree timeline objects time))]
    (when-let [shape (get objects id)]
      (apply-shape-properties shape (map (comp :property second) ops)))))

(defn unit-texts
  "The texts of `timeline` (as an export plays it, its split animations
  kept, see `dwa/export-timeline`) whose units the markup exports play
  one by one (see `cta/unit-timelines`): `{text-id units}`. A text that
  underlines or strikes some of its text plays whole (see `decorated?`)."
  [objects timeline]
  (into {}
        (remove (fn [[id _]] (decorated? (get objects id))))
        (cta/unit-timelines (cta/resolve-animations timeline objects) objects)))

(defn- restore-units!
  "The text `shape` back at rest, its units as designed."
  [shape {:keys [apply-shape-properties]}]
  (apply-shape-properties (assoc shape :text-units []) [:opacity :text-units]))

(defn- rest-units
  "The units `bases` (see `cta/unit-timelines`) at rest, shown."
  [bases]
  (mapv #(assoc % :dx 0 :dy 0 :rotation 0 :scale-x 1 :scale-y 1 :opacity 1 :blur 0) bases))

(defn collect-unit-vectors
  "A promise of the vectors of the units of the texts `texts` (see
  `unit-texts`) of `timeline` for an SVG, as `timeline->svg` takes them:
  `[text-id k]` `{:markup :x :y :width :height :box}`, the outlines of
  the glyphs of the unit `k` at rest (see `svgv/unit-markup`), the
  centre of its box in the SVG, which it turns and scales around, and
  what they cover, where its blur plays (see `cta/svg-unit-blurs`), and `[text-id
  :rest]` `{:markup}`, the glyphs of no unit. WASM outlines them
  (`bridge`, see `wasm.api/text-unit-outlines`), a text at a time, which
  it sets back at rest; the page gets its turn (`yield`) after each,
  `on-progress` hears how many are done, and it stops there when
  `cancelled?`. For Lottie (`lottie?`), as `timeline->lottie` takes them:
  `{:runs [{:content :fills}] :center}` on the page."
  [objects timeline texts {:keys [bridge yield cancelled? on-progress lottie?]
                           :or {bridge (default-bridge)}}]
  (let [board    (get objects (:board-id timeline))
        ox       (or (:x (:selrect board)) 0)
        oy       (or (:y (:selrect board)) 0)
        resolved (cta/resolve-animations timeline objects)
        {:keys [apply-shape-properties text-unit-outlines clean-modifiers]} bridge
        yield    (or yield default-yield)
        n        (count texts)]
    (when clean-modifiers
      (clean-modifiers))
    (reduce
     (fn [p [i [text-id units]]]
       (.then p (fn [images]
                  (throw-if-cancelled cancelled?)
                  (let [shape    (get objects text-id)
                        outlines (try
                                   (apply-shape-properties (assoc shape :opacity 1
                                                                  :text-units (rest-units (:bases units)))
                                                           [:opacity :text-units])
                                   (text-unit-outlines text-id)
                                   (finally
                                     (restore-units! shape bridge)))
                        drawing  (fn [runs center]
                                   (if lottie?
                                     {:runs (mapv (fn [{:keys [paragraph span content]}]
                                                    {:content content
                                                     :fills (svgv/span-fills shape paragraph span)})
                                                  runs)
                                      :center center}
                                     {:markup (svgv/unit-markup shape runs (dm/get-in resolved [:tracks text-id]))
                                      :x (- (:x center) ox)
                                      :y (- (:y center) oy)
                                      ;; what its glyphs cover, where its blur plays
                                      :box (when (seq runs)
                                             (let [{:keys [x y width height]}
                                                   (grc/join-rects (map #(path/calc-selrect (:content %)) runs))]
                                               {:x (- x ox) :y (- y oy) :width width :height height}))
                                      :width 0
                                      :height 0}))
                        images   (reduce (fn [images {:keys [unit center runs]}]
                                           (assoc images [text-id (if (some? unit) unit :rest)]
                                                  (drawing runs center)))
                                         images
                                         outlines)]
                    (when on-progress
                      (on-progress (inc i) n))
                    (.then (yield) (fn [_] images))))))
     (js/Promise.resolve {})
     (map-indexed vector texts))))

(defn- layer-ids-to-shoot
  "The layers of a Lottie export of `timeline` (see
  `cta/lottie-export-layer-ids`) and its board, the pictures
  `collect-layer-images` takes, but those `skip?` is true of."
  [objects timeline skip?]
  (cond->> (conj (cta/lottie-export-layer-ids objects (cta/resolve-animations timeline objects))
                 (:board-id timeline))
    (some? skip?) (remove skip?)))

(defn- layer-image
  "The picture of the layer `id` at rest for the markup exports, its
  layers in `idset` hidden (see `collect-layer-images`): `[id image]`,
  nil when the shape is gone."
  [objects id idset board bridge]
  (when-let [shape (get objects id)]
    (let [ox    (or (:x (:selrect board)) 0)
          oy    (or (:y (:selrect board)) 0)
          hide  (child-layer-ids objects id idset)
          shot  (shoot-shape-at-rest objects id hide {:bridge bridge})
          bytes (:bytes shot)
          ;; where the picture is: the box it covers, turned shape,
          ;; shadows and blur in it, or the shape's box
          box   (or (:rect shot) (:selrect shape))
          href  (str "data:image/png;base64," (u8->base64 bytes))]
      [id {:href href
           :x (- (or (:x box) 0) ox)
           :y (- (or (:y box) 0) oy)
           :width (or (:width box) 0)
           :height (or (:height box) 0)
           :bytes bytes
           :id (str "img_" id)
           :w (mth/round (or (:width box) 0))
           :h (mth/round (or (:height box) 0))
           :p href
           :e 1}])))

(defn collect-layer-images
  "A promise of the pictures of the export layers at rest, but of those
  whose id `skip?` is true of. Children that are themselves layers are
  hidden so they are not baked into the parent; the board itself is
  pictured without them. The animation shown at `time` is undone first,
  else a layer that fades in would be pictured invisible. A layer at a
  time: the page gets its turn (`yield`) after each picture,
  `on-progress` hears how many are taken, and it stops there when
  `cancelled?`."
  [objects timeline {:keys [bridge skip? time yield cancelled? on-progress]
                     :or {bridge (default-bridge) time 0}}]
  (let [idset (set (layer-ids-to-shoot objects timeline nil))
        ids   (vec (layer-ids-to-shoot objects timeline skip?))
        board (get objects (:board-id timeline))
        yield (or yield default-yield)
        n     (count ids)]
    (restore-rest-properties! objects timeline time bridge)
    (reduce (fn [p [i id]]
              (-> p
                  (.then
                   (fn [images]
                     (throw-if-cancelled cancelled?)
                     (let [image (layer-image objects id idset board bridge)]
                       (when on-progress (on-progress (inc i) n))
                       (-> (yield)
                           (.then (fn [_] (cond-> images (some? image) (conj image))))))))))
            (js/Promise.resolve {})
            (map-indexed vector ids))))

(defn- decode-png
  [bytes]
  (js/Promise.
   (fn [resolve reject]
     (let [blob (js/Blob. #js [bytes] #js {:type "image/png"})
           url  (wapi/create-uri blob)
           img  (js/Image.)]
       (set! (.-onload img)
             (fn []
               (let [w   (.-naturalWidth img)
                     h   (.-naturalHeight img)
                     c   (js/document.createElement "canvas")]
                 (set! (.-width c) w)
                 (set! (.-height c) h)
                 (let [ctx (.getContext c "2d")]
                   (.drawImage ctx img 0 0)
                   (wapi/revoke-uri url)
                   (resolve (.getImageData ctx 0 0 w h))))))
       (set! (.-onerror img)
             (fn [err]
               (wapi/revoke-uri url)
               (reject err)))
       (set! (.-src img) url)))))

(defn board-offset
  "Where the board, its `selrect`, starts in a picture that covers `rect`
  (see `wasm.api/get-shape-extrect`) at `scale`: `[x y]` in pixels."
  [selrect rect scale]
  [(mth/round (* scale (- (:x selrect) (:x rect))))
   (mth/round (* scale (- (:y selrect) (:y rect))))])

(defn crop-frames
  "`frames` (ImageData or `{:data :width :height}`, RGBA) cut to the
  board, its `selrect` at `scale`: the export shows the board only. A
  board that does not clip its layers is pictured as far as they reach,
  which changes as they move. `rects` are the boxes each picture covers
  on the canvas (see `wasm.api/get-shape-extrect`), at `scale`; where a
  picture does not cover the board, it is transparent. Without them,
  `frames` as they are."
  [frames rects selrect scale]
  (if-not (and selrect
               (= (count rects) (count frames))
               (every? some? rects))
    frames
    (let [width  (mth/round (* scale (:width selrect)))
          height (mth/round (* scale (:height selrect)))]
      (mapv (fn [image rect]
              (let [[data w h] (image-parts image)
                    [ox oy]    (board-offset selrect rect scale)]
                (if (and (zero? ox) (zero? oy) (= w width) (= h height))
                  image
                  (let [out (js/Uint8ClampedArray. (* 4 width height))
                        x0  (max 0 ox)
                        x1  (min w (+ ox width))]
                    (when (< x0 x1)
                      (dotimes [row height]
                        (let [sy (+ oy row)]
                          (when (and (>= sy 0) (< sy h))
                            (.set out
                                  (.subarray data (* 4 (+ x0 (* sy w))) (* 4 (+ x1 (* sy w))))
                                  (* 4 (+ (- x0 ox) (* row width))))))))
                    {:data out :width width :height height}))))
            frames
            rects))))

(defn- with-board-clipped
  "Call `f`, which returns a promise, while `board` clips its layers in
  WASM (through `bridge`), and put it back as it is after. A board that
  does not clip them is pictured as far as they reach, which changes as
  they move, and with it where its pixels start: its layers would shake
  by a pixel from one frame to the next. Clipped, each picture is the
  board, on the same pixels."
  [bridge board f]
  (let [apply-properties (:apply-shape-properties bridge)]
    (if (and apply-properties (:show-content board))
      (do
        ;; with the selrect, which drops the box WASM keeps of the board
        (apply-properties (assoc board :show-content false) [:show-content :selrect])
        (-> (f)
            (.finally (fn []
                        (apply-properties board [:show-content :selrect])
                        nil))))
      (f))))

(defn- capture-frames
  "The pictures of the board `board-id` at `times`, decoded, cut to the
  board (see `with-board-clipped`, `crop-frames`); `on-frame` hears each
  as it is decoded: the picture, the box it covers (nil without a way to
  know it) and its scale."
  [{:keys [objects timeline board-id times bridge cancelled? on-progress on-frame
           yield decode scale]}]
  (let [decode   (or decode decode-png)
        yield    (or yield default-yield)
        extrect  (:get-shape-extrect bridge)
        n        (count times)]
    (with-board-clipped
      bridge (get objects board-id)
      (fn []
        (-> (reduce
             (fn [p t]
               (-> p
                   (.then
                    (fn [acc]
                      (throw-if-cancelled cancelled?)
                      (let [bytes (render-shape-at-time objects timeline board-id t
                                                        {:bridge bridge :scale (or scale 1)})
                            ;; where the picture is, as posed
                            rect  (when extrect (extrect board-id))]
                        (-> (decode bytes)
                            (.then (fn [image]
                                     (when on-frame
                                       (on-frame image rect (or scale 1)))
                                     (when on-progress
                                       (on-progress (inc (count acc)) n))
                                     (-> (yield)
                                         (.then (fn [_] (conj acc [image rect]))))))))))))
             (js/Promise.resolve [])
             times)
            (.then (fn [shots]
                     (crop-frames (mapv first shots)
                                  (mapv second shots)
                                  (:selrect (get objects board-id))
                                  (or scale 1)))))))))

(defn- download-blob
  [name ext mtype blob]
  (let [url (wapi/create-uri blob)]
    (dom/trigger-download-uri (safe-filename name ext) mtype url)
    (wapi/revoke-uri url)))

(defn- finish-export-state
  []
  (ptk/reify ::finish-export-state
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state :export-animation))))

(defn progress-percent
  "How far an export is, 0 to 100, from its `progress` (`{:phase :current
  :total}`): capturing the frames is the first 80 % of a raster export,
  encoding them the rest; the pictures of the layers are the first 90 %
  of a markup export, writing the file the rest. Of several boards (see
  `run-exports`), the `:board` of `:boards` exporting has its share."
  [{:keys [phase current total board boards]}]
  (let [f   (if (and (number? total) (pos? total)) (/ (or current 0) total) 0)
        one (case phase
              :capture (* 80 f)
              :encode  (+ 80 (* 20 f))
              :layers  (* 90 f)
              :write   (+ 90 (* 10 f))
              0)]
    (mth/round (if (and (number? boards) (> boards 1))
                 (/ (+ (* 100 (or board 0)) one) boards)
                 one))))

(defn- set-export-progress
  [progress]
  (ptk/reify ::set-export-progress
    ptk/UpdateEvent
    (update [_ state]
      (update state :export-animation merge
              (select-keys progress [:phase :current :total :board :boards])
              {:percent (progress-percent progress)}))))

(defn- phase-progress
  "The `on-progress` of a phase of an export: `on-progress` hears
  `{:phase :current :total}`."
  [on-progress phase]
  (fn [current total]
    (when on-progress
      (on-progress {:phase phase :current current :total total}))))

(defn- raster-export
  "A promise of the raster export (`:mp4` `:webm` `:gif` `:avif`) of
  `opts` (see `run-export`)."
  [{:keys [page files board-id format quality fps scale loop? audio? bridge
           decode yield on-progress on-frame cancelled?]
    :or {audio? true}
    :as opts}]
  (let [tl       (dwa/board-export-timeline page files board-id)
        ;; MP4 and WebM carry the clips of the board
        sound?   (and audio? (contains? #{:mp4 :webm} format))
        objects  (:objects page)
        board    (get objects board-id)
        scale    (or scale 1)
        bw       (* (or (:width (:selrect board)) 1) scale)
        bh       (* (or (:height (:selrect board)) 1) scale)
        times    (cta/frame-times (:duration tl) fps)
        encoding (phase-progress on-progress :encode)]
    (when sound?
      (prefetch-audio! tl))
    ;; An MP4 larger than the browser encodes is made as large as it does
    ;; (see `fit-video-size`), which `export-notes` tells.
    (-> (if (= format :mp4)
          (fit-video-size bw bh (mp4-encodable? fps quality))
          (js/Promise.resolve 1))
        (.then
         (fn [fit]
           (if (nil? fit)
             {:status :unavailable}
             (let [bw (* bw fit)
                   bh (* bh fit)]
               (-> (capture-frames {:objects objects
                                    :timeline tl
                                    :board-id board-id
                                    :times times
                                    :bridge bridge
                                    :cancelled? cancelled?
                                    :on-progress (phase-progress on-progress :capture)
                                    :on-frame on-frame
                                    :yield yield
                                    :decode decode
                                    :scale (* scale fit)})
                   ;; Ping-pong plays the captured frames back to the start.
                   ;; Videos do not loop, so they end on the first frame.
                   (.then
                    (fn [frames]
                      (cond-> frames
                        (= :ping-pong (cta/playback-mode tl))
                        (cta/ping-pong-frames (and loop? (not (#{:mp4 :webm} format)))))))
                   (.then
                    (fn [frames]
                      (case format
                        :gif (encode-gif (merge (select-keys opts [:gif-encoder :quantize :apply-palette])
                                                {:frames frames
                                                 :fps fps
                                                 :keep-alpha? true
                                                 :loop? (boolean loop?)
                                                 :colors (cta/gif-colors quality)
                                                 :cancelled? cancelled?
                                                 :on-progress encoding
                                                 :yield yield}))
                        :avif (encode-avif {:frames frames
                                            :fps fps
                                            :bitrate (avif-bitrate bw bh quality)
                                            :loop? (boolean loop?)
                                            :flatten-bg (board-backdrop board)
                                            :keep-alpha? true
                                            :cancelled? cancelled?
                                            :on-progress encoding
                                            :yield yield})
                        (:mp4 :webm)
                        (-> (if sound?
                              (export-audio tl (count frames) fps)
                              (js/Promise.resolve nil))
                            (.then
                             (fn [sound]
                               (encode-video {:format format
                                              :frames frames
                                              :fps fps
                                              :bitrate (cta/video-bitrate bw bh quality)
                                              :flatten-bg (board-backdrop board)
                                              :keep-alpha? true
                                              :audio sound
                                              :cancelled? cancelled?
                                              :on-progress encoding
                                              :yield yield})))))))
                   (.then
                    (fn [result]
                      (cond-> (assoc result :name (or (:name board) (:name tl)))
                        (< fit 1) (assoc :fitted [(even-dim bw) (even-dim bh)]))))))))))))

(def format-labels
  "How the formats of an animation export are written."
  {:mp4 "MP4"
   :webm "WebM"
   :gif "GIF"
   :avif "AVIF"
   :svg "SVG"
   :lottie "Lottie"
   :dotlottie "dotLottie"})

(def ^:private dotlottie-mtype
  "The media type of a dotLottie archive, whose file ends in `.lottie`."
  "application/zip+dotlottie")

(defn lottie->dotlottie
  "A promise of the dotLottie archive (a blob) of the Lottie document
  `doc`: `manifest.json` naming the animation `id`, the document as
  `animations/<id>.json`, and the PNG `images` (`{file-name bytes}`) its
  image assets point at, as `images/<file-name>`. `loop?` tells players
  whether it plays over and over."
  [doc id images loop?]
  (let [writer   (uz/writer (uz/blob-writer :mtype dotlottie-mtype))
        manifest {:version "1"
                  :generator "Penpot"
                  :animations [{:id id :loop loop? :autoplay true}]}
        files    (concat [["manifest.json" (js/JSON.stringify (clj->js manifest))]
                          [(str "animations/" id ".json") (js/JSON.stringify (clj->js doc))]]
                         (map (fn [[file-name bytes]] [(str "images/" file-name) bytes])
                              images))]
    (-> (reduce (fn [written [path content]]
                  (p/then written (fn [_] (uz/add writer path content))))
                (p/resolved nil)
                files)
        (p/then (fn [_] (uz/close writer))))))

(defn- markup-file
  "A promise of the SVG, Lottie or dotLottie of the board `board` playing
  `tl`, with the pictures `images` (see `collect-layer-images` and
  `collect-unit-vectors`): `{:status :ok :blob :ext :mtype}`. `opts` go
  to `cta/timeline->svg`, their `:propagate` to `cta/timeline->lottie`
  too."
  [tl objects board format images opts]
  (case format
    :svg
    (let [svg (cta/timeline->svg tl objects images opts)]
      (js/Promise.resolve {:status :ok
                           :blob (wapi/create-blob svg "image/svg+xml")
                           :ext ".svg"
                           :mtype "image/svg+xml"}))

    :lottie
    (let [assets (into {} (map (fn [[id img]]
                                 [id (select-keys img [:id :w :h :p :e :x :y :runs :center])]))
                       images)
          data   (cta/timeline->lottie tl objects assets (select-keys opts [:propagate]))
          json   (js/JSON.stringify (clj->js data) nil 2)]
      (js/Promise.resolve {:status :ok
                           :blob (wapi/create-blob json "application/json")
                           :ext ".json"
                           :mtype "application/json"}))

    ;; the images go into the archive, next to the document
    :dotlottie
    (let [name      (or (:name board) (:name tl))
          file-name #(str (:id %) ".png")
          ;; the outlines of the units of texts are no images
          assets    (into {} (map (fn [[id img]]
                                    [id (if (some? (:runs img))
                                          (select-keys img [:runs :center])
                                          (-> (select-keys img [:id :w :h :x :y])
                                              (assoc :u "/images/" :p (file-name img) :e 0)))]))
                          images)
          data      (cta/timeline->lottie tl objects assets (select-keys opts [:propagate]))
          files     (into {} (keep (fn [[_ img]] (when (some? (:bytes img)) [(file-name img) (:bytes img)])))
                          images)]
      (p/let [blob (lottie->dotlottie data (safe-filename name "") files
                                      (not= :once (cta/playback-mode tl)))]
        {:status :ok :blob blob :ext ".lottie" :mtype dotlottie-mtype}))))

(defn outline-texts
  "`objects` with the texts in the board of `tl`, but the split texts of
  `texts` (their units play on their own), as the outlines of their
  glyphs at rest, which WASM gives through `bridge`: `[drawn view]`, the
  objects an SVG draws (see `svgv/outlined`) and the ones its CSS and a
  Lottie take (see `svgv/outlined-view`); a text that paints its text in
  more than one way (see `svgv/text-fills`) is a group of paths in both
  (see `svgv/outlined-group`)."
  [objects tl texts bridge]
  (let [{:keys [shape-to-path text-unit-outlines clean-modifiers]} bridge]
    (when clean-modifiers
      (clean-modifiers))
    (reduce (fn [[drawn view :as both] id]
              (let [shape (get objects id)
                    fills (svgv/text-fills shape)]
                (if (some? fills)
                  (let [content (when shape-to-path (shape-to-path id))]
                    (if (and (some? content) (pos? (count content)))
                      [(assoc drawn id (svgv/outlined shape content fills))
                       (assoc view id (svgv/outlined-view shape content fills))]
                      both))
                  (let [runs (when text-unit-outlines
                               (into [] (mapcat :runs) (text-unit-outlines id)))]
                    (if (seq runs)
                      (let [group (svgv/outlined-group shape runs)]
                        [(merge drawn group) (merge view group)])
                      both)))))
            [objects objects]
            (->> (cfh/get-children-ids-with-self objects (:board-id tl))
                 (filter #(let [shape (get objects %)]
                            (and (cfh/text-shape? shape) (not (:hidden shape)))))
                 (remove texts)))))

(defn- vector-layers
  "A promise of the vectors of the layers of the SVG export of `tl` and of
  its board, at rest (see `app.main.data.exports.svg-vectors`), but of
  the split texts of `texts`, which play their units as pictures:
  `{:images {id {:markup}} :fonts css :objects}`, the `@font-face` CSS of
  their texts and the objects its CSS takes. With `outline?`, its texts
  as the outlines of their glyphs (see `outline-texts`). A layer at a
  time, as `collect-layer-images` takes them."
  [objects tl texts {:keys [yield cancelled? on-progress outline? bridge]}]
  (let [board-id (:board-id tl)
        resolved (cta/resolve-animations tl objects)
        layers   (cta/svg-export-layer-ids objects resolved)
        drawn    (into [board-id] (remove texts) layers)
        [shapes view] (if outline?
                        (outline-texts objects tl texts (or bridge (default-bridge)))
                        [objects objects])
        drawable (svgv/without-layers shapes (set layers))
        yield    (or yield default-yield)
        n        (count drawn)]
    (p/let [_     (svgv/load-media objects)
            ;; but those of the split texts, whose units are pictures
            fonts (svgv/fonts-css (apply dissoc shapes (keys texts)) drawn)
            images
            (reduce (fn [p [i id]]
                      (.then p (fn [images]
                                 (throw-if-cancelled cancelled?)
                                 (let [markup (svgv/layer-markup drawable id (dm/get-in resolved [:tracks id]))]
                                   (when on-progress (on-progress (inc i) n))
                                   (.then (yield) (fn [_] (assoc images id {:markup markup})))))))
                    (js/Promise.resolve {})
                    (map-indexed vector drawn))]
      {:images images :fonts fonts :objects view})))

(defn- markup-export
  "A promise of the markup export (`:svg` `:lottie` `:dotlottie`) of
  `opts` (see `run-export`): the pictures of its layers one at a time,
  so it stops between them when `cancelled?`."
  [{:keys [page files board-id format bridge time yield decode on-progress on-frame cancelled? ids? bbox?
           outline?]}]
  (let [tl       (dwa/board-export-timeline page files board-id)
        objects  (:objects page)
        board    (get objects board-id)
        ;; split texts play their units, each on its own
        texts    (unit-texts objects tl)
        vectors? (= :svg format)
        lottie?  (contains? #{:lottie :dotlottie} format)
        ;; Lottie draws the other texts as the outlines of their glyphs
        ;; (see `outline-texts`)
        shapes   (if lottie? (second (outline-texts objects tl texts bridge)) objects)
        ;; Lottie draws the layers it can as vectors, SVG all (see
        ;; `vector-layers`)
        layers   (set (cta/lottie-export-layer-ids shapes (cta/resolve-animations tl shapes)))
        skip?    #(or vectors?
                      (contains? texts %)
                      (and lottie? (cta/lottie-vector-layer? shapes layers %)))
        ;; the pictures (or vectors) of the layers, then of the units of
        ;; split texts
        n-layers (if vectors?
                   (->> (cta/svg-export-layer-ids objects (cta/resolve-animations tl objects))
                        (cons board-id)
                        (remove texts)
                        (count))
                   (count (layer-ids-to-shoot shapes tl skip?)))
        ;; the units of a split text are outlined at once
        total    (+ n-layers (count texts))
        pictures (phase-progress on-progress :layers)]
    ;; the board at its end, where what comes in has come
    (when on-frame
      (let [bytes (render-shape-at-time objects tl board-id (:duration tl) {:bridge bridge})
            rect  (when-let [extrect (:get-shape-extrect bridge)]
                    (extrect board-id))]
        (-> ((or decode decode-png) bytes)
            (.then #(on-frame % rect 1))
            ;; only a picture to look at
            (.catch (constantly nil)))))
    (p/let [vectors (when vectors?
                      (vector-layers objects tl texts
                                     {:yield yield
                                      :outline? outline?
                                      :bridge bridge
                                      :cancelled? cancelled?
                                      :on-progress (fn [i _] (pictures i total))}))
            images  (if vectors?
                      (:images vectors)
                      (collect-layer-images
                       shapes tl
                       {:bridge bridge
                        :time (or time 0)
                        :yield yield
                        :cancelled? cancelled?
                        :on-progress (fn [i _] (pictures i total))
                        :skip? skip?}))
            units  (do (throw-if-cancelled cancelled?)
                       (collect-unit-vectors
                        objects tl texts
                        {:bridge bridge
                         :yield yield
                         :lottie? lottie?
                         :cancelled? cancelled?
                         :on-progress (fn [j _] (pictures (+ n-layers j) total))}))
            result (do (throw-if-cancelled cancelled?)
                       (markup-file tl (if lottie? shapes (or (:objects vectors) objects)) board format
                                    (merge images units)
                                    {:ids? ids? :bbox? bbox? :fonts (:fonts vectors)
                                     :propagate cgm/set-objects-modifiers}))]
      (when on-progress
        (on-progress {:phase :write :current 1 :total 1}))
      (assoc result :name (or (:name board) (:name tl))))))

(defn run-export
  "Export the animation of the board `board-id` of `page`, with the one of
  the component copies in it (`files` holds the files of their
  components, see `dwa/board-export-timeline`), as `format` (`:mp4`
  `:webm` `:gif` `:avif` `:svg` `:lottie` `:dotlottie`), with
  `:quality` `:fps` `:scale` `:loop?`, `:audio?` (the sound of its
  clips in MP4 and WebM, true by default), and for SVG `:ids?`,
  `:bbox?` (see `cta/timeline->svg`) and `:outline?`, its texts as the
  outlines of their glyphs (see `outline-texts`). `ctx` holds `page`
  `files` `board-id`, the `:time` the shapes show (undone for the
  pictures of the markup exports), and may replace the `:bridge` to
  WASM, `:decode`,
  `:yield` and the GIF encoder (`:gif-encoder` `:quantize`
  `:apply-palette`, see `encode-gif`); `:on-progress` hears `{:phase
  :current :total}` (see `progress-percent`), `:on-frame` the pictures
  of the board as they are taken (decoded, as `:decode` does, with the
  box each covers and its scale: each frame of a raster export, once
  the board at its end for a markup export). Neither reads nor changes
  the workspace.

  `{:promise :cancel}`: the promise of the result, `{:status :ok :blob
  :name :ext :mtype}` with what `export-notes` tells, `{:status
  :unavailable}` (no codec), or `{:status :nothing}` (nothing plays);
  `cancel` makes it reject with `:export-cancelled` at its next frame or
  picture."
  [{:keys [page files board-id] :as ctx} params]
  (let [cancel? (atom false)
        opts    (-> (merge ctx params)
                    (update :bridge #(or % (default-bridge)))
                    (assoc :cancelled? #(deref cancel?)))]
    {:cancel  #(reset! cancel? true)
     :promise (cond
                (nil? (dwa/board-export-timeline page files board-id))
                (js/Promise.resolve {:status :nothing})

                ;; a native promise, as the others (`p/let` makes one of promesa)
                (contains? markup-formats (:format params))
                (js/Promise.resolve (markup-export opts))

                :else
                (raster-export opts))}))

(defn- file-names
  "The names in a ZIP of the files of `results` (see `run-export`): each
  after its board, a number added to a name that comes again."
  [results]
  (first
   (reduce (fn [[names seen] {:keys [name ext]}]
             (let [base  (safe-filename name "")
                   n     (get seen base 0)
                   fname (if (zero? n) (str base ext) (str base "-" (inc n) ext))]
               [(conj names fname) (assoc seen base (inc n))]))
           [[] {}]
           results)))

(defn- zip-results
  "A promise of the ZIP (a blob) of the files of `results`."
  [results]
  (let [writer (uz/writer (uz/blob-writer :mtype "application/zip"))]
    ;; a native promise, as the others (`p/then` makes one of promesa)
    (js/Promise.resolve
     (-> (reduce (fn [written [fname {:keys [blob]}]]
                   (p/then written (fn [_] (uz/add writer fname blob))))
                 (p/resolved nil)
                 (map vector (file-names results) results))
         (p/then (fn [_] (uz/close writer)))))))

(defn run-exports
  "Export the boards `boards` (`[{:board-id :scale}]`) of the page of
  `ctx` as `params` (see `run-export`), one after another:
  `{:promise :cancel}`. One board gives its result; several give a ZIP
  of their files (`:name` `name`, `:files` their names, the boards that
  play nothing left out), with the notes of the first fitted one and the dropped audio of
  any (see `export-notes`). `:on-progress` hears the progress of each
  board with `:board`, its index, and `:boards`, how many (see
  `progress-percent`); `cancel` stops the one exporting."
  [{:keys [on-progress] :as ctx} params boards name]
  (if (<= (count boards) 1)
    (let [{:keys [board-id scale]} (first boards)]
      (run-export (assoc ctx :board-id board-id) (assoc params :scale scale)))
    (let [n          (count boards)
          cancelled? (atom false)
          cancel-one (atom nil)
          promise
          (-> (reduce
               (fn [p [i {:keys [board-id scale]}]]
                 (.then p (fn [results]
                            (when @cancelled?
                              (throw (ex-info "cancelled" {:type :export-cancelled})))
                            (let [{:keys [promise cancel]}
                                  (run-export (assoc ctx
                                                     :board-id board-id
                                                     :on-progress #(when on-progress
                                                                     (on-progress (assoc % :board i :boards n))))
                                              (assoc params :scale scale))]
                              (reset! cancel-one cancel)
                              (.then promise #(conj results %))))))
               (js/Promise.resolve [])
               (map-indexed vector boards))
              (.then
               (fn [results]
                 (let [done (filterv #(= :ok (:status %)) results)]
                   (if (empty? done)
                     (or (d/seek #(= :unavailable (:status %)) results)
                         {:status :nothing})
                     (.then (zip-results done)
                            (fn [blob]
                              (cond-> {:status :ok
                                       :blob blob
                                       :name name
                                       :ext ".zip"
                                       :mtype "application/zip"
                                       :files (file-names done)}
                                (some :fitted done)        (assoc :fitted (some :fitted done))
                                (some :audio-dropped done) (assoc :audio-dropped true)))))))))]
      {:promise promise
       :cancel (fn []
                 (reset! cancelled? true)
                 (when-let [cancel @cancel-one]
                   (cancel)))})))

(defn download-result
  "Download the file of an export `result` (see `run-export`)."
  [{:keys [name ext mtype blob]}]
  (download-blob name ext mtype blob))

(defn export-notes
  "What an export ending with `result` (see `run-export`) tells:
  `{:unavailable? :notes}`, the notes `[:fitted w h]` (it was made as
  large as the browser encodes, see `fit-video-size`) and
  `[:audio-dropped]` (no audio codec of the format, see `encode-video`)."
  [result]
  (if (= :unavailable (:status result))
    {:unavailable? true :notes []}
    {:unavailable? false
     :notes (cond-> []
              (:fitted result)        (conj (into [:fitted] (:fitted result)))
              (:audio-dropped result) (conj [:audio-dropped]))}))

(defn note-text
  "The text of a note of `export-notes`."
  [[kind & args]]
  (case kind
    :fitted        (apply tr "workspace.options.export.fitted" args)
    :audio-dropped (tr "workspace.options.export.audio-dropped")
    nil))

(defn notes-shown
  "The notification of the `notes` of an export (see `export-notes`), if
  any."
  [notes]
  (when (seq notes)
    [(ntf/warn (str/join " " (map note-text notes)))]))

(defn export-finished
  "What an export ending with `result` shows: that the browser encodes
  none of the codecs of the format, or what the file had to go without
  (see `export-notes`); and the export panel free again either way."
  [result]
  (let [{:keys [unavailable? notes]} (export-notes result)]
    (if unavailable?
      [(ntf/error (tr "workspace.options.export.codec-unavailable"))
       (finish-export-state)]
      (conj (vec (notes-shown notes)) (finish-export-state)))))

(defn- export-in-place
  "A promise of the export of the active board, or of the `:boards` of
  `params` into a ZIP named after the file (see `run-exports`), run in
  the workspace tab, downloaded: the canvas renders in full quality
  meanwhile and shows the animation at the playhead again after."
  [state params]
  (let [bridge   (or (:bridge params) (default-bridge))
        was-fast (begin-full-quality! bridge)
        boards   (or (not-empty (:boards params))
                     [{:board-id (dwa/active-board-id state) :scale (or (:scale params) 1)}])
        {:keys [promise cancel]}
        (run-exports {:page (dsh/lookup-page state)
                      :files (dsh/lookup-libraries state)
                      :time (dwa/playhead state)
                      :bridge bridge
                      :on-progress #(st/emit! (set-export-progress %))}
                     (dissoc params :bridge :boards :scale)
                     boards
                     (:name (dsh/lookup-file state)))]
    (reset! cancel-fn cancel)
    (-> promise
        (.then
         (fn [result]
           (when (= :ok (:status result))
             (download-result result))
           result))
        (.finally
         (fn []
           (reset! cancel-fn nil)
           (st/emit! (dwa/apply-preview))
           (when (and was-fast (not (wasm-gesture/active?)))
             (wasm-gesture/try-begin-interactive-transform!)
             (wasm.api/set-modifiers-start)))))))

(defn export-animation
  "Export the animation of the active board, with the one of the
  component copies in it (see `dwa/export-timeline`), as `format`
  (`:mp4` `:webm` `:gif` `:avif` `:svg` `:lottie` `:dotlottie`); with
  the sound of its audio clips for MP4 and WebM unless `:audio?` is
  false."
  [{:keys [format] :as params}]
  (ptk/reify ::export-animation
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :export-animation {:in-progress true
                                      :current 0
                                      :total 0
                                      :percent 0
                                      :cancelling? false
                                      :format format}))

    ptk/WatchEvent
    (watch [_ state _]
      (let [tl (or (seq (:boards params)) (dwa/export-timeline state))]
        (if (nil? tl)
          (rx/of (finish-export-state))
          (rx/concat
           (rx/of (ev/event {::ev/name "export-animation"
                             ::ev/origin "workspace:sidebar"
                             :format (name format)}))
           (->> (rx/from (export-in-place state params))
                (rx/mapcat (comp rx/from export-finished))
                (rx/catch
                 (fn [cause]
                   (if (= :export-cancelled (:type (ex-data cause)))
                     (rx/of (finish-export-state))
                     (rx/of (ntf/error (or (ex-message cause)
                                           (tr "workspace.options.export.codec-unavailable")))
                            (finish-export-state))))))))))))

(defn cancel-in-place!
  "Cancel the export run in the workspace tab, if one runs (see
  `export-animation`): it stops at its next frame or picture."
  []
  (when-let [cancel @cancel-fn]
    (cancel)))
