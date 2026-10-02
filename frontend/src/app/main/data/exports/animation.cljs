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
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.color :as clr]
   [app.common.types.modifiers :as ctm]
   [app.main.data.event :as ev]
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

(defn render-shape-at-rest
  "Rasterize `shape-id` in rest pose, hiding `hide-ids` for the shot. It
  is rasterized opaque: the layer of the export has its opacity, which
  its animation may change. A text shows its units at rest, whatever the
  canvas posed (see `cta/text-unit-entries`)."
  [objects shape-id hide-ids {:keys [bridge] :or {bridge (default-bridge)}}]
  (let [{:keys [use-shape set-shape-hidden clean-modifiers
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
      (render-shape-pixels shape-id 1 :png)
      (finally
        (when (and apply-shape-properties shape)
          (apply-shape-properties shape [:opacity]))
        (doseq [id hide-ids]
          (let [shape (get objects id)]
            (if apply-shape-properties
              (apply-shape-properties shape [:hidden])
              (do (use-shape id)
                  (set-shape-hidden (boolean (:hidden shape)))))))))))

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

(defn board-backdrop
  [board]
  (clr/hex->rgb (or (-> board :fills first :fill-color) "#ffffff")))

(defn- image-parts
  [image]
  (if (map? image)
    [(:data image) (:width image) (:height image)]
    [(.-data image) (.-width image) (.-height image)]))

(defn- default-yield
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve 0))))

(defn- throw-if-cancelled
  [cancelled?]
  (when (and cancelled? (cancelled?))
    (throw (ex-info "cancelled" {:type :export-cancelled}))))

(defn encode-gif
  "Encode decoded frames with gifenc. `frames` is a seq of ImageData or
  `{:data :width :height}` maps."
  [{:keys [frames fps loop? colors cancelled? on-progress
           gif-encoder quantize apply-palette]
    :or {colors 128 fps 30}}]
  (let [encoder      (or gif-encoder gifenc/GIFEncoder)
        quantize*    (or quantize gifenc/quantize)
        apply-pal*   (or apply-palette gifenc/applyPalette)
        ^js gif      (encoder)
        delay        (max 1 (mth/round (/ 1000.0 fps)))
        n            (count frames)]
    (doseq [[i image] (map-indexed vector frames)]
      (throw-if-cancelled cancelled?)
      (let [[data w h] (image-parts image)
            palette    (quantize* data colors)
            index      (apply-pal* data palette)]
        (.writeFrame gif index w h
                     #js {:palette palette
                          :delay delay
                          :repeat (if loop? 0 -1)}))
      (when on-progress (on-progress (inc i) n)))
    (.finish gif)
    (let [bytes (.bytes gif)]
      {:status :ok
       :blob (js/Blob. #js [bytes] #js {:type "image/gif"})
       :mtype "image/gif"
       :ext ".gif"})))

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

(def ^:private whole-text-formats
  "The formats that play the split animations of a text as animations of
  the whole text (see `cta/flatten-splits`)."
  #{:svg :lottie :dotlottie})

(def audio-formats
  "The formats that carry the sound of the audio clips."
  #{:mp4 :webm})

(defn export-caveats
  "The notes the export of `timeline` (of the shapes of `objects`) needs,
  by format, as the Animated export tab shows them: `:split-text` for the
  formats that play a text split into characters, words or lines as a
  whole (see `cta/split-animations?`), and `:no-audio` for the ones
  without the sound of its audio clips."
  [objects timeline]
  (let [split? (and (some? timeline) (cta/split-animations? timeline objects))
        audio? (seq (:audio-clips timeline))]
    (reduce (fn [caveats format]
              (cond-> caveats
                (and split? (contains? whole-text-formats format))
                (update format (fnil conj []) :split-text)

                (and audio? (not (contains? audio-formats format)))
                (update format (fnil conj []) :no-audio)))
            {}
            [:gif :avif :svg :lottie :dotlottie])))

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

(defn encode-video
  "Encode decoded frames with Mediabunny, with the sound `audio` (an
  `AudioBuffer` as long as the frames) when given. Returns a promise of
  `{:status :ok|:unavailable :blob ...}`, with `:audio-dropped true` when
  the browser encodes no audio codec the format carries."
  [{:keys [format frames fps bitrate cancelled? on-progress flatten-bg audio]
    :as params}]
  (let [media           (merge (default-media)
                               (select-keys params [:output :mp4-format :webm-format
                                                    :target :sample-source :video-sample
                                                    :quality :first-codec
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
                     (.getSupportedVideoCodecs fmt))]
        (-> (js/Promise.all
             #js [(first-codec codecs
                               #js {:width w
                                    :height h
                                    :quality (new quality-ctor #js {:bitrate bitrate})
                                    :frameRate fps})
                  (if (some? audio)
                    (audio-codec format media)
                    (js/Promise.resolve nil))])
            (.then
             (fn [[codec acodec]]
               (if (nil? codec)
                 {:status :unavailable}
                 (let [source  (new source-ctor
                                    #js {:codec codec
                                         :quality (new quality-ctor #js {:bitrate bitrate})
                                         :alpha "discard"})
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
                                          yuv    (rgba->i420 (:data pixels)
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
                                               (on-progress (inc i) n))))))))))
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

(defn encode-avif
  "Encode decoded frames as an animated AVIF: AV1 through WebCodecs, in
  the container `avif.js` writes. Returns a promise of `{:status
  :ok|:unavailable :blob ...}`."
  [{:keys [frames fps bitrate loop? cancelled? on-progress flatten-bg]
    :as params}]
  (let [{:keys [video-encoder video-frame write-avif]}
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
                 (let [chunks  (atom [])
                       failure (atom nil)
                       n       (count frames)
                       step    (/ 1000000.0 fps)

                       ^js encoder
                       (new video-encoder
                            #js {:output (fn [^js chunk _]
                                           (let [data (js/Uint8Array. (.-byteLength chunk))]
                                             (.copyTo chunk data)
                                             (swap! chunks conj #js {:data data
                                                                     :key (= "key" (.-type chunk))
                                                                     :duration 1})))
                                 :error (fn [cause] (reset! failure cause))})

                       check!
                       (fn []
                         (throw-if-cancelled cancelled?)
                         (when-let [cause @failure]
                           (throw cause)))]
                   (.configure encoder config)
                   (-> (reduce
                        (fn [p [i image]]
                          (-> p
                              (.then (fn [_] (check!) (encoder-ready encoder)))
                              (.then
                               (fn [_]
                                 (check!)
                                 (let [pixels (frame-pixels image flatten-bg)
                                       yuv    (rgba->i420 (:data pixels)
                                                          (:width pixels)
                                                          (:height pixels))
                                       ^js frame
                                       (new video-frame (:data yuv)
                                            #js {:format (:format yuv)
                                                 :codedWidth (:width yuv)
                                                 :codedHeight (:height yuv)
                                                 :timestamp (mth/round (* i step))
                                                 :duration (mth/round step)
                                                 :colorSpace av1-color-space})]
                                   (try
                                     (.encode encoder frame #js {:keyFrame (zero? (mod i fps))})
                                     (finally
                                       (.close frame)))
                                   (when on-progress
                                     (on-progress (inc i) n)))))))
                        (js/Promise.resolve nil)
                        (map-indexed vector frames))
                       (.then (fn [_] (.flush encoder)))
                       (.then
                        (fn [_]
                          (check!)
                          (let [bytes (write-avif #js {:width w
                                                       :height h
                                                       :timescale fps
                                                       :loop (boolean loop?)
                                                       :color av1-cicp
                                                       :samples (into-array @chunks)})]
                            {:status :ok
                             :blob (js/Blob. #js [bytes] #js {:type "image/avif"})
                             :mtype "image/avif"
                             :ext ".avif"})))
                       (.finally
                        (fn []
                          (when (not= "closed" (.-state encoder))
                            (.close encoder))))))))))))))

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

(defn collect-layer-images
  "Rasterize each export layer at rest, but for those whose id `skip?` is
  true of. Children that are themselves layers are hidden so they are not
  baked into the parent; the board itself is rasterized without them.
  The animation shown at `time` is undone first, else a layer that fades
  in would be rasterized invisible."
  [objects timeline {:keys [bridge skip? time] :or {bridge (default-bridge) time 0}}]
  (let [ids  (conj (cta/export-layer-ids objects (:board-id timeline) (:tracks timeline))
                   (:board-id timeline))
        idset (set ids)
        board (get objects (:board-id timeline))
        ox    (or (:x (:selrect board)) 0)
        oy    (or (:y (:selrect board)) 0)]
    (restore-rest-properties! objects timeline time bridge)
    (into {}
          (keep
           (fn [id]
             (when-let [shape (get objects id)]
               (let [hide  (child-layer-ids objects id idset)
                     bytes (render-shape-at-rest objects id hide {:bridge bridge})
                     sr    (:selrect shape)
                     href  (str "data:image/png;base64," (u8->base64 bytes))]
                 [id {:href href
                      :x (- (or (:x sr) 0) ox)
                      :y (- (or (:y sr) 0) oy)
                      :width (or (:width sr) 0)
                      :height (or (:height sr) 0)
                      :bytes bytes
                      :id (str "img_" id)
                      :w (mth/round (or (:width sr) 0))
                      :h (mth/round (or (:height sr) 0))
                      :p href
                      :e 1}]))))
          (cond->> ids
            (some? skip?) (remove skip?)))))

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

(defn- capture-frames
  [{:keys [objects timeline board-id times bridge cancelled? on-progress yield
           decode scale]}]
  (let [decode (or decode decode-png)
        yield  (or yield default-yield)
        n      (count times)]
    (-> (js/Promise.resolve [])
        (.then
         (fn [_]
           (reduce
            (fn [p t]
              (-> p
                  (.then
                   (fn [acc]
                     (throw-if-cancelled cancelled?)
                     (let [bytes (render-shape-at-time objects timeline board-id t
                                                       {:bridge bridge :scale (or scale 1)})]
                       (-> (decode bytes)
                           (.then (fn [image]
                                    (when on-progress
                                      (on-progress (inc (count acc)) n))
                                    (-> (yield)
                                        (.then (fn [_] (conj acc image))))))))))))
            (js/Promise.resolve [])
            times))))))

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

(defn- set-export-progress
  [current total]
  (ptk/reify ::set-export-progress
    ptk/UpdateEvent
    (update [_ state]
      (update state :export-animation merge {:current current :total total}))))

(defn- export-raster!
  [state {:keys [format quality fps scale loop? audio? bridge decode yield]
          :or {audio? true}}]
  (let [tl       (dwa/export-timeline state)
        ;; MP4 and WebM carry the clips of the board
        sound?   (and audio? (contains? #{:mp4 :webm} format))
        objects  (dsh/lookup-page-objects state)
        board-id (:board-id tl)
        board    (get objects board-id)
        scale    (or scale 1)
        bw       (* (or (:width (:selrect board)) 1) scale)
        bh       (* (or (:height (:selrect board)) 1) scale)
        times    (cta/frame-times (:duration tl) fps)
        bridge   (or bridge (default-bridge))
        cancel?  (atom false)
        _        (reset! cancel-fn #(reset! cancel? true))
        was-fast (begin-full-quality! bridge)
        on-prog  (fn [i n] (st/emit! (set-export-progress i n)))]
    (when sound?
      (prefetch-audio! tl))
    ;; An MP4 larger than the browser encodes is made as large as it does
    ;; (see `fit-video-size`), which `export-finished` tells.
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
                                    :cancelled? #(deref cancel?)
                                    :on-progress on-prog
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
                        :gif (encode-gif {:frames frames
                                          :fps fps
                                          :loop? (boolean loop?)
                                          :colors (cta/gif-colors quality)
                                          :cancelled? #(deref cancel?)})
                        :avif (encode-avif {:frames frames
                                            :fps fps
                                            :bitrate (avif-bitrate bw bh quality)
                                            :loop? (boolean loop?)
                                            :flatten-bg (board-backdrop board)
                                            :cancelled? #(deref cancel?)})
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
                                              :audio sound
                                              :cancelled? #(deref cancel?)})))))))
                   (.then
                    (fn [result]
                      (cond-> result
                        (< fit 1) (assoc :fitted [(even-dim bw) (even-dim bh)])))))))))
        (.then
         (fn [result]
           (when (= :ok (:status result))
             (download-blob (or (:name board) (:name tl))
                            (:ext result)
                            (:mtype result)
                            (:blob result)))
           result))
        (.finally
         (fn []
           (reset! cancel-fn nil)
           (st/emit! (dwa/apply-preview))
           (when (and was-fast (not (wasm-gesture/active?)))
             (wasm-gesture/try-begin-interactive-transform!)
             (wasm.api/set-modifiers-start)))))))

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

(defn- export-markup!
  [state format {:keys [bridge]}]
  (let [tl      (dwa/export-timeline state :text-split :whole)
        objects (dsh/lookup-page-objects state)
        board   (get objects (:board-id tl))
        bridge  (or bridge (default-bridge))
        _       (begin-full-quality! bridge)
        ;; Lottie draws the layers it can as vectors
        layers  (set (cta/export-layer-ids objects (:board-id tl) (:tracks tl)))
        images  (collect-layer-images objects tl {:bridge bridge
                                                  :time (dwa/playhead state)
                                                  :skip? (when (#{:lottie :dotlottie} format)
                                                           #(cta/lottie-vector-layer? objects layers %))})]
    (try
      (case format
        :svg
        (let [svg (cta/timeline->svg tl objects images)
              blob (wapi/create-blob svg "image/svg+xml")]
          (download-blob (or (:name board) (:name tl)) ".svg" "image/svg+xml" blob)
          {:status :ok :blob blob})

        :lottie
        (let [assets (into {} (map (fn [[id img]]
                                     [id (select-keys img [:id :w :h :p :e])]))
                           images)
              data   (cta/timeline->lottie tl objects assets)
              json   (js/JSON.stringify (clj->js data) nil 2)
              blob   (wapi/create-blob json "application/json")]
          (download-blob (or (:name board) (:name tl)) ".json" "application/json" blob)
          {:status :ok :blob blob})

        ;; the images go into the archive, next to the document
        :dotlottie
        (let [name      (or (:name board) (:name tl))
              file-name #(str (:id %) ".png")
              assets    (into {} (map (fn [[id img]]
                                        [id (-> (select-keys img [:id :w :h])
                                                (assoc :u "/images/" :p (file-name img) :e 0))]))
                              images)
              data      (cta/timeline->lottie tl objects assets)
              files     (into {} (map (fn [[_ img]] [(file-name img) (:bytes img)])) images)]
          (p/let [blob (lottie->dotlottie data (safe-filename name "") files
                                          (not= :once (cta/playback-mode tl)))]
            (download-blob name ".lottie" dotlottie-mtype blob)
            {:status :ok :blob blob})))
      (finally
        (st/emit! (dwa/apply-preview))))))

(defn export-finished
  "What an export ending with `result` shows: that the browser encodes
  none of the codecs of the format (`:unavailable`), or what the video
  had to go without: its full size (`:fitted`, the `[width height]` it
  has, see `fit-video-size`) and its sound (`:audio-dropped`, see
  `encode-video`); and the export panel free again either way."
  [result]
  (if (= :unavailable (:status result))
    [(ntf/error (tr "workspace.options.export.codec-unavailable"))
     (finish-export-state)]
    (let [notes (cond-> []
                  (:fitted result)
                  (conj (let [[w h] (:fitted result)]
                          (tr "workspace.options.export.fitted" w h)))

                  (:audio-dropped result)
                  (conj (tr "workspace.options.export.audio-dropped")))]
      (cond-> []
        (seq notes) (conj (ntf/warn (str/join " " notes)))
        :always     (conj (finish-export-state))))))

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
                                      :cancelling? false
                                      :format format}))

    ptk/WatchEvent
    (watch [_ state _]
      (let [tl (dwa/export-timeline state)]
        (if (nil? tl)
          (rx/of (finish-export-state))
          (rx/concat
           (rx/of (ev/event {::ev/name "export-animation"
                             ::ev/origin "workspace:sidebar"
                             :format (name format)}))
           (->> (rx/from
                 (if (#{:svg :lottie :dotlottie} format)
                   (js/Promise.resolve (export-markup! state format params))
                   (export-raster! state params)))
                (rx/mapcat (comp rx/from export-finished))
                (rx/catch
                 (fn [cause]
                   (if (= :export-cancelled (:type (ex-data cause)))
                     (rx/of (finish-export-state))
                     (rx/of (ntf/error (or (ex-message cause)
                                           (tr "workspace.options.export.codec-unavailable")))
                            (finish-export-state))))))))))))

(defn cancel-export-animation
  []
  (ptk/reify ::cancel-export-animation
    ptk/UpdateEvent
    (update [_ state]
      (when-let [cancel @cancel-fn]
        (cancel))
      (assoc-in state [:export-animation :cancelling?] true))))
