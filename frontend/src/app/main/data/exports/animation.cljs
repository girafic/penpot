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
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.color :as clr]
   [app.common.types.modifiers :as ctm]
   [app.main.data.event :as ev]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.animation :as dwa]
   [app.main.store :as st]
   [app.render-wasm.api :as wasm.api]
   [app.render-wasm.gesture :as wasm-gesture]
   [app.render-wasm.shape :as wasm.shape]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [clojure.string :as str]
   [potok.v2.core :as ptk]))

(defonce ^:private cancel-fn (atom nil))

(defn default-bridge
  []
  {:set-modifiers wasm.api/set-modifiers
   :clean-modifiers wasm.api/clean-modifiers
   :use-shape wasm.api/use-shape
   :set-shape-hidden wasm.api/set-shape-hidden
   :render-shape-pixels wasm.api/render-shape-pixels
   :apply-shape-properties wasm.shape/apply-shape-properties!
   :set-modifiers-end wasm.api/set-modifiers-end
   :interactive? wasm-gesture/active?
   :end-interactive wasm-gesture/try-end-interactive-transform!})

(defn- geometry-entries
  [modif-tree]
  (into []
        (keep (fn [[id {:keys [modifiers]}]]
                (when (and id (ctm/has-geometry? modifiers))
                  [id (ctm/modifiers->transform modifiers)])))
        modif-tree))

(defn- property-ops
  [modif-tree]
  (->> modif-tree
       (mapcat (fn [[id {:keys [modifiers]}]]
                 (->> (:structure-parent modifiers)
                      (filter #(= :change-property (:type %)))
                      (map #(vector id %)))))
       (group-by first)))

(defn apply-pose!
  "Write `modif-tree` to WASM without scheduling a viewport render."
  [objects modif-tree {:keys [bridge] :or {bridge (default-bridge)}}]
  (let [{:keys [set-modifiers clean-modifiers apply-shape-properties]} bridge
        entries (geometry-entries modif-tree)]
    (if (seq entries)
      (set-modifiers entries :request-render? false)
      (when clean-modifiers
        (clean-modifiers)))
    (doseq [[id ops] (property-ops modif-tree)]
      (when-let [shape (get objects id)]
        (let [ops   (map second ops)
              shape (reduce ctm/apply-modifier shape ops)]
          (apply-shape-properties shape (map :property ops)))))))

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
  "Rasterize `shape-id` in rest pose, hiding `hide-ids` for the shot."
  [objects shape-id hide-ids {:keys [bridge] :or {bridge (default-bridge)}}]
  (let [{:keys [use-shape set-shape-hidden clean-modifiers
                render-shape-pixels apply-shape-properties]} bridge]
    (try
      (when clean-modifiers
        (clean-modifiers))
      (doseq [id hide-ids]
        (use-shape id)
        (set-shape-hidden true))
      (render-shape-pixels shape-id 1 :png)
      (finally
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

(defn rgba->i420
  "Convert interleaved RGBA/RGBX to I420 (BT.601, full range). Pads to
  even width/height so H.264 4:2:0 is valid for QuickTime."
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
              Y  (unsigned-bit-shift-right (+ (* 77 r) (* 150 g) (* 29 b)) 8)]
          (aset out (+ (* y w) x) (clamp8 Y)))))
    (dotimes [cy ch]
      (dotimes [cx cw]
        (let [sx (min (* cx 2) (dec src-w))
              sy (min (* cy 2) (dec src-h))
              i  (* 4 (+ (* sy src-w) sx))
              r  (aget data i)
              g  (aget data (+ i 1))
              b  (aget data (+ i 2))
              U  (+ 128 (bit-shift-right (+ (* -43 r) (* -85 g) (* 128 b)) 8))
              V  (+ 128 (bit-shift-right (+ (* 128 r) (* -107 g) (* -21 b)) 8))
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
   :first-codec   mb/getFirstEncodableVideoCodec})

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
  "The color space of `rgba->i420` output: sRGB primaries and transfer,
  BT.601 matrix, full range."
  #js {:primaries "bt709" :transfer "iec61966-2-1" :matrix "smpte170m" :fullRange true})

(def ^:private av1-cicp
  "`av1-color-space` as CICP codes, for the AVIF `colr` box."
  #js {:primaries 1 :transfer 13 :matrix 6 :fullRange true})

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
     (:gif :svg :lottie)
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

(defn encode-video
  "Encode decoded frames with Mediabunny. Returns a promise of
  `{:status :ok|:unavailable :blob ...}`."
  [{:keys [format frames fps bitrate cancelled? on-progress flatten-bg]
    :as params}]
  (let [media           (merge (default-media)
                               (select-keys params [:output :mp4-format :webm-format
                                                    :target :sample-source :video-sample
                                                    :quality :first-codec]))
        output-ctor     (:output media)
        format-ctor     (output-format-ctor format media)
        target-ctor     (:target media)
        source-ctor     (:sample-source media)
        sample-ctor     (:video-sample media)
        quality-ctor    (:quality media)
        first-codec     (:first-codec media)
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
        (-> (first-codec codecs
                         #js {:width w
                              :height h
                              :quality (new quality-ctor #js {:bitrate bitrate})
                              :frameRate fps})
            (.then
             (fn [codec]
               (if (nil? codec)
                 {:status :unavailable}
                 (let [source (new source-ctor
                                   #js {:codec codec
                                        :quality (new quality-ctor #js {:bitrate bitrate})
                                        :alpha "discard"})]
                   (.addVideoTrack output source #js {:frameRate fps})
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
                          (.finalize output)))
                       (.then
                        (fn [_]
                          (let [buf   (.-buffer target)
                                mtype (if (= format :mp4) "video/mp4" "video/webm")
                                ext   (if (= format :mp4) ".mp4" ".webm")]
                            (if (or (nil? buf) (zero? (.-byteLength buf)))
                              {:status :unavailable}
                              {:status :ok
                               :blob (js/Blob. #js [buf] #js {:type mtype})
                               :mtype mtype
                               :ext ext
                               :closed @closed}))))))))))))))

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

(defn collect-layer-images
  "Rasterize each export layer at rest, but for those whose id `skip?` is
  true of. Children that are themselves layers are hidden so they are not
  baked into the parent."
  [objects timeline {:keys [bridge skip?] :or {bridge (default-bridge)}}]
  (let [ids  (cta/export-layer-ids objects (:board-id timeline) (:tracks timeline))
        idset (set ids)
        board (get objects (:board-id timeline))
        ox    (or (:x (:selrect board)) 0)
        oy    (or (:y (:selrect board)) 0)]
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
                      :id (str "img_" (subs (str id) 0 8))
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
  [state {:keys [format quality fps scale loop? bridge decode yield]}]
  (let [tl       (dwa/current-timeline state)
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
    (-> (capture-frames {:objects objects
                         :timeline tl
                         :board-id board-id
                         :times times
                         :bridge bridge
                         :cancelled? #(deref cancel?)
                         :on-progress on-prog
                         :yield yield
                         :decode decode
                         :scale scale})
        ;; Ping-pong plays the captured frames back to the start. Videos
        ;; do not loop, so they end on the first frame.
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
             (encode-video {:format format
                            :frames frames
                            :fps fps
                            :bitrate (cta/video-bitrate bw bh quality)
                            :flatten-bg (board-backdrop board)
                            :cancelled? #(deref cancel?)}))))
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

(defn- export-markup!
  [state format {:keys [bridge]}]
  (let [tl      (dwa/current-timeline state)
        objects (dsh/lookup-page-objects state)
        board   (get objects (:board-id tl))
        bridge  (or bridge (default-bridge))
        _       (begin-full-quality! bridge)
        ;; Lottie draws the layers it can as vectors
        layers  (set (cta/export-layer-ids objects (:board-id tl) (:tracks tl)))
        images  (collect-layer-images objects tl {:bridge bridge
                                                  :skip? (when (= format :lottie)
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
          {:status :ok :blob blob}))
      (finally
        (st/emit! (dwa/apply-preview))))))

(defn export-animation
  "Export the current board timeline as `format` (`:mp4` `:webm` `:gif`
  `:avif` `:svg` `:lottie`)."
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
      (let [tl (dwa/current-timeline state)]
        (if (nil? tl)
          (rx/of (finish-export-state))
          (rx/concat
           (rx/of (ev/event {::ev/name "export-animation"
                             ::ev/origin "workspace:sidebar"
                             :format (name format)}))
           (->> (rx/from
                 (if (#{:svg :lottie} format)
                   (js/Promise.resolve (export-markup! state format params))
                   (export-raster! state params)))
                (rx/map
                 (fn [result]
                   (if (= :unavailable (:status result))
                     (ntf/error (tr "workspace.options.export.codec-unavailable"))
                     (finish-export-state))))
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
