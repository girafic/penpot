;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.animation-formats
  "The formats of an animation export as its settings show them: their
  names, which carry sound, how large a file comes out, and whether the
  browser can encode them. Light: none of the encoders (see
  `app.main.data.exports.animation`), so that the view mode can offer an
  export without loading them."
  (:require
   [app.common.math :as mth]
   [app.common.types.animation :as cta]))

(def format-labels
  "How the formats of an animation export are written."
  {:mp4 "MP4"
   :webm "WebM"
   :gif "GIF"
   :avif "AVIF"
   :svg "SVG"
   :lottie "Lottie"
   :dotlottie "dotLottie"})

(def audio-formats
  "The formats that carry the sound of the audio clips."
  #{:mp4 :webm})

(def avif-bitrate-factor
  "AV1 needs about half the bitrate of H.264 for the same quality."
  0.5)

(defn av1-codec
  "AV1 codec string (main profile, 8 bit) with a level that allows a
  `width`×`height` frame: 5.1 up to 8192×4352, 6.0 above."
  [width height]
  (if (and (<= width 8192) (<= height 4352) (<= (* width height) 8912896))
    "av01.0.13M.08"
    "av01.0.16M.08"))

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

(def ^:private video-codecs
  "The video codecs the encoder may write in a container, as WebCodecs
  names them, in the order it prefers them."
  {:mp4  ["avc1.42001f" "hvc1.1.6.L93.B0" "vp09.00.10.08" "av01.0.04M.08"]
   :webm ["vp09.00.10.08" "vp8" "av01.0.04M.08"]})

(defn- config-supported?
  "A promise of whether `encoder` (`VideoEncoder`) encodes a small frame
  as `codec`."
  [^js encoder codec]
  (-> (.isConfigSupported encoder #js {:codec codec :width 64 :height 64})
      (.then #(boolean (.-supported ^js %)))
      (.catch (fn [_] false))))

(defn codec-supported?
  "A promise of whether the browser can encode `format`, asked of
  WebCodecs only, which the encoders write through: MP4 and WebM with one
  of the codecs their container takes, AVIF with AV1; the others always.
  The workspace asks the encoders themselves (see
  `app.main.data.exports.animation/codec-supported?`). `encoder` replaces
  `VideoEncoder` (in tests)."
  ([format]
   (codec-supported? format (when (exists? js/VideoEncoder) js/VideoEncoder)))
  ([format encoder]
   (case format
     (:gif :svg :lottie :dotlottie)
     (js/Promise.resolve true)

     (:mp4 :webm :avif)
     (if (nil? encoder)
       (js/Promise.resolve false)
       (let [codecs (if (= format :avif) [(av1-codec 64 64)] (get video-codecs format))]
         (-> (js/Promise.all (into-array (map #(config-supported? encoder %) codecs)))
             (.then #(boolean (some true? %))))))

     (js/Promise.resolve false))))
