;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.media.validation
  "Schemas and validation functions for media uploads.
   Leaf namespace — depends on app.common.* and app.config only."
  (:require
   [app.common.exceptions :as ex]
   [app.common.media :as cm]
   [app.common.schema :as sm]
   [app.config :as cf]
   [cuerdas.core :as str]
   [datoteka.fs :as fs])
  (:import
   java.nio.file.Files
   java.nio.file.OpenOption
   java.util.Arrays))

(def schema:upload
  [:map {:title "Upload"}
   [:filename :string]
   [:size ::sm/int]
   [:path ::fs/path]
   [:mtype {:optional true} :string]
   [:headers {:optional true}
    [:map-of :string :string]]])

(def schema:input
  [:map {:title "Input"}
   [:path ::fs/path]
   [:mtype {:optional true} ::sm/text]])

(def check-input
  (sm/check-fn schema:input))

(defn validate-media-type!
  ([upload] (validate-media-type! upload cm/image-types))
  ([upload allowed]
   (when-not (contains? allowed (:mtype upload))
     (ex/raise :type :validation
               :code :media-type-not-allowed
               :hint "Seems like you are uploading an invalid media object"))

   upload))

(defn validate-media-size!
  [upload]
  (let [max-size (cf/get :media-max-file-size)]
    (when (> (:size upload) max-size)
      (ex/raise :type :restriction
                :code :media-max-file-size-reached
                :hint (str/ffmt "the uploaded file size % is greater than the maximum %"
                                (:size upload)
                                max-size)))
    upload))

(defn- read-head
  "The first `n` bytes of the file at `path`, fewer for a shorter one."
  ^bytes [path n]
  (with-open [in (Files/newInputStream (fs/path path) (make-array OpenOption 0))]
    (let [buf  (byte-array n)
          read (.readNBytes in buf 0 (int n))]
      (Arrays/copyOf buf (int read)))))

(defn- text-at?
  "Whether the bytes of `head` from `offset` are the ASCII `text`."
  [^bytes head offset text]
  (and (<= (+ offset (count text)) (alength head))
       (every? (fn [i]
                 (= (int (nth text i)) (bit-and (aget head (+ offset i)) 0xff)))
               (range (count text)))))

(def audio-head-size
  "How many of the first bytes of a file tell its audio type (see
  `sniff-audio-head`)."
  64)

(defn sniff-audio-head
  "The canonical type (see `cm/audio-canonical-types`) of the audio file
  whose first bytes are `head` (`audio-head-size` of them, fewer for a
  shorter file), by their magic numbers; nil when they are of no audio
  file a clip plays."
  [^bytes head]
  (let [at (fn [i] (when (< i (alength head)) (bit-and (aget head i) 0xff)))]
    (cond
      (text-at? head 0 "ID3")
      "audio/mpeg"

      ;; an MPEG audio frame: 11 bits of sync, a version and a layer
      ;; (layer 0 is the one of ADTS, raw AAC, which it is not)
      (and (= 0xff (at 0))
           (some? (at 1))
           (= 0xe0 (bit-and (at 1) 0xe0))
           (not= 0 (bit-and (at 1) 0x06)))
      "audio/mpeg"

      (and (text-at? head 0 "RIFF") (text-at? head 8 "WAVE"))
      "audio/wav"

      (text-at? head 0 "OggS")
      "audio/ogg"

      ;; an ISO base media file (MP4, M4A)
      (text-at? head 4 "ftyp")
      "audio/mp4"

      ;; EBML, the header of Matroska and WebM
      (= [0x1a 0x45 0xdf 0xa3] (mapv at (range 4)))
      "audio/webm"

      (text-at? head 0 "fLaC")
      "audio/flac")))

(defn sniff-audio-type
  "The canonical type of the audio file at `path` (see
  `sniff-audio-head`), or nil."
  [path]
  (sniff-audio-head (read-head path audio-head-size)))

(defn validate-audio!
  "The audio `upload` with its canonical type: its declared type one of
  `cm/audio-types`, its bytes of that type (see `sniff-audio-type`, so
  that no HTML or SVG is stored as audio) and its size up to
  `:audio-max-file-size`. The server does not decode it, nor check its
  duration: the browser does both before the upload."
  [upload]
  (let [mtype    (cm/canonical-audio-type (:mtype upload))
        max-size (cf/get :audio-max-file-size)]
    (when (nil? mtype)
      (ex/raise :type :validation
                :code :media-type-not-allowed
                :hint "Seems like you are uploading an invalid media object"))

    (when (not= mtype (sniff-audio-type (:path upload)))
      (ex/raise :type :validation
                :code :media-type-mismatch
                :hint "the content of the uploaded file is not of its declared audio type"))

    (when (> (:size upload) max-size)
      (ex/raise :type :restriction
                :code :media-max-file-size-reached
                :hint (str/ffmt "the uploaded file size % is greater than the maximum %"
                                (:size upload)
                                max-size)))

    (assoc upload :mtype mtype)))

(defn validate-font-size!
  "Validates that the font file `upload` does not exceed the configured
  `:font-max-file-size` limit.  Accepts the same map shape as
  `validate-media-size!` — requires a `:size` key in bytes."
  [upload]
  (let [max-size (cf/get :font-max-file-size)]
    (when (> (:size upload) max-size)
      (ex/raise :type :restriction
                :code :font-max-file-size-reached
                :hint (str/ffmt "the uploaded font size % is greater than the maximum %"
                                (:size upload)
                                max-size)))
    upload))
