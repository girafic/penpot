;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.media
  "Media assets helpers (images, fonts, etc)"
  (:require
   [cuerdas.core :as str]))

(def font-types
  #{"font/ttf"
    "font/woff"
    "font/woff2"
    "font/otf"})

(def image-types
  #{"image/jpeg"
    "image/png"
    "image/webp"
    "image/gif"
    "image/svg+xml"})

(def tempfile-types
  (conj image-types "application/pdf" "application/zip"))

(def audio-canonical-types
  "The media types of the audio files the clips of Penpot Motion play,
  as they are stored."
  #{"audio/mpeg"
    "audio/wav"
    "audio/ogg"
    "audio/mp4"
    "audio/webm"
    "audio/flac"})

(def audio-type-aliases
  "Other media types browsers give the same audio files, by the
  canonical type each stands for."
  {"audio/mp3"      "audio/mpeg"
   "audio/x-mp3"    "audio/mpeg"
   "audio/wave"     "audio/wav"
   "audio/x-wav"    "audio/wav"
   "audio/vnd.wave" "audio/wav"
   "audio/opus"     "audio/ogg"
   "audio/m4a"      "audio/mp4"
   "audio/x-m4a"    "audio/mp4"
   "audio/x-flac"   "audio/flac"})

(def audio-types
  "The media types an audio file may come with: the canonical ones and
  their aliases."
  (into audio-canonical-types (keys audio-type-aliases)))

(defn canonical-audio-type
  "The canonical type of the audio media type `mtype`, nil when it is no
  audio type a clip plays."
  [mtype]
  (or (get audio-type-aliases mtype)
      (get audio-canonical-types mtype)))

(def storage-object-types
  "Every content-type the system stores in storage objects: uploaded
  images and fonts, generated thumbnails, temporary files and the
  `application/octet-stream` fallback used by file data and font
  variants, and the audio files of the clips of Penpot Motion (their
  aliases too: the import checks the bytes of an audio file against
  its canonical type). Sorted for determinism. Every member must have a
  producing code path; anticipated but unproduced types (such as
  `image/apng`) stay out until some flow actually stores them.
  Import boundaries normalize the stored string (lowercase, no
  parameters) before checking membership, so legacy spelling
  variants keep importing while unknown types are rejected."
  (into (sorted-set)
        (concat image-types
                font-types
                audio-types
                ["application/octet-stream"
                 "application/pdf"
                 "application/zip"])))

(defn format->extension
  [format]
  (case format
    :png  ".png"
    :jpeg ".jpg"
    :webp ".webp"
    :gif ".gif"
    :svg  ".svg"))

(defn format->mtype
  [format]
  (case format
    :png  "image/png"
    :jpeg "image/jpeg"
    :jpg  "image/jpeg"
    :webp "image/webp"
    :gif "image/gif"
    :svg  "image/svg+xml"
    "application/octet-stream"))

(defn mtype->format
  [mtype]
  (case mtype
    "image/png"     :png
    "image/jpeg"    :jpeg
    "image/webp"    :webp
    "image/gif"     :gif
    "image/svg+xml" :svg
    nil))

(defn mtype->extension [mtype]
  ;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
  (case mtype
    "image/apng"               ".apng"
    "image/avif"               ".avif"
    "image/gif"                ".gif"
    "image/jpeg"               ".jpg"
    "image/png"                ".png"
    "image/svg+xml"            ".svg"
    "image/webp"               ".webp"
    "application/zip"          ".zip"
    "application/zip+dotlottie" ".lottie"
    "application/penpot"       ".penpot"
    "application/pdf"          ".pdf"
    "text/plain"               ".txt"
    "font/woff"                ".woff"
    "font/woff2"               ".woff2"
    "font/ttf"                 ".ttf"
    "font/otf"                 ".otf"
    "audio/mpeg"               ".mp3"
    "audio/wav"                ".wav"
    "audio/ogg"                ".ogg"
    "audio/mp4"                ".m4a"
    "audio/webm"               ".webm"
    "audio/flac"               ".flac"
    "application/octet-stream" ".bin"
    nil))

(defn strip-image-extension
  [filename]
  (let [image-extensions-re #"(\.png)|(\.jpg)|(\.jpeg)|(\.webp)|(\.gif)|(\.svg)$"]
    (str/replace filename image-extensions-re "")))

(defn parse-font-weight
  [variant]
  (cond
    (re-seq #"(?i)(?:^|[-_\s])(hairline|thin)(?=(?:[-_\s]|$|italic\b))" variant)               100
    (re-seq #"(?i)(?:^|[-_\s])(extra\s*light|ultra\s*light)(?=(?:[-_\s]|$|italic\b))" variant) 200
    (re-seq #"(?i)(?:^|[-_\s])(light)(?=(?:[-_\s]|$|italic\b))" variant)                       300
    (re-seq #"(?i)(?:^|[-_\s])(normal|regular)(?=(?:[-_\s]|$|italic\b))" variant)              400
    (re-seq #"(?i)(?:^|[-_\s])(medium)(?=(?:[-_\s]|$|italic\b))" variant)                      500
    (re-seq #"(?i)(?:^|[-_\s])(semi\s*bold|demi\s*bold)(?=(?:[-_\s]|$|italic\b))" variant)     600
    (re-seq #"(?i)(?:^|[-_\s])(extra\s*bold|ultra\s*bold)(?=(?:[-_\s]|$|italic\b))" variant)   800
    (re-seq #"(?i)(?:^|[-_\s])(bold)(?=(?:[-_\s]|$|italic\b))" variant)                        700
    (re-seq #"(?i)(?:^|[-_\s])(extra\s*black|ultra\s*black)(?=(?:[-_\s]|$|italic\b))" variant) 950
    (re-seq #"(?i)(?:^|[-_\s])(black|heavy|solid)(?=(?:[-_\s]|$|italic\b))" variant)           900
    :else                                                                                      400))

(defn parse-font-style
  [variant]
  (if (or (re-seq #"(?i)(?:^|[-_\s])(italic)(?:[-_\s]|$)" variant)
          (re-seq #"(?i)italic$" variant))
    "italic"
    "normal"))

(defn font-weight->name
  [weight]
  (case (long weight)
    100 "Hairline"
    200 "Extra Light"
    300 "Light"
    400 "Regular"
    500 "Medium"
    600 "Semi Bold"
    700 "Bold"
    800 "Extra Bold"
    900 "Black"
    950 "Extra Black"))

(defn font-display-variant
  [variant-name weight style]
  (cond
    (and (string? variant-name) (not (str/blank? variant-name)))
    (str/trim variant-name)

    :else
    (let [base (font-weight->name weight)
          italic? (= "italic" style)]
      (cond-> base
        italic? (str " Italic")))))
