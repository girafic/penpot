;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.exports-animation-formats-test
  "Whether the browser can encode a format of an animation export, asked
  of WebCodecs alone, as the view mode asks it."
  (:require
   [app.main.data.exports.animation-formats :as daf]
   [cljs.test :as t :include-macros true]))

(defn- encoder
  "A `VideoEncoder` that encodes the `codecs` only."
  [codecs]
  #js {:isConfigSupported
       (fn [^js config]
         (js/Promise.resolve #js {:supported (contains? codecs (.-codec config))}))})

(t/deftest the-browser-encodes-a-format
  (t/async
    done
    (-> (js/Promise.all
         #js [(daf/codec-supported? :gif nil)
              (daf/codec-supported? :lottie nil)
              (daf/codec-supported? :mp4 nil)
              (daf/codec-supported? :mp4 (encoder #{"avc1.42001f"}))
              (daf/codec-supported? :mp4 (encoder #{"vp8"}))
              (daf/codec-supported? :webm (encoder #{"vp8"}))
              (daf/codec-supported? :avif (encoder #{(daf/av1-codec 64 64)}))
              (daf/codec-supported? :avif (encoder #{"vp8"}))])
        (.then (fn [[gif lottie no-encoder mp4 mp4-vp8 webm avif avif-not]]
                 (t/is (true? gif) "a GIF, always")
                 (t/is (true? lottie) "and markup")
                 (t/is (false? no-encoder) "no video without WebCodecs")
                 (t/is (true? mp4) "MP4 with H.264")
                 (t/is (false? mp4-vp8) "but not with VP8, which it does not take")
                 (t/is (true? webm) "WebM with VP8")
                 (t/is (true? avif) "AVIF with AV1")
                 (t/is (false? avif-not) "and only with it")))
        (.finally done))))

(t/deftest the-size-a-format-comes-out
  (t/is (pos? (daf/estimated-size :mp4 100 80 :high 1000 30)))
  (t/is (< (daf/estimated-size :avif 1920 1080 :high 2000 30)
           (daf/estimated-size :mp4 1920 1080 :high 2000 30))
        "AVIF smaller than MP4")
  (t/is (zero? (daf/estimated-size :svg 100 80 :high 1000 30)))
  (t/is (= "~2 KB" (daf/approx-size-label 2048))))
