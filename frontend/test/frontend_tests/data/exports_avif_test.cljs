;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.exports-avif-test
  (:require
   ["/app/main/data/exports/avif.js" :as avif]
   [app.main.data.exports.animation :as dea]
   [cljs.test :as t :include-macros true]))

;; OBUs as Chrome's AV1 encoder writes them: every temporal unit starts
;; with a temporal delimiter, key frames carry the sequence header.
(def ^:private temporal-delimiter [0x12 0x00])

;; 320×180, main profile, BT.601 matrix, sRGB, full range.
(def ^:private sequence-header
  [0x0a 0x0d 0x00 0x00 0x00 0x04 0x3c 0xfe 0xcc 0x01 0xa2 0x02 0x1a 0x0d 0x08])

(def ^:private frame-obu [0x32 0x03 0xaa 0xbb 0xcc])

(def ^:private key-unit (concat temporal-delimiter sequence-header frame-obu))
(def ^:private delta-unit (concat temporal-delimiter frame-obu))

(defn- u8
  [xs]
  (js/Uint8Array.from (clj->js xs)))

(defn- ->vec
  [^js bytes]
  (vec (array-seq (js/Array.from bytes))))

(defn- fourcc-at
  [^js bytes pos]
  (.decode (js/TextDecoder.) (.subarray bytes pos (+ pos 4))))

(defn- u32-at
  [^js bytes pos]
  (.getUint32 (js/DataView. (.-buffer bytes) (.-byteOffset bytes)) pos))

(defn- top-level-boxes
  [^js bytes]
  (loop [pos 0 acc []]
    (if (>= pos (.-length bytes))
      acc
      (let [size (u32-at bytes pos)]
        (recur (+ pos size) (conj acc {:type (fourcc-at bytes (+ pos 4)) :pos pos :size size}))))))

(defn- box-pos
  "Offset of the first box of `type`, searching the bytes for its fourcc."
  [^js bytes type]
  (loop [pos 4]
    (cond
      (> (+ pos 4) (.-length bytes)) nil
      (= type (fourcc-at bytes pos)) (- pos 4)
      :else (recur (inc pos)))))

(defn- write
  [samples loop?]
  (avif/writeSequence
   #js {:width 320
        :height 180
        :timescale 30
        :loop loop?
        :color #js {:primaries 1 :transfer 13 :matrix 6 :fullRange true}
        :samples (into-array
                  (map (fn [[unit key?]]
                         #js {:data (u8 unit) :key key? :duration 1})
                       samples))}))

(t/deftest parse-sequence-header-reads-the-color-config
  (let [info (avif/parseSequenceHeader (u8 (drop 2 sequence-header)))]
    (t/is (= 0 (.-profile info)))
    (t/is (= 0 (.-level info)))
    (t/is (= 8 (.-bitDepth info)))
    (t/is (= 0 (.-monochrome info)))
    (t/is (= [1 1] [(.-subsamplingX info) (.-subsamplingY info)]))
    (t/is (= [1 13 6 1] [(.-primaries info) (.-transfer info) (.-matrix info) (.-fullRange info)]))))

(t/deftest samples-drop-temporal-delimiters
  (t/is (= (vec (concat sequence-header frame-obu))
           (->vec (avif/withoutTemporalDelimiters (u8 key-unit)))))
  (t/is (= (vec sequence-header)
           (->vec (avif/sequenceHeaderObu (u8 key-unit)))))
  (t/testing "bytes that are not whole OBUs stay as they are"
    (let [broken [0x12 0x00 0x32 0x09 0x01]]
      (t/is (= broken (->vec (avif/withoutTemporalDelimiters (u8 broken))))))))

(t/deftest write-sequence-lays-out-an-avif-sequence
  (let [bytes  (write [[key-unit true] [delta-unit false] [delta-unit false]] true)
        boxes  (top-level-boxes bytes)
        mdat   (last boxes)
        data   (+ (:pos mdat) 8)
        stco   (box-pos bytes "stco")
        iloc   (box-pos bytes "iloc")
        elst   (box-pos bytes "elst")]
    (t/is (= ["ftyp" "meta" "moov" "mdat"] (mapv :type boxes)))
    (t/is (= (.-length bytes) (reduce + (map :size boxes))))
    (t/is (= "avis" (fourcc-at bytes 8)))
    (t/testing "samples are stored without temporal delimiters"
      (t/is (= (vec (concat sequence-header frame-obu frame-obu frame-obu))
               (->vec (.subarray bytes data)))))
    (t/testing "the track and the still image point at the samples"
      (t/is (= data (u32-at bytes (+ stco 16))))
      (t/is (= data (u32-at bytes (+ iloc 22))))
      (t/is (= (+ (count sequence-header) (count frame-obu))
               (u32-at bytes (+ iloc 26)))))
    (t/testing "only the first sample is a sync sample"
      (let [stss (box-pos bytes "stss")]
        (t/is (= [1 1] [(u32-at bytes (+ stss 12)) (u32-at bytes (+ stss 16))]))))
    (t/testing "the edit repeats forever when looping"
      (t/is (= 1 (bit-and 0xffffff (u32-at bytes (+ elst 8))))))))

(t/deftest write-sequence-plays-once-without-loop
  (let [bytes (write [[key-unit true] [delta-unit false]] false)
        elst  (box-pos bytes "elst")]
    (t/is (= 0 (bit-and 0xffffff (u32-at bytes (+ elst 8)))))))

(t/deftest write-sequence-needs-a-key-frame-with-sequence-header
  (t/is (thrown? js/Error (write [[delta-unit false]] true)))
  (t/is (thrown? js/Error (write [[delta-unit true]] true))))

;; A VideoEncoder that turns every frame into one temporal unit.
(defn- fake-encoder
  [calls supported?]
  (let [ctor (fn [^js init]
               (let [output (.-output init)]
                 #js {:encodeQueueSize 0
                      :configure (fn [config]
                                   (swap! calls conj [:configure (.-codec config)]))
                      :encode (fn [_frame ^js opts]
                                (let [key? (.-keyFrame opts)
                                      unit (u8 (if key? key-unit delta-unit))]
                                  (swap! calls conj [:encode key?])
                                  (output #js {:type (if key? "key" "delta")
                                               :byteLength (.-length unit)
                                               :copyTo (fn [dest] (.set dest unit))}
                                          #js {})))
                      :flush (fn [] (js/Promise.resolve nil))
                      :close (fn []
                               (swap! calls conj [:close]))
                      :addEventListener (fn [_ _ _])
                      :state "configured"}))]
    (set! (.-isConfigSupported ^js ctor)
          (fn [_] (js/Promise.resolve #js {:supported supported?})))
    ctor))

(defn- fake-frame
  [calls]
  (fn [_data ^js init]
    (swap! calls conj [:frame (.-format init) (.. init -colorSpace -fullRange)])
    #js {:close (fn [] (swap! calls conj [:frame-closed]))}))

(defn- frames
  [n]
  (vec (repeat n {:data (js/Uint8ClampedArray. (* 4 4 4)) :width 4 :height 4})))

(t/deftest encode-avif-unavailable-without-webcodecs
  (t/async
    done
    (-> (dea/encode-avif {:frames (frames 1) :fps 30 :bitrate 1000
                          :video-encoder nil})
        (.then (fn [result]
                 (t/is (= :unavailable (:status result)))
                 (done))))))

(t/deftest encode-avif-unavailable-when-av1-unsupported
  (t/async
    done
    (let [calls (atom [])]
      (-> (dea/encode-avif {:frames (frames 1) :fps 30 :bitrate 1000
                            :video-encoder (fake-encoder calls false)
                            :video-frame (fake-frame calls)})
          (.then (fn [result]
                   (t/is (= :unavailable (:status result)))
                   (t/is (empty? @calls))
                   (done)))))))

(t/deftest encode-avif-writes-an-animated-avif
  (t/async
    done
    (let [calls   (atom [])
          written (atom nil)]
      (-> (dea/encode-avif {:frames (frames 3)
                            :fps 2
                            :bitrate 1000
                            :loop? true
                            :flatten-bg [255 255 255]
                            :video-encoder (fake-encoder calls true)
                            :video-frame (fake-frame calls)
                            :write-avif (fn [^js opts]
                                          (reset! written opts)
                                          (avif/writeSequence opts))})
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (= "image/avif" (:mtype result)))
                   (t/is (= ".avif" (:ext result)))
                   (t/is (pos? (.-size (:blob result))))
                   (t/testing "a key frame every second, frames closed after encoding"
                     (t/is (= [[:encode true] [:encode false] [:encode true]]
                              (filterv #(= :encode (first %)) @calls)))
                     (t/is (= 3 (count (filter #(= [:frame-closed] %) @calls)))))
                   (t/testing "frames are full range I420"
                     (t/is (every? #(= [:frame "I420" true] %)
                                   (filter #(= :frame (first %)) @calls))))
                   (t/is (= [:close] (last @calls)))
                   (t/is (true? (.-loop @written)))
                   (t/is (= 3 (.-length (.-samples @written))))
                   (done)))
          (.catch (fn [err]
                    (t/is (nil? err) (str err))
                    (done)))))))

(t/deftest avif-is-estimated-below-video
  (let [avif-size (dea/estimated-size :avif 1920 1080 :high 2000 30)
        mp4-size  (dea/estimated-size :mp4 1920 1080 :high 2000 30)]
    (t/is (pos? avif-size))
    (t/is (< avif-size mp4-size))))
