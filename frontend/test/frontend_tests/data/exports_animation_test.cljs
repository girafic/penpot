;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.exports-animation-test
  (:require
   [app.common.types.animation :as cta]
   [app.common.types.shape :as cts]
   [app.main.data.exports.animation :as dea]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]))

(defn- log
  [calls k & args]
  (swap! calls conj (into [k] args)))

(defn- test-objects
  []
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 100 :height 80
                                :fills [{:fill-color "#112233" :fill-opacity 1}]})
        child (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20})
        bid   (:id board)
        cid   (:id child)]
    {:board board
     :child child
     :bid bid
     :cid cid
     :objects {bid (assoc board :shapes [cid])
               cid (assoc child :parent-id bid :frame-id bid)}}))

(t/deftest apply-pose-sets-modifiers-without-render
  (let [calls   (atom [])
        {:keys [objects bid cid]} (test-objects)
        tl (-> (cta/make-timeline {:board-id bid :duration 1000})
               (cta/add-keyframe cid {:time 0 :property :x :value 10})
               (cta/add-keyframe cid {:time 1000 :property :x :value 50})
               (cta/add-keyframe cid {:time 0 :property :opacity :value 1})
               (cta/add-keyframe cid {:time 1000 :property :opacity :value 0}))
        tree (cta/timeline->modif-tree tl objects 1000)
        bridge {:set-modifiers (fn [entries & {:keys [request-render?]}]
                                 (log calls :set-modifiers entries request-render?))
                :apply-shape-properties (fn [shape props]
                                          (log calls :props (:id shape) props))}]
    (dea/apply-pose! objects tree {:bridge bridge})
    (t/is (some #(= :set-modifiers (first %)) @calls))
    (t/is (some (fn [[k _ render?]]
                  (and (= k :set-modifiers) (false? render?)))
                @calls))
    (t/is (some #(= :props (first %)) @calls))))

(t/deftest render-shape-at-rest-hides-and-restores
  (let [hidden (atom #{})
        {:keys [objects bid cid]} (test-objects)
        pixels (atom 0)
        bridge {:clean-modifiers (fn [] (log hidden :clean))
                :use-shape (fn [id] (log hidden :use id))
                :set-shape-hidden (fn [v] (swap! hidden conj [:hide v]))
                :apply-shape-properties (fn [shape props]
                                          (swap! hidden conj [:restore (:id shape) props]))
                :render-shape-pixels (fn [id _ _]
                                       (swap! pixels inc)
                                       (t/is (some #{[:hide true]} @hidden))
                                       (js/Uint8Array. #js [1 2 3]))}]
    (let [bytes (dea/render-shape-at-rest objects bid [cid] {:bridge bridge})]
      (t/is (= 1 @pixels))
      (t/is (pos? (.-length bytes)))
      (t/is (some (fn [entry]
                    (and (= :restore (first entry))
                         (= cid (second entry))
                         (= [:hidden] (nth entry 2))))
                  @hidden)))))

(t/deftest flatten-rgba-composites-onto-backdrop
  (let [data (js/Uint8ClampedArray. #js [255 0 0 128 0 0 0 0])
        out  (dea/flatten-rgba data 2 1 [0 255 0])]
    (t/is (= 255 (aget (:data out) 3)))
    (t/is (= 255 (aget (:data out) 7)))
    (t/is (= 0 (aget (:data out) 4)))
    (t/is (= 255 (aget (:data out) 5)))))

(t/deftest rgba->i420-uses-bt601-and-even-size
  (let [red  (js/Uint8ClampedArray. #js [255 0 0 255 255 0 0 255
                                         255 0 0 255 255 0 0 255])
        out  (dea/rgba->i420 red 2 2)
        y    (aget (:data out) 0)
        u    (aget (:data out) 4)
        v    (aget (:data out) 5)]
    (t/is (= 2 (:width out)))
    (t/is (= 2 (:height out)))
    (t/is (= "I420" (:format out)))
    (t/is (= 6 (.-length (:data out))))
    (t/is (= 76 y))
    (t/is (= 85 u))
    (t/is (= 255 v))))

(t/deftest rgba->i420-pads-odd-dimensions
  (let [px  (js/Uint8ClampedArray. #js [0 255 0 255])
        out (dea/rgba->i420 px 1 1)]
    (t/is (= 2 (:width out)))
    (t/is (= 2 (:height out)))
    (t/is (= 6 (.-length (:data out))))))

(t/deftest encode-gif-writes-loop-flag
  (let [repeat (atom nil)
        gif    #js {:writeFrame (fn [_ _ _ opts]
                                  (reset! repeat (.-repeat opts)))
                    :finish (fn [])
                    :bytes (fn [] (js/Uint8Array. #js [71 73 70]))}
        result (dea/encode-gif
                {:frames [{:data #js [255 0 0 255] :width 1 :height 1}]
                 :fps 30
                 :loop? true
                 :gif-encoder (fn [] gif)
                 :quantize (fn [data _] data)
                 :apply-palette (fn [data _] data)})]
    (t/is (= :ok (:status result)))
    (t/is (= "image/gif" (:mtype result)))
    (t/is (= 0 @repeat))
    (t/is (pos? (.-size (:blob result))))))

(t/deftest encode-gif-plays-once-without-loop
  (let [repeat (atom nil)
        gif    #js {:writeFrame (fn [_ _ _ opts]
                                  (reset! repeat (.-repeat opts)))
                    :finish (fn [])
                    :bytes (fn [] (js/Uint8Array. #js [71 73 70]))}
        result (dea/encode-gif
                {:frames [{:data #js [255 0 0 255] :width 1 :height 1}]
                 :fps 12
                 :loop? false
                 :gif-encoder (fn [] gif)
                 :quantize (fn [data _] data)
                 :apply-palette (fn [data _] data)})]
    (t/is (= -1 @repeat))
    (t/is (= :ok (:status result)))))

(t/deftest encode-gif-abort-skips-download
  (t/is (thrown-with-msg?
         js/Error
         #"cancelled"
         (let [gif #js {:writeFrame (fn [_ _ _ _])
                        :finish (fn [])
                        :bytes (fn [] (js/Uint8Array. #js [1]))}]
           (dea/encode-gif
            {:frames [{:data #js [1] :width 1 :height 1}
                      {:data #js [1] :width 1 :height 1}]
             :fps 30
             :cancelled? (fn [] true)
             :gif-encoder (fn [] gif)
             :quantize (fn [data _] data)
             :apply-palette (fn [data _] data)})))))

(t/deftest render-shape-at-time-is-silent
  (let [calls (atom [])
        {:keys [objects bid cid]} (test-objects)
        tl (-> (cta/make-timeline {:board-id bid :duration 1000})
               (cta/add-keyframe cid {:time 0 :property :x :value 10})
               (cta/add-keyframe cid {:time 1000 :property :x :value 50}))
        bridge {:set-modifiers (fn [_entries & {:keys [request-render?]}]
                                 (swap! calls conj [:set-modifiers request-render?]))
                :apply-shape-properties (fn [_ _]
                                          (swap! calls conj [:props]))
                :render-shape-pixels (fn [id _ _]
                                       (swap! calls conj [:pixels id])
                                       (js/Uint8Array. #js [1 2 3]))}]
    (let [bytes (dea/render-shape-at-time objects tl bid 1000 {:bridge bridge})]
      (t/is (pos? (.-length bytes)))
      (t/is (= [:set-modifiers false] (first @calls)))
      (t/is (some #{[:pixels bid]} @calls)))))

(def ^:private *closed (atom 0))

(defn- fake-format []
  #js {:getSupportedVideoCodecs (fn [] #js ["avc" "vp9"])})

(defn- fake-target []
  #js {:buffer (js/ArrayBuffer. 8)})

(defn- fake-output [_opts]
  #js {:addVideoTrack (fn [_ _])
       :start (fn [] (js/Promise.resolve nil))
       :finalize (fn [] (js/Promise.resolve nil))})

(defn- fake-sample-source [_opts]
  #js {:add (fn [_ _] (js/Promise.resolve nil))
       :close (fn [])})

(defn- fake-video-sample [_img _opts]
  #js {:close (fn [] (swap! *closed inc))})

(defn- fake-quality [_opts]
  #js {})

(defn- media-fakes
  [first-codec]
  {:output fake-output
   :mp4-format fake-format
   :webm-format fake-format
   :target fake-target
   :sample-source fake-sample-source
   :video-sample fake-video-sample
   :quality fake-quality
   :first-codec first-codec})

(t/deftest encode-video-unavailable-without-codec-probe
  (t/async
    done
    (-> (dea/encode-video (merge {:format :mp4
                                  :frames [{:data #js [0] :width 2 :height 2}]
                                  :fps 30
                                  :bitrate 1000}
                                 (media-fakes (fn [_ _] (js/Promise.resolve nil)))))
        (.then (fn [result]
                 (t/is (= :unavailable (:status result)))
                 (done))))))

(t/deftest encode-video-unavailable-when-codec-unsupported
  (t/async
    done
    (-> (dea/encode-video
         (merge {:format :mp4
                 :frames [{:data (js/Uint8ClampedArray. #js [0 0 0 255]) :width 1 :height 1}]
                 :fps 30
                 :bitrate 1000}
                (media-fakes (fn [_ _] (js/Promise.resolve nil)))))
        (.then (fn [result]
                 (t/is (= :unavailable (:status result)))
                 (done)))
        (.catch (fn [err]
                  (t/is (nil? err) (str err))
                  (done))))))

(t/deftest encode-video-encodes-and-closes-frames
  (t/async
    done
    (reset! *closed 0)
    (-> (dea/encode-video
         (merge {:format :webm
                 :frames [{:data (js/Uint8ClampedArray. #js [0 0 0 255]) :width 1 :height 1}]
                 :fps 30
                 :bitrate 1000}
                (media-fakes (fn [_ _] (js/Promise.resolve "vp9")))))
        (.then (fn [result]
                 (t/is (= :ok (:status result)))
                 (t/is (= "video/webm" (:mtype result)))
                 (t/is (= 1 @*closed))
                 (done)))
        (.catch (fn [err]
                  (t/is (nil? err) (str err))
                  (done))))))

(t/deftest encode-video-mp4-when-supported
  (t/async
    done
    (reset! *closed 0)
    (let [codecs (atom nil)]
      (-> (dea/encode-video
           (merge {:format :mp4
                   :frames [{:data (js/Uint8ClampedArray. #js [0 0 0 255]) :width 1 :height 1}]
                   :fps 30
                   :bitrate 1000
                   :flatten-bg [255 255 255]}
                  (media-fakes (fn [cs _]
                                 (reset! codecs (vec (array-seq cs)))
                                 (js/Promise.resolve "avc")))))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (= "video/mp4" (:mtype result)))
                   (t/is (= ["avc"] @codecs))
                   (t/is (= 1 @*closed))
                   (done)))
          (.catch (fn [err]
                    (t/is (nil? err) (str err))
                    (done)))))))

(t/deftest encode-video-passes-pixel-buffer
  (t/async
    done
    (let [seen (atom nil)
          sample-ctor (fn [data init]
                        (reset! seen {:view? (js/ArrayBuffer.isView data)
                                      :format (.-format init)
                                      :w (.-codedWidth init)
                                      :h (.-codedHeight init)})
                        #js {:close (fn [])})]
      (-> (dea/encode-video
           (merge {:format :mp4
                   :frames [{:data (js/Uint8ClampedArray. #js [0 0 0 255]) :width 1 :height 1}]
                   :fps 30
                   :bitrate 1000
                   :flatten-bg [255 255 255]}
                  (media-fakes (fn [_ _] (js/Promise.resolve "avc")))
                  {:video-sample sample-ctor}))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (true? (:view? @seen)))
                   (t/is (= "I420" (:format @seen)))
                   (t/is (even? (:w @seen)))
                   (t/is (even? (:h @seen)))
                   (done)))
          (.catch (fn [err]
                    (t/is (nil? err) (str err))
                    (done)))))))

(t/deftest estimated-size-is-positive-for-raster
  (t/is (pos? (dea/estimated-size :mp4 100 80 :high 1000 30)))
  (t/is (pos? (dea/estimated-size :gif 100 80 :high 1000 30)))
  (t/is (zero? (dea/estimated-size :svg 100 80 :high 1000 30))))

(t/deftest collect-layer-images-builds-assets
  (let [{:keys [objects bid cid]} (test-objects)
        tl (-> (cta/make-timeline {:board-id bid :duration 500})
               (cta/add-keyframe cid {:time 0 :property :x :value 10}))
        bridge {:clean-modifiers (fn [])
                :use-shape (fn [_])
                :set-shape-hidden (fn [_])
                :apply-shape-properties (fn [_ _])
                :render-shape-pixels (fn [_ _ _]
                                       (js/Uint8Array. #js [137 80 78 71]))}]
    (let [images (dea/collect-layer-images objects tl {:bridge bridge})]
      (t/is (contains? images cid))
      (t/is (str/starts-with? (get-in images [cid :href]) "data:image/png;base64,"))
      (t/is (some? (get-in images [cid :id]))))))
