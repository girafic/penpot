;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.exports-animation-test
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.components :as cthc]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.exports.animation :as dea]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.lottie :as dwl]
   [app.util.zip :as uz]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(defn- finish
  "A callback ending an async test with `done` and returning nothing:
  `done` starts the next tests at once and returns what they return, a
  promise that `.finally` would wait for and take the rejection of."
  [done]
  (fn [] (done) nil))

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
                :clean-modifiers (fn [] (log calls :clean-modifiers))
                :apply-shape-properties (fn [shape props]
                                          (log calls :props (:id shape) props))}]
    (dea/apply-pose! objects tree {:bridge bridge})
    (t/is (= [:clean-modifiers :props :set-modifiers] (mapv first @calls))
          "the modifiers of the frame before go first, with the copies they made,
          and the properties before the new ones, which copy the shapes")
    (t/is (some (fn [[k _ render?]]
                  (and (= k :set-modifiers) (false? render?)))
                @calls))
    (t/is (some #(= :props (first %)) @calls))))

(t/deftest apply-pose-composes-the-transforms-of-parents
  (let [calls  (atom [])
        {:keys [objects bid cid]} (test-objects)
        tl     (-> (cta/make-timeline {:board-id bid :duration 1000})
                   (cta/add-animation cid {:type :move :start 0 :duration 1000 :offset-x 0 :offset-y 40}))
        tree   (cta/timeline->modif-tree tl objects 500)
        result [[cid :propagated]]
        bridge {:set-modifiers (fn [entries & _] (log calls :set-modifiers entries))
                :propagate-modifiers (fn [entries snap? axis]
                                       (log calls :propagate (mapv first entries) (mapv (comp :kind second) entries)
                                            snap? axis)
                                       result)
                :apply-shape-properties (fn [_ _])}]
    (dea/apply-pose! objects tree {:bridge bridge})
    (let [[[_ ids kinds snap? axis] [_ entries]] @calls]
      (t/is (= [cid] ids))
      (t/is (every? keyword? kinds) "each transform says how it applies")
      (t/is (= [false nil] [snap? axis]) "no snap to the pixel grid")
      (t/is (= result entries) "WASM gets the transforms composed with those of the parents"))
    (t/testing "without the propagation (WASM not ready), the transforms as they are"
      (reset! calls [])
      (dea/apply-pose! objects tree {:bridge (assoc bridge :propagate-modifiers (constantly []))})
      (let [[[_ entries]] (filter #(= :set-modifiers (first %)) @calls)]
        (t/is (= [cid] (mapv first entries)))))))

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

(t/deftest rgba->i420-uses-bt709-limited-range-and-even-size
  (let [i420 (fn [r g b]
               (let [px  (js/Uint8ClampedArray. (clj->js (apply concat (repeat 4 [r g b 255]))))
                     out (dea/rgba->i420 px 2 2)]
                 [(aget (:data out) 0) (aget (:data out) 4) (aget (:data out) 5)]))
        out  (dea/rgba->i420 (js/Uint8ClampedArray. 16) 2 2)]
    (t/is (= 2 (:width out)))
    (t/is (= 2 (:height out)))
    (t/is (= "I420" (:format out)))
    (t/is (= 6 (.-length (:data out))))
    (t/is (= [16 128 128] (i420 0 0 0)) "black at 16")
    (t/is (= [235 128 128] (i420 255 255 255)) "white at 235")
    (t/is (= [63 102 240] (i420 255 0 0)) "red, BT.709")))

(t/deftest rgba->i420-pads-odd-dimensions
  (let [px  (js/Uint8ClampedArray. #js [0 255 0 255])
        out (dea/rgba->i420 px 1 1)]
    (t/is (= 2 (:width out)))
    (t/is (= 2 (:height out)))
    (t/is (= 6 (.-length (:data out))))))

(defn- fake-gif
  "A gifenc encoder that notes the `repeat` of its frames in `repeat` and
  how many it wrote in `written`."
  [repeat written]
  #js {:writeFrame (fn [_ _ _ opts]
                     (swap! written inc)
                     (reset! repeat (.-repeat opts)))
       :finish (fn [])
       :bytes (fn [] (js/Uint8Array. #js [71 73 70]))})

(defn- gif-params
  [gif & {:as params}]
  (merge {:frames [{:data #js [255 0 0 255] :width 1 :height 1}]
          :fps 30
          :gif-encoder (fn [] gif)
          :quantize (fn [data _] data)
          :apply-palette (fn [data _] data)
          :yield #(js/Promise.resolve nil)}
         params))

(t/deftest encode-gif-writes-loop-flag
  (t/async
    done
    (let [repeat (atom nil)]
      (-> (dea/encode-gif (gif-params (fake-gif repeat (atom 0)) :loop? true))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (= "image/gif" (:mtype result)))
                   (t/is (= 0 @repeat))
                   (t/is (pos? (.-size (:blob result))))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest encode-gif-plays-once-without-loop
  (t/async
    done
    (let [repeat (atom nil)]
      (-> (dea/encode-gif (gif-params (fake-gif repeat (atom 0)) :fps 12 :loop? false))
          (.then (fn [result]
                   (t/is (= -1 @repeat))
                   (t/is (= :ok (:status result)))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest encode-gif-abort-skips-download
  (t/async
    done
    (let [written (atom 0)
          frame   {:data #js [1] :width 1 :height 1}]
      (-> (dea/encode-gif (gif-params (fake-gif (atom nil) written)
                                      :frames [frame frame]
                                      :cancelled? (fn [] true)))
          (.then (fn [_] (t/is false "it should stop")))
          (.catch (fn [cause]
                    (t/is (= :export-cancelled (:type (ex-data cause))))
                    (t/is (= 0 @written))))
          (.finally (finish done))))))

(t/deftest encode-gif-gives-the-page-its-turn-between-frames
  (t/async
    done
    (let [written (atom 0)
          turns   (atom [])
          cancel? (atom false)
          frame   {:data #js [1] :width 1 :height 1}]
      (-> (dea/encode-gif (gif-params (fake-gif (atom nil) written)
                                      :frames [frame frame frame]
                                      :cancelled? #(deref cancel?)
                                      ;; cancelled at the turn after the first frame
                                      :yield (fn []
                                               (swap! turns conj @written)
                                               (reset! cancel? true)
                                               (js/Promise.resolve nil))))
          (.then (fn [_] (t/is false "it should stop")))
          (.catch (fn [cause]
                    (t/is (= :export-cancelled (:type (ex-data cause))))
                    (t/is (= [1] @turns) "a turn after the first frame")
                    (t/is (= 1 @written) "and no frame after it")))
          (.finally (finish done))))))

(t/deftest the-yield-is-a-task-of-a-message-channel
  (t/async
    done
    (let [posted  (atom 0)
          closed  (atom 0)
          ;; `this-as` first: inside a `let`, `this` is not the new object
          channel (fn []
                    (this-as this
                             (let [port1 #js {:close #(swap! closed inc)}]
                               (set! (.-port1 this) port1)
                               (set! (.-port2 this)
                                     #js {:postMessage (fn [_]
                                                         (swap! posted inc)
                                                         (js/setTimeout #((.-onmessage port1) #js {}) 0))})
                               this)))]
      (-> (dea/yield-to-ui channel)
          (.then (fn [_]
                   (t/is (= 1 @posted))
                   (t/is (= 1 @closed) "its port closed")))
          (.catch (fn [cause]
                    (t/is false (str "the yield failed: " (ex-message cause)))))
          (.finally (finish done))))))

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

(t/deftest encode-video-gives-the-page-its-turn-after-each-frame
  (t/async
    done
    (reset! *closed 0)
    (let [turns (atom 0)
          frame {:data (js/Uint8ClampedArray. #js [0 0 0 255]) :width 1 :height 1}]
      (-> (dea/encode-video
           (merge {:format :webm
                   :frames [frame frame frame]
                   :fps 30
                   :bitrate 1000
                   :yield (fn [] (swap! turns inc) (js/Promise.resolve nil))}
                  (media-fakes (fn [_ _] (js/Promise.resolve "vp9")))))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (= 3 @turns) "one turn a frame")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

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
                                      :color (js->clj (.-colorSpace init))
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
                   (t/is (= {"primaries" "bt709" "transfer" "iec61966-2-1" "matrix" "bt709" "fullRange" false}
                            (:color @seen))
                         "the sRGB of the board, as limited range BT.709 video")
                   (t/is (even? (:w @seen)))
                   (t/is (even? (:h @seen)))
                   (done)))
          (.catch (fn [err]
                    (t/is (nil? err) (str err))
                    (done)))))))

;; SOUND

(def ^:private one-frame
  [{:data (js/Uint8ClampedArray. #js [0 0 0 255]) :width 1 :height 1}])

(defn- audio-media
  "The fakes of `media-fakes`, for a video with sound: what the output
  and the sources are asked goes to `log`, in order, and the browser
  encodes the audio codecs of `encodable`."
  [log encodable]
  (merge (media-fakes (fn [_ _] (js/Promise.resolve "avc")))
         {:output (fn [_opts]
                    #js {:addVideoTrack (fn [_ _] (swap! log conj :video-track))
                         :addAudioTrack (fn [source] (swap! log conj [:audio-track (.-codec source)]))
                         :start (fn [] (swap! log conj :start) (js/Promise.resolve nil))
                         :finalize (fn [] (swap! log conj :finalize) (js/Promise.resolve nil))})
          :sample-source (fn [_opts]
                           #js {:add (fn [_ _] (swap! log conj :frame) (js/Promise.resolve nil))
                                :close (fn [] (swap! log conj :frames-closed))})
          :audio-source (fn [opts]
                          #js {:codec (.-codec opts)
                               :add (fn [buffer]
                                      (swap! log conj [:audio (.-name buffer)])
                                      (js/Promise.resolve nil))
                               :close (fn [] (swap! log conj :audio-closed))})
          :first-audio-codec (fn [codecs _]
                               (swap! log conj [:probe (vec (array-seq codecs))])
                               (js/Promise.resolve (some (set encodable) (array-seq codecs))))}))

(defn- failed
  [done]
  (fn [err]
    (t/is (nil? err) (str err))
    (done)))

(t/deftest encode-video-adds-the-sound-after-the-frames
  (t/async
    done
    (let [log (atom [])]
      (-> (dea/encode-video (merge {:format :mp4 :frames (into one-frame one-frame) :fps 30 :bitrate 1000
                                    :audio #js {:name "mix"}}
                                   (audio-media log #{"aac" "opus"})))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (nil? (:audio-dropped result)))
                   (t/is (= [[:probe ["aac" "opus"]] :video-track [:audio-track "aac"] :start
                             :frame :frame :frames-closed [:audio "mix"] :audio-closed :finalize]
                            @log)
                         "one audio track, added before the start, fed after the frames")
                   (done)))
          (.catch (failed done))))))

(t/deftest a-video-without-sound-has-no-audio-track
  (t/async
    done
    (let [log (atom [])]
      (-> (dea/encode-video (merge {:format :webm :frames one-frame :fps 30 :bitrate 1000}
                                   (audio-media log #{"opus"})))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (nil? (:audio-dropped result)))
                   (t/is (= [:video-track :start :frame :frames-closed :finalize] @log))
                   (done)))
          (.catch (failed done))))))

(t/deftest a-video-goes-without-sound-when-no-codec-encodes-it
  (t/async
    done
    (let [log (atom [])]
      (-> (dea/encode-video (merge {:format :webm :frames one-frame :fps 30 :bitrate 1000
                                    :audio #js {:name "mix"}}
                                   (audio-media log #{"aac"})))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (true? (:audio-dropped result)))
                   (t/is (= [[:probe ["opus" "vorbis"]] :video-track :start :frame :frames-closed :finalize]
                            @log))
                   (done)))
          (.catch (failed done))))))

(t/deftest the-sound-of-a-video-takes-a-codec-its-format-carries
  (t/async
    done
    (let [codec (fn [format encodable]
                  (dea/audio-codec format {:first-audio-codec
                                           (fn [codecs _]
                                             (js/Promise.resolve (some (set encodable) (array-seq codecs))))
                                           :quality fake-quality}))]
      (-> (p/all [(codec :mp4 #{"aac" "opus" "vorbis"})
                  (codec :mp4 #{"opus" "vorbis"})
                  (codec :mp4 #{"vorbis"})
                  (codec :webm #{"aac" "opus" "vorbis"})
                  (codec :webm #{"aac" "vorbis"})
                  (codec :webm #{"aac"})])
          (p/then (fn [codecs]
                    (t/is (= ["aac" "opus" nil "opus" "vorbis" nil] (vec codecs))
                          "MP4 takes AAC, else Opus; WebM Opus, else Vorbis")
                    (done)))
          (p/catch (failed done))))))

(defn- u32
  [n]
  (mapv #(bit-and (unsigned-bit-shift-right n %) 255) [24 16 8 0]))

(defn- ascii
  [text]
  (mapv #(.charCodeAt text %) (range (count text))))

(defn- box
  "The bytes of an MP4 box of `type` holding `parts`."
  [type & parts]
  (let [body (vec (apply concat parts))]
    (into (u32 (+ 8 (count body))) (concat (ascii type) body))))

(defn- track
  "A track whose media is `handler`, of `n` samples in one chunk at
  `offset` of the file."
  [handler n offset]
  (box "trak"
       (box "mdia"
            (box "hdlr" (u32 0) (u32 0) (ascii handler) (repeat 13 0))
            (box "minf"
                 (box "stbl"
                      (box "stsz" (u32 0) (u32 0) (u32 n) (mapcat (fn [_] (u32 2)) (range n)))
                      (box "stco" (u32 0) (u32 1) (u32 offset)))))))

(defn- movie
  "An MP4 whose movie box comes first, with a picture track and a sound
  track of 3 samples, and the chunk offsets of both."
  []
  (let [ftyp    (box "ftyp" (ascii "isom") (u32 0))
        sizes   (count (concat ftyp (box "moov" (track "vide" 1 0) (track "soun" 3 0))))
        picture (+ sizes 8)
        sound   (+ picture 2)]
    {:bytes (js/Uint8Array. (clj->js (concat ftyp
                                             (box "moov" (track "vide" 1 picture) (track "soun" 3 sound))
                                             (box "mdat" [7 7] [9 9 9 9 9 9]))))
     :picture picture
     :sound sound}))

(defn- read-boxes
  [^js view start end]
  (loop [offset start acc []]
    (if (>= offset end)
      acc
      (let [size (.getUint32 view offset)
            type (apply str (map #(char (.getUint8 view (+ offset 4 %))) (range 4)))]
        (recur (+ offset size) (conj acc {:type type :offset offset :size size}))))))

(defn- box-tree
  "The boxes of the MP4 `buffer`, nested as far as a sample table."
  [buffer]
  (let [view (js/DataView. buffer)
        walk (fn walk [start end]
               (mapv (fn [{:keys [type offset size] :as b}]
                       (cond-> b
                         (#{"moov" "trak" "mdia" "minf" "stbl"} type)
                         (assoc :children (walk (+ offset 8) (+ offset size)))))
                     (read-boxes view start end)))]
    (walk 0 (.-byteLength buffer))))

(defn- find-boxes
  [tree type]
  (mapcat #(if (= type (:type %)) [%] (find-boxes (:children %) type)) tree))

(t/deftest an-aac-movie-gets-a-roll-group
  (let [{:keys [bytes picture sound]} (movie)
        before (.-buffer bytes)
        after  (dea/add-aac-roll-group before)
        view   (js/DataView. after)
        tree   (box-tree after)
        [video audio] (find-boxes tree "stbl")
        types  #(mapv :type (:children %))]
    (t/is (= (+ 54 (.-byteLength before)) (.-byteLength after)))
    (t/is (= (.-byteLength after) (reduce + (map :size tree))) "the boxes fill the file")
    (t/is (= ["stsz" "stco"] (types video)))
    (t/is (= ["stsz" "stco" "sgpd" "sbgp"] (types audio)))
    (let [sgpd (d/seek #(= "sgpd" (:type %)) (:children audio))
          sbgp (d/seek #(= "sbgp" (:type %)) (:children audio))]
      (t/is (= -1 (.getInt16 view (+ (:offset sgpd) 24))) "a roll distance of -1")
      (t/is (= 3 (.getUint32 view (+ (:offset sbgp) 20))) "for its 3 samples"))
    (t/testing "the chunks still point at their samples"
      (let [[v a] (map #(.getUint32 view (+ (:offset %) 16)) (find-boxes tree "stco"))
            bytes (js/Uint8Array. after)]
        (t/is (= [(+ 54 picture) (+ 54 sound)] [v a]))
        (t/is (= [7 9] [(aget bytes v) (aget bytes a)]))))
    (t/testing "once"
      (t/is (identical? after (dea/add-aac-roll-group after))))
    (t/testing "a file that is no such movie stays as it is"
      (let [webm (.-buffer (js/Uint8Array. #js [26 69 223 163 0 0 0 0]))]
        (t/is (identical? webm (dea/add-aac-roll-group webm)))))))

(defn- clip-at
  [start duration]
  {:audio-id (uuid/next) :name "beat.mp3" :source-duration 4000 :start start :duration duration})

(t/deftest the-sound-of-a-video-lasts-as-its-frames
  (t/async
    done
    (let [mixed  (atom nil)
          mix    (fn [plan length _]
                   (reset! mixed [(mapv (juxt :at :offset :length) plan) length])
                   (js/Promise.resolve #js {:name "mix"}))
          ;; the second clip is cut at the end of the timeline
          tl     (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                     (ctau/add-audio-clip (clip-at 200 300))
                     (ctau/add-audio-clip (clip-at 800 600)))
          n      (count (cta/frame-times 1000 30))
          there  (count (cta/ping-pong-frames (range n) false))]
      (-> (dea/export-audio tl n 30 :mix mix)
          (p/then
           (fn [sound]
             (t/is (= "mix" (.-name sound)))
             (t/is (= [[[200 0 300] [800 0 200]] (/ (* 1000 n) 30)] @mixed) "as long as the frames")
             (dea/export-audio (assoc tl :playback :ping-pong) there 30 :mix mix)))
          (p/then
           (fn [_]
             (t/is (= [[[200 0 300] [800 0 200]] (/ (* 1000 there) 30)] @mixed)
                   "there and back, silent on the way back")
             (dea/export-audio (assoc tl :playback :loop) n 30 :mix mix)))
          (p/then
           (fn [_]
             (t/is (= [[200 0 300] [800 0 200]] (first @mixed)) "a loop plays its clips once")
             (reset! mixed nil)
             (dea/export-audio (cta/make-timeline {:board-id (uuid/next) :duration 1000}) n 30 :mix mix)))
          (p/then
           (fn [sound]
             (t/is (nil? sound) "no clip, no sound")
             (t/is (nil? @mixed))
             (done)))
          (p/catch (failed done))))))

(defn- copy-scene
  "A component whose main board plays a clip, with a copy of it in board
  B, whose timeline plays `own-clip` when given."
  [own-clip]
  (let [file    (-> (cthf/sample-file :file1)
                    (ctho/add-simple-component :anim :main :main-rect
                                               :root-params {:x 0 :y 0 :width 200 :height 200}
                                               :child-params {:x 10 :y 20 :width 50 :height 50})
                    (ctho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                    (cthc/instantiate-component :anim :copy :parent-label :board-b))
        main    (cthi/id :main)
        board   (cthi/id :board-b)
        main-tl (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                    (cta/add-keyframe (cthi/id :main-rect) {:time 0 :property :x :value 10})
                    (cta/add-keyframe (cthi/id :main-rect) {:time 1000 :property :x :value 60})
                    (ctau/add-audio-clip (assoc (clip-at 0 1000) :name "main")))
        file    (cthf/apply-changes
                 file
                 (cond-> (-> (pcb/empty-changes nil)
                             (pcb/with-page (cthf/current-page file))
                             (pcb/set-timeline main main-tl))
                   (some? own-clip)
                   (pcb/set-timeline board (-> (cta/make-timeline {:board-id board :duration 2000})
                                               (ctau/add-audio-clip own-clip)))))]
    {:page (cthf/current-page file) :files {(:id file) file} :board board}))

(t/deftest a-board-exports-its-own-clips-and-those-of-the-mains-of-its-copies
  (cthi/reset-idmap!)
  (let [{:keys [page files board]} (copy-scene (assoc (clip-at 500 1000) :name "own"))
        tl    (dwa/board-export-timeline page files board)
        clips (sort-by (juxt :lane :start) (vals (:audio-clips tl)))]
    (t/is (seq (:tracks tl)) "it plays the copy")
    (t/is (= [["own" 500 0] ["main" 0 1] ["main" 1000 1]] (map (juxt :name :start :lane) clips))
          "the clip of the main at each round of the copy, after the lanes of the board")
    (t/is (= (get-in page [:timelines board :audio-clips])
             (select-keys (:audio-clips tl) (keys (get-in page [:timelines board :audio-clips]))))))
  (cthi/reset-idmap!)
  (let [{:keys [page files board]} (copy-scene nil)
        tl (dwa/board-export-timeline page files board)]
    (t/is (seq (:tracks tl)) "it plays the copy")
    (t/is (= [["main" 0 0]] (map (juxt :name :start :lane) (vals (:audio-clips tl))))
          "none of its own: the one of the main, on the clock of the copy")))

(defn- frame
  "A frame of `w` × `h` pixels, all of `value` (each channel)."
  [w h value]
  {:data (.fill (js/Uint8ClampedArray. (* 4 w h)) value) :width w :height h})

(defn- pixel
  [{:keys [data width]} x y]
  (aget data (* 4 (+ x (* width y)))))

(t/deftest the-frames-of-a-board-are-cut-to-the-board
  ;; a moon over the edge of a board that does not clip: the picture of
  ;; the board grows and moves as it goes round, the export does not
  (let [board  {:x 0 :y 0 :width 4 :height 3}
        frames [(frame 4 3 10) (frame 4 5 20) (frame 5 3 30) (frame 2 2 40)]
        rects  [{:x 0 :y 0} {:x 0 :y -2} {:x -1 :y 0} {:x 1 :y 1}]
        out    (dea/crop-frames frames rects board 1)]
    (t/is (= #{[4 3]} (into #{} (map (juxt :width :height)) out)) "the size of the board")
    (t/is (identical? (first frames) (first out)) "a picture of the board as it is")
    (t/is (= [20 20] [(pixel (nth out 1) 0 0) (pixel (nth out 1) 3 2)]) "what is above it cut")
    (t/is (= [30 30] [(pixel (nth out 2) 0 0) (pixel (nth out 2) 3 2)]) "what is left of it cut")
    (t/is (= [0 40 40 0] [(pixel (nth out 3) 0 0) (pixel (nth out 3) 1 1)
                          (pixel (nth out 3) 2 2) (pixel (nth out 3) 3 2)])
          "where the picture does not reach, transparent"))
  (t/testing "at a scale"
    (let [[out] (dea/crop-frames [(frame 4 4 7)] [{:x 0 :y -0.5}] {:x 0 :y 0 :width 2 :height 1} 2)]
      (t/is (= [4 2] ((juxt :width :height) out)))
      (t/is (= [7 7] [(pixel out 0 0) (pixel out 3 1)]))))
  (t/testing "without boxes or the board: the frames as they are"
    (let [frames [(frame 2 2 1) (frame 3 1 2)]]
      (t/is (identical? frames (dea/crop-frames frames [nil nil] {:x 0 :y 0 :width 2 :height 2} 1)))
      (t/is (identical? frames (dea/crop-frames frames [{:x 0 :y 0} {:x 0 :y 0}] nil 1)))
      (t/is (= [] (dea/crop-frames [] [] {:x 0 :y 0 :width 2 :height 2} 1))))))

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
    (t/async
      done
      (-> (dea/collect-layer-images objects tl {:bridge bridge :yield #(js/Promise.resolve nil)})
          (.then (fn [images]
                   (t/is (contains? images cid))
                   (t/is (str/starts-with? (get-in images [cid :href]) "data:image/png;base64,"))
                   (t/is (some? (get-in images [cid :id])))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest an-image-covers-the-box-of-its-picture
  ;; a shadow or a turn makes the picture larger than the shape's box
  (let [{:keys [objects bid cid]} (test-objects)
        tl     (-> (cta/make-timeline {:board-id bid :duration 500})
                   (cta/add-keyframe cid {:time 0 :property :x :value 10}))
        bridge {:clean-modifiers (fn [])
                :use-shape (fn [_])
                :set-shape-hidden (fn [_])
                :apply-shape-properties (fn [_ _])
                :render-shape-pixels (fn [_ _ _] (js/Uint8Array. #js [137 80 78 71]))
                :get-shape-extrect (fn [id] (when (= id cid) {:x 4 :y 6 :width 33 :height 30}))}]
    (t/async
      done
      (-> (dea/collect-layer-images objects tl {:bridge bridge :yield #(js/Promise.resolve nil)})
          (.then (fn [images]
                   (t/is (= [4 6 33 30 33 30]
                            ((juxt :x :y :width :height :w :h) (get images cid))))
                   (t/is (= [0 0 100 80] ((juxt :x :y :width :height) (get images bid)))
                         "without a box to read, that of the shape")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest collect-layer-images-renders-the-layers-as-designed
  ;; The preview at the playhead faded the layer out: it is rasterized
  ;; with its own opacity.
  (let [{:keys [objects bid cid]} (test-objects)
        tl     (-> (cta/make-timeline {:board-id bid :duration 1000})
                   (cta/add-keyframe cid {:time 0 :property :opacity :value 0})
                   (cta/add-keyframe cid {:time 1000 :property :opacity :value 1}))
        calls  (atom [])
        bridge {:clean-modifiers (fn [])
                :use-shape (fn [_])
                :set-shape-hidden (fn [_])
                :apply-shape-properties (fn [shape props] (log calls :props shape (vec props)))
                :render-shape-pixels (fn [id _ _]
                                       (log calls :render id)
                                       (js/Uint8Array. #js [137 80 78 71]))}]
    (t/async
      done
      (-> (dea/collect-layer-images objects tl {:bridge bridge :time 0 :yield #(js/Promise.resolve nil)})
          (.then (fn [_]
                   (let [calls  @calls
                         props  (fn [calls] (filter #(and (= :props (first %)) (= cid (:id (second %)))) calls))
                         render (first (keep-indexed #(when (= [:render cid] %2) %1) calls))
                         [_ rest-shape rest-props] (first (props calls))]
                     (t/is (= (get objects cid) rest-shape) "its own properties")
                     (t/is (= [:opacity] rest-props))
                     (t/is (= 1 (:opacity (second (last (props (take render calls)))))) "rasterized opaque")
                     (t/is (= (get objects cid) (second (first (props (drop render calls)))))
                           "and given its opacity back"))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest export-formats-follow-the-renderer
  (let [{:keys [objects bid cid]} (test-objects)
        tl        (cta/add-keyframe (cta/make-timeline {:board-id bid :duration 1000})
                                    cid {:time 0 :property :opacity :value 1})
        text      (cts/setup-shape {:type :text :x 40 :y 40 :width 20 :height 10})
        with-text (-> objects
                      (assoc (:id text) (assoc text :parent-id bid :frame-id bid))
                      (update-in [bid :shapes] conj (:id text)))]
    (t/is (= [:mp4 :webm :gif :avif :svg :lottie :dotlottie] (dea/export-formats with-text tl true)))
    (t/testing "without WASM, Lottie when all its layers draw as vectors"
      (t/is (= [:lottie :dotlottie] (dea/export-formats objects tl false)))
      (t/is (= [] (dea/export-formats with-text tl false))))
    (t/is (= [] (dea/export-formats objects nil true)))))

(t/deftest a-dotlottie-archive-reads-back
  (t/async
    done
    (let [doc    {:v "5.7.4" :w 100 :h 50 :layers []
                  :assets [{:id "img_1" :w 10 :h 10 :u "/images/" :p "img_1.png" :e 0}]}
          png    (js/Uint8Array. #js [137 80 78 71 13 10 26 10])
          read   @#'dwl/read-dotlottie
          text   #(p/then (uz/read-as-text %) (fn [text] (js->clj (js/JSON.parse text) :keywordize-keys true)))
          bytes  #(p/then (.getData ^js % (uz/bytes-writer)) vec)]
      (-> (p/let [blob     (dea/lottie->dotlottie doc "intro" {"img_1.png" png} true)
                  {:keys [files] :as read-back} (read blob)
                  manifest (text (get files "manifest.json"))
                  image    (bytes (get files "images/img_1.png"))]
            (t/is (= #{"manifest.json" "animations/intro.json" "images/img_1.png"} (set (keys files))))
            (t/is (= {:version "1" :generator "Penpot"
                      :animations [{:id "intro" :loop true :autoplay true}]}
                     manifest))
            (t/is (= doc (:doc read-back)) "the importer reads the document back")
            (t/is (= (vec png) image) "the image keeps its bytes"))
          (p/catch (fn [cause] (t/is (nil? cause))))
          (p/finally (fn [] (done)))))))

(defn- text-board
  "A board with a text of three words in it."
  []
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 300 :height 100})
        text  (-> (cts/setup-shape {:type :text :x 10 :y 10 :width 200 :height 20})
                  (assoc :content {:type "root"
                                   :children [{:type "paragraph-set"
                                               :children [{:type "paragraph"
                                                           :children [{:text "Hi you there"}]}]}]}))
        bid   (:id board)]
    {:bid bid
     :tid (:id text)
     :objects {bid (assoc board :shapes [(:id text)])
               (:id text) (assoc text :parent-id bid :frame-id bid)}}))

(defn- words-use
  "A use of a style of `entry`."
  [entry]
  (cta/make-animation {:type :style :style-ref-id (uuid/next) :style-ref-file (uuid/next)
                       :name "Words" :start 0 :entries [entry]}))

(t/deftest markup-formats-note-the-split-text-they-play-whole
  ;; they play the units of a split text, but of one that underlines or
  ;; strikes some of its text
  (let [{:keys [objects bid tid]} (text-board)
        with  #(cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid %)
        split {:type :fade :split :word :start 0 :duration 500}
        lined (assoc-in objects [tid :content :children 0 :children 0 :children 0 :text-decoration] "underline")
        notes {:svg [:split-text] :lottie [:split-text] :dotlottie [:split-text]}]
    (t/is (= {} (dea/export-caveats objects (with split))))
    (t/is (= notes (dea/export-caveats lined (with split))))
    (t/is (= notes (dea/export-caveats lined (with (words-use split))))
          "a split entry of a style use too")
    (t/is (= {} (dea/export-caveats lined (with {:type :fade}))))
    (t/is (= {} (dea/export-caveats objects nil)))))

(t/deftest each-unit-of-a-split-text-is-its-own-picture
  (let [{:keys [objects bid tid]} (text-board)
        tl     (cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid
                                  {:type :fade :split :word :start 0 :duration 500})
        shots  (atom [])
        bridge {:clean-modifiers (fn [])
                :apply-shape-properties (fn [shape props]
                                          (when (some #{:text-units} props)
                                            (swap! shots conj (:text-units shape))))
                :render-shape-pixels (fn [_ _ _] (js/Uint8Array. #js [137 80 78 71]))
                :get-shape-extrect (fn [_] {:x 10 :y 10 :width 200 :height 20})
                ;; as the browser crops each to its ink
                :crop-picture (fn [bytes rect] (p/resolved {:bytes bytes :rect (update rect :x + 5)}))}
        texts  (dea/unit-texts objects tl)]
    (t/is (= #{tid} (set (keys texts))))
    (t/async
      done
      (-> (dea/collect-unit-images objects tl texts {:bridge bridge :yield #(js/Promise.resolve nil)})
          (p/then (fn [images]
                    (t/is (= #{[tid 0] [tid 1] [tid 2] [tid :rest]} (set (keys images))))
                    (t/is (= [15 10 200 20] ((juxt :x :y :width :height) (get images [tid 1])))
                          "where its picture is, in the board")
                    (let [[first-shot second-shot :as all] @shots]
                      (t/is (= [3 7] (mapv :start first-shot)) "the first word alone: the others hidden")
                      (t/is (every? #(zero? (:opacity %)) first-shot))
                      (t/is (= [0 7] (mapv :start second-shot)))
                      (t/is (= 3 (count (nth all 3))) "then all of them: what is no word")
                      (t/is (= [[3 7] [0 7] [0 3] [0 3 7]] (mapv #(mapv :start %) (take 4 all)))
                            "each picture hides the other units, the last all of them")
                      (t/is (every? #(zero? (:opacity %)) (mapcat identity (take 4 all))))
                      (t/is (= [] (last all)) "and the text back at rest"))))
          (p/catch (fn [cause] (t/is (nil? cause) (str cause))))
          (p/finally done)))))

(t/deftest the-unit-pictures-stop-at-a-cancel-with-the-text-at-rest
  (let [{:keys [objects bid tid]} (text-board)
        tl       (cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid
                                    {:type :fade :split :word :start 0 :duration 500})
        shots    (atom [])
        cancel?  (atom false)
        progress (atom [])
        bridge   {:clean-modifiers (fn [])
                  :apply-shape-properties (fn [shape props]
                                            (when (some #{:text-units} props)
                                              (swap! shots conj (:text-units shape))))
                  :render-shape-pixels (fn [_ _ _] (js/Uint8Array. #js [137 80 78 71]))
                  :get-shape-extrect (fn [_] {:x 10 :y 10 :width 200 :height 20})
                  :crop-picture (fn [bytes rect] (p/resolved {:bytes bytes :rect rect}))}
        texts    (dea/unit-texts objects tl)]
    (t/is (= 4 (dea/unit-shot-count texts)) "three words and the rest")
    (t/async
      done
      (-> (dea/collect-unit-images objects tl texts
                                   {:bridge bridge
                                    :yield #(js/Promise.resolve nil)
                                    :cancelled? #(deref cancel?)
                                    ;; cancelled after the first picture
                                    :on-progress (fn [i n]
                                                   (swap! progress conj [i n])
                                                   (reset! cancel? true))})
          (.then (fn [_] (t/is false "it should stop")))
          (.catch (fn [cause]
                    (t/is (= :export-cancelled (:type (ex-data cause))))
                    (t/is (= [[1 4]] @progress) "one picture of four")
                    (t/is (= 2 (count @shots)) "one picture, then")
                    (t/is (= [] (last @shots)) "the text back at rest")))
          (.finally (finish done))))))

(t/deftest a-markup-export-counts-the-unit-pictures
  (let [{:keys [objects bid tid]} (text-board)
        tl       (cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid
                                    {:type :fade :split :word :start 0 :duration 500})
        progress (atom [])
        bridge   {:clean-modifiers (fn [])
                  :use-shape (fn [_])
                  :set-shape-hidden (fn [_])
                  :set-modifiers (fn [& _])
                  :apply-shape-properties (fn [_ _])
                  :render-shape-pixels (fn [_ _ _] (js/Uint8Array. #js [137 80 78 71]))
                  :get-shape-extrect (fn [_] {:x 10 :y 10 :width 200 :height 20})
                  :crop-picture (fn [bytes rect] (p/resolved {:bytes bytes :rect rect}))}]
    (t/async
      done
      (-> (:promise (dea/run-export {:page {:id (uuid/next) :objects objects :timelines {bid tl}}
                                     :files {}
                                     :board-id bid
                                     :bridge bridge
                                     :yield #(js/Promise.resolve nil)
                                     :on-progress #(swap! progress conj %)}
                                    {:format :svg}))
          (.then (fn [result]
                   (let [layers (filter #(= :layers (:phase %)) @progress)]
                     (t/is (= :ok (:status result)))
                     (t/is (= (count layers) (:total (first layers))) "every picture counted")
                     (t/is (= (+ 1 4) (:total (first layers))) "the board, then the units of the text")
                     (t/is (= :write (:phase (last @progress)))))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest markup-exports-read-the-board-without-split-keys
  (let [{:keys [objects bid tid]} (text-board)
        tl    (-> (cta/make-timeline {:board-id bid :duration 1000})
                  (cta/add-animation tid {:type :fade :split :word})
                  (cta/add-animation tid (words-use {:type :move :split :word :start 0 :duration 500})))
        page  {:objects objects :timelines {bid tl}}
        split (fn [timeline]
                (->> (get-in timeline [:tracks tid :animations])
                     (mapcat #(cons % (:entries %)))
                     (keep :split)))]
    (t/is (= [:word :word] (split (dwa/board-export-timeline page {} bid))))
    (t/is (= [] (split (dwa/board-export-timeline page {} bid :text-split :whole))))))

(t/deftest frames-after-a-split-show-the-text-at-rest
  (let [{:keys [objects bid tid]} (text-board)
        tl     (cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid
                                  {:type :fade :split :word :start 0 :duration 500})
        sent   (atom [])
        bridge {:set-modifiers (fn [& _])
                :clean-modifiers (fn [])
                :apply-shape-properties (fn [shape props]
                                          (when (some #{:text-units} props)
                                            (swap! sent conj (:text-units shape))))}]
    (doseq [time [250 500 750]]
      (dea/apply-pose! objects (cta/timeline->modif-tree tl objects time) {:bridge bridge}))
    (t/is (= 3 (count @sent)) "every frame")
    (t/is (seq (first @sent)) "posed while it plays")
    (t/is (= [[] []] (rest @sent)) "at rest from its end")))

(t/deftest a-text-is-shot-at-rest
  (let [{:keys [objects tid]} (text-board)
        calls  (atom [])
        bridge {:use-shape (fn [_])
                :set-shape-hidden (fn [_])
                :clean-modifiers (fn [])
                :apply-shape-properties (fn [shape props] (log calls :props shape (vec props)))
                :render-shape-pixels (fn [_ _ _] (js/Uint8Array. 0))}]
    (dea/render-shape-at-rest objects tid [] {:bridge bridge})
    (let [[_ shape props] (first @calls)]
      (t/is (= [:opacity :text-units] props))
      (t/is (= [] (:text-units shape)) "whatever the canvas posed"))))

;; AUDIO IN THE EXPORT PANEL

(t/deftest formats-without-sound-say-so
  (let [{:keys [objects bid]} (test-objects)
        tl    (cta/make-timeline {:board-id bid :duration 1000})
        clips (ctau/add-audio-clip tl (clip-at 0 500))]
    (t/is (= {} (dea/export-caveats objects tl)) "no clips, nothing to say")
    (t/is (= {:gif [:no-audio] :avif [:no-audio] :svg [:no-audio] :lottie [:no-audio] :dotlottie [:no-audio]}
             (dea/export-caveats objects clips))
          "MP4 and WebM carry it")
    (t/testing "a board that plays only audio"
      (t/is (= [:mp4 :webm :gif :avif :svg :lottie :dotlottie] (dea/export-formats objects clips true)))
      (t/is (not-any? dea/audio-formats (dea/export-formats objects clips false))
            "without the WASM renderer, no format with sound"))))

(defn- export-scene
  "The board of `test-objects` on a page of its own, its rect moving over
  `duration` ms; and a bridge to a fake WASM that notes its calls."
  [duration]
  (let [{:keys [objects bid cid]} (test-objects)
        tl    (-> (cta/make-timeline {:board-id bid :duration duration})
                  (cta/add-keyframe cid {:time 0 :property :x :value 10})
                  (cta/add-keyframe cid {:time duration :property :x :value 50}))
        calls (atom [])]
    {:page {:id (uuid/next) :objects objects :timelines {bid tl}}
     :bid bid
     :calls calls
     :bridge {:set-modifiers (fn [_ & _] (swap! calls conj :modifiers))
              :clean-modifiers (fn [] nil)
              :use-shape (fn [_] nil)
              :set-shape-hidden (fn [_] nil)
              :apply-shape-properties (fn [_ _] nil)
              :render-shape-pixels (fn [id _ _]
                                     (swap! calls conj [:pixels id])
                                     (js/Uint8Array. #js [137 80 78 71]))}}))

(defn- export-ctx
  "What `run-export` takes for the scene: the page and no files, frames
  decoded as 2x2 px, no turn for the page, a fake GIF encoder."
  [{:keys [page bid bridge]} & {:as more}]
  (merge {:page page
          :files {}
          :board-id bid
          :bridge bridge
          :decode (fn [_] (js/Promise.resolve {:data (js/Uint8ClampedArray. 16) :width 2 :height 2}))
          :yield #(js/Promise.resolve nil)
          ;; gifenc is a browser module
          :gif-encoder (fn [] (fake-gif (atom nil) (atom 0)))
          :quantize (fn [data _] data)
          :apply-palette (fn [data _] data)}
         more))

(t/deftest an-export-runs-from-a-page-alone
  (t/async
    done
    (let [scene    (export-scene 100)
          progress (atom [])
          frames   (atom 0)
          rect     {:x 0 :y 0 :width 10 :height 10}
          {:keys [promise]}
          (dea/run-export (export-ctx scene
                                      :bridge (assoc (:bridge scene) :get-shape-extrect (constantly rect))
                                      :on-progress #(swap! progress conj %)
                                      :on-frame (fn [image at scale]
                                                  (t/is (= 2 (:width image)) "decoded")
                                                  (t/is (= [rect 1] [at scale]) "where it is, at what scale")
                                                  (swap! frames inc)))
                          {:format :gif :quality :low :fps 30 :scale 1 :loop? true})]
      (-> promise
          (.then (fn [result]
                   (t/is (= [:ok ".gif" "image/gif"] ((juxt :status :ext :mtype) result)))
                   (t/is (string? (:name result)) "named after the board")
                   (t/is (pos? (.-size (:blob result))))
                   (t/is (= [:capture :encode] (distinct (map :phase @progress))) "the frames, then their encoding")
                   (t/is (= 100 (dea/progress-percent (last @progress))) "100 % at the end only")
                   (t/is (every? #(< (dea/progress-percent %) 100) (butlast @progress)))
                   (t/is (some #{[:pixels (:bid scene)]} @(:calls scene)) "through the bridge")
                   (t/is (= (count (filter vector? @(:calls scene))) @frames)
                         "each picture as it is taken")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest an-export-pictures-a-board-that-does-not-clip-clipped
  (t/async
    done
    (let [scene  (export-scene 100)
          bid    (:bid scene)
          scene  (update-in scene [:page :objects bid] assoc :show-content true)
          pushed (atom [])
          {:keys [promise]}
          (dea/run-export (export-ctx scene
                                      :bridge (assoc (:bridge scene) :apply-shape-properties
                                                     (fn [shape props]
                                                       (when (= bid (:id shape))
                                                         (swap! pushed conj [(:show-content shape) props])))))
                          {:format :gif :quality :low :fps 30 :scale 1 :loop? true})]
      (-> promise
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (= [[false [:show-content :selrect]] [true [:show-content :selrect]]] @pushed)
                         "clipped while it is pictured, then as it was")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest an-export-stops-at-its-next-frame-when-cancelled
  (t/async
    done
    (let [scene  (export-scene 1000)
          cancel (atom nil)
          {:keys [promise] :as job}
          (dea/run-export (export-ctx scene :on-progress (fn [_] (@cancel)))
                          {:format :gif :quality :low :fps 30 :scale 1})]
      (reset! cancel (:cancel job))
      (-> promise
          (.then (fn [_] (t/is false "it should stop")))
          (.catch (fn [cause]
                    (t/is (= :export-cancelled (:type (ex-data cause))))
                    (t/is (= 1 (count (filter vector? @(:calls scene)))) "one frame taken")))
          (.finally (finish done))))))

(t/deftest a-markup-export-stops-between-its-pictures-when-cancelled
  (t/async
    done
    (let [scene    (export-scene 500)
          cancel   (atom nil)
          progress (atom [])
          {:keys [promise] :as job}
          (dea/run-export (export-ctx scene :on-progress (fn [p] (swap! progress conj p) (@cancel)))
                          {:format :svg})]
      (reset! cancel (:cancel job))
      (-> promise
          (.then (fn [_] (t/is false "it should stop")))
          (.catch (fn [cause]
                    (t/is (= :export-cancelled (:type (ex-data cause))))
                    (t/is (= [:layers] (mapv :phase @progress)) "after its first picture")))
          (.finally (finish done))))))

(t/deftest a-markup-export-writes-its-file-from-a-page-alone
  (t/async
    done
    (let [scene    (export-scene 500)
          progress (atom [])
          frames   (atom 0)
          {:keys [promise]} (dea/run-export (export-ctx scene
                                                        :on-progress #(swap! progress conj %)
                                                        :on-frame (fn [_] (swap! frames inc)))
                                            {:format :svg})]
      (-> promise
          (.then (fn [result]
                   (t/is (= [:ok ".svg" "image/svg+xml"] ((juxt :status :ext :mtype) result)))
                   (t/is (= :write (:phase (last @progress))))
                   (t/is (= 100 (dea/progress-percent (last @progress))))
                   (t/is (= 1 @frames) "once, the board at its end")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest an-export-of-a-board-that-plays-nothing-is-nothing
  (t/async
    done
    (let [scene (update (export-scene 500) :page assoc :timelines {})]
      (-> (:promise (dea/run-export (export-ctx scene) {:format :mp4}))
          (.then (fn [result] (t/is (= :nothing (:status result)))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest the-notes-of-an-export
  (t/is (= {:unavailable? true :notes []} (dea/export-notes {:status :unavailable})))
  (t/is (= {:unavailable? false :notes [[:fitted 4092 2302] [:audio-dropped]]}
           (dea/export-notes {:status :ok :fitted [4092 2302] :audio-dropped true})))
  (t/is (= {:unavailable? false :notes []} (dea/export-notes {:status :ok}))))

(t/deftest the-percent-of-an-export
  (t/is (= 0 (dea/progress-percent {:phase :capture :current 0 :total 10})))
  (t/is (= 40 (dea/progress-percent {:phase :capture :current 5 :total 10})))
  (t/is (= 90 (dea/progress-percent {:phase :encode :current 5 :total 10})))
  (t/is (= 45 (dea/progress-percent {:phase :layers :current 1 :total 2})))
  (t/is (= 100 (dea/progress-percent {:phase :write :current 1 :total 1})))
  (t/is (= 0 (dea/progress-percent nil))))

(t/deftest the-end-of-an-export-frees-the-panel
  (let [types #(mapv ptk/type (dea/export-finished %))]
    (t/is (= [:app.main.data.notifications/show ::dea/finish-export-state] (types {:status :unavailable}))
          "a codec it has not: the error, and the panel free again")
    (t/is (= [:app.main.data.notifications/show ::dea/finish-export-state] (types {:status :ok :audio-dropped true}))
          "a video without its sound: a warning")
    (t/is (= [:app.main.data.notifications/show ::dea/finish-export-state] (types {:status :ok :fitted [4092 2302]}))
          "a video smaller than asked: a warning")
    (t/is (= [:app.main.data.notifications/show ::dea/finish-export-state]
             (types {:status :ok :fitted [4092 2302] :audio-dropped true}))
          "both in one")
    (t/is (= [::dea/finish-export-state] (types {:status :ok})))))

(t/deftest a-video-too-large-for-the-encoder-is-made-as-large-as-it-takes
  (t/async
    done
    ;; as Chrome's H.264: at most 4096 px a side and 4096 × 2304 px in all
    (let [encodable? (fn [w h] (js/Promise.resolve (and (<= w 4096) (<= h 4096) (<= (* w h) (* 4096 2304)))))]
      (-> (js/Promise.all #js [(dea/fit-video-size 1920 1080 encodable?)
                               (dea/fit-video-size 5120 2880 encodable?)
                               (dea/fit-video-size 3000 6000 encodable?)
                               (dea/fit-video-size 100 100 (fn [_ _] (js/Promise.resolve false)))])
          (.then
           (fn [[full wide tall none]]
             (t/is (= 1 full) "as it is when it fits")
             (t/is (< 4070 (* 5120 wide) 4096) "as wide as it takes, in proportion")
             (t/is (< (* 0.99 4096 2304) (* 5120 wide 2880 wide) (* 4096 2304)))
             (t/is (< 4070 (* 6000 tall) 4096) "a tall one as high as it takes")
             (t/is (nil? none) "none at all: unavailable")
             (done)))
          (.catch (fn [cause] (t/is (nil? cause) (str cause)) (done)))))))
