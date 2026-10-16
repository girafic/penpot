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
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-tab :as dat]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.lottie :as dwl]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.store :as st]
   [app.render-wasm.api :as wasm.api]
   [app.render-wasm.gesture :as wasm-gesture]
   [app.util.zip :as uz]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]
   [frontend-tests.helpers.export-job :as thj]
   [frontend-tests.helpers.mock :as mock]
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

(defn- recording-gif
  "A gifenc encoder, quantizer and palette mapper that note the options
  of each call in `seen`; the palette has a transparent color at 1."
  [seen]
  {:gif-encoder (fn []
                  #js {:writeFrame (fn [_ _ _ ^js opts]
                                     (swap! seen update :frames (fnil conj [])
                                            [(.-transparent opts) (.-transparentIndex opts) (.-dispose opts)]))
                       :finish (fn [])
                       :bytes (fn [] (js/Uint8Array. #js [71 73 70]))})
   :quantize (fn [_ _ & [^js opts]]
               (swap! seen update :quantize (fnil conj []) (some-> opts .-format))
               #js [#js [9 9 9 255] #js [0 0 0 0]])
   :apply-palette (fn [data _ & [format]]
                    (swap! seen update :apply (fnil conj []) format)
                    data)})

(t/deftest encode-gif-keeps-transparency
  (t/async
    done
    (let [clear  (atom {})
          opaque (atom {})
          frame  (fn [alpha] {:data (js/Uint8ClampedArray. #js [9 9 9 alpha]) :width 1 :height 1})]
      (-> (js/Promise.all
           #js [(dea/encode-gif (merge {:frames [(frame 0) (frame 255)] :fps 30 :keep-alpha? true
                                        :yield #(js/Promise.resolve nil)}
                                       (recording-gif clear)))
                (dea/encode-gif (merge {:frames [(frame 255)] :fps 30 :keep-alpha? true
                                        :yield #(js/Promise.resolve nil)}
                                       (recording-gif opaque)))])
          (.then (fn [_]
                   (t/testing "a pixel transparent or opaque, each frame clearing the one before"
                     (t/is (= ["rgba4444" "rgba4444"] (:quantize @clear)))
                     (t/is (= ["rgba4444" "rgba4444"] (:apply @clear)))
                     (t/is (= [[true 1 2] [true 1 2]] (:frames @clear))))
                   (t/testing "opaque frames as before"
                     (t/is (= [nil] (:quantize @opaque) (:apply @opaque)))
                     (t/is (= [[nil nil nil]] (:frames @opaque))))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

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

(t/deftest ping-pong-videos-capture-the-way-back-where-layers-follow
  (let [{:keys [objects bid cid]} (test-objects)
        dot       (cts/setup-shape {:type :rect :x 60 :y 10 :width 10 :height 10})
        did       (:id dot)
        objects   (-> objects
                      (assoc did (assoc dot :parent-id bid :frame-id bid))
                      (update-in [bid :shapes] conj did))
        tl        (-> (cta/make-timeline {:board-id bid :duration 1000 :playback :ping-pong})
                      (cta/add-keyframe cid {:time 0 :property :x :value 10 :easing :linear})
                      (cta/add-keyframe cid {:time 1000 :property :x :value 50}))
        following (cta/set-follow tl did {:leader cid :delay 200})
        times     [0 250 500 750 1000]
        moved     (fn [back?]
                    (let [entries (atom nil)
                          bridge  {:set-modifiers (fn [entries' & _] (reset! entries entries'))
                                   :apply-shape-properties (fn [_ _])
                                   :render-shape-pixels (fn [_ _ _] (js/Uint8Array. #js [1]))}]
                      (dea/render-shape-at-time objects following bid 500 {:bridge bridge :back? back?})
                      (:e (get (into {} @entries) did))))]
    (t/is (= [750 500 250 0] (dea/back-times following times false)) "back to the start")
    (t/is (= [750 500 250] (dea/back-times following times true)) "the next round shows the start")
    (t/is (nil? (dea/back-times tl times false)) "without follows the frames there play backwards")
    (t/is (nil? (dea/back-times (assoc following :playback :loop) times false)))
    (t/is (< (js/Math.abs (- 12 (moved false))) 0.01) "there: where the rect was at 300 ms")
    (t/is (< (js/Math.abs (- 28 (moved true))) 0.01) "back: where it was at 700 ms, on its way back")))

(t/deftest guides-are-in-no-export
  (let [{:keys [objects bid cid]} (test-objects)
        tl     (-> (cta/make-timeline {:board-id bid :duration 1000})
                   (cta/add-keyframe cid {:time 0 :property :x :value 10})
                   (cta/set-guide cid true))
        page   {:objects objects :timelines {bid tl}}
        calls  (atom [])
        bridge {:use-shape #(swap! calls conj [:use %])
                :set-shape-hidden #(swap! calls conj [:hidden %])}]
    (t/is (= #{cid} (dea/guide-ids page bid)))
    (t/is (empty? (:tracks (dwa/board-export-timeline page {} bid :bake true)))
          "the keyframe exports play nothing of it")
    (t/async
      done
      (-> (dea/guides-hidden bridge objects #{cid}
                             (fn []
                               (swap! calls conj :export)
                               (js/Promise.resolve :done)))
          (.then (fn [result]
                   (t/is (= :done result))
                   (t/is (= [[:use cid] [:hidden true] :export [:use cid] [:hidden false]] @calls)
                         "hidden while it exports, shown as it was after")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest lottie-reads-the-follows-it-makes-children
  (let [{:keys [objects bid cid]} (test-objects)
        dot     (cts/setup-shape {:type :rect :x 60 :y 10 :width 10 :height 10})
        did     (:id dot)
        objects (-> objects
                    (assoc did (assoc dot :parent-id bid :frame-id bid))
                    (update-in [bid :shapes] conj did))
        tl      (-> (cta/make-timeline {:board-id bid :duration 1000})
                    (cta/add-keyframe cid {:time 0 :property :x :value 10})
                    (cta/add-keyframe cid {:time 1000 :property :x :value 50})
                    (cta/set-follow did {:leader cid}))
        page    {:objects objects :timelines {bid tl}}
        follow  #(get-in (dwa/board-export-timeline page {} bid :bake %) [:tracks did :follow])]
    (t/is (some? (follow :lottie)) "Lottie makes the dot a child of the rect")
    (t/is (nil? (follow true)) "SVG and CSS play it by keyframes")))

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

(defn- recording-media
  "`media-fakes` that note the codecs probed for alpha, the alpha of the
  source and the format of each sample in `seen`."
  [seen alpha-codecs]
  (merge (media-fakes (fn [_ _] (js/Promise.resolve "av1")))
         {:can-encode (fn [codec ^js opts]
                        (swap! seen update :probed (fnil conj []) [codec (.-alpha opts)])
                        (js/Promise.resolve (contains? alpha-codecs codec)))
          :sample-source (fn [^js opts]
                           (swap! seen assoc :codec (.-codec opts) :alpha (.-alpha opts))
                           (fake-sample-source opts))
          :video-sample (fn [data ^js opts]
                          (swap! seen update :formats (fnil conj []) (.-format opts))
                          (fake-video-sample data opts))}))

(defn- video-frames
  [n alpha]
  (vec (repeatedly n #(hash-map :data (js/Uint8ClampedArray. #js [9 9 9 alpha, 9 9 9 alpha,
                                                                  9 9 9 alpha, 9 9 9 alpha])
                                :width 2
                                :height 2))))

(t/deftest encode-video-keeps-the-transparency-of-a-webm
  (t/async
    done
    (let [seen (atom {})]
      (-> (dea/encode-video (merge {:format :webm
                                    :frames (video-frames 2 128)
                                    :fps 30
                                    :bitrate 1000
                                    :flatten-bg [255 255 255]
                                    :keep-alpha? true}
                                   (recording-media seen #{"vp8"})))
          (.then (fn [result]
                   (t/is (= :ok (:status result)))
                   (t/is (= [["vp9" "keep"] ["vp8" "keep"]] (:probed @seen)) "the first that keeps it")
                   (t/is (= ["vp8" "keep"] ((juxt :codec :alpha) @seen)))
                   (t/is (= ["I420A" "I420A"] (:formats @seen)))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest encode-video-flattens-what-keeps-no-transparency
  (t/async
    done
    (let [opaque   (atom {})
          no-codec (atom {})
          mp4      (atom {})]
      (-> (js/Promise.all
           #js [(dea/encode-video (merge {:format :webm :frames (video-frames 1 255) :fps 30 :bitrate 1000
                                          :keep-alpha? true}
                                         (recording-media opaque #{"vp9"})))
                (dea/encode-video (merge {:format :webm :frames (video-frames 1 0) :fps 30 :bitrate 1000
                                          :keep-alpha? true}
                                         (recording-media no-codec #{})))
                (dea/encode-video (merge {:format :mp4 :frames (video-frames 1 0) :fps 30 :bitrate 1000
                                          :keep-alpha? true}
                                         (recording-media mp4 #{"vp9"})))])
          (.then (fn [_]
                   (t/testing "opaque frames"
                     (t/is (nil? (:probed @opaque)))
                     (t/is (= ["av1" "discard" ["I420"]] ((juxt :codec :alpha :formats) @opaque))))
                   (t/testing "no codec that keeps it"
                     (t/is (= 2 (count (:probed @no-codec))))
                     (t/is (= ["av1" "discard" ["I420"]] ((juxt :codec :alpha :formats) @no-codec))))
                   (t/testing "an MP4"
                     (t/is (nil? (:probed @mp4)))
                     (t/is (= ["discard" ["I420"]] ((juxt :alpha :formats) @mp4))))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest rgba-to-i420a-adds-the-alpha-plane
  (let [data (js/Uint8ClampedArray. #js [0 0 0 0, 255 255 255 128, 0 0 0 255])
        {:keys [width height format] :as yuv} (dea/rgba->i420a data 3 1)
        bytes (vec (array-seq (js/Array.from (:data yuv))))]
    (t/is (= [4 2 "I420A"] [width height format]))
    (t/is (= (+ 8 2 2 8) (count bytes)) "Y, U, V, then A")
    (t/is (= [0 128 255 255 0 128 255 255] (subvec bytes 12)) "the opacity, padded")))

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

(t/deftest a-dotlottie-plays-in-the-tab-with-its-images-in-it
  ;; lottie-web plays a Lottie, not the archive: its images as data URIs
  (let [data   {:assets [{:id "img_a" :w 10 :h 10 :u "/images/" :p "img_a.png" :e 0}
                         {:id "comp_x" :layers []}]}
        images {:a {:id "img_a" :bytes (js/Uint8Array. #js [1]) :p "data:image/png;base64,AQ==" :e 1}
                :t {:runs [] :center {:x 0 :y 0}}}
        doc    (dea/dotlottie-preview data images)]
    (t/is (= {:id "img_a" :w 10 :h 10 :u "" :p "data:image/png;base64,AQ==" :e 1} (first (:assets doc))))
    (t/is (= {:id "comp_x" :layers []} (second (:assets doc))) "its precomps as they are")))

(t/deftest a-dotlottie-makes-its-preview-only-for-the-tab
  ;; The Lottie the export tab plays holds a copy of every picture: made
  ;; only when asked for, not for a download.
  (t/async
    done
    (let [board  (cts/setup-shape {:type :frame :x 0 :y 0 :width 100 :height 80 :name "Hero"})
          rect   (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20})
          bid    (:id board)
          rid    (:id rect)
          objs   {bid (assoc board :shapes [rid])
                  rid (assoc rect :parent-id bid :frame-id bid)}
          tl     (-> (cta/make-timeline {:board-id bid :duration 1000})
                     (cta/add-keyframe rid {:time 0 :property :x :value 10})
                     (cta/add-keyframe rid {:time 1000 :property :x :value 60}))
          export #(#'dea/markup-file tl objs board :dotlottie {} %)]
      (-> (js/Promise.all #js [(export {}) (export {:preview? true})])
          (.then (fn [[download tab]]
                   (t/is (= :ok (:status download)))
                   (t/is (nil? (:preview-blob download)) "none for a download")
                   (t/is (instance? js/Blob (:preview-blob tab)) "the tab gets it")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          ;; never `done` itself: it would adopt the promise of the next test
          (.finally (fn [] (done)))))))

(t/deftest one-export-runs-at-a-time-in-the-workspace
  ;; The canvas draws the frames of one: a second waits for nothing.
  (let [types (fn [state]
                (let [out (atom [])]
                  (->> (ptk/watch (dea/export-animation {:format :mp4}) state (rx/empty))
                       (rx/subs! #(swap! out conj (ptk/type %))))
                  @out))]
    (t/is (= [::dea/start-export-animation] (types {})))
    (t/is (= [] (types {:export-animation {:in-progress true}})) "not while one runs")))

(t/deftest the-export-tab-asks-for-the-lottie-it-plays
  ;; lottie-web plays no dotLottie archive: the tab plays the Lottie made
  ;; for it (see `dea/markup-file`), which it asks for.
  (let [live @@#'dat/live
        seen (atom nil)]
    (with-redefs [dea/run-exports (fn [ctx _ _ _]
                                    (reset! seen ctx)
                                    {:promise (js/Promise.resolve {:status :nothing}) :cancel (fn [])})
                  dat/tab-bridge  (constantly {})]
      (#'dat/run-job {:format :dotlottie :board-id (uuid/next) :scale 1} {:objects {}} {}))
    (reset! @#'dat/live live)
    (t/is (true? (:preview? @seen)))))

(t/deftest several-boards-export-into-one-zip
  (t/async
    done
    (let [[a b c]  [(uuid/next) (uuid/next) (uuid/next)]
          calls    (atom [])
          progress (atom [])]
      (mock/with-mocks
        {dea/run-export (fn [{:keys [board-id on-progress preview?]} params]
                          (swap! calls conj [board-id (:scale params) (:format params) preview?])
                          (on-progress {:phase :capture :current 1 :total 2})
                          {:promise (js/Promise.resolve
                                     (if (= board-id c)
                                       {:status :nothing}
                                       (cond-> {:status :ok :blob (js/Blob. #js ["gif"]) :name "Hero" :ext ".gif"}
                                         (= board-id b) (assoc :fitted [100 50]))))
                           :cancel (fn [])})}
        (fn [done']
          (-> (:promise (dea/run-exports {:on-progress #(swap! progress conj %) :preview? true}
                                         {:format :gif}
                                         [{:board-id a :scale 2} {:board-id b :scale 1} {:board-id c :scale 0.5}]
                                         "Deck"))
              (.then (fn [result]
                       (t/is (= [[a 2 :gif false] [b 1 :gif false] [c 0.5 :gif false]] @calls)
                             "one after another, each at its scale, with nothing to preview in a ZIP")
                       (t/is (= [13 47 80] (mapv dea/progress-percent @progress)) "each board its share")
                       (t/is (= ["Deck" ".zip" "application/zip"] ((juxt :name :ext :mtype) result)))
                       (t/is (= ["Hero.gif" "Hero-2.gif"] (:files result))
                             "what plays nothing left out, a name that comes again numbered")
                       (t/is (= [100 50] (:fitted result)))
                       (uz/get-entries (uz/reader (:blob result)))))
              (.then (fn [entries]
                       (t/is (= ["Hero.gif" "Hero-2.gif"] (mapv #(.-filename ^js %) entries)))))
              (.catch (fn [cause] (t/is false (ex-message cause))))
              (.finally #(done'))))
        done))))

(t/deftest several-boards-stop-when-cancelled
  (t/async
    done
    (let [calls  (atom [])
          ended  (atom nil)
          cancel (atom nil)]
      (mock/with-mocks
        {dea/run-export (fn [{:keys [board-id]} _]
                          (swap! calls conj board-id)
                          ;; cancelled while the first board exports
                          (js/setTimeout #(@cancel) 0)
                          {:promise (js/Promise. (fn [resolve] (reset! ended resolve)))
                           :cancel (fn [] (@ended {:status :ok :blob (js/Blob. #js [""]) :name "A" :ext ".gif"}))})}
        (fn [done']
          (let [{:keys [promise] :as run}
                (dea/run-exports {} {:format :gif} [{:board-id :a :scale 1} {:board-id :b :scale 1}] "Deck")]
            (reset! cancel (:cancel run))
            (-> promise
                (.then (fn [_] (t/is false "not cancelled")))
                (.catch (fn [cause]
                          (t/is (= :export-cancelled (:type (ex-data cause))))
                          (t/is (= [:a] @calls) "the next one never starts")))
                (.finally #(done')))))
        done))))

(t/deftest the-canvas-shows-the-playhead-again-after-an-export-in-place
  (let [seen  (atom [])
        begun (atom 0)
        sub   (rx/sub! (->> st/stream
                            (rx/map ptk/type)
                            (rx/filter #{::dwa/apply-preview ::dwm/settle-local-transform}))
                       #(swap! seen conj %))]
    (try
      (with-redefs [wasm-gesture/active? (constantly false)
                    wasm-gesture/try-begin-interactive-transform! #(swap! begun inc)
                    wasm.api/set-modifiers-start (fn [])]
        ;; the preview poses in fast mode, which nothing else settles
        (dea/restore-canvas! false)
        (t/is (= [::dwa/apply-preview ::dwm/settle-local-transform] @seen)
              "posed, then in full quality")
        (t/is (zero? @begun))
        (reset! seen [])
        (dea/restore-canvas! true)
        (t/is (= [::dwa/apply-preview] @seen) "fast again, as before the export")
        (t/is (= 1 @begun)))
      (finally
        (rx/dispose! sub)))))

(t/deftest one-board-exports-as-it-is
  (let [seen (atom nil)]
    (with-redefs [dea/run-export (fn [ctx params] (reset! seen [(:board-id ctx) (:scale params)]) :one)]
      (t/is (= :one (dea/run-exports {} {:format :mp4} [{:board-id :a :scale 2}] "Deck")))
      (t/is (= [:a 2] @seen)))))

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

(t/deftest an-export-in-place-waits-for-the-faces-of-its-texts
  ;; A file just opened may still load them: an export would keep the
  ;; faces at hand meanwhile, glyphs missing.
  (t/async
    done
    (let [{:keys [objects bid tid]} (text-board)
          checks (atom 0)
          relaid (atom [])
          wait   #(apply dea/texts-laid-out objects [bid] :relayout (fn [id] (swap! relaid conj id)) %&)]
      (-> (wait :pending (fn [_] (if (< (swap! checks inc) 3) #{"face"} #{})))
          (.then (fn [_]
                   (t/is (= 3 @checks) "until the faces are there")
                   (t/is (= [tid] @relaid) "laid out again with them")
                   (reset! relaid [])
                   (wait :pending (constantly #{}))))
          (.then (fn [_]
                   (t/is (empty? @relaid) "nothing to wait for, nothing laid out")
                   (wait :pending (constantly #{"face"}) :timeout-ms 120)))
          (.then (fn [_]
                   (t/is (= [tid] @relaid) "given up on, laid out with what it has")
                   (-> (wait :pending (constantly #{"face"}) :cancelled? (constantly true))
                       (.then (constantly :waited))
                       (.catch #(:type (ex-data %))))))
          (.then (fn [outcome] (t/is (= :export-cancelled outcome) "cancelled meanwhile")))
          (.catch (fn [cause] (t/is false (str "failed: " cause))))
          (.finally (finish done))))))

(t/deftest an-export-in-place-exports-the-file-as-the-wait-leaves-it
  ;; What is edited while it waits for the faces of its texts goes along,
  ;; as WASM draws it so.
  (t/async
    done
    (let [{:keys [objects bid tid]} (text-board)
          [file-id page-id] [(uuid/next) (uuid/next)]
          before  @st/state
          checks  (atom 0)
          seen    (atom nil)
          pending (fn [_]
                    (let [n (swap! checks inc)]
                      (when (= n 2)
                        (swap! st/state assoc-in [:files file-id :data :pages-index page-id :objects tid :name]
                               "Edited"))
                      (if (< n 3) #{"face"} #{})))]
      (swap! st/state assoc
             :current-file-id file-id
             :current-page-id page-id
             :files {file-id {:id file-id :name "File"
                              :data {:pages-index {page-id {:id page-id :objects objects}}}}})
      (mock/with-mocks
        {dea/run-exports (fn [ctx _ _ _]
                           (reset! seen (:page ctx))
                           {:promise (js/Promise.resolve {:status :nothing}) :cancel (fn [])})}
        (fn [done']
          (st/emit! (dea/export-animation {:format :svg
                                           :boards [{:board-id bid :scale 1}]
                                           :bridge {:text-faces-pending pending
                                                    :relayout-text (fn [_])}}))
          (-> (thj/wait-until #(some? @seen))
              (.then (fn [_]
                       (t/is (= 3 @checks) "after the faces")
                       (t/is (= "Edited" (get-in @seen [:objects tid :name])) "the file as it is then")))
              (.catch (fn [cause] (t/is false (str "failed: " cause))))
              (.finally (fn []
                          (reset! st/state before)
                          (done')
                          nil))))
        done))))

(defn- words-use
  "A use of a style of `entry`."
  [entry]
  (cta/make-animation {:type :style :style-ref-id (uuid/next) :style-ref-file (uuid/next)
                       :name "Words" :start 0 :entries [entry]}))

(t/deftest an-svg-outlines-its-texts-on-request
  (let [{:keys [objects bid tid]} (text-board)
        red     [{:fill-color "#ff0000" :fill-opacity 1}]
        objects (assoc-in objects [tid :content :children 0 :children 0 :children 0 :fills] red)
        mixed   (update-in objects [tid :content :children 0 :children 0 :children]
                           conj {:text " and you" :fills [{:fill-color "#00ff00" :fill-opacity 1}]})
        tl      (cta/add-keyframe (cta/make-timeline {:board-id bid :duration 1000}) tid
                                  {:time 0 :property :x :value 10})
        content (path/from-plain [{:command :move-to :params {:x 20 :y 12}}
                                  {:command :line-to :params {:x 60 :y 12}}
                                  {:command :line-to :params {:x 60 :y 26}}
                                  {:command :close-path :params {}}])
        asked   (atom [])
        bridge  {:shape-to-path (fn [id] (swap! asked conj id) content)
                 :text-unit-outlines (fn [_] [{:unit nil :center {:x 0 :y 0}
                                               :runs [{:paragraph 0 :span 0 :content content}
                                                      {:paragraph 0 :span 1 :content content}]}])
                 :clean-modifiers #(swap! asked conj :clean)}
        [drawn view] (dea/outline-texts objects tl {} bridge)]
    (t/is (= [:clean tid] @asked) "at rest")
    (t/is (= [:path red] ((juxt :type :fills) (get drawn tid))) "a path in the paint of its text")
    (t/is (= (get-in objects [tid :selrect]) (get-in view [tid :selrect])) "its CSS on the box of the text")
    (let [drawn (first (dea/outline-texts mixed tl {} bridge))
          group (get drawn tid)]
      (t/is (= :group (:type group)) "a text in several fills, a group")
      (t/is (= [red [{:fill-color "#00ff00" :fill-opacity 1}]]
               (mapv #(:fills (get drawn %)) (:shapes group)))
            "of a path by span, in its fills"))
    (t/is (= objects (first (dea/outline-texts objects tl {tid :units} bridge)))
          "as one whose units play as pictures")
    (t/testing "its issues"
      (let [on      #(assoc % :export {:format :svg :outline true})
            stroked (-> tl
                        (cta/add-keyframe tid {:time 0 :property :stroke-width :index 0 :value 1})
                        (cta/add-keyframe tid {:time 1000 :property :stroke-width :index 0 :value 3}))
            trimmed (-> tl
                        (cta/add-keyframe tid {:time 0 :property :trim-end :value 0})
                        (cta/add-keyframe tid {:time 1000 :property :trim-end :value 1}))
            svg     #(set (get (dea/export-issues %1 %2) :svg))]
        (t/is (not (contains? (svg objects stroked) :svg/appearance)) "the strokes of a text play on its glyphs")
        (t/is (contains? (svg objects trimmed) :svg/appearance) "its trim stays, a text takes no length")
        (t/is (not (contains? (svg objects (on trimmed)) :svg/appearance)) "but of an outline")))))

(t/deftest an-svg-has-the-units-of-a-split-text-as-vectors
  (let [{:keys [objects bid tid]} (text-board)
        tl      (cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid
                                   {:type :fade :split :word :start 0 :duration 500})
        texts   (dea/unit-texts objects tl)
        content (path/from-plain [{:command :move-to :params {:x 20 :y 12}}
                                  {:command :line-to :params {:x 30 :y 12}}
                                  {:command :close-path :params {}}])
        shown   (atom [])
        bridge  {:apply-shape-properties (fn [shape _] (swap! shown conj (count (:text-units shape))))
                 :text-unit-outlines (fn [_] [{:unit 1 :center {:x 70 :y 20} :runs [{:paragraph 0 :span 0 :content content}]}
                                              {:unit nil :center {:x 0 :y 0} :runs [{:paragraph 0 :span 0 :content content}]}])}]
    (t/async
      done
      (-> (dea/collect-unit-vectors objects tl texts {:bridge bridge :yield #(js/Promise.resolve nil)})
          (.then (fn [images]
                   (t/is (= #{[tid 1] [tid :rest]} (set (keys images))))
                   (t/is (= [3 0] @shown) "its three words shown at rest, then set back")
                   (t/is (= {:x 70 :y 20 :width 0 :height 0} (dissoc (get images [tid 1]) :markup :box))
                         "the middle of its box, which it turns around")
                   (t/is (= {:x 20 :y 12 :width 10} (select-keys (get-in images [[tid 1] :box]) [:x :y :width]))
                         "and what its glyphs cover, where its blur plays")
                   (t/is (str/includes? (get-in images [[tid 1] :markup]) "<path"))))
          (.then #(dea/collect-unit-vectors objects tl texts {:bridge bridge :yield (fn [] (js/Promise.resolve nil))
                                                              :lottie? true}))
          (.then (fn [images]
                   (t/is (= {:x 70 :y 20} (get-in images [[tid 1] :center])) "for Lottie, on the page")
                   (t/is (= [content] (mapv :content (get-in images [[tid 1] :runs]))))
                   (t/is (some? (get-in images [[tid 1] :runs 0 :fills])) "in the fills of its span")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally #(done))))))

(defn- unit-vectors-outcome
  "How `dea/collect-unit-vectors` of `args` ends: `:done`, the type of
  what stopped it or the message of what failed. Outside the body of
  `t/async`, which compiles to an async function that awaits the promise
  of a `let`."
  [& args]
  (-> (apply dea/collect-unit-vectors args)
      (.then (constantly :done))
      (.catch (fn [cause] (or (:type (ex-data cause)) (ex-message cause))))))

(t/deftest the-units-of-split-texts-stop-at-a-cancel-with-the-text-at-rest
  ;; Cancelled between two texts, or failing in one, it leaves no text
  ;; with its units shown.
  (let [{:keys [objects bid tid]} (text-board)
        other   (uuid/next)
        objects (-> objects
                    (update-in [bid :shapes] conj other)
                    (assoc other (assoc (get objects tid) :id other)))
        split   {:type :fade :split :word :start 0 :duration 500}
        tl      (-> (cta/make-timeline {:board-id bid :duration 1000})
                    (cta/add-animation tid split)
                    (cta/add-animation other split))
        texts   (dea/unit-texts objects tl)
        shown   (atom [])
        cancel  (atom false)
        show    (fn [shape _] (swap! shown conj [(:id shape) (count (:text-units shape))]))
        ;; cancelled once the first text is done
        cancelling {:bridge {:apply-shape-properties show :text-unit-outlines (fn [_] [])}
                    :cancelled? #(deref cancel)
                    :yield (fn [] (reset! cancel true) (js/Promise.resolve nil))}
        failing {:bridge {:apply-shape-properties show
                          :text-unit-outlines (fn [_] (throw (js/Error. "no outlines")))}
                 :yield #(js/Promise.resolve nil)}]
    (t/async
      done
      (-> (unit-vectors-outcome objects tl texts cancelling)
          (.then (fn [result]
                   (let [[[first-id shown-units] [same-id rest-units] :as calls] @shown]
                     (t/is (= 2 (count texts)))
                     (t/is (= :export-cancelled result))
                     (t/is (= 2 (count calls)) "the next text never shown")
                     (t/is (= first-id same-id))
                     (t/is (= [3 0] [shown-units rest-units]) "the one done set back at rest"))
                   (reset! shown [])
                   (unit-vectors-outcome objects tl texts failing)))
          (.then (fn [result]
                   (t/is (= "no outlines" result) "a failure goes on")
                   (t/is (= 0 (second (peek @shown))) "the text set back at rest all the same")))
          (.catch (fn [cause] (t/is false (str "failed: " cause))))
          (.finally (fn [] (done) nil))))))

(t/deftest markup-formats-note-the-split-text-they-play-whole
  ;; they play the units of a split text, but of one that underlines or
  ;; strikes some of its text
  (let [{:keys [objects bid tid]} (text-board)
        with  #(cta/add-animation (cta/make-timeline {:board-id bid :duration 1000}) tid %)
        split {:type :fade :split :word :start 0 :duration 500}
        lined (assoc-in objects [tid :content :children 0 :children 0 :children 0 :text-decoration] "underline")
        split? (fn [objects tl]
                 (let [issues (dea/export-issues objects tl)]
                   (mapv #(contains? (set (get issues %)) :split-text) [:svg :lottie :dotlottie :gif])))]
    (t/is (= [false false false false] (split? objects (with split))))
    (t/is (= [true true true false] (split? lined (with split))))
    (t/is (= [true true true false] (split? lined (with (words-use split))))
          "a split entry of a style use too")
    (t/is (= [false false false false] (split? lined (with {:type :fade}))))
    (t/is (= {} (dea/export-issues objects nil)))))

(t/deftest a-markup-export-counts-its-pictures-and-split-texts
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
                  :text-unit-outlines (fn [_] [])}
        export   (fn [format]
                   (reset! progress [])
                   (:promise (dea/run-export {:page {:id (uuid/next) :objects objects :timelines {bid tl}}
                                              :files {}
                                              :board-id bid
                                              :bridge bridge
                                              :yield #(js/Promise.resolve nil)
                                              :on-progress #(swap! progress conj %)}
                                             {:format format})))
        counted  (fn [result total label]
                   (let [layers (filter #(= :layers (:phase %)) @progress)]
                     (t/is (= :ok (:status result)))
                     (t/is (= (count layers) (:total (first layers))) "every picture counted")
                     (t/is (= total (:total (first layers))) label)
                     (t/is (= :write (:phase (last @progress))))))]
    (t/async
      done
      (-> (export :lottie)
          (.then #(counted % 1 "the split text, outlined at once (the board is vectors)"))
          (.then #(export :svg))
          (.then #(counted % (+ 1 1) "the board, then the text, whose units are outlined at once"))
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
    (t/is (not-any? #(contains? (set %) :no-audio) (vals (dea/export-issues objects tl)))
          "no clips, nothing to say")
    (t/is (= #{:gif :avif :svg :lottie :dotlottie}
             (into #{} (keep (fn [[format kinds]] (when (some #{:no-audio} kinds) format)))
                   (dea/export-issues objects clips)))
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

(defn- group-scene
  "A board holding a group of a rect and `other` (at 40,10, 20 x 20),
  the group growing from 50 to 100 px wide, on a page of its own; the
  bridge to a fake WASM of `export-scene`."
  [other]
  (let [{:keys [bridge calls]} (export-scene 500)
        board (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 80
                                :fills [{:fill-color "#112233" :fill-opacity 1}]})
        rect  (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20
                                :fills [{:fill-color "#ff0000" :fill-opacity 1}]})
        other (cts/setup-shape (merge {:x 40 :y 10 :width 20 :height 20} other))
        gid   (uuid/next)
        group (cts/setup-shape {:id gid :type :group :x 10 :y 10 :width 50 :height 20
                                :shapes [(:id rect) (:id other)]})
        [bid rid oid] (map :id [board rect other])
        tl    (-> (cta/make-timeline {:board-id bid :duration 500})
                  (cta/add-keyframe gid {:time 0 :property :width :value 50})
                  (cta/add-keyframe gid {:time 500 :property :width :value 100}))]
    {:page {:id (uuid/next)
            :objects {bid (assoc board :shapes [gid])
                      gid (assoc group :parent-id bid :frame-id bid)
                      rid (assoc rect :parent-id gid :frame-id bid)
                      oid (assoc other :parent-id gid :frame-id bid)}
            :timelines {bid tl}}
     :bid bid :gid gid :rid rid :oid oid
     :calls calls
     :bridge bridge}))

(t/deftest a-lottie-export-resizes-a-group-on-its-layers
  ;; Its layers are pictured on their own, as the document draws them,
  ;; and resize; its layer scales not.
  (t/async
    done
    (let [scene    (group-scene {:type :image :metadata {:id (uuid/next) :width 20 :height 20 :mtype "image/png"}})
          progress (atom [])
          {:keys [promise]} (dea/run-export (export-ctx scene :on-progress #(swap! progress conj %))
                                            {:format :lottie})]
      (-> promise
          (.then (fn [result] (.then (.text (:blob result)) #(js->clj (js/JSON.parse %) :keywordize-keys true))))
          (.then (fn [doc]
                   (let [layers (into (vec (:layers doc)) (mapcat :layers) (:assets doc))
                         rcs    (->> layers
                                     (keep #(some (fn [item] (when (= "rc" (:ty item)) item))
                                                  (get-in % [:shapes 0 :it])))
                                     (filter #(= 1 (get-in % [:s :a]))))]
                     (t/is (some #{[:pixels (:oid scene)]} @(:calls scene)) "the image pictured on its own")
                     (t/is (not (some #{[:pixels (:gid scene)]} @(:calls scene))) "not the group, its layers baked in")
                     (t/is (= 1 (count rcs)) "the rect resizes")
                     (t/is (= 0 (get-in (d/seek #(= "Group" (:nm %)) layers) [:ks :s :a])) "the group scales not")
                     (t/is (= 1 (get-in (d/seek #(= 2 (:ty %)) layers) [:ks :s :a]))
                           "the image stretches on its own")
                     (t/is (= (count (filter #(= [:pixels] (take 1 %)) (filter vector? @(:calls scene))))
                              (:total (last (filter #(= :layers (:phase %)) @progress))))
                           "as many pictures as it counts"))))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (finish done))))))

(t/deftest an-svg-export-resizes-a-group-on-its-layers
  (t/async
    done
    (let [scene (group-scene {:type :rect :fills [{:fill-color "#00ff00" :fill-opacity 1}]})
          {:keys [promise]} (dea/run-export (export-ctx scene) {:format :svg})]
      (-> promise
          (.then (fn [result] (.text (:blob result))))
          (.then (fn [svg]
                   (t/is (not (re-find #"scale\(2, 1\)" svg)) "not a scale of the group")
                   (t/is (re-find #"width: 40px" svg) "its rects resize")))
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

;; THE ISSUES OF THE MARKUP EXPORTS

(defn- issues-of
  "The issue kinds of the export of the rect of `test-objects`, changed by
  `f`, animated by `kfs` (its x by default), in `format`."
  [format & {:keys [f kfs] :or {f identity}}]
  (let [{:keys [objects bid cid]} (test-objects)
        objects (update objects cid f)
        tl      (reduce #(cta/add-keyframe %1 cid %2)
                        (cta/make-timeline {:board-id bid :duration 1000})
                        (or kfs [{:time 0 :property :x :value 10} {:time 1000 :property :x :value 50}]))]
    (get (dea/export-issues objects tl) format [])))

(def ^:private shadow
  {:id (uuid/next) :style :drop-shadow :offset-x 4 :offset-y 4 :blur 8 :spread 0 :hidden false
   :color {:color "#000000" :opacity 0.3}})

(t/deftest an-svg-has-its-layers-as-vectors
  (t/is (= [] (issues-of :svg)) "a move plays")
  (t/is (= [] (issues-of :lottie)) "a plain rect is a vector")
  (t/is (= [] (issues-of :mp4)) "videos play as the canvas"))

(t/deftest markup-exports-resize-a-group-on-its-layers
  ;; Not a stretch: its layers resize as on the canvas (see
  ;; `aexp/expand-group-sizes`), in SVG and Lottie.
  (let [{:keys [objects bid cid]} (test-objects)
        gid     (uuid/next)
        objects (-> objects
                    (assoc gid (cts/setup-shape {:id gid :type :group :x 10 :y 10 :width 20 :height 20
                                                 :shapes [cid] :parent-id bid :frame-id bid}))
                    (assoc-in [bid :shapes] [gid])
                    (assoc-in [cid :parent-id] gid))
        tl      (-> (cta/make-timeline {:board-id bid :duration 1000})
                    (cta/add-keyframe gid {:time 0 :property :width :value 20})
                    (cta/add-keyframe gid {:time 1000 :property :width :value 40}))
        issues  (dea/export-issues objects tl)]
    (t/is (= [] (get issues :svg [])))
    (t/is (not-any? #{:lottie/size} (get issues :lottie [])))))

(t/deftest what-svg-keeps-as-it-rests
  (t/is (= []
           (issues-of :svg :kfs [{:time 0 :property :fill-color :value "#ff0000"}
                                 {:time 1000 :property :fill-color :value "#00ff00"}]))
        "a fill plays on its vectors")
  (t/is (= []
           (issues-of :svg :f #(assoc % :shadow [shadow (assoc shadow :style :inner-shadow)])
                      :kfs [{:time 0 :property :shadow-spread :index 1 :value 0}
                            {:time 1000 :property :shadow-spread :index 1 :value 8}]))
        "SMIL plays the spread of shadows, inner ones too")
  (t/is (= []
           (issues-of :svg :f #(assoc % :r1 4 :r2 4 :r3 0 :r4 0)
                      :kfs [{:time 0 :property :r1 :value 4} {:time 1000 :property :r1 :value 12}]))
        "and uneven corners")
  (t/is (= []
           (issues-of :svg :kfs [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}
                                 {:time 0 :property :blur :value 0} {:time 1000 :property :blur :value 4}]))
        "a rect changes its size, and blurs as the canvas")
  (t/is (= []
           (issues-of :svg :f #(assoc % :rotation 30)
                      :kfs [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}]))
        "a turned one resizes on its outline (see `aexp/svg-outline-sized?`)")
  (t/is (= []
           (issues-of :svg :f #(assoc % :rotation 30 :type :circle)
                      :kfs [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}]))
        "a turned ellipse resizes on its element (see `aexp/svg-box-sized?`)")
  (t/is (= []
           (issues-of :svg :f #(assoc % :fills [{:fill-color "#ffffff" :fill-opacity 0.5} {:fill-color "#000000" :fill-opacity 1}])
                      :kfs [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}]))
        "and two fills, laid over its box as it plays")
  (t/is (= [:svg/size]
           (issues-of :svg :f #(assoc % :fills [{:fill-image {:id (uuid/next) :width 10 :height 10 :mtype "image/png"}}])
                      :kfs [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}]))
        "an image stretches, which keeps the size it rests at")
  (t/is (= [:lottie/blur]
           (issues-of :lottie :kfs [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}
                                    {:time 0 :property :blur :value 0} {:time 1000 :property :blur :value 4}]))
        "in Lottie too")
  (t/is (= [:lottie/size]
           (issues-of :lottie :f #(assoc % :rotation 30)
                      :kfs [{:time 0 :property :width :value 20} {:time 1000 :property :width :value 40}]))
        "and a turned one stretches there too"))

(t/deftest the-issues-of-copies-masks-and-transparency
  (let [{:keys [objects bid cid]} (test-objects)
        tl      (-> (cta/make-timeline {:board-id bid :duration 1000 :playback :loop})
                    (cta/add-keyframe cid {:time 0 :property :x :value 10})
                    (cta/add-keyframe cid {:time 1000 :property :x :value 50}))
        issues  #(dea/export-issues %1 %2)
        has?    (fn [issues format kind] (boolean (some #{kind} (get issues format))))]
    (t/testing "a copy looping on its own clock"
      (t/is (has? (issues objects (assoc tl :copy-loops [700])) :svg :copy-clock)
            "starts over with the board, its loops not fitting")
      (t/is (has? (issues objects (assoc tl :copy-loops [700])) :gif :copy-clock))
      (t/is (not (has? (issues objects (assoc tl :copy-loops [700])) :mp4 :copy-clock)) "a video ends")
      (t/is (not (has? (issues objects (assoc tl :copy-loops [500])) :svg :copy-clock)) "they fit")
      (t/is (has? (issues objects (assoc tl :copy-loops [500] :playback :once)) :lottie :copy-clock)
            "it stops at the end of a board that plays once"))
    (t/testing "a layer in a masked group"
      (let [mask    (cts/setup-shape {:type :rect :x 0 :y 0 :width 30 :height 30})
            group   (cts/setup-shape {:type :group :x 0 :y 0 :width 30 :height 30})
            [mid gid] (map :id [mask group])
            masked  (-> objects
                        (assoc-in [bid :shapes] [gid])
                        (assoc gid (assoc group :parent-id bid :frame-id bid :shapes [mid cid] :masked-group true))
                        (assoc mid (assoc mask :parent-id gid :frame-id bid))
                        (assoc-in [cid :parent-id] gid))]
        (t/is (not-any? #(has? (issues masked tl) % :mask) [:svg :lottie :webm])
              "an SVG and a Lottie keep it to its mask, a video draws as the canvas")
        (t/is (not (has? (issues masked tl) :lottie :lottie/images)) "as vectors")
        (t/is (not (has? (issues masked tl) :lottie :lottie/matte)))
        (doseq [[mode matte?] [[:alpha-inverted false] [:luma false] [:luma-inverted true]]]
          (t/is (= matte? (has? (issues (assoc-in masked [gid :mask-mode] mode) tl) :lottie :lottie/matte))
                (str "lottie-web plays a mask by " mode)))
        (t/is (has? (issues (assoc-in masked [gid :mask-mode] :luma-inverted) tl) :dotlottie :lottie/matte))
        (t/is (not (has? (issues (assoc-in masked [gid :mask-mode] :luma-inverted) tl) :svg :lottie/matte)))))
    (t/testing "a board that shows through"
      (let [clear (update objects bid assoc :fills [])]
        (t/is (has? (issues clear tl) :gif :gif/alpha))
        (t/is (has? (issues clear tl) :mp4 :mp4/alpha))
        (t/is (not (has? (issues clear tl) :webm :mp4/alpha)) "WebM keeps it")
        (t/is (has? (issues (update objects bid assoc :hide-fill-on-export true) tl) :mp4 :mp4/alpha))
        (t/is (not (has? (issues objects tl) :gif :gif/alpha)) "an opaque fill")
        (t/is (has? (issues (assoc-in objects [bid :fills 0 :fill-opacity] 0.5) tl) :gif :gif/alpha)
              "a fill one sees through")))))

(t/deftest lottie-notes-its-effects
  (t/is (= [:lottie/shadow] (issues-of :lottie :f #(assoc % :shadow [shadow]))))
  (t/is (= [] (issues-of :lottie :f #(assoc % :shadow [(assoc shadow :hidden true)]))) "a hidden one draws nothing")
  (t/is (= [:lottie/shadow] (issues-of :dotlottie :f #(assoc % :shadow [(assoc shadow :style :inner-shadow)])))
        "an inner shadow casts as layers, no image")
  (t/is (= [:lottie/blur] (issues-of :lottie :f #(assoc % :blur {:id (uuid/next) :type :layer-blur :value 4 :hidden false}))))
  (t/is (= [:lottie/stroke]
           (issues-of :lottie :f #(assoc % :strokes [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 2
                                                      :stroke-style :solid :stroke-alignment :inner}])))
        "an inner stroke")
  (t/is (= []
           (issues-of :lottie :f #(assoc % :strokes [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 2
                                                      :stroke-style :solid :stroke-alignment :center}])))
        "a centered one is as Lottie draws it"))

(t/deftest no-markup-format-blends-or-blurs-the-background
  (doseq [format [:svg :lottie :dotlottie]]
    (t/is (some #{:blend-mode} (issues-of format :f #(assoc % :blend-mode :multiply))) (name format))
    (t/is (some #{:background-blur}
                (issues-of format :f #(assoc % :background-blur {:id (uuid/next) :type :background-blur
                                                                 :value 6 :hidden false})))
          (name format)))
  (t/is (= [] (issues-of :gif :f #(assoc % :blend-mode :multiply))) "a GIF plays it"))
