;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.util.audio-test
  (:require
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.uuid :as uuid]
   [app.util.audio :as audio]
   [cljs.test :as t :include-macros true]
   [promesa.core :as p]))

(defn- fake-offline-context
  "A class like `OfflineAudioContext` that writes down in `calls` how it
  is made and what plays in it (the rate of a source when it is not 1),
  and renders `rendered`."
  [calls rendered]
  (fn [channels frames rate]
    (swap! calls conj [:context channels frames rate])
    (let [destination #js {:name "destination"}]
      #js {:destination destination
           :createBufferSource
           (fn []
             (let [source #js {:playbackRate #js {:value 1}}]
               (set! (.-connect source) (fn [_] (swap! calls conj [:connect (.-buffer source)])))
               (set! (.-start source) (fn [at offset length]
                                        (let [rate (.. source -playbackRate -value)]
                                          (when (not= 1 rate)
                                            (swap! calls conj [:rate (.-buffer source) rate])))
                                        (swap! calls conj [:start (.-buffer source) at offset length])))
               source))
           :createGain
           (fn []
             #js {:gain #js {:setValueAtTime
                             (fn [value time] (swap! calls conj [:set value time]))
                             :linearRampToValueAtTime
                             (fn [value time] (swap! calls conj [:ramp value time]))}
                  :connect (fn [node]
                             (swap! calls conj [:to (if (= node destination) :destination node)]))})
           :decodeAudioData
           (fn [bytes]
             (swap! calls conj [:decode bytes])
             (js/Promise.resolve rendered))
           :startRendering
           (fn []
             (swap! calls conj [:render])
             (js/Promise.resolve rendered))})))

(defn- fail
  [done]
  (fn [cause]
    (t/is (nil? cause) (str cause))
    (done)))

(t/deftest the-mix-plays-each-part-where-the-plan-puts-it
  (t/async
    done
    (let [calls    (atom [])
          rendered #js {:name "mix"}
          plan     [{:clip-id :a :at 200 :offset 0 :length 300 :points [[0 0.0] [100 0.5] [300 0.5]]}
                    {:clip-id :b :at 0 :offset 1500 :length 1000 :points [[0 1.0] [1000 1.0]]}]]
      (-> (audio/mix! plan 2033.3333333333333
                      {:load #(p/resolved (name (:clip-id %)))
                       :offline-context (fake-offline-context calls rendered)})
          (p/then
           (fn [result]
             (t/is (identical? rendered result))
             (t/is (= [:context 2 97600 48000] (first @calls))
                   "stereo at 48 kHz, as long as the frames: 61 at 30 fps")
             (t/is (= [[:set 0.0 0.2] [:ramp 0.5 0.3] [:ramp 0.5 0.5]
                       [:connect "a"] [:to :destination] [:start "a" 0.2 0 0.3]
                       [:set 1.0 0] [:ramp 1.0 1]
                       [:connect "b"] [:to :destination] [:start "b" 0 1.5 1]
                       [:render]]
                      (rest @calls)))
             (done)))
          (p/catch (fail done))))))

(t/deftest a-fast-part-reads-its-file-faster
  (t/async
    done
    (let [calls (atom [])
          plan  [{:clip-id :a :at 0 :offset 500 :length 1000 :speed 2 :points [[0 1.0] [1000 1.0]]}]]
      (-> (audio/mix! plan 1000 {:load #(p/resolved (name (:clip-id %)))
                                 :offline-context (fake-offline-context calls #js {})})
          (p/then
           (fn [_]
             (t/is (= [[:rate "a" 2] [:start "a" 0 0.5 2]]
                      (filterv #(contains? #{:rate :start} (first %)) @calls))
                   "a second of the mix plays two of the file, from 0.5 s")
             (done)))
          (p/catch (fail done))))))

(t/deftest a-part-whose-file-does-not-load-is-left-out
  (t/async
    done
    (let [calls (atom [])
          plan  [{:clip-id :a :at 0 :offset 0 :length 100 :points [[0 1.0] [100 1.0]]}
                 {:clip-id :b :at 50 :offset 0 :length 100 :points [[0 1.0] [100 1.0]]}]]
      (-> (audio/mix! plan 1000
                      {:load #(if (= :a (:clip-id %))
                                (p/rejected (ex-info "gone" {}))
                                (p/resolved "b"))
                       :offline-context (fake-offline-context calls #js {})})
          (p/then
           (fn [_]
             (t/is (= [[:start "b" 0.05 0 0.1]] (filterv #(= :start (first %)) @calls)))
             (t/is (= [:render] (last @calls)))
             (done)))
          (p/catch (fail done))))))

(t/deftest decoding-takes-no-audio-context
  (t/async
    done
    (let [calls (atom [])
          bytes (js/ArrayBuffer. 4)]
      (-> (audio/decode bytes {:offline-context (fake-offline-context calls #js {:name "decoded"})})
          (p/then
           (fn [buffer]
             (t/is (= "decoded" (.-name buffer)))
             (t/is (= [[:context 2 1 48000] [:decode bytes]] @calls))
             (done)))
          (p/catch (fail done))))))

(defn- clip
  [& {:as params}]
  (ctau/make-audio-clip (merge {:audio-id (uuid/next) :source-duration 4000 :duration 1000} params)))

(t/deftest a-file-too-long-is-not-fetched
  (t/async
    done
    (let [fetched (atom 0)
          long    (assoc (clip) :source-duration (inc ctau/max-audio-duration))]
      (t/is (audio/too-long? long))
      (t/is (not (audio/too-long? (clip))))
      (-> (audio/load! long {:fetch (fn [_] (swap! fetched inc) (p/resolved (js/ArrayBuffer. 4)))
                             :decode p/resolved})
          (p/then (fn [_]
                    (t/is false "it should not load")
                    (done)))
          (p/catch
           (fn [cause]
             (t/is (= :audio-too-long (:type (ex-data cause))))
             (t/is (= 0 @fetched))
             (done)))))))

(t/deftest a-file-loads-once-for-all-its-clips
  (t/async
    done
    (let [fetched (atom 0)
          a       (clip)
          b       (clip :audio-id (:audio-id a) :start 2000)
          opts    {:fetch (fn [_] (swap! fetched inc) (p/resolved (js/ArrayBuffer. 4)))
                   :decode (fn [_] (p/resolved #js {:name "decoded"}))}]
      (-> (p/all [(audio/load! a opts) (audio/load! b opts)])
          (p/then
           (fn [[x y]]
             (t/is (identical? x y))
             (t/is (= 1 @fetched))
             (done)))
          (p/catch (fail done))))))

(t/deftest a-failed-load-is-tried-again
  (t/async
    done
    (let [fetched (atom 0)
          a       (clip)
          opts    {:fetch (fn [_]
                            (if (= 1 (swap! fetched inc))
                              (p/rejected (ex-info "offline" {}))
                              (p/resolved (js/ArrayBuffer. 4))))
                   :decode (fn [_] (p/resolved #js {:name "decoded"}))}]
      (-> (audio/load! a opts)
          (p/catch (fn [_] :failed))
          (p/then (fn [first-try]
                    (t/is (= :failed first-try))
                    (audio/load! a opts)))
          (p/then
           (fn [buffer]
             (t/is (= "decoded" (.-name buffer)))
             (t/is (= 2 @fetched))
             (done)))
          (p/catch (fail done))))))

(defn- channel-buffer
  "What has the channels of an `AudioBuffer` of `samples` at `rate`."
  [rate samples]
  (let [data (js/Float32Array.from (clj->js samples))]
    #js {:sampleRate rate
         :length (.-length data)
         :numberOfChannels 1
         :getChannelData (fn [_] data)}))

(t/deftest peaks-hold-the-min-and-max-of-each-bucket
  (let [;; at 1 kHz, 20 ms of a sine of 100 Hz: 5 ms is half a period
        sine   (map #(js/Math.sin (* 2 js/Math.PI (/ % 10))) (range 20))
        result (audio/peaks (channel-buffer 1000 sine) 5)
        round  #(/ (js/Math.round (* 1000 %)) 1000)]
    (t/is (= 5 (:bucket-ms result)))
    (t/is (= [[0 0.951 -0.951 0 0 0.951 -0.951 0]]
             (mapv #(mapv round (array-seq %)) (:channels result))))
    (t/is (= 0.951 (round (:peak result))) "the loudest"))
  (t/testing "nothing of an empty buffer"
    (t/is (= [[]] (mapv #(vec (array-seq %)) (:channels (audio/peaks (channel-buffer 48000 []) 5))))))
  (t/testing "a bucket at the end shorter than the others"
    (t/is (= [[0.5 1]] (mapv #(vec (array-seq %)) (:channels (audio/peaks (channel-buffer 1000 [0.5 1]) 5)))))))

;; LIVE PLAYBACK

(defn- live-context
  "A fake running `AudioContext` whose current time (s) is `clock`, an
  atom: the sources started and stopped in it go to `log`, and the gains
  set (`:set`, `:ramp`), which take no time before 0 (as browsers)."
  [clock log & {:keys [output-latency] :or {output-latency 0}}]
  (let [ctx #js {:state "running"
                 :outputLatency output-latency
                 :baseLatency 0
                 :destination #js {}
                 :createBufferSource
                 (fn []
                   (let [source #js {:connect (fn [_]) :disconnect (fn []) :playbackRate #js {:value 1}}]
                     (set! (.-start source) (fn [at offset length]
                                              (let [rate (.. source -playbackRate -value)]
                                                (when (not= 1 rate)
                                                  (swap! log conj [:rate (.-name (.-buffer source)) rate])))
                                              (swap! log conj [:start (.-name (.-buffer source)) at offset length])))
                     (set! (.-stop source) (fn [] (swap! log conj [:stop (.-name (.-buffer source))])))
                     source))
                 :createGain
                 (fn []
                   (let [call (fn [kind]
                                (fn [value time]
                                  (when (neg? time)
                                    (throw (js/RangeError. "The time must be non-negative")))
                                  (swap! log conj [kind value time])))]
                     #js {:gain #js {:setValueAtTime (call :set) :linearRampToValueAtTime (call :ramp)}
                          :connect (fn [_])
                          :disconnect (fn [])}))}]
    (js/Object.defineProperty ctx "currentTime" #js {:get (fn [] @clock)})
    ctx))

(defn- decoded
  "A decoded file `name` of `ms`, kept as `audio/load!` keeps it."
  [name ms]
  (let [audio-id (uuid/next)
        n        (js/Math.round (* 48 ms))
        data     (js/Float32Array. n)]
    (audio/remember! audio-id #js {:name name :duration (/ ms 1000) :length n :sampleRate 48000
                                   :numberOfChannels 1 :getChannelData (fn [_] data)})
    audio-id))

(defn- with-clip
  [timeline audio-id & {:as params}]
  (ctau/add-audio-clip timeline (merge {:audio-id audio-id :name "x" :source-duration 4000
                                        :start 0 :offset 0 :duration 2000}
                                       params)))

(defn- starts [log] (filterv #(= :start (first %)) @log))
(defn- gains [log] (filterv #(contains? #{:set :ramp} (first %)) @log))
(defn- stops [log] (filterv #(= :stop (first %)) @log))

(t/deftest playback-from-within-a-clip-starts-it-part-way
  (let [clock (atom 10.0)
        log   (atom [])
        song  (decoded "song" 4000)
        tl    (with-clip (cta/make-timeline {:board-id (uuid/next) :duration 3000}) song)]
    (audio/use-context! (live-context clock log))
    (audio/follow! tl 500 1 true)
    (t/is (= [[:start "song" 10 0.5 1.5]] (starts log)) "one source, 500 ms into the file")
    (audio/follow! tl 500 1 false)
    (t/is (= [[:stop "song"]] (stops log)) "paused, silent")
    (audio/use-context! nil)))

(t/deftest playback-as-the-context-starts-is-on-its-clock
  (let [clock (atom 0.0)
        log   (atom [])
        song  (decoded "song" 4000)
        tl    (with-clip (cta/make-timeline {:board-id (uuid/next) :duration 3000}) song :fade-in 1000)
        ms    #(js/Math.round (* 1000 %))]
    ;; heard 5 ms after the context plays it: at -5 ms of its time
    (audio/use-context! (live-context clock log :output-latency 0.005))
    (audio/follow! tl 0 1 true)
    (t/is (= [[:start "song" 0 5 1995]]
             (mapv (fn [[k n at offset length]] [k n (ms at) (ms offset) (ms length)]) (starts log)))
          "from now, 5 ms into it")
    (t/is (= [[:set 5 0] [:ramp 1000 995] [:ramp 1000 1995]]
             (mapv (fn [[k value at]] [k (ms value) (ms at)]) (gains log)))
          "its fade from there")
    (audio/stop-all!)
    (audio/use-context! nil)))

(t/deftest a-fast-clip-plays-from-twice-as-far-in
  (let [clock (atom 10.0)
        log   (atom [])
        song  (decoded "song" 4000)
        tl    (with-clip (cta/make-timeline {:board-id (uuid/next) :duration 3000}) song :duration 1000 :speed 2)]
    (audio/use-context! (live-context clock log))
    (audio/follow! tl 500 1 true)
    (t/is (= [[:rate "song" 2] [:start "song" 10 1 1]]
             (filterv #(contains? #{:rate :start} (first %)) @log))
          "half a second into it: a second into the file, the second left of it")
    (audio/stop-all!)
    (audio/use-context! nil)))

(t/deftest steady-playback-only-schedules-more
  (let [clock (atom 10.0)
        log   (atom [])
        beat  (decoded "beat" 4000)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback :loop})
                  (with-clip beat :duration 500))
        play  (fn [s time] (reset! clock s) (audio/follow! tl time 1 true))]
    (audio/use-context! (live-context clock log))
    (play 10.0 0)
    (t/is (= 4 (count (starts log))) "four loops ahead")
    (play 10.5 500)
    (play 11.2 200)
    (t/is (= [4 0] [(count (starts log)) (count (stops log))]) "the loop wraps as scheduled")
    (play 12.6 600)
    (t/is (= [8 0] [(count (starts log)) (count (stops log))]) "the next ones, once these run low")
    (t/testing "a seek starts it again"
      (reset! log [])
      (play 12.7 100)
      (t/is (= 5 (count (stops log))) "the ones not heard to their end yet")
      (t/is (= [:start "beat" 12.7 0.1 0.4] (first (starts log)))))
    (t/testing "an edit of the clips starts it again"
      (reset! log [])
      (audio/follow! (with-clip tl beat :start 300) 100 1 true)
      (t/is (seq (stops log)))
      (t/is (seq (starts log))))
    (audio/stop-all!)
    (audio/use-context! nil)))

(t/deftest the-way-back-of-ping-pong-is-heard-as-silence
  (let [clock (atom 10.0)
        log   (atom [])
        beat  (decoded "beat" 4000)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback :ping-pong})
                  (with-clip beat :start 800 :duration 400))]
    (audio/use-context! (live-context clock log))
    (audio/follow! tl 900 1 true)
    (t/is (= [[:start "beat" 10 0.1 0.1] [:start "beat" 11.9 0 0.2] [:start "beat" 13.9 0 0.1]]
             (starts log))
          "to the end of the timeline, and on each way there after")
    (reset! log [])
    (reset! clock 10.3)
    (audio/follow! tl 800 -1 true)
    (t/is (empty? @log) "the turn as scheduled")
    (reset! clock 10.4)
    (audio/follow! tl 700 1 true)
    (t/is (seq (stops log)) "a change of direction starts it again")
    (audio/stop-all!)
    (audio/use-context! nil)))

(t/deftest the-audible-time-lags-behind-the-output-latency
  (let [ctx (live-context (atom 10.0) (atom []) :output-latency 0.2)]
    (audio/use-context! ctx)
    (t/is (= 9800 (js/Math.round (audio/now-ms))))
    (audio/use-context! nil)
    (t/is (nil? (audio/now-ms)) "without a context, none")))

;; THE DECODED FILES KEPT

(defn- mono
  "What decoding `ms` of mono audio at 48 kHz gives."
  [ms]
  (let [data (js/Float32Array. (* 48 ms))]
    #js {:sampleRate 48000 :length (.-length data) :numberOfChannels 1 :getChannelData (fn [_] data)}))

(t/deftest the-files-kept-take-at-most-the-cap
  (audio/forget-all!)
  (with-redefs [audio/max-cache-bytes (audio/buffer-bytes (mono 3000))]
    (let [[a b c d e] (repeatedly 5 uuid/next)]
      (audio/remember! a (mono 1000))
      (audio/remember! b (mono 1000))
      (audio/remember! c (mono 1000))
      (t/is (every? audio/decoded? [a b c]) "as many as fit")
      (audio/load! (clip :audio-id a))
      (audio/remember! d (mono 1500))
      (t/is (= [true false false true] (mapv audio/decoded? [a b c d]))
            "the ones used longest ago go first")
      (t/is (<= (audio/cached-bytes) audio/max-cache-bytes))
      (t/is (some? (audio/waveform b)) "their waveforms stay")
      (audio/remember! e (mono 4000))
      (t/is (not (audio/decoded? e)) "one larger than all of it is not kept")
      (t/is (= :too-much (audio/failure e)))
      (t/is (= [true true] (mapv audio/decoded? [a d])) "and nothing goes for it")))
  (audio/forget-all!))

(t/deftest the-files-playing-stay
  (audio/forget-all!)
  (let [clock (atom 10.0)
        log   (atom [])
        [song other] (repeatedly 2 uuid/next)
        tl    (with-clip (cta/make-timeline {:board-id (uuid/next) :duration 3000}) song)]
    (with-redefs [audio/max-cache-bytes (audio/buffer-bytes (mono 2000))]
      (audio/remember! song (mono 1500))
      (audio/use-context! (live-context clock log))
      (audio/follow! tl 0 1 true)
      (audio/remember! other (mono 1000))
      (t/is (audio/decoded? song) "playing, it stays")
      (t/is (= :too-much (audio/failure other)) "no room for another one")
      (audio/stop-all!)
      (audio/remember! other (mono 1000))
      (t/is (= [false true] (mapv audio/decoded? [song other])) "stopped, it goes first"))
    (audio/use-context! nil)
    (audio/forget-all!)))

(t/deftest a-file-that-failed-is-not-tried-each-frame
  (t/async
    done
    (audio/forget-all!)
    (let [fetched (atom 0)
          [song lost] (repeatedly 2 uuid/next)
          tl      (-> (cta/make-timeline {:board-id (uuid/next) :duration 3000})
                      (with-clip song)
                      (with-clip lost :start 2000 :duration 1000))
          opts    {:fetch (fn [audio-id]
                            (swap! fetched inc)
                            (if (= audio-id lost)
                              (p/rejected (js/TypeError. "Failed to fetch"))
                              (p/resolved (js/ArrayBuffer. 4))))
                   :decode (fn [_] (p/rejected (js/Error. "EncodingError")))}]
      (audio/load-clips! tl opts)
      (-> (p/delay 10)
          (p/then (fn []
                    (t/is (= [:undecodable :not-loaded] (mapv audio/failure [song lost])))
                    (audio/load-clips! tl opts)
                    (t/is (= 2 @fetched) "not again so soon")
                    (audio/forget-all!)
                    (t/is (nil? (audio/failure song)) "forgotten with the rest")
                    (done)))
          (p/catch (fail done))))))

(t/deftest what-loads-while-all-is-forgotten-is-not-kept
  (t/async
    done
    (audio/forget-all!)
    (let [song    (uuid/next)
          release (atom nil)
          loading (audio/load! (clip :audio-id song)
                               {:fetch (fn [_] (p/create (fn [resolve _] (reset! release resolve))))
                                :decode (fn [_] (p/resolved (mono 100)))})]
      (audio/forget-all!)
      (@release (js/ArrayBuffer. 4))
      (-> loading
          (p/then (fn [buffer]
                    (t/is (some? buffer) "who asked for it gets it")
                    (t/is (not (audio/decoded? song)) "but it is not kept")
                    (done)))
          (p/catch (fail done))))))
