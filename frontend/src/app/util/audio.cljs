;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.util.audio
  "Sound in the browser for Penpot Motion: the files of the audio clips,
  decoded once and kept by `:audio-id`, and the offline mix of an audio
  plan (see `app.common.types.animation/audio-plan`), which the video
  exports carry."
  (:require
   [app.common.logging :as log]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.config :as cf]
   [promesa.core :as p]))

(log/set-level! :warn)

(def sample-rate
  "The rate, in Hz, the files of the clips are decoded and mixed at: the
  one of video."
  48000)

(def ^:private channels
  "Mixes are stereo; a mono file plays on both sides."
  2)

(defn- offline-context-class
  []
  (or (unchecked-get js/globalThis "OfflineAudioContext")
      (unchecked-get js/globalThis "webkitOfflineAudioContext")))

(defn- no-web-audio
  []
  (p/rejected (ex-info "no web audio in this browser" {:type :audio-unavailable})))

(defn decode
  "A promise of the `AudioBuffer` of the encoded audio `bytes`, an
  `ArrayBuffer` it takes over, at `sample-rate`. It rejects what the
  browser cannot play. Decoding needs no `AudioContext`, so no user
  gesture."
  ([bytes]
   (decode bytes nil))
  ([bytes {:keys [offline-context]}]
   (if-let [ctor (or offline-context (offline-context-class))]
     (.decodeAudioData ^js (new ctor channels 1 sample-rate) bytes)
     (no-web-audio))))

(defonce ^:private loaded
  ;; `:audio-id` -> the promise of its `AudioBuffer`
  (atom {}))

(defonce ^:private buffers
  ;; `:audio-id` -> its `AudioBuffer`, once decoded
  (atom {}))

(defonce ^:private waveforms
  ;; `:audio-id` -> its `peaks`
  (atom {}))

(defonce ^:private last-used
  ;; `:audio-id` -> when its `AudioBuffer` was last kept or asked for, a
  ;; count of all those uses
  (atom {}))

(defonce ^:private uses
  (atom 0))

(defonce ^:private failures
  ;; `:audio-id` -> `{:reason :at}`, why its file did not load and when
  ;; (see `failure`)
  (atom {}))

(defonce ^:private generation
  ;; Bumped by `forget-all!`: a file it forgets while it loads is not
  ;; kept once decoded.
  (atom 0))

(defonce version
  ;; Bumped each time a file is decoded or fails to, so the clips drawn
  ;; of it draw again.
  (atom 0))

(def max-cache-bytes
  "The most memory, in bytes, the decoded files kept may take: 512 MiB,
  about four files of 5 minutes (see `ctau/max-audio-duration`) in
  stereo. Past it the files not playing go, the one used longest ago
  first, and a file that still does not fit is not kept: its clips stay
  silent (see `failure`)."
  (* 512 1024 1024))

(def ^:private retry-ms
  "How long, in ms, playback leaves a file that failed to load before it
  tries again (see `load-clips!`)."
  10000)

(def peak-ms
  "The span, in ms, of each min and max of a waveform (see `peaks`)."
  5)

(defn peaks
  "The waveform of `buffer` (an `AudioBuffer`, or what has its
  `numberOfChannels`, `sampleRate`, `length` and `getChannelData`): for
  each channel, the min and the max of each `bucket-ms` of it, one after
  the other in a `Float32Array`, and the `:peak`, the loudest of them."
  [^js buffer bucket-ms]
  (let [rate     (.-sampleRate buffer)
        length   (.-length buffer)
        size     (max 1 (js/Math.round (* rate (/ bucket-ms 1000))))
        buckets  (js/Math.ceil (/ length size))
        channel  (fn [i]
                   (let [^js data (.getChannelData buffer i)
                         out      (js/Float32Array. (* 2 buckets))]
                     (dotimes [b buckets]
                       (let [from (* b size)
                             to   (min length (+ from size))]
                         (loop [j from lo 1.0 hi -1.0]
                           (if (< j to)
                             (let [v (aget data j)]
                               (recur (inc j) (min lo v) (max hi v)))
                             (do (aset out (* 2 b) lo)
                                 (aset out (inc (* 2 b)) hi))))))
                     out))
        channels (mapv channel (range (.-numberOfChannels buffer)))]
    {:bucket-ms bucket-ms
     :channels channels
     :peak (reduce (fn [peak ^js data]
                     (.reduce data (fn [peak v] (max peak (js/Math.abs v))) peak))
                   0
                   channels)}))

(defn waveform
  "The `peaks` of the file `audio-id`, once it is decoded, else nil."
  [audio-id]
  (get @waveforms audio-id))

(defn buffer-bytes
  "The memory, in bytes, the decoded `buffer` takes: a 32-bit float for
  each sample of each channel."
  [^js buffer]
  (* 4 (or (.-length buffer) 0) (or (.-numberOfChannels buffer) 1)))

(defn cached-bytes
  "The memory, in bytes, the decoded files kept take."
  []
  (reduce + 0 (map buffer-bytes (vals @buffers))))

(defn decoded?
  "Whether the decoded file `audio-id` is kept, ready to play."
  [audio-id]
  (contains? @buffers audio-id))

(defn failure
  "Why the file `audio-id` does not play, when its last load failed:
  `:not-loaded` (it could not be fetched), `:undecodable` (this browser
  cannot decode it), `:unavailable` (no Web Audio here) or `:too-much`
  (no room for it, see `max-cache-bytes`); else nil."
  [audio-id]
  (:reason (get @failures audio-id)))

(defn- touch!
  [audio-id]
  (swap! last-used assoc audio-id (swap! uses inc)))

(defn- failed!
  [audio-id reason]
  (swap! failures assoc audio-id {:reason reason :at (js/Date.now)})
  (swap! version inc))

(declare playing-ids)

(defn- make-room!
  "Let go of decoded files, the one used longest ago first, but those of
  `keep-ids`, until `size` more bytes fit in `max-cache-bytes`. Their
  waveforms stay: they load again when they play."
  [size keep-ids]
  (loop []
    (when (> (+ (cached-bytes) size) max-cache-bytes)
      (when-let [audio-id (->> @last-used
                               (filter (fn [[id _]] (and (decoded? id) (not (contains? keep-ids id)))))
                               (sort-by val)
                               (ffirst))]
        (swap! buffers dissoc audio-id)
        (swap! loaded dissoc audio-id)
        (swap! last-used dissoc audio-id)
        (recur)))))

(defn- decoded!
  "Keep `buffer`, the decoded file `audio-id`, and its waveform, making
  room for it (see `make-room!`) but among the files playing. When it
  does not fit even so, it is not kept, nothing goes for it, and it
  fails as `:too-much`. `buffer` either way."
  [audio-id ^js buffer]
  (swap! buffers dissoc audio-id)
  (let [size     (buffer-bytes buffer)
        keep-ids (playing-ids)
        playing  (reduce + 0 (keep (fn [[id kept]]
                                     (when (contains? keep-ids id) (buffer-bytes kept)))
                                   @buffers))]
    (if (> (+ playing size) max-cache-bytes)
      (do (swap! loaded dissoc audio-id)
          (failed! audio-id :too-much))
      (do (make-room! size keep-ids)
          (swap! buffers assoc audio-id buffer)
          (swap! waveforms assoc audio-id (peaks buffer peak-ms))
          (swap! failures dissoc audio-id)
          (touch! audio-id)
          (swap! version inc)))
    buffer))

(defn remember!
  "Keep `buffer`, the decoded file `audio-id`, as `load!` keeps it: what
  adding a file just decoded and uploaded does, so it is not fetched
  again."
  [audio-id ^js buffer]
  (swap! loaded assoc audio-id (p/resolved buffer))
  (decoded! audio-id buffer))

(defn- fetch-bytes
  [audio-id]
  (-> (js/fetch (cf/resolve-file-media {:id audio-id}))
      (p/then (fn [^js response]
                (if (.-ok response)
                  (.arrayBuffer response)
                  (p/rejected (ex-info "audio file not found"
                                       {:type :audio-not-found
                                        :audio-id audio-id
                                        :status (.-status response)})))))))

(defn too-long?
  "Whether the file of `clip` is longer than any clip may play (see
  `ctau/max-audio-duration`): it is not decoded, and stays silent."
  [clip]
  (> (:source-duration clip) ctau/max-audio-duration))

(defn load!
  "A promise of the decoded file of `clip`, its `AudioBuffer`, fetched
  and decoded once for all the clips of the same `:audio-id`, and kept
  while there is room (see `decoded!`); a failed load is noted (see
  `failure`) and tried again by the next call. The file of a clip that
  is `too-long?` is refused before it is fetched. `:fetch` (a promise of
  the bytes of an `:audio-id`) and `:decode` replace the network and the
  decoder in tests."
  ([clip]
   (load! clip nil))
  ([{:keys [audio-id] :as clip} {fetch :fetch decode* :decode}]
   (cond
     (too-long? clip)
     (p/rejected (ex-info "audio file too long" {:type :audio-too-long :audio-id audio-id}))

     (contains? @loaded audio-id)
     (do (touch! audio-id)
         (get @loaded audio-id))

     :else
     (let [gen     @generation
           ;; what failed, when it does: the fetch, else the decoding
           stage   (volatile! :not-loaded)
           loading (-> ((or fetch fetch-bytes) audio-id)
                       (p/then (fn [bytes]
                                 (vreset! stage :undecodable)
                                 ((or decode* decode) bytes)))
                       (p/then (fn [buffer]
                                 (if (= gen @generation)
                                   (decoded! audio-id buffer)
                                   buffer))))]
       (swap! loaded assoc audio-id loading)
       (p/catch loading
                (fn [cause]
                  (swap! loaded #(cond-> % (identical? loading (get % audio-id)) (dissoc audio-id)))
                  (when (= gen @generation)
                    (failed! audio-id (if (= :audio-unavailable (:type (ex-data cause)))
                                        :unavailable
                                        @stage)))))
       loading))))

(defn- schedule-part!
  "Plays `buffer` in the offline context `ctx` as `part` of a plan says
  (see `mix!`): from `:offset` ms of it, at its `:speed` (so its
  `:length` ms take `:length` times `:speed` ms of the file)."
  [^js ctx ^js buffer {:keys [at offset length speed points]}]
  (let [^js source (.createBufferSource ctx)
        ^js gain   (.createGain ctx)
        ^js param  (.-gain gain)
        speed      (or speed 1)
        ;; in seconds from the start of the mix
        time       #(/ (+ at %) 1000)]
    (set! (.-buffer source) buffer)
    (set! (.. source -playbackRate -value) speed)
    (doseq [[i [ms value]] (map-indexed vector points)]
      (if (zero? i)
        (.setValueAtTime param value (time ms))
        (.linearRampToValueAtTime param value (time ms))))
    (.connect source gain)
    (.connect gain (.-destination ctx))
    ;; the offset and the length of `start` are in seconds of the file
    (.start source (time 0) (/ offset 1000) (* speed (/ length 1000)))))

(defn mix!
  "A promise of the mix of the parts of `plan` (see `cta/audio-plan`), an
  `AudioBuffer` of `length` ms in stereo at `sample-rate`: each part the
  decoded file `load` gives for it (a promise of an `AudioBuffer`), from
  `:offset` ms into the file for `:length` ms, `:at` ms into the mix,
  through its gain `:points`. A part whose file does not load is left
  out."
  [plan length {:keys [load offline-context]}]
  (if-let [ctor (or offline-context (offline-context-class))]
    (let [frames  (max 1 (js/Math.round (* length (/ sample-rate 1000))))
          ^js ctx (new ctor channels frames sample-rate)
          loads   (map (fn [part]
                         (p/catch (load part)
                                  (fn [cause]
                                    (log/warn :hint "audio clip left out of the mix"
                                              :clip-id (:clip-id part)
                                              :cause cause)
                                    nil)))
                       plan)]
      (p/then (p/all loads)
              (fn [buffers]
                (doseq [[part buffer] (map vector plan buffers)
                        :when (some? buffer)]
                  (schedule-part! ctx buffer part))
                (.startRendering ctx))))
    (no-web-audio)))

;; LIVE PLAYBACK
;;
;; The sound of the timeline playing in the workspace or the viewer. It
;; does not keep time itself: it follows the picture (see `follow!`),
;; which keeps time with the audible clock of the context while sound
;; plays (see `now-ms`). Each voice follows a timeline of its own: the
;; view mode plays the sound of the mains of the component copies in a
;; board on their clock, which goes on past the end of the board.

(defonce ^:private context
  ;; the `AudioContext` of live playback, made by `unlock!`
  (atom nil))

(defonce ^:private playing
  ;; what each voice plays: voice -> `{:sources :origin :elapsed :until
  ;; :key}`, the parts of the clips heard from the audible time `:origin`
  ;; (ms) on, when playback was `:elapsed` ms from its start, scheduled
  ;; until `:until` ms of it
  (atom {}))

(def ^:private tolerance
  "How far, in ms, the sound may be off the picture before it starts again
  where the picture is."
  40)

(def ^:private window
  "How much playback, in ms, is scheduled at a time."
  4000)

(def ^:private lookahead
  "How long before what is scheduled runs out, in ms, the next window is
  scheduled."
  1500)

(defn- audio-context-class
  []
  (or (unchecked-get js/globalThis "AudioContext")
      (unchecked-get js/globalThis "webkitAudioContext")))

(defn use-context!
  "Play through `ctx`, an `AudioContext` (or a fake one, in tests); nil
  forgets it."
  [ctx]
  (reset! context ctx))

(defn unlock!
  "Make live playback able to sound: a page may only start sound from a
  user gesture, so the play buttons and Space call this before they
  play. Makes the context the first time and resumes it when suspended."
  []
  (let [^js ctx (or @context
                    (when-let [ctor (audio-context-class)]
                      (reset! context (new ctor #js {:latencyHint "interactive"}))))]
    (when (and (some? ctx) (= "suspended" (.-state ctx)))
      (-> (.resume ctx)
          (.catch (fn [_] nil))))
    ctx))

(defn when-running
  "Call `f` once live playback can sound (see `unlock!`): at once when its
  context runs or there is none yet, else when it resumes, or after
  `timeout` ms when it does not (the playback is then silent)."
  [f timeout]
  (let [^js ctx @context]
    (if (or (nil? ctx) (= "running" (.-state ctx)))
      (f)
      (let [done (volatile! false)
            once (fn [_]
                   (when-not @done
                     (vreset! done true)
                     (f)))]
        (-> (.resume ctx)
            (.then once)
            (.catch once))
        (js/setTimeout once timeout)))))

(defn- running
  []
  (let [^js ctx @context]
    (when (and (some? ctx) (= "running" (.-state ctx)))
      ctx)))

(defn now-ms
  "The audible time of the context of live playback, in ms: the time of
  the sound the speakers play now (its output timestamp, else its
  current time less its output and base latencies). Nil when no context
  runs."
  []
  (when-let [^js ctx (running)]
    (let [^js stamp (when (fn? (.-getOutputTimestamp ctx)) (.getOutputTimestamp ctx))
          time      (if (and (some? stamp) (pos? (.-contextTime stamp)))
                      (.-contextTime stamp)
                      (- (.-currentTime ctx)
                         (or (.-outputLatency ctx) 0)
                         (or (.-baseLatency ctx) 0)))]
      (* 1000 time))))

(defn- release!
  [[^js source ^js gain]]
  (.disconnect source)
  (.disconnect gain))

(defn- stop!
  [[^js source :as part]]
  (try
    (.stop source)
    (catch :default _ nil))
  (release! part))

(defn- stop-voice!
  [voice]
  (run! stop! (:sources (get @playing voice)))
  (swap! playing dissoc voice))

(defn stop-all!
  "Stop all the sound of live playback, of every voice."
  []
  (run! stop! (mapcat :sources (vals @playing)))
  (reset! playing {}))

(defn- playing-ids
  "The files of the clips live playback plays."
  []
  (into #{} (comp (mapcat (comp vals first :key)) (map :audio-id)) (vals @playing)))

(defn forget-all!
  "Stop the sound and let go of every file decoded, and of what failed,
  as leaving a page, the workspace or the viewer does: the files of the
  clips shown next load again."
  []
  (stop-all!)
  (swap! generation inc)
  (reset! loaded {})
  (reset! buffers {})
  (reset! waveforms {})
  (reset! last-used {})
  (reset! failures {})
  (swap! version inc))

(defn- shown-at
  "`[time direction]` of `timeline` playback shows `elapsed` ms after its
  start (see `cta/playback-time`)."
  [timeline elapsed]
  (let [duration (max 1 (:duration timeline))
        back?    (and (= :ping-pong (cta/playback-mode timeline))
                      (> (mod elapsed (* 2 duration)) duration))]
    [(cta/playback-time timeline elapsed) (if back? -1 1)]))

(defn- elapsed-at
  "The ms after its start, on its first cycle, at which playback of
  `timeline` shows `time` going in `direction`."
  [timeline time direction]
  (if (and (= :ping-pong (cta/playback-mode timeline)) (neg? direction))
    (- (* 2 (:duration timeline)) time)
    time))

(defn- sound-key
  "What of `timeline` the scheduled sound depends on: a change of it
  schedules it again."
  [timeline]
  (let [clips (:audio-clips timeline)]
    [clips
     (:duration timeline)
     (cta/playback-mode timeline)
     (into #{} (comp (map :audio-id) (filter #(contains? @buffers %))) (vals clips))]))

(defn- points-from
  "The gain `points` of a part (`[[ms gain] ...]`, see `cta/audio-plan`)
  from `t` ms into it on: the gain there, between the points around it,
  then the later points."
  [points t]
  (let [[before after] (split-with #(<= (first %) t) points)]
    (if-let [[t0 g0] (last before)]
      (let [[t1 g1] (first after)]
        (into [[t (if (some? t1) (+ g0 (* (- g1 g0) (/ (- t t0) (- t1 t0)))) g0)]] after))
      points)))

(defn- schedule!
  "Play in `ctx` the parts of `timeline` from `from` to `to` ms after the
  start of its playback, which is heard `elapsed` ms from its start at
  the audible time `origin` (ms): what can still be heard of each, on
  the time of the context, never before it (the audible time is behind
  it, before 0 as the context starts). The `[source gain end]` of each,
  its end in seconds of the context; a part that fails is left out."
  [^js ctx timeline origin elapsed from to]
  (let [current (.-currentTime ctx)
        clips   (:audio-clips timeline)]
    (into []
          (keep (fn [{:keys [clip-id at offset length speed points]}]
                  (when-let [^js buffer (get @buffers (:audio-id (get clips clip-id)))]
                    (let [start  (/ (+ origin (- (+ from at) elapsed)) 1000)
                          late   (max 0 (- current start))
                          length (/ length 1000)
                          speed  (or speed 1)]
                      (when (< late length)
                        (try
                          (let [^js source (.createBufferSource ctx)
                                ^js gain   (.createGain ctx)
                                ^js param  (.-gain gain)]
                            (set! (.-buffer source) buffer)
                            (set! (.. source -playbackRate -value) speed)
                            ;; the gain from where the part is heard now
                            (doseq [[i [ms value]] (map-indexed vector (points-from points (* late 1000)))]
                              (let [at (max current (+ start (/ ms 1000)))]
                                (if (zero? i)
                                  (.setValueAtTime param value at)
                                  (.linearRampToValueAtTime param value at))))
                            (.connect source gain)
                            (.connect gain (.-destination ctx))
                            ;; from where the part is heard now, in seconds of the file
                            (.start source (+ start late) (+ (/ offset 1000) (* late speed)) (* (- length late) speed))
                            [source gain (+ start length)])
                          (catch :default cause
                            (log/warn :hint "audio clip not played" :clip-id clip-id :cause cause)
                            nil)))))))
          (cta/audio-plan timeline from (- to from)))))

(defn- start!
  "Play with `voice` the sound of `timeline` from the audible time `now`,
  at which the picture shows `time` going in `direction`."
  [^js ctx voice timeline time direction now]
  (stop-voice! voice)
  (let [elapsed (elapsed-at timeline time direction)
        until   (+ elapsed window)]
    (swap! playing assoc voice
           {:sources (schedule! ctx timeline now elapsed elapsed until)
            :origin now
            :elapsed elapsed
            :until until
            :key (sound-key timeline)})))

(defn load-clips!
  "Start loading the files of the clips of `timeline` not loaded yet, but
  one that failed less than `retry-ms` ago: a finished one plays from
  the next frame on (see `sound-key`). `opts` go to `load!`."
  ([timeline]
   (load-clips! timeline nil))
  ([timeline opts]
   (let [since (- (js/Date.now) retry-ms)]
     (doseq [clip (vals (:audio-clips timeline))
             :let [audio-id (:audio-id clip)]
             :when (and (not (decoded? audio-id))
                        (not (contains? @loaded audio-id))
                        (not (too-long? clip))
                        (not (some-> (get @failures audio-id) :at (> since))))]
       (p/catch (load! clip opts) (constantly nil))))))

(defn follow!
  "Keep the sound of `timeline` with its picture, which shows `time` going
  in `direction` (1, or -1 on the way back of ping-pong), playing or not
  (`playing?`): call it at each frame shown and at each move of the
  playhead. Sound starts again where the picture is when it is off it by
  more than `tolerance`, when the direction changes, or when the clips
  or the files loaded of them change; steady playback, loops and the
  turns of ping-pong included, only schedule more of it (see
  `lookahead`). Silent without a running context (see `unlock!`). Each
  `voice` follows its own timeline; without one, the main voice."
  ([timeline time direction playing?]
   (follow! ::main timeline time direction playing?))
  ([voice timeline time direction playing?]
   (let [^js ctx (running)
         now     (now-ms)]
     (if (or (not playing?) (nil? ctx) (nil? timeline) (empty? (:audio-clips timeline)))
       (when (contains? @playing voice)
         (stop-voice! voice))
       (let [{:keys [origin elapsed until key] :as current} (get @playing voice)
             expected (when current (+ elapsed (- now origin)))
             [shown dir] (when expected (shown-at timeline expected))]
         (load-clips! timeline)
         (cond
           (or (nil? current)
               (not= key (sound-key timeline))
               (not= dir direction)
               (> (mth/abs (- shown time)) tolerance))
           (start! ctx voice timeline time direction now)

           (< (- until expected) lookahead)
           (let [current      (.-currentTime ctx)
                 [done going] ((juxt filter remove) #(< (nth % 2) current) (:sources (get @playing voice)))]
             ;; the parts heard to their end are let go
             (run! release! done)
             (swap! playing update voice assoc
                    :sources (into (vec going) (schedule! ctx timeline origin elapsed until (+ until window)))
                    :until (+ until window)))))))))
