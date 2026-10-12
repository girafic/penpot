;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.audio-recording
  "Sound recorded from the microphone into the timeline of a board of
  Penpot Motion, as a voice-over: from the playhead, the animation
  playing along, an audio clip where it started once it stops (added as
  a dropped file is, see `app.main.data.workspace.audio`)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.media :as cm]
   [app.common.types.audio-clip :as ctau]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.audio :as dwaa]
   [app.util.audio :as audio]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(def ^:private recorder-types
  "The types a recording is asked in, the first one the browser records:
  Chrome and Firefox record Opus in WebM or Ogg, Safari AAC in MP4."
  ["audio/webm;codecs=opus" "audio/ogg;codecs=opus" "audio/mp4" "audio/webm"])

(def ^:private type-extensions
  "The extension of a recording by its canonical type, which names it."
  {"audio/webm" "webm"
   "audio/ogg"  "ogg"
   "audio/mp4"  "m4a"
   "audio/wav"  "wav"
   "audio/mpeg" "mp3"})

(defn supported?
  "Whether the browser records sound from a microphone."
  []
  (and (exists? js/MediaRecorder)
       (exists? js/navigator)
       (some? (.-mediaDevices js/navigator))
       (fn? (.. js/navigator -mediaDevices -getUserMedia))))

(defn recorder-type
  "The type to record in: the first of `recorder-types` that `supports?`
  (as `MediaRecorder.isTypeSupported`); nil lets the browser choose."
  [supports?]
  (d/seek supports? recorder-types))

(defn recording-name
  "The name of a new recording in `timeline`, as Penpot names new layers
  (not translated, as other people may read it): \"Recording 1\", or the
  first number its audio clips do not have yet (their names end in the
  extension of their file)."
  [timeline]
  (let [clips (vals (:audio-clips timeline))
        taken (into #{} (map #(str/replace (or (:name %) "") #"\.[^.]+$" "")) clips)]
    (d/seek (complement taken)
            (map #(str "Recording " %) (range 1 (+ 2 (count clips)))))))

(defn recording-file
  "The recording `blob` of the recorder type `mime` (with its codecs, nil
  for that of `blob`) as a `js/File` named `name` with the extension of
  its type, typed as it is stored (see `dwaa/audio-type`)."
  [^js blob mime name]
  (let [base  (str/trim (first (str/split (or (not-empty mime) (.-type blob) "") ";")))
        mtype (or (cm/canonical-audio-type base) "audio/webm")]
    (js/File. #js [blob] (str name "." (get type-extensions mtype "webm")) #js {:type mtype})))

(defn- microphone-error
  "The notice of `cause`, why the microphone did not record (a
  `DOMException` of `getUserMedia` or of the recorder)."
  [cause]
  (ntf/error
   (case (some-> cause .-name)
     ("NotAllowedError" "SecurityError")      (tr "workspace.audio.microphone-denied")
     ("NotFoundError" "OverconstrainedError") (tr "workspace.audio.microphone-missing")
     ("NotReadableError" "AbortError")        (tr "workspace.audio.microphone-busy")
     (tr "workspace.audio.recording-failed"))))

(defn recording
  "The recording in progress: `{:board-id :pending? true}` while the
  browser asks for the microphone, then `{:board-id :start :since}`, where
  it started on the timeline (ms) and when (`Date.now`); nil without."
  [state]
  (dm/get-in state [:workspace-animation :audio-recording]))

(defonce ^:private current
  ;; the recorder recording, `{:recorder :discard?}`
  (atom nil))

(defn open-microphone!
  "A promise of the stream of the microphone, once the user allows it
  (replaced in tests)."
  []
  (.getUserMedia (.-mediaDevices js/navigator)
                 #js {:audio #js {:echoCancellation true
                                  :noiseSuppression true
                                  :autoGainControl true}}))

(defn make-recorder
  "A `MediaRecorder` of `stream`, in the first type the browser records
  (see `recorder-type`; replaced in tests)."
  [stream]
  (if-let [mime (recorder-type #(.isTypeSupported js/MediaRecorder %))]
    (js/MediaRecorder. stream #js {:mimeType mime})
    (js/MediaRecorder. stream)))

(defn record
  "An observable of what `recorder` records of `stream`: `:started` once
  it records, then the recording, a `js/Blob`, once it stops, and done.
  The tracks of `stream` stop then, the microphone free again, or when
  it is no longer observed."
  [^js recorder ^js stream]
  (rx/create
   (fn [subs]
     (let [chunks  #js []
           release (fn [] (doseq [track (.getTracks stream)] (.stop ^js track)))]
       (set! (.-ondataavailable recorder)
             (fn [^js event]
               (when (pos? (.. event -data -size))
                 (.push chunks (.-data event)))))
       (set! (.-onstart recorder) (fn [_] (rx/push! subs :started)))
       (set! (.-onstop recorder)
             (fn [_]
               (release)
               (rx/push! subs (js/Blob. chunks #js {:type (.-mimeType recorder)}))
               (rx/end! subs)))
       (set! (.-onerror recorder)
             (fn [^js event]
               (release)
               (rx/error! subs (or (.-error event) event))))
       ;; in slices of a second, so a long one does not wait for its end
       (.start recorder 1000)
       (fn []
         (when (not= "inactive" (.-state recorder))
           (.stop recorder))
         (release))))))

(def recording-clip-id
  "The id of the clip the timeline dock shows of the recording in
  progress (see `with-recording-clip`)."
  #uuid "00000000-0000-0000-0000-0000000ec0d1")

(def recording-audio-id
  "The `:audio-id` of the clip of the recording in progress: its waveform
  is the one measured as it records (see `waveform`)."
  #uuid "00000000-0000-0000-0000-0000000ec0d2")

(defonce ^:private live
  ;; the waveform of the recording in progress (see `start-meter!`):
  ;; `{:data :filled :peak}`, the min and max of each `audio/peak-ms` of it
  ;; in `data`, `filled` of them so far
  (atom nil))

(defn- start-meter!
  "Measure the sound of `mic` as it records since `since` (`Date.now`),
  through the context of live playback (made by the button that starts
  it, see `audio/unlock!`): its waveform grows (see `waveform`). A
  function that stops it; nil without a context."
  [^js mic since]
  (when-let [^js ctx (audio/unlock!)]
    (let [source   (.createMediaStreamSource ctx mic)
          analyser (.createAnalyser ctx)
          buckets  (js/Math.ceil (/ ctau/max-audio-duration audio/peak-ms))
          data     (js/Float32Array. (* 2 buckets))
          samples  (js/Float32Array. 2048)
          measure  (fn []
                     (.getFloatTimeDomainData analyser samples)
                     (let [[lo hi] (loop [i 0 lo 0 hi 0]
                                     (if (< i (.-length samples))
                                       (let [v (aget samples i)]
                                         (recur (inc i) (min lo v) (max hi v)))
                                       [lo hi]))
                           until   (min buckets (js/Math.floor (/ (- (js/Date.now) since) audio/peak-ms)))]
                       ;; what the last look heard, over the time since it
                       (swap! live (fn [{:keys [filled peak] :as state}]
                                     (loop [b filled]
                                       (when (< b until)
                                         (aset data (* 2 b) lo)
                                         (aset data (inc (* 2 b)) hi)
                                         (recur (inc b))))
                                     (assoc state
                                            :filled (max filled until)
                                            :peak (max peak (- lo) hi))))))
          timer    (do (set! (.-fftSize analyser) 2048)
                       (.connect source analyser)
                       (reset! live {:data data :filled 0 :peak 0})
                       (js/setInterval measure 40))]
      (fn []
        (js/clearInterval timer)
        (.disconnect source)
        (reset! live nil)))))

(defn waveform
  "The waveform of the file `audio-id`, as `audio/waveform` gives it, and
  of the recording in progress so far (see `recording-audio-id`)."
  [audio-id]
  (if (= audio-id recording-audio-id)
    (when-let [{:keys [^js data filled peak]} @live]
      {:bucket-ms audio/peak-ms
       :channels [(.subarray data 0 (* 2 filled))]
       :peak peak})
    (audio/waveform audio-id)))

(defn with-recording-clip
  "`timeline` with a clip of the recording `rec` (see `recording`), as the
  timeline dock shows it while it records: where it started, on the lane
  its clip goes to (see `ctau/add-audio-clip`), locked, its
  `:recording-since` when it started, which the dock draws it as long
  since (see `app.main.ui.workspace.timeline.lanes`)."
  [timeline {:keys [start since]}]
  (-> timeline
      (ctau/add-audio-clip {:id recording-clip-id
                            :audio-id recording-audio-id
                            :name (recording-name timeline)
                            :source-duration ctau/max-audio-duration
                            :start start
                            :duration ctau/min-clip-duration
                            :locked true})
      (update-in [:audio-clips recording-clip-id] assoc :recording-since since)))

(defn- recording-cleared
  []
  (ptk/reify ::recording-cleared
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation dissoc :audio-recording))))

(defn- recording-started
  "The recording of the board `board-id` records since `since`
  (`Date.now`): from where playback started when it started it
  (`play-start?`), else from the playhead."
  [board-id play-start? since]
  (ptk/reify ::recording-started
    ptk/UpdateEvent
    (update [_ state]
      (let [anim (:workspace-animation state)]
        (assoc-in state [:workspace-animation :audio-recording]
                  {:board-id board-id
                   :start (if play-start? (get anim :play-start 0) (get anim :playhead 0))
                   :since since})))))

(defn- recording-began
  "The recorder records since `since`: the animation plays along, from the
  playhead, unless it plays already."
  [board-id since]
  (ptk/reify ::recording-began
    ptk/WatchEvent
    (watch [_ state _]
      (if (dm/get-in state [:workspace-animation :playing?])
        (rx/of (recording-started board-id false since))
        (rx/of (dwa/play) (recording-started board-id true since))))))

(defn- recording-stopped
  "Add the recording `blob` of the recorder type `mime` to the timeline of
  its board, where it started, as a dropped file is; playback stops, at
  its start, to hear it."
  [blob mime]
  (ptk/reify ::recording-stopped
    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [board-id start]} (recording state)]
        (if (or (nil? board-id) (nil? start))
          (rx/of (recording-cleared))
          (let [timeline (dm/get-in (dsh/lookup-page state) [:timelines board-id])
                file     (recording-file blob mime (recording-name timeline))]
            (rx/of (dwa/pause)
                   (recording-cleared)
                   (dwa/set-playhead start)
                   (dwaa/add-audio-files board-id [file] start nil))))))))

(defn stop-audio-recording
  "Stop recording: its clip goes on the timeline (see `recording-stopped`),
  none with `discard?`."
  ([] (stop-audio-recording false))
  ([discard?]
   (ptk/reify ::stop-audio-recording
     ptk/EffectEvent
     (effect [_ _ _]
       (when-let [{:keys [^js recorder]} @current]
         (swap! current assoc :discard? discard?)
         (when (not= "inactive" (.-state recorder))
           (.stop recorder)))))))

(defn start-audio-recording
  "Record sound from the microphone into the timeline of the active board,
  once the user allows it, from the playhead, the animation playing
  along: until stopped (see `stop-audio-recording`), the timeline closed
  or `ctau/max-audio-duration`; dropped when the workspace closes. A
  notice when the browser does not record, or the microphone does not."
  []
  (ptk/reify ::start-audio-recording
    ptk/UpdateEvent
    (update [_ state]
      (let [board-id (dwa/active-board-id state)]
        (cond-> state
          (and (some? board-id) (nil? (recording state)) (nil? @current) (supported?))
          (assoc-in [:workspace-animation :audio-recording] {:board-id board-id :pending? true}))))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [board-id pending?]} (recording state)]
        (cond
          (nil? (dwa/active-board-id state))
          (rx/empty)

          (not (supported?))
          (rx/of (ntf/error (tr "workspace.audio.recording-unsupported")))

          (not pending?)
          (rx/empty)

          :else
          (->> (rx/from (open-microphone!))
               (rx/mapcat
                (fn [mic]
                  (let [recorder (make-recorder mic)
                        _        (reset! current {:recorder recorder})
                        events   (rx/share (record recorder mic))]
                    (rx/merge
                     (->> events
                          (rx/map (fn [event]
                                    (cond
                                      (= :started event)
                                      (let [since (js/Date.now)]
                                        (swap! current assoc :stop-meter (start-meter! mic since))
                                        (recording-began board-id since))

                                      (:discard? @current)
                                      (recording-cleared)

                                      :else
                                      (recording-stopped event (.-mimeType recorder))))))
                     ;; what stops it but the user
                     (->> (rx/merge (rx/map (constantly false) (rx/timer ctau/max-audio-duration))
                                    (rx/map (constantly false)
                                            (rx/filter (ptk/type? :app.main.data.workspace.animation/close-timeline) stream))
                                    (rx/map (constantly true)
                                            (rx/filter (ptk/type? :app.main.data.workspace/finalize-workspace) stream)))
                          (rx/take 1)
                          (rx/take-until (rx/filter #(not= :started %) events))
                          (rx/map stop-audio-recording))))))
               (rx/catch (fn [cause]
                           (rx/of (recording-cleared) (microphone-error cause))))
               (rx/finalize (fn []
                              (when-let [stop-meter (:stop-meter @current)]
                                (stop-meter))
                              (reset! current nil)))))))))

(defn toggle-audio-recording
  "Start recording sound into the timeline, or stop; nothing while the
  browser asks for the microphone."
  []
  (ptk/reify ::toggle-audio-recording
    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [pending?] :as rec} (recording state)]
        (cond
          pending?     (rx/empty)
          (some? rec)  (rx/of (stop-audio-recording))
          :else        (rx/of (start-audio-recording)))))))
