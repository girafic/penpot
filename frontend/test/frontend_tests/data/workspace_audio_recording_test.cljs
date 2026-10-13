;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.workspace-audio-recording-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.audio :as dwaa]
   [app.main.data.workspace.audio-recording :as dwar]
   [app.main.ui.workspace.timeline.recording-options :as rom]
   [app.util.audio :as audio]
   [app.util.i18n :refer [tr]]
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.mock :as mock]
   [frontend-tests.helpers.pages :as thp]
   [frontend-tests.helpers.state :as ths]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before (fn []
             (thp/reset-idmap!)
             (cthi/reset-idmap!))})

(t/deftest a-recording-is-a-file-of-its-type
  (let [blob (js/Blob. #js ["sound"] #js {:type "audio/ogg"})]
    (doseq [[mime file-name file-type] [["audio/webm;codecs=opus" "Take.webm" "audio/webm"]
                                        ["audio/ogg;codecs=opus" "Take.ogg" "audio/ogg"]
                                        ["audio/mp4" "Take.m4a" "audio/mp4"]
                                        [nil "Take.ogg" "audio/ogg"]]]
      (let [file (dwar/recording-file blob mime "Take")]
        (t/is (= [file-name file-type] [(.-name file) (.-type file)]) (str mime))
        (t/is (= file-type (dwaa/audio-type file)) "a type a clip plays")))))

(t/deftest a-recording-is-asked-in-a-type-the-browser-records
  (t/is (= "audio/webm;codecs=opus" (dwar/recorder-type (constantly true))))
  (t/is (= "audio/mp4" (dwar/recorder-type #{"audio/mp4"})) "as Safari")
  (t/is (nil? (dwar/recorder-type (constantly false))) "or as the browser likes"))

(t/deftest recordings-are-numbered
  (let [named (fn [& names]
                {:audio-clips (into {} (map-indexed (fn [i n] [i {:name n}])) names)})]
    (t/is (= "Recording 1" (dwar/recording-name {})))
    (t/is (= "Recording 2" (dwar/recording-name (named "Recording 1.webm" "beat.mp3"))))
    (t/is (= "Recording 1" (dwar/recording-name (named "Recording 2.ogg"))) "the first free one")
    (t/is (= "Recording 3" (dwar/recording-name (named "Recording 1.webm" "Recording 2.webm"))))))

(defn- fake-mic
  "A stream of a microphone whose track counts in `stopped` how often it
  stopped."
  [stopped]
  #js {:getTracks (fn [] #js [#js {:stop #(swap! stopped inc)}])})

(defn- fake-recorder
  "A `MediaRecorder` that records \"sound\" in WebM."
  []
  (let [recorder #js {:state "inactive" :mimeType "audio/webm;codecs=opus"}]
    (set! (.-start recorder)
          (fn [_]
            (set! (.-state recorder) "recording")
            (js/setTimeout #((.-onstart recorder) #js {}) 0)))
    (set! (.-stop recorder)
          (fn []
            (set! (.-state recorder) "inactive")
            ((.-ondataavailable recorder) #js {:data (js/Blob. #js ["sound"])})
            ((.-onstop recorder) #js {})))
    recorder))

(t/deftest the-recorder-gives-what-it-recorded
  (t/async
    done
    (let [stopped  (atom 0)
          recorder (fake-recorder)
          events   (atom [])]
      (->> (dwar/record recorder (fake-mic stopped))
           (rx/subs! (fn [event]
                       (swap! events conj event)
                       (when (= :started event)
                         (.stop recorder)))
                     (fn [cause] (t/is false (str cause)) (done))
                     (fn []
                       (t/is (= :started (first @events)))
                       (t/is (instance? js/Blob (second @events)) "then the recording")
                       (t/is (pos? @stopped) "the microphone free again")
                       (done)))))))

(defn- store-at
  "A store of board A, whose timeline plays a second, the timeline on it
  at `playhead`, playing."
  [playhead]
  (let [file  (-> (cthf/sample-file :file1 :page-label :page-1)
                  (ctho/add-frame :board-a :x 0 :y 0 :width 400 :height 300))
        file  (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                           (pcb/with-page (cthf/current-page file))
                                           (pcb/set-timeline (cthi/id :board-a)
                                                             (cta/make-timeline {:board-id (cthi/id :board-a)
                                                                                 :duration 1000}))))
        store (ths/setup-store file)]
    (ptk/emit! store #(update % :workspace-animation assoc
                              :board-id (cthi/id :board-a) :playhead playhead :playing? true))
    store))

(t/deftest a-recording-becomes-a-clip-where-it-started
  (t/async
    done
    (let [store   (store-at 300)
          stopped (atom 0)
          added   (atom nil)]
      (mock/with-mocks
        {dwar/supported?        (constantly true)
         dwar/open-microphone!  (fn [_] (js/Promise.resolve (fake-mic stopped)))
         dwar/make-recorder     (fn [_] (fake-recorder))
         ;; called with options (variadic)
         dwaa/add-audio-files   (fn [board-id files time lane & {:keys [trim]}]
                                  (reset! added [board-id (map #(.-name %) files) time lane trim])
                                  (ptk/data-event ::added))}
        (fn [done']
          (ths/run-store
           store done'
           [(ptk/reify ::stop-once-recording
              ptk/WatchEvent
              (watch [_ _ stream]
                (->> stream
                     (rx/filter (ptk/type? :app.main.data.workspace.audio-recording/recording-started))
                     (rx/take 1)
                     ;; longer than a clip at least (see `ctau/min-clip-duration`)
                     (rx/delay 30)
                     (rx/map (fn [_] (dwar/stop-audio-recording))))))
            (dwar/toggle-audio-recording)]
           (fn [state]
             (t/is (= [(cthi/id :board-a) ["Recording 1.webm"] 300 nil 0]
                      @added)
                   "from the playhead, as it was when it started")
             (t/is (nil? (dwar/recording state)) "done")
             (t/is (pos? @stopped) "the microphone free again"))
           (fn [stream] (->> stream (rx/filter #(= ::added (ptk/type %))) (rx/take 1) (rx/delay 20)))))
        done))))

(t/deftest a-microphone-denied-tells-why
  (t/async
    done
    (let [store (store-at 0)]
      (mock/with-mocks
        {dwar/supported?       (constantly true)
         dwar/open-microphone! (fn [_] (js/Promise.reject #js {:name "NotAllowedError"}))}
        (fn [done']
          (ths/run-store
           store done'
           [(dwar/start-audio-recording)]
           (fn [state]
             (t/is (= (tr "workspace.audio.microphone-denied") (get-in state [:notification :content])))
             (t/is (nil? (dwar/recording state)) "nothing records"))
           (fn [stream]
             (->> stream
                  (rx/filter #(= :app.main.data.notifications/show (ptk/type %)))
                  (rx/take 1)
                  (rx/delay 20)))))
        done))))

(t/deftest a-recording-shows-as-a-clip-of-its-own-as-it-records
  ;; Where it started, on the lane its clip goes to, locked; the dock
  ;; draws it as long as it has lasted, with the waveform measured.
  (let [beat     {:id (random-uuid) :audio-id (random-uuid) :name "beat.mp3"
                  :source-duration 2000 :start 0 :offset 0 :duration 2000 :lane 0}
        timeline (-> (cta/make-timeline {:board-id (random-uuid) :duration 1000})
                     (assoc-in [:audio-clips (:id beat)] beat))
        clip     (get-in (dwar/with-recording-clip timeline {:start 300 :since 1000})
                         [:audio-clips dwar/recording-clip-id])]
    (t/is (= [300 1 "Recording 1" true 1000]
             ((juxt :start :lane :name :locked :recording-since) clip))
          "on a lane of its own, as its clip will")
    (t/is (= dwar/recording-audio-id (:audio-id clip)))
    (t/is (nil? (dwar/waveform dwar/recording-audio-id)) "no waveform but while it records")))

(t/deftest a-pre-roll-plays-before-where-the-clip-goes
  (t/is (= [1500 500] (dwar/pre-roll-from 1500 4000 1000)))
  (t/is (= [600 0] (dwar/pre-roll-from 600 4000 1000)) "from 0 at most")
  (t/is (= [0 0] (dwar/pre-roll-from 4000 4000 2000)) "at the end, from the start, as playback starts over")
  (t/is (= [1500 1500] (dwar/pre-roll-from 1500 4000 0)) "none"))

(t/deftest microphones-without-labels-are-numbered
  (let [options (rom/microphone-options [{:id "a" :label "USB mic"} {:id "b" :label nil}])]
    (t/is (= ["system" "a" "b"] (mapv :id options)) "the system's first")
    (t/is (= "USB mic" (:label (second options))))
    (t/is (= (tr "workspace.animation.recording-microphone" 2) (:label (last options))))))

(t/deftest stopped-in-its-pre-roll-a-recording-adds-nothing
  ;; Its clip would go after where it stopped: none, and the playhead back
  ;; where it would have gone.
  (t/async
    done
    (let [store   (store-at 900)
          added   (atom nil)
          started (atom nil)]
      (ptk/emit! store #(-> %
                            (assoc-in [:workspace-animation :playing?] false)
                            (assoc-in [:workspace-animation :recording-options] {:pre-roll 500})))
      (mock/with-mocks
        {dwar/supported?        (constantly true)
         dwar/open-microphone!  (fn [_] (js/Promise.resolve (fake-mic (atom 0))))
         dwar/make-recorder     (fn [_] (fake-recorder))
         dwa/play               (fn [] (ptk/data-event ::played))
         dwaa/add-audio-files   (fn [_ _ _ _ & _] (reset! added true) (ptk/data-event ::added))}
        (fn [done']
          (ths/run-store
           store done'
           [(ptk/reify ::stop-in-pre-roll
              ptk/WatchEvent
              (watch [_ _ stream]
                (->> stream
                     (rx/filter (ptk/type? :app.main.data.workspace.audio-recording/recording-started))
                     (rx/take 1)
                     (rx/tap #(reset! started (dwar/recording @store)))
                     ;; once playback has started, within the pre-roll
                     (rx/delay 30)
                     (rx/map (fn [_] (dwar/stop-audio-recording))))))
            (dwar/toggle-audio-recording)]
           (fn [state]
             (t/is (= [900 500] ((juxt :start :offset) @started)) "where its clip goes, past a pre-roll")
             (t/is (nil? @added) "nothing added")
             (t/is (= 900 (get-in state [:workspace-animation :playhead])))
             (t/is (nil? (dwar/recording state))))
           (fn [stream]
             (->> stream
                  (rx/filter (ptk/type? :app.main.data.workspace.audio-recording/recording-cleared))
                  (rx/take 1)
                  (rx/delay 20)))))
        done))))

(t/deftest the-other-clips-keep-quiet-while-it-records
  (let [store  (store-at 0)
        sounds (atom [])]
    (with-redefs [audio/follow! (mock/stub (fn [_ _ _ sounding?] (swap! sounds conj sounding?)))]
      (ptk/emit! store (dwa/set-playhead 100))
      (ptk/emit! store #(assoc-in % [:workspace-animation :audio-recording] {:start 0 :since 0 :quiet? true}))
      (ptk/emit! store (dwa/set-playhead 200))
      (t/is (= [true false] @sounds) "playing, then quiet"))))

;; HOW A RECORDING ENDS

(t/deftest a-recording-stops-before-a-clip-would-be-too-long
  ;; What an encoder pads on keeps it a file a clip plays (see
  ;; `dwaa/prepare`).
  (t/is (< dwar/recording-limit ctau/max-audio-duration)))

(defn- record-until
  "Record on `store` with the microphone `mic` until `stop` (an event)
  comes `delay` ms after the recording records (without, while the
  browser asks for the microphone, `then` called after), then `check`
  the state, what `add-audio-files` got (its options, `:board-id` and
  `:time`, nil when not called)
  and whether a recorder was made; `open` opens the microphone (a
  promise of `mic` by default)."
  [done store {:keys [stop delay check open mic then]}]
  (let [added (atom nil)
        made  (atom false)]
    (mock/with-mocks
      {dwar/supported?       (constantly true)
       dwar/open-microphone! (or open (fn [_] (js/Promise.resolve mic)))
       dwar/make-recorder    (fn [_] (reset! made true) (fake-recorder))
       dwa/play              (fn [] (ptk/data-event ::played))
       ;; as it is called: four arguments, then options
       dwaa/add-audio-files  (fn [board-id _ time _ & {:as opts}]
                               (reset! added (assoc opts :board-id board-id :time time))
                               (ptk/data-event ::added))}
      (fn [done']
        (ths/run-store
         store done'
         [(ptk/reify ::stop-it
            ptk/WatchEvent
            (watch [_ _ stream]
              (if (some? delay)
                (->> stream
                     (rx/filter (ptk/type? :app.main.data.workspace.audio-recording/recording-started))
                     (rx/take 1)
                     (rx/delay delay)
                     (rx/map (constantly stop)))
                ;; once it hears, before the browser answers
                (rx/concat
                 (->> stream
                      (rx/filter (ptk/type? :app.main.data.workspace.audio-recording/start-audio-recording))
                      (rx/take 1)
                      (rx/delay 10)
                      (rx/map (constantly stop)))
                 (->> (rx/timer 10)
                      (rx/tap (fn [_] (when then (then))))
                      (rx/ignore))))))
          (dwar/start-audio-recording)]
         (fn [state] (check state @added @made))
         (fn [stream]
           (->> stream
                (rx/filter (ptk/type? :app.main.data.workspace.audio-recording/recording-cleared))
                (rx/take 1)
                (rx/delay 30)))))
      done)))

(t/deftest closing-the-timeline-keeps-what-it-recorded
  (t/async
    done
    (let [stopped (atom 0)]
      (record-until
       done (store-at 300)
       {:mic   (fake-mic stopped)
        :stop  (ptk/data-event :app.main.data.workspace.animation/close-timeline)
        :delay 30
        :check (fn [state added _]
                 (t/is (some? added) "its clip added")
                 (t/is (nil? (dwar/recording state)))
                 (t/is (pos? @stopped) "the microphone free again"))}))))

(t/deftest closing-the-workspace-drops-what-it-recorded
  (t/async
    done
    (let [stopped (atom 0)]
      (record-until
       done (store-at 300)
       {:mic   (fake-mic stopped)
        :stop  (ptk/data-event :app.main.data.workspace/finalize-workspace)
        :delay 30
        :check (fn [state added _]
                 (t/is (nil? added) "nothing added")
                 (t/is (nil? (dwar/recording state)))
                 (t/is (pos? @stopped) "the microphone free again"))}))))

(t/deftest closing-the-workspace-while-the-browser-asks-records-nothing
  ;; The microphone it gives afterwards is freed at once.
  (t/async
    done
    (let [stopped (atom 0)
          give    (atom nil)]
      (record-until
       done (store-at 300)
       {:open  (fn [_] (js/Promise. (fn [resolve _] (reset! give resolve))))
        :stop  (ptk/data-event :app.main.data.workspace/finalize-workspace)
        :then  #(@give (fake-mic stopped))
        :check (fn [state added made]
                 (t/is (nil? (dwar/recording state)) "it asks no more")
                 (t/is (nil? added))
                 (t/is (not made) "no recorder")
                 (t/is (pos? @stopped) "the microphone given freed"))}))))

(t/deftest a-recording-at-its-limit-keeps-what-it-recorded
  ;; It stops by itself, short of what a clip plays, and its clip is
  ;; added.
  (t/async
    done
    (let [saved dwar/recording-limit
          _     (set! dwar/recording-limit 40)]
      (record-until
       (fn [] (set! dwar/recording-limit saved) (done))
       (store-at 300)
       {:mic   (fake-mic (atom 0))
        :stop  (ptk/data-event ::nothing)
        :delay 5000
        :check (fn [state added _]
                 (t/is (some? added) "its clip added")
                 (t/is (nil? (dwar/recording state))))}))))

(t/deftest a-microphone-denied-once-it-ended-tells-nothing
  ;; Ended while the browser asked, then denied: no notice, as on the
  ;; dashboard.
  (t/async
    done
    (let [deny (atom nil)]
      (record-until
       done (store-at 300)
       {:open  (fn [_] (js/Promise. (fn [_ reject] (reset! deny reject))))
        :stop  (ptk/data-event :app.main.data.workspace/finalize-workspace)
        :then  #(@deny (doto (js/Error. "NotAllowedError") (unchecked-set "name" "NotAllowedError")))
        :check (fn [state _ _]
                 (t/is (nil? (dwar/recording state)))
                 (t/is (nil? (get-in state [:notification :content])) "no notice"))}))))

(t/deftest a-recording-left-for-another-page-goes-on-its-own
  (t/async
    done
    (let [store   (store-at 300)
          page-id (:current-page-id @store)]
      (record-until
       done store
       {:mic   (fake-mic (atom 0))
        :stop  (ptk/reify ::other-page
                 ptk/UpdateEvent
                 (update [_ state] (assoc state :current-page-id (uuid/next)))
                 ptk/WatchEvent
                 (watch [_ _ _] (rx/of (ptk/data-event :app.main.data.workspace.pages/finalize-page))))
        :delay 30
        :check (fn [_ added _]
                 (t/is (= page-id (:page-id added)) "on the page it started on"))}))))

;; MICROPHONES AND OPTIONS

(defn- with-media-devices
  "Call `f` with `devices` as the `navigator.mediaDevices` of the page; a
  promise of what it gives, the navigator as it was after."
  [devices f]
  (let [saved (js/Object.getOwnPropertyDescriptor js/globalThis "navigator")
        put   #(js/Object.defineProperty js/globalThis "navigator" %)
        back  #(if (some? saved) (put saved) (js-delete js/globalThis "navigator"))]
    (put #js {:value #js {:mediaDevices devices} :configurable true :writable true})
    (-> (js/Promise.resolve (f))
        (.then (fn [v] (back) v) (fn [e] (back) (js/Promise.reject e))))))

(defn- error-named [name] (doto (js/Error. name) (unchecked-set "name" name)))

(t/deftest a-microphone-gone-falls-back-to-the-system-one
  (t/async
    done
    (let [asked (atom [])
          media #js {:getUserMedia
                     (fn [constraints]
                       (let [device (some-> (.. constraints -audio -deviceId) .-exact)]
                         (swap! asked conj device)
                         (if (some? device)
                           (js/Promise.reject (error-named "OverconstrainedError"))
                           (js/Promise.resolve :system))))}]
      (-> (with-media-devices media #(dwar/open-microphone! "gone"))
          (.then (fn [mic]
                   (t/is (= :system mic))
                   (t/is (= ["gone" nil] @asked))))
          (.catch (fn [e] (t/is false (str e))))
          (.then (fn [_] (done) nil))))))

(t/deftest a-microphone-denied-is-not-asked-again
  (t/async
    done
    (let [asked (atom 0)
          media #js {:getUserMedia (fn [_] (swap! asked inc) (js/Promise.reject (error-named "NotAllowedError")))}]
      (-> (with-media-devices media #(dwar/open-microphone! "usb"))
          (.then (fn [_] (t/is false "allowed")))
          (.catch (fn [e]
                    (t/is (= "NotAllowedError" (.-name e)))
                    (t/is (= 1 @asked))))
          (.then (fn [_] (done) nil))))))

(t/deftest the-microphones-are-the-inputs-of-the-browser
  (t/async
    done
    (let [device (fn [kind id label] #js {:kind kind :deviceId id :label label})
          media  #js {:enumerateDevices
                      (fn []
                        (js/Promise.resolve
                         #js [(device "audioinput" "default" "Default")
                              (device "audioinput" "communications" "Communications")
                              (device "videoinput" "cam" "Camera")
                              (device "audioinput" "a" "USB mic")
                              (device "audioinput" "b" "")
                              ;; hidden until one is allowed
                              (device "audioinput" "" "")]))}]
      (-> (with-media-devices media dwar/list-microphones!)
          (.then (fn [mics]
                   (t/is (= [{:id "a" :label "USB mic"} {:id "b" :label nil}] mics)
                         "but the system's own, unlabelled before one is allowed")))
          (.catch (fn [e] (t/is false (str e))))
          (.then (fn [_] (done) nil))))))

(t/deftest the-options-of-recording-are-kept
  (let [store (store-at 0)]
    (ptk/emit! store (dwar/set-recording-options {:pre-roll 2000 :bogus 1}))
    (t/is (= {:pre-roll 2000 :device-id nil :quiet? false}
             (select-keys (dwar/recording-options @store) [:pre-roll :device-id :quiet? :bogus]))
          "the ones it knows, over the defaults")
    (t/is (= 2000 (:pre-roll (get @storage/user :app.main.data.workspace.audio-recording/options)))
          "for the next time")
    (ptk/emit! store (dwar/set-recording-options {:pre-roll 0}))))
