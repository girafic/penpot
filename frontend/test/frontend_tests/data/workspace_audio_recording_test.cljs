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
   [app.main.data.workspace.audio :as dwaa]
   [app.main.data.workspace.audio-recording :as dwar]
   [app.util.i18n :refer [tr]]
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
         dwar/open-microphone!  #(js/Promise.resolve (fake-mic stopped))
         dwar/make-recorder     (fn [_] (fake-recorder))
         dwaa/add-audio-files   (mock/stub (fn [board-id files time lane]
                                             (reset! added [board-id (map #(.-name %) files) time lane])
                                             (ptk/data-event ::added)))}
        (fn [done']
          (ths/run-store
           store done'
           [(ptk/reify ::stop-once-recording
              ptk/WatchEvent
              (watch [_ _ stream]
                (->> stream
                     (rx/filter (ptk/type? :app.main.data.workspace.audio-recording/recording-started))
                     (rx/take 1)
                     (rx/map (fn [_] (dwar/stop-audio-recording))))))
            (dwar/toggle-audio-recording)]
           (fn [state]
             (t/is (= [(cthi/id :board-a) ["Recording 1.webm"] 300 nil]
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
         dwar/open-microphone! #(js/Promise.reject #js {:name "NotAllowedError"})}
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
