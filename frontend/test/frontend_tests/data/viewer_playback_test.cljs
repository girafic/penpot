;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-playback-test
  (:require
   [app.common.geom.point :as gpt]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.modifiers :as ctm]
   [app.common.uuid :as uuid]
   [app.main.data.viewer :as dv]
   [app.main.ui.viewer.interactions :as vi]
   [app.util.audio :as audio]
   [app.util.timers :as ts]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(t/deftest the-view-mode-plays-in-real-time
  (let [frames (atom [])
        times  (atom [])]
    (with-redefs [ts/raf        (fn [f] (swap! frames conj f) (count @frames))
                  ts/cancel-af! (fn [_] nil)]
      (let [sub (rx/subs! #(swap! times conj %) (dv/frame-times 1000))]
        ;; animation frames at these times (ms), each scheduling the next
        (doseq [now [0 16 116 5116 5132]]
          ((peek @frames) now))
        (rx/dispose! sub)))
    (t/is (= [1016 1116 1616 1632] @times)
          "as far as the time that passed, a long pause (a hidden tab) 500 ms at most")))

;; SOUND

(defn- sounding-context
  "A fake running `AudioContext` whose current time (s) is `clock`, an
  atom: the starts and stops of its sources go to `log`, a start with
  its offset (s)."
  [clock log]
  (let [ctx #js {:state "running"
                 :destination #js {}
                 :createBufferSource
                 (fn []
                   #js {:connect (fn [_]) :disconnect (fn [])
                        :playbackRate #js {:value 1}
                        :start (fn [_ offset _] (swap! log conj [:start offset]))
                        :stop (fn [] (swap! log conj [:stop]))})
                 :createGain
                 (fn []
                   #js {:gain #js {:setValueAtTime (fn [_ _]) :linearRampToValueAtTime (fn [_ _])}
                        :connect (fn [_]) :disconnect (fn [])})}]
    (js/Object.defineProperty ctx "currentTime" #js {:get (fn [] @clock)})
    ctx))

(defn- viewer-state
  "A view of a board whose timeline of 3 s (of `playback`) plays a clip,
  2 s from 0 unless given, the time of its playback at `time`."
  [playback time & {:keys [start duration] :or {start 0 duration 2000}}]
  (let [page-id  (uuid/next)
        board-id (uuid/next)
        audio-id (uuid/next)
        n        96000]
    (audio/remember! audio-id #js {:duration 2 :length n :sampleRate 48000 :numberOfChannels 1
                                   :getChannelData (fn [_] (js/Float32Array. n))})
    {:board-id board-id
     :state {:route {:params {:query {:page-id (str page-id)}}}
             :viewer {:pages {page-id {:objects {}
                                       :timelines {board-id (-> (cta/make-timeline {:board-id board-id :duration 3000
                                                                                    :playback playback})
                                                                (ctau/add-audio-clip {:audio-id audio-id :name "x"
                                                                                      :source-duration 2000
                                                                                      :start start
                                                                                      :duration duration}))}}}}
             :viewer-local {:timeline {:time time}}}}))

(defn- play-frames
  "Play the board of `state` for frames at the times `seconds` of the
  context; what the sound did."
  [{:keys [state board-id]} seconds]
  (let [frames (atom [])
        clock  (atom (first seconds))
        log    (atom [])]
    (audio/use-context! (sounding-context clock log))
    (with-redefs [ts/raf        (fn [f] (swap! frames conj f) (count @frames))
                  ts/cancel-af! (fn [_] nil)]
      (let [sub (rx/subs! (fn [_]) (ptk/watch (dv/play-timeline board-id) state (rx/subject)))]
        (doseq [s seconds]
          (reset! clock s)
          ((peek @frames) 0))
        (rx/dispose! sub)))
    (audio/stop-all!)
    (audio/use-context! nil)
    @log))

(t/deftest playing-starts-the-sound-where-it-is
  (let [log (play-frames (viewer-state :once 500) [10 10.016])]
    (t/is (= [:start 0.516] (first log)) "500 ms in, and the frame after")))

(t/deftest the-way-back-of-ping-pong-plays-no-sound
  ;; a clip from 2 to 2.8 s, played from 2.9 s: to the end and back,
  ;; where the clip plays not
  (let [log (play-frames (viewer-state :ping-pong 2900 :start 2000 :duration 800) [10 10.016 10.2 10.9])]
    (t/is (empty? log))))

(t/deftest pausing-stops-the-sound
  (let [clock (atom 10)
        log   (atom [])
        {:keys [state]} (viewer-state :once 0)
        pages (get-in state [:viewer :pages])
        tl    (first (vals (:timelines (first (vals pages)))))]
    (audio/use-context! (sounding-context clock log))
    (audio/follow! tl 0 1 true)
    (t/is (= [[:start 0]] @log))
    (ptk/effect (dv/pause-timeline) state (rx/subject))
    (t/is (= [[:start 0] [:stop]] @log))
    (t/testing "showing a moment plays no sound"
      (reset! log [])
      (t/is (= [::dv/pause-timeline ::dv/seek-timeline]
               (let [types (atom [])]
                 (->> (ptk/watch (dv/show-moment (uuid/next) 500) state (rx/subject))
                      (rx/subs! #(swap! types conj (ptk/type %))))
                 @types)))
      (t/is (empty? @log)))
    (audio/use-context! nil)))

(t/deftest the-layers-of-an-animated-board-follow-it
  (cthi/reset-idmap!)
  (let [file    (-> (cthf/sample-file :file1)
                    (ctho/add-frame :board :x 0 :y 0 :width 400 :height 300)
                    (ctho/add-frame :arm :parent-label :board :x 100 :y 100 :width 80 :height 80)
                    (ctho/add-rect :moon :parent-label :arm :x 130 :y 90 :width 20 :height 20))
        objects (:objects (cthf/current-page file))
        shown   (vi/animated-objects objects {(cthi/id :arm) {:modifiers (ctm/move-modifiers (gpt/point 50 0))}})]
    (t/is (= 150 (get-in shown [(cthi/id :arm) :x])))
    (t/is (= 180 (get-in shown [(cthi/id :moon) :x])) "the layer of the board follows it")))
