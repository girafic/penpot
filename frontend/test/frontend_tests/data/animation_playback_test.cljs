;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-playback-test
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.animation :as dwa]
   [cljs.test :as t :include-macros true]))

(defn- steps
  "How far playback moves on in frames `dts` ms apart."
  [dts]
  (first (reduce (fn [[steps carry] dt]
                   (let [[step carry] (dwa/frame-step carry dt)]
                     [(conj steps step) carry]))
                 [[] 0]
                 dts)))

(t/deftest playback-keeps-to-the-time-that-passes
  (t/testing "a slow frame moves on by all the time it took"
    (t/is (= [16 210 16] (steps [16 210 16]))))
  (t/testing "the fractions of a ms carry over"
    (let [steps (steps (repeat 60 16.5))]
      (t/is (every? #{16 17} steps))
      (t/is (= 990 (reduce + steps)))))
  (t/testing "back in a hidden tab, it goes on where it was"
    (t/is (= [500] (steps [60000]))))
  (t/is (= [0] (steps [-5])) "a clock going back does not"))

(t/deftest the-dock-rests-while-playing
  (let [anim {:playing? true :playhead 1234 :play-start 500 :direction -1
              :preview {:time 1234} :selected-kfs #{:a}}]
    (t/is (= {:playing? true :playhead 500 :play-start 500 :selected-kfs #{:a}}
             (dwa/dock-state anim))
          "at the playhead it started from")
    (t/is (= (dwa/dock-state anim)
             (dwa/dock-state (assoc anim :playhead 2000 :preview {:time 2000})))
          "the same as it plays on")
    (t/is (= {:playing? false :playhead 1234}
             (dwa/dock-state (-> anim
                                 (assoc :playing? false)
                                 (dissoc :play-start :selected-kfs))))
          "and where it is once it stops")))

(t/deftest the-panels-show-where-playback-started
  (let [state {:workspace-animation {:playing? true :playhead 1234 :play-start 500}}]
    (t/is (= 500 (dwa/shown-playhead state)))
    (t/is (= 1234 (dwa/shown-playhead (assoc-in state [:workspace-animation :playing?] false))))
    (t/is (= 0 (dwa/shown-playhead {})))))

(t/deftest only-the-board-that-plays-shows-its-animation
  (let [file-id (uuid/next)
        page-id (uuid/next)
        a       (uuid/next)
        b       (uuid/next)
        state   {:current-file-id file-id
                 :current-page-id page-id
                 :workspace-layout #{:animation-timeline}
                 :workspace-animation {:preview {:board-id a :time 500}}
                 :files {file-id {:data {:pages-index {page-id {:timelines {a {:board-id a}
                                                                            b {:board-id b}}}}}}}}]
    (t/is (= {:timelines {a {:board-id a}} :board-id a :time 500}
             (dsh/lookup-animation-preview state))
          "the other boards stay as they are")
    (t/is (nil? (dsh/lookup-animation-preview (update state :workspace-layout disj :animation-timeline)))
          "none outside motion mode")))
