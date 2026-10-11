;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.comments-moments-test
  "Comments about a moment of the animation of a board (Penpot Motion),
  like the time-stamped comments of Figma."
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.time :as ct]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.data.comments :as dcmt]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.comments :as dwcm]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.viewport :as dwv]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before thi/reset-idmap!})

(defn- animated-file
  "Board A at 100,100 with a rect moving right, and board B at 1000,100
  that plays nothing."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 100 :y 100 :width 400 :height 300)
                 (ths/add-sample-shape :rect :type :rect :parent-label :board-a
                                       :x 150 :y 200 :width 50 :height 50)
                 (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300))
        tl   (-> (cta/make-timeline {:board-id (thi/id :board-a) :duration 1500})
                 (cta/add-keyframe (thi/id :rect) {:time 0 :property :x :value 50})
                 (cta/add-keyframe (thi/id :rect) {:time 1000 :property :x :value 150}))
        page (thf/current-page file)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- workspace-state
  "The workspace on `file`, in motion mode with the timeline on board A
  and its playhead at 1200 ms."
  [file]
  {:current-file-id (:id file)
   :current-page-id (:id (thf/current-page file))
   :files {(:id file) file}
   :workspace-layout #{:animation-timeline}
   :workspace-animation {:board-id (thi/id :board-a) :playhead 1200}})

(defn- emitted
  "The types of the events `event` emits on `state`."
  [event state]
  (let [types (atom [])]
    (->> (ptk/watch event state (rx/empty))
         (rx/subs! #(swap! types conj (ptk/type %))))
    @types))

(t/deftest a-comment-in-motion-mode-is-about-the-moment-at-the-playhead
  (let [state (workspace-state (animated-file))]
    (t/is (= 1200 (dwcm/moment-at state (gpt/point 300 200)))
          "on the board the timeline shows")
    (t/is (nil? (dwcm/moment-at (update state :workspace-layout disj :animation-timeline)
                                (gpt/point 300 200)))
          "not out of motion mode")
    (t/is (nil? (dwcm/moment-at state (gpt/point 1200 200)))
          "not on another board")
    (t/is (nil? (dwcm/moment-at (assoc-in state [:workspace-animation :board-id] (thi/id :board-b))
                                (gpt/point 1200 200)))
          "not on a board that plays nothing")
    (t/is (nil? (dwcm/moment-at state (gpt/point 700 700)))
          "not out of the boards")))

(t/deftest opening-a-thread-shows-its-moment
  (let [file   (animated-file)
        state  (workspace-state file)
        thread {:id (random-uuid)
                :page-id (:id (thf/current-page file))
                :frame-id (thi/id :board-a)
                :animation-time 400}]
    (t/is (= [::dwa/pause ::dwa/set-active-board ::dwa/set-playhead]
             (emitted (dwcm/show-moment thread 400) state))
          "in motion mode, the playhead goes to it")
    (t/is (= [::dwa/toggle-motion-mode ::dwa/pause ::dwa/set-active-board ::dwa/set-playhead]
             (emitted (dwcm/show-moment thread 400)
                      (update state :workspace-layout disj :animation-timeline)))
          "a click on it enters motion mode first")
    (t/is (= []
             (emitted (dwcm/show-moment thread 400 false)
                      (update state :workspace-layout disj :animation-timeline)))
          "opening it out of motion mode leaves it")
    (t/is (= [::dws/select-shapes ::dwa/pause ::dwa/set-active-board ::dwa/set-playhead]
             (emitted (dwcm/show-moment thread 400)
                      (-> state
                          (assoc-in [:workspace-local :selected] #{(thi/id :board-b)}))))
          "a selection on another board is dropped, the timeline goes to the board")
    (t/is (= []
             (emitted (dwcm/show-moment (assoc thread :frame-id uuid/zero) 400) state))
          "nothing for a thread out of the boards")))

(defn- thread-on
  "A comment thread at 300,200 on board A of `file`, about 400 ms."
  [file]
  {:id (random-uuid)
   :page-id (:id (thf/current-page file))
   :file-id (:id file)
   :project-id (random-uuid)
   :owner-id (random-uuid)
   :file-name "file"
   :seqn 1
   :content "here"
   :participants #{}
   :created-at (ct/now)
   :modified-at (ct/now)
   :position (gpt/point 300 200)
   :animation-time 400})

(t/deftest opening-a-thread-from-the-maximized-timeline-docks-it
  (let [file   (animated-file)
        state  (workspace-state file)
        thread (thread-on file)
        docks? #(= ::dwa/dock-timeline (first (emitted (dwcm/open-thread-moment thread) %)))]
    (t/is (docks? (assoc-in state [:workspace-animation :maximized?] true))
          "the thread shows on the canvas the timeline covered")
    (t/is (not (docks? state)) "docked, it stays")))

(t/deftest a-thread-opened-from-the-maximized-timeline-shows-on-the-docked-canvas
  ;; The canvas in its small window shows the thread, but docked it goes
  ;; back where it was before, where the thread is out of view: it moves
  ;; to the thread then.
  (t/async done
    (let [file   (animated-file)
          thread (thread-on file)
          state  (-> (workspace-state file)
                     (assoc :workspace-local {:vbox (grc/make-rect 100 100 400 300)
                                              :vport {:width 400 :height 300}
                                              :zoom 1 :zoom-inverse 1})
                     (assoc-in [:workspace-animation :maximized?] true)
                     (assoc-in [:workspace-animation :restore-view]
                               {:vbox (grc/make-rect 1000 1000 800 600) :zoom 1 :zoom-inverse 1}))
          dock   (first (filter #(= ::dwa/dock-timeline (ptk/type %))
                                (let [events (atom [])]
                                  (->> (ptk/watch (dwcm/open-thread-moment thread) state (rx/empty))
                                       (rx/subs! #(swap! events conj %)))
                                  @events)))
          resize (dwv/update-viewport-size :both {:width 800 :height 600})
          ;; as the store goes: the dock, the canvas taking its size
          state' (->> (ptk/update dock state) (ptk/update resize))
          stream (rx/subject)
          events (atom [])]
      (->> (ptk/watch dock (ptk/update dock state) stream)
           (rx/subs! #(swap! events conj %)
                     (fn [cause] (t/is false (str cause)) (done))
                     (fn []
                       (let [[restore then] @events
                             restored (ptk/update restore state')
                             centred  (reduce #(ptk/update %2 %1) restored
                                              (let [out (atom [])]
                                                (->> (ptk/watch then restored (rx/empty))
                                                     (rx/subs! #(swap! out conj %)))
                                                @out))
                             vbox     (get-in centred [:workspace-local :vbox])]
                         (t/is (not (grc/contains-point? (get-in restored [:workspace-local :vbox])
                                                         (:position thread)))
                               "back where it was, the thread out of view")
                         (t/is (grc/contains-point? vbox (:position thread)) "then it shows"))
                       (done))))
      (rx/push! stream resize))))

(t/deftest timestamps-in-comments-read-as-their-labels
  (let [content (str "Here " (dcmt/timestamp-token 1250 "1.25 s")
                     " @[Ann](6c5c9d2a-3e8f-8173-8005-8a2f2b35b5a1)")]
    (t/is (= "Here @[1.25 s](time:1250) @[Ann](6c5c9d2a-3e8f-8173-8005-8a2f2b35b5a1)" content)
          "written like mentions")
    (t/is (= 1250 (dcmt/timestamp-time "time:1250")))
    (t/is (nil? (dcmt/timestamp-time "time:-5")) "only moments from the start on")
    (t/is (nil? (dcmt/timestamp-time "6c5c9d2a-3e8f-8173-8005-8a2f2b35b5a1")))
    (t/is (= [(uuid "6c5c9d2a-3e8f-8173-8005-8a2f2b35b5a1")]
             (dcmt/extract-mentions content))
          "they mention no one")))
