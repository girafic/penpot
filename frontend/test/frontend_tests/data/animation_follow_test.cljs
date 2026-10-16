;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-follow-test
  (:require
   [app.common.files.changes :as cpc]
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.main.data.changes :as dch]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.selection :as dws]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before thi/reset-idmap!})

(defn- follow-file
  "Board A with a rect, the lead, moving right, and a dot."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :lead :type :rect :parent-label :board-a
                                       :x 40 :y 40 :width 40 :height 40)
                 (ths/add-sample-shape :dot :type :rect :parent-label :board-a
                                       :x 140 :y 50 :width 20 :height 20))
        tl   (-> (cta/make-timeline {:board-id (thi/id :board-a) :duration 1000})
                 (cta/add-keyframe (thi/id :lead) {:time 0 :property :x :value 40})
                 (cta/add-keyframe (thi/id :lead) {:time 1000 :property :x :value 140}))
        page (thf/current-page file)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- workspace-state
  [file]
  {:current-file-id (:id file)
   :current-page-id (:id (thf/current-page file))
   :files {(:id file) file}
   :permissions {:can-edit true}
   :workspace-layout #{:animation-timeline}
   :workspace-local {:selected #{(thi/id :dot)}}
   :workspace-animation {:board-id (thi/id :board-a)}})

(defn- emitted
  "The events `event` emits on `state`, as `stream` goes; none for a
  watch that gives nil."
  ([event state]
   (emitted event state (rx/empty)))
  ([event state stream]
   (let [events (atom [])]
     (some->> (ptk/watch event state stream)
              (rx/subs! #(swap! events conj %)))
     @events)))

(defn- committed-timeline
  "The timeline of board A once the commits `event` emits on `state` are
  applied to `file`; nil when it commits nothing."
  [event state file]
  (let [commits (->> (emitted event state)
                     (filter #(= ::dch/commit-changes (ptk/type %)))
                     (mapcat #(emitted % state))
                     (map deref))]
    (when (seq commits)
      (let [data (reduce #(cpc/process-changes %1 (:redo-changes %2) false) (:data file) commits)]
        (get-in data [:pages-index (:id (thf/current-page file)) :timelines (thi/id :board-a)])))))

(t/deftest a-layer-follows-its-leader-in-one-commit
  (let [file  (follow-file)
        state (workspace-state file)
        tl    (committed-timeline (dwa/set-follow (thi/id :dot) {:leader (thi/id :lead) :delay 100})
                                  state file)]
    (t/is (= {:leader (thi/id :lead) :delay 100} (get-in tl [:tracks (thi/id :dot) :follow])))
    (t/is (= 1 (count (filter #(= ::dch/commit-changes (ptk/type %))
                              (emitted (dwa/set-follow (thi/id :dot) {:leader (thi/id :lead)}) state))))
          "one undo step")))

(t/deftest a-layer-cannot-follow-its-board-nor-itself
  (let [file  (follow-file)
        state (workspace-state file)]
    (t/is (nil? (committed-timeline (dwa/set-follow (thi/id :dot) {:leader (thi/id :board-a)}) state file)))
    (t/is (nil? (committed-timeline (dwa/set-follow (thi/id :dot) {:leader (thi/id :dot)}) state file)))))

(t/deftest a-follow-changes-and-goes
  (let [file  (follow-file)
        page  (thf/current-page file)
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page page)
                                          (pcb/set-timeline (thi/id :board-a)
                                                            (cta/set-follow (get-in page [:timelines (thi/id :board-a)])
                                                                            (thi/id :dot) {:leader (thi/id :lead)}))))
        state (workspace-state file)
        [set] (emitted (dwa/update-follow (thi/id :dot) {:rotation false}) state)]
    (t/is (= {:leader (thi/id :lead) :rotation false}
             (get-in (committed-timeline set state file) [:tracks (thi/id :dot) :follow]))
          "what it takes of the leader")
    (t/is (nil? (get-in (committed-timeline (dwa/clear-follow (thi/id :dot)) state file)
                        [:tracks (thi/id :dot)]))
          "nothing left of its track")))

(t/deftest a-click-on-a-layer-picks-the-leader
  (let [file    (follow-file)
        state   (workspace-state file)
        stream  (rx/subject)
        events  (atom [])
        picking (ptk/update (dwa/pick-leader (thi/id :dot)) state)]
    (t/is (= (thi/id :dot) (dwa/picking-leader picking)))
    (->> (ptk/watch (dwa/pick-leader (thi/id :dot)) picking stream)
         (rx/subs! #(swap! events conj %)))
    (rx/push! stream (dws/select-shape (thi/id :lead)))
    (let [[end] @events
          ;; the click has selected the leader
          picked (-> (ptk/update end picking)
                     (assoc-in [:workspace-local :selected] #{(thi/id :lead)}))]
      (t/is (= ::dwa/end-pick-leader (ptk/type end)))
      (t/is (nil? (dwa/picking-leader picked)) "picking ends")
      (t/is (= [::dwa/set-follow ::dws/select-shape] (mapv ptk/type (emitted end picked)))
            "it follows the leader and stays selected"))))

(t/deftest a-click-on-nothing-cancels-the-pick
  (t/async done
    (let [state  (workspace-state (follow-file))
          stream (rx/subject)
          events (atom [])]
      (->> (ptk/watch (dwa/pick-leader (thi/id :dot)) state stream)
           (rx/subs! #(swap! events conj %)
                     (fn [cause] (t/is false (str cause)) (done))
                     (fn []
                       (let [[end] @events]
                         (t/is (= ::dwa/end-pick-leader (ptk/type end)))
                         (t/is (= [::dws/select-shape] (mapv ptk/type (emitted end state)))
                               "it follows nothing and stays selected"))
                       (done))))
      (rx/push! stream (dws/deselect-all)))))
