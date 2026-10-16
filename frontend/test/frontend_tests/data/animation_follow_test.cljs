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
   [frontend-tests.helpers.async :as async]
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

(defn- ^:async emitted
  "The events `event` emits on `state`, once its watch ends."
  [event state]
  (let [events (atom [])]
    (await (async/observe (ptk/watch event state (rx/empty)) :on-next #(swap! events conj %)))
    @events))

(defn- ^:async committed-timeline
  "The timeline of board A once the commits `event` emits on `state` are
  applied to `file`; nil when it commits nothing."
  [event state file]
  (let [events  (await (emitted event state))
        commits (filter #(= ::dch/commit-changes (ptk/type %)) events)
        done    (await (js/Promise.all (into-array (map #(emitted % state) commits))))
        commits (into [] (comp (mapcat identity) (map deref)) done)]
    (when (seq commits)
      (let [data (reduce #(cpc/process-changes %1 (:redo-changes %2) false) (:data file) commits)]
        (get-in data [:pages-index (:id (thf/current-page file)) :timelines (thi/id :board-a)])))))

(defn- following-file
  "`follow-file` with the dot following the lead."
  []
  (let [file (follow-file)
        page (thf/current-page file)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a)
                                                  (cta/set-follow (get-in page [:timelines (thi/id :board-a)])
                                                                  (thi/id :dot) {:leader (thi/id :lead)}))))))

(t/deftest ^:async a-layer-follows-its-leader-in-one-commit
  (let [file   (follow-file)
        state  (workspace-state file)
        tl     (await (committed-timeline (dwa/set-follow (thi/id :dot) {:leader (thi/id :lead) :delay 100})
                                          state file))
        events (await (emitted (dwa/set-follow (thi/id :dot) {:leader (thi/id :lead)}) state))]
    (t/is (= {:leader (thi/id :lead) :delay 100} (get-in tl [:tracks (thi/id :dot) :follow])))
    (t/is (= 1 (count (filter #(= ::dch/commit-changes (ptk/type %)) events))) "one undo step")))

(t/deftest ^:async a-layer-cannot-follow-its-board-nor-itself
  (let [file  (follow-file)
        state (workspace-state file)]
    (t/is (nil? (await (committed-timeline (dwa/set-follow (thi/id :dot) {:leader (thi/id :board-a)}) state file))))
    (t/is (nil? (await (committed-timeline (dwa/set-follow (thi/id :dot) {:leader (thi/id :dot)}) state file))))))

(t/deftest ^:async a-follow-changes-and-goes
  (let [file    (following-file)
        state   (workspace-state file)
        follow  #(get-in % [:tracks (thi/id :dot) :follow])
        [parts] (await (emitted (dwa/update-follow (thi/id :dot) {:rotation false}) state))
        [rest]  (await (emitted (dwa/update-follow (thi/id :dot) {:from :rest}) state))]
    (t/is (= {:leader (thi/id :lead) :rotation false}
             (follow (await (committed-timeline parts state file))))
          "what it takes of the leader")
    (t/is (= {:leader (thi/id :lead) :from :rest}
             (follow (await (committed-timeline rest state file))))
          "from where the leader rests")
    (t/is (nil? (get-in (await (committed-timeline (dwa/clear-follow (thi/id :dot)) state file))
                        [:tracks (thi/id :dot)]))
          "nothing left of its track")))

(defn- picking
  "The pick of the leader of the dot on `state`, watched on `stream`:
  `[state promise events]`, the promise resolving once it ends."
  [state stream]
  (let [event  (dwa/pick-leader (thi/id :dot))
        events (atom [])
        ended  (async/observe (ptk/watch event (ptk/update event state) stream)
                              :on-next #(swap! events conj %))]
    [(ptk/update event state) ended events]))

(t/deftest ^:async a-layer-chosen-in-the-layers-leads
  (let [state  (workspace-state (follow-file))
        stream (rx/subject)
        [picking ended events] (picking state stream)]
    (t/is (= (thi/id :dot) (dwa/picking-leader picking)))
    (rx/push! stream (dws/select-shape (thi/id :lead)))
    (await ended)
    (let [[end] @events
          ;; it is selected in the layers
          picked (-> (ptk/update end picking)
                     (assoc-in [:workspace-local :selected] #{(thi/id :lead)}))]
      (t/is (= ::dwa/end-pick-leader (ptk/type end)))
      (t/is (nil? (dwa/picking-leader picked)) "picking ends")
      (t/is (= [::dwa/set-follow ::dws/select-shape] (mapv ptk/type (await (emitted end picked))))
            "it follows the leader and stays selected"))))

(defn- ^:async ended-by
  "The events the pick of the leader of the dot emits when `ending`
  comes, once it ends."
  [ending]
  (let [stream (rx/subject)
        [_ ended events] (picking (workspace-state (follow-file)) stream)]
    (rx/push! stream ending)
    (await ended)
    @events))

(t/deftest ^:async picking-again-or-cancelling-ends-the-pick
  (t/is (empty? (await (ended-by (dwa/pick-leader (thi/id :lead))))) "picked again: nothing picked")
  (t/is (empty? (await (ended-by (dwa/cancel-pick-leader)))) "cancelled: nothing picked"))

(t/deftest ^:async leaving-motion-mode-the-page-or-the-file-ends-the-pick
  (let [state  (workspace-state (follow-file))
        closed (await (ended-by (dwa/close-timeline)))
        paged  (await (ended-by (ptk/data-event :app.main.data.workspace.pages/finalize-page {})))
        filed  (await (ended-by (ptk/data-event :app.main.data.workspace/finalize-workspace {})))]
    (t/is (= [::dwa/end-pick-leader] (mapv ptk/type closed)))
    (t/is (= [::dwa/end-pick-leader] (mapv ptk/type paged)))
    (t/is (= [::dwa/end-pick-leader] (mapv ptk/type filed)))
    (t/is (nil? (dwa/picking-leader (ptk/update (first closed) state))) "no pick left")
    (t/is (empty? (await (emitted (first closed) state))) "nothing picked, nothing selected")))

(t/deftest ^:async a-pick-selects-no-layer-gone-from-the-page
  (let [file   (follow-file)
        page   (thf/current-page file)
        state  (update-in (workspace-state file)
                          [:files (:id file) :data :pages-index (:id page) :objects] dissoc (thi/id :dot))
        stream (rx/subject)
        [_ ended events] (picking state stream)]
    (rx/push! stream (dws/select-shape (thi/id :lead)))
    (await ended)
    (t/is (empty? (await (emitted (first @events) state))))))

(t/deftest ^:async only-the-keyframe-exports-bake-follows
  (let [file  (following-file)
        page  (thf/current-page file)
        files {(:id file) file}
        board (thi/id :board-a)
        dot   (thi/id :dot)
        kept  (dwa/board-export-timeline page files board)
        baked (dwa/board-export-timeline page files board :bake true)]
    (t/is (some? (get-in kept [:tracks dot :follow])) "the video exports play the follow")
    (t/is (nil? (get-in baked [:tracks dot :follow])))
    (t/is (some #(= :x (:property %)) (get-in baked [:tracks dot :keyframes])) "SVG, Lottie and CSS read keyframes")
    (await (async/settle))))

(defn- picking-state
  "The workspace on `follow-file` while the leader of the dot is picked."
  []
  (let [state (workspace-state (follow-file))]
    (ptk/update (dwa/pick-leader (thi/id :dot)) state)))

(t/deftest ^:async a-click-on-the-canvas-picks-the-leader
  (let [picking (picking-state)
        [end]   (await (emitted (dwa/pick-leader-at (thi/id :lead)) picking))]
    (t/is (= ::dwa/end-pick-leader (ptk/type end)))
    (t/is (nil? (dwa/picking-leader (ptk/update end picking))) "picking ends")
    (t/is (= [::dwa/set-follow ::dws/select-shape]
             (mapv ptk/type (await (emitted end (ptk/update end picking)))))
          "it follows the layer clicked and stays selected")))

(t/deftest ^:async a-click-on-nothing-or-a-layer-it-cannot-follow-waits-for-another
  (let [picking (picking-state)
        state   (update picking :workspace-animation dissoc :picking-leader)]
    (t/is (empty? (await (emitted (dwa/pick-leader-at nil) picking)))
          "nothing, or a layer the canvas does not know under the pointer yet")
    (t/is (empty? (await (emitted (dwa/pick-leader-at (thi/id :board-a)) picking))) "its board")
    (t/is (empty? (await (emitted (dwa/pick-leader-at (thi/id :lead)) state))) "nothing without a pick")))

(t/deftest ^:async esc-cancels-the-pick
  (let [picking (picking-state)
        stream  (rx/subject)
        event   (dwa/pick-leader (thi/id :dot))
        events  (atom [])
        ended   (async/observe (ptk/watch event picking stream) :on-next #(swap! events conj %))]
    (rx/push! stream (dws/deselect-all))
    (await ended)
    (t/is (= [::dwa/end-pick-leader] (mapv ptk/type @events)))
    (t/is (= [::dws/select-shape] (mapv ptk/type (await (emitted (first @events) picking))))
          "it follows nothing and stays selected")))

