;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-ik-test
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

(defn- arm-file
  "Board A with an arm drawn bent clockwise: an upper arm (pivot 100,100),
  a forearm following it (pivot 200,130), a hand following that (pivot
  300,100); a ball and a rect that follows nothing."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :upper :type :rect :parent-label :board-a
                                       :x 100 :y 95 :width 100 :height 10)
                 (ths/add-sample-shape :fore :type :rect :parent-label :board-a
                                       :x 200 :y 125 :width 100 :height 10)
                 (ths/add-sample-shape :hand :type :rect :parent-label :board-a
                                       :x 300 :y 90 :width 20 :height 20)
                 (ths/add-sample-shape :ball :type :rect :parent-label :board-a
                                       :x 240 :y 140 :width 10 :height 10))
        tl   (-> (cta/make-timeline {:board-id (thi/id :board-a) :duration 1000})
                 (cta/set-track-origin (thi/id :upper) 0 0.5)
                 (cta/set-track-origin (thi/id :fore) 0 0.5)
                 (cta/set-track-origin (thi/id :hand) 0 0.5)
                 (cta/set-follow (thi/id :fore) {:leader (thi/id :upper)})
                 (cta/set-follow (thi/id :hand) {:leader (thi/id :fore)}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- workspace-state
  [file]
  {:current-file-id (:id file)
   :current-page-id (:id (thf/current-page file))
   :files {(:id file) file}
   :permissions {:can-edit true}
   :workspace-layout #{:animation-timeline}
   :workspace-local {:selected #{(thi/id :hand)}}
   :workspace-animation {:board-id (thi/id :board-a)}})

(defn- ^:async emitted
  "The events `event` emits on `state`, once its watch ends."
  [event state]
  (let [events (atom [])]
    (await (async/observe (ptk/watch event state (rx/empty)) :on-next #(swap! events conj %)))
    @events))

(defn- ^:async committed
  "The page data once the commits `event` emits on `state` are applied
  to `file`, with the events it emitted: `[data events]`."
  [event state file]
  (let [events  (await (emitted event state))
        commits (filter #(= ::dch/commit-changes (ptk/type %)) events)
        done    (await (js/Promise.all (into-array (map #(emitted % state) commits))))
        commits (into [] (comp (mapcat identity) (map deref)) done)
        data    (reduce #(cpc/process-changes %1 (:redo-changes %2) false) (:data file) commits)]
    [(get-in data [:pages-index (:id (thf/current-page file))]) events]))

(defn- ik-of
  [page]
  (cta/ik-of (get-in page [:timelines (thi/id :board-a)]) (thi/id :hand)))

(t/deftest ^:async a-hand-reaches-a-goal-bent-as-drawn
  (let [file      (arm-file)
        state     (workspace-state file)
        [page]    (await (committed (dwa/set-ik (thi/id :hand) {:goal (thi/id :ball)}) state file))
        [refused] (await (committed (dwa/set-ik (thi/id :hand) {:goal (thi/id :fore)}) state file))]
    (t/is (= {:goal (thi/id :ball) :bend :cw :turn :chain} (ik-of page)) "clockwise, as the arm is drawn")
    (t/is (nil? (ik-of refused)) "not a link of its own chain")))

(t/deftest ^:async a-reach-changes-and-goes
  (let [file    (arm-file)
        page    (thf/current-page file)
        file    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                            (pcb/with-page page)
                                            (pcb/set-timeline (thi/id :board-a)
                                                              (cta/set-ik (get-in page [:timelines (thi/id :board-a)])
                                                                          (thi/id :hand) {:goal (thi/id :ball)}))))
        state   (workspace-state file)
        [keep]  (await (emitted (dwa/update-ik (thi/id :hand) {:turn :keep}) state))
        [kept]  (await (committed keep state file))
        [gone]  (await (committed (dwa/clear-ik (thi/id :hand)) state file))
        [moved] (await (committed (dwa/set-ik (thi/id :hand) {:goal (thi/id :upper)}) state file))]
    (t/is (= {:goal (thi/id :ball) :bend :ccw :turn :keep} (ik-of kept)) "its bend kept")
    (t/is (nil? (ik-of gone)))
    (t/is (= (thi/id :ball) (:goal (ik-of moved))) "a link of its chain is no goal")))

(t/deftest ^:async a-new-goal-is-a-ring-on-the-hand
  (let [file          (arm-file)
        state         (workspace-state file)
        [page events] (await (committed (dwa/add-goal (thi/id :hand)) state file))
        tl            (get-in page [:timelines (thi/id :board-a)])
        {:keys [goal bend]} (ik-of page)
        ring          (get-in page [:objects goal])]
    (t/is (= :circle (:type ring)))
    (t/is (= (str (get-in page [:objects (thi/id :hand) :name]) " goal") (:name ring)) "named after the hand")
    (t/is (= [288 88] [(:x ring) (:y ring)]) "centred on the pivot of the hand")
    (t/is (= (thi/id :board-a) (:parent-id ring)))
    (t/is (cta/guide? tl goal) "left out of the exports")
    (t/is (= :cw bend))
    (t/is (= 1 (count (filter #(= ::dch/commit-changes (ptk/type %)) events))) "one undo step")
    (t/is (some #(= ::dws/select-shapes (ptk/type %)) events) "selected to be moved")))

(defn- picking-goal
  "The workspace on `arm-file` while the goal of the hand is picked."
  []
  (let [state (workspace-state (arm-file))]
    (ptk/update (dwa/pick-goal (thi/id :hand)) state)))

(t/deftest ^:async a-click-picks-the-goal
  (let [picking (picking-goal)
        [end]   (await (emitted (dwa/pick-leader-at (thi/id :ball)) picking))]
    (t/is (= (thi/id :hand) (dwa/picking-leader picking)))
    (t/is (= :goal (dwa/picking-purpose picking)))
    (t/is (= ::dwa/end-pick-leader (ptk/type end)))
    (t/is (= :leader (dwa/picking-purpose (ptk/update end picking))) "picking ends")
    (t/is (= [::dwa/set-ik ::dws/select-shape] (mapv ptk/type (await (emitted end (ptk/update end picking)))))
          "it reaches the layer clicked and stays selected")
    (t/is (empty? (await (emitted (dwa/pick-leader-at (thi/id :fore)) picking)))
          "a link of its chain waits for another click")))

(defn- reaching-file
  "`arm-file` with the hand reaching the ball as `ik` says, the forearm
  following the upper arm with `follow`."
  [ik follow]
  (let [file (arm-file)
        page (thf/current-page file)
        tl   (-> (get-in page [:timelines (thi/id :board-a)])
                 (cta/set-follow (thi/id :fore) (merge {:leader (thi/id :upper)} follow))
                 (cta/set-ik (thi/id :hand) (merge {:goal (thi/id :ball)} ik)))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(t/deftest ^:async a-broken-chain-keeps-the-settings-of-its-reach
  (let [file   (reaching-file {} {:delay 50})
        state  (workspace-state file)
        [set]  (await (emitted (dwa/update-ik (thi/id :hand) {:bend :cw :turn :keep}) state))
        [page] (await (committed set state file))]
    (t/is (= {:goal (thi/id :ball) :bend :cw :turn :keep} (ik-of page))
          "it reaches with them once the chain is mended")))

(t/deftest ^:async a-new-goal-keeps-the-bend-and-the-turn
  (let [file   (reaching-file {:bend :cw :turn :keep} {})
        state  (workspace-state file)
        [page] (await (committed (dwa/add-goal (thi/id :hand)) state file))
        ik     (ik-of page)]
    (t/is (not= (thi/id :ball) (:goal ik)) "a new ring")
    (t/is (= [:cw :keep] [(:bend ik) (:turn ik)]))))

