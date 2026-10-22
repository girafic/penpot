;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-wiggle-test
  "The wiggles of the workspace: set, changed, shaken another way and
  removed, each in one undo step."
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
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.async :as async]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before thi/reset-idmap!})

(defn- wiggle-file
  "Board A with a dot, and a timeline, its wiggles in `wiggles`."
  [& wiggles]
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :dot :type :rect :parent-label :board-a
                                       :x 140 :y 50 :width 20 :height 20))
        tl   (reduce #(cta/set-wiggle %1 (thi/id :dot) %2)
                     (cta/make-timeline {:board-id (thi/id :board-a) :duration 1000})
                     wiggles)
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

(defn- ^:async committed
  "The wiggles of the dot once the commits `event` emits (and those of
  the events it emits) are applied to `file`, and how many commits:
  `[wiggles commits]`."
  [event file]
  (let [state  (workspace-state file)
        ;; a change goes through `dwa/set-wiggle`, which commits
        expand (fn expand [event]
                 (-> (emitted event state)
                     (.then (fn [events]
                              (js/Promise.all
                               (into-array (map #(if (= ::dwa/set-wiggle (ptk/type %))
                                                   (expand %)
                                                   (js/Promise.resolve [%]))
                                                events)))))
                     (.then #(into [] (mapcat identity) %))))
        events  (await (expand event))
        commits (filter #(= ::dch/commit-changes (ptk/type %)) events)
        done    (await (js/Promise.all (into-array (map #(emitted % state) commits))))
        changes (into [] (comp (mapcat identity) (map deref)) done)
        data    (reduce #(cpc/process-changes %1 (:redo-changes %2) false) (:data file) changes)
        tl      (get-in data [:pages-index (:id (thf/current-page file)) :timelines (thi/id :board-a)])]
    [(cta/wiggles-of tl (thi/id :dot)) (count commits)]))

(t/deftest ^:async a-wiggle-is-set-in-one-commit
  ;; the file first: it gives the dot its id
  (let [file              (wiggle-file)
        [wiggles commits] (await (committed (dwa/set-wiggle (thi/id :dot) (dwa/new-wiggle :rotation)) file))]
    (t/is (= 1 commits) "one undo step")
    (t/is (= [:rotation] (mapv :property wiggles)))
    (t/is (= 10 (:amplitude (first wiggles))) "ten degrees")
    (t/is (pos? (:seed (first wiggles))))))

(t/deftest ^:async a-wiggle-changes-in-place
  (let [file              (wiggle-file {:property :x :frequency 2 :amplitude 10 :seed 7}
                                       {:property :rotation :frequency 2 :amplitude 10 :seed 8})
        [moved commits]   (await (committed (dwa/update-wiggle (thi/id :dot) :x {:property :opacity :amplitude 0.2})
                                            file))
        [shook]           (await (committed (dwa/shuffle-wiggle (thi/id :dot) :rotation) file))
        [gone]            (await (committed (dwa/remove-wiggle (thi/id :dot) :x) file))]
    (t/is (= 1 commits))
    (t/is (= [:opacity :rotation] (mapv :property moved)) "another property, in its place")
    (t/is (= 7 (:seed (first moved))) "shaking as it did")
    (t/is (not= 8 (:seed (second shook))) "another way")
    (t/is (= [:rotation] (mapv :property gone)))))
