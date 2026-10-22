;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-path-test
  "The rides of the workspace: a layer set riding a path, changed and
  riding none again, each in one undo step, a path picked on the canvas,
  and one it cannot ride refused."
  (:require
   [app.common.files.changes :as cpc]
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.common.types.path :as path]
   [app.main.data.changes :as dch]
   [app.main.data.workspace.animation :as dwa]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.async :as async]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before thi/reset-idmap!})

(defn- path-file
  "Board A with a dot, a road (a path), a lead and a group holding a rect,
  and a timeline that `f` makes of an empty one."
  [f]
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :dot :type :rect :parent-label :board-a
                                       :x 140 :y 50 :width 20 :height 20)
                 (ths/add-sample-shape :road :type :path :parent-label :board-a
                                       :content (path/from-string "M 10 200 L 390 200"))
                 (ths/add-sample-shape :lead :type :rect :parent-label :board-a
                                       :x 300 :y 50 :width 20 :height 20)
                 (tho/add-group :group :parent-label :board-a)
                 (ths/add-sample-shape :inner :type :rect :parent-label :group
                                       :x 40 :y 40 :width 20 :height 20))
        tl   (f (cta/make-timeline {:board-id (thi/id :board-a) :duration 1000}))
        page (thf/current-page file)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- riding
  "A timeline `tl` with the dot riding the road, its progress keyframed."
  [tl]
  (-> tl
      (cta/set-path-follow (thi/id :dot) {:path (thi/id :road)})
      (cta/add-keyframe (thi/id :dot) {:time 0 :property :path-progress :value 0})
      (cta/add-keyframe (thi/id :dot) {:time 1000 :property :path-progress :value 1})))

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

(def ^:private expanded
  "The events whose own events count: those that end in a commit."
  #{::dwa/set-path-follow ::dwa/update-path-follow ::dwa/pick-leader-at ::dwa/end-pick-leader})

(defn- ^:async committed
  "The timeline of board A once the commits `event` emits on `state` (and
  those of the events it emits) are applied to `file`, and how many
  commits: `[timeline commits]`."
  [event file state]
  (let [expand  (fn expand [event]
                  (-> (emitted event state)
                      (.then (fn [events]
                               (js/Promise.all
                                (into-array (map #(if (contains? expanded (ptk/type %))
                                                    (expand %)
                                                    (js/Promise.resolve [%]))
                                                 events)))))
                      (.then #(into [] (mapcat identity) %))))
        events  (await (expand event))
        commits (filter #(= ::dch/commit-changes (ptk/type %)) events)
        done    (await (js/Promise.all (into-array (map #(emitted % state) commits))))
        changes (into [] (comp (mapcat identity) (map deref)) done)
        data    (reduce #(cpc/process-changes %1 (:redo-changes %2) false) (:data file) changes)]
    [(get-in data [:pages-index (:id (thf/current-page file)) :timelines (thi/id :board-a)])
     (count commits)]))

(t/deftest ^:async a-layer-rides-a-path-in-one-commit
  ;; the file first: it gives the shapes their ids
  (let [file          (path-file #(cta/set-follow % (thi/id :dot) {:leader (thi/id :lead)}))
        [tl commits]  (await (committed (dwa/set-path-follow (thi/id :dot) {:path (thi/id :road) :orient true})
                                        file (workspace-state file)))]
    (t/is (= 1 commits) "one undo step")
    (t/is (= {:path (thi/id :road) :orient true :turn 0 :progress 0} (cta/path-follow-of tl (thi/id :dot))))
    (t/is (nil? (cta/follow-of tl (thi/id :dot))) "following its lead no more")))

(t/deftest ^:async a-path-it-cannot-ride-is-refused
  (let [file         (path-file identity)
        state        (workspace-state file)
        [tl commits] (await (committed (dwa/set-path-follow (thi/id :dot) {:path (thi/id :group)}) file state))]
    (t/is (= 0 commits) "a group has no outline")
    (t/is (nil? (cta/path-follow-of tl (thi/id :dot))))
    (t/is (= 0 (second (await (committed (dwa/set-path-follow (thi/id :dot) {:path (thi/id :board-a)})
                                         file state))))
          "nor its board")))

(t/deftest ^:async a-ride-changes-and-goes
  (let [file          (path-file riding)
        state         (workspace-state file)
        [turned n]    (await (committed (dwa/update-path-follow (thi/id :dot) {:orient true :turn 90}) file state))
        [gone m]      (await (committed (dwa/clear-path-follow (thi/id :dot)) file state))]
    (t/is (= 1 n))
    (t/is (= {:path (thi/id :road) :orient true :turn 90 :progress 0} (cta/path-follow-of turned (thi/id :dot))))
    (t/is (= 2 (count (filter #(= :path-progress (:property %))
                              (get-in turned [:tracks (thi/id :dot) :keyframes]))))
          "its progress as it was")
    (t/is (= 1 m))
    (t/is (nil? (cta/path-follow-of gone (thi/id :dot))))
    (t/is (nil? (get-in gone [:tracks (thi/id :dot)])) "nor the keyframes of its progress")))

(t/deftest ^:async a-path-is-picked-on-the-canvas
  (let [file         (path-file identity)
        state        (ptk/update (dwa/pick-path (thi/id :dot)) (workspace-state file))
        _            (t/is (= :path (dwa/picking-purpose state)))
        [tl commits] (await (committed (dwa/pick-leader-at (thi/id :road)) file state))
        [_ none]     (await (committed (dwa/pick-leader-at (thi/id :group)) file state))]
    (t/is (= 1 commits))
    (t/is (= (thi/id :road) (:path (cta/path-follow-of tl (thi/id :dot)))))
    (t/is (= 0 none) "a click on what it cannot ride leaves the pick waiting")))
