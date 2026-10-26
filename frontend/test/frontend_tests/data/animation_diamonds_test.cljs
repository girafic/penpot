;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-diamonds-test
  "The keyframe diamonds of the Design tab: a diamond keys only what the
  layer has (a group shows the menus of its layers)."
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

(defn- group-file
  "Board A with the group :group holding the dashed rectangle :dashed,
  and an empty timeline."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (tho/add-group :group :parent-label :board-a)
                 (ths/add-sample-shape :dashed :type :rect :parent-label :group
                                       :x 10 :y 10 :width 100 :height 60
                                       :fills [{:fill-color-gradient {:type :linear :start-x 0 :start-y 0 :end-x 1 :end-y 0 :width 1
                                                                      :stops [{:color "#ff0000" :opacity 1 :offset 0}
                                                                              {:color "#0000ff" :opacity 1 :offset 1}]}}]
                                       :strokes [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 2
                                                  :stroke-style :dashed :stroke-alignment :center}]))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline (thi/id :board-a)
                                                  (cta/make-timeline {:board-id (thi/id :board-a) :duration 1000}))))))

(defn- workspace-state
  [file selected]
  {:current-file-id (:id file)
   :current-page-id (:id (thf/current-page file))
   :files {(:id file) file}
   :permissions {:can-edit true}
   :workspace-layout #{:animation-timeline}
   :workspace-local {:selected #{selected}}
   :workspace-animation {:board-id (thi/id :board-a) :playhead 250}})

(defn- ^:async emitted
  [event state]
  (let [events (atom [])]
    (await (async/observe (ptk/watch event state (rx/empty)) :on-next #(swap! events conj %)))
    @events))

(defn- commits
  [events]
  (filter #(= ::dch/commit-changes (ptk/type %)) events))

(defn- ^:async committed
  "The page once the commits `event` emits on `state` are applied to
  `file`."
  [event state file]
  (let [events  (await (emitted event state))
        done    (await (js/Promise.all (into-array (map #(emitted % state) (commits events)))))
        changes (into [] (comp (mapcat identity) (map deref)) done)
        data    (reduce #(cpc/process-changes %1 (:redo-changes %2) false) (:data file) changes)]
    (get-in data [:pages-index (:id (thf/current-page file))])))

(t/deftest ^:async a-group-keys-none-of-the-paint-of-its-layers
  (let [file  (group-file)
        state (workspace-state file (thi/id :group))]
    (doseq [[properties index] [[[:stroke-dash :stroke-gap :stroke-dash-offset] 0]
                                [[:fill-gradient] 0]
                                [[:font-size] nil]]]
      (t/is (empty? (commits (await (emitted (dwa/toggle-keyframes (thi/id :group) properties index) state))))
            (str properties ": no keyframe of the group, nothing to save")))))

(t/deftest ^:async the-layer-itself-keys-them
  (let [file  (group-file)
        page  (await (committed (dwa/toggle-keyframes (thi/id :dashed) [:stroke-dash :stroke-gap] 0)
                                (workspace-state file (thi/id :dashed)) file))
        kfs   (get-in page [:timelines (thi/id :board-a) :tracks (thi/id :dashed) :keyframes])]
    (t/is (= #{[250 :stroke-dash 12] [250 :stroke-gap 12]}
             (into #{} (map (juxt :time :property :value)) kfs))
          "its dash and gap as it draws them")))
