;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-morph-test
  "The outline of a path as keyframes (path morphing) in the workspace."
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

(def ^:private triangle
  (path/from-plain [{:command :move-to :params {:x 20 :y 30}}
                    {:command :line-to :params {:x 60 :y 30}}
                    {:command :line-to :params {:x 20 :y 70}}
                    {:command :close-path :params {}}]))

(defn- morph-file
  "Board A with the path :blob (a triangle, box 40 by 40 at 20, 30) and an
  empty timeline."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :blob :type :path :parent-label :board-a :content triangle))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline (thi/id :board-a)
                                                  (cta/make-timeline {:board-id (thi/id :board-a) :duration 1000}))))))

(defn- workspace-state
  [file playhead]
  {:current-file-id (:id file)
   :current-page-id (:id (thf/current-page file))
   :files {(:id file) file}
   :permissions {:can-edit true}
   :workspace-layout #{:animation-timeline}
   :workspace-local {:selected #{(thi/id :blob)}}
   :workspace-animation {:board-id (thi/id :board-a) :playhead playhead}})

(defn- ^:async emitted
  [event state]
  (let [events (atom [])]
    (await (async/observe (ptk/watch event state (rx/empty)) :on-next #(swap! events conj %)))
    @events))

(defn- ^:async committed
  "The page once the commits `event` emits on `state` are applied to
  `file`."
  [event state file]
  (let [events  (await (emitted event state))
        commits (filter #(= ::dch/commit-changes (ptk/type %)) events)
        done    (await (js/Promise.all (into-array (map #(emitted % state) commits))))
        commits (into [] (comp (mapcat identity) (map deref)) done)
        data    (reduce #(cpc/process-changes %1 (:redo-changes %2) false) (:data file) commits)]
    (get-in data [:pages-index (:id (thf/current-page file))])))

(defn- with-page
  [file page]
  (assoc-in file [:data :pages-index (:id page)] page))

(t/deftest ^:async the-diamond-of-a-path-keeps-its-outline
  (let [file  (morph-file)
        page  (await (committed (dwa/toggle-keyframes (thi/id :blob) [:content]) (workspace-state file 250) file))
        track (get-in page [:timelines (thi/id :board-a) :tracks (thi/id :blob)])
        blob  (get-in page [:objects (thi/id :blob)])]
    (t/is (= [[250 :content]] (mapv (juxt :time :property) (:keyframes track))))
    (t/is (= (str (cta/outline-of blob)) (str (:value (first (:keyframes track))))) "its outline as it is")
    (t/is (= (cta/outline-box blob) (:outline-box track)) "for its box")
    (let [file  (with-page file page)
          again (await (committed (dwa/toggle-keyframes (thi/id :blob) [:content]) (workspace-state file 250) file))]
      (t/is (empty? (get-in again [:timelines (thi/id :board-a) :tracks (thi/id :blob) :keyframes]))
            "the diamond again takes it away"))))
