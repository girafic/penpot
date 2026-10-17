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
   [app.common.geom.point :as gpt]
   [app.common.logic.timelines :as cltl]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.path :as path]
   [app.common.types.path.morph :as morph]
   [app.main.data.changes :as dch]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.path.changes :as dwpc]
   [app.util.i18n :refer [tr]]
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
    (t/is (= (str (ctao/outline-of blob)) (str (:value (first (:keyframes track))))) "its outline as it is")
    (t/is (= (ctao/outline-box blob) (:outline-box track)) "for its box")
    (let [file  (with-page file page)
          again (await (committed (dwa/toggle-keyframes (thi/id :blob) [:content]) (workspace-state file 250) file))]
      (t/is (empty? (get-in again [:timelines (thi/id :board-a) :tracks (thi/id :blob) :keyframes]))
            "the diamond again takes it away"))))

(def ^:private flat
  (path/from-plain [{:command :move-to :params {:x 0 :y 0}}
                    {:command :line-to :params {:x 40 :y 0}}
                    {:command :line-to :params {:x 40 :y 20}}
                    {:command :close-path :params {}}]))

(defn- morphing-file
  "`morph-file` with the outline of :blob going from itself (at 0) to a
  flat triangle (at 2000) and :blob moving 100 to the right meanwhile."
  []
  (let [file  (morph-file)
        page  (thf/current-page file)
        blob  (get-in page [:objects (thi/id :blob)])
        id    (:id blob)
        tl    (-> (get-in page [:timelines (thi/id :board-a)])
                  (assoc :duration 2000)
                  (cta/set-outline-at blob 0 (ctao/outline-of blob))
                  (cta/set-outline-at blob 2000 flat)
                  (cta/add-keyframe id {:time 0 :property :x :value 20})
                  (cta/add-keyframe id {:time 2000 :property :x :value 120}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- shown-outline
  [page time]
  (let [tl (get-in page [:timelines (thi/id :board-a)])]
    (str (:content (get (cltl/shown-shapes {(thi/id :board-a) tl} (:objects page) time) (thi/id :blob))))))

(t/deftest an-edit-in-motion-mode-starts-from-the-outline-at-the-playhead
  (let [file  (morphing-file)
        state (workspace-state file 1000)
        blob  (get-in (thf/current-page file) [:objects (thi/id :blob)])
        {:keys [time content]} (dwa/outline-edit state (thi/id :blob))]
    (t/is (= 1000 time))
    (t/is (not= (str (:content blob)) (str content)) "the outline between them")
    (t/is (nil? (dwa/outline-edit (assoc state :workspace-layout #{}) (thi/id :blob))) "not outside motion mode")
    (t/is (nil? (dwa/outline-edit (workspace-state (morph-file) 1000) (thi/id :blob)))
          "nor on a path without a Path row and auto-keyframe off")
    (t/is (some? (dwa/outline-edit (assoc-in (workspace-state (morph-file) 1000) [:workspace-animation :auto-keyframe?] true)
                                   (thi/id :blob)))
          "with it on")))

(t/deftest ^:async an-edit-at-the-playhead-keeps-its-outline-and-the-rest
  (let [file    (morphing-file)
        before  (thf/current-page file)
        blob    (get-in before [:objects (thi/id :blob)])
        ;; the outline at 1 s, grown to the left by 30
        edited  (path/move-content (:content (dwa/outline-edit (workspace-state file 1000) (thi/id :blob)))
                                   (gpt/point -30 0))
        after   (await (committed (dwa/set-outline-at-playhead (thi/id :blob) edited) (workspace-state file 1000) file))
        row     (filter #(= :content (:property %))
                        (get-in after [:timelines (thi/id :board-a) :tracks (thi/id :blob) :keyframes]))]
    (t/is (= [0 1000 2000] (mapv :time row)))
    (t/is (= (str (:content blob)) (str (get-in after [:objects (thi/id :blob) :content]))) "the path at rest as it was")
    (t/is (= (shown-outline before 0) (shown-outline after 0)) "the outline at 0 where it was")
    (t/is (= (shown-outline before 2000) (shown-outline after 2000)) "and at 2 s")
    (let [tl (get-in after [:timelines (thi/id :board-a)])
          dx (- (get-in (cta/values-at (cta/resolve-animations tl (:objects after)) 1000) [(thi/id :blob) :x])
                (get-in blob [:selrect :x]))
          at (:content (get (cltl/shown-shapes {(thi/id :board-a) tl} (:objects after) 1000) (thi/id :blob)))]
      (t/is (morph/close? (path/move-content edited (gpt/point dx 0)) at 0.01)
            "at 1 s, the edited outline where the path is then: the nodes left alone stay"))))

(t/deftest ^:async a-first-edit-with-auto-keyframe-keeps-the-outline-before-at-0
  (let [file  (morph-file)
        blob  (get-in (thf/current-page file) [:objects (thi/id :blob)])
        state (assoc-in (workspace-state file 500) [:workspace-animation :auto-keyframe?] true)
        page  (await (committed (dwa/set-outline-at-playhead (thi/id :blob) (path/move-content (:content blob) (gpt/point 0 10)))
                                state file))
        row   (get-in page [:timelines (thi/id :board-a) :tracks (thi/id :blob)])]
    (t/is (= [0 500] (mapv :time (:keyframes row))))
    (t/is (= (str (ctao/outline-of blob)) (str (:value (first (:keyframes row))))))
    (t/is (= (ctao/outline-box blob) (:outline-box row)))))

(t/deftest ^:async finishing-an-edit-in-motion-mode-keeps-a-keyframe
  (let [file   (morphing-file)
        id     (thi/id :blob)
        state  (-> (workspace-state file 1000)
                   (assoc-in [:workspace-local :edit-path id] {:old-content triangle :outline-edit true}))
        events (await (emitted (dwpc/finalize-path-content id)
                               (assoc-in state [:workspace-drawing :object :content] flat)))
        same   (await (emitted (dwpc/finalize-path-content id)
                               (assoc-in state [:workspace-drawing :object :content] triangle)))
        plain  (await (emitted (dwpc/finalize-path-content id)
                               (-> state
                                   (assoc-in [:workspace-local :edit-path id :outline-edit] false)
                                   (assoc-in [:workspace-drawing :object :content] flat))))]
    (t/is (= [::dwa/set-outline-at-playhead] (mapv ptk/type events)) "a keyframe, the path left as it is")
    (t/is (empty? same) "nothing when nothing changed")
    (t/is (not= [::dwa/set-outline-at-playhead] (mapv ptk/type plain)) "outside, the path changes")))

(defn- with-row-flag
  "`file` with the Path row of :blob `flag`ged (`:locked` or `:hidden`)."
  [file flag]
  (let [page (thf/current-page file)
        tl   (cta/toggle-slot-flag (get-in page [:timelines (thi/id :board-a)]) (thi/id :blob) flag :content nil)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- commits
  [events]
  (filter #(= ::dch/commit-changes (ptk/type %)) events))

(defn- warned?
  [events]
  (some #(= ::ntf/show (ptk/type %)) events))

(t/deftest ^:async a-locked-or-hidden-path-row-keeps-its-outline
  (doseq [flag [:locked :hidden]]
    (let [file  (with-row-flag (morphing-file) flag)
          state (workspace-state file 1000)]
      (t/is (nil? (dwa/outline-edit state (thi/id :blob)))
            (str (name flag) ": an edit changes the path itself, as with auto-keyframe"))
      (t/is (empty? (commits (await (emitted (dwa/set-outline-at-playhead (thi/id :blob) flat) state))))
            (str (name flag) ": no keyframe of it")))))

(t/deftest edit-at-playhead-starts-a-path-row
  (let [state (workspace-state (morph-file) 1000)
        asked (dwa/take-outline-edit-request state)]
    (t/is (nil? (dwa/outline-edit state (thi/id :blob))) "a path without a row edits itself")
    (let [state (ptk/update (dwa/request-outline-edit (thi/id :blob)) state)]
      (t/is (some? (dwa/outline-edit state (thi/id :blob))) "unless the edit asks for its outline")
      (t/is (nil? (dwa/outline-edit (dwa/take-outline-edit-request state) (thi/id :blob)))
            "which the edit takes as it starts"))
    (t/is (= state asked) "nothing to take without a request")))

(def ^:private huge
  (path/from-plain (into [{:command :move-to :params {:x 0 :y 0}}]
                         (map (fn [i] {:command :line-to :params {:x i :y (mod i 7)}}))
                         (range 1 5000))))

(t/deftest ^:async an-outline-too-large-stays-out
  (let [file   (-> (morph-file)
                   (ths/add-sample-shape :huge :type :path :parent-label :board-a :content huge))
        state  (workspace-state file 0)
        events (await (emitted (dwa/toggle-keyframes (thi/id :huge) [:content]) state))]
    (t/is (empty? (commits events)) "no keyframe of a path past 4096 segments")
    (t/is (warned? events) "but a warning")
    (let [state  (workspace-state (morphing-file) 1000)
          events (await (emitted (dwa/set-outline-at-playhead (thi/id :blob) huge) state))]
      (t/is (empty? (commits events)) "nor an edit of one to so many")
      (t/is (warned? events)))))

(t/deftest ^:async an-outline-edit-without-nodes-keeps-what-it-has
  (let [file   (morphing-file)
        id     (thi/id :blob)
        state  (-> (workspace-state file 1000)
                   (assoc-in [:workspace-local :edit-path id] {:old-content triangle :outline-edit true})
                   (assoc-in [:workspace-drawing :object :content] (path/from-plain [])))
        events (await (emitted (dwpc/finalize-path-content id) state))]
    (t/is (empty? events) "not the path deleted")))

(t/deftest ^:async a-playhead-edit-is-one-undo-step
  (let [file  (morph-file)
        blob  (get-in (thf/current-page file) [:objects (thi/id :blob)])
        state (assoc-in (workspace-state file 500) [:workspace-animation :auto-keyframe?] true)
        done  (commits (await (emitted (dwa/set-outline-at-playhead (thi/id :blob) (path/move-content (:content blob) (gpt/point 0 10)))
                                       state)))]
    (t/is (= 1 (count done)) "the keyframe at 0 and the edit in one commit")))

(t/deftest ^:async a-recorded-edit-undoes-with-it
  (let [file   (morph-file)
        id     (thi/id :blob)
        group  (random-uuid)
        state  (assoc-in (workspace-state file 500) [:workspace-animation :auto-keyframe?] true)
        commit {:redo-changes [{:type :mod-obj :id id :operations [{:type :set :attr :opacity :val 0.5}]}]
                :undo-changes [{:type :mod-obj :id id :operations [{:type :set :attr :opacity :val 1}]}]
                :undo-group group}
        events (await (emitted (#'dwa/record-canvas-edit commit) state))
        done   (await (js/Promise.all (into-array (map #(emitted % state) (commits events)))))]
    (t/is (seq (commits events)) "the fade recorded")
    (t/is (every? #(= group (:undo-group (deref %))) (mapcat identity done))
          "in the undo group of the edit, so both undo together")))

(t/deftest a-paste-tells-the-outlines-it-left-out
  (let [toast (dwa/paste-result-toast {:skipped 7 :outlines-left-out 5 :grown #{}})]
    (t/is (= (str (tr "workspace.animation.paste-skipped" 2) " "
                  (tr "workspace.animation.paste-outline-too-large" 5 32768))
             (get-in (ptk/update toast {}) [:notification :content]))
          "the others with their reason, the outlines with theirs")))
