;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-morph-test
  "The outline of a path as keyframes (path morphing) in the workspace."
  (:require
   [app.common.data.undo-stack :as u]
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
   [app.main.data.workspace.path :as drp]
   [app.main.data.workspace.path.changes :as dwpc]
   [app.main.data.workspace.path.edition :as dwpe]
   [app.main.data.workspace.path.helpers :as dwph]
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
                  (tr "workspace.animation.paste-outline-too-large" 5 ctao/max-outline-row-segments))
             (get-in (ptk/update toast {}) [:notification :content]))
          "the others with their reason, the outlines with theirs")))

(def ^:private big
  "4000 segments: eight in a row hold 32000, a ninth takes it past 32768."
  (path/from-plain (into [{:command :move-to :params {:x 0 :y 0}}]
                         (map (fn [i] {:command :line-to :params {:x (/ i 100) :y (mod i 7)}}))
                         (range 1 4000))))

(defn- full-row-file
  "`morph-file` with the path :big whose Path row holds eight outlines of
  4000 segments, at 0 to 700."
  []
  (let [file  (-> (morph-file)
                  (ths/add-sample-shape :big :type :path :parent-label :board-a :content big))
        page  (thf/current-page file)
        shape (get-in page [:objects (thi/id :big)])
        tl    (reduce #(cta/set-outline-at %1 shape %2 (ctao/outline-of shape))
                      (get-in page [:timelines (thi/id :board-a)])
                      (range 0 800 100))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(t/deftest ^:async a-full-path-row-takes-no-more
  (let [file  (full-row-file)
        id    (thi/id :big)
        state (workspace-state file 900)]
    (t/is (= 8 (count (get-in (thf/current-page file) [:timelines (thi/id :board-a) :tracks id :keyframes]))))
    (let [events (await (emitted (dwa/toggle-keyframes id [:content]) state))]
      (t/is (empty? (commits events)) "the diamond adds no ninth")
      (t/is (warned? events)))
    (let [events (await (emitted (dwa/add-keyframe-at id :content nil 900) state))]
      (t/is (empty? (commits events)) "nor a keyframe added on the row")
      (t/is (warned? events)))
    (let [events (await (emitted (dwa/set-outline-at-playhead id big) state))]
      (t/is (empty? (commits events)) "nor an edit at the playhead")
      (t/is (warned? events)))))

(t/deftest ^:async a-new-row-past-the-limit-at-0-warns
  ;; a path past what a keyframe holds: the edit would fit, its outline
  ;; at 0 does not, so neither goes in
  (let [file   (-> (morph-file)
                   (ths/add-sample-shape :huge :type :path :parent-label :board-a :content huge))
        state  (assoc-in (workspace-state file 500) [:workspace-animation :auto-keyframe?] true)
        events (await (emitted (dwa/set-outline-at-playhead (thi/id :huge) triangle) state))]
    (t/is (empty? (commits events)))
    (t/is (warned? events))))

(t/deftest an-edit-at-a-keyframe-starts-from-its-own-outline
  ;; the row goes from the triangle to a square, matched as curves: at
  ;; the square's keyframe the edit starts from its own lines
  (let [file    (morph-file)
        page    (thf/current-page file)
        blob    (get-in page [:objects (thi/id :blob)])
        tl      (-> (get-in page [:timelines (thi/id :board-a)])
                    (cta/set-outline-at blob 0 (ctao/outline-of blob))
                    (cta/set-outline-at blob 1000 (path/from-plain [{:command :move-to :params {:x 0 :y 0}}
                                                                    {:command :line-to :params {:x 40 :y 0}}
                                                                    {:command :line-to :params {:x 40 :y 40}}
                                                                    {:command :line-to :params {:x 0 :y 40}}
                                                                    {:command :close-path :params {}}])))
        file    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                            (pcb/with-page page)
                                            (pcb/set-timeline (thi/id :board-a) tl)))
        content (:content (dwa/outline-edit (workspace-state file 1000) (thi/id :blob)))]
    (t/is (= [:move-to :line-to :line-to :line-to :close-path] (mapv :command (vec content))))))

(t/deftest ^:async an-outline-edit-that-draws-nothing-keeps-what-it-has
  (let [file   (morphing-file)
        id     (thi/id :blob)
        state  (-> (workspace-state file 1000)
                   (assoc-in [:workspace-local :edit-path id] {:old-content triangle :outline-edit true})
                   (assoc-in [:workspace-drawing :object :content]
                             (path/from-plain [{:command :move-to :params {:x 0 :y 0}}
                                               {:command :move-to :params {:x 10 :y 10}}])))]
    (t/is (empty? (await (emitted (dwpc/finalize-path-content id) state))) "no warning, no keyframe")))

(t/deftest the-path-editor-takes-the-request-of-an-outline-edit
  (let [file  (morph-file)
        id    (thi/id :blob)
        state (->> (workspace-state file 1000)
                   (ptk/update (dwa/request-outline-edit id))
                   (ptk/update (dwpe/start-path-edit id)))]
    (t/is (true? (get-in state [:workspace-local :edit-path id :outline-edit])) "an edit of its outline")
    (t/is (nil? (get-in state [:workspace-animation :outline-edit-id])) "the request taken")))

(defn- editing
  "`state` editing the outline of :blob (the triangle) with the nodes
  `nodes` selected, an outline edit with `outline?`."
  [state nodes outline?]
  (let [id   (thi/id :blob)
        blob (get-in (thf/current-page (get-in state [:files (:current-file-id state)])) [:objects id])]
    (-> state
        (assoc-in [:workspace-drawing :object] blob)
        (assoc-in [:workspace-local :edition] id)
        (assoc-in [:workspace-local :edit-path id] {:edit-mode :move
                                                    :selection {:nodes nodes :segments #{} :handlers #{}}
                                                    :outline-edit outline?}))))

(defn- edited-points
  [state]
  (mapv (fn [{:keys [params]}] [(:x params) (:y params)])
        (filter #(not= :close-path (:command %)) (vec (get-in state [:workspace-drawing :object :content])))))

(t/deftest the-first-point-tool-starts-the-outline-at-a-node
  (let [state (editing (workspace-state (morph-file) 1000) #{2} true)
        after (ptk/update (drp/start-at-node) state)]
    (t/is (= [20 70] (first (edited-points after))) "from the selected node")
    (t/is (= #{[20 30] [60 30] [20 70]} (set (edited-points after))) "the same triangle")
    (t/is (true? (get-in after [:workspace-local :edit-path (thi/id :blob) :outline-fixed])) "set by hand")
    (let [plain (editing state #{2} false)]
      (t/is (= plain (ptk/update (drp/start-at-node) plain)) "only in an edit of an outline"))))

(t/deftest the-direction-tool-turns-the-outline-around
  (let [state (editing (workspace-state (morph-file) 1000) #{} true)
        after (ptk/update (drp/reverse-direction) state)]
    (t/is (= [[20 30] [20 70] [60 30]] (take 3 (edited-points after))) "every subpath, from the same start")
    (t/is (true? (get-in after [:workspace-local :edit-path (thi/id :blob) :outline-fixed])))))

(defn- content-keyframe
  [page time]
  (some #(when (and (= :content (:property %)) (= time (:time %))) %)
        (get-in page [:timelines (thi/id :board-a) :tracks (thi/id :blob) :keyframes])))

(t/deftest ^:async a-fixed-edit-marks-its-keyframe
  (let [file  (morphing-file)
        state (workspace-state file 1000)
        blob  (get-in (thf/current-page file) [:objects (thi/id :blob)])
        fixed (await (committed (dwa/set-outline-at-playhead (thi/id :blob) (:content blob) {:fixed? true}) state file))]
    (t/is (true? (:fixed-start (content-keyframe fixed 1000))) "set by hand")
    (let [file  (with-page file fixed)
          plain (await (committed (dwa/set-outline-at-playhead (thi/id :blob) (path/move-content (:content blob) (gpt/point 0 5)))
                                  (workspace-state file 1000) file))]
      (t/is (true? (:fixed-start (content-keyframe plain 1000))) "a plain edit keeps it")
      (let [file  (with-page file plain)
            state (workspace-state file 1000)
            done  (commits (await (emitted (dwa/set-outline-fixed (thi/id :blob) false) state)))
            auto  (await (committed (dwa/set-outline-fixed (thi/id :blob) false) state file))]
        (t/is (= 1 (count done)) "Auto in one undo step")
        (t/is (nil? (:fixed-start (content-keyframe auto 1000))) "matched automatically again")))))

(defn- selected-points
  [state]
  (let [id      (thi/id :blob)
        content (get-in state [:workspace-drawing :object :content])]
    (dwph/node-positions content (get-in state [:workspace-local :edit-path id :selection :nodes]))))

(t/deftest the-tools-keep-selection-and-handler-types-with-their-nodes
  (let [file  (morph-file)
        id    (thi/id :blob)
        state (-> (editing (workspace-state file 1000) #{1} true)
                  (assoc-in [:workspace-local :edit-path id :handler-types] {2 :mirror}))
        back  (ptk/update (drp/reverse-direction) state)
        types (fn [st]
                (let [content (vec (get-in st [:workspace-drawing :object :content]))]
                  (into {}
                        (map (fn [[i t]] [[(get-in content [i :params :x]) (get-in content [i :params :y])] t]))
                        (get-in st [:workspace-local :edit-path id :handler-types]))))]
    (t/is (= #{(gpt/point 60 30)} (selected-points back)) "reversed, the node still selected")
    (t/is (= {[20 70] :mirror} (types back)) "and its handler type on it")
    (let [started (ptk/update (drp/start-at-node) state)]
      (t/is (= #{(gpt/point 60 30)} (selected-points started)) "started at it, the node still selected")
      (t/is (= {[20 70] :mirror} (types started))))))

(t/deftest an-undo-takes-the-first-point-back
  (let [state (editing (workspace-state (morph-file) 1000) #{2} true)
        entry (#'app.main.data.workspace.path.undo/make-entry state)
        after (ptk/update (drp/start-at-node) state)
        undo  (#'app.main.data.workspace.path.undo/load-entry after entry)]
    (t/is (true? (get-in after [:workspace-local :edit-path (thi/id :blob) :outline-fixed])))
    (t/is (not (get-in undo [:workspace-local :edit-path (thi/id :blob) :outline-fixed]))
          "undone, the edit no longer sets it")))

;; UNDO OF THE FIRST POINT AND THE WAY ROUND

(def ^:private make-entry #'app.main.data.workspace.path.undo/make-entry)
(def ^:private load-entry #'app.main.data.workspace.path.undo/load-entry)

(defn- held
  "`state` with the edited path as the path editor holds it, its close a
  line back to its start (see `path/close-subpaths`)."
  [state]
  (update-in state [:workspace-drawing :object :content] path/close-subpaths))

(defn- types-by-point
  "The handler types of the edit in `state`, by the position of their
  nodes."
  [state]
  (let [content (vec (get-in state [:workspace-drawing :object :content]))]
    (into {}
          (map (fn [[i t]] [[(get-in content [i :params :x]) (get-in content [i :params :y])] t]))
          (get-in state [:workspace-local :edit-path (thi/id :blob) :handler-types]))))

(t/deftest an-undo-keeps-handler-types-and-selection-on-their-nodes
  (doseq [[label tool nodes] [["first point" drp/start-at-node #{1}]
                              ["way round of one" drp/reverse-direction #{1}]
                              ["way round of all" drp/reverse-direction #{}]]]
    (let [file   (morph-file)
          id     (thi/id :blob)
          state  (-> (editing (workspace-state file 1000) nodes true)
                     (held)
                     (assoc-in [:workspace-local :edit-path id :handler-types] {2 :mirror}))
          before (make-entry state)
          after  (ptk/update (tool) state)
          done   (make-entry after)
          undone (load-entry after before)
          redone (load-entry undone done)]
      (t/is (not= (edited-points state) (edited-points after)) (str label ": the tool changed the order"))
      (t/is (= (edited-points state) (edited-points undone)) (str label ": undone, the content as it was"))
      (t/is (= {[20 70] :mirror} (types-by-point undone)) (str label ": undone, the mode on its node"))
      (t/is (= {[20 70] :mirror} (types-by-point redone)) (str label ": redone, the mode on its node"))
      (t/is (= (edited-points after) (edited-points redone)) (str label ": redone, the content of the tool"))
      (when (seq nodes)
        (t/is (= #{(gpt/point 60 30)} (selected-points undone)) (str label ": undone, the node still selected"))
        (t/is (= #{(gpt/point 60 30)} (selected-points redone)) (str label ": redone, the node still selected"))))))

(t/deftest an-undo-of-a-handle-drag-keeps-its-handle-selected
  (let [file    (morph-file)
        id      (thi/id :blob)
        curve   (path/from-plain [{:command :move-to :params {:x 20 :y 30}}
                                  {:command :line-to :params {:x 60 :y 30}}
                                  {:command :curve-to :params {:c1x 60 :c1y 50 :c2x 40 :c2y 70 :x 20 :y 70}}
                                  {:command :line-to :params {:x 20 :y 30}}])
        state   (-> (editing (workspace-state file 1000) #{} true)
                    (assoc-in [:workspace-drawing :object :content] curve)
                    (assoc-in [:workspace-local :edit-path id :selection :handlers] #{[2 :c1]}))
        before  (make-entry state)
        dragged (assoc-in state [:workspace-drawing :object :content]
                          (path/from-plain (assoc-in (vec curve) [2 :params :c1x] 70)))
        undone  (load-entry dragged before)]
    (t/is (= #{[2 :c1]} (get-in undone [:workspace-local :edit-path id :selection :handlers]))
          "the handle still selected, as before")))

(t/deftest an-undo-takes-a-handle-mode-back-from-an-edit-without-any
  (let [file   (morph-file)
        id     (thi/id :blob)
        state  (-> (editing (workspace-state file 1000) #{1} true)
                   (held))
        before (make-entry state)
        mirror (ptk/update (drp/set-handler-type :mirror) state)
        undone (load-entry mirror before)]
    (t/is (nil? (get-in state [:workspace-local :edit-path id :handler-types])) "an edit that began by drawing has none")
    (t/is (= {[60 30] :mirror} (types-by-point mirror)))
    (t/is (empty? (get-in undone [:workspace-local :edit-path id :handler-types])) "undone, none again")))

(t/deftest choosing-a-handle-mode-is-one-undo-step
  (let [file   (morph-file)
        id     (thi/id :blob)
        state  (-> (editing (workspace-state file 1000) #{1} true)
                   (held)
                   (assoc-in [:workspace-local :edit-path id :handler-types] {1 :mirror}))
        stack  (u/append (u/make-stack) (make-entry state))
        indep  (ptk/update (drp/set-handler-type :independent) state)
        stack' (u/append stack (make-entry indep))]
    (t/is (= {[60 30] :independent} (types-by-point indep)))
    (t/is (not= stack stack') "an entry of its own, though the nodes stay where they are")
    (t/is (= {1 :mirror} (:handler-types (u/peek (u/undo stack')))) "undone, the mode before")))

(t/deftest an-undo-of-a-removed-node-gives-its-mode-back
  (let [file    (morph-file)
        id      (thi/id :blob)
        state   (-> (editing (workspace-state file 1000) #{2} true)
                    (held)
                    (assoc-in [:workspace-local :edit-path id :handler-types] {1 :mirror 2 :aligned}))
        before  (make-entry state)
        removed (ptk/update (drp/remove-node) state)
        undone  (load-entry removed before)]
    (t/is (= {[60 30] :mirror [20 70] :aligned} (types-by-point undone)) "both modes on their nodes again")))

(defn- fixed-row-file
  "`morph-file` with the Path row of :blob going from the triangle to
  `to` (at 1000), held by hand there."
  [to]
  (let [file (morph-file)
        page (thf/current-page file)
        blob (get-in page [:objects (thi/id :blob)])
        tl   (-> (get-in page [:timelines (thi/id :board-a)])
                 (cta/set-outline-at blob 0 (ctao/outline-of blob))
                 (cta/set-outline-at blob 1000 to)
                 (cta/set-outline-fixed (thi/id :blob) 1000 true))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- board-timeline
  [file]
  (get-in (thf/current-page file) [:timelines (thi/id :board-a)]))

(t/deftest ^:async auto-shows-where-it-matches-again
  (let [square (path/from-plain [{:command :move-to :params {:x 40 :y 40}}
                                 {:command :line-to :params {:x 0 :y 40}}
                                 {:command :line-to :params {:x 0 :y 0}}
                                 {:command :line-to :params {:x 40 :y 0}}
                                 {:command :close-path :params {}}])
        other  (path/from-plain [{:command :move-to :params {:x 0 :y 40}}
                                 {:command :line-to :params {:x 0 :y 0}}
                                 {:command :line-to :params {:x 40 :y 0}}
                                 {:command :close-path :params {}}])
        ;; each file its own :blob, asked right after it is made
        auto?  (fn [file] (dwa/outline-auto? (board-timeline file) (thi/id :blob) 1000))
        mixed  (fixed-row-file square)]
    (t/is (true? (auto? mixed)) "a row of other structures")
    (t/is (false? (auto? (with-row-flag mixed :locked))) "not a locked one")
    (t/is (false? (auto? (with-row-flag mixed :hidden))) "nor a hidden one")
    (let [locked (with-row-flag mixed :locked)]
      (t/is (empty? (commits (await (emitted (dwa/set-outline-fixed (thi/id :blob) false) (workspace-state locked 1000)))))
            "a locked row keeps its mark"))
    (t/is (false? (auto? (fixed-row-file other)))
          "nor a row of one structure, which pairs point by point")))

(t/deftest auto-leaves-the-first-keyframe-alone
  ;; the mark of the first keyframe pairs with nothing (see `morph/match`)
  (let [square (path/from-plain [{:command :move-to :params {:x 40 :y 40}}
                                 {:command :line-to :params {:x 0 :y 40}}
                                 {:command :line-to :params {:x 0 :y 0}}
                                 {:command :line-to :params {:x 40 :y 0}}
                                 {:command :close-path :params {}}])
        file   (morph-file)
        page   (thf/current-page file)
        blob   (get-in page [:objects (thi/id :blob)])
        tl     (-> (get-in page [:timelines (thi/id :board-a)])
                   (cta/set-outline-at blob 0 (ctao/outline-of blob))
                   (cta/set-outline-at blob 1000 (ctao/outline-of (assoc blob :content square)))
                   (cta/set-outline-fixed (thi/id :blob) 0 true))]
    (t/is (true? (:fixed-start (some #(when (and (= :content (:property %)) (zero? (:time %))) %)
                                     (:keyframes (cta/get-track tl (thi/id :blob)))))))
    (t/is (false? (dwa/outline-auto? tl (thi/id :blob) 0)) "no Auto on the first keyframe")))

(t/deftest first-point-shows-where-a-subpath-can-start
  ;; as the path editor holds them (see `path/close-subpaths`): two loops
  ;; of one subpath, its start coming back between them
  (let [merged (path/close-subpaths (path/from-string "M0 0 L10 0 L10 10 Z L20 20 L30 0 Z"))
        open   (path/close-subpaths (path/from-string "M0 0 L10 0 L10 10"))
        sel    (fn [nodes] {:nodes nodes :segments #{} :handlers #{}})]
    (t/is (false? (dwph/first-point-available? merged (sel #{}))) "no node")
    (t/is (false? (dwph/first-point-available? merged (sel #{1 2}))) "two nodes")
    (t/is (false? (dwph/first-point-available? merged (sel #{0}))) "its start")
    (t/is (false? (dwph/first-point-available? merged (sel #{3}))) "its start where it comes back")
    (t/is (true? (dwph/first-point-available? merged (sel #{2}))) "another node")
    (t/is (false? (dwph/first-point-available? open (sel #{1}))) "a node of an open subpath")))

(t/deftest the-first-point-tool-leaves-a-start-as-it-is
  (let [file   (morph-file)
        merged (path/close-subpaths (path/from-string "M0 0 L10 0 L10 10 Z L20 20 L30 0 Z"))
        state  (-> (editing (workspace-state file 1000) #{3} true)
                   (assoc-in [:workspace-drawing :object :content] merged))]
    (t/is (= state (ptk/update (drp/start-at-node) state))
          "the content and the mark as they are, at the start where it comes back")))
