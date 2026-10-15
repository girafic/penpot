;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-styles-test
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.common.test-helpers.tokens :as ctht]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.types.tokens-lib :as ctob]
   [app.common.types.tokens-status :as ctos]
   [app.main.data.changes :as dch]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.animation-styles :as dwas]
   [app.main.data.workspace.libraries :as dwl]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.tokens.propagation :as dwtp]
   [app.main.data.workspace.undo :as dwu]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.pages :as thp]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]
   [frontend-tests.tokens.helpers.state :as tohs]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before (fn []
             (thp/reset-idmap!)
             (cthi/reset-idmap!)
             (thw/setup-wasm-mocks!))
   :after  thw/teardown-wasm-mocks!})

(defn- style
  "The style `label`: a fade in and a scale in, 300 ms, the durations of
  its entries `k` times as long."
  ([label] (style label 1))
  ([label k]
   (ctas/make-animation-style
    {:id (cthi/id label)
     :name (name label)
     :entries [{:id (cthi/id [label 0]) :type :fade :start 0 :duration (* k 300)}
               {:id (cthi/id [label 1]) :type :scale :start 100 :duration (* k 200)}]})))

(defn- setup-file
  "Board A at 0,0 with the rects :r1 (a fade and a move from 100 ms), :r2
  and :r3, the top-most; board B at 1000,0 with :b1. Both boards play a
  second. The styles `labels` (see `style`), and `f` changing the
  timelines, by board label, with the file at hand."
  [& {:keys [labels f] :or {f (fn [_ tls] tls)}}]
  (doseq [label labels]
    (cthi/set-id! label (random-uuid))
    (dotimes [n 2] (cthi/set-id! [label n] (random-uuid))))
  (let [file (-> (cthf/sample-file :file1 :page-label :page-1)
                 (ctho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (cths/add-sample-shape :r1 :type :rect :parent-label :board-a
                                        :x 10 :y 10 :width 50 :height 50)
                 (cths/add-sample-shape :r2 :type :rect :parent-label :board-a
                                        :x 100 :y 10 :width 50 :height 50)
                 (cths/add-sample-shape :r3 :type :rect :parent-label :board-a
                                        :x 200 :y 10 :width 50 :height 50)
                 (ctho/add-frame :board-b :x 1000 :y 0 :width 400 :height 300)
                 (cths/add-sample-shape :b1 :type :rect :parent-label :board-b
                                        :x 1010 :y 10 :width 50 :height 50))
        file (cthf/apply-changes file (reduce #(pcb/add-animation-style %1 (style %2))
                                              (pcb/empty-changes nil)
                                              labels))
        tls  (f file
                {:board-a (-> (cta/make-timeline {:board-id (cthi/id :board-a) :duration 1000})
                              (cta/add-animation (cthi/id :r1) {:type :fade :start 100 :duration 300})
                              (cta/add-animation (cthi/id :r1) {:type :move :start 100 :duration 300}))
                 :board-b (cta/make-timeline {:board-id (cthi/id :board-b) :duration 1000})})]
    (cthf/apply-changes file (reduce-kv (fn [changes label tl]
                                          (pcb/set-timeline changes (cthi/id label) tl))
                                        (-> (pcb/empty-changes nil)
                                            (pcb/with-page (cthf/current-page file)))
                                        tls))))

(defn- use-of
  "A use from `start` of the style `label` of `file`."
  [file label start]
  (ctas/make-use (ctas/get-animation-style (:data file) (cthi/id label)) (:id file) start))

(defn- store-of
  "A store with `file`, Motion on unless `motion?` is false."
  ([file] (store-of file true))
  ([file motion?]
   (let [store (ths/setup-store file)]
     (when motion?
       (ptk/emit! store #(update % :features (fnil conj #{}) "animation/v1")))
     store)))

(defn- timeline-of
  [state board-label]
  (get (:timelines (thp/current-page state)) (cthi/id board-label)))

(defn- animations-of
  [state board-label label]
  (get-in (timeline-of state board-label) [:tracks (cthi/id label) :animations]))

(defn- styles-of
  [state]
  (get-in (ths/get-file-from-state state) [:data :animation-styles]))

(defn- select
  [labels]
  (dws/select-shapes (into (d/ordered-set) (map cthi/id) labels)))

(defn- toast
  [state]
  (get-in state [:notification :content]))

(defn- watch-undo-stack
  "Put the commits on the undo stack, as the workspace does (see
  `app.main.data.workspace/initialize-workspace`), which the test store
  leaves out, until the event `stop` (`:the/end` by default; an async
  run sends it at once, before its commits)."
  ([] (watch-undo-stack :the/end))
  ([stop]
   (ptk/reify ::watch-undo-stack
     ptk/WatchEvent
     (watch [_ _ stream]
       (->> stream
            (rx/filter dch/commit?)
            (rx/map deref)
            (rx/filter #(and (= :local (:source %)) (:save-undo? %) (seq (:undo-changes %))))
            (rx/map #(dwu/append-undo (select-keys % [:undo-changes :redo-changes :undo-group :tags])
                                      false))
            (rx/take-until (rx/filter #(= stop %) stream)))))))

(defn- count-commits
  "Count in `counter` the commits made in the store."
  [counter]
  (ptk/reify ::count-commits
    ptk/WatchEvent
    (watch [_ _ stream]
      (->> stream
           (rx/filter dch/commit?)
           (rx/map deref)
           (rx/filter #(= :local (:source %)))
           (rx/tap (fn [_] (swap! counter inc)))
           (rx/ignore)
           (rx/take-until (rx/filter #(= :the/end %) stream))))))

(defn- plays
  "What the board `board-label` shows of `label` every 50 ms of a second."
  [state board-label label]
  (let [resolved (cta/resolve-animations (timeline-of state board-label)
                                         (:objects (thp/current-page state)))]
    (mapv #(get (cta/values-at resolved %) (cthi/id label)) (range 0 1001 50))))

(t/deftest saving-a-style-links-the-layer-to-it
  (t/async
    done
    (let [store (store-of (setup-file))]
      (ths/run-store
       store done
       [(select [:r1])
        (dwa/save-animation-style (cthi/id :r1) "Cards / Enter")]
       (fn [state]
         (let [style (first (vals (styles-of state)))
               uses  (animations-of state :board-a :r1)]
           (t/is (= 1 (count (styles-of state))))
           (t/is (= ["Cards" "Enter"] [(:path style) (:name style)]))
           (t/is (= [[:fade 0 300] [:move 0 300]] (mapv (juxt :type :start :duration) (:entries style))))
           (t/is (= [[(:id style) 100 300]] (mapv (juxt :style-ref-id :start :duration) uses)))))))))

(t/deftest one-undo-removes-a-saved-style
  (t/async
    done
    (let [file  (setup-file)
          store (store-of file)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (select [:r1])
        (dwa/save-animation-style (cthi/id :r1) "Enter")
        dwu/undo]
       (fn [state]
         (t/is (empty? (styles-of state)))
         (t/is (= (get-in (cthf/current-page file) [:timelines (cthi/id :board-a)])
                  (timeline-of state :board-a))))))))

(t/deftest saving-more-than-a-style-holds-is-refused
  (t/async
    done
    (let [file  (setup-file :f (fn [_ tls]
                                 (update tls :board-a
                                         #(reduce (fn [tl n]
                                                    (cta/add-animation tl (cthi/id :r2) {:type :fade :start (* 10 n)
                                                                                         :duration 10}))
                                                  %
                                                  (range 33)))))
          store (store-of file)]
      (ths/run-store
       store done
       [(select [:r2])
        (dwa/save-animation-style (cthi/id :r2) "Too many")]
       (fn [state]
         (t/is (empty? (styles-of state)))
         (t/is (= 33 (count (animations-of state :board-a :r2))))
         (t/is (= (tr "workspace.animation.style-too-many-entries" 32 33) (toast state))))))))

(t/deftest applying-a-style-staggers-its-uses
  (t/async
    done
    (let [file  (setup-file :labels [:pop])
          store (store-of file)]
      (ths/run-store
       store done
       [(select [:r1 :r2 :r3])
        (dwa/apply-animation-style [(cthi/id :r1) (cthi/id :r2) (cthi/id :r3)] (:id file) (cthi/id :pop))]
       (fn [state]
         (let [start (fn [label] (:start (last (animations-of state :board-a label))))]
           (t/is (= [0 100 200] (mapv start [:r3 :r2 :r1])) "top-most first, 100 ms apart")
           (t/is (every? #(cta/style-use? (last (animations-of state :board-a %))) [:r1 :r2 :r3]))))))))

(t/deftest updating-a-style-updates-its-uses
  (t/async
    done
    (let [file    (setup-file :labels [:pop]
                              :f (fn [file tls]
                                   (-> tls
                                       (update :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 700))
                                       (update :board-b cta/add-animation (cthi/id :b1) (use-of file :pop 0)))))
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (count-commits commits)
        (dwas/update-animation-style (style :pop 2) (:id file))]
       (fn [state]
         (t/is (= 1 @commits) "the style and its uses in one change set")
         (t/is (= [600 1300] ((juxt :duration cta/animation-end) (first (animations-of state :board-a :r2)))))
         (t/is (= 600 (:duration (first (animations-of state :board-b :b1)))))
         (t/is (= [1300 1000] (mapv #(:duration (timeline-of state %)) [:board-a :board-b]))
               "board A grows to fit"))))))

(t/deftest one-undo-reverts-a-style-update
  (t/async
    done
    (let [file  (setup-file :labels [:pop]
                            :f (fn [file tls]
                                 (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 700))))
          store (store-of file)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (dwas/update-animation-style (style :pop 2) (:id file))
        dwu/undo]
       (fn [state]
         (t/is (= (:entries (style :pop)) (:entries (get (styles-of state) (cthi/id :pop)))))
         (t/is (= (get-in (cthf/current-page file) [:timelines (cthi/id :board-a)])
                  (timeline-of state :board-a))))))))

(t/deftest a-style-of-another-file-is-not-updated
  (t/async
    done
    (let [file    (setup-file :labels [:pop])
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(count-commits commits)
        (dwas/update-animation-style (style :pop 2) (random-uuid))
        (dwas/update-animation-style (assoc (style :pop 2) :id (random-uuid)) (:id file))]
       (fn [state]
         (t/is (= 0 @commits))
         (t/is (= (:entries (style :pop)) (:entries (get (styles-of state) (cthi/id :pop))))))))))

(t/deftest renaming-a-group-of-styles-is-one-step
  (t/async
    done
    (let [labels  [:s1 :s2 :s3 :s4 :s5]
          file    (setup-file :labels labels
                              :f (fn [file tls]
                                   (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :s1 0))))
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (count-commits commits)
        (dwas/rename-animation-styles (:id file)
                                      (into {} (map (fn [l] [(cthi/id l) (str "Cards / " (name l) "!")])) labels))]
       (fn [state]
         (t/is (= 1 @commits) "the styles and the uses in one change set")
         (t/is (= #{"Cards"} (into #{} (map :path) (vals (styles-of state)))))
         (t/is (= "s1!" (:name (first (animations-of state :board-a :r2)))) "its uses follow"))))))

(t/deftest deleting-a-style-leaves-its-presets
  (t/async
    done
    (let [file  (setup-file :labels [:pop]
                            :f (fn [file tls]
                                 (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 200))))
          store (store-of file)]
      (ths/run-store
       store done
       [(dwas/delete-animation-style (cthi/id :pop))]
       (fn [state]
         (t/is (empty? (styles-of state)))
         (t/is (= [[:fade 200] [:scale 300]] (mapv (juxt :type :start) (animations-of state :board-a :r2)))))))))

(t/deftest one-undo-brings-back-a-deleted-style
  (t/async
    done
    (let [file  (setup-file :labels [:pop]
                            :f (fn [file tls]
                                 (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 200))))
          store (store-of file)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (dwas/delete-animation-style (cthi/id :pop))
        dwu/undo]
       (fn [state]
         (t/is (contains? (styles-of state) (cthi/id :pop)))
         (t/is (= (get-in (cthf/current-page file) [:timelines (cthi/id :board-a)])
                  (timeline-of state :board-a))))))))

(t/deftest detaching-a-use-leaves-presets-that-play-the-same
  (let [file  (setup-file :labels [:pop]
                          :f (fn [file tls]
                               (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 200))))
        uid   (:id (first (get-in (cthf/current-page file)
                                  [:timelines (cthi/id :board-a) :tracks (cthi/id :r2) :animations])))
        store (store-of file)]
    (t/async
      done
      (let [before (plays @store :board-a :r2)]
        (ths/run-store
         store done
         [(select [:r2])
          (dwa/detach-animation-style (cthi/id :r2) uid)]
         (fn [state]
           (t/is (= [:fade :scale] (mapv :type (animations-of state :board-a :r2))))
           (t/is (= before (plays state :board-a :r2)))))))))

(t/deftest repair-brings-missed-uses-back-to-their-style
  (t/async
    done
    (let [;; the style was edited, but a stale track write kept the old
          ;; entries in the use of r2
          file    (setup-file :labels [:pop]
                              :f (fn [file tls]
                                   (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 0))))
          file    (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                               (pcb/with-library-data (:data file))
                                               (pcb/update-animation-style (style :pop 2))))
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (count-commits commits)
        (dwas/repair-local-style-uses)]
       (fn [state]
         (t/is (= 1 @commits))
         (t/is (empty? (get-in state [:workspace-undo :items])) "a repair is no undo step")
         (t/is (= (:entries (style :pop 2)) (:entries (first (animations-of state :board-a :r2))))))))))

(t/deftest repair-of-a-file-without-drift-commits-nothing
  (t/async
    done
    (let [file    (setup-file :labels [:pop]
                              :f (fn [file tls]
                                   (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 0))))
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(count-commits commits)
        (dwas/repair-local-style-uses)]
       (fn [_]
         (t/is (= 0 @commits)))))))

(t/deftest remote-changes-to-local-styles-ask-for-a-repair
  (let [file-id (random-uuid)
        use     (assoc (ctas/make-use (style :pop) file-id 0) :id (random-uuid))
        track   {:shape-id (random-uuid) :keyframes [] :animations [use]}]
    (t/is (dwas/touches-local-styles? file-id [{:type :mod-animation-style}]))
    (t/is (dwas/touches-local-styles? file-id [{:type :set-timeline-track :params track}]))
    (t/is (dwas/touches-local-styles? file-id [{:type :set-timeline-track-animations :animations [use]}]))
    (t/is (not (dwas/touches-local-styles? (random-uuid) [{:type :set-timeline-track :params track}])))
    (t/is (not (dwas/touches-local-styles? file-id [{:type :mod-obj}])))))

(t/deftest previewing-a-style-commits-nothing
  (t/async
    done
    (let [file    (setup-file :labels [:pop]
                              :f (fn [file tls]
                                   (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 0))))
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(count-commits commits)
        (select [:r2])
        (dwa/preview-animation-style (style :pop 2))
        (dwa/preview-animation-style (style :pop 3))]
       (fn [state]
         (t/is (= 0 @commits))
         (t/is (= (style :pop 3) (get-in state [:workspace-animation :style-preview])))
         (t/is (= (get-in (cthf/current-page file) [:timelines (cthi/id :board-a)])
                  (timeline-of state :board-a))))))))

(t/deftest previews-and-one-commit-are-one-undo-step
  (t/async
    done
    (let [file    (setup-file :labels [:pop]
                              :f (fn [file tls]
                                   (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 0))))
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (count-commits commits)
        (select [:r2])
        (dwa/preview-animation-style (style :pop 2))
        (dwa/preview-animation-style (style :pop 3))
        (dwas/update-animation-style (style :pop 3) (:id file))]
       (fn [state]
         (t/is (= 1 @commits))
         (t/is (= 1 (count (get-in state [:workspace-undo :items]))))
         (t/is (nil? (get-in state [:workspace-animation :style-preview])) "the commit ends the preview"))))))

(t/deftest changed-and-deleted-library-styles-need-a-sync-with-motion-on
  (let [lib-id  (random-uuid)
        _       (doseq [label [:lib-style [:lib-style 0] [:lib-style 1]]]
                  (cthi/set-id! label (random-uuid)))
        changed (assoc (style :lib-style) :modified-at (js/Date. 20000))
        file    (setup-file
                 :f (fn [_ tls]
                      (update tls :board-a cta/add-animation (cthi/id :r2)
                              (ctas/make-use changed lib-id 0))))
        library {:id lib-id
                 :synced-at (js/Date. 10000)
                 :modified-at (js/Date. 30000)
                 :data {:id lib-id :animation-styles {(:id changed) changed}}}
        needs   (fn [library & args]
                  (mapv (juxt :asset-type :asset-id :shape-id)
                        (apply dwl/assets-need-sync library (:data file) args)))]
    (t/is (= [[:animation-style (:id changed) (cthi/id :r2)]] (needs library true)))
    (t/is (= [] (needs library false)) "without Motion, a change of styles alone needs no sync")
    (t/is (= [[:animation-style (:id changed) (cthi/id :r2)]]
             (needs (assoc-in library [:data :animation-styles] {}) true))
          "a style gone from the library")
    (t/is (= [] (needs library (js/Date. 40000) true)) "dismissed until the next change")))

(t/deftest without-motion-the-style-events-do-nothing
  (t/async
    done
    (let [file  (setup-file :labels [:pop]
                            :f (fn [file tls]
                                 (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 0))))
          uid   (:id (first (get-in (cthf/current-page file)
                                    [:timelines (cthi/id :board-a) :tracks (cthi/id :r2) :animations])))
          store (store-of file false)]
      (ths/run-store
       store done
       [(select [:r1 :r3])
        (dwa/save-animation-style (cthi/id :r1) "Enter")
        (dwa/apply-animation-style [(cthi/id :r3)] (:id file) (cthi/id :pop))
        (dwa/detach-animation-style (cthi/id :r2) uid)
        (dwa/preview-animation-style (style :pop 2))]
       (fn [state]
         (t/is (= (styles-of state) (get-in file [:data :animation-styles])))
         (t/is (= (get-in (cthf/current-page file) [:timelines (cthi/id :board-a)])
                  (timeline-of state :board-a)))
         (t/is (nil? (get-in state [:workspace-animation :style-preview]))))))))

;; DESIGN TOKENS

(defn- with-tokens
  "`file` with a token set, active through a theme, of the `tokens`
  (`[name type value]`)."
  [file tokens]
  (-> (ctht/add-tokens-lib file)
      (ctht/update-tokens-lib
       (fn [lib]
         (reduce (fn [lib [token-name type value]]
                   (ctob/add-token lib (cthi/id :motion-set)
                                   (ctob/make-token :name token-name :type type :value value)))
                 (-> lib
                     (ctob/add-set (ctob/make-token-set :id (cthi/new-id! :motion-set) :name "motion"))
                     (ctob/add-theme (ctob/make-token-theme :id (cthi/new-id! :motion-theme)
                                                            :name "theme" :sets #{"motion"})))
                 tokens)))
      (ctht/update-tokens-status
       #(ctos/set-tokens-status % #{(cthi/id :motion-theme)} #{(cthi/id :motion-set)}))))

(defn- bind-entry
  "`file` with the entry `n` of its style `label` binding `tokens`."
  [file label n tokens]
  (let [style (-> (ctas/get-animation-style (:data file) (cthi/id label))
                  (assoc-in [:entries n :applied-tokens] tokens))]
    (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                 (pcb/with-library-data (:data file))
                                 (pcb/update-animation-style style)))))

(defn- scale-entry
  [state label]
  (get-in (styles-of state) [(cthi/id label) :entries 1]))

(t/deftest updating-styles-at-once-is-one-change-set
  (t/async
    done
    (let [file    (setup-file :labels [:pop :zoom]
                              :f (fn [file tls]
                                   (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :zoom 0))))
          store   (store-of file)
          commits (atom 0)]
      (ths/run-store
       store done
       [(count-commits commits)
        (dwas/update-animation-styles [(style :pop 2) (style :zoom 2)] (:id file))]
       (fn [state]
         (t/is (= 1 @commits))
         (t/is (= [600 600] (mapv #(-> (styles-of state) (get (cthi/id %)) :entries first :duration)
                                  [:pop :zoom])))
         (t/is (= 600 (:duration (first (animations-of state :board-a :r2))))
               "the uses follow"))))))

(t/deftest a-token-change-reaches-the-styles-it-sets-and-their-uses
  (t/async
    done
    (let [file  (-> (setup-file :labels [:pop]
                                :f (fn [file tls]
                                     (update tls :board-a cta/add-animation (cthi/id :r2) (use-of file :pop 0))))
                    (bind-entry :pop 1 {:amount "scale.big"})
                    (with-tokens [["scale.big" :number "1.5"]]))
          store (store-of file)

          undo
          (fn []
            (tohs/run-store-async
             store done [dwu/undo ::stop-undo]
             (fn [state]
               (t/is (= 0.5 (:amount (scale-entry state :pop)))
                     "one undo takes the style back")
               (t/is (= 0.5 (:amount (get-in (first (animations-of state :board-a :r2)) [:entries 1])))
                     "and its use"))))]
      (tohs/run-store-async
       store undo
       [(watch-undo-stack ::stop-undo)
        (dwtp/propagate-workspace-tokens)]
       (fn [state]
         (t/is (= [1.5 {:amount "scale.big"}] ((juxt :amount :applied-tokens) (scale-entry state :pop))))
         (t/is (= [1.5 {:amount "scale.big"}]
                  ((juxt :amount :applied-tokens) (get-in (first (animations-of state :board-a :r2)) [:entries 1])))
               "the use follows, the name of the token with it"))))))

(t/deftest a-token-that-sets-nothing-new-commits-no-style
  (t/async
    done
    (let [file    (-> (setup-file :labels [:pop])
                      (bind-entry :pop 1 {:amount "scale.half"})
                      (with-tokens [["scale.half" :number "0.5"]]))
          store   (store-of file)
          before  (get-in file [:data :animation-styles (cthi/id :pop)])
          commits (atom 0)]
      (tohs/run-store-async
       store done
       [(count-commits commits)
        (dwtp/propagate-workspace-tokens)]
       (fn [state]
         (t/is (= 0 @commits))
         (t/is (= before (get (styles-of state) (cthi/id :pop)))))))))

;; DESIGN TOKENS ON PRESETS AND KEYFRAMES

(defn- with-keyframes-page
  "`file` with a page :page-2 holding a board :board-c whose rect :c1 has
  keyframes of x, the first one easing by the token `ease.out`; the first
  page stays the current one."
  [file]
  (let [file (-> (cthf/add-sample-page file :page-2)
                 (ctho/add-frame :board-c :x 0 :y 0 :width 200 :height 200)
                 (cths/add-sample-shape :c1 :type :rect :parent-label :board-c
                                        :x 10 :y 10 :width 20 :height 20))
        tl   (-> (cta/make-timeline {:board-id (cthi/id :board-c) :duration 1000})
                 (cta/add-keyframe (cthi/id :c1) {:time 0 :property :x :value 10
                                                  :applied-tokens {:easing "ease.out"}})
                 (cta/add-keyframe (cthi/id :c1) {:time 500 :property :x :value 60}))
        file (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page (cthf/current-page file))
                                          (pcb/set-timeline (cthi/id :board-c) tl)))]
    (cthf/switch-to-page file :page-1)))

(defn- bound-file
  "A file whose rect :r3 fades bound to `motion.fast` (300 ms as it is)
  and whose second page has a keyframe bound to `ease.out`, with these
  tokens: `motion.fast` 150 ms, `ease.out` a curve."
  []
  (-> (setup-file :f (fn [_ tls]
                       (update tls :board-a cta/add-animation (cthi/id :r3)
                               {:type :fade :start 0 :duration 300
                                :applied-tokens {:duration "motion.fast"}})))
      (with-keyframes-page)
      (with-tokens [["motion.fast" :duration "150ms"]
                    ["ease.out" :cubic-bezier [0.16 1 0.3 1]]])))

(defn- page-2-keyframe
  [state]
  (-> (ths/get-file-from-state state)
      (get-in [:data :pages-index (cthi/id :page-2) :timelines (cthi/id :board-c)
               :tracks (cthi/id :c1) :keyframes 0])))

(t/deftest a-token-change-reaches-bound-presets-and-keyframes-on-every-page
  (t/async
    done
    (let [store (store-of (bound-file))

          undo
          (fn []
            (tohs/run-store-async
             store done [dwu/undo ::stop-undo]
             (fn [state]
               (t/is (= 300 (:duration (first (animations-of state :board-a :r3))))
                     "one undo takes the preset back")
               (t/is (nil? (:easing (page-2-keyframe state))) "and the keyframe"))))]
      (tohs/run-store-async
       store undo
       [(watch-undo-stack ::stop-undo)
        (dwtp/propagate-workspace-tokens)]
       (fn [state]
         (t/is (= [150 {:duration "motion.fast"}]
                  ((juxt :duration :applied-tokens) (first (animations-of state :board-a :r3)))))
         (t/is (= {:type :bezier :curve [0.16 1 0.3 1]} (:easing (page-2-keyframe state)))
               "a keyframe on another page"))))))

(t/deftest resolved-timelines-commit-nothing-again
  (t/async
    done
    (let [store   (store-of (bound-file))
          commits (atom 0)
          again
          (fn []
            (tohs/run-store-async
             store done
             [(count-commits commits)
              (dwtp/propagate-workspace-tokens)]
             (fn [_]
               (t/is (= 0 @commits)))))]
      (tohs/run-store-async
       store again
       [(dwtp/propagate-workspace-tokens)]
       (fn [_] nil)))))

(t/deftest a-keyframe-easing-binds-and-an-edit-detaches-it
  (t/async
    done
    (let [file    (-> (setup-file :f (fn [_ tls]
                                       (-> tls
                                           (update :board-a cta/add-keyframe (cthi/id :r2)
                                                   {:id (cthi/new-id! :k0) :time 0 :property :x :value 100})
                                           (update :board-a cta/add-keyframe (cthi/id :r2)
                                                   {:time 500 :property :x :value 200}))))
                      (with-tokens [["spring.soft" :spring {:stiffness 120 :damping 14 :mass 1}]]))
          store   (store-of file)
          key-of  #(-> (timeline-of % :board-a) :tracks (get (cthi/id :r2)) :keyframes first)
          detach
          (fn []
            (tohs/run-store-async
             store done
             [(dwa/set-keyframe-easing (cthi/id :r2) (cthi/id :k0) :ease-in)]
             (fn [state]
               (t/is (= [:ease-in nil] ((juxt :easing :applied-tokens) (key-of state)))
                     "a hand edit detaches the token"))))]
      (tohs/run-store-async
       store detach
       [(select [:r2])
        (dwa/bind-keyframe-easing (cthi/id :r2) (cthi/id :k0) "spring.soft"
                                  {:type :spring :stiffness 120 :damping 14 :mass 1})]
       (fn [state]
         (t/is (= [{:type :spring :stiffness 120 :damping 14 :mass 1} {:easing "spring.soft"}]
                  ((juxt :easing :applied-tokens) (key-of state)))
               "bound, it takes the value at once"))))))

(t/deftest a-preset-stretched-in-the-dock-keeps-its-length-when-tokens-change
  (t/async
    done
    (let [file  (bound-file)
          store (store-of file)
          base  (get-in (cthf/current-page file) [:timelines (cthi/id :board-a)])
          fade  (first (get-in base [:tracks (cthi/id :r3) :animations]))]
      (tohs/run-store-async
       store done
       [(dwa/retime-animation base (cthi/id :r3) (:id fade) 0 500)
        (dwtp/propagate-workspace-tokens)]
       (fn [state]
         (t/is (= [500 nil] ((juxt :duration :applied-tokens) (first (animations-of state :board-a :r3))))
               "the drag let go of the token, which so sets it no more"))))))

;; KEYFRAME ENTRIES

(defn- with-x-row
  "Board A's timeline in `tls` with a row of :r1 that moves it 40 px to
  the right from 100 to 500 ms (from where it rests, 10 px into the
  board)."
  [tls]
  (update tls :board-a
          #(-> %
               (cta/add-keyframe (cthi/id :r1) {:time 100 :property :x :value 10 :easing :linear})
               (cta/add-keyframe (cthi/id :r1) {:time 500 :property :x :value 50}))))

(defn- file-plays
  "What the board `board-label` of `file` shows of `label` (see `plays`)."
  [file board-label label]
  (let [page     (cthf/current-page file)
        resolved (cta/resolve-animations (get-in page [:timelines (cthi/id board-label)]) (:objects page))]
    (mapv #(get (cta/values-at resolved %) (cthi/id label)) (range 0 1001 50))))

(defn- x-keyframes
  [state board-label label]
  (->> (get-in (timeline-of state board-label) [:tracks (cthi/id label) :keyframes])
       (filter #(= :x (:property %)))
       (mapv (juxt :time :value))))

(t/deftest saving-a-style-takes-the-keyframes-of-the-layer
  (t/async
    done
    (let [file  (setup-file :f (fn [_ tls] (with-x-row tls)))
          store (store-of file)]
      (ths/run-store
       store done
       [(dwa/save-animation-style (cthi/id :r1) "Slide")]
       (fn [state]
         (let [style (first (vals (styles-of state)))]
           (t/is (= [[:fade 0] [:move 0] [:keys 0]] (mapv (juxt :type :start) (:entries style)))
                 "the presets and a keyframe entry")
           (t/is (= [[0 0] [400 40]] (mapv (juxt :time :value) (:keyframes (d/seek cta/keys-entry? (:entries style)))))
                 "relative to where the layer rests")
           (t/is (empty? (x-keyframes state :board-a :r1)) "they leave the layer")
           (t/is (= (file-plays file :board-a :r1) (plays state :board-a :r1))
                 "which plays as before")))))))

(defn- update-file
  "`setup-file` with the style :pop used from 0 by :r1, which also has a
  row of keyframes (see `with-x-row`), and by :b1: `[file use-id]`, the
  use of :r1."
  []
  (let [file (setup-file :labels [:pop]
                         :f (fn [file tls]
                              (-> tls
                                  (with-x-row)
                                  (update :board-a cta/add-animation (cthi/id :r1) (use-of file :pop 0))
                                  (update :board-b cta/add-animation (cthi/id :b1) (use-of file :pop 0)))))]
    [file (->> (get-in (cthf/current-page file) [:timelines (cthi/id :board-a) :tracks (cthi/id :r1) :animations])
               (d/seek cta/style-use?)
               :id)]))

(t/deftest updating-a-style-from-a-layer-reaches-its-uses
  (t/async
    done
    (let [[file use-id] (update-file)
          store         (store-of file)
          commits       (atom 0)]
      (ths/run-store
       store done
       [(count-commits commits)
        (dwas/update-style-from-layer (cthi/id :r1) use-id)]
       (fn [state]
         (let [entries (:entries (get (styles-of state) (cthi/id :pop)))]
           (t/is (= 1 @commits) "one change set")
           (t/is (= {:fade 2 :scale 1 :move 1 :keys 1} (frequencies (map :type entries)))
                 "the style takes what the layer plays")
           (t/is (= [use-id] (mapv :id (animations-of state :board-a :r1)))
                 "the use, the same, takes the place of the presets")
           (t/is (empty? (x-keyframes state :board-a :r1)) "and of the keyframes")
           (t/is (= (file-plays file :board-a :r1) (plays state :board-a :r1)) "the layer plays as before")
           (t/is (= entries (:entries (first (animations-of state :board-b :b1)))) "another use follows")
           (t/is (= (plays state :board-a :r1) (plays state :board-b :b1))
                 "and plays it from where its layer rests")))))))

(t/deftest one-undo-reverts-an-update-from-a-layer
  (t/async
    done
    (let [[file use-id] (update-file)
          store         (store-of file)
          page          (cthf/current-page file)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (dwas/update-style-from-layer (cthi/id :r1) use-id)
        dwu/undo]
       (fn [state]
         (t/is (= (:entries (style :pop)) (:entries (get (styles-of state) (cthi/id :pop)))))
         (t/is (= (get-in page [:timelines (cthi/id :board-a)]) (timeline-of state :board-a))
               "the layer as it was")
         (t/is (= (get-in page [:timelines (cthi/id :board-b)]) (timeline-of state :board-b))
               "and the other uses"))))))

(t/deftest a-diamond-keys-its-property-alone
  ;; The design tab has a diamond for each of X, Y, W, H and the scales,
  ;; which keys only its own property.
  (t/async
    done
    (let [store   (store-of (setup-file))
          keyed   (fn [state]
                    (into (sorted-set)
                          (map :property)
                          (get-in (timeline-of state :board-a) [:tracks (cthi/id :r1) :keyframes])))]
      (ths/run-store
       store done
       [(select [:r1])
        (dwa/toggle-keyframes (cthi/id :r1) [:y])
        (dwa/toggle-keyframes (cthi/id :r1) [:width])]
       (fn [state]
         (t/is (= #{:width :y} (keyed state)) "Y and W, not X or H"))))))
