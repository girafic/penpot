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
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.main.data.changes :as dch]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.libraries :as dwl]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.pages :as thp]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]
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
  leaves out."
  []
  (ptk/reify ::watch-undo-stack
    ptk/WatchEvent
    (watch [_ _ stream]
      (->> stream
           (rx/filter dch/commit?)
           (rx/map deref)
           (rx/filter #(and (= :local (:source %)) (:save-undo? %) (seq (:undo-changes %))))
           (rx/map #(dwu/append-undo (select-keys % [:undo-changes :redo-changes :undo-group :tags])
                                     false))
           (rx/take-until (rx/filter #(= :the/end %) stream))))))

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
        (dwl/update-animation-style (style :pop 2) (:id file))]
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
        (dwl/update-animation-style (style :pop 2) (:id file))
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
        (dwl/update-animation-style (style :pop 2) (random-uuid))
        (dwl/update-animation-style (assoc (style :pop 2) :id (random-uuid)) (:id file))]
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
        (dwl/rename-animation-styles (:id file)
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
       [(dwl/delete-animation-style (cthi/id :pop))]
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
        (dwl/delete-animation-style (cthi/id :pop))
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
        (dwl/repair-local-style-uses)]
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
        (dwl/repair-local-style-uses)]
       (fn [_]
         (t/is (= 0 @commits)))))))

(t/deftest remote-changes-to-local-styles-ask-for-a-repair
  (let [file-id (random-uuid)
        use     (assoc (ctas/make-use (style :pop) file-id 0) :id (random-uuid))
        track   {:shape-id (random-uuid) :keyframes [] :animations [use]}]
    (t/is (dwl/touches-local-styles? file-id [{:type :mod-animation-style}]))
    (t/is (dwl/touches-local-styles? file-id [{:type :set-timeline-track :params track}]))
    (t/is (not (dwl/touches-local-styles? (random-uuid) [{:type :set-timeline-track :params track}])))
    (t/is (not (dwl/touches-local-styles? file-id [{:type :mod-obj}])))))

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
        (dwl/update-animation-style (style :pop 3) (:id file))]
       (fn [state]
         (t/is (= 1 @commits))
         (t/is (= 1 (count (get-in state [:workspace-undo :items]))))
         (t/is (nil? (get-in state [:workspace-animation :style-preview])) "the commit ends the preview"))))))

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
