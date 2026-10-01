;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.logic.animation-styles-sync-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.logic.libraries :as cll]
   [app.common.logic.timelines :as cltl]
   [app.common.test-helpers.components :as thc]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [clojure.test :as t]))

(t/use-fixtures :each thi/test-fixture)

(defn- three
  "A style `label` of a scale in, a turn and a fade out, 300 ms long, the
  durations of its entries `k` times as long."
  ([label] (three label 1))
  ([label k]
   (ctas/make-animation-style
    {:id (thi/id label)
     :name (name label)
     :entries [{:id (thi/id [label 0]) :type :scale :start 0 :duration (* k 200)}
               {:id (thi/id [label 1]) :type :rotate :start 50 :duration (* k 200)}
               {:id (thi/id [label 2]) :type :fade :direction :out :start 100 :duration (* k 200)}]})))

(defn- with-styles
  "`file` with the animation styles `labels` (see `three`)."
  [file & labels]
  (doseq [label labels]
    (thi/new-id! label)
    (dotimes [n 3] (thi/new-id! [label n])))
  (thf/apply-changes file (reduce #(pcb/add-animation-style %1 (three %2)) (pcb/empty-changes nil) labels)))

(defn- style
  [file label]
  (ctas/get-animation-style (:data file) (thi/id label)))

(defn- edit-style
  "`file` with its style `label` made `k` times as long."
  [file label k]
  (thf/apply-changes file (-> (pcb/empty-changes nil)
                              (pcb/with-library-data (:data file))
                              (pcb/update-animation-style (assoc (three label k) :name (str (name label) " x" k))))))

(defn- delete-style
  [file label]
  (thf/apply-changes file (-> (pcb/empty-changes nil)
                              (pcb/with-library-data (:data file))
                              (pcb/delete-animation-style (thi/id label)))))

(defn- with-timeline
  "`file` with the timeline of the board `label` of its current page, a
  second long when new, changed by `f`."
  [file label f]
  (let [page (thf/current-page file)
        id   (thi/id label)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline id (f (or (get (:timelines page) id)
                                                            (cta/make-timeline {:board-id id :duration 1000}))))))))

(defn- use-of
  "A use from `start` of the style `label` of `library` (a file)."
  [library label start]
  (ctas/make-use (style library label) (:id library) start))

(defn- styled-file
  "A file with the styles :one and :two. Its first page has board :a
  (1000 ms) where :r1 uses :one from 0 and :r2 only has keyframes; its
  second page has board :b (1000 ms) where :r3 uses :one from 300 and :two
  from 0."
  []
  (let [file (-> (thf/sample-file :file1 :page-label :page1)
                 (with-styles :one :two)
                 (tho/add-frame :a :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :r1 :type :rect :parent-label :a :x 10 :y 10 :width 50 :height 50)
                 (ths/add-sample-shape :r2 :type :rect :parent-label :a :x 100 :y 10 :width 50 :height 50))
        file (with-timeline file :a #(-> %
                                         (cta/add-animation (thi/id :r1) (use-of file :one 0))
                                         (cta/add-keyframe (thi/id :r2) {:time 0 :property :x :value 100})))
        file (-> file
                 (thf/add-sample-page :page2)
                 (tho/add-frame :b :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :r3 :type :rect :parent-label :b :x 10 :y 10 :width 50 :height 50))]
    (with-timeline file :b #(-> %
                                (cta/add-animation (thi/id :r3) (use-of file :one 300))
                                (cta/add-animation (thi/id :r3) (use-of file :two 0))))))

(defn- sync-changes
  "The changes syncing, in `file`, the uses of the styles of `library` (of
  `style-label` only, when given), with the files `libraries` loaded."
  [file library style-label & {:keys [libraries early-return?]}]
  (cll/generate-sync-file-changes (pcb/empty-changes)
                                  nil
                                  :animation-styles
                                  (:id file)
                                  (some-> style-label thi/id)
                                  (:id library)
                                  (into {} (map (juxt :id identity)) (or libraries [file library]))
                                  (:id file)
                                  (boolean early-return?)))

(defn- timeline
  [file page-label board-label]
  (get-in (thf/get-page file page-label) [:timelines (thi/id board-label)]))

(defn- uses
  "`[name start duration]` of the animations of `shape-label` on the
  board `board-label` of the page `page-label`."
  [file page-label board-label shape-label]
  (mapv (juxt :name :start :duration)
        (get-in (timeline file page-label board-label) [:tracks (thi/id shape-label) :animations])))

(t/deftest editing-a-style-updates-its-uses-on-every-page
  (let [file    (edit-style (styled-file) :one 4)
        changes (sync-changes file file :one)
        synced  (thf/apply-changes file changes)]
    (t/is (= {:set-timeline-track 2 :update-timeline 1}
             (frequencies (map :type (:redo-changes changes)))))
    (t/is (= #{(thi/id :r1) (thi/id :r3)}
             (into #{} (keep :shape-id) (:redo-changes changes))))
    (t/is (= [(thi/id :b) {:duration 1200}]
             ((juxt :id :attrs) (first (filter #(= :update-timeline (:type %)) (:redo-changes changes))))))

    (t/testing "the uses take the new entries and length"
      (t/is (= [["one x4" 0 900]] (uses synced :page1 :a :r1)))
      (t/is (= [["one x4" 300 900] ["two" 0 300]] (uses synced :page2 :b :r3)))
      (t/is (= (:entries (style synced :one))
               (get-in (timeline synced :page1 :a) [:tracks (thi/id :r1) :animations 0 :entries]))))

    (t/testing "the boards grow to fit them"
      (t/is (= 1000 (:duration (timeline synced :page1 :a))))
      (t/is (= 1200 (:duration (timeline synced :page2 :b)))))

    (t/testing "one undo puts every use and duration back"
      (let [undone (thf/apply-undo-changes synced changes)]
        (t/is (= (timeline file :page1 :a) (timeline undone :page1 :a)))
        (t/is (= (timeline file :page2 :b) (timeline undone :page2 :b)))))

    (t/testing "nothing to sync, no changes"
      (t/is (empty? (:redo-changes (sync-changes synced synced :one)))))))

(t/deftest syncing-one-style-leaves-the-others
  (let [file   (-> (styled-file) (edit-style :one 2) (edit-style :two 2))
        synced (thf/apply-changes file (sync-changes file file :two))]
    (t/is (= [["one" 0 300]] (uses synced :page1 :a :r1)))
    (t/is (= [["one" 300 300] ["two x2" 0 500]] (uses synced :page2 :b :r3)))))

(defn- consumer-file
  "A file using the style :lib-style of `library` and the style :other of
  `other`, both from 0 on :c1 in its board :c."
  [library other]
  (let [file (-> (thf/sample-file :consumer :page-label :page1)
                 (tho/add-frame :c :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :c1 :type :rect :parent-label :c :x 10 :y 10 :width 50 :height 50))]
    (with-timeline file :c #(-> %
                                (cta/add-animation (thi/id :c1) (use-of library :lib-style 0))
                                (cta/add-animation (thi/id :c1) (use-of other :other 0))))))

(t/deftest a-file-follows-the-styles-of-its-library
  (let [library  (with-styles (thf/sample-file :library) :lib-style)
        other    (with-styles (thf/sample-file :other) :other)
        consumer (consumer-file library other)
        edited   (edit-style library :lib-style 2)]
    (t/testing "an edit reaches the uses of the library, not those of another one"
      (let [synced (thf/apply-changes consumer (sync-changes consumer edited nil
                                                             :libraries [consumer edited other]))]
        (t/is (= [["lib-style x2" 0 500] ["other" 0 300]] (uses synced :page1 :c :c1)))))

    (t/testing "a deleted style leaves its presets"
      (let [deleted (delete-style library :lib-style)
            synced  (thf/apply-changes consumer (sync-changes consumer deleted nil
                                                              :libraries [consumer deleted]))
            track   (get-in (timeline synced :page1 :c) [:tracks (thi/id :c1)])]
        (t/is (= [:scale :rotate :fade :style] (mapv :type (:animations track))))
        (t/is (= [0 50 100 0] (mapv :start (:animations track))))))

    (t/testing "without the data of the library, nothing changes"
      (t/is (empty? (:redo-changes (sync-changes consumer (delete-style library :lib-style) nil
                                                 :libraries [consumer])))))))

(t/deftest early-return-stops-at-the-first-change
  (let [file    (edit-style (styled-file) :one 4)
        changes (sync-changes file file :one :early-return? true)]
    (t/is (= [(thi/id :r1)] (into [] (keep :shape-id) (:redo-changes changes))))))

(t/deftest copies-play-the-synced-use-of-their-main
  (let [file      (-> (thf/sample-file :file1 :page-label :page1)
                      (with-styles :one)
                      (tho/add-simple-component :anim :main :main-rect
                                                :root-params {:x 0 :y 0 :width 200 :height 200}
                                                :child-params {:x 10 :y 20 :width 50 :height 50}))
        file      (with-timeline file :main #(-> %
                                                 (assoc :playback :loop)
                                                 (cta/add-animation (thi/id :main-rect) (use-of file :one 600))))
        file      (-> file
                      (thf/add-sample-page :page2)
                      (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                      (thc/instantiate-component :anim :copy :parent-label :board-b))
        synced    (as-> (edit-style file :one 4) $
                    (thf/apply-changes $ (sync-changes $ $ :one)))
        main-page (thf/get-page synced :page1)
        page      (thf/get-page synced :page2)
        copy-rect (first (get-in page [:objects (thi/id :copy) :shapes]))
        copies    (cltl/instance-timelines (:objects page) (thi/id :board-b) nil
                                           (constantly (select-keys main-page [:objects :timelines])))
        copy      (get copies [:copy (thi/id :copy) (thi/id :main)])]
    (t/is (= 1500 (:duration (timeline synced :page1 :main))) "the main grows to fit the use")
    (t/is (= [["one x4" 600 900]]
             (mapv (juxt :name :start :duration) (get-in copy [:tracks copy-rect :animations]))))
    (t/is (= 1500 (:duration copy)) "the copy keeps the clock of its main")))

(t/deftest one-change-per-track-with-a-use
  (let [file   (-> (thf/sample-file :file1 :page-label :page1)
                   (with-styles :one)
                   (tho/add-frame :a :x 0 :y 0 :width 2000 :height 2000))
        labels (mapv #(keyword (str "s" %)) (range 250))
        file   (reduce (fn [file [n label]]
                         (ths/add-sample-shape file label :type :rect :parent-label :a
                                               :x (* 8 (mod n 25)) :y (* 8 (quot n 25)) :width 5 :height 5))
                       file
                       (map-indexed vector labels))
        file   (with-timeline file :a
                 (fn [timeline]
                   (reduce (fn [timeline [n label]]
                             (if (< n 200)
                               (cta/add-animation timeline (thi/id label) (use-of file :one (* 10 n)))
                               (cta/add-keyframe timeline (thi/id label) {:time 0 :property :x :value n})))
                           timeline
                           (map-indexed vector labels))))
        file    (edit-style file :one 2)
        changes (sync-changes file file :one)]
    (t/is (= 200 (count (filter #(= :set-timeline-track (:type %)) (:redo-changes changes)))))
    (t/is (= (set (map thi/id (take 200 labels)))
             (into #{} (keep :shape-id) (:redo-changes changes))))))
