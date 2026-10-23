;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.logic.timelines-test
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.logic.libraries :as cll]
   [app.common.logic.shapes :as cls]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.test-helpers.components :as thc]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.common.types.animation-follow :as ctaf]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.animation-style :as ctas]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(t/use-fixtures :each thi/test-fixture)

(defn- animated-file
  "Board A at 100,100 with a rect at 150,200 (50,100 in the board), both
  moving right, and an empty board B at 1000,100."
  []
  (let [file  (-> (thf/sample-file :file1)
                  (tho/add-frame :board-a :x 100 :y 100 :width 400 :height 300)
                  (ths/add-sample-shape :rect :type :rect :parent-label :board-a
                                        :x 150 :y 200 :width 50 :height 50)
                  (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300))
        a     (thi/id :board-a)
        rect  (thi/id :rect)
        tl    (-> (cta/make-timeline {:board-id a :duration 1500})
                  (cta/add-keyframe a {:time 0 :property :x :value 100})
                  (cta/add-keyframe a {:time 1000 :property :x :value 200})
                  (cta/add-keyframe rect {:time 0 :property :x :value 50})
                  (cta/add-keyframe rect {:time 1000 :property :x :value 150})
                  (cta/add-animation rect {:type :fade :start 0 :duration 400}))
        page  (thf/current-page file)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline a tl)))))

(defn- timelines
  [file]
  (:timelines (thf/current-page file)))

(defn- x-values
  [timeline shape-id]
  (mapv :value (cta/property-keyframes timeline shape-id :x)))

(defn- duplicate
  "Duplicate the shape `label` moved by `delta`; the file and the id of
  the copy."
  [file label delta]
  (let [page    (thf/current-page file)
        id      (thi/id label)
        changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                (:objects page) page #{id} delta
                                                {(:id file) file} (:data file) (:id file))
        copy-id (->> (:redo-changes changes)
                     (some #(when (and (= :add-obj (:type %)) (= id (:old-id %)))
                              (-> % :obj :id))))]
    [(thf/apply-changes file changes) copy-id]))

(defn- relocate-changes
  [file label parent-label]
  (let [page (thf/current-page file)]
    (cls/generate-relocate (-> (pcb/empty-changes nil)
                               (pcb/with-page-id (:id page))
                               (pcb/with-objects (:objects page))
                               (pcb/with-library-data (:data file)))
                           (thi/id parent-label)
                           0
                           #{(thi/id label)})))

(defn- relocate
  [file label parent-label]
  (thf/apply-changes file (relocate-changes file label parent-label)))

(t/deftest duplicated-layer-gets-the-animation
  (let [[file copy] (duplicate (animated-file) :rect (gpt/point 10 20))
        rect        (thi/id :rect)
        timeline    (get (timelines file) (thi/id :board-a))
        original    (cta/get-track timeline rect)
        track       (cta/get-track timeline copy)]
    (t/is (some? copy))
    ;; it moved 10 to the right, so does its animation
    (t/is (= [60 160] (x-values timeline copy)))
    (t/is (= [50 150] (x-values timeline rect)))
    (t/is (= 1 (count (:animations track))))
    (t/is (empty? (filter (set (map :id (:keyframes original))) (map :id (:keyframes track)))))
    (t/is (cta/valid-timeline? timeline))))

(t/deftest duplicated-board-gets-its-timeline
  (let [[file copy] (duplicate (animated-file) :board-a (gpt/point 500 0))
        timeline    (get (timelines file) copy)
        rect-copy   (->> (:tracks timeline) keys (remove #{copy}) first)]
    (t/is (some? timeline))
    (t/is (= copy (:board-id timeline)))
    (t/is (= 1500 (:duration timeline)))
    ;; the board moved 500 on the canvas; its layers did not move in it
    (t/is (= [600 700] (x-values timeline copy)))
    (t/is (= [50 150] (x-values timeline rect-copy)))
    (t/is (not= (thi/id :rect) rect-copy))
    (t/is (= 2 (count (timelines file))))))

(t/deftest moved-layer-takes-its-animation-along
  (let [file     (relocate (animated-file) :rect :board-b)
        a        (get (timelines file) (thi/id :board-a))
        b        (get (timelines file) (thi/id :board-b))
        rect     (thi/id :rect)]
    (t/is (nil? (cta/get-track a rect)))
    (t/is (some? (cta/get-track a (thi/id :board-a))))
    ;; same place on the canvas, 900 further left in its new board
    (t/is (= [-850 -750] (x-values b rect)))
    (t/is (= 1 (count (:animations (cta/get-track b rect)))))
    (t/is (cta/valid-timeline? b))))

(t/deftest board-moved-into-another-gives-its-tracks-away
  (let [file (relocate (animated-file) :board-a :board-b)
        tls  (timelines file)
        b    (get tls (thi/id :board-b))]
    (t/is (not (contains? tls (thi/id :board-a))))
    ;; 100 and 200 on the canvas, 1000 further left in board B
    (t/is (= [-900 -800] (x-values b (thi/id :board-a))))
    (t/is (= [-850 -750] (x-values b (thi/id :rect))))))

(t/deftest pasted-layer-brings-its-animation
  (let [file    (animated-file)
        page    (thf/current-page file)
        rect    (thi/id :rect)
        b       (thi/id :board-b)
        ;; copied on the page, then pasted at 1020,150 in board B
        motion  (cltl/animation-sources page (:objects page) [rect])
        pasted  (-> (ths/get-shape file :rect)
                    (assoc :parent-id b :frame-id b)
                    (gsh/move (gpt/point 870 -50)))
        objects (assoc (:objects page) rect pasted)
        changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                objects page #{rect} (gpt/point 0 0)
                                                {(:id file) file} (:data file) (:id file)
                                                {:motion motion})
        file    (thf/apply-changes file changes)
        board-b (get (timelines file) b)
        copy    (first (keys (:tracks board-b)))]
    (t/is (some? copy))
    (t/is (not= rect copy))
    ;; 20 into board B, where it was 50 into board A
    (t/is (= [20 120] (x-values board-b copy)))))

(defn- motion-scene
  "A board at 0,0 with the rects A at 100,100 and B at 300,100, both
  moving 200 to the right from 0 to 1000 ms."
  []
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 1000 :height 600})
        bid   (:id board)
        rect  (fn [x] (-> (cts/setup-shape {:type :rect :x x :y 100 :width 100 :height 50})
                          (assoc :frame-id bid :parent-id bid)))
        a     (rect 100)
        b     (rect 300)
        tl    (reduce (fn [tl {:keys [id selrect]}]
                        (-> tl
                            (cta/add-keyframe id {:time 0 :property :x :value (:x selrect)})
                            (cta/add-keyframe id {:time 1000 :property :x :value (+ 200 (:x selrect))})))
                      (cta/make-timeline {:board-id bid :duration 1000})
                      [a b])]
    {:objects {bid (assoc board :shapes [(:id a) (:id b)]) (:id a) a (:id b) b}
     :timelines {bid tl}
     :board bid
     :a (:id a)
     :b (:id b)}))

(defn- shown-by
  "The shape `id` of `objects` as the modif-tree `tree` shows it."
  [tree objects id]
  (gsh/transform-shape (get objects id) (get-in tree [id :modifiers])))

(t/deftest edit-preview-shows-a-move-where-it-ends-up
  (let [{:keys [objects timelines board a b]} (motion-scene)
        shown   (cltl/shown-shapes timelines objects 500)
        tree    (cltl/edit-preview timelines board objects 500 {a {:modifiers (ctm/move-modifiers (gpt/point 30 0))}})]
    ;; A where the animation shows it, 30 further right
    (t/is (mth/close? (+ 30 (get-in shown [a :selrect :x])) (get-in (shown-by tree objects a) [:selrect :x])))
    ;; B, left alone, where the animation shows it
    (t/is (mth/close? (get-in shown [b :selrect :x]) (get-in (shown-by tree objects b) [:selrect :x])))))

(t/deftest edit-preview-turns-a-shape-where-it-is-shown
  (let [{:keys [objects timelines board a]} (motion-scene)
        shape   (get objects a)
        shown   (cltl/shown-shapes timelines objects 500)
        tree    (cltl/edit-preview timelines board objects 500
                                   {a {:modifiers (ctm/rotation-modifiers shape (gsh/shape->center shape) 30)}})
        turned  (shown-by tree objects a)]
    (t/is (mth/close? 30 (:rotation turned)))
    ;; around its center as shown, not the one it has at rest
    (t/is (gpt/close? (gsh/shape->center (get shown a)) (gsh/shape->center turned)))))

(t/deftest every-board-shows-its-animation-as-it-plays
  (let [{:keys [objects timelines a]} (motion-scene)
        board   (cts/setup-shape {:type :frame :x 2000 :y 0 :width 1000 :height 600})
        bid     (:id board)
        c       (-> (cts/setup-shape {:type :rect :x 2100 :y 100 :width 100 :height 50})
                    (assoc :frame-id bid :parent-id bid))
        objects (assoc objects bid (assoc board :shapes [(:id c)]) (:id c) c)
        ;; C moves 400 to the right in 400 ms, over and over
        looping (-> (cta/make-timeline {:board-id bid :duration 400 :playback :loop})
                    (cta/add-keyframe (:id c) {:time 0 :property :x :value 100 :easing :linear})
                    (cta/add-keyframe (:id c) {:time 400 :property :x :value 500}))
        shown   (cltl/shown-shapes (assoc timelines bid looping) objects 500)]
    ;; A at 500 ms of its timeline, C at 100 ms of its second loop
    (t/is (= (get-in (cltl/shown-shapes timelines objects 500) [a :selrect :x])
             (get-in shown [a :selrect :x])))
    (t/is (mth/close? (+ 2000 200) (get-in shown [(:id c) :selrect :x])))))

(defn- component-file
  "An animated component: its main, a board at 0,0 whose rect moves from
  10 to 60 in 1 s, over and over. A copy of it in board B at 1000,100."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-simple-component :anim :main :main-rect
                                           :root-params {:x 0 :y 0 :width 200 :height 200}
                                           :child-params {:x 10 :y 20 :width 50 :height 50})
                 (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                 (thc/instantiate-component :anim :copy :parent-label :board-b))
        main (thi/id :main)
        tl   (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                 (cta/add-keyframe (thi/id :main-rect) {:time 0 :property :x :value 10 :easing :linear})
                 (cta/add-keyframe (thi/id :main-rect) {:time 1000 :property :x :value 60}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline main tl)))))

(defn- component-scene
  "The scene of `component-file`, or of `file`, one like it."
  ([]
   (component-scene (component-file)))
  ([file]
   (let [page    (thf/current-page file)
         objects (:objects page)
         copy    (thi/id :copy)]
     {:objects   objects
      :timelines (:timelines page)
      :board     (thi/id :board-b)
      :copy      copy
      :copy-rect (first (get-in objects [copy :shapes]))
      :main-page (constantly {:objects objects :timelines (:timelines page)})})))

(t/deftest copies-play-the-animation-of-their-main
  (let [{:keys [objects board copy copy-rect main-page]} (component-scene)
        at        #(- (get-in objects [%1 :selrect :x]) (get-in objects [%2 :selrect :x]))
        ;; where the rect of the copy is in B, less where the one of the
        ;; main is in the main
        dx        (- (at copy-rect board) (at (thi/id :main-rect) (thi/id :main)))
        timelines (cltl/instance-timelines objects board nil main-page)
        timeline  (get timelines [:copy copy (thi/id :main)])]
    (t/is (= [[:copy copy (thi/id :main)]] (keys timelines)))
    (t/is (= board (:board-id timeline)))
    (t/is (= [1000 :loop] [(:duration timeline) (:playback timeline)]) "the clock of its main")
    (t/is (= [(+ 10 dx) (+ 60 dx)] (x-values timeline copy-rect)))
    ;; at 250 ms the copy shows as the main does then
    (t/is (mth/close? (+ (get-in objects [board :selrect :x]) 10 dx 12.5)
                      (get-in (cltl/shown-shapes timelines objects 250) [copy-rect :selrect :x])))))

#?(:cljs
   (t/deftest the-timelines-of-copies-are-made-once-while-their-page-stays
     (let [file  (component-file)
           {:keys [objects timelines board]} (component-scene file)
           files {(:id file) file}
           once  (cltl/copy-timelines objects timelines files board)]
       (t/is (seq once) "the copy plays")
       (t/is (identical? once (cltl/copy-timelines objects timelines files board)) "every frame reads the same")
       (t/is (not (identical? once (cltl/copy-timelines objects (assoc timelines board {}) files board)))
             "other timelines make them anew")
       (t/is (not (identical? once (cltl/copy-timelines (assoc objects :x 1) timelines files board)))
             "and so do other objects")
       (let [page-id (:id (thf/current-page file))
             slower  (assoc-in files [(:id file) :data :pages-index page-id :timelines (thi/id :main) :duration] 2000)]
         (t/is (= [2000] (mapv :duration (vals (cltl/copy-timelines objects timelines slower board))))
               "and files where their main plays otherwise, as it plays now")))))

(t/deftest a-copy-plays-on-the-clock-its-board-gives-it
  ;; as a precomposition: from 500 ms on, at twice its pace
  (let [{:keys [objects board copy copy-rect main-page]} (component-scene)
        own       (cta/set-copy-clock (cta/make-timeline {:board-id board}) copy {:start 500 :speed 2})
        plain     (cltl/instance-timelines objects board nil main-page)
        timelines (cltl/instance-timelines objects board own main-page)
        timeline  (get timelines [:copy copy (thi/id :main)])
        ;; where it shows, at rest when nothing moves it
        x-at      #(or (get-in (cltl/shown-shapes %1 objects %2) [copy-rect :selrect :x])
                       (get-in objects [copy-rect :selrect :x]))]
    (t/is (= {:start 500 :speed 2} (cta/copy-clock own copy)))
    (t/is (= [500 2] [(:clock-start timeline) (:clock-speed timeline)]))
    (t/is (mth/close? (x-at plain 0) (x-at timelines 400)) "at rest until it starts")
    (t/is (mth/close? (x-at plain 250) (x-at timelines 625)) "then at twice its pace")
    (t/is (= 1000 (cta/clock-end timeline)) "a second of the main in half a second, from 500 ms")
    (t/is (= 1000 (:duration (cltl/with-copies-clock (cta/make-timeline {:board-id board}) [timeline] false)))
          "a board without tracks keeps time with it")
    (t/is (not (contains? (cta/set-copy-clock own copy {:start 0 :speed 1}) :copies)) "the defaults")
    (t/is (not (contains? (cta/remove-shapes own [copy]) :copies)) "a copy deleted")))

(t/deftest the-clock-of-a-copy-goes-as-a-setting-of-the-timeline
  ;; only what changed is sent (see `pcb/change-timeline`): the clocks
  ;; of the copies with the other settings
  (let [{:keys [board copy]} (component-scene)
        page    {:id (uuid/next) :timelines {board (cta/make-timeline {:board-id board})}}
        timed   (cta/set-copy-clock (get-in page [:timelines board]) copy {:start 300 :speed 2})
        changes (-> (pcb/empty-changes nil) (pcb/with-page page) (pcb/change-timeline board timed))
        change  (first (:redo-changes changes))]
    (t/is (= :update-timeline (:type change)))
    (t/is (= {:copies {copy {:start 300 :speed 2}}} (:attrs change)))
    (t/is (sm/validate cta/schema:timeline-attrs (:attrs change)))
    (t/is (not (sm/validate cta/schema:timeline-attrs {:copies {copy {:speed 10}}})) "within its pace")))

(defn- timed-copy-file
  "`component-file` with its copy in board B playing from 500 ms on at
  twice its pace, B looping in 4 s with no track, and an empty board C
  at 2000,100."
  []
  (let [file (-> (component-file)
                 (tho/add-frame :board-c :x 2000 :y 100 :width 400 :height 300))
        b    (thi/id :board-b)
        tl   (-> (cta/make-timeline {:board-id b :duration 4000 :playback :loop})
                 (cta/set-copy-clock (thi/id :copy) {:start 500 :speed 2}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline b tl)))))

(def ^:private timed
  {:start 500 :speed 2})

(t/deftest a-duplicated-board-keeps-the-clocks-of-its-copies
  ;; though the board has no track of its own
  (let [[file board] (duplicate (timed-copy-file) :board-b (gpt/point 0 500))
        tl           (get (timelines file) board)
        copy         (first (get-in (thf/current-page file) [:objects board :shapes]))]
    (t/is (not= (thi/id :copy) copy))
    (t/is (= timed (cta/copy-clock tl copy)))
    (t/is (= [4000 :loop] [(:duration tl) (:playback tl)]) "the settings of its board")
    (t/is (= timed (cta/copy-clock (get (timelines file) (thi/id :board-b)) (thi/id :copy)))
          "the original keeps its own")
    (t/is (cta/valid-timeline? tl))))

(t/deftest a-duplicated-copy-keeps-its-clock
  (let [[file copy] (duplicate (timed-copy-file) :copy (gpt/point 10 0))
        tl          (get (timelines file) (thi/id :board-b))]
    (t/is (= timed (cta/copy-clock tl copy)))
    (t/is (= timed (cta/copy-clock tl (thi/id :copy)))))
  (t/testing "by alt-drag: a board of its own until it is dropped"
    (let [file    (timed-copy-file)
          page    (thf/current-page file)
          id      (thi/id :copy)
          b       (thi/id :board-b)
          changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                  (:objects page) page #{id} (gpt/point 0 0)
                                                  {(:id file) file} (:data file) (:id file)
                                                  :alt-duplication? true)
          copy    (some #(when (and (= :add-obj (:type %)) (= id (:old-id %))) (-> % :obj :id))
                        (:redo-changes changes))
          file    (thf/apply-changes file changes)
          page    (thf/current-page file)
          dropped (thf/apply-changes file (cls/generate-relocate (-> (pcb/empty-changes nil)
                                                                     (pcb/with-page-id (:id page))
                                                                     (pcb/with-objects (:objects page))
                                                                     (pcb/with-library-data (:data file)))
                                                                 b 0 #{copy}))]
      (t/is (= timed (cta/copy-clock (get (timelines file) copy) copy)) "while it is dragged")
      (t/is (= timed (cta/copy-clock (get (timelines dropped) b) copy)) "once it is dropped")
      (t/is (not (contains? (timelines dropped) copy))))))

(t/deftest a-pasted-copy-brings-its-clock
  ;; what the clipboard takes of a copy that has no track of its own
  (let [file    (timed-copy-file)
        page    (thf/current-page file)
        id      (thi/id :copy)
        motion  (cltl/animation-sources page (:objects page)
                                        (cfh/get-children-ids-with-self (:objects page) id))
        changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                (:objects page) page #{id} (gpt/point 0 0)
                                                {(:id file) file} (:data file) (:id file)
                                                {:motion motion})
        copy    (some #(when (and (= :add-obj (:type %)) (= id (:old-id %))) (-> % :obj :id))
                      (:redo-changes changes))
        file    (thf/apply-changes file changes)]
    (t/is (empty? (:tracks motion)))
    (t/is (= timed (cta/copy-clock (get (timelines file) (thi/id :board-b)) copy)))))

(t/deftest a-moved-copy-takes-its-clock-along
  (let [file (relocate (timed-copy-file) :copy :board-c)
        tls  (timelines file)
        c    (get tls (thi/id :board-c))]
    (t/is (= timed (cta/copy-clock c (thi/id :copy))))
    (t/is (= [4000 :loop] [(:duration c) (:playback c)]) "the settings of the board it left")
    (t/is (not (contains? (get tls (thi/id :board-b)) :copies)))
    (t/is (cta/valid-timeline? c))))

(t/deftest exports-play-a-copy-on-its-clock
  (let [{:keys [objects board copy copy-rect main-page]} (component-scene)
        own      (cta/set-copy-clock (cta/make-timeline {:board-id board :duration 2000}) copy {:start 500 :speed 2})
        plain    (vals (cltl/instance-timelines objects board nil main-page))
        copies   (vals (cltl/instance-timelines objects board own main-page))
        exported #(cta/expand-loops (cltl/with-instance-tracks (cta/make-timeline {:board-id board :duration 2000}) % objects))
        x-at     #(get-in (cta/values-at %1 %2) [copy-rect :x])]
    (t/is (= [500.0] (mapv double (:copy-loops (cltl/with-instance-tracks own copies objects))))
          "it loops every 500 ms")
    (t/is (mth/close? (x-at (exported plain) 0) (x-at (exported copies) 300)) "at rest until it starts")
    (t/is (mth/close? (x-at (exported plain) 250) (x-at (exported copies) 625)))))

(t/deftest the-sound-of-a-copy-keeps-its-clock
  (let [clip  {:id (uuid/next) :audio-id (uuid/next) :name "ding.wav" :source-duration 1000
               :start 0 :offset 0 :duration 1000 :lane 0 :fade-out 200}
        copy  {:duration 1000 :playback :once :clock-start 500 :clock-speed 2
               :audio-clips {(:id clip) clip}}
        board (cltl/with-instance-clips (cta/make-timeline {:board-id (uuid/next) :duration 3000})
                {[:copy (uuid/next) (uuid/next)] copy})
        baked (first (vals (:audio-clips board)))]
    (t/is (= [500 500 2 100] ((juxt :start :duration :speed :fade-out) baked))
          "from 500 ms, at twice its pace")))

(t/deftest a-copy-animated-in-its-board-keeps-that-animation
  (let [{:keys [objects board copy-rect main-page]} (component-scene)
        own (-> (cta/make-timeline {:board-id board})
                (cta/add-keyframe copy-rect {:time 0 :property :opacity :value 1}))]
    (t/is (empty? (cltl/instance-timelines objects board own main-page)))
    (t/is (empty? (cltl/instance-timelines objects (thi/id :main) nil main-page))
          "a main plays its own timeline")))

(t/deftest exports-play-the-copies-in-a-board
  (let [{:keys [objects board copy-rect main-page]} (component-scene)
        at     #(- (get-in objects [%1 :selrect :x]) (get-in objects [%2 :selrect :x]))
        dx     (- (at copy-rect board) (at (thi/id :main-rect) (thi/id :main)))
        copies (vals (cltl/instance-timelines objects board nil main-page))
        host   (-> (cta/make-timeline {:board-id board})
                   (cltl/with-copies-clock copies false))
        tl     (cltl/with-instance-tracks host copies objects)
        x-at   #(get-in (cta/values-at (cta/expand-loops tl) %) [copy-rect :x])]
    (t/is (= [1000 :loop] [(:duration host) (:playback host)])
          "a board without tracks keeps time with its copies")
    (t/is (= [1000] (:copy-loops tl)) "the cycles of the copies that loop")
    (t/is (mth/close? (+ 10 dx 12.5) (x-at 250)))
    (let [longer (cltl/with-instance-tracks (assoc host :duration 3000) copies objects)]
      ;; in a board that plays 3 s, the copy plays its second second as
      ;; its first (unrolled, each round starts 1 ms late)
      (t/is (mth/close? (+ 10 dx 12.5)
                        (get-in (cta/values-at (cta/expand-loops longer) 1250) [copy-rect :x])
                        0.1)))
    (t/is (= [3000 :once] (-> (cta/make-timeline {:board-id board :duration 3000})
                              (cta/add-keyframe board {:time 0 :property :opacity :value 1})
                              (cltl/with-copies-clock copies true)
                              ((juxt :duration cta/playback-mode))))
          "one with tracks keeps its own")
    (t/is (= 2500 (-> (cta/make-timeline {:board-id board :duration 2500})
                      (cltl/with-copies-clock copies true)
                      :duration))
          "a longer duration set on it")))

(defn- scale-copy
  "`objects` with the copy `copy-id` and its layers as big as `factor`
  times, from its top left corner (only their selrects, all the timelines
  read)."
  [objects copy-id factor]
  (let [{cx :x cy :y} (get-in objects [copy-id :selrect])]
    (reduce (fn [objects id]
              (update-in objects [id :selrect]
                         (fn [{:keys [x y width height] :as rect}]
                           (assoc rect
                                  :x (+ cx (* factor (- x cx)))
                                  :y (+ cy (* factor (- y cy)))
                                  :width (* factor width)
                                  :height (* factor height)))))
            objects
            (cons copy-id (cfh/get-children-ids objects copy-id)))))

(t/deftest a-scaled-copy-plays-its-main-scaled
  (let [{:keys [objects board copy copy-rect timelines]} (component-scene)
        objects   (scale-copy objects copy 2)
        main-page (constantly {:objects objects :timelines timelines})
        at        (- (get-in objects [copy-rect :selrect :x]) (get-in objects [board :selrect :x]))
        timeline  (get (cltl/instance-timelines objects board nil main-page) [:copy copy (thi/id :main)])]
    ;; from where it is, twice as far: 50 in the main, 100 here
    (t/is (= [at (+ at 100)] (x-values timeline copy-rect)))))

(defn- outline-corners
  "The path content of the corners `points`, closed."
  [points]
  (path/from-plain
   (-> [{:command :move-to :params (zipmap [:x :y] (first points))}]
       (into (map (fn [p] {:command :line-to :params (zipmap [:x :y] p)})) (rest points))
       (conj {:command :close-path :params {}}))))

(t/deftest a-scaled-copy-plays-the-morph-of-its-main-scaled
  ;; the main holds a triangle (40 by 40) whose outline goes to a flat
  ;; one (40 by 20); a copy twice as big draws it twice as big
  (let [file      (-> (thf/sample-file :file1)
                      (tho/add-simple-component :anim :main :main-path
                                                :root-params {:x 0 :y 0 :width 200 :height 200}
                                                :child-params {:type :path
                                                               :content (outline-corners [[10 20] [50 20] [10 60]])})
                      (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                      (thc/instantiate-component :anim :copy :parent-label :board-b))
        main-path (get-in (thf/current-page file) [:objects (thi/id :main-path)])
        tl        (-> (cta/make-timeline {:board-id (thi/id :main) :duration 1000})
                      (cta/set-outline-at main-path 0 (ctao/outline-of main-path))
                      (cta/set-outline-at main-path 1000 (outline-corners [[0 0] [40 0] [40 20]])))
        file      (thf/apply-changes file (-> (pcb/empty-changes nil)
                                              (pcb/with-page (thf/current-page file))
                                              (pcb/set-timeline (thi/id :main) tl)))
        {:keys [objects board copy copy-rect timelines]} (component-scene file)
        objects   (scale-copy objects copy 2)
        timeline  (get (cltl/instance-timelines objects board nil (constantly {:objects objects :timelines timelines}))
                       [:copy copy (thi/id :main)])
        track     (get-in timeline [:tracks copy-rect])
        end       (:value (last (cta/property-keyframes timeline copy-rect :content)))
        drawn     (path/calc-selrect (ctao/outline->content (get objects copy-rect) track end))]
    (t/is (some? end) "the copy plays the outline of the main")
    (t/is (= [80 40] [(mth/round (:width drawn)) (mth/round (:height drawn))])
          "twice as big, through the box its row was made for")))

(t/deftest a-component-in-a-copy-plays-its-own-main
  (let [file      (-> (thf/sample-file :file1)
                      (tho/add-nested-component :inner :inner-main :inner-rect
                                                :outer :outer-main :nested
                                                :root1-params {:x 0 :y 0 :width 200 :height 200}
                                                :main1-child-params {:x 10 :y 20 :width 50 :height 50}
                                                :main2-root-params {:x 1000 :y 0 :width 400 :height 400})
                      (tho/add-frame :host :x 3000 :y 100 :width 600 :height 600)
                      (thc/instantiate-component :outer :copy :parent-label :host))
        inner     (thi/id :inner-main)
        tl        (-> (cta/make-timeline {:board-id inner :duration 800 :playback :loop})
                      (cta/add-keyframe (thi/id :inner-rect) {:time 0 :property :x :value 10 :easing :linear})
                      (cta/add-keyframe (thi/id :inner-rect) {:time 800 :property :x :value 60}))
        file      (thf/apply-changes file (-> (pcb/empty-changes nil)
                                              (pcb/with-page (thf/current-page file))
                                              (pcb/set-timeline inner tl)))
        page      (thf/current-page file)
        objects   (:objects page)
        host      (thi/id :host)
        copy      (thi/id :copy)
        ;; the rect of the component inside the copy of the outer one
        rect      (->> (cfh/get-children-ids objects copy)
                       (filter #(= :rect (get-in objects [% :type])))
                       (first))
        at        (- (get-in objects [rect :selrect :x]) (get-in objects [host :selrect :x]))
        timelines (cltl/instance-timelines objects host nil
                                           (constantly {:objects objects :timelines (:timelines page)}))
        timeline  (get timelines [:copy copy inner])]
    (t/is (some? rect))
    (t/is (= [[:copy copy inner]] (keys timelines)) "on the clock of the inner main")
    (t/is (= [800 :loop] [(:duration timeline) (:playback timeline)]))
    (t/is (= [at (+ at 50)] (x-values timeline rect)))))

;; ANIMATION CLIPS: what copying the animation of layers takes

(defn- close-all?
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map #(mth/close? (double %1) (double %2) 0.001) expected actual))))

(defn- with-timeline
  "`file` with the timeline of the board `label` changed by `f`."
  [file label f]
  (let [page (thf/current-page file)
        id   (thi/id label)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline id (f (get (:timelines page) id)))))))

(defn- clip-of
  "The animation clip of the layers `labels` of `file`."
  [file labels & {:as opts}]
  (let [page (thf/current-page file)]
    (cltl/animation-clip (:id file) page (:objects page) {(:id file) file}
                         (mapv thi/id labels) opts)))

(t/deftest a-clip-holds-what-a-layer-plays
  (let [file  (with-timeline (animated-file) :board-a #(cta/set-track-origin % (thi/id :rect) 0 1))
        clip  (clip-of file [:rect] :whole-layer? true)
        layer (first (:layers clip))]
    (t/is (= [(:id file) (:id (thf/current-page file))] [(:file-id clip) (:page-id clip)]))
    (t/is (= (thi/id :rect) (:shape-id layer)))
    (t/is (= [50 150] (mapv :value (get-in layer [:track :keyframes]))))
    (t/is (= [:fade] (mapv :type (get-in layer [:track :animations]))))
    (t/is (= {:x 0.0 :y 1.0} (get-in layer [:track :origin])))
    (t/is (close-all? [50 100 50 50 0] ((juxt :x :y :width :height :rotation) (:pose layer))))
    (t/is (cltl/animation-clip-valid? clip))))

(t/deftest a-clip-of-the-whole-layer-leaves-out-hidden-rows
  (let [file (animated-file)
        rect (thi/id :rect)
        file (with-timeline file :board-a #(cta/toggle-slot-flag % rect :hidden :x))]
    (t/testing "the whole layer"
      (let [track (-> (clip-of file [:rect] :whole-layer? true) :layers first :track)]
        (t/is (empty? (:keyframes track)))
        (t/is (= [:fade] (mapv :type (:animations track))))
        (t/is (nil? (:hidden track)))))
    (t/testing "one row, hidden or not"
      (let [track (-> (clip-of file [:rect] :properties [:x] :presets? false) :layers first :track)]
        (t/is (= [50 150] (mapv :value (:keyframes track))))
        (t/is (nil? (:animations track)))))
    (t/testing "some keyframes"
      (let [keyframe (-> (get-in (thf/current-page file) [:timelines (thi/id :board-a)])
                         (cta/property-keyframes rect :x)
                         first)
            track    (-> (clip-of file [:rect] :keyframe-ids [(:id keyframe)] :presets? false)
                         :layers first :track)]
        (t/is (= [50] (mapv :value (:keyframes track))))))))

(t/deftest a-clip-leaves-out-what-is-not-animated
  (t/is (= [] (:layers (clip-of (animated-file) [:board-b] :whole-layer? true)))))

(t/deftest a-clip-of-a-copy-holds-the-animation-of-its-main
  (let [file      (component-file)
        page      (thf/current-page file)
        objects   (:objects page)
        files     {(:id file) file}
        board     (thi/id :board-b)
        copy-rect (first (get-in objects [(thi/id :copy) :shapes]))
        at        #(- (get-in objects [%1 :selrect :x]) (get-in objects [%2 :selrect :x]))
        dx        (- (at copy-rect board) (at (thi/id :main-rect) (thi/id :main)))
        layer     (-> (cltl/animation-clip (:id file) page objects files [copy-rect] {:whole-layer? true})
                      :layers first)]
    (t/is (= copy-rect (:shape-id layer)))
    (t/is (close-all? [(+ 10 dx) (+ 60 dx)] (mapv :value (get-in layer [:track :keyframes]))))
    (t/is (mth/close? (at copy-rect board) (get-in layer [:pose :x])))
    (t/is (cltl/animated-layer? page files copy-rect))
    (t/is (not (cltl/animated-layer? page files board)))))

(t/deftest animation-clip-valid-checks-what-comes-from-the-clipboard
  (let [clip (clip-of (animated-file) [:rect] :whole-layer? true)
        with (fn [f] (update-in clip [:layers 0 :track] f))]
    (t/is (cltl/animation-clip-valid? clip))
    (t/is (not (cltl/animation-clip-valid? (with #(assoc-in % [:keyframes 0 :value] "#ffffff")))))
    (t/is (cltl/animation-clip-valid? (with #(assoc-in % [:keyframes 0 :time] -10)))
          "a keyframe before the start, as on the timeline")
    (t/is (cltl/animation-clip-valid? (with #(assoc-in % [:animations 0 :start] -1)))
          "a preset from before the start")
    (t/is (not (cltl/animation-clip-valid?
                (assoc clip :layers (vec (repeat (inc cltl/max-clip-layers) (first (:layers clip))))))))
    (t/testing "the keyframe entries of a style use"
      (let [use  (ctas/make-use (ctas/make-animation-style
                                 {:entries [{:type :keys :property :x :start 0 :duration 400
                                             :keyframes [{:time 0 :value 0} {:time 400 :value 40}]}]})
                                (uuid/next) 0)
            with (fn [f] (update-in clip [:layers 0 :track :animations] (fnil conj []) (f use)))]
        (t/is (cltl/animation-clip-valid? (with identity)))
        (t/is (not (cltl/animation-clip-valid? (with #(assoc-in % [:entries 0 :keyframes 1 :time] 500))))
              "a keyframe past its entry")
        (t/is (not (cltl/animation-clip-valid? (with #(assoc-in % [:entries 0 :keyframes 1 :value] "#ffffff"))))
              "a colour on a position")))))

(t/deftest a-clip-takes-the-audio-clips-copied-along
  (let [ping   (uuid/next)
        tl     (reduce ctau/add-audio-clip
                       (cta/make-timeline {:board-id (uuid/next) :duration 3000})
                       [{:id (uuid/next) :audio-id ping :name "ping.wav" :source-duration 1000
                         :start 800 :duration 500 :locked true}
                        {:id (uuid/next) :audio-id ping :name "ping.wav" :source-duration 1000
                         :start 200 :duration 500}])
        ids    (keys (:audio-clips tl))
        audio  (cltl/audio-copy tl ids)
        clip   (assoc (clip-of (animated-file) [:rect] :whole-layer? true)
                      :board-id (:board-id tl) :audio audio)]
    (t/is (= [200 800] (mapv :start audio)) "by start")
    (t/is (not-any? :locked audio) "never locked")
    (t/is (cltl/animation-clip-valid? clip))
    (t/is (cltl/animation-clip-valid? (assoc clip :layers [])) "the clips alone")
    (t/is (not (cltl/animation-clip-valid? (assoc clip :audio (vec (repeat (inc cltl/max-clip-audio) (first audio)))))))
    (t/is (= 0 (cltl/paste-start clip)) "the first keyframe")
    (t/is (= 200 (cltl/paste-start (assoc clip :layers []))) "the first clip")))

(t/deftest pasted-audio-clips-get-a-lane-of-their-own-file
  (let [[ping beat] [(uuid/next) (uuid/next)]
        clip        #(ctau/make-audio-clip (merge {:audio-id ping :name "x" :source-duration 3000
                                                   :start 0 :duration 500}
                                                  %))
        ;; a lane of another file, one of the same file that is free there
        tl          (reduce ctau/add-audio-clip
                            (cta/make-timeline {:board-id (uuid/next) :duration 2000})
                            [(clip {:audio-id beat :lane 0}) (clip {:lane 1 :start 1500})])
        copied      [(clip {:start 100}) (clip {:start 300 :lane 2})]
        {:keys [timeline ids]} (cltl/paste-audio tl copied 900)
        pasted      (mapv #(ctau/get-audio-clip timeline %) ids)]
    (t/is (= [[1000 1] [1200 2]] (mapv (juxt :start :lane) pasted))
          "900 ms later: on the lane of their file where it is free, the next one where it is not")
    (t/is (empty? (filter (set (map :id copied)) ids)) "new ids")
    (t/is (= 2000 (:duration timeline)) "the duration stays")
    (let [{:keys [timeline ids]} (cltl/paste-audio tl [(clip {:start 100})] -500)]
      (t/is (= [-400] (mapv #(:start (ctau/get-audio-clip timeline %)) ids)) "before 0 too"))))

;; PASTING ANIMATION CLIPS

(defn- paste-file
  "`animated-file` with more rects: one in board A at 300,200 (200,100 in
  it), one in board B at 1100,150 (100,50 in it), and one out of any
  board."
  []
  (-> (animated-file)
      (ths/add-sample-shape :rect-a :type :rect :parent-label :board-a
                            :x 300 :y 200 :width 50 :height 50)
      (ths/add-sample-shape :rect-b :type :rect :parent-label :board-b
                            :x 1100 :y 150 :width 100 :height 100)
      (ths/add-sample-shape :loose :type :rect :x 3000 :y 3000 :width 10 :height 10)))

(defn- paste
  "Paste the first layer of `clip` onto the layers `labels` of `file`."
  [file clip labels opts]
  (let [page  (thf/current-page file)
        layer (first (:layers clip))
        pairs (mapv (fn [label] {:layer layer :target-id (thi/id label)}) labels)]
    (cltl/paste-animation (:id file) (:id page) (:timelines page) (:objects page) clip pairs opts)))

(defn- track-in
  [result board-label label]
  (get-in result [:timelines (thi/id board-label) :tracks (thi/id label)]))

(defn- row
  "`[time value]` of the keyframes of `property` of `track`."
  [track property]
  (->> (:keyframes track)
       (filter #(= property (:property %)))
       (mapv (juxt :time :value))))

(t/deftest paste-takes-the-animation-where-each-target-is
  (let [file   (paste-file)
        clip   (clip-of file [:rect] :whole-layer? true)
        result (paste file clip [:rect-a :rect-b] {:mode :replace})
        b      (get-in result [:timelines (thi/id :board-b)])]
    (t/is (close-all? [200 300] (mapv second (row (track-in result :board-a :rect-a) :x))))
    (t/is (close-all? [100 200] (mapv second (row (track-in result :board-b :rect-b) :x))))
    (t/is (= #{(thi/id :rect-a) (thi/id :rect-b)} (:targets result)))
    (t/is (= [:fade] (mapv :type (:animations (track-in result :board-b :rect-b)))) "presets too")
    (t/testing "a board without a timeline gets one named after it"
      (t/is (= (get-in (thf/current-page file) [:objects (thi/id :board-b) :name]) (:name b)))
      (t/is (cta/valid-timeline? b)))))

(t/deftest paste-replaces-what-the-clip-carries
  (let [file (with-timeline (paste-file) :board-a
               #(-> %
                    (cta/add-keyframe (thi/id :rect-a) {:time 0 :property :x :value 0})
                    (cta/add-keyframe (thi/id :rect-a) {:time 0 :property :opacity :value 1})
                    (cta/add-animation (thi/id :rect-a) {:type :scale :start 0 :duration 300})))]
    (t/testing "only the rows it has keyframes of"
      (let [clip  (clip-of file [:rect] :properties [:x] :presets? false)
            track (track-in (paste file clip [:rect-a] {:mode :replace}) :board-a :rect-a)]
        (t/is (close-all? [200 300] (mapv second (row track :x))))
        (t/is (= [[0 1]] (row track :opacity)))
        (t/is (= [:scale] (mapv :type (:animations track))))))
    (t/testing "its presets in place of the target's"
      (let [clip  (clip-of file [:rect] :whole-layer? true)
            track (track-in (paste file clip [:rect-a] {:mode :replace}) :board-a :rect-a)]
        (t/is (= [:fade] (mapv :type (:animations track))))))))

(t/deftest paste-keeps-what-is-locked
  (let [file  (with-timeline (paste-file) :board-a
                #(let [id (thi/id :rect-a)
                       tl (-> %
                              (cta/add-keyframe id {:time 0 :property :x :value 0})
                              (cta/add-animation id {:type :scale :start 0 :duration 300})
                              (cta/toggle-slot-flag id :locked :x))]
                   (cta/toggle-animation-flag tl id (-> tl (cta/get-track id) :animations first :id) :locked)))
        clip  (clip-of file [:rect] :whole-layer? true)
        track (track-in (paste file clip [:rect-a] {:mode :replace}) :board-a :rect-a)]
    (t/is (= [[0 0]] (row track :x)) "a locked row takes nothing")
    (t/is (= [:scale :fade] (mapv :type (:animations track))) "a locked preset stays")))

(t/deftest paste-merges-at-the-playhead
  (let [file  (with-timeline (paste-file) :board-a
                #(cta/add-keyframe % (thi/id :rect-a) {:time 0 :property :x :value 0}))
        clip  (clip-of file [:rect] :whole-layer? true)
        track (track-in (paste file clip [:rect-a] {:mode :merge :time 500}) :board-a :rect-a)]
    (t/is (= [0 500 1500] (mapv first (row track :x))))
    (t/is (= [[500 900]] (mapv (juxt :start cta/animation-end) (:animations track))))))

(t/deftest paste-onto-the-layer-copied
  (let [file (paste-file)
        clip (clip-of file [:rect] :whole-layer? true)
        rect (thi/id :rect)]
    (t/testing "the dock pastes onto it again"
      (let [track (track-in (paste file clip [:rect] {:mode :merge :time 1000}) :board-a :rect)]
        (t/is (= [[0 50] [1000 50] [2000 150]] (row track :x)))))
    (t/testing "the canvas leaves it out"
      (t/is (= #{(thi/id :rect-a)}
               (:targets (paste file clip [:rect :rect-a] {:mode :replace :skip-source? true})))))
    (t/testing "a layer with its id on another page is another layer"
      (t/is (= #{rect}
               (:targets (paste file (assoc clip :page-id (uuid/next)) [:rect]
                                {:mode :replace :skip-source? true})))))))

(t/deftest a-merge-paste-leaves-out-outlines-past-what-a-row-holds
  ;; five outlines of 4000 segments onto a path with five of its own: the
  ;; row would pass 32768, so the clip's outlines stay out
  (let [big      (fn [k] (path/from-plain (into [{:command :move-to :params {:x 0 :y 0}}]
                                                (map (fn [i] {:command :line-to :params {:x i :y (mod (+ i k) 7)}}))
                                                (range 1 4000))))
        file     (-> (paste-file)
                     (ths/add-sample-shape :src :type :path :parent-label :board-a
                                           :content (outline-corners [[10 20] [50 20] [10 60]]))
                     (ths/add-sample-shape :dst :type :path :parent-label :board-a
                                           :content (outline-corners [[110 20] [150 20] [110 60]])))
        objects  (:objects (thf/current-page file))
        file     (with-timeline file :board-a
                   #(as-> % tl
                      (reduce (fn [tl t] (cta/set-outline-at tl (get objects (thi/id :src)) t (big t))) tl [0 100 200 300 400])
                      (reduce (fn [tl t] (cta/set-outline-at tl (get objects (thi/id :dst)) t (big t))) tl [1000 1100 1200 1300 1400])))
        clip     (clip-of file [:src] :whole-layer? true)
        merged   (paste file clip [:dst] {:mode :merge})
        replaced (paste file clip [:dst] {:mode :replace})
        times    (fn [track] (mapv :time (filter #(= :content (:property %)) (:keyframes track))))]
    (t/is (= 5 (:outlines-left-out merged)))
    (t/is (= 5 (:skipped merged)) "counted among what the target could not take")
    (t/is (= [1000 1100 1200 1300 1400] (times (track-in merged :board-a :dst))) "its own outlines as they were")
    (t/is (sm/validate cta/schema:track (track-in merged :board-a :dst)) "a row the file takes")
    (t/is (= [0 100 200 300 400] (times (track-in replaced :board-a :dst))) "replacing its own, the clip's go in")
    (t/is (zero? (:outlines-left-out replaced)))))

(t/deftest paste-gives-the-pivot-to-a-target-that-does-not-turn
  (let [file   (with-timeline (paste-file) :board-a
                 #(-> %
                      (cta/add-keyframe (thi/id :rect) {:time 0 :property :rotation :value 0})
                      (cta/add-keyframe (thi/id :rect) {:time 1000 :property :rotation :value 90})
                      (cta/set-track-origin (thi/id :rect) 0 1)
                      (cta/add-keyframe (thi/id :rect-a) {:time 0 :property :rotation :value 10})))
        clip   (clip-of file [:rect] :whole-layer? true)
        result (paste file clip [:rect-a :rect-b] {:mode :merge})]
    (t/is (= 1 (:pivot-kept result)))
    (t/is (nil? (:origin (track-in result :board-a :rect-a))))
    (t/is (= {:x 0.0 :y 1.0} (:origin (track-in result :board-b :rect-b))))))

(t/deftest paste-targets-leave-out-what-a-target-carries
  (let [file    (-> (paste-file)
                    (tho/add-group :group :parent-label :board-a)
                    (ths/add-sample-shape :child :type :rect :parent-label :group
                                          :x 120 :y 120 :width 10 :height 10))
        objects (:objects (thf/current-page file))
        layers  (get-in objects [(thi/id :board-a) :shapes])]
    (t/is (= [(thi/id :group)] (cltl/paste-targets objects [(thi/id :child) (thi/id :group)])))
    (t/is (= (vec (reverse layers)) (cltl/paste-targets objects layers))
          "from the top-most layer down")))

(t/deftest paste-staggers-the-targets
  (let [file   (ths/add-sample-shape (paste-file) :rect-c :type :rect :parent-label :board-a
                                     :x 400 :y 200 :width 10 :height 10)
        clip   (clip-of file [:rect] :properties [:x] :presets? false)
        result (paste file clip [:rect-a :rect-c :rect-b] {:mode :replace :time 0 :stagger 100})
        start  #(ffirst (row (track-in result %1 %2) :x))]
    (t/is (= [0 100 200] [(start :board-a :rect-a) (start :board-a :rect-c) (start :board-b :rect-b)]))))

(t/deftest paste-grows-the-boards-to-fit
  (let [file   (paste-file)
        clip   (clip-of file [:rect] :whole-layer? true)
        result (paste file clip [:rect-a :rect-b] {:mode :replace :time 800})]
    (t/is (= 1800 (get-in result [:timelines (thi/id :board-a) :duration])))
    (t/is (= 1800 (get-in result [:timelines (thi/id :board-b) :duration])))
    (t/is (= #{(thi/id :board-a)} (:grown result)) "only the boards that had a timeline")))

(t/deftest paste-refuses-what-is-too-large
  (let [file  (paste-file)
        page  (thf/current-page file)
        clip  (clip-of file [:rect] :whole-layer? true)
        layer (first (:layers clip))
        heavy (assoc-in layer [:track :keyframes]
                        (mapv #(cta/make-keyframe {:time % :property :x :value %}) (range 2001)))
        paste #(cltl/paste-animation (:id file) (:id page) (:timelines page) (:objects page) clip
                                     (vec (repeat %2 {:layer %1 :target-id (thi/id :rect-a)}))
                                     {:mode :replace})]
    (t/is (= {:error :too-large} (paste layer (inc cltl/max-paste-targets))))
    (t/is (= {:error :too-large} (paste heavy 5)) "2001 keyframes onto 5 layers")))

(t/deftest paste-skips-a-layer-out-of-any-board
  (let [file (paste-file)
        clip (clip-of file [:rect] :whole-layer? true)]
    (t/is (= #{(thi/id :rect-a)} (:targets (paste file clip [:loose :rect-a] {:mode :replace}))))))

(t/deftest a-paste-is-one-step-to-undo
  (let [file    (paste-file)
        page    (thf/current-page file)
        clip    (clip-of file [:rect] :whole-layer? true)
        pairs   (mapv (fn [label] {:layer (first (:layers clip)) :target-id (thi/id label)})
                      [:rect-a :rect-b])
        changes (:changes (cltl/generate-paste-animation (pcb/empty-changes nil) (:id file) page clip
                                                         pairs {:mode :replace}))
        pasted  (thf/apply-changes file changes)]
    (t/is (= 2 (count (timelines pasted))))
    (t/is (= (timelines file) (timelines (thf/apply-undo-changes pasted changes))))))

;; ANIMATION STYLE USES

(defn- style-use
  "A use, from 100 ms, of a style of a fade in (0-300 ms), a scale in
  (100-400 ms) and a move out to the right (200-400 ms), with `params`
  over it."
  [& {:as params}]
  (cta/make-animation
   (merge {:type :style
           :style-ref-id (uuid/next)
           :style-ref-file (uuid/next)
           :name "Pop"
           :start 100
           :entries [{:type :fade :start 0 :duration 300}
                     {:type :scale :start 100 :duration 300}
                     {:type :move :direction :out :start 200 :duration 200 :offset-x 30 :offset-y 0}]}
          params)))

(defn- values-at
  "The values of `shape-id` that `timeline` plays at `time`."
  [timeline objects shape-id time]
  (get (cta/values-at (cta/resolve-animations timeline objects) time) shape-id))

(t/deftest copies-play-a-style-use-of-their-main
  (let [file     (with-timeline (component-file) :main
                   #(cta/add-animation % (thi/id :main-rect) (style-use)))
        {:keys [objects timelines board copy copy-rect main-page]} (component-scene file)
        main     (get timelines (thi/id :main))
        copies   (cltl/instance-timelines objects board nil main-page)
        timeline (get copies [:copy copy (thi/id :main)])
        exported (cltl/with-instance-tracks (cta/make-timeline {:board-id board}) (vals copies) objects)
        shown    (fn [time] (get (cta/values-at (cta/expand-loops exported) time) copy-rect))]
    (t/is (cta/style-use? (first (get-in timeline [:tracks copy-rect :animations]))))
    (t/is (nil? (get-in exported [:tracks copy-rect :animations])) "the exports bake it")
    (doseq [time [0 150 250 350 450]
            :let [played (values-at main objects (thi/id :main-rect) time)]]
      (t/is (close-all? ((juxt :opacity :scale-x) played)
                        ((juxt :opacity :scale-x) (values-at timeline objects copy-rect time)))
            (str "t=" time))
      (t/is (close-all? ((juxt :opacity :scale-x) played) ((juxt :opacity :scale-x) (shown time)))
            (str "exported, t=" time)))))

(t/deftest a-style-use-goes-along-with-a-paste
  (let [use    (style-use)
        file   (with-timeline (paste-file) :board-a #(cta/add-animation % (thi/id :rect) use))
        clip   (clip-of file [:rect] :whole-layer? true)
        result (paste file clip [:rect-b] {:mode :replace})
        pasted (first (filter cta/style-use? (:animations (track-in result :board-b :rect-b))))
        kept   #(select-keys % [:style-ref-id :style-ref-file :name :entries :start :duration])]
    (t/is (cltl/animation-clip-valid? clip))
    (t/is (= (kept use) (kept pasted)))
    (t/is (not= (:id use) (:id pasted)))
    (t/testing "the target plays it from where it rests"
      (let [objects (:objects (thf/current-page file))
            source  (get (timelines file) (thi/id :board-a))
            target  (get-in result [:timelines (thi/id :board-b)])]
        (doseq [time [0 150 250 350 450 600 1000]
                :let [a (values-at source objects (thi/id :rect) time)
                      b (values-at target objects (thi/id :rect-b) time)]]
          (t/is (close-all? ((juxt :opacity :scale-x) a) ((juxt :opacity :scale-x) b))
                (str "t=" time))
          ;; rect rests at 50 in board A, rect-b at 100 in board B
          (t/is (mth/close? 50 (- (:x b) (:x a))) (str "t=" time)))))))

(t/deftest a-clip-checks-the-style-uses-it-carries
  (let [file (with-timeline (animated-file) :board-a #(cta/add-animation % (thi/id :rect) (style-use)))
        clip (clip-of file [:rect] :whole-layer? true)
        with (fn [f] (update-in clip [:layers 0 :track :animations 1] f))]
    (t/is (cta/style-use? (get-in clip [:layers 0 :track :animations 1])))
    (t/is (cltl/animation-clip-valid? clip))
    (t/is (cltl/animation-clip-valid? (with #(assoc % :start -1))) "a use from before the start")
    (t/is (not (cltl/animation-clip-valid? (with #(assoc-in % [:entries 0 :start] -1)))))))

(t/deftest a-style-use-counts-its-entries-toward-the-paste-limit
  (let [file  (paste-file)
        page  (thf/current-page file)
        use   (style-use :entries (mapv #(hash-map :type :fade :start (* 10 %) :duration 10) (range 32)))
        clip  (clip-of file [:rect] :whole-layer? true)
        layer (update (first (:layers clip)) :track assoc :keyframes [] :animations [use])
        paste #(cltl/paste-animation (:id file) (:id page) (:timelines page) (:objects page) clip
                                     (vec (repeat % {:layer layer :target-id (thi/id :rect-a)}))
                                     {:mode :replace})
        most  (quot cltl/max-paste-keyframes 32)]
    (t/is (= 32 (count (:entries use))))
    (t/is (= {:error :too-large} (paste (inc most))) "32 for each target")
    (t/is (= #{(thi/id :rect-a)} (:targets (paste most))))))

(t/deftest paste-keeps-the-pivot-of-a-style-use-that-turns
  (let [spin (style-use :entries [{:type :rotate :start 0 :duration 400}])]
    (t/testing "on the target"
      (let [file   (with-timeline (paste-file) :board-a
                     #(-> %
                          (cta/add-keyframe (thi/id :rect) {:time 0 :property :rotation :value 0})
                          (cta/add-keyframe (thi/id :rect) {:time 1000 :property :rotation :value 90})
                          (cta/set-track-origin (thi/id :rect) 0 1)
                          (cta/add-animation (thi/id :rect-a) spin)))
            result (paste file (clip-of file [:rect] :whole-layer? true) [:rect-a :rect-b] {:mode :merge})]
        (t/is (= 1 (:pivot-kept result)))
        (t/is (nil? (:origin (track-in result :board-a :rect-a))))
        (t/is (= {:x 0.0 :y 1.0} (:origin (track-in result :board-b :rect-b))))))
    (t/testing "in the clip"
      (let [file   (with-timeline (paste-file) :board-a
                     #(-> %
                          (cta/add-animation (thi/id :rect) spin)
                          (cta/set-track-origin (thi/id :rect) 0 1)))
            result (paste file (clip-of file [:rect] :whole-layer? true) [:rect-b] {:mode :merge})]
        (t/is (= 0 (:pivot-kept result)))
        (t/is (= {:x 0.0 :y 1.0} (:origin (track-in result :board-b :rect-b))))))))

(t/deftest paste-replaces-the-style-uses-of-the-target
  (let [file   (paste-file)
        rect-a (thi/id :rect-a)
        file   (with-timeline file :board-a
                 #(let [tl (-> %
                               (cta/add-animation rect-a {:type :scale :start 0 :duration 300})
                               (cta/add-animation rect-a (style-use :name "Mine"))
                               (cta/add-animation rect-a (style-use :name "Locked")))]
                    (cta/toggle-animation-flag tl rect-a (-> tl (cta/get-track rect-a) :animations last :id)
                                               :locked)))
        clip   (assoc-in (clip-of file [:rect] :whole-layer? true)
                         [:layers 0 :track :animations] [(style-use :name "Pasted")])
        track  (track-in (paste file clip [:rect-a] {:mode :replace}) :board-a :rect-a)]
    (t/is (= [[:style "Locked"] [:style "Pasted"]] (mapv (juxt :type :name) (:animations track)))
          "a use counts as a preset: the unlocked ones go, the locked one stays")))

;; TEXT SPLITS

(defn- text-component-file
  "An animated text component: its main, a board at 0,0 whose text fades
  in word by word in 1 s, playing as `playback` says. A copy of it in
  board B at 1000,100."
  [playback]
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame-with-text :main :main-text "Hi you there"
                                          :frame-params {:x 0 :y 0 :width 200 :height 200})
                 (thc/make-component :anim :main)
                 (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                 (thc/instantiate-component :anim :copy :parent-label :board-b))
        main (thi/id :main)
        tl   (-> (cta/make-timeline {:board-id main :duration 1000 :playback playback})
                 (cta/add-animation (thi/id :main-text) {:type :fade :split :word :start 0 :duration 1000}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline main tl)))))

(defn- unit-ranges
  "The ranges of the units of `shape-id` off their rest at `time`."
  [timeline objects shape-id time]
  (mapv (juxt :paragraph :start :end) (get (cta/text-unit-entries timeline objects time) shape-id)))

(t/deftest copies-keep-the-split-animations-of-their-texts
  (let [{:keys [objects board copy main-page] text :copy-rect} (component-scene (text-component-file :once))
        timeline (get (cltl/instance-timelines objects board nil main-page) [:copy copy (thi/id :main)])]
    (t/is (= [[:word 0.5 :forward]]
             (mapv (juxt :split :spread :order) (get-in timeline [:tracks text :animations]))))
    (t/is (= [[0 0 2] [0 3 6] [0 7 12]] (unit-ranges timeline objects text 0)))
    (t/testing "a copy with a text of its own plays its own units"
      (let [objects (update-in objects [text :content] txt/change-text "One two")]
        (t/is (= [[0 0 3] [0 4 7]] (unit-ranges timeline objects text 0)))))))

(t/deftest exports-play-the-units-of-a-copy-on-its-clock
  (doseq [playback [:loop :ping-pong]]
    (let [{:keys [objects board main-page] text :copy-rect} (component-scene (text-component-file playback))
          copies   (vals (cltl/instance-timelines objects board nil main-page))
          exported (cltl/with-instance-tracks (cta/make-timeline {:board-id board :duration 3000})
                     copies objects)
          track    (get-in exported [:tracks text])
          at       #(get (cta/text-unit-entries exported objects %) text)]
      (t/is (= [:word] (mapv :split (:animations track))) (name playback))
      (t/is (= {:duration 1000 :playback playback} (:split-clock track)))
      (doseq [time [100 400 700]]
        (t/is (seq (at time)))
        (if (= :loop playback)
          (t/is (= (at time) (at (+ 1000 time))) (str "loop, t=" time))
          (t/is (= (at time) (at (- 2000 time))) (str "ping-pong, t=" time)))))))

(t/deftest markup-exports-play-the-units-of-a-copy-whole
  (let [{:keys [objects board main-page] text :copy-rect} (component-scene (text-component-file :loop))
        copies   (vals (cltl/instance-timelines objects board nil main-page))
        exported (cltl/with-instance-tracks (cta/make-timeline {:board-id board :duration 3000})
                   copies objects :text-split :whole)
        track    (get-in exported [:tracks text])
        opacity  #(get-in (cta/values-at (cta/expand-loops exported) %) [text :opacity])]
    (t/is (nil? (:animations track)))
    (t/is (= {} (cta/text-unit-entries exported objects 400)))
    (t/is (contains? (:loops track) :opacity) "the fade of the whole text loops as keyframes")
    (t/is (< (opacity 0) (opacity 400) 1))
    (t/is (mth/close? (opacity 400) (opacity 1400) 0.02))))

(defn- split-paste-file
  "`paste-file` with a text in board A, and the rect of board A fading in
  word by word from 500 ms too."
  []
  (-> (paste-file)
      (ths/add-sample-shape :text-a :type :text :parent-label :board-a
                            :x 300 :y 300 :width 100 :height 20)
      (with-timeline :board-a
        #(cta/add-animation % (thi/id :rect) {:type :fade :split :word :start 500 :duration 400}))))

(t/deftest paste-leaves-the-split-presets-out-of-a-layer-that-is-no-text
  (let [file   (split-paste-file)
        result (paste file (clip-of file [:rect] :whole-layer? true) [:text-a :rect-a] {:mode :replace})]
    (t/is (= 1 (:skipped result)))
    (t/is (= [[:fade nil] [:fade :word]]
             (mapv (juxt :type :split) (:animations (track-in result :board-a :text-a)))))
    (t/is (= [[:fade nil]]
             (mapv (juxt :type :split) (:animations (track-in result :board-a :rect-a)))))
    (t/is (close-all? [200 300] (mapv second (row (track-in result :board-a :rect-a) :x)))
          "the rest goes on")))

(t/deftest a-style-use-with-a-split-entry-goes-whole-onto-any-layer
  (let [use    (style-use :entries [{:type :fade :split :word :start 0 :duration 300}
                                    {:type :scale :start 100 :duration 300}])
        file   (with-timeline (split-paste-file) :board-a #(cta/add-animation % (thi/id :rect-a) use))
        clip   (clip-of file [:rect-a] :whole-layer? true)
        result (paste file clip [:rect-b] {:mode :replace})
        pasted (first (:animations (track-in result :board-b :rect-b)))]
    (t/is (= 0 (:skipped result)))
    (t/is (= (:entries use) (:entries pasted)))))

(t/deftest units-at-rest-unposes-texts
  (let [id   (uuid/next)
        tree {id {:modifiers (-> (ctm/empty)
                                 (ctm/change-property :opacity 0.5)
                                 (ctm/change-property :text-units [{:kind :word :opacity 0}]))}}
        rest (cltl/units-at-rest tree [id])]
    (t/is (cltl/poses-units? tree id))
    (t/is (not (cltl/poses-units? tree (uuid/next))))
    (t/is (= {:opacity 0.5 :text-units []} (cta/appearance-changes (get-in rest [id :modifiers]))))
    (t/is (= 2 (count (get-in rest [id :modifiers :structure-parent]))) "one units change, not two")
    (t/is (= {:text-units []} (cta/appearance-changes (get-in (cltl/units-at-rest {} [id]) [id :modifiers])))
          "also for a text the tree does not pose")))

(t/deftest a-copy-of-a-library-component-plays-the-units-of-its-text
  (let [library   (let [file (-> (thf/sample-file :library)
                                 (tho/add-frame-with-text :lib-main :lib-text "Hi you there"
                                                          :frame-params {:x 0 :y 0 :width 200 :height 200})
                                 (thc/make-component :lib-anim :lib-main))
                        tl   (-> (cta/make-timeline {:board-id (thi/id :lib-main) :duration 1000})
                                 (cta/add-animation (thi/id :lib-text)
                                                    {:type :fade :split :word :start 0 :duration 1000}))]
                    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                                (pcb/with-page (thf/current-page file))
                                                (pcb/set-timeline (thi/id :lib-main) tl))))
        file      (-> (thf/sample-file :consumer)
                      (tho/add-frame :host :x 1000 :y 100 :width 400 :height 300)
                      (thc/instantiate-component :lib-anim :lib-copy :parent-label :host :library library))
        objects   (:objects (thf/current-page file))
        copy      (thi/id :lib-copy)
        text      (first (get-in objects [copy :shapes]))
        lib-page  (thf/current-page library)
        timelines (cltl/instance-timelines objects (thi/id :host) nil
                                           (constantly {:objects (:objects lib-page)
                                                        :timelines (:timelines lib-page)}))
        timeline  (get timelines [:copy copy (thi/id :lib-main)])]
    (t/is (= :text (get-in objects [text :type])))
    (t/is (= [[0 0 2] [0 3 6] [0 7 12]] (unit-ranges timeline objects text 0)))))

;; AUDIO CLIPS

(defn- audio-clip
  "The params of a clip, on the first free lane unless given."
  [& {:as params}]
  (merge {:id (uuid/next) :audio-id (uuid/next) :name "beat.mp3" :source-duration 4000
          :start 0 :duration 1000}
         params))

(defn- with-clips
  "`file` with the audio `clips` in the timeline of the board `label`, a
  new one of 2 s when it has none."
  [file label & clips]
  (with-timeline file label
    (fn [timeline]
      (reduce ctau/add-audio-clip
              (or timeline (cta/make-timeline {:board-id (thi/id label) :duration 2000}))
              clips))))

(defn- delete
  [file & labels]
  (let [page        (thf/current-page file)
        [_ changes] (cls/generate-delete-shapes (pcb/empty-changes) (:data file) page (:objects page)
                                                (into #{} (map thi/id) labels) {:components-v2 true})]
    (thf/apply-changes file changes)))

(defn- clips-of
  [file label]
  (get-in (timelines file) [(thi/id label) :audio-clips]))

(t/deftest the-clips-keep-the-timeline-of-their-board
  (let [file (-> (animated-file)
                 (with-timeline :board-a #(cta/remove-track % (thi/id :board-a)))
                 (with-clips :board-a (audio-clip :start 200)))
        tl   (get (timelines (delete file :rect)) (thi/id :board-a))]
    (t/is (empty? (:tracks tl)) "its last animated layer deleted")
    (t/is (= 1 (count (:audio-clips tl))))
    (t/is (not (contains? (timelines (delete file :board-a)) (thi/id :board-a)))
          "the board deleted, they go with it")))

(t/deftest copies-of-a-component-play-the-sound-of-their-main
  (let [file   (with-clips (component-file) :main (audio-clip :start 200))
        {:keys [objects board copy main-page timelines]} (component-scene file)
        main   (thi/id :main)
        copies (cltl/instance-timelines objects board nil main-page)]
    (t/is (= [[:copy copy main]] (keys copies)) "one entry, with the tracks")
    (t/is (= (clips-of file :main) (get-in copies [[:copy copy main] :audio-clips])))
    (t/testing "only the clips of the board that is the main"
      (let [host      (uuid/next)
            main-page (constantly {:objects objects
                                   :timelines (-> timelines
                                                  (assoc host (assoc (get timelines main) :board-id host))
                                                  (update main dissoc :audio-clips))})
            copies    (cltl/instance-timelines objects board nil main-page)]
        (t/is (not-any? #(contains? % :audio-clips) (vals copies)))))))

(t/deftest a-main-that-only-sounds-gives-its-copies-its-clock
  (let [file   (-> (component-file)
                   (with-timeline :main #(cta/remove-track % (thi/id :main-rect)))
                   (with-clips :main (audio-clip :start 200)))
        {:keys [objects board copy main-page]} (component-scene file)
        copies (cltl/instance-timelines objects board nil main-page)
        entry  (get copies [:copy copy (thi/id :main)])]
    (t/is (= 1 (count copies)))
    (t/is (= [board {} 1000 :loop] [(:board-id entry) (:tracks entry) (:duration entry) (:playback entry)]))
    (t/is (= 1 (count (:audio-clips entry))))))

(defn- copies-with-sound
  "Three copies, in board B, of a main of 1 s playing as `playback` says
  a clip from 200 ms to 1.2 s, and the timeline of B, 2.5 s long, with a
  clip of its own."
  [playback]
  (let [file   (-> (component-file)
                   (with-timeline :main #(assoc % :playback playback))
                   (with-clips :main (audio-clip :start 200 :fade-in 50 :fade-out 100))
                   (thc/instantiate-component :anim :copy2 :parent-label :board-b)
                   (thc/instantiate-component :anim :copy3 :parent-label :board-b)
                   (with-clips :board-b (audio-clip :start 0 :duration 500)))
        {:keys [objects board main-page]} (component-scene file)
        tl     (-> (get (timelines file) board) (assoc :duration 2500))]
    {:timeline tl
     :copies (cltl/instance-timelines objects board tl main-page)}))

(t/deftest the-copies-of-a-main-sound-once-on-their-clock
  (let [{:keys [timeline copies]} (copies-with-sound :loop)
        played (cltl/with-instance-clips timeline copies)
        theirs (->> (vals (:audio-clips played))
                    (remove #(contains? (:audio-clips timeline) (:id %)))
                    (sort-by :start))]
    (t/is (= 3 (count copies)))
    (t/is (= [[200 800 1] [1200 800 1] [2200 300 1]] (mapv (juxt :start :duration :lane) theirs))
          "once for the three copies, at each round, cut at its end and at the board's")
    (t/is (= [50 nil] ((juxt :fade-in :fade-out) (first theirs))) "a fade out cut off goes")
    (t/is (= played (cltl/with-instance-clips timeline copies)) "the same ids each time")
    (t/is (= 4 (count (cta/audio-plan played 0 2500))))
    (t/testing "ping-pong: on the way there only"
      (let [{:keys [timeline copies]} (copies-with-sound :ping-pong)
            played (cltl/with-instance-clips timeline copies)]
        (t/is (= [0 200 2200] (sort (map :start (vals (:audio-clips played))))))))
    (t/testing "once: the first round only"
      (let [{:keys [timeline copies]} (copies-with-sound :once)
            played (cltl/with-instance-clips timeline copies)]
        (t/is (= [0 200] (sort (map :start (vals (:audio-clips played))))))))
    (t/testing "no copies with sound: the timeline as it is"
      (t/is (= timeline (cltl/with-instance-clips timeline {}))))))

(t/deftest a-moved-layer-leaves-the-clips-of-its-board
  (let [file (-> (animated-file)
                 (with-clips :board-a (audio-clip :start 200))
                 (relocate :rect :board-b))]
    (t/is (= 1 (count (clips-of file :board-a))))
    (t/is (some? (get (timelines file) (thi/id :board-b))))
    (t/is (nil? (clips-of file :board-b)))))

(t/deftest a-board-put-into-another-gives-it-its-clips
  (let [file    (-> (animated-file)
                    (with-clips :board-a (audio-clip :start 0))
                    ;; board B plays only audio, on two lanes
                    (with-clips :board-b (audio-clip :start 300) (audio-clip :start 300)))
        changes (relocate-changes file :board-b :board-a)
        moved   (thf/apply-changes file changes)
        clips   (vals (clips-of moved :board-a))
        set-ups (filter #(= :set-timeline-audio-clip (:type %)) (:redo-changes changes))]
    (t/is (not (contains? (timelines moved) (thi/id :board-b))))
    (t/is (= [[0 0] [300 1] [300 2]] (sort (map (juxt :start :lane) clips)))
          "at their starts, on lanes after the one of the host")
    (t/is (= (set (keys (clips-of file :board-b)))
             (set (keep #(when (= 300 (:start %)) (:id %)) clips))))
    (t/is (= (count set-ups) (count (set (map (juxt :id :clip-id) set-ups))))
          "one change for each clip")
    (t/is (= (timelines file)
             (timelines (thf/apply-changes moved {:redo-changes (vec (:undo-changes changes))})))
          "one undo")))

(t/deftest a-duplicated-board-gets-copies-of-its-clips
  (let [file        (with-clips (animated-file) :board-b (audio-clip :start 300 :lane 1))
        [file copy] (duplicate file :board-b (gpt/point 0 500))
        tl          (get (timelines file) copy)
        [original]  (vals (clips-of file :board-b))
        [clip]      (vals (:audio-clips tl))]
    (t/is (= 2000 (:duration tl)) "its timeline, though it has no track")
    (t/is (not= (:id original) (:id clip)) "with new ids")
    (t/is (= (dissoc original :id) (dissoc clip :id)) "of the same file, start and lane")
    (t/is (cta/valid-timeline? tl)))
  (t/testing "with its tracks"
    (let [[file copy] (duplicate (with-clips (animated-file) :board-a (audio-clip :start 300))
                                 :board-a (gpt/point 500 0))
          tl          (get (timelines file) copy)]
      (t/is (= 2 (count (:tracks tl))))
      (t/is (= [300] (map :start (vals (:audio-clips tl)))))))
  (t/testing "by alt-drag"
    (let [file    (with-clips (animated-file) :board-b (audio-clip :start 300))
          page    (thf/current-page file)
          b       (thi/id :board-b)
          changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                  (:objects page) page #{b} (gpt/point 0 500)
                                                  {(:id file) file} (:data file) (:id file)
                                                  :alt-duplication? true)
          copy    (some #(when (and (= :add-obj (:type %)) (= b (:old-id %))) (-> % :obj :id))
                        (:redo-changes changes))
          file    (thf/apply-changes file changes)]
      (t/is (= [300] (map :start (vals (get-in (timelines file) [copy :audio-clips]))))))))

(t/deftest a-pasted-board-brings-its-clips
  (let [file    (with-clips (animated-file) :board-b (audio-clip :start 300))
        page    (thf/current-page file)
        b       (thi/id :board-b)
        ;; what the clipboard takes of a board that plays only audio
        motion  (cltl/animation-sources page (:objects page) [b])
        changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                (:objects page) page #{b} (gpt/point 0 600)
                                                {(:id file) file} (:data file) (:id file)
                                                {:motion motion})
        copy    (some #(when (and (= :add-obj (:type %)) (= b (:old-id %))) (-> % :obj :id))
                      (:redo-changes changes))
        file    (thf/apply-changes file changes)]
    (t/is (empty? (:tracks motion)))
    (t/is (= #{b} (set (keys (:audio motion)))))
    (t/is (= [300] (map :start (vals (get-in (timelines file) [copy :audio-clips])))))))

(t/deftest pasted-animation-leaves-the-clips-and-their-length
  ;; board B plays 500 ms and a clip that goes on to 3500 ms
  (let [file   (-> (paste-file)
                   (with-clips :board-b (audio-clip :start 500 :duration 3000))
                   (with-timeline :board-b #(assoc % :duration 500)))
        clips  (clips-of file :board-b)
        page   (thf/current-page file)
        rect   (thi/id :rect)
        b      (thi/id :board-b)]
    (t/testing "a pasted layer"
      (let [motion  (cltl/animation-sources page (:objects page) [rect])
            pasted  (-> (ths/get-shape file :rect)
                        (assoc :parent-id b :frame-id b)
                        (gsh/move (gpt/point 870 -50)))
            changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                    (assoc (:objects page) rect pasted) page #{rect}
                                                    (gpt/point 0 0)
                                                    {(:id file) file} (:data file) (:id file)
                                                    {:motion motion})
            tl      (get (timelines (thf/apply-changes file changes)) b)]
        (t/is (= clips (:audio-clips tl)))
        (t/is (= 1000 (:duration tl)) "as long as the pasted track, not the clip")
        (t/is (not-any? #(= :set-timeline-audio-clip (:type %)) (:redo-changes changes)))))
    (t/testing "a pasted animation"
      (let [result (paste file (clip-of file [:rect] :whole-layer? true) [:rect-b] {:mode :replace})
            tl     (get-in result [:timelines b])]
        (t/is (= clips (:audio-clips tl)))
        (t/is (= 1000 (:duration tl)))))))

(t/deftest a-board-with-only-clips-keeps-its-own-clock
  (let [{:keys [objects board main-page]} (component-scene)
        copies (vals (cltl/instance-timelines objects board nil main-page))
        own    (-> (cta/make-timeline {:board-id board :duration 2500})
                   (ctau/add-audio-clip (audio-clip)))]
    (t/is (= [2500 :once] ((juxt :duration cta/playback-mode) (cltl/with-copies-clock own copies true))))
    (t/is (= [1000 :loop] ((juxt :duration cta/playback-mode)
                           (cltl/with-copies-clock (dissoc own :audio-clips) copies false)))
          "without them, the clock of its copies")))

;; CODE

(t/deftest the-view-mode-plays-copies-on-their-own-clock
  (let [file      (component-file)
        board     (thi/id :board-b)
        own       (-> (cta/make-timeline {:board-id board :duration 400})
                      (cta/add-keyframe board {:time 0 :property :opacity :value 0})
                      (cta/add-keyframe board {:time 400 :property :opacity :value 1}))
        file      (thf/apply-changes file (-> (pcb/empty-changes nil)
                                              (pcb/with-page (thf/current-page file))
                                              (pcb/set-timeline board own)))
        page      (thf/current-page file)
        objects   (:objects page)
        files     {(:id file) file}
        copy-rect (first (get-in objects [(thi/id :copy) :shapes]))
        copies    (cltl/copy-timelines objects (:timelines page) files board)
        tree      (cltl/view-animation-tree page files board 400 1250)]
    (t/is (= (get (cltl/animation-tree copies objects 250) copy-rect) (get tree copy-rect))
          "the copy 1250 ms on, 250 ms into its loop")
    (t/is (not= (get (cltl/animation-tree copies objects 400) copy-rect) (get tree copy-rect))
          "not at the time of the board")
    (t/is (= (get (cltl/animation-tree {board own} objects 400) board) (get tree board))
          "the board at its own time, its end")))

(t/deftest the-view-mode-plays-on-while-a-copy-loops
  (let [once (fn [duration] (cta/make-timeline {:board-id (uuid/next) :duration duration}))
        loop (assoc (once 1600) :playback :loop)]
    (t/is (cltl/view-playback-ended? (once 500) [] 600))
    (t/is (not (cltl/view-playback-ended? (once 500) [] 400)))
    (t/is (not (cltl/view-playback-ended? (once 500) [loop] 60000)) "a looping copy goes on")
    (t/is (not (cltl/view-playback-ended? (once 500) [(once 1000)] 800)) "a longer copy plays to its end")
    (t/is (cltl/view-playback-ended? (once 500) [(once 1000)] 1100))))

(t/deftest code-timelines-of-a-board-with-its-own-tracks
  (let [file (component-file)
        page (thf/current-page file)
        main (thi/id :main)
        code (cltl/code-timelines page {(:id file) file} main)]
    (t/is (= main (:board-id code)))
    (t/is (= (get-in page [:timelines main]) (:timeline code)) "its stored timeline")
    (t/is (= [] (:copies code)))
    (t/is (= {} (:splits code)))))

(t/deftest code-timelines-play-the-copies-on-their-own-clock
  (let [file  (component-file)
        page  (thf/current-page file)
        board (thi/id :board-b)
        code  (cltl/code-timelines page {(:id file) file} board)
        [entry :as copies] (:copies code)]
    (t/is (= [1000 :loop] ((juxt :duration :playback) (:timeline code)))
          "a board without tracks keeps time with its copy")
    (t/is (= (get-in page [:objects board :name]) (:name (:timeline code))))
    (t/is (= 1 (count copies)))
    (t/is (= (thi/id :copy) (:head-id entry)))
    (t/is (= [board 1000 :loop] ((juxt :board-id :duration :playback) (:timeline entry)))
          "the clock of its main, in the board")))

(t/deftest code-timelines-of-a-board-that-plays-nothing
  (let [file (-> (component-file)
                 (tho/add-frame :board-c :x 2000 :y 0 :width 100 :height 100))]
    (t/is (nil? (cltl/code-timelines (thf/current-page file) {(:id file) file} (thi/id :board-c))))))

(t/deftest code-timelines-play-split-text-whole
  (let [file  (-> (thf/sample-file :file1)
                  (tho/add-frame :board :x 0 :y 0 :width 400 :height 300)
                  (tho/add-text :title "Hi you there" :text-params {:parent-label :board}))
        board (thi/id :board)
        title (thi/id :title)
        tl    (cta/add-animation (cta/make-timeline {:board-id board :duration 1000})
                                 title {:type :fade :split :word})
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page (thf/current-page file))
                                          (pcb/set-timeline board tl)))
        code  (cltl/code-timelines (thf/current-page file) {} board)]
    (t/is (= {title :word} (:splits code)) "the design plays it by words")
    (t/is (not-any? :split (get-in code [:timeline :tracks title :animations]))
          "the code plays it whole")))

;; FOLLOW

(defn- following-file
  "`animated-file` with a dot in board A that follows the rect."
  []
  (let [file (-> (animated-file)
                 (ths/add-sample-shape :dot :type :rect :parent-label :board-a
                                       :x 300 :y 200 :width 10 :height 10))
        a    (thi/id :board-a)
        tl   (cta/set-follow (get (timelines file) a) (thi/id :dot) {:leader (thi/id :rect) :delay 100})]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline a tl)))))

(t/deftest a-copied-board-follows-in-its-copy
  (let [file     (following-file)
        page     (thf/current-page file)
        a        (thi/id :board-a)
        changes  (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                 (:objects page) page #{a} (gpt/point 500 0)
                                                 {(:id file) file} (:data file) (:id file))
        copy-of  (into {} (keep #(when (= :add-obj (:type %)) [(:old-id %) (-> % :obj :id)])) (:redo-changes changes))
        file     (thf/apply-changes file changes)
        tl       (get (timelines file) (copy-of a))
        dot      (copy-of (thi/id :dot))
        rect     (copy-of (thi/id :rect))]
    (t/is (= {:leader rect :delay 100} (get-in tl [:tracks dot :follow])) "the copy of its leader")
    (t/is (cta/valid-timeline? tl))))

(t/deftest a-copied-follower-follows-the-same-leader-in-its-board
  (let [[file dot] (duplicate (following-file) :dot (gpt/point 10 0))
        tl         (get (timelines file) (thi/id :board-a))]
    (t/is (= (thi/id :rect) (get-in tl [:tracks dot :follow :leader])))))

(t/deftest a-follower-moved-out-of-the-board-follows-no-more
  (let [file (relocate (following-file) :dot :board-b)
        tls  (timelines file)]
    (t/is (nil? (cta/follow-of (get tls (thi/id :board-b)) (thi/id :dot))))
    (t/is (nil? (cta/follow-of (get tls (thi/id :board-a)) (thi/id :dot))))))

(t/deftest a-follower-of-a-deleted-leader-follows-no-more
  (let [file (delete (following-file) :rect)]
    (t/is (nil? (cta/follow-of (get (timelines file) (thi/id :board-a)) (thi/id :dot))))))

;; PROPERTY LINKS

(defn- linking-file
  "`animated-file` with a dot in board A whose rotation plays the X of
  the rect."
  []
  (let [file (-> (animated-file)
                 (ths/add-sample-shape :dot :type :rect :parent-label :board-a
                                       :x 300 :y 200 :width 10 :height 10))
        a    (thi/id :board-a)
        tl   (cta/set-link (get (timelines file) a) (thi/id :dot)
                           {:property :rotation :source (thi/id :rect) :source-property :x :factor 2})]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline a tl)))))

(t/deftest a-copied-board-links-in-its-copy
  (let [file     (linking-file)
        page     (thf/current-page file)
        a        (thi/id :board-a)
        changes  (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                 (:objects page) page #{a} (gpt/point 500 0)
                                                 {(:id file) file} (:data file) (:id file))
        copy-of  (into {} (keep #(when (= :add-obj (:type %)) [(:old-id %) (-> % :obj :id)])) (:redo-changes changes))
        file     (thf/apply-changes file changes)
        tl       (get (timelines file) (copy-of a))]
    (t/is (= (copy-of (thi/id :rect)) (:source (cta/link-of tl (copy-of (thi/id :dot)) :rotation)))
          "the copy of its source")
    (t/is (cta/valid-timeline? tl))))

(t/deftest a-link-whose-source-leaves-the-board-or-goes-links-no-more
  (t/is (empty? (cta/links-of (get (timelines (relocate (linking-file) :rect :board-b)) (thi/id :board-a))
                              (thi/id :dot))))
  (t/is (empty? (cta/links-of (get (timelines (delete (linking-file) :rect)) (thi/id :board-a))
                              (thi/id :dot)))))

(defn- riding-file
  "`animated-file` with a road in board A (a path) that a dot rides."
  []
  (let [file (-> (animated-file)
                 (ths/add-sample-shape :road :type :path :parent-label :board-a
                                       :content (path/from-string "M 100 300 L 500 300"))
                 (ths/add-sample-shape :dot :type :rect :parent-label :board-a
                                       :x 300 :y 200 :width 10 :height 10))
        a    (thi/id :board-a)
        tl   (cta/set-path-follow (get (timelines file) a) (thi/id :dot) {:path (thi/id :road) :orient true})]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline a tl)))))

(t/deftest a-copied-board-rides-in-its-copy
  (let [file     (riding-file)
        page     (thf/current-page file)
        a        (thi/id :board-a)
        changes  (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                 (:objects page) page #{a} (gpt/point 500 0)
                                                 {(:id file) file} (:data file) (:id file))
        copy-of  (into {} (keep #(when (= :add-obj (:type %)) [(:old-id %) (-> % :obj :id)])) (:redo-changes changes))
        file     (thf/apply-changes file changes)
        tl       (get (timelines file) (copy-of a))]
    (t/is (= {:path (copy-of (thi/id :road)) :orient true}
             (get-in tl [:tracks (copy-of (thi/id :dot)) :path-follow]))
          "the copy of its path")
    (t/is (cta/valid-timeline? tl))))

(t/deftest a-path-that-leaves-the-board-or-goes-is-ridden-no-more
  (t/is (nil? (cta/path-follow-of (get (timelines (relocate (riding-file) :road :board-b)) (thi/id :board-a))
                                  (thi/id :dot))))
  (t/is (nil? (cta/path-follow-of (get (timelines (delete (riding-file) :road)) (thi/id :board-a))
                                  (thi/id :dot)))))

(t/deftest a-path-and-what-moves-with-it-lead-none-of-its-riders
  (let [file    (riding-file)
        objects (:objects (thf/current-page file))
        tl      (get (timelines file) (thi/id :board-a))]
    (t/is (not (cltl/can-follow? objects tl (thi/id :road) (thi/id :dot)))
          "the road follows not the dot that rides it, through others neither")
    (t/is (cltl/can-follow? objects tl (thi/id :road) (thi/id :rect)))
    (t/is (not (cltl/can-ride-path? objects (cta/set-follow tl (thi/id :rect) {:leader (thi/id :dot)})
                                    (thi/id :road) (thi/id :rect)))
          "nor rides a layer that follows one of them")))

(t/deftest a-rider-pasted-where-it-cannot-ride-keeps-no-progress
  (let [file    (riding-file)
        a       (thi/id :board-a)
        dot     (thi/id :dot)
        tl      (-> (get (timelines file) a)
                    (cta/add-keyframe dot {:time 0 :property :path-progress :value 0})
                    (cta/add-keyframe dot {:time 1000 :property :path-progress :value 1})
                    (cta/add-keyframe dot {:time 0 :property :opacity :value 1}))
        file    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                            (pcb/with-page (thf/current-page file))
                                            (pcb/set-timeline a tl)))
        page    (thf/current-page file)
        objects (:objects page)
        copy-id (uuid/next)
        copy    (assoc (get objects dot) :id copy-id :parent-id (thi/id :board-b) :frame-id (thi/id :board-b))
        changes (-> (pcb/empty-changes nil)
                    (pcb/with-page page)
                    (pcb/with-objects objects)
                    (pcb/add-object copy))
        changes (cltl/generate-copy-timelines changes page {dot copy-id}
                                              (cltl/animation-sources page objects #{dot}))
        pasted  (get (timelines (thf/apply-changes file changes)) (thi/id :board-b))
        track   (get-in pasted [:tracks copy-id])]
    (t/is (some? track) "its opacity goes with it")
    (t/is (nil? (:path-follow track)) "the road is in another board")
    (t/is (not-any? #(= :path-progress (:property %)) (:keyframes track)) "nor its progress along it")
    (t/is (cta/valid-timeline? pasted))))

(t/deftest a-copy-shakes-its-own-way
  (let [file    (animated-file)
        page    (thf/current-page file)
        a       (thi/id :board-a)
        rect    (thi/id :rect)
        wiggle  {:property :rotation :frequency 2 :amplitude 10 :seed 42}
        file    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                            (pcb/with-page page)
                                            (pcb/set-timeline a (cta/set-wiggle (get (timelines file) a) rect wiggle))))
        page    (thf/current-page file)
        copy-of #(into {} (keep (fn [c] (when (= :add-obj (:type c)) [(:old-id c) (-> c :obj :id)])))
                       (:redo-changes %))]
    (t/testing "a duplicated layer"
      (let [changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                    (:objects page) page #{rect} (gpt/point 10 0)
                                                    {(:id file) file} (:data file) (:id file))
            copied  (cta/wiggle-of (get (timelines (thf/apply-changes file changes)) a)
                                   (get (copy-of changes) rect) :rotation)]
        (t/is (not= 42 (:seed copied)))
        (t/is (= (dissoc (cta/wiggle-of (get (timelines file) a) rect :rotation) :seed) (dissoc copied :seed)))))
    (t/testing "a duplicated board, its layers"
      (let [changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                    (:objects page) page #{a} (gpt/point 500 0)
                                                    {(:id file) file} (:data file) (:id file))
            ids     (copy-of changes)
            tl      (get (timelines (thf/apply-changes file changes)) (get ids a))]
        (t/is (not= 42 (:seed (cta/wiggle-of tl (get ids rect) :rotation))))
        (t/is (cta/valid-timeline? tl))))
    (t/testing "pasted after a cut, its original gone: as it did"
      ;; a cut copies and deletes: its animation is read before
      (let [sources (cltl/animation-sources page (:objects page) #{rect})
            cut     (delete file :rect)
            after   (thf/current-page cut)
            copy-id (uuid/next)
            ;; a paste works on the page with the shapes of the
            ;; clipboard, under their old ids
            changes (-> (pcb/empty-changes nil)
                        (pcb/with-page after)
                        (pcb/with-objects (assoc (:objects after) rect (get (:objects page) rect)))
                        (pcb/add-object (assoc (get (:objects page) rect) :id copy-id)))
            changes (cltl/generate-copy-timelines changes after {rect copy-id} sources)
            pasted  (cta/wiggle-of (get (timelines (thf/apply-changes cut changes)) a) copy-id :rotation)]
        (t/is (= 42 (:seed pasted)))))))

(defn- reaching-board-file
  "`animated-file` with an arm in board A (an upper arm, a forearm
  following it and a hand following that) whose hand reaches a ball."
  []
  (let [file (-> (animated-file)
                 (ths/add-sample-shape :upper :type :rect :parent-label :board-a
                                       :x 120 :y 300 :width 100 :height 10)
                 (ths/add-sample-shape :fore :type :rect :parent-label :board-a
                                       :x 220 :y 300 :width 100 :height 10)
                 (ths/add-sample-shape :hand :type :rect :parent-label :board-a
                                       :x 320 :y 295 :width 20 :height 20)
                 (ths/add-sample-shape :ball :type :rect :parent-label :board-a
                                       :x 300 :y 350 :width 10 :height 10))
        a    (thi/id :board-a)
        tl   (-> (get (timelines file) a)
                 (cta/set-follow (thi/id :fore) {:leader (thi/id :upper)})
                 (cta/set-follow (thi/id :hand) {:leader (thi/id :fore)})
                 (cta/set-ik (thi/id :hand) {:goal (thi/id :ball) :bend :cw}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline a tl)))))

(t/deftest a-copied-board-reaches-in-its-copy
  (let [file    (reaching-board-file)
        page    (thf/current-page file)
        a       (thi/id :board-a)
        changes (cll/generate-duplicate-changes (pcb/empty-changes nil)
                                                (:objects page) page #{a} (gpt/point 500 0)
                                                {(:id file) file} (:data file) (:id file))
        copy-of (into {} (keep #(when (= :add-obj (:type %)) [(:old-id %) (-> % :obj :id)])) (:redo-changes changes))
        file    (thf/apply-changes file changes)
        tl      (get (timelines file) (copy-of a))]
    (t/is (= {:goal (copy-of (thi/id :ball)) :bend :cw} (get-in tl [:tracks (copy-of (thi/id :hand)) :ik]))
          "the copy of its goal")
    (t/is (cta/valid-timeline? tl))))

(t/deftest a-goal-moved-out-of-the-board-or-deleted-is-reached-no-more
  (let [moved   (relocate (reaching-board-file) :ball :board-b)
        deleted (delete (reaching-board-file) :ball)]
    (t/is (nil? (cta/ik-of (get (timelines moved) (thi/id :board-a)) (thi/id :hand))))
    (t/is (nil? (cta/ik-of (get (timelines deleted) (thi/id :board-a)) (thi/id :hand))))
    (t/is (some? (cta/follow-of (get (timelines deleted) (thi/id :board-a)) (thi/id :hand)))
          "it follows still")))

(defn- following-component-file
  "A component whose main holds a rect moving from 10 to 60 in 1 s, a dot
  that follows the layer `leader` and a still square; a copy of it in
  board B at 1000,100."
  [leader]
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-simple-component :anim :main :main-rect
                                           :root-params {:x 0 :y 0 :width 200 :height 200}
                                           :child-params {:x 10 :y 20 :width 50 :height 50})
                 (ths/add-sample-shape :main-dot :type :rect :parent-label :main
                                       :x 120 :y 120 :width 10 :height 10)
                 (ths/add-sample-shape :main-still :type :rect :parent-label :main
                                       :x 150 :y 20 :width 10 :height 10)
                 (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                 (thc/instantiate-component :anim :copy :parent-label :board-b))
        main (thi/id :main)
        tl   (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                 (cta/add-keyframe (thi/id :main-rect) {:time 0 :property :x :value 10 :easing :linear})
                 (cta/add-keyframe (thi/id :main-rect) {:time 1000 :property :x :value 60})
                 (cta/set-follow (thi/id :main-dot) {:leader (thi/id leader)}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline main tl)))))

(defn- copy-of
  "The layer of the copy `copy` of `objects` that plays `main-label`."
  [objects copy main-label]
  (d/seek #(= (thi/id main-label) (get-in objects [% :shape-ref]))
          (cfh/get-children-ids objects copy)))

(t/deftest a-copy-of-a-component-follows-in-the-copy
  (let [{:keys [objects board copy main-page]} (component-scene (following-component-file :main-rect))
        tl (get (cltl/instance-timelines objects board nil main-page) [:copy copy (thi/id :main)])]
    (t/is (= (copy-of objects copy :main-rect)
             (get-in tl [:tracks (copy-of objects copy :main-dot) :follow :leader]))
          "the rect of the copy, not of the main")))

(defn- riding-component-file
  "`following-component-file` whose dot rides the rect of the main in
  place of following it, its progress keyframed and looping."
  []
  (let [file (following-component-file :main-still)
        main (thi/id :main)
        dot  (thi/id :main-dot)
        tl   (-> (get (timelines file) main)
                 (cta/set-follow dot nil)
                 (cta/set-path-follow dot {:path (thi/id :main-rect) :orient true})
                 (cta/add-keyframe dot {:time 0 :property :path-progress :value 0})
                 (cta/add-keyframe dot {:time 1000 :property :path-progress :value 1})
                 (cta/toggle-slot-flag dot :loops :path-progress))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline main tl)))))

(t/deftest a-copy-of-a-component-rides-the-copy-of-its-path
  (let [{:keys [objects board copy main-page]} (component-scene (riding-component-file))
        tl (get (cltl/instance-timelines objects board nil main-page) [:copy copy (thi/id :main)])]
    (t/is (= {:path (copy-of objects copy :main-rect) :orient true}
             (get-in tl [:tracks (copy-of objects copy :main-dot) :path-follow]))
          "the rect of the copy, not of the main")))

(t/deftest a-copy-of-a-component-without-its-path-rides-none
  (let [{:keys [objects board copy main-page]} (component-scene (riding-component-file))
        rect    (copy-of objects copy :main-rect)
        dot     (copy-of objects copy :main-dot)
        ;; the rect of the copy gone, as when it is swapped
        objects (-> (dissoc objects rect)
                    (update-in [copy :shapes] (partial filterv #(not= rect %))))
        tl      (get (cltl/instance-timelines objects board nil main-page) [:copy copy (thi/id :main)])
        track   (get-in tl [:tracks dot])]
    (t/is (nil? (:path-follow track)))
    (t/is (not-any? #(= :path-progress (:property %)) (:keyframes track)) "nor the keyframes of its progress")
    (t/is (not (contains? (:loops track #{}) :path-progress)) "nor their flags")))

(t/deftest a-path-of-a-component-copy-is-ridden-when-the-board-animates-it
  ;; what the copy plays of its main carries no rider of the board
  (let [file    (-> (following-component-file :main-still)
                    (ths/add-sample-shape :dot-b :type :rect :parent-label :board-b
                                          :x 1300 :y 300 :width 10 :height 10))
        objects (:objects (thf/current-page file))
        board   (thi/id :board-b)
        rect    (copy-of objects (thi/id :copy) :main-rect)
        tl      (cta/make-timeline {:board-id board :duration 1000})]
    (t/is (not (cltl/can-ride-path? objects tl (thi/id :dot-b) rect)))
    (t/is (cltl/can-ride-path? objects (cta/add-keyframe tl rect {:time 0 :property :opacity :value 1})
                               (thi/id :dot-b) rect)
          "the board animates it itself")))

(t/deftest a-copy-keeps-a-follow-whose-leader-plays-nothing-there
  ;; a still leader moves nothing, and a still goal or upper arm drives a
  ;; chain (see `a-copy-of-a-rig-reaches-the-copy-of-its-still-goal`)
  (let [{:keys [objects board copy main-page]} (component-scene (following-component-file :main-still))
        copies (cltl/instance-timelines objects board nil main-page)
        tl     (get copies [:copy copy (thi/id :main)])
        dot    (copy-of objects copy :main-dot)]
    (t/is (= (copy-of objects copy :main-still) (get-in tl [:tracks dot :follow :leader])))
    (t/is (nil? (get-in (cltl/animation-tree copies objects 500) [dot :modifiers])) "it stays where it is")))

(t/deftest a-copy-of-a-component-links-in-the-copy
  (let [file  (following-component-file :main-rect)
        page  (thf/current-page file)
        main  (thi/id :main)
        tl    (-> (get (:timelines page) main)
                  (cta/set-link (thi/id :main-still) {:property :rotation :source (thi/id :main-rect)
                                                      :source-property :x})
                  (cta/set-link (thi/id :main-dot) {:property :opacity :source (thi/id :main-still)
                                                    :source-property :rotation :factor 0.01}))
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page page)
                                          (pcb/set-timeline main tl)))
        {:keys [objects board copy main-page]} (component-scene file)
        still (copy-of objects copy :main-still)
        tl    (get (cltl/instance-timelines objects board nil main-page) [:copy copy main])]
    (t/is (= (copy-of objects copy :main-rect) (:source (cta/link-of tl still :rotation)))
          "the rect of the copy, not of the main")
    (let [objects (-> objects
                      (dissoc still)
                      (update-in [copy :shapes] #(filterv (partial not= still) %)))
          tl      (get (cltl/instance-timelines objects board nil main-page) [:copy copy main])]
      (t/is (nil? (cta/link-of tl (copy-of objects copy :main-dot) :opacity))
            "a source the copy lacks links nothing"))))

(t/deftest the-code-plays-a-follow-by-keyframes
  (let [file (following-file)
        page (thf/current-page file)
        a    (thi/id :board-a)
        dot  (thi/id :dot)
        code (cltl/code-timelines page {(:id file) file} a)
        track (get-in code [:timeline :tracks dot])]
    (t/is (nil? (:follow track)))
    (t/is (= #{:x} (into #{} (map :property) (:keyframes track))) "it moves along x as the rect does")))

(t/deftest the-exports-play-the-follows-of-copies-by-keyframes
  (let [{:keys [objects board copy main-page]} (component-scene (following-component-file :main-rect))
        copies   (cltl/instance-timelines objects board nil main-page)
        exported (cltl/with-instance-tracks (cta/make-timeline {:board-id board :duration 1000}) (vals copies) objects)
        track    (get-in exported [:tracks (copy-of objects copy :main-dot)])
        xs       (->> (:keyframes track) (filter #(= :x (:property %))) (sort-by :time) (map :value))]
    (t/is (nil? (:follow track)))
    (t/is (mth/close? 50 (- (last xs) (first xs)) 0.5) "it moves 50 as the rect of its copy does")))

(defn- centre-in
  "Where the layer `id` of `objects` is, moved by the modif-tree `tree`."
  [tree objects id]
  (let [mods  (get-in tree [id :modifiers])
        shape (cond-> (get objects id) (some? mods) (gsh/transform-shape mods))
        c     (grc/rect->center (:selrect shape))]
    [(:x c) (:y c)]))

(t/deftest the-exports-play-the-follows-of-retimed-copies-as-the-canvas
  ;; a copy at twice the pace, and a copy playing there and back: its
  ;; dot follows the rect 200 ms later on the clock of the copy
  (doseq [[clock playback] [[{:start 0 :speed 2} :loop] [{:start 0 :speed 1} :ping-pong]]]
    (let [file   (following-component-file :main-rect)
          main   (thi/id :main)
          page   (thf/current-page file)
          tl     (-> (get-in page [:timelines main])
                     (assoc :playback playback)
                     (cta/set-follow (thi/id :main-dot) {:leader (thi/id :main-rect) :delay 200}))
          file   (thf/apply-changes file (-> (pcb/empty-changes nil)
                                             (pcb/with-page page)
                                             (pcb/set-timeline main tl)))
          {:keys [objects board copy main-page]} (component-scene file)
          host   (cta/set-copy-clock (cta/make-timeline {:board-id board :duration 2000}) copy clock)
          copies (cltl/instance-timelines objects board host main-page)
          export (cltl/with-instance-tracks host (vals copies) objects)
          dot    (copy-of objects copy :main-dot)]
      (doseq [t [300 450 1100 1200 1500 1900]]
        (let [[cx cy] (centre-in (cltl/animation-tree copies objects t) objects dot)
              [ex ey] (centre-in (ctaf/timeline->modif-tree export objects t) objects dot)]
          (t/is (and (mth/close? cx ex 1) (mth/close? cy ey 1))
                (str playback " at " t ": canvas " [cx cy] ", export " [ex ey])))))))

(t/deftest a-follower-in-a-ping-pong-copy-lags-on-the-way-back
  ;; the dot is where the rect of the copy was 200 ms before, on its way
  ;; back too
  (let [file   (following-component-file :main-rect)
        main   (thi/id :main)
        page   (thf/current-page file)
        tl     (-> (get-in page [:timelines main])
                   (assoc :playback :ping-pong)
                   (cta/set-follow (thi/id :main-dot) {:leader (thi/id :main-rect) :delay 200}))
        file   (thf/apply-changes file (-> (pcb/empty-changes nil)
                                           (pcb/with-page page)
                                           (pcb/set-timeline main tl)))
        {:keys [objects board copy main-page]} (component-scene file)
        copies (cltl/instance-timelines objects board (cta/make-timeline {:board-id board :duration 2000}) main-page)
        rect   (copy-of objects copy :main-rect)
        dot    (copy-of objects copy :main-dot)
        x      (fn [id elapsed] (first (centre-in (cltl/animation-tree copies objects elapsed) objects id)))]
    (doseq [elapsed [500 1100 1300 1500 1700]]
      (t/is (mth/close? (- (x dot elapsed) (x dot 200)) (- (x rect (- elapsed 200)) (x rect 0)) 0.5)
            (str "at " elapsed)))))

(t/deftest the-view-mode-shows-followers-behind-on-the-way-back
  ;; the board plays ping-pong: at 500 ms on its way back the dot is
  ;; where the rect was at 600, at 500 on its way there where it was at 400
  (let [file    (following-file)
        a       (thi/id :board-a)
        page    (update-in (thf/current-page file) [:timelines a] assoc :playback :ping-pong)
        objects (:objects page)
        dot-x   (fn [time elapsed]
                  (first (centre-in (cltl/view-animation-tree page {} a time elapsed) objects (thi/id :dot))))]
    (t/is (mth/close? 20 (- (dot-x 500 2500) (dot-x 500 500)) 0.5))))

(defn- reaching-file
  "`following-component-file` with an arm in board B (an upper arm, a
  forearm following it, a hand following that) and a ball."
  []
  (let [file (-> (following-component-file :main-rect)
                 (ths/add-sample-shape :upper :type :rect :parent-label :board-b
                                       :x 1100 :y 300 :width 100 :height 10)
                 (ths/add-sample-shape :fore :type :rect :parent-label :board-b
                                       :x 1200 :y 300 :width 100 :height 10)
                 (ths/add-sample-shape :hand :type :rect :parent-label :board-b
                                       :x 1300 :y 295 :width 20 :height 20)
                 (ths/add-sample-shape :ball :type :rect :parent-label :board-b
                                       :x 1250 :y 350 :width 10 :height 10))
        b    (thi/id :board-b)
        tl   (-> (cta/make-timeline {:board-id b :duration 1000})
                 (cta/set-follow (thi/id :fore) {:leader (thi/id :upper)})
                 (cta/set-follow (thi/id :hand) {:leader (thi/id :fore)}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline b tl)))))

(t/deftest a-hand-reaches-a-layer-of-its-board
  (let [file    (reaching-file)
        objects (:objects (thf/current-page file))
        tl      (get (timelines file) (thi/id :board-b))
        rect    (copy-of objects (thi/id :copy) :main-rect)
        reach?  #(cltl/can-reach? objects %1 (thi/id :hand) %2)]
    (t/is (reach? tl (thi/id :ball)))
    (t/is (not (reach? tl (thi/id :fore))) "not a link of its chain")
    (t/is (not (cltl/can-reach? objects tl (thi/id :fore) (thi/id :ball))) "a chain of one link")
    (t/is (not (reach? tl rect)) "not a layer of a copy the board does not animate")
    (t/is (reach? (cta/add-keyframe tl rect {:time 0 :property :x :value 1010}) rect))
    (let [goals (set (cltl/reach-goals objects tl (thi/id :hand)))]
      (t/is (contains? goals (thi/id :ball)) "the layers it can reach")
      (t/is (not-any? goals [(thi/id :upper) (thi/id :fore) (thi/id :hand) (thi/id :copy) rect])))))

(defn- rig-component-file
  "A component whose main holds an arm (an upper arm with no track, a
  forearm following it, a hand following that) reaching a still ball;
  a copy of it in board B, and a copy of a component that holds a copy
  of it in board C."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-simple-component :rig :rig-main :upper
                                           :root-params {:x 0 :y 0 :width 400 :height 300}
                                           :child-params {:x 50 :y 95 :width 100 :height 10})
                 (ths/add-sample-shape :fore :type :rect :parent-label :rig-main
                                       :x 150 :y 95 :width 100 :height 10)
                 (ths/add-sample-shape :hand :type :rect :parent-label :rig-main
                                       :x 290 :y 90 :width 20 :height 20)
                 (ths/add-sample-shape :ball :type :rect :parent-label :rig-main
                                       :x 235 :y 155 :width 10 :height 10))
        main (thi/id :rig-main)
        tl   (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                 (cta/set-follow (thi/id :fore) {:leader (thi/id :upper)})
                 (cta/set-follow (thi/id :hand) {:leader (thi/id :fore)})
                 (cta/set-ik (thi/id :hand) {:goal (thi/id :ball)}))]
    (-> file
        (thf/apply-changes (-> (pcb/empty-changes nil)
                               (pcb/with-page (thf/current-page file))
                               (pcb/set-timeline main tl)))
        (tho/add-frame :board-b :x 1000 :y 100 :width 600 :height 400)
        (thc/instantiate-component :rig :copy :parent-label :board-b)
        (tho/add-frame :outer-main :x 0 :y 1000 :width 600 :height 400)
        (thc/instantiate-component :rig :nested :parent-label :outer-main)
        (thc/make-component :outer :outer-main)
        (tho/add-frame :board-c :x 2000 :y 100 :width 800 :height 600)
        (thc/instantiate-component :outer :copy2 :parent-label :board-c))))

(defn- played-in
  "The copy of the main shape `label` in the copy `head` of `objects`,
  through a component in a component too."
  [objects head label]
  (let [main (thi/id label)]
    (d/seek (fn [id]
              (loop [ref (get-in objects [id :shape-ref]) depth 0]
                (cond (nil? ref)    false
                      (= ref main)  true
                      (> depth 4)   false
                      :else         (recur (get-in objects [ref :shape-ref]) (inc depth)))))
            (cfh/get-children-ids objects head))))

(t/deftest a-copy-of-a-rig-reaches-the-copy-of-its-still-goal
  (let [file      (rig-component-file)
        page      (thf/current-page file)
        objects   (:objects page)
        main-page (constantly {:objects objects :timelines (:timelines page)})]
    (doseq [[board head] [[(thi/id :board-b) (thi/id :copy)] [(thi/id :board-c) (thi/id :copy2)]]]
      (let [copies (cltl/instance-timelines objects board nil main-page)
            tl     (get copies [:copy head (thi/id :rig-main)])
            [upper fore hand ball] (map #(played-in objects head %) [:upper :fore :hand :ball])
            centre #(let [mods  (get-in % [%2 :modifiers])
                          shape (cond-> (get objects %2) (some? mods) (gsh/transform-shape mods))]
                      (grc/rect->center (:selrect shape)))]
        (t/is (every? some? [upper fore hand ball]) (str "the copy " head))
        (t/is (= ball (get-in tl [:tracks hand :ik :goal])) "it reaches the copy of the still ball")
        (t/is (= upper (get-in tl [:tracks fore :follow :leader])) "its forearm follows the copy of the upper arm")
        (doseq [elapsed [0 300 700]]
          (let [tree (cltl/animation-tree copies objects elapsed)
                w    (centre tree hand)
                g    (centre tree ball)]
            (t/is (and (mth/close? (:x g) (:x w) 0.01) (mth/close? (:y g) (:y w) 0.01))
                  (str "on the ball at " elapsed ": " w " " g))))
        (let [export (cltl/with-instance-tracks (cta/make-timeline {:board-id board :duration 1000})
                       (vals copies) objects)
              tree   (ctaf/timeline->modif-tree export objects 300)
              w      (centre tree hand)
              g      (centre tree ball)]
          (t/is (and (mth/close? (:x g) (:x w) 0.5) (mth/close? (:y g) (:y w) 0.5))
                "the exports bake it alike"))))))

(t/deftest the-guides-of-a-board-are-its-own-and-those-of-its-copies
  (let [file    (rig-component-file)
        main    (thi/id :rig-main)
        tl      (cta/set-guide (get-in (thf/current-page file) [:timelines main]) (thi/id :ball) true)
        file    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                            (pcb/with-page (thf/current-page file))
                                            (pcb/set-timeline main tl)))
        page    (thf/current-page file)
        objects (:objects page)
        guides  #(cltl/guide-ids page {(:id file) file} (thi/id %))]
    (t/is (= #{(thi/id :ball)} (guides :rig-main)) "its own")
    (t/is (= #{(played-in objects (thi/id :copy) :ball)} (guides :board-b)) "the ring of a copy")
    (t/is (= #{(played-in objects (thi/id :copy2) :ball)} (guides :board-c)) "of a copy in a copy")
    (t/is (empty? (cltl/guide-ids page {} (thi/id :board-b))) "none without the files of the components")))

(t/deftest a-layer-follows-one-of-its-board-only
  (let [file    (-> (following-file)
                    (tho/add-group :group :parent-label :board-a)
                    (ths/add-sample-shape :inner :type :rect :parent-label :group
                                          :x 120 :y 120 :width 10 :height 10))
        page    (thf/current-page file)
        objects (:objects page)
        tl      (get (timelines file) (thi/id :board-a))
        ids     #(set (cltl/follow-leaders objects tl (thi/id %)))]
    (t/is (= #{(thi/id :group) (thi/id :inner)} (ids :rect))
          "not itself, the board, nor the dot that follows it")
    (t/is (= #{(thi/id :rect) (thi/id :group) (thi/id :inner)} (ids :dot)))
    (t/is (not (contains? (ids :rect) (thi/id :board-b))) "not of another board")
    (t/is (not (contains? (ids :inner) (thi/id :group))) "not the group it is in")
    (t/is (not (contains? (ids :group) (thi/id :inner))) "not a layer in it")
    (t/is (not (contains? (ids :rect) (thi/id :dot))) "not one that follows it")))

(defn- grouped-following-file
  "Board A with a group holding a rect, and two dots: the first follows
  the group, the group follows the second."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 100 :y 100 :width 400 :height 300)
                 (tho/add-group :group :parent-label :board-a)
                 (ths/add-sample-shape :inner :type :rect :parent-label :group
                                       :x 120 :y 120 :width 40 :height 40)
                 (ths/add-sample-shape :dot :type :rect :parent-label :board-a
                                       :x 300 :y 200 :width 10 :height 10)
                 (ths/add-sample-shape :lead :type :rect :parent-label :board-a
                                       :x 300 :y 300 :width 10 :height 10)
                 (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300))
        a    (thi/id :board-a)
        tl   (-> (cta/make-timeline {:board-id a :duration 1000})
                 (cta/add-keyframe (thi/id :lead) {:time 0 :property :x :value 200})
                 (cta/add-keyframe (thi/id :lead) {:time 1000 :property :x :value 250})
                 (cta/set-follow (thi/id :dot) {:leader (thi/id :group)})
                 (cta/set-follow (thi/id :group) {:leader (thi/id :lead)}))]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page (thf/current-page file))
                                (pcb/set-timeline a tl)))))

(t/deftest a-follower-moved-into-its-leader-follows-no-more
  ;; it moves with it as a layer of it already
  (let [file (relocate (grouped-following-file) :dot :group)
        tl   (get (timelines file) (thi/id :board-a))]
    (t/is (nil? (cta/follow-of tl (thi/id :dot))))
    (t/is (some? (cta/follow-of tl (thi/id :group))) "the others keep theirs")))

(t/deftest a-leader-moved-into-its-follower-leads-no-more
  (let [file (relocate (grouped-following-file) :lead :group)
        tl   (get (timelines file) (thi/id :board-a))]
    (t/is (nil? (cta/follow-of tl (thi/id :group))))
    (t/is (some? (cta/follow-of tl (thi/id :dot))))))

(t/deftest a-leader-moved-to-another-board-leads-no-more
  (let [file (relocate (following-file) :rect :board-b)]
    (t/is (nil? (cta/follow-of (get (timelines file) (thi/id :board-a)) (thi/id :dot))))))

(t/deftest a-layer-of-a-copy-leads-when-its-board-animates-it
  ;; what it plays of its main moves no follower
  (let [file    (-> (following-component-file :main-rect)
                    (ths/add-sample-shape :dot-b :type :rect :parent-label :board-b
                                          :x 1300 :y 300 :width 10 :height 10))
        {:keys [objects board copy]} (component-scene file)
        rect    (copy-of objects copy :main-rect)
        tl      (cta/make-timeline {:board-id board})]
    (t/is (not (cltl/can-follow? objects tl (thi/id :dot-b) rect)))
    (t/is (not (contains? (set (cltl/follow-leaders objects tl (thi/id :dot-b))) rect)))
    (let [tl (-> tl
                 (cta/add-keyframe rect {:time 0 :property :y :value 50})
                 (cta/add-keyframe rect {:time 1000 :property :y :value 90}))]
      (t/is (cltl/can-follow? objects tl (thi/id :dot-b) rect) "animated by the board itself"))))

(defn- pasted-follow
  "How a copy of the dot of `grouped-following-file` pasted into the layer
  `parent` follows (see `cta/follow-of`)."
  [parent]
  (let [file    (grouped-following-file)
        page    (thf/current-page file)
        objects (:objects page)
        copy-id (uuid/next)
        copy    (assoc (get objects (thi/id :dot)) :id copy-id :parent-id (thi/id parent))
        changes (-> (pcb/empty-changes nil)
                    (pcb/with-page page)
                    (pcb/with-objects objects)
                    (pcb/add-object copy))
        changes (cltl/generate-copy-timelines changes page {(thi/id :dot) copy-id}
                                              (cltl/animation-sources page objects #{(thi/id :dot)}))]
    (cta/follow-of (get (timelines (thf/apply-changes file changes)) (thi/id :board-a)) copy-id)))

(t/deftest a-follower-pasted-into-its-leader-follows-no-more
  ;; each call makes its file: read the ids after it
  (let [beside (pasted-follow :board-a)]
    (t/is (= (thi/id :group) (:leader beside)) "pasted beside it, it follows the group"))
  (t/is (nil? (pasted-follow :group)) "pasted into it, it moves with it already"))
