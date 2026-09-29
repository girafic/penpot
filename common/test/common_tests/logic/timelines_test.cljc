;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.logic.timelines-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.logic.libraries :as cll]
   [app.common.logic.shapes :as cls]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.test-helpers.components :as thc]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape :as cts]
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

(defn- relocate
  [file label parent-label]
  (let [page (thf/current-page file)]
    (thf/apply-changes file (cls/generate-relocate (-> (pcb/empty-changes nil)
                                                       (pcb/with-page-id (:id page))
                                                       (pcb/with-objects (:objects page))
                                                       (pcb/with-library-data (:data file)))
                                                   (thi/id parent-label)
                                                   0
                                                   #{(thi/id label)}))))

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

(defn- component-scene
  "An animated component: its main, a board at 0,0 whose rect moves from
  10 to 60 in 1 s, over and over. A copy of it in board B at 1000,100."
  []
  (let [file     (-> (thf/sample-file :file1)
                     (tho/add-simple-component :anim :main :main-rect
                                               :root-params {:x 0 :y 0 :width 200 :height 200}
                                               :child-params {:x 10 :y 20 :width 50 :height 50})
                     (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                     (thc/instantiate-component :anim :copy :parent-label :board-b))
        main     (thi/id :main)
        tl       (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                     (cta/add-keyframe (thi/id :main-rect) {:time 0 :property :x :value 10 :easing :linear})
                     (cta/add-keyframe (thi/id :main-rect) {:time 1000 :property :x :value 60}))
        file     (thf/apply-changes file (-> (pcb/empty-changes nil)
                                             (pcb/with-page (thf/current-page file))
                                             (pcb/set-timeline main tl)))
        page     (thf/current-page file)
        objects  (:objects page)
        copy     (thi/id :copy)]
    {:objects   objects
     :timelines (:timelines page)
     :board     (thi/id :board-b)
     :copy      copy
     :copy-rect (first (get-in objects [copy :shapes]))
     :main-page (constantly {:objects objects :timelines (:timelines page)})}))

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
