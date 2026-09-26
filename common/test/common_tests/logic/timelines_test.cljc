;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.logic.timelines-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.logic.libraries :as cll]
   [app.common.logic.shapes :as cls]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
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
     :timeline tl
     :a (:id a)
     :b (:id b)}))

(defn- shown-by
  "The shape `id` of `objects` as the modif-tree `tree` shows it."
  [tree objects id]
  (gsh/transform-shape (get objects id) (get-in tree [id :modifiers])))

(t/deftest edit-preview-shows-a-move-where-it-ends-up
  (let [{:keys [objects timeline a b]} (motion-scene)
        shown   (cltl/shown-shapes timeline objects 500)
        tree    (cltl/edit-preview timeline objects 500 {a {:modifiers (ctm/move-modifiers (gpt/point 30 0))}})]
    ;; A where the animation shows it, 30 further right
    (t/is (mth/close? (+ 30 (get-in shown [a :selrect :x])) (get-in (shown-by tree objects a) [:selrect :x])))
    ;; B, left alone, where the animation shows it
    (t/is (mth/close? (get-in shown [b :selrect :x]) (get-in (shown-by tree objects b) [:selrect :x])))))

(t/deftest edit-preview-turns-a-shape-where-it-is-shown
  (let [{:keys [objects timeline a]} (motion-scene)
        shape   (get objects a)
        shown   (cltl/shown-shapes timeline objects 500)
        tree    (cltl/edit-preview timeline objects 500
                                   {a {:modifiers (ctm/rotation-modifiers shape (gsh/shape->center shape) 30)}})
        turned  (shown-by tree objects a)]
    (t/is (mth/close? 30 (:rotation turned)))
    ;; around its center as shown, not the one it has at rest
    (t/is (gpt/close? (gsh/shape->center (get shown a)) (gsh/shape->center turned)))))
