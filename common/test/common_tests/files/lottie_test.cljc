;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.files.lottie-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.files.lottie :as lottie]
   [app.common.math :as mth]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.types.animation :as cta]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [clojure.test :as t]
   [clojure.walk :as walk]))

(t/use-fixtures :each thi/test-fixture)

;; --- A small Lottie document

(defn- static [v] {:a 0 :k v})

(defn- kf
  "A keyframe at `frame`, easing out of it like `ease-in-out`."
  [frame value & {:as opts}]
  (merge {:t frame
          :s (if (sequential? value) value [value])
          :o {:x [0.42] :y [0]}
          :i {:x [0.58] :y [1]}}
         opts))

(defn- anim [& kfs] {:a 1 :k (vec kfs)})

(def ^:private rest-ks
  {:o (static 100) :r (static 0) :p (static [0 0 0]) :a (static [0 0 0]) :s (static [100 100 100])})

(def ^:private group-tr
  {:ty "tr" :p (static [0 0]) :a (static [0 0]) :s (static [100 100]) :r (static 0) :o (static 100)})

(defn- rect-group
  [[x y w h] color]
  {:ty "gr" :nm "Rect"
   :it [{:ty "rc" :nm "Rect path" :s (static [w h]) :p (static [(+ x (/ w 2)) (+ y (/ h 2))]) :r (static 0)}
        {:ty "fl" :c (static color) :o (static 100)}
        group-tr]})

(defn- layer
  [ind name shapes & {:as opts}]
  (merge {:ty 4 :ind ind :nm name :ks rest-ks :shapes shapes :ip 0 :op 60 :st 0} opts))

(defn- document
  [layers & {:as opts}]
  (merge {:v "5.7.0" :fr 30 :ip 0 :op 60 :w 200 :h 100 :nm "Test" :layers layers :assets []} opts))

(defn- shape-named
  [{:keys [shapes]} name]
  (some #(when (= name (:name %)) %) shapes))

(defn- keyframes-of
  [{:keys [timeline]} shape property]
  (mapv (juxt :time :value) (cta/property-keyframes timeline (:id shape) property)))

(defn- norm
  "`x` with its whole numbers as integers, to 3 decimals the others, so
  they compare the same on the JVM and in JS."
  [x]
  (walk/postwalk (fn [v]
                   (if (number? v)
                     (let [v (mth/precision v 3)]
                       (if (== v (mth/round v)) (long (mth/round v)) v))
                     v))
                 x))

(defn- bounds
  "The x, y, width and height of the selrect of `shape`."
  [shape]
  (norm ((juxt :x :y :width :height) (:selrect shape))))

(defn- close-pairs?
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map (fn [[t0 v0] [t1 v1]] (and (= t0 t1) (mth/close? v0 v1 0.001)))
                          expected actual))))

;; --- Tests

(t/deftest a-layer-draws-where-the-lottie-draws-it
  (let [result (lottie/lottie->shapes
                (document [(layer 1 "Box" [(rect-group [20 10 40 30] [1 0 0 1])])])
                {:x 100 :y 50})
        [board & shapes] (:shapes result)
        box    (shape-named result "Box")]
    (t/is (= :frame (:type board)))
    (t/is (= [100 50 200 100] (bounds board)))
    (t/is (= [(:id board)] (distinct (map :parent-id shapes))))
    (t/is (= :rect (:type box)))
    (t/is (= [120 60 40 30] (bounds box)))
    (t/is (= [{:fill-color "#ff0000" :fill-opacity 1}] (:fills box)))
    (t/is (= (:id board) (get-in result [:timeline :board-id])))
    (t/is (= 2000 (get-in result [:timeline :duration])) "60 frames at 30 fps")
    (t/is (empty? (get-in result [:timeline :tracks])))
    (t/is (empty? (:skipped result)))))

(t/deftest layer-keyframes-become-keyframes
  (let [ks     (assoc rest-ks
                      :p (static [40 25 0]) :a (static [40 25 0])
                      :o (anim (kf 0 0) (kf 15 100 :h 1) (kf 30 50))
                      :s (anim (kf 0 [50 50 100]) (kf 30 [100 100 100])))
        result (lottie/lottie->shapes
                (document [(layer 1 "Box" [(rect-group [20 10 40 30] [0 0 1 1])] :ks ks)])
                {:x 0 :y 0})
        box    (shape-named result "Box")
        [first-kf second-kf] (cta/property-keyframes (:timeline result) (:id box) :opacity)]
    (t/is (= :rect (:type box)) "it turns around its middle: no board needed")
    (t/is (close-pairs? [[0 0] [500 1] [1000 0.5]] (keyframes-of result box :opacity)))
    (t/is (= {:type :bezier :curve [0.42 0 0.58 1]} (:easing first-kf)))
    (t/is (= :step (:interpolation second-kf)) "a hold")
    (t/is (close-pairs? [[0 0.5] [1000 1]] (keyframes-of result box :scale-x)))
    (t/is (close-pairs? [[0 0.5] [1000 1]] (keyframes-of result box :scale-y)))))

(t/deftest an-anchor-away-from-the-middle-makes-a-board-around-it
  (let [ks     (assoc rest-ks
                      :p (static [20 10 0]) :a (static [20 10 0])
                      :r (anim (kf 0 0) (kf 30 90)))
        result (lottie/lottie->shapes
                (document [(layer 1 "Box" [(rect-group [20 10 40 30] [0 0 1 1])] :ks ks)])
                {:x 0 :y 0})
        node   (shape-named result "Box")
        rect   (shape-named result "Rect path")
        {:keys [x y width height]} (:selrect node)]
    (t/is (= :frame (:type node)))
    (t/is (= [20 10] (norm [(+ x (/ width 2)) (+ y (/ height 2))])) "centred on the anchor")
    (t/is (true? (:show-content node)) "it does not clip")
    (t/is (= (:id node) (:parent-id rect)))
    (t/is (= [20 10 40 30] (bounds rect)))
    (t/is (close-pairs? [[0 0] [1000 90]] (keyframes-of result node :rotation)))))

(t/deftest positions-move-from-rest-along-their-motion-path
  (let [ks     (assoc rest-ks
                      :p (anim (kf 0 [40 25 0] :to [10 0 0] :ti [0 -5 0])
                               (kf 30 [100 55 0]))
                      :a (static [40 25 0]))
        result (lottie/lottie->shapes
                (document [(layer 1 "Box" [(rect-group [20 10 40 30] [0 0 1 1])] :ks ks)])
                {:x 0 :y 0})
        box    (shape-named result "Box")
        xs     (cta/property-keyframes (:timeline result) (:id box) :x)
        ys     (cta/property-keyframes (:timeline result) (:id box) :y)]
    (t/is (= [[0 20] [1000 80]] (norm (mapv (juxt :time :value) xs))))
    (t/is (= [[0 10] [1000 40]] (norm (mapv (juxt :time :value) ys))))
    (t/is (= 10 (:path-out (first xs))) "the tangent out of the first keyframe")
    (t/is (= -5 (:path-in (second ys))) "the tangent into the second one")))

(t/deftest in-and-out-points-show-the-layer-only-meanwhile
  (let [result (lottie/lottie->shapes
                (document [(layer 1 "Box" [(rect-group [0 0 10 10] [0 0 1 1])] :ip 15 :op 45)])
                {:x 0 :y 0})
        box    (shape-named result "Box")]
    (t/is (= [[0 0] [500 1] [1500 0]] (keyframes-of result box :opacity)))
    (t/is (every? #(= :step (:interpolation %))
                  (cta/property-keyframes (:timeline result) (:id box) :opacity)))))

(t/deftest group-transforms-are-baked-into-the-outline
  (let [line   {:ty "gr" :nm "Line"
                :it [{:ty "sh" :nm "Line path" :ks (static {:c false :v [[0 0] [10 0]] :i [[0 0] [0 0]] :o [[0 0] [0 0]]})}
                     {:ty "st" :c (static [0 0 0 1]) :o (static 100) :w (static 2) :lc 2}
                     (assoc group-tr :p (static [50 50]) :r (static 90))]}
        result (lottie/lottie->shapes (document [(layer 1 "Line" [line])]) {:x 0 :y 0})
        path   (shape-named result "Line")
        [p0 p1] (path/get-points (:content path))]
    (t/is (= :path (:type path)))
    (t/is (mth/close? 50 (:x p0)))
    (t/is (mth/close? 50 (:y p0)))
    (t/is (mth/close? 50 (:x p1)))
    (t/is (mth/close? 60 (:y p1)) "turned by 90 degrees")
    (t/is (= [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 2 :stroke-alignment :center
               :stroke-style :solid :stroke-cap-start :round :stroke-cap-end :round}]
             (norm (:strokes path))))))

(t/deftest parents-carry-the-layers-they-parent
  (let [null   {:ty 3 :ind 1 :nm "Controller" :ip 0 :op 60 :st 0
                :ks (assoc rest-ks :p (static [100 50 0]) :r (anim (kf 0 0) (kf 30 45)))}
        child  (layer 2 "Child" [(rect-group [0 0 20 20] [0 1 0 1])] :parent 1
                      :ks (assoc rest-ks :p (static [-10 -10 0])))
        result (lottie/lottie->shapes (document [child null]) {:x 0 :y 0})
        node   (shape-named result "Controller")
        box    (shape-named result "Child")]
    (t/is (= :frame (:type node)))
    (t/is (= (:id node) (:parent-id box)) "the child moves with its parent")
    (t/is (= [90 40 20 20] (bounds box)))
    (t/is (close-pairs? [[0 0] [1000 45]] (keyframes-of result node :rotation)))))

(t/deftest precompositions-play-from-their-start
  (let [inner  (layer 1 "Inner" [(rect-group [0 0 10 10] [0 0 1 1])]
                      :ks (assoc rest-ks :o (anim (kf 0 0) (kf 15 100))))
        doc    (document [{:ty 0 :ind 1 :nm "Pre" :refId "comp_1" :ks rest-ks :ip 0 :op 60 :st 15
                           :w 200 :h 100}]
                         :assets [{:id "comp_1" :layers [inner]}])
        result (lottie/lottie->shapes doc {:x 0 :y 0})
        pre    (shape-named result "Pre")
        box    (shape-named result "Inner")]
    (t/is (= :frame (:type pre)))
    (t/is (= (:id pre) (:parent-id box)))
    (t/is (= [[500 0] [1000 1]] (keyframes-of result box :opacity)) "15 frames later")))

(t/deftest trim-paths-animate-the-strokes
  (let [line   {:ty "gr" :nm "Line"
                :it [{:ty "sh" :ks (static {:c false :v [[0 0] [10 0]] :i [[0 0] [0 0]] :o [[0 0] [0 0]]})}
                     {:ty "st" :c (static [0 0 0 1]) :o (static 100) :w (static 2)}
                     group-tr]}
        trim   {:ty "tm" :s (anim (kf 0 0) (kf 30 100)) :e (anim (kf 0 0) (kf 15 100)) :o (static 0) :m 1}
        result (lottie/lottie->shapes (document [(layer 1 "Spark" [line trim])]) {:x 0 :y 0})
        path   (shape-named result "Spark")]
    (t/is (close-pairs? [[0 0] [1000 1]] (keyframes-of result path :trim-start)))
    (t/is (close-pairs? [[0 0] [500 1]] (keyframes-of result path :trim-end)))))

(t/deftest what-a-board-cannot-show-is-counted
  (let [morph  {:ty "gr"
                :it [{:ty "sh" :ks (anim (kf 0 [{:c true :v [[0 0] [1 0] [1 1]] :i [[0 0] [0 0] [0 0]] :o [[0 0] [0 0] [0 0]]}])
                                         (kf 30 [{:c true :v [[0 0] [2 0] [2 2]] :i [[0 0] [0 0] [0 0]] :o [[0 0] [0 0] [0 0]]}]))}
                     {:ty "fl" :c (static [0 0 0 1]) :o (static 100)}
                     group-tr]}
        result (lottie/lottie->shapes
                (document [{:ty 5 :ind 1 :nm "Title" :ks rest-ks :ip 0 :op 60}
                           (layer 2 "Matte" [(rect-group [0 0 10 10] [0 0 0 1])] :td 1)
                           (layer 3 "Matted" [(rect-group [0 0 10 10] [1 0 0 1])] :tt 1)
                           (layer 4 "Morph" [morph])])
                {:x 0 :y 0})]
    (t/is (= {:text 1 :matte 2 :path-morph 1} (:skipped result)))
    (t/is (nil? (shape-named result "Matte")) "a matte is not drawn")
    (t/is (some? (shape-named result "Matted")))
    (t/is (= :path (:type (shape-named result "Morph"))) "at its first keyframe")))

(t/deftest image-layers-show-the-uploaded-image
  (let [media-id (uuid/next)
        doc      (document [{:ty 2 :ind 1 :nm "Photo" :refId "img_1" :ip 0 :op 60 :st 0
                             :ks (assoc rest-ks :p (static [50 30 0]) :a (static [10 5 0]))}]
                           :assets [{:id "img_1" :w 20 :h 10 :e 1 :p "data:image/png;base64,AAAA"}])
        result   (lottie/lottie->shapes doc {:x 0 :y 0
                                             :images {"img_1" {:id media-id :width 40 :height 20
                                                               :mtype "image/png" :w 20 :h 10}}})
        photo    (some #(when (= :rect (:type %)) %) (:shapes result))]
    (t/is (= [{:id "img_1" :w 20 :h 10 :data-uri "data:image/png;base64,AAAA"}] (lottie/images doc)))
    (t/is (= [40 25 20 10] (bounds photo)))
    (t/is (= media-id (get-in photo [:fills 0 :fill-image :id])))))

(t/deftest the-board-goes-into-a-file-with-its-timeline
  (let [file    (thf/sample-file :file1)
        page    (thf/current-page file)
        ks      (assoc rest-ks :p (static [40 25 0]) :a (static [40 25 0])
                       :o (anim (kf 0 0) (kf 30 100)))
        result  (lottie/lottie->shapes
                 (document [(layer 1 "Box" [(rect-group [20 10 40 30] [0 0 1 1])] :ks ks)
                            (layer 2 "Spinner" [(rect-group [0 0 10 10] [1 0 0 1])]
                                   :ks (assoc rest-ks :r (anim (kf 0 0) (kf 30 180))))])
                 {:x 0 :y 0})
        board   (first (:shapes result))
        changes (as-> (-> (pcb/empty-changes nil (:id page))
                          (pcb/with-page page)
                          (pcb/with-objects (:objects page))) changes
                  (reduce pcb/add-object changes (:shapes result))
                  (pcb/change-timeline changes (:id board) (:timeline result)))
        file'   (thf/apply-changes file changes)
        page'   (thf/current-page file')]
    (t/is (= (count (:shapes result)) (dec (count (:objects page')))) "all the shapes, but the root")
    (t/is (cta/valid-timeline? (get-in page' [:timelines (:id board)])))
    (t/is (every? cts/valid-shape? (vals (:objects page'))))))

(t/deftest exported-animations-come-back
  (let [board  (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300 :name "Board"})
        shape  (cts/setup-shape {:type :rect :x 100 :y 100 :width 200 :height 100 :name "Box"
                                 :fills [{:fill-color "#336699" :fill-opacity 1}]})
        tl     (-> (cta/make-timeline {:board-id (:id board) :duration 1000 :playback :loop})
                   (cta/add-keyframe (:id shape) {:time 0 :property :x :value 100 :easing :ease-in})
                   (cta/add-keyframe (:id shape) {:time 1000 :property :x :value 400})
                   (cta/add-keyframe (:id shape) {:time 0 :property :opacity :value 1})
                   (cta/add-keyframe (:id shape) {:time 1000 :property :opacity :value 0}))
        doc    (cta/timeline->lottie tl {(:id board) board (:id shape) shape})
        result (lottie/lottie->shapes doc {:x 0 :y 0})
        box    (shape-named result "Box")
        xs     (cta/property-keyframes (:timeline result) (:id box) :x)]
    (t/is (= [100 100 200 100] (bounds box)))
    (t/is (= "#336699" (get-in box [:fills 0 :fill-color])))
    (t/is (close-pairs? [[0 100] [1000 400]] (mapv (juxt :time :value) xs)))
    (t/is (= (cta/easing-curve :ease-in) (cta/easing-curve (:easing (first xs)))))
    (t/is (close-pairs? [[0 1] [1000 0]] (keyframes-of result box :opacity)))
    (t/is (= 1000 (get-in result [:timeline :duration])))))
