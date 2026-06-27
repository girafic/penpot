;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.animation-test
  (:require
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- mk-timeline []
  (cta/make-timeline {:name "Test" :duration 1000}))

(t/deftest make-timeline-defaults
  (let [tl (cta/make-timeline {})]
    (t/is (uuid? (:id tl)))
    (t/is (= "Animation" (:name tl)))
    (t/is (= cta/default-duration (:duration tl)))
    (t/is (= {} (:tracks tl)))
    (t/is (cta/valid-timeline? tl))))

(t/deftest add-and-remove-keyframe
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-keyframe sid {:time 500 :property :x :value 100}))
        kfs (get-in tl [:tracks sid :keyframes])]
    (t/is (= 2 (count kfs)))
    (t/is (= [0 500] (mapv :time kfs)))
    (t/is (cta/valid-timeline? tl))

    (t/testing "re-adding the same time/property replaces the value"
      (let [tl2 (cta/add-keyframe tl sid {:time 0 :property :x :value 42})
            kfs (get-in tl2 [:tracks sid :keyframes])]
        (t/is (= 2 (count kfs)))
        (t/is (= 42 (:value (first kfs))))))

    (t/testing "removing the last keyframe drops the track"
      (let [first-id  (:id (first kfs))
            second-id (:id (second kfs))
            tl2 (-> tl
                    (cta/remove-keyframe sid first-id)
                    (cta/remove-keyframe sid second-id))]
        (t/is (nil? (cta/get-track tl2 sid)))))))

(t/deftest linear-interpolation
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0 :easing :linear})
                (cta/add-keyframe sid {:time 1000 :property :x :value 100 :easing :linear}))]
    (t/testing "before first / after last keyframe clamps"
      (t/is (= 0 (get-in (cta/values-at tl -50) [sid :x])))
      (t/is (= 100 (get-in (cta/values-at tl 2000) [sid :x]))))

    (t/testing "midpoint interpolates linearly"
      (t/is (mth/close? 50.0 (double (get-in (cta/values-at tl 500) [sid :x])) 0.001)))

    (t/testing "quarter point"
      (t/is (mth/close? 25.0 (double (get-in (cta/values-at tl 250) [sid :x])) 0.001)))))

(t/deftest step-interpolation-holds-value
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :opacity :value 0 :interpolation :step})
                (cta/add-keyframe sid {:time 1000 :property :opacity :value 1}))]
    (t/is (= 0 (get-in (cta/values-at tl 999) [sid :opacity])))
    (t/is (= 1 (get-in (cta/values-at tl 1000) [sid :opacity])))))

(t/deftest cubic-bezier-bounds-and-monotonic
  (let [curve [0.42 0.0 0.58 1.0]]
    (t/is (= 0.0 (cta/cubic-bezier curve 0.0)))
    (t/is (= 1.0 (cta/cubic-bezier curve 1.0)))
    (t/testing "ease-in-out stays within [0,1] and is monotonic"
      (loop [t 0.0 prev -1.0]
        (when (<= t 1.0)
          (let [v (cta/cubic-bezier curve t)]
            (t/is (and (>= v -0.0001) (<= v 1.0001)))
            (t/is (>= v (- prev 0.0001)))
            (recur (+ t 0.1) v)))))))

(t/deftest values-at-multiple-properties
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-keyframe sid {:time 1000 :property :x :value 200})
                (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                (cta/add-keyframe sid {:time 1000 :property :opacity :value 0}))
        vs  (cta/values-at tl 500)]
    (t/is (mth/close? 100.0 (double (get-in vs [sid :x])) 0.001))
    (t/is (mth/close? 0.5 (double (get-in vs [sid :opacity])) 0.001))))

(t/deftest timeline->opacity-only-opacity
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                (cta/add-keyframe sid {:time 1000 :property :opacity :value 0})
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-keyframe sid {:time 1000 :property :x :value 50}))
        op  (cta/timeline->opacity tl 500)]
    (t/is (contains? op sid))
    (t/is (mth/close? 0.5 (double (get op sid)) 0.001))))

(t/deftest timeline->modif-tree-moves-shape
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 100})
        sid   (:id shape)
        objects {sid shape}
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :x :value (-> shape :selrect :x)})
                  (cta/add-keyframe sid {:time 1000 :property :x
                                         :value (+ (-> shape :selrect :x) 100)}))
        ;; at t=0 there is no displacement -> empty modifiers (no entry)
        tree0 (cta/timeline->modif-tree tl objects 0)
        ;; at t=1000 the shape should move +100 in x
        tree1 (cta/timeline->modif-tree tl objects 1000)]
    (t/is (not (contains? tree0 sid)))
    (t/is (contains? tree1 sid))
    (t/is (some? (get-in tree1 [sid :modifiers])))))

(t/deftest remove-shapes-drops-tracks
  (let [a (uuid/next)
        b (uuid/next)
        tl (-> (mk-timeline)
               (cta/add-keyframe a {:time 0 :property :x :value 0})
               (cta/add-keyframe b {:time 0 :property :y :value 0}))
        tl2 (cta/remove-shapes tl [a])]
    (t/is (nil? (cta/get-track tl2 a)))
    (t/is (some? (cta/get-track tl2 b)))))
