;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.animation-test
  (:require
   [app.common.data :as d]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.animation :as cta]
   [app.common.types.color :as clr]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [clojure.test :as t]
   [common-tests.types.shape-decode-encode-test :refer [json-roundtrip]]))

(defn- mk-timeline []
  (cta/make-timeline {:board-id (uuid/next) :name "Test" :duration 1000}))

(t/deftest make-timeline-defaults
  (let [board-id (uuid/next)
        tl (cta/make-timeline {:board-id board-id})]
    (t/is (= board-id (:board-id tl)))
    (t/is (= "Animation" (:name tl)))
    (t/is (= cta/default-duration (:duration tl)))
    (t/is (= {} (:tracks tl)))
    (t/is (cta/valid-timeline? tl))))

(t/deftest timeline-requires-board-id
  (t/is (cta/valid-timeline? (mk-timeline)))
  (t/is (not (cta/valid-timeline? (dissoc (mk-timeline) :board-id)))))

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

(t/deftest width-and-height-resize-the-shape
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 40})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :width :value 100})
                  (cta/add-keyframe sid {:time 1000 :property :width :value 200})
                  (cta/add-keyframe sid {:time 0 :property :height :value 40})
                  (cta/add-keyframe sid {:time 1000 :property :height :value 80}))
        mods  (get-in (cta/timeline->modif-tree tl {sid shape} 1000) [sid :modifiers])
        shape' (gsh/transform-shape shape mods)]
    (t/is (mth/close? 200.0 (double (-> shape' :selrect :width)) 0.01))
    (t/is (mth/close? 80.0 (double (-> shape' :selrect :height)) 0.01))))

(t/deftest opacity-is-a-change-property-modifier
  ;; Opacity must ride the modifier pipeline (so it works in the WASM
  ;; renderer via set-shape-opacity, and in SVG/export via transform-shape).
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 100})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :opacity :value 0}))
        tree  (cta/timeline->modif-tree tl {sid shape} 500)
        modifiers (get-in tree [sid :modifiers])]
    (t/testing "opacity-only track still yields a (non-empty) modifier"
      (t/is (contains? tree sid))
      (t/is (not (ctm/empty? modifiers))))
    (t/testing "applying the modifier sets the interpolated opacity"
      (let [shape' (ctm/apply-structure-modifiers shape modifiers)]
        (t/is (mth/close? 0.5 (double (:opacity shape')) 0.001))))))

(t/deftest remove-shapes-drops-tracks
  (let [a (uuid/next)
        b (uuid/next)
        tl (-> (mk-timeline)
               (cta/add-keyframe a {:time 0 :property :x :value 0})
               (cta/add-keyframe b {:time 0 :property :y :value 0}))
        tl2 (cta/remove-shapes tl [a])]
    (t/is (nil? (cta/get-track tl2 a)))
    (t/is (some? (cta/get-track tl2 b)))

    (t/testing "accepts a set of ids (as used by the deletion path)"
      (let [tl3 (cta/remove-shapes tl #{a b})]
        (t/is (empty? (:tracks tl3)))))))

(t/deftest easing->css-output
  (t/is (= "linear" (cta/easing->css :linear)))
  (t/is (= "ease-in-out" (cta/easing->css :ease-in-out)))
  (t/is (= "cubic-bezier(0.42, 0, 0.58, 1)"
           (cta/easing->css {:type :bezier :curve [0.42 0.0 0.58 1.0]}))))

(t/deftest timeline->css-output
  (let [shape (cts/setup-shape {:type :rect :x 10 :y 20 :width 100 :height 100})
        sid   (:id shape)
        bx    (-> shape :selrect :x)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback :loop})
                  (cta/add-keyframe sid {:time 0 :property :x :value bx :easing :ease-in})
                  (cta/add-keyframe sid {:time 1000 :property :x :value (+ bx 100)})
                  (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :opacity :value 0}))
        css   (cta/timeline->css tl {sid shape})]
    (t/is (re-find #"@keyframes penpot-anim-" css))
    (t/is (re-find #"\.penpot-shape-" css))
    (t/is (re-find #"translate\(100px, 0px\)" css))
    (t/is (re-find #"opacity: 0;" css))
    (t/is (re-find #"animation-timing-function: ease-in;" css))
    (t/is (re-find #"linear infinite;" css))))

(t/deftest timeline->css-writes-fill-stroke-shadow-blur
  (let [shape (cts/setup-shape
               {:type :rect :x 0 :y 0 :width 100 :height 100
                :fills [{:fill-color "#000000" :fill-opacity 1}]
                :strokes [{:stroke-color "#111111" :stroke-opacity 1 :stroke-width 1}]
                :shadow [{:id (uuid/next) :style :drop-shadow
                          :offset-x 0 :offset-y 0 :blur 0 :spread 0
                          :hidden false
                          :color {:color "#000000" :opacity 1}}]
                :blur {:id (uuid/next) :type :layer-blur :value 0 :hidden false}})
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                  (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#000000"})
                  (cta/add-keyframe sid {:time 1000 :property :fill-color :index 0 :value "#ff0000"})
                  (cta/add-keyframe sid {:time 0 :property :stroke-width :index 0 :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :stroke-width :index 0 :value 8})
                  (cta/add-keyframe sid {:time 0 :property :shadow-offset-x :index 0 :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :shadow-offset-x :index 0 :value 10})
                  (cta/add-keyframe sid {:time 0 :property :blur :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :blur :value 4}))
        css   (cta/timeline->css tl {sid shape})]
    (t/is (re-find #"background-color: #ff0000;" css))
    (t/is (re-find #"border-width: 8px;" css))
    (t/is (re-find #"box-shadow:" css))
    (t/is (re-find #"filter: blur\(4px\);" css))))

(t/deftest timeline->lottie-output
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 800 :height 600})
        bid   (:id board)
        shape (cts/setup-shape {:type :rect :x 100 :y 100 :width 200 :height 100})
        sid   (:id shape)
        bx    (-> shape :selrect :x)
        tl    (-> (cta/make-timeline {:board-id bid :name "Anim" :duration 1000 :playback :loop})
                  (cta/add-keyframe sid {:time 0 :property :x :value bx :easing :ease-in})
                  (cta/add-keyframe sid {:time 1000 :property :x :value (+ bx 300)})
                  (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :opacity :value 0}))
        L     (cta/timeline->lottie tl {bid board sid shape})
        layer (first (:layers L))
        ks    (:ks layer)]
    (t/is (= "5.7.0" (:v L)))
    (t/is (= 60 (:fr L)))
    (t/is (= 60.0 (:op L)))                         ; 1000ms @ 60fps
    (t/is (= 800 (:w L)))
    (t/is (= 600 (:h L)))
    (t/is (= 4 (:ty layer)))
    (t/is (true? (get-in ks [:p :s])))              ; split position
    (t/is (= 1 (get-in ks [:p :x :a])))             ; x animated
    (t/is (= 1 (get-in ks [:o :a])))                ; opacity animated
    (t/is (= [100.0] (get-in ks [:o :k 0 :s])))     ; opacity 1 -> 100
    (t/is (= [0.0] (get-in ks [:o :k 1 :s])))       ; opacity 0 -> 0
    ;; anchor is the shape centre
    (t/is (= [200.0 150.0 0] (get-in ks [:a :k])))))

(t/deftest timeline->lottie-writes-fill-stroke-and-blur
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        bid   (:id board)
        shape (cts/setup-shape
               {:type :rect :x 0 :y 0 :width 40 :height 40
                :fills [{:fill-color "#000000" :fill-opacity 1}]
                :strokes [{:stroke-color "#111111" :stroke-opacity 1 :stroke-width 2}]
                :blur {:id (uuid/next) :type :layer-blur :value 0 :hidden false}})
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id bid :duration 1000})
                  (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#000000"})
                  (cta/add-keyframe sid {:time 1000 :property :fill-color :index 0 :value "#ffffff"})
                  (cta/add-keyframe sid {:time 0 :property :stroke-width :index 0 :value 2})
                  (cta/add-keyframe sid {:time 1000 :property :stroke-width :index 0 :value 8})
                  (cta/add-keyframe sid {:time 0 :property :blur :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :blur :value 6}))
        L     (cta/timeline->lottie tl {bid board sid shape})
        layer (first (:layers L))
        items (get-in layer [:shapes 0 :it])
        fill  (d/seek #(= "fl" (:ty %)) items)
        stroke (d/seek #(= "st" (:ty %)) items)]
    (t/is (= 1 (get-in fill [:c :a])))
    (t/is (= 1 (get-in stroke [:w :a])))
    (t/is (= [8] (get-in stroke [:w :k 1 :s])))
    (t/is (= 29 (get-in layer [:ef 0 :ty])))))

(t/deftest loop-repeats-property-span
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 100 :property :x :value 0})
                (cta/add-keyframe sid {:time 300 :property :x :value 100})
                (cta/add-keyframe sid {:time 0 :property :y :value 5})
                (cta/toggle-loop sid :x))
        x   #(double (get-in (cta/values-at tl %) [sid :x]))]
    (t/is (cta/looping? tl sid :x))
    (t/is (not (cta/looping? tl sid :y)))
    (t/is (cta/valid-timeline? tl))
    (t/is (mth/close? 0.0 (x 50) 0.001))             ; before the first keyframe
    (t/is (mth/close? 50.0 (x 200) 0.001))
    (t/is (mth/close? 50.0 (x 400) 0.001))           ; second repetition
    (t/is (mth/close? 25.0 (x 750) 0.001))           ; 750 -> 150
    (t/is (= 5 (get-in (cta/values-at tl 400) [sid :y])))
    (t/testing "toggling again stops the loop"
      (let [tl (cta/toggle-loop tl sid :x)]
        (t/is (not (cta/looping? tl sid :x)))
        (t/is (nil? (get-in tl [:tracks sid :loops])))
        (t/is (= 100 (get-in (cta/values-at tl 400) [sid :x])))))))

(t/deftest remove-property-and-track
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-keyframe sid {:time 500 :property :x :value 100})
                (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                (cta/toggle-loop sid :x))
        tl' (cta/remove-property tl sid :x)]
    (t/is (= [:opacity] (mapv :property (get-in tl' [:tracks sid :keyframes]))))
    (t/is (not (cta/looping? tl' sid :x)))
    (t/is (nil? (get-in (cta/remove-property tl' sid :opacity) [:tracks sid])))
    (t/is (nil? (get-in (cta/remove-track tl sid) [:tracks sid])))))

(t/deftest paste-keyframes-at-time
  (let [src (uuid/next)
        dst (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe src {:time 100 :property :opacity :value 0})
                (cta/add-keyframe src {:time 400 :property :opacity :value 1}))
        kfs (cta/property-keyframes tl src :opacity)
        tl' (cta/paste-keyframes tl dst kfs #{:opacity} 900)
        pasted (get-in tl' [:tracks dst :keyframes])]
    (t/is (= [900 1200] (mapv :time pasted)))
    (t/is (= [0 1] (mapv :value pasted)))
    (t/is (not-any? (set (map :id kfs)) (map :id pasted)))
    (t/is (cta/looping? tl' dst :opacity))
    (t/is (= 1200 (:duration tl')))
    (t/is (cta/valid-timeline? tl'))))

(t/deftest expand-loops-matches-values-at
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 100 :property :x :value 0})
                (cta/add-keyframe sid {:time 300 :property :x :value 100})
                (cta/toggle-loop sid :x))
        ex  (cta/expand-loops tl)
        x   (fn [tl t] (double (get-in (cta/values-at tl t) [sid :x])))]
    (t/is (nil? (get-in ex [:tracks sid :loops])))
    (t/is (= 1000 (:time (peek (get-in ex [:tracks sid :keyframes])))))
    (t/is (cta/valid-timeline? ex))
    (doseq [t [0 150 200 450 620 999 1000]]
      (t/is (mth/close? (x tl t) (x ex t) 1.0) (str "t=" t)))))

(t/deftest keyframes-range-and-retime
  (let [a  (uuid/next)
        b  (uuid/next)
        tl (-> (mk-timeline)
               (cta/add-keyframe a {:time 100 :property :x :value 0})
               (cta/add-keyframe a {:time 300 :property :x :value 10})
               (cta/add-keyframe b {:time 500 :property :opacity :value 1}))]
    (t/is (= [100 500] (cta/keyframes-range tl [a b])))
    (t/is (= [100 300] (cta/keyframes-range tl [a])))
    (t/is (nil? (cta/keyframes-range tl [(uuid/next)])))
    (t/testing "moving keeps the spacing"
      (let [tl' (cta/retime-keyframes tl [a b] [100 500] [150 550])]
        (t/is (= [150 350] (mapv :time (get-in tl' [:tracks a :keyframes]))))
        (t/is (= [550] (mapv :time (get-in tl' [:tracks b :keyframes]))))))
    (t/testing "stretching from the start keeps the end"
      (let [tl' (cta/retime-keyframes tl [a b] [100 500] [300 500])]
        (t/is (= [300 400] (mapv :time (get-in tl' [:tracks a :keyframes]))))
        (t/is (= [500] (mapv :time (get-in tl' [:tracks b :keyframes]))))))
    (t/testing "other tracks are untouched and times stay non negative"
      (let [tl' (cta/retime-keyframes tl [a] [100 300] [-50 150])]
        (t/is (= [0 150] (mapv :time (get-in tl' [:tracks a :keyframes]))))
        (t/is (= [500] (mapv :time (get-in tl' [:tracks b :keyframes]))))
        (t/is (cta/valid-timeline? tl'))))))

(defn- hex-close?
  [expected actual]
  (let [[er eg eb] (map int (clr/hex->rgb expected))
        [ar ag ab] (map int (clr/hex->rgb actual))]
    (and (<= (mth/abs (- er ar)) 1)
         (<= (mth/abs (- eg ag)) 1)
         (<= (mth/abs (- eb ab)) 1))))

(t/deftest color-keyframes-mix-in-rgb
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :fill-color :index 0
                                       :value "#000000" :easing :linear})
                (cta/add-keyframe sid {:time 1000 :property :fill-color :index 0
                                       :value "#ffffff"}))]
    (t/is (cta/valid-timeline? tl))
    (t/testing "midpoint is a mid grey"
      (t/is (hex-close? "#7f7f7f" (get-in (cta/values-at tl 500) [sid :fill-color 0]))))
    (t/testing "a step holds the start colour"
      (let [tl (cta/update-keyframe tl sid (:id (first (cta/property-keyframes tl sid :fill-color 0)))
                                    #(assoc % :interpolation :step))]
        (t/is (= "#000000" (get-in (cta/values-at tl 999) [sid :fill-color 0])))))))

(t/deftest same-time-property-and-index-replaces-keyframe
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#000000"})
                (cta/add-keyframe sid {:time 0 :property :fill-color :index 1 :value "#ff0000"})
                (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#00ff00"}))
        kfs (get-in tl [:tracks sid :keyframes])]
    (t/is (= 2 (count kfs)))
    (t/is (= "#00ff00" (:value (d/seek #(= 0 (:index %)) kfs))))
    (t/is (= "#ff0000" (:value (d/seek #(= 1 (:index %)) kfs))))))

(t/deftest gradient-fill-stays-put
  (let [shape (cts/setup-shape
               {:type :rect :x 0 :y 0 :width 100 :height 100
                :fills [{:fill-color "#0000ff" :fill-opacity 1}
                        {:fill-color-gradient {:type :linear
                                               :start-x 0 :start-y 0
                                               :end-x 1 :end-y 1
                                               :width 1
                                               :stops [{:color "#ff0000" :offset 0}
                                                       {:color "#0000ff" :offset 1}]}}]})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#0000ff"})
                  (cta/add-keyframe sid {:time 1000 :property :fill-color :index 0 :value "#ff0000"}))
        tree  (cta/timeline->modif-tree tl {sid shape} 1000)
        shape' (ctm/apply-structure-modifiers shape (get-in tree [sid :modifiers]))
        fills  (vec (:fills shape'))]
    (t/is (hex-close? "#ff0000" (:fill-color (first fills))))
    (t/is (some? (:fill-color-gradient (second fills))))
    (t/is (nil? (:fill-color (second fills))))))

(t/deftest shadow-offset-and-blur-follow-time
  (let [shape (cts/setup-shape
               {:type :rect :x 0 :y 0 :width 100 :height 100
                :shadow [{:id (uuid/next)
                          :style :drop-shadow
                          :offset-x 0 :offset-y 0 :blur 0 :spread 0
                          :hidden false
                          :color {:color "#000000" :opacity 1}}]
                :blur {:id (uuid/next) :type :layer-blur :value 0 :hidden false}})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :shadow-offset-x :index 0 :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :shadow-offset-x :index 0 :value 20})
                  (cta/add-keyframe sid {:time 0 :property :blur :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :blur :value 10}))
        tree  (cta/timeline->modif-tree tl {sid shape} 500)
        shape' (ctm/apply-structure-modifiers shape (get-in tree [sid :modifiers]))]
    (t/is (mth/close? 10.0 (double (get-in shape' [:shadow 0 :offset-x])) 0.001))
    (t/is (mth/close? 5.0 (double (get-in shape' [:blur :value])) 0.001))))

(t/deftest corner-radius-follows-time
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 40
                                :r1 0 :r2 0 :r3 0 :r4 0})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :r1 :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :r1 :value 20}))
        mods  (get-in (cta/timeline->modif-tree tl {sid shape} 1000) [sid :modifiers])
        shape' (ctm/apply-structure-modifiers shape mods)]
    (t/is (mth/close? 20.0 (double (:r1 shape')) 0.001))
    (t/is (= 0 (:r2 shape')))))

(t/deftest modifier-tree-puts-in-the-mixed-fill
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 100
                                :fills [{:fill-color "#000000" :fill-opacity 1}]})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#000000"})
                  (cta/add-keyframe sid {:time 1000 :property :fill-color :index 0 :value "#ffffff"})
                  (cta/add-keyframe sid {:time 0 :property :fill-opacity :index 0 :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :fill-opacity :index 0 :value 0}))
        tree  (cta/timeline->modif-tree tl {sid shape} 500)
        shape' (ctm/apply-structure-modifiers shape (get-in tree [sid :modifiers]))
        fill   (first (vec (:fills shape')))]
    (t/is (contains? tree sid))
    (t/is (hex-close? "#7f7f7f" (:fill-color fill)))
    (t/is (mth/close? 0.5 (double (:fill-opacity fill)) 0.001))))

(t/deftest positions-are-relative-to-the-board
  (let [board (cts/setup-shape {:type :frame :x 100 :y 50 :width 400 :height 300})
        bid   (:id board)
        rect  (cts/setup-shape {:type :rect :x 150 :y 80 :width 20 :height 20})
        sid   (:id rect)
        tl    (-> (cta/make-timeline {:board-id bid})
                  (cta/add-keyframe sid {:time 0 :property :x :value 10})
                  (cta/add-keyframe sid {:time 0 :property :y :value 20}))
        pos   (fn [objects]
                (let [mods (get-in (cta/timeline->modif-tree tl objects 0) [sid :modifiers])]
                  (-> (gsh/transform-shape (get objects sid) mods) :selrect ((juxt :x :y)))))
        close (fn [[ax ay] [bx by]] (and (mth/close? ax bx 0.001) (mth/close? ay by 0.001)))]
    (t/is (= (gpt/point 100 50) (cta/position-origin tl {bid board} sid)))
    (t/is (= (gpt/point 0 0) (cta/position-origin tl {bid board} bid)))
    (t/is (close [110 70] (pos {bid board sid rect})))
    (t/testing "moving the board moves the animation with it"
      (let [moved (update board :selrect assoc :x 300 :y 0)]
        (t/is (close [310 20] (pos {bid moved sid rect})))))))

(t/deftest frame-times-include-start-and-end
  (let [times (cta/frame-times 1000 30)]
    (t/is (= 31 (count times)))
    (t/is (= 0 (first times)))
    (t/is (= 1000 (last times)))
    (t/is (= 33 (second times)))
    (t/is (every? int? times))))

(t/deftest frame-times-zero-duration
  (t/is (= [0] (cta/frame-times 0 30)))
  (t/is (= [0] (cta/frame-times 1000 0))))

(t/deftest video-bitrate-grows-with-quality-and-area
  (let [low    (cta/video-bitrate 1920 1080 :low)
        medium (cta/video-bitrate 1920 1080 :medium)
        high   (cta/video-bitrate 1920 1080 :high)
        small  (cta/video-bitrate 960 540 :medium)]
    (t/is (< low medium))
    (t/is (< medium high))
    (t/is (< small medium))
    (t/is (pos? low))))

(t/deftest gif-colors-per-quality
  (t/is (= 64 (cta/gif-colors :low)))
  (t/is (= 128 (cta/gif-colors :medium)))
  (t/is (= 256 (cta/gif-colors :high)))
  (t/is (= cta/max-export-frames 1800)))

(t/deftest export-layer-ids-includes-siblings
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        a     (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20})
        b     (cts/setup-shape {:type :rect :x 40 :y 10 :width 20 :height 20})
        child (cts/setup-shape {:type :rect :x 12 :y 12 :width 8 :height 8})
        bid   (:id board)
        aid   (:id a)
        bid-b (:id b)
        cid   (:id child)
        objects {bid (assoc board :shapes [aid bid-b])
                 aid (assoc a :parent-id bid :frame-id bid :shapes [cid])
                 bid-b (assoc b :parent-id bid :frame-id bid)
                 cid (assoc child :parent-id aid :frame-id bid)}
        tracks {aid {:shape-id aid :keyframes []}
                cid {:shape-id cid :keyframes []}}
        ids   (cta/export-layer-ids objects bid tracks)]
    (t/is (= [aid cid bid-b] ids))))

(t/deftest timeline->lottie-with-assets-uses-image-layers
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 800 :height 600})
        bid   (:id board)
        shape (cts/setup-shape {:type :rect :x 100 :y 100 :width 200 :height 100})
        sid   (:id shape)
        bx    (-> shape :selrect :x)
        tl    (-> (cta/make-timeline {:board-id bid :name "Anim" :duration 1000})
                  (cta/add-keyframe sid {:time 0 :property :x :value bx})
                  (cta/add-keyframe sid {:time 1000 :property :x :value (+ bx 300)})
                  (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :opacity :value 0}))
        objects {bid (assoc board :shapes [sid])
                 sid (assoc shape :parent-id bid :frame-id bid)}
        assets {sid {:id "img_0" :w 200 :h 100 :p "data:image/png;base64,AAA" :e 1}}
        L     (cta/timeline->lottie tl objects assets)
        layer (first (:layers L))]
    (t/is (= 2 (:ty layer)))
    (t/is (= "img_0" (:refId layer)))
    (t/is (= 1 (get-in layer [:ks :p :x :a])))
    (t/is (= 1 (get-in layer [:ks :o :a])))
    (t/is (= "img_0" (get-in L [:assets 0 :id])))))

(t/deftest timeline->svg-wraps-images-and-css
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100})
        bid   (:id board)
        shape (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20})
        sid   (:id shape)
        bx    (-> shape :selrect :x)
        tl    (-> (cta/make-timeline {:board-id bid :duration 1000 :playback :loop})
                  (cta/add-keyframe sid {:time 0 :property :x :value bx})
                  (cta/add-keyframe sid {:time 1000 :property :x :value (+ bx 40)}))
        objects {bid (assoc board :shapes [sid])
                 sid (assoc shape :parent-id bid :frame-id bid)}
        images {sid {:href "data:image/png;base64,AAA" :x 10 :y 10 :width 20 :height 20}}
        svg   (cta/timeline->svg tl objects images)]
    (t/is (re-find #"<svg" svg))
    (t/is (re-find #"@keyframes" svg))
    (t/is (re-find (re-pattern (str "class=\"" (cta/shape-css-class sid) "\"")) svg))
    (t/is (re-find #"translate\(40px, 0px\)" svg))
    (t/is (re-find #"linear infinite;" svg))
    (t/is (re-find #"overflow=\"hidden\"" svg))))

(t/deftest timeline->svg-nests-child-layers
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100})
        a     (cts/setup-shape {:type :rect :x 10 :y 10 :width 40 :height 40})
        c     (cts/setup-shape {:type :rect :x 12 :y 12 :width 8 :height 8})
        bid   (:id board)
        aid   (:id a)
        cid   (:id c)
        tl    (-> (cta/make-timeline {:board-id bid :duration 500})
                  (cta/add-keyframe aid {:time 0 :property :x :value 10})
                  (cta/add-keyframe cid {:time 0 :property :x :value 12}))
        objects {bid (assoc board :shapes [aid])
                 aid (assoc a :parent-id bid :frame-id bid :shapes [cid])
                 cid (assoc c :parent-id aid :frame-id bid)}
        images {aid {:href "data:image/png;base64,AAA" :x 10 :y 10 :width 40 :height 40}
                cid {:href "data:image/png;base64,BBB" :x 12 :y 12 :width 8 :height 8}}
        svg   (cta/timeline->svg tl objects images)
        parent-class (cta/shape-css-class aid)
        child-class  (cta/shape-css-class cid)]
    (t/is (re-find (re-pattern (str parent-class "\".*" child-class)) svg))
    (t/is (re-find #"x=\"2\" y=\"2\"" svg))))

(t/deftest track-origin-defaults-to-center
  (t/is (= {:x 0.5 :y 0.5} (cta/track-origin nil)))
  (t/is (= {:x 0.5 :y 0.5} (cta/track-origin {:keyframes []})))
  (t/is (= {:x 0.0 :y 1.0} (cta/track-origin {:origin {:x 0 :y 1}}))))

(t/deftest set-track-origin-clamps-and-drops-default
  (let [sid (uuid/next)
        tl  (cta/set-track-origin (mk-timeline) sid -0.2 1.4)]
    (t/is (= {:x 0.0 :y 1.0} (cta/track-origin (cta/get-track tl sid))))
    (t/is (cta/valid-timeline? tl))
    (let [tl2 (cta/set-track-origin tl sid 0.5 0.5)]
      (t/is (nil? (:origin (cta/get-track tl2 sid))))
      (t/is (= {:x 0.5 :y 0.5} (cta/track-origin (cta/get-track tl2 sid)))))))

(t/deftest scale-from-top-left-keeps-position
  (let [shape (cts/setup-shape {:type :rect :x 10 :y 20 :width 100 :height 40})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/set-track-origin sid 0 0)
                  (cta/add-keyframe sid {:time 0 :property :scale-x :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :scale-x :value 2}))
        mods  (get-in (cta/timeline->modif-tree tl {sid shape} 1000) [sid :modifiers])
        shape' (gsh/transform-shape shape mods)]
    (t/is (mth/close? 10.0 (double (-> shape' :selrect :x)) 0.01))
    (t/is (mth/close? 200.0 (double (-> shape' :selrect :width)) 0.01))))

(t/deftest scale-from-center-shifts-position
  (let [shape (cts/setup-shape {:type :rect :x 10 :y 20 :width 100 :height 40})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :scale-x :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :scale-x :value 2}))
        mods  (get-in (cta/timeline->modif-tree tl {sid shape} 1000) [sid :modifiers])
        shape' (gsh/transform-shape shape mods)]
    (t/is (mth/close? -40.0 (double (-> shape' :selrect :x)) 0.01))
    (t/is (mth/close? 200.0 (double (-> shape' :selrect :width)) 0.01))))

(t/deftest timeline->css-writes-transform-origin
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 40 :height 40})
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                  (cta/set-track-origin sid 0 1)
                  (cta/add-keyframe sid {:time 0 :property :scale-x :value 1})
                  (cta/add-keyframe sid {:time 1000 :property :scale-x :value 2}))
        css   (cta/timeline->css tl {sid shape})]
    (t/is (re-find #"transform-origin: 0% 100%;" css))))

(t/deftest timeline->lottie-anchor-follows-origin
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 800 :height 600})
        bid   (:id board)
        shape (cts/setup-shape {:type :rect :x 100 :y 100 :width 200 :height 100})
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id bid :name "Anim" :duration 1000})
                  (cta/set-track-origin sid 0 0)
                  (cta/add-keyframe sid {:time 0 :property :x :value 100}))
        L     (cta/timeline->lottie tl {bid board sid shape})
        ks    (:ks (first (:layers L)))]
    (t/is (= [100.0 100.0 0] (get-in ks [:a :k])))))

;; PRESET ANIMATIONS

(defn- close?
  [expected actual]
  (mth/close? (double expected) (double actual) 0.001))

(defn- board-and-rect
  "A board at 100,100 with a rect at 50,100 inside it."
  []
  (let [board (cts/setup-shape {:type :frame :x 100 :y 100 :width 800 :height 600})
        rect  (cts/setup-shape {:type :rect :x 150 :y 200 :width 100 :height 50})
        bid   (:id board)]
    {:board board
     :rect rect
     :objects {bid board
               (:id rect) (assoc rect :frame-id bid :parent-id bid)}}))

(defn- values-at
  [timeline objects time]
  (cta/values-at (cta/resolve-animations timeline objects) time))

(t/deftest make-animation-takes-type-defaults
  (let [animation (cta/make-animation {:type :move :start -5 :duration 0 :easing nil})]
    (t/is (= :in (:direction animation)))
    (t/is (= [0 1] [(:start animation) (:duration animation)]))
    (t/is (= [0 40] [(:offset-x animation) (:offset-y animation)]))
    (t/is (not (contains? animation :easing)))
    (t/is (uuid? (:id animation)))))

(t/deftest add-animation-grows-duration
  (let [sid (uuid/next)
        tl  (cta/add-animation (mk-timeline) sid {:type :fade :start 900 :duration 400})]
    (t/is (= 1300 (:duration tl)))
    (t/is (cta/valid-timeline? tl))
    (t/is (= [900 1300] (cta/keyframes-range tl [sid])))))

(t/deftest animations-keep-their-track
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-animation sid {:type :fade}))
        aid (:id (first (get-in tl [:tracks sid :animations])))
        kid (:id (first (get-in tl [:tracks sid :keyframes])))]
    (t/testing "without keyframes"
      (t/is (some? (cta/get-track (cta/remove-keyframe tl sid kid) sid)))
      (t/is (some? (cta/get-track (cta/remove-property tl sid :x) sid))))
    (t/testing "removing the animation keeps the keyframes"
      (let [tl2 (cta/remove-animation tl sid aid)]
        (t/is (= 1 (count (get-in tl2 [:tracks sid :keyframes]))))
        (t/is (not (contains? (cta/get-track tl2 sid) :animations)))))
    (t/testing "removing both drops the track"
      (t/is (nil? (-> tl
                      (cta/remove-animation sid aid)
                      (cta/remove-keyframe sid kid)
                      (cta/get-track sid)))))))

(t/deftest update-animation-keeps-it-valid
  (let [sid (uuid/next)
        tl  (cta/add-animation (mk-timeline) sid {:type :fade})
        aid (:id (first (get-in tl [:tracks sid :animations])))
        tl2 (cta/update-animation tl sid aid #(assoc % :start 800 :duration 600 :direction :out))
        animation (cta/get-animation tl2 sid aid)]
    (t/is (= [800 600 :out] [(:start animation) (:duration animation) (:direction animation)]))
    (t/is (= 1400 (:duration tl2)))
    (t/is (cta/valid-timeline? tl2))))

(t/deftest fade-in-hides-until-it-starts
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                (cta/add-animation sid {:type :fade :start 200 :duration 400}))
        at  #(get-in (values-at tl objects %) [sid :opacity])]
    (t/is (not (contains? (cta/get-track (cta/resolve-animations tl objects) sid) :animations)))
    (t/is (close? 0 (at 0)))
    (t/is (close? 0 (at 200)))
    (t/is (< 0 (at 400) 1))
    (t/is (close? 1 (at 600)))
    (t/is (close? 1 (at 1000)))))

(t/deftest fade-out-ends-hidden
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                (cta/add-animation sid {:type :fade :direction :out :start 600 :duration 400}))
        at  #(get-in (values-at tl objects %) [sid :opacity])]
    (t/is (close? 1 (at 0)))
    (t/is (close? 1 (at 600)))
    (t/is (close? 0 (at 1000)))))

(t/deftest move-in-starts-offset-from-its-place
  (let [{:keys [board rect objects]} (board-and-rect)
        sid   (:id rect)
        tl    (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                  (cta/add-animation sid {:type :move :offset-x -30 :offset-y 40 :duration 500}))
        start (get (values-at tl objects 0) sid)
        end   (get (values-at tl objects 500) sid)]
    (t/is (close? 20 (:x start)))
    (t/is (close? 140 (:y start)))
    (t/is (close? 50 (:x end)))
    (t/is (close? 100 (:y end)))))

(t/deftest animation-takes-over-keyframes-in-its-span
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                (cta/add-keyframe sid {:time 0 :property :opacity :value 0.5})
                (cta/add-keyframe sid {:time 300 :property :opacity :value 0.2})
                (cta/add-keyframe sid {:time 1000 :property :opacity :value 0.8})
                (cta/add-animation sid {:type :fade :start 200 :duration 400}))
        kfs (->> (get-in (cta/resolve-animations tl objects) [:tracks sid :keyframes])
                 (filterv #(= :opacity (:property %))))]
    (t/is (= [0 200 600 1000] (mapv :time kfs)))
    (t/is (close? 0 (:value (nth kfs 1))))
    ;; the value the keyframes give where the fade ends
    (t/is (close? (+ 0.2 (* 0.6 (/ 300 700))) (:value (nth kfs 2))))))

(t/deftest retime-moves-animations
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-animation sid {:type :fade :start 100 :duration 200}))
        tl2 (cta/retime-keyframes tl [sid] [0 300] [500 800])
        animation (first (get-in tl2 [:tracks sid :animations]))]
    (t/is (= [600 200] [(:start animation) (:duration animation)]))))

(t/deftest animations-reach-the-preview-and-the-exports
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                (cta/add-animation sid {:type :fade :duration 400}))]
    (t/is (some? (get-in (cta/timeline->modif-tree tl objects 0) [sid :modifiers])))
    (t/is (re-find #"opacity" (cta/timeline->css tl objects)))))

(t/deftest modifiers-give-the-shape-at-a-time
  ;; The design tab shows the shape at the playhead this way.
  (let [{:keys [board rect objects]} (board-and-rect)
        sid    (:id rect)
        tl     (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                   (cta/add-keyframe sid {:time 0 :property :rotation :value 0})
                   (cta/add-keyframe sid {:time 1000 :property :rotation :value 90})
                   (cta/add-keyframe sid {:time 0 :property :x :value 50})
                   (cta/add-keyframe sid {:time 1000 :property :x :value 150})
                   (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                   (cta/add-keyframe sid {:time 1000 :property :opacity :value 0.5}))
        at     (fn [time]
                 (gsh/transform-shape rect (get-in (cta/timeline->modif-tree tl objects time)
                                                   [sid :modifiers])))
        rotated (assoc rect :rotation 30)
        shape0  (gsh/transform-shape rotated (get-in (cta/timeline->modif-tree tl (assoc objects sid rotated) 0)
                                                     [sid :modifiers]))]
    (t/is (close? 45 (:rotation (at 500))))
    (t/is (close? 0.75 (:opacity (at 500))))
    (t/is (close? 0 (:rotation shape0)))
    ;; its box is where the keyframes put it, and it turns around its own
    ;; center (not around the place it moved from)
    (t/is (close? 150 (-> (at 0) :selrect :x)))
    (t/is (close? 250 (-> (at 1000) :selrect :x)))
    (let [bounds (-> (at 1000) :points grc/points->rect)]
      (t/is (close? 300 (+ (:x bounds) (/ (:width bounds) 2))))
      (t/is (close? 50 (:width bounds))))))

(t/deftest animations-keep-their-order
  (let [sid  (uuid/next)
        tl   (-> (mk-timeline)
                 (cta/add-animation sid {:type :fade :start 0})
                 (cta/add-animation sid {:type :scale :direction :out :start 600}))
        fade (first (get-in tl [:tracks sid :animations]))
        tl2  (cta/update-animation tl sid (:id fade) #(assoc % :start 800))]
    (t/is (= [:fade :scale] (mapv :type (get-in tl2 [:tracks sid :animations]))))
    (t/is (= 800 (:start (cta/get-animation tl2 sid (:id fade)))))))

;; HIDDEN AND LOCKED ROWS

(t/deftest hidden-rows-are-left-out
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                (cta/add-keyframe sid {:time 0 :property :rotation :value 0})
                (cta/add-keyframe sid {:time 1000 :property :rotation :value 90})
                (cta/add-animation sid {:type :fade :duration 400}))
        aid (:id (first (get-in tl [:tracks sid :animations])))
        at  #(get (values-at %1 objects %2) sid)]
    (t/is (re-find #"rotate\(90deg\)" (cta/timeline->css tl objects)))

    (t/testing "a hidden property"
      (let [tl2 (cta/toggle-slot-flag tl sid :hidden :rotation)]
        (t/is (cta/slot-flag? tl2 sid :hidden :rotation))
        (t/is (cta/valid-timeline? tl2))
        (t/is (not (contains? (at tl2 500) :rotation)))
        (t/is (close? 0 (:opacity (at tl2 0))))
        ;; still there to show and edit
        (t/is (= 2 (count (cta/property-keyframes tl2 sid :rotation))))
        (t/is (not (re-find #"rotate\(90deg\)" (cta/timeline->css tl2 objects))))))

    (t/testing "a hidden animation"
      (let [tl2 (cta/toggle-animation-flag tl sid aid :hidden)]
        (t/is (:hidden (cta/get-animation tl2 sid aid)))
        (t/is (cta/valid-timeline? tl2))
        (t/is (not (contains? (at tl2 0) :opacity)))
        (t/is (close? 45 (:rotation (at tl2 500))))
        (t/is (:hidden (-> tl2
                           (cta/update-animation sid aid #(assoc % :start 100))
                           (cta/get-animation sid aid))))))

    (t/testing "nothing left to play once all of it is hidden"
      (let [tl2 (-> tl
                    (cta/toggle-slot-flag sid :hidden :rotation)
                    (cta/toggle-animation-flag sid aid :hidden))]
        (t/is (nil? (cta/get-track (cta/resolve-animations tl2 objects) sid)))
        (t/is (empty? (cta/timeline->modif-tree tl2 objects 500)))))

    (t/testing "shown again"
      (t/is (= tl (-> tl
                      (cta/toggle-slot-flag sid :hidden :rotation)
                      (cta/toggle-slot-flag sid :hidden :rotation))))
      (t/is (= tl (-> tl
                      (cta/toggle-animation-flag sid aid :hidden)
                      (cta/toggle-animation-flag sid aid :hidden)))))))

(t/deftest hidden-fill-slot
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#000000"})
                (cta/add-keyframe sid {:time 0 :property :fill-color :index 1 :value "#ffffff"})
                (cta/toggle-slot-flag sid :hidden :fill-color 1))
        values (get (cta/values-at (cta/resolve-animations tl {}) 0) sid)]
    (t/is (cta/valid-timeline? tl))
    (t/is (cta/slot-flag? tl sid :hidden :fill-color 1))
    (t/is (not (cta/slot-flag? tl sid :hidden :fill-color 0)))
    (t/is (= {0 "#000000"} (:fill-color values)))))

(t/deftest locked-rows-are-kept-from-edits
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-keyframe sid {:time 500 :property :x :value 100})
                (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                (cta/add-animation sid {:type :fade :start 600 :duration 200}))
        aid (:id (first (get-in tl [:tracks sid :animations])))
        kid (:id (first (cta/property-keyframes tl sid :x)))
        tl  (-> tl
                (cta/toggle-slot-flag sid :locked :x)
                (cta/toggle-animation-flag sid aid :locked))]
    (t/is (cta/valid-timeline? tl))

    (t/testing "its keyframes"
      (t/is (= tl (cta/add-keyframe tl sid {:time 200 :property :x :value 5})))
      (t/is (= tl (cta/update-keyframe tl sid kid #(assoc % :time 100))))
      (t/is (= tl (cta/remove-keyframe tl sid kid)))
      (t/is (= tl (cta/remove-property tl sid :x)))
      (t/is (= tl (cta/toggle-loop tl sid :x))))

    (t/testing "its animation"
      (t/is (= tl (cta/update-animation tl sid aid #(assoc % :start 0))))
      (t/is (= tl (cta/remove-animation tl sid aid))))

    (t/testing "moving the layer leaves them in place"
      (let [tl2 (cta/retime-keyframes tl [sid] [0 800] [100 900])]
        (t/is (= [0 500] (mapv :time (cta/property-keyframes tl2 sid :x))))
        (t/is (= [100] (mapv :time (cta/property-keyframes tl2 sid :opacity))))
        (t/is (= 600 (:start (cta/get-animation tl2 sid aid))))))

    (t/testing "deleting the keyframes of the layer keeps them"
      (let [tl2 (cta/remove-track tl sid)]
        (t/is (= [:x :x] (mapv :property (get-in tl2 [:tracks sid :keyframes]))))
        (t/is (= [aid] (mapv :id (get-in tl2 [:tracks sid :animations]))))
        (t/is (cta/slot-flag? tl2 sid :locked :x))
        (t/is (cta/valid-timeline? tl2))))

    (t/testing "pasting skips them"
      (let [kfs (into (cta/property-keyframes tl sid :x)
                      (cta/property-keyframes tl sid :opacity))
            tl2 (cta/paste-keyframes tl sid kfs #{:x} 300)]
        (t/is (= [0 500] (mapv :time (cta/property-keyframes tl2 sid :x))))
        (t/is (= [0 300] (mapv :time (cta/property-keyframes tl2 sid :opacity))))
        (t/is (not (cta/looping? tl2 sid :x)))))

    (t/testing "unlocked, they can be edited again"
      (let [tl2 (-> tl
                    (cta/toggle-slot-flag sid :locked :x)
                    (cta/toggle-animation-flag sid aid :locked))]
        (t/is (empty? (cta/property-keyframes (cta/remove-property tl2 sid :x) sid :x)))
        (t/is (nil? (cta/get-animation (cta/remove-animation tl2 sid aid) sid aid)))))))

(t/deftest keyframes-added-between-keep-the-animation
  (let [sid    (uuid/next)
        tl     (-> (mk-timeline)
                   (cta/add-keyframe sid {:time 0 :property :x :value 0})
                   (cta/add-keyframe sid {:time 1000 :property :x :value 100})
                   (cta/add-keyframe sid {:time 0 :property :y :value 0 :interpolation :step})
                   (cta/add-keyframe sid {:time 1000 :property :y :value 100}))
        values #(get-in (cta/values-at %1 %2) [sid %3])
        add    (fn [tl property time]
                 (cta/add-keyframe tl sid (merge {:time time
                                                  :property property
                                                  :value (values tl time property)}
                                                 (cta/split-timing tl sid property nil time))))]
    (t/is (= {:easing :ease} (cta/split-timing tl sid :x nil 0)))
    (t/testing "a linear segment stays linear"
      (let [tl2 (add tl :x 500)]
        (t/is (= [0 500 1000] (mapv :time (cta/property-keyframes tl2 sid :x))))
        (doseq [t [250 500 750]]
          (t/is (close? (values tl t :x) (values tl2 t :x)) (str "t=" t)))))
    (t/testing "a hold stays a hold"
      (let [tl2 (add tl :y 500)]
        (t/is (= :step (:interpolation (second (cta/property-keyframes tl2 sid :y)))))
        (doseq [t [250 750 999]]
          (t/is (= 0 (values tl2 t :y)) (str "t=" t)))))))

(t/deftest removing-the-last-keyframe-drops-the-flags
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0})
                (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
                (cta/toggle-loop sid :x)
                (cta/toggle-slot-flag sid :hidden :x))
        kid (:id (first (cta/property-keyframes tl sid :x)))
        tl2 (cta/remove-keyframe tl sid kid)]
    (t/is (not (cta/looping? tl2 sid :x)))
    (t/is (nil? (get-in tl2 [:tracks sid :hidden])))
    ;; so a new keyframe starts a new row
    (t/is (not (-> tl2
                   (cta/add-keyframe sid {:time 0 :property :x :value 5})
                   (cta/slot-flag? sid :hidden :x))))))

;; EASING: SPRINGS AND HOLDS

(defn- spring-preset
  [id]
  (:easing (d/seek #(= id (:id %)) cta/spring-presets)))

(defn- progress-samples
  [easing]
  (mapv #(cta/easing-progress easing (/ % 100)) (range 101)))

(t/deftest springs-start-and-settle
  (doseq [{:keys [id easing]} cta/spring-presets]
    (t/is (close? 0 (cta/easing-progress easing 0)) (name id))
    (t/is (close? 1 (cta/easing-progress easing 1)) (name id))
    (t/is (< (mth/abs (- 1 (cta/easing-progress easing 0.95))) 0.02) (name id))))

(t/deftest bouncy-overshoots-and-slow-does-not
  (let [bouncy (progress-samples (spring-preset :bouncy))
        slow   (progress-samples (spring-preset :slow))]
    (t/is (> (reduce max bouncy) 1.1))
    (t/is (<= (reduce max slow) 1.0001))
    (t/is (apply <= slow))))

(t/deftest spring-easing-validates
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 0 :property :x :value 0 :easing (spring-preset :gentle)})
                (cta/add-keyframe sid {:time 1000 :property :x :value 100}))]
    (t/is (cta/valid-timeline? tl))
    (t/is (not (cta/valid-timeline?
                (assoc-in tl [:tracks sid :keyframes 0 :easing] {:type :wobble}))))
    (t/testing "the spring drives the value between the keyframes"
      (t/is (> (get-in (cta/values-at tl 300) [sid :x]) 30)))))

(t/deftest back-curves-leave-the-unit-range
  (let [curve (fn [id] (:easing (d/seek #(= id (:id %)) cta/bezier-presets)))]
    (t/is (neg? (reduce min (progress-samples (curve :ease-in-back)))))
    (t/is (> (reduce max (progress-samples (curve :ease-out-back))) 1))))

(t/deftest css-writes-springs-and-holds
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 40 :height 40})
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                  (cta/add-keyframe sid {:time 0 :property :x :value 0 :easing (spring-preset :quick)})
                  (cta/add-keyframe sid {:time 500 :property :x :value 50 :interpolation :step})
                  (cta/add-keyframe sid {:time 1000 :property :x :value 100}))
        css   (cta/timeline->css tl {sid shape})]
    (t/is (re-find #"animation-timing-function: linear\(0 0%, " css))
    (t/is (re-find #"animation-timing-function: steps\(1, end\)" css))))

(t/deftest lottie-unrolls-springs
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 800 :height 600})
        bid   (:id board)
        shape (cts/setup-shape {:type :rect :x 100 :y 100 :width 200 :height 100})
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id bid :duration 1000})
                  (cta/add-keyframe sid {:time 0 :property :opacity :value 0 :easing (spring-preset :bouncy)})
                  (cta/add-keyframe sid {:time 1000 :property :opacity :value 1}))
        kfs   (get-in (cta/expand-springs tl) [:tracks sid :keyframes])]
    (t/is (> (count kfs) 2))
    (t/is (not-any? #(= :spring (:type (:easing %))) kfs))
    (t/is (= [0 1000] [(:time (first kfs)) (:time (last kfs))]))
    (t/is (map? (cta/timeline->lottie tl {bid board sid shape})))))

(t/deftest spring-bounce-round-trips
  (let [gentle (spring-preset :gentle)
        figma  (cta/spring-with-bounce gentle 0.75)]
    (t/is (close? 0.25 (cta/spring-bounce gentle)))
    (t/is (close? 0.75 (cta/spring-bounce figma)))
    (t/testing "a bounce of 0.75 overshoots about 44% early on, like Figma's"
      (let [[t peak] (cta/spring-peak figma)]
        (t/is (< 0.1 t 0.13))
        (t/is (close? 1.444 peak))
        (t/is (close? peak (cta/easing-progress figma t)))))
    (t/testing "dragging the overshoot sideways finds the bounce back"
      (t/is (close? 0.75 (cta/bounce-for-peak (first (cta/spring-peak figma)))))
      (t/is (close? 0.97 (cta/bounce-for-peak 0)))
      (t/is (close? 0 (cta/bounce-for-peak 1)))
      (t/is (> (cta/bounce-for-peak 0.1) (cta/bounce-for-peak 0.5))))
    (t/is (nil? (cta/spring-peak (spring-preset :slow))))))

(t/deftest springs-bounce-up-to-figmas-maximum
  (let [spring (cta/spring-with-bounce (spring-preset :gentle) 1.0)]
    (t/is (close? 0.97 (cta/spring-bounce spring)))
    (t/testing "it swings many times, so it is followed with many points"
      (t/is (> (cta/spring-samples spring 40) 400)))
    (t/is (close? 1 (cta/easing-progress spring 1)))
    (t/is (< (mth/abs (- 1 (cta/easing-progress spring 0.99))) 0.01))))

(t/deftest timeline-survives-json
  ;; Files are exported and imported as JSON (binfile v3).
  (let [sid      (uuid/next)
        spring   {:type :spring :stiffness 300 :damping 20 :mass 1}
        timeline (-> (cta/make-timeline {:board-id (uuid/next) :playback :ping-pong})
                     (cta/add-keyframe sid {:time 0 :property :x :value 0 :easing spring})
                     (cta/add-keyframe sid {:time 500 :property :x :value 10
                                            :easing {:type :bezier :curve [0.1 0.2 0.3 1.4]}})
                     (cta/add-keyframe sid {:time 0 :property :fill-color :index 0 :value "#ff0000"
                                            :interpolation :step})
                     (cta/add-keyframe sid {:time 0 :property :opacity :value 1 :easing :ease-in})
                     (cta/add-animation sid {:type :move :offset-x 10 :easing cta/overshoot-easing})
                     (cta/toggle-loop sid :x)
                     (cta/toggle-slot-flag sid :hidden :fill-color 0)
                     (cta/toggle-slot-flag sid :locked :opacity)
                     (cta/set-track-origin sid 0 1))
        encode   (sm/encoder cta/schema:timeline (sm/json-transformer))
        decode   (sm/decoder cta/schema:timeline (sm/json-transformer))]
    (t/is (cta/valid-timeline? timeline))
    (t/is (= timeline (-> timeline encode json-roundtrip decode)))))

(defn- arc-timeline
  "A shape moving 100 to the right in a second, arcing 30 up on the way."
  [sid]
  (-> (mk-timeline)
      (cta/add-keyframe sid {:time 0 :property :x :value 0 :easing :linear})
      (cta/add-keyframe sid {:time 1000 :property :x :value 100})
      (cta/add-keyframe sid {:time 0 :property :y :value 0 :easing :linear :path-out -40})
      (cta/add-keyframe sid {:time 1000 :property :y :value 0 :path-in -40})))

(t/deftest motion-path-curves-the-position
  (let [sid (uuid/next)
        tl  (arc-timeline sid)
        at  #(get (cta/values-at tl %) sid)]
    (t/is (cta/valid-timeline? tl))
    (t/is (close? 50 (:x (at 500))))
    (t/is (close? -30 (:y (at 500))))
    (t/is (close? 0 (:y (at 0))))
    (t/is (close? 0 (:y (at 1000))))
    (t/testing "a hold does not follow the path"
      (let [tl (cta/update-keyframe tl sid (:id (first (cta/property-keyframes tl sid :y)))
                                    #(assoc % :interpolation :step))]
        (t/is (close? 0 (:y (get (cta/values-at tl 500) sid))))))))

(t/deftest motion-path-exports-as-steps
  (let [sid      (uuid/next)
        tl       (arc-timeline sid)
        expanded (cta/expand-paths tl)
        ys       (cta/property-keyframes expanded sid :y)]
    (t/is (< 20 (count ys)))
    (t/is (not-any? #(or (:path-in %) (:path-out %)) ys))
    (t/is (cta/valid-timeline? expanded))
    (doseq [t [100 250 500 750 900]]
      (t/is (mth/close? (double (get-in (cta/values-at tl t) [sid :y]))
                        (double (get-in (cta/values-at expanded t) [sid :y]))
                        1.0)
            (str "t=" t)))
    (t/testing "straight segments stay as they are"
      (t/is (= 2 (count (cta/property-keyframes expanded sid :x)))))))

(t/deftest path-trim-reaches-the-shape
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 100})
        sid   (:id shape)
        tl    (-> (mk-timeline)
                  (cta/add-keyframe sid {:time 0 :property :trim-end :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :trim-end :value 1}))
        at    #(gsh/transform-shape shape (get-in (cta/timeline->modif-tree tl {sid shape} %)
                                                  [sid :modifiers]))]
    (t/is (cta/valid-timeline? tl))
    (t/is (close? 0.5 (:trim-end (at 500))))
    ;; the others at their rest, so the whole trim is known
    (t/is (= 0 (:trim-start (at 500))))
    (t/is (= 0 (:trim-offset (at 500))))))

(t/deftest appearance-changes-leave-geometry-and-layout-out
  (let [shape     (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 100})
        sid       (:id shape)
        tl        (-> (mk-timeline)
                      (cta/add-keyframe sid {:time 0 :property :trim-end :value 0})
                      (cta/add-keyframe sid {:time 1000 :property :trim-end :value 1})
                      (cta/add-keyframe sid {:time 0 :property :rotation :value 0})
                      (cta/add-keyframe sid {:time 1000 :property :rotation :value 90}))
        ;; with an edit of the layout at the same time
        modifiers (-> (get-in (cta/timeline->modif-tree tl {sid shape} 500) [sid :modifiers])
                      (ctm/change-property :layout-item-h-sizing :fix))
        changes   (cta/appearance-changes modifiers)
        objects   (cta/apply-appearance-modifiers {sid shape} {sid {:modifiers modifiers}})]
    (t/is (= #{:trim-start :trim-end :trim-offset} (set (keys changes))))
    (t/is (close? 0.5 (:trim-end changes)))
    (t/is (close? 0.5 (get-in objects [sid :trim-end])))
    ;; the rotation takes the transform path
    (t/is (= (:rotation shape) (get-in objects [sid :rotation])))
    (t/is (nil? (get-in objects [sid :layout-item-h-sizing])))
    (t/is (nil? (cta/appearance-changes (ctm/move (ctm/empty) (gpt/point 10 0)))))))

(t/deftest playback-mode-defaults-to-once
  (t/is (= :once (cta/playback-mode (mk-timeline)))))

(t/deftest playback-mode-reads-timelines-saved-with-loop
  (t/is (= :loop (cta/playback-mode (assoc (mk-timeline) :loop true))))
  (t/is (= :once (cta/playback-mode (assoc (mk-timeline) :loop false)))))

(t/deftest playback-mode-is-stored-on-the-timeline
  (let [tl (cta/make-timeline {:board-id (uuid/next) :playback :ping-pong})]
    (t/is (= :ping-pong (cta/playback-mode tl)))
    (t/is (cta/valid-timeline? tl))))

(t/deftest playback-time-stops-at-the-end-when-played-once
  (let [tl (mk-timeline)]
    (t/is (= 400 (cta/playback-time tl 400)))
    (t/is (= 1000 (cta/playback-time tl 1300)))
    (t/is (not (cta/playback-ended? tl 1000)))
    (t/is (cta/playback-ended? tl 1001))))

(t/deftest playback-time-wraps-when-looping
  (let [tl (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback :loop})]
    (t/is (= 400 (cta/playback-time tl 400)))
    (t/is (= 300 (cta/playback-time tl 1300)))
    (t/is (not (cta/playback-ended? tl 5000)))))

(t/deftest playback-time-goes-back-and-forth-in-ping-pong
  (let [tl (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback :ping-pong})]
    (t/is (= 400 (cta/playback-time tl 400)))
    (t/is (= 1000 (cta/playback-time tl 1000)))
    (t/is (= 700 (cta/playback-time tl 1300)))
    (t/is (= 0 (cta/playback-time tl 2000)))
    (t/is (= 400 (cta/playback-time tl 2400)))
    (t/is (not (cta/playback-ended? tl 5000)))))

(t/deftest advance-playback-follows-the-timeline-as-it-is
  (let [tl   (mk-timeline)
        step #(cta/advance-playback %1 %2 %3 16)]
    (t/testing "played once, it stops at the end"
      (t/is (= {:time 416 :direction 1} (step tl 400 1)))
      (t/is (= {:time 1000 :direction 1 :ended? true} (step tl 990 1))))
    (t/testing "a longer duration set while playing plays on"
      (t/is (= {:time 1006 :direction 1} (step (assoc tl :duration 2000) 990 1))))
    (t/testing "looping, it wraps at the duration it has now"
      (t/is (= {:time 6 :direction 1} (step (assoc tl :playback :loop) 990 1)))
      (t/is (= {:time 206 :direction 1} (step (assoc tl :playback :loop :duration 500) 690 1))))
    (t/testing "in ping-pong, it turns at both ends"
      (let [tl (assoc tl :playback :ping-pong)]
        (t/is (= {:time 994 :direction -1} (step tl 990 1)))
        (t/is (= {:time 484 :direction -1} (step tl 500 -1)))
        (t/is (= {:time 6 :direction 1} (step tl 10 -1)))))
    (t/testing "going back when ping-pong is turned off, it plays forward"
      (t/is (= {:time 516 :direction 1} (step (assoc tl :playback :loop) 500 -1))))))

(t/deftest cycle-duration-doubles-in-ping-pong
  (t/is (= 1000 (cta/cycle-duration (mk-timeline))))
  (t/is (= 1000 (cta/cycle-duration (assoc (mk-timeline) :playback :loop))))
  (t/is (= 2000 (cta/cycle-duration (assoc (mk-timeline) :playback :ping-pong)))))

(defn- fade-timeline
  [shape playback]
  (let [sid (:id shape)]
    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback playback})
        (cta/add-keyframe sid {:time 0 :property :opacity :value 1})
        (cta/add-keyframe sid {:time 1000 :property :opacity :value 0}))))

(t/deftest timeline->css-plays-once
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 10 :height 10})
        css   (cta/timeline->css (fade-timeline shape :once) {(:id shape) shape})]
    (t/is (re-find #"linear 1;" css))))

(t/deftest timeline->css-alternates-in-ping-pong
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 10 :height 10})
        css   (cta/timeline->css (fade-timeline shape :ping-pong) {(:id shape) shape})]
    (t/is (re-find #"linear infinite alternate;" css))))

(t/deftest expand-ping-pong-plays-the-timeline-backwards-after-it
  (let [sid (uuid/next)
        tl  (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback :ping-pong})
                (cta/add-keyframe sid {:time 200 :property :x :value 0 :easing :ease-in})
                (cta/add-keyframe sid {:time 700 :property :x :value 100
                                       :easing {:type :bezier :curve [0.34 1.56 0.64 1.0]}})
                (cta/add-keyframe sid {:time 900 :property :x :value 50}))
        ex  (cta/expand-ping-pong tl)
        x   (fn [timeline t] (get-in (cta/values-at timeline t) [sid :x]))]
    (t/is (= 2000 (:duration ex)))
    (t/is (cta/valid-timeline? ex))
    (t/testing "the first half plays as before"
      (doseq [t [0 100 200 350 500 700 800 900 1000]]
        (t/is (close? (x tl t) (x ex t)) (str "at " t))))
    (t/testing "the second half plays it backwards, easings mirrored"
      (doseq [t [0 100 200 350 500 700 800 900 1000]]
        (t/is (close? (x tl t) (x ex (- 2000 t))) (str "mirrored at " t))))))

(t/deftest expand-ping-pong-mirrors-a-step
  (let [sid (uuid/next)
        tl  (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000 :playback :ping-pong})
                (cta/add-keyframe sid {:time 0 :property :opacity :value 1 :interpolation :step})
                (cta/add-keyframe sid {:time 500 :property :opacity :value 0}))
        ex  (cta/expand-ping-pong tl)
        op  (fn [t] (get-in (cta/values-at ex t) [sid :opacity]))]
    (t/is (close? 1 (op 499)))
    (t/is (close? 0 (op 500)))
    (t/is (close? 0 (op 1500)))
    (t/is (close? 1 (op 1501)))
    (t/is (close? 1 (op 2000)))))

(t/deftest expand-ping-pong-leaves-other-modes-alone
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 10 :height 10})]
    (doseq [playback [:once :loop]]
      (let [tl (fade-timeline shape playback)]
        (t/is (= tl (cta/expand-ping-pong tl)))))))

(t/deftest timeline->lottie-bakes-ping-pong
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 100 :height 100})
        shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 10 :height 10})
        tl    (assoc (fade-timeline shape :ping-pong) :board-id (:id board))
        L     (cta/timeline->lottie tl {(:id board) board (:id shape) shape})]
    (t/is (= 120.0 (:op L)))                        ; 2000ms @ 60fps
    (t/is (= [[100.0] [0.0] [100.0]]
             (mapv :s (get-in L [:layers 0 :ks :o :k]))))))

(t/deftest ping-pong-frames-play-back-without-repeating-the-ends
  (t/testing "a loop goes back to the first frame by itself"
    (t/is (= [:a :b :c :d :c :b] (cta/ping-pong-frames [:a :b :c :d] true))))
  (t/testing "played once, it ends on the first frame"
    (t/is (= [:a :b :c :d :c :b :a] (cta/ping-pong-frames [:a :b :c :d] false))))
  (t/is (= [:a] (cta/ping-pong-frames [:a] true)))
  (t/is (= [:a :b] (cta/ping-pong-frames [:a :b] true)))
  (t/is (= [:a :b :a] (cta/ping-pong-frames [:a :b] false))))

(defn- moved
  "`objects` with the shapes of `ids` moved by `dx`, `dy`."
  [objects ids dx dy]
  (reduce #(update %1 %2 gsh/transform-shape (ctm/move-modifiers (gpt/point dx dy)))
          objects
          ids))

(defn- turned
  "`objects` with the shape `id` turned by `angle` around its center."
  [objects id angle]
  (let [shape (get objects id)]
    (update objects id gsh/transform-shape
            (ctm/rotation-modifiers shape (gsh/shape->center shape) angle))))

(defn- edit-timeline
  "A timeline of the board of `board-and-rect` with keyframes of `property`
  of its rect at 0 and 1000."
  [{:keys [board rect]} property from to]
  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
      (cta/add-keyframe (:id rect) {:time 0 :property property :value from})
      (cta/add-keyframe (:id rect) {:time 1000 :property property :value to})))

(defn- recorded
  [timeline shape-id property time]
  (:value (d/seek #(= time (:time %)) (cta/property-keyframes timeline shape-id property))))

(t/deftest record-edit-adds-the-change-to-the-value-shown
  (let [{:keys [rect objects] :as scene} (board-and-rect)
        rid   (:id rect)
        tl    (edit-timeline scene :x 0 200)
        shown (get-in (values-at tl objects 500) [rid :x])
        tl'   (cta/record-edit tl objects (moved objects [rid] 30 40) 500)]
    (t/is (close? (+ shown 30) (recorded tl' rid :x 500)))
    ;; y is not animated: the move stays on the shape
    (t/is (empty? (cta/property-keyframes tl' rid :y)))
    (t/is (cta/valid-timeline? tl'))))

(t/deftest record-edit-turns-the-short-way
  (let [{:keys [rect objects] :as scene} (board-and-rect)
        rid   (:id rect)
        tl    (edit-timeline scene :rotation 0 90)
        shown (get-in (values-at tl objects 500) [rid :rotation])
        ;; the shape keeps its rotation within 0-360: -20 turns 0 into 340
        after (turned objects rid -20)]
    (t/is (close? 340 (get-in after [rid :rotation])))
    (t/is (close? (- shown 20) (recorded (cta/record-edit tl objects after 500) rid :rotation 500)))))

(t/deftest record-edit-takes-opacity-as-it-ends-up
  (let [{:keys [rect objects] :as scene} (board-and-rect)
        rid (:id rect)
        tl  (edit-timeline scene :opacity 0 1)
        tl' (cta/record-edit tl objects (assoc-in objects [rid :opacity] 0.3) 500)]
    (t/is (close? 0.3 (recorded tl' rid :opacity 500)))))

(t/deftest record-edit-leaves-hidden-and-locked-rows
  (let [{:keys [rect objects] :as scene} (board-and-rect)
        rid   (:id rect)
        tl    (edit-timeline scene :x 0 200)
        after (moved objects [rid] 30 0)]
    (t/is (nil? (recorded (cta/record-edit (cta/toggle-slot-flag tl rid :hidden :x nil) objects after 500) rid :x 500)))
    (t/is (nil? (recorded (cta/record-edit (cta/toggle-slot-flag tl rid :locked :x nil) objects after 500) rid :x 500)))))

(t/deftest record-edit-of-the-board-leaves-its-layers
  (let [{:keys [board rect objects] :as scene} (board-and-rect)
        rid (:id rect)
        tl  (edit-timeline scene :x 0 200)
        ;; the board moves and takes the rect along
        tl' (cta/record-edit tl objects (moved objects [(:id board) rid] 30 0) 500)]
    (t/is (= tl tl'))))

(t/deftest record-edit-records-an-animated-fill
  (let [{:keys [rect objects]} (board-and-rect)
        rid     (:id rect)
        objects (assoc-in objects [rid :fills] [{:fill-color "#000000" :fill-opacity 1}])
        tl      (-> (cta/make-timeline {:board-id (:parent-id (get objects rid)) :duration 1000})
                    (cta/add-keyframe rid {:time 0 :property :fill-color :index 0 :value "#000000"})
                    (cta/add-keyframe rid {:time 1000 :property :fill-color :index 0 :value "#ffffff"}))
        after   (assoc-in objects [rid :fills 0 :fill-color] "#ff0000")
        tl'     (cta/record-edit tl objects after 500)]
    (t/is (= "#ff0000" (:value (d/seek #(= 500 (:time %))
                                       (cta/property-keyframes tl' rid :fill-color 0)))))))

(t/deftest shift-keyframes-moves-them-together
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 100 :property :x :value 0})
                (cta/add-keyframe sid {:time 400 :property :x :value 10})
                (cta/add-keyframe sid {:time 700 :property :y :value 0}))
        ids (mapv (fn [{:keys [id]}] {:shape-id sid :keyframe-id id})
                  (take 2 (get-in tl [:tracks sid :keyframes])))
        times #(mapv :time (get-in % [:tracks sid :keyframes]))]
    (t/is (= [150 450 700] (times (cta/shift-keyframes tl ids 50))))
    (t/testing "none goes before the start"
      (t/is (= [0 300 700] (times (cta/shift-keyframes tl ids -250)))))
    (t/testing "the duration grows to hold them"
      (t/is (= 1400 (:duration (cta/shift-keyframes tl ids 1000)))))
    (t/testing "one landing on another of its property takes its place"
      (let [kfs  #(mapv (juxt :time :property :value) (get-in % [:tracks sid :keyframes]))
            y-id (-> tl (get-in [:tracks sid :keyframes]) last :id)]
        (t/is (= [[400 :x 0] [700 :y 0]] (kfs (cta/shift-keyframes tl (take 1 ids) 300))))
        (t/is (= [[100 :x 0] [400 :x 10] [400 :y 0]]
                 (kfs (cta/shift-keyframes tl [{:shape-id sid :keyframe-id y-id}] -300))))))
    (t/testing "a locked one stays"
      (let [tl (cta/toggle-slot-flag tl sid :locked :x nil)]
        (t/is (= tl (cta/shift-keyframes tl ids 50)))))))

(t/deftest snap-times-leave-out-what-is-dragged
  (let [sid (uuid/next)
        tl  (-> (mk-timeline)
                (cta/add-keyframe sid {:time 100 :property :x :value 0})
                (cta/add-keyframe sid {:time 400 :property :x :value 10})
                (cta/add-animation sid {:type :fade :start 500 :duration 200}))
        kf  (-> tl (get-in [:tracks sid :keyframes]) first :id)
        an  (-> tl (get-in [:tracks sid :animations]) first :id)]
    (t/is (= [0 100 400 500 700 1000] (vec (cta/snap-times tl))))
    (t/is (= [0 400 1000] (vec (cta/snap-times tl :keyframe-ids #{kf} :animation-ids #{an}))))
    (t/testing "the nearest within the threshold"
      (t/is (= 400 (cta/snap-time (cta/snap-times tl) 395 10)))
      (t/is (= 500 (cta/snap-time (cta/snap-times tl) 460 45)))
      (t/is (nil? (cta/snap-time (cta/snap-times tl) 250 10))))))
