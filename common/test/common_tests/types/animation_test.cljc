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
   [app.common.types.audio-clip :as ctau]
   [app.common.types.color :as clr]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.types.text-units :as tu]
   [app.common.uuid :as uuid]
   [clojure.string :as str]
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
    (t/is (re-find #"linear infinite;" css))
    (t/testing "with reduced motion, the shape stays as designed"
      (t/is (re-find #"@media \(prefers-reduced-motion: reduce\) \{\s+\.penpot-shape-\S+ \{\s+animation: none;" css)))))

;; DESIGN TOKENS

(t/deftest presets-and-keyframes-keep-the-tokens-of-their-settings
  (let [fade (cta/make-animation {:type :fade :start 0 :duration 300
                                  :applied-tokens {:duration "motion.fast" :amount "opacity.half"
                                                   :offset-x "space.s" :easing 3}})
        key  (cta/make-keyframe {:time 0 :property :x :value 10
                                 :applied-tokens {:easing "ease.out" :value "space.s"}})]
    (t/is (= {:duration "motion.fast" :amount "opacity.half"} (:applied-tokens fade))
          "an offset belongs to a move only, and a name is a string")
    (t/is (= {:easing "ease.out"} (:applied-tokens key)) "a keyframe binds its easing only")
    (t/is (not (contains? (cta/make-keyframe {:time 0 :property :x :value 10 :applied-tokens {:easing 1}})
                          :applied-tokens)))
    (let [tl (-> (cta/make-timeline {:board-id (uuid/next)})
                 (cta/add-animation (uuid/next) fade))]
      (t/is (sm/validate cta/schema:timeline
                         (assoc-in tl [:tracks (first (keys (:tracks tl))) :keyframes] [key]))))))

(t/deftest a-setting-set-by-hand-lets-go-of-its-token
  (let [fade (cta/make-animation {:type :fade :start 100 :duration 300
                                  :applied-tokens {:start "motion.delay" :duration "motion.fast"
                                                   :easing "ease.out"}})]
    (t/testing "only the settings whose values change"
      (t/is (= {:start "motion.delay" :easing "ease.out"}
               (:applied-tokens (cta/set-by-hand fade {:duration 500}))))
      (t/is (= (:applied-tokens fade) (:applied-tokens (cta/set-by-hand fade {:duration 300})))
            "the same value keeps it")
      (t/is (not (contains? (cta/set-by-hand fade {:start 0 :duration 1 :easing :linear}) :applied-tokens))))
    (t/testing "moving or stretching in the dock sets times by hand"
      (let [sid   (uuid/next)
            tl    (cta/add-animation (cta/make-timeline {:board-id (uuid/next)}) sid fade)
            moved (cta/retime-keyframes tl [sid] [100 400] [200 500])
            grown (cta/retime-keyframes tl [sid] [100 400] [100 700])]
        (t/is (= {:duration "motion.fast" :easing "ease.out"}
                 (get-in moved [:tracks sid :animations 0 :applied-tokens]))
              "a move changes the start only")
        (t/is (= {:start "motion.delay" :easing "ease.out"}
                 (get-in grown [:tracks sid :animations 0 :applied-tokens]))
              "a stretch from the start changes the duration only")))))

(defn- bound-timeline
  "A timeline of a layer `shape` with a fade bound to `motion.fast`, a
  hidden move bound to `motion.slow`, keyframes of x bound to `ease.out`
  (a hold first) and a use of a style whose entry is bound to
  `motion.fast` too."
  [shape]
  (-> (cta/make-timeline {:board-id (uuid/next) :duration 2000})
      (cta/add-animation shape {:id (uuid/next) :type :fade :start 0 :duration 300
                                :applied-tokens {:duration "motion.fast"}})
      (cta/add-animation shape {:id (uuid/next) :type :move :start 500 :duration 300 :hidden true
                                :offset-x 10 :applied-tokens {:duration "motion.slow"}})
      (cta/add-animation shape {:type :style :style-ref-id (uuid/next) :style-ref-file (uuid/next)
                                :name "Pop" :start 1000
                                :entries [{:type :fade :start 0 :duration 300
                                           :applied-tokens {:duration "motion.fast"}}]})
      (cta/add-keyframe shape {:time 0 :property :x :value 0 :interpolation :step
                               :applied-tokens {:easing "ease.out"}})
      (cta/add-keyframe shape {:time 600 :property :x :value 40})))

(t/deftest the-tokens-of-a-timeline-set-its-presets-and-keyframes
  (let [shape    (uuid/next)
        tl       (bound-timeline shape)
        values   {"motion.fast" 150 "motion.slow" 450 "ease.out" [0 0 0.58 1]}
        resolved (cta/resolve-timeline-tokens tl values)
        [fade move use] (get-in resolved [:tracks shape :animations])
        k0       (first (get-in resolved [:tracks shape :keyframes]))]
    (t/is (= [150 450] [(:duration fade) (:duration move)]))
    (t/is (true? (:hidden move)) "a preset keeps its flags")
    (t/is (= (get-in tl [:tracks shape :animations 2]) use) "a use follows its style, not the timeline")
    (t/is (= {:type :bezier :curve [0 0 0.58 1]} (:easing k0)))
    (t/is (nil? (:interpolation k0)) "a held keyframe bound to a curve eases")
    (t/is (= {:easing "ease.out"} (:applied-tokens k0)) "the names stay")
    (t/is (= {:type :spring :stiffness 120 :damping 14 :mass 1}
             (-> (cta/resolve-timeline-tokens tl {"ease.out" {:stiffness 120 :damping 14 :mass 1}})
                 (get-in [:tracks shape :keyframes 0 :easing])))
          "or a spring")
    (t/is (= tl (cta/resolve-timeline-tokens tl {})) "nothing to resolve, the same timeline")
    (t/is (= resolved (cta/resolve-timeline-tokens resolved values)) "resolved, it stays")))

(t/deftest renamed-tokens-reach-presets-and-keyframes
  (let [shape   (uuid/next)
        rename  #(case % "motion.fast" "motion.quick" "ease.out" "ease.soft" %)
        renamed (cta/rename-timeline-tokens (bound-timeline shape) rename)
        [fade move use] (get-in renamed [:tracks shape :animations])]
    (t/is (= ["motion.quick" "motion.slow"] [(get-in fade [:applied-tokens :duration])
                                             (get-in move [:applied-tokens :duration])]))
    (t/is (= "motion.fast" (get-in use [:entries 0 :applied-tokens :duration]))
          "a use takes the names of its style by sync")
    (t/is (= {:easing "ease.soft"} (get-in renamed [:tracks shape :keyframes 0 :applied-tokens])))))

(t/deftest a-timeline-plays-in-the-prototype-unless-told-not-to
  (let [tl (cta/make-timeline {:board-id (uuid/next)})]
    (t/is (cta/autoplay? tl) "by default")
    (t/is (not (contains? tl :autoplay)))
    (t/is (false? (:autoplay (cta/make-timeline {:board-id (uuid/next) :autoplay false}))))
    (t/is (not (cta/autoplay? (cta/with-autoplay tl false))))
    (t/is (= tl (cta/with-autoplay (cta/with-autoplay tl false) true)) "only the exception is kept")
    (t/is (sm/validate cta/schema:timeline (cta/with-autoplay tl false)))))

(t/deftest timeline->css-keeps-the-rest-transform
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 100})
        sid   (:id shape)
        fade  (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                  (cta/add-keyframe sid {:time 0 :property :opacity :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :opacity :value 1}))
        move  (-> fade
                  (cta/add-keyframe sid {:time 0 :property :x :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :x :value 100}))
        rest  {:transform (constantly "matrix(0.866, 0.5, -0.5, 0.866, 0, 0)")}]
    (t/is (not (str/includes? (cta/timeline->css fade {sid shape} rest) "transform:"))
          "a fade leaves the transform of the shape at rest alone")
    (t/is (str/includes? (cta/timeline->css move {sid shape} rest)
                         "transform: translate(100px, 0px) rotate(0deg) matrix(0.866, 0.5, -0.5, 0.866, 0, 0) scale(1, 1);")
          "a move plays on top of it")
    (t/is (str/includes? (cta/timeline->css move {sid shape})
                         "transform: translate(100px, 0px) rotate(0deg) scale(1, 1);")
          "without one, as before")))

(t/deftest frame-values-are-what-css-plays
  (let [shape  (cts/setup-shape {:type :rect :x 10 :y 20 :width 100 :height 50})
        origin {:x 0 :y 0}]
    (t/is (= {:opacity 0.5} (cta/frame-values shape {:opacity 0.5} origin))
          "no transform while none of its properties plays")
    (let [{:keys [translate-x translate-y rotate scale-x scale-y]}
          (cta/frame-values shape {:x 40 :width 200} origin)]
      (t/is (== 30 translate-x))
      (t/is (== 0 translate-y))
      (t/is (== 0 rotate))
      (t/is (== 2 scale-x))
      (t/is (== 1 scale-y)))))

(t/deftest a-spring-over-a-segment-moves-as-the-stretched-one
  (doseq [{:keys [easing]} cta/spring-presets
          share [0.25 0.5 0.75]]
    (let [length 400
          spring (cta/spring-over easing length)]
      (t/is (< (mth/abs (- (cta/spring-free-progress spring (* share (/ length 1000.0)))
                           (cta/spring-progress easing share)))
               1e-6)
            (str (:stiffness easing) "/" (:damping easing) " at " share))
      (t/is (= (:mass easing) (:mass spring)) "its mass as it is"))))

(t/deftest timeline->css-stops-at-the-end
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 100})
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                  (cta/add-keyframe sid {:time 0 :property :x :value 0 :easing :linear})
                  (cta/add-keyframe sid {:time 2000 :property :x :value 200}))
        css   (cta/timeline->css tl {sid shape})]
    (t/is (re-find #"100% \{[^}]*translate\(100px, 0px\)" css)
          "the end, on the way to the keyframe past it")
    (t/is (not (re-find #"200%" css)) "no stop past the end")))

(t/deftest timeline->css-names-every-shape-apart
  ;; The ids of the shapes of a file begin alike.
  (let [a   (cts/setup-shape {:id #uuid "e7bdc995-7266-8021-8008-b4418c7390cd"
                              :type :rect :x 0 :y 0 :width 10 :height 10})
        b   (cts/setup-shape {:id #uuid "e7bdc995-7266-8021-8008-b442136429e5"
                              :type :rect :x 0 :y 0 :width 10 :height 10})
        tl  (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                (cta/add-keyframe (:id a) {:time 0 :property :opacity :value 0})
                (cta/add-keyframe (:id b) {:time 0 :property :opacity :value 0}))
        css (cta/timeline->css tl {(:id a) a (:id b) b})]
    (t/is (not= (cta/shape-css-class (:id a)) (cta/shape-css-class (:id b))))
    (t/is (= 2 (count (set (re-seq #"@keyframes (\S+)" css)))))))

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

(t/deftest paste-track-onto-a-layer
  (let [src    (uuid/next)
        dst    (uuid/next)
        tl     (-> (mk-timeline)
                   (cta/add-keyframe src {:time 100 :property :opacity :value 0})
                   (cta/add-keyframe src {:time 400 :property :opacity :value 1}))
        kfs    (cta/property-keyframes tl src :opacity)
        track  {:shape-id  dst
                :keyframes (mapv #(-> % (dissoc :id) (update :time + 800)) kfs)
                :loops     #{:opacity}}
        tl'    (:timeline (cta/paste-track tl dst track :merge))
        pasted (get-in tl' [:tracks dst :keyframes])]
    (t/is (= [900 1200] (mapv :time pasted)))
    (t/is (= [0 1] (mapv :value pasted)))
    (t/is (not-any? (set (map :id kfs)) (map :id pasted)))
    (t/is (cta/looping? tl' dst :opacity))
    (t/is (= 1200 (:duration tl')))
    (t/is (cta/valid-timeline? tl'))))

(t/deftest paste-track-replaces-a-like-preset
  (let [sid   (uuid/next)
        fade  {:type :fade :start 0 :duration 300}
        tl    (-> (mk-timeline)
                  (cta/add-animation sid {:type :move :start 0 :duration 400})
                  (cta/add-animation sid fade))
        paste (fn [tl animation]
                (:timeline (cta/paste-track tl sid {:shape-id sid :keyframes [] :animations [animation]}
                                            :merge)))
        rows  #(mapv (juxt :type :direction :start :duration) (get-in % [:tracks sid :animations]))]

    (t/testing "pasted twice at the same time, it is there once"
      (t/is (= [[:move :in 0 400] [:fade :in 0 300]]
               (rows (-> tl (paste fade) (paste fade))))))

    (t/testing "it takes the place of the one there, in its row"
      (t/is (= [[:move :in 0 400] [:fade :in 0 500]]
               (rows (paste tl (assoc fade :duration 500)))))
      (t/is (cta/valid-timeline? (paste tl (assoc fade :duration 500)))))

    (t/testing "at another time or the other way, it is added"
      (t/is (= [[:move :in 0 400] [:fade :in 0 300] [:fade :in 600 300]]
               (rows (paste tl (assoc fade :start 600)))))
      (t/is (= [[:move :in 0 400] [:fade :in 0 300] [:fade :out 0 300]]
               (rows (paste tl (assoc fade :direction :out))))))

    (t/testing "a locked one stays, and takes nothing"
      (let [fid (:id (second (get-in tl [:tracks sid :animations])))
            tl  (cta/toggle-animation-flag tl sid fid :locked)]
        (t/is (= [[:move :in 0 400] [:fade :in 0 300]]
                 (rows (paste tl (assoc fade :duration 500)))))))))

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
        ;; an image fill has no vectors
        shape (cts/setup-shape {:type :rect :x 100 :y 100 :width 200 :height 100
                                :fills [{:fill-image {:id (uuid/next) :width 10 :height 10
                                                      :mtype "image/png"}}]})
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
    (t/is (= "img_0" (get-in L [:assets 0 :id])))
    (t/testing "the image, from the layer's origin, sits on the shape"
      (t/is (= [100.0 50.0 0] (get-in layer [:ks :a :k])))
      (t/is (= 200.0 (double (get-in layer [:ks :p :x :k 0 :s 0]))))
      (t/is (= 150.0 (double (get-in layer [:ks :p :y :k])))))
    (t/testing "embedded, or a file in a dotLottie archive"
      (t/is (= {:u "" :p "data:image/png;base64,AAA" :e 1}
               (select-keys (get-in L [:assets 0]) [:u :p :e])))
      (let [archived (cta/timeline->lottie tl objects
                                           {sid {:id "img_0" :w 200 :h 100
                                                 :u "/images/" :p "img_0.png" :e 0}})]
        (t/is (= {:u "/images/" :p "img_0.png" :e 0}
                 (select-keys (get-in archived [:assets 0]) [:u :p :e])))))
    (t/testing "a timeline keeps dotLottie as its export format"
      (t/is (cta/valid-timeline? (assoc tl :export {:format :dotlottie}))))))

(t/deftest lottie-image-layer-has-the-opacity-of-the-shape
  ;; its image is rasterized opaque
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100})
        shape   (cts/setup-shape {:type :text :x 10 :y 10 :width 50 :height 20 :opacity 0.5})
        [bid sid] (map :id [board shape])
        objects {bid (assoc board :shapes [sid])
                 sid (assoc shape :parent-id bid :frame-id bid)}
        tl      (-> (cta/make-timeline {:board-id bid :duration 1000})
                    (cta/add-keyframe sid {:time 0 :property :x :value 10})
                    (cta/add-keyframe sid {:time 1000 :property :x :value 50}))
        L       (cta/timeline->lottie tl objects {sid {:id "img_0" :w 50 :h 20 :p "data:," :e 1}})
        layer   (first (:layers L))]
    (t/is (= 2 (:ty layer)))
    (t/is (= 50.0 (double (get-in layer [:ks :o :k]))))))

(t/deftest timeline->svg-gives-a-layer-its-opacity
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100})
        shape   (cts/setup-shape {:type :rect :x 10 :y 10 :width 20 :height 20 :opacity 0.5})
        [bid sid] (map :id [board shape])
        objects {bid (assoc board :shapes [sid])
                 sid (assoc shape :parent-id bid :frame-id bid)}
        tl      (-> (cta/make-timeline {:board-id bid :duration 500})
                    (cta/add-keyframe sid {:time 0 :property :x :value 10}))]
    (t/is (re-find (re-pattern (str "class=\"" (cta/shape-css-class sid) "\" opacity=\"0.5\""))
                   (cta/timeline->svg tl objects {})))))

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
    (t/is (re-find #"x=\"12\" y=\"12\"" svg) "where it is in the SVG")))

(t/deftest timeline->svg-nests-and-clips-layers
  ;; A layer inside a board that is none follows the layer holding that
  ;; board, and both boards keep it to their outlines.
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 200})
        card    (cts/setup-shape {:type :frame :x 10 :y 10 :width 100 :height 100})
        holder  (cts/setup-shape {:type :frame :x 20 :y 20 :width 50 :height 50})
        dot     (cts/setup-shape {:type :rect :x 30 :y 30 :width 10 :height 10})
        [bid cid hid did] (map :id [board card holder dot])
        objects {bid (assoc board :shapes [cid])
                 cid (assoc card :parent-id bid :frame-id bid :shapes [hid])
                 hid (assoc holder :parent-id cid :frame-id cid :shapes [did])
                 did (assoc dot :parent-id hid :frame-id hid)}
        tl      (-> (cta/make-timeline {:board-id bid :duration 500})
                    (cta/add-keyframe cid {:time 0 :property :opacity :value 0})
                    (cta/add-keyframe did {:time 0 :property :x :value 30}))
        svg     (cta/timeline->svg tl objects {})
        clip    #(str "clip-path=\"url\\(#penpot-clip-" % "\\)\"")]
    (t/is (re-find (re-pattern (str (cta/shape-css-class cid) "\".*" (clip cid) ".*" (clip hid)
                                    ".*" (cta/shape-css-class did)))
                   svg))
    (t/is (re-find (re-pattern (str "<clipPath id=\"penpot-clip-" cid "\">")) svg))
    (t/is (re-find (re-pattern (str "<clipPath id=\"penpot-clip-" hid "\">")) svg))
    (t/is (re-find #"x=\"30\" y=\"30\"" svg))))

(t/deftest timeline->svg-turns-layers-around-their-pivot
  ;; A share of an SVG transform-origin is of the whole drawing.
  (let [board   (cts/setup-shape {:type :frame :x 50 :y 50 :width 200 :height 200})
        shape   (cts/setup-shape {:type :rect :x 100 :y 100 :width 40 :height 20})
        [bid sid] (map :id [board shape])
        objects {bid (assoc board :shapes [sid])
                 sid (assoc shape :parent-id bid :frame-id bid)}
        tl      (-> (cta/make-timeline {:board-id bid :duration 500})
                    (cta/add-keyframe sid {:time 0 :property :rotation :value 0})
                    (cta/add-keyframe sid {:time 500 :property :rotation :value 90}))]
    (t/is (re-find #"transform-origin: 70px 60px;" (cta/timeline->svg tl objects {})))
    (t/is (re-find #"transform-origin: 50% 50%;" (cta/timeline->css tl objects)) "unlike CSS")))

(t/deftest timeline->svg-paints-the-layers-of-a-layout-as-it-does
  ;; A flex board paints its first layer on top: in SVG, last.
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 200 :height 100
                                  :layout :flex :layout-flex-dir :column})
        one     (cts/setup-shape {:type :rect :x 0 :y 0 :width 40 :height 40})
        two     (cts/setup-shape {:type :rect :x 0 :y 40 :width 40 :height 40})
        bid     (:id board)
        objects {bid (assoc board :shapes [(:id one) (:id two)])
                 (:id one) (assoc one :parent-id bid :frame-id bid)
                 (:id two) (assoc two :parent-id bid :frame-id bid)}
        tl      (reduce #(cta/add-keyframe %1 (:id %2) {:time 0 :property :opacity :value 1})
                        (cta/make-timeline {:board-id bid :duration 500})
                        [one two])
        svg     (cta/timeline->svg tl objects {})
        drawn   (fn [shape] (str/index-of svg (str "class=\"" (cta/shape-css-class (:id shape)) "\"")))]
    (t/is (< (drawn two) (drawn one)))))

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

(defn- close-all?
  "Whether the numbers of `actual` are close to those of `expected`,
  nested alike."
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map (fn [e a]
                            (if (sequential? e) (close-all? e a) (close? e a)))
                          expected actual))))

(defn- lottie-of
  "The Lottie document of `shape` in `board`, with the keyframes `kfs`
  (an opacity one without any: a layer is an animated shape)."
  [board shape kfs & [assets]]
  (let [bid (:id board)
        sid (:id shape)
        tl  (reduce #(cta/add-keyframe %1 sid %2)
                    (cta/make-timeline {:board-id bid :duration 1000})
                    (or (seq kfs) [{:time 0 :property :opacity :value 1}]))]
    (cta/timeline->lottie tl {bid (assoc board :shapes [sid])
                              sid (assoc shape :parent-id bid :frame-id bid)}
                          assets)))

(defn- lottie-item
  [items ty]
  (d/seek #(= ty (:ty %)) items))

(t/deftest lottie-draws-a-rect-as-vectors
  (let [board (cts/setup-shape {:type :frame :x 100 :y 100 :width 400 :height 300})
        shape (cts/setup-shape {:type :rect :x 150 :y 200 :width 100 :height 50
                                :r1 8 :r2 8 :r3 8 :r4 8
                                :fills [{:fill-color "#ff0000" :fill-opacity 1}]
                                :strokes [{:stroke-color "#0000ff" :stroke-opacity 1
                                           :stroke-width 2 :stroke-style :dashed}]})
        L     (lottie-of board shape [{:time 0 :property :width :value 100}
                                      {:time 1000 :property :width :value 200}])
        items (get-in L [:layers 0 :shapes 0 :it])
        rc    (lottie-item items "rc")]
    (t/is (= ["rc" "st" "fl" "tr"] (mapv :ty items)) "the stroke over the fill")
    (t/testing "the size follows the width, from the top left corner, in the board"
      (t/is (close-all? [[100 50] [200 50]] (mapv :s (get-in rc [:s :k]))))
      (t/is (close-all? [[100 125] [150 125]] (mapv :s (get-in rc [:p :k])))))
    (t/is (close? 8 (get-in rc [:r :k])))
    (t/is (= "d" (get-in (lottie-item items "st") [:d 0 :n])) "dashed")
    (t/is (close-all? [1 0 0 1] (get-in (lottie-item items "fl") [:c :k])))
    (t/testing "with an image of it, still vectors"
      (let [L (lottie-of board shape [] {(:id shape) {:id "img_0" :w 100 :h 50 :p "data:," :e 1}})]
        (t/is (= 4 (get-in L [:layers 0 :ty])))))))

(defn- in-board
  "`objects` of `board` and `shapes` inside it, in stacking order (the
  last on top), and `children` (`{parent-id [shape ...]}`) inside them."
  [board shapes & [children]]
  (let [bid    (:id board)
        inside (fn [objects parent-id shapes]
                 (reduce (fn [objects shape]
                           (assoc objects (:id shape) (assoc shape :parent-id parent-id :frame-id bid)))
                         (assoc-in objects [parent-id :shapes] (mapv :id shapes))
                         shapes))]
    (reduce-kv inside
               (inside {bid board} bid shapes)
               (or children {}))))

(defn- lottie-with
  "The Lottie document of the board of `objects`, with an opacity
  keyframe of each of `animated`."
  [board objects animated]
  (let [tl (reduce #(cta/add-keyframe %1 (:id %2) {:time 0 :property :opacity :value 1})
                   (cta/make-timeline {:board-id (:id board) :duration 1000})
                   animated)]
    (cta/timeline->lottie tl objects)))

(t/deftest lottie-lists-the-top-most-layer-first
  (let [board  (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        bottom (cts/setup-shape {:type :rect :name "bottom" :x 0 :y 0 :width 40 :height 40})
        top    (cts/setup-shape {:type :rect :name "top" :x 20 :y 20 :width 40 :height 40})
        L      (lottie-with board (in-board board [bottom top]) [bottom top])]
    (t/is (= ["top" "bottom" "Board"] (mapv :nm (:layers L))) "the board's own at the bottom")))

(t/deftest lottie-lists-the-layers-of-a-layout-as-it-paints-them
  ;; A flex or grid board paints its first layer on top, a reversed flex
  ;; its last, and a z-index goes before either.
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300
                                :layout :flex :layout-flex-dir :column})
        one   (cts/setup-shape {:type :rect :name "one" :x 0 :y 0 :width 40 :height 40})
        two   (cts/setup-shape {:type :rect :name "two" :x 0 :y 40 :width 40 :height 40})
        names (fn [board one two]
                (mapv :nm (:layers (lottie-with board (in-board board [one two]) [one two]))))]
    (t/is (= ["one" "two" "Board"] (names board one two)))
    (t/is (= ["one" "two" "Board"] (names (assoc board :layout :grid) one two)))
    (t/is (= ["two" "one" "Board"] (names (assoc board :layout-flex-dir :column-reverse) one two)))
    (t/is (= ["two" "one" "Board"] (names board one (assoc two :layout-item-z-index 1))))))

(t/deftest lottie-draws-the-layers-of-a-layout-in-its-layer-as-it-paints-them
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        flex    (cts/setup-shape {:type :frame :name "flex" :x 20 :y 20 :width 100 :height 100
                                  :layout :flex :layout-flex-dir :row
                                  :fills [{:fill-color "#eeeeee" :fill-opacity 1}]})
        one     (cts/setup-shape {:type :rect :name "one" :x 20 :y 20 :width 40 :height 40
                                  :fills [{:fill-color "#ff0000" :fill-opacity 1}]})
        two     (cts/setup-shape {:type :rect :name "two" :x 40 :y 40 :width 40 :height 40
                                  :fills [{:fill-color "#0000ff" :fill-opacity 1}]})
        objects (in-board board [flex] {(:id flex) [one two]})
        layer   (first (:layers (lottie-with board objects [flex])))]
    (t/is (= ["one" "two" "shape"] (mapv :nm (:shapes layer))))))

(t/deftest lottie-draws-a-group-with-its-layers
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        group   (cts/setup-shape {:type :group :name "G" :x 10 :y 10 :width 100 :height 50})
        a       (cts/setup-shape {:type :rect :name "a" :x 10 :y 10 :width 40 :height 50
                                  :fills [{:fill-color "#ff0000" :fill-opacity 1}]})
        b       (cts/setup-shape {:type :circle :name "b" :x 60 :y 10 :width 50 :height 50 :opacity 0.5
                                  :fills [{:fill-color "#0000ff" :fill-opacity 1}]})
        objects (in-board board [group] {(:id group) [a b]})
        layer   (first (:layers (lottie-with board objects [group])))
        [top bottom] (:shapes layer)]
    (t/is (= ["G" "Board"] (mapv :nm (:layers (lottie-with board objects [group])))) "its layers draw in it")
    (t/is (= ["b" "a"] [(:nm top) (:nm bottom)]) "the top-most first")
    (t/is (close? 50 (get-in (peek (:it top)) [:o :k])) "with their opacity")
    (t/is (= "sh" (get-in top [:it 0 :it 0 :ty])) "as paths")
    (t/is (close-all? [0 0 1 1] (get-in top [:it 0 :it 1 :c :k])))))

(t/deftest lottie-keeps-a-board-to-its-outline
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        inner   (cts/setup-shape {:type :frame :name "inner" :x 20 :y 20 :width 100 :height 100
                                  :fills [{:fill-color "#eeeeee" :fill-opacity 1}]})
        child   (cts/setup-shape {:type :rect :name "child" :x 80 :y 80 :width 100 :height 100
                                  :fills [{:fill-color "#000000" :fill-opacity 1}]})
        objects (in-board board [inner] {(:id inner) [child]})
        layer   (first (:layers (lottie-with board objects [inner])))]
    (t/is (true? (:hasMask layer)))
    (t/is (close-all? [[20 20] [120 20] [120 120] [20 120]]
                      (get-in layer [:masksProperties 0 :pt :k :v])))
    (t/is (true? (get-in layer [:masksProperties 0 :pt :k :c])) "back on its start, it closes")
    (t/is (= ["child" "shape"] (mapv :nm (:shapes layer))) "its layers over its own shape")))

(t/deftest lottie-draws-the-board-at-the-bottom
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300
                                  :fills [{:fill-color "#336699" :fill-opacity 1}]})
        shape   (cts/setup-shape {:type :rect :name "shape" :x 10 :y 10 :width 40 :height 40})
        layers  (fn [board] (:layers (lottie-with board (in-board board [shape]) [shape])))
        bottom  (peek (layers board))
        fill    (lottie-item (get-in bottom [:shapes 0 :it]) "fl")]
    (t/is (= ["shape" "Board"] (mapv :nm (layers board))))
    (t/is (close-all? [0.2 0.4 0.6 1] (get-in fill [:c :k])))
    (t/is (nil? (:hasMask bottom)) "the composition clips it already")
    (t/testing "but not its fill when it is not to be exported"
      (t/is (= ["shape"] (mapv :nm (layers (assoc board :hide-fill-on-export true))))))
    (t/testing "nor when it draws nothing"
      (t/is (= ["shape"] (mapv :nm (layers (assoc board :fills []))))))))

(t/deftest lottie-puts-the-layers-inside-a-layer-in-a-precomp
  ;; so they move, fade and are clipped with it
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300 :fills []})
        card    (cts/setup-shape {:type :frame :name "card" :x 20 :y 20 :width 100 :height 100
                                  :opacity 0.5
                                  :fills [{:fill-color "#eeeeee" :fill-opacity 1}]})
        moving  (cts/setup-shape {:type :rect :name "moving" :x 30 :y 30 :width 20 :height 20})
        still   (cts/setup-shape {:type :rect :name "still" :x 60 :y 60 :width 20 :height 20
                                  :fills [{:fill-color "#000000" :fill-opacity 1}]})
        objects (in-board board [card] {(:id card) [still moving]})
        tl      (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                    (cta/add-keyframe (:id card) {:time 0 :property :y :value 60})
                    (cta/add-keyframe (:id card) {:time 1000 :property :y :value 20})
                    (cta/add-keyframe (:id moving) {:time 0 :property :x :value 30})
                    (cta/add-keyframe (:id moving) {:time 1000 :property :x :value 60}))
        L       (cta/timeline->lottie tl objects)
        [layer] (:layers L)
        comp    (d/seek #(= (:refId layer) (:id %)) (:assets L))
        [top content] (:layers comp)]
    (t/is (= 1 (count (:layers L))))
    (t/is (= 0 (:ty layer)) "a precomp")
    (t/is (= 1 (get-in layer [:ks :p :y :a])) "that moves with the card")
    (t/is (close? 50 (get-in layer [:ks :o :k])) "fades with it")
    (t/is (true? (:hasMask layer)) "and keeps to it")
    (t/is (= ["moving" "card"] [(:nm top) (:nm content)]) "the card itself at the bottom")
    (t/is (= 1 (get-in top [:ks :p :x :a])))
    (t/is (= 0 (get-in content [:ks :p :y :a])) "which the precomp moves")
    (t/is (close? 100 (get-in content [:ks :o :k])))
    (t/is (nil? (:hasMask content)))
    (t/is (= ["still" "shape"] (mapv :nm (:shapes content))) "with what is no layer")))

(t/deftest lottie-nests-precomps
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300 :fills []})
        outer   (cts/setup-shape {:type :frame :name "outer" :x 0 :y 0 :width 200 :height 200})
        inner   (cts/setup-shape {:type :frame :name "inner" :x 10 :y 10 :width 100 :height 100})
        leaf    (cts/setup-shape {:type :rect :name "leaf" :x 20 :y 20 :width 10 :height 10})
        objects (in-board board [outer] {(:id outer) [inner] (:id inner) [leaf]})
        L       (lottie-with board objects [outer inner leaf])
        comps   (into {} (comp (filter :layers) (map (juxt :id identity))) (:assets L))
        refs    (fn refs [layers]
                  (mapcat #(when-let [ref (:refId %)]
                             (cons ref (refs (:layers (get comps ref)))))
                          layers))]
    (t/is (= 2 (count comps)))
    (t/is (= 2 (count (refs (:layers L)))))
    (t/is (every? #(contains? comps %) (refs (:layers L))) "each one there")))

(t/deftest lottie-clips-the-layers-of-a-board-that-is-no-layer
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300 :fills []})
        frame   (cts/setup-shape {:type :frame :name "frame" :x 20 :y 20 :width 100 :height 100})
        moving  (cts/setup-shape {:type :rect :name "moving" :x 30 :y 30 :width 20 :height 20})
        objects (in-board board [frame] {(:id frame) [moving]})
        [layer] (:layers (lottie-with board objects [moving]))]
    (t/is (= 0 (:ty layer)))
    (t/is (true? (:hasMask layer)))
    (t/is (= 0 (get-in layer [:ks :o :a])) "it does not move")
    (t/testing "unless it shows its content"
      (let [objects (assoc-in objects [(:id frame) :show-content] true)]
        (t/is (= ["moving" "Board"] (mapv :nm (:layers (lottie-with board objects [moving]))))
              "the frame drawn with the board")))))

(t/deftest lottie-layer-keeps-its-opacity
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        shape (cts/setup-shape {:type :rect :x 10 :y 10 :width 40 :height 40 :opacity 0.25})
        L     (lottie-of board shape [{:time 0 :property :x :value 10}
                                      {:time 1000 :property :x :value 50}])]
    (t/is (close? 25 (get-in L [:layers 0 :ks :o :k])))))

(t/deftest lottie-draws-drop-shadows-as-effects
  (let [board  (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        shadow {:id (uuid/next) :style :drop-shadow :offset-x 0 :offset-y 4 :blur 6 :spread 0
                :hidden false :color {:color "#000000" :opacity 0.5}}
        shape  (cts/setup-shape {:type :rect :x 0 :y 0 :width 40 :height 40 :shadow [shadow]
                                 :fills [{:fill-color "#ffffff" :fill-opacity 1}]})
        layer  (first (:layers (lottie-with board (in-board board [shape]) [shape])))
        ef     (first (:ef layer))
        value  #(get-in (d/seek (fn [e] (= % (:nm e))) (:ef ef)) [:v :k])]
    (t/is (= 25 (:ty ef)))
    (t/is (close? 180 (value "Direction")) "down")
    (t/is (close? 4 (value "Distance")))
    (t/is (close? 12 (value "Softness")))
    (t/is (close? 127.5 (value "Opacity")))
    (t/testing "an inner shadow, or a text inside, draws no vectors"
      (let [inner   (assoc shadow :style :inner-shadow)
            objects (in-board board [(assoc shape :shadow [inner])])]
        (t/is (not (cta/lottie-vector-layer? objects #{(:id shape)} (:id shape))))))))

(t/deftest lottie-writes-one-keyframe-as-a-value
  ;; players draw no layer with an animated value of one keyframe
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 40 :height 20
                                :fills [{:fill-color "#000000" :fill-opacity 1}]})
        layer (get-in (lottie-of board shape [{:time 200 :property :opacity :value 0.5}
                                              {:time 200 :property :fill-color :index 0 :value "#ff0000"}
                                              {:time 200 :property :width :value 80}])
                      [:layers 0])
        items (get-in layer [:shapes 0 :it])]
    (t/is (= {:a 0 :k 50.0} (get-in layer [:ks :o])))
    (t/is (= 0 (get-in (lottie-item items "fl") [:c :a])))
    (t/is (close-all? [1 0 0 1] (get-in (lottie-item items "fl") [:c :k])))
    (t/is (close-all? [80 20] (get-in (lottie-item items "rc") [:s :k])))))

(t/deftest lottie-draws-an-ellipse
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        shape (cts/setup-shape {:type :circle :x 0 :y 0 :width 40 :height 20
                                :fills [{:fill-color "#00ff00" :fill-opacity 0.5}]})
        items (get-in (lottie-of board shape []) [:layers 0 :shapes 0 :it])
        el    (lottie-item items "el")]
    (t/is (= ["el" "fl" "tr"] (mapv :ty items)))
    (t/is (close-all? [40 20] (get-in el [:s :k])))
    (t/is (close-all? [20 10] (get-in el [:p :k])))
    (t/is (close? 50 (get-in (lottie-item items "fl") [:o :k])))))

(def ^:private test-path
  [{:command :move-to :params {:x 110 :y 110}}
   {:command :line-to :params {:x 150 :y 110}}
   {:command :curve-to :params {:x 150 :y 150 :c1x 160 :c1y 120 :c2x 160 :c2y 140}}
   {:command :line-to :params {:x 110 :y 110}}
   {:command :close-path :params {}}])

(defn- path-shape
  [attrs]
  (cts/setup-shape (merge {:type :path
                           :content (path/content test-path)
                           :fills [{:fill-color "#000000" :fill-opacity 1}]}
                          attrs)))

(t/deftest lottie-draws-a-path-by-its-vertices
  (let [board (cts/setup-shape {:type :frame :x 100 :y 100 :width 400 :height 300})
        shape (path-shape {})
        items (get-in (lottie-of board shape []) [:layers 0 :shapes 0 :it])
        k     (get-in (lottie-item items "sh") [:ks :k])]
    (t/is (true? (:c k)))
    (t/testing "in the board, the way back to the first vertex joining it"
      (t/is (close-all? [[10 10] [50 10] [50 50]] (:v k))))
    (t/testing "the tangents of the curve, from their vertex"
      (t/is (close-all? [[0 0] [10 10] [0 0]] (:o k)))
      (t/is (close-all? [[0 0] [0 0] [10 -10]] (:i k))))))

(t/deftest lottie-draws-a-path-before-it-turns
  (let [board   (cts/setup-shape {:type :frame :x 100 :y 100 :width 400 :height 300})
        shape    (path-shape {})
        turned   (gsh/transform-shape shape (ctm/rotation-modifiers shape (gsh/shape->center shape) 90))
        vertices #(get-in (lottie-of board % []) [:layers 0 :shapes 0 :it 0 :ks :k :v])]
    (t/is (close? 90 (:rotation turned)))
    ;; the layer turns it
    (t/is (close-all? (vertices shape) (vertices turned)))))

(t/deftest lottie-trims-the-strokes-only
  (let [board  (cts/setup-shape {:type :frame :x 100 :y 100 :width 400 :height 300})
        shape  (path-shape {:strokes [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 4}]})
        groups (get-in (lottie-of board shape [{:time 0 :property :trim-end :value 0}
                                               {:time 1000 :property :trim-end :value 1}])
                       [:layers 0 :shapes])
        [strokes fills] groups]
    (t/is (= ["strokes" "fills"] (mapv :nm groups)))
    (t/is (= ["sh" "tm" "st" "tr"] (mapv :ty (:it strokes))))
    (t/is (= ["sh" "fl" "tr"] (mapv :ty (:it fills))))
    (t/is (close-all? [[0] [100]] (mapv :s (get-in (lottie-item (:it strokes) "tm") [:e :k]))))))

(t/deftest lottie-draws-a-gradient
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 50
                                :fills [{:fill-opacity 1
                                         :fill-color-gradient
                                         {:type :linear :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5 :width 1
                                          :stops [{:color "#000000" :offset 0}
                                                  {:color "#ffffff" :opacity 0.5 :offset 1}]}}]})
        gf    (lottie-item (get-in (lottie-of board shape []) [:layers 0 :shapes 0 :it]) "gf")]
    (t/is (= 1 (:t gf)))
    (t/is (close-all? [0 25] (get-in gf [:s :k])))
    (t/is (close-all? [100 25] (get-in gf [:e :k])))
    (t/is (= 2 (get-in gf [:g :p])))
    (t/is (close-all? [0 0 0 0 1 1 1 1 0 1 1 0.5] (get-in gf [:g :k :k])))))

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
    (t/is (= (:duration tl) (:duration tl2)) "the duration stays, the animation goes past the end")
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
      (let [kfs   (into (cta/property-keyframes tl sid :x)
                        (cta/property-keyframes tl sid :opacity))
            track {:shape-id  sid
                   :keyframes (mapv #(-> % (dissoc :id) (update :time + 300)) kfs)
                   :loops     #{:x}}
            tl2   (:timeline (cta/paste-track tl sid track :merge))]
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
                     (cta/add-animation sid {:type :fade :split :char :spread 0.3 :order :random})
                     (cta/toggle-loop sid :x)
                     (cta/toggle-slot-flag sid :hidden :fill-color 0)
                     (cta/toggle-slot-flag sid :locked :opacity)
                     (cta/set-track-origin sid 0 1)
                     (assoc :export {:format :mp4 :audio false})
                     (ctau/add-audio-clip {:audio-id (uuid/next) :name "beat.mp3" :source-duration 4000
                                           :start 100 :offset 250 :duration 1500 :lane 1 :volume 0.8
                                           :fade-in 50 :fade-out 300 :muted true :locked true}))
        encode   (sm/encoder cta/schema:timeline (sm/json-transformer))
        decode   (sm/decoder cta/schema:timeline (sm/json-transformer))]
    (t/is (cta/valid-timeline? timeline))
    (t/is (= timeline (-> timeline encode json-roundtrip decode)))))

;; ANIMATION STYLE USES

(defn- style-use
  "A use, from 100 ms, of a style of a fade in (0-300 ms), an overshooting
  scale in (100-500 ms) and a move out to the right (200-400 ms), with
  `params` over it."
  [& {:as params}]
  (cta/make-animation
   (merge {:type :style
           :style-ref-id (uuid/next)
           :style-ref-file (uuid/next)
           :name "Pop"
           :start 100
           :entries [{:type :fade :start 0 :duration 300}
                     {:type :scale :start 100 :duration 400 :easing cta/overshoot-easing}
                     {:type :move :direction :out :start 200 :duration 200 :offset-x 30 :offset-y 0}]}
          params)))

(defn- style-presets
  "The presets `style-use` stands for, written out: from `start`, `k`
  times as long."
  [start k]
  [{:type :fade :start start :duration (* k 300)}
   {:type :scale :start (+ start (* k 100)) :duration (* k 400) :easing cta/overshoot-easing}
   {:type :move :direction :out :start (+ start (* k 200)) :duration (* k 200)
    :offset-x 30 :offset-y 0}])

(defn- styled-and-written-out
  "Two timelines of `rect` (of `board`) moving right in a second, one with
  `use` and one with `presets` in its place."
  [board rect use presets]
  (let [sid (:id rect)
        tl  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                (cta/add-keyframe sid {:time 0 :property :x :value 50})
                (cta/add-keyframe sid {:time 1000 :property :x :value 150}))]
    [(cta/add-animation tl sid use)
     (reduce #(cta/add-animation %1 sid %2) tl presets)]))

(t/deftest a-style-use-plays-as-the-presets-it-stands-for
  (let [{:keys [board rect objects]} (board-and-rect)
        sid     (:id rect)
        objects (assoc-in objects [(:id board) :shapes] [sid])

        [styled written] (styled-and-written-out board rect (style-use) (style-presets 100 1))]
    (t/is (cta/valid-timeline? styled))
    (t/is (= (:duration written) (:duration styled)))
    (t/is (= [] (filterv #(not= (values-at written objects %) (values-at styled objects %))
                         (range 0 (inc (:duration styled)) 10)))
          "the times it plays otherwise")
    (doseq [time [0 150 250 350 450 550 1000]]
      (t/is (= (cta/timeline->modif-tree written objects time)
               (cta/timeline->modif-tree styled objects time))
            (str "t=" time)))
    (t/is (= (cta/timeline->css written objects) (cta/timeline->css styled objects)))
    (t/is (= (cta/timeline->svg written objects {}) (cta/timeline->svg styled objects {})))
    (t/is (= (mapv :ks (:layers (cta/timeline->lottie written objects nil)))
             (mapv :ks (:layers (cta/timeline->lottie styled objects nil)))))))

(t/deftest a-style-use-stretches-its-entries-to-its-duration
  (let [{:keys [board rect objects]} (board-and-rect)
        sid  (:id rect)
        use  (style-use :duration 1000)
        play #(get (values-at (cta/add-animation (cta/make-timeline {:board-id (:id board)}) sid %)
                              objects %2)
                   sid)]
    (t/is (= [[100 600] [300 800] [500 400]] (mapv (juxt :start :duration) (cta/expand-style use))))
    ;; twice as long, it shows at 100 + 2t what it shows at 100 + t
    (doseq [t (range 0 501 10)]
      (let [once  (play (assoc use :duration 500) (+ 100 t))
            twice (play use (+ 100 (* 2 t)))]
        (t/is (= (set (keys once)) (set (keys twice))))
        (t/is (every? #(close? (get once %) (get twice %)) (keys once)) (str "t=" t))))))

(t/deftest expanding-a-style-use-keeps-the-ids-of-its-entries
  (let [use (style-use)
        id  (uuid/next)]
    (t/is (= (cta/expand-style use) (cta/expand-style use)))
    (t/is (= (mapv :id (:entries use)) (mapv :id (cta/expand-style use))))
    (t/is (every? uuid? (map :id (:entries use))))
    (t/is (= {:id id :type :fade :direction :in :start 0 :duration 400 :amount 0}
             (cta/make-style-entry {:id id :type :fade :hidden true :locked true}))
          "an entry keeps its id and has no flags")))

(t/deftest track-animations-expand-the-uses-in-place
  (let [hidden  (cta/make-animation {:type :rotate :hidden true})
        use     (style-use)
        visible (cta/make-animation {:type :fade :direction :out :start 700})
        ids     #(mapv :id (cta/track-animations {:animations %}))]
    (t/is (= (conj (mapv :id (:entries use)) (:id visible))
             (ids [hidden use visible])))
    (t/is (= [] (ids [(assoc use :hidden true)])))
    (t/is (= [] (ids nil)))))

(t/deftest hidden-and-locked-style-uses
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                (cta/add-animation sid (style-use)))
        uid (:id (first (get-in tl [:tracks sid :animations])))]
    (t/testing "a hidden one plays nothing"
      (let [tl (cta/toggle-animation-flag tl sid uid :hidden)]
        (t/is (cta/valid-timeline? tl))
        (t/is (nil? (get (values-at tl objects 300) sid)))
        (t/is (not (re-find #"opacity" (cta/timeline->css tl objects))))))
    (t/testing "a locked one is kept from edits"
      (let [tl (cta/toggle-animation-flag tl sid uid :locked)]
        (t/is (= [uid] (mapv :id (get-in (cta/remove-track tl sid) [:tracks sid :animations]))))
        (t/is (= tl (cta/retime-keyframes tl [sid] [0 1000] [500 1500])))
        (t/is (= tl (cta/update-animation tl sid uid #(assoc % :start 0))))
        (t/is (= tl (cta/remove-animation tl sid uid)))))))

(t/deftest a-style-use-keeps-its-style-through-edits
  (let [sid  (uuid/next)
        use  (style-use)
        tl   (cta/add-animation (mk-timeline) sid use)
        kept #(select-keys % [:style-ref-id :style-ref-file :name :entries])]
    (t/testing "an update"
      (let [tl2  (cta/update-animation tl sid (:id use) #(assoc % :start 300 :duration 1000))
            use2 (cta/get-animation tl2 sid (:id use))]
        (t/is (= [300 1000] [(:start use2) (:duration use2)]))
        (t/is (= (kept use) (kept use2)))
        (t/is (cta/valid-timeline? tl2))))
    (t/testing "a retime, as a preset"
      (let [tl2         (-> tl
                            (cta/add-animation sid {:type :fade :start 100 :duration 500})
                            (cta/retime-keyframes [sid] [0 1000] [0 2000]))
            [use2 fade] (get-in tl2 [:tracks sid :animations])]
        (t/is (= [200 1000] [(:start use2) (:duration use2)]))
        (t/is (= [(:start fade) (:duration fade)] [(:start use2) (:duration use2)]))
        (t/is (= (kept use) (kept use2)))))))

(t/deftest a-style-use-survives-json
  (let [sid      (uuid/next)
        timeline (-> (mk-timeline)
                     (cta/add-animation sid {:type :fade})
                     (cta/add-animation sid (style-use :locked true)))
        encode   (sm/encoder cta/schema:timeline (sm/json-transformer))
        decode   (sm/decoder cta/schema:timeline (sm/json-transformer))
        json     (-> timeline encode json-roundtrip)]
    (t/is (cta/valid-timeline? timeline))
    (t/is (= ["fade" "style"] (->> json :tracks vals first :animations (mapv :type))))
    (t/is (= timeline (decode json)))))

(t/deftest an-animation-of-no-known-type-is-not-valid
  (let [sid  (uuid/next)
        fade (cta/make-animation {:type :fade})
        with #(assoc-in (mk-timeline) [:tracks sid] {:shape-id sid :keyframes [] :animations [%]})]
    (doseq [animation [(assoc fade :type 5) (dissoc fade :type) (assoc fade :type "5")
                       (assoc fade :type :style)]]
      (let [tl (with animation)]
        (t/is (not (cta/valid-timeline? tl)) (pr-str (:type animation)))
        (t/is (some? (sm/explain cta/schema:timeline tl)))))))

(t/deftest builtin-styles-are-the-composites-and-the-text-presets
  (t/is (= #{:slide-up :slide-down :slide-left :slide-right :pop :zoom :spin
             :typewriter :rise-words :cascade-lines :pop-letters}
           (set (keys cta/builtin-styles)))))

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
    ;; keeping the last frame
    (t/is (re-find #"linear 1 both;" css))
    (t/testing "no shape, no CSS"
      (t/is (= "" (cta/timeline->css (fade-timeline shape :once) {}))))))

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

(defn- times-and-values
  [timeline shape-id property & [index]]
  (mapv (juxt :time :value) (cta/property-keyframes timeline shape-id property index)))

(defn- keyframes-are?
  "Whether `kfs` (`[time value]` pairs) are at `times` with `values`."
  [kfs times values]
  (and (= times (mapv first kfs))
       (every? true? (map close? values (map second kfs)))))

(t/deftest record-unanimated-plays-the-change
  (let [{:keys [board rect objects]} (board-and-rect)
        rid   (:id rect)
        tl    (cta/make-timeline {:board-id (:id board) :duration 1000})
        ;; the rect is at 50 in the board
        after (moved objects [rid] 30 0)
        tl'   (cta/record-unanimated tl objects after [rid] 500)]
    (t/testing "the value before at the start, the new one at the time"
      (t/is (keyframes-are? (times-and-values tl' rid :x) [0 500] [50 80]))
      (t/is (cta/valid-timeline? tl')))
    (t/testing "what does not change records nothing"
      (t/is (empty? (cta/property-keyframes tl' rid :y))))
    (t/testing "at the start, only the new value"
      (t/is (keyframes-are? (times-and-values (cta/record-unanimated tl objects after [rid] 0) rid :x)
                            [0] [80])))
    (t/testing "only the shapes given"
      (t/is (= tl (cta/record-unanimated tl objects after [] 500))))))

(t/deftest record-unanimated-leaves-the-animated-properties
  (let [{:keys [rect objects] :as scene} (board-and-rect)
        rid (:id rect)
        tl  (edit-timeline scene :x 0 200)
        ;; the rect is at 100 in the board
        tl' (cta/record-unanimated tl objects (moved objects [rid] 30 40) [rid] 500)]
    (t/is (= (cta/property-keyframes tl rid :x) (cta/property-keyframes tl' rid :x)))
    (t/is (keyframes-are? (times-and-values tl' rid :y) [0 500] [100 140]))))

(t/deftest record-unanimated-takes-opacity-and-turns
  (let [{:keys [board rect objects]} (board-and-rect)
        rid (:id rect)
        tl  (cta/make-timeline {:board-id (:id board) :duration 1000})]
    (t/is (keyframes-are? (times-and-values (cta/record-unanimated tl objects (assoc-in objects [rid :opacity] 0.3) [rid] 500)
                                            rid :opacity)
                          [0 500] [1 0.3]))
    (t/testing "the short way"
      (t/is (keyframes-are? (times-and-values (cta/record-unanimated tl objects (turned objects rid -20) [rid] 500)
                                              rid :rotation)
                            [0 500] [0 -20])))))

(t/deftest record-unanimated-records-a-fill-not-one-added
  (let [{:keys [board rect objects]} (board-and-rect)
        rid     (:id rect)
        objects (assoc-in objects [rid :fills] [{:fill-color "#000000" :fill-opacity 1}])
        tl      (cta/make-timeline {:board-id (:id board) :duration 1000})
        colors  #(times-and-values % rid :fill-color 0)]
    (t/is (= [[0 "#000000"] [500 "#ff0000"]]
             (colors (cta/record-unanimated tl objects (assoc-in objects [rid :fills 0 :fill-color] "#ff0000") [rid] 500))))
    (t/testing "a fill added on top moves the others down, no color changes"
      (t/is (= tl (cta/record-unanimated tl objects
                                         (update-in objects [rid :fills] #(into [{:fill-color "#ff0000" :fill-opacity 1}] %))
                                         [rid] 500))))))

(t/deftest record-unanimated-leaves-an-edited-path
  (let [{:keys [board rect objects]} (board-and-rect)
        rid   (:id rect)
        tl    (cta/make-timeline {:board-id (:id board) :duration 1000})
        after (-> (moved objects [rid] 30 0)
                  (assoc-in [rid :content] [:edited]))]
    (t/is (= tl (cta/record-unanimated tl objects after [rid] 500)))))

(t/deftest markers-name-moments
  (let [tl      (-> (mk-timeline)
                    (cta/add-marker {:time 600 :name "outro"})
                    (cta/add-marker {:time 200 :name "intro"})
                    (cta/add-marker {:time 1500}))
        [a b _] (:markers tl)
        names   #(mapv :name (:markers %))]
    (t/is (= ["intro" "outro" "Marker 3"] (names tl)) "in time order, named by their number")
    (t/is (= 1500 (:duration tl)) "the duration grows to hold them")
    (t/is (cta/valid-timeline? tl))
    (t/testing "moving one keeps them in order, none before the start"
      (t/is (= ["outro" "intro" "Marker 3"] (names (cta/update-marker tl (:id a) #(assoc % :time 900)))))
      (t/is (= 0 (-> (cta/update-marker tl (:id b) #(assoc % :time -50)) :markers first :time))))
    (t/testing "the playhead jumps between them"
      (t/is (= "outro" (:name (cta/marker-after tl 200))))
      (t/is (= "intro" (:name (cta/marker-before tl 600))))
      (t/is (nil? (cta/marker-before tl 200))))
    (t/testing "drags snap to them, but for the one dragged"
      (t/is (contains? (cta/snap-times tl) 600))
      (t/is (not (contains? (cta/snap-times tl :marker-ids #{(:id b)}) 600))))
    (t/testing "they go away"
      (t/is (not (contains? (reduce #(cta/remove-marker %1 (:id %2)) tl (:markers tl)) :markers))))))

(t/deftest lottie-plays-the-markers-by-name
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 40 :height 20})
        bid   (:id board)
        sid   (:id shape)
        tl    (-> (cta/make-timeline {:board-id bid :duration 1000 :playback :ping-pong})
                  (cta/add-keyframe sid {:time 0 :property :opacity :value 0})
                  (cta/add-keyframe sid {:time 1000 :property :opacity :value 1})
                  (cta/add-marker {:time 500 :name "loop"})
                  (cta/add-marker {:time 0 :name "intro"}))
        L     (cta/timeline->lottie tl {bid board sid shape})]
    ;; each up to the next, the last to the end and not past it on the way back
    (t/is (= ["intro" "loop"] (mapv :cm (:markers L))))
    (t/is (close-all? [[0 30] [30 30]] (mapv (juxt :tm :dr) (:markers L))))
    (t/testing "no markers, none in the document"
      (t/is (not (contains? (cta/timeline->lottie (dissoc tl :markers) {bid board sid shape}) :markers))))))

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
    (t/testing "the duration stays: they go past the end"
      (let [tl' (cta/shift-keyframes tl ids 1000)]
        (t/is (= 1000 (:duration tl')))
        (t/is (= [700 1100 1400] (sort (times tl'))))
        (t/is (cta/valid-timeline? tl'))))
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
    (t/testing "without the end, when the end is dragged"
      (t/is (= [0 100 400 500 700] (vec (cta/snap-times tl :end? false)))))
    (t/testing "the nearest within the threshold"
      (t/is (= 400 (cta/snap-time (cta/snap-times tl) 395 10)))
      (t/is (= 500 (cta/snap-time (cta/snap-times tl) 460 45)))
      (t/is (nil? (cta/snap-time (cta/snap-times tl) 250 10))))))

;; AUDIO CLIPS

(defn- audio-clip
  "A clip of a 4 s file from `start` on the timeline, playing `duration` ms
  of it from `offset`, with `params` over it."
  [& {:as params}]
  (ctau/make-audio-clip (merge {:audio-id (uuid/next) :name "beat.mp3" :source-duration 4000
                                :start 0 :offset 0 :duration 1000}
                               params)))

(defn- with-clips
  [timeline & clips]
  (reduce ctau/add-audio-clip timeline clips))

(defn- plan
  "The parts of the `audio-plan` of `timeline` as `[at offset length]`."
  [timeline elapsed span]
  (mapv (juxt :at :offset :length) (cta/audio-plan timeline elapsed span)))

(t/deftest a-timeline-with-audio-clips-validates
  (let [tl (with-clips (mk-timeline) (audio-clip :start 200 :volume 0.5 :fade-in 100))
        id (-> tl :audio-clips keys first)]
    (t/is (cta/valid-timeline? tl))
    (t/is (cta/valid-timeline? (assoc tl :export {:format :webm :audio false})))
    (doseq [[k v] [[:audio-id nil] [:start -1] [:offset -1] [:fade-out -1] [:duration 9]
                   [:volume 4.5] [:lane -1] [:source-duration (inc ctau/max-audio-duration)]
                   [:speed 0.1] [:speed 5]]]
      (t/is (not (cta/valid-timeline? (assoc-in tl [:audio-clips id k] v))) (str k " " v)))))

(t/deftest a-timeline-with-only-clips-is-not-empty
  (t/is (cta/timeline-empty? (mk-timeline)))
  (t/is (not (cta/timeline-empty? (with-clips (mk-timeline) (audio-clip)))))
  (t/is (not (cta/timeline-empty? (cta/add-keyframe (mk-timeline) (uuid/next) {:time 0 :property :x :value 0})))))

(t/deftest the-audio-plan-plays-what-sounds-ahead
  (let [a  (audio-clip :start 200 :duration 300)
        ;; longer than what is left of the timeline
        b  (audio-clip :start 600 :offset 1000 :duration 2000)
        tl (with-clips (mk-timeline) a b)]
    (t/is (= [[200 0 300] [600 1000 400]] (plan tl 0 1000)) "cut at the end of the timeline")
    (t/is (= [(:id a) (:id b)] (mapv :clip-id (cta/audio-plan tl 0 1000))))
    (t/is (= [(:audio-id a) (:audio-id b)] (mapv :audio-id (cta/audio-plan tl 0 1000))))
    (t/is (= [[0 100 200] [300 1000 400]] (plan tl 300 1000)) "a clip from part-way")
    (t/is (= [[0 0 100]] (plan tl 200 100)) "only the span")
    (t/is (= [] (plan tl 1000 1000)) "played once, nothing after the end")
    (t/testing "muted clips do not sound"
      (t/is (= [[200 0 300]] (plan (ctau/set-audio-clip-muted tl (:id b) true) 0 1000))))))

(t/deftest the-audio-plan-repeats-the-clips-of-a-loop
  (let [tl (-> (mk-timeline)
               (assoc :playback :loop)
               (with-clips (audio-clip :start 200 :duration 300)))]
    (t/is (= [[200 0 300] [1200 0 300] [2200 0 300]] (plan tl 0 3000)))
    (t/is (= [[0 100 200] [900 0 300]] (plan tl 2300 1500)) "from within a cycle")))

(t/deftest the-way-back-of-ping-pong-is-silent
  (let [tl (-> (mk-timeline)
               (assoc :playback :ping-pong)
               (with-clips (audio-clip :start 200 :duration 300)))]
    (t/is (= [[200 0 300] [2200 0 300]] (plan tl 0 4000)))
    (t/is (= [] (plan tl 1000 1000)) "the way back")
    (t/is (= [[200 0 300]] (plan tl 2000 1000)) "forward again")))

(t/deftest the-audio-plan-reads-a-fast-clip-faster
  (let [tl (with-clips (mk-timeline) (audio-clip :offset 500 :duration 1000 :speed 2))]
    (t/is (= [{:at 0 :offset 500 :length 1000 :speed 2}]
             (mapv #(select-keys % [:at :offset :length :speed]) (cta/audio-plan tl 0 1000)))
          "a second of the timeline plays two of the file")
    (t/is (= [[0 1100 700]] (plan tl 300 1000)) "from part-way, twice as far into the file")
    (t/is (= [1] (mapv :speed (cta/audio-plan (with-clips (mk-timeline) (audio-clip)) 0 1000))))))

(t/deftest the-audio-plan-gives-the-gain-of-each-part
  (let [tl (with-clips (mk-timeline) (audio-clip :duration 1000 :volume 0.5 :fade-in 200))]
    (t/is (= [[0 0.0] [200 0.5] [1000 0.5]] (:points (first (cta/audio-plan tl 0 1000)))))
    (t/is (= [[0 0.25] [100 0.5] [500 0.5]] (:points (first (cta/audio-plan tl 100 500))))
          "from within its fade in")))

(t/deftest snap-times-take-the-edges-of-the-clips
  (let [a  (audio-clip :start 200 :duration 300)
        b  (audio-clip :start 600 :duration 100)
        tl (with-clips (mk-timeline) a b)]
    (t/is (= [0 200 500 600 700 1000] (vec (cta/snap-times tl))))
    (t/is (= [0 600 700 1000] (vec (cta/snap-times tl :clip-ids #{(:id a)}))) "but of the dragged ones")))

(t/deftest loop-track-repeats-a-track-every-period
  (let [id     (uuid/next)
        track  (-> (cta/make-timeline {:board-id (uuid/next) :duration 3000})
                   (cta/add-keyframe id {:time 200 :property :x :value 0 :easing :linear})
                   (cta/add-keyframe id {:time 600 :property :x :value 100})
                   (cta/get-track id))
        looped (cta/loop-track track 1000)
        tl     (-> (cta/make-timeline {:board-id (uuid/next) :duration 3000})
                   (assoc-in [:tracks id] looped))]
    (t/is (= #{:x} (:loops looped)))
    ;; closed at the start and at the end of the period by the values
    ;; held there
    (t/is (= [0 200 600 1000] (mapv :time (:keyframes looped))))
    (t/is (= [0 0 100 100] (mapv :value (:keyframes looped))))
    ;; 1400 ms is 400 ms into the second round
    (t/is (mth/close? 50 (get-in (cta/values-at tl 1400) [id :x])))
    (let [longer (update track :keyframes conj
                         (cta/make-keyframe {:time 1500 :property :x :value 300}))]
      (t/is (= 1000 (:time (last (:keyframes (cta/loop-track longer 1000)))))
            "cut at the period"))))

;; RETARGETING: the animation of a layer onto another one

(defn- pose-values
  [pose]
  ((juxt :x :y :width :height :rotation) pose))

(defn- pose
  [x y width height rotation]
  {:x x :y y :width width :height height :rotation rotation})

(defn- track-with
  "The track, of a new shape, of the keyframes `[property time value
  extra]`."
  [& keyframes]
  (let [id (uuid/next)]
    (-> (reduce (fn [timeline [property time value extra]]
                  (cta/add-keyframe timeline id (merge {:time time :property property :value value}
                                                       extra)))
                (mk-timeline)
                keyframes)
        (cta/get-track id))))

(defn- values-of
  "The values of the keyframes of `property` of `track`, by time."
  ([track property]
   (values-of track property nil))
  ([track property index]
   (->> (:keyframes track)
        (filter #(cta/same-slot? % property index))
        (sort-by :time)
        (mapv :value))))

(def ^:private styled-rect
  (cts/setup-shape
   {:type :rect :x 0 :y 0 :width 100 :height 50
    :fills [{:fill-color "#000000" :fill-opacity 1}]
    :strokes [{:stroke-color "#111111" :stroke-opacity 1 :stroke-width 1}]
    :shadow [{:id (uuid/next) :style :drop-shadow
              :offset-x 0 :offset-y 0 :blur 0 :spread 0
              :hidden false
              :color {:color "#000000" :opacity 1}}]
    :blur {:id (uuid/next) :type :layer-blur :value 0 :hidden false}}))

(t/deftest rest-pose-is-where-a-shape-rests-in-its-board
  (let [{:keys [board rect objects]} (board-and-rect)
        timeline (cta/make-timeline {:board-id (:id board)})]
    (t/is (close-all? [50 100 100 50 0]
                      (pose-values (cta/rest-pose timeline objects (:id rect)))))
    (t/testing "a board rests where it is on the canvas"
      (t/is (close-all? [100 100 800 600 0]
                        (pose-values (cta/rest-pose timeline objects (:id board))))))
    (t/testing "with its rotation"
      (let [objects (turned objects (:id rect) 30)]
        (t/is (close? 30 (:rotation (cta/rest-pose timeline objects (:id rect)))))))))

(t/deftest retarget-track-moves-positions-to-the-target
  (let [track  (track-with [:x 0 50] [:x 1000 150] [:y 0 20] [:y 1000 120])
        result (cta/retarget-track track (pose 50 20 100 50 0)
                                   styled-rect (pose 300 70 100 50 0))]
    (t/is (close-all? [300 400] (values-of (:track result) :x)))
    (t/is (close-all? [70 170] (values-of (:track result) :y)))
    (t/is (= 0 (:skipped result)))
    (t/is (= (:id styled-rect) (get-in result [:track :shape-id])))))

(t/deftest retarget-track-turns-rotation-from-the-target-rotation
  (let [track  (track-with [:rotation 0 0] [:rotation 1000 90])
        result (cta/retarget-track track (pose 0 0 100 50 0) styled-rect (pose 0 0 100 50 45))]
    (t/is (close-all? [45 135] (values-of (:track result) :rotation)))))

(t/deftest retarget-track-scales-sizes-to-the-target
  (let [track (track-with [:width 0 100] [:width 1000 200] [:height 0 50] [:height 1000 25])]
    (t/is (close-all? [50 100]
                      (values-of (:track (cta/retarget-track track (pose 0 0 100 50 0)
                                                             styled-rect (pose 0 0 50 100 0)))
                                 :width)))
    (t/is (close-all? [100 50]
                      (values-of (:track (cta/retarget-track track (pose 0 0 100 50 0)
                                                             styled-rect (pose 0 0 50 100 0)))
                                 :height)))
    (t/testing "a source without width keeps the values"
      (t/is (close-all? [100 200]
                        (values-of (:track (cta/retarget-track track (pose 0 0 0 0 0)
                                                               styled-rect (pose 0 0 50 100 0)))
                                   :width))))))

(t/deftest retarget-track-keeps-factors-and-looks
  (let [track  (track-with [:scale-x 0 0.5] [:scale-x 1000 1]
                           [:opacity 0 0] [:opacity 1000 1]
                           [:r1 0 0] [:r1 1000 12]
                           [:fill-color 0 "#000000" {:index 0}] [:fill-color 1000 "#ff0000" {:index 0}])
        result (cta/retarget-track track (pose 0 0 100 50 0) styled-rect (pose 300 70 50 25 45))]
    (t/is (close-all? [0.5 1] (values-of (:track result) :scale-x)))
    (t/is (close-all? [0 1] (values-of (:track result) :opacity)))
    (t/is (close-all? [0 12] (values-of (:track result) :r1)))
    (t/is (= ["#000000" "#ff0000"] (values-of (:track result) :fill-color 0)))))

(t/deftest retarget-track-keeps-exact-values
  (let [track  (track-with [:x 0 50] [:rotation 0 10] [:width 0 100])
        result (cta/retarget-track track (pose 50 0 100 50 0) styled-rect (pose 300 70 50 25 45)
                                   :exact? true)]
    (t/is (close-all? [50] (values-of (:track result) :x)))
    (t/is (close-all? [10] (values-of (:track result) :rotation)))
    (t/is (close-all? [100] (values-of (:track result) :width)))))

(t/deftest retarget-track-leaves-out-what-the-target-cannot-show
  (let [track (track-with [:x 0 0] [:x 1000 10]
                          [:fill-color 0 "#000000" {:index 1}]
                          [:fill-color 1000 "#ffffff" {:index 1}]
                          [:r1 0 0]
                          [:blur 0 4]
                          [:trim-end 0 0.5])
        from  (pose 0 0 100 50 0)]
    (t/testing "a fill slot the target does not have"
      (let [result (cta/retarget-track track from styled-rect from)]
        (t/is (= [] (values-of (:track result) :fill-color 1)))
        (t/is (= 2 (:skipped result)))))
    (t/testing "radius on an ellipse, blur and trim on a shape without them"
      (let [ellipse (cts/setup-shape {:type :circle :x 0 :y 0 :width 100 :height 50})
            result  (cta/retarget-track track from ellipse from)]
        (t/is (= #{:x} (into #{} (map :property) (get-in result [:track :keyframes]))))
        (t/is (= 5 (:skipped result)))))
    (t/testing "a fill that is a gradient takes no colour"
      (let [gradient (assoc styled-rect :fills [{:fill-color-gradient {:type :linear}}
                                                {:fill-color "#000000"}])
            fill-0   (track-with [:fill-color 0 "#ff0000" {:index 0}] [:fill-color 0 "#ff0000" {:index 1}])
            result   (cta/retarget-track fill-0 from gradient from)]
        (t/is (= [1] (mapv :index (get-in result [:track :keyframes]))))
        (t/is (= 1 (:skipped result)))))
    (t/testing "a text keeps its position, size, rotation and opacity"
      (let [text   (cts/setup-shape {:type :text :x 0 :y 0 :width 100 :height 20})
            moving (track-with [:x 0 0] [:y 0 0] [:width 0 100] [:height 0 20]
                               [:rotation 0 0] [:opacity 0 1])
            result (cta/retarget-track moving from text from)]
        (t/is (= #{:x :y :width :height :rotation :opacity}
                 (into #{} (map :property) (get-in result [:track :keyframes]))))
        (t/is (= 0 (:skipped result)))))))

(t/deftest retarget-track-makes-a-new-unlocked-track
  (let [id       (uuid/next)
        timeline (-> (mk-timeline)
                     (cta/add-keyframe id {:time 0 :property :x :value 0})
                     (cta/add-keyframe id {:time 500 :property :x :value 10})
                     (cta/add-keyframe id {:time 0 :property :opacity :value 1})
                     (cta/add-animation id {:type :fade :start 0 :duration 300})
                     (cta/toggle-loop id :x)
                     (cta/toggle-slot-flag id :hidden :opacity)
                     (cta/set-track-origin id 0 1)
                     (cta/toggle-slot-flag id :locked :x))
        track    (cta/get-track timeline id)
        result   (:track (cta/retarget-track track (pose 0 0 100 50 0) styled-rect (pose 0 0 100 50 0)))
        old-ids  (into #{} (map :id) (concat (:keyframes track) (:animations track)))]
    (t/is (= #{:x} (:loops result)))
    (t/is (nil? (:hidden result)))
    (t/is (nil? (:locked result)))
    (t/is (= {:x 0.0 :y 1.0} (:origin result)))
    (t/is (= 1 (count (:animations result))))
    (t/is (not-any? old-ids (map :id (concat (:keyframes result) (:animations result)))))
    (t/is (sm/validate cta/schema:track result))))

(t/deftest keyframe-value-fits-its-property
  (t/is (cta/keyframe-value-fits? {:property :x :value 10}))
  (t/is (cta/keyframe-value-fits? {:property :fill-color :value "#ff00ff"}))
  (t/is (not (cta/keyframe-value-fits? {:property :x :value "#ffffff"})))
  (t/is (not (cta/keyframe-value-fits? {:property :fill-color :value 3}))))

;; TEXT SPLITS

(defn- text-content
  "Text content of `paragraphs`, one leaf each."
  [& paragraphs]
  {:type "root"
   :children [{:type "paragraph-set"
               :children (mapv (fn [text] {:type "paragraph" :children [{:text text}]})
                               paragraphs)}]})

(defn- board-and-text
  "A board at 100,100 with a text of `paragraphs` at 50,100 inside it."
  [& paragraphs]
  (let [board (cts/setup-shape {:type :frame :x 100 :y 100 :width 800 :height 600})
        text  (-> (cts/setup-shape {:type :text :x 150 :y 200 :width 300 :height 40})
                  (assoc :content (apply text-content paragraphs)))
        bid   (:id board)]
    {:board board
     :text text
     :objects {bid (assoc board :shapes [(:id text)])
               (:id text) (assoc text :frame-id bid :parent-id bid)}}))

(defn- text-timeline
  "A timeline of `board` with `animations` on the shape `sid`."
  [board sid & animations]
  (reduce #(cta/add-animation %1 sid %2)
          (cta/make-timeline {:board-id (:id board) :duration 2000})
          animations))

(defn- text-units-at
  "The `:text-units` the animation of `timeline` gives `sid` at `time`,
  `::none` without."
  [timeline objects sid time]
  (-> (cta/timeline->modif-tree timeline objects time)
      (get-in [sid :modifiers])
      (cta/appearance-changes)
      (get :text-units ::none)))

(defn- ranges
  [entries]
  (mapv (juxt :paragraph :start :end) entries))

(t/deftest a-split-animation-keeps-its-settings
  (let [sid       (uuid/next)
        tl        (cta/add-animation (mk-timeline) sid {:type :fade :split :word})
        aid       (:id (first (get-in tl [:tracks sid :animations])))
        animation (cta/get-animation tl sid aid)
        settings  #(select-keys % [:split :spread :order :seed])]
    (t/is (= {:split :word :spread 0.5 :order :forward} (settings animation))
          "half spread and in reading order by default")
    (t/is (cta/valid-timeline? tl))
    (t/is (= animation (cta/make-animation animation)))
    (t/is (= {:split :word :spread 0.2 :order :edges}
             (settings (-> tl
                           (cta/update-animation sid aid #(assoc % :duration 900 :spread 0.2 :order :edges))
                           (cta/get-animation sid aid)))))))

(t/deftest split-settings-are-made-valid
  (t/is (= 1 (:spread (cta/make-animation {:type :fade :split :char :spread 3}))))
  (t/is (= 0 (:spread (cta/make-animation {:type :fade :split :char :spread -1}))))
  (t/is (= {} (select-keys (cta/make-animation {:type :fade :split :letter :spread 1 :order :edges})
                           [:split :spread :order :seed]))
        "no split settings without a known split")
  (t/is (not (sm/validate cta/schema:preset-animation
                          (assoc (cta/make-animation {:type :fade}) :split :letter))))
  (let [animation (cta/make-animation {:type :fade :split :word :order :random})]
    (t/is (int? (:seed animation)) "a random order gets a seed")
    (t/is (= (:seed animation) (:seed (cta/make-animation animation))) "and keeps it")
    (t/is (sm/validate cta/schema:preset-animation animation))))

(t/deftest a-split-fade-in-holds-its-units-until-they-start
  (let [{:keys [board text objects]} (board-and-text "Hi you there")
        sid (:id text)
        tl  (text-timeline board sid {:type :fade :split :word :start 100 :duration 1000})
        at  #(text-units-at tl objects sid %)]
    (t/is (= [[0 0 2] [0 3 6] [0 7 12]] (ranges (at 0))))
    (t/is (every? #(and (= :word (:kind %)) (zero? (:opacity %))) (at 0))
          "an entrance holds its effect, as on a layer")
    (t/is (apply > (map :opacity (at 400))) "the units start one after the other")
    (t/is (= [] (at 1100)) "at rest once it ends, and there")
    (t/is (= [] (at 1600)))))

(t/deftest a-spread-1-fade-ends-at-rest
  (let [{:keys [board text objects]} (board-and-text "Hi you there")
        sid (:id text)
        tl  (text-timeline board sid {:type :fade :split :word :spread 1 :start 100 :duration 1000})]
    (t/is (= [[0 7 12]] (ranges (text-units-at tl objects sid 1050))) "the last unit plays last")
    (t/is (= [] (text-units-at tl objects sid 1100)))))

(t/deftest split-animations-stay-on-the-track-of-a-text
  (let [{:keys [board text objects]} (board-and-text "Hi you")
        sid   (:id text)
        track #(cta/get-track (cta/resolve-animations % objects) sid)]
    (t/testing "a whole move gives keyframes, a split fade none"
      (let [resolved (track (text-timeline board sid {:type :fade :split :word} {:type :move}))]
        (t/is (= #{:x :y} (into #{} (map :property) (:keyframes resolved))))
        (t/is (= [[:fade :word]] (mapv (juxt :type :split) (:animations resolved))))))
    (t/testing "a text with split animations only keeps its track"
      (let [resolved (track (text-timeline board sid {:type :fade :split :word}))]
        (t/is (= [] (:keyframes resolved)))
        (t/is (= 1 (count (:animations resolved))))))))

(t/deftest a-split-animation-plays-whole-on-a-layer-that-is-no-text
  (let [{:keys [board rect objects]} (board-and-rect)
        sid   (:id rect)
        fade  {:type :fade :start 200 :duration 400}
        split (text-timeline board sid (assoc fade :split :word))
        whole (text-timeline board sid fade)
        times [0 300 500 700]]
    (t/is (= (mapv #(values-at whole objects %) times)
             (mapv #(values-at split objects %) times)))
    (t/is (not (contains? (cta/get-track (cta/resolve-animations split objects) sid) :animations)))
    (t/is (= ::none (text-units-at split objects sid 300)))))

(t/deftest a-hidden-split-animation-plays-nothing
  (let [{:keys [board text objects]} (board-and-text "Hi you")
        sid (:id text)
        tl  (text-timeline board sid {:type :fade :split :word})
        aid (:id (first (get-in tl [:tracks sid :animations])))
        tl  (cta/toggle-animation-flag tl sid aid :hidden)]
    (t/is (= ::none (text-units-at tl objects sid 0)))
    (t/is (= {} (cta/text-unit-entries tl objects 0)))
    (t/is (nil? (cta/get-track (cta/resolve-animations tl objects) sid)))))

(t/deftest split-animations-of-a-unit-play-together
  (let [{:keys [board text objects]} (board-and-text "Hi you")
        sid     (:id text)
        tl      (text-timeline board sid
                               {:type :fade :split :word :start 100}
                               {:type :move :split :word :start 100 :offset-x 0 :offset-y 30})
        entries (text-units-at tl objects sid 0)]
    (t/is (= [[0 0 2] [0 3 6]] (ranges entries)) "one entry a word")
    (t/is (every? #(and (zero? (:opacity %)) (== 30 (:dy %)) (zero? (:dx %))) entries))))

(t/deftest a-text-plays-one-kind-of-units
  (let [{:keys [board text objects]} (board-and-text "Hi you")
        sid      (:id text)
        tl       (text-timeline board sid
                                {:type :fade :split :word :start 100}
                                {:type :move :split :char :start 100})
        entries  (text-units-at tl objects sid 0)
        resolved (cta/get-track (cta/resolve-animations tl objects) sid)]
    (t/is (= [[0 0 2] [0 3 6]] (ranges entries)) "by words, the first kind")
    (t/is (every? #(zero? (:dy %)) entries) "the move by characters plays whole")
    (t/is (= #{:x :y} (into #{} (map :property) (:keyframes resolved))))
    (t/is (= [:word] (mapv :split (:animations resolved))))))

(t/deftest a-text-past-the-live-cap-plays-its-splits-whole
  (let [{:keys [board text objects]} (board-and-text (apply str (repeat (inc tu/live-unit-cap) "a")))
        sid      (:id text)
        tl       (text-timeline board sid {:type :fade :split :char})
        resolved (cta/get-track (cta/resolve-animations tl objects) sid)]
    (t/is (= ::none (text-units-at tl objects sid 0)))
    (t/is (= #{:opacity} (into #{} (map :property) (:keyframes resolved))))
    (t/is (not (contains? resolved :animations)))))

(t/deftest lines-come-from-the-position-data
  (let [{:keys [board text objects]} (board-and-text "Hi you" "there")
        sid  (:id text)
        tl   (text-timeline board sid {:type :fade :split :line :start 100})
        rest {:dx 0 :dy 0 :rotation 0 :scale-x 1 :scale-y 1 :opacity 0 :blur 0}]
    (t/is (= [(merge {:kind :line :index 0 :line-count 1} rest)]
             (text-units-at tl objects sid 0))
          "without position data, the text is one line")
    (let [line    (fn [y text] {:x 150 :y y :width 100 :height 20 :text text :fills []})
          objects (assoc-in objects [sid :position-data] [(line 220 "Hi you") (line 240 "there")])]
      (t/is (= [(merge {:kind :line :index 0 :line-count 2} rest)
                (merge {:kind :line :index 1 :line-count 2} rest)]
               (text-units-at tl objects sid 0))))))

(t/deftest a-split-entry-of-a-style-use-plays-as-the-split-preset
  (let [entry {:type :fade :split :word :spread 0.8 :start 0 :duration 600}
        use   (cta/make-animation {:type :style :style-ref-id (uuid/next) :style-ref-file (uuid/next)
                                   :name "Words" :start 100 :entries [entry]})
        times [0 150 300 500 700 1000]]
    (t/testing "on a text, by its units"
      (let [{:keys [board text objects]} (board-and-text "Hi you there")
            sid    (:id text)
            styled (text-timeline board sid use)
            plain  (text-timeline board sid (assoc entry :start 100))]
        (t/is (seq (get (cta/text-unit-entries styled objects 300) sid)))
        (t/is (= (mapv #(cta/text-unit-entries plain objects %) times)
                 (mapv #(cta/text-unit-entries styled objects %) times)))))
    (t/testing "on a rectangle, whole, the use keeping all its entries"
      (let [{:keys [board rect objects]} (board-and-rect)
            sid    (:id rect)
            styled (text-timeline board sid use)
            whole  (text-timeline board sid (-> entry (dissoc :split :spread) (assoc :start 100)))]
        (t/is (= (:entries use) (get-in styled [:tracks sid :animations 0 :entries])))
        (t/is (= (mapv #(values-at whole objects %) times)
                 (mapv #(values-at styled objects %) times)))))))

(t/deftest appearance-changes-hold-the-units-of-a-text
  (t/is (= {:text-units []} (cta/appearance-changes (ctm/change-property nil :text-units [])))))

(t/deftest text-presets-split-their-animations
  (doseq [preset [:typewriter :rise-words :cascade-lines :pop-letters]
          :let [animations (mapv cta/make-animation (get cta/builtin-styles preset))]]
    (t/is (seq animations) (name preset))
    (t/is (every? #(and (:split %) (sm/validate cta/schema:preset-animation %)) animations)
          (name preset))))

(defn- markup-of
  "The CSS, SVG and Lottie exports of `timeline`."
  [timeline objects]
  [(cta/timeline->css timeline objects)
   (cta/timeline->svg timeline objects {})
   (cta/timeline->lottie timeline objects nil)])

(t/deftest markup-exports-play-split-animations-whole
  (let [{:keys [board text objects]} (board-and-text "Hi you there")
        sid  (:id text)
        fade {:type :fade :start 100 :duration 600}
        use  #(cta/make-animation {:type :style :style-ref-id (uuid/next) :style-ref-file (uuid/next)
                                   :name "Words" :start 100 :entries [%]})]
    (t/testing "a preset"
      (let [split (text-timeline board sid (assoc fade :split :word))]
        (t/is (cta/split-animations? split objects))
        (t/is (= (markup-of (text-timeline board sid fade) objects) (markup-of split objects)))))
    (t/testing "an entry of a style use"
      (let [split (text-timeline board sid (use (assoc fade :split :word :start 0)))]
        (t/is (cta/split-animations? split objects))
        (t/is (= (markup-of (text-timeline board sid (use (assoc fade :start 0))) objects)
                 (markup-of split objects)))
        (t/is (str/includes? (first (markup-of split objects)) "opacity") "the text is not static")))
    (t/testing "none on a layer that is no text"
      (let [{:keys [board rect objects]} (board-and-rect)]
        (t/is (not (cta/split-animations? (text-timeline board (:id rect) (assoc fade :split :word))
                                          objects)))))))

(t/deftest flatten-splits-leaves-no-split-keys
  (let [sid       (uuid/next)
        tl        (-> (mk-timeline)
                      (cta/add-animation sid {:type :fade :split :char :order :random})
                      (cta/add-animation sid {:type :style :style-ref-id (uuid/next) :style-ref-file (uuid/next)
                                              :entries [{:type :move :split :line :spread 1}]}))
        flattened (cta/flatten-splits tl)
        keys-of   (fn [animation] (into #{} (keys animation)))]
    (t/is (not-any? #(some (keys-of %) [:split :spread :order :seed])
                    (concat (get-in flattened [:tracks sid :animations])
                            (get-in flattened [:tracks sid :animations 1 :entries]))))
    (t/is (cta/valid-timeline? flattened))))

(t/deftest retarget-track-keeps-split-presets-on-a-text-only
  (let [track  {:shape-id (uuid/next)
                :keyframes []
                :animations [(cta/make-animation {:type :fade :split :word})
                             (cta/make-animation {:type :move})]}
        from   (pose 0 0 100 50 0)
        text   (cts/setup-shape {:type :text :x 0 :y 0 :width 100 :height 50})
        onto   #(cta/retarget-track track from % from)]
    (t/testing "onto a rectangle the move goes, the split fade not"
      (let [{:keys [track skipped]} (onto styled-rect)]
        (t/is (= [:move] (mapv :type (:animations track))))
        (t/is (= 1 skipped))))
    (t/testing "onto a text both go"
      (let [{:keys [track skipped]} (onto text)]
        (t/is (= [[:fade :word] [:move nil]] (mapv (juxt :type :split) (:animations track))))
        (t/is (= 0 skipped))))))

(t/deftest a-text-changes-its-kind-of-units-on-all-its-splits
  (let [[fade move scale locked]
        [(cta/make-animation {:type :fade :split :word})
         (cta/make-animation {:type :move :split :word})
         (cta/make-animation {:type :scale})
         (cta/make-animation {:type :rotate :split :word :locked true})]
        animations [fade move scale locked]
        kinds      #(mapv :split %)]
    (t/is (= [:char :char nil :word] (kinds (cta/with-split-kind animations (:id fade) :char cta/make-animation)))
          "the others that split follow, the whole one and the locked one stay")
    (t/is (= [nil :word nil :word] (kinds (cta/with-split-kind animations (:id fade) nil cta/make-animation)))
          "back to the whole layer, only that one")
    (t/is (= [:line :line :line :word] (kinds (cta/with-split-kind animations (:id scale) :line cta/make-animation))))
    (t/is (not-any? #(contains? % :hidden) (cta/with-split-kind animations (:id fade) :char cta/make-style-entry))
          "entries stay entries")))

(t/deftest the-text-presets-are-the-ones-that-split
  (t/is (= #{:typewriter :rise-words :cascade-lines :pop-letters} cta/text-styles)))

(t/deftest units-follow-a-text-edited-while-it-plays
  (let [{:keys [board text objects]} (board-and-text "Hi you there")
        sid  (:id text)
        tl   (text-timeline board sid {:type :fade :split :word :start 0 :duration 1000})
        at   #(get (cta/text-unit-entries tl (assoc-in objects [sid :content] (apply text-content %&)) 300) sid)]
    (t/is (= 3 (count (at "Hi you there"))))
    (t/is (= 1 (count (at "Hi"))) "fewer units")
    (t/is (= 5 (count (at "Hi you there and on"))) "more units")
    (t/is (= [] (at "")) "none")
    (t/is (= 3 (count (at "Hi you there"))) "and back")))

(t/deftest a-text-at-the-live-cap-still-plays-its-units
  (let [{:keys [board text objects]} (board-and-text (apply str (repeat tu/live-unit-cap "a")))
        sid (:id text)
        tl  (text-timeline board sid {:type :fade :split :char :start 100})]
    (t/is (= tu/live-unit-cap (count (text-units-at tl objects sid 0))))))

;; BLUR

(defn- blur-at
  [timeline objects sid time]
  (-> (cta/timeline->modif-tree timeline objects time)
      (get-in [sid :modifiers])
      (cta/appearance-changes)
      (get :blur ::none)))

(t/deftest a-blur-animation-blurs-any-layer
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (text-timeline board sid {:type :blur :start 100 :duration 400})]
    (t/is (= 8 (:amount (first (get-in tl [:tracks sid :animations])))) "8 px by default")
    (t/is (= {:type :layer-blur :value 8 :hidden false} (dissoc (blur-at tl objects sid 0) :id))
          "a layer without a blur gets one")
    (t/is (< 0 (:value (blur-at tl objects sid 300)) 8))
    (t/is (nil? (blur-at tl objects sid 500)) "and none once it is sharp")
    (t/testing "a layer with a blur of its own ends at it"
      (let [objects (assoc-in objects [sid :blur] {:id (uuid/next) :type :layer-blur :value 4 :hidden false})]
        (t/is (= 12 (:value (blur-at tl objects sid 0))))
        (t/is (= 4 (:value (blur-at tl objects sid 500))))))
    (t/is (str/includes? (cta/timeline->css tl objects) "filter: blur(") "CSS blurs it")))

(t/deftest the-units-of-a-text-blur
  (let [{:keys [board text objects]} (board-and-text "Hi you")
        sid (:id text)
        tl  (text-timeline board sid {:type :blur :split :word :start 100 :duration 400 :amount 6})]
    (t/is (= [6 6] (mapv :blur (text-units-at tl objects sid 0))))
    (t/is (= [] (text-units-at tl objects sid 500)))))
