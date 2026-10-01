;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.plugins.motion-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.test-helpers.components :as cthc]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.store :as st]
   [app.plugins.api :as api]
   [app.plugins.motion :as motion]
   [app.plugins.register :as r]
   [app.plugins.shape :as shape]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]
   [potok.v2.core :as ptk]))

(def ^:private plugin-id "00000000-0000-0000-0000-000000000000")

(defn- setup
  "A store with `file` (an empty one by default), Motion on when
  `motion?`, and the plugin context: `[store context]`."
  ([motion?]
   (setup (cthf/sample-file :file1 :page-label :page1) motion?))
  ([file motion?]
   (let [store       (ths/setup-store file)
         ^js context (api/create-context plugin-id)]
     ;; as `app.plugins` does, which the tests leave out with the runtime
     (set! motion/shape-proxy shape/shape-proxy)
     (set! motion/shape-proxy? shape/shape-proxy?)
     (set! st/state store)
     (ptk/emit! store #(cond-> (assoc-in % [:plugins :flags plugin-id :throw-validation-errors] true)
                         motion? (update :features (fnil conj #{}) "animation/v1")))
     [store context])))

(defn- stored-timelines
  [store ^js context]
  (let [file-id (aget (. context -currentFile) "$id")
        page-id (aget (. context -currentPage) "$id")]
    (get-in @store [:files file-id :data :pages-index page-id :timelines])))

(defn- stored-track
  [store context ^js board ^js shape]
  (get-in (stored-timelines store context) [(aget board "$id") :tracks (aget shape "$id")]))

(defn- board-with-rect
  "A board with a rectangle inside: `[board rect]`."
  [^js context]
  (let [^js board (.createBoard context)
        ^js rect  (.createRectangle context)]
    (.appendChild board rect)
    [board rect]))

(t/deftest property-names-cover-the-model
  (t/is (= cta/animatable-properties (set (keys motion/property-names)))))

(t/deftest board-gets-a-timeline
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            [^js board ^js rect] (board-with-rect context)]

        (t/testing "a board has no animation until it gets one"
          (t/is (nil? (.-timeline board)))
          (t/is (zero? (.-length (.-timelines (. context -currentPage))))))

        (let [^js timeline (.addTimeline board #js {:duration 2000 :playback "loop"})]
          (t/testing "it takes the settings given"
            (t/is (= 2000 (.-duration timeline)))
            (t/is (= "loop" (.-playback timeline)))
            (t/is (= :loop (get-in (stored-timelines store context) [(aget board "$id") :playback]))))

          (t/testing "the board and the page find it"
            (t/is (some? (.-timeline board)))
            (t/is (= 1 (.-length (.-timelines (. context -currentPage)))))
            (t/is (= (aget board "$id") (aget (.-board timeline) "$id"))))

          (t/testing "its settings change"
            (set! (.-duration timeline) 3000)
            (set! (.-playback timeline) "ping-pong")
            (set! (.-name timeline) "Intro")
            (t/is (= {:duration 3000 :playback :ping-pong :name "Intro"}
                     (select-keys (get (stored-timelines store context) (aget board "$id"))
                                  [:duration :playback :name])))
            (t/is (thrown? js/Error (set! (.-playback timeline) "bounce")))
            (t/is (thrown? js/Error (set! (.-duration timeline) -1))))

          (t/testing "a layer is no board with an animation"
            (t/is (thrown? js/Error (.addTimeline rect))))

          (t/testing "it goes away"
            (.remove timeline)
            (t/is (empty? (stored-timelines store context)))
            (t/is (nil? (.-timeline board)))))))))

(t/deftest keyframes-animate-the-layers
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]  (setup true)
            [^js board ^js rect] (board-with-rect context)
            ^js timeline         (.addTimeline board)
            ^js from             (.addKeyframe timeline rect #js {:property "x" :time 0 :value 0 :easing "linear"})
            ^js to               (.addKeyframe timeline rect #js {:property "x" :time 1000 :value 100})]

        (t/testing "they read as given, the easing `ease` by default"
          (t/is (= ["x" 0 0 "linear"] [(.-property from) (.-time from) (.-value from) (.-easing from)]))
          (t/is (= "ease" (.-easing to)))
          (t/is (= 2 (count (:keyframes (stored-track store context board rect)))))
          (t/is (= 2 (.-length (.-keyframes timeline)))))

        (t/testing "the animation gives the values between them"
          (t/is (mth/close? 50 (.valueAt timeline rect "x" 500)))
          (t/is (nil? (.valueAt timeline rect "y" 500))))

        (t/testing "they change"
          (set! (.-value to) 200)
          (t/is (mth/close? 100 (.valueAt timeline rect "x" 500)))
          (set! (.-easing from) "hold")
          (t/is (= "hold" (.-easing from)))
          (t/is (= 0 (.valueAt timeline rect "x" 500)))
          (set! (.-easing from) #js {:type "spring" :stiffness 170 :damping 26 :mass 1})
          (t/is (= {:type :spring :stiffness 170 :damping 26 :mass 1}
                   (:easing (first (:keyframes (stored-track store context board rect))))))
          (set! (.-time to) 2500)
          (t/is (= 2500 (.-duration timeline)) "the timeline grows to hold it"))

        (t/testing "fills, strokes and shadows take an index"
          (let [^js fill (.addKeyframe timeline rect #js {:property "fillColor" :time 0 :value "#ff0000"})]
            (t/is (= 0 (.-index fill)))
            (t/is (thrown? js/Error (set! (.-value fill) 12)))))

        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "color" :time 0 :value 1})))
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time -5 :value 1})))
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time 0 :value "far"})))
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time 0 :value 1 :index 2})))
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time 0 :value 1 :easing "bouncy"})))
          (t/is (thrown? js/Error (.addKeyframe timeline (.createRectangle context) #js {:property "x" :time 0 :value 1}))
                "a layer outside the board"))

        (t/testing "they go away"
          (.remove from)
          (t/is (= 2 (count (:keyframes (stored-track store context board rect)))))
          (.clear timeline rect)
          (t/is (nil? (stored-track store context board rect))))))))

(t/deftest preset-animations-play-effects
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]  (setup true)
            [^js board ^js rect] (board-with-rect context)
            ^js timeline         (.addTimeline board)
            ^js fade             (.addAnimation timeline rect #js {:type "fade" :start 100 :duration 400})]

        (t/testing "they take the defaults of their type"
          (t/is (= ["fade" "in" 100 400] [(.-type fade) (.-direction fade) (.-start fade) (.-duration fade)]))
          (t/is (= "ease-out" (.-easing fade)) "entrances ease out"))

        (t/testing "they change"
          (set! (.-direction fade) "out")
          (t/is (= "ease-in" (.-easing fade)) "exits ease in")
          (set! (.-easing fade) "linear")
          (set! (.-amount fade) 0.2)
          (t/is (= {:direction :out :easing :linear :amount 0.2}
                   (select-keys (first (:animations (stored-track store context board rect)))
                                [:direction :easing :amount])))
          (t/is (thrown? js/Error (set! (.-easing fade) "hold")) "only a keyframe holds"))

        (t/testing "a style adds several"
          (let [added (.addAnimationStyle timeline rect "pop" 600)]
            (t/is (= 2 (.-length added)))
            (t/is (= 3 (.-length (.-animations timeline))))
            (t/is (thrown? js/Error (.addAnimationStyle timeline rect "wobble")))))

        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (.addAnimation timeline rect #js {:type "shake"})))
          (t/is (thrown? js/Error (.addAnimation timeline rect #js {:type "fade" :direction "up"}))))

        (t/testing "the animation writes CSS"
          (t/is (re-find #"@keyframes" (.toCSS timeline))))

        (t/testing "they go away"
          (.remove fade)
          (t/is (= 2 (count (:animations (stored-track store context board rect))))))))))

(t/deftest markers-name-moments
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            [^js board _]       (board-with-rect context)
            ^js timeline        (.addTimeline board #js {:duration 1000})
            ^js outro           (.addMarker timeline #js {:time 800 :name "outro"})
            ^js intro           (.addMarker timeline #js {:time 0})
            names               #(mapv (fn [^js marker] (.-name marker)) (.-markers timeline))]

        (t/testing "they are by time, named by their number when not given a name"
          (t/is (= ["Marker 2" "outro"] (names)))
          (t/is (= [0 800] (mapv (fn [^js marker] (.-time marker)) (.-markers timeline)))))

        (t/testing "they change"
          (set! (.-name intro) "intro")
          (set! (.-time outro) 1500)
          (t/is (= [["intro" 0] ["outro" 1500]]
                   (mapv (juxt :name :time) (:markers (get (stored-timelines store context) (aget board "$id"))))))
          (t/is (= 1500 (.-duration timeline)) "the timeline grows to hold them")
          (t/is (thrown? js/Error (set! (.-name intro) " ")))
          (t/is (thrown? js/Error (set! (.-time intro) -1))))

        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (.addMarker timeline #js {:name "no time"})))
          (t/is (thrown? js/Error (.addMarker timeline #js {:time 10 :name 3}))))

        (t/testing "they go away"
          (.remove intro)
          (t/is (= ["outro"] (names))))))))

(t/deftest record-keeps-changes-as-keyframes
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]  (setup true)
            [^js board ^js rect] (board-with-rect context)
            ^js other            (.createRectangle context)
            _                    (.appendChild board other)
            ^js timeline         (.addTimeline board #js {:duration 1000})
            opacity              #(mapv (juxt :time :value)
                                        (filter (fn [kf] (= :opacity (:property kf)))
                                                (:keyframes (stored-track store context board %))))]
        (.record timeline 500 #js [rect]
                 (fn []
                   (set! (.-opacity rect) 0.5)
                   (set! (.-opacity other) 0.2)))

        (t/testing "the value before at 0, the new one at the time"
          (t/is (= [[0 1] [500 0.5]] (opacity rect))))

        (t/testing "the changes stay on the shapes, only those given are recorded"
          (t/is (= 0.5 (.-opacity rect)))
          (t/is (= 0.2 (.-opacity other)))
          (t/is (nil? (stored-track store context board other))))

        (t/testing "an animated property gets a keyframe at the time"
          (.record timeline 800 #js [rect] #(set! (.-opacity rect) 0.8))
          (t/is (= [[0 1] [500 0.5] [800 0.8]] (opacity rect))))

        (t/testing "what is not valid is refused, before the callback runs"
          (let [ran? (atom false)]
            (t/is (thrown? js/Error (.record timeline -1 #js [rect] #(reset! ran? true))))
            (t/is (thrown? js/Error (.record timeline 100 rect #(reset! ran? true))))
            (t/is (thrown? js/Error (.record timeline 100 #js [(.createRectangle context)] #(reset! ran? true))))
            (t/is (thrown? js/Error (.record timeline 100 #js [rect] "not a function")))
            (t/is (false? @ran?))))))))

(t/deftest motion-needs-its-feature
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup false)
            [^js board _]       (board-with-rect context)]
        (t/is (thrown? js/Error (.addTimeline board)))
        (t/is (empty? (stored-timelines store context)))))))

(t/deftest a-timeline-keeps-its-export-settings
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            [^js board _]       (board-with-rect context)
            ^js timeline        (.addTimeline board)
            settings            #(js->clj (.-exportSettings timeline) :keywordize-keys true)]

        (t/testing "the export panel's defaults until they are set"
          (t/is (= {:format "mp4" :quality "high" :fps 30 :size "1" :loop true} (settings))))

        (t/testing "setting some changes only those"
          (set! (.-exportSettings timeline) #js {:format "gif" :fps 15})
          (t/is (= {:format "gif" :quality "high" :fps 15 :size "1" :loop true} (settings)))
          (t/is (= {:format :gif :fps 15}
                   (get-in (stored-timelines store context) [(aget board "$id") :export]))))

        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:format "mov"})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:fps 0})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:size "3"})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:loop "yes"}))))))))

(t/deftest a-board-lists-the-copies-that-animate
  (thw/with-wasm-mocks*
    (fn []
      (let [file        (-> (cthf/sample-file :motion-file :page-label :motion-page)
                            (ctho/add-simple-component :anim-comp :anim-main :anim-rect
                                                       :root-params {:x 0 :y 0 :width 200 :height 200}
                                                       :child-params {:x 10 :y 20 :width 50 :height 50})
                            (ctho/add-frame :anim-host :x 1000 :y 0 :width 400 :height 400)
                            (cthc/instantiate-component :anim-comp :anim-copy :parent-label :anim-host))
            main        (cthi/id :anim-main)
            timeline    (-> (cta/make-timeline {:board-id main :duration 1200 :playback :loop})
                            (cta/add-keyframe (cthi/id :anim-rect) {:time 0 :property :x :value 10})
                            (cta/add-keyframe (cthi/id :anim-rect) {:time 1200 :property :x :value 60}))
            file        (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                                     (pcb/with-page (cthf/current-page file))
                                                     (pcb/set-timeline main timeline)))
            store       (ths/setup-store file)
            _           (api/create-context plugin-id)
            _           (set! motion/shape-proxy shape/shape-proxy)
            _           (set! motion/shape-proxy? shape/shape-proxy?)
            _           (set! st/state store)
            _           (ptk/emit! store #(assoc-in % [:plugins :flags plugin-id :throw-validation-errors] true))
            page-id     (cthf/current-page-id file)
            proxy       #(shape/shape-proxy plugin-id (:id file) page-id %)
            ^js copies  (.-animatedCopies (proxy (cthi/id :anim-host)))]
        (t/is (= 1 (.-length copies)))
        (let [^js copy (aget copies 0)]
          (t/is (= (cthi/id :anim-copy) (aget (.-copy copy) "$id")))
          (t/is (= [1200 "loop"] [(.-duration copy) (.-playback copy)])))
        (t/is (nil? (.-timeline (proxy (cthi/id :anim-host)))) "without a timeline of its own")
        (t/is (zero? (.-length (.-animatedCopies (proxy main)))) "the main plays its own")))))

(defn- copy-file
  "Board A at 100,100 with the rect `source` at 150,200 (50,100 in it)
  moving right by 100 and down by 40 in a second and fading in, and the
  rects `b` and `c` 100 and 200 to its right; board B at 1000,100 with the
  rect `d` at 1100,150 (100,50 in it). With `keyframes`, `source` moves
  along that many keyframes instead."
  ([] (copy-file nil))
  ([keyframes]
   (let [file     (-> (cthf/sample-file :copy-file :page-label :copy-page)
                      (ctho/add-frame :board-a :x 100 :y 100 :width 400 :height 300)
                      (cths/add-sample-shape :source :type :rect :parent-label :board-a
                                             :x 150 :y 200 :width 50 :height 50)
                      (cths/add-sample-shape :b :type :rect :parent-label :board-a
                                             :x 250 :y 200 :width 50 :height 50)
                      (cths/add-sample-shape :c :type :rect :parent-label :board-a
                                             :x 350 :y 200 :width 50 :height 50)
                      (ctho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                      (cths/add-sample-shape :d :type :rect :parent-label :board-b
                                             :x 1100 :y 150 :width 100 :height 100))
         source   (cthi/id :source)
         timeline (cta/make-timeline {:board-id (cthi/id :board-a) :duration (or keyframes 1000)})
         timeline (if (some? keyframes)
                    (assoc-in timeline [:tracks source]
                              {:shape-id source
                               :keyframes (mapv #(cta/make-keyframe {:time % :property :x :value %})
                                                (range keyframes))})
                    (-> timeline
                        (cta/add-keyframe source {:time 0 :property :x :value 50})
                        (cta/add-keyframe source {:time 1000 :property :x :value 150})
                        (cta/add-keyframe source {:time 0 :property :y :value 100})
                        (cta/add-keyframe source {:time 1000 :property :y :value 140})
                        (cta/add-animation source {:type :fade :start 0 :duration 300})))]
     (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                  (pcb/with-page (cthf/current-page file))
                                  (pcb/set-timeline (cthi/id :board-a) timeline))))))

(defn- shape-of
  "The proxy of the shape `label` of `file`."
  [file label]
  (shape/shape-proxy plugin-id (:id file) (cthf/current-page-id file) (cthi/id label)))

(defn- keyframes-of
  "`[time value]` of the keyframes of `property` of the layer `label` in
  the timeline of the board `board-label`."
  [store context board-label label property]
  (->> (get-in (stored-timelines store context)
               [(cthi/id board-label) :tracks (cthi/id label) :keyframes])
       (filter #(= property (:property %)))
       (mapv (juxt :time :value))))

(t/deftest copy-animation-onto-other-layers
  (thw/with-wasm-mocks*
    (fn []
      (let [file                (copy-file)
            [store ^js context] (setup file true)
            shape               (partial shape-of file)
            ^js timeline        (.-timeline (shape :board-a))
            result              (.copyAnimation timeline (shape :source)
                                                #js [(shape :b) (shape :c) (shape :d)]
                                                #js {:stagger 100})
            x                   #(keyframes-of store context %1 %2 :x)]

        (t/testing "each target moves from where it rests, one after the other"
          (t/is (= [[0 150] [1000 250]] (x :board-a :b)))
          (t/is (= [[100 250] [1100 350]] (x :board-a :c))))

        (t/testing "a target in another board lands in its timeline"
          (t/is (= [[200 100] [1200 200]] (x :board-b :d)))
          (t/is (= [[200 50] [1200 90]] (keyframes-of store context :board-b :d :y))))

        (t/testing "the presets go along"
          (t/is (= [:fade] (mapv :type (get-in (stored-timelines store context)
                                               [(cthi/id :board-a) :tracks (cthi/id :b) :animations])))))

        (t/testing "it tells what it did"
          (t/is (object? result))
          (t/is (= {:targets 3 :skipped 0 :pivotKept 0} (js->clj result :keywordize-keys true))))))))

(t/deftest copy-animation-takes-options
  (thw/with-wasm-mocks*
    (fn []
      (let [file                (copy-file)
            [store ^js context] (setup file true)
            shape               (partial shape-of file)
            ^js timeline        (.-timeline (shape :board-a))
            track               #(get-in (stored-timelines store context)
                                         [(cthi/id :board-a) :tracks (cthi/id %)])
            x                   #(keyframes-of store context :board-a % :x)]

        (t/testing "only the properties given, without the presets"
          (.copyAnimation timeline (shape :source) #js [(shape :b)]
                          #js {:properties #js ["x"] :presets false})
          (t/is (= #{:x} (into #{} (map :property) (:keyframes (track :b)))))
          (t/is (empty? (:animations (track :b)))))

        (t/testing "the values of the shape, at another start, rounded"
          (.copyAnimation timeline (shape :source) #js [(shape :c)] #js {:values "exact" :start 12.6})
          (t/is (= [[13 50] [1013 150]] (x :c))))

        (t/testing "added to what the target has, or in its place"
          (.addKeyframe timeline (shape :b) #js {:property "x" :time 500 :value 0})
          (.copyAnimation timeline (shape :source) #js [(shape :b)] #js {:replace false})
          (t/is (= [[0 150] [500 0] [1000 250]] (x :b)))
          (.copyAnimation timeline (shape :source) #js [(shape :b)])
          (t/is (= [[0 150] [1000 250]] (x :b))))))))

(t/deftest copy-animation-refuses-what-is-not-valid
  (thw/with-wasm-mocks*
    (fn []
      (let [file                (copy-file)
            [store ^js context] (setup file true)
            shape               (partial shape-of file)
            ^js timeline        (.-timeline (shape :board-a))
            before              (stored-timelines store context)
            copy                #(.copyAnimation timeline (shape :source) %1 %2)
            elsewhere           (shape/shape-proxy plugin-id (:id file) (uuid/next) (cthi/id :b))]
        (t/is (thrown? js/Error (copy #js [(shape :b)] #js {:start -5})))
        (t/is (thrown? js/Error (copy #js [(shape :b)] #js {:stagger "x"})))
        (t/is (thrown? js/Error (copy #js [(shape :b)] #js {:values "absolute"})))
        (t/is (thrown? js/Error (copy #js [(shape :b)] #js {:properties #js ["colour"]})))
        (t/is (thrown? js/Error (copy #js [(shape :b)] "fast")) "options that are no object")
        (t/is (thrown? js/Error (copy #js [elsewhere] nil)) "a shape of another page")
        (t/is (thrown? js/Error (copy #js [#js {:id "b"}] nil)) "not a shape")
        (t/is (thrown? js/Error (copy (shape :b) nil)) "not an array")
        (t/is (thrown? js/Error (.copyAnimation timeline (shape :d) #js [(shape :b)]))
              "a shape out of the board")
        (t/is (thrown? js/Error (.copyAnimation timeline (shape :b) #js [(shape :c)]))
              "nothing to copy")
        (with-redefs [r/check-permission (constantly false)]
          (t/is (thrown? js/Error (copy #js [(shape :b)] nil)) "without content:write"))
        (t/is (= before (stored-timelines store context)))))))

(t/deftest copy-animation-needs-motion
  (thw/with-wasm-mocks*
    (fn []
      (let [file                (copy-file)
            [store ^js context] (setup file false)
            timeline            (motion/timeline-proxy plugin-id (:id file) (cthf/current-page-id file)
                                                       (cthi/id :board-a))]
        (t/is (thrown? js/Error (.copyAnimation ^js timeline (shape-of file :source)
                                                #js [(shape-of file :b)])))
        (t/is (= (:timelines (cthf/current-page file)) (stored-timelines store context)))))))

(t/deftest copy-animation-has-limits
  (thw/with-wasm-mocks*
    (fn []
      (let [file                (copy-file (inc cltl/max-paste-keyframes))
            [store ^js context] (setup file true)
            ^js timeline        (.-timeline (shape-of file :board-a))]
        (t/is (thrown? js/Error (.copyAnimation timeline (shape-of file :source)
                                                #js [(shape-of file :b)])))
        (t/is (= (:timelines (cthf/current-page file)) (stored-timelines store context)))))))
