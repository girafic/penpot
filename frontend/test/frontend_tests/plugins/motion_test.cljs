;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.plugins.motion-test
  (:require
   [app.common.math :as mth]
   [app.common.test-helpers.files :as cthf]
   [app.common.types.animation :as cta]
   [app.main.store :as st]
   [app.plugins.api :as api]
   [app.plugins.motion :as motion]
   [app.plugins.shape :as shape]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]
   [potok.v2.core :as ptk]))

(def ^:private plugin-id "00000000-0000-0000-0000-000000000000")

(defn- setup
  "A store with an empty file, Motion on when `motion?`, and the plugin
  context: `[store context]`."
  [motion?]
  (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
        ^js context (api/create-context plugin-id)]
    ;; as `app.plugins` does, which the tests leave out with the runtime
    (set! motion/shape-proxy shape/shape-proxy)
    (set! motion/shape-proxy? shape/shape-proxy?)
    (set! st/state store)
    (ptk/emit! store #(cond-> (assoc-in % [:plugins :flags plugin-id :throw-validation-errors] true)
                        motion? (update :features (fnil conj #{}) "animation/v1")))
    [store context]))

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
