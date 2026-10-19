;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.plugins.motion-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.test-helpers.components :as cthc]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.common.test-helpers.tokens :as ctht]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.tokens-lib :as ctob]
   [app.common.types.tokens-status :as ctos]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.tokens.propagation :as dwtp]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.plugins.api :as api]
   [app.plugins.library :as library]
   [app.plugins.motion :as motion]
   [app.plugins.register :as r]
   [app.plugins.shape :as shape]
   [app.util.audio :as audio]
   [app.util.code-gen.common :as cgc]
   [app.util.code-gen.motion :as cgm]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [cuerdas.core :as str]
   [frontend-tests.helpers.mock :as mock]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]
   [frontend-tests.tokens.helpers.state :as tohs]
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
     (set! motion/lib-animation-style-proxy library/lib-animation-style-proxy)
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

(defn- board-with-rects
  "A board with `n` rectangles inside: `[board rects]`."
  [^js context n]
  (let [^js board (.createBoard context)
        rects     (vec (repeatedly n #(let [^js rect (.createRectangle context)]
                                        (.appendChild board rect)
                                        rect)))]
    [board rects]))

(t/deftest the-outline-of-a-path-is-a-keyframe
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            [^js board ^js rect] (board-with-rect context)
            ^js blob     (.createPath context)
            _            (.appendChild board blob)
            _            (set! (.-d blob) "M10,10L50,10L10,50Z")
            ^js timeline (.addTimeline board)
            ^js from     (.addKeyframe timeline blob #js {:property "path" :time 0 :value "M10,10L50,10L10,50Z" :easing "linear"})
            ^js to       (.addKeyframe timeline blob #js {:property "path" :time 1000 :value "M10,10L90,10L10,90Z"})
            plain        #(str/replace (str %) #"\.0(?=\D|$)" "")]
        (t/testing "its values are SVG paths where the path rests, as its `d`"
          (t/is (= "path" (.-property from)))
          (t/is (= "M10,10L50,10L10,50Z" (plain (.-value from))))
          (t/is (= "M10,10L90,10L10,90Z" (plain (.-value to))))
          (t/is (= "M10,10L70,10L10,70Z" (plain (.valueAt timeline blob "path" 500))) "halfway")
          (t/is (some? (:outline-box (stored-track store context board blob))) "made for its box"))

        (t/testing "they change"
          (set! (.-value to) "M10,10L30,10L10,30Z")
          (t/is (= "M10,10L30,10L10,30Z" (plain (.-value to))))
          (t/is (thrown? js/Error (set! (.-value to) 5))))

        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "path" :time 0 :value "M0,0L10,0Z"}))
                "a rectangle, no path")
          (t/is (thrown? js/Error (.addKeyframe timeline blob #js {:property "path" :time 0 :value "far"})))
          (t/is (thrown? js/Error (.addKeyframe timeline blob #js {:property "path" :time 0 :value "M0,0L10,0Z" :pathIn 3})))
          (t/is (thrown? js/Error (.createAnimationStyle (.. context -library -local)
                                                         #js [#js {:type "keyframes" :property "path" :duration 500
                                                                   :keyframes #js [#js {:time 0 :value "M0,0L10,0Z"}]}]))
                "no style holds one"))))))

(t/deftest a-path-keyframe-holds-its-first-point-by-hand
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            ^js board    (.createBoard context)
            ^js blob     (.createPath context)
            _            (.appendChild board blob)
            _            (set! (.-d blob) "M10,10L50,10L10,50Z")
            ^js timeline (.addTimeline board)
            _            (.addKeyframe timeline blob #js {:property "path" :time 0 :value "M10,10L50,10L10,50Z" :easing "linear"})
            ^js square   (.addKeyframe timeline blob #js {:property "path" :time 1000 :value "M50,50L10,50L10,10L50,10Z"})
            ^js fade     (.addKeyframe timeline blob #js {:property "opacity" :time 0 :value 1})
            stored       #(->> (:keyframes (stored-track store context board blob))
                               (filter (fn [kf] (and (= :content (:property kf)) (= 1000 (:time kf)))))
                               first
                               :fixed-start)]
        (t/is (false? (.-fixedStart square)) "matched on its own at first")
        (set! (.-fixedStart square) true)
        (t/is (true? (.-fixedStart square)))
        (t/is (true? (stored)) "kept on the keyframe")
        (set! (.-fixedStart square) false)
        (t/is (nil? (stored)) "and taken off")
        (t/is (thrown? js/Error (set! (.-fixedStart fade) true)) "not on another property")
        (set! (.-fixedStart square) true)
        (t/is (str/starts-with? (str/replace (str (.valueAt timeline blob "path" 500)) #"\.0(?=\D|$)" "") "M30,30")
              "half way, the triangle's first point goes to the square's")
        (set! (.-fixedStart square) false)
        (t/is (thrown? js/Error (set! (.-fixedStart square) "yes")) "a boolean")))))

(t/deftest the-outlines-of-a-path-hold-a-budget
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            ^js board    (.createBoard context)
            ^js blob     (.createPath context)
            _            (.appendChild board blob)
            _            (set! (.-d blob) "M10,10L50,10L10,50Z")
            ^js timeline (.addTimeline board)
            ;; 4000 segments, eight of which a row holds
            big          (str "M0,0" (str/join (map #(str "L" % "," (mod % 7)) (range 1 4000))) "Z")
            count-kfs    #(count (:keyframes (stored-track store context board blob)))]
        (doseq [t (range 8)]
          (.addKeyframe timeline blob #js {:property "path" :time (* t 100) :value big}))
        (t/is (= 8 (count-kfs)))
        (t/is (thrown? js/Error (.addKeyframe timeline blob #js {:property "path" :time 900 :value big}))
              "a ninth goes over the row")
        (t/is (= 8 (count-kfs)))
        (t/is (some? (.addKeyframe timeline blob #js {:property "path" :time 700 :value big}))
              "one in place of one at its time fits")
        (let [^js first-kf (first (filter #(= 0 (.-time ^js %)) (.-keyframes timeline)))
              small        "M10,10L50,10L10,50Z"
              bigger       (str big "L1,1")]
          (set! (.-value first-kf) small)
          (t/is (= 8 (count-kfs)))
          (.addKeyframe timeline blob #js {:property "path" :time 800 :value big})
          (t/is (thrown? js/Error (set! (.-value first-kf) bigger))
                "a value that takes the row past it is refused")
          (t/is (= "M10,10L50,10L10,50Z" (str/replace (str (.-value first-kf)) #"\.0(?=\D|$)" ""))
                "and the keyframe keeps its own"))))))

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

(t/deftest a-board-with-a-timeline-serializes
  ;; The timeline of a board and what is in it lead back to the board. As
  ;; `parent` and `children`, those links are not enumerable, so a plugin
  ;; that sends a board to its UI (`structuredClone`, as the MCP server's
  ;; results go) is done with it instead of following them for ever.
  (thw/with-wasm-mocks*
    (fn []
      (let [[_ ^js context] (setup true)
            [^js board ^js rect] (board-with-rect context)
            ^js timeline (.addTimeline board #js {:duration 1000})]
        (.addKeyframe timeline rect #js {:property "x" :time 0 :value 0})
        (.addAnimation timeline rect #js {:type "fade" :start 0 :duration 300})
        (t/is (not (.includes (js/Object.keys board) "timeline")))
        (t/is (not (.includes (js/Object.keys timeline) "board")))
        (t/is (some? (js/structuredClone board)) "as a plugin sends it to its UI")
        (t/is (some? (js/structuredClone #js [board timeline (aget (.-keyframes timeline) 0)])))
        (t/is (str/includes? (js/JSON.stringify timeline) "\"property\":\"x\"")
              "the timeline with its keyframes")
        (t/testing "the links are there to follow"
          (t/is (= (aget board "$id") (aget (.-board timeline) "$id")))
          (t/is (= (aget rect "$id") (aget (.-shape (aget (.-keyframes timeline) 0)) "$id")))
          (t/is (= (aget rect "$id") (aget (.-shape (aget (.-animations timeline) 0)) "$id"))))))))

(t/deftest a-layer-turns-and-scales-around-its-origin
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]  (setup true)
            [^js board ^js rect] (board-with-rect context)
            ^js timeline         (.addTimeline board)
            origin               #(js->clj (.originOf timeline rect) :keywordize-keys true)]
        (t/is (= {:x 0.5 :y 0.5} (origin)) "its center at first")
        (.setOrigin timeline rect #js {:x 0 :y 0.5})
        (t/is (= {:x 0 :y 0.5} (:origin (stored-track store context board rect))) "as the pivot of the Design tab")
        (t/is (= {:x 0 :y 0.5} (origin)))
        (t/is (thrown? js/Error (.setOrigin timeline rect #js {:x 2 :y 0})) "within its box")
        (t/is (thrown? js/Error (.setOrigin timeline rect "left")))
        (.setOrigin timeline rect #js {:x 0.5 :y 0.5})
        (t/is (nil? (:origin (stored-track store context board rect))) "its center again, as unset")))))

(t/deftest a-property-plays-one-of-another-layer
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]           (setup true)
            [^js board [^js car ^js wheel ^js third]] (board-with-rects context 3)
            ^js timeline                  (.addTimeline board)
            links                         #(mapv (fn [^js link]
                                                   (-> (js->clj link :keywordize-keys true)
                                                       (update :source (fn [^js s] (aget s "$id")))))
                                                 (.linksOf timeline %))]
        (t/is (= [] (links wheel)) "none at first")
        (.link timeline wheel #js {:property "rotation" :source car :sourceProperty "x" :factor 3.6})
        (t/is (= [{:property :rotation :source (aget car "$id") :source-property :x :factor 3.6}]
                 (:links (stored-track store context board wheel)))
              "the defaults left out")
        (t/is (= [{:property "rotation" :source (aget car "$id") :sourceProperty "x" :factor 3.6
                   :offset 0 :delay 0}]
                 (links wheel)))
        (.link timeline wheel #js {:property "rotation" :source car :sourceProperty "y" :delay 100})
        (t/is (= ["y" 1 100] ((juxt :sourceProperty :factor :delay) (first (links wheel))))
              "linking it again changes that link")
        (.link timeline wheel #js {:property "opacity" :source wheel})
        (t/is (= "opacity" (:sourceProperty (second (links wheel)))) "its own property, by default")
        (t/testing "what cannot link is refused"
          (t/is (thrown? js/Error (.link timeline third #js {:property "fillColor" :source car})))
          (t/is (thrown? js/Error (.link timeline third #js {:property "x" :source "car"})))
          (t/is (thrown? js/Error (.link timeline third #js {:property "x" :source car :factor "2"})))
          (t/is (thrown? js/Error (.link timeline third #js {:property "x" :source car :factor js/NaN})))
          (t/is (thrown? js/Error (.link timeline third #js {:property "x" :source car :offset js/Infinity})))
          (t/is (thrown? js/Error (.link timeline third #js {:property "x" :source car :delay -5})))
          (t/is (thrown? js/Error (.link timeline third #js {:property "x" :source board}))
                "the board is no layer of itself"))
        (.unlink timeline wheel "rotation")
        (t/is (= ["opacity"] (map :property (links wheel))))))))

(t/deftest a-link-maps-a-range-of-its-source
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]                (setup true)
            [^js board [^js slider ^js needle]] (board-with-rects context 2)
            ^js timeline                       (.addTimeline board)
            link                               #(-> (js->clj (first (.linksOf timeline needle)) :keywordize-keys true)
                                                    (dissoc :source))]
        (.link timeline needle #js {:property "rotation" :source slider :sourceProperty "x"
                                    :input #js [0 200] :output #js [0 270] :easing "ease-in-out"})
        (t/is (= {:property :rotation :source (aget slider "$id") :source-property :x
                  :input [0 200] :output [0 270] :easing :ease-in-out}
                 (first (:links (stored-track store context board needle))))
              "a clamp as absence")
        (t/is (= {:input [0 200] :output [0 270] :clamp true :easing "ease-in-out"}
                 (select-keys (link) [:input :output :clamp :easing])))
        (t/testing "what cannot map is refused"
          (t/is (thrown? js/Error (.link timeline needle #js {:property "rotation" :source slider :input #js [0 200]}))
                "an input without an output")
          (t/is (thrown? js/Error (.link timeline needle #js {:property "rotation" :source slider
                                                              :input #js [0] :output #js [0 1]})))
          (t/is (thrown? js/Error (.link timeline needle #js {:property "rotation" :source slider
                                                              :input #js [0 js/NaN] :output #js [0 1]}))
                "a number that is none")
          (t/is (thrown? js/Error (.link timeline needle #js {:property "rotation" :source slider
                                                              :input #js [0 1] :output #js [0 1] :easing "bounce"}))))))))

(t/deftest the-light-of-a-board-takes-keyframes
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]    (setup true)
            [^js board [^js card]] (board-with-rects context 1)
            ^js timeline           (.addTimeline board)
            properties             #(mapv (juxt :property :value)
                                          (:keyframes (stored-track store context board board)))]
        (t/is (thrown? js/Error (.addKeyframe timeline board #js {:property "lightAngle" :time 0 :value 90}))
              "a board with no light")
        (t/is (thrown? js/Error (.addKeyframe timeline card #js {:property "lightAngle" :time 0 :value 90}))
              "a layer has no light")
        (set! (.-light board) #js {:angle 90})
        (.addKeyframe timeline board #js {:property "lightAngle" :time 0 :value 90})
        (.addKeyframe timeline board #js {:property "lightAmbient" :time 500 :value 0.6})
        (t/is (= [[:light-angle 90] [:light-ambient 0.6]] (properties)))))))

(t/deftest a-layer-follows-another-of-its-board
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]           (setup true)
            [^js board [^js card ^js badge ^js third]] (board-with-rects context 3)
            ^js timeline                  (.addTimeline board)
            follow                        #(some-> (.followOf timeline %)
                                                   (js->clj :keywordize-keys true)
                                                   (update :leader (fn [^js leader] (aget leader "$id"))))]
        (t/is (nil? (follow badge)) "none at first")
        (.setFollow timeline badge #js {:leader card :delay 100})
        (t/is (= {:leader (aget card "$id") :delay 100} (:follow (stored-track store context board badge)))
              "the defaults left out")
        (t/is (= {:leader (aget card "$id") :from "start" :position true :rotation true :scale true :tilt true
                  :opacity false :delay 100}
                 (follow badge)))
        (.setFollow timeline badge #js {:leader card :delay 100 :from "rest"})
        (t/is (= "rest" (:from (follow badge))) "from where the card rests")
        (t/is (= :rest (get-in (stored-track store context board badge) [:follow :from])))
        (.setFollow timeline badge #js {:leader card :rotation false})
        (t/is (false? (:rotation (follow badge))))
        (.setFollow timeline badge #js {:leader card :tilt false})
        (t/is (false? (:tilt (follow badge))) "its tilt too")
        (t/testing "a leader it cannot follow is refused"
          (t/is (thrown? js/Error (.setFollow timeline card #js {:leader badge})) "one that follows it")
          (t/is (thrown? js/Error (.setFollow timeline badge #js {:leader board})) "the board it is in")
          (t/is (thrown? js/Error (.setFollow timeline badge #js {:leader badge})) "itself")
          (t/is (thrown? js/Error (.setFollow timeline third #js {:leader card :delay -5})))
          (t/is (thrown? js/Error (.setFollow timeline third #js {:leader card :scale "yes"})))
          (t/is (thrown? js/Error (.setFollow timeline third #js {:leader card :from "end"}))))
        (.setFollow timeline badge nil)
        (t/is (nil? (follow badge)) "null follows none")))))

(t/deftest a-layer-of-a-copy-leads-only-when-its-board-animates-it
  (thw/with-wasm-mocks*
    (fn []
      (let [file            (-> (cthf/sample-file :motion-file :page-label :motion-page)
                                (ctho/add-simple-component :anim-comp :anim-main :anim-rect
                                                           :root-params {:x 0 :y 0 :width 200 :height 200}
                                                           :child-params {:x 10 :y 20 :width 50 :height 50})
                                (ctho/add-frame :anim-host :x 1000 :y 0 :width 400 :height 400)
                                (cthc/instantiate-component :anim-comp :anim-copy :parent-label :anim-host)
                                (ctho/add-rect :own :parent-label :anim-host :x 1300 :y 50 :width 40 :height 40))
            _               (setup file true)
            page-id         (cthf/current-page-id file)
            proxy           #(shape/shape-proxy plugin-id (:id file) page-id (cthi/id %))
            ^js timeline    (.addTimeline (proxy :anim-host))
            refused         (try (.setFollow timeline (proxy :own) #js {:leader (proxy :anim-copy)})
                                 nil
                                 (catch :default cause (ex-message cause)))]
        (t/is (str/includes? (str refused) "component copy the board does not animate")
              "the message names the rule")
        (.addKeyframe timeline (proxy :anim-copy) #js {:property "opacity" :time 0 :value 1})
        (.setFollow timeline (proxy :own) #js {:leader (proxy :anim-copy)})
        (t/is (= (cthi/id :anim-copy) (aget (.-leader (.followOf timeline (proxy :own))) "$id"))
              "a copy the board animates leads")))))

(t/deftest the-end-of-an-arm-reaches-a-goal
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]  (setup true)
            [^js board [^js upper ^js fore ^js hand ^js ball]] (board-with-rects context 4)
            ^js timeline         (.addTimeline board)
            ik                   #(some-> (.ikOf timeline %)
                                          (js->clj :keywordize-keys true)
                                          (update :goal (fn [^js goal] (aget goal "$id"))))]
        (.setFollow timeline fore #js {:leader upper})
        (.setFollow timeline hand #js {:leader fore})
        (t/is (nil? (ik hand)) "none at first")
        (.setIk timeline hand #js {:goal ball :end "keep"})
        (t/is (= {:goal (aget ball "$id") :bend "ccw" :end "keep"} (ik hand)) "a straight arm bends counter-clockwise")
        (t/is (= {:goal (aget ball "$id") :turn :keep} (:ik (stored-track store context board hand))))
        (.setIk timeline hand #js {:goal ball :bend "cw"})
        (t/is (= {:goal (aget ball "$id") :bend "cw" :end "keep"} (ik hand)) "what is not given stays")
        (t/testing "a goal or a chain it cannot reach is refused"
          (t/is (thrown? js/Error (.setIk timeline hand #js {:goal fore})) "a link of its chain")
          (t/is (thrown? js/Error (.setIk timeline fore #js {:goal ball})) "a chain of one link")
          (t/is (thrown? js/Error (.setIk timeline hand #js {:goal board})))
          (t/is (thrown? js/Error (.setIk timeline hand #js {:goal ball :bend "up"})))
          (t/is (thrown? js/Error (.setIk timeline hand #js {:goal ball :end "free"})))
          (let [refused (try (.setIk timeline hand #js {:goal fore}) nil
                             (catch :default cause (ex-message cause)))]
            (t/is (str/includes? (str refused) "out of the chain") "the message names the rule")))
        (.setIk timeline hand nil)
        (t/is (nil? (ik hand)) "null reaches none")))))

(t/deftest a-plugin-hears-why-a-chain-cannot-reach
  (thw/with-wasm-mocks*
    (fn []
      (let [[_ ^js context] (setup true)
            [^js board [^js upper ^js fore ^js hand ^js ball ^js upper2 ^js fore2 ^js hand2 ^js thumb]]
            (board-with-rects context 8)
            ^js timeline (.addTimeline board)
            refused      #(try (.setIk timeline %1 %2) nil
                               (catch :default cause (ex-message cause)))]
        (.setFollow timeline fore #js {:leader upper})
        (.setFollow timeline hand #js {:leader fore})
        (.setFollow timeline fore2 #js {:leader upper2})
        (.setFollow timeline hand2 #js {:leader fore2})
        (t/is (nil? (refused hand #js {:goal hand2})) "a hand reaches for a hand of another chain")
        (.setIk timeline hand2 #js {:goal ball})
        (t/is (str/includes? (str (refused hand2 #js {:goal hand})) "led by the chain") "each for the other")
        (.setFollow timeline thumb #js {:leader fore})
        (t/is (str/includes? (str (refused thumb #js {:goal ball})) "in one chain only") "a link in two chains")
        (t/is (str/includes? (str (refused fore #js {:goal ball})) "chain of two follows"))))))

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
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time "soon" :value 1})))
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time 0 :value "far"})))
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time 0 :value 1 :index 2})))
          (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "x" :time 0 :value 1 :easing "bouncy"})))
          (t/is (thrown? js/Error (.addKeyframe timeline (.createRectangle context) #js {:property "x" :time 0 :value 1}))
                "a layer outside the board"))

        (t/testing "a time before 0, as in the dock"
          (let [^js early (.addKeyframe timeline rect #js {:property "x" :time -5 :value 1})]
            (t/is (= -5 (.-time early)))
            (.remove early)))

        (t/testing "they go away"
          (.remove from)
          (t/is (= 2 (count (:keyframes (stored-track store context board rect)))))
          (.clear timeline rect)
          (t/is (nil? (stored-track store context board rect))))))))

(t/deftest a-group-trims-the-layers-in-it
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context]  (setup true)
            [^js board ^js rect] (board-with-rect context)
            ^js other            (.createRectangle context)
            _                    (.appendChild board other)
            ^js group            (.group context #js [rect other])
            ^js timeline         (.addTimeline board)]
        (t/is (= "together" (.trimModeOf timeline group)) "by default")
        (.setTrimMode timeline group "in-turn")
        (t/is (= "in-turn" (.trimModeOf timeline group)))
        (t/is (= :in-turn (:trim-mode (stored-track store context board group))))
        (t/is (thrown? js/Error (.setTrimMode timeline group "sideways")))
        (t/is (thrown? js/Error (.setTrimMode timeline rect "in-turn")) "only a group")
        (.setTrimMode timeline group "together")
        (t/is (= "together" (.trimModeOf timeline group)))
        (t/is (not (contains? (stored-track store context board group) :trim-mode)))))))

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
            (t/is (= ["fade" "scale"] (mapv #(.-type ^js %) added)))
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
          (t/is (= {:format "mp4" :quality "high" :fps 30 :size "1" :loop true :audio true
                    :includeIds false :includeBoundingBox false :outlineText false}
                   (settings))))

        (t/testing "setting some changes only those"
          (set! (.-exportSettings timeline) #js {:format "gif" :fps 15})
          (t/is (= {:format "gif" :quality "high" :fps 15 :size "1" :loop true :audio true
                    :includeIds false :includeBoundingBox false :outlineText false}
                   (settings)))
          (t/is (= {:format :gif :fps 15}
                   (get-in (stored-timelines store context) [(aget board "$id") :export]))))

        (t/testing "a dotLottie archive is a format"
          (set! (.-exportSettings timeline) #js {:format "dotlottie"})
          (t/is (= "dotlottie" (:format (settings)))))

        (t/testing "a size of its own, as the export panel takes it"
          (set! (.-exportSettings timeline) #js {:size "3,5"})
          (t/is (= "3.5" (:size (settings))))
          (set! (.-exportSettings timeline) #js {:size "512w"})
          (t/is (= "w512" (:size (settings)))))

        (t/testing "a video leaves its sound out when asked"
          (set! (.-exportSettings timeline) #js {:audio false})
          (t/is (false? (:audio (settings))))
          (t/is (false? (get-in (stored-timelines store context) [(aget board "$id") :export :audio]))))

        (t/testing "an SVG names its layers and has a bounding box when asked"
          (set! (.-exportSettings timeline) #js {:format "svg" :includeIds true :includeBoundingBox true})
          (t/is (= [true true] ((juxt :includeIds :includeBoundingBox) (settings))))
          (t/is (= [true true] ((juxt :ids :bbox) (get-in (stored-timelines store context) [(aget board "$id") :export])))))

        (t/testing "an SVG outlines its texts when asked"
          (set! (.-exportSettings timeline) #js {:outlineText true})
          (t/is (true? (:outlineText (settings))))
          (t/is (true? (get-in (stored-timelines store context) [(aget board "$id") :export :outline]))))

        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:includeIds "yes"})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:format "mov"})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:fps 0})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:size "11"})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:size "big"})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:loop "yes"})))
          (t/is (thrown? js/Error (set! (.-exportSettings timeline) #js {:audio "no"}))))))))

(t/deftest a-timeline-plays-when-the-prototype-shows-it
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            [^js board _]       (board-with-rect context)
            ^js timeline        (.addTimeline board)
            stored              #(get (stored-timelines store context) (aget board "$id"))]
        (t/is (true? (.-autoplay timeline)) "by default")
        (set! (.-autoplay timeline) false)
        (t/is (false? (.-autoplay timeline)))
        (t/is (false? (:autoplay (stored))))
        (set! (.-autoplay timeline) true)
        (t/is (not (contains? (stored) :autoplay)) "only the exception is kept")
        (t/is (thrown? js/Error (set! (.-autoplay timeline) "yes")))
        (let [^js other (.createBoard context)]
          (t/is (false? (.-autoplay ^js (.addTimeline other #js {:autoplay false}))))
          (t/is (thrown? js/Error (.addTimeline other #js {:autoplay 1}))))))))

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

(t/deftest a-timeline-gives-the-code-of-the-inspect
  (thw/with-wasm-mocks*
    (fn []
      (let [file          (-> (cthf/sample-file :motion-file :page-label :motion-page)
                              (ctho/add-simple-component :anim-comp :anim-main :anim-rect
                                                         :root-params {:x 0 :y 0 :width 200 :height 200}
                                                         :child-params {:x 10 :y 20 :width 50 :height 50})
                              (ctho/add-frame :anim-host :x 1000 :y 0 :width 400 :height 400)
                              (cthc/instantiate-component :anim-comp :anim-copy :parent-label :anim-host)
                              (ctho/add-rect :own :parent-label :anim-host :x 1300 :y 50 :width 40 :height 40)
                              (ctho/add-frame :still :x 2000 :y 0 :width 100 :height 100))
            main          (cthi/id :anim-main)
            host          (cthi/id :anim-host)
            file          (cthf/apply-changes
                           file
                           (-> (pcb/empty-changes nil)
                               (pcb/with-page (cthf/current-page file))
                               (pcb/set-timeline main (-> (cta/make-timeline {:board-id main :duration 1200 :playback :loop})
                                                          (cta/add-keyframe (cthi/id :anim-rect) {:time 0 :property :x :value 10})
                                                          (cta/add-keyframe (cthi/id :anim-rect) {:time 1200 :property :x :value 60})))
                               (pcb/set-timeline host (-> (cta/make-timeline {:board-id host :duration 2000})
                                                          (cta/add-keyframe (cthi/id :own) {:time 0 :property :opacity :value 0})
                                                          (cta/add-keyframe (cthi/id :own) {:time 500 :property :opacity :value 1})))
                               (pcb/set-timeline (cthi/id :still) (cta/make-timeline {:board-id (cthi/id :still)}))))
            [store _]     (setup file false)
            page          (cthf/current-page file)
            files         (:files @store)
            objects       (:objects page)
            proxy         #(shape/shape-proxy plugin-id (:id file) (:id page) %)
            ^js timeline  (.-timeline (proxy host))
            ids           (into #{} (cfh/selected-with-children objects [host]))
            copy-rect     (first (get-in objects [(cthi/id :anim-copy) :shapes]))]

        (t/testing "the CSS of the Inspect with the board selected"
          (t/is (= (cgm/animation-css page files ids) (.toCSS timeline)))
          (t/is (str/includes? (.toCSS timeline)
                               (str "." (cgc/shape->selector (get objects copy-rect))
                                    " {\n  animation: penpot-anim-" copy-rect " 1200ms linear infinite;"))
                "a looping copy plays on the clock of its main, with the class of the Inspect"))

        (t/testing "its JSON, a plain object"
          (let [data (cgm/animation-json page files ids)]
            (t/is (= data (js->clj (.toJSON timeline))))
            (t/is (= (cgm/json-text data) (js/JSON.stringify timeline nil 2))
                  "as text, the text of the Inspect")))

        (t/testing "its React"
          (t/is (= (cgm/animation-react page files ids) (.toReact timeline))))

        (t/testing "a board without copies keeps its keyframes"
          (let [css (.toCSS ^js (.-timeline (proxy main)))]
            (t/is (str/includes? css (str "@keyframes penpot-anim-" (cthi/id :anim-rect))))
            (t/is (str/includes? css (str "." (cgc/shape->selector (get objects (cthi/id :anim-rect))) " {")))))

        (t/testing "nothing animating, no code"
          (let [^js still (.-timeline (proxy (cthi/id :still)))]
            (t/is (= ["" ""] [(.toCSS still) (.toReact still)]))
            (t/is (= [] (js->clj (.-layers (.toJSON still)))))))

        (t/testing "reading needs no content:write"
          (with-redefs [r/check-permission (constantly false)]
            (t/is (= (cgm/animation-react page files ids) (.toReact timeline)))))))))

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
        (t/is (thrown? js/Error (copy #js [(shape :b)] #js {:start "soon"})))
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

;; SPLIT ANIMATIONS: the units of a text one by one

(defn- text-file
  "A board with the texts `words` (\"Hi you there\") and `styled`, which
  plays a use of a style whose entry fades it in word by word, and the
  rect `rect`."
  []
  (let [file   (-> (cthf/sample-file :text-file :page-label :text-page)
                   (ctho/add-frame-with-text :board :words "Hi you there"
                                             :frame-params {:x 0 :y 0 :width 400 :height 300})
                   (cths/add-sample-shape :styled :type :text :parent-label :board
                                          :x 10 :y 100 :width 200 :height 20)
                   (cths/add-sample-shape :rect :type :rect :parent-label :board
                                          :x 10 :y 200 :width 50 :height 50))
        board  (cthi/id :board)
        use    {:type :style :style-ref-id (uuid/next) :style-ref-file (uuid/next) :name "Words"
                :entries [{:type :fade :split :word :start 0 :duration 600}]}
        tl     (cta/add-animation (cta/make-timeline {:board-id board}) (cthi/id :styled) use)]
    (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                 (pcb/with-page (cthf/current-page file))
                                 (pcb/set-timeline board tl)))))

(t/deftest split-animations-play-a-text-by-its-units
  (thw/with-wasm-mocks*
    (fn []
      (let [file                (text-file)
            [store ^js context] (setup file true)
            shape               (partial shape-of file)
            ^js timeline        (.-timeline (shape :board))
            ^js fade            (.addAnimation timeline (shape :words) #js {:type "fade" :split "word"})
            stored              #(:animations (get-in (stored-timelines store context)
                                                      [(cthi/id :board) :tracks (cthi/id %)]))]

        (t/testing "a text plays its words one by one"
          (t/is (= ["word" 0.5 "forward"] [(.-split fade) (.-spread fade) (.-order fade)]))
          (t/is (= [:word] (mapv :split (stored :words)))))

        (t/testing "a random order gets a seed"
          (t/is (number? (.-seed (.addAnimation timeline (shape :words)
                                                #js {:type "move" :split "word" :order "random"})))))

        (t/testing "its settings change"
          (set! (.-spread fade) 0.2)
          (set! (.-order fade) "edges")
          (t/is (= [0.2 "edges"] [(.-spread fade) (.-order fade)])))

        (t/testing "what is not valid is refused and changes nothing"
          (let [before (stored-timelines store context)
                add    #(.addAnimation timeline (shape %1) (clj->js %2))]
            (t/is (thrown? js/Error (add :rect {:type "fade" :split "word"})) "a layer that is no text")
            (t/is (thrown? js/Error (add :words {:type "fade" :split "char"})) "another kind than its splits")
            (t/is (thrown? js/Error (add :styled {:type "fade" :split "char"}))
                  "another kind than the entry of its style use")
            (t/is (thrown? js/Error (add :words {:type "fade" :split "letter"})))
            (t/is (thrown? js/Error (add :words {:type "fade" :split "word" :spread 2})))
            (t/is (thrown? js/Error (add :words {:type "fade" :split "word" :order "sideways"})))
            (t/is (thrown? js/Error (add :words {:type "fade" :spread 0.5})) "a spread without a split")
            (t/is (thrown? js/Error (set! (.-split fade) "char")) "another kind than the other split")
            (t/is (thrown? js/Error (set! (.-spread fade) -1)))
            (t/is (thrown? js/Error (.addAnimationStyle timeline (shape :rect) "typewriter"))
                  "a text style on a rectangle")
            (t/is (= before (stored-timelines store context)))))

        (t/testing "the same kind on the text of a style use"
          (t/is (some? (.addAnimation timeline (shape :styled) #js {:type "move" :split "word"}))))

        (t/testing "a whole animation again"
          (set! (.-split fade) nil)
          (t/is (nil? (.-split fade)))
          (t/is (nil? (.-spread fade))))

        (t/testing "the CSS plays it on the whole text"
          (t/is (re-find #"opacity" (.toCSS timeline))))))))

(t/deftest text-styles-go-on-texts
  (thw/with-wasm-mocks*
    (fn []
      (let [file         (text-file)
            _            (setup file true)
            ^js timeline (.-timeline (shape-of file :board))
            added        (.addAnimationStyle timeline (shape-of file :words) "typewriter" 100)]
        (t/is (= [["fade" "char" 100]] (mapv (fn [^js a] [(.-type a) (.-split a) (.-start a)]) added)))))))

(t/deftest a-plugin-makes-applies-and-edits-a-style
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            [^js board rects]   (board-with-rects context 3)
            ^js lib             (.. context -library -local)
            ^js style           (.createAnimationStyle lib #js [#js {:type "fade" :duration 300}
                                                                #js {:type "move" :duration 300 :offsetY 40}])
            ^js timeline        (.addTimeline board #js {:duration 500})
            uses                (into [] (map-indexed #(.applyAnimationStyle timeline %2 style (* %1 80))) rects)
            stored-uses         #(mapv (fn [rect] (first (:animations (stored-track store context board rect)))) rects)]

        (t/testing "the style is in the library of the file"
          (set! (.-name style) "Card enter")
          (t/is (= ["Card enter"] (mapv #(.-name ^js %) (.-animationStyles lib))))
          (t/is (= 300 (.-duration style)))
          (t/is (= [["fade" 0 300] ["move" 0 300]]
                   (mapv (fn [^js e] [(.-type e) (.-start e) (.-duration e)]) (.-entries style)))))

        (t/testing "three layers play it, staggered"
          (t/is (= [0 80 160] (mapv #(.-start ^js %) uses)))
          (t/is (= ["Card enter" "Card enter" "Card enter"] (mapv #(.-name ^js %) uses)))
          (t/is (= 3 (.-length (.-appliedStyles timeline))))
          (t/is (zero? (.-length (.-animations timeline))) "the uses are not preset animations")
          (t/is (every? #(= (aget style "$id") (:style-ref-id %)) (stored-uses)))
          (t/is (= (aget style "$id") (aget (.-style ^js (first uses)) "$id"))))

        (t/testing "an edit of its entries reaches every use, and the timeline grows"
          (set! (.-entries style) #js [#js {:type "fade" :duration 900}])
          (t/is (= [[1 900] [1 900] [1 900]] (mapv (juxt (comp count :entries) :duration) (stored-uses))))
          (t/is (= 1060 (.-duration timeline))))

        (t/testing "a use moves and stretches"
          (set! (.-start ^js (last uses)) 200)
          (set! (.-duration ^js (last uses)) 1200)
          (t/is (= [200 1200] ((juxt :start :duration) (last (stored-uses)))))
          (t/is (thrown? js/Error (set! (.-duration ^js (last uses)) 0))))

        (t/testing "plugin data"
          (.setPluginData style "role" "entrance")
          (t/is (= "entrance" (.getPluginData style "role"))))

        (t/testing "a use detaches into its presets"
          (let [presets (.detach ^js (first uses))]
            (t/is (= ["fade"] (mapv #(.-type ^js %) presets)))
            (t/is (= 2 (.-length (.-appliedStyles timeline))))
            (t/is (= 1 (.-length (.-animations timeline))))))

        (t/testing "a copy, then a removal that leaves presets"
          (let [^js copy (.clone style)]
            (t/is (= 2 (.-length (.-animationStyles lib))))
            (.remove copy)
            (t/is (= 1 (.-length (.-animationStyles lib)))))
          (.remove style)
          (t/is (zero? (.-length (.-appliedStyles timeline))))
          (t/is (= 3 (.-length (.-animations timeline)))))))))

(t/deftest a-style-of-another-file-is-read-only
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            lib-id   (uuid/next)
            style    (ctas/make-animation-style {:name "Kit pop" :entries [{:type :fade :duration 250}]})
            _        (ptk/emit! store #(assoc-in % [:files lib-id]
                                                 {:id lib-id :name "Kit"
                                                  :data {:id lib-id :animation-styles {(:id style) style}}}))
            ^js kit-style    (aget (.-animationStyles (library/library-proxy plugin-id lib-id)) 0)
            [^js board rects] (board-with-rects context 1)
            ^js timeline      (.addTimeline board)]
        (t/is (= "Kit pop" (.-name kit-style)))
        (t/is (thrown-with-msg? js/Error #"another file" (set! (.-name kit-style) "Mine")))
        (t/is (thrown-with-msg? js/Error #"another file" (set! (.-entries kit-style) #js [#js {:type "fade"}])))
        (t/is (thrown-with-msg? js/Error #"another file" (.remove kit-style)))
        (t/is (thrown? js/Error (.createAnimationStyle (library/library-proxy plugin-id lib-id)
                                                       #js [#js {:type "fade"}])))
        (t/testing "its layers of the file play it"
          (let [^js use (.applyAnimationStyle timeline (first rects) kit-style 100)]
            (t/is (= ["Kit pop" 100 250] [(.-name use) (.-start use) (.-duration use)]))
            (t/is (= (str lib-id) (.-libraryId (.-style use))))))))))

(t/deftest bad-style-entries-are-refused
  (thw/with-wasm-mocks*
    (fn []
      (let [[_ ^js context] (setup true)
            ^js lib (.. context -library -local)
            create  #(.createAnimationStyle lib (clj->js %))]
        (t/is (thrown? js/Error (create [])) "no entries")
        (t/is (thrown? js/Error (create (repeat 33 {:type "fade"}))) "more than 32")
        (t/is (thrown-with-msg? js/Error #"Entry 2" (create [{:type "fade"} {:type "shake"}])) "an unknown type")
        (t/is (thrown? js/Error (create [{:type "fade" :start -10}])) "a negative start")
        (t/is (thrown? js/Error (.createAnimationStyle lib "fade")) "not an array")
        (t/testing "keyframe entries"
          (let [row (fn [n] {:type "keyframes" :property "x"
                             :keyframes (mapv #(hash-map :time % :value %) (range n))})]
            (t/is (thrown-with-msg? js/Error #"Keyframe 2"
                                    (create [{:type "keyframes" :property "x"
                                              :keyframes [{:time 0 :value 0} {:time 100 :value "#ff0000"}]}]))
                  "a colour on a position")
            (t/is (thrown-with-msg? js/Error #"within the duration"
                                    (create [{:type "keyframes" :property "x" :duration 100
                                              :keyframes [{:time 0 :value 0} {:time 200 :value 1}]}])))
            (t/is (thrown-with-msg? js/Error #"Not a property" (create [{:type "keyframes" :property "colour"
                                                                         :keyframes [{:time 0 :value 0}]}])))
            (t/is (thrown-with-msg? js/Error #"256 keyframes" (create [(row 257)])))
            (t/is (thrown-with-msg? js/Error #"256 keyframes" (create [(row 130) (row 130)])) "in all")))
        (t/is (zero? (.-length (.-animationStyles lib))) "nothing was made")))))

(t/deftest a-plugin-makes-a-style-of-keyframes
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup true)
            [^js board rects]   (board-with-rects context 2)
            [^js one ^js two]   rects
            _                   (set! (.-x two) 50)
            ^js lib             (.. context -library -local)
            ^js style           (.createAnimationStyle
                                 lib
                                 #js [#js {:type "keyframes" :property "rotation" :start 0 :duration 600
                                           :keyframes #js [#js {:time 0 :value 0 :easing "linear"}
                                                           #js {:time 300 :value 8 :easing "linear"}
                                                           #js {:time 600 :value 0}]}
                                      #js {:type "keyframes" :property "x"
                                           :keyframes #js [#js {:time 0 :value 0 :easing "linear"}
                                                           #js {:time 400 :value 40}]}])
            ^js timeline        (.addTimeline board #js {:duration 1000})
            entries             #(mapv (fn [^js e] [(.-type e) (.-property e) (.-start e) (.-duration e)
                                                    (.-length (.-keyframes e))])
                                       (.-entries style))]
        (doseq [rect rects]
          (.applyAnimationStyle timeline rect style 0))

        (t/testing "its entries read back"
          (t/is (= [["keyframes" "rotation" 0 600 3] ["keyframes" "x" 0 400 2]] (entries)))
          (let [^js wobble (aget (.-entries style) 0)]
            (t/is (= [{"time" 300 "value" 8 "easing" "linear"}]
                     (js->clj (.slice (.-keyframes wobble) 1 2)))))
          (let [before (js->clj (.-entries style))]
            (set! (.-entries style) (.-entries style))
            (t/is (= before (js->clj (.-entries style))) "and write back the same")))

        (t/testing "two layers play them from where they rest"
          (t/is (= [40 90] [(.valueAt timeline one "x" 400) (.valueAt timeline two "x" 400)]))
          (t/is (= [8 8] [(.valueAt timeline one "rotation" 300) (.valueAt timeline two "rotation" 300)])))

        (t/testing "an update from a layer reaches every use"
          (.addKeyframe timeline one #js {:property "y" :time 0 :value 0 :easing "linear"})
          (.addKeyframe timeline one #js {:property "y" :time 500 :value 30})
          (.updateFromShape style one)
          (t/is (= #{"rotation" "x" "y"} (into #{} (map #(.-property ^js %)) (.-entries style))))
          (t/is (empty? (:keyframes (stored-track store context board one))) "the keyframes went into it")
          (t/is (= 30 (.valueAt timeline two "y" 500)))
          (t/is (= [1 1] (mapv #(count (:animations (stored-track store context board %))) rects))))

        (t/testing "a layer that does not use it is refused"
          (t/is (thrown-with-msg? js/Error #"does not use" (.updateFromShape style board))))))))

(t/deftest the-style-members-need-motion
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context] (setup false)
            ^js lib (.. context -library -local)
            style   (ctas/make-animation-style {:name "Pop" :entries [{:type :fade :duration 250}]})
            file-id (aget (. context -currentFile) "$id")]
        (t/is (thrown? js/Error (.createAnimationStyle lib #js [#js {:type "fade"}])))
        (ptk/emit! store #(assoc-in % [:files file-id :data :animation-styles (:id style)] style))
        (let [^js pop (aget (.-animationStyles lib) 0)]
          (t/is (= "Pop" (.-name pop)) "they can be read")
          (t/is (thrown? js/Error (set! (.-name pop) "Bounce"))))))))

;; AUDIO CLIPS

(defn- audio-file-of
  "Board A with a timeline of 3 s and, in it, a clip of a 4 s file from
  500 ms, playing 2 s of it from 1 s, on lane 0; with `other?`, a clip of
  another file on lane 1 too: `[file clip-id other-id]`."
  [& {:keys [other?]}]
  (let [file     (-> (cthf/sample-file :audio-file :page-label :audio-page)
                     (ctho/add-frame :board-a :x 0 :y 0 :width 400 :height 300))
        board    (cthi/id :board-a)
        clip-id  (uuid/next)
        other-id (uuid/next)
        timeline (cond-> (-> (cta/make-timeline {:board-id board :duration 3000})
                             (ctau/add-audio-clip {:id clip-id :audio-id (uuid/next) :name "beat.mp3"
                                                   :source-duration 4000 :start 500 :offset 1000
                                                   :duration 2000 :lane 0}))
                   other? (ctau/add-audio-clip {:id other-id :audio-id (uuid/next) :name "voice.wav"
                                                :source-duration 2000 :start 0 :duration 2000 :lane 1}))]
    [(cthf/apply-changes file (-> (pcb/empty-changes nil)
                                  (pcb/with-page (cthf/current-page file))
                                  (pcb/set-timeline board timeline)))
     clip-id
     other-id]))

(t/deftest a-timeline-plays-audio-clips
  (thw/with-wasm-mocks*
    (fn []
      (let [[file clip-id]      (audio-file-of)
            [store ^js context] (setup file true)
            ^js timeline        (.-timeline (shape-of file :board-a))
            stored              #(get-in (stored-timelines store context) [(cthi/id :board-a) :audio-clips %])
            ^js clip            (first (.-audioClips timeline))
            spans               #(mapv (fn [^js clip] [(.-start clip) (.-duration clip)]) (.-audioClips timeline))]

        (t/testing "they read as they are stored"
          (t/is (= [(str clip-id) "beat.mp3" 4000 500 1000 2000 0 1 false 0 0 false]
                   [(.-id clip) (.-name clip) (.-sourceDuration clip) (.-start clip) (.-offset clip)
                    (.-duration clip) (.-lane clip) (.-volume clip) (.-muted clip) (.-fadeIn clip)
                    (.-fadeOut clip) (.-locked clip)])))

        (t/testing "they change, each fade at most half of the clip"
          (set! (.-start clip) 250)
          (set! (.-volume clip) 0.5)
          (set! (.-muted clip) true)
          (set! (.-fadeIn clip) 300)
          (set! (.-fadeOut clip) 5000)
          (set! (.-lane clip) 2)
          (set! (.-duration clip) 1500)
          (t/is (= [250 1500 2 0.5 true 300 750]
                   ((juxt :start :duration :lane :volume :muted :fade-in :fade-out) (stored clip-id))))
          (set! (.-offset clip) 3000)
          (t/is (= [3000 1000] ((juxt :offset :duration) (stored clip-id))) "shorter where the file ends")
          (t/is (= 3000 (.-duration timeline)) "the timeline keeps its length"))

        (t/testing "a start before 0, as in the dock"
          (set! (.-start clip) -300)
          (t/is (= -300 (:start (stored clip-id))))
          (set! (.-start clip) 250))

        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (set! (.-volume clip) 5)) "four times as loud at most")
          (t/is (thrown? js/Error (set! (.-start clip) "soon")))
          (t/is (thrown? js/Error (set! (.-lane clip) 1.5)))
          (t/is (thrown? js/Error (set! (.-muted clip) "yes")))
          (t/is (thrown? js/Error (set! (.-duration clip) 0)))
          (t/is (thrown? js/Error (.split clip -5))))

        (t/testing "it splits at a time of the timeline"
          (let [^js right (.split clip 750)]
            (t/is (= [[250 500] [750 500]] (spans)))
            (t/is (= [3000 3500] [(.-offset clip) (.-offset right)]) "the same file, lined up")
            (t/is (nil? (.split clip 5000)) "nothing outside it")
            (.remove right)
            (t/is (= [[250 500]] (spans)))))

        (t/testing "a locked clip takes no write but its unlocking"
          (set! (.-locked clip) true)
          (t/is (thrown? js/Error (set! (.-start clip) 0)))
          (t/is (thrown? js/Error (.split clip 400)))
          (t/is (thrown? js/Error (.remove clip)))
          (t/is (= [[250 500]] (spans)))
          (set! (.-locked clip) false)
          (set! (.-start clip) 0)
          (t/is (= [[0 500]] (spans))))

        (t/testing "writes need content:write"
          (with-redefs [r/check-permission (constantly false)]
            (t/is (thrown? js/Error (set! (.-start clip) 100))))
          (t/is (= [[0 500]] (spans))))))))

(t/deftest a-clip-plays-faster-or-slower
  (thw/with-wasm-mocks*
    (fn []
      (let [[file _]     (audio-file-of)
            _            (setup file true)
            ^js timeline (.-timeline (shape-of file :board-a))
            ^js clip     (first (.-audioClips timeline))]
        (t/is (= [1 2000] [(.-speed clip) (.-duration clip)]))
        (set! (.-speed clip) 2)
        (t/is (= [2 1000 1000] [(.-speed clip) (.-duration clip) (.-offset clip)])
              "the same part of its file, in half the time")
        (set! (.-speed clip) 0.5)
        (t/is (= [0.5 4000] [(.-speed clip) (.-duration clip)]))
        (t/is (thrown? js/Error (set! (.-speed clip) 0.1)))
        (t/is (thrown? js/Error (set! (.-speed clip) 5)))
        (t/is (thrown? js/Error (set! (.-speed clip) "fast")))
        (t/is (= 0.5 (.-speed clip)))))))

(t/deftest a-lane-plays-a-single-file
  (thw/with-wasm-mocks*
    (fn []
      (let [[file clip-id] (audio-file-of :other? true)
            _              (setup file true)
            ^js timeline   (.-timeline (shape-of file :board-a))
            ^js clip       (some #(when (= (str clip-id) (.-id ^js %)) %) (.-audioClips timeline))]
        (t/is (thrown? js/Error (set! (.-lane clip) 1)) "not onto the lane of another file")
        (t/is (= 0 (.-lane clip)))
        (set! (.-lane clip) 2)
        (t/is (= 2 (.-lane clip)) "onto an empty one")))))

(t/deftest a-clip-renames-its-file
  ;; All the clips that play it, a locked one too; another file stays.
  (thw/with-wasm-mocks*
    (fn []
      (let [[file clip-id other-id] (audio-file-of :other? true)
            _                       (setup file true)
            ^js timeline            (.-timeline (shape-of file :board-a))
            by-id                   (fn [id] (some #(when (= (str id) (.-id ^js %)) %) (.-audioClips timeline)))
            ^js clip                (by-id clip-id)
            ^js right               (.split clip 1500)
            names                   #(mapv (fn [^js c] (.-name c)) [clip right (by-id other-id)])]
        (set! (.-locked right) true)
        (set! (.-name clip) "  Drums ")
        (t/is (= ["Drums" "Drums" "voice.wav"] (names)))
        (t/is (thrown? js/Error (set! (.-name clip) " ")) "not blank")
        (t/is (thrown? js/Error (set! (.-name clip) 7)))
        (t/is (= ["Drums" "Drums" "voice.wav"] (names)))))))

(t/deftest audio-clips-need-motion
  (thw/with-wasm-mocks*
    (fn []
      (let [[file _]     (audio-file-of)
            _            (setup file false)
            ^js timeline (motion/timeline-proxy plugin-id (:id file) (cthf/current-page-id file)
                                                (cthi/id :board-a))
            ^js clip     (first (.-audioClips timeline))]
        (t/is (= 500 (.-start clip)) "they read")
        (t/is (thrown? js/Error (set! (.-start clip) 100)))
        (t/is (= 500 (.-start clip)))))))

(defn- fake-decode
  "Decodes the bytes \"ms:N\" to N ms of audio, and nothing else."
  [bytes]
  (let [text (.decode (js/TextDecoder.) bytes)]
    (if (.startsWith text "ms:")
      (let [ms (js/parseInt (subs text 3))
            n  (js/Math.round (* 48 ms))]
        (js/Promise.resolve #js {:duration (/ ms 1000) :length n :sampleRate 48000 :numberOfChannels 1
                                 :getChannelData (fn [_] (js/Float32Array. n))}))
      (js/Promise.reject (js/Error. "undecodable")))))

(defn- upload-stub
  "An `rp/cmd!` that uploads a file media object `id`, recording each
  upload in `calls`; other commands answer nothing."
  [calls id]
  (mock/stub
   (fn [cmd params]
     (if (= :upload-file-media-object cmd)
       (do (swap! calls conj [(:name params) (.-type (:content params))])
           (rx/of {:id id :mtype (.-type (:content params))}))
       (rx/empty)))))

(defn- text-bytes
  [text]
  (.encode (js/TextEncoder.) text))

(t/deftest a-plugin-adds-audio-from-bytes
  (t/async
    done
    (let [[file _]            (audio-file-of)
          [store ^js context] (setup file true)
          ^js timeline        (motion/timeline-proxy plugin-id (:id file) (cthf/current-page-id file)
                                                     (cthi/id :board-a))
          calls               (atom [])
          audio-id            (uuid/next)]
      (thw/setup-wasm-mocks!)
      (mock/with-mocks
        {rp/cmd! (upload-stub calls audio-id)
         audio/decode (mock/stub fake-decode)}
        (fn [done']
          (-> (.addAudioClip timeline "voice.wav" (text-bytes "ms:2500") "audio/x-wav" #js {:start 1200 :lane 1})
              (.then (fn [^js clip]
                       (let [stored (get-in (stored-timelines store context)
                                            [(cthi/id :board-a) :audio-clips (uuid/uuid (.-id clip))])]
                         (t/is (= [["voice.wav" "audio/wav"]] @calls) "uploaded as its canonical type")
                         (t/is (= [audio-id "voice.wav" 1200 0 2500 2500 1]
                                  ((juxt :audio-id :name :start :offset :duration :source-duration :lane) stored)))
                         (t/is (= 2 (.-length (.-audioClips timeline))))
                         (t/is (= 3000 (.-duration timeline)) "the timeline keeps its length")
                         (t/is (some? (audio/waveform audio-id)) "decoded once, not fetched again"))))
              (.catch (fn [cause] (t/is (nil? cause) (str cause))))
              (.finally (fn []
                          (thw/teardown-wasm-mocks!)
                          (done')))))
        done))))

(t/deftest audio-that-is-not-valid-is-not-uploaded
  (t/async
    done
    (let [[file _]            (audio-file-of)
          [store ^js context] (setup file true)
          ^js timeline        (motion/timeline-proxy plugin-id (:id file) (cthf/current-page-id file)
                                                     (cthi/id :board-a))
          before              (stored-timelines store context)
          calls               (atom [])
          add                 #(.addAudioClip timeline %1 %2 %3 %4)
          rejects             (fn [label promise]
                                (-> promise
                                    (.then (fn [_] (t/is false label)))
                                    (.catch (fn [_] (t/is true label)))))]
      (thw/setup-wasm-mocks!)
      (mock/with-mocks
        {rp/cmd! (upload-stub calls (uuid/next))
         audio/decode (mock/stub fake-decode)}
        (fn [done']
          (-> (js/Promise.all
               #js [(rejects "an image" (add "x.png" (text-bytes "ms:100") "image/png" nil))
                    (rejects "too long" (add "long.mp3" (text-bytes (str "ms:" (inc ctau/max-audio-duration)))
                                             "audio/mpeg" nil))
                    (rejects "undecodable" (add "noise.mp3" (text-bytes "noise") "audio/mpeg" nil))
                    (rejects "no bytes" (add "x.mp3" "ms:100" "audio/mpeg" nil))
                    (rejects "a start that is no number" (add "x.mp3" (text-bytes "ms:100") "audio/mpeg" #js {:start "soon"}))
                    (rejects "a lane of another file" (add "x.mp3" (text-bytes "ms:100") "audio/mpeg" #js {:lane 0}))
                    (rejects "without content:write"
                             (with-redefs [r/check-permission (constantly false)]
                               (add "x.mp3" (text-bytes "ms:100") "audio/mpeg" nil)))])
              (.then (fn [_]
                       (t/is (empty? @calls) "no upload")
                       (t/is (= before (stored-timelines store context)))))
              (.finally (fn []
                          (thw/teardown-wasm-mocks!)
                          (done')))))
        done))))

(t/deftest style-entries-carry-their-tokens
  (let [entry (cta/make-style-entry {:type :move :start 0 :duration 150
                                     :applied-tokens {:duration "motion.fast" :offset-y "space.l"}})
        ^js js (motion/format-style-entry entry)]
    (t/is (= {"duration" "motion.fast" "offsetY" "space.l"} (js->clj (.-appliedTokens js))))
    (t/is (= {:duration "motion.fast" :offset-y "space.l"}
             (:applied-tokens (first (motion/parse-style-entries #js [js]))))
          "and read them back")
    (t/is (nil? (.-appliedTokens ^js (motion/format-style-entry (dissoc entry :applied-tokens)))))
    (t/testing "what is not valid is refused"
      (let [entry-of #(motion/parse-style-entries
                       #js [#js {:type "fade" :start 0 :duration 300 :appliedTokens %}])]
        (t/is (string? (entry-of #js {:offsetX "space.s"})) "a fade has no offset")
        (t/is (string? (entry-of #js {:color "brand"})) "no token sets a color")
        (t/is (string? (entry-of #js {:duration "not a name"})))
        (t/is (string? (entry-of "motion.fast")))))))

(defn- tokens-file
  "A board :board-a holding a rect :r1, and the active tokens
  `motion.base` (300 ms) and `ease.spring` (a spring)."
  []
  (-> (cthf/sample-file :file1 :page-label :page1)
      (ctho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
      (cths/add-sample-shape :r1 :type :rect :parent-label :board-a
                             :x 10 :y 10 :width 50 :height 50)
      (ctht/add-tokens-lib)
      (ctht/update-tokens-lib
       #(-> %
            (ctob/add-set (ctob/make-token-set :id (cthi/new-id! :motion-set) :name "motion"))
            (ctob/add-theme (ctob/make-token-theme :id (cthi/new-id! :motion-theme)
                                                   :name "theme" :sets #{"motion"}))
            (ctob/add-token (cthi/id :motion-set)
                            (ctob/make-token :name "motion.base" :type :duration :value "300ms"))
            (ctob/add-token (cthi/id :motion-set)
                            (ctob/make-token :name "ease.spring" :type :spring
                                             :value {:stiffness 120 :damping 14 :mass 1}))))
      (ctht/update-tokens-status
       #(ctos/set-tokens-status % #{(cthi/id :motion-theme)} #{(cthi/id :motion-set)}))))

(t/deftest presets-and-keyframes-bind-tokens
  (t/async
    done
    (thw/setup-wasm-mocks!)
    (let [[store ^js context] (setup (tokens-file) true)
          ^js page     (.-currentPage context)
          ^js board    (.getShapeById page (str (cthi/id :board-a)))
          ^js rect     (.getShapeById page (str (cthi/id :r1)))
          ^js timeline (.addTimeline board)
          ^js fade     (.addAnimation timeline rect #js {:type "fade" :start 0 :duration 500
                                                         :appliedTokens #js {:duration "motion.base"}})
          ^js from     (.addKeyframe timeline rect #js {:property "x" :time 0 :value 0})]
      (.addKeyframe timeline rect #js {:property "x" :time 1000 :value 100})
      (set! (.-appliedTokens from) #js {:easing "ease.spring"})

      (t/testing "they read back"
        (t/is (= {"duration" "motion.base"} (js->clj (.-appliedTokens fade))))
        (t/is (= {"easing" "ease.spring"} (js->clj (.-appliedTokens from)))))

      (t/testing "what is not valid is refused"
        (t/is (thrown? js/Error (set! (.-appliedTokens fade) #js {:offsetX "space.s"})) "a fade has no offset")
        (t/is (thrown? js/Error (set! (.-appliedTokens from) #js {:duration "motion.base"}))
              "a keyframe binds its easing only")
        (t/is (thrown? js/Error (.addKeyframe timeline rect #js {:property "y" :time 0 :value 0
                                                                 :appliedTokens "ease.spring"}))))

      (tohs/run-store-async
       store
       (fn []
         (thw/teardown-wasm-mocks!)
         (done))
       [(dwtp/propagate-style-tokens)]
       (fn [_]
         (t/testing "the values follow the tokens"
           (t/is (= 300 (.-duration fade)))
           (t/is (= {"type" "spring" "stiffness" 120 "damping" 14 "mass" 1}
                    (js->clj (.-easing from))))
           (let [layer (first (get (js->clj (.toJSON timeline)) "layers"))
                 x     (some #(when (= "x" (get % "property")) %) (get layer "properties"))]
             (t/is (= {"duration" "motion.base"} (get-in layer ["presets" 0 "tokens"])))
             (t/is (= {"easing" "ease.spring"} (get-in x ["keyframes" 0 "tokens"])))))

         (t/testing "a setting set by hand unbinds its token"
           (set! (.-duration fade) 400)
           (t/is (nil? (.-appliedTokens fade)))
           (set! (.-easing from) "linear")
           (t/is (nil? (.-appliedTokens from)))))))))
