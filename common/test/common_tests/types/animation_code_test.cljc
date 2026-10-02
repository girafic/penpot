;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.types.animation-code-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.logic.timelines :as cltl]
   [app.common.test-helpers.components :as thc]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.types.animation :as cta]
   [app.common.types.animation-code :as ctac]
   [app.common.types.animation-style :as ctas]
   [app.common.types.audio-clip :as ctau]
   [app.common.uuid :as uuid]
   [clojure.string :as cstr]
   [clojure.test :as t]))

(t/use-fixtures :each thi/test-fixture)

(defn- scene
  "An animated component, its main a board whose rect moves over and
  over, and board B holding a copy of it, then a rect :back and on top of
  it a rect :front. B animates itself, :front (a fade in and an x, its
  opacity row hidden) and :back (a hidden preset only). `[file code]`."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-simple-component :anim :main :main-rect
                                           :root-params {:x 0 :y 0 :width 200 :height 200}
                                           :child-params {:x 10 :y 20 :width 50 :height 50})
                 (tho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                 (thc/instantiate-component :anim :copy :parent-label :board-b)
                 (tho/add-rect :back :parent-label :board-b :x 1100 :y 150 :width 40 :height 40)
                 (tho/add-rect :front :parent-label :board-b :x 1200 :y 150 :width 40 :height 40))
        main  (thi/id :main)
        board (thi/id :board-b)
        front (thi/id :front)
        mtl   (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                  (cta/add-keyframe (thi/id :main-rect) {:time 0 :property :x :value 10})
                  (cta/add-keyframe (thi/id :main-rect) {:time 1000 :property :x :value 60}))
        btl   (-> (cta/make-timeline {:board-id board :duration 1200})
                  (cta/add-keyframe board {:time 0 :property :opacity :value 0.5})
                  (cta/add-keyframe board {:time 600 :property :opacity :value 1})
                  (cta/add-keyframe front {:time 0 :property :x :value 200})
                  (cta/add-keyframe front {:time 400 :property :x :value 240})
                  (cta/add-keyframe front {:time 0 :property :opacity :value 1})
                  (cta/add-animation front {:type :fade :start 0 :duration 400})
                  (assoc-in [:tracks front :hidden] #{:opacity})
                  (cta/add-animation (thi/id :back) {:type :scale :hidden true}))
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page (thf/current-page file))
                                          (pcb/set-timeline main mtl)
                                          (pcb/set-timeline board btl)))
        page  (thf/current-page file)]
    [file (cltl/code-timelines page {(:id file) file} board)]))

(t/deftest code-layers-in-the-order-of-the-layers
  (let [[file code] (scene)
        objects     (:objects (thf/current-page file))
        ids         (mapv :id (ctac/code-layers code objects))
        copy-rect   (first (get-in objects [(thi/id :copy) :shapes]))]
    (t/is (= [(thi/id :board-b) (thi/id :front) copy-rect] ids)
          "the board, then top-most first; :back plays nothing it shows")
    (t/is (= [(thi/id :front)]
             (mapv :id (ctac/code-layers code objects :ids #{(thi/id :front)}))))))

(t/deftest a-code-layer-has-its-track-as-designed-and-as-it-plays
  (let [[file code] (scene)
        objects     (:objects (thf/current-page file))
        front       (thi/id :front)
        layer       (first (ctac/code-layers code objects :ids #{front}))
        properties  #(into #{} (map :property) (:keyframes %))]
    (t/is (= [:fade] (mapv :type (get-in layer [:source :animations]))) "the preset as designed")
    (t/is (= #{:x} (properties (:source layer))) "without the hidden row")
    (t/is (nil? (get-in layer [:track :animations])))
    (t/is (= #{:x :opacity} (properties (:track layer))) "the fade as keyframes")
    (t/is (= (cta/rest-pose (:timeline code) objects front) (:rest layer)))
    (t/is (= {:x 0.5 :y 0.5} (:origin layer)))
    (t/is (nil? (:copy layer)))))

(t/deftest a-copy-layer-plays-on-the-clock-of-its-main
  (let [[file code] (scene)
        objects     (:objects (thf/current-page file))
        copy-rect   (first (get-in objects [(thi/id :copy) :shapes]))
        layer       (first (ctac/code-layers code objects :ids #{copy-rect}))]
    (t/is (= (thi/id :copy) (get-in layer [:copy :head-id])))
    (t/is (= [1000 :loop] ((juxt :duration :playback) (:clock layer))))
    (t/is (= 1200 (:duration (:timeline code))) "the board keeps its own")))

(t/deftest property-names-cover-the-model
  (t/is (= cta/animatable-properties (set (keys ctac/property-names))))
  (t/is (= cta/animatable-properties (set ctac/property-order)))
  (t/is (= (count ctac/property-order) (count (set ctac/property-order))) "each once"))

;; JSON

(defn- board-scene
  "Board :board at 0,0 holding the rects :a (at 20,20) and :b, its
  timeline `f` of an empty one of 1200 ms: `[objects code]`."
  [f]
  (let [file  (-> (thf/sample-file :file1)
                  (tho/add-frame :board :x 0 :y 0 :width 400 :height 300 :name "Card")
                  (tho/add-rect :a :parent-label :board :x 20 :y 20 :width 40 :height 40)
                  (tho/add-rect :b :parent-label :board :x 100 :y 20 :width 40 :height 40))
        board (thi/id :board)
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page (thf/current-page file))
                                          (pcb/set-timeline board (f (cta/make-timeline {:board-id board
                                                                                         :duration 1200})))))
        page  (thf/current-page file)]
    [(:objects page) (cltl/code-timelines page {(:id file) file} board)]))

(defn- layer-json
  "The JSON of the layer `label` in `doc`."
  [doc label]
  (let [id (str (thi/id label))]
    (some #(when (= id (get % "id")) %) (get doc "layers"))))

(defn- property-json
  [layer property]
  (some #(when (= property (get % "property")) %) (get layer "properties")))

(t/deftest the-json-is-versioned
  (let [[objects code] (board-scene identity)
        doc (ctac/motion-json code objects)]
    (t/is (= ["version" "generator" "board" "duration" "playback" "markers" "layers" "audio"] (keys doc)))
    (t/is (= [1 "Penpot" 1200 "once"] ((juxt #(get % "version") #(get % "generator") #(get % "duration") #(get % "playback")) doc)))
    (t/is (= {"id" (str (thi/id :board)) "name" "Card" "width" 400 "height" 300} (get doc "board")))
    (t/is (not (contains? doc "loopCount")))))

(t/deftest each-segment-has-its-easing
  (let [[objects code]
        (board-scene #(-> %
                          (cta/add-keyframe (thi/id :a) {:time 0 :property :x :value 20 :easing :ease-in-out})
                          (cta/add-keyframe (thi/id :a) {:time 100 :property :x :value 30
                                                         :easing {:type :bezier :curve [0.1 0.2 0.3 0.4]}})
                          (cta/add-keyframe (thi/id :a) {:time 200 :property :x :value 40
                                                         :easing {:type :spring :stiffness 300 :damping 20 :mass 1}})
                          (cta/add-keyframe (thi/id :a) {:time 300 :property :x :value 50 :interpolation :step})
                          (cta/add-keyframe (thi/id :a) {:time 400 :property :x :value 60})
                          (cta/add-keyframe (thi/id :a) {:time 500 :property :x :value 70})))
        doc       (ctac/motion-json code objects)
        keyframes (get (property-json (layer-json doc :a) "x") "keyframes")
        easings   (mapv #(get % "easing") keyframes)]
    (t/is (= [0 100 200 300 400 500] (mapv #(get % "time") keyframes)))
    (t/is (= ["bezier" "ease-in-out"] ((juxt #(get % "type") #(get % "name")) (nth easings 0))))
    (t/is (= {"type" "bezier" "curve" [0.1 0.2 0.3 0.4]} (nth easings 1)))
    (t/is (= {"type" "spring" "stiffness" 300 "damping" 20 "mass" 1} (nth easings 2)))
    (t/is (= {"type" "hold"} (nth easings 3)))
    (t/is (= {"type" "bezier" "curve" [0 0 1 1] "name" "linear"} (nth easings 4)) "none is linear")
    (t/is (nil? (nth easings 5)) "the last keyframe starts no segment")))

(t/deftest a-fade-in-is-keyframes-and-a-preset
  (let [[objects code] (board-scene #(cta/add-animation % (thi/id :a) {:type :fade :start 0 :duration 400}))
        layer   (layer-json (ctac/motion-json code objects) :a)
        opacity (get (property-json layer "opacity") "keyframes")]
    (t/is (= [[0 0] [400 1]] (mapv (juxt #(get % "time") #(get % "value")) opacity)))
    (t/is (= "ease-out" (get-in opacity [0 "easing" "name"])) "an entrance eases out")
    (t/is (= "rectangle" (get layer "type")) "types named as in the plugin API")
    (t/is (= {"type" "fade" "direction" "in" "start" 0 "duration" 400}
             (select-keys (first (get layer "presets")) ["type" "direction" "start" "duration"])))))

(t/deftest the-presets-of-a-style-use-name-their-style
  (let [style (ctas/make-animation-style {:name "Card enter"
                                          :entries [{:type :fade :start 0 :duration 300
                                                     :applied-tokens {:duration "motion.fast"}}]})
        [objects code] (board-scene #(cta/add-animation % (thi/id :b) (ctas/make-use style (uuid/next) 500)))
        preset (first (get (layer-json (ctac/motion-json code objects) :b) "presets"))]
    (t/is (= ["fade" 500 "Card enter" {"duration" "motion.fast"}]
             ((juxt #(get % "type") #(get % "start") #(get % "style") #(get % "tokens")) preset)))))

(t/deftest loops-paths-and-hidden-rows
  (let [[objects code]
        (board-scene #(-> %
                          (cta/add-keyframe (thi/id :a) {:time 0 :property :x :value 20 :path-out 0.3})
                          (cta/add-keyframe (thi/id :a) {:time 600 :property :x :value 80 :path-in 0.25})
                          (cta/add-keyframe (thi/id :a) {:time 0 :property :rotation :value 0})
                          (cta/add-keyframe (thi/id :a) {:time 300 :property :rotation :value 90})
                          (cta/add-keyframe (thi/id :a) {:time 0 :property :opacity :value 1})
                          (assoc-in [:tracks (thi/id :a) :loops] #{:rotation})
                          (assoc-in [:tracks (thi/id :a) :hidden] #{:opacity})
                          (cta/add-animation (thi/id :a) {:type :scale :hidden true})))
        layer (layer-json (ctac/motion-json code objects) :a)]
    (t/is (= ["x" "rotation"] (mapv #(get % "property") (get layer "properties")))
          "in the order of the dock, the hidden opacity left out")
    (t/is (true? (get (property-json layer "rotation") "loop")))
    (t/is (= [0 300] (mapv #(get % "time") (get (property-json layer "rotation") "keyframes"))) "one cycle")
    (t/is (= [0.3 0.25] ((juxt #(get-in % ["keyframes" 0 "pathOut"]) #(get-in % ["keyframes" 1 "pathIn"]))
                         (property-json layer "x"))))
    (t/is (= [] (get layer "presets")) "the hidden preset left out")))

(t/deftest a-split-text-plays-whole-and-says-so
  (let [file  (-> (thf/sample-file :file1)
                  (tho/add-frame :board :x 0 :y 0 :width 400 :height 300)
                  (tho/add-text :title "Hi you there" :text-params {:parent-label :board}))
        board (thi/id :board)
        tl    (cta/add-animation (cta/make-timeline {:board-id board :duration 1000})
                                 (thi/id :title) {:type :fade :split :word :start 0 :duration 500})
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page (thf/current-page file))
                                          (pcb/set-timeline board tl)))
        page  (thf/current-page file)
        layer (layer-json (ctac/motion-json (cltl/code-timelines page {} board) (:objects page)) :title)]
    (t/is (= "word" (get layer "split")))
    (t/is (= [0 500] (mapv #(get % "time") (get (property-json layer "opacity") "keyframes")))
          "keyframes of the whole layer")))

(t/deftest a-copy-layer-has-the-clock-of-its-main
  (let [[file code] (scene)
        objects   (:objects (thf/current-page file))
        copy-rect (first (get-in objects [(thi/id :copy) :shapes]))
        layer     (some #(when (= (str copy-rect) (get % "id")) %)
                        (get (ctac/motion-json code objects) "layers"))]
    (t/is (= {"copy" (str (thi/id :copy))
              "name" (get-in objects [(thi/id :copy) :name])
              "duration" 1000
              "playback" "loop"}
             (get layer "component")))))

(t/deftest markers-and-audio-clips
  (let [[objects code]
        (board-scene #(-> %
                          (assoc :markers [{:id (uuid/next) :time 800 :name "out"}
                                           {:id (uuid/next) :time 0 :name "intro"}])
                          (ctau/add-audio-clip {:id (uuid/next) :audio-id (uuid/next) :name "beat.mp3"
                                                :source-duration 4000 :start 100 :duration 1000
                                                :fade-out 200})))
        doc (ctac/motion-json code objects)]
    (t/is (= [{"name" "intro" "time" 0} {"name" "out" "time" 800}] (get doc "markers")))
    (t/is (= [{"name" "beat.mp3" "start" 100 "duration" 1000 "offset" 0 "speed" 1
               "volume" 1 "fadeIn" 0 "fadeOut" 200 "muted" false}]
             (get doc "audio")))))

(t/deftest json-numbers-are-equal-on-both-runtimes
  (let [[objects code] (board-scene #(cta/add-keyframe % (thi/id :a) {:time 0 :property :x :value 20.123456}))
        layer (layer-json (ctac/motion-json code objects) :a)]
    (t/is (= 20.123 (get-in (property-json layer "x") ["keyframes" 0 "value"])))
    (t/is (integer? (get-in layer ["rest" "x"])) "a whole number is an integer")))

;; REACT

(defn- react-of
  "The React code of the board of `board-scene` with timeline `f`, the
  classes of the layers their names."
  [f]
  (let [[objects code] (board-scene f)]
    (ctac/motion-react code objects :class-name #(:name %))))

(def ^:private fixture-react
  (str "import { MotionConfig, motion } from \"motion/react\";\n"
       "import { steps } from \"motion\";\n"
       "\n"
       "/* Card: 1200 ms, loop. */\n"
       "export function CardMotion() {\n"
       "  return (\n"
       "    <MotionConfig reducedMotion=\"user\">\n"
       "      {/* Rect1 */}\n"
       "      <motion.div\n"
       "        className=\"Rect1\"\n"
       "        style={{ originX: 0.5, originY: 0.5 }}\n"
       "        initial={{ x: 0, opacity: 0 }}\n"
       "        animate={{ x: [0, 40, 40], opacity: [0, 1, 1] }}\n"
       "        transition={{\n"
       "          x: { duration: 1.2, times: [0, 0.3333, 1], ease: [[0, 0, 0.58, 1], \"linear\"], repeat: Infinity },\n"
       "          opacity: { duration: 1.2, times: [0, 0.5, 1], ease: [steps(1, \"end\"), \"linear\"], repeat: Infinity },\n"
       "        }}\n"
       "      />\n"
       "    </MotionConfig>\n"
       "  );\n"
       "}\n"))

(t/deftest react-of-a-fixture
  (t/is (= fixture-react
           (react-of #(-> %
                          (assoc :playback :loop)
                          (cta/add-keyframe (thi/id :a) {:time 0 :property :x :value 20 :easing :ease-out})
                          (cta/add-keyframe (thi/id :a) {:time 400 :property :x :value 60})
                          (cta/add-keyframe (thi/id :a) {:time 0 :property :opacity :value 0 :interpolation :step})
                          (cta/add-keyframe (thi/id :a) {:time 600 :property :opacity :value 1}))))))

(t/deftest react-repeats-as-the-clock-plays
  (let [move   #(-> %
                    (cta/add-keyframe (thi/id :a) {:time 0 :property :x :value 20})
                    (cta/add-keyframe (thi/id :a) {:time 400 :property :x :value 60}))
        once   (react-of move)
        pong   (react-of #(-> % (assoc :playback :ping-pong) move))]
    (t/is (not (cstr/includes? once "repeat")) "played once, the last values stay")
    (t/is (cstr/includes? pong "repeat: Infinity, repeatType: \"reverse\""))
    (t/is (cstr/includes? once "ease: [\"linear\", \"linear\"]") "no easing is linear")
    (t/is (not (cstr/includes? once "steps")) "no hold, no import of steps")))

(t/deftest one-spring-segment-plays-as-a-spring
  (let [spring {:type :spring :stiffness 300 :damping 20 :mass 1}
        native (react-of #(-> %
                              (cta/add-keyframe (thi/id :a) {:time 100 :property :x :value 20 :easing spring})
                              (cta/add-keyframe (thi/id :a) {:time 500 :property :x :value 60})))
        {:keys [stiffness damping]} (cta/spring-over spring 400)]
    (t/is (cstr/includes? native (str "x: { type: \"spring\", stiffness: " (cta/format-number stiffness)
                                      ", damping: " (cta/format-number damping)
                                      ", mass: 1, delay: 0.1 }")))
    (t/is (cstr/includes? native "animate={{ x: 40 }}"))
    (t/is (not (cstr/includes? (react-of #(-> %
                                              (assoc :playback :loop)
                                              (cta/add-keyframe (thi/id :a) {:time 100 :property :x :value 20 :easing spring})
                                              (cta/add-keyframe (thi/id :a) {:time 500 :property :x :value 60})))
                               "type: \"spring\""))
          "on a loop it is sampled")))

(t/deftest scales-turns-and-the-transform-template
  (let [pop    (react-of #(cta/add-animation % (thi/id :a) {:type :scale :start 0 :duration 400}))
        wide   (react-of #(-> %
                              (cta/add-keyframe (thi/id :a) {:time 0 :property :width :value 40})
                              (cta/add-keyframe (thi/id :a) {:time 400 :property :width :value 80})))
        turned (react-of #(-> %
                              (cta/add-keyframe (thi/id :a) {:time 0 :property :rotation :value 0})
                              (cta/add-keyframe (thi/id :a) {:time 400 :property :rotation :value 90})
                              (cta/add-keyframe (thi/id :a) {:time 0 :property :width :value 40})
                              (cta/add-keyframe (thi/id :a) {:time 400 :property :width :value 80})))]
    (t/is (cstr/includes? pop "scale: [") "equal axes, one scale")
    (t/is (not (cstr/includes? pop "scaleX")))
    (t/is (cstr/includes? wide "scaleX: [1, 2, 2]"))
    (t/is (not (cstr/includes? wide "transformTemplate")))
    (t/is (cstr/includes? turned "transformTemplate=") "Penpot turns, then scales")
    (t/is (cstr/includes? turned "rotate(${withUnit(rotate, \"deg\")})")
          "motion.dev gives the turn with its unit")
    (t/is (cstr/includes? turned "const withUnit = ") "and the module the helper")
    (t/is (not (cstr/includes? wide "withUnit")))))

(t/deftest a-copy-plays-on-the-clock-of-its-main-in-react
  (let [[file code] (scene)
        objects     (:objects (thf/current-page file))
        copy-rect   (first (get-in objects [(thi/id :copy) :shapes]))
        react       (ctac/motion-react code objects :ids #{copy-rect})]
    (t/is (cstr/includes? react "x: { duration: 1, times: [0, 1], ease: [\"linear\"], repeat: Infinity }"))))

(t/deftest nothing-animates-no-react
  (let [[objects code] (board-scene identity)]
    (t/is (nil? (ctac/motion-react code objects)))))
