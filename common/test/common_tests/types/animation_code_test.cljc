;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.types.animation-code-test
  (:require
   [app.common.animation.css :as acss]
   [app.common.files.changes-builder :as pcb]
   [app.common.logic.timelines :as cltl]
   [app.common.test-helpers.components :as thc]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.common.types.animation-code :as ctac]
   [app.common.types.animation-style :as ctas]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.types.text :as txt]
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

(t/deftest the-outline-of-a-path-is-its-svg-path
  (let [outline (path/from-plain [{:command :move-to :params {:x 0 :y 0}}
                                  {:command :line-to :params {:x 40 :y 0}}
                                  {:command :line-to :params {:x 40 :y 20}}
                                  {:command :close-path :params {}}])
        [objects code] (board-scene #(cta/add-keyframe % (thi/id :a) {:time 0 :property :content :value outline}))
        values  (mapv #(get % "value") (get (property-json (layer-json (ctac/motion-json code objects) :a) "path") "keyframes"))]
    (t/is (= 1 (count values)))
    (t/is (string? (first values)))
    (t/is (cstr/starts-with? (first values) "M0") "in the own frame of its path")))

(t/deftest the-css-leaves-the-outline-of-a-path-out
  (let [outline (path/from-plain [{:command :move-to :params {:x 0 :y 0}}
                                  {:command :line-to :params {:x 40 :y 0}}
                                  {:command :close-path :params {}}])
        [objects code] (board-scene #(-> %
                                         (cta/add-keyframe (thi/id :a) {:time 0 :property :content :value outline})
                                         (cta/add-keyframe (thi/id :a) {:time 500 :property :content :value outline})))
        css (acss/timeline->css (:timeline code) objects)]
    (t/is (not (cstr/includes? css "@keyframes")) "no animation of a box")))

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

(t/deftest the-keyframe-entries-of-a-style-play-as-keyframes
  (let [style (ctas/make-animation-style {:name "Nudge"
                                          :entries [{:type :fade :start 0 :duration 300}
                                                    {:type :keys :property :x :start 0 :duration 400
                                                     :keyframes [{:time 0 :value 0 :easing :linear}
                                                                 {:time 400 :value 30}]}]})
        [objects code] (board-scene #(cta/add-animation % (thi/id :a) (ctas/make-use style (uuid/next) 100)))
        layer (layer-json (ctac/motion-json code objects) :a)]
    (t/is (= ["fade"] (mapv #(get % "type") (get layer "presets"))) "no preset of them")
    (t/is (= [[100 20] [500 50]]
             (mapv (juxt #(get % "time") #(get % "value")) (get (property-json layer "x") "keyframes")))
          "their keyframes, from where the layer rests")))

(t/deftest bound-presets-and-keyframes-name-their-tokens
  (let [[objects code]
        (board-scene #(-> %
                          (cta/add-animation (thi/id :a) {:type :fade :start 0 :duration 300
                                                          :applied-tokens {:easing "ease.spring"}})
                          (cta/add-keyframe (thi/id :b) {:time 0 :property :x :value 0
                                                         :easing :ease-in
                                                         :applied-tokens {:easing "ease.in"}})
                          (cta/add-keyframe (thi/id :b) {:time 400 :property :x :value 80})))
        json      (ctac/motion-json code objects)
        preset    (first (get (layer-json json :a) "presets"))
        keyframes (get (property-json (layer-json json :b) "x") "keyframes")]
    (t/is (= {"easing" "ease.spring"} (get preset "tokens")))
    (t/is (= {"easing" "ease.in"} (get-in keyframes [0 "tokens"])))
    (t/is (not (contains? (second keyframes) "tokens")) "the last keyframe starts no segment")))

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

(defn- split-text-page
  "A page whose board plays the words of the text :title one after the
  other: moving up, or by `animation` (or a vector of them)."
  ([]
   (split-text-page {:type :move :split :word :spread 1 :start 0 :duration 600
                     :offset-x 0 :offset-y 20}))
  ([animation]
   (split-text-page animation {}))
  ([animation board-attrs]
   (let [file  (-> (thf/sample-file :file1)
                   (tho/add-frame :board (merge {:x 0 :y 0 :width 400 :height 300} board-attrs))
                   (tho/add-text :title "Hi you there" :text-params {:parent-label :board}))
         board (thi/id :board)
         tl    (reduce #(cta/add-animation %1 (thi/id :title) %2)
                       (cta/make-timeline {:board-id board :duration 1000})
                       (if (map? animation) [animation] animation))
         file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                           (pcb/with-page (thf/current-page file))
                                           (pcb/set-timeline board tl)))]
     (thf/current-page file))))

(t/deftest an-orthographic-board-plays-no-camera-in-react
  (let [react (fn [attrs]
                (let [file  (-> (thf/sample-file :file1)
                                (tho/add-frame :board (merge {:x 0 :y 0 :width 400 :height 300} attrs)))
                      board (thi/id :board)
                      tl    (-> (cta/make-timeline {:board-id board :duration 1000})
                                (cta/add-keyframe board {:time 0 :property :perspective :value 400})
                                (cta/add-keyframe board {:time 1000 :property :perspective :value 1200}))
                      file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                                        (pcb/with-page (thf/current-page file))
                                                        (pcb/set-timeline board tl)))
                      page  (thf/current-page file)]
                  (str (ctac/motion-react (cltl/code-timelines page {} board) (:objects page)))))]
    (t/is (cstr/includes? (react {}) "perspective: "))
    (t/is (not (cstr/includes? (react {:orthographic true}) "perspective: "))
          "seen without perspective, none plays")))

(t/deftest the-units-that-turn-in-3d-are-seen-from-the-board
  ;; Penpot Motion: each unit with the perspective of its board in front of
  ;; its middle, as render-wasm draws it
  (let [flip    {:type :flip-x :split :word :spread 1 :start 0 :duration 600}
        options {:units {:kinds #{:word}
                         :selector (fn [_ k] (str ".title .penpot-unit-" k))
                         :origin (constantly "50% 50%")
                         :display "inline-block"}}
        code    (fn [page] (cltl/code-timelines page {} (thi/id :board) :units? #(= :text (:type %))))
        css     (fn [page] (acss/timeline->css (:timeline (code page)) (:objects page) options))]
    (t/testing "CSS"
      (let [out (css (split-text-page flip))]
        (t/is (cstr/includes? out "transform: perspective(1000px) translate(0px, 0px) rotate(0deg) rotateX(90deg) rotateY(0deg)"))
        (t/is (cstr/includes? out "rotateX(0deg)")))
      (t/is (cstr/includes? (css (split-text-page flip {:perspective 600})) "perspective(600px) translate("))
      (let [out (css (split-text-page flip {:orthographic true}))]
        (t/is (cstr/includes? out "rotateX(90deg)"))
        (t/is (not (cstr/includes? out "perspective(")) "an orthographic board: none")))
    (t/testing "React"
      (let [page  (split-text-page flip)
            react (ctac/motion-react (code page) (:objects page))]
        (t/is (= 3 (count (re-seq #"transformTemplate=\{\(\{ x = 0, y = 0, rotate = 0, rotateX = 0" react))))
        (t/is (cstr/includes? react "`perspective(1000px) translate(${withUnit(x, \"px\")}"))
        (t/is (cstr/includes? react "const withUnit") "the helper of the template")))
    (t/testing "React, a unit that scales and turns: the template reads the scale"
      (let [page  (split-text-page [flip {:type :scale :split :word :spread 1 :start 0 :duration 600
                                          :amount 0.5}])
            react (ctac/motion-react (code page) (:objects page))]
        (t/is (cstr/includes? react "scale: ") "scaleX and scaleY play alike, as one scale")
        (t/is (cstr/includes? react "scale = 1, scaleX = scale, scaleY = scale"))))))

(t/deftest the-code-can-play-the-units-of-a-text
  (let [page  (split-text-page)
        board (thi/id :board)
        code  (cltl/code-timelines page {} board :units? #(= :text (:type %)))]
    (t/testing "the JSON lists the keyframes of each unit"
      (let [layer (layer-json (ctac/motion-json code (:objects page)) :title)
            units (get layer "units")]
        (t/is (= ["word" 3] [(get units "kind") (get units "count")]))
        (t/is (= [0 1 2] (mapv #(get % "index") (get units "keyframes"))))
        (t/is (= [[0 20] [200 0]]
                 (->> (get-in units ["keyframes" 0 "properties"])
                      (some #(when (= "y" (get % "property")) %))
                      (#(get % "keyframes"))
                      (mapv (juxt (fn [k] (get k "time")) (fn [k] (get k "value"))))))
              "the first word, 20 px down, rises in a third of the span")
        (t/is (nil? (property-json layer "y")) "the text itself stays")))
    (t/testing "the React plays a span by unit, with its text"
      (let [react (ctac/motion-react code (:objects page))]
        (t/is (= 3 (count (re-seq #"<motion.span" react))))
        (t/is (cstr/includes? react "className=\"penpot-unit penpot-unit-2\""))
        (t/is (cstr/includes? react ">{\"there\"}</motion.span>"))
        (t/is (cstr/includes? react "{\" \"}") "the space between them as it is")))
    (t/testing "the CSS, a rule by unit"
      (let [css (acss/timeline->css (:timeline code) (:objects page)
                                    {:units {:kinds #{:word}
                                             :selector (fn [_ k] (str ".title .penpot-unit-" k))
                                             :origin (constantly "50% 50%")
                                             :display "inline-block"}})]
        (t/is (= 3 (count (re-seq #"@keyframes penpot-unit-" css))))
        (t/is (cstr/includes? css ".title .penpot-unit-1 {\n  display: inline-block;"))))
    (t/testing "the texts the code cannot play so play whole"
      (let [whole (cltl/code-timelines page {} board)]
        (t/is (nil? (get (layer-json (ctac/motion-json whole (:objects page)) :title) "units")))))))

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
       "          opacity: { duration: 1.2, times: [0, 0.5, 1], ease: [(p) => (p < 1 ? 0 : 1), \"linear\"], repeat: Infinity },\n"
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

(t/deftest react-tilts-in-the-axes-of-the-layer
  (let [react (react-of #(-> %
                             (cta/add-keyframe (thi/id :a) {:time 0 :property :rotate-y :value 0})
                             (cta/add-keyframe (thi/id :a) {:time 400 :property :rotate-y :value 180})))]
    (t/is (cstr/includes? react "animate={{ rotateY: [0, 180, 180] }}"))
    (t/is (cstr/includes? react (str "rotate(${withUnit(rotate, \"deg\")}) "
                                     "rotateX(${withUnit(rotateX, \"deg\")}) rotateY(${withUnit(rotateY, \"deg\")}) "
                                     "scale(${scaleX}, ${scaleY})"))
          "the tilt between its turn and its scale")
    (t/is (cstr/includes? react "const withUnit"))))

(t/deftest react-plays-the-light-of-the-board
  ;; lit from the right, 30° up, a fifth all round: :a turns to the light
  (let [[objects code] (board-scene #(-> %
                                         (cta/add-keyframe (thi/id :a) {:time 0 :property :rotate-y :value 0 :easing :linear})
                                         (cta/add-keyframe (thi/id :a) {:time 1200 :property :rotate-y :value 60})))
        objects (update objects (thi/id :board) assoc :light {:angle 90 :height 30 :ambient 0.2})
        react   (ctac/motion-react code objects :class-name #(:name %))
        line    (some #(when (cstr/includes? % "filter: [") %) (cstr/split-lines react))]
    (t/is (some? line) "a filter that changes as it turns")
    (t/is (not (cstr/includes? line "opacity:")) "the depth cues leave its opacity as it is")
    (t/is (cstr/includes? react "\"blur(0px) brightness(1)\"") "looking at the viewer")
    (t/is (cstr/includes? react "\"blur(0px) brightness(1.667)\"") "turned to the light")
    (t/is (not (cstr/includes? (ctac/motion-react code (update objects (thi/id :board) dissoc :light)
                                                  :class-name #(:name %))
                               "brightness"))
          "no light, none")))

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
    (t/is (cstr/includes? native (str "x: { type: \"spring\", stiffness: " (acss/format-number stiffness)
                                      ", damping: " (acss/format-number damping)
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

(t/deftest react-components-have-names-of-their-own
  (let [[objects code] (board-scene #(-> %
                                         (cta/add-keyframe (thi/id :a) {:time 0 :property :x :value 20})
                                         (cta/add-keyframe (thi/id :a) {:time 400 :property :x :value 60})))
        named (fn [board-name] (assoc-in objects [(thi/id :board) :name] board-name))
        names (fn [react] (map second (re-seq #"export function (\w+)\(" react)))]
    (t/is (= ["CardMotion"] (names (ctac/motion-react code objects))))
    (t/is (= ["Board01PenpotMotionMotion"] (names (ctac/motion-react code (named "01 · Penpot Motion"))))
          "an identifier starts with a letter")
    (t/is (= ["BoardMotion"] (names (ctac/motion-react code (named "· 🎬 ·")))))
    (t/is (= ["CardMotion" "CardMotion2"] (names (ctac/motion-react [code code] objects)))
          "two boards of one name")))

(t/deftest nothing-animates-no-react
  (let [[objects code] (board-scene identity)]
    (t/is (nil? (ctac/motion-react code objects)))))

(t/deftest a-text-plays-its-colour-in-the-code
  ;; Its glyphs paint in the fills of its spans: `color` in CSS and React,
  ;; not a background, and the stroke of its glyphs, not a border.
  (let [file  (-> (thf/sample-file :file1)
                  (tho/add-frame :board :x 0 :y 0 :width 400 :height 300)
                  (tho/add-text :title "Hi" :text-params {:parent-label :board}))
        board (thi/id :board)
        tid   (thi/id :title)
        tl    (-> (cta/make-timeline {:board-id board :duration 1000})
                  (cta/add-keyframe tid {:time 0 :property :fill-color :index 0 :value "#ff0000"})
                  (cta/add-keyframe tid {:time 1000 :property :fill-color :index 0 :value "#00ff00"}))
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page (thf/current-page file))
                                          (pcb/set-timeline board tl)))
        page  (thf/current-page file)
        code  (cltl/code-timelines page {} board)
        react (ctac/motion-react code (:objects page))
        css   (acss/timeline->css tl (:objects page))]
    (t/is (cstr/includes? react "color: [\"#ff0000\"") "its colour in React")
    (t/is (not (cstr/includes? react "backgroundColor")))
    (t/is (re-find #"color: #00ff00;" css) "and in CSS")
    (t/is (not (cstr/includes? css "background-color")))
    (t/is (= {:text-stroke-width 4}
             (select-keys (acss/frame-values (assoc (get (:objects page) tid) :strokes [{:stroke-color "#000000" :stroke-width 2}])
                                             {:stroke-width {0 4}} {:x 0 :y 0})
                          [:text-stroke-width :border-width]))
          "the stroke of its glyphs")))


(t/deftest gradients-play-in-css-react-and-json
  (let [g      (fn [a b & {:as attrs}]
                 (merge {:type :linear :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5 :width 1
                         :stops [{:color a :opacity 1 :offset 0} {:color b :opacity 1 :offset 1}]}
                        attrs))
        from   (g "#ff0000" "#0000ff")
        to     (g "#00ff00" "#ff0000" :start-x 0.5 :start-y 0 :end-x 0.5 :end-y 1)
        [objects code] (board-scene #(-> %
                                         (cta/add-keyframe (thi/id :a) {:time 0 :property :fill-gradient :index 0 :value from})
                                         (cta/add-keyframe (thi/id :a) {:time 600 :property :fill-gradient :index 0 :value to})))
        objects (assoc-in objects [(thi/id :a) :fills] [{:fill-color-gradient from :fill-opacity 1}])]
    (t/testing "React: motion.dev blends the background image"
      (let [react (ctac/motion-react code objects :class-name #(:name %))]
        (t/is (cstr/includes? react "initial={{ backgroundImage: \"linear-gradient(90deg, #ff0000 0%, #0000ff 100%)\" }}"))
        (t/is (cstr/includes? react "\"linear-gradient(180deg, #00ff00 0%, #ff0000 100%)\""))))
    (t/testing "CSS: registered custom properties, which CSS plays"
      (let [css (acss/timeline->css (:timeline code) objects)]
        (t/is (cstr/includes? css "@property --penpot-gradient-color-1 {"))
        (t/is (cstr/includes? css "syntax: \"<color>\";"))
        (t/is (cstr/includes? css "--penpot-gradient-angle: 90deg;"))
        (t/is (cstr/includes? css "--penpot-gradient-angle: 180deg;"))
        (t/is (cstr/includes? css (str "background-image: linear-gradient(var(--penpot-gradient-angle), "
                                       "var(--penpot-gradient-color-0) var(--penpot-gradient-at-0), "
                                       "var(--penpot-gradient-color-1) var(--penpot-gradient-at-1));")))))
    (t/testing "JSON: the gradient as the plugin API gives it"
      (let [doc   (ctac/motion-json code objects)
            value (get-in (property-json (layer-json doc :a) "fillGradient") ["keyframes" 0 "value"])]
        (t/is (= "linear" (get value "type")))
        (t/is (= {"color" "#ff0000" "opacity" 1 "offset" 0} (get-in value ["stops" 0])))))))

(t/deftest the-type-style-plays-on-the-element-of-a-text
  (let [text  {:type :text :selrect {:x 0 :y 0 :width 100 :height 20}}
        frame (acss/frame-values text {:font-size 30 :letter-spacing 2 :line-height 1.5 :font-weight 650.4}
                                 {:x 0 :y 0})]
    (t/is (= [30 2 1.5 650] ((juxt :font-size :letter-spacing :line-height :font-weight) frame)))
    (t/is (empty? (select-keys (acss/frame-values (assoc text :type :rect) {:font-size 30} {:x 0 :y 0})
                               [:font-size])))
    (t/is (= "fontSize" (get ctac/property-names :font-size)))))

(t/deftest the-sides-of-a-stroke-play-as-border-widths
  (let [rect  {:type :rect :selrect {:x 0 :y 0 :width 100 :height 60}
               :strokes [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 2
                          :stroke-width-top 2 :stroke-width-right 2 :stroke-width-bottom 2 :stroke-width-left 2}]}
        frame (acss/frame-values rect {:stroke-width-left {0 7}} {:x 0 :y 0})]
    (t/is (= [2 2 2 7] (:border-width frame)))
    (t/is (= "strokeWidthLeft" (get ctac/property-names :stroke-width-left)))))

(t/deftest the-spacing-of-a-layout-plays-in-css
  (let [board {:type :frame :layout :flex :selrect {:x 0 :y 0 :width 300 :height 200}
               :layout-gap {:row-gap 4 :column-gap 6} :layout-padding {:p1 8 :p2 8 :p3 8 :p4 8}}
        frame (acss/frame-values board {:layout-row-gap 20 :layout-padding-left 12} {:x 0 :y 0})]
    (t/is (= [20 6] (:gap frame)))
    (t/is (= [8 8 8 12] (:padding frame)))
    (t/is (= "rowGap" (get ctac/property-names :layout-row-gap)))
    (t/is (= "minWidth" (get ctac/property-names :layout-min-width)))))

(t/deftest the-depth-cues-have-names-of-the-plugin-api
  (t/is (= ["depthFocus" "depthFade" "depthBlur"]
           (mapv #(get ctac/property-names %) cta/depth-cue-properties))))

;; FILL OPACITY

(def ^:private red-to-blue
  {:type :linear :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5 :width 1
   :stops [{:color "#ff0000" :opacity 1 :offset 0} {:color "#0000ff" :opacity 1 :offset 1}]})

(defn- painted
  "Board :board holding the layer :a, `shape` (a rect or a text), its
  `fill-opacity` going from 1 to 0.4 in 1200 ms: `[objects code]`."
  [shape]
  (let [file  (-> (thf/sample-file :file1)
                  (tho/add-frame :board :x 0 :y 0 :width 400 :height 300 :name "Card")
                  (ths/add-sample-shape :a (merge {:parent-label :board :x 20 :y 20 :width 40 :height 40
                                                   :name "Layer"}
                                                  shape)))
        board (thi/id :board)
        tl    (-> (cta/make-timeline {:board-id board :duration 1200})
                  (cta/add-keyframe (thi/id :a) {:time 0 :property :fill-opacity :index 0 :value 1})
                  (cta/add-keyframe (thi/id :a) {:time 1200 :property :fill-opacity :index 0 :value 0.4}))
        file  (thf/apply-changes file (-> (pcb/empty-changes nil)
                                          (pcb/with-page (thf/current-page file))
                                          (pcb/set-timeline board tl)))
        page  (thf/current-page file)]
    [(:objects page) (cltl/code-timelines page {(:id file) file} board)]))

(defn- painted-react
  [shape]
  (let [[objects code] (painted shape)]
    (ctac/motion-react code objects :class-name #(:name %))))

(t/deftest the-opacity-of-a-flat-fill-plays-its-colour-and-no-image
  (let [react (painted-react {:type :rect :fills [{:fill-color "#ff0000" :fill-opacity 1}]})]
    (t/is (cstr/includes? react "backgroundColor"))
    (t/is (not (cstr/includes? react "backgroundImage"))
          "no background image, which would clear one the layer has")))

(t/deftest the-opacity-of-a-gradient-fill-plays-its-image-and-no-colour
  (let [react (painted-react {:type :rect :fills [{:fill-color-gradient red-to-blue :fill-opacity 1}]})]
    (t/is (cstr/includes? react "backgroundImage"))
    (t/is (cstr/includes? react "rgba(255, 0, 0, 0.4)") "its stops fade")
    (t/is (not (cstr/includes? react "backgroundColor")))))

(t/deftest the-opacity-of-a-gradient-text-plays-no-empty-colour
  (let [text  (-> (cts/setup-shape {:type :text :x 20 :y 20 :width 80 :height 20})
                  (update :content txt/change-text "Hi")
                  (update :content (fn [content]
                                     (txt/transform-nodes txt/is-text-node?
                                                          #(assoc % :fills [{:fill-color-gradient red-to-blue
                                                                             :fill-opacity 1}])
                                                          content))))
        ;; nil when it plays nothing React shows
        react (str (painted-react (dissoc text :id)))]
    (t/is (not (cstr/includes? react "color: \"\"")))
    (t/is (not (cstr/includes? react "color: [\"\"")))))

(t/deftest the-css-of-a-gradient-fill-rests-at-its-opacity
  (let [[objects code] (painted {:type :rect :fills [{:fill-color-gradient red-to-blue :fill-opacity 0.5}]})
        css (acss/timeline->css (:timeline code) objects)]
    (t/is (cstr/includes? css "--penpot-gradient-color-0: rgba(255, 0, 0, 0.5);")
          "the resting stops are faded by the opacity of the fill")))

;; TRIMS AND VALUES THAT PLAY NOTHING

(defn- trimmed
  "`[objects code]` of :a trimming its stroke, its end 0 to 1 in 800 ms."
  []
  (board-scene #(-> %
                    (cta/add-keyframe (thi/id :a) {:time 0 :property :trim-end :value 0 :easing :linear})
                    (cta/add-keyframe (thi/id :a) {:time 800 :property :trim-end :value 1}))))

(t/deftest react-leaves-out-a-value-that-moves-nothing
  (let [react (react-of #(-> %
                             (cta/add-keyframe (thi/id :a) {:time 0 :property :x :value 20})
                             (cta/add-keyframe (thi/id :a) {:time 400 :property :x :value 20})
                             (cta/add-keyframe (thi/id :a) {:time 0 :property :y :value 20})
                             (cta/add-keyframe (thi/id :a) {:time 400 :property :y :value 60})))]
    (t/is (cstr/includes? react "animate={{ y: [0, 40, 40] }}"))
    (t/is (not (cstr/includes? react "x: ")) "x stays where it rests all along")))

(t/deftest react-trims-the-element-that-paints-the-stroke
  (let [[objects code] (trimmed)
        layers         (atom nil)
        react          (ctac/motion-react code objects :class-name #(:name %)
                                          :markup (fn [_ els] (reset! layers els) "      <div />"))
        layer          (get @layers (thi/id :a))]
    (t/is (cstr/includes? react "<div />") "the markup holds the board")
    (t/is (nil? (:attrs layer)) "nothing plays on the layer's own box")
    (t/is (cstr/includes? ((:trim layer) 0 "  ") "strokeDasharray: [\"0 1\", \"1 0\", \"1 0\"]"))
    (t/is (cstr/includes? ((:trim layer) 0.25 "  ") "strokeDashoffset: [0.25, 0.25, 0.25]")
          "a piece of the outline that starts a quarter along")
    (t/is (nil? (ctac/motion-react code objects)) "without the markup, no element to trim")))

(t/deftest css-plays-a-trim-on-the-elements-of-the-stroke
  (let [[objects code] (trimmed)
        dash  (fn [values] [(str "dash: " (get values :trim-end) ";")])
        css   (acss/timeline->css (:timeline code) objects
                                  {:trims (fn [shape _] [[(str "." (:name shape) " path") dash]])})]
    (t/is (cstr/includes? css "@keyframes penpot-trim-"))
    (t/is (cstr/includes? css "0% { dash: 0; animation-timing-function: linear; }"))
    (t/is (cstr/includes? css "Rect1 path {\n  animation: penpot-trim-"))
    (t/is (not (cstr/includes? css "@keyframes penpot-anim-"))
          "a layer that only trims plays nothing on its box")
    (t/is (cstr/includes? (acss/timeline->css (:timeline code) objects) "@keyframes penpot-anim-")
          "without trims, as before")))
