;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.ui.inspect-motion-test
  "The animation of the selection in the Inspect (Penpot Motion): its
  code, from the code model of its boards."
  (:require
   [app.common.animation.css :as acss]
   [app.common.files.changes-builder :as pcb]
   [app.common.geom.matrix :as gmt]
   [app.common.test-helpers.components :as cthc]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cthsh]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.path :as path]
   [app.common.uuid :as uuid]
   [app.main.ui.inspect.motion-timeline :as mtl]
   [app.util.code-gen :as cg]
   [app.util.code-gen.common :as cgc]
   [app.util.code-gen.markup-jsx :as jsx]
   [app.util.code-gen.motion :as cgm]
   [app.util.motion-labels :as labels]
   [cljs.test :as t :include-macros true]
   [cuerdas.core :as str]))

(t/use-fixtures :each {:before cthi/reset-idmap!})

(defn- scene
  "An animated component (its main board's rect moves from 10 to 60 in a
  second, over and over) and board B holding a copy of it and a rect
  :own; `(own timeline copy-rect)` gives B's timeline from an empty one
  (nil: B has none)."
  [own]
  (let [file      (-> (cthf/sample-file :file1)
                      (ctho/add-simple-component :anim :main :main-rect
                                                 :root-params {:x 0 :y 0 :width 200 :height 200}
                                                 :child-params {:x 10 :y 20 :width 50 :height 50})
                      (ctho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                      (cthc/instantiate-component :anim :copy :parent-label :board-b)
                      (ctho/add-rect :own :parent-label :board-b :x 1300 :y 150 :width 40 :height 40))
        main      (cthi/id :main)
        board     (cthi/id :board-b)
        copy-rect (first (get-in (cthf/current-page file) [:objects (cthi/id :copy) :shapes]))
        mtl       (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                      (cta/add-keyframe (cthi/id :main-rect) {:time 0 :property :x :value 10})
                      (cta/add-keyframe (cthi/id :main-rect) {:time 1000 :property :x :value 60}))
        file      (cthf/apply-changes
                   file
                   (cond-> (-> (pcb/empty-changes nil)
                               (pcb/with-page (cthf/current-page file))
                               (pcb/set-timeline main mtl))
                     (some? own)
                     (pcb/set-timeline board (own (cta/make-timeline {:board-id board :duration 2000})
                                                  copy-rect))))]
    {:page (cthf/current-page file)
     :files {(:id file) file}
     :board board
     :copy-rect copy-rect}))

(defn- anim-count
  "How many `@keyframes` the `css` has for the shape `id`."
  [css id]
  (count (re-seq (re-pattern (str "@keyframes penpot-anim-" id)) css)))

(t/deftest the-css-of-a-board-plays-its-animated-copies
  (let [{:keys [page files board copy-rect]} (scene nil)
        ids   #{board (cthi/id :copy) copy-rect (cthi/id :own)}
        css   (cgm/animation-css page files ids)
        class (cgc/shape->selector (get-in page [:objects copy-rect]))]
    (t/is (= 1 (anim-count css copy-rect)))
    (t/is (str/includes? css (str "." class " {\n  animation: penpot-anim-" copy-rect " 1000ms linear infinite;"))
          "on the clock of its main")))

(t/deftest the-css-of-a-board-without-copies-is-its-own
  (let [{:keys [page files]} (scene nil)
        main    (cthi/id :main)
        objects (:objects page)
        ids     (into #{main} (get-in objects [main :shapes]))]
    (t/is (= (acss/timeline->css (get-in page [:timelines main]) objects
                                 {:ids ids :selector cgm/code-selector})
             (cgm/animation-css page files ids))
          "a board with no rest transform: today's output")))

(t/deftest a-copy-layer-its-board-animates-plays-once-in-the-css
  (let [{:keys [page files board copy-rect]}
        (scene (fn [timeline copy-rect]
                 (-> timeline
                     (cta/add-keyframe copy-rect {:time 0 :property :opacity :value 0.5})
                     (cta/add-keyframe copy-rect {:time 500 :property :opacity :value 1}))))
        css (cgm/animation-css page files #{board (cthi/id :copy) copy-rect})]
    (t/is (= 1 (anim-count css copy-rect)))
    (t/is (str/includes? css "2000ms linear 1 both") "the board's own animation of it")))

(t/deftest one-selected-layer-gives-its-rules-only
  (let [{:keys [page files copy-rect]}
        (scene (fn [timeline _]
                 (-> timeline
                     (cta/add-keyframe (cthi/id :own) {:time 0 :property :x :value 300})
                     (cta/add-keyframe (cthi/id :own) {:time 500 :property :x :value 340}))))
        css (cgm/animation-css page files #{(cthi/id :own)})]
    (t/is (= 1 (anim-count css (cthi/id :own))))
    (t/is (zero? (anim-count css copy-rect)))
    (t/is (nil? (cgm/animation-css page files #{})) "nothing selected, nothing")))

(t/deftest the-code-of-the-inspect-leaves-the-helper-layers-out
  (let [{:keys [page files board]}
        (scene (fn [timeline _]
                 (-> timeline
                     (cta/add-keyframe (cthi/id :own) {:time 0 :property :x :value 300})
                     (cta/set-guide (cthi/id :own) true))))
        objects (:objects page)
        own     (cthi/id :own)
        class   (cgc/shape->selector (get objects own))
        html    (fn [objects shapes] (cg/generate-markup-code objects "html" shapes))
        css     (fn [objects shapes] (cg/generate-style-code objects "css" shapes (vals objects)))
        [objects' shapes'] (cgm/without-guides page files objects [(get objects board)])]
    (t/is (= #{own} (cgm/guide-ids page files [board])))
    (t/is (= #{own} (cgm/guide-ids page files [own])) "selected itself")
    (t/is (str/includes? (html objects [(get objects board)]) class) "a helper layer, before")
    (t/is (not (contains? objects' own)))
    (t/is (not (str/includes? (html objects' shapes') class)) "the HTML of its board without it")
    (t/is (not (str/includes? (css objects' shapes') class)) "the CSS too")
    (t/is (empty? (second (cgm/without-guides page files objects [(get objects own)])))
          "nothing of it, selected itself")
    (t/is (identical? objects (first (cgm/without-guides page files objects [(get objects (cthi/id :main))])))
          "a board with no helper layers, the same objects")))

(t/deftest a-turned-layer-keeps-its-turn-in-the-css
  (let [turn   (fn [page id]
                 (update-in page [:objects id] assoc
                            :transform (gmt/rotate-matrix 30)
                            :transform-inverse (gmt/inverse (gmt/rotate-matrix 30))))
        fade   (fn [timeline _]
                 (-> timeline
                     (cta/add-keyframe (cthi/id :own) {:time 0 :property :opacity :value 0})
                     (cta/add-keyframe (cthi/id :own) {:time 500 :property :opacity :value 1})))
        move   (fn [timeline copy-rect]
                 (-> (fade timeline copy-rect)
                     (cta/add-keyframe (cthi/id :own) {:time 0 :property :x :value 300})
                     (cta/add-keyframe (cthi/id :own) {:time 500 :property :x :value 340})))
        css-of (fn [own-fn]
                 (let [{:keys [page files]} (scene own-fn)
                       own (cthi/id :own)]
                   (cgm/animation-css (turn page own) files #{own})))]
    (t/is (not (str/includes? (css-of fade) "transform:"))
          "a fade leaves its turn to the code of the layer")
    (t/is (str/includes? (css-of move) "rotate(0deg) matrix(")
          "a move keeps its turn")))

(t/deftest the-json-of-a-board-plays-its-copies
  (let [{:keys [page files board copy-rect]} (scene nil)
        ids  #{board (cthi/id :copy) copy-rect}
        data (cgm/animation-json page files ids)
        copy (some #(when (= (str copy-rect) (get % "id")) %) (get data "layers"))]
    (t/is (map? data) "one board, one document")
    (t/is (= [1 1000 "loop"] [(get data "version") (get-in copy ["component" "duration"])
                              (get-in copy ["component" "playback"])]))
    (t/is (= data (js->clj (js/JSON.parse (cgm/json-text data))))
          "the text reads back as the data")
    (t/is (nil? (cgm/animation-json page files #{(cthi/id :own)})) "nothing animates there")))

(t/deftest the-react-of-the-inspect-has-the-classes-of-its-html
  (let [{:keys [page files board copy-rect]} (scene nil)
        react (cgm/animation-react page files #{board (cthi/id :copy) copy-rect})
        class (cgc/shape->selector (get-in page [:objects copy-rect]))]
    (t/is (str/includes? react (str "className=\"shape rect " class "\"")))
    (t/is (str/includes? react "repeat: Infinity") "on the clock of its main")
    (t/is (nil? (cgm/animation-react page files #{(cthi/id :own)})))))

(t/deftest the-timeline-rows-of-a-board
  (let [{:keys [page files board copy-rect]}
        (scene (fn [timeline _]
                 (-> timeline
                     (cta/add-keyframe (cthi/id :own) {:time 0 :property :x :value 300})
                     (cta/add-keyframe (cthi/id :own) {:time 500 :property :x :value 340})
                     (cta/add-animation (cthi/id :own) {:type :fade :start 1000 :duration 500})
                     (ctau/add-audio-clip {:id (uuid/next) :audio-id (uuid/next) :name "beat.mp3"
                                           :source-duration 4000 :start 0 :duration 1000}))))
        objects (:objects page)
        ids     #{board (cthi/id :copy) copy-rect (cthi/id :own)}
        codes   (cgm/code-of-boards page files ids)
        [{:keys [axis end rows playback]}] (mtl/motion-rows codes objects ids)
        own     (some #(when (= (cthi/id :own) (:id %)) %) rows)
        copy    (some #(when (= copy-rect (:id %)) %) rows)
        clip    (some #(when (= :audio (:kind %)) %) rows)]
    (t/is (= [2000 100 :once] [axis end playback]))
    (t/is (= [[0 0] [500 25]] (mapv (juxt :time :at) (:diamonds own))) "diamonds in % of the axis")
    (t/is (= [(labels/animation-label {:type :fade :direction :in}) 50 25]
             ((juxt :label :at :width) (first (:blocks own)))))
    (t/is (= {:duration 1000 :playback :loop} (select-keys (:copy copy) [:duration :playback]))
          "a copy row has the clock of its main")
    (t/is (= ["beat.mp3" 0 50] ((juxt :name :at :width) clip)) "the board selected, its clips")
    (t/is (not-any? #(= :audio (:kind %)) (:rows (first (mtl/motion-rows codes objects #{(cthi/id :own)}))))
          "a layer selected, no clips")
    (t/is (= [] (mtl/motion-rows codes objects #{})) "nothing selected, nothing")))

(t/deftest the-code-plays-the-words-of-a-text-one-by-one
  (let [file    (-> (cthf/sample-file :file1)
                    (ctho/add-frame :board :x 0 :y 0 :width 400 :height 300)
                    (ctho/add-text :title "Hi you there" :text-params {:parent-label :board}))
        board   (cthi/id :board)
        title   (cthi/id :title)
        tl      (cta/add-animation (cta/make-timeline {:board-id board :duration 1000})
                                   title {:type :fade :split :word :spread 1 :start 0 :duration 600})
        file    (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                             (pcb/with-page (cthf/current-page file))
                                             (pcb/set-timeline board tl)))
        page    (cthf/current-page file)
        objects (:objects page)
        codes   (cgm/code-of-boards page {} #{title})
        units   (cgm/code-units codes objects)
        text    (cgm/with-code-units (get objects title) units)
        html    (cg/generate-markup-code (update-vals objects #(cgm/with-code-units % units)) "html" [text])
        css     (cgm/codes-css codes objects #{title})]
    (t/is (= 3 (count (get units title))) "three words")
    (t/is (str/includes? html "<span class=\"penpot-unit penpot-unit-0\">Hi</span> <span class=\"penpot-unit penpot-unit-1\">you</span>")
          "its HTML wraps them")
    (t/is (str/includes? css (str (cgm/code-selector text) " .penpot-unit-2 {")) "its CSS plays each")
    (t/is (str/includes? css "display: inline-block;"))))

(defn- text-scene
  "Board :board holding a text :title of `text` its animation `anim` plays
  (a preset of `cta/add-animation`): `{:page :ids}`."
  [text anim]
  (let [file  (-> (cthf/sample-file :file1)
                  (ctho/add-frame :board :x 0 :y 0 :width 400 :height 300)
                  (ctho/add-text :title text :text-params {:parent-label :board}))
        board (cthi/id :board)
        tl    (cta/add-animation (cta/make-timeline {:board-id board :duration 1000}) (cthi/id :title) anim)
        file  (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                           (pcb/with-page (cthf/current-page file))
                                           (pcb/set-timeline board tl)))]
    {:page (cthf/current-page file)
     :files {(:id file) file}
     :ids #{board (cthi/id :title)}}))

(t/deftest the-react-of-a-text-keeps-its-spaces
  (let [{:keys [page files ids]} (text-scene "Hi you there" {:type :fade :split :word :spread 1 :start 0 :duration 600})
        react (cgm/animation-react page files ids)]
    (t/is (re-find #"\{\"Hi\"\}\s*</motion.span>\s*\{\" \"\}\s*<motion.span" react)
          "the space between two words it plays one by one"))
  (let [{:keys [page files ids]} (text-scene "Hi\n\nthere" {:type :fade :start 0 :duration 600})
        react (cgm/animation-react page files ids)]
    (t/is (str/includes? react "{\"\u00a0\"}") "the no-break space of an empty line")))

;; THE MARKUP OF THE REACT CODE AND TRIMS

(defn- trim-scene
  "Board :card holding a rect :bg at rest, a rect :box fading in and a
  path :ring whose stroke draws itself, its trim end 0 to 1 in 800 ms:
  `{:page :files :board :ids}`."
  []
  (let [ring  (path/from-plain [{:command :move-to :params {:x 20 :y 150}}
                                {:command :line-to :params {:x 220 :y 150}}])
        file  (-> (cthf/sample-file :file1)
                  (ctho/add-frame :card :x 0 :y 0 :width 400 :height 300 :name "Card")
                  (ctho/add-rect :bg :parent-label :card :x 10 :y 10 :width 100 :height 100 :name "Bg")
                  (ctho/add-rect :box :parent-label :card :x 200 :y 10 :width 40 :height 40 :name "Box")
                  (cthsh/add-sample-shape :ring :parent-label :card :type :path :name "Ring"
                                          :content ring :fills []
                                          :strokes [{:stroke-color "#22c55e" :stroke-opacity 1 :stroke-width 4
                                                     :stroke-alignment :center :stroke-style :solid}]))
        board (cthi/id :card)
        tl    (-> (cta/make-timeline {:board-id board :duration 1000})
                  (cta/add-keyframe (cthi/id :ring) {:time 0 :property :trim-end :value 0 :easing :linear})
                  (cta/add-keyframe (cthi/id :ring) {:time 800 :property :trim-end :value 1})
                  (cta/add-keyframe (cthi/id :box) {:time 0 :property :opacity :value 0})
                  (cta/add-keyframe (cthi/id :box) {:time 500 :property :opacity :value 1}))
        file  (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                           (pcb/with-page (cthf/current-page file))
                                           (pcb/set-timeline board tl)))
        page  (cthf/current-page file)]
    {:page page
     :files {(:id file) file}
     :board board
     :ids (into #{board} (get-in page [:objects board :shapes]))}))

(t/deftest the-markup-parses-elements-texts-and-comments
  (t/is (= [{:tag "div" :attrs [["class" "a b"]]
             :children [{:tag "img" :attrs [["src" "x.png"]] :children []}
                        {:comment " c "}
                        {:tag "span" :attrs [] :children [{:text "a & b"}]}]}]
           (jsx/parse "<div class=\"a b\"><img src=\"x.png\"></img><!-- c --><span>a &amp; b</span></div>"))
        "an `</img>` closes nothing; the references read as their characters"))

(t/deftest the-react-of-a-board-holds-its-markup
  (let [{:keys [page files ids]} (trim-scene)
        objects (:objects page)
        class   #(cgc/shape->selector (get objects (cthi/id %)))
        react   (cgm/animation-react page files ids)]
    (t/is (str/includes? react (str "<div className=\"frame " (class :card) "\">")) "the board holds its layers")
    (t/is (str/includes? react (str "<div className=\"shape rect " (class :bg) "\" />")) "a layer at rest is there too")
    (t/is (str/includes? react (str "<motion.div\n")) "an animated one is a motion element")
    (t/is (str/includes? react (str "className=\"shape rect " (class :box) "\"")))
    (t/is (str/includes? react "<motion.path\n") "the stroke it trims")
    (t/is (str/includes? react "pathLength=\"1\""))
    (t/is (str/includes? react "strokeDasharray: [\"0 1\""))
    (t/is (str/includes? react "xmlnsXlink=") "the attributes of SVG as JSX names them")
    (t/is (str/includes? react "strokeWidth: \"4\"") "its style an object")
    (t/is (not (str/includes? react " class=")) "no attribute of HTML that JSX names otherwise")
    (t/is (not (str/includes? react "import { steps }")) "a hold needs no import")))

(t/deftest the-css-of-the-inspect-trims-the-stroke-its-html-draws
  (let [{:keys [page files board ids]} (trim-scene)
        objects (:objects page)
        codes   (cgm/code-of-boards page files ids)
        ring    (cthi/id :ring)
        markup  (cg/generate-markup-code (cgm/marked-objects codes objects nil) "html" [(get objects board)])
        css     (cgm/animation-css page files ids)]
    (t/is (= #{ring} (cgm/code-trims codes objects)))
    (t/is (str/includes? markup "pathLength=\"1\"") "its outline of length 1")
    (t/is (str/includes? css (str ".strokes[id$=\"-" ring "\"]")) "on the element that paints the stroke")
    (t/is (str/includes? css "stroke-dasharray: 0 1;"))
    (t/is (not (str/includes? css (str "penpot-anim-" ring))) "nothing plays on its box")))
