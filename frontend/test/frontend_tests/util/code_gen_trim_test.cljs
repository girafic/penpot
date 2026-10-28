;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.util.code-gen-trim-test
  "The trim of strokes in the code of the Inspect (Penpot Motion): the
  HTML marks the elements that paint a trimmed stroke, its CSS and its
  React code play the trim on them."
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cthsh]
   [app.common.types.animation :as cta]
   [app.common.types.path :as path]
   [app.util.code-gen :as cg]
   [app.util.code-gen.common :as cgc]
   [app.util.code-gen.motion :as cgm]
   [cljs.test :as t :include-macros true]
   [cuerdas.core :as str]))

(t/use-fixtures :each {:before cthi/reset-idmap!})

(defn- stroke
  "A solid stroke 4 px wide, aligned `alignment`."
  [alignment & {:as more}]
  (merge {:stroke-color "#22c55e" :stroke-opacity 1 :stroke-width 4
          :stroke-alignment alignment :stroke-style :solid}
         more))

(defn- scene
  "Board :card (with `board-params`) holding the layers `layers`
  (`[label params]`, rects unless their params say otherwise), the ones
  of `trims` drawing their strokes, their trim end going 0 to 1 in 800
  ms: `{:page :files :objects :ids}`."
  [board-params layers trims]
  (let [file  (reduce (fn [file [label params]]
                        (cthsh/add-sample-shape file label (merge {:type :rect :parent-label :card
                                                                   :x 20 :y 20 :width 100 :height 60 :fills []}
                                                                  params)))
                      (-> (cthf/sample-file :file1)
                          (cthsh/add-sample-shape :card (merge {:type :frame :name "Card" :x 0 :y 0
                                                                :width 400 :height 300}
                                                               board-params)))
                      layers)
        board (cthi/id :card)
        tl    (reduce (fn [tl label]
                        (-> tl
                            (cta/add-keyframe (cthi/id label) {:time 0 :property :trim-end :value 0 :easing :linear})
                            (cta/add-keyframe (cthi/id label) {:time 800 :property :trim-end :value 1})))
                      (cta/make-timeline {:board-id board :duration 1000})
                      trims)
        file  (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                           (pcb/with-page (cthf/current-page file))
                                           (pcb/set-timeline board tl)))
        page  (cthf/current-page file)]
    {:page page
     :files {(:id file) file}
     :objects (:objects page)
     :ids (into #{board} (get-in page [:objects board :shapes]))}))

(defn- markup-of
  "The HTML of the code of the Inspect of the layer `label` of `scene`, as
  it writes it for the animation of its board."
  [{:keys [page files ids objects]} label]
  (let [marked (cgm/marked-objects (cgm/code-of-boards page files ids) objects nil)]
    (cg/generate-markup-code marked "html" [(get marked (cthi/id label))])))

(defn- markers
  "The values of the trim markers of `markup`, in order."
  [markup]
  (mapv second (re-seq #"data-penpot-trim=\"([^\"]*)\"" markup)))

(defn- selector
  [{:keys [objects]} label from]
  (str "." (cgc/shape->selector (get objects (cthi/id label))) " [data-penpot-trim=\"" from "\"]"))

(t/deftest an-inner-and-an-outer-stroke-mark-the-element-that-paints-each
  (let [scene (scene {} [[:inner {:name "Inner" :strokes [(stroke :inner) (stroke :center)]}]
                         [:outer {:name "Outer" :strokes [(stroke :outer)]}]]
                     [:inner :outer])
        inner (markup-of scene :inner)
        outer (markup-of scene :outer)
        css   (cgm/animation-css (:page scene) (:files scene) (:ids scene))]
    (t/is (= ["0" "0"] (markers inner)) "one element for each of its two strokes")
    (t/is (= ["0"] (markers outer)))
    (t/is (not (str/includes? (str inner outer) "<use href=")) "no copy left to a `<use>`")
    (t/is (re-find #"<[a-z]+ data-penpot-trim=\"0\"[^>]*\smask=" outer) "of an outer stroke, the one with its mask")
    (t/is (str/includes? css (str (selector scene :inner "0") " {")) "the CSS plays the trim on them")
    (t/is (str/includes? css (str (selector scene :outer "0") " {")))
    (t/is (= 1 (count (re-seq (re-pattern (str "@keyframes penpot-trim-" (cthi/id :inner))) css)))
          "one rule for both strokes, which start alike")))

(t/deftest the-pieces-of-an-outline-start-where-they-begin
  (let [two   (path/from-plain [{:command :move-to :params {:x 20 :y 20}}
                                {:command :line-to :params {:x 120 :y 20}}
                                {:command :move-to :params {:x 20 :y 60}}
                                {:command :line-to :params {:x 120 :y 60}}])
        scene (scene {} [[:lines {:type :path :name "Lines" :content two :strokes [(stroke :center)]}]] [:lines])
        css   (cgm/animation-css (:page scene) (:files scene) (:ids scene))]
    (t/is (= ["0" "0.5"] (markers (markup-of scene :lines))) "each subpath, where it begins on them all")
    (t/is (str/includes? css (selector scene :lines "0.5")))
    (t/is (str/includes? css "stroke-dashoffset: 0.5;") "the second piece from half way along")))

(t/deftest a-hidden-stroke-takes-no-place
  (let [scene (scene {} [[:rect {:name "Rect" :strokes [(stroke :center) (stroke :inner :hidden true)]}]] [:rect])
        css   (cgm/animation-css (:page scene) (:files scene) (:ids scene))]
    (t/is (= ["0"] (markers (markup-of scene :rect))) "the stroke it draws")
    (t/is (str/includes? css (selector scene :rect "0")))))

(t/deftest a-board-marks-its-own-strokes-only
  (let [scene (scene {:strokes [(stroke :center)]} [[:rect {:name "Rect" :strokes [(stroke :center)]}]] [:card])]
    (t/is (= ["0"] (markers (markup-of scene :card)))
          "not the stroke of the layer its SVG draws in it")))
