;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.util.code-gen.motion
  "The code of the animation of shapes (Penpot Motion) as the Motion tab
  of the Inspect and the plugin API give it: from the code model of the
  boards they are in (see `cltl/code-timelines`), which plays the
  animated component copies on their own clock, as the view mode does,
  with the classes the HTML of the Inspect gives the shapes."
  (:require
   [app.common.animation.css :as acss]
   [app.common.animation.export :as aexp]
   [app.common.animation.svg :as asvg]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.logic.timelines :as cltl]
   [app.common.types.animation :as cta]
   [app.common.types.animation-code :as ctac]
   [app.common.types.shape-tree :as ctst]
   [app.util.code-gen.common :as cgc]
   [app.util.code-gen.style-css-values :as sv]
   [cuerdas.core :as str]))

(defn code-selector
  "The CSS selector of `shape` in the code of the Inspect: the class its
  HTML gives it."
  [shape]
  (dm/str "." (cgc/shape->selector shape)))

(defn code-units?
  "Whether the code of the Inspect plays the units of the split
  animations of `shape` one by one: a text it writes as HTML (not as SVG,
  see `cgc/svg-markup?`), whose characters or words it wraps (see
  `code-units`)."
  [shape]
  (and (cfh/text-shape? shape) (not (cgc/svg-markup? shape))))

(defn code-timelines
  "The code model of the board `board-id` of `page` (see
  `cltl/code-timelines`), playing by units the texts the code can play
  so (see `code-units?`)."
  [page files board-id]
  (cltl/code-timelines page files board-id :units? code-units?))

(defn code-of-boards
  "The code model (see `code-timelines`) of each board the shapes of
  `ids` of `page` are in, by the name of the board; `files` holds the
  files of the components, by id."
  [page files ids]
  (let [objects (:objects page)
        boards  (->> (into #{} (keep #(cfh/get-shape-id-root-frame objects %)) ids)
                     (sort-by (fn [id] [(dm/get-in objects [id :name]) (str id)])))]
    (into [] (keep #(code-timelines page files %)) boards)))

(defn guide-ids
  "The helper layers of the boards the shapes of `ids` of `page` are in,
  as goal rings (see `cltl/guide-ids`), which the code of the Inspect
  leaves out as the exports do; `files` holds the files of the
  components, by id."
  [page files ids]
  (let [objects (:objects page)]
    (into #{}
          (comp (keep #(cfh/get-shape-id-root-frame objects %))
                (distinct)
                (mapcat #(cltl/guide-ids page files %)))
          ids)))

(defn without-guides
  "`[objects shapes]`: `objects` and the selected `shapes` of them (with
  the children of each) without the helper layers of the boards those
  are in (see `guide-ids`), which the code of the Inspect leaves out as
  the exports do; `page` has the timelines, `files` the files of the
  components, by id."
  [page files objects shapes]
  (let [guides (guide-ids page files (map :id shapes))]
    (if (empty? guides)
      [objects shapes]
      [(:objects (reduce #(ctst/delete-shape %1 %2 true) {:objects objects} guides))
       (into []
             (comp (remove #(contains? guides (:id %)))
                   (map #(d/update-when % :shapes (partial filterv (complement guides)))))
             shapes)])))

(defn code-units
  "`{text-id bases}`: the texts of `codes` (see `code-of-boards`) the code
  plays by units, with where their units are (see `cta/unit-timelines`),
  which their HTML wraps (`:code-units` of a shape, see
  `app.main.ui.shapes.text.html-text`)."
  [codes objects]
  (into {}
        (comp (mapcat (fn [{:keys [timeline copies]}] (cons timeline (map :timeline copies))))
              (mapcat #(cta/unit-timelines (cta/resolve-animations % objects) objects))
              (map (fn [[id {:keys [bases]}]] [id bases])))
        codes))

(defn with-code-units
  "`shape` with the units its HTML wraps, `:code-units`, when `units` (see
  `code-units`) has them."
  [shape units]
  (if-let [bases (get units (:id shape))]
    (assoc shape :code-units bases)
    shape))

(defn code-trims
  "The layers of the boards of `codes` (see `code-of-boards`) whose
  strokes the code trims: those their timelines, as they play, trim (the
  trim of a group plays on the layers in it, see `cta/resolve-animations`),
  of the ones it writes as SVG (see `cgc/svg-markup?`), but texts."
  [codes objects]
  (into #{}
        (comp (mapcat (fn [{:keys [timeline copies]}] (cons timeline (map :timeline copies))))
              (mapcat #(:tracks (cta/resolve-animations % objects)))
              (keep (fn [[id track]]
                      (let [shape (get objects id)]
                        (when (and (some? shape)
                                   (cgc/svg-markup? shape)
                                   (not (cfh/text-shape? shape))
                                   (some #(contains? cta/trim-properties (:property %)) (:keyframes track)))
                          id)))))
        codes))

(defn with-code-trims
  "`shape` marked `:code-trim` when the code trims its strokes (see
  `code-trims`), so its SVG has them ready for it (see
  `app.main.data.exports.svg-vectors/trimmed-markup`)."
  [shape trims]
  (cond-> shape
    (contains? trims (:id shape)) (assoc :code-trim true)))

(defn marked-objects
  "`objects` as the code of the Inspect writes their HTML for the
  animation of the boards of `codes`: the texts it plays by units with
  their units (see `with-code-units`), the layers whose strokes it trims
  marked (see `with-code-trims`), and without the helper layers `guides`
  (see `guide-ids`)."
  [codes objects guides]
  (let [units   (code-units codes objects)
        trims   (code-trims codes objects)
        objects (if (seq guides)
                  (:objects (reduce #(ctst/delete-shape %1 %2 true) {:objects objects} guides))
                  objects)]
    (d/update-vals objects #(-> % (with-code-units units) (with-code-trims trims)))))

(defn- trim-paint
  "The selectors of the elements the trim of the strokes of `shape` plays
  on in the code of the Inspect (see `acss/timeline->css`):
  the one each of its strokes paints, or each piece of it (see
  `app.main.data.exports.svg-vectors/trimmed-markup`), with the
  declarations of its trim from where its piece starts. None of a shape
  the code draws as a box, whose border no trim dashes, or of a text."
  [shape selector]
  (when (and (cgc/svg-markup? shape) (not= :text (:type shape)))
    (let [pieces (aexp/trim-pieces shape)]
      (for [index (range (count (:strokes shape)))
            :let  [paint (asvg/stroke-paint shape index)]
            [k [from _]] (if (some? pieces) (map-indexed vector pieces) [[nil [0]]])]
        [(str selector " " paint (when (some? k) (str "." (asvg/trim-piece-class k))))
         #(asvg/trim-declarations % from)]))))

(def ^:private unit-options
  "How the CSS of the Inspect plays the units of a text (see
  `acss/timeline->css`): characters and words, the classes its HTML gives
  them, each turning around its middle, as blocks in the line, which CSS
  can transform."
  {:kinds #{:char :word}
   :selector (fn [text k] (dm/str (code-selector text) " .penpot-unit-" k))
   :origin (constantly "50% 50%")
   :display "inline-block"})

(defn codes-css
  "CSS animating the layers of the boards of `codes` (see
  `cltl/code-timelines`), those of `ids` only when given, with the
  selectors of the code of the Inspect and keeping the transforms its CSS
  gives the shapes of `objects` at rest: for each board, its own timeline
  and, each on its own clock, those of the animated component copies in
  it. Nil when nothing of them animates."
  [codes objects ids]
  (let [options {:ids ids
                 :selector code-selector
                 ;; flat: the animation writes the tilt itself (see
                 ;; `acss/frame-values`)
                 :transform #(sv/get-value :transform (dissoc % :rotate-x :rotate-y :translate-z) objects {})
                 :units (assoc unit-options :texts #(code-units? (get objects %)))
                 ;; on the strokes its SVG draws (see `code-trims`)
                 :trims (fn [shape _] (trim-paint shape (code-selector shape)))}]
    (some->> (seq codes)
             (mapcat (fn [{:keys [timeline copies]}] (cons timeline (map :timeline copies))))
             (map #(acss/timeline->css % objects options))
             (remove str/blank?)
             (seq)
             (str/join "\n\n"))))

(defn animation-css
  "CSS animating the shapes of `ids` of `page` in the code of the Inspect
  (see `codes-css`), for each board they are in; `files` holds the files
  of the components, by id. Nil when nothing of them animates."
  [page files ids]
  (codes-css (code-of-boards page files ids) (:objects page) ids))

(defn animation-json
  "The JSON data of the animation of the shapes of `ids` of `page` (see
  `ctac/motion-json`): of the board they are in, or a vector, one by
  board, when they are in more than one; nil when nothing of them
  animates or plays sound. `files` holds the files of the components, by
  id."
  [page files ids]
  (let [objects (:objects page)
        docs    (into []
                      (comp (map #(ctac/motion-json % objects :ids ids))
                            (filter #(or (seq (get % "layers")) (seq (get % "audio")))))
                      (code-of-boards page files ids))]
    (case (count docs)
      0 nil
      1 (first docs)
      docs)))

(defn json-text
  "The JSON `data` as text, two spaces indented."
  [data]
  (js/JSON.stringify (clj->js data) nil 2))

(defn codes-react
  "React code for motion.dev animating the layers of the boards of `codes`
  (see `ctac/motion-react`), those of `ids` only when given, with the
  classes the HTML of the Inspect gives them; nil when nothing of them
  animates."
  [codes objects ids]
  (ctac/motion-react codes objects
                     :ids ids
                     :class-name cgc/shape->selector))

(defn animation-react
  "React code for motion.dev animating the shapes of `ids` of `page` (see
  `codes-react`); nil when nothing of them animates. `files` holds the
  files of the components, by id."
  [page files ids]
  (codes-react (code-of-boards page files ids) (:objects page) ids))
