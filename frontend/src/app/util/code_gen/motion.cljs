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
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.logic.timelines :as cltl]
   [app.common.types.animation :as cta]
   [app.common.types.animation-code :as ctac]
   [app.util.code-gen.common :as cgc]
   [app.util.code-gen.style-css-values :as sv]
   [cuerdas.core :as str]))

(defn code-selector
  "The CSS selector of `shape` in the code of the Inspect: the class its
  HTML gives it."
  [shape]
  (dm/str "." (cgc/shape->selector shape)))

(defn code-of-boards
  "The code model (see `cltl/code-timelines`) of each board the shapes of
  `ids` of `page` are in, by the name of the board; `files` holds the
  files of the components, by id."
  [page files ids]
  (let [objects (:objects page)
        boards  (->> (into #{} (keep #(cfh/get-shape-id-root-frame objects %)) ids)
                     (sort-by (fn [id] [(dm/get-in objects [id :name]) (str id)])))]
    (into [] (keep #(cltl/code-timelines page files %)) boards)))

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
                 :transform #(sv/get-value :transform % objects {})}]
    (some->> (seq codes)
             (mapcat (fn [{:keys [timeline copies]}] (cons timeline (map :timeline copies))))
             (map #(cta/timeline->css % objects options))
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
