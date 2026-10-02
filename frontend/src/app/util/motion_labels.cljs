;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.util.motion-labels
  "The names Penpot Motion shows for its preset animations, shared by the
  workspace (the design tab, the dock, the style editor) and the Inspect
  of the view mode, which loads no workspace code."
  (:require
   [app.util.i18n :refer [tr]]))

(defn split-label
  "The name of the units a split animation of `kind` plays, the whole
  layer for nil."
  [kind]
  (case kind
    :char (tr "workspace.animation.split.char")
    :word (tr "workspace.animation.split.word")
    :line (tr "workspace.animation.split.line")
    (tr "workspace.animation.split.layer")))

(defn animation-label
  "Name of a preset animation, with the units it plays one by one when it
  splits a text (\"Fade in · Words\"), or of the style a style use plays,
  as the sidebar and the timeline show it."
  [{:keys [type direction split] :as animation}]
  (let [out?  (= direction :out)
        label (case type
                :style  (:name animation)
                :fade   (if out? (tr "workspace.animation.fade-out") (tr "workspace.animation.fade-in"))
                :move   (if out? (tr "workspace.animation.move-out") (tr "workspace.animation.move-in"))
                :scale  (if out? (tr "workspace.animation.scale-out") (tr "workspace.animation.scale-in"))
                :rotate (if out? (tr "workspace.animation.rotate-out") (tr "workspace.animation.rotate-in"))
                :blur   (if out? (tr "workspace.animation.blur-out") (tr "workspace.animation.blur-in"))
                (name type))]
    (if (and (some? split) (not= :style type))
      (tr "workspace.animation.label.split" label (split-label split))
      label)))
