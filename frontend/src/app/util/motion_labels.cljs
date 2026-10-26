;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.util.motion-labels
  "The names Penpot Motion shows for its preset animations and the
  properties it animates, shared by the workspace (the design tab, the
  dock, the style editor) and the Inspect of the view mode, which loads
  no workspace code."
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

(defn property-label
  "The name of `property` (and of its `index`, the fill, stroke or shadow
  it animates, counted from 1), as the dock names its rows."
  [property index]
  (let [n (when (some? index) (inc index))]
    (case property
      :x "Position X"
      :y "Position Y"
      ;; the pairs a wiggle shakes together (see `cta/wiggle-properties`)
      :position (tr "workspace.animation.position")
      :scale (tr "workspace.animation.scale")
      :width "Width"
      :height "Height"
      :scale-x "Scale X"
      :scale-y "Scale Y"
      :rotation "Rotation"
      :rotate-x "Rotation X"
      :rotate-y "Rotation Y"
      :translate-z "Position Z"
      :perspective (tr "workspace.options.perspective")
      :perspective-x (tr "workspace.options.vanishing-point-x")
      :perspective-y (tr "workspace.options.vanishing-point-y")
      :light-angle (tr "workspace.animation.light-angle")
      :light-height (tr "workspace.animation.light-height")
      :light-ambient (tr "workspace.animation.light-ambient")
      :depth-focus (tr "workspace.animation.depth-focus")
      :depth-fade (tr "workspace.animation.depth-fade")
      :depth-blur (tr "workspace.animation.depth-blur")
      :r1 (tr "workspace.options.radius-top-left")
      :r2 (tr "workspace.options.radius-top-right")
      :r3 (tr "workspace.options.radius-bottom-right")
      :r4 (tr "workspace.options.radius-bottom-left")
      :opacity (tr "workspace.options.opacity")
      :fill-color (tr "workspace.animation.fill" n)
      :fill-opacity (tr "workspace.animation.fill-opacity" n)
      :fill-gradient (tr "workspace.animation.fill-gradient" n)
      :stroke-color (tr "workspace.animation.stroke" n)
      :stroke-opacity (tr "workspace.animation.stroke-opacity" n)
      :stroke-width (tr "workspace.animation.stroke-width" n)
      :stroke-gradient (tr "workspace.animation.stroke-gradient" n)
      :stroke-width-top (tr "workspace.animation.stroke-width-top" n)
      :stroke-width-right (tr "workspace.animation.stroke-width-right" n)
      :stroke-width-bottom (tr "workspace.animation.stroke-width-bottom" n)
      :stroke-width-left (tr "workspace.animation.stroke-width-left" n)
      :stroke-dash (tr "workspace.animation.stroke-dash" n)
      :stroke-gap (tr "workspace.animation.stroke-gap" n)
      :stroke-dash-offset (tr "workspace.animation.stroke-dash-offset" n)
      :shadow-offset-x (tr "workspace.animation.shadow-x" n)
      :shadow-offset-y (tr "workspace.animation.shadow-y" n)
      :shadow-blur (tr "workspace.animation.shadow-blur" n)
      :shadow-spread (tr "workspace.animation.shadow-spread" n)
      :shadow-color (tr "workspace.animation.shadow-color" n)
      :shadow-opacity (tr "workspace.animation.shadow-opacity" n)
      :blur (tr "workspace.animation.blur")
      :background-blur (tr "workspace.animation.background-blur")
      :layout-row-gap (tr "workspace.animation.layout-row-gap")
      :layout-column-gap (tr "workspace.animation.layout-column-gap")
      :layout-padding-top (tr "workspace.animation.layout-padding-top")
      :layout-padding-right (tr "workspace.animation.layout-padding-right")
      :layout-padding-bottom (tr "workspace.animation.layout-padding-bottom")
      :layout-padding-left (tr "workspace.animation.layout-padding-left")
      :layout-margin-top (tr "workspace.animation.layout-margin-top")
      :layout-margin-right (tr "workspace.animation.layout-margin-right")
      :layout-margin-bottom (tr "workspace.animation.layout-margin-bottom")
      :layout-margin-left (tr "workspace.animation.layout-margin-left")
      :layout-min-width (tr "workspace.animation.layout-min-width")
      :layout-max-width (tr "workspace.animation.layout-max-width")
      :layout-min-height (tr "workspace.animation.layout-min-height")
      :layout-max-height (tr "workspace.animation.layout-max-height")
      :font-size (tr "workspace.animation.font-size")
      :letter-spacing (tr "workspace.animation.letter-spacing")
      :line-height (tr "workspace.animation.line-height")
      :font-weight (tr "workspace.animation.font-weight")
      :trim-start (tr "workspace.animation.trim-start")
      :trim-end (tr "workspace.animation.trim-end")
      :trim-offset (tr "workspace.animation.trim-offset")
      :content (tr "workspace.animation.path")
      :path-progress (tr "workspace.animation.path-progress")
      (name property))))

(defn animation-label
  "Name of a preset animation, with the units it plays one by one when it
  splits a text (\"Fade in · Words\"), of the style a style use plays, or
  of the row a keyframe entry of a style animates, as the sidebar and the
  timeline show it."
  [{:keys [type direction split] :as animation}]
  (let [out?  (= direction :out)
        label (case type
                :style  (:name animation)
                :keys   (tr "workspace.animation.keys-entry"
                            (property-label (:property animation) (:index animation)))
                :fade   (if out? (tr "workspace.animation.fade-out") (tr "workspace.animation.fade-in"))
                :move   (if out? (tr "workspace.animation.move-out") (tr "workspace.animation.move-in"))
                :scale  (if out? (tr "workspace.animation.scale-out") (tr "workspace.animation.scale-in"))
                :rotate (if out? (tr "workspace.animation.rotate-out") (tr "workspace.animation.rotate-in"))
                :flip-x (if out? (tr "workspace.animation.flip-x-out") (tr "workspace.animation.flip-x-in"))
                :flip-y (if out? (tr "workspace.animation.flip-y-out") (tr "workspace.animation.flip-y-in"))
                :blur   (if out? (tr "workspace.animation.blur-out") (tr "workspace.animation.blur-in"))
                (name type))]
    (if (and (some? split) (not= :style type))
      (tr "workspace.animation.label.split" label (split-label split))
      label)))
