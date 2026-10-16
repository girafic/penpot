;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.viewport.motion-path
  "The motion path of the selected layer in motion mode: the way it
  moves, its position keyframes, and the handles that curve the path
  (see `dwa/motion-path`); and the line to the layer it follows."
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.rect :as grc]
   [app.common.logic.timelines :as cltl]
   [app.common.types.animation :as cta]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.util.dom :as dom]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:timeline
  (l/derived dwa/current-timeline st/state))

(def ^:private ref:playhead
  (l/derived dwa/playhead st/state =))

(def ^:private ref:playing?
  (l/derived #(dm/get-in % [:workspace-animation :playing?] false) st/state =))

(def ^:private ref:back?
  (l/derived dwa/playing-back? st/state =))

(def ^:private color
  "Like the selection handles."
  "var(--color-accent-tertiary)")

(mf/defc path-handle*
  "A handle of the path at a keyframe. Without one yet, a hollow knob a
  third of the way to the neighbour keyframe: dragging it curves the
  path."
  {::mf/private true}
  [{:keys [shape-id time side point handle unset? zoom]}]
  (let [[px py] point
        [hx hy] (mapv + point handle)

        on-pointer-down
        (mf/use-fn
         (mf/deps shape-id time side)
         (fn [event]
           (when (dom/left-mouse? event)
             (dom/stop-propagation event)
             (st/emit! (dwa/start-path-handle-drag shape-id time side)))))]

    [:g
     [:line {:x1 px :y1 py :x2 hx :y2 hy
             :stroke color
             :stroke-width (/ 1 zoom)
             :stroke-dasharray (when unset? (dm/str (/ 2 zoom) " " (/ 2 zoom)))
             :pointer-events "none"}]
     [:circle {:cx hx :cy hy
               :r (/ 4 zoom)
               :fill (if unset? "white" color)
               :stroke color
               :stroke-width (/ 1 zoom)
               :style {:cursor "move"}
               :pointer-events "visible"
               :on-pointer-down on-pointer-down}]]))

(mf/defc path-keyframe*
  "A position keyframe on the path. A double click curves the path
  through it, or makes it straight again."
  {::mf/private true}
  [{:keys [shape-id keyframe zoom]}]
  (let [{:keys [time point out in prev next]} keyframe
        [x y] point

        ;; A keyframe marks where the shape is at that time, often right
        ;; on it: grabbing it moves the shape, as grabbing the shape does.
        on-pointer-down
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             (st/emit! (dwt/start-move-selected)))))

        on-double-click
        (mf/use-fn
         (mf/deps shape-id time)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dwa/toggle-path-smooth shape-id time))))

        towards
        (fn [other]
          (mapv #(/ (- %2 %1) 3) point other))]

    [:g
     (when (some? next)
       [:> path-handle* {:shape-id shape-id
                         :time time
                         :side :out
                         :point point
                         :handle (if (= [0 0] out) (towards next) out)
                         :unset? (= [0 0] out)
                         :zoom zoom}])
     (when (some? prev)
       [:> path-handle* {:shape-id shape-id
                         :time time
                         :side :in
                         :point point
                         :handle (if (= [0 0] in) (towards prev) in)
                         :unset? (= [0 0] in)
                         :zoom zoom}])
     [:rect {:x (- x (/ 4 zoom))
             :y (- y (/ 4 zoom))
             :width (/ 8 zoom)
             :height (/ 8 zoom)
             :transform (dm/str "rotate(45 " x " " y ")")
             :fill color
             :pointer-events "visible"
             :on-pointer-down on-pointer-down
             :on-double-click on-double-click}]]))

(mf/defc follow-line*
  "A dashed line from the layer `shape-id` to the pivot of the layer it
  follows (see `cta/follow-of`), both where `timeline` shows them at
  `playhead`, on the way back of ping-pong with `back?`."
  {::mf/private true}
  [{:keys [shape-id timeline objects playhead back? zoom]}]
  (let [leader (dm/get-in timeline [:tracks shape-id :follow :leader])
        ends   (mf/with-memo [timeline objects playhead back? shape-id leader]
                 (when (and (some? leader) (contains? objects leader) (contains? objects shape-id))
                   (let [board (:board-id timeline)
                         moved (cltl/shown-shapes {board timeline} objects playhead
                                                  :back (when back? #{board}))
                         at    #(or (get moved %) (get objects %))]
                     {:from (grc/rect->center (:selrect (at shape-id)))
                      :to   (cta/origin-point (at leader) (cta/track-origin (cta/get-track timeline leader)))})))]
    (when-let [{:keys [from to]} ends]
      [:g {:class "follow-line"}
       [:line {:x1 (:x from) :y1 (:y from) :x2 (:x to) :y2 (:y to)
               :stroke color
               :stroke-width (/ 1 zoom)
               :stroke-dasharray (dm/str (/ 6 zoom) " " (/ 3 zoom))
               :pointer-events "none"}]
       [:circle {:cx (:x to) :cy (:y to)
                 :r (/ 3 zoom)
                 :fill color
                 :pointer-events "none"}]])))

(mf/defc motion-path*
  "The motion path of the layer `shape-id`. While it is edited (a drag,
  `editing?`), the path follows the edit as it will be recorded (see
  `dwa/edited-motion-path`). While it stands still, the line to the
  layer it follows, if any."
  [{:keys [shape-id zoom editing?]}]
  (let [timeline (mf/deref ref:timeline)
        objects  (mf/deref refs/workspace-page-objects)
        edit     (mf/deref dwm/motion-edit)
        edit     (when editing? edit)
        playhead (mf/deref ref:playhead)
        playing? (mf/deref ref:playing?)
        back?    (mf/deref ref:back?)
        path     (mf/with-memo [timeline objects shape-id edit playhead]
                   (when (some? timeline)
                     (if (some? edit)
                       (dwa/edited-motion-path timeline objects shape-id playhead edit)
                       (dwa/motion-path timeline objects shape-id))))]
    [:*
     (when (and (some? timeline) (not playing?) (not editing?))
       [:> follow-line* {:shape-id shape-id
                         :timeline timeline
                         :objects objects
                         :playhead playhead
                         :back? back?
                         :zoom zoom}])
     (when (some? path)
       [:g {:class "motion-path"}
        [:polyline {:points (str/join " " (map (fn [[x y]] (dm/str x "," y)) (:points path)))
                    :fill "none"
                    :stroke color
                    :stroke-width (/ 1 zoom)
                    :stroke-dasharray (dm/str (/ 4 zoom) " " (/ 4 zoom))
                    :pointer-events "none"}]
        (for [keyframe (:keyframes path)]
          [:> path-keyframe* {:key (dm/str (:time keyframe))
                              :shape-id shape-id
                              :keyframe keyframe
                              :zoom zoom}])])]))
