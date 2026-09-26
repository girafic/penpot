;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.viewport.motion-path
  "The motion path of the selected layer in motion mode: the way it
  moves, its position keyframes, and the handles that curve the path
  (see `dwa/motion-path`)."
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.util.dom :as dom]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:timeline
  (l/derived dwa/current-timeline st/state))

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

(mf/defc motion-path*
  [{:keys [shape-id zoom]}]
  (let [timeline (mf/deref ref:timeline)
        objects  (mf/deref refs/workspace-page-objects)
        path     (mf/with-memo [timeline objects shape-id]
                   (when (some? timeline)
                     (dwa/motion-path timeline objects shape-id)))]
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
                             :zoom zoom}])])))
