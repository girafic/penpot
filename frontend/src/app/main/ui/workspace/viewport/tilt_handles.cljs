;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.viewport.tilt-handles
  "Handles on the canvas for the 3D rotation (Penpot Motion, see
  `app.common.geom.tilt`): on the outline of a selected layer in 3D as it
  shows, one on its right edge that turns it about Y as it is dragged
  across and one on its top edge that turns it about X as it is dragged
  up and down, as the 3D tool of the Design tab does (with Shift in steps
  of 15°); and the vanishing point of its board, or of a selected board
  that sees layers in 3D, as a crosshair that moves it. A drag is one
  undo step."
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes.transforms :as gtr]
   [app.common.geom.tilt :as gtl]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [app.main.ui.workspace.sidebar.options.menus.tilt-tool :as tilt-tool]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [rumext.v2 :as mf]))

(defn- midpoint
  [a b]
  (gpt/point (/ (+ (:x a) (:x b)) 2) (/ (+ (:y a) (:y b)) 2)))

(defn- seen-board
  "The board whose vanishing point the canvas shows for `shape` of
  `objects`: `shape`, a board that sees layers in 3D, or the board of
  `shape` in 3D; none for an orthographic one, which sees without
  perspective."
  [objects shape]
  (let [board (if (cfh/frame-shape? shape)
                (when (some #(let [child (get objects %)]
                               (and (= (:id shape) (:frame-id child)) (gtl/three-d? child)))
                            (cfh/get-children-ids objects (:id shape)))
                  shape)
                (when (gtl/three-d? shape)
                  (let [board (get objects (:frame-id shape))]
                    (when (and (cfh/frame-shape? board) (not= uuid/zero (:id board)))
                      board))))]
    (when-not (true? (:orthographic board))
      board)))

(defn shown-handles
  "The handles that show for the selected `shape` and the board it is seen
  with, `board` (see `seen-board`): `:tilt` unless the layer is locked,
  `:vanishing-point` when there is such a board and it is not locked."
  [shape board]
  (cond-> #{}
    (not (:blocked shape))                     (conj :tilt)
    (and (some? board) (not (:blocked board))) (conj :vanishing-point)))

(defn- use-drag
  "Pointer handlers of a handle: `on-start` when it is grabbed, then
  `on-move` with the client px it has moved by and whether Shift is down,
  in one undo step."
  [on-start on-move]
  (let [drag-ref (mf/use-ref nil)

        on-pointer-down
        (mf/use-fn
         (mf/deps on-start)
         (fn [event]
           (when (zero? (.-button event))
             (dom/stop-propagation event)
             (dom/prevent-default event)
             (.setPointerCapture (dom/get-current-target event) (.-pointerId event))
             (let [undo-id (js/Symbol)]
               (mf/set-ref-val! drag-ref {:x (.-clientX event) :y (.-clientY event) :undo-id undo-id})
               (st/emit! (dwu/start-undo-transaction undo-id))
               (on-start)))))

        on-pointer-move
        (mf/use-fn
         (mf/deps on-move)
         (fn [event]
           (when-let [{:keys [x y]} (mf/ref-val drag-ref)]
             (on-move (- (.-clientX event) x) (- (.-clientY event) y) (kbd/shift? event)))))

        on-pointer-up
        (mf/use-fn
         (fn [_]
           (when-let [{:keys [undo-id]} (mf/ref-val drag-ref)]
             (mf/set-ref-val! drag-ref nil)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))]
    {:on-pointer-down on-pointer-down
     :on-pointer-move on-pointer-move
     :on-pointer-up on-pointer-up}))

(mf/defc tilt-handle*
  "A handle at `point` turning the layer about `axis` (`:x` or `:y`) as
  it is dragged, `tilt` its tilt `[rx ry]` (see `dwa/tilt-layers`)."
  {::mf/private true}
  [{:keys [point axis zoom tilt shape-id motion-id]}]
  (let [start-ref (mf/use-ref nil)
        on-start  (mf/use-fn (mf/deps tilt) #(mf/set-ref-val! start-ref tilt))
        on-move   (mf/use-fn
                   (mf/deps axis shape-id motion-id)
                   (fn [dx dy snap?]
                     (let [[dx dy] (if (= :x axis) [0 dy] [dx 0])
                           tilt    (tilt-tool/drag-tilt (mf/ref-val start-ref) dx dy snap?)]
                       (st/emit! (dwa/tilt-layers [shape-id] motion-id tilt)))))
        handlers  (use-drag on-start on-move)]
    [:circle {:cx (dm/get-prop point :x)
              :cy (dm/get-prop point :y)
              :r (/ 5 zoom)
              :fill "var(--app-white)"
              :stroke "var(--color-accent-tertiary)"
              :stroke-width (/ 1.5 zoom)
              :data-testid (dm/str "tilt-handle-" (name axis))
              :style {:cursor (if (= :x axis) "ns-resize" "ew-resize")}
              :on-pointer-down (:on-pointer-down handlers)
              :on-pointer-move (:on-pointer-move handlers)
              :on-pointer-up (:on-pointer-up handlers)
              :on-lost-pointer-capture (:on-pointer-up handlers)}
     [:title (if (= :x axis)
               (tr "workspace.viewport.tilt-handle.x")
               (tr "workspace.viewport.tilt-handle.y"))]]))

(mf/defc vanishing-point*
  "The vanishing point of `board` as a crosshair that moves it as it is
  dragged (see `dwa/move-vanishing-point`)."
  {::mf/private true}
  [{:keys [board zoom motion?]}]
  (let [point     (gtl/vanishing-point board)
        start-ref (mf/use-ref nil)
        on-start  (mf/use-fn (mf/deps point) #(mf/set-ref-val! start-ref point))
        on-move   (mf/use-fn
                   (mf/deps board zoom motion?)
                   (fn [dx dy _]
                     (let [start (mf/ref-val start-ref)
                           p     (-> (gpt/point (+ (:x start) (/ dx zoom)) (+ (:y start) (/ dy zoom)))
                                     (gpt/transform (gtr/inverse-transform-matrix board)))
                           sr    (:selrect board)]
                       (when (and (pos? (:width sr)) (pos? (:height sr)))
                         (st/emit! (dwa/move-vanishing-point
                                    (:id board) motion?
                                    [(/ (- (:x p) (:x sr)) (:width sr))
                                     (/ (- (:y p) (:y sr)) (:height sr))]))))))
        handlers  (use-drag on-start on-move)
        x         (dm/get-prop point :x)
        y         (dm/get-prop point :y)
        arm       (/ 10 zoom)
        width     (/ 1.5 zoom)
        color     "var(--color-accent-tertiary)"]
    [:g {:data-testid "vanishing-point"}
     [:line {:x1 (- x arm) :y1 y :x2 (+ x arm) :y2 y
             :stroke color :stroke-width width :pointer-events "none"}]
     [:line {:x1 x :y1 (- y arm) :x2 x :y2 (+ y arm)
             :stroke color :stroke-width width :pointer-events "none"}]
     [:circle {:cx x :cy y :r (/ 5 zoom)
               :fill "transparent"
               :stroke color
               :stroke-width width
               :style {:cursor "move"}
               :on-pointer-down (:on-pointer-down handlers)
               :on-pointer-move (:on-pointer-move handlers)
               :on-pointer-up (:on-pointer-up handlers)
               :on-lost-pointer-capture (:on-pointer-up handlers)}
      [:title (tr "workspace.viewport.vanishing-point")]]]))

(mf/defc tilt-handles*
  "The handles of the 3D rotation of the selected `shape` of `objects`
  (the shapes as they show) and the vanishing point it is seen with, but
  of a locked layer or board (see `shown-handles`); in motion mode
  (`motion?`) an animated value takes the one at the playhead."
  [{:keys [shape objects zoom motion?]}]
  (let [points (mf/with-memo [objects shape]
                 (when (gtl/three-d? shape)
                   (gtl/projected-points objects shape)))
        board  (mf/with-memo [objects shape]
                 (seen-board objects shape))
        shown  (shown-handles shape board)
        tilt   [(or (:rotate-x shape) 0) (or (:rotate-y shape) 0)]
        motion-id (when motion? (:id shape))]
    [:g {:class "tilt-handles"}
     (when (contains? shown :vanishing-point)
       [:> vanishing-point* {:board board :zoom zoom :motion? motion?}])
     (when-let [[p0 p1 p2] (when (contains? shown :tilt) points)]
       [:*
        [:> tilt-handle* {:point (midpoint p0 p1) :axis :x :zoom zoom :tilt tilt
                          :shape-id (:id shape) :motion-id motion-id}]
        [:> tilt-handle* {:point (midpoint p1 p2) :axis :y :zoom zoom :tilt tilt
                          :shape-id (:id shape) :motion-id motion-id}]])]))
