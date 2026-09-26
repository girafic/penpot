;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.animation
  "Events for the keyframe based timeline animation feature (Penpot
  Motion). Persistent edits go through the timeline changes (see
  `commit-timeline`) so they are undoable & synced; playhead / playback
  state is transient and lives under `:workspace-animation`."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.geom.shapes :as gsh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.layout :as layout]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.undo :as dwu]
   [app.main.features :as features]
   [app.main.streams :as ms]
   [app.main.worker :as mw]
   [app.util.dom :as dom]
   [app.util.mouse :as mse]
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [clojure.string :as str]
   [potok.v2.core :as ptk]))

(def ^:private frame-step
  "Playback tick in ms (~60fps)."
  16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-timelines
  [state]
  (get (dsh/lookup-page state) :timelines))

(defn- selection-board-id
  "The board (top-level frame) of the current selection: the selected
  board itself, or the root frame containing the selected shapes. `nil`
  when nothing is selected."
  [state]
  (let [objects  (dsh/lookup-page-objects state)
        selected (dsh/lookup-selected state)]
    (when-let [shape-id (first selected)]
      (cfh/get-shape-id-root-frame objects shape-id))))

(defn active-board-id
  "The board the timeline dock currently targets: the board of the
  selection or, when nothing is selected, the last targeted one (see
  `set-active-board`), so the timeline stays visible and playable."
  [state]
  (or (selection-board-id state)
      (dm/get-in state [:workspace-animation :board-id])))

(defn current-timeline
  "The timeline of the active board (timelines are keyed by board-id)."
  [state]
  (get (get-timelines state) (active-board-id state)))

(defn board-timeline
  "The timeline of `board` or, while the board has none, a new empty one
  named after it. The first edit stores the new one, so the timeline of
  any board can be shown and edited right away."
  [timelines board]
  (when-let [board-id (:id board)]
    (or (get timelines board-id)
        (cta/make-timeline {:board-id board-id :name (:name board)}))))

(defn- current-or-new-timeline
  "The timeline of the active board, a new one when it has none yet (see
  `board-timeline`)."
  [state]
  (let [objects (dsh/lookup-page-objects state)]
    (board-timeline (get-timelines state) (get objects (active-board-id state)))))

(defn playhead
  [state]
  (dm/get-in state [:workspace-animation :playhead] 0))

(defn- item-at
  [items index]
  (get (if (vector? items) items (vec items)) index))

(defn- shape-property-value
  "Capture the current value of an animatable property of `shape` so it
  can be stored as a keyframe; positions relative to `origin` (see
  `cta/position-origin`). Scale defaults to 1 (authored by editing the
  keyframe value)."
  ([shape property origin]
   (shape-property-value shape property origin nil))
  ([shape property origin index]
   (case property
     :x        (- (-> shape :selrect :x) (:x origin))
     :y        (- (-> shape :selrect :y) (:y origin))
     :rotation (or (:rotation shape) 0)
     :opacity  (or (:opacity shape) 1)
     :width    (or (-> shape :selrect :width) 0)
     :height   (or (-> shape :selrect :height) 0)
     :scale-x  1
     :scale-y  1
     :r1       (or (:r1 shape) 0)
     :r2       (or (:r2 shape) 0)
     :r3       (or (:r3 shape) 0)
     :r4       (or (:r4 shape) 0)
     :fill-color     (:fill-color (item-at (:fills shape) index))
     :fill-opacity   (or (:fill-opacity (item-at (:fills shape) index)) 1)
     :stroke-color   (:stroke-color (item-at (:strokes shape) index))
     :stroke-opacity (or (:stroke-opacity (item-at (:strokes shape) index)) 1)
     :stroke-width   (or (:stroke-width (item-at (:strokes shape) index)) 0)
     :shadow-offset-x (or (:offset-x (item-at (:shadow shape) index)) 0)
     :shadow-offset-y (or (:offset-y (item-at (:shadow shape) index)) 0)
     :shadow-blur     (or (:blur (item-at (:shadow shape) index)) 0)
     :shadow-spread   (or (:spread (item-at (:shadow shape) index)) 0)
     :shadow-color    (get-in (item-at (:shadow shape) index) [:color :color])
     :shadow-opacity  (or (get-in (item-at (:shadow shape) index) [:color :opacity]) 1)
     :blur            (or (get-in shape [:blur :value]) 0)
     :background-blur (or (get-in shape [:background-blur :value]) 0)
     (get cta/trim-properties property))))

(def transform-properties
  [:x :y :width :height :scale-x :scale-y :rotation])

(def appearance-properties
  [:fill-color :fill-opacity
   :stroke-color :stroke-opacity :stroke-width
   :shadow-offset-x :shadow-offset-y :shadow-blur
   :shadow-spread :shadow-color :shadow-opacity
   :blur :background-blur
   :trim-start :trim-end :trim-offset])

(def animatable-properties
  (into transform-properties (cons :opacity appearance-properties)))

(defn- store-value
  [m property index v]
  (if (some? index)
    (assoc-in m [property index] v)
    (assoc m property v)))

(defn shape-values-at
  "The value each animatable property of `shape` shows at `time`:
  interpolated from its keyframes when animated, otherwise its own.
  Indexed properties nest as `{index value}`."
  [timeline objects shape time]
  (let [origin   (cta/position-origin timeline objects (:id shape))
        animated (-> (cta/resolve-animations timeline objects)
                     (cta/values-at time)
                     (get (:id shape)))
        values   (into {}
                       (map (fn [property]
                              [property (get animated property
                                             (shape-property-value shape property origin))]))
                       (into transform-properties
                             (concat [:opacity :r1 :r2 :r3 :r4 :blur :background-blur]
                                     (keys cta/trim-properties))))]
    (reduce
     (fn [values {:keys [index properties]}]
       (reduce (fn [values property]
                 (store-value values property index
                              (or (cta/value-of animated property index)
                                  (shape-property-value shape property origin index))))
               values
               properties))
     values
     (cta/appearance-slots shape))))

(defn shape-at
  "`shape` as the animation shows it at `time`: moved, scaled and rotated,
  with its animated opacity, fills, strokes, shadows and blur."
  [timeline objects shape time]
  (let [shape-id  (:id shape)
        modifiers (-> (update timeline :tracks select-keys [shape-id])
                      (cta/timeline->modif-tree objects time)
                      (dm/get-in [shape-id :modifiers]))]
    (gsh/transform-shape shape modifiers)))

(defn value->display
  "The value of `property` as shown to the user: scale and opacity as
  percentages (positions are already relative to the board)."
  [property value]
  (case property
    (:scale-x :scale-y :opacity :fill-opacity :stroke-opacity :shadow-opacity
              :trim-start :trim-end :trim-offset)
    (* value 100)
    value))

(defn display->value
  "Inverse of `value->display`."
  [property display]
  (case property
    (:scale-x :scale-y :opacity :fill-opacity :stroke-opacity :shadow-opacity
              :trim-start :trim-end :trim-offset)
    (/ display 100)
    display))

(defn keyframe-at
  "The keyframe of `property` of `shape-id` placed exactly at `time`."
  ([timeline shape-id property time]
   (keyframe-at timeline shape-id property time nil))
  ([timeline shape-id property time index]
   (d/seek #(and (cta/same-slot? % property index) (= (:time %) time))
           (dm/get-in timeline [:tracks shape-id :keyframes]))))

(defn keyframe-times
  "Sorted, distinct times of the keyframes of `properties` of `shape-id`."
  ([timeline shape-id properties]
   (keyframe-times timeline shape-id properties nil))
  ([timeline shape-id properties index]
   (let [properties (set properties)]
     (->> (dm/get-in timeline [:tracks shape-id :keyframes])
          (filter #(and (contains? properties (:property %))
                        (= (:index %) index)))
          (map :time)
          (distinct)
          (sort)))))

(declare apply-preview select-keyframe set-playhead set-keyframe-value)

(defn- commit-timeline
  "Commit `timeline` (or, when nil, delete it) as the timeline keyed by
  `board-id` on the current page, optionally in the `undo-group` of
  another change (so both undo together). Only what changed is sent (see
  `pcb/change-timeline`), so edits other users make meanwhile to other
  layers are kept; nothing, when nothing changed."
  ([it state board-id timeline]
   (commit-timeline it state board-id timeline nil))
  ([it state board-id timeline undo-group]
   (let [page    (dsh/lookup-page state)
         changes (-> (pcb/empty-changes it)
                     (pcb/with-page page)
                     (pcb/change-timeline board-id timeline))]
     (if (empty? (:redo-changes changes))
       (ptk/data-event ::timeline-unchanged {})
       (dch/commit-changes
        (cond-> changes
          (some? undo-group) (assoc :undo-group undo-group)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TIMELINE CRUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn delete-timeline
  [board-id]
  (ptk/reify ::delete-timeline
    ptk/WatchEvent
    (watch [it state _]
      (rx/of (commit-timeline it state board-id nil)
             (dwm/clear-local-transform)))))

(defn- update-current-timeline
  "Apply `f` to the current timeline (a new one when the active board has
  none yet) and commit the result."
  [f]
  (ptk/reify ::update-current-timeline
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-or-new-timeline state)]
        (let [tl' (f tl)]
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn set-duration
  [duration]
  (update-current-timeline #(assoc % :duration (max 1 (int duration)))))

(defn rename-timeline
  [name]
  (update-current-timeline #(assoc % :name name)))

(defn set-playback-mode
  "Set what playback does at the end of the timeline, one of
  `cta/playback-modes`."
  [mode]
  (update-current-timeline #(-> % (assoc :playback mode) (dissoc :loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYFRAME CRUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- row-values
  "`shape-values-at`, with the rows of `properties` (and optional `index`)
  of `shape` played even while hidden, so a keyframe added to one of them
  keeps its animation."
  [tl objects shape properties index time]
  (let [shape-id (:id shape)
        tl       (reduce (fn [tl property]
                           (cond-> tl
                             (cta/slot-flag? tl shape-id :hidden property index)
                             (cta/toggle-slot-flag shape-id :hidden property index)))
                         tl
                         properties)]
    (shape-values-at tl objects shape time)))

(defn- inserted-keyframe
  "A keyframe of `property` (and optional `index`) of `shape-id` at `time`
  that leaves the animation as it is: the value `values` (see
  `row-values`) gives there, with the timing of the segment it splits
  (see `cta/split-timing`)."
  [tl values shape-id property index time]
  (cond-> (merge {:time time
                  :property property
                  :value (cta/value-of values property index)}
                 (cta/split-timing tl shape-id property index time))
    (some? index) (assoc :index index)))

(defn toggle-keyframes
  "Toggle the keyframes of `properties` of `shape-id` at the playhead:
  remove the ones placed there or, when there are none, add them with
  the value each property shows there, so the animation is unchanged.
  Locked properties keep their keyframes."
  ([shape-id properties]
   (toggle-keyframes shape-id properties nil))
  ([shape-id properties index]
   (ptk/reify ::toggle-keyframes
     ptk/WatchEvent
     (watch [it state _]
       (let [objects    (dsh/lookup-page-objects state)
             existing   (current-timeline state)
             tl         (or existing
                            (when (get objects shape-id)
                              (current-or-new-timeline state)))
             properties (remove #(cta/slot-flag? tl shape-id :locked % index) properties)]
         (if (and (some? tl) (seq properties))
           (let [time     (playhead state)
                 shape    (get objects shape-id)
                 values   (row-values tl objects shape properties index time)
                 placed   (keep #(keyframe-at tl shape-id % time index) properties)
                 tl'      (if (seq placed)
                            (reduce #(cta/remove-keyframe %1 shape-id (:id %2)) tl placed)
                            (reduce (fn [tl property]
                                      (cta/add-keyframe tl shape-id
                                                        (inserted-keyframe tl values shape-id property index time)))
                                    tl
                                    properties))
                 events   (cond-> [(commit-timeline it state (:board-id tl') tl')
                                   (apply-preview)]
                            (nil? existing)
                            (conj (layout/toggle-layout-flag :animation-timeline :force? true)))]
             (apply rx/of events))
           (rx/empty)))))))

(defn add-keyframe-at
  "Add a keyframe of `property` (and optional `index`) of `shape-id` at
  `time` that leaves the animation as it is (see `inserted-keyframe`),
  select it and move the playhead to it, so its value can be changed
  right away. A locked property takes none."
  [shape-id property index time]
  (ptk/reify ::add-keyframe-at
    ptk/WatchEvent
    (watch [it state _]
      (let [tl      (current-timeline state)
            objects (dsh/lookup-page-objects state)
            shape   (get objects shape-id)
            time    (max 0 (int time))]
        (if (and (some? tl)
                 (some? shape)
                 (not (cta/slot-flag? tl shape-id :locked property index)))
          (let [id     (uuid/next)
                values (row-values tl objects shape [property] index time)
                tl'    (cta/add-keyframe tl shape-id
                                         (-> (inserted-keyframe tl values shape-id property index time)
                                             (assoc :id id)))]
            (rx/of (commit-timeline it state (:board-id tl') tl')
                   (select-keyframe shape-id id)
                   (set-playhead time)))
          (rx/empty))))))

(defn set-value-at-playhead
  "Set `property` of `shape-id` to `value` at the playhead: the value of
  the keyframe there, or a new keyframe. For the properties a shape has
  none of its own of (see `cta/trim-properties`), which can not be
  recorded from an edit of the shape."
  [shape-id property value]
  (ptk/reify ::set-value-at-playhead
    ptk/WatchEvent
    (watch [_ state _]
      (let [tl   (current-or-new-timeline state)
            time (playhead state)]
        (if-let [keyframe (keyframe-at tl shape-id property time)]
          (rx/of (set-keyframe-value shape-id (:id keyframe) value))
          (rx/of (update-current-timeline
                  #(cta/add-keyframe % shape-id
                                     (merge {:time time :property property :value value}
                                            (cta/split-timing % shape-id property nil time))))))))))

(defn move-keyframe
  [shape-id keyframe-id time]
  (ptk/reify ::move-keyframe
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/update-keyframe tl shape-id keyframe-id
                                       #(assoc % :time (max 0 (int time))))]
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn retime-keyframes
  "Commit `base` (the timeline as it was when a drag started) with the
  keyframes of `shape-ids` retimed from the span `from` to `to`."
  [base shape-ids from to]
  (ptk/reify ::retime-keyframes
    ptk/WatchEvent
    (watch [it state _]
      (let [tl' (cta/retime-keyframes base shape-ids from to)]
        (rx/of (commit-timeline it state (:board-id tl') tl')
               (apply-preview))))))

(defn set-track-origin
  "Set the scale/rotation pivot of `shape-id` (normalized 0–1)."
  [shape-id ox oy]
  (ptk/reify ::set-track-origin
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/set-track-origin tl shape-id ox oy)]
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn set-keyframe-value
  [shape-id keyframe-id value]
  (ptk/reify ::set-keyframe-value
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/update-keyframe tl shape-id keyframe-id
                                       #(assoc % :value value))]
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn set-keyframe-easing
  "Set the easing of the segment that starts at a keyframe. `:hold` keeps
  its value until the next keyframe instead."
  [shape-id keyframe-id easing]
  (ptk/reify ::set-keyframe-easing
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/update-keyframe tl shape-id keyframe-id
                                       (fn [keyframe]
                                         (if (= easing :hold)
                                           (assoc keyframe :interpolation :step)
                                           (-> keyframe
                                               (assoc :easing easing)
                                               (dissoc :interpolation)))))]
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn delete-keyframe
  [shape-id keyframe-id]
  (ptk/reify ::delete-keyframe
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/remove-keyframe tl shape-id keyframe-id)]
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOTION PATHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private path-samples
  "Points a motion path is drawn with between two keyframes."
  24)

(defn motion-path
  "The motion path of `shape-id`, the way its center moves, in canvas
  coordinates: `:points` along it, and `:keyframes` where both its `:x`
  and `:y` have a keyframe, with their `:point`, their handles (`:out`
  towards the next one, `:in` from the previous one, `[dx dy]`) and
  whether there is a `:next` and a `:prev` one. Nil when its position
  does not move."
  [timeline objects shape-id]
  (let [xs    (cta/property-keyframes timeline shape-id :x)
        ys    (cta/property-keyframes timeline shape-id :y)
        times (into (sorted-set) (map :time) (concat xs ys))]
    (when-let [shape (and (> (count times) 1) (get objects shape-id))]
      (let [origin   (cta/position-origin timeline objects shape-id)
            selrect  (:selrect shape)
            cx       (+ (:x origin) (/ (:width selrect 0) 2))
            cy       (+ (:y origin) (/ (:height selrect 0) 2))
            base-x   (- (:x selrect 0) (:x origin))
            base-y   (- (:y selrect 0) (:y origin))
            resolved (-> (cta/resolve-animations timeline objects)
                         (update :tracks select-keys [shape-id]))
            point    (fn [time]
                       (let [values (get (cta/values-at resolved time) shape-id)]
                         [(+ cx (get values :x base-x))
                          (+ cy (get values :y base-y))]))
            start    (first times)
            end      (last times)
            span     (- end start)
            points   (into []
                           (map #(point (+ start (* span (/ % (* path-samples (dec (count times))))))))
                           (range (inc (* path-samples (dec (count times))))))
            pairs    (into []
                           (keep (fn [time]
                                   (let [x (d/seek #(= time (:time %)) xs)
                                         y (d/seek #(= time (:time %)) ys)]
                                     (when (and x y) [time x y]))))
                           times)
            at       (fn [[_ x y]]
                       [(+ cx (:value x)) (+ cy (:value y))])]
        {:points points
         :keyframes
         (into []
               (map-indexed
                (fn [i [time x y :as pair]]
                  {:time time
                   :point (at pair)
                   :out [(:path-out x 0) (:path-out y 0)]
                   :in [(:path-in x 0) (:path-in y 0)]
                   :prev (some-> (get pairs (dec i)) at)
                   :next (some-> (get pairs (inc i)) at)}))
               pairs)}))))

(defn- set-path-keyframe
  "`keyframe` with the handle components `out` and `in` of its axis; a
  zero one is left out."
  [keyframe out in]
  (cond-> (dissoc keyframe :path-in :path-out)
    (and (some? out) (not (mth/almost-zero? out))) (assoc :path-out out)
    (and (some? in) (not (mth/almost-zero? in))) (assoc :path-in in)))

(defn set-path-handles
  "Set the handles of the motion path at the position keyframes of
  `shape-id` at `time`: `out` towards the next keyframe and `in` from the
  previous one, both `[dx dy]`; nil ones make it straight there."
  [shape-id time out in]
  (update-current-timeline
   (fn [tl]
     (reduce (fn [tl [property axis]]
               (if-let [keyframe (keyframe-at tl shape-id property time)]
                 (cta/update-keyframe tl shape-id (:id keyframe)
                                      #(set-path-keyframe % (get out axis) (get in axis)))
                 tl))
             tl
             [[:x 0] [:y 1]]))))

(defn toggle-path-smooth
  "Curve the motion path through the position keyframes of `shape-id` at
  `time` along its neighbours, or make it straight there again."
  [shape-id time]
  (ptk/reify ::toggle-path-smooth
    ptk/WatchEvent
    (watch [_ state _]
      (let [tl      (current-timeline state)
            objects (dsh/lookup-page-objects state)
            {:keys [point prev next out in]}
            (some->> (motion-path tl objects shape-id)
                     :keyframes
                     (d/seek #(= time (:time %))))]
        (cond
          (nil? point)
          (rx/empty)

          (or (not= [0 0] out) (not= [0 0] in))
          (rx/of (set-path-handles shape-id time nil nil))

          :else
          ;; Along the line from the previous keyframe to the next one,
          ;; a third of the way to each.
          (let [from    (or prev point)
                to      (or next point)
                tangent (mapv #(/ (- %2 %1) 6) from to)]
            (rx/of (set-path-handles shape-id time tangent (mapv - tangent)))))))))

(defn start-path-handle-drag
  "Drag the `side` (`:out` or `:in`) handle of the motion path at the
  position keyframes of `shape-id` at `time`. The other one turns with
  it, unless Alt is held."
  [shape-id time side]
  (ptk/reify ::start-path-handle-drag
    ptk/WatchEvent
    (watch [_ state stream]
      (let [tl       (current-timeline state)
            objects  (dsh/lookup-page-objects state)
            keyframe (some->> (motion-path tl objects shape-id)
                              :keyframes
                              (d/seek #(= time (:time %))))]
        (if (nil? keyframe)
          (rx/empty)
          (let [[px py]  (:point keyframe)
                undo-id  (js/Symbol)
                stopper  (mse/drag-stopper stream)]
            (rx/concat
             (rx/of (dwu/start-undo-transaction undo-id))
             (->> ms/mouse-position
                  (rx/filter some?)
                  (rx/with-latest-from ms/mouse-position-alt)
                  (rx/map (fn [[position alt?]]
                            (let [handle [(- (:x position) px) (- (:y position) py)]
                                  other  (mapv - handle)]
                              (if (= side :out)
                                (set-path-handles shape-id time handle (if alt? (:in keyframe) other))
                                (set-path-handles shape-id time (if alt? (:out keyframe) other) handle)))))
                  (rx/take-until stopper))
             (rx/of (dwu/commit-undo-transaction undo-id)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRACK ACTIONS (timeline context menu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn copy-keyframes
  "Copy the keyframes of `properties` of `shape-id`, with their loop
  flags, to the animation clipboard."
  ([shape-id properties]
   (copy-keyframes shape-id properties nil))
  ([shape-id properties index]
   (ptk/reify ::copy-keyframes
     ptk/UpdateEvent
     (update [_ state]
       (if-let [tl (current-timeline state)]
         (let [properties (set properties)
               keyframes  (filterv #(and (contains? properties (:property %))
                                         (or (nil? index) (= (:index %) index)))
                                   (dm/get-in tl [:tracks shape-id :keyframes]))
               loops      (into #{}
                                (keep (fn [property]
                                        (when (cta/looping? tl shape-id property index)
                                          (if (some? index) [property index] property))))
                                properties)]
           (assoc-in state [:workspace-animation :clipboard]
                     {:keyframes keyframes
                      :loops     loops}))
         state)))))

(defn paste-keyframes
  "Paste the animation clipboard onto `shape-id`, starting at the
  playhead."
  [shape-id]
  (ptk/reify ::paste-keyframes
    ptk/WatchEvent
    (watch [it state _]
      (let [tl (current-timeline state)
            {:keys [keyframes loops]} (dm/get-in state [:workspace-animation :clipboard])]
        (if (and (some? tl) (seq keyframes))
          (let [tl' (cta/paste-keyframes tl shape-id keyframes loops (playhead state))]
            (rx/of (commit-timeline it state (:board-id tl') tl')
                   (apply-preview)))
          (rx/empty))))))

(defn toggle-track-loop
  ([shape-id property]
   (toggle-track-loop shape-id property nil))
  ([shape-id property index]
   (update-current-timeline #(cta/toggle-loop % shape-id property index))))

(defn- commit-flag
  "Commit `tl'`, with a flag of one of its rows toggled. Showing or hiding
  a row drops the preview first: its shape may not be animated anymore."
  [it state tl' flag]
  (apply rx/of
         (cond-> [(commit-timeline it state (:board-id tl') tl')]
           (= flag :hidden) (conj (dwm/clear-local-transform))
           :always          (conj (apply-preview)))))

(defn toggle-track-flag
  "Hide or show (`:hidden`), lock or unlock (`:locked`) the keyframes of
  `property` (and optional `index`) of `shape-id`. Hidden keyframes are
  left out of playback and exports; locked ones are kept from edits."
  ([shape-id flag property]
   (toggle-track-flag shape-id flag property nil))
  ([shape-id flag property index]
   (ptk/reify ::toggle-track-flag
     ptk/WatchEvent
     (watch [it state _]
       (if-let [tl (current-timeline state)]
         (commit-flag it state (cta/toggle-slot-flag tl shape-id flag property index) flag)
         (rx/empty))))))

(defn delete-track
  "Delete the keyframes of `property` of `shape-id`, or its whole track
  when `property` is nil."
  ([shape-id property]
   (delete-track shape-id property nil))
  ([shape-id property index]
   (ptk/reify ::delete-track
     ptk/WatchEvent
     (watch [it state _]
       (if-let [tl (current-timeline state)]
         (let [tl' (if (some? property)
                     (cta/remove-property tl shape-id property index)
                     (cta/remove-track tl shape-id))]
           ;; Drop the preview first: the shape may not be animated anymore.
           (rx/of (commit-timeline it state (:board-id tl') tl')
                  (dwm/clear-local-transform)
                  (apply-preview)))
         (rx/empty))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRESET ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- preset-animations
  "The animations `preset` adds: an animation type, or the several of an
  animation style (see `cta/animation-styles`)."
  [preset]
  (if (contains? cta/animation-types preset)
    [{:type preset}]
    (get cta/animation-styles preset)))

(defn add-animation-preset
  "Add the animations of `preset` to the shapes of `shape-ids` that are in
  the active board, starting at the playhead."
  [shape-ids preset]
  (ptk/reify ::add-animation-preset
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-or-new-timeline state)]
        (let [objects  (dsh/lookup-page-objects state)
              board-id (:board-id tl)
              time     (playhead state)
              targets  (filter #(= board-id (cfh/get-shape-id-root-frame objects %)) shape-ids)
              tl'      (reduce (fn [tl [shape-id animation]]
                                 (cta/add-animation tl shape-id (assoc animation :start time)))
                               tl
                               (for [shape-id targets
                                     animation (preset-animations preset)]
                                 [shape-id animation]))]
          (if (= tl tl')
            (rx/empty)
            (rx/of (commit-timeline it state board-id tl')
                   (apply-preview))))
        (rx/empty)))))

(defn update-animation
  "Merge `attrs` into the animation `animation-id` of `shape-id`. A nil
  setting goes back to the default of the animation type."
  [shape-id animation-id attrs]
  (update-current-timeline
   #(cta/update-animation % shape-id animation-id (fn [animation] (merge animation attrs)))))

(defn retime-animation
  "Commit `base` (the timeline as it was when a drag started) with the
  animation `animation-id` of `shape-id` over `start` and `duration`."
  [base shape-id animation-id start duration]
  (ptk/reify ::retime-animation
    ptk/WatchEvent
    (watch [it state _]
      (let [tl' (cta/update-animation base shape-id animation-id
                                      #(assoc % :start start :duration duration))]
        (rx/of (commit-timeline it state (:board-id tl') tl')
               (apply-preview))))))

(defn toggle-animation-flag
  "Hide or show (`:hidden`), lock or unlock (`:locked`) the animation
  `animation-id` of `shape-id`, see `toggle-track-flag`."
  [shape-id animation-id flag]
  (ptk/reify ::toggle-animation-flag
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (commit-flag it state (cta/toggle-animation-flag tl shape-id animation-id flag) flag)
        (rx/empty)))))

(defn remove-animation
  [shape-id animation-id]
  (ptk/reify ::remove-animation
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/remove-animation tl shape-id animation-id)]
          ;; Drop the preview first: the shape may not be animated anymore.
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (dwm/clear-local-transform)
                 (apply-preview)))
        (rx/empty)))))

(defn- store-keyframe-selection
  [state selected]
  (update state :workspace-animation assoc
          :selected-kfs selected
          :selected-kf (when (= 1 (count selected)) (first selected))))

(defn select-keyframe
  "Select a keyframe or, with `toggle?`, add it to (or remove it from) the
  selection; a nil `keyframe-id` clears it. `:selected-kf` holds the
  keyframe while it is the only one selected (for the easing editor)."
  ([shape-id keyframe-id]
   (select-keyframe shape-id keyframe-id false))
  ([shape-id keyframe-id toggle?]
   (ptk/reify ::select-keyframe
     ptk/UpdateEvent
     (update [_ state]
       (let [keyframe {:shape-id shape-id :keyframe-id keyframe-id}
             selected (dm/get-in state [:workspace-animation :selected-kfs] #{})
             selected (cond
                        (nil? keyframe-id)            #{}
                        (not toggle?)                 #{keyframe}
                        (contains? selected keyframe) (disj selected keyframe)
                        :else                         (conj selected keyframe))]
         (store-keyframe-selection state selected))))))

(defn select-keyframes
  "Replace the keyframe selection. An empty collection clears it."
  [keyframes]
  (ptk/reify ::select-keyframes
    ptk/UpdateEvent
    (update [_ state]
      (store-keyframe-selection state (set keyframes)))))

(defn delete-selected-keyframes
  []
  (ptk/reify ::delete-selected-keyframes
    ptk/WatchEvent
    (watch [it state _]
      (let [tl       (current-timeline state)
            selected (dm/get-in state [:workspace-animation :selected-kfs])]
        (if (and (some? tl) (seq selected))
          (let [tl' (reduce (fn [tl {:keys [shape-id keyframe-id]}]
                              (cta/remove-keyframe tl shape-id keyframe-id))
                            tl
                            selected)]
            (if (= tl tl')
              (rx/of (select-keyframe nil nil))
              ;; Drop the preview first: a shape may not be animated anymore.
              (rx/of (commit-timeline it state (:board-id tl') tl')
                     (select-keyframe nil nil)
                     (dwm/clear-local-transform)
                     (apply-preview))))
          (rx/empty))))))

(defn set-active-board
  "Remember the board the timeline dock targets (see `active-board-id`).
  The canvas shows the animation of that board, the one edits on the
  canvas are recorded to."
  [board-id]
  (ptk/reify ::set-active-board
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :board-id] board-id))

    ptk/WatchEvent
    (watch [_ state _]
      (if (and (contains? (:workspace-layout state) :animation-timeline)
               (not= board-id (dm/get-in state [:workspace-animation :preview :board-id])))
        (rx/of (apply-preview))
        (rx/empty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAYBACK & PREVIEW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-preview
  "Recompute and apply the transient modifiers for the current timeline at
  the playhead (editor scrubbing/playback preview). The modif-tree covers
  transforms AND opacity (as a change-property modifier).

  Renderer-aware: with the WASM renderer (`render-wasm/v1`) the modifiers
  are pushed to the WASM canvas via `set-wasm-modifiers` (transforms for
  every shape, the other attributes as shape properties); with the
  classic SVG renderer they go through `set-modifiers` (the transforms
  move the shape nodes, see `use-dynamic-modifiers`, and the shapes paint
  the other attributes, see `cta/appearance-changes`). When there is no
  active timeline, any stale preview is cleared."
  []
  (ptk/reify ::apply-preview
    ptk/UpdateEvent
    (update [_ state]
      ;; What the canvas shows, for an edit on it to show over it (see
      ;; `dsh/lookup-animation-preview`).
      (if-let [tl (current-timeline state)]
        (assoc-in state [:workspace-animation :preview] {:board-id (:board-id tl) :time (playhead state)})
        (update state :workspace-animation dissoc :preview)))

    ptk/WatchEvent
    (watch [_ state _]
      (if-let [tl (current-timeline state)]
        (let [objects    (dsh/lookup-page-objects state)
              time       (playhead state)
              modif-tree (cta/timeline->modif-tree tl objects time)]
          ;; The selection frame follows the selected shapes (see the
          ;; viewport), not every shape the preview moves.
          (if ^boolean (features/active-feature? state "render-wasm/v1")
            (rx/of (dwm/set-wasm-modifiers modif-tree :skip-selrect? true :animation-preview? true))
            (rx/of (dwm/set-modifiers modif-tree false false {:animation-preview? true}))))
        (rx/of (dwm/clear-local-transform))))))

(defn index-preview
  "Let hovering and clicking on the canvas find the shapes where the
  animation shows them at the playhead. Only while paused: playback
  moves on too fast for it."
  []
  (ptk/reify ::index-preview
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-id (:current-page-id state)
            tl      (current-timeline state)
            objects (dsh/lookup-page-objects state)
            preview (when (and (some? tl)
                               (not (dm/get-in state [:workspace-animation :playing?])))
                      (cltl/shown-shapes tl objects (playhead state)))]
        (if (some? page-id)
          (->> (mw/ask! {:cmd :index/set-preview
                         :page-id page-id
                         :objects preview})
               (rx/ignore))
          (rx/empty))))))

(defn clear-index-preview
  "Hovering and clicking on the canvas find the shapes where they are
  again, on every page (see `index-preview`)."
  []
  (ptk/reify ::clear-index-preview
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (mw/ask! {:cmd :index/clear-previews})
           (rx/ignore)))))

(defn set-playhead
  [time]
  (ptk/reify ::set-playhead
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :playhead] (max 0 (int time))))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (apply-preview)))))

(declare pause)

(defn- show-frame
  "Move the playhead to `time`, playing on in `direction` (see
  `cta/advance-playback`)."
  [time direction]
  (ptk/reify ::show-frame
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation assoc
              :playhead (max 0 (int time))
              :direction direction))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (apply-preview)))))

(defn- playback-step
  "Play one frame on from the playhead, with the timeline as it is now: a
  change made while playing (duration, playback mode, keyframes) or a
  move of the playhead shows right away. At the end of a timeline played
  once, pause."
  []
  (ptk/reify ::playback-step
    ptk/WatchEvent
    (watch [_ state _]
      (if-let [tl (current-or-new-timeline state)]
        (let [direction (dm/get-in state [:workspace-animation :direction] 1)
              {:keys [time direction ended?]}
              (cta/advance-playback tl (playhead state) direction frame-step)]
          (if ended?
            (rx/of (show-frame time direction) (pause))
            (rx/of (show-frame time direction))))
        (rx/of (pause))))))

(defn play
  []
  (ptk/reify ::play
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :playing?] true))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [tl       (current-or-new-timeline state)
            duration (or (:duration tl) 0)
            stopper  (rx/filter (ptk/type? ::pause) stream)]
        (if (or (nil? tl) (<= duration 0))
          (rx/of (pause))
          (rx/concat
           ;; Played to the end, it starts over.
           (if (>= (playhead state) duration)
             (rx/of (show-frame 0 1))
             (rx/empty))
           (->> (rx/interval frame-step)
                (rx/map (fn [_] (playback-step)))
                (rx/take-until stopper))))))))

(defn pause
  []
  (ptk/reify ::pause
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :playing?] false))))

(defn toggle-play
  []
  (ptk/reify ::toggle-play
    ptk/WatchEvent
    (watch [_ state _]
      (if (dm/get-in state [:workspace-animation :playing?])
        (rx/of (pause))
        (rx/of (play))))))

(defn stop
  "Stop playback and rewind to the start."
  []
  (ptk/reify ::stop
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (pause) (set-playhead 0)))))

(defn clear-preview
  "Stop playback and drop the preview modifiers from the canvas."
  []
  (ptk/reify ::clear-preview
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation dissoc :preview))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (pause)
             (dwm/clear-local-transform)))))

(defn close-timeline
  "Leave motion mode. The timeline dock clears the preview when it
  unmounts."
  []
  (ptk/reify ::close-timeline
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (layout/remove-layout-flag :animation-timeline)))))

(defn toggle-motion-mode
  "Enter or leave motion mode: the timeline dock, plus the motion panel
  in place of the design panel."
  []
  (ptk/reify ::toggle-motion-mode
    ptk/WatchEvent
    (watch [_ state _]
      (if (contains? (:workspace-layout state) :animation-timeline)
        (rx/of (close-timeline))
        (rx/of (layout/toggle-layout-flag :animation-timeline :force? true)
               (layout/set-options-mode :design))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CANVAS KEYFRAMES (motion mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-attrs
  "`{shape-id {attr value}}` of the attributes set by `:mod-obj` changes."
  [changes]
  (reduce (fn [result {:keys [type id operations]}]
            (if (= type :mod-obj)
              (reduce (fn [result {:keys [type attr val]}]
                        (cond-> result
                          (= type :set) (assoc-in [id attr] val)))
                      result
                      operations)
              result))
          {}
          changes))

(defn- with-attrs
  "`objects` with the attributes `changes` set."
  [objects changes]
  (reduce-kv (fn [objects id attrs]
               (d/update-when objects id merge attrs))
             objects
             (set-attrs changes)))

(defn- record-canvas-edit
  "Record the edit a commit made on the canvas as keyframes at the
  playhead (see `cta/record-edit`). They join the undo group of the edit,
  so both undo together. The preview shows the shapes as the edit left
  them either way."
  [{:keys [redo-changes undo-changes undo-group]}]
  (ptk/reify ::record-canvas-edit
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [objects (dsh/lookup-page-objects state)
              tl'     (cta/record-edit tl
                                       (with-attrs objects undo-changes)
                                       (with-attrs objects redo-changes)
                                       (playhead state))]
          (if (= tl tl')
            (rx/of (apply-preview))
            (rx/of (commit-timeline it state (:board-id tl) tl' undo-group)
                   (apply-preview))))
        (rx/empty)))))

(defn- restore-preview
  "Show the animation again once an edit drops its modifiers."
  []
  (ptk/reify ::restore-preview
    ptk/WatchEvent
    (watch [_ state _]
      (if (some? (current-timeline state))
        (rx/of (apply-preview))
        (rx/empty)))))

(defn start-canvas-keyframes
  "While motion mode is on, turn the local edits on the canvas (not
  undo/redo nor other users' changes) into keyframes, see
  `record-canvas-edit`."
  []
  (ptk/reify ::start-canvas-keyframes
    ptk/WatchEvent
    (watch [_ _ stream]
      (let [stopper (rx/filter (ptk/type? ::stop-canvas-keyframes) stream)]
        (->> (rx/merge
              (->> stream
                   (rx/filter dch/commit?)
                   (rx/map deref)
                   ;; The store gets an event before the stream does, so
                   ;; the changes of the commit are applied by now.
                   ;; Recording right away shows the result in the frame
                   ;; the edit ends in, with no flash of the shape at rest.
                   ;; Any other change (a duplicate, an undo, another
                   ;; user's edit) may change what the animation shows.
                   (rx/map (fn [{:keys [source save-undo? redo-changes] :as commit}]
                             (if (and (= source :local)
                                      save-undo?
                                      (some #(= :mod-obj (:type %)) redo-changes))
                               (record-canvas-edit commit)
                               (restore-preview)))))
              ;; An edit ends by dropping its modifiers, the animation
              ;; shown with them.
              (->> stream
                   (rx/filter (ptk/type? ::dwm/clear-local-transform))
                   (rx/map restore-preview)))
             (rx/take-until stopper))))))

(defn stop-canvas-keyframes
  []
  (ptk/data-event ::stop-canvas-keyframes {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- safe-filename
  [name ext]
  (dm/str (-> (or name "animation")
              (str/replace #"[^a-zA-Z0-9_-]+" "-"))
          ext))

(defn export-css
  "Generate and download a CSS `@keyframes` file for the current timeline."
  []
  (ptk/reify ::export-css
    ptk/EffectEvent
    (effect [_ state _]
      (when-let [tl (current-timeline state)]
        (let [objects (dsh/lookup-page-objects state)
              css     (cta/timeline->css tl objects)
              blob    (wapi/create-blob css "text/css")]
          (dom/trigger-download (safe-filename (:name tl) ".css") blob))))))

(defn export-lottie
  "Generate and download a Lottie (bodymovin JSON) file for the current
  timeline. The Lottie map contains no uuids, so it serializes cleanly."
  []
  (ptk/reify ::export-lottie
    ptk/EffectEvent
    (effect [_ state _]
      (when-let [tl (current-timeline state)]
        (let [objects (dsh/lookup-page-objects state)
              data    (cta/timeline->lottie tl objects)
              json    (js/JSON.stringify (clj->js data) nil 2)
              blob    (wapi/create-blob json "application/json")]
          (dom/trigger-download (safe-filename (:name tl) ".json") blob))))))
