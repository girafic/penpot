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
   [app.common.animation.css :as acss]
   [app.common.animation.lottie :as alottie]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.geom.shapes :as gsh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.path-names :as cpn]
   [app.common.transit :as t]
   [app.common.types.animation :as cta]
   [app.common.types.animation-follow :as ctaf]
   [app.common.types.animation-style :as ctas]
   [app.common.types.shape :as cts]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.animation-mode :as dwam]
   [app.main.data.workspace.edition :as dwe]
   [app.main.data.workspace.layout :as layout]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.main.data.workspace.viewport :as dwv]
   [app.main.data.workspace.viewport-wasm :as dwvw]
   [app.main.data.workspace.zoom :as dwz]
   [app.main.features :as features]
   [app.main.repo :as rp]
   [app.main.streams :as ms]
   [app.main.worker :as mw]
   [app.util.audio :as audio]
   [app.util.clipboard :as clipboard]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.mouse :as mse]
   [app.util.storage :as storage]
   [app.util.timers :as ts]
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [clojure.string :as str]
   [potok.v2.core :as ptk]))

(def ^:private max-frame-gap
  "The most playback moves on in one frame, in ms: back in a hidden tab,
  it goes on where it was instead of jumping."
  500)

(defn frame-step
  "How far playback moves on in a frame `dt` ms after the one before, with
  `carry` ms left over by the frames before: `[step carry]`, the step in
  whole ms. Playback so keeps to the time that passes, however long the
  frames take, and the steps add up to it. With `clamp?` (the default) a
  step is at most `max-frame-gap`; without it, as while it keeps time
  with the sound it plays, the picture catches up with the sound."
  ([carry dt]
   (frame-step carry dt true))
  ([carry dt clamp?]
   (let [dt    (cond-> (max 0 dt) clamp? (min max-frame-gap))
         total (+ carry dt)
         step  (mth/floor total)]
     [step (- total step)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-timelines
  [state]
  (get (dsh/lookup-page state) :timelines))

(dm/export dwam/motion-mode?)
(dm/export dwam/active-board-id)
(dm/export dwam/timeline-maximized?)
(dm/export dwam/canvas-in-window?)

(defn current-timeline
  "The timeline of the active board (timelines are keyed by board-id)."
  [state]
  (get (get-timelines state) (active-board-id state)))

(defn copy-timelines
  "The timelines playing the animations of the component copies in the
  board `board-id` of `objects`, by `[:copy id board]` (see
  `cltl/instance-timelines`); `files` holds the files of the components,
  by id."
  [objects timelines files board-id]
  (cltl/copy-timelines objects timelines files board-id))

(defn- with-style-preview
  "`timelines` with the uses of the style being edited synced with it, as
  the canvas shows them before the style is committed (see
  `preview-animation-style`)."
  [state timelines]
  (if-let [style (dm/get-in state [:workspace-animation :style-preview])]
    (let [objects (dsh/lookup-page-objects state)]
      (d/update-vals timelines #(ctas/sync-timeline % objects (:current-file-id state) {(:id style) style} (:id style))))
    timelines))

(defn- shown-timelines
  "The timelines the canvas shows at the playhead: only the one of the
  active board plays, with the ones of the component copies in it, which
  play the animation of their mains (see `cltl/instance-timelines`); the
  other boards stay as they are. A style being edited shows on its uses
  (see `preview-animation-style`)."
  [state]
  (let [board-id  (active-board-id state)
        timelines (with-style-preview state (get-timelines state))]
    (if (some? board-id)
      (merge (select-keys timelines [board-id])
             (copy-timelines (dsh/lookup-page-objects state) timelines
                             (dsh/lookup-libraries state) board-id))
      {})))

(defn- plays-animation?
  [objects timelines main-page board-id]
  (let [timeline (get timelines board-id)]
    (boolean (or (and (some? timeline) (not (cta/timeline-empty? timeline)))
                 (seq (cltl/instance-timelines objects board-id timeline main-page))))))

(defn animated-board-ids
  "Ids of the boards of `page` that play an animation: their own, or the
  one of component copies in them (see `shown-timelines`); `files` holds
  the files of the components, by id."
  [page files]
  (let [objects   (:objects page)
        timelines (:timelines page)
        main-page (cltl/main-page-of files)]
    (into #{}
          (filter (partial plays-animation? objects timelines main-page))
          (ctst/get-root-frames-ids objects))))

(defn animated-board?
  "Whether the board `board-id` of `page` plays an animation (see
  `animated-board-ids`)."
  [page files board-id]
  (plays-animation? (:objects page) (:timelines page) (cltl/main-page-of files) board-id))

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

(defn- playback-timeline
  "The timeline playback of the active board keeps time with: its own
  or, while that has no tracks, the one of the animations of the
  component copies in the board (see `cltl/with-copies-clock`), with the
  sound of their mains (see `cltl/with-instance-clips`)."
  [state]
  (when-let [tl (current-or-new-timeline state)]
    (let [copies (dissoc (shown-timelines state) (:board-id tl))]
      (-> tl
          (cltl/with-copies-clock (vals copies) (contains? (get-timelines state) (:board-id tl)))
          (cltl/with-instance-clips copies)))))

(defn board-export-timeline
  "The timeline the exports of the board `board-id` of `page` read: its
  own, with the animations of the component copies in it and the sound
  of their mains (see `cltl/with-instance-tracks` and
  `cltl/with-instance-clips`) and, while it has no tracks, keeping time
  with them. With `bake`, for the exports that read keyframes (SVG,
  Lottie, CSS), the layers of the board that follow others play
  keyframes (see `ctaf/bake-follows`), but those Lottie can make children
  of their leaders with `bake` `:lottie` (see
  `alottie/parent-candidates`), and its helper layers nothing (see
  `cta/drop-guides`); the others play their follows
  (those of copies always play keyframes, see
  `cltl/with-instance-tracks`). Nil when the board has no timeline and
  no animated copies.
  `files` holds the files of the components, by id. With `text-split`
  `:whole`, every split animation plays as an animation of its whole
  text, as the markup exports (CSS, SVG and Lottie) play them (see
  `cta/flatten-splits`)."
  [page files board-id & {:keys [text-split bake]}]
  (let [objects   (:objects page)
        timelines (:timelines page)
        stored?   (contains? timelines board-id)
        copies    (copy-timelines objects timelines files board-id)]
    (when-let [own (when (or stored? (seq copies))
                     (board-timeline timelines (get objects board-id)))]
      (-> (cond-> own
            (= :whole text-split) cta/flatten-splits
            bake                  (ctaf/bake-follows objects :keep? (if (= :lottie bake)
                                                                      (alottie/parent-candidates own objects)
                                                                      (constantly false)))
            bake                  (cta/drop-guides))
          (cltl/with-copies-clock (vals copies) stored?)
          (cltl/with-instance-tracks (vals copies) objects :text-split text-split)
          (cltl/with-instance-clips copies)
          ;; and those of the copies
          (cond-> bake (cta/drop-guides))))))

(defn export-timeline
  "The timeline the exports of the active board read (see
  `board-export-timeline`, which takes `opts`)."
  [state & {:as opts}]
  (board-export-timeline (dsh/lookup-page state)
                         (dsh/lookup-libraries state)
                         (active-board-id state)
                         opts))

(defn board-playhead
  "The playhead of the board `board-id`: each board keeps its own (see
  `switch-playhead`)."
  [state board-id]
  (let [anim (:workspace-animation state)]
    (if (= board-id (:board-id anim))
      (get anim :playhead 0)
      (get-in anim [:playheads board-id] 0))))

(defn playhead
  [state]
  (dm/get-in state [:workspace-animation :playhead] 0))

(defn playing-back?
  "Whether the playhead shows the way back of ping-pong, where playback
  goes on back (see `cta/advance-playback`)."
  [state]
  (neg? (dm/get-in state [:workspace-animation :direction] 1)))

(defn- back-boards
  "The active board while the playhead shows its way back (see
  `playing-back?`), for `cltl/animation-tree`."
  [state]
  (when (playing-back? state)
    #{(active-board-id state)}))

(defn shown-playhead
  "The playhead the panels show values at: while playing, the one
  playback started from, so they do not render again each frame."
  [state]
  (let [anim (:workspace-animation state)]
    (if (:playing? anim)
      (get anim :play-start 0)
      (get anim :playhead 0))))

(defn dock-state
  "The animation state `anim` as the timeline dock shows it: while it
  plays, at the playhead playback started from, and without what changes
  each frame, so the dock does not render again each frame."
  [anim]
  (cond-> (dissoc anim :preview :direction)
    (:playing? anim) (assoc :playhead (get anim :play-start 0))))

(def time-units
  "The units the timeline shows times in, in the order its unit button
  goes through them (see `toggle-time-unit`)."
  [:ms :s :frames])

(defn time-unit
  "How the timeline shows times, `:ms`, `:s` or `:frames` (see
  `frame-rate`), as the user last chose; `anim` is the
  `:workspace-animation` state."
  [anim]
  (or (:time-unit anim) (get storage/user ::time-unit :ms)))

(def default-frame-rate
  "Frames per second of a timeline whose export sets none, as the export
  panel offers it."
  30)

(defn frame-rate
  "Frames per second the timeline `timeline` counts frames in: the frame
  rate it is exported with (see `cta/schema:export`), so that a frame of
  the timeline is a frame of the video."
  [timeline]
  (or (dm/get-in timeline [:export :fps]) default-frame-rate))

(defn time->frames
  "`time` (ms) in frames at `fps`, to a tenth of a frame."
  [time fps]
  (mth/precision (/ (* time fps) 1000) 1))

(defn frames->time
  "The time (ms) of the frame `frames` at `fps`."
  [frames fps]
  (mth/round (/ (* frames 1000) fps)))

(defn frame-time
  "`time` (ms) on the nearest whole frame at `fps`."
  [time fps]
  (frames->time (mth/round (/ (* time fps) 1000)) fps))

(defn time->field
  "`time` (ms) as a field shows it in the time `unit` of the timeline,
  in frames at `fps` (see `time-units`)."
  [time unit fps]
  (case unit
    :s      (/ time 1000)
    :frames (time->frames time fps)
    time))

(defn field->time
  "The time (ms) a field in `unit` holds as `value`."
  [value unit fps]
  (case unit
    :s      (mth/round (* value 1000))
    :frames (frames->time value fps)
    value))

(defn format-seconds
  "`time` (ms) in seconds, to the millisecond: `1.25`."
  [time]
  (dm/str (mth/precision (/ time 1000) 3)))

(defn format-time
  "`time` (ms) in the time `unit` of the timeline (see `time-unit`):
  `1250 ms`, `1.25 s` or, at `fps` frames per second, `37.5 f`."
  ([time unit]
   (format-time time unit default-frame-rate))
  ([time unit fps]
   (case unit
     :s      (dm/str (format-seconds time) " s")
     :frames (dm/str (time->frames time fps) " f")
     (dm/str time " ms"))))

(defn toggle-time-unit
  "Show the times of the timeline in the next unit of `time-units`:
  milliseconds, seconds, frames and milliseconds again. The choice is
  kept for the next time."
  []
  (ptk/reify ::toggle-time-unit
    ptk/UpdateEvent
    (update [_ state]
      (let [unit (time-unit (:workspace-animation state))
            next (or (second (drop-while #(not= unit %) time-units)) (first time-units))]
        (assoc-in state [:workspace-animation :time-unit] next)))

    ptk/EffectEvent
    (effect [_ state _]
      (swap! storage/user assoc ::time-unit (dm/get-in state [:workspace-animation :time-unit])))))

(defn full-quality?
  "Whether the animation plays in full quality (effects, antialiasing)
  rather than fast, as the user last chose; `anim` is the
  `:workspace-animation` state. At rest it always shows in full quality."
  [anim]
  (let [value (:full-quality? anim)]
    (if (some? value) value (get storage/user ::full-quality false))))

(defn audio-muted?
  "Whether playback in the workspace plays no sound, as last chosen."
  [anim]
  (let [value (:audio-muted? anim)]
    (if (some? value) value (get storage/user ::audio-muted false))))

(defn- follow-audio!
  "Keep the sound of the timeline playing with the picture (see
  `audio/follow!`): silent while paused or muted, and while sound records
  with the clips kept quiet (`:quiet?` of the recording, see
  `app.main.data.workspace.audio-recording`)."
  [state]
  (let [anim (:workspace-animation state)]
    (audio/follow! (playback-timeline state)
                   (playhead state)
                   (get anim :direction 1)
                   (and (:playing? anim)
                        (not (audio-muted? anim))
                        (not (true? (dm/get-in anim [:audio-recording :quiet?])))))))

(defn toggle-audio-muted
  "Play the sound of the audio clips in the workspace, or not. The choice
  is kept for the next time."
  []
  (ptk/reify ::toggle-audio-muted
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :audio-muted?]
                (not (audio-muted? (:workspace-animation state)))))

    ptk/EffectEvent
    (effect [_ state _]
      (swap! storage/user assoc ::audio-muted (dm/get-in state [:workspace-animation :audio-muted?]))
      (follow-audio! state))))

(defn toggle-full-quality
  "Play the animation in full quality instead of fast, or back (see
  `full-quality?`). The choice is kept for the next time."
  []
  (ptk/reify ::toggle-full-quality
    ptk/UpdateEvent
    (update [_ state]
      (let [value (not (full-quality? (:workspace-animation state)))]
        (assoc-in state [:workspace-animation :full-quality?] value)))

    ptk/EffectEvent
    (effect [_ state _]
      (swap! storage/user assoc ::full-quality (dm/get-in state [:workspace-animation :full-quality?])))))

(defn row-height
  "How tall the rows of the timeline are, in px, as the user last made
  them; nil while they are as tall as they start (see
  `timeline.lanes/row-height`)."
  [anim]
  (or (:row-height anim) (get storage/user ::row-height)))

(defn set-row-height
  "Make the rows of the timeline, and the bars in them, `height` px tall.
  The choice is kept for the next time."
  [height]
  (ptk/reify ::set-row-height
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :row-height] height))

    ptk/EffectEvent
    (effect [_ _ _]
      (swap! storage/user assoc ::row-height height))))

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
  (into transform-properties (concat [:opacity :r1 :r2 :r3 :r4] appearance-properties)))

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
  with its animated opacity, fills, strokes, shadows and blur. A scale
  leaves its strokes, corners and text as they are, which the design
  tab edits."
  [timeline objects shape time]
  (let [shape-id  (:id shape)
        modifiers (-> (update timeline :tracks select-keys [shape-id])
                      (ctaf/timeline->modif-tree objects time)
                      (dm/get-in [shape-id :modifiers]))]
    (gsh/transform-shape shape (cond-> modifiers
                                 (some? modifiers)
                                 (update :structure-child
                                         (fn [ops] (filterv #(not= :scale-content (:type %)) ops)))))))

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

(declare apply-preview select-keyframe set-playhead set-keyframe-value
         store-keyframe-selection)

(defn commit-page-timeline
  "Commit `timeline` (or, when nil, delete it) as the timeline keyed by
  `board-id` on the page `page-id` of the current file, which need not
  be the current page, optionally in the `undo-group` of another change
  (so both undo together). Only what changed is sent (see
  `pcb/change-timeline`), so edits other users make meanwhile to other
  layers are kept; nothing, when nothing changed."
  ([it state page-id board-id timeline]
   (commit-page-timeline it state page-id board-id timeline nil))
  ([it state page-id board-id timeline undo-group]
   (let [changes (-> (pcb/empty-changes it)
                     (pcb/with-page (dsh/lookup-page state page-id))
                     (pcb/change-timeline board-id timeline))]
     (if (empty? (:redo-changes changes))
       (ptk/data-event ::timeline-unchanged {})
       (dch/commit-changes
        (cond-> changes
          (some? undo-group) (assoc :undo-group undo-group)))))))

(defn commit-timeline
  "Commit `timeline` as the timeline keyed by `board-id` on the current
  page (see `commit-page-timeline`)."
  ([it state board-id timeline]
   (commit-timeline it state board-id timeline nil))
  ([it state board-id timeline undo-group]
   (commit-page-timeline it state (:current-page-id state) board-id timeline undo-group)))

(defn set-copy-clock
  "Play the animation of the main of the component copy `copy-id`, in
  the active board, from another time or at another pace, as a
  precomposition: `changes` holds its `:start` (ms) and/or `:speed`
  (see `cta/copy-clock`); the other keeps what it was."
  [copy-id changes]
  (ptk/reify ::set-copy-clock
    ptk/WatchEvent
    (watch [it state _]
      (when-let [tl (current-or-new-timeline state)]
        (let [tl' (cta/set-copy-clock tl copy-id (merge (cta/copy-clock tl copy-id) changes))]
          (when (not= tl tl')
            (rx/of (commit-timeline it state (:board-id tl') tl')
                   (apply-preview))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOLLOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- layer-board-timeline
  "The timeline of the board the layer `shape-id` is in, a new one when it
  has none yet (see `board-timeline`)."
  [state shape-id]
  (let [objects (dsh/lookup-page-objects state)]
    (board-timeline (get-timelines state)
                    (get objects (cfh/get-shape-id-root-frame objects shape-id)))))

(defn set-follow
  "Make the layer `shape-id` follow another layer of its board as
  `follow` says (see `cta/set-follow`), in one undo step; nil follows
  none. A leader it cannot follow (see `cltl/can-follow?`) changes
  nothing."
  [shape-id follow]
  (ptk/reify ::set-follow
    ptk/WatchEvent
    (watch [it state _]
      (let [tl      (layer-board-timeline state shape-id)
            objects (dsh/lookup-page-objects state)
            leader  (:leader follow)
            tl'     (some-> tl (cta/set-follow shape-id follow))]
        (if (and (some? tl)
                 (not= tl tl')
                 (or (nil? leader) (cltl/can-follow? objects tl shape-id leader)))
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview))
          (rx/empty))))))

(defn clear-follow
  "Make the layer `shape-id` follow none (see `set-follow`)."
  [shape-id]
  (set-follow shape-id nil))

(defn update-follow
  "Change how the layer `shape-id` follows its leader: `changes` taken
  into its follow (see `cta/follow-of`), such as `:from` `:rest` to take
  all that moves the leader from where it rests."
  [shape-id changes]
  (ptk/reify ::update-follow
    ptk/WatchEvent
    (watch [_ state _]
      (if-let [follow (some-> (layer-board-timeline state shape-id) (cta/follow-of shape-id))]
        (rx/of (set-follow shape-id (merge follow changes)))
        (rx/empty)))))

(defn set-ik
  "Make the layer `shape-id`, the end of a chain of two follows, reach
  another layer of its board as `ik` says (see `cta/set-ik`), in one undo
  step; nil reaches none. Its elbow bends as the chain is drawn bent (see
  `ctaf/natural-bend`) unless `ik` or the goal it reached says otherwise.
  A new goal it cannot reach (see `cltl/can-reach?`) changes nothing;
  the bend and turn of a chain that cannot reach for now still change."
  [shape-id ik]
  (ptk/reify ::set-ik
    ptk/WatchEvent
    (watch [it state _]
      (let [tl      (layer-board-timeline state shape-id)
            objects (dsh/lookup-page-objects state)
            goal    (:goal ik)
            ik      (cond-> ik
                      (and (some? goal) (nil? (:bend ik)))
                      (assoc :bend (or (some-> tl (cta/ik-of shape-id) :bend)
                                       (some-> tl (ctaf/natural-bend objects shape-id))
                                       :ccw)))
            tl'     (some-> tl (cta/set-ik shape-id ik))]
        ;; only a new goal is checked: a chain broken for now keeps
        ;; what it is set to, and reaches as set once mended
        (if (and (some? tl)
                 (not= tl tl')
                 (or (nil? goal)
                     (= goal (some-> tl (cta/ik-of shape-id) :goal))
                     (cltl/can-reach? objects tl shape-id goal)))
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview))
          (rx/empty))))))

(defn clear-ik
  "Make the layer `shape-id` reach none (see `set-ik`)."
  [shape-id]
  (set-ik shape-id nil))

(defn update-ik
  "Change how the layer `shape-id` reaches its goal: `changes` taken into
  it (see `cta/ik-of`), such as `:bend` or `:turn`."
  [shape-id changes]
  (ptk/reify ::update-ik
    ptk/WatchEvent
    (watch [_ state _]
      (if-let [ik (some-> (layer-board-timeline state shape-id) (cta/ik-of shape-id))]
        (rx/of (set-ik shape-id (merge ik changes)))
        (rx/empty)))))

(def ^:private goal-size
  "The size of the ring `add-goal` makes."
  24)

(defn add-goal
  "Give the layer `shape-id`, the end of a chain of two follows (see
  `ctaf/ik-links`), a new goal to reach: a ring on its pivot where the
  canvas shows it, a helper layer the exports leave out (see
  `cta/guide?`), selected to be moved, in one undo step. Nothing changes
  where it reaches, as its elbow bends as drawn (see `set-ik`), or as it
  was set to, with how it turns."
  [shape-id]
  (ptk/reify ::add-goal
    ptk/WatchEvent
    (watch [it state _]
      (let [page    (dsh/lookup-page state)
            objects (:objects page)
            tl      (layer-board-timeline state shape-id)
            shape   (get objects shape-id)]
        (if (and (some? tl) (some? shape) (some? (ctaf/ik-links tl objects shape-id)))
          (let [board-id (:board-id tl)
                shown    (or (get (dsh/lookup-shown-shapes state) shape-id) shape)
                pivot    (cta/origin-point shown (cta/track-origin (cta/get-track tl shape-id)))
                half     (/ goal-size 2)
                ring     (cts/setup-shape {:type :circle
                                           :name (dm/str (:name shape) " goal")
                                           :x (- (:x pivot) half)
                                           :y (- (:y pivot) half)
                                           :width goal-size
                                           :height goal-size
                                           :fills []
                                           :strokes [{:stroke-color "#FF3D8B"
                                                      :stroke-opacity 1
                                                      :stroke-width 2
                                                      :stroke-style :solid
                                                      :stroke-alignment :inner}]
                                           :parent-id board-id
                                           :frame-id board-id})
                ;; what it is set to stays, a new reach bends as drawn
                old      (cta/ik-of tl shape-id)
                bend     (or (:bend old) (ctaf/natural-bend tl objects shape-id) :ccw)
                tl'      (-> tl
                             (cta/set-guide (:id ring) true)
                             (cta/set-ik shape-id (assoc old :goal (:id ring) :bend bend)))
                changes  (-> (pcb/empty-changes it (:id page))
                             (pcb/with-page page)
                             (pcb/with-objects objects)
                             (pcb/add-object ring)
                             (pcb/change-timeline board-id tl'))
                undo-id  (js/Symbol)]
            (rx/of (dwu/start-undo-transaction undo-id)
                   (dch/commit-changes changes)
                   (dws/select-shapes (d/ordered-set (:id ring)))
                   (dwu/commit-undo-transaction undo-id)
                   (apply-preview)))
          (rx/empty))))))

(defn picking-leader
  "The layer whose leader, or goal (see `picking-purpose`), the next
  click on a layer picks (see `pick-leader`), if any."
  [state]
  (dm/get-in state [:workspace-animation :picking-leader]))

(defn picking-purpose
  "What the pick of the layer `picking-leader` is for: its leader
  (`:leader`, see `pick-leader`) or its goal (`:goal`, see `pick-goal`)."
  [state]
  (dm/get-in state [:workspace-animation :picking-purpose] :leader))

(defn- end-pick-leader
  "End the pick of a leader or goal of `shape-id` (see `pick-leader` and
  `pick-goal`), whose leader is `leader`, or goal `goal`, or none.
  `shape-id` stays selected, while it is on the page."
  [shape-id {:keys [leader goal]}]
  (ptk/reify ::end-pick-leader
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation dissoc :picking-leader :picking-purpose))

    ptk/WatchEvent
    (watch [_ state _]
      (if (and (some? shape-id) (contains? (dsh/lookup-page-objects state) shape-id))
        (let [tl     (layer-board-timeline state shape-id)
              follow (some-> tl (cta/follow-of shape-id))]
          (rx/concat
           (cond
             (and (some? leader) (not= leader shape-id))
             (rx/of (set-follow shape-id (assoc follow :leader leader)))

             (and (some? goal) (not= goal shape-id))
             ;; as stored: an elbow never set bends as drawn
             (rx/of (set-ik shape-id (assoc (dm/get-in tl [:tracks shape-id :ik]) :goal goal)))

             :else
             (rx/empty))
           (rx/of (dws/select-shape shape-id))))
        (rx/empty)))))

(defn cancel-pick-leader
  "Stop waiting for a leader to be picked (see `pick-leader`)."
  []
  (end-pick-leader nil {}))

(defn pick-leader-at
  "A click on the canvas or in the layers while a leader or goal is
  picked (see `pick-leader` and `pick-goal`): the layer `leader-id`
  clicked becomes the leader, or the goal. A click on nothing (nil), or
  on a layer it cannot follow or reach (see `cltl/can-follow?` and
  `cltl/can-reach?`), leaves the pick waiting for another: a click lands
  before the canvas knows the layer under the pointer when it comes at
  once."
  [leader-id]
  (ptk/reify ::pick-leader-at
    ptk/WatchEvent
    (watch [_ state _]
      (let [shape-id (picking-leader state)
            tl       (some->> shape-id (layer-board-timeline state))
            objects  (dsh/lookup-page-objects state)
            goal?    (= :goal (picking-purpose state))]
        (if (and (some? leader-id)
                 (some? tl)
                 (if goal?
                   (cltl/can-reach? objects tl shape-id leader-id)
                   (cltl/can-follow? objects tl shape-id leader-id)))
          (rx/of (end-pick-leader shape-id (if goal? {:goal leader-id} {:leader leader-id})))
          (rx/empty))))))

(def ^:private pick-endings
  "What ends the pick of a leader, as nothing can be picked then: leaving
  motion mode, the page or the file."
  #{::close-timeline
    :app.main.data.workspace.pages/finalize-page
    :app.main.data.workspace/finalize-workspace})

(defn- pick
  "Make the next layer clicked on the canvas or in the layers the leader
  (`purpose` `:leader`) or the goal (`:goal`) of the layer `shape-id`,
  see `pick-leader` and `pick-goal`."
  [shape-id purpose]
  (ptk/reify ::pick-leader
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation
              #(if (= :goal purpose)
                 (assoc % :picking-leader shape-id :picking-purpose :goal)
                 (-> (assoc % :picking-leader shape-id) (dissoc :picking-purpose)))))

    ptk/WatchEvent
    (watch [_ _ stream]
      (let [of-type (fn [types] (rx/filter #(contains? types (ptk/type %)) stream))]
        (->> (rx/merge
              ;; a selection made otherwise moves on
              (->> (of-type #{::dws/select-shape ::dws/select-shapes})
                   (rx/map #(cancel-pick-leader)))
              ;; Esc deselects
              (->> (of-type #{::dws/deselect-all})
                   (rx/map #(end-pick-leader shape-id {})))
              (->> (of-type pick-endings)
                   (rx/map #(cancel-pick-leader))))
             (rx/take 1)
             (rx/take-until (of-type #{::pick-leader ::end-pick-leader})))))))

(defn pick-leader
  "Make the next layer clicked on the canvas or in the layers (see
  `pick-leader-at`, they wait for a click then) the leader of the layer
  `shape-id` (see `set-follow`), which stays selected. Esc cancels,
  `shape-id` still selected; so does picking again (the picker of the
  Follow section). A selection made otherwise (a right-click, select
  all, a duplicate, Tab, a drag in the layers) and leaving motion mode,
  the page or the file end it with no leader."
  [shape-id]
  (pick shape-id :leader))

(defn pick-goal
  "Make the next layer clicked on the canvas or in the layers the goal
  the layer `shape-id` reaches (see `set-ik`), as `pick-leader` does for
  a leader."
  [shape-id]
  (pick shape-id :goal))

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

(defn set-export
  "Keep with the timeline of the board `board-id` how it is exported as
  an animation (see `cta/schema:export`), which the export panel offers
  again."
  [board-id export]
  (ptk/reify ::set-export
    ptk/WatchEvent
    (watch [it state _]
      ;; A board that only has animated copies gets a timeline for it,
      ;; as long as they play (see `cltl/with-copies-clock`).
      (let [objects   (dsh/lookup-page-objects state)
            timelines (get-timelines state)
            stored?   (contains? timelines board-id)]
        (if-let [tl (board-timeline timelines (get objects board-id))]
          (let [copies (vals (copy-timelines objects timelines (dsh/lookup-libraries state) board-id))
                tl     (cond-> tl
                         (not stored?) (cltl/with-copies-clock copies false))]
            (rx/of (commit-timeline it state board-id (update tl :export merge export))))
          (rx/empty))))))

(defn rename-timeline
  [name]
  (update-current-timeline #(assoc % :name name)))

(defn toggle-autoplay
  "Play the timeline when the prototype shows its board, or keep the board
  at rest there until it is played (see `cta/autoplay?`)."
  []
  (update-current-timeline #(cta/with-autoplay % (not (cta/autoplay? %)))))

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
            time    (int time)]
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

(defn shift-keyframes-from
  "Commit `base` (the timeline as it was when a drag started) with the
  keyframes of `keyframes` moved `dt` ms along (see `cta/shift-keyframes`)."
  [base keyframes dt]
  (ptk/reify ::shift-keyframes-from
    ptk/WatchEvent
    (watch [it state _]
      (let [tl' (cta/shift-keyframes base keyframes dt)]
        (rx/of (commit-timeline it state (:board-id tl') tl')
               (apply-preview))))))

(defn shift-selected-keyframes
  "Move the selected keyframes `dt` ms along, as the arrow keys do. The
  playhead goes along when it is on one of them, so the value field keeps
  showing that keyframe."
  [dt]
  (ptk/reify ::shift-selected-keyframes
    ptk/WatchEvent
    (watch [it state _]
      (let [tl       (current-timeline state)
            selected (dm/get-in state [:workspace-animation :selected-kfs])]
        (if (and (some? tl) (seq selected))
          (let [tl'     (cta/shift-keyframes tl selected dt)
                time-of (fn [tl {:keys [shape-id keyframe-id]}]
                          (:time (d/seek #(= keyframe-id (:id %))
                                         (dm/get-in tl [:tracks shape-id :keyframes]))))
                ;; how far they went (a locked one stays)
                moved   (or (some #(let [from (time-of tl %) to (time-of tl' %)]
                                     (when (not= from to) (- to from)))
                                  selected)
                            0)
                on?     (some #(= (playhead state) (time-of tl %)) selected)]
            (rx/concat
             (rx/of (commit-timeline it state (:board-id tl') tl'))
             (if (and on? (not (zero? moved)))
               (rx/of (set-playhead (+ (playhead state) moved)))
               (rx/of (apply-preview)))))
          (rx/empty))))))

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
                                         ;; set by hand, no token sets it
                                         (if (= easing :hold)
                                           (-> keyframe
                                               (assoc :interpolation :step)
                                               (dissoc :applied-tokens))
                                           (-> keyframe
                                               (assoc :easing easing)
                                               (dissoc :interpolation :applied-tokens)))))]
          (rx/of (commit-timeline it state (:board-id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn bind-keyframe-easing
  "Bind the easing of the keyframe `keyframe-id` of `shape-id` to the
  design token `token-name` (a cubic bezier or a spring token), its
  `easing` the one the token gives now (see `cta/token-setting`); token
  changes set it from then on (see `cta/resolve-timeline-tokens`)."
  [shape-id keyframe-id token-name easing]
  (ptk/reify ::bind-keyframe-easing
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/update-keyframe tl shape-id keyframe-id
                                       (fn [keyframe]
                                         (cond-> (-> keyframe
                                                     (assoc :applied-tokens {:easing token-name})
                                                     (dissoc :interpolation))
                                           (some? easing) (assoc :easing easing))))]
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

(defn edited-motion-path
  "The motion path of `shape-id` (see `motion-path`) as it will be once
  `edit`, a modif-tree of `objects` in progress (a drag), is recorded in
  `timeline` at `time` (see `cta/record-edit`): as the canvas shows the
  shapes meanwhile (see `cltl/edit-preview`)."
  [timeline objects shape-id time edit]
  (let [after (cltl/edited-objects objects edit)]
    (motion-path (cta/record-edit timeline objects after time) after shape-id)))
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

(defn write-animation-clip
  "Put the animation clip `clip` (see `cltl/animation-clip`) on the system
  clipboard as a `:copied-animation` payload, where any tab or file
  pastes it. Every copy of animation writes it this way, so the last one
  is the one pasted."
  [clip]
  (ptk/reify ::write-animation-clip
    ptk/WatchEvent
    (watch [_ _ _]
      ;; written at once, within the user gesture that copies
      (->> (rx/from (clipboard/to-clipboard
                     (t/encode-str {:type :copied-animation :version 1 :clip clip}
                                   {:type :json-verbose})))
           (rx/catch (fn [cause]
                       (js/console.error "clipboard blocked:" cause)
                       (rx/empty)))
           (rx/ignore)))))

(defn copy-keyframes
  "Copy the animation of `shape-id` to the clipboard (see
  `write-animation-clip`): with `property`, the keyframes of that row
  (with `index`), hidden or not; otherwise what the layer plays, with its
  presets and pivot."
  [shape-id property index]
  (ptk/reify ::copy-keyframes
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)
            page    (dsh/lookup-page state)
            files   (dsh/lookup-libraries state)
            clip    (if (some? property)
                      (cltl/animation-clip file-id page (:objects page) files [shape-id]
                                           :properties [[property index]]
                                           :presets? false)
                      (cltl/animation-clip file-id page (:objects page) files [shape-id]
                                           :whole-layer? true))]
        (when (seq (:layers clip))
          (rx/of (write-animation-clip clip)))))))

(defn selected-keyframes-clip
  "The animation clip (see `cltl/animation-clip`) of the keyframes
  selected in the dock, hidden or not, by layer."
  [state]
  (let [selected  (dm/get-in state [:workspace-animation :selected-kfs])
        shape-ids (into [] (comp (map :shape-id) (distinct) (take cltl/max-clip-layers)) selected)
        page      (dsh/lookup-page state)]
    (cltl/animation-clip (:current-file-id state) page (:objects page)
                         (dsh/lookup-libraries state) shape-ids
                         :keyframe-ids (map :keyframe-id selected)
                         :presets? false)))

(defn payload-clip
  "The animation clip of the clipboard text `text` when it holds a valid
  `:copied-animation` payload (see `write-animation-clip`), or nil."
  [text]
  ;; a cheap look first: the clipboard may hold large shape payloads
  (when (and (string? text) (str/includes? text "~:copied-animation"))
    (let [payload (ex/ignoring (t/decode-str text))]
      (when (and (map? payload)
                 (= :copied-animation (:type payload))
                 (int? (:version payload))
                 (cltl/animation-clip-valid? (:clip payload)))
        (:clip payload)))))

(defn clip-targets
  "What the dock pastes the animation clip `clip` onto, the pairs
  `[{:layer :target-id}]` of `cltl/paste-animation`: with one layer, onto
  each layer of `ids` (see `cltl/paste-targets`) of `objects`; otherwise,
  or without `ids`, back onto the layers it was copied from, when it was
  copied on the page `page-id` of the file `file-id`."
  [clip objects ids file-id page-id]
  (let [layers  (:layers clip)
        targets (cltl/paste-targets objects ids)]
    (cond
      (and (= 1 (count layers)) (seq targets))
      (mapv #(hash-map :layer (first layers) :target-id %) targets)

      (and (= file-id (:file-id clip)) (= page-id (:page-id clip)))
      (into []
            (keep #(when (contains? objects (:shape-id %))
                     {:layer % :target-id (:shape-id %)}))
            layers)

      :else
      [])))

(defn paste-result-toast
  "The toast telling what a paste of an animation clip (see
  `cltl/paste-animation`) left out or changed, or nil."
  [{:keys [skipped grown]}]
  (let [notes (cond-> []
                (pos? skipped) (conj (tr "workspace.animation.paste-skipped" skipped))
                (seq grown)    (conj (tr "workspace.animation.paste-grown")))]
    (when (seq notes)
      (ntf/show {:content (str/join " " notes)
                 :type :toast
                 :level (if (pos? skipped) :warning :info)
                 :timeout 5000}))))

(defn- pasted-keyframes
  "The keyframes of the layers `targets` in `timelines` that are not in
  `before`, as the keyframe selection holds them."
  [before timelines targets]
  (let [ids-of (fn [timelines shape-id]
                 (when-let [[_ track] (cltl/find-track timelines shape-id)]
                   (into #{} (map :id) (:keyframes track))))]
    (into #{}
          (mapcat (fn [shape-id]
                    (let [old (ids-of before shape-id)]
                      (for [id (ids-of timelines shape-id)
                            :when (not (contains? old id))]
                        {:shape-id shape-id :keyframe-id id}))))
          targets)))

(defn- with-pasted-audio
  "`result`, of `cltl/paste-animation`, with the audio clips of `clip`
  pasted into the timeline of the board `board-id` (a new one named after
  it when it has none, see `cltl/paste-audio`), `dt` ms later: `:clips`,
  their ids."
  [result objects clip board-id dt]
  (if (and (seq (:audio clip)) (some? board-id))
    (let [timeline (or (dm/get-in result [:timelines board-id])
                       (board-timeline (:timelines result) (get objects board-id)))
          {:keys [timeline ids]} (cltl/paste-audio timeline (:audio clip) dt)]
      (-> result
          (assoc-in [:timelines board-id] timeline)
          (assoc :clips ids)))
    result))

(defn- with-own-audio
  "An observable of `clip`, an animation clip copied in another file, with
  its audio clips playing copies, in the file `file-id`, of the media
  objects they played: one clone of each, before the paste, as
  `dwcp/with-own-audio` does for boards. A clip whose clone fails keeps
  its id (the server makes a copy when it saves it, see
  `bfc/update-media-references!`)."
  [file-id clip]
  (->> (rx/from (into #{} (map :audio-id) (:audio clip)))
       (rx/merge-map (fn [id]
                       (->> (rp/cmd! :clone-file-media-object {:file-id file-id :is-local true :id id})
                            (rx/map (fn [fmo] [id (:id fmo)]))
                            (rx/catch (fn [_] (rx/empty))))))
       (rx/reduce conj {})
       (rx/map (fn [index]
                 (-> clip
                     (assoc ::own-audio? true)
                     (update :audio (partial mapv #(update % :audio-id (fn [id] (get index id id))))))))))

(defn- paste-clip*
  "Paste the animation clip `clip` onto the layers `ids` (see
  `clip-targets`), added to what they play, and its audio clips into the
  active board (see `cltl/paste-audio`), with the earliest of them (see
  `cltl/paste-start`) at the playhead, and select what was pasted."
  [clip ids]
  (ptk/reify ::paste-clip
    ptk/WatchEvent
    (watch [it state _]
      (cond
        (-> state :workspace-global :read-only?)
        (rx/empty)

        (not (features/active-feature? state "animation/v1"))
        (rx/of (ntf/warn (tr "workspace.animation.paste-needs-motion")))

        ;; the audio of another file is cloned into this one first
        (and (seq (:audio clip))
             (not= (:current-file-id state) (:file-id clip))
             (not (::own-audio? clip)))
        (->> (with-own-audio (:current-file-id state) clip)
             (rx/map #(paste-clip* % ids)))

        :else
        (let [file-id (:current-file-id state)
              page    (dsh/lookup-page state)
              objects (:objects page)
              time    (playhead state)
              start   (cltl/paste-start clip)
              pairs   (clip-targets clip objects ids file-id (:id page))
              ;; the layers keep their distance to the audio clips
              result  (-> (cltl/paste-animation file-id (:id page) (:timelines page) objects clip pairs
                                                {:mode :merge
                                                 :time (+ time (- (cltl/paste-start (dissoc clip :audio)) start))})
                          (with-pasted-audio objects clip (active-board-id state) (- time start)))]
          (cond
            (= :too-large (:error result))
            (rx/of (ntf/warn (tr "workspace.animation.paste-too-large")))

            (and (empty? (:targets result)) (empty? (:clips result)))
            (rx/of (ntf/warn (tr "workspace.animation.paste-no-target")))

            :else
            (let [pasted  (pasted-keyframes (:timelines page) (:timelines result) (:targets result))
                  clips   (:clips result)
                  toast   (paste-result-toast result)
                  changes (cltl/commit-timelines (pcb/empty-changes it) page (:timelines result))]
              (rx/from (cond-> [(dch/commit-changes changes)
                                #(cond-> (store-keyframe-selection % pasted)
                                   (seq clips)
                                   (update :workspace-animation
                                           (fn [anim]
                                             (-> (dissoc anim :selected-anims)
                                                 (assoc :selected-clips (set clips)
                                                        :selected-clip (first clips))))))]
                         (motion-mode? state) (conj (apply-preview))
                         (some? toast)        (conj toast))))))))))

(defn paste-clip-at-playhead
  "Paste the animation clip `clip`, from Cmd/Ctrl+V in the dock, at the
  playhead: with one layer, onto the selected layers of the active board;
  otherwise back onto the layers it was copied from (see
  `clip-targets`)."
  [clip]
  (ptk/reify ::paste-clip-at-playhead
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects   (dsh/lookup-page-objects state)
            board-id  (active-board-id state)
            in-board? #(or (= % board-id) (= board-id (cfh/get-shape-id-root-frame objects %)))]
        (rx/of (paste-clip* clip (filter in-board? (dsh/lookup-selected state))))))))

(defn paste-clip
  "Paste the animation clip `clip` at the playhead onto `shape-id`, from
  the menu of its row, when it holds one layer; otherwise back onto the
  layers it was copied from (see `clip-targets`)."
  [clip shape-id]
  (paste-clip* clip [shape-id]))

(defn paste-clipboard-onto
  "Paste the animation on the clipboard onto `shape-id` (see
  `paste-clip`)."
  [shape-id]
  (ptk/reify ::paste-clipboard-onto
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rx/from (clipboard/read-text))
           (rx/map (fn [text]
                     (if-let [clip (payload-clip text)]
                       (paste-clip clip shape-id)
                       (ntf/warn (tr "workspace.animation.paste-invalid")))))
           (rx/catch (fn [cause]
                       (rx/of (ntf/warn (or (clipboard/error-message cause)
                                            (tr "workspace.animation.paste-invalid"))))))))))

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
;; MARKERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-marker
  "Add a marker named `name` at `time`, or at the playhead (see
  `cta/add-marker`)."
  ([name]
   (add-marker name nil))
  ([name time]
   (ptk/reify ::add-marker
     ptk/WatchEvent
     (watch [it state _]
       (if-let [tl (current-or-new-timeline state)]
         (let [tl' (cta/add-marker tl {:time (or time (playhead state)) :name name})]
           (rx/of (commit-timeline it state (:board-id tl') tl')))
         (rx/empty))))))

(defn move-marker-from
  "Commit `base` (the timeline as it was when a drag started) with the
  marker `marker-id` at `time`."
  [base marker-id time]
  (ptk/reify ::move-marker-from
    ptk/WatchEvent
    (watch [it state _]
      (let [tl' (cta/update-marker base marker-id #(assoc % :time time))]
        (rx/of (commit-timeline it state (:board-id tl') tl'))))))

(defn rename-marker
  [marker-id name]
  (update-current-timeline #(cta/update-marker % marker-id (fn [marker] (assoc marker :name name)))))

(defn remove-marker
  [marker-id]
  (update-current-timeline #(cta/remove-marker % marker-id)))

(defn jump-to-marker
  "Move the playhead to the next marker (`direction` 1) or to the one
  before (-1)."
  [direction]
  (ptk/reify ::jump-to-marker
    ptk/WatchEvent
    (watch [_ state _]
      (let [tl     (current-timeline state)
            time   (playhead state)
            marker (when (some? tl)
                     (if (pos? direction)
                       (cta/marker-after tl time)
                       (cta/marker-before tl time)))]
        (if (some? marker)
          (rx/of (set-playhead (:time marker)))
          (rx/empty))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRESET ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- preset-animations
  "The animations `preset` adds: an animation type, or the several of a
  built-in composite (see `cta/builtin-styles`)."
  [preset]
  (if (contains? cta/animation-types preset)
    [{:type preset}]
    (get cta/builtin-styles preset)))

(def ^:private default-stagger
  "Milliseconds between the starts of a preset added to several layers."
  100)

(defn stagger
  "The milliseconds between the starts of a preset added to several
  layers, as last set."
  [state]
  (dm/get-in state [:workspace-animation :stagger] default-stagger))

(defn set-stagger
  [ms]
  (ptk/reify ::set-stagger
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :stagger] (max 0 (int ms))))))

(defn- layer-order
  "The layers of `board-id`, top-most first as the layers panel and the
  timeline list them."
  [objects board-id]
  (letfn [(walk [id]
            (cons id (mapcat walk (reverse (dm/get-in objects [id :shapes])))))]
    (walk board-id)))

(defn- add-staggered
  "Add to the shapes of `shape-ids` that are in the active board the
  animations `animations-at` gives for a start time: from the playhead,
  one shape after the other by the `stagger` in the order of the layers,
  top-most first."
  [it state shape-ids animations-at]
  (if-let [tl (current-or-new-timeline state)]
    (let [objects  (dsh/lookup-page-objects state)
          board-id (:board-id tl)
          time     (playhead state)
          step     (stagger state)
          order    (into {} (map-indexed (fn [index id] [id index])) (layer-order objects board-id))
          targets  (->> shape-ids
                        (filter #(= board-id (cfh/get-shape-id-root-frame objects %)))
                        (sort-by order))
          tl'      (reduce (fn [tl [shape-id animation]]
                             (cta/add-animation tl shape-id animation))
                           tl
                           (for [[index shape-id] (map-indexed vector targets)
                                 animation        (animations-at (+ time (* index step)))]
                             [shape-id animation]))]
      (if (= tl tl')
        (rx/empty)
        (rx/of (commit-timeline it state board-id tl')
               (apply-preview))))
    (rx/empty)))

(defn add-animation-preset
  "Add the animations of `preset` to the shapes of `shape-ids` that are in
  the active board, starting at the playhead, one after the other by the
  `stagger` in the order of the layers, top-most first. A text preset
  (see `cta/text-styles`) goes to the texts among them only."
  [shape-ids preset]
  (ptk/reify ::add-animation-preset
    ptk/WatchEvent
    (watch [it state _]
      (let [objects   (dsh/lookup-page-objects state)
            shape-ids (cond->> shape-ids
                        (contains? cta/text-styles preset)
                        (filter #(cfh/text-shape? (get objects %))))]
        (add-staggered it state shape-ids
                       (fn [start]
                         (map #(assoc % :start start) (preset-animations preset))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANIMATION STYLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn motion?
  "Whether the current file animates (the `animation/v1` feature)."
  [state]
  (features/active-feature? state "animation/v1"))

(defn shape-timeline
  "The timeline of the board of the shape `shape-id`, nil when it has none."
  [state shape-id]
  (let [objects (dsh/lookup-page-objects state)]
    (get (get-timelines state) (cfh/get-shape-id-root-frame objects shape-id))))

(defn save-animation-style
  "Save the presets, style uses and rows of keyframes of the shape
  `shape-id` that are neither locked nor hidden (nor looping, for rows)
  as a new animation style of the current file, named `name` (slashes put
  it in groups), and put one use of it in their place, so the shape
  follows the style (see `ctas/replace-with-use`): one undo step. Refused,
  with a message, when they hold more entries or keyframes than a style
  does."
  [shape-id name]
  (ptk/reify ::save-animation-style
    ptk/WatchEvent
    (watch [it state _]
      (let [tl     (when (motion? state) (shape-timeline state shape-id))
            track  (cta/get-track tl shape-id)
            result (when (some? track)
                     (ctas/animations->entries track (cta/rest-pose tl (dsh/lookup-page-objects state) shape-id)))]
        (cond
          (nil? track)
          (rx/empty)

          (= :too-many-entries (:error result))
          (rx/of (ntf/warn (tr "workspace.animation.style-too-many-entries"
                               cta/max-style-entries (:count result))))

          (= :too-many-keyframes (:error result))
          (rx/of (ntf/warn (tr "workspace.animation.style-too-many-keyframes"
                               cta/max-style-keyframes (:count result))))

          (some? (:error result))
          (rx/empty)

          :else
          (let [[path name] (cpn/split-group-name name)
                file-id (:current-file-id state)
                style   (ctas/make-animation-style {:name name :path path :entries (:entries result)})
                use     (ctas/make-use style file-id (:start result))
                tl'     (update-in tl [:tracks shape-id] ctas/replace-with-use use)
                changes (-> (pcb/empty-changes it)
                            (pcb/add-animation-style style)
                            (pcb/with-page (dsh/lookup-page state))
                            (pcb/change-timeline (:board-id tl) tl'))]
            (rx/of (dch/commit-changes changes)
                   (apply-preview))))))))

(defn apply-animation-style
  "Add a use of the animation style `style-id` of the file `library-id`
  (the current one, or a linked library) to the shapes of `shape-ids` that
  are in the active board, from the playhead, one after the other by the
  `stagger` in the order of the layers, top-most first."
  [shape-ids library-id style-id]
  (ptk/reify ::apply-animation-style
    ptk/WatchEvent
    (watch [it state _]
      (let [style (dm/get-in (dsh/lookup-libraries state) [library-id :data :animation-styles style-id])]
        (if (and (motion? state) (some? style))
          (add-staggered it state shape-ids #(vector (ctas/make-use style library-id %)))
          (rx/empty))))))

(defn detach-animation-style
  "Turn the style use `use-id` of the shape `shape-id` into the presets it
  plays, in its place, and its keyframe entries into keyframes of the
  shape (see `ctas/detach-track-use`), unless it is locked."
  [shape-id use-id]
  (ptk/reify ::detach-animation-style
    ptk/WatchEvent
    (watch [it state _]
      (let [tl  (when (motion? state) (shape-timeline state shape-id))
            use (cta/get-animation tl shape-id use-id)]
        (if (and (cta/style-use? use) (not (:locked use)))
          (let [objects (dsh/lookup-page-objects state)
                tl'     (update-in tl [:tracks shape-id] ctas/detach-track-use use-id tl objects)]
            (rx/of (commit-timeline it state (:board-id tl) tl')
                   (apply-preview)))
          (rx/empty))))))

(defn preview-animation-style
  "Show `style`, a style of the current file being edited, on its uses
  without committing it: the canvas plays them synced with it (see
  `ctas/sync-timeline`) until it is committed (see
  `app.main.data.workspace.libraries/update-animation-style`) or the
  preview ends (see `end-animation-style-preview`)."
  [style]
  (ptk/reify ::preview-animation-style
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (motion? state)
        (assoc-in [:workspace-animation :style-preview] style)))

    ptk/WatchEvent
    (watch [_ state _]
      (if (motion? state)
        (rx/of (apply-preview))
        (rx/empty)))))

(defn end-animation-style-preview
  "Stop showing a style being edited on its uses, with nothing committed
  (see `preview-animation-style`)."
  []
  (ptk/reify ::end-animation-style-preview
    ptk/UpdateEvent
    (update [_ state]
      (d/update-when state :workspace-animation dissoc :style-preview))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (apply-preview)))))

(defn update-animation
  "Merge `attrs` into the animation `animation-id` of `shape-id`. A nil
  setting goes back to the default of the animation type."
  [shape-id animation-id attrs]
  (update-current-timeline
   #(cta/update-animation % shape-id animation-id (fn [animation] (merge animation attrs)))))

(defn set-animation-split
  "Play the animation `animation-id` of the text `shape-id` by `kind` of
  units (`:char`, `:word` or `:line`; nil for the whole layer), and the
  other split presets of the layer by it too, a text playing one kind
  (see `cta/with-split-kind`): one change to undo."
  [shape-id animation-id kind]
  (update-current-timeline
   #(d/update-in-when % [:tracks shape-id :animations]
                      cta/with-split-kind animation-id kind cta/make-animation)))

(defn retime-animation
  "Commit `base` (the timeline as it was when a drag started) with the
  animation `animation-id` of `shape-id` over `start` and `duration`, set
  by hand: the tokens that bound them let go (see `cta/set-by-hand`)."
  [base shape-id animation-id start duration]
  (ptk/reify ::retime-animation
    ptk/WatchEvent
    (watch [it state _]
      (let [tl' (cta/update-animation base shape-id animation-id
                                      #(cta/set-by-hand % {:start start :duration duration}))]
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
  "`state` with the keyframes `selected`; selecting some lets go of the
  selected preset animations and audio clips (see `dwaa/selection-of`),
  but when they are `added?` to what is selected."
  ([state selected]
   (store-keyframe-selection state selected false))
  ([state selected added?]
   (update state :workspace-animation
           #(cond-> (assoc %
                           :selected-kfs selected
                           :selected-kf (when (= 1 (count selected)) (first selected)))
              (and (seq selected) (not added?))
              (dissoc :selected-anims :selected-clip :selected-clips)))))

(defn select-animation-block
  "Select the preset animation `animation-id` of `shape-id` in the dock,
  alone or, with `toggle?`, adding it to (or taking it out of) what is
  selected there, keyframes and clips too (see `dwaa/selection-of`)."
  [shape-id animation-id toggle?]
  (ptk/reify ::select-animation-block
    ptk/UpdateEvent
    (update [_ state]
      (let [ref {:shape-id shape-id :animation-id animation-id}]
        (update state :workspace-animation
                (fn [anim]
                  (if toggle?
                    (let [selected (or (:selected-anims anim) #{})
                          selected (if (contains? selected ref) (disj selected ref) (conj selected ref))]
                      (if (empty? selected)
                        (dissoc anim :selected-anims)
                        (assoc anim :selected-anims selected)))
                    (-> anim
                        (assoc :selected-anims #{ref})
                        (dissoc :selected-kfs :selected-kf :selected-clip :selected-clips)))))))))

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
         (store-keyframe-selection state selected toggle?))))))

(defn select-keyframes
  "Replace the keyframe selection. An empty collection clears it."
  [keyframes]
  (ptk/reify ::select-keyframes
    ptk/UpdateEvent
    (update [_ state]
      (store-keyframe-selection state (set keyframes)))))

(defn select-slot-keyframes
  "Select the keyframes of `property` (and optional `index`) of `shape-id`,
  what a click on its row in the timeline picks; with `add?` add them to
  the selection. The keyframes of a locked property cannot be selected."
  [shape-id property index add?]
  (ptk/reify ::select-slot-keyframes
    ptk/UpdateEvent
    (update [_ state]
      (let [tl (current-timeline state)]
        (if (or (nil? tl) (cta/slot-flag? tl shape-id :locked property index))
          state
          (let [picked (into #{}
                             (map (fn [kf] {:shape-id shape-id :keyframe-id (:id kf)}))
                             (cta/property-keyframes tl shape-id property index))]
            (store-keyframe-selection
             state
             (if add?
               (into (dm/get-in state [:workspace-animation :selected-kfs] #{}) picked)
               picked)
             add?)))))))

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

(declare pause)

(defn- switch-playhead
  "The animation state `anim` targeting `board-id`: each board keeps its
  playhead, so the one of the board left stays with it and the one of
  `board-id` comes back, the start the first time."
  [anim board-id]
  (let [prev (:board-id anim)]
    (if (= prev board-id)
      anim
      (-> anim
          (cond-> (some? prev)
            (assoc-in [:playheads prev] (get anim :playhead 0)))
          (assoc :board-id board-id
                 :playhead (get-in anim [:playheads board-id] 0)
                 ;; shown on the way there (see `playing-back?`)
                 :direction 1)))))

(defn set-active-board
  "Remember the board the timeline dock targets (see `active-board-id`).
  The canvas shows the animation of that board, the one edits on the
  canvas are recorded to, at the playhead the board was left at (see
  `switch-playhead`). What played stops."
  [board-id]
  (ptk/reify ::set-active-board
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation switch-playhead board-id))

    ptk/WatchEvent
    (watch [_ state _]
      (let [shown (dm/get-in state [:workspace-animation :preview :board-id])]
        (if (not= board-id shown)
          (rx/concat
           (if (dm/get-in state [:workspace-animation :playing?])
             (rx/of (pause))
             (rx/empty))
           ;; The board shown before goes back to how it is.
           (if (contains? (:workspace-layout state) :animation-timeline)
             (rx/of (dwm/clear-local-transform)
                    (apply-preview))
             (rx/empty)))
          (rx/empty))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAYBACK & PREVIEW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn texts-at-rest
  "`modif-tree`, the preview of the animation, with the text in edition
  at rest (see `cltl/units-at-rest`), and the texts the canvas last posed
  (see `dwm/set-wasm-modifiers`) that it poses no more, as one whose last
  split animation was removed, so that their poses clear."
  [modif-tree state]
  (let [edition (dm/get-in state [:workspace-local :edition])
        posed   (into #{}
                      (keep (fn [[id {:keys [property value]}]]
                              (when (and (= :text-units property) (seq value)) id)))
                      (:wasm-props state))]
    (cltl/units-at-rest modif-tree
                        (cond-> (into #{} (remove #(cltl/poses-units? modif-tree %)) posed)
                          (cltl/poses-units? modif-tree edition)
                          (conj edition)))))

(defn- previewed-timelines
  "The timelines the canvas shows at the playhead (see `shown-timelines`):
  in motion mode, or out of it while a board plays from its title (see
  `toggle-board-play`); none else, the boards showing as they are."
  [state]
  (when (or (motion-mode? state) (dm/get-in state [:workspace-animation :playing?]))
    (not-empty (shown-timelines state))))

(defn apply-preview
  "Recompute and apply the transient modifiers for the timeline of the
  active board at the playhead (editor scrubbing/playback preview), see
  `previewed-timelines`: out of motion mode, but while a board plays,
  the canvas shows the boards as they are, so an edit of a timeline
  there (through the plugin API, an export) poses nothing, which the
  dock would never settle in full quality (see
  `dwm/settle-local-transform`): the canvas would draw no blur or
  shadow until the next gesture. The modif-tree covers transforms AND opacity (as a
  change-property modifier).

  Renderer-aware: with the WASM renderer (`render-wasm/v1`) the modifiers
  are pushed to the WASM canvas via `set-wasm-modifiers` (transforms for
  every shape, the other attributes as shape properties); with the
  classic SVG renderer they go through `set-modifiers` (the transforms
  move the shape nodes, see `use-dynamic-modifiers`, and the shapes paint
  the other attributes, see `cta/appearance-changes`). When the active
  board has no timeline, any stale preview is cleared."
  []
  (ptk/reify ::apply-preview
    ptk/UpdateEvent
    (update [_ state]
      ;; What the canvas shows, for an edit on it to show over it (see
      ;; `dsh/lookup-animation-preview`).
      (if (some? (previewed-timelines state))
        (assoc-in state [:workspace-animation :preview] {:board-id (active-board-id state)
                                                         :time (playhead state)
                                                         :back? (playing-back? state)})
        (update state :workspace-animation dissoc :preview)))

    ptk/WatchEvent
    (watch [_ state _]
      (if-let [timelines (previewed-timelines state)]
        (let [objects    (dsh/lookup-page-objects state)
              modif-tree (-> (cltl/animation-tree timelines objects (playhead state) :back (back-boards state))
                             (texts-at-rest state))]
          ;; The selection frame follows the selected shapes (see the
          ;; viewport), not every shape the preview moves.
          (if ^boolean (features/active-feature? state "render-wasm/v1")
            (rx/of (dwm/set-wasm-modifiers modif-tree
                                           :skip-selrect? true
                                           :animation-preview? true
                                           :full-quality? (full-quality? (:workspace-animation state))))
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
      (let [page-id   (:current-page-id state)
            timelines (shown-timelines state)
            objects   (dsh/lookup-page-objects state)
            preview   (when (and (seq timelines)
                                 (not (dm/get-in state [:workspace-animation :playing?])))
                        (cltl/shown-shapes timelines objects (playhead state) :back (back-boards state)))]
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
      ;; a seek shows the way there; only playback goes back (see
      ;; `show-frame`)
      (update state :workspace-animation assoc :playhead (max 0 (int time)) :direction 1))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (apply-preview)))

    ptk/EffectEvent
    (effect [_ state _]
      (follow-audio! state))))

(defn- selected-elsewhere?
  "Out of motion mode, where the title of a board plays its animation
  (see `toggle-board-play`), whether something else than that board is
  selected: playback stops then, before it gets in the way of editing."
  [state]
  (and (not (motion-mode? state))
       (let [selected (dsh/get-selected-ids state)
             board-id (dm/get-in state [:workspace-animation :board-id])]
         (and (seq selected)
              (not (and (= 1 (count selected))
                        (contains? selected board-id)))))))

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
      (rx/of (apply-preview)))

    ptk/EffectEvent
    (effect [_ state _]
      (follow-audio! state))))

(defn- frame-steps
  "How far playback moves on at each animation frame (see `frame-step`),
  from the frame after the first on, by the time `clock` gives (ms, or nil
  while it has none), clamped or not (`clamp?`)."
  [clock clamp?]
  (rx/create
   (fn [subs]
     (let [frame  (volatile! nil)
           before (volatile! nil)
           carry  (volatile! 0)
           tick   (fn tick [_]
                    (let [now (clock)]
                      (when-let [then (when (some? now) @before)]
                        (let [[step left] (frame-step @carry (- now then) clamp?)]
                          (vreset! carry left)
                          (when (pos? step)
                            (rx/push! subs step))))
                      (vreset! before now)
                      (vreset! frame (ts/raf tick))))]
       (vreset! frame (ts/raf tick))
       #(ts/cancel-af! @frame)))))

(defn- sound-clock?
  "Whether playback of `timeline` keeps time with the audible clock of the
  sound it plays (see `audio/now-ms`), unclamped, so picture and sound
  cannot drift apart: while it has clips, sound is not muted and the
  context runs."
  [state timeline]
  (and (seq (:audio-clips timeline))
       (not (audio-muted? (:workspace-animation state)))
       (some? (audio/now-ms))))

(defn- playback-step
  "Play `dt` ms on from the playhead, with the timeline as it is now: a
  change made while playing (duration, playback mode, keyframes) or a
  move of the playhead shows right away. At the end of a timeline played
  once, pause, and when something else gets selected out of motion mode
  (see `selected-elsewhere?`)."
  [dt]
  (ptk/reify ::playback-step
    ptk/WatchEvent
    (watch [_ state _]
      (let [tl (playback-timeline state)]
        (if (and (some? tl) (not (selected-elsewhere? state)))
          (let [direction (dm/get-in state [:workspace-animation :direction] 1)
                {:keys [time direction ended?]}
                (cta/advance-playback tl (playhead state) direction dt)]
            (if ended?
              (rx/of (show-frame time direction) (pause))
              (rx/of (show-frame time direction))))
          (rx/of (pause)))))))

(defn- playback-ended
  "Out of motion mode, where the title of a board plays its animation
  (see `toggle-board-play`), the board shows as it is again once it
  stops, and plays from the start the next time."
  []
  (ptk/reify ::playback-ended
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (not (motion-mode? state))
        (update :workspace-animation #(-> % (dissoc :preview) (assoc :playhead 0 :direction 1)))))

    ptk/WatchEvent
    (watch [_ state _]
      (if (motion-mode? state)
        (rx/empty)
        (rx/of (dwm/clear-local-transform))))))

(defn play
  []
  (ptk/reify ::play
    ptk/UpdateEvent
    (update [_ state]
      ;; Where it starts from, which the timeline dock shows while it
      ;; plays (see `app.main.ui.workspace.timeline`).
      (let [tl   (playback-timeline state)
            from (playhead state)]
        (update state :workspace-animation assoc
                :playing? true
                :play-start (if (>= from (or (:duration tl) 0)) 0 from))))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [tl       (playback-timeline state)
            duration (or (:duration tl) 0)
            stopper  (rx/filter (ptk/type? ::pause) stream)]
        (if (or (nil? tl) (<= duration 0))
          (rx/of (pause))
          (rx/concat
           ;; Played to the end, it starts over.
           (if (>= (playhead state) duration)
             (rx/of (show-frame 0 1))
             (rx/empty))
           ;; Each frame moves on by the time that passed since the last
           ;; one: a slow frame skips ahead instead of slowing it down.
           (let [sound? (sound-clock? state tl)]
             (->> (frame-steps (if sound? audio/now-ms #(js/performance.now)) (not sound?))
                  (rx/map playback-step)
                  (rx/take-until stopper)))
           (rx/of (playback-ended))))))

    ptk/EffectEvent
    (effect [_ state _]
      (follow-audio! state))))

(defn pause
  []
  (ptk/reify ::pause
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :playing?] false))

    ;; stop and the clearing of the preview pause too
    ptk/EffectEvent
    (effect [_ _ _]
      (audio/stop-all!))))

(defn toggle-play
  []
  (ptk/reify ::toggle-play
    ptk/WatchEvent
    (watch [_ state _]
      (if (dm/get-in state [:workspace-animation :playing?])
        (rx/of (pause))
        (rx/of (play))))))

(defn toggle-board-play
  "Play the animation of the board `board-id` on the canvas, or stop it,
  from the title of the board (see `viewport.widgets/frame-title*`). In
  motion mode it plays and pauses as in the timeline; out of it, without
  the timeline, it plays from the start until it ends, is stopped or
  something else is selected, then the board shows as it is again (see
  `playback-ended`)."
  [board-id]
  (ptk/reify ::toggle-board-play
    ptk/WatchEvent
    (watch [_ state _]
      (cond
        (dm/get-in state [:workspace-animation :playing?])
        (rx/of (pause))

        (motion-mode? state)
        (rx/of (set-active-board board-id) (play))

        :else
        (rx/of (set-active-board board-id) (set-playhead 0) (play))))))

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

(declare dock-timeline)

(defn close-timeline
  "Leave motion mode. The timeline dock clears the preview when it
  unmounts, and opens docked again the next time (see
  `toggle-timeline-maximized`)."
  []
  (ptk/reify ::close-timeline
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dock-timeline)
             (layout/remove-layout-flag :animation-timeline)))))

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

(defn- after-viewport-resize
  "`event` once the canvas has taken its new size (its resize observer
  tells, see `dwv/update-viewport-size`), or after a while without one."
  [stream event]
  (->> stream
       (rx/filter (ptk/type? ::dwv/update-viewport-size))
       (rx/debounce 120)
       (rx/take 1)
       (rx/timeout 1500 (rx/of nil))
       (rx/map (constantly event))))

(defn- fit-view
  "`view` with its vbox as large as the canvas `vport` shows at its zoom,
  around the same centre, as a resized canvas keeps it (see
  `dwv/update-viewport-size`): the canvas need not be as large as when
  the view was kept, as when the timeline was maximized out of motion
  mode, before its dock took its room."
  [{:keys [vbox zoom] :as view} {:keys [width height] :as vport}]
  (if (or (nil? vbox) (nil? vport) (not (pos? zoom)))
    view
    (let [width'  (/ width zoom)
          height' (/ height zoom)]
      (assoc view :vbox (-> vbox
                            (update :x + (/ (- (:width vbox) width') 2))
                            (update :y + (/ (- (:height vbox) height') 2))
                            (assoc :width width' :height height'))))))

(defn- restore-view
  "The canvas back where it was before the timeline was maximized (see
  `toggle-timeline-maximized`), on the page it was on. Maximized again
  before (the canvas still taking its size), the view stays kept for the
  next time it docks."
  []
  (ptk/reify ::restore-view
    ptk/UpdateEvent
    (update [_ state]
      (let [anim (:workspace-animation state)
            view (:restore-view anim)]
        (cond
          (timeline-maximized? anim)
          state

          (not= (:page-id view) (:current-page-id state))
          (update state :workspace-animation dissoc :restore-view)

          :else
          (-> state
              (update :workspace-animation dissoc :restore-view)
              (update :workspace-local #(merge % (fit-view (dissoc view :page-id) (:vport %))))))))

    ptk/EffectEvent
    (effect [_ state _]
      (dwvw/maybe-sync-workspace-local-viewport! state))))

(defn dock-timeline
  "Dock the maximized timeline back (see `toggle-timeline-maximized`), so
  the canvas shows what opens there, back where it was; then the event
  `then`, if any, on the canvas as it is back."
  ([] (dock-timeline nil))
  ([then]
   (ptk/reify ::dock-timeline
     ptk/UpdateEvent
     (update [_ state]
       (update state :workspace-animation dissoc :maximized?))

     ptk/WatchEvent
     (watch [_ state stream]
       (rx/concat
        (if (some? (dm/get-in state [:workspace-animation :restore-view]))
          (after-viewport-resize stream (restore-view))
          (rx/empty))
        (if (some? then) (rx/of then) (rx/empty)))))))

(defn- maximize-timeline
  "Let the timeline dock cover the whole workspace, the canvas in a small
  window over it fitted to the active board, once the window has its
  size; where the canvas was is kept to go back to (see `dock-timeline`).
  Nothing without a board on the page for the timeline to show."
  []
  (ptk/reify ::maximize-timeline
    ptk/UpdateEvent
    (update [_ state]
      (if (nil? (dsh/lookup-shape state (active-board-id state)))
        state
        (-> state
            (assoc-in [:workspace-animation :maximized?] true)
            (update-in [:workspace-animation :restore-view]
                       #(or % (-> (select-keys (:workspace-local state) [:vbox :zoom :zoom-inverse])
                                  (assoc :page-id (:current-page-id state))))))))

    ptk/WatchEvent
    (watch [_ state stream]
      (if (canvas-in-window? state)
        (after-viewport-resize stream (dwz/fit-to-shapes #{(active-board-id state)}))
        (rx/empty)))))

(defn canvas-window
  "Where the canvas window over the maximized timeline is (see
  `app.main.ui.workspace.canvas-window`), as the user last left it; nil
  where it starts."
  [anim]
  (if (contains? anim :canvas-window)
    (:canvas-window anim)
    (get storage/user ::canvas-window)))

(defn set-canvas-window
  "Put the canvas window at `frame` (nil: where it starts), the board the
  timeline shows fitted in it again with `refit`: `:now`, or
  `:after-resize`, once it has its new size. Kept for the next time."
  [frame refit]
  (ptk/reify ::set-canvas-window
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :canvas-window] frame))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [board-id (active-board-id state)]
        (cond
          (or (nil? refit) (nil? board-id)) (rx/empty)
          (= refit :now) (rx/of (dwz/fit-to-shapes #{board-id}))
          :else (after-viewport-resize stream (dwz/fit-to-shapes #{board-id})))))

    ptk/EffectEvent
    (effect [_ _ _]
      (swap! storage/user assoc ::canvas-window frame))))

(defn canvas-window-hidden?
  "Whether the canvas window over the maximized timeline is hidden, as the
  user last left it."
  [anim]
  (if (contains? anim :canvas-window-hidden?)
    (true? (:canvas-window-hidden? anim))
    (true? (get storage/user ::canvas-window-hidden))))

(defn toggle-canvas-window
  "Hide the canvas window over the maximized timeline, or show it again.
  Kept for the next time."
  []
  (ptk/reify ::toggle-canvas-window
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation
              #(assoc % :canvas-window-hidden? (not (canvas-window-hidden? %)))))

    ptk/EffectEvent
    (effect [_ state _]
      (swap! storage/user assoc ::canvas-window-hidden
             (canvas-window-hidden? (:workspace-animation state))))))

(defn toggle-timeline-maximized
  "Let the timeline dock cover the whole workspace, or dock it back. Out
  of motion mode, enter it with the dock maximized."
  []
  (ptk/reify ::toggle-timeline-maximized
    ptk/WatchEvent
    (watch [_ state _]
      (cond
        (not (motion-mode? state))
        (rx/of (toggle-motion-mode) (maximize-timeline))

        (timeline-maximized? (:workspace-animation state))
        (rx/of (dock-timeline))

        :else
        (rx/of (maximize-timeline))))))

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

(defn auto-keyframe?
  [state]
  (dm/get-in state [:workspace-animation :auto-keyframe?] false))

(defn toggle-auto-keyframe
  "Turn auto-keyframe on or off: while it is on, an edit in motion mode
  also records the properties it changes that are not animated yet (see
  `record-canvas-edit`)."
  []
  (ptk/reify ::toggle-auto-keyframe
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:workspace-animation :auto-keyframe?] not))))

(defn- auto-keyframe-ids
  "The shapes an edit records with auto-keyframe on: the selected ones it
  changed in the board of `timeline`. Not the ones it changes along with
  them, like the layers of a moved group or of a resized board, which
  follow the animation of the selected ones."
  [state timeline changes]
  (let [objects  (dsh/lookup-page-objects state)
        board-id (:board-id timeline)
        changed  (set-attrs changes)]
    (filterv #(and (contains? changed %)
                   (or (= % board-id)
                       (= board-id (cfh/get-shape-id-root-frame objects %))))
             (dsh/lookup-selected state))))

(defn- record-canvas-edit
  "Record the edit a commit made on the canvas as keyframes at the
  playhead (see `cta/record-edit`), with auto-keyframe on also in the
  properties it changes that are not animated yet (see
  `cta/record-unanimated`), which gives a board without animation its
  timeline. They join the undo group of the edit, so both undo together.
  The preview shows the shapes as the edit left them either way."
  [{:keys [redo-changes undo-changes undo-group]}]
  (ptk/reify ::record-canvas-edit
    ptk/WatchEvent
    (watch [it state _]
      (let [auto? (auto-keyframe? state)]
        (if-let [tl (if auto? (current-or-new-timeline state) (current-timeline state))]
          (let [objects (dsh/lookup-page-objects state)
                before  (with-attrs objects undo-changes)
                after   (with-attrs objects redo-changes)
                time    (playhead state)
                tl'     (cond-> (cta/record-edit tl before after time)
                          auto? (cta/record-unanimated before after
                                                       (auto-keyframe-ids state tl redo-changes)
                                                       time))]
            (if (= tl tl')
              (rx/of (apply-preview))
              (rx/of (commit-timeline it state (:board-id tl) tl' undo-group)
                     (apply-preview))))
          (rx/empty))))))

(defn- restore-preview
  "Show the animation again once an edit drops its modifiers."
  []
  (ptk/reify ::restore-preview
    ptk/WatchEvent
    (watch [_ state _]
      (if (seq (shown-timelines state))
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
                   (rx/map restore-preview))
              ;; A text in edition shows at rest (see `texts-at-rest`), and
              ;; as the animation poses it once left.
              (->> stream
                   (rx/filter #(contains? #{::dwe/start-edition-mode ::dwe/clear-edition-mode}
                                          (ptk/type %)))
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
      (when-let [tl (export-timeline state :text-split :whole :bake true)]
        (let [objects (dsh/lookup-page-objects state)
              css     (acss/timeline->css tl objects)
              blob    (wapi/create-blob css "text/css")]
          (dom/trigger-download (safe-filename (:name tl) ".css") blob))))))

(defn export-lottie
  "Generate and download a Lottie (bodymovin JSON) file for the current
  timeline. The Lottie map contains no uuids, so it serializes cleanly."
  []
  (ptk/reify ::export-lottie
    ptk/EffectEvent
    (effect [_ state _]
      (when-let [tl (export-timeline state :text-split :whole :bake true)]
        (let [objects (dsh/lookup-page-objects state)
              data    (alottie/timeline->lottie tl objects)
              json    (js/JSON.stringify (clj->js data) nil 2)
              blob    (wapi/create-blob json "application/json")]
          (dom/trigger-download (safe-filename (:name tl) ".json") blob))))))
