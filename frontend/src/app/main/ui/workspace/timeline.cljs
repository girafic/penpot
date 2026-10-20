;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline
  "Bottom timeline dock for the keyframe based animation feature (Penpot
  Motion), laid out like Figma's: the layer tree of the board with a bar
  over each animated layer and a row of keyframes per animated property,
  under a zoomable ruler with a scrubbable playhead."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.color :as clr]
   [app.common.types.component :as ctk]
   [app.common.uuid :as uuid]
   [app.config :as cfg]
   [app.main.data.comments :as dcm]
   [app.main.data.shortcuts :as ds]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.animation-mode :as dwam]
   [app.main.data.workspace.audio :as dwaa]
   [app.main.data.workspace.audio-recording :as dwar]
   [app.main.data.workspace.clipboard :as dwcp]
   [app.main.data.workspace.comments :as dwcm]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.shortcuts :as sc]
   [app.main.data.workspace.undo :as dwu]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.color-input :refer [color-input*]]
   [app.main.ui.components.context-menu-a11y :refer [context-menu*]]
   [app.main.ui.components.editable-label :refer [editable-label*]]
   [app.main.ui.components.numeric-input :as deprecated-input]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.utilities.swatch :refer [swatch*]]
   [app.main.ui.hooks.resize :as r]
   [app.main.ui.workspace.sidebar.options.menus.motion :as motion]
   [app.main.ui.workspace.timeline.audio-lane :refer [use-lane-rename]]
   [app.main.ui.workspace.timeline.easing :refer [easing-editor*]]
   [app.main.ui.workspace.timeline.keys :as tlk]
   [app.main.ui.workspace.timeline.lanes :as lanes]
   [app.main.ui.workspace.timeline.recording :refer [recording-controls* recording-row-index]]
   [app.util.audio :as audio]
   [app.util.dom :as dom]
   [app.util.dom.dnd :as dnd]
   [app.util.dom.normalize-wheel :as nw]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.motion-labels :as labels]
   [app.util.shape-icon :as usi]
   [app.util.theme :as theme]
   [cuerdas.core :as str]
   [goog.events :as events]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:timelines
  (l/derived #(get % :timelines) refs/workspace-page))

(def ^:private ref:dock-animation
  "The animation state the dock shows (see `dwa/dock-state`); what
  follows the playhead as it plays reads `ref:playhead`."
  (l/derived dwa/dock-state refs/workspace-animation =))

(def ^:private ref:playhead
  (l/derived #(get % :playhead 0) refs/workspace-animation))

(def ^:private property-order
  (into {} (map-indexed (fn [i p] [p i]) dwa/animatable-properties)))

(defn- track-slots
  "Distinct `[property index]` slots of `keyframes`, in display order."
  [keyframes]
  (->> keyframes
       (map (juxt :property :index))
       (distinct)
       (sort-by (fn [[property index]]
                  [(get property-order property 99) (or index -1)]))))

(defn- time->pct
  "Where `time` is on an axis of `span` ms from `origin` (0 or before), as
  a share of its width."
  [time span origin]
  (if (pos? span)
    (str (* 100 (/ (double (- time (or origin 0))) span)) "%")
    "0%"))

(def ^:private tick-steps
  "Candidate spacings (ms) between the labelled ticks of the ruler."
  [10 20 25 50 100 200 250 500 1000 2000 2500 5000 10000 30000 60000])

(def ^:private min-tick-spacing
  "Minimum distance (px) between two labelled ticks."
  64)

(defn scroll-view-size
  "The size of the view of the timeline in its scroll, from the content
  box `rect` of the scroll (its scrollbars left out): in whole pixels
  rounded down. `clientWidth` and `clientHeight` round to the nearest
  pixel, so at a browser zoom the view reached a fraction of a pixel past
  the scroll and overflowed it: scrollbars that take room (macOS with a
  mouse) came in, shrank the scroll and went again, over and over, and
  the timeline shook."
  [^js rect]
  {:width (mth/floor (.-width rect))
   :height (mth/floor (.-height rect))})

(def ^:private frame-tick-steps
  "Candidate spacings (frames) between the labelled ticks of the ruler,
  in frames (see `dwa/time-units`)."
  [1 2 5 10 15 30 60 150 300 600 1800 3600 18000])

(defn- tick-step
  "The spacing (ms) between the labelled ticks of the ruler of a
  `duration` drawn `width` pixels wide: whole frames at `fps` in
  frames."
  [duration width unit fps]
  (let [px-per-ms (/ width (max 1 duration))
        steps     (if (= unit :frames)
                    (map #(/ (* % 1000) fps) frame-tick-steps)
                    tick-steps)]
    (or (d/seek #(>= (* % px-per-ms) min-tick-spacing) steps)
        (last steps))))

(defn- format-tick
  "Like Figma: whole seconds as `1s` and, below one-second steps, the
  milliseconds into the current second (`1s 100ms 200ms …`), or in
  seconds (`1s 1.1s 1.2s …`) when those are the time `unit`; in frames,
  the frame at `fps` (`0f 15f 30f …`)."
  [time step unit fps]
  (cond
    (= unit :frames)        (dm/str (mth/round (/ (* time fps) 1000)) "f")
    (zero? (mod time 1000)) (dm/str (quot time 1000) "s")
    (= unit :s)             (dm/str (dwa/format-seconds time) "s")
    (< step 1000)           (dm/str (mod time 1000) "ms")
    :else                   (dm/str (/ time 1000.0) "s")))

(def ^:private min-zoom
  "Smallest horizontal zoom. Below 1 the duration no longer fills the view."
  0.1)

(def ^:private max-zoom
  "Largest horizontal zoom of the time axis (1 fits the duration)."
  20)

(def ^:private row-height-per-pixel
  "How many px taller a pixel of Alt + wheel makes the rows, up the
  wheel; down it, shorter."
  0.05)

(def ^:private zoom-per-pixel
  "How much a pixel of Ctrl/Cmd + wheel zooms, as much as on the canvas
  (see `app.main.ui.workspace.viewport.actions/scale-per-pixel`)."
  0.0057)

;; The zoom slider is logarithmic, so each step zooms by the same factor.
(defn- slider->zoom
  [value]
  (* min-zoom (mth/pow (/ max-zoom min-zoom) (/ value 100))))

(defn- zoom->slider
  [zoom]
  (* 100 (/ (mth/log10 (/ zoom min-zoom))
            (mth/log10 (/ max-zoom min-zoom)))))

(defn content-start
  "The earliest time the dock shows something of `timeline` at, 0 or
  before: a keyframe, the start of a preset, of a style use or of an
  audio clip, which may lie before the start of the timeline, where they
  do not play (see `cta/shift-keyframes`). The time axis starts there."
  [timeline]
  (let [tracks (vals (:tracks timeline))]
    (apply min 0 (concat (map :time (mapcat :keyframes tracks))
                         (map :start (mapcat :animations tracks))
                         (map :start (vals (:audio-clips timeline)))))))

(defn content-end
  "The latest time the dock shows something of `timeline` at: a
  keyframe, the end of an animated copy (`copy-spans`, by copy) or of an
  audio clip, which may run far past the duration and is trimmed at its
  end."
  [timeline copy-spans]
  (apply max 0 (concat (map :time (mapcat :keyframes (vals (:tracks timeline))))
                       (map second (vals copy-spans))
                       (map ctau/clip-end (vals (:audio-clips timeline))))))

(defn- axis-span
  "Milliseconds the ruler covers. The duration fills the view at zoom 1.
  Zooming out lengthens the ruler so time keeps running to the right
  edge, and what is past the duration (see `content-end`) stays on the
  axis, which scrolls to it."
  [duration zoom latest]
  (let [fitted (if (< zoom 1)
                 (/ duration (max zoom 0.0001))
                 duration)]
    (max duration fitted (or latest 0))))

(defn- content-zoom
  "Width factor of the time axis. Zooming in widens it; zooming out
  keeps it full width and spends the extra room on time past the duration."
  [duration zoom span]
  (let [fitted (if (< zoom 1) (/ duration (max zoom 0.0001)) duration)]
    (* (max zoom 1) (/ span fitted))))

(defn- select-on-press
  "Select the layer of a pressed bar or block: on its own or, with Shift,
  added to (or taken out of) the selection, like a click on its label."
  [event shape-id selected]
  (cond
    (kbd/shift? event)
    (st/emit! (dws/select-shape shape-id true))

    (not (contains? selected shape-id))
    (st/emit! (dws/select-shape shape-id false))))

(defn- select-layer
  "Select the layer of a clicked keyframe or property unless it is
  selected already, so the design tab edits that layer. With Shift it is
  added to the selection, never taken out of it: Shift also picks several
  keyframes."
  [event shape-id selected]
  (when-not (contains? selected shape-id)
    (st/emit! (dws/select-shape shape-id (kbd/shift? event)))))

(defn- pointer->time
  "Translate a pointer event over a time axis at the client `rect`, of
  `span` ms from `origin` (0 by default, before when something lies
  there), into a time value (ms) on it."
  ([event rect span]
   (pointer->time event rect span 0))
  ([event rect span origin]
   (let [x        (:x (dom/get-client-position event))
         fraction (/ (- x (:left rect)) (max 1 (:width rect)))
         fraction (-> fraction (max 0.0) (min 1.0))]
     (int (+ origin (* fraction span))))))

(def ^:private c-key? (kbd/is-key-ignore-case? "c"))
(def ^:private x-key? (kbd/is-key-ignore-case? "x"))
(def ^:private m-key? (kbd/is-key-ignore-case? "m"))
(def ^:private s-key? (kbd/is-key-ignore-case? "s"))
(def ^:private b-key? (kbd/is-key-ignore-case? "b"))

(defn nudge-step
  "How far, in ms, an arrow key `event` moves the selected keyframes or
  clip: 10 ms, 100 ms with Shift, back with the left arrow. In frames
  (`unit`, see `dwa/time-units`), a frame or 10 at `fps`, counted from
  the frame nearest to `start`, where the selection starts, so that it
  lands on frames although a frame lasts no whole number of ms."
  ([event]
   (nudge-step event :ms nil nil))
  ([event unit fps start]
   (let [sign (if (kbd/left-arrow? event) -1 1)]
     (if (and (= unit :frames) (some? start))
       (let [frames (* sign (if (kbd/shift? event) 10 1))
             frame  (mth/round (/ (* start fps) 1000))]
         (- (dwa/frames->time (+ frame frames) fps) start))
       (* sign (if (kbd/shift? event) 100 10))))))

(defn- selection-start
  "Where `selection` (see `dwaa/selection-of`) starts in `timeline`: its
  earliest keyframe, animation or clip; nil when it holds none."
  [timeline {:keys [kfs anims clips]}]
  (let [times (concat
               (keep (fn [{:keys [shape-id keyframe-id]}]
                       (:time (d/seek #(= keyframe-id (:id %))
                                      (dm/get-in timeline [:tracks shape-id :keyframes]))))
                     kfs)
               (keep (fn [{:keys [shape-id animation-id]}]
                       (:start (d/seek #(= animation-id (:id %))
                                       (dm/get-in timeline [:tracks shape-id :animations]))))
                     anims)
               (keep #(:start (ctau/get-audio-clip timeline %)) clips))]
    (when (seq times)
      (reduce min times))))

(def property-keys
  "The keys that show only one kind of property in the focused dock, as
  in After Effects: Position (P), Scale (S), Rotation (R) and Opacity
  (T), with the properties each shows; U shows all of them again (see
  `shown-properties`)."
  {"p" #{:x :y}
   "s" #{:scale-x :scale-y}
   "r" #{:rotation}
   "t" #{:opacity}})

(defn- property-key
  "The key of `property-keys` (or \"u\") the key `event` presses, nil for
  another."
  [event]
  (let [k (str/lower (str (.-key ^js event)))]
    (when (or (contains? property-keys k) (= "u" k))
      k)))

(defn shown-properties
  "The properties the dock shows (nil for all) after the key `k` of
  `property-keys` (\"u\": all of them) when it shows `shown`: only those
  of the key, or all again when they were the only ones; with Shift
  (`add?`), those of the key added to the ones shown, or taken out when
  shown already."
  [shown k add?]
  (let [props (get property-keys k)]
    (cond
      (nil? props)                  nil
      (not add?)                    (when-not (= shown props) props)
      (every? (or shown #{}) props) (not-empty (reduce disj shown props))
      :else                         (into (or shown #{}) props))))

(defn- property-names
  "The names of the kinds of property of `property-keys` among the
  properties `props`, as the dock lists the ones it shows."
  [props]
  (cond-> []
    (every? props (get property-keys "p")) (conj (tr "workspace.animation.position"))
    (every? props (get property-keys "s")) (conj (tr "workspace.animation.scale"))
    (every? props (get property-keys "r")) (conj (tr "workspace.options.rotation"))
    (every? props (get property-keys "t")) (conj (tr "workspace.options.opacity"))))

(defn dock-key-action
  "What a key `event` does in the focused dock (but in a text field, which
  keeps its keys), by what is selected there: `:add-marker` (M),
  `:jump-to-marker` (Alt with an arrow) or `:split-all-clips` (Shift +
  Cmd/Ctrl + B, the clips under the playhead, as Final Cut's Blade All);
  with an audio clip selected (`selected-clip`), `:split-clip` (Cmd/Ctrl
  + B, as Final Cut, DaVinci Resolve or CapCut, or S), `:delete-clip`
  (Delete, Backspace) or `:nudge-clip` (the arrows, see `nudge-step`);
  `:copy-selection` or `:cut-selection` (Cmd/Ctrl + C or X) for the
  keyframes `selected-kfs` and the clips, together; else
  `:delete-keyframes` or `:shift-keyframes` (the arrows) for the
  keyframes; `:show-properties` for the keys of After Effects (P, S, R,
  T and U, with Shift too, see `shown-properties`). Nil for a key the
  dock leaves to the workspace."
  [event selected-kfs selected-clip & [selected-anims]]
  (let [plain?  (not (or (kbd/mod? event) (kbd/alt? event) (kbd/shift? event)))
        delete? (or (kbd/delete? event) (kbd/backspace? event))
        copy?   (and (kbd/mod? event) (or (c-key? event) (x-key? event)))
        arrow?  (or (kbd/left-arrow? event) (kbd/right-arrow? event))
        blade?  (and (kbd/mod? event) (not (kbd/alt? event)) (b-key? event))]
    (cond
      (and plain? (m-key? event))                 :add-marker
      (and (kbd/alt? event) arrow?)               :jump-to-marker
      (and blade? (kbd/shift? event))             :split-all-clips
      (and (some? selected-clip) blade?)          :split-clip
      (and (some? selected-clip) plain? (s-key? event)) :split-clip
      (and (some? selected-clip) delete?)         :delete-clip
      (and (some? selected-clip) arrow?)          :nudge-clip
      (and copy? (or (seq selected-kfs) (some? selected-clip)))
      (if (x-key? event) :cut-selection :copy-selection)
      (and (seq selected-anims) delete?)          :delete-clip
      (and (seq selected-anims) arrow?)           :nudge-clip
      (and (not (kbd/mod? event)) (not (kbd/alt? event)) (some? (property-key event)))
      :show-properties
      delete?                                     :delete-keyframes
      (and (seq selected-kfs) arrow?)             :shift-keyframes)))

(def ^:private marquee-threshold
  "Pixels the pointer must move before a lane click becomes a box select."
  4)

(def ^:private drag-threshold
  "Pixels the pointer must move before a pressed keyframe is dragged."
  3)

(def ^:private snap-distance
  "Pixels within which a drag in the timeline snaps (see `snap-target`)."
  6)

(def ^:private row-overscan
  "Rows whose labels are rendered past the ones in view, so the next ones
  are there as the rows scroll."
  8)

(def ^:private snap-context
  "What a drag in a lane snaps with, `{:timeline :target :show}`, and the
  `:unit` times show in, see `timeline*`."
  (mf/create-context nil))

(defn rect-from-points
  "Axis-aligned rect covering two pointer positions."
  [x0 y0 x1 y1]
  {:left   (min x0 x1)
   :top    (min y0 y1)
   :right  (max x0 x1)
   :bottom (max y0 y1)})

(defn rects-intersect?
  [{a-left :left a-top :top a-right :right a-bottom :bottom}
   {b-left :left b-top :top b-right :right b-bottom :bottom}]
  (and (<= a-left b-right)
       (<= b-left a-right)
       (<= a-top b-bottom)
       (<= b-top a-bottom)))

(defn keyframes-in-rect
  "Keyframe refs whose boxes overlap `marquee`."
  [hits marquee]
  (into #{}
        (comp (filter #(rects-intersect? % marquee))
              (map #(select-keys % [:shape-id :keyframe-id])))
        hits))

(defn track-items
  "The keyframes and preset animations of the layer `shape-id` and the
  layers in it, as the bar of its track holds them: `{:kfs :anims}` (see
  `dwaa/selection-of`)."
  [timeline objects shape-id]
  (let [ids (cfh/get-children-ids-with-self objects shape-id)]
    {:kfs   (into #{} (mapcat (fn [id]
                                (map #(hash-map :shape-id id :keyframe-id (:id %))
                                     (dm/get-in timeline [:tracks id :keyframes]))))
                  ids)
     :anims (into #{} (mapcat (fn [id]
                                (map #(hash-map :shape-id id :animation-id (:id %))
                                     (dm/get-in timeline [:tracks id :animations]))))
                  ids)}))

(defn- selected-tracks
  "Ids of the layers of the `rows` whose track is all selected (see
  `track-items`): their bars show it."
  [timeline objects rows {:keys [kfs anims]}]
  (if (and (empty? kfs) (empty? anims))
    #{}
    (into #{}
          (comp (filter #(= :layer (:type %)))
                (map :id)
                (filter (fn [id]
                          (let [items (track-items timeline objects id)]
                            (and (pos? (dwaa/selection-count items))
                                 (every? kfs (:kfs items))
                                 (every? anims (:anims items)))))))
          rows)))

(defn items-in-rect
  "What a box `marquee` picks, `{:kfs :anims :clips}` (see
  `dwaa/selection-of`), of the `hits` of each kind: the keyframes, the
  blocks of the preset animations and the clips it touches, and all of a
  track whose bar it touches (`items-of`, of the layer)."
  [{:keys [kfs anims clips bars]} marquee items-of]
  (let [touched (fn [boxes] (filter #(rects-intersect? % marquee) boxes))
        tracks  (map #(items-of (:shape-id %)) (touched bars))]
    {:kfs   (into (into #{} (map #(select-keys % [:shape-id :keyframe-id])) (touched kfs))
                  (mapcat :kfs) tracks)
     :anims (into (into #{} (map #(select-keys % [:shape-id :animation-id])) (touched anims))
                  (mapcat :anims) tracks)
     :clips (into #{} (map :clip-id) (touched clips))}))

(defn- whole-selection?
  "Whether the keys act on all of `selection` together (see
  `dwaa/selection-of`): when it holds more than one kind, or blocks."
  [{:keys [kfs anims clips]}]
  (or (seq anims) (< 1 (count (filter seq [kfs clips])))))

(defn- merge-selection
  [a b]
  (merge-with into a b))

(defn clips-in-rect
  "Ids of the clips whose boxes overlap `marquee` (see `lanes/clip-boxes`)."
  [hits marquee]
  (into #{} (comp (filter #(rects-intersect? % marquee)) (map :clip-id)) hits))

(defn marquee-drag?
  "True once the pointer has moved far enough to count as a box, not a click."
  [x0 y0 x y]
  (>= (max (mth/abs (- x x0)) (mth/abs (- y y0))) marquee-threshold))

(defn- release-pointer
  "Let go of the pointer of `event` if its target still holds it."
  [^js event]
  (let [node (dom/get-target event)
        id   (.-pointerId event)]
    (when (and (some? id) (.hasPointerCapture ^js node id))
      (.releasePointerCapture ^js node id))))

(defn- hover-key
  "What of a `lanes/hit` shows under the pointer: the lanes draw again
  when it changes."
  [target]
  [(:type target) (:row target) (:mode target)
   (:id (:keyframe target)) (:id (:from target)) (:id (:clip target))])

(defn- lanes-cursor
  [{:keys [type mode locked? animation clip]}]
  (case type
    (:ruler :duration)       "ew-resize"
    :bar                     (if (= :move mode) "grab" "ew-resize")
    :animation               (cond (:locked animation) "default"
                                   (= :move mode)      "grab"
                                   :else               "ew-resize")
    :audio                   (cond (:locked clip)    "default"
                                   (= :move mode)    "grab"
                                   (= :volume mode)  "ns-resize"
                                   :else             "ew-resize")
    (:keyframe :easing)      (if locked? "default" "pointer")
    :copy                    "grab"
    (:segment :lane :row)    "crosshair"
    "default"))

(defn- animated-ids
  "Ids of the layers of `board-id` (itself included) that have keyframes,
  preset animations, a leader they follow (see `cta/follow-of`), links
  (see `cta/links-of`) or
  animated children, or that are component copies playing the animation
  of their mains (`copy-spans`, see `timeline-rows`)."
  ([objects board-id timeline]
   (animated-ids objects board-id timeline {}))
  ([objects board-id timeline copy-spans]
   (letfn [(walk [id]
             (let [child-ids (mapcat walk (dm/get-in objects [id :shapes]))]
               (if (or (seq (dm/get-in timeline [:tracks id :keyframes]))
                       (seq (dm/get-in timeline [:tracks id :animations]))
                       (some? (dm/get-in timeline [:tracks id :follow]))
                       (seq (dm/get-in timeline [:tracks id :links]))
                       (contains? copy-spans id)
                       (seq child-ids))
                 (cons id child-ids)
                 child-ids)))]
     (set (walk board-id)))))

(defn- outside-ids
  "Ids of the layers out of the board `board-id` that its `timeline`
  animates, the top-most ones, in the order of the layers panel: a layer
  dragged out of every board keeps its track there for when it comes
  back (see `cltl/generate-move-timelines`), and plays with the board on
  the canvas."
  [objects board-id timeline]
  (let [ids (into #{}
                  (filter (fn [id]
                            (and (not= id board-id)
                                 (contains? objects id)
                                 (not (some #(= board-id %) (cfh/get-parent-ids objects id)))
                                 (or (seq (dm/get-in timeline [:tracks id :keyframes]))
                                     (seq (dm/get-in timeline [:tracks id :animations]))))))
                  (keys (:tracks timeline)))]
    (when (seq ids)
      (letfn [(walk [id]
                (if (contains? ids id)
                  [id]
                  (mapcat walk (reverse (dm/get-in objects [id :shapes])))))]
        (vec (walk uuid/zero))))))

(defn timeline-rows
  "Flatten the layers of `board-id` (top-most first, like the layers
  panel) into rows: one per layer followed, while it is expanded, by one
  per preset animation, one per animated property and its children. Only
  the layers that animate show, and the ones holding them (see
  `animated-ids`), under the row of the board, which always does: a layer
  gets its row with its first animation. Animated layers start expanded;
  `expanded` holds the ones the user toggled, or is `:all` to open them
  all (see `filter-rows`). A component copy playing
  the animation of its main, over the span `copy-spans` gives for it (see
  `cta/copy-clock`), has a bar over it, `:copy`, and keeps its layers folded away
  unless the board animates them. The layers out of the board that its
  timeline animates (see `outside-ids`) follow, marked `:outside?`."
  [objects board-id timeline expanded copy-spans]
  (let [outside  (outside-ids objects board-id timeline)
        animated (into (animated-ids objects board-id timeline)
                       (mapcat #(animated-ids objects % timeline))
                       outside)
        playing  (into (animated-ids objects board-id timeline copy-spans)
                       (mapcat #(animated-ids objects % timeline copy-spans))
                       outside)]
    (letfn [(walk [id depth]
              (when-let [shape (get objects id)]
                (let [copy-span  (get copy-spans id)
                      ;; A copy shows its own animation, in the board,
                      ;; unfolded; the one of its main, as a bar.
                      folded?    (and (some? copy-span) (not (contains? animated id)))
                      children   (when-not folded?
                                   (filter #(contains? playing %) (reverse (:shapes shape))))
                      animations (dm/get-in timeline [:tracks id :animations])
                      slots      (track-slots (dm/get-in timeline [:tracks id :keyframes]))
                      expanded?  (and (not folded?)
                                      (or (= :all expanded)
                                          (get expanded id (contains? playing id))))]
                  (cons {:type :layer
                         :id id
                         :depth depth
                         :shape shape
                         :expandable? (boolean (or (seq children) (seq animations) (seq slots)))
                         :expanded? expanded?
                         :copy copy-span
                         :range (when (contains? animated id)
                                  (cta/keyframes-range timeline (cons id (cfh/get-children-ids objects id))))}
                        (when expanded?
                          (concat
                           (for [animation animations]
                             {:type :animation :id id :animation animation :depth (inc depth)})
                           (for [[property index] slots]
                             {:type :property :id id :property property :index index :depth (inc depth)})
                           (mapcat #(walk % (inc depth)) children)))))))]
      (into (vec (walk board-id 0))
            (mapcat (fn [id]
                      (let [[row & rows] (walk id 1)]
                        (cons (assoc row :outside? true) rows))))
            outside))))

(defn- row-label
  "What the row `row` of `timeline-rows` is named in the dock."
  [{:keys [type shape property index animation]}]
  (case type
    :layer     (:name shape)
    :property  (labels/property-label property index)
    :animation (labels/animation-label animation)
    nil))

(defn filter-rows
  "The `rows` (see `timeline-rows`, all layers open) named by every word
  of `query`, ignoring case, with the names of the layers holding them
  (\"pos\" or \"pos x\" for Position X, \"dot pos\" for the position of
  the layer Dot), and the layers holding them, as the search of Blender's
  dope sheet: properties, preset animations, or layers, which keep all
  their rows. With `properties` (a set, see `shown-properties`), only
  the rows of those properties, as the keys of After Effects show them.
  The board, the first row, and the rows of the audio lanes always stay."
  ([rows query]
   (filter-rows rows query nil))
  ([rows query properties]
   (let [words   (into [] (remove str/empty?) (str/split (str/lower (str/trim query)) #"\s+"))
         match?  (fn [row stack]
                   (let [text (str/lower (str/join " " (keep row-label (conj (mapv :row stack) row))))]
                     (and (every? #(str/includes? text %) words)
                          (or (nil? properties) (contains? properties (:property row))))))
         layered #{:layer :property :animation}]
     (if (and (empty? words) (nil? properties))
       rows
       (loop [rows rows, stack [], out []]
         (if-let [{:keys [type depth] :as row} (first rows)]
           (if-not (contains? layered type)
             (recur (rest rows) stack (conj out row))
             ;; the open layers above, each with whether it matched and
             ;; whether it is out yet
             (let [stack    (filterv #(< (:depth (:row %)) depth) stack)
                   matched? (or (zero? depth) (match? row stack) (some :matched? stack))]
               (if (= :layer type)
                 (let [entry {:row row :matched? (and (pos? depth) (match? row stack))}]
                   (if (or (zero? depth) (:matched? entry))
                     (recur (rest rows)
                            (conj (mapv #(assoc % :out? true) stack) (assoc entry :out? true))
                            (into out (comp (remove :out?) (map :row)) (conj stack entry)))
                     (recur (rest rows) (conj stack entry) out)))
                 (if matched?
                   (recur (rest rows)
                          (mapv #(assoc % :out? true) stack)
                          (conj (into out (comp (remove :out?) (map :row)) stack) row))
                   (recur (rest rows) stack out)))))
           out))))))

(defn expand-to
  "`expanded` (see `timeline-rows`) with the closed parents of the layers
  `ids` opened up to `board-id`, so that their rows show. The same map
  when they are open already; layers out of the board are left out."
  [expanded objects board-id timeline ids]
  (let [animated (animated-ids objects board-id timeline)]
    (reduce (fn [expanded id]
              (let [parents (cfh/get-parent-ids objects id)]
                (if (some #(= board-id %) parents)
                  (reduce (fn [expanded parent]
                            (cond-> expanded
                              (not (get expanded parent (contains? animated parent)))
                              (assoc parent true)))
                          expanded
                          (concat (take-while #(not= board-id %) parents) [board-id]))
                  expanded)))
            expanded
            ids)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUB COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mf/defc row-controls*
  "Jump to the previous / next keyframe of the property, and toggle its
  keyframe at the playhead (unless it is locked)."
  {::mf/private true}
  [{:keys [timeline shape-id property index playhead locked?]}]
  (let [properties (mf/with-memo [property] [property])
        times      (dwa/keyframe-times timeline shape-id properties index)
        prev-time  (last (filter #(< % playhead) times))
        next-time  (d/seek #(> % playhead) times)
        active?    (some? (dwa/keyframe-at timeline shape-id property playhead index))

        on-prev
        (mf/use-fn
         (mf/deps prev-time)
         #(st/emit! (dwa/set-playhead prev-time)))

        on-next
        (mf/use-fn
         (mf/deps next-time)
         #(st/emit! (dwa/set-playhead next-time)))

        on-toggle
        (mf/use-fn
         (mf/deps shape-id properties index)
         #(st/emit! (dwa/toggle-keyframes shape-id properties index)))]

    [:div {:class (stl/css :row-controls)}
     [:button {:class (stl/css :nav-btn)
               :title (tr "workspace.animation.prev-keyframe")
               :disabled (nil? prev-time)
               :on-click on-prev}
      [:> i/icon* {:icon-id i/arrow-left :size "s"}]]
     [:button {:class (stl/css-case :diamond-btn true :active active?)
               :title (if active?
                        (tr "workspace.animation.remove-keyframe")
                        (tr "workspace.animation.add-keyframe"))
               :aria-pressed active?
               :disabled locked?
               :on-click on-toggle}
      [:span {:class (stl/css :diamond)}]]
     [:button {:class (stl/css :nav-btn)
               :title (tr "workspace.animation.next-keyframe")
               :disabled (nil? next-time)
               :on-click on-next}
      [:> i/icon* {:icon-id i/arrow-right :size "s"}]]]))

(defn- position-offsets
  "`{shape-id {:x offset :y offset}}`: how much the animated positions
  are ahead of X and Y as the design tab shows them at `time`. The design
  tab gives the top left corner of the box around the shape in its board;
  a keyframe keeps the position of the shape itself, which turning it
  leaves alone, so it moves on a straight line while it turns."
  [timeline objects time values]
  (into {}
        (keep (fn [[shape-id {:keys [x y]}]]
                (when-let [shape (and (or (some? x) (some? y)) (get objects shape-id))]
                  (let [shown (dwa/shape-at timeline objects shape time)
                        box   (-> (gsh/translate-to-frame shown (get objects (:frame-id shape)))
                                  (get :points)
                                  (grc/points->rect))]
                    [shape-id {:x (some-> x (- (:x box)))
                               :y (some-> y (- (:y box)))}]))))
        values))

(defn outline-nodes
  "How many nodes the Path row shows: of the outline of `keyframe` when
  the playhead is on one, else of `value`, the outline shown (matched to
  the others, see `ctao/matched-outlines`)."
  [keyframe value]
  (let [outline (if (some? keyframe) (:value keyframe) value)]
    (if (some? outline)
      (count (remove #(= :close-path (:command %)) outline))
      0)))

(mf/defc property-value*
  "The value of the property at the playhead, editable on a keyframe
  placed there unless it is locked. A position shows `offset` less (see
  `position-offsets`)."
  {::mf/private true}
  [{:keys [shape-id property value offset keyframe locked?]}]
  (let [kf-id     (:id keyframe)
        disabled? (or locked? (nil? keyframe))
        hint      (when (and (nil? keyframe) (not locked?))
                    (tr "workspace.animation.value-needs-keyframe"))
        offset    (or offset 0)

        on-number
        (mf/use-fn
         (mf/deps shape-id kf-id property offset)
         (fn [display]
           (st/emit! (dwa/set-keyframe-value shape-id kf-id (+ offset (dwa/display->value property display))))))

        on-color
        (mf/use-fn
         (mf/deps shape-id kf-id)
         (fn [hex]
           (when (some? keyframe)
             (st/emit! (dwa/set-keyframe-value shape-id kf-id hex)))))]

    (cond
      ;; an outline, which the canvas edits (see `dwa/outline-edit`)
      (= :content property)
      [:span {:class (stl/css :row-value :row-outline)}
       (tr "workspace.animation.path-nodes" (outline-nodes keyframe value))]

      (cta/color-property? property)
      [:div {:class (stl/css-case :row-color true :disabled disabled?)
             :title hint}
       [:> swatch* {:background {:color (or value "#000000") :opacity 1}
                    :size "small"}]
       [:> color-input* {:value (clr/remove-hash (or value "#000000"))
                         :class (stl/css :row-value)
                         :disabled disabled?
                         :on-change on-color}]]

      :else
      [:> deprecated-input/numeric-input* {:class (stl/css :row-value)
                                           :value (some-> value (- offset) (->> (dwa/value->display property)))
                                           :is-disabled disabled?
                                           :title hint
                                           :on-change on-number}])))

(mf/defc row-toggles*
  "Show / hide and lock / unlock buttons of a row, like the ones of the
  layers panel; the row of an audio lane gives the icon of its sound
  (`visibility-icon`) and the titles of both."
  {::mf/private true}
  [{:keys [hidden? locked? on-toggle-visibility on-toggle-lock visibility-icon visibility-title lock-title]}]
  (let [visibility (or visibility-title
                       (if hidden? (tr "workspace.shape.menu.show") (tr "workspace.shape.menu.hide")))
        lock       (or lock-title
                       (if locked? (tr "workspace.shape.menu.unlock") (tr "workspace.shape.menu.lock")))]
    ;; Clicks on them do not reach the row label, which selects the layer.
    [:div {:class (stl/css-case :row-toggles true :has-state (or hidden? locked?))
           :on-click dom/stop-propagation}
     [:button {:type "button"
               :class (stl/css-case :row-toggle true :active hidden?)
               :title visibility
               :aria-label visibility
               :aria-pressed hidden?
               :on-click on-toggle-visibility}
      [:> i/icon* {:icon-id (or visibility-icon (if hidden? i/hide i/shown)) :size "s"}]]
     [:button {:type "button"
               :class (stl/css-case :row-toggle true :active locked?)
               :title lock
               :aria-label lock
               :aria-pressed locked?
               :on-click on-toggle-lock}
      [:> i/icon* {:icon-id (if locked? i/lock i/unlock) :size "s"}]]]))

(defn- use-row-hover
  "Pointer enter and leave handlers of the label of the row at `row-index`,
  telling `on-hover` whether it is under the pointer: its lane shows it
  too."
  [row-index on-hover]
  [(mf/use-fn (mf/deps row-index on-hover) #(on-hover row-index))
   (mf/use-fn (mf/deps on-hover) #(on-hover nil))])

(defn- row-style
  "Where the label of the row at `row-index` is, among the labels."
  [row-index]
  #js {"top" (dm/str "calc(var(--timeline-row-height) * " row-index ")")})

(mf/defc layer-row*
  "The label of a layer of the board, in a tree like the layers panel. Its
  lane (see `lanes/draw!`) has, when it is animated, a bar over the span of
  its keyframes and its children's."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [row-index shape depth expandable? expanded? selected? hovered? outside?
           leader-id leader-name goal-id goal-name link link-name
           on-toggle on-select on-context-menu on-hover]}]
  (let [shape-id (:id shape)
        ;; in the colour of components, as in the layers panel
        component? (ctk/instance-head? shape)
        hidden?  (true? (:hidden shape))
        blocked? (true? (:blocked shape))

        [on-pointer-enter on-pointer-leave] (use-row-hover row-index on-hover)

        on-toggle-click
        (mf/use-fn
         (mf/deps shape-id expanded? on-toggle)
         (fn [event]
           (dom/stop-propagation event)
           (on-toggle shape-id expanded?)))

        ;; Like the layers panel: show or hide, lock or unlock (a locked
        ;; layer leaves the selection).
        on-toggle-visibility
        (mf/use-fn
         (mf/deps shape-id hidden?)
         #(st/emit! (dwsh/update-shape-flags [shape-id] {:hidden (not hidden?)})))

        on-toggle-blocking
        (mf/use-fn
         (mf/deps shape-id blocked?)
         #(if blocked?
            (st/emit! (dwsh/update-shape-flags [shape-id] {:blocked false}))
            (st/emit! (dwsh/update-shape-flags [shape-id] {:blocked true})
                      (dws/deselect-shape shape-id))))

        on-label-click
        (mf/use-fn
         (mf/deps shape-id on-select)
         (fn [event]
           (on-select event shape-id)))

        on-row-context-menu
        (mf/use-fn
         (mf/deps shape-id on-context-menu)
         (fn [event]
           (on-context-menu event shape-id nil)))

        on-leader-click
        (mf/use-fn
         (mf/deps leader-id)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dws/select-shape leader-id))))

        on-goal-click
        (mf/use-fn
         (mf/deps goal-id)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dws/select-shape goal-id))))

        link-source (:source link)
        on-link-click
        (mf/use-fn
         (mf/deps link-source)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dws/select-shape link-source))))]

    [:div {:class (stl/css-case :row true
                                :layer-row true
                                :component-row component?
                                :selected selected?
                                :hovered hovered?
                                :hidden-layer hidden?)
           :style (row-style row-index)
           :on-pointer-enter on-pointer-enter
           :on-pointer-leave on-pointer-leave
           :on-context-menu on-row-context-menu}
     [:div {:class (stl/css :row-label)
            :style #js {"paddingInlineStart" (dm/str (* depth 12) "px")}
            :on-click on-label-click}
      [:button {:class (stl/css :chevron)
                :disabled (not expandable?)
                :on-click on-toggle-click}
       (when expandable?
         [:> i/icon* {:icon-id (if expanded? i/arrow-down i/arrow-right) :size "s"}])]
      [:> i/icon* {:icon-id (usi/get-shape-icon shape)
                   :size "s"
                   :class (stl/css :layer-icon)}]
      [:span {:class (stl/css :layer-name) :title (:name shape)} (:name shape)]
      ;; the layer it reaches (see `cta/ik-of`), or else follows (see
      ;; `cta/follow-of`), selected by a click
      (cond
        (some? goal-id)
        [:button {:type "button"
                  :class (stl/css :follow-chip)
                  :title (tr "workspace.animation.reaches-hint")
                  :on-click on-goal-click}
         (tr "workspace.animation.reaches" goal-name)]

        (some? leader-id)
        [:button {:type "button"
                  :class (stl/css :follow-chip)
                  :title (tr "workspace.animation.follows-hint")
                  :on-click on-leader-click}
         (tr "workspace.animation.follows" leader-name)]

        ;; its first link (see `cta/links-of`), its source selected by a click
        (some? link)
        [:button {:type "button"
                  :class (stl/css :follow-chip)
                  :title (tr "workspace.animation.links-hint")
                  :on-click on-link-click}
         (tr "workspace.animation.linked"
             (labels/property-label (:property link) nil)
             link-name
             (labels/property-label (:source-property link) nil))])
      ;; Out of the board, which still animates it (see `outside-ids`).
      (when outside?
        [:span {:class (stl/css :outside-hint)
                :title (tr "workspace.animation.outside-board-hint")}
         (tr "workspace.animation.outside-board")])
      [:> row-toggles* {:hidden? hidden?
                        :locked? blocked?
                        :on-toggle-visibility on-toggle-visibility
                        :on-toggle-lock on-toggle-blocking}]]]))

(mf/defc animation-row*
  "The label of a preset animation of a layer; its lane has a block over
  its span. A hidden one is left out of playback and exports; a locked one
  is kept from edits."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [row-index shape-id animation depth hovered?
           on-select on-context-menu on-hover]}]
  (let [animation-id (:id animation)
        hidden?      (true? (:hidden animation))
        locked?      (true? (:locked animation))

        [on-pointer-enter on-pointer-leave] (use-row-hover row-index on-hover)

        on-toggle-visibility
        (mf/use-fn
         (mf/deps shape-id animation-id)
         #(st/emit! (dwa/toggle-animation-flag shape-id animation-id :hidden)))

        on-toggle-lock
        (mf/use-fn
         (mf/deps shape-id animation-id)
         #(st/emit! (dwa/toggle-animation-flag shape-id animation-id :locked)))

        on-label-click
        (mf/use-fn
         (mf/deps shape-id on-select)
         (fn [event]
           (on-select event shape-id)))

        on-row-context-menu
        (mf/use-fn
         (mf/deps shape-id animation-id on-context-menu)
         (fn [event]
           (on-context-menu event shape-id nil nil animation-id)))]

    [:div {:class (stl/css-case :row true :animation-row true :hovered hovered? :muted hidden?)
           :style (row-style row-index)
           :on-pointer-enter on-pointer-enter
           :on-pointer-leave on-pointer-leave
           :on-context-menu on-row-context-menu}
     [:div {:class (stl/css :row-label)
            :style #js {"paddingInlineStart" (dm/str (* depth 12) "px")}
            :on-click on-label-click}
      [:span {:class (stl/css :tree-line)}]
      [:span {:class (stl/css :property-name)} (motion/animation-label animation)]
      [:> row-toggles* {:hidden? hidden?
                        :locked? locked?
                        :on-toggle-visibility on-toggle-visibility
                        :on-toggle-lock on-toggle-lock}]]]))

(mf/defc audio-group-row*
  "The row of the group of the audio lanes, like a folder of the layers
  panel: a click shows or hides their rows (collapsed, its lane shows all
  their clips), and its buttons mute or lock all the clips."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [row-index lanes collapsed? muted? locked? hovered? on-hover]}]
  (let [[on-pointer-enter on-pointer-leave] (use-row-hover row-index on-hover)

        on-toggle
        (mf/use-fn #(st/emit! (dwaa/toggle-audio-collapsed)))

        on-toggle-muted
        (mf/use-fn
         (mf/deps muted?)
         #(st/emit! (dwaa/set-all-muted (not muted?))))

        on-toggle-lock
        (mf/use-fn
         (mf/deps locked?)
         #(st/emit! (dwaa/set-all-locked (not locked?))))]

    [:div {:class (stl/css-case :row true :audio-group-row true :hovered hovered? :muted muted?)
           :style (row-style row-index)
           :on-pointer-enter on-pointer-enter
           :on-pointer-leave on-pointer-leave}
     [:div {:class (stl/css :row-label)
            :on-click on-toggle}
      [:button {:type "button"
                :class (stl/css :chevron)
                :aria-expanded (not collapsed?)
                :aria-label (if collapsed?
                              (tr "workspace.animation.expand-all")
                              (tr "workspace.animation.collapse-all"))}
       [:> i/icon* {:icon-id (if collapsed? i/arrow-right i/arrow-down) :size "s"}]]
      [:span {:class (stl/css :audio-icon)}
       [:> i/icon* {:icon-id i/volume :size "s"}]]
      [:span {:class (stl/css :layer-name)}
       (tr "workspace.animation.audio-group" lanes)]
      [:> row-toggles* {:hidden? muted?
                        :locked? locked?
                        :visibility-icon (if muted? i/volume-off i/volume)
                        :visibility-title (if muted?
                                            (tr "workspace.animation.audio-unmute-all")
                                            (tr "workspace.animation.audio-mute-all"))
                        :lock-title (if locked?
                                      (tr "workspace.animation.audio-unlock-all")
                                      (tr "workspace.animation.audio-lock-all"))
                        :on-toggle-visibility on-toggle-muted
                        :on-toggle-lock on-toggle-lock}]]]))

(mf/defc audio-row*
  "The label of a lane of audio clips (see `dwaa/audio-rows`), the name of
  the file it plays (a double click renames it), which a drag up or down
  puts at another place among the lanes (`on-drag-down`), with the
  buttons of a layer row: its sound, which mutes all its clips or plays
  them (`muted?` when all are muted), and its lock (`locked?` when all
  are locked). A drop of audio files on it adds them on this lane."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [row-index lane number name muted? locked? hovered? on-hover on-drag-down]}]
  (let [[on-pointer-enter on-pointer-leave] (use-row-hover row-index on-hover)
        label (if (str/blank? name) (tr "workspace.animation.audio-lane" number) name)
        [renaming? on-start-rename on-cancel-rename on-rename] (use-lane-rename lane)

        on-pointer-down
        (mf/use-fn
         (mf/deps lane on-drag-down)
         (fn [event]
           (on-drag-down event lane)))

        on-toggle-muted
        (mf/use-fn
         (mf/deps lane muted?)
         #(st/emit! (dwaa/set-lane-muted lane (not muted?))))

        on-toggle-lock
        (mf/use-fn
         (mf/deps lane locked?)
         #(st/emit! (dwaa/set-lane-locked lane (not locked?))))]

    [:div {:class (stl/css-case :row true :audio-row true :hovered hovered? :muted muted?)
           :style (row-style row-index)
           :data-audio-lane lane
           :on-pointer-enter on-pointer-enter
           :on-pointer-leave on-pointer-leave}
     [:div {:class (stl/css :row-label)
            :on-pointer-down on-pointer-down}
      [:span {:class (stl/css :audio-icon)}
       [:> i/icon* {:icon-id i/volume :size "s"}]]
      (if renaming?
        ;; neither a drag of the lane nor the keys of the dock meanwhile
        [:span {:class (stl/css :audio-rename)
                :on-pointer-down dom/stop-propagation
                :on-key-down dom/stop-propagation}
         [:> editable-label* {:value name
                              :display-value label
                              :is-editing true
                              :class-input (stl/css :audio-rename-input)
                              :on-change on-rename
                              :on-cancel on-cancel-rename}]]
        [:span {:class (stl/css :property-name)
                :title label
                :on-double-click on-start-rename}
         label])
      [:> row-toggles* {:hidden? muted?
                        :locked? locked?
                        :visibility-icon (if muted? i/volume-off i/volume)
                        :visibility-title (if muted?
                                            (tr "workspace.animation.lane-unmute")
                                            (tr "workspace.animation.lane-mute"))
                        :lock-title (if locked?
                                      (tr "workspace.animation.lane-unlock")
                                      (tr "workspace.animation.lane-lock"))
                        :on-toggle-visibility on-toggle-muted
                        :on-toggle-lock on-toggle-lock}]]]))

(mf/defc property-row*
  "The label of an animated property of a layer, with its keyframe controls
  and its value; its lane has the keyframes. A hidden one is left out of
  playback and exports; a locked one is kept from edits."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [row-index timeline shape-id property index depth looping? hidden? locked? link-name
           value offset playhead hovered? on-context-menu on-select-layer on-hover]}]
  (let [[on-pointer-enter on-pointer-leave] (use-row-hover row-index on-hover)

        ;; A click on the property picks its keyframes (Shift adds them to
        ;; the selection); one on its buttons or value field does not.
        on-label-click
        (mf/use-fn
         (mf/deps shape-id property index on-select-layer)
         (fn [event]
           (on-select-layer event shape-id)
           (when-not (some-> ^js (dom/get-target event) (.closest "button, input"))
             (st/emit! (dwa/select-slot-keyframes shape-id property index (kbd/shift? event))))))

        on-row-context-menu
        (mf/use-fn
         (mf/deps shape-id property index on-context-menu)
         (fn [event]
           (on-context-menu event shape-id property index)))

        on-toggle-visibility
        (mf/use-fn
         (mf/deps shape-id property index)
         #(st/emit! (dwa/toggle-track-flag shape-id :hidden property index)))

        on-toggle-lock
        (mf/use-fn
         (mf/deps shape-id property index)
         #(st/emit! (dwa/toggle-track-flag shape-id :locked property index)))]

    [:div {:class (stl/css-case :row true :property-row true :hovered hovered? :muted hidden?)
           :style (row-style row-index)
           :on-pointer-enter on-pointer-enter
           :on-pointer-leave on-pointer-leave
           :on-context-menu on-row-context-menu}
     [:div {:class (stl/css :row-label)
            :style #js {"paddingInlineStart" (dm/str (* depth 12) "px")}
            :on-click on-label-click}
      [:span {:class (stl/css :tree-line)}]
      [:span {:class (stl/css :property-name)}
       (labels/property-label property index)
       (when looping?
         [:> i/icon* {:icon-id i/loop
                      :size "s"
                      :class (stl/css :loop-icon)}])
       ;; a link plays it (see `cta/links-of`): its own keyframes do not
       (when (some? link-name)
         [:span {:class (stl/css :linked-mark)
                 :title (tr "workspace.animation.linked-row-hint" link-name)}
          (tr "workspace.animation.linked-row" link-name)])]
      [:> row-controls* {:timeline timeline
                         :shape-id shape-id
                         :property property
                         :index index
                         :playhead playhead
                         :locked? locked?}]
      [:> property-value* {:shape-id shape-id
                           :property property
                           :value value
                           :offset offset
                           :keyframe (dwa/keyframe-at timeline shape-id property playhead index)
                           :locked? locked?}]
      [:> row-toggles* {:hidden? hidden?
                        :locked? locked?
                        :on-toggle-visibility on-toggle-visibility
                        :on-toggle-lock on-toggle-lock}]]]))

(mf/defc marker-name-input*
  "Renames a marker: Enter or leaving the field keeps the name, Escape
  drops it."
  {::mf/private true}
  [{:keys [marker on-done]}]
  (let [input-ref (mf/use-ref nil)

        done
        (mf/use-fn
         (mf/deps marker on-done)
         (fn [keep?]
           (let [name (some-> (mf/ref-val input-ref) dom/get-value str/trim)]
             (when (and keep? (seq name) (not= name (:name marker)))
               (st/emit! (dwa/rename-marker (:id marker) name)))
             (on-done nil))))

        on-key-down
        (mf/use-fn
         (mf/deps done)
         (fn [event]
           (cond
             (kbd/enter? event) (done true)
             (kbd/esc? event)   (done false))))

        on-blur
        (mf/use-fn (mf/deps done) #(done true))]

    (mf/with-effect []
      (when-let [node (mf/ref-val input-ref)]
        (dom/focus! node)
        (dom/select-text! node)))

    [:input {:class (stl/css :marker-input)
             :ref input-ref
             :default-value (:name marker)
             :on-key-down on-key-down
             :on-blur on-blur
             :on-pointer-down dom/stop-propagation
             :on-double-click dom/stop-propagation}]))

(mf/defc marker*
  "A marker on its lane: a click moves the playhead to it, dragging moves
  it (snapping like the keyframes), a double click renames it."
  {::mf/private true}
  [{:keys [marker duration origin lane-ref editing? on-edit on-context-menu]}]
  (let [{:keys [id time name]} marker
        snap     (mf/use-ctx snap-context)
        ;; The drag in progress: its undo transaction, the timeline as it
        ;; was when it started, and whether it moved (else it is a click).
        drag-ref (mf/use-ref nil)

        on-pointer-down
        (mf/use-fn
         (mf/deps snap)
         (fn [event]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             (let [undo-id (js/Symbol)]
               (dom/capture-pointer event)
               (mf/set-ref-val! drag-ref {:undo-id undo-id
                                          :base ((:timeline snap))
                                          :x0 (:x (dom/get-client-position event))})
               (st/emit! (dwu/start-undo-transaction undo-id))))))

        on-pointer-move
        (mf/use-fn
         (mf/deps id duration origin snap)
         (fn [event]
           (when-let [{:keys [base x0] :as drag} (mf/ref-val drag-ref)]
             (let [x      (:x (dom/get-client-position event))
                   moved? (or (:moved? drag) (> (mth/abs (- x x0)) 3))]
               (mf/set-ref-val! drag-ref (assoc drag :moved? moved?))
               (when-let [node (and moved? (mf/ref-val lane-ref))]
                 (let [t      (pointer->time event (dom/get-bounding-rect node) duration origin)
                       target ((:target snap) t event {:marker-ids #{id}})]
                   ((:show snap) target)
                   (st/emit! (dwa/move-marker-from base id (or target t)))))))))

        on-pointer-up
        (mf/use-fn
         (mf/deps time snap)
         (fn [event]
           (when-let [{:keys [undo-id moved?]} (mf/ref-val drag-ref)]
             (dom/release-pointer event)
             (mf/set-ref-val! drag-ref nil)
             ((:show snap) nil)
             (when-not moved?
               (st/emit! (dwa/set-playhead time)))
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        on-double-click
        (mf/use-fn
         (mf/deps id on-edit)
         (fn [event]
           (dom/stop-propagation event)
           (on-edit id)))

        on-marker-context-menu
        (mf/use-fn
         (mf/deps id on-context-menu)
         (fn [event]
           (on-context-menu event id)))]

    [:div {:class (stl/css :marker)
           :data-marker-id (dm/str id)
           :style #js {"left" (time->pct time duration origin)}
           :title (dm/str name " · " (dwa/format-time time (:unit snap) (:fps snap)))
           :on-pointer-down on-pointer-down
           :on-pointer-move on-pointer-move
           :on-pointer-up on-pointer-up
           :on-double-click on-double-click
           :on-context-menu on-marker-context-menu}
     [:span {:class (stl/css :marker-pin)}]
     (if editing?
       [:> marker-name-input* {:marker marker :on-done on-edit}]
       [:span {:class (stl/css :marker-name)} name])]))

(mf/defc marker-playhead*
  "The playhead across the row of the markers, over them: the lanes draw
  it across the rows (see `lanes/draw!`)."
  {::mf/private true}
  [{:keys [span origin]}]
  (let [playhead (mf/deref ref:playhead)]
    [:div {:class (stl/css :marker-playhead)
           :style #js {"left" (time->pct playhead span origin)}}]))

(mf/defc comment-pin*
  "A comment thread about a moment of the animation, as the avatar of its
  author at the moment: a click opens it and shows the moment."
  {::mf/private true}
  [{:keys [thread duration origin unit fps open?]}]
  (let [time  (:animation-time thread)
        owner (dcm/get-owner thread)

        on-click
        (mf/use-fn
         (mf/deps thread)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dwcm/open-thread-moment thread))))]

    [:button {:type "button"
              :class (stl/css-case :comment-pin true
                                   :comment-pin-open open?
                                   :comment-pin-unread (pos? (:count-unread-comments thread))
                                   :comment-pin-resolved (:is-resolved thread))
              :data-testid (dm/str "timeline-comment-" (:seqn thread))
              :style #js {"left" (time->pct time duration origin)}
              :title (dm/str (tr "labels.comment") " #" (:seqn thread)
                             " · " (dwa/format-time time unit fps))
              :on-pointer-down dom/stop-propagation
              :on-double-click dom/stop-propagation
              :on-click on-click}
     [:img {:class (stl/css :comment-pin-avatar)
            :src (cfg/resolve-profile-photo-url owner)
            :alt ""}]]))

(mf/defc comment-pins*
  "The comment threads about moments of the animation of the board
  `board-id` of `objects`, on the ruler at their moments, while the
  comments show on the canvas and as their filters let them (see
  `dcm/apply-filters`)."
  {::mf/private true}
  [{:keys [board-id objects duration origin unit fps]}]
  (let [threads-map (mf/deref refs/threads)
        local       (mf/deref refs/comments-local)
        profile     (mf/deref refs/profile)
        page-id     (mf/deref refs/current-page-id)
        layout      (mf/deref refs/workspace-layout)
        drawing     (mf/deref refs/workspace-drawing)
        shown?      (or (= :comments (:tool drawing))
                        (contains? layout :display-comments))

        threads
        (mf/with-memo [threads-map local profile page-id objects board-id]
          (->> (vals threads-map)
               (filter #(and (= page-id (:page-id %))
                             (some? (:animation-time %))
                             (= board-id (dwcm/thread-board-id objects %))))
               (dcm/apply-filters local profile)
               (sort-by :seqn)))]

    (when shown?
      (for [thread threads]
        [:> comment-pin* {:key (dm/str (:id thread))
                          :thread thread
                          :duration duration
                          :origin origin
                          :unit unit
                          :fps fps
                          :open? (= (:id thread) (:open local))}]))))

(mf/defc marker-row*
  "The markers of the timeline, above the layers: named moments the drags
  snap to and the playhead jumps between. A double click on the lane adds
  one there; the controls go to the one before or after the playhead, or
  add one at it. The lane, `axis-width` wide, is at `lane-ref`: it moves
  as the timeline scrolls (see `sync-scroll!`)."
  {::mf/private true}
  [{:keys [markers playhead duration origin axis-width editing lane-ref on-edit on-context-menu]}]
  (let [snap     (mf/use-ctx snap-context)
        prev     (d/seek #(< (:time %) playhead) (reverse markers))
        next     (d/seek #(> (:time %) playhead) markers)
        new-name (tr "workspace.animation.marker-name" (inc (count markers)))

        on-add
        (mf/use-fn
         (mf/deps new-name)
         #(st/emit! (dwa/add-marker new-name)))

        on-prev
        (mf/use-fn
         (mf/deps prev)
         #(st/emit! (dwa/set-playhead (:time prev))))

        on-next
        (mf/use-fn
         (mf/deps next)
         #(st/emit! (dwa/set-playhead (:time next))))

        on-lane-double-click
        (mf/use-fn
         (mf/deps new-name duration origin snap)
         (fn [event]
           (dom/stop-propagation event)
           (when-let [node (mf/ref-val lane-ref)]
             (let [t (pointer->time event (dom/get-bounding-rect node) duration origin)]
               (st/emit! (dwa/add-marker new-name (or ((:target snap) t event {}) t)))))))]

    [:div {:class (stl/css :row :marker-row)}
     [:div {:class (stl/css :row-label)}
      [:span {:class (stl/css :property-name)} (tr "workspace.animation.markers")]
      [:div {:class (stl/css :row-controls)}
       [:button {:class (stl/css :nav-btn)
                 :title (tr "workspace.animation.prev-marker")
                 :disabled (nil? prev)
                 :on-click on-prev}
        [:> i/icon* {:icon-id i/arrow-left :size "s"}]]
       [:button {:class (stl/css :nav-btn)
                 :title (tr "workspace.animation.add-marker")
                 :on-click on-add}
        [:> i/icon* {:icon-id i/add :size "s"}]]
       [:button {:class (stl/css :nav-btn)
                 :title (tr "workspace.animation.next-marker")
                 :disabled (nil? next)
                 :on-click on-next}
        [:> i/icon* {:icon-id i/arrow-right :size "s"}]]]]
     [:div {:class (stl/css :marker-clip)}
      [:div {:class (stl/css :marker-lane)
             :ref lane-ref
             :style #js {"width" (dm/str axis-width "px")}
             :title (tr "workspace.animation.marker-lane-hint")
             :on-double-click on-lane-double-click}
       (for [marker markers]
         [:> marker* {:key (dm/str (:id marker))
                      :marker marker
                      :duration duration
                      :origin origin
                      :lane-ref lane-ref
                      :editing? (= editing (:id marker))
                      :on-edit on-edit
                      :on-context-menu on-context-menu}])
       [:> marker-playhead* {:span duration :origin origin}]]]]))

;; The parts of the dock that follow the playhead as it plays: the rest of
;; it shows the time playback started from (see `ref:dock-animation`), the
;; lanes draw it on their own (see `draw-lanes`).

(mf/defc current-time*
  {::mf/private true}
  [{:keys [unit fps]}]
  (let [playhead (mf/deref ref:playhead)]
    [:span {:class (stl/css :current-time)} (dwa/format-time playhead unit fps)]))

(mf/defc playhead-follower*
  "While a zoomed timeline plays, pages the view to keep the playhead
  in sight. `geo-ref` places the lanes (see `lanes/draw!`)."
  {::mf/private true}
  [{:keys [scroll-ref geo-ref zoom span playing?]}]
  (let [playhead (mf/deref ref:playhead)]
    (mf/with-layout-effect [playhead zoom span playing?]
      (when (and playing? (> zoom 1))
        (when-let [^js node (mf/ref-val scroll-ref)]
          (let [{:keys [width axis-width origin]} (mf/ref-val geo-ref)
                x    (+ lanes/start-gap (* (/ (- playhead origin) (max 1 span)) axis-width))
                left (.-scrollLeft node)]
            (when (or (< x (+ left lanes/start-gap))
                      (> x (+ left width)))
              (set! (.-scrollLeft node) (- x lanes/start-gap)))))))
    nil))

(def ^:private next-playback-mode
  {:once :loop
   :loop :ping-pong
   :ping-pong :once})

(def ^:private playback-mode-icons
  {:once i/loop-off
   :loop i/loop
   :ping-pong i/ping-pong})

(defn- playback-mode-label
  [mode]
  (case mode
    :loop      (tr "workspace.animation.loop")
    :ping-pong (tr "workspace.animation.ping-pong")
    (tr "workspace.animation.play-once")))

(mf/defc clip-bar*
  "The settings of the selected audio `clip`, in the toolbar of the dock:
  its volume (a drag of the slider is one undo step), its speed (in %),
  its fades (in the time `unit` of the dock), a cut at the playhead
  (Cmd/Ctrl + B), and the length of the timeline fitted to its clips
  (which adding or moving them never changes). Delete removes it; its row
  mutes and locks its lane, its menu the clip. A locked clip only shows
  them."
  {::mf/private true}
  [{:keys [clip unit fps]}]
  (let [clip-id  (:id clip)
        volume   (mth/round (* 100 (or (:volume clip) 1)))
        locked?  (true? (:locked clip))
        fade-in  (or (:fade-in clip) 0)
        fade-out (or (:fade-out clip) 0)
        step     (case unit :s 0.1 :frames 1 10)

        on-fade
        (mf/use-fn
         (mf/deps clip-id unit fps fade-in fade-out)
         (fn [which value]
           (when (number? value)
             (let [ms (max 0 (dwa/field->time value unit fps))]
               (st/emit! (if (= which :in)
                           (dwaa/set-clip-fades clip-id ms fade-out)
                           (dwaa/set-clip-fades clip-id fade-in ms)))))))

        speed (mth/round (* 100 (ctau/clip-speed clip)))

        on-speed
        (mf/use-fn
         (mf/deps clip-id)
         (fn [value]
           (when (number? value)
             (st/emit! (dwaa/set-clip-speed clip-id (/ value 100))))))

        on-fade-in  (mf/use-fn (mf/deps on-fade) #(on-fade :in %))
        on-fade-out (mf/use-fn (mf/deps on-fade) #(on-fade :out %))

        on-volume-down
        (mf/use-fn
         (fn [_]
           (let [undo-id (js/Symbol)]
             (st/emit! (dwu/start-undo-transaction undo-id))
             (events/listenOnce js/window "pointerup"
                                #(st/emit! (dwu/commit-undo-transaction undo-id))))))

        on-volume
        (mf/use-fn
         (mf/deps clip-id)
         (fn [event]
           (let [value (d/parse-double (dom/get-target-val event) 100)]
             (st/emit! (dwaa/set-clip-volume clip-id (/ value 100))))))

        ;; the field goes on where the slider stops, up to four times
        on-volume-value
        (mf/use-fn
         (mf/deps clip-id)
         (fn [value]
           (when (number? value)
             (st/emit! (dwaa/set-clip-volume clip-id (/ value 100))))))

        on-split
        (mf/use-fn (mf/deps clip-id) #(st/emit! (dwaa/split-clip clip-id)))

        on-fit
        (mf/use-fn #(st/emit! (dwaa/fit-timeline-to-audio)))]

    [:div {:class (stl/css :clip-bar)}
     [:span {:class (stl/css :clip-name) :title (:name clip)} (:name clip)]
     [:input {:type "range"
              :class (stl/css :zoom-slider :volume-slider)
              :min 0
              :max 200
              :step 1
              :value (min volume 200)
              :disabled locked?
              :style #js {"--zoom-fill" (dm/str (/ (min volume 200) 2) "%")}
              :title (tr "workspace.animation.clip-volume")
              :aria-label (tr "workspace.animation.clip-volume")
              :on-pointer-down on-volume-down
              :on-change on-volume}]
     [:div {:class (stl/css :duration-field :fade-field)
            :title (tr "workspace.animation.clip-volume")}
      [:> deprecated-input/numeric-input* {:min 0
                                           :max (* 100 ctau/max-volume)
                                           :step 5
                                           :value volume
                                           :is-disabled locked?
                                           :on-change on-volume-value}]
      [:span {:class (stl/css :field-unit)} "%"]]
     [:div {:class (stl/css :duration-field :fade-field)
            :title (tr "workspace.animation.clip-speed")}
      [:label (tr "workspace.animation.clip-speed-short")]
      [:> deprecated-input/numeric-input* {:min (* 100 ctau/min-speed)
                                           :max (* 100 ctau/max-speed)
                                           :step 5
                                           :value speed
                                           :is-disabled locked?
                                           :on-change on-speed}]
      [:span {:class (stl/css :field-unit)} "%"]]
     [:div {:class (stl/css :duration-field :fade-field)
            :title (tr "workspace.animation.clip-fade-in")}
      [:label (tr "workspace.animation.clip-fade-in-short")]
      [:> deprecated-input/numeric-input* {:key (dm/str "in-" (name unit))
                                           :min 0
                                           :step step
                                           :value (dwa/time->field fade-in unit fps)
                                           :is-disabled locked?
                                           :on-change on-fade-in}]]
     [:div {:class (stl/css :duration-field :fade-field)
            :title (tr "workspace.animation.clip-fade-out")}
      [:label (tr "workspace.animation.clip-fade-out-short")]
      [:> deprecated-input/numeric-input* {:key (dm/str "out-" (name unit))
                                           :min 0
                                           :step step
                                           :value (dwa/time->field fade-out unit fps)
                                           :is-disabled locked?
                                           :on-change on-fade-out}]]
     [:button {:type "button"
               :class (stl/css :ctrl-btn)
               :title (dm/str (tr "workspace.animation.clip-split") " (" (ds/meta "b") ")")
               :disabled locked?
               :on-click on-split}
      [:> i/icon* {:icon-id i/separate-nodes}]]
     [:button {:type "button"
               :class (stl/css :ctrl-btn)
               :title (tr "workspace.animation.fit-to-audio")
               :on-click on-fit}
      [:> i/icon* {:icon-id i/fit-content}]]]))

(mf/defc toolbar*
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [timeline playing? recording? full-quality? audio-muted? audio-recording clip clip-count unit fps zoom
           on-zoom-change row-height on-row-height-change maximized? canvas-hidden?]}]
  (let [duration (:duration timeline)
        playback (cta/playback-mode timeline)
        autoplay? (cta/autoplay? timeline)

        ;; a page may start sound only from a user gesture: this one
        on-toggle-play
        (mf/use-fn #(do (audio/unlock!)
                        (st/emit! (dwa/toggle-play))))

        on-toggle-audio
        (mf/use-fn #(do (audio/unlock!)
                        (st/emit! (dwa/toggle-audio-muted))))

        on-toggle-recording
        (mf/use-fn #(st/emit! (dwa/toggle-auto-keyframe)))

        on-stop
        (mf/use-fn #(st/emit! (dwa/stop)))

        ;; Like the numeric inputs of the sidebar: a typed duration applies
        ;; on Enter or when leaving the field, so playback does not stop at
        ;; the first digit; the arrows change it by a step (10 steps with
        ;; Shift) right away: 1 ms, 0.1 s in seconds or a frame.
        on-duration-change
        (mf/use-fn
         (mf/deps unit fps)
         (fn [value]
           (when (number? value)
             (st/emit! (dwa/set-duration (dwa/field->time value unit fps))))))

        on-toggle-unit
        (mf/use-fn #(st/emit! (dwa/toggle-time-unit)))

        on-toggle-quality
        (mf/use-fn #(st/emit! (dwa/toggle-full-quality)))

        on-cycle-playback
        (mf/use-fn
         (mf/deps playback)
         (fn [_] (st/emit! (dwa/set-playback-mode (next-playback-mode playback)))))

        on-toggle-autoplay
        (mf/use-fn #(st/emit! (dwa/toggle-autoplay)))

        on-toggle-maximized
        (mf/use-fn #(st/emit! (dwa/toggle-timeline-maximized)))

        on-toggle-canvas
        (mf/use-fn #(st/emit! (dwa/toggle-canvas-window)))

        on-close
        (mf/use-fn #(st/emit! (dwa/close-timeline)))]

    [:div {:class (stl/css :toolbar)}
     [:div {:class (stl/css :playback-controls)}
      [:button {:type "button"
                :class (stl/css-case :play-btn true :active playing?)
                :title (tr "workspace.animation.play")
                :on-click on-toggle-play}
       (if playing?
         [:span {:class (stl/css :pause-icon)}]
         [:> i/icon* {:icon-id i/play}])]
      [:button {:class (stl/css :ctrl-btn)
                :title (tr "workspace.animation.rewind")
                :on-click on-stop}
       [:> i/icon* {:icon-id i/reload}]]
      [:button {:class (stl/css-case :ctrl-btn true :toggle-btn true :active (not= :once playback))
                :title (playback-mode-label playback)
                :on-click on-cycle-playback}
       [:> i/icon* {:icon-id (playback-mode-icons playback)}]]
      ;; whether the prototype plays it when the board shows
      [:button {:type "button"
                :class (stl/css-case :ctrl-btn true :toggle-btn true :active autoplay?)
                :title (tr "workspace.animation.autoplay")
                :aria-pressed autoplay?
                :on-click on-toggle-autoplay}
       [:> i/icon* {:icon-id i/interaction}]]
      ;; Playing renders fast, without shadows and blur, unless asked
      ;; for full quality; at rest it is always in full quality.
      [:button {:type "button"
                :class (stl/css-case :ctrl-btn true :toggle-btn true :active full-quality?)
                :title (tr "workspace.animation.full-quality")
                :aria-pressed full-quality?
                :on-click on-toggle-quality}
       [:> i/icon* {:icon-id i/effects}]]
      ;; the sound of the audio clips, while the board has some
      (when (seq (:audio-clips timeline))
        [:button {:type "button"
                  :class (stl/css-case :ctrl-btn true :toggle-btn true :active (not audio-muted?))
                  :title (if audio-muted?
                           (tr "workspace.animation.unmute")
                           (tr "workspace.animation.mute"))
                  :aria-pressed (not audio-muted?)
                  :on-click on-toggle-audio}
         [:> i/icon* {:icon-id (if audio-muted? i/volume-off i/volume)}]])
      ;; sound from the microphone into the timeline, from the playhead
      [:> recording-controls* {:recording audio-recording}]]

     [:button {:type "button"
               :class (stl/css-case :rec-btn true :active recording?)
               :title (tr "workspace.animation.auto-keyframe")
               :aria-pressed recording?
               :on-click on-toggle-recording}
      [:span {:class (stl/css :rec-dot)}]
      (tr "workspace.animation.rec")]

     [:> current-time* {:unit unit :fps fps}]

     [:div {:class (stl/css :duration-field)}
      [:label (tr "workspace.animation.duration")]
      (case unit
        :s
        [:> deprecated-input/numeric-input* {:key "s"
                                             :min 0.001
                                             :step 0.1
                                             :value (/ duration 1000)
                                             :on-change on-duration-change}]
        :frames
        [:> deprecated-input/numeric-input* {:key "frames"
                                             :min 1
                                             :step 1
                                             :value (dwa/time->frames duration fps)
                                             :on-change on-duration-change}]
        [:> deprecated-input/numeric-input* {:key "ms"
                                             :min 1
                                             :step 1
                                             :is-integer true
                                             :value duration
                                             :on-change on-duration-change}])
      [:button {:type "button"
                :class (stl/css :unit-btn)
                :title (case unit
                         :ms (tr "workspace.animation.show-seconds")
                         :s  (tr "workspace.animation.show-frames" fps)
                         (tr "workspace.animation.show-milliseconds"))
                :on-click on-toggle-unit}
       (case unit :s "s" :frames "f" "ms")]]

     ;; the settings of one clip; of several, how many
     (cond
       (some? clip)
       [:> clip-bar* {:clip clip :unit unit :fps fps}]

       (> clip-count 1)
       [:div {:class (stl/css :clip-bar)}
        [:span {:class (stl/css :clip-name)} (tr "workspace.animation.clips-selected" clip-count)]])

     [:div {:class (stl/css :toolbar-end)}
      [:div {:class (stl/css :slider-control)
             :title (tr "workspace.animation.row-height")}
       [:> i/icon* {:icon-id i/text-lineheight :size "s"}]
       [:input {:type "range"
                :class (stl/css :zoom-slider :row-height-slider)
                :min lanes/default-row-height
                :max lanes/max-row-height
                :step 1
                :value row-height
                :style #js {"--zoom-fill" (dm/str (* 100 (/ (- row-height lanes/default-row-height)
                                                            (- lanes/max-row-height lanes/default-row-height)))
                                                  "%")}
                :aria-label (tr "workspace.animation.row-height")
                :on-change on-row-height-change}]]
      [:div {:class (stl/css :slider-control)
             :title (tr "workspace.animation.zoom")}
       ;; the icon of the row height, across: the width of the time
       [:> i/icon* {:icon-id i/text-lineheight :size "s" :class (stl/css :zoom-icon)}]
       [:input {:type "range"
                :class (stl/css :zoom-slider)
                :min 0
                :max 100
                :step 1
                :value (zoom->slider zoom)
                :style #js {"--zoom-fill" (dm/str (zoom->slider zoom) "%")}
                :aria-label (tr "workspace.animation.zoom")
                :on-change on-zoom-change}]]
      ;; maximized, the canvas in its small window or out of sight
      (when maximized?
        [:button {:type "button"
                  :class (stl/css-case :ctrl-btn true :toggle-btn true :active (not canvas-hidden?))
                  :title (if canvas-hidden?
                           (tr "workspace.animation.canvas-window-show")
                           (tr "workspace.animation.canvas-window-hide"))
                  :aria-pressed (not canvas-hidden?)
                  :on-click on-toggle-canvas}
         ;; the canvas in a small window: on while it shows (see `:active`)
         [:> i/icon* {:icon-id i/picture-in-picture}]])
      ;; over the whole workspace, as Ctrl + Space does
      [:button {:type "button"
                :class (stl/css-case :ctrl-btn true :toggle-btn true :active maximized?)
                :title (if maximized?
                         (tr "workspace.animation.restore-size" (sc/get-tooltip :toggle-timeline-maximized))
                         (tr "workspace.animation.maximize" (sc/get-tooltip :toggle-timeline-maximized)))
                :aria-pressed maximized?
                :on-click on-toggle-maximized}
       [:> i/icon* {:icon-id (if maximized? i/collapse i/expand)}]]
      [:button {:class (stl/css :close-btn)
                :title (tr "labels.close")
                :on-click on-close}
       [:> i/icon* {:icon-id i/close}]]]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ROOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mf/defc empty-state*
  {::mf/private true}
  []
  [:div {:class (stl/css :empty-state)}
   [:p (tr "workspace.animation.select-board")]])

(defn- clip-problem
  "Why the audio `clip` stays silent here, or nil: its file lasts too
  long (`:too-long`, see `audio/too-long?`) or did not load (see
  `audio/failure`)."
  [clip]
  (if (audio/too-long? clip)
    :too-long
    (audio/failure (:audio-id clip))))

(defn- problem-text
  [problem]
  (case problem
    :too-long    (tr "workspace.audio.silent.too-long" (/ ctau/max-audio-duration 60000))
    :not-loaded  (tr "workspace.audio.silent.not-loaded")
    :undecodable (tr "workspace.audio.silent.undecodable")
    :unavailable (tr "workspace.audio.silent.unavailable")
    :too-much    (tr "workspace.audio.silent.too-much")))

(defn- lanes-title
  "The tooltip of what a `lanes/hit` is on, with times in `unit`; for an
  audio clip that stays silent, why."
  [{:keys [type keyframe animation clip mode]} unit fps]
  (case type
    :keyframe  (dwa/format-time (:time keyframe) unit fps)
    :easing    (tr "workspace.animation.easing")
    :animation (motion/animation-label animation)
    :audio     (case mode
                 :fade-in  (tr "workspace.animation.clip-fade-in-time"
                               (dwa/format-time (or (:fade-in clip) 0) unit fps))
                 :fade-out (tr "workspace.animation.clip-fade-out-time"
                               (dwa/format-time (or (:fade-out clip) 0) unit fps))
                 :volume   (tr "workspace.animation.clip-volume-at"
                               (dm/str (mth/round (* 100 (or (:volume clip) 1))) "%"))
                 (if-let [problem (clip-problem clip)]
                   (dm/str (:name clip) "\n" (problem-text problem))
                   (:name clip)))
    :duration  (tr "workspace.animation.drag-duration")
    nil))

(mf/defc timeline*
  []
  (let [anim       (mf/deref ref:dock-animation)
        timelines  (mf/deref ref:timelines)
        objects    (mf/deref refs/workspace-page-objects)
        selected   (mf/deref refs/selected-shapes)

        ;; The dock targets the board (top-level frame) of the selection,
        ;; and keeps the last one while nothing is selected.
        sel-board-id (dwam/selection-board-id objects selected timelines)
        board-id   (dwam/target-board-id objects selected anim timelines)
        board      (get objects board-id)
        ;; A board without animation shows a new empty timeline; the
        ;; first edit stores it.
        timeline   (mf/with-memo [timelines board]
                     (dwa/board-timeline timelines board))
        ;; The component copies in the board that play the animation of
        ;; their mains, and when each ends; the board keeps time with
        ;; them while it has no tracks itself.
        files      (mf/deref refs/files)
        file-id    (mf/use-ctx ctx/current-file-id)
        copies     (mf/with-memo [objects timelines files board-id]
                     (when (some? board-id)
                       (dwa/copy-timelines objects timelines files board-id)))
        ;; when each copy plays the animation of its main, on the clock
        ;; the board gives it (see `cta/copy-clock`)
        copy-spans (mf/with-memo [copies]
                     (reduce (fn [spans [[_ id] tl]]
                               (let [start (:clock-start tl 0)
                                     end   (cta/clock-end tl)]
                                 (update spans id (fn [[s e]] [(min (or s start) start) (max (or e end) end)]))))
                             {}
                             copies))
        clock      (mf/with-memo [timeline copies timelines board-id]
                     (some-> timeline
                             (cltl/with-copies-clock (vals copies) (contains? timelines board-id))))
        playhead   (get anim :playhead 0)
        playing?   (get anim :playing? false)
        unit       (dwa/time-unit anim)
        fps        (dwa/frame-rate timeline)
        recording? (get anim :auto-keyframe? false)
        ;; over the whole workspace; docked without a board to show, so
        ;; one can be picked on the canvas
        maximized? (mf/deref refs/canvas-in-window)
        canvas-hidden? (dwa/canvas-window-hidden? anim)
        ;; as tall as the user made the rows (see `lanes/row-height`)
        row-height (mth/clamp (or (dwa/row-height anim) lanes/default-row-height)
                              lanes/default-row-height
                              lanes/max-row-height)
        duration   (get clock :duration 1000)

        selected-kfs (get anim :selected-kfs #{})
        selected-clip (get anim :selected-clip)
        selected-clips (dwaa/selected-clips anim)
        selected-anims (or (:selected-anims anim) #{})
        ;; all that is selected, which moves together
        selection     (dwaa/selection-of anim)

        ;; A decoded file draws its waveform (see `audio/version`).
        audio-version (mf/deref audio/version)

        ;; Easing editor of the segment starting at a keyframe, opened from
        ;; the button of the segment and placed at its client `rect`.
        easing*     (mf/use-state nil)
        easing      (deref easing*)
        easing-kf   (when (some? easing)
                      (d/seek #(= (:id %) (:keyframe-id easing))
                              (dm/get-in timeline [:tracks (:shape-id easing) :keyframes])))
        popover-ref (mf/use-ref nil)

        on-edit-easing
        (mf/use-fn
         (fn [rect shape-id keyframe-id]
           (reset! easing* {:shape-id shape-id
                            :keyframe-id keyframe-id
                            :top (:top rect)
                            :left (+ (:left rect) (/ (:width rect) 2))})))

        on-close-easing
        (mf/use-fn #(reset! easing* nil))

        on-easing-key-down
        (mf/use-fn
         (mf/deps on-close-easing)
         (fn [event]
           (when (kbd/esc? event)
             (dom/stop-propagation event)
             (on-close-easing))))

        ;; The view of the rows and the time axis, which it fills at zoom 1
        ;; (see `scroll-view-size`).
        view-size*  (mf/use-state nil)
        view-size   (deref view-size*)
        view-width  (get view-size :width 0)
        view-height (get view-size :height 0)

        scroll-ref   (mf/use-ref nil)
        observer-ref (mf/use-ref nil)

        on-scroll-node
        (mf/use-fn
         (fn [node]
           (mf/set-ref-val! scroll-ref node)
           (some-> ^js (mf/ref-val observer-ref) (.disconnect))
           (mf/set-ref-val! observer-ref nil)
           (when (some? node)
             (let [observer
                   (js/ResizeObserver.
                    (fn [entries]
                      (let [size (scroll-view-size (.-contentRect ^js (aget entries 0)))]
                        (swap! view-size* #(if (= % size) % size)))))]
               (.observe observer node)
               (mf/set-ref-val! observer-ref observer)))))

        ;; Horizontal zoom of the time axis.
        zoom*      (mf/use-state 1)
        zoom       (deref zoom*)
        latest     (mf/with-memo [timeline copy-spans]
                     (content-end timeline copy-spans))
        ;; Before 0 when something lies there, as in Figma, else from 0.
        origin     (mf/with-memo [timeline]
                     (content-start timeline))
        ;; While the end of the timeline is dragged the axis stays as it
        ;; was, unless the duration outgrows it (see `on-duration-down`).
        axis-lock* (mf/use-state nil)
        axis-lock  (deref axis-lock*)
        span       (if (some? axis-lock)
                     (max (:span axis-lock) (- duration origin))
                     (- (axis-span duration zoom latest) origin))
        axis-zoom  (if (some? axis-lock)
                     (:zoom axis-lock)
                     (content-zoom duration zoom span))
        axis-width (lanes/axis-width view-width axis-zoom)
        tick       (tick-step span axis-width unit fps)

        tick-label
        (mf/use-fn
         (mf/deps tick unit fps)
         (fn [time]
           (format-tick time tick unit fps)))

        ;; Drags snap to the playhead, the keyframes and the ends of the
        ;; animations and of the timeline, unless Ctrl/Cmd is held; a line
        ;; shows where. What they snap to is read when they move, so the
        ;; functions stay the same while the timeline changes.
        snap-state-ref (mf/use-ref nil)
        _              (mf/set-ref-val! snap-state-ref {:timeline timeline
                                                        :playhead playhead
                                                        :span span
                                                        :axis-width axis-width
                                                        :unit unit
                                                        :fps fps})
        snap-line*     (mf/use-state nil)
        snap-line      (deref snap-line*)

        snap-target
        (mf/use-fn
         (fn [time event {:keys [playhead?] :or {playhead? true} :as except}]
           (let [{:keys [timeline playhead span axis-width]} (mf/ref-val snap-state-ref)]
             (when-not (kbd/mod? event)
               (cta/snap-time (cond-> (cta/snap-times timeline except)
                                playhead? (conj playhead))
                              time
                              (* snap-distance (/ span (max 1 axis-width))))))))

        show-snap
        (mf/use-fn
         (fn [time]
           (reset! snap-line* time)))

        ;; In frames, the playhead and the dragged keyframes land on whole
        ;; frames when they snap to nothing, unless Ctrl/Cmd is held.
        frame-snap
        (mf/use-fn
         (fn [time event]
           (let [{:keys [unit fps]} (mf/ref-val snap-state-ref)]
             (if (and (= unit :frames) (not (kbd/mod? event)))
               (dwa/frame-time time fps)
               time))))

        snap-span
        (mf/use-fn
         ;; `[start end]` moved so that the end nearer to what it snaps to
         ;; lands on it
         (fn [[start end] event except]
           (let [ts (snap-target start event except)
                 te (snap-target end event except)
                 ds (some-> ts (- start))
                 de (some-> te (- end))
                 d  (cond
                      (nil? ds) de
                      (nil? de) ds
                      (<= (mth/abs ds) (mth/abs de)) ds
                      :else de)]
             (show-snap (cond (nil? d) nil (= d ds) ts :else te))
             (if (some? d) [(+ start d) (+ end d)] [start end]))))

        snap
        (mf/with-memo [unit fps]
          {:timeline #(:timeline (mf/ref-val snap-state-ref))
           :target snap-target
           :show show-snap
           :unit unit
           :fps fps})

        ;; Scroll position to apply once the new zoom is laid out.
        anchor-ref (mf/use-ref nil)

        set-zoom
        (mf/use-fn
         (mf/deps zoom span origin duration latest axis-width)
         (fn [new-zoom]
           (let [new-zoom (mth/clamp new-zoom min-zoom max-zoom)
                 from     (content-zoom duration zoom span)
                 to       (content-zoom duration new-zoom (- (axis-span duration new-zoom latest) origin))
                 playhead (dwa/playhead @st/state)]
             ;; Keep the playhead at the same place on screen.
             (when-let [^js node (mf/ref-val scroll-ref)]
               (let [x (* (/ (- playhead origin) (max 1 span)) axis-width)]
                 (mf/set-ref-val! anchor-ref (+ (.-scrollLeft node)
                                                (* x (- (/ to from) 1))))))
             (reset! zoom* new-zoom))))

        ;; The zoom of the last render, for the wheel, which zooms from it
        ;; once per frame.
        zoom-state-ref (mf/use-ref nil)
        _              (mf/set-ref-val! zoom-state-ref {:zoom zoom :set-zoom set-zoom})

        on-zoom-change
        (mf/use-fn
         (mf/deps set-zoom)
         (fn [event]
           (-> (dom/get-target event)
               (dom/get-value)
               (d/parse-double 0)
               (slider->zoom)
               (set-zoom))))

        ;; Where the rows scroll to once they are laid out taller or
        ;; shorter, so that the row at `y` of the view stays there.
        row-anchor-ref (mf/use-ref nil)

        set-row-height
        (mf/use-fn
         (mf/deps row-height)
         (fn [height y]
           (let [height (mth/clamp (mth/round height) lanes/default-row-height lanes/max-row-height)]
             (when (not= height row-height)
               (when-let [^js node (mf/ref-val scroll-ref)]
                 (mf/set-ref-val! row-anchor-ref
                                  (lanes/row-anchored-scroll (.-scrollTop node) y row-height height)))
               (st/emit! (dwa/set-row-height height))))))

        ;; The row height of the last render, for the wheel (see
        ;; `zoom-state-ref`).
        row-state-ref (mf/use-ref nil)
        _             (mf/set-ref-val! row-state-ref {:row-height row-height
                                                      :set-row-height set-row-height})

        ;; the slider keeps the top row in place
        on-row-height-change
        (mf/use-fn
         (mf/deps set-row-height)
         (fn [event]
           (-> (dom/get-target event)
               (dom/get-value)
               (d/parse-double lanes/default-row-height)
               (set-row-height lanes/header-height))))

        ;; Layer tree of the board, like the layers panel.
        expanded*  (mf/use-state {})
        expanded   (deref expanded*)
        ;; the layers, then the lanes of the audio clips
        audio-collapsed? (true? (:audio-collapsed? anim))
        ;; the recording of sound, a clip of its own as it records
        audio-recording (when (some? (dm/get-in anim [:audio-recording :since]))
                          (:audio-recording anim))
        ;; What the rows are filtered by (see `filter-rows`): while it
        ;; holds something, every layer is open.
        row-filter* (mf/use-state "")
        row-filter  (deref row-filter*)
        ;; the properties the keys of After Effects show, nil for all
        ;; (see `shown-properties`)
        prop-filter* (mf/use-state nil)
        prop-filter  (deref prop-filter*)
        filtering?  (or (not (str/blank? row-filter)) (some? prop-filter))

        on-show-all-properties
        (mf/use-fn
         (fn [_]
           (reset! prop-filter* nil)))

        on-row-filter-change
        (mf/use-fn
         (fn [event]
           (reset! row-filter* (dom/get-target-val event))))

        on-row-filter-key-down
        (mf/use-fn
         (fn [event]
           (when (kbd/esc? event)
             (dom/stop-propagation event)
             (if (str/blank? (dom/get-target-val event))
               (dom/blur! (dom/get-target event))
               (reset! row-filter* "")))))

        rows       (mf/with-memo [objects board-id timeline expanded copy-spans audio-collapsed? audio-recording
                                  row-filter prop-filter filtering?]
                     (when (some? timeline)
                       (-> (timeline-rows objects board-id timeline (if filtering? :all expanded) copy-spans)
                           (filter-rows row-filter prop-filter)
                           (into (dwaa/audio-rows (cond-> timeline
                                                    (some? audio-recording)
                                                    (dwar/with-recording-clip audio-recording))
                                                  :collapsed? audio-collapsed?)))))

        ;; The selection of the last render and the layers newly selected,
        ;; which the rows scroll to once they show (see the effects).
        selected-ref (mf/use-ref nil)
        reveal-ref   (mf/use-ref nil)

        ;; The rows the view shows: only their labels are rendered (and a
        ;; few more, see `sync-scroll!`), the lanes draw only them.
        row-count   (count rows)
        row-window* (mf/use-state [0 0])
        row-window  (deref row-window*)
        row-start   (min (first row-window) row-count)
        row-end     (min (second row-window) row-count)
        ;; What the view scrolls over: the rows under the ruler and the
        ;; markers.
        content-height (+ lanes/header-height
                          (* row-count row-height))

        ;; The values at the playhead of the properties with a label, so
        ;; scrubbing does not work them out for the whole board.
        resolved   (mf/with-memo [timeline objects]
                     (when (some? timeline)
                       (cta/resolve-animations timeline objects)))
        shown-ids  (mf/with-memo [rows row-start row-end]
                     (into #{}
                           (comp (filter #(= :property (:type %)))
                                 (map :id))
                           (subvec (or rows []) row-start row-end)))
        values     (mf/with-memo [resolved shown-ids playhead]
                     (when (some? resolved)
                       (cta/shown-values resolved shown-ids playhead)))

        position-offsets
        (mf/with-memo [timeline objects playhead values]
          (position-offsets timeline objects playhead values))

        on-toggle-layer
        (mf/use-fn
         (fn [shape-id expanded?]
           (swap! expanded* assoc shape-id (not expanded?))))

        ;; All the layers fold up, leaving the list of those of the board,
        ;; or open as they start: the animated ones (see `timeline-rows`).
        collapsed?
        (not-any? #(and (= :layer (:type %)) (:expanded? %) (pos? (:depth %))) rows)

        on-toggle-all-layers
        (mf/use-fn
         (mf/deps collapsed? objects board-id)
         (fn []
           (reset! expanded*
                   (if collapsed?
                     {}
                     (into {board-id true}
                           (map (fn [id] [id false]))
                           (cfh/get-children-ids objects board-id))))))

        on-select-layer
        (mf/use-fn
         (fn [event shape-id]
           (st/emit! (dws/select-shape shape-id (kbd/shift? event)))))

        ;; Dragging a selected track, block or clip, with more selected,
        ;; moves all that is selected together (see `dwaa/move-selection`),
        ;; snapping by the span of the one dragged, from the timeline as it
        ;; was when the drag started, as one undo step.
        sel-drag-ref (mf/use-ref nil)

        on-sel-move
        (mf/use-fn
         (mf/deps span axis-width snap-span)
         (fn [event]
           (when-let [{:keys [base selection x0 start end]} (mf/ref-val sel-drag-ref)]
             (let [dx     (- (:x (dom/get-client-position event)) x0)
                   dt     (mth/round (* dx (/ span (max 1 axis-width))))
                   except {:keyframe-ids (into #{} (map :keyframe-id) (:kfs selection))
                           :animation-ids (into #{} (map :animation-id) (:anims selection))
                           :clip-ids (:clips selection)}
                   [to _] (snap-span [(+ start dt) (+ end dt)] event except)]
               (st/emit! (dwaa/commit-selection-move base selection (- to start)))))))

        on-sel-up
        (mf/use-fn
         (mf/deps show-snap)
         (fn [_event]
           (when-let [{:keys [undo-id listen-keys]} (mf/ref-val sel-drag-ref)]
             (mf/set-ref-val! sel-drag-ref nil)
             (show-snap nil)
             (run! events/unlistenByKey listen-keys)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        start-selection-drag!
        (mf/use-fn
         (mf/deps timeline on-sel-move on-sel-up)
         (fn [event selection start end]
           (let [undo-id (js/Symbol)
                 keys    [(events/listen js/window "pointermove" on-sel-move)
                          (events/listen js/window "pointerup" on-sel-up)]]
             (mf/set-ref-val! sel-drag-ref {:base timeline
                                            :selection selection
                                            :start start
                                            :end end
                                            :x0 (:x (dom/get-client-position event))
                                            :listen-keys keys
                                            :undo-id undo-id})
             (st/emit! (dwu/start-undo-transaction undo-id)))))

        ;; Dragging a layer bar retimes the keyframes of the layer and its
        ;; children from the timeline as it was when the drag started; the
        ;; whole drag is a single undo step.
        bar-drag-ref (mf/use-ref nil)

        on-bar-move
        (mf/use-fn
         (mf/deps span axis-width)
         (fn [event]
           (when-let [{:keys [base ids range mode x0]} (mf/ref-val bar-drag-ref)]
             (let [[start end] range
                   dx     (- (:x (dom/get-client-position event)) x0)
                   dt     (mth/round (* dx (/ span (max 1 axis-width))))
                   ;; the keyframes of the bar move, they snap to the others
                   except {:keyframe-ids (into #{}
                                               (comp (mapcat #(dm/get-in base [:tracks % :keyframes]))
                                                     (map :id))
                                               ids)}
                   to     (case mode
                            :start (let [t (min (+ start dt) (dec end))
                                         s (snap-target t event except)]
                                     (show-snap s)
                                     [(min (or s t) (dec end)) end])
                            :end   (let [t (max (inc start) (+ end dt))
                                         s (snap-target t event except)]
                                     (show-snap s)
                                     [start (max (inc start) (or s t))])
                            (snap-span [(+ start dt) (+ end dt)] event except))]
               (st/emit! (dwa/retime-keyframes base ids range to))))))

        on-bar-up
        (mf/use-fn
         (fn [_event]
           (when-let [{:keys [undo-id listen-keys]} (mf/ref-val bar-drag-ref)]
             (mf/set-ref-val! bar-drag-ref nil)
             (show-snap nil)
             (doseq [key listen-keys]
               (events/unlistenByKey key))
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        on-bar-down
        (mf/use-fn
         (mf/deps timeline objects selected selection on-bar-move on-bar-up start-selection-drag!)
         (fn [event shape-id mode]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             (let [ids    (cfh/get-children-ids-with-self objects shape-id)
                   range  (cta/keyframes-range timeline ids)
                   items  (track-items timeline objects shape-id)
                   held?  (and (every? (:kfs selection) (:kfs items))
                               (every? (:anims selection) (:anims items))
                               (pos? (dwaa/selection-count items)))]
               (cond
                 ;; Shift or Cmd/Ctrl adds the track to what is selected, or
                 ;; takes it out
                 (or (kbd/shift? event) (kbd/mod? event))
                 (st/emit! (dwaa/select-items
                            (if held?
                              (merge-with #(reduce disj %1 %2) selection items)
                              (merge-selection selection items))))

                 ;; selected with more: all of it moves
                 (and held? (= :move mode) (some? range)
                      (> (dwaa/selection-count selection) (dwaa/selection-count items)))
                 (start-selection-drag! event selection (first range) (second range))

                 :else
                 (do
                   (select-on-press event shape-id selected)
                   (when (some? range)
                     (let [undo-id (js/Symbol)
                           ;; Follow the pointer on the window, so the drag keeps
                           ;; going below the bar and past its right end.
                           keys [(events/listen js/window "pointermove" on-bar-move)
                                 (events/listen js/window "pointerup" on-bar-up)]]
                       (mf/set-ref-val! bar-drag-ref {:base timeline
                                                      :ids ids
                                                      :range range
                                                      :listen-keys keys
                                                      ;; a single instant can only move
                                                      :mode (if (apply = range) :move mode)
                                                      :x0 (:x (dom/get-client-position event))
                                                      :undo-id undo-id})
                       (st/emit! (dwu/start-undo-transaction undo-id))))))))))

        ;; Dragging an animation block works like a layer bar, from the
        ;; timeline as it was when the drag started, as one undo step.
        animation-drag-ref (mf/use-ref nil)

        on-animation-move
        (mf/use-fn
         (mf/deps span axis-width)
         (fn [event]
           (when-let [{:keys [base shape-id animation mode x0]} (mf/ref-val animation-drag-ref)]
             (let [start  (:start animation)
                   end    (cta/animation-end animation)
                   dx     (- (:x (dom/get-client-position event)) x0)
                   dt     (mth/round (* dx (/ span (max 1 axis-width))))
                   except {:animation-ids #{(:id animation)}}
                   [start end]
                   (case mode
                     :start (let [t (min (+ start dt) (dec end))
                                  s (snap-target t event except)]
                              (show-snap s)
                              [(min (or s t) (dec end)) end])
                     :end   (let [t (max (inc start) (+ end dt))
                                  s (snap-target t event except)]
                              (show-snap s)
                              [start (max (inc start) (or s t))])
                     (snap-span [(+ start dt) (+ end dt)] event except))]
               (st/emit! (dwa/retime-animation base shape-id (:id animation) start (- end start)))))))

        on-animation-up
        (mf/use-fn
         (fn [_event]
           (when-let [{:keys [undo-id listen-keys]} (mf/ref-val animation-drag-ref)]
             (mf/set-ref-val! animation-drag-ref nil)
             (show-snap nil)
             (doseq [key listen-keys]
               (events/unlistenByKey key))
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        on-animation-down
        (mf/use-fn
         (mf/deps timeline selected selection on-animation-move on-animation-up start-selection-drag!)
         (fn [event shape-id animation mode]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             (let [ref {:shape-id shape-id :animation-id (:id animation)}]
               (cond
                 ;; Shift or Cmd/Ctrl adds the block to what is selected,
                 ;; or takes it out
                 (or (kbd/shift? event) (kbd/mod? event))
                 (st/emit! (dwa/select-animation-block shape-id (:id animation) true))

                 ;; selected with more: all of it moves
                 (and (= :move mode) (contains? (:anims selection) ref)
                      (> (dwaa/selection-count selection) 1))
                 (start-selection-drag! event selection (:start animation) (cta/animation-end animation))

                 :else
                 (do
                   ;; Its settings are in the sidebar of the selected layer.
                   (select-on-press event shape-id selected)
                   (st/emit! (dwa/select-animation-block shape-id (:id animation) false))
                   ;; A locked animation stays where it is.
                   (when-not (:locked animation)
                     (let [undo-id (js/Symbol)
                           keys    [(events/listen js/window "pointermove" on-animation-move)
                                    (events/listen js/window "pointerup" on-animation-up)]]
                       (mf/set-ref-val! animation-drag-ref {:base timeline
                                                            :shape-id shape-id
                                                            :animation animation
                                                            :mode mode
                                                            :x0 (:x (dom/get-client-position event))
                                                            :listen-keys keys
                                                            :undo-id undo-id})
                       (st/emit! (dwu/start-undo-transaction undo-id))))))))))

        dock-ref   (mf/use-ref nil)

        ;; Dragging the top edge resizes the dock up to half of the window.
        {on-resize-start :on-pointer-down
         on-resize-end   :on-lost-pointer-capture
         on-resize-move  :on-pointer-move
         dock-height     :size}
        (r/use-resize-hook :timeline-dock 240 120 "0.5" :y true :bottom)

        ;; Where the lanes are (see `lanes/draw!`). The scroll is kept here
        ;; as the view scrolls (see `sync-scroll!`), which does not render
        ;; the timeline again.
        geo-ref    (mf/use-ref {:scroll-x 0 :scroll-y 0})
        _          (mf/set-ref-val! geo-ref (assoc (mf/ref-val geo-ref)
                                                   :width (max 0 (- view-width lanes/label-width))
                                                   :height view-height
                                                   :row-height row-height
                                                   :span span
                                                   :origin origin
                                                   :axis-width axis-width))
        canvas-ref (mf/use-ref nil)

        ;; The client rect of the time axis, which may reach past the view.
        axis-rect
        (mf/use-fn
         (fn []
           (let [left (:left (dom/get-bounding-rect (mf/ref-val canvas-ref)))
                 {:keys [axis-width scroll-x]} (mf/ref-val geo-ref)
                 x0   (- (+ left lanes/start-gap) scroll-x)]
             {:left x0 :width axis-width :right (+ x0 axis-width)})))

        on-scrub
        (mf/use-fn
         (mf/deps span origin axis-rect)
         (fn [event]
           (let [t      (pointer->time event (axis-rect) span origin)
                 target (snap-target t event {:playhead? false})]
             (show-snap target)
             (st/emit! (dwa/set-playhead (or target (frame-snap t event)))))))

        ;; Dragging the end of the timeline, on the background of the rows
        ;; (see `lanes/hit`), changes its duration, as one undo step. The
        ;; end follows the pointer, snapping like the other drags; past the
        ;; right end of the ruler the duration keeps growing, the faster the
        ;; farther the pointer is. A click on it without a drag is one on
        ;; the background: it clears the keyframe selection.
        duration-drag-ref (mf/use-ref nil)

        grow-duration
        (mf/use-fn
         (mf/deps axis-rect)
         (fn grow []
           (when-let [{:keys [x] :as drag} (mf/ref-val duration-drag-ref)]
             (let [over (- x (:right (axis-rect)))]
               (if (pos? over)
                 (let [{:keys [timeline span]} (mf/ref-val snap-state-ref)]
                   (st/emit! (dwa/set-duration (+ (:duration timeline)
                                                  (mth/ceil (* span 0.05 (/ (min over 100) 100))))))
                   (mf/set-ref-val! duration-drag-ref (assoc drag :timer (js/setTimeout grow 50))))
                 (mf/set-ref-val! duration-drag-ref (dissoc drag :timer)))))))

        on-duration-move
        (mf/use-fn
         (mf/deps grow-duration axis-rect origin)
         (fn [event]
           (when-let [{:keys [x0] :as drag} (mf/ref-val duration-drag-ref)]
             (let [x      (:x (dom/get-client-position event))
                   rect   (axis-rect)
                   moved? (or (:moved? drag) (> (mth/abs (- x x0)) 3))]
               (mf/set-ref-val! duration-drag-ref (assoc drag :x x :moved? moved?))
               (cond
                 (not moved?)
                 nil

                 (> x (:right rect))
                 (do (show-snap nil)
                     (when-not (:timer drag)
                       (grow-duration)))

                 :else
                 (let [{:keys [span]} (mf/ref-val snap-state-ref)
                       t (max 1 (mth/round (+ origin (* span (/ (- x (:left rect)) (max 1 (:width rect)))))))
                       s (snap-target t event {:end? false})]
                   (show-snap s)
                   (st/emit! (dwa/set-duration (or s t)))))))))

        on-duration-up
        (mf/use-fn
         (fn [event]
           (when-let [{:keys [undo-id listen-keys timer moved?]} (mf/ref-val duration-drag-ref)]
             (mf/set-ref-val! duration-drag-ref nil)
             (js/clearTimeout timer)
             (doseq [key listen-keys]
               (events/unlistenByKey key))
             (reset! axis-lock* nil)
             (show-snap nil)
             (when-not (or moved? (kbd/shift? event))
               (st/emit! (dwa/select-keyframes [])))
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        on-duration-down
        (mf/use-fn
         (mf/deps span axis-zoom on-duration-move on-duration-up)
         (fn [event]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             (let [undo-id (js/Symbol)
                   keys    [(events/listen js/window "pointermove" on-duration-move)
                            (events/listen js/window "pointerup" on-duration-up)]]
               (reset! axis-lock* {:span span :zoom axis-zoom})
               (mf/set-ref-val! duration-drag-ref {:undo-id undo-id
                                                   :listen-keys keys
                                                   :x0 (:x (dom/get-client-position event))})
               (st/emit! (dwu/start-undo-transaction undo-id))))))

        ;; A keyframe or a property selects its layer, so the design tab
        ;; changes that layer and not the one selected before.
        on-select-keyframe-layer
        (mf/use-fn
         (mf/deps selected)
         (fn [event shape-id]
           (select-layer event shape-id selected)))

        ;; Right-click menu of a whole track (`property` nil) or of one
        ;; property lane.
        menu*      (mf/use-state nil)
        menu       (deref menu*)

        ;; What the clipboard holds, read when the menu opens: Chrome reads
        ;; it without asking, the other browsers try when pasting.
        paste-type* (mf/use-state nil)
        can-paste?  (or (not (cfg/check-browser? :chrome))
                        (= :copied-animation (deref paste-type*)))

        on-context-menu
        (mf/use-fn
         (fn [event shape-id property & [index animation-id]]
           (dom/prevent-default event)
           (dom/stop-propagation event)
           (let [{:keys [x y]} (dom/get-client-position event)]
             (reset! menu* {:top y
                            :left x
                            :shape-id shape-id
                            :property property
                            :index index
                            :animation-id animation-id}))))

        on-close-menu
        (mf/use-fn #(reset! menu* nil))

        ;; The marker being renamed, see `marker-row*`.
        marker-edit* (mf/use-state nil)
        marker-edit  (deref marker-edit*)

        on-edit-marker
        (mf/use-fn #(reset! marker-edit* %))

        on-marker-context-menu
        (mf/use-fn
         (fn [event marker-id]
           (dom/prevent-default event)
           (dom/stop-propagation event)
           (let [{:keys [x y]} (dom/get-client-position event)]
             (reset! menu* {:top y :left x :marker-id marker-id}))))

        ;; The row under the pointer, on its label or its lane.
        hover-row*     (mf/use-state nil)
        hover-row      (deref hover-row*)
        hover-row-ref  (mf/use-ref nil)
        ;; What of the lanes is under the pointer (see `lanes/hit`).
        hover-ref      (mf/use-ref nil)
        ;; The press on the lanes being dragged: a keyframe, the playhead
        ;; on the ruler, a box selection or an easing button.
        press-ref      (mf/use-ref nil)

        ;; The colours of the lanes (see `lanes/read-palette`), the labels
        ;; of the rows and the lane of the markers, which move by the scroll.
        palette-ref     (mf/use-ref nil)
        strip-ref       (mf/use-ref nil)
        marker-lane-ref (mf/use-ref nil)
        ruler-lane-ref  (mf/use-ref nil)

        ;; What the lanes show, read as they draw: they draw again on their
        ;; own as the view scrolls and as the playhead plays.
        scene-ref (mf/use-ref nil)
        _         (mf/set-ref-val! scene-ref {:rows rows
                                              :timeline timeline
                                              :selected selected
                                              :selected-kfs selected-kfs
                                              :duration duration
                                              :span span
                                              :tick tick
                                              :tick-label tick-label
                                              :animation-label motion/animation-label
                                              :markers (:markers timeline)
                                              :snap-line snap-line
                                              :easing easing
                                              :selected-clips selected-clips
                                              :selected-anims selected-anims
                                              :selection selection
                                              :selected-tracks (selected-tracks timeline objects rows selection)
                                              :waveform dwar/waveform
                                              :problem clip-problem
                                              :audio-version audio-version})

        draw-lanes
        (mf/use-fn
         (fn []
           (let [canvas  (mf/ref-val canvas-ref)
                 palette (mf/ref-val palette-ref)
                 geo     (mf/ref-val geo-ref)]
             (when (and (some? canvas) (some? palette)
                        (pos? (:width geo)) (pos? (:height geo)))
               (let [press (mf/ref-val press-ref)]
                 (lanes/draw! canvas geo
                              (assoc (mf/ref-val scene-ref)
                                     :playhead (deref ref:playhead)
                                     :now (js/Date.now)
                                     :hover (mf/ref-val hover-ref)
                                     :hover-row (mf/ref-val hover-row-ref)
                                     :marquee (when (= :marquee (:kind press)) (:rect press)))
                              (lanes/read-palette palette)
                              (.-fontFamily (js/getComputedStyle palette))))))))

        ;; The view stays put as the timeline scrolls: the labels, the
        ;; markers and the lanes move in it by the scroll, and the labels of
        ;; the rows that come into view are rendered.
        sync-scroll!
        (mf/use-fn
         (fn []
           (when-let [^js node (mf/ref-val scroll-ref)]
             (let [scroll-x (.-scrollLeft node)
                   scroll-y (.-scrollTop node)
                   geo      (assoc (mf/ref-val geo-ref) :scroll-x scroll-x :scroll-y scroll-y)
                   window   (lanes/visible-rows geo row-overscan)]
               (mf/set-ref-val! geo-ref geo)
               (some-> (mf/ref-val strip-ref)
                       (dom/set-css-property! "transform" (dm/str "translateY(" (- scroll-y) "px)")))
               (some-> (mf/ref-val marker-lane-ref)
                       (dom/set-css-property! "transform" (dm/str "translateX(" (- scroll-x) "px)")))
               (some-> (mf/ref-val ruler-lane-ref)
                       (dom/set-css-property! "transform" (dm/str "translateX(" (- scroll-x) "px)")))
               (swap! row-window* #(if (= % window) % window))))))

        on-scroll
        (mf/use-fn
         (mf/deps sync-scroll! draw-lanes)
         (fn [_]
           (sync-scroll!)
           (draw-lanes)))

        set-hover-row
        (mf/use-fn
         (fn [row-index]
           (when (not= row-index (mf/ref-val hover-row-ref))
             (mf/set-ref-val! hover-row-ref row-index)
             (reset! hover-row* row-index))))

        on-label-hover
        (mf/use-fn
         (mf/deps set-hover-row draw-lanes)
         (fn [row-index]
           (set-hover-row row-index)
           (draw-lanes)))

        update-hover!
        (mf/use-fn
         (mf/deps unit fps set-hover-row draw-lanes)
         (fn [target]
           (when (not= (hover-key target) (hover-key (mf/ref-val hover-ref)))
             (mf/set-ref-val! hover-ref target)
             (when-let [^js canvas (mf/ref-val canvas-ref)]
               (set! (.. canvas -style -cursor) (lanes-cursor target))
               (set! (.-title canvas) (or (lanes-title target unit fps) "")))
             (set-hover-row (:row target))
             (draw-lanes))))

        canvas-point
        (mf/use-fn
         (fn [event]
           (let [{:keys [left top]} (dom/get-bounding-rect (mf/ref-val canvas-ref))
                 {:keys [x y]}      (dom/get-client-position event)]
             [(- x left) (- y top)])))

        ;; Dragging an audio clip moves it, onto the audio row under the
        ;; pointer (under the last one, onto a lane of its own, see
        ;; `dwaa/lane-after`), trims it at an edge, changes a fade by its
        ;; grip on the top edge or its volume by its line (up louder, a
        ;; pixel a percent), from the timeline as it was when the drag
        ;; started, as one undo step. It snaps as the other drags do
        ;; (Ctrl/Cmd keeps it from snapping). A locked clip stays.
        clip-drag-ref (mf/use-ref nil)

        on-clip-move
        (mf/use-fn
         (mf/deps span axis-width canvas-point snap-target snap-span show-snap)
         (fn [event]
           (when-let [{:keys [base clip mode x0 y0 lane group]} (mf/ref-val clip-drag-ref)]
             (let [start  (:start clip)
                   end    (ctau/clip-end clip)
                   dx     (- (:x (dom/get-client-position event)) x0)
                   dy     (- (:y (dom/get-client-position event)) y0)
                   dt     (mth/round (* dx (/ span (max 1 axis-width))))
                   except (if (some? group)
                            {:clip-ids (:clips group)
                             :keyframe-ids (into #{} (map :keyframe-id) (:kfs group))
                             :animation-ids (into #{} (map :animation-id) (:anims group))}
                            {:clip-ids #{(:id clip)}})
                   edge   (fn [t]
                            (let [s (snap-target t event except)]
                              (show-snap s)
                              (or s t)))
                   time   (case mode
                            :start    (edge (+ start dt))
                            :end      (edge (+ end dt))
                            :fade-in  (edge (+ start (or (:fade-in clip) 0) dt))
                            :fade-out (edge (+ (- end (or (:fade-out clip) 0)) dt))
                            ;; the volume it is dragged to, past full on up
                            :volume   (- (or (:volume clip) 1) (/ dy 100))
                            (first (snap-span [(+ start dt) (+ end dt)] event except)))
                   [_ y]  (canvas-point event)
                   rows   (:rows (mf/ref-val scene-ref))
                   index  (lanes/row-index (mf/ref-val geo-ref) y)
                   row    (get rows index)
                   lane   (cond
                            (= :audio (:type row))
                            (:lane row)

                            ;; under the last row (the audio rows are the last)
                            (and (= :move mode) (some? index) (>= index (count rows)))
                            (dwaa/lane-after base (:id clip))

                            :else
                            lane)]
               (st/emit! (if (some? group)
                           ;; all that is selected, together, each clip on its lane
                           (dwaa/commit-selection-move base group (- time start))
                           (dwaa/commit-clip-drag base (:id clip) mode time lane)))))))

        on-clip-up
        (mf/use-fn
         (mf/deps show-snap)
         (fn [_event]
           (when-let [{:keys [undo-id listen-keys]} (mf/ref-val clip-drag-ref)]
             (mf/set-ref-val! clip-drag-ref nil)
             (show-snap nil)
             (run! events/unlistenByKey listen-keys)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        on-clip-down
        (mf/use-fn
         (mf/deps timeline on-clip-move on-clip-up)
         (fn [event {:keys [clip mode lane]}]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             (let [selection (:selection (mf/ref-val scene-ref))
                   group?    (and (= :move mode)
                                  (contains? (:clips selection) (:id clip))
                                  (> (dwaa/selection-count selection) 1))]
               (cond
                 ;; Shift or Cmd/Ctrl adds the clip to the selection, or
                 ;; takes it out
                 (or (kbd/shift? event) (kbd/mod? event))
                 (st/emit! (dwaa/toggle-clip (:id clip)))

                 :else
                 (do
                   ;; a selected clip takes the others along
                   (when-not group?
                     (st/emit! (dwaa/select-clip (:id clip))))
                   (when-not (:locked clip)
                     (let [undo-id (js/Symbol)
                           keys    [(events/listen js/window "pointermove" on-clip-move)
                                    (events/listen js/window "pointerup" on-clip-up)]]
                       (mf/set-ref-val! clip-drag-ref {:base timeline
                                                       :clip clip
                                                       :mode mode
                                                       :lane lane
                                                       :group (when group? selection)
                                                       :x0 (:x (dom/get-client-position event))
                                                       :y0 (:y (dom/get-client-position event))
                                                       :listen-keys keys
                                                       :undo-id undo-id})
                       (st/emit! (dwu/start-undo-transaction undo-id))))))))))

        ;; Dragging the label of an audio lane up or down puts the lane at
        ;; the place of the one under the pointer (see `dwaa/move-lane`),
        ;; marked while it moves; one undo step. It starts once the pointer
        ;; has moved, so a click on the label or its buttons stays one.
        lane-drag*  (mf/use-state nil)
        lane-drag   (deref lane-drag*)

        on-lane-drag-down
        (mf/use-fn
         (mf/deps canvas-point)
         (fn [event lane]
           (when (and (dom/left-mouse? event)
                      (not (some-> ^js (dom/get-target event) (.closest "button"))))
             (let [rows      (:rows (mf/ref-val scene-ref))
                   first-row (d/index-of-pred rows #(= :audio (:type %)))
                   lanes   (filterv #(= :audio (:type %)) rows)
                   from    (d/index-of-pred lanes #(= lane (:lane %)))
                   y0      (:y (dom/get-client-position event))
                   current (atom nil)
                   keys    (atom nil)
                   place   (fn [event]
                             (let [[_ y] (canvas-point event)
                                   index (or (lanes/row-index (mf/ref-val geo-ref) y) 0)]
                               (mth/clamp (- index first-row) 0 (dec (count lanes)))))
                   on-move (fn [event]
                             (when (or (some? @current)
                                       (> (mth/abs (- (:y (dom/get-client-position event)) y0)) 3))
                               (let [drag {:from from :to (place event) :first-row first-row}]
                                 (reset! current drag)
                                 (reset! lane-drag* drag))))
                   on-up   (fn [_]
                             (run! events/unlistenByKey @keys)
                             (reset! lane-drag* nil)
                             (when-let [{:keys [to]} @current]
                               (when (not= to from)
                                 (st/emit! (dwaa/move-lane lane to)))))]
               (when (and (some? first-row) (some? from))
                 (reset! keys [(events/listen js/window "pointermove" on-move)
                               (events/listen js/window "pointerup" on-up)]))))))

        on-lanes-down
        (mf/use-fn
         (mf/deps canvas-point objects on-duration-down on-bar-down on-animation-down on-clip-down on-scrub)
         (fn [event]
           (let [[x y]  (canvas-point event)
                 geo    (mf/ref-val geo-ref)
                 scene  (mf/ref-val scene-ref)
                 target (lanes/hit geo scene x y)
                 left?  (dom/left-mouse? event)]
             (case (:type target)
               :duration
               (on-duration-down event)

               :bar
               (on-bar-down event (:shape-id target) (:mode target))

               :animation
               (on-animation-down event (:shape-id target) (:animation target) (:mode target))

               :audio
               (on-clip-down event target)

               ;; The animation of a component copy is the one of its
               ;; main, edited there: its bar picks the copy, and dragging
               ;; it moves when the copy starts (see `cta/copy-clock`).
               :copy
               (when left?
                 (select-on-press event (:shape-id target) (:selected scene))
                 (dom/capture-pointer event)
                 (mf/set-ref-val! press-ref {:kind :copy
                                             :shape-id (:shape-id target)
                                             :start (first (:span target))
                                             :x0 x}))

               :ruler
               (when left?
                 (dom/capture-pointer event)
                 (mf/set-ref-val! press-ref {:kind :scrub})
                 (on-scrub event))

               :keyframe
               (when left?
                 (let [{:keys [shape-id keyframe locked?]} target
                       selected-kfs (:selected-kfs scene)
                       kf-ref       {:shape-id shape-id :keyframe-id (:id keyframe)}
                       selected?    (contains? selected-kfs kf-ref)]
                   (dom/capture-pointer event)
                   (if locked?
                     ;; A locked keyframe stays where it is.
                     (mf/set-ref-val! press-ref {:kind :keyframe :target target})
                     (let [undo-id (js/Symbol)]
                       (mf/set-ref-val! press-ref
                                        {:kind :keyframe
                                         :target target
                                         :x0 x
                                         :undo-id undo-id
                                         :base (:timeline scene)
                                         :selected? selected?
                                         ;; a selected keyframe takes the others along,
                                         ;; the selected blocks and clips too
                                         :moving (if (and selected? (> (count selected-kfs) 1))
                                                   selected-kfs
                                                   #{kf-ref})
                                         :others (when selected?
                                                   (select-keys (:selection scene) [:anims :clips]))})
                       (st/emit! (dwu/start-undo-transaction undo-id))))))

               :easing
               (when left?
                 (dom/capture-pointer event)
                 (mf/set-ref-val! press-ref {:kind :easing :target target}))

               ;; Box selection of keyframes or, from the audio rows, of
               ;; clips. Positions are kept on what the view scrolls over,
               ;; so the box and what it holds stay together when the
               ;; timeline scrolls while dragging.
               (:segment :lane :row :empty)
               (when left?
                 (dom/capture-pointer event)
                 (mf/set-ref-val! press-ref {:kind :marquee
                                             :start (lanes/content-point geo x y)
                                             :additive? (or (kbd/shift? event) (kbd/mod? event))
                                             :base (:selection scene)
                                             :hits {:kfs (lanes/keyframe-boxes geo scene)
                                                    :anims (lanes/animation-boxes geo scene)
                                                    :clips (lanes/clip-boxes geo scene)
                                                    :bars (lanes/bar-boxes geo scene)}
                                             :items-of #(track-items (:timeline scene) objects %)
                                             :moved? false}))

               nil))))

        on-lanes-move
        (mf/use-fn
         (mf/deps canvas-point span axis-rect on-scrub update-hover!)
         (fn [event]
           (let [[x y] (canvas-point event)
                 press (mf/ref-val press-ref)]
             (case (:kind press)
               :scrub
               (on-scrub event)

               :copy
               (when (or (:moved? press) (> (mth/abs (- x (:x0 press))) drag-threshold))
                 (let [{:keys [shape-id start undo-id]} press
                       {:keys [span axis-width]} (mf/ref-val geo-ref)
                       undo-id (or undo-id (js/Symbol))
                       t       (+ start (mth/round (* (- x (:x0 press)) (/ span (max 1 axis-width)))))
                       to      (snap-target t event {})]
                   (when-not (:undo-id press)
                     (st/emit! (dwu/start-undo-transaction undo-id)))
                   (mf/set-ref-val! press-ref (assoc press :moved? true :undo-id undo-id))
                   (show-snap to)
                   (st/emit! (dwa/set-copy-clock shape-id {:start (max 0 (or to (frame-snap t event)))}))))

               :keyframe
               (when (and (some? (:undo-id press))
                          (or (:moved? press) (> (mth/abs (- x (:x0 press))) drag-threshold)))
                 (let [{:keys [base moving target others]} press
                       {:keys [span axis-width]} (mf/ref-val geo-ref)
                       from (:time (:keyframe target))
                       ;; as far as the pointer moved: past the start of the
                       ;; axis too, which then grows before 0 (see `content-start`)
                       t    (+ from (mth/round (* (- x (:x0 press)) (/ span (max 1 axis-width)))))
                       to   (snap-target t event {:keyframe-ids (into #{} (map :keyframe-id) moving)})
                       dt   (- (or to (frame-snap t event)) from)]
                   (mf/set-ref-val! press-ref (assoc press :moved? true))
                   (show-snap to)
                   (st/emit! (if (pos? (dwaa/selection-count others))
                               (dwaa/commit-selection-move base (assoc others :kfs moving) dt)
                               (dwa/shift-keyframes-from base moving dt)))))

               :marquee
               (let [{:keys [start additive? base hits items-of moved? emitted]} press
                     [x0 y0] start
                     [x y]   (lanes/content-point (mf/ref-val geo-ref) x y)]
                 ;; Once a box, it follows the pointer even back to its start.
                 (when (or moved? (marquee-drag? x0 y0 x y))
                   (let [rect     (rect-from-points x0 y0 x y)
                         picked   (items-in-rect hits rect items-of)
                         selected (if additive? (merge-selection base picked) picked)]
                     (mf/set-ref-val! press-ref (assoc press :moved? true :emitted selected :rect rect))
                     (draw-lanes)
                     (when (not= selected emitted)
                       (st/emit! (dwaa/select-items selected))))))

               ;; What is under the pointer, unless it drags a bar, a block
               ;; or the end of the timeline.
               (when (zero? (.-buttons ^js event))
                 (update-hover! (lanes/hit (mf/ref-val geo-ref) (mf/ref-val scene-ref) x y)))))))

        on-lanes-up
        (mf/use-fn
         (mf/deps canvas-point on-edit-easing draw-lanes)
         (fn [event]
           (when-let [press (mf/ref-val press-ref)]
             (mf/set-ref-val! press-ref nil)
             (release-pointer event)
             (case (:kind press)
               :scrub
               (show-snap nil)

               :copy
               (do
                 (show-snap nil)
                 (when-let [undo-id (:undo-id press)]
                   (st/emit! (dwu/commit-undo-transaction undo-id))))

               ;; A click selects the keyframe (and its layer) and moves the
               ;; playhead to it, a locked one only the playhead. A dragged
               ;; one keeps the ones it took along.
               :keyframe
               (let [{:keys [target undo-id moved? selected?]} press
                     {:keys [shape-id keyframe locked?]} target
                     kf-id (:id keyframe)
                     time  (or (->> (dm/get-in (dwa/current-timeline @st/state)
                                               [:tracks shape-id :keyframes])
                                    (d/seek #(= kf-id (:id %)))
                                    (:time))
                               (:time keyframe))]
                 (show-snap nil)
                 (select-layer event shape-id (:selected (mf/ref-val scene-ref)))
                 (if (or locked? (and moved? selected?))
                   (st/emit! (dwa/set-playhead time))
                   (st/emit! (dwa/select-keyframe shape-id kf-id
                                                  (and (not moved?) (or (kbd/shift? event) (kbd/mod? event))))
                             (dwa/set-playhead time)))
                 (when (some? undo-id)
                   (st/emit! (dwu/commit-undo-transaction undo-id))))

               :easing
               (let [{:keys [target]} press
                     [x y] (canvas-point event)]
                 (when (and (not (:locked? target))
                            (lanes/in-rect? (:rect target) x y))
                   (let [{:keys [left top]} (dom/get-bounding-rect (mf/ref-val canvas-ref))
                         rect (:rect target)]
                     (on-edit-easing {:left (+ left (:x rect))
                                      :top (+ top (:y rect))
                                      :width (:width rect)}
                                     (:shape-id target)
                                     (:id (:from target))))))

               ;; A click on nothing clears all that is selected.
               :marquee
               (when-not (or (:moved? press) (:additive? press))
                 (st/emit! (dwaa/select-items {})))

               nil)
             (draw-lanes))))

        on-lanes-leave
        (mf/use-fn
         (mf/deps update-hover!)
         (fn [_]
           (when (nil? (mf/ref-val press-ref))
             (update-hover! nil))))

        ;; A double click on a keyframe deletes it; on the lane of a
        ;; property it adds a keyframe there.
        on-lanes-double-click
        (mf/use-fn
         (mf/deps canvas-point axis-rect span origin on-select-keyframe-layer)
         (fn [event]
           (let [[x y]  (canvas-point event)
                 target (lanes/hit (mf/ref-val geo-ref) (mf/ref-val scene-ref) x y)
                 {:keys [type shape-id keyframe locked?]} target]
             (case type
               :keyframe
               (when-not locked?
                 (st/emit! (dwa/delete-keyframe shape-id (:id keyframe))
                           (dwa/select-keyframe nil nil)))

               (:segment :lane)
               (do (on-select-keyframe-layer event shape-id)
                   (st/emit! (dwa/add-keyframe-at shape-id (:property target) (:index target)
                                                  (pointer->time event (axis-rect) span origin))))

               nil))))

        on-lanes-context-menu
        (mf/use-fn
         (mf/deps canvas-point on-context-menu)
         (fn [event]
           (dom/prevent-default event)
           (let [[x y] (canvas-point event)
                 geo   (mf/ref-val geo-ref)
                 row   (some->> (lanes/row-index geo y)
                                (get (:rows (mf/ref-val scene-ref))))]
             (when (and (some? row) (<= 0 x (:width geo)))
               (case (:type row)
                 :layer     (on-context-menu event (:id row) nil)
                 :animation (on-context-menu event (:id row) nil nil (:id (:animation row)))
                 :property  (on-context-menu event (:id row) (:property row) (:index row))
                 ;; the menu of the clip there, which it selects
                 :audio     (when-let [clip (:clip (lanes/hit geo (mf/ref-val scene-ref) x y))]
                              (let [{:keys [x y]} (dom/get-client-position event)]
                                (st/emit! (dwaa/select-clip (:id clip)))
                                (reset! menu* {:top y :left x :clip-id (:id clip)})))
                 nil)))))

        ;; Audio files dropped on the dock become clips of the board from
        ;; the time under the pointer, on the audio lane under it or on a
        ;; free one (see `dwaa/add-audio-files`).
        on-drag-over
        (mf/use-fn
         (fn [event]
           (when (dnd/has-type? event "Files")
             (dom/prevent-default event))))

        on-drop
        (mf/use-fn
         (mf/deps board-id span origin axis-rect canvas-point)
         (fn [event]
           (when (dnd/has-type? event "Files")
             (let [files (filter dwaa/audio-file? (dnd/get-files event))]
               (when (and (seq files) (some? board-id))
                 (dom/prevent-default event)
                 (let [[_ y] (canvas-point event)
                       row   (get (:rows (mf/ref-val scene-ref)) (lanes/row-index (mf/ref-val geo-ref) y))
                       lane  (if (= :audio (:type row))
                               (:lane row)
                               (some-> (dom/get-target event)
                                       (.closest "[data-audio-lane]")
                                       (dom/get-data "audio-lane")
                                       (d/parse-integer)))]
                   (st/emit! (dwaa/add-audio-files board-id files
                                                   (pointer->time event (axis-rect) span origin)
                                                   lane))))))))

        ;; A click in the dock focuses it; then Delete/Backspace delete the
        ;; selected keyframes, the arrows move them (10 ms, 100 ms with
        ;; Shift) and Cmd/Ctrl+C and X copy and cut them to the clipboard,
        ;; with the selected audio clips,
        ;; instead of reaching the workspace shortcuts for the selected
        ;; shapes (Cmd/Ctrl+V: see `on-paste`). M adds a marker at the
        ;; playhead and Alt with the arrows goes to the marker before or
        ;; after it. P, S, R and T show only one kind of property, as in
        ;; After Effects (see `shown-properties`).
        on-key-down
        (mf/use-fn
         (mf/deps selected-kfs selected-clip selected-anims selection timeline unit fps)
         (fn [event]
           (when-not (tlk/editing-text? event)
             (when-let [action (dock-key-action event selected-kfs selected-clip selected-anims)]
               (dom/prevent-default event)
               (dom/stop-propagation event)
               (case action
                 ;; Shift+M leaves motion mode (see the workspace shortcuts)
                 :add-marker       (st/emit! (dwa/add-marker (tr "workspace.animation.marker-name"
                                                                 (inc (count (:markers timeline))))))
                 :jump-to-marker   (st/emit! (dwa/jump-to-marker (if (kbd/left-arrow? event) -1 1)))
                 :split-clip       (st/emit! (dwaa/split-selected-clips))
                 :split-all-clips  (st/emit! (dwaa/split-all-clips))
                 ;; with more than one kind selected, or blocks, all of it
                 :delete-clip      (st/emit! (if (whole-selection? selection)
                                               (dwaa/delete-selection)
                                               (dwaa/delete-selected-clips)))
                 :nudge-clip       (st/emit! (if (whole-selection? selection)
                                               (dwaa/nudge-selection (nudge-step event unit fps (selection-start timeline selection)))
                                               (dwaa/nudge-selected-clips (nudge-step event unit fps (selection-start timeline selection)))))
                 :delete-keyframes (st/emit! (if (whole-selection? selection)
                                               (dwaa/delete-selection)
                                               (dwa/delete-selected-keyframes)))
                 :show-properties  (swap! prop-filter* shown-properties (property-key event) (kbd/shift? event))
                 :copy-selection   (st/emit! (dwaa/copy-selection))
                 :cut-selection    (st/emit! (dwaa/cut-selection))
                 :shift-keyframes  (st/emit! (if (whole-selection? selection)
                                               (dwaa/nudge-selection (nudge-step event unit fps (selection-start timeline selection)))
                                               (dwa/shift-selected-keyframes (nudge-step event unit fps (selection-start timeline selection))))))))))

        ;; A locked property or animation keeps its keyframes, loop and span.
        menu-locked?
        (when-let [{:keys [shape-id property index animation-id]} menu]
          (if (some? animation-id)
            (true? (:locked (cta/get-animation timeline shape-id animation-id)))
            (and (some? property)
                 (cta/slot-flag? timeline shape-id :locked property index))))

        ;; The use of an animation style the menu is on: it can be
        ;; detached, and its style edited when it is one of this file.
        menu-use
        (when-let [{:keys [shape-id animation-id]} menu]
          (let [animation (some->> animation-id (cta/get-animation timeline shape-id))]
            (when (cta/style-use? animation)
              animation)))

        menu-style-id
        (when (= file-id (:style-ref-file menu-use))
          (:style-ref-id menu-use))

        ;; The context menu re-runs its effects when the options change,
        ;; so they must stay identical between renders.
        ;; The clip the menu is on.
        menu-clip
        (some->> (:clip-id menu) (ctau/get-audio-clip timeline))

        ;; Copy and Cut of the menu of a clip take what is selected with
        ;; it, or it alone.
        menu-in-selection?
        (contains? selected-clips (:clip-id menu))

        menu-options
        (mf/with-memo [menu can-paste? menu-locked? (some? menu-use) menu-style-id menu-clip menu-in-selection?]
          (when-let [{:keys [shape-id property index animation-id marker-id clip-id]} menu]
            (cond
              (some? clip-id)
              (let [locked? (true? (:locked menu-clip))
                    copy    (fn [event]
                              (if menu-in-selection?
                                (st/emit! event)
                                (st/emit! (dwaa/select-clip clip-id) event)))]
                [{:name (tr "workspace.shape.menu.copy")
                  :id "copy-clip"
                  :handler #(copy (dwaa/copy-selection))}
                 {:name (tr "workspace.shape.menu.cut")
                  :id "cut-clip"
                  :disabled locked?
                  :handler #(copy (dwaa/cut-selection))}
                 {:name (tr "workspace.shape.menu.paste")
                  :id "paste-clip"
                  :disabled (not can-paste?)
                  :handler #(st/emit! (dwaa/paste-clipboard-at-playhead))}
                 {:name :separator}
                 {:name (tr "workspace.animation.clip-split-short")
                  :id "split-clip"
                  :disabled locked?
                  :handler #(st/emit! (dwaa/split-clip clip-id))}
                 {:name (if (:muted menu-clip)
                          (tr "workspace.animation.clip-unmute")
                          (tr "workspace.animation.clip-mute"))
                  :id "mute-clip"
                  :disabled locked?
                  :handler #(st/emit! (dwaa/toggle-clip-muted clip-id))}
                 {:name (if locked?
                          (tr "workspace.animation.clip-unlock")
                          (tr "workspace.animation.clip-lock"))
                  :id "lock-clip"
                  :handler #(st/emit! (dwaa/toggle-clip-locked clip-id))}
                 {:name :separator}
                 {:name (tr "workspace.animation.clip-delete")
                  :id "delete-clip"
                  :disabled locked?
                  :handler #(st/emit! (dwaa/delete-clip clip-id))}])

              (some? marker-id)
              [{:name (tr "workspace.animation.rename-marker")
                :id "rename-marker"
                :handler #(reset! marker-edit* marker-id)}
               {:name (tr "workspace.animation.remove-marker")
                :id "remove-marker"
                :handler #(st/emit! (dwa/remove-marker marker-id))}]

              (some? animation-id)
              (cond-> []
                (some? menu-use)
                (conj {:name (tr "workspace.animation.detach-style")
                       :id "detach-style"
                       :disabled menu-locked?
                       :handler #(st/emit! (dwa/detach-animation-style shape-id animation-id))})

                (some? menu-style-id)
                (conj {:name (tr "workspace.animation.edit-style")
                       :id "edit-style"
                       :handler #(st/emit! (dw/show-animation-style-in-assets menu-style-id))})

                :always
                (conj {:name (tr "workspace.animation.remove-animation")
                       :id "remove-animation"
                       :disabled menu-locked?
                       :handler #(st/emit! (dwa/remove-animation shape-id animation-id))}))

              :else
              (cond-> [{:name (tr "workspace.shape.menu.copy")
                        :id "copy"
                        :handler #(st/emit! (dwa/copy-keyframes shape-id property index))}
                       {:name (tr "workspace.shape.menu.paste")
                        :id "paste"
                        :disabled (not can-paste?)
                        :handler #(st/emit! (dwa/paste-clipboard-onto shape-id))}
                       {:name :separator}]
                (some? property)
                (conj {:name (tr "workspace.animation.loop-track")
                       :id "loop-track"
                       :disabled menu-locked?
                       :handler #(st/emit! (dwa/toggle-track-loop shape-id property index))})

                :always
                (conj {:name (tr "workspace.animation.delete-track")
                       :id "delete-track"
                       :disabled menu-locked?
                       :handler #(st/emit! (dwa/delete-track shape-id property index))})))))

        menu-looping?
        (and (some? (:property menu))
             (cta/looping? timeline (:shape-id menu) (:property menu) (:index menu)))]

    ;; The paste entry of the menu needs to know what the clipboard holds.
    (mf/with-effect [menu]
      (when (and (some? menu) (cfg/check-browser? :chrome))
        (dwcp/clipboard-payload-type (partial reset! paste-type*)))
      nil)

    ;; Cmd/Ctrl+V in the dock pastes an animation, copied in the dock or
    ;; on the canvas, at the playhead (see `dwa/paste-clip-at-playhead`).
    ;; Anything else on the clipboard goes on to the workspace, which
    ;; pastes it as before.
    (mf/with-effect []
      (let [on-paste
            (fn [^js event]
              (when-not (tlk/editing-text? event)
                (when-let [clip (some-> (.getBrowserEvent event)
                                        (.-clipboardData)
                                        (.getData "text/plain")
                                        (dwa/payload-clip))]
                  (dom/prevent-default event)
                  (dom/stop-propagation event)
                  (st/emit! (dwa/paste-clip-at-playhead clip)))))
            key (events/listen (mf/ref-val dock-ref) "paste" on-paste)]
        #(events/unlistenByKey key)))

    ;; Focus the easing editor when it opens, so Escape closes it.
    (mf/with-effect [easing]
      (some-> (mf/ref-val popover-ref) (dom/focus!)))

    ;; Closing the dock while its end or a clip is dragged stops the drag.
    (mf/with-effect []
      #(do (when-let [{:keys [listen-keys timer]} (mf/ref-val duration-drag-ref)]
             (js/clearTimeout timer)
             (run! events/unlistenByKey listen-keys))
           (when-let [{:keys [listen-keys undo-id]} (mf/ref-val clip-drag-ref)]
             (run! events/unlistenByKey listen-keys)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

    ;; The files of the clips load, so their waveforms draw (see
    ;; `audio/version`) and they play at once.
    (mf/with-effect [(:audio-clips timeline)]
      (audio/load-clips! timeline))

    (mf/with-effect [sel-board-id]
      (when (some? sel-board-id)
        (st/emit! (dwa/set-active-board sel-board-id))))

    ;; While the dock is shown (motion mode), canvas edits of animated
    ;; properties become keyframes. Leaving motion mode (or hiding the UI)
    ;; unmounts it: stop recording and playing, drop the preview.
    (mf/with-effect []
      (st/emit! (dwa/start-canvas-keyframes))
      #(st/emit! (dwa/stop-canvas-keyframes)
                 (dwa/clear-preview)
                 (dwa/clear-index-preview)))

    ;; Once the playhead rests, the canvas shows the animation there in
    ;; full quality (playing and scrubbing render it fast), and hovering
    ;; and clicking on it find the shapes where it shows them.
    (mf/with-effect [playhead timeline objects playing?]
      (when-not playing?
        (let [timer (js/setTimeout #(st/emit! (dwm/settle-local-transform)
                                              (dwa/index-preview))
                                   150)]
          #(js/clearTimeout timer))))

    ;; Space plays and pauses, the same way the play button does. Capture
    ;; runs before the workspace hand-tool listener, so Space does not pan
    ;; while the timeline is open. A focused play button would also turn
    ;; Space into a click; preventDefault keeps that from toggling twice.
    (mf/with-effect [(some? timeline)]
      (when (some? timeline)
        (letfn [(on-key [event]
                  (when (tlk/plain-space? event)
                    (dom/prevent-default event)
                    (dom/stop-propagation event)
                    (audio/unlock!)
                    (st/emit! (dwa/toggle-play))))]
          (let [key (events/listen js/window "keydown" on-key true)]
            #(events/unlistenByKey key)))))

    (mf/with-layout-effect [zoom]
      (when-let [left (mf/ref-val anchor-ref)]
        (mf/set-ref-val! anchor-ref nil)
        (when-let [^js node (mf/ref-val scroll-ref)]
          (set! (.-scrollLeft node) left))))

    ;; Layers selected on the canvas or in the layers panel show in the
    ;; timeline: their parents open and, once their rows are there, the
    ;; rows scroll to the first unless one of their rows is in view (as
    ;; the one clicked in the timeline is).
    (mf/with-layout-effect [selected]
      (let [prev  (mf/ref-val selected-ref)
            added (into #{} (remove #(contains? prev %)) selected)]
        (mf/set-ref-val! selected-ref selected)
        (when (seq added)
          (let [open (expand-to expanded objects board-id timeline added)]
            (mf/set-ref-val! reveal-ref {:ids added :expanded open})
            (when-not (identical? open expanded)
              (reset! expanded* open))))))

    (mf/with-layout-effect [selected expanded]
      (when-let [{:keys [ids] :as reveal} (mf/ref-val reveal-ref)]
        (when (identical? expanded (:expanded reveal))
          (mf/set-ref-val! reveal-ref nil)
          (when-let [^js node (mf/ref-val scroll-ref)]
            (let [geo      (assoc (mf/ref-val geo-ref) :scroll-y (.-scrollTop node))
                  ;; a layer folded away in a component copy shows as it
                  row-ids  (into #{} (map :id) rows)
                  ids      (into #{}
                                 (keep #(d/seek row-ids (cons % (cfh/get-parent-ids objects %))))
                                 ids)
                  indices  (keep-indexed (fn [index row]
                                           (when (contains? ids (:id row)) index))
                                         rows)
                  scroll-y (lanes/scroll-to-rows geo indices)]
              (when (some? scroll-y)
                (set! (.-scrollTop node) scroll-y)))))))

    (mf/with-layout-effect [row-height]
      (when-let [top (mf/ref-val row-anchor-ref)]
        (mf/set-ref-val! row-anchor-ref nil)
        (when-let [^js node (mf/ref-val scroll-ref)]
          (set! (.-scrollTop node) top))))

    ;; The labels, the markers and the lanes where the timeline is
    ;; scrolled to, after each render.
    (mf/with-layout-effect nil
      (sync-scroll!)
      (draw-lanes))

    ;; While it plays the dock does not render again (see
    ;; `ref:dock-animation`): the lanes follow the playhead on their own.
    (mf/with-effect [draw-lanes]
      (add-watch ref:playhead ::lanes
                 (fn [_ _ old new]
                   (when (not= old new)
                     (draw-lanes))))
      #(remove-watch ref:playhead ::lanes))

    ;; The lanes take their colours from the theme (see `lanes/palette`).
    (mf/with-effect [draw-lanes]
      (theme/add-color-scheme-listener! draw-lanes))

    ;; Ctrl/Cmd + wheel, and a pinch on a trackpad, zoom the time axis
    ;; like the canvas: out when scrolled down, by the pixels scrolled,
    ;; once per frame. Alt + wheel makes the rows taller up the wheel,
    ;; shorter down it, keeping the one under the pointer there. The
    ;; listener is not passive, so it can stop the browser zoom.
    (mf/with-effect [(some? timeline)]
      (when-let [node (mf/ref-val scroll-ref)]
        (let [scale (volatile! 1)
              frame (volatile! nil)
              ;; the pixels scrolled with Alt not yet applied, and where
              rows-delta (volatile! 0)
              rows-y     (volatile! 0)
              rows-frame (volatile! nil)

              zoom!
              (fn []
                (let [{:keys [zoom set-zoom]} (mf/ref-val zoom-state-ref)]
                  (vreset! frame nil)
                  (set-zoom (* zoom @scale))
                  (vreset! scale 1)))

              rows!
              (fn []
                (let [{:keys [row-height set-row-height]} (mf/ref-val row-state-ref)
                      step (mth/round (* row-height-per-pixel @rows-delta))]
                  (vreset! rows-frame nil)
                  (when-not (zero? step)
                    (vreset! rows-delta 0)
                    (set-row-height (- row-height step) @rows-y))))

              on-wheel
              (fn [^js event]
                ;; the wheel fields are on the native event only
                (let [event (.getBrowserEvent event)]
                  (cond
                    (or (kbd/ctrl? event) (kbd/meta? event))
                    (do
                      (dom/prevent-default event)
                      (let [wheel ^js (nw/normalize-wheel event)
                            delta (+ (.-pixelX wheel) (.-pixelY wheel))
                            step  (+ 1 (mth/abs (* zoom-per-pixel delta)))]
                        (vswap! scale * (if (pos? delta) (/ 1 step) step))
                        (when (nil? @frame)
                          (vreset! frame (js/requestAnimationFrame zoom!)))))

                    (kbd/alt? event)
                    (do
                      (dom/prevent-default event)
                      (let [wheel ^js (nw/normalize-wheel event)]
                        (vswap! rows-delta + (.-pixelX wheel) (.-pixelY wheel))
                        (vreset! rows-y (- (.-clientY event) (:top (dom/get-bounding-rect node))))
                        (when (nil? @rows-frame)
                          (vreset! rows-frame (js/requestAnimationFrame rows!))))))))

              key (events/listen node "wheel" on-wheel #js {:passive false})]
          #(do (events/unlistenByKey key)
               (some-> @frame js/cancelAnimationFrame)
               (some-> @rows-frame js/cancelAnimationFrame)))))

    ;; While sound records, its clip grows: the lanes draw again
    ;; meanwhile, whether the animation plays or not.
    (mf/with-effect [(:since audio-recording)]
      (when (some? (:since audio-recording))
        (let [timer (js/setInterval draw-lanes 50)]
          #(js/clearInterval timer))))

    ;; As it starts, the rows scroll to its lane, unless it is in view:
    ;; the lanes of audio come after those of the layers.
    (mf/with-layout-effect [(:since audio-recording)]
      (when (some? (:since audio-recording))
        (when-let [^js node (mf/ref-val scroll-ref)]
          (when-let [index (recording-row-index rows)]
            (let [geo (assoc (mf/ref-val geo-ref) :scroll-y (.-scrollTop node))]
              (when-let [scroll-y (lanes/scroll-to-rows geo [index])]
                (set! (.-scrollTop node) scroll-y)))))))

    ;; Maximized with no board to show, it docks, so one can be picked on
    ;; the canvas.
    (mf/with-effect [(some? timeline) (dwa/timeline-maximized? anim)]
      (when (and (nil? timeline) (dwa/timeline-maximized? anim))
        (st/emit! (dwa/dock-timeline))))

    ;; The dock sits in its own row of the workspace grid. Its height is
    ;; shared with the sidebars through a CSS variable on the workspace
    ;; node (see workspace.scss), capped at half of the window.
    (mf/with-layout-effect [dock-height]
      (let [workspace (dom/get-parent (mf/ref-val dock-ref))]
        (dom/set-css-property! workspace "--workspace-bottom-dock-height"
                               (dm/str "min(" dock-height "px, 50vh)"))
        #(dom/unset-css-property! workspace "--workspace-bottom-dock-height")))

    [:*
     [:section {:class (stl/css-case :timeline-dock true
                                     :recording recording?
                                     :maximized maximized?)
                :ref dock-ref
                :style #js {"--timeline-row-height" (dm/str row-height "px")}
                :tab-index -1
                :on-key-down on-key-down
                :on-drag-over on-drag-over
                :on-drop on-drop}
      (when-not maximized?
        [:div {:class (stl/css :resize-area)
               :on-pointer-down on-resize-start
               :on-lost-pointer-capture on-resize-end
               :on-pointer-move on-resize-move}])
      (if (nil? timeline)
        [:> empty-state*]
        [:*
         [:> toolbar* {:timeline clock
                       :playing? playing?
                       :recording? recording?
                       :full-quality? (dwa/full-quality? anim)
                       :audio-muted? (dwa/audio-muted? anim)
                       :audio-recording (:audio-recording anim)
                       :clip (when (= 1 (count selected-clips))
                               (some->> selected-clip (ctau/get-audio-clip timeline)))
                       :clip-count (count selected-clips)
                       :unit unit
                       :fps fps
                       :zoom zoom
                       :on-zoom-change on-zoom-change
                       :row-height row-height
                       :on-row-height-change on-row-height-change
                       :maximized? maximized?
                       :canvas-hidden? canvas-hidden?}]
         [:> playhead-follower* {:scroll-ref scroll-ref
                                 :geo-ref geo-ref
                                 :zoom zoom
                                 :span span
                                 :playing? playing?}]

         [:div {:class (stl/css :timeline-body)}
          [:div {:class (stl/css :timeline-main)}
           [:div {:class (stl/css :timeline-scroll)
                  :ref on-scroll-node
                  :on-scroll on-scroll}
            [:div {:class (stl/css :timeline-content)
                   :style #js {"width" (dm/str (lanes/content-width axis-width) "px")
                               "height" (dm/str content-height "px")}}
             [:div {:class (stl/css :timeline-view)
                    :style #js {"width" (dm/str view-width "px")
                                "height" (dm/str view-height "px")}}
              [:canvas {:class (stl/css :lanes)
                        :ref canvas-ref
                        :on-pointer-down on-lanes-down
                        :on-pointer-move on-lanes-move
                        :on-pointer-up on-lanes-up
                        :on-lost-pointer-capture on-lanes-up
                        :on-pointer-leave on-lanes-leave
                        :on-double-click on-lanes-double-click
                        :on-context-menu on-lanes-context-menu}]
              [:div {:class (stl/css :palette)
                     :ref palette-ref
                     :aria-hidden true}
               (for [[color css] lanes/palette]
                 [:span {:key (name color)
                         :data-color (name color)
                         :style #js {"color" css}}])]

              ;; The comments about moments of the animation, on the ruler at
              ;; their moments; the lane moves as the timeline scrolls.
              [:div {:class (stl/css :ruler-comments)}
               [:div {:class (stl/css :ruler-comment-lane)
                      :ref ruler-lane-ref
                      :style #js {"width" (dm/str axis-width "px")}}
                [:> comment-pins* {:board-id board-id
                                   :objects objects
                                   :duration span
                                   :origin origin
                                   :unit unit
                                   :fps fps}]]]

              [:div {:class (stl/css :ruler-corner)}
               [:button {:type "button"
                         :class (stl/css :chevron)
                         :title (if collapsed?
                                  (tr "workspace.animation.expand-all")
                                  (tr "workspace.animation.collapse-all"))
                         :on-click on-toggle-all-layers}
                [:> i/icon* {:icon-id (if collapsed? i/arrow-right i/arrow-down) :size "s"}]]
               ;; Shows only the properties, animations or layers named so,
               ;; as the search of Blender's dope sheet.
               [:label {:class (stl/css-case :row-filter true :active filtering?)
                        :title (tr "workspace.animation.filter-rows-hint")}
                [:> i/icon* {:icon-id i/search :size "s" :class (stl/css :row-filter-icon)}]
                [:input {:type "text"
                         :class (stl/css :row-filter-input)
                         :placeholder (tr "workspace.animation.filter-rows")
                         :aria-label (tr "workspace.animation.filter-rows")
                         :spell-check false
                         :value row-filter
                         :on-change on-row-filter-change
                         :on-key-down on-row-filter-key-down}]
                ;; the kinds of property the keys of After Effects show
                (when (some? prop-filter)
                  [:button {:type "button"
                            :class (stl/css :row-filter-props)
                            :title (tr "workspace.animation.show-all-properties")
                            :on-click on-show-all-properties}
                   (str/join ", " (property-names prop-filter))
                   [:> i/icon* {:icon-id i/close :size "s"}]])]]

              [:& (mf/provider snap-context) {:value snap}
               [:> marker-row* {:markers (:markers timeline)
                                :playhead playhead
                                :duration span
                                :origin origin
                                :axis-width axis-width
                                :editing marker-edit
                                :lane-ref marker-lane-ref
                                :on-edit on-edit-marker
                                :on-context-menu on-marker-context-menu}]]

              [:div {:class (stl/css :row-labels)}
               [:div {:class (stl/css :row-strip)
                      :ref strip-ref}
                ;; where a dragged audio lane goes
                (when-let [{:keys [from to first-row]} lane-drag]
                  [:div {:class (stl/css-case :lane-drop true :below (> to from))
                         :style (row-style (+ first-row to))}])
                (for [row-index (range row-start row-end)
                      :let [{:keys [type id depth] :as row} (get rows row-index)
                            hovered? (= row-index hover-row)]]
                  (case type
                    :layer
                    [:> layer-row* {:key (dm/str id)
                                    :row-index row-index
                                    :shape (:shape row)
                                    :depth depth
                                    :expandable? (:expandable? row)
                                    :expanded? (:expanded? row)
                                    :outside? (:outside? row)
                                    :leader-id (dm/get-in timeline [:tracks id :follow :leader])
                                    :leader-name (dm/get-in objects [(dm/get-in timeline [:tracks id :follow :leader]) :name])
                                    :goal-id (dm/get-in timeline [:tracks id :ik :goal])
                                    :goal-name (dm/get-in objects [(dm/get-in timeline [:tracks id :ik :goal]) :name])
                                    :link (first (dm/get-in timeline [:tracks id :links]))
                                    :link-name (dm/get-in objects [(dm/get-in timeline [:tracks id :links 0 :source]) :name])
                                    :selected? (contains? selected id)
                                    :hovered? hovered?
                                    :on-toggle on-toggle-layer
                                    :on-select on-select-layer
                                    :on-context-menu on-context-menu
                                    :on-hover on-label-hover}]

                    :audio-group
                    [:> audio-group-row* {:key "audio-group"
                                          :row-index row-index
                                          :lanes (:count row)
                                          :collapsed? (:collapsed? row)
                                          :muted? (:muted? row)
                                          :locked? (:locked? row)
                                          :hovered? hovered?
                                          :on-hover on-label-hover}]

                    :audio
                    [:> audio-row* {:key (dm/str "audio-" (:lane row))
                                    :row-index row-index
                                    :lane (:lane row)
                                    :number (:number row)
                                    :name (:name row)
                                    :muted? (every? :muted (:clips row))
                                    :locked? (every? :locked (:clips row))
                                    :hovered? hovered?
                                    :on-hover on-label-hover
                                    :on-drag-down on-lane-drag-down}]

                    :animation
                    (let [animation (:animation row)]
                      [:> animation-row* {:key (dm/str id "-" (:id animation))
                                          :row-index row-index
                                          :shape-id id
                                          :animation animation
                                          :depth depth
                                          :hovered? hovered?
                                          :on-select on-select-layer
                                          :on-context-menu on-context-menu
                                          :on-hover on-label-hover}])

                    (let [property (:property row)
                          index    (:index row)]
                      [:> property-row* {:key (dm/str id "-" (name property) "-" index)
                                         :row-index row-index
                                         :timeline timeline
                                         :shape-id id
                                         :property property
                                         :index index
                                         :depth depth
                                         :looping? (cta/looping? timeline id property index)
                                         :link-name (when (nil? index)
                                                      (some->> (cta/link-of timeline id property)
                                                               :source
                                                               (get objects)
                                                               :name))
                                         :hidden? (cta/slot-flag? timeline id :hidden property index)
                                         :locked? (cta/slot-flag? timeline id :locked property index)
                                         :value (cta/value-of (get values id) property index)
                                         :offset (when (nil? index)
                                                   (get-in position-offsets [id property]))
                                         :playhead playhead
                                         :hovered? hovered?
                                         :on-context-menu on-context-menu
                                         :on-select-layer on-select-keyframe-layer
                                         :on-hover on-label-hover}])))]]]]]]]])]

     ;; Above the dock, so it is placed outside of it.
     (when (some? easing-kf)
       [:*
        [:div {:class (stl/css :easing-backdrop)
               :on-pointer-down on-close-easing}]
        [:div {:class (stl/css :easing-popover)
               :ref popover-ref
               :tab-index -1
               :style #js {"top" (dm/str (:top easing) "px")
                           "left" (dm/str (:left easing) "px")}
               :on-key-down on-easing-key-down}
         [:span {:class (stl/css :easing-title)} (tr "workspace.animation.easing")]
         [:> easing-editor* {:shape-id (:shape-id easing)
                             :keyframe easing-kf}]]])
     [:> context-menu* {:show (some? menu)
                        :on-close on-close-menu
                        :fixed true
                        :min-width true
                        :top (:top menu)
                        :left (:left menu)
                        :selected (when menu-looping? "loop-track")
                        :options (or menu-options [])}]]))
