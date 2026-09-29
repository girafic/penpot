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
   [app.common.types.color :as clr]
   [app.common.types.component :as ctk]
   [app.config :as cfg]
   [app.main.data.comments :as dcm]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.comments :as dwcm]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.color-input :refer [color-input*]]
   [app.main.ui.components.context-menu-a11y :refer [context-menu*]]
   [app.main.ui.components.numeric-input :as deprecated-input]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.utilities.swatch :refer [swatch*]]
   [app.main.ui.hooks.resize :as r]
   [app.main.ui.workspace.sidebar.options.menus.motion :as motion]
   [app.main.ui.workspace.timeline.easing :refer [easing-editor*]]
   [app.main.ui.workspace.timeline.lanes :as lanes]
   [app.util.dom :as dom]
   [app.util.dom.normalize-wheel :as nw]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.shape-icon :as usi]
   [app.util.text.ui :as txu]
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

(defn- property-label
  [property index]
  (let [n (when (some? index) (inc index))]
    (case property
      :x "Position X"
      :y "Position Y"
      :width "Width"
      :height "Height"
      :scale-x "Scale X"
      :scale-y "Scale Y"
      :rotation "Rotation"
      :r1 (tr "workspace.options.radius-top-left")
      :r2 (tr "workspace.options.radius-top-right")
      :r3 (tr "workspace.options.radius-bottom-right")
      :r4 (tr "workspace.options.radius-bottom-left")
      :opacity (tr "workspace.options.opacity")
      :fill-color (tr "workspace.animation.fill" n)
      :fill-opacity (tr "workspace.animation.fill-opacity" n)
      :stroke-color (tr "workspace.animation.stroke" n)
      :stroke-opacity (tr "workspace.animation.stroke-opacity" n)
      :stroke-width (tr "workspace.animation.stroke-width" n)
      :shadow-offset-x (tr "workspace.animation.shadow-x" n)
      :shadow-offset-y (tr "workspace.animation.shadow-y" n)
      :shadow-blur (tr "workspace.animation.shadow-blur" n)
      :shadow-spread (tr "workspace.animation.shadow-spread" n)
      :shadow-color (tr "workspace.animation.shadow-color" n)
      :shadow-opacity (tr "workspace.animation.shadow-opacity" n)
      :blur (tr "workspace.animation.blur")
      :background-blur (tr "workspace.animation.background-blur")
      :trim-start (tr "workspace.animation.trim-start")
      :trim-end (tr "workspace.animation.trim-end")
      :trim-offset (tr "workspace.animation.trim-offset")
      (name property))))

(defn- track-slots
  "Distinct `[property index]` slots of `keyframes`, in display order."
  [keyframes]
  (->> keyframes
       (map (juxt :property :index))
       (distinct)
       (sort-by (fn [[property index]]
                  [(get property-order property 99) (or index -1)]))))

(defn- time->pct
  [time duration]
  (if (pos? duration)
    (str (* 100 (/ (double time) duration)) "%")
    "0%"))

(def ^:private tick-steps
  "Candidate spacings (ms) between the labelled ticks of the ruler."
  [10 20 25 50 100 200 250 500 1000 2000 2500 5000 10000 30000 60000])

(def ^:private min-tick-spacing
  "Minimum distance (px) between two labelled ticks."
  64)

(defn- tick-step
  [duration width]
  (let [px-per-ms (/ width (max 1 duration))]
    (or (d/seek #(>= (* % px-per-ms) min-tick-spacing) tick-steps)
        (peek tick-steps))))

(defn- format-tick
  "Like Figma: whole seconds as `1s` and, below one-second steps, the
  milliseconds into the current second (`1s 100ms 200ms …`), or in
  seconds (`1s 1.1s 1.2s …`) when those are the time `unit`."
  [time step unit]
  (cond
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

(defn- axis-span
  "Milliseconds the ruler covers. The duration fills the view at zoom 1.
  Zooming out lengthens the ruler so time keeps running to the right
  edge, and a keyframe past the duration stays on the axis."
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

(defn- editing-text?
  "Whether a key event comes from a text field, which keeps its keys."
  [event]
  (let [target (dom/get-target event)]
    (or (contains? #{"INPUT" "TEXTAREA"} (dom/get-tag-name target))
        (some-> target .-isContentEditable)
        (txu/some-text-editor-content? target))))

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

(defn- plain-space?
  "Unmodified Space, outside a text field. Key repeat does not count, so
  holding Space does not toggle on every repeat."
  [^js event]
  (let [native (if (fn? (.-getBrowserEvent event))
                 (.getBrowserEvent event)
                 event)]
    (and (or (kbd/space? event) (kbd/space? native))
         (not (.-repeat native))
         (not (editing-text? event))
         (not (kbd/shift? event))
         (not (kbd/alt? event))
         (not (kbd/mod? event)))))

(defn- pointer->time
  "Translate a pointer event over a time axis at the client `rect` into a
  time value (ms) clamped to [0, duration]."
  [event rect duration]
  (let [x        (:x (dom/get-client-position event))
        fraction (/ (- x (:left rect)) (max 1 (:width rect)))
        fraction (-> fraction (max 0.0) (min 1.0))]
    (int (* fraction duration))))

(def ^:private c-key? (kbd/is-key-ignore-case? "c"))
(def ^:private x-key? (kbd/is-key-ignore-case? "x"))
(def ^:private v-key? (kbd/is-key-ignore-case? "v"))
(def ^:private m-key? (kbd/is-key-ignore-case? "m"))

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

(def ^:private hint-height
  "Room under the rows for the hint of a timeline without tracks."
  48)

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
   (:id (:keyframe target)) (:id (:from target))])

(defn- lanes-cursor
  [{:keys [type mode locked? animation]}]
  (case type
    (:ruler :duration)       "ew-resize"
    :bar                     (if (= :move mode) "grab" "ew-resize")
    :animation               (cond (:locked animation) "default"
                                   (= :move mode)      "grab"
                                   :else               "ew-resize")
    (:keyframe :easing)      (if locked? "default" "pointer")
    :copy                    "pointer"
    (:segment :lane :row)    "crosshair"
    "default"))

(defn- animated-ids
  "Ids of the layers of `board-id` (itself included) that have keyframes,
  preset animations or animated children, or that are component copies
  playing the animation of their mains (`copy-ends`, see `timeline-rows`)."
  ([objects board-id timeline]
   (animated-ids objects board-id timeline {}))
  ([objects board-id timeline copy-ends]
   (letfn [(walk [id]
             (let [child-ids (mapcat walk (dm/get-in objects [id :shapes]))]
               (if (or (seq (dm/get-in timeline [:tracks id :keyframes]))
                       (seq (dm/get-in timeline [:tracks id :animations]))
                       (contains? copy-ends id)
                       (seq child-ids))
                 (cons id child-ids)
                 child-ids)))]
     (set (walk board-id)))))

(defn- timeline-rows
  "Flatten the layers of `board-id` (top-most first, like the layers
  panel) into rows: one per layer followed, while it is expanded, by one
  per preset animation, one per animated property and its children.
  Animated layers start expanded; `expanded` holds the ones the user
  toggled. A component copy playing the animation of its main, which
  ends at the time `copy-ends` gives for it, has a bar over it, `:copy`,
  and keeps its layers folded away unless the board animates them."
  [objects board-id timeline expanded copy-ends]
  (let [animated (animated-ids objects board-id timeline)
        playing  (animated-ids objects board-id timeline copy-ends)]
    (letfn [(walk [id depth]
              (when-let [shape (get objects id)]
                (let [copy-end   (get copy-ends id)
                      ;; A copy shows its own animation, in the board,
                      ;; unfolded; the one of its main, as a bar.
                      folded?    (and (some? copy-end) (not (contains? animated id)))
                      children   (when-not folded? (reverse (:shapes shape)))
                      animations (dm/get-in timeline [:tracks id :animations])
                      slots      (track-slots (dm/get-in timeline [:tracks id :keyframes]))
                      expanded?  (and (not folded?) (get expanded id (contains? playing id)))]
                  (cons {:type :layer
                         :id id
                         :depth depth
                         :shape shape
                         :expandable? (boolean (or (seq children) (seq animations) (seq slots)))
                         :expanded? expanded?
                         :copy (when (some? copy-end) [0 copy-end])
                         :range (when (contains? animated id)
                                  (cta/keyframes-range timeline (cons id (cfh/get-children-ids objects id))))}
                        (when expanded?
                          (concat
                           (for [animation animations]
                             {:type :animation :id id :animation animation :depth (inc depth)})
                           (for [[property index] slots]
                             {:type :property :id id :property property :index index :depth (inc depth)})
                           (mapcat #(walk % (inc depth)) children)))))))]
      (vec (walk board-id 0)))))

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

    (if (cta/color-property? property)
      [:div {:class (stl/css-case :row-color true :disabled disabled?)
             :title hint}
       [:> swatch* {:background {:color (or value "#000000") :opacity 1}
                    :size "small"}]
       [:> color-input* {:value (clr/remove-hash (or value "#000000"))
                         :class (stl/css :row-value)
                         :disabled disabled?
                         :on-change on-color}]]
      [:> deprecated-input/numeric-input* {:class (stl/css :row-value)
                                           :value (some-> value (- offset) (->> (dwa/value->display property)))
                                           :is-disabled disabled?
                                           :title hint
                                           :on-change on-number}])))

(mf/defc row-toggles*
  "Show / hide and lock / unlock buttons of a row, like the ones of the
  layers panel."
  {::mf/private true}
  [{:keys [hidden? locked? on-toggle-visibility on-toggle-lock]}]
  (let [visibility (if hidden? (tr "workspace.shape.menu.show") (tr "workspace.shape.menu.hide"))
        lock       (if locked? (tr "workspace.shape.menu.unlock") (tr "workspace.shape.menu.lock"))]
    ;; Clicks on them do not reach the row label, which selects the layer.
    [:div {:class (stl/css-case :row-toggles true :has-state (or hidden? locked?))
           :on-click dom/stop-propagation}
     [:button {:type "button"
               :class (stl/css-case :row-toggle true :active hidden?)
               :title visibility
               :aria-label visibility
               :aria-pressed hidden?
               :on-click on-toggle-visibility}
      [:> i/icon* {:icon-id (if hidden? i/hide i/shown) :size "s"}]]
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
  #js {"top" (dm/str (* row-index lanes/row-height) "px")})

(mf/defc layer-row*
  "The label of a layer of the board, in a tree like the layers panel. Its
  lane (see `lanes/draw!`) has, when it is animated, a bar over the span of
  its keyframes and its children's."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [row-index shape depth expandable? expanded? selected? hovered?
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
           (on-context-menu event shape-id nil)))]

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

(mf/defc property-row*
  "The label of an animated property of a layer, with its keyframe controls
  and its value; its lane has the keyframes. A hidden one is left out of
  playback and exports; a locked one is kept from edits."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [row-index timeline shape-id property index depth looping? hidden? locked?
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
       (property-label property index)
       (when looping?
         [:> i/icon* {:icon-id i/loop
                      :size "s"
                      :class (stl/css :loop-icon)}])]
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
  [{:keys [marker duration lane-ref editing? on-edit on-context-menu]}]
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
         (mf/deps id duration snap)
         (fn [event]
           (when-let [{:keys [base x0] :as drag} (mf/ref-val drag-ref)]
             (let [x      (:x (dom/get-client-position event))
                   moved? (or (:moved? drag) (> (mth/abs (- x x0)) 3))]
               (mf/set-ref-val! drag-ref (assoc drag :moved? moved?))
               (when-let [node (and moved? (mf/ref-val lane-ref))]
                 (let [t      (pointer->time event (dom/get-bounding-rect node) duration)
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
           :style #js {"left" (time->pct time duration)}
           :title (dm/str name " · " (dwa/format-time time (:unit snap)))
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
  [{:keys [span]}]
  (let [playhead (mf/deref ref:playhead)]
    [:div {:class (stl/css :marker-playhead)
           :style #js {"left" (time->pct playhead span)}}]))

(mf/defc comment-pin*
  "A comment thread about a moment of the animation, as the avatar of its
  author at the moment: a click opens it and shows the moment."
  {::mf/private true}
  [{:keys [thread duration unit open?]}]
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
              :style #js {"left" (time->pct time duration)}
              :title (dm/str (tr "labels.comment") " #" (:seqn thread)
                             " · " (dwa/format-time time unit))
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
  [{:keys [board-id objects duration unit]}]
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
                          :unit unit
                          :open? (= (:id thread) (:open local))}]))))

(mf/defc marker-row*
  "The markers of the timeline, above the layers: named moments the drags
  snap to and the playhead jumps between. A double click on the lane adds
  one there; the controls go to the one before or after the playhead, or
  add one at it. The lane, `axis-width` wide, is at `lane-ref`: it moves
  as the timeline scrolls (see `sync-scroll!`)."
  {::mf/private true}
  [{:keys [markers playhead duration axis-width editing lane-ref on-edit on-context-menu]}]
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
         (mf/deps new-name duration snap)
         (fn [event]
           (dom/stop-propagation event)
           (when-let [node (mf/ref-val lane-ref)]
             (let [t (pointer->time event (dom/get-bounding-rect node) duration)]
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
                      :lane-ref lane-ref
                      :editing? (= editing (:id marker))
                      :on-edit on-edit
                      :on-context-menu on-context-menu}])
       [:> marker-playhead* {:span duration}]]]]))

;; The parts of the dock that follow the playhead as it plays: the rest of
;; it shows the time playback started from (see `ref:dock-animation`), the
;; lanes draw it on their own (see `draw-lanes`).

(mf/defc current-time*
  {::mf/private true}
  [{:keys [unit]}]
  (let [playhead (mf/deref ref:playhead)]
    [:span {:class (stl/css :current-time)} (dwa/format-time playhead unit)]))

(mf/defc playhead-follower*
  "While a zoomed timeline plays, pages the view to keep the playhead
  in sight. `geo-ref` places the lanes (see `lanes/draw!`)."
  {::mf/private true}
  [{:keys [scroll-ref geo-ref zoom span playing?]}]
  (let [playhead (mf/deref ref:playhead)]
    (mf/with-layout-effect [playhead zoom span playing?]
      (when (and playing? (> zoom 1))
        (when-let [^js node (mf/ref-val scroll-ref)]
          (let [{:keys [width axis-width]} (mf/ref-val geo-ref)
                x    (+ lanes/start-gap (* (/ playhead (max 1 span)) axis-width))
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

(mf/defc toolbar*
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [timeline playing? recording? full-quality? unit zoom on-zoom-change]}]
  (let [duration (:duration timeline)
        playback (cta/playback-mode timeline)
        seconds? (= unit :s)

        on-toggle-play
        (mf/use-fn #(st/emit! (dwa/toggle-play)))

        on-toggle-recording
        (mf/use-fn #(st/emit! (dwa/toggle-auto-keyframe)))

        on-stop
        (mf/use-fn #(st/emit! (dwa/stop)))

        ;; Like the numeric inputs of the sidebar: a typed duration applies
        ;; on Enter or when leaving the field, so playback does not stop at
        ;; the first digit; the arrows change it by a step (10 steps with
        ;; Shift) right away: 1 ms, or 0.1 s in seconds.
        on-duration-change
        (mf/use-fn
         (mf/deps seconds?)
         (fn [value]
           (when (number? value)
             (st/emit! (dwa/set-duration (if seconds? (mth/round (* value 1000)) value))))))

        on-toggle-unit
        (mf/use-fn #(st/emit! (dwa/toggle-time-unit)))

        on-toggle-quality
        (mf/use-fn #(st/emit! (dwa/toggle-full-quality)))

        on-cycle-playback
        (mf/use-fn
         (mf/deps playback)
         (fn [_] (st/emit! (dwa/set-playback-mode (next-playback-mode playback)))))

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
      [:button {:class (stl/css-case :ctrl-btn true :active (not= :once playback))
                :title (playback-mode-label playback)
                :on-click on-cycle-playback}
       [:> i/icon* {:icon-id (playback-mode-icons playback)}]]
      ;; Playing renders fast, without shadows and blur, unless asked
      ;; for full quality; at rest it is always in full quality.
      [:button {:type "button"
                :class (stl/css-case :ctrl-btn true :active full-quality?)
                :title (tr "workspace.animation.full-quality")
                :aria-pressed full-quality?
                :on-click on-toggle-quality}
       [:> i/icon* {:icon-id i/effects}]]]

     [:button {:type "button"
               :class (stl/css-case :rec-btn true :active recording?)
               :title (tr "workspace.animation.auto-keyframe")
               :aria-pressed recording?
               :on-click on-toggle-recording}
      [:span {:class (stl/css :rec-dot)}]
      (tr "workspace.animation.rec")]

     [:> current-time* {:unit unit}]

     [:div {:class (stl/css :duration-field)}
      [:label (tr "workspace.animation.duration")]
      (if seconds?
        [:> deprecated-input/numeric-input* {:key "s"
                                             :min 0.001
                                             :step 0.1
                                             :value (/ duration 1000)
                                             :on-change on-duration-change}]
        [:> deprecated-input/numeric-input* {:key "ms"
                                             :min 1
                                             :step 1
                                             :is-integer true
                                             :value duration
                                             :on-change on-duration-change}])
      [:button {:type "button"
                :class (stl/css :unit-btn)
                :title (if seconds?
                         (tr "workspace.animation.show-milliseconds")
                         (tr "workspace.animation.show-seconds"))
                :on-click on-toggle-unit}
       (if seconds? "s" "ms")]]

     [:div {:class (stl/css :toolbar-end)}
      [:input {:type "range"
               :class (stl/css :zoom-slider)
               :min 0
               :max 100
               :step 1
               :value (zoom->slider zoom)
               :style #js {"--zoom-fill" (dm/str (zoom->slider zoom) "%")}
               :title (tr "workspace.animation.zoom")
               :aria-label (tr "workspace.animation.zoom")
               :on-change on-zoom-change}]
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

(defn- lanes-title
  "The tooltip of what a `lanes/hit` is on, with times in `unit`."
  [{:keys [type keyframe animation]} unit]
  (case type
    :keyframe  (dwa/format-time (:time keyframe) unit)
    :easing    (tr "workspace.animation.easing")
    :animation (motion/animation-label animation)
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
        sel-board-id (when-let [sid (first selected)]
                       (cfh/get-shape-id-root-frame objects sid))
        board-id   (or sel-board-id (:board-id anim))
        board      (get objects board-id)
        ;; A board without animation shows a new empty timeline; the
        ;; first edit stores it.
        timeline   (mf/with-memo [timelines board]
                     (dwa/board-timeline timelines board))
        ;; The component copies in the board that play the animation of
        ;; their mains, and when each ends; the board keeps time with
        ;; them while it has no tracks itself.
        files      (mf/deref refs/files)
        copies     (mf/with-memo [objects timelines files board-id]
                     (when (some? board-id)
                       (dwa/copy-timelines objects timelines files board-id)))
        copy-ends  (mf/with-memo [copies]
                     (reduce (fn [ends [[_ id] tl]]
                               (update ends id (fnil max 0) (cta/cycle-duration tl)))
                             {}
                             copies))
        clock      (mf/with-memo [timeline copies timelines board-id]
                     (some-> timeline
                             (cltl/with-copies-clock (vals copies) (contains? timelines board-id))))
        playhead   (get anim :playhead 0)
        playing?   (get anim :playing? false)
        unit       (dwa/time-unit anim)
        recording? (get anim :auto-keyframe? false)
        duration   (get clock :duration 1000)

        selected-kfs (get anim :selected-kfs #{})

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

        ;; The view of the rows and the time axis, which it fills at zoom 1.
        view-size*  (mf/use-state nil)
        view-size   (deref view-size*)
        view-width  (get view-size :width 0)
        view-height (get view-size :height 0)

        observe-view
        (r/use-resize-observer
         (fn [_ {:keys [width height]}]
           (let [size {:width width :height height}]
             (swap! view-size* #(if (= % size) % size)))))

        scroll-ref (mf/use-ref nil)

        on-scroll-node
        (mf/use-fn
         (mf/deps observe-view)
         (fn [node]
           (mf/set-ref-val! scroll-ref node)
           (observe-view node)))

        ;; Horizontal zoom of the time axis.
        zoom*      (mf/use-state 1)
        zoom       (deref zoom*)
        latest     (mf/with-memo [timeline copy-ends]
                     (apply max 0 (concat (map :time (mapcat :keyframes (vals (:tracks timeline))))
                                          (vals copy-ends))))
        ;; While the end of the timeline is dragged the axis stays as it
        ;; was, unless the duration outgrows it (see `on-duration-down`).
        axis-lock* (mf/use-state nil)
        axis-lock  (deref axis-lock*)
        span       (if (some? axis-lock)
                     (max (:span axis-lock) duration)
                     (axis-span duration zoom latest))
        axis-zoom  (if (some? axis-lock)
                     (:zoom axis-lock)
                     (content-zoom duration zoom span))
        axis-width (lanes/axis-width view-width axis-zoom)
        tick       (tick-step span axis-width)

        tick-label
        (mf/use-fn
         (mf/deps tick unit)
         (fn [time]
           (format-tick time tick unit)))

        ;; Drags snap to the playhead, the keyframes and the ends of the
        ;; animations and of the timeline, unless Ctrl/Cmd is held; a line
        ;; shows where. What they snap to is read when they move, so the
        ;; functions stay the same while the timeline changes.
        snap-state-ref (mf/use-ref nil)
        _              (mf/set-ref-val! snap-state-ref {:timeline timeline
                                                        :playhead playhead
                                                        :span span
                                                        :axis-width axis-width})
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
        (mf/with-memo [unit]
          {:timeline #(:timeline (mf/ref-val snap-state-ref))
           :target snap-target
           :show show-snap
           :unit unit})

        ;; Scroll position to apply once the new zoom is laid out.
        anchor-ref (mf/use-ref nil)

        set-zoom
        (mf/use-fn
         (mf/deps zoom span duration latest axis-width)
         (fn [new-zoom]
           (let [new-zoom (mth/clamp new-zoom min-zoom max-zoom)
                 from     (content-zoom duration zoom span)
                 to       (content-zoom duration new-zoom (axis-span duration new-zoom latest))
                 playhead (dwa/playhead @st/state)]
             ;; Keep the playhead at the same place on screen.
             (when-let [^js node (mf/ref-val scroll-ref)]
               (let [x (* (/ playhead (max 1 span)) axis-width)]
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

        ;; Layer tree of the board, like the layers panel.
        expanded*  (mf/use-state {})
        expanded   (deref expanded*)
        rows       (mf/with-memo [objects board-id timeline expanded copy-ends]
                     (when (some? timeline)
                       (timeline-rows objects board-id timeline expanded copy-ends)))

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
        ;; markers, and a hint while the timeline has no tracks.
        hint?       (and (empty? (:tracks timeline)) (empty? copies))
        content-height (+ lanes/header-height
                          (* row-count lanes/row-height)
                          (if hint? hint-height 0))

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
                       (-> (update resolved :tracks select-keys shown-ids)
                           (cta/values-at playhead))))

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
                            :start (let [t (mth/clamp (+ start dt) 0 (dec end))
                                         s (snap-target t event except)]
                                     (show-snap s)
                                     [(mth/clamp (or s t) 0 (dec end)) end])
                            :end   (let [t (max (inc start) (+ end dt))
                                         s (snap-target t event except)]
                                     (show-snap s)
                                     [start (max (inc start) (or s t))])
                            (let [dt (max dt (- start))
                                  [s e] (snap-span [(+ start dt) (+ end dt)] event except)]
                              (if (neg? s) [0 (- e s)] [s e])))]
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
         (mf/deps timeline objects selected on-bar-move on-bar-up)
         (fn [event shape-id mode]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             (select-on-press event shape-id selected)
             (let [ids   (cfh/get-children-ids-with-self objects shape-id)
                   range (cta/keyframes-range timeline ids)]
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
                   (st/emit! (dwu/start-undo-transaction undo-id))))))))

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
                     :start (let [t (mth/clamp (+ start dt) 0 (dec end))
                                  s (snap-target t event except)]
                              (show-snap s)
                              [(mth/clamp (or s t) 0 (dec end)) end])
                     :end   (let [t (max (inc start) (+ end dt))
                                  s (snap-target t event except)]
                              (show-snap s)
                              [start (max (inc start) (or s t))])
                     (let [dt (max dt (- start))
                           [s e] (snap-span [(+ start dt) (+ end dt)] event except)]
                       (if (neg? s) [0 (- e s)] [s e])))]
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
         (mf/deps timeline selected on-animation-move on-animation-up)
         (fn [event shape-id animation mode]
           (dom/stop-propagation event)
           (when (dom/left-mouse? event)
             ;; Its settings are in the sidebar of the selected layer.
             (select-on-press event shape-id selected)
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
                 (st/emit! (dwu/start-undo-transaction undo-id)))))))

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
                                                   :span span
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
         (mf/deps span axis-rect)
         (fn [event]
           (let [t      (pointer->time event (axis-rect) span)
                 target (snap-target t event {:playhead? false})]
             (show-snap target)
             (st/emit! (dwa/set-playhead (or target t))))))

        ;; Dragging the end of the timeline changes its duration, as one
        ;; undo step. The end follows the pointer, snapping like the other
        ;; drags; past the right end of the ruler the duration keeps growing,
        ;; the faster the farther the pointer is. A click on it without a
        ;; drag moves the playhead there, as on the rest of the ruler.
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
         (mf/deps grow-duration axis-rect)
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
                       t (max 1 (mth/round (* span (/ (- x (:left rect)) (max 1 (:width rect))))))
                       s (snap-target t event {:end? false})]
                   (show-snap s)
                   (st/emit! (dwa/set-duration (or s t)))))))))

        on-duration-up
        (mf/use-fn
         (mf/deps on-scrub)
         (fn [event]
           (when-let [{:keys [undo-id listen-keys timer moved?]} (mf/ref-val duration-drag-ref)]
             (mf/set-ref-val! duration-drag-ref nil)
             (js/clearTimeout timer)
             (doseq [key listen-keys]
               (events/unlistenByKey key))
             (reset! axis-lock* nil)
             (show-snap nil)
             (when-not moved?
               (on-scrub event))
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
        can-paste? (boolean (seq (dm/get-in anim [:clipboard :keyframes])))

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
                                              :easing easing})

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
         (mf/deps unit set-hover-row draw-lanes)
         (fn [target]
           (when (not= (hover-key target) (hover-key (mf/ref-val hover-ref)))
             (mf/set-ref-val! hover-ref target)
             (when-let [^js canvas (mf/ref-val canvas-ref)]
               (set! (.. canvas -style -cursor) (lanes-cursor target))
               (set! (.-title canvas) (or (lanes-title target unit) "")))
             (set-hover-row (:row target))
             (draw-lanes))))

        canvas-point
        (mf/use-fn
         (fn [event]
           (let [{:keys [left top]} (dom/get-bounding-rect (mf/ref-val canvas-ref))
                 {:keys [x y]}      (dom/get-client-position event)]
             [(- x left) (- y top)])))

        on-lanes-down
        (mf/use-fn
         (mf/deps canvas-point on-duration-down on-bar-down on-animation-down on-scrub)
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

               ;; The animation of a component copy is the one of its
               ;; main, edited there: its bar only picks the copy.
               :copy
               (when left?
                 (select-on-press event (:shape-id target) (:selected scene)))

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
                                         ;; a selected keyframe takes the others along
                                         :moving (if (and selected? (> (count selected-kfs) 1))
                                                   selected-kfs
                                                   #{kf-ref})})
                       (st/emit! (dwu/start-undo-transaction undo-id))))))

               :easing
               (when left?
                 (dom/capture-pointer event)
                 (mf/set-ref-val! press-ref {:kind :easing :target target}))

               ;; Box selection of keyframes. Positions are kept on what the
               ;; view scrolls over, so the box and its keyframes stay
               ;; together when the timeline scrolls while dragging.
               (:segment :lane :row :empty)
               (when left?
                 (dom/capture-pointer event)
                 (mf/set-ref-val! press-ref {:kind :marquee
                                             :start (lanes/content-point geo x y)
                                             :additive? (kbd/shift? event)
                                             :base (:selected-kfs scene)
                                             :hits (lanes/keyframe-boxes geo scene)
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

               :keyframe
               (when (and (some? (:undo-id press))
                          (or (:moved? press) (> (mth/abs (- x (:x0 press))) drag-threshold)))
                 (let [{:keys [base moving target]} press
                       t    (pointer->time event (axis-rect) span)
                       to   (snap-target t event {:keyframe-ids (into #{} (map :keyframe-id) moving)})
                       from (:time (:keyframe target))]
                   (mf/set-ref-val! press-ref (assoc press :moved? true))
                   (show-snap to)
                   (st/emit! (dwa/shift-keyframes-from base moving (- (or to t) from)))))

               :marquee
               (let [{:keys [start additive? base hits moved? emitted]} press
                     [x0 y0] start
                     [x y]   (lanes/content-point (mf/ref-val geo-ref) x y)]
                 ;; Once a box, it follows the pointer even back to its start.
                 (when (or moved? (marquee-drag? x0 y0 x y))
                   (let [rect     (rect-from-points x0 y0 x y)
                         picked   (keyframes-in-rect hits rect)
                         selected (if additive? (into base picked) picked)]
                     (mf/set-ref-val! press-ref (assoc press :moved? true :emitted selected :rect rect))
                     (draw-lanes)
                     (when (not= selected emitted)
                       (st/emit! (dwa/select-keyframes selected))))))

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
                   (st/emit! (dwa/select-keyframe shape-id kf-id (and (not moved?) (kbd/shift? event)))
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

               ;; A click on nothing clears the keyframe selection.
               :marquee
               (when-not (or (:moved? press) (:additive? press))
                 (st/emit! (dwa/select-keyframes [])))

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
         (mf/deps canvas-point axis-rect span on-select-keyframe-layer)
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
                                                  (pointer->time event (axis-rect) span))))

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
                 :property  (on-context-menu event (:id row) (:property row) (:index row)))))))

        ;; A click in the dock focuses it; then Delete/Backspace delete the
        ;; selected keyframes, the arrows move them (10 ms, 100 ms with
        ;; Shift) and Cmd/Ctrl+C, X and V copy, cut and paste them at the
        ;; playhead, instead of reaching the workspace shortcuts for the
        ;; selected shapes. M adds a marker at the playhead and Alt with
        ;; the arrows goes to the marker before or after it.
        on-key-down
        (mf/use-fn
         (mf/deps selected-kfs can-paste? timeline)
         (fn [event]
           (cond
             (editing-text? event)
             nil

             ;; Shift+M leaves motion mode (see the workspace shortcuts)
             (and (m-key? event) (not (kbd/mod? event)) (not (kbd/alt? event)) (not (kbd/shift? event)))
             (do (dom/prevent-default event)
                 (dom/stop-propagation event)
                 (st/emit! (dwa/add-marker (tr "workspace.animation.marker-name"
                                               (inc (count (:markers timeline)))))))

             (and (kbd/alt? event) (or (kbd/left-arrow? event) (kbd/right-arrow? event)))
             (do (dom/prevent-default event)
                 (dom/stop-propagation event)
                 (st/emit! (dwa/jump-to-marker (if (kbd/left-arrow? event) -1 1))))

             (or (kbd/delete? event) (kbd/backspace? event))
             (do (dom/prevent-default event)
                 (dom/stop-propagation event)
                 (st/emit! (dwa/delete-selected-keyframes)))

             (and (kbd/mod? event) (or (c-key? event) (x-key? event)) (seq selected-kfs))
             (do (dom/prevent-default event)
                 (dom/stop-propagation event)
                 (if (x-key? event)
                   (st/emit! (dwa/copy-selected-keyframes) (dwa/delete-selected-keyframes))
                   (st/emit! (dwa/copy-selected-keyframes))))

             (and (kbd/mod? event) (v-key? event) can-paste?)
             (do (dom/prevent-default event)
                 (dom/stop-propagation event)
                 (st/emit! (dwa/paste-keyframes-at-playhead)))

             (and (seq selected-kfs)
                  (or (kbd/left-arrow? event) (kbd/right-arrow? event)))
             (do (dom/prevent-default event)
                 (dom/stop-propagation event)
                 (st/emit! (dwa/shift-selected-keyframes
                            (* (if (kbd/left-arrow? event) -1 1)
                               (if (kbd/shift? event) 100 10))))))))

        ;; A locked property or animation keeps its keyframes, loop and span.
        menu-locked?
        (when-let [{:keys [shape-id property index animation-id]} menu]
          (if (some? animation-id)
            (true? (:locked (cta/get-animation timeline shape-id animation-id)))
            (and (some? property)
                 (cta/slot-flag? timeline shape-id :locked property index))))

        ;; The context menu re-runs its effects when the options change,
        ;; so they must stay identical between renders.
        menu-options
        (mf/with-memo [menu can-paste? menu-locked?]
          (when-let [{:keys [shape-id property index animation-id marker-id]} menu]
            (cond
              (some? marker-id)
              [{:name (tr "workspace.animation.rename-marker")
                :id "rename-marker"
                :handler #(reset! marker-edit* marker-id)}
               {:name (tr "workspace.animation.remove-marker")
                :id "remove-marker"
                :handler #(st/emit! (dwa/remove-marker marker-id))}]

              (some? animation-id)
              [{:name (tr "workspace.animation.remove-animation")
                :id "remove-animation"
                :disabled menu-locked?
                :handler #(st/emit! (dwa/remove-animation shape-id animation-id))}]

              :else
              (let [properties (if (some? property) [property] dwa/animatable-properties)]
                (cond-> [{:name (tr "workspace.shape.menu.copy")
                          :id "copy"
                          :handler #(st/emit! (dwa/copy-keyframes shape-id properties index))}
                         {:name (tr "workspace.shape.menu.paste")
                          :id "paste"
                          :disabled (not can-paste?)
                          :handler #(st/emit! (dwa/paste-keyframes shape-id))}
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
                         :handler #(st/emit! (dwa/delete-track shape-id property index))}))))))

        menu-looping?
        (and (some? (:property menu))
             (cta/looping? timeline (:shape-id menu) (:property menu) (:index menu)))]

    ;; Focus the easing editor when it opens, so Escape closes it.
    (mf/with-effect [easing]
      (some-> (mf/ref-val popover-ref) (dom/focus!)))

    ;; Closing the dock while its end is dragged stops the drag.
    (mf/with-effect []
      #(when-let [{:keys [listen-keys timer]} (mf/ref-val duration-drag-ref)]
         (js/clearTimeout timer)
         (run! events/unlistenByKey listen-keys)))

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
                  (when (plain-space? event)
                    (dom/prevent-default event)
                    (dom/stop-propagation event)
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
    ;; once per frame. The listener is not passive, so it can stop the
    ;; browser zoom.
    (mf/with-effect [(some? timeline)]
      (when-let [node (mf/ref-val scroll-ref)]
        (let [scale (volatile! 1)
              frame (volatile! nil)

              zoom!
              (fn []
                (let [{:keys [zoom set-zoom]} (mf/ref-val zoom-state-ref)]
                  (vreset! frame nil)
                  (set-zoom (* zoom @scale))
                  (vreset! scale 1)))

              on-wheel
              (fn [^js event]
                ;; the wheel fields are on the native event only
                (let [event (.getBrowserEvent event)]
                  (when (or (kbd/ctrl? event) (kbd/meta? event))
                    (dom/prevent-default event)
                    (let [wheel ^js (nw/normalize-wheel event)
                          delta (+ (.-pixelX wheel) (.-pixelY wheel))
                          step  (+ 1 (mth/abs (* zoom-per-pixel delta)))]
                      (vswap! scale * (if (pos? delta) (/ 1 step) step))
                      (when (nil? @frame)
                        (vreset! frame (js/requestAnimationFrame zoom!)))))))

              key (events/listen node "wheel" on-wheel #js {:passive false})]
          #(do (events/unlistenByKey key)
               (some-> @frame js/cancelAnimationFrame)))))

    ;; The dock sits in its own row of the workspace grid. Its height is
    ;; shared with the sidebars through a CSS variable on the workspace
    ;; node (see workspace.scss), capped at half of the window.
    (mf/with-layout-effect [dock-height]
      (let [workspace (dom/get-parent (mf/ref-val dock-ref))]
        (dom/set-css-property! workspace "--workspace-bottom-dock-height"
                               (dm/str "min(" dock-height "px, 50vh)"))
        #(dom/unset-css-property! workspace "--workspace-bottom-dock-height")))

    [:*
     [:section {:class (stl/css-case :timeline-dock true :recording recording?)
                :ref dock-ref
                :tab-index -1
                :on-key-down on-key-down}
      [:div {:class (stl/css :resize-area)
             :on-pointer-down on-resize-start
             :on-lost-pointer-capture on-resize-end
             :on-pointer-move on-resize-move}]
      (if (nil? timeline)
        [:> empty-state*]
        [:*
         [:> toolbar* {:timeline clock
                       :playing? playing?
                       :recording? recording?
                       :full-quality? (dwa/full-quality? anim)
                       :unit unit
                       :zoom zoom
                       :on-zoom-change on-zoom-change}]
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
                                   :unit unit}]]]

              [:div {:class (stl/css :ruler-corner)}
               [:button {:type "button"
                         :class (stl/css :chevron)
                         :title (if collapsed?
                                  (tr "workspace.animation.expand-all")
                                  (tr "workspace.animation.collapse-all"))
                         :on-click on-toggle-all-layers}
                [:> i/icon* {:icon-id (if collapsed? i/arrow-right i/arrow-down) :size "s"}]]]

              [:& (mf/provider snap-context) {:value snap}
               [:> marker-row* {:markers (:markers timeline)
                                :playhead playhead
                                :duration span
                                :axis-width axis-width
                                :editing marker-edit
                                :lane-ref marker-lane-ref
                                :on-edit on-edit-marker
                                :on-context-menu on-marker-context-menu}]]

              [:div {:class (stl/css :row-labels)}
               [:div {:class (stl/css :row-strip)
                      :ref strip-ref}
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
                                    :selected? (contains? selected id)
                                    :hovered? hovered?
                                    :on-toggle on-toggle-layer
                                    :on-select on-select-layer
                                    :on-context-menu on-context-menu
                                    :on-hover on-label-hover}]

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
                                         :hidden? (cta/slot-flag? timeline id :hidden property index)
                                         :locked? (cta/slot-flag? timeline id :locked property index)
                                         :value (cta/value-of (get values id) property index)
                                         :offset (when (nil? index)
                                                   (get-in position-offsets [id property]))
                                         :playhead playhead
                                         :hovered? hovered?
                                         :on-context-menu on-context-menu
                                         :on-select-layer on-select-keyframe-layer
                                         :on-hover on-label-hover}])))

                (when hint?
                  [:div {:class (stl/css :tracks-hint)
                         :style #js {"top" (dm/str (* row-count lanes/row-height) "px")}}
                   (tr "workspace.animation.tracks-hint")])]]]]]]]])]

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
