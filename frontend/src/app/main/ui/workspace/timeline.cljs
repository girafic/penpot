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
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.color :as clr]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.animation :as dwa]
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
   [app.main.ui.workspace.timeline.easing :refer [curve-icon* easing-editor*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.shape-icon :as usi]
   [app.util.text.ui :as txu]
   [cuerdas.core :as str]
   [goog.events :as events]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:timelines
  (l/derived #(get % :timelines) refs/workspace-page))

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

(defn- format-seconds
  "`time` (ms) in seconds, to the millisecond: `1.25`."
  [time]
  (dm/str (mth/precision (/ time 1000) 3)))

(defn- format-time
  "`time` (ms) in the time `unit` of the timeline: `1250 ms` or `1.25 s`."
  [time unit]
  (if (= unit :s)
    (dm/str (format-seconds time) " s")
    (dm/str time " ms")))

(defn- format-tick
  "Like Figma: whole seconds as `1s` and, below one-second steps, the
  milliseconds into the current second (`1s 100ms 200ms …`), or in
  seconds (`1s 1.1s 1.2s …`) when those are the time `unit`."
  [time step unit]
  (cond
    (zero? (mod time 1000)) (dm/str (quot time 1000) "s")
    (= unit :s)             (dm/str (format-seconds time) "s")
    (< step 1000)           (dm/str (mod time 1000) "ms")
    :else                   (dm/str (/ time 1000.0) "s")))

(def ^:private min-zoom
  "Smallest horizontal zoom. Below 1 the duration no longer fills the view."
  0.1)

(def ^:private max-zoom
  "Largest horizontal zoom of the time axis (1 fits the duration)."
  20)

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
  "Translate a pointer event over `node` into a time value (ms) clamped to
  [0, duration]."
  [event node duration]
  (let [rect     (dom/get-bounding-rect node)
        x        (:x (dom/get-client-position event))
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

(def ^:private snap-distance
  "Pixels within which a drag in the timeline snaps (see `snap-target`)."
  6)

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

(defn marquee-style
  "Position and size of the box of `marquee`, a rect in the tracks."
  [{:keys [left top right bottom]}]
  {:left   left
   :top    top
   :width  (- right left)
   :height (- bottom top)})

(defn marquee-drag?
  "True once the pointer has moved far enough to count as a box, not a click."
  [x0 y0 x y]
  (>= (max (mth/abs (- x x0)) (mth/abs (- y y0))) marquee-threshold))

(defn- closest
  [node selector]
  (when (some? node)
    (let [el (if (instance? js/Element node) node (.-parentElement node))]
      (when (some? el)
        (.closest ^js el selector)))))

(defn- marquee-origin?
  "Whether a box selection starts where `event` presses: on an empty spot
  of a lane or below the rows, not on a keyframe, bar, block or button."
  [event tracks]
  (let [target (dom/get-target event)]
    (and (or (identical? target tracks)
             (some? (closest target (str "." (stl/css :row-lane)))))
         (nil? (closest target "button"))
         (nil? (closest target (str "." (stl/css :layer-bar))))
         (nil? (closest target (str "." (stl/css :animation-block))))
         (nil? (closest target (str "." (stl/css :keyframe)))))))

(defn- lane-at
  "The property lane under the pointer of `event`, off its keyframes and
  easing buttons, as `[node shape-id property index]`. Looked up by
  position: the tracks capture the pointer to draw the box selection,
  which can make them the target of the click."
  [event]
  (let [{:keys [x y]} (dom/get-client-position event)
        target (.elementFromPoint js/document x y)
        node   (closest target (str "." (stl/css :property-lane)))]
    (when (and (some? node)
               (nil? (closest target "button"))
               (nil? (closest target (str "." (stl/css :keyframe)))))
      (when-let [shape-id (uuid/parse* (dom/get-data node "shape-id"))]
        [node
         shape-id
         (keyword (dom/get-data node "property"))
         (some-> (dom/get-data node "index") (d/parse-integer))]))))

(defn- in-tracks
  "Client `rect` in the coordinates of the tracks, whose client rect is
  `origin`: they stay put when the timeline scrolls."
  [{:keys [left top right bottom] :as rect} origin]
  (assoc rect
         :left   (- left (:left origin))
         :top    (- top (:top origin))
         :right  (- right (:left origin))
         :bottom (- bottom (:top origin))))

(defn- collect-keyframe-hits
  "The boxes of the keyframes that can be selected, in the tracks (see
  `in-tracks`)."
  [tracks-node origin]
  (when (some? tracks-node)
    (into []
          (keep (fn [node]
                  (when-let [sid (uuid/parse* (dom/get-data node "shape-id"))]
                    (when-let [kid (uuid/parse* (dom/get-data node "keyframe-id"))]
                      (-> (dom/get-bounding-rect node)
                          (in-tracks origin)
                          (assoc :shape-id sid
                                 :keyframe-id kid))))))
          ;; Locked keyframes cannot be selected.
          (array-seq (.querySelectorAll tracks-node (str "." (stl/css :keyframe)
                                                         ":not(." (stl/css :locked) ")"))))))

(defn- show-marquee!
  "Draw the box of `marquee` (a rect in the tracks), or hide it when nil.
  Set on the node, so dragging does not render the timeline again."
  [node marquee]
  (if (some? marquee)
    (let [{:keys [left top width height]} (marquee-style marquee)]
      (doto node
        (dom/set-css-property! "left" (dm/str left "px"))
        (dom/set-css-property! "top" (dm/str top "px"))
        (dom/set-css-property! "width" (dm/str width "px"))
        (dom/set-css-property! "height" (dm/str height "px"))
        (dom/set-css-property! "display" "block")))
    (dom/unset-css-property! node "display")))

(defn- capture-current-target
  [event]
  (when-let [node (dom/get-current-target event)]
    (.setPointerCapture ^js node (.-pointerId event))))

(defn- release-current-target
  [event]
  (when-let [node (dom/get-current-target event)]
    (when (.-pointerId event)
      (try
        (.releasePointerCapture ^js node (.-pointerId event))
        (catch :default _ nil)))))

(defn- animated-ids
  "Ids of the layers of `board-id` (itself included) that have keyframes,
  preset animations or animated children."
  [objects board-id timeline]
  (letfn [(walk [id]
            (let [child-ids (mapcat walk (dm/get-in objects [id :shapes]))]
              (if (or (seq (dm/get-in timeline [:tracks id :keyframes]))
                      (seq (dm/get-in timeline [:tracks id :animations]))
                      (seq child-ids))
                (cons id child-ids)
                child-ids)))]
    (set (walk board-id))))

(defn- timeline-rows
  "Flatten the layers of `board-id` (top-most first, like the layers
  panel) into rows: one per layer followed, while it is expanded, by one
  per preset animation, one per animated property and its children.
  Animated layers start expanded; `expanded` holds the ones the user
  toggled."
  [objects board-id timeline expanded]
  (let [animated (animated-ids objects board-id timeline)]
    (letfn [(walk [id depth]
              (when-let [shape (get objects id)]
                (let [children   (reverse (:shapes shape))
                      animations (dm/get-in timeline [:tracks id :animations])
                      slots      (track-slots (dm/get-in timeline [:tracks id :keyframes]))
                      expanded?  (get expanded id (contains? animated id))]
                  (cons {:type :layer
                         :id id
                         :depth depth
                         :shape shape
                         :expandable? (boolean (or (seq children) (seq animations) (seq slots)))
                         :expanded? expanded?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUB COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mf/defc keyframe*
  {::mf/private true}
  [{:keys [keyframe shape-id duration lane-node-ref selected? selected-kfs locked? on-select-layer]}]
  (let [time        (:time keyframe)
        kf-id       (:id keyframe)
        snap        (mf/use-ctx snap-context)
        ;; The drag in progress: its undo transaction, the timeline as it
        ;; was when it started and the keyframes it moves.
        dragging-ref (mf/use-ref nil)

        on-pointer-down
        (mf/use-fn
         (mf/deps locked? selected? selected-kfs shape-id kf-id time snap)
         (fn [event]
           ;; Only the left button drags; the right one opens the row menu.
           ;; A locked keyframe stays where it is.
           (when (and (dom/left-mouse? event) (not locked?))
             (let [undo-id (js/Symbol)]
               (dom/stop-propagation event)
               (dom/capture-pointer event)
               (mf/set-ref-val! dragging-ref
                                {:undo-id undo-id
                                 :base ((:timeline snap))
                                 :from time
                                 ;; a selected keyframe takes the others along
                                 :moving (if (and selected? (> (count selected-kfs) 1))
                                           selected-kfs
                                           #{{:shape-id shape-id :keyframe-id kf-id}})})
               (st/emit! (dwu/start-undo-transaction undo-id))))))

        on-pointer-move
        (mf/use-fn
         (mf/deps duration snap)
         (fn [event]
           (when-let [{:keys [base from moving]} (mf/ref-val dragging-ref)]
             (when-let [node (mf/ref-val lane-node-ref)]
               (let [t      (pointer->time event node duration)
                     target ((:target snap) t event {:keyframe-ids (into #{} (map :keyframe-id) moving)})]
                 ((:show snap) target)
                 (st/emit! (dwa/shift-keyframes-from base moving (- (or target t) from))))))))

        on-pointer-up
        (mf/use-fn
         (mf/deps snap)
         (fn [event]
           (when-let [{:keys [undo-id]} (mf/ref-val dragging-ref)]
             (dom/release-pointer event)
             (mf/set-ref-val! dragging-ref nil)
             ((:show snap) nil)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        on-double-click
        (mf/use-fn
         (mf/deps shape-id kf-id locked?)
         (fn [event]
           (dom/stop-propagation event)
           (when-not locked?
             (st/emit! (dwa/delete-keyframe shape-id kf-id)
                       (dwa/select-keyframe nil nil)))))

        ;; A locked keyframe only moves the playhead to it (and selects
        ;; its layer).
        on-click
        (mf/use-fn
         (mf/deps shape-id kf-id time locked? on-select-layer)
         (fn [event]
           (dom/stop-propagation event)
           (on-select-layer event shape-id)
           (if locked?
             (st/emit! (dwa/set-playhead time))
             (st/emit! (dwa/select-keyframe shape-id kf-id (kbd/shift? event))
                       (dwa/set-playhead time)))))]

    [:div {:class (stl/css-case :keyframe true :selected selected? :locked locked?)
           :data-shape-id (dm/str shape-id)
           :data-keyframe-id (dm/str kf-id)
           :style #js {"left" (time->pct time duration)}
           :title (format-time time (:unit snap))
           :on-pointer-down on-pointer-down
           :on-pointer-move on-pointer-move
           :on-pointer-up on-pointer-up
           :on-click on-click
           :on-double-click on-double-click}]))

(mf/defc segment*
  "The line between two keyframes. Hovering it shows a button with its
  easing (the one of the keyframe it starts at), which opens the easing
  editor."
  {::mf/private true}
  [{:keys [shape-id from to duration open? locked? on-edit-easing]}]
  (let [from-id (:id from)

        on-click
        (mf/use-fn
         (mf/deps shape-id from-id on-edit-easing)
         (fn [event]
           (dom/stop-propagation event)
           (on-edit-easing event shape-id from-id)))]

    [:div {:class (stl/css :segment)
           :style #js {"left" (dm/str "calc(" (time->pct (:time from) duration) " + 7px)")
                       "width" (dm/str "calc(" (time->pct (- (:time to) (:time from)) duration) " - 14px)")}}
     [:button {:class (stl/css-case :easing-btn true :open open?)
               :title (tr "workspace.animation.easing")
               :disabled locked?
               :on-click on-click}
      [:> curve-icon* {:easing (:easing from)
                       :is-hold (= :step (:interpolation from))}]]]))

(mf/defc property-lane*
  "The keyframes of one property, joined by lines. A double click on it
  adds a keyframe (see `lane-at`)."
  {::mf/private true}
  [{:keys [shape-id property index keyframes duration selected-kfs active? locked?
           easing-kf-id on-edit-easing on-select-layer]}]
  (let [node-ref (mf/use-ref nil)]
    [:div {:class (stl/css-case :property-lane true :active active?)
           :ref node-ref
           :data-shape-id (dm/str shape-id)
           :data-property (name property)
           :data-index (when (some? index) (dm/str index))}
     (for [[from to] (partition 2 1 keyframes)]
       [:> segment* {:key (dm/str (:id from) "-" (:id to))
                     :shape-id shape-id
                     :from from
                     :to to
                     :duration duration
                     :open? (= (:id from) easing-kf-id)
                     :locked? locked?
                     :on-edit-easing on-edit-easing}])
     (for [kf keyframes]
       [:> keyframe* {:key (dm/str (:id kf))
                      :keyframe kf
                      :shape-id shape-id
                      :duration duration
                      :selected? (contains? selected-kfs {:shape-id shape-id :keyframe-id (:id kf)})
                      :selected-kfs selected-kfs
                      :locked? locked?
                      :on-select-layer on-select-layer
                      :lane-node-ref node-ref}])]))

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

(mf/defc layer-row*
  "A layer of the board: tree label on the left and, when animated, a bar
  over the span of its keyframes (and its children's). Dragging the bar
  moves them; dragging its ends stretches them."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [shape depth expandable? expanded? selected? range duration
           on-toggle on-select on-context-menu on-bar-down]}]
  (let [shape-id (:id shape)
        hidden?  (true? (:hidden shape))
        blocked? (true? (:blocked shape))

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

        on-bar-pointer-down
        (mf/use-fn
         (mf/deps shape-id on-bar-down)
         (fn [event]
           (let [mode (-> (dom/get-current-target event)
                          (dom/get-data "mode")
                          (keyword))]
             (on-bar-down event shape-id mode))))]

    [:div {:class (stl/css-case :row true
                                :layer-row true
                                :selected selected?
                                :hidden-layer hidden?)
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
                        :on-toggle-lock on-toggle-blocking}]]

     [:div {:class (stl/css :row-lane)}
      (when-let [[start end] range]
        [:div {:class (stl/css-case :layer-bar true :active selected?)
               :style #js {"left" (time->pct start duration)
                           "width" (time->pct (- end start) duration)}
               :data-mode "move"
               :on-pointer-down on-bar-pointer-down}
         [:span {:class (stl/css :bar-handle :bar-start)
                 :data-mode "start"
                 :on-pointer-down on-bar-pointer-down}]
         [:span {:class (stl/css :bar-handle :bar-end)
                 :data-mode "end"
                 :on-pointer-down on-bar-pointer-down}]])]]))

(mf/defc animation-row*
  "A preset animation of a layer: its name on the left and a block over
  its span on the right. Dragging the block moves it; dragging its ends
  changes its start or its end. A hidden one is left out of playback and
  exports; a locked one is kept from edits."
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [shape-id animation depth duration selected?
           on-select on-context-menu on-animation-down]}]
  (let [animation-id (:id animation)
        start        (:start animation)
        hidden?      (true? (:hidden animation))
        locked?      (true? (:locked animation))
        label        (motion/animation-label animation)

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
           (on-context-menu event shape-id nil nil animation-id)))

        on-block-pointer-down
        (mf/use-fn
         (mf/deps shape-id animation on-animation-down)
         (fn [event]
           (let [mode (-> (dom/get-current-target event)
                          (dom/get-data "mode")
                          (keyword))]
             (on-animation-down event shape-id animation mode))))]

    [:div {:class (stl/css-case :row true :animation-row true :muted hidden?)
           :on-context-menu on-row-context-menu}
     [:div {:class (stl/css :row-label)
            :style #js {"paddingInlineStart" (dm/str (* depth 12) "px")}
            :on-click on-label-click}
      [:span {:class (stl/css :tree-line)}]
      [:span {:class (stl/css :property-name)} label]
      [:> row-toggles* {:hidden? hidden?
                        :locked? locked?
                        :on-toggle-visibility on-toggle-visibility
                        :on-toggle-lock on-toggle-lock}]]
     [:div {:class (stl/css :row-lane)}
      [:div {:class (stl/css-case :animation-block true :active selected? :locked locked?)
             :style #js {"left" (time->pct start duration)
                         "width" (time->pct (:duration animation) duration)}
             :title label
             :data-mode "move"
             :on-pointer-down on-block-pointer-down}
       [:span {:class (stl/css :bar-handle :bar-start)
               :data-mode "start"
               :on-pointer-down on-block-pointer-down}]
       [:span {:class (stl/css :animation-block-name)} label]
       [:span {:class (stl/css :bar-handle :bar-end)
               :data-mode "end"
               :on-pointer-down on-block-pointer-down}]]]]))

(mf/defc property-row*
  "An animated property of a layer: label, keyframe controls and value on
  the left, its keyframes on the right. A hidden one is left out of
  playback and exports; a locked one is kept from edits."
  {::mf/private true}
  [{:keys [timeline shape-id property index depth keyframes looping? hidden? locked?
           value offset playhead duration active? selected-kfs on-context-menu
           easing-kf-id on-edit-easing on-select-layer]}]
  (let [on-label-click
        (mf/use-fn
         (mf/deps shape-id on-select-layer)
         (fn [event]
           (on-select-layer event shape-id)))

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

    [:div {:class (stl/css-case :row true :property-row true :muted hidden?)
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
                        :on-toggle-lock on-toggle-lock}]]
     [:div {:class (stl/css :row-lane)}
      [:> property-lane* {:shape-id shape-id
                          :property property
                          :index index
                          :keyframes keyframes
                          :duration duration
                          :selected-kfs selected-kfs
                          :active? active?
                          :locked? locked?
                          :easing-kf-id easing-kf-id
                          :on-edit-easing on-edit-easing
                          :on-select-layer on-select-layer}]]]))

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
                 (let [t      (pointer->time event node duration)
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
           :title (dm/str name " · " (format-time time (:unit snap)))
           :on-pointer-down on-pointer-down
           :on-pointer-move on-pointer-move
           :on-pointer-up on-pointer-up
           :on-double-click on-double-click
           :on-context-menu on-marker-context-menu}
     [:span {:class (stl/css :marker-pin)}]
     (if editing?
       [:> marker-name-input* {:marker marker :on-done on-edit}]
       [:span {:class (stl/css :marker-name)} name])]))

(mf/defc marker-row*
  "The markers of the timeline, above the layers: named moments the drags
  snap to and the playhead jumps between. A double click on the lane adds
  one there; the controls go to the one before or after the playhead, or
  add one at it."
  {::mf/private true}
  [{:keys [markers playhead duration editing on-edit on-context-menu]}]
  (let [snap     (mf/use-ctx snap-context)
        lane-ref (mf/use-ref nil)
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

        ;; kept from the box selection and the double click of the lanes
        on-lane-pointer-down
        (mf/use-fn dom/stop-propagation)

        on-lane-double-click
        (mf/use-fn
         (mf/deps new-name duration snap)
         (fn [event]
           (dom/stop-propagation event)
           (when-let [node (mf/ref-val lane-ref)]
             (let [t (pointer->time event node duration)]
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
     [:div {:class (stl/css :row-lane :marker-lane)
            :ref lane-ref
            :title (tr "workspace.animation.marker-lane-hint")
            :on-pointer-down on-lane-pointer-down
            :on-double-click on-lane-double-click}
      (for [marker markers]
        [:> marker* {:key (dm/str (:id marker))
                     :marker marker
                     :duration duration
                     :lane-ref lane-ref
                     :editing? (= editing (:id marker))
                     :on-edit on-edit
                     :on-context-menu on-context-menu}])]]))

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
  {::mf/private true}
  [{:keys [timeline playing? recording? playhead unit zoom on-zoom-change]}]
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
       [:> i/icon* {:icon-id (playback-mode-icons playback)}]]]

     [:button {:type "button"
               :class (stl/css-case :rec-btn true :active recording?)
               :title (tr "workspace.animation.auto-keyframe")
               :aria-pressed recording?
               :on-click on-toggle-recording}
      [:span {:class (stl/css :rec-dot)}]
      (tr "workspace.animation.rec")]

     [:span {:class (stl/css :current-time)} (format-time playhead unit)]

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

(mf/defc timeline*
  []
  (let [anim       (mf/deref refs/workspace-animation)
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
        playhead   (get anim :playhead 0)
        playing?   (get anim :playing? false)
        unit       (dwa/time-unit anim)
        recording? (get anim :auto-keyframe? false)
        duration   (get timeline :duration 1000)

        selected-kfs (get anim :selected-kfs #{})

        ;; Easing editor of the segment starting at a keyframe, opened from
        ;; the button of the segment.
        easing*     (mf/use-state nil)
        easing      (deref easing*)
        easing-kf   (when (some? easing)
                      (d/seek #(= (:id %) (:keyframe-id easing))
                              (dm/get-in timeline [:tracks (:shape-id easing) :keyframes])))
        popover-ref (mf/use-ref nil)

        on-edit-easing
        (mf/use-fn
         (fn [event shape-id keyframe-id]
           (let [rect (dom/get-bounding-rect (dom/get-current-target event))]
             (reset! easing* {:shape-id shape-id
                              :keyframe-id keyframe-id
                              :top (:top rect)
                              :left (+ (:left rect) (/ (:width rect) 2))}))))

        on-close-easing
        (mf/use-fn #(reset! easing* nil))

        on-easing-key-down
        (mf/use-fn
         (mf/deps on-close-easing)
         (fn [event]
           (when (kbd/esc? event)
             (dom/stop-propagation event)
             (on-close-easing))))

        ruler-ref  (mf/use-ref nil)

        ;; The tick spacing depends on the ruler width.
        ruler-width* (mf/use-state 0)
        observe-ruler
        (r/use-resize-observer #(reset! ruler-width* (:width %2)))

        on-ruler-node
        (mf/use-fn
         (mf/deps observe-ruler)
         (fn [node]
           (mf/set-ref-val! ruler-ref node)
           (observe-ruler node)))

        ruler-width (deref ruler-width*)

        ;; Horizontal zoom of the time axis; ruler and lanes scroll together.
        zoom*      (mf/use-state 1)
        zoom       (deref zoom*)
        latest     (mf/with-memo [timeline]
                     (apply max 0 (map :time (mapcat :keyframes (vals (:tracks timeline))))))
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
        tick       (tick-step span ruler-width)
        ticks      (range 0 (inc span) (/ tick 2))

        ;; Drags snap to the playhead, the keyframes and the ends of the
        ;; animations and of the timeline, unless Ctrl/Cmd is held; a line
        ;; shows where. What they snap to is read when they move, so the
        ;; functions stay the same while the timeline changes.
        snap-state-ref (mf/use-ref nil)
        _              (mf/set-ref-val! snap-state-ref {:timeline timeline
                                                        :playhead playhead
                                                        :span span
                                                        :ruler-width ruler-width})
        snap-line*     (mf/use-state nil)
        snap-line      (deref snap-line*)

        snap-target
        (mf/use-fn
         (fn [time event {:keys [playhead?] :or {playhead? true} :as except}]
           (let [{:keys [timeline playhead span ruler-width]} (mf/ref-val snap-state-ref)]
             (when-not (kbd/mod? event)
               (cta/snap-time (cond-> (cta/snap-times timeline except)
                                playhead? (conj playhead))
                              time
                              (* snap-distance (/ span (max 1 ruler-width))))))))

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

        scroll-ref (mf/use-ref nil)
        ;; Scroll position to apply once the new zoom is laid out.
        anchor-ref (mf/use-ref nil)

        set-zoom
        (mf/use-fn
         (mf/deps zoom playhead span duration ruler-width)
         (fn [new-zoom]
           (let [new-zoom (mth/clamp new-zoom min-zoom max-zoom)
                 from     (content-zoom duration zoom span)
                 to       (content-zoom duration new-zoom (axis-span duration new-zoom latest))]
             ;; Keep the playhead at the same place on screen.
             (when-let [^js node (mf/ref-val scroll-ref)]
               (let [x (* (/ playhead (max 1 span)) ruler-width)]
                 (mf/set-ref-val! anchor-ref (+ (.-scrollLeft node)
                                                (* x (- (/ to from) 1))))))
             (reset! zoom* new-zoom))))

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
        rows       (mf/with-memo [objects board-id timeline expanded]
                     (when (some? timeline)
                       (timeline-rows objects board-id timeline expanded)))
        values     (mf/with-memo [timeline objects playhead]
                     (when (some? timeline)
                       (-> (cta/resolve-animations timeline objects)
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
         (mf/deps span ruler-width)
         (fn [event]
           (when-let [{:keys [base ids range mode x0]} (mf/ref-val bar-drag-ref)]
             (let [[start end] range
                   dx     (- (:x (dom/get-client-position event)) x0)
                   dt     (mth/round (* dx (/ span (max 1 ruler-width))))
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
         (mf/deps span ruler-width)
         (fn [event]
           (when-let [{:keys [base shape-id animation mode x0]} (mf/ref-val animation-drag-ref)]
             (let [start  (:start animation)
                   end    (cta/animation-end animation)
                   dx     (- (:x (dom/get-client-position event)) x0)
                   dt     (mth/round (* dx (/ span (max 1 ruler-width))))
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

        drag-ref   (mf/use-ref false)
        dock-ref   (mf/use-ref nil)

        ;; Dragging the top edge resizes the dock up to half of the window.
        {on-resize-start :on-pointer-down
         on-resize-end   :on-lost-pointer-capture
         on-resize-move  :on-pointer-move
         dock-height     :size}
        (r/use-resize-hook :timeline-dock 240 120 "0.5" :y true :bottom)

        on-scrub
        (mf/use-fn
         (mf/deps span)
         (fn [event]
           (let [node   (mf/ref-val ruler-ref)
                 t      (pointer->time event node span)
                 target (snap-target t event {:playhead? false})]
             (show-snap target)
             (st/emit! (dwa/set-playhead (or target t))))))

        on-ruler-down
        (mf/use-fn
         (mf/deps span)
         (fn [event]
           (dom/capture-pointer event)
           (mf/set-ref-val! drag-ref true)
           (on-scrub event)))

        on-ruler-move
        (mf/use-fn
         (mf/deps span)
         (fn [event]
           (when (mf/ref-val drag-ref)
             (on-scrub event))))

        on-ruler-up
        (mf/use-fn
         (fn [event]
           (dom/release-pointer event)
           (mf/set-ref-val! drag-ref false)
           (show-snap nil)))

        ;; Dragging the end of the timeline changes its duration, as one
        ;; undo step. The end follows the pointer, snapping like the other
        ;; drags; past the right end of the ruler the duration keeps growing,
        ;; the faster the farther the pointer is. A click on it without a
        ;; drag moves the playhead there, as on the rest of the ruler.
        duration-drag-ref (mf/use-ref nil)

        grow-duration
        (mf/use-fn
         (fn grow []
           (when-let [{:keys [x] :as drag} (mf/ref-val duration-drag-ref)]
             (let [rect (dom/get-bounding-rect (mf/ref-val ruler-ref))
                   over (- x (:right rect))]
               (if (pos? over)
                 (let [{:keys [timeline span]} (mf/ref-val snap-state-ref)]
                   (st/emit! (dwa/set-duration (+ (:duration timeline)
                                                  (mth/ceil (* span 0.05 (/ (min over 100) 100))))))
                   (mf/set-ref-val! duration-drag-ref (assoc drag :timer (js/setTimeout grow 50))))
                 (mf/set-ref-val! duration-drag-ref (dissoc drag :timer)))))))

        on-duration-move
        (mf/use-fn
         (mf/deps grow-duration)
         (fn [event]
           (when-let [{:keys [x0] :as drag} (mf/ref-val duration-drag-ref)]
             (let [x      (:x (dom/get-client-position event))
                   rect   (dom/get-bounding-rect (mf/ref-val ruler-ref))
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

        ;; Box selection of keyframes. Positions are kept in the tracks, so
        ;; the box and its keyframes stay together when the timeline
        ;; scrolls while dragging.
        tracks-ref       (mf/use-ref nil)
        marquee-ref      (mf/use-ref nil)
        marquee-drag-ref (mf/use-ref nil)

        pointer-in-tracks
        (mf/use-fn
         (fn [event]
           (let [tracks (mf/ref-val tracks-ref)
                 origin (dom/get-bounding-rect tracks)
                 label  (.querySelector ^js tracks (str "." (stl/css :row-label)))
                 ;; The box stays in the tracks, right of the labels: they
                 ;; cover the keyframes scrolled under them.
                 min-x  (if (some? label)
                          (- (:right (dom/get-bounding-rect label)) (:left origin))
                          0)
                 {:keys [x y]} (dom/get-client-position event)]
             [(mth/clamp (- x (:left origin)) min-x (:width origin))
              (mth/clamp (- y (:top origin)) 0 (:height origin))])))

        on-marquee-down
        (mf/use-fn
         (mf/deps selected-kfs pointer-in-tracks)
         (fn [event]
           (let [tracks (mf/ref-val tracks-ref)]
             (when (and (some? tracks)
                        (dom/left-mouse? event)
                        (marquee-origin? event tracks))
               (capture-current-target event)
               (mf/set-ref-val! marquee-drag-ref
                                {:start (pointer-in-tracks event)
                                 :additive? (kbd/shift? event)
                                 :base selected-kfs
                                 :hits (or (collect-keyframe-hits tracks (dom/get-bounding-rect tracks)) [])
                                 :moved? false
                                 :emitted nil})))))

        on-marquee-move
        (mf/use-fn
         (mf/deps pointer-in-tracks)
         (fn [event]
           (when-let [{:keys [start additive? base hits moved? emitted] :as drag}
                      (mf/ref-val marquee-drag-ref)]
             (let [[x0 y0] start
                   [x y]   (pointer-in-tracks event)]
               ;; Once a box, it follows the pointer even back to its start.
               (when (or moved? (marquee-drag? x0 y0 x y))
                 (let [rect     (rect-from-points x0 y0 x y)
                       picked   (keyframes-in-rect hits rect)
                       selected (if additive? (into base picked) picked)]
                   (show-marquee! (mf/ref-val marquee-ref) rect)
                   (mf/set-ref-val! marquee-drag-ref
                                    (assoc drag :moved? true :emitted selected))
                   (when (not= selected emitted)
                     (st/emit! (dwa/select-keyframes selected)))))))))

        on-marquee-up
        (mf/use-fn
         (fn [event]
           (when-let [{:keys [moved? additive?]} (mf/ref-val marquee-drag-ref)]
             (mf/set-ref-val! marquee-drag-ref nil)
             (release-current-target event)
             (show-marquee! (mf/ref-val marquee-ref) nil)
             (when-not moved?
               (when-not additive?
                 (st/emit! (dwa/select-keyframes [])))))))

        ;; A keyframe or a property selects its layer, so the design tab
        ;; changes that layer and not the one selected before.
        on-select-keyframe-layer
        (mf/use-fn
         (mf/deps selected)
         (fn [event shape-id]
           (select-layer event shape-id selected)))

        ;; A double click on the lane of a property adds a keyframe there.
        on-tracks-double-click
        (mf/use-fn
         (mf/deps span on-select-keyframe-layer)
         (fn [event]
           (when-let [[node shape-id property index] (lane-at event)]
             (on-select-keyframe-layer event shape-id)
             (st/emit! (dwa/add-keyframe-at shape-id property index
                                            (pointer->time event node span))))))

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

             (and (m-key? event) (not (kbd/mod? event)) (not (kbd/alt? event)))
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

    ;; Once the playhead rests, hovering and clicking on the canvas find
    ;; the shapes where the animation shows them.
    (mf/with-effect [playhead timeline objects playing?]
      (when-not playing?
        (let [timer (js/setTimeout #(st/emit! (dwa/index-preview)) 150)]
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

    ;; Ctrl/Cmd + wheel zooms the time axis. The listener is not passive,
    ;; so it can stop the browser zoom.
    (mf/with-effect [set-zoom zoom (some? timeline)]
      (when-let [node (mf/ref-val scroll-ref)]
        (let [on-wheel
              (fn [^js event]
                (when (or (.-ctrlKey event) (.-metaKey event))
                  (.preventDefault event)
                  (set-zoom (* zoom (if (pos? (.-deltaY event)) 0.9 1.1)))))
              key (events/listen node "wheel" on-wheel #js {:passive false})]
          #(events/unlistenByKey key))))

    ;; While playing a zoomed timeline, page the view to keep the playhead
    ;; visible.
    (mf/with-layout-effect [playhead zoom span]
      (when (and playing? (> zoom 1))
        (when-let [^js node (mf/ref-val scroll-ref)]
          (when-let [^js ruler (mf/ref-val ruler-ref)]
            (let [label (.-offsetLeft ruler)
                  x     (+ label (* (/ playhead (max 1 span)) (.-clientWidth ruler)))
                  left  (.-scrollLeft node)]
              (when (or (< x (+ left label))
                        (> x (+ left (.-clientWidth node))))
                (set! (.-scrollLeft node) (- x label))))))))

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
         [:> toolbar* {:timeline timeline
                       :playing? playing?
                       :recording? recording?
                       :playhead playhead
                       :unit unit
                       :zoom zoom
                       :on-zoom-change on-zoom-change}]

         [:div {:class (stl/css :timeline-body)}
          [:div {:class (stl/css :timeline-main)}
           [:div {:class (stl/css :timeline-scroll)
                  :ref scroll-ref}
            [:div {:class (stl/css :timeline-content)
                   :style #js {"--timeline-zoom" axis-zoom}}
             [:div {:class (stl/css :ruler-row)}
              [:div {:class (stl/css :ruler-corner)}
               [:button {:type "button"
                         :class (stl/css :chevron)
                         :title (if collapsed?
                                  (tr "workspace.animation.expand-all")
                                  (tr "workspace.animation.collapse-all"))
                         :on-click on-toggle-all-layers}
                [:> i/icon* {:icon-id (if collapsed? i/arrow-right i/arrow-down) :size "s"}]]]
              [:div {:class (stl/css :ruler)
                     :ref on-ruler-node
                     :on-pointer-down on-ruler-down
                     :on-pointer-move on-ruler-move
                     :on-pointer-up on-ruler-up}
               (for [t ticks
                     :let [major? (zero? (mod t tick))]]
                 [:div {:key (dm/str t)
                        :class (stl/css-case :tick true :major major?)
                        :style #js {"left" (time->pct t span)}}
                  (when (and major? (< t span))
                    [:span {:class (stl/css :tick-label)} (format-tick t tick unit)])])
               (when (> span duration)
                 [:div {:class (stl/css :duration-shade)
                        :style #js {"left" (time->pct duration span)}}])
               [:div {:class (stl/css :duration-handle)
                      :style #js {"left" (time->pct duration span)}
                      :title (tr "workspace.animation.drag-duration")
                      :on-pointer-down on-duration-down}]
               [:div {:class (stl/css :playhead)
                      :style #js {"left" (time->pct playhead span)}}
                [:div {:class (stl/css :playhead-handle)}]]]]

             [:div {:class (stl/css :tracks)
                    :ref tracks-ref
                    :on-pointer-down on-marquee-down
                    :on-pointer-move on-marquee-move
                    :on-pointer-up on-marquee-up
                    :on-lost-pointer-capture on-marquee-up
                    :on-double-click on-tracks-double-click}
              ;; playhead line spanning the tracks area
              [:div {:class (stl/css :playhead-axis)}
               (when (> span duration)
                 [:div {:class (stl/css :duration-shade)
                        :style #js {"left" (time->pct duration span)}}])
               [:div {:class (stl/css :playhead-line)
                      :style #js {"left" (time->pct playhead span)}}]
               (for [marker (:markers timeline)]
                 [:div {:key (dm/str (:id marker))
                        :class (stl/css :marker-line)
                        :style #js {"left" (time->pct (:time marker) span)}}])
               (when (some? snap-line)
                 [:div {:class (stl/css :snap-line)
                        :style #js {"left" (time->pct snap-line span)}}])]

              [:& (mf/provider snap-context) {:value snap}
               [:> marker-row* {:markers (:markers timeline)
                                :playhead playhead
                                :duration span
                                :editing marker-edit
                                :on-edit on-edit-marker
                                :on-context-menu on-marker-context-menu}]
               (for [{:keys [type id depth] :as row} rows]
                 (case type
                   :layer
                   [:> layer-row* {:key (dm/str id)
                                   :shape (:shape row)
                                   :depth depth
                                   :expandable? (:expandable? row)
                                   :expanded? (:expanded? row)
                                   :selected? (contains? selected id)
                                   :range (:range row)
                                   :duration span
                                   :on-toggle on-toggle-layer
                                   :on-select on-select-layer
                                   :on-context-menu on-context-menu
                                   :on-bar-down on-bar-down}]

                   :animation
                   (let [animation (:animation row)]
                     [:> animation-row* {:key (dm/str id "-" (:id animation))
                                         :shape-id id
                                         :animation animation
                                         :depth depth
                                         :duration span
                                         :selected? (contains? selected id)
                                         :on-select on-select-layer
                                         :on-context-menu on-context-menu
                                         :on-animation-down on-animation-down}])

                   (let [property (:property row)
                         index    (:index row)]
                     [:> property-row* {:key (dm/str id "-" (name property) "-" index)
                                        :timeline timeline
                                        :shape-id id
                                        :property property
                                        :index index
                                        :depth depth
                                        :keyframes (cta/property-keyframes timeline id property index)
                                        :looping? (cta/looping? timeline id property index)
                                        :hidden? (cta/slot-flag? timeline id :hidden property index)
                                        :locked? (cta/slot-flag? timeline id :locked property index)
                                        :value (cta/value-of (get values id) property index)
                                        :offset (when (nil? index)
                                                  (get-in position-offsets [id property]))
                                        :playhead playhead
                                        :duration span
                                        :active? (contains? selected id)
                                        :selected-kfs selected-kfs
                                        :on-context-menu on-context-menu
                                        :easing-kf-id (when (= (:shape-id easing) id)
                                                        (:keyframe-id easing))
                                        :on-edit-easing on-edit-easing
                                        :on-select-layer on-select-keyframe-layer}])))]

              [:div {:class (stl/css :marquee)
                     :ref marquee-ref}]

              (when (empty? (:tracks timeline))
                [:div {:class (stl/css :tracks-hint)}
                 (tr "workspace.animation.tracks-hint")])]]]]]])]

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
