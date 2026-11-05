;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer.shapes
  "The main container for a frame in viewer mode"
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.tilt :as gtl]
   [app.common.types.shape.interactions :as ctsi]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.preview :as dvp]
   [app.main.data.viewer.scroll :as dvs]
   [app.main.refs :as refs]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.shapes.bool :as bool]
   [app.main.ui.shapes.circle :as circle]
   [app.main.ui.shapes.frame :as frame]
   [app.main.ui.shapes.group :as group]
   [app.main.ui.shapes.image :as image]
   [app.main.ui.shapes.path :as path]
   [app.main.ui.shapes.rect :as rect]
   [app.main.ui.shapes.shape :refer [shape-container]]
   [app.main.ui.shapes.svg-raw :as svg-raw]
   [app.main.ui.shapes.text :as text]
   [app.main.ui.viewer.drag-scroll :as dds]
   [app.main.ui.viewer.viewport-common :as vpc]
   [app.util.dom :as dom]
   [app.util.object :as obj]
   [app.util.timers :as tm]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def base-frame-ctx (mf/create-context nil))
(def frame-offset-ctx (mf/create-context nil))

(def ^:private ref:viewer-show-interactions
  (l/derived :show-interactions refs/viewer-local))

(def ^:private ref:preview-hidden
  (l/derived dvp/preview-hidden? st/state))

(defn- find-relative-to-base-frame
  [shape objects overlays-ids base-frame]
  (cond
    (cfh/frame-shape? shape) shape
    (or (empty? overlays-ids) (nil? shape) (cfh/root? shape)) base-frame
    :else (find-relative-to-base-frame (cfh/get-parent objects (:id shape)) objects overlays-ids base-frame)))

(defn overlay-anchor
  "What an overlay placed by the layer `relative-to-id` of `objects` holds
  to as the page of the screen `base-frame` scrolls (see
  `app.main.ui.viewer/viewer-wrapper`): `:page`, by a layer of the screen
  that scrolls with it (not one fixed when scrolling, see
  `vpc/screen-fixed?`), and it scrolls with it; `:screen`, by the screen
  itself, and it stays on the screen, up from its first one at the side
  it snaps to on a lower one (see `app.main.ui.viewer/overlay-on-screen`);
  `:layer`, by a fixed layer or a layer of one of the `overlays` that
  stays, and it stays too. By a layer of an overlay that scrolls with the
  page it does as well."
  [objects relative-to-id base-frame overlays]
  (let [base-id (:id base-frame)
        parents (when (contains? objects relative-to-id)
                  (cfh/get-parent-ids objects relative-to-id))]
    (cond
      (or (nil? parents) (= relative-to-id base-id))
      :screen

      (some #(= % base-id) parents)
      (if (vpc/screen-fixed? objects relative-to-id) :layer :page)

      :else
      (let [ids (set (cons relative-to-id parents))]
        (if (= :page (:anchor (d/seek #(contains? ids (:id %)) overlays)))
          :page
          :layer)))))

(defn stuck-place
  "Where an overlay placed by the layer `relative-to-id` of `objects` goes
  when a sticky layer of the screen `base-frame` that is or holds it
  sticks as its page scrolled `scroll` units down (see
  `vpc/screen-stuck-move`): `:objects` with that one where it shows, and
  the overlay holds to it (`:anchor` `:layer`, its place on the first
  screen) on a board that shows as a screen (`screen?`, see
  `app.main.ui.viewer/stays-on-screen?`), else to the page. Nil when none
  sticks."
  [objects relative-to-id base-frame scroll screen?]
  (when-let [{:keys [id dy]} (vpc/screen-stuck-move objects (:id base-frame) relative-to-id scroll)]
    (let [move (gpt/point 0 (if screen? (- dy scroll) dy))]
      {:objects (dvs/shift-objects objects (into {}
                                                 (map #(vector % move))
                                                 (cfh/get-children-ids-with-self objects id)))
       :anchor  (if screen? :layer :page)})))

(defn- stuck-place-now
  "`stuck-place` as the page of the screen `base-frame` scrolled now."
  [objects relative-to-id base-frame]
  (let [state  @st/state
        zoom   (dm/get-in state [:viewer-local :zoom] 1)
        scroll (/ (or (dom/get-scroll-pos (dom/get-element "viewer-section")) 0) zoom)]
    (stuck-place objects relative-to-id base-frame scroll
                 (or (some? (dv/device-shown state)) (some? (dvs/screen-h base-frame))))))

(defn- ignore-frame-shape
  [shape objects manual?]
  (let [shape (cond-> shape ;; When the the interaction is not manual and its origin is a frame,
                ;; we need to ignore it on all the find-frame calculations
                (and (:frame-id shape) (not manual?))
                (assoc :type :rect))
        objects (assoc objects (:id shape) shape)]
    [shape objects]))

(defn- activate-interaction
  [interaction shape base-frame frame-offset objects overlays]
  (case (:action-type interaction)
    :navigate
    (when-let [frame-id (:destination interaction)]
      (let [viewer-section (dom/get-element "viewer-section")
            scroll (if (:preserve-scroll interaction)
                     (dom/get-scroll-pos viewer-section)
                     0)]
        (st/emit! (dv/set-nav-scroll scroll)
                  (dv/go-to-frame frame-id (:animation interaction)))))

    :open-overlay
    (let [manual?                    (= :manual (:overlay-pos-type interaction))
          [shape objects]            (ignore-frame-shape shape objects manual?)
          dest-frame-id              (:destination interaction)
          dest-frame                 (dvs/design-box (get objects dest-frame-id))
          relative-to-id             (if manual?
                                       (if (= (:type shape) :frame) ;; manual interactions are always from "self"
                                         (:frame-id shape)
                                         (:id shape))
                                       (:position-relative-to interaction))
          ;; by a sticky layer of the screen where it sticks
          stuck                      (stuck-place-now objects relative-to-id base-frame)
          objects                    (or (:objects stuck) objects)
          shape                      (get objects (:id shape) shape)
          relative-to-shape          (dvs/design-box (or (get objects relative-to-id) base-frame))
          close-click-outside        (:close-click-outside interaction)
          background-overlay         (:background-overlay interaction)
          overlays-ids               (set (map :id overlays))
          relative-to-base-frame     (find-relative-to-base-frame relative-to-shape objects overlays-ids base-frame)
          ;; a layer fixed in a container scrolls with the screen
          fixed-base?                (vpc/screen-fixed? objects relative-to-id)
          [position snap-to]         (ctsi/calc-overlay-position interaction
                                                                 shape
                                                                 objects
                                                                 relative-to-shape
                                                                 relative-to-base-frame
                                                                 dest-frame
                                                                 frame-offset)]
      (when dest-frame-id
        (st/emit! (dv/open-overlay dest-frame-id
                                   position
                                   snap-to
                                   close-click-outside
                                   background-overlay
                                   (:animation interaction)
                                   fixed-base?
                                   (or (:anchor stuck)
                                       (overlay-anchor objects relative-to-id base-frame overlays))))))

    :toggle-overlay
    (let [manual?                    (= :manual (:overlay-pos-type interaction))
          [shape objects]            (ignore-frame-shape shape objects manual?)
          dest-frame-id              (:destination interaction)
          dest-frame                 (dvs/design-box (get objects dest-frame-id))
          relative-to-id             (if (= :manual (:overlay-pos-type interaction))
                                       (if (= (:type shape) :frame) ;; manual interactions are always from "self"
                                         (:frame-id shape)
                                         (:id shape))
                                       (:position-relative-to interaction))
          stuck                      (stuck-place-now objects relative-to-id base-frame)
          objects                    (or (:objects stuck) objects)
          shape                      (get objects (:id shape) shape)
          relative-to-shape          (dvs/design-box (or (get objects relative-to-id) base-frame))
          overlays-ids               (set (map :id overlays))
          relative-to-base-frame     (find-relative-to-base-frame relative-to-shape objects overlays-ids base-frame)
          fixed-base?                (cfh/fixed? objects (:id base-frame))
          [position snap-to]         (ctsi/calc-overlay-position interaction
                                                                 shape
                                                                 objects
                                                                 relative-to-shape
                                                                 relative-to-base-frame
                                                                 dest-frame
                                                                 frame-offset)

          close-click-outside        (:close-click-outside interaction)
          background-overlay         (:background-overlay interaction)]
      (when dest-frame-id
        (st/emit! (dv/toggle-overlay dest-frame-id
                                     position
                                     snap-to
                                     close-click-outside
                                     background-overlay
                                     (:animation interaction)
                                     fixed-base?
                                     (or (:anchor stuck)
                                         (overlay-anchor objects relative-to-id base-frame overlays))))))

    :close-overlay
    (let [dest-frame-id (or (:destination interaction)
                            (if (and (= (:type shape) :frame)
                                     (some #(= (:id %) (:id shape)) overlays))
                              (:id shape)
                              (:frame-id shape)))]
      (st/emit! (dv/close-overlay dest-frame-id (:animation interaction))))

    :prev-screen
    ;; the preview of the prototype in the workspace, in its own history
    (st/emit! (if (dv/preview? @st/state)
                (dvp/preview-step :back)
                (rt/nav-back-local)))

    :open-url
    (st/emit! (dom/open-new-window (:url interaction)))

    ;; the timeline of the board on screen (Penpot Motion)
    :play-to-marker
    (when-let [board-id (:id base-frame)]
      (st/emit! (dv/play-to-marker board-id interaction)))

    nil))

;; Perform the opposite action of an interaction, if possible
(defn- deactivate-interaction
  [interaction shape base-frame frame-offset objects overlays]
  (case (:action-type interaction)
    :open-overlay
    (let [frame-id (or (:destination interaction)
                       (if (= (:type shape) :frame)
                         (:id shape)
                         (:frame-id shape)))]
      (st/emit! (dv/close-overlay frame-id)))

    :toggle-overlay
    (let [manual?                    (= :manual (:overlay-pos-type interaction))
          [shape objects]            (ignore-frame-shape shape objects manual?)
          dest-frame-id              (:destination interaction)
          dest-frame                 (dvs/design-box (get objects dest-frame-id))
          relative-to-id             (if (= :manual (:overlay-pos-type interaction))
                                       (if (= (:type shape) :frame) ;; manual interactions are always from "self"
                                         (:frame-id shape)
                                         (:id shape))
                                       (:position-relative-to interaction))
          relative-to-shape          (dvs/design-box (or (get objects relative-to-id) base-frame))
          overlays-ids               (set (map :id overlays))
          relative-to-base-frame     (find-relative-to-base-frame relative-to-shape objects overlays-ids base-frame)
          fixed-base?                (cfh/fixed? objects (:id base-frame))
          [position snap-to]         (ctsi/calc-overlay-position interaction
                                                                 shape
                                                                 objects
                                                                 relative-to-shape
                                                                 relative-to-base-frame
                                                                 dest-frame
                                                                 frame-offset)

          close-click-outside        (:close-click-outside interaction)
          background-overlay         (:background-overlay interaction)]
      (when dest-frame-id
        (st/emit! (dv/toggle-overlay dest-frame-id
                                     position
                                     snap-to
                                     close-click-outside
                                     background-overlay
                                     (:animation interaction)
                                     fixed-base?
                                     (overlay-anchor objects relative-to-id base-frame overlays)))))


    :close-overlay
    (let [manual?                    (= :manual (:overlay-pos-type interaction))
          [shape objects]            (ignore-frame-shape shape objects manual?)
          dest-frame-id              (:destination interaction)
          dest-frame                 (dvs/design-box (get objects dest-frame-id))
          relative-to-id             (if (= :manual (:overlay-pos-type interaction))
                                       (if (= (:type shape) :frame) ;; manual interactions are always from "self"
                                         (:frame-id shape)
                                         (:id shape))
                                       (:position-relative-to interaction))
          relative-to-shape          (dvs/design-box (or (get objects relative-to-id) base-frame))
          close-click-outside        (:close-click-outside interaction)
          background-overlay         (:background-overlay interaction)
          overlays-ids               (set (map :id overlays))
          relative-to-base-frame     (find-relative-to-base-frame relative-to-shape objects overlays-ids base-frame)
          fixed-base?                (cfh/fixed? objects (:id base-frame))
          [position snap-to]         (ctsi/calc-overlay-position interaction
                                                                 shape
                                                                 objects
                                                                 relative-to-shape
                                                                 relative-to-base-frame
                                                                 dest-frame
                                                                 frame-offset)]
      (when dest-frame-id
        (st/emit! (dv/open-overlay dest-frame-id
                                   position
                                   snap-to
                                   close-click-outside
                                   background-overlay
                                   (:animation interaction)
                                   fixed-base?
                                   (overlay-anchor objects relative-to-id base-frame overlays)))))
    nil))

(defn- touch?
  "Whether `event` comes from a finger on a real touch screen."
  [event]
  (= "touch" (.-pointerType event)))

(defn- on-pointer-down
  [event shape base-frame frame-offset objects overlays]
  (let [;; on a touch device, or by a finger, a click acts on release (see
        ;; `on-pointer-up`)
        click?       (not (or (dds/defer-taps?) (touch? event)))
        interactions (->> (:interactions shape)
                          (filter #(or (and click? (= (:event-type %) :click))
                                       (= (:event-type %) :mouse-press))))]
    (when (seq interactions)
      (dom/stop-propagation event)
      (doseq [interaction interactions]
        (activate-interaction interaction shape base-frame frame-offset objects overlays)))))

(defn- on-pointer-up
  [event shape base-frame frame-offset objects overlays]
  (let [releases (->> (:interactions shape)
                      (filter #(= (:event-type %) :mouse-press)))
        ;; a tap on a touch device, or by a finger: a press that did not
        ;; drag
        taps     (when (and (or (dds/defer-taps?) (touch? event)) (not (dds/dragged?)))
                   (->> (:interactions shape)
                        (filter #(= (:event-type %) :click))))]
    (when (or (seq releases) (seq taps))
      (dom/stop-propagation event)
      (doseq [interaction releases]
        (deactivate-interaction interaction shape base-frame frame-offset objects overlays))
      (doseq [interaction taps]
        (activate-interaction interaction shape base-frame frame-offset objects overlays)))))

(defn- on-pointer-enter
  [event shape base-frame frame-offset objects overlays]
  (let [interactions (->> (:interactions shape)
                          (filter #(or (= (:event-type %) :mouse-enter)
                                       (= (:event-type %) :mouse-over))))]
    (when (seq interactions)
      (dom/stop-propagation event)
      (doseq [interaction interactions]
        (activate-interaction interaction shape base-frame frame-offset objects overlays)))))

(defn- on-pointer-leave
  [event shape base-frame frame-offset objects overlays]
  (let [interactions     (->> (:interactions shape)
                              (filter #(= (:event-type %) :mouse-leave)))
        interactions-inv (->> (:interactions shape)
                              (filter #(= (:event-type %) :mouse-over)))]
    (when (or (seq interactions) (seq interactions-inv))
      (dom/stop-propagation event)
      (doseq [interaction interactions]
        (activate-interaction interaction shape base-frame frame-offset objects overlays))
      (doseq [interaction interactions-inv]
        (deactivate-interaction interaction shape base-frame frame-offset objects overlays)))))

(defn- on-load
  "The timers of the after-delay interactions of `shape`, from now; none
  while the preview of the prototype is `hidden?` (see
  `dvp/preview-hidden`): shown again, they start over."
  [shape base-frame frame-offset objects overlays hidden?]
  (let [interactions (when-not hidden?
                       (->> (:interactions shape)
                            (filter #(= (:event-type %) :after-delay))))]
    (loop [interactions (seq interactions)
           sems []]
      (if-let [interaction (first interactions)]
        (let [sem (tm/schedule (:delay interaction)
                               #(activate-interaction interaction shape base-frame frame-offset objects overlays))]
          (recur (next interactions)
                 (conj sems sem)))
        sems))))

(mf/defc interaction
  [{:keys [shape interactions show-interactions]}]
  (let [{:keys [x y width height]} (:selrect shape)]
    (when-not (empty? interactions)
      [:rect {:x (- x 1)
              :y (- y 1)
              :width (+ width 2)
              :height (+ height 2)
              :fill "var(--color-accent-tertiary)"
              :stroke "var(--color-accent-tertiary)"
              :stroke-width (if show-interactions 1 0)
              :fill-opacity (if show-interactions 0.2 0)
              :pointer-events "none"
              :transform (gsh/transform-str shape)}])))

;; --- WASM viewer hotspots ---
;; In WASM viewer mode the frame pixels come from a WASM snapshot, so we don't
;; render the SVG visuals at all. We only render the actionable areas (hotspots)
;; on top of the image: a transparent hit/highlight rect per interactive shape,
;; wired to the same interaction handlers as the regular SVG tree. A shape
;; tilted or in depth (Penpot Motion, see `app.common.geom.tilt`) takes the
;; outline it shows at.

(defn hotspot-ids
  "The board `frame-id` of `objects` and the shapes in it, in the order
  they are drawn, the bottom one first: each shape before what it holds,
  the layers of a 3D space by their depth (see `gtl/depth-sorted`), those
  of the 3D space groups in it among them and those groups under them all
  (see `gtl/space-layers`), so that the hotspot on top is where what shows
  on top is."
  [objects frame-id]
  (letfn [(walk [id]
            (let [shape (get objects id)]
              (if (gtl/three-d-space? shape)
                (let [[layers groups] (gtl/space-layers objects (:shapes shape))]
                  (concat groups (mapcat #(cons % (walk %)) (gtl/depth-sorted objects shape layers))))
                (mapcat #(cons % (walk %)) (:shapes shape)))))]
    (cons frame-id (walk frame-id))))

(defn- hidden-back?
  "Whether `shape` of `objects`, or a layer it is in, hides its back and
  shows it: none of it shows (see `gtl/back-facing?`)."
  [objects shape]
  (loop [shape shape]
    (cond
      (or (nil? shape) (cfh/root? shape)) false
      (and (:backface-hidden shape) (gtl/back-facing? objects shape)) true
      :else (recur (get objects (:parent-id shape))))))

(defn hotspot-outline
  "Where the hotspot of `shape` of `objects` is: `:box`, its box as it
  lies, when nothing tilts it; the corners it shows at through the tilts
  and depths of it and the layers it is in (see `gtl/projected-points`);
  nil when there is nothing of it to click: its back is hidden, or it
  reaches the viewer."
  [objects shape]
  (if (nil? (gtl/projection objects shape))
    :box
    (when-not (hidden-back? objects shape)
      (gtl/projected-points objects shape))))

(mf/defc hotspot*
  [{:keys [shape objects all-objects clip]}]
  (let [base-frame   (mf/use-ctx base-frame-ctx)
        frame-offset (mf/use-ctx frame-offset-ctx)
        show-interactions (mf/deref ref:viewer-show-interactions)
        overlays     (mf/deref refs/viewer-overlays)
        hidden?      (mf/deref ref:preview-hidden)
        interactions (:interactions shape)
        {:keys [x y width height]} (:selrect shape)

        on-pd (mf/use-fn (mf/deps shape base-frame frame-offset all-objects overlays)
                         #(on-pointer-down % shape base-frame frame-offset all-objects overlays))
        on-pu (mf/use-fn (mf/deps shape base-frame frame-offset all-objects overlays)
                         #(on-pointer-up % shape base-frame frame-offset all-objects overlays))
        on-pe (mf/use-fn (mf/deps shape base-frame frame-offset all-objects overlays)
                         #(on-pointer-enter % shape base-frame frame-offset all-objects overlays))
        on-pl (mf/use-fn (mf/deps shape base-frame frame-offset all-objects overlays)
                         #(on-pointer-leave % shape base-frame frame-offset all-objects overlays))

        outline (mf/with-memo [objects shape]
                  (hotspot-outline (or objects all-objects) shape))]

    (mf/with-effect [hidden?]
      (let [sems (on-load shape base-frame frame-offset all-objects overlays hidden?)]
        (partial run! tm/dispose! sems)))

    (when (some? outline)
      [:g {:style {:cursor (when (ctsi/actionable? interactions) "pointer")}
           ;; in a board that scrolls its content: within its box (see
           ;; `viewport.wasm/wasm-hotspots-svg*`)
           :clip-path clip
           :on-pointer-down on-pd
           :on-pointer-up on-pu
           :on-pointer-enter on-pe
           :on-pointer-leave on-pl}
       (if (= :box outline)
         [:rect {:x (- x 1)
                 :y (- y 1)
                 :width (+ width 2)
                 :height (+ height 2)
                 :fill "var(--color-accent-tertiary)"
                 :stroke "var(--color-accent-tertiary)"
                 :stroke-width (if show-interactions 1 0)
                 :fill-opacity (if show-interactions 0.2 0)
                 ;; This rect is the only hit target, so it must always capture
                 ;; pointer events even when fully transparent.
                 :pointer-events "all"
                 :transform (gsh/transform-str shape)}]
         [:path {:d (str "M" (str/join " L" (map #(str (:x %) " " (:y %)) outline)) " Z")
                 :fill "var(--color-accent-tertiary)"
                 :stroke "var(--color-accent-tertiary)"
                 :stroke-width (if show-interactions 1 0)
                 :fill-opacity (if show-interactions 0.2 0)
                 :pointer-events "all"}])])))

(defn hotspot-shapes
  "The shapes of the board `frame-id` of `objects` that get a hotspot
  (see `frame-hotspots*`), those with interactions, in the order of
  `hotspot-ids`; with `shape-filter` (of an id), those it is true of."
  [objects frame-id shape-filter]
  (->> (cond->> (hotspot-ids objects frame-id)
         shape-filter (filter shape-filter))
       (keep #(get objects %))
       (filter (fn [s] (and (not (:hidden s))
                            (seq (:interactions s)))))))

(mf/defc frame-hotspots*
  "Renders interaction hotspots for a frame subtree (WASM viewer mode).
  `objects` must be the prepared (vbox-space) objects and `frame` the prepared
  frame; only shapes with interactions produce a hotspot.

  Optional `shape-filter` is a predicate that receives the shape id and returns
  true when it should be included (used to split fixed-scroll vs normal layers).
  `clips`, the clip path of each shape in a board that scrolls its content."
  [{:keys [objects all-objects shape-filter frame clips]}]
  (let [all-objects (or all-objects objects)
        hotspots    (hotspot-shapes objects (:id frame) shape-filter)]
    [:g {}
     (for [shape hotspots]
       [:> hotspot* {:key (str (:id shape))
                     :shape shape
                     :objects objects
                     :all-objects all-objects
                     :clip (get clips (:id shape))}])]))


;; TODO: use-memo use-fn

(defn generic-wrapper-factory
  "Wrap some svg shape and add interaction controls"
  [component]
  (mf/fnc generic-wrapper
    {::mf/wrap-props false}
    [props]
    (let [shape              (unchecked-get props "shape")
          childs             (unchecked-get props "childs")
          frame              (unchecked-get props "frame")
          objects            (unchecked-get props "objects")
          all-objects        (or (unchecked-get props "all-objects") objects)
          base-frame         (mf/use-ctx base-frame-ctx)
          frame-offset       (mf/use-ctx frame-offset-ctx)
          show-interactions  (mf/deref ref:viewer-show-interactions)
          overlays           (mf/deref refs/viewer-overlays)
          hidden?            (mf/deref ref:preview-hidden)
          interactions       (:interactions shape)
          svg-element?       (and (= :svg-raw (:type shape))
                                  (not= :svg (get-in shape [:content :tag])))

          ;; The objects parameter has the shapes that we must draw. It may be a subset of
          ;; all-objects in some cases (e.g. if there are fixed elements). But for interactions
          ;; handling we need access to all objects inside the page.

          on-pointer-down
          (mf/use-fn (mf/deps shape base-frame frame-offset all-objects)
                     #(on-pointer-down % shape base-frame frame-offset all-objects overlays))

          on-pointer-up
          (mf/use-fn (mf/deps shape base-frame frame-offset all-objects)
                     #(on-pointer-up % shape base-frame frame-offset all-objects overlays))

          on-pointer-enter
          (mf/use-fn (mf/deps shape base-frame frame-offset all-objects)
                     #(on-pointer-enter % shape base-frame frame-offset all-objects overlays))

          on-pointer-leave
          (mf/use-fn (mf/deps shape base-frame frame-offset all-objects)
                     #(on-pointer-leave % shape base-frame frame-offset all-objects overlays))]

      (mf/with-effect [hidden?]
        (let [sems (on-load shape base-frame frame-offset objects overlays hidden?)]
          (partial run! tm/dispose! sems)))

      (if-not svg-element?
        [:> shape-container {:shape shape
                             :cursor (when (ctsi/actionable? interactions) "pointer")
                             :on-pointer-down on-pointer-down
                             :on-pointer-up on-pointer-up
                             :on-pointer-enter on-pointer-enter
                             :on-pointer-leave on-pointer-leave}

         [:& component {:shape shape
                        :frame frame
                        :childs childs
                        :is-child-selected? true
                        :objects objects}]

         [:& interaction {:shape shape
                          :interactions interactions
                          :show-interactions show-interactions}]]

        ;; Don't wrap svg elements inside a <g> otherwise some can break
        [:& component {:shape shape
                       :frame frame
                       :childs childs
                       :objects objects}]))))

(defn frame-wrapper
  [shape-container]
  (generic-wrapper-factory (frame/frame-shape shape-container)))

(defn group-wrapper
  [shape-container]
  (generic-wrapper-factory (group/group-shape shape-container)))

(defn bool-wrapper
  [shape-container]
  (generic-wrapper-factory (bool/bool-shape shape-container)))

(defn svg-raw-wrapper
  [shape-container]
  (generic-wrapper-factory (svg-raw/svg-raw-shape shape-container)))

(defn rect-wrapper
  []
  (generic-wrapper-factory rect/rect-shape))

(defn image-wrapper
  []
  (generic-wrapper-factory image/image-shape))

(defn path-wrapper
  []
  (generic-wrapper-factory path/path-shape))

(defn text-wrapper
  []
  (generic-wrapper-factory text/text-shape))

(defn circle-wrapper
  []
  (generic-wrapper-factory circle/circle-shape))

(declare shape-container-factory)

(defn frame-container-factory
  [objects all-objects]
  (let [shape-container (shape-container-factory objects all-objects)
        frame-wrapper   (frame-wrapper shape-container)
        lookup-xf       (keep (d/getf objects))]
    (mf/fnc frame-container
      {::mf/wrap-props false}
      [props]
      (let [shape  (unchecked-get props "shape")
            childs (into [] lookup-xf (:shapes shape))
            props  (obj/merge props
                              #js {:childs childs
                                   :objects objects
                                   :all-objects all-objects})]
        [:> frame-wrapper props]))))

(defn group-container-factory
  [objects all-objects]
  (let [shape-container (shape-container-factory objects all-objects)
        group-wrapper (group-wrapper shape-container)]
    (mf/fnc group-container
      {::mf/wrap-props false}
      [props]
      (let [childs   (mapv #(get objects %) (:shapes (unchecked-get props "shape")))
            props    (obj/merge! #js {} props
                                 #js {:childs childs
                                      :objects objects
                                      :all-objects all-objects})]
        (when (not-empty childs)
          [:> group-wrapper props])))))

(defn bool-container-factory
  [objects all-objects]
  (let [shape-container (shape-container-factory objects all-objects)
        bool-wrapper (bool-wrapper shape-container)]
    (mf/fnc bool-container
      {::mf/wrap-props false}
      [props]
      (let [childs (->> (cfh/get-children-ids objects (:id (unchecked-get props "shape")))
                        (select-keys objects))
            props  (obj/merge! #js {} props
                               #js {:childs childs
                                    :objects objects})]
        [:> bool-wrapper props]))))

(defn svg-raw-container-factory
  [objects all-objects]
  (let [shape-container (shape-container-factory objects all-objects)
        svg-raw-wrapper (svg-raw-wrapper shape-container)]
    (mf/fnc svg-raw-container
      {::mf/wrap-props false}
      [props]
      (let [childs (mapv #(get objects %) (:shapes (unchecked-get props "shape")))
            props  (obj/merge! #js {} props
                               #js {:childs childs
                                    :objects objects})]
        [:> svg-raw-wrapper props]))))

(defn shape-container-factory
  [objects all-objects]
  (let [path-wrapper   (path-wrapper)
        text-wrapper   (text-wrapper)
        rect-wrapper   (rect-wrapper)
        image-wrapper  (image-wrapper)
        circle-wrapper (circle-wrapper)]
    (mf/fnc shape-container
      {::mf/wrap-props false
       ::mf/wrap [mf/memo]}
      [props]
      (let [shape   (unchecked-get props "shape")
            frame   (unchecked-get props "frame")

            group-container
            (mf/with-memo [objects]
              (group-container-factory objects all-objects))

            frame-container
            (mf/with-memo [objects]
              (frame-container-factory objects all-objects))

            bool-container
            (mf/with-memo [objects]
              (bool-container-factory objects all-objects))

            svg-raw-container
            (mf/with-memo [objects]
              (svg-raw-container-factory objects all-objects))]
        (when (and shape (not (:hidden shape)))
          (let [shape (if frame
                        (gsh/translate-to-frame shape frame)
                        shape)

                opts #js {:shape shape
                          :objects objects
                          :all-objects all-objects}]
            (case (:type shape)
              :frame   [:> frame-container opts]
              :text    [:> text-wrapper opts]
              :rect    [:> rect-wrapper opts]
              :path    [:> path-wrapper opts]
              :image   [:> image-wrapper opts]
              :circle  [:> circle-wrapper opts]
              :group   [:> group-container {:shape shape :frame frame :objects objects}]
              :bool    [:> bool-container {:shape shape :frame frame :objects objects}]
              :svg-raw [:> svg-raw-container {:shape shape :frame frame :objects objects}])))))))
