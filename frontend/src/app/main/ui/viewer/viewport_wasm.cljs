;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer.viewport-wasm
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.modifiers :as gm]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.main.data.viewer.scroll :as dvs]
   [app.main.render-viewer-wasm :as rwv]
   [app.main.ui.viewer.shapes :as shapes]
   [app.main.ui.viewer.viewport-common :as vpc]
   [app.render-wasm.api :as wasm.api]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn- canvas-dimensions
  "Physical canvas pixels (CSS layout size × DPR), matching the workspace WASM path."
  [scale size]
  (let [css-w (js/Math.round (* scale (:base-width size)))
        css-h (js/Math.round (* scale (:base-height size)))
        dpr   (wasm.api/get-dpr)]
    {:width  (js/Math.round (* css-w dpr))
     :height (js/Math.round (* css-h dpr))}))

(def ^:private hotspot-interval
  "How often, ms, the hotspots follow the animation as it plays: often
  enough to click what moves, seldom enough to keep playback smooth."
  100)

(defn- use-posed-modifiers
  "The modif-tree the ref `modifiers-ref` (of a delay, see
  `interactions/viewport*`) holds, followed at most every
  `hotspot-interval` ms, the last one always; nil without `follow?`."
  [modifiers-ref follow?]
  (let [state* (mf/use-state #(when follow? (some-> modifiers-ref deref force)))]
    (mf/with-effect [modifiers-ref follow?]
      (when (and follow? (some? modifiers-ref))
        (let [key     (js/Symbol "hotspots")
              last-at (volatile! 0)
              timer   (volatile! nil)
              take!   (fn []
                        (vreset! last-at (js/Date.now))
                        (vreset! timer nil)
                        (reset! state* (force @modifiers-ref)))]
          (take!)
          (add-watch modifiers-ref key
                     (fn [_ _ _ _]
                       (let [wait (- hotspot-interval (- (js/Date.now) @last-at))]
                         (cond
                           (not (pos? wait)) (take!)
                           (nil? @timer)     (vreset! timer (js/setTimeout take! wait))))))
          (fn []
            (remove-watch modifiers-ref key)
            (some-> @timer js/clearTimeout)))))
    (when follow? (deref state*))))

(defn- use-posed-objects
  "The objects `prepared` of the board `frame` (see `vpc/prepare-objects`)
  as the animation of `modifiers-ref` poses them, followed at most every
  `hotspot-interval` ms (see `use-posed-modifiers`); as they are without
  `follow?`. One for all the hotspots of a viewport: posing all its
  shapes takes a while."
  [modifiers-ref follow? prepared objects frame size delta]
  (let [modifiers (use-posed-modifiers modifiers-ref follow?)]
    (mf/with-memo [prepared modifiers objects frame size delta]
      (if (and (seq modifiers) (some? objects))
        (->> (gsh/apply-objects-modifiers objects (gm/set-objects-modifiers modifiers objects))
             (vpc/prepare-objects frame size delta))
        prepared))))

(defonce ^:private no-scroll
  (atom nil))

(mf/defc wasm-hotspots-svg*
  "The hotspots of the board, over its WASM picture, where its shapes
  show: as the animation poses them (`posed`, see `use-posed-objects`),
  tilted and in depth (see `shapes/hotspot*`). The layers of `pins` show
  that far up (see `vpc/fixed-pins`). The layers in its containers
  follow their scroll (`scroll-ref`) at once, the interactions they open
  from too, and the hotspots there show within the boxes of their
  containers, which clip them: one scrolled out of the box takes no
  click. `sheet-root`, the root of an overlay that scrolls as one (see
  `dvs/scroll-entries`)."
  [{:keys [vbox size class posed prepared-all prepared-frame shape-filter
           objects frame scroll-ref pins sheet-root]}]
  (let [scroll     (mf/deref (or scroll-ref no-scroll))
        shifts     (mf/with-memo [objects scroll sheet-root]
                     (when (and (seq scroll) (some? objects))
                       (not-empty (dvs/view-shifts objects scroll :overlay-root sheet-root))))
        prepared   (mf/with-memo [posed pins shifts]
                     (cond-> posed
                       (seq pins)   (vpc/pin-objects pins)
                       (seq shifts) (dvs/shift-objects shifts)))
        prepared-all (mf/with-memo [prepared-all shifts]
                       (cond-> prepared-all (seq shifts) (dvs/shift-objects shifts)))
        prepared-frame (or (get prepared (:id prepared-frame)) prepared-frame)
        containers (mf/with-memo [objects frame sheet-root]
                     (when (some? objects)
                       (not-empty (dvs/container-ids objects (:id frame) :overlay-root? (some? sheet-root)))))
        prefix     (str "scroll-clip-" (str/replace (mf/use-id) #"[^A-Za-z0-9]" "") "-")
        clip-id    #(str prefix %)
        clips      (mf/with-memo [prepared prepared-frame shape-filter containers prefix]
                     (when (some? containers)
                       (into {}
                             (keep (fn [shape]
                                     (when-let [container (dvs/innermost-container objects containers (:id shape))]
                                       [(:id shape) (str "url(#" (clip-id container) ")")])))
                             (shapes/hotspot-shapes prepared (:id prepared-frame) shape-filter))))]
    [:svg {:view-box vbox
           :width (:width size)
           :height (:height size)
           :version "1.1"
           :xmlnsXlink "http://www.w3.org/1999/xlink"
           :xmlns "http://www.w3.org/2000/svg"
           :fill "none"
           :style {:position "absolute"
                   :top 0
                   :left 0}
           :class class}
     (when (seq clips)
       [:defs
        (for [id containers]
          (let [outer (dvs/innermost-container objects containers id)
                {:keys [x y width height]} (:selrect (dvs/design-box (get prepared id)))]
            [:clipPath {:key (dm/str id)
                        :id (clip-id id)
                        :clip-path (when (some? outer) (str "url(#" (clip-id outer) ")"))}
             [:rect {:x x :y y :width width :height height}]]))])
     [:> shapes/frame-hotspots*
      {:objects prepared
       :all-objects prepared-all
       :frame prepared-frame
       :shape-filter shape-filter
       :clips clips}]]))

(mf/defc wasm-backdrops*
  "Under the layers of `mask-set` that draw on a canvas of their own
  (fixed when scrolling, or sticky), boxes that blur the layers that
  scroll where one of them has a background blur: over the canvas of the
  others, it finds nothing under it there (see `vpc/backdrop-style`).
  The layers of `pins` that far up (see `vpc/fixed-pins`)."
  [{:keys [prepared mask-set size scale pins]}]
  (let [shapes (mf/with-memo [prepared mask-set pins]
                 (let [objects (cond-> prepared (seq pins) (vpc/pin-objects pins))]
                   (into []
                         (comp (keep #(get objects %))
                               (filter vpc/background-blur?))
                         mask-set)))]
    (when (seq shapes)
      [:div {:style {:position "absolute"
                     :top 0
                     :left 0
                     :width (:width size)
                     :height (:height size)
                     :pointer-events "none"}}
       (for [shape shapes]
         [:div {:key (dm/str (:id shape))
                :style (vpc/backdrop-style shape scale)}])])))

(mf/defc wasm-layer*
  "The WASM picture of the layers of `shape-filter` and their hotspots,
  those of `pins` that far up (see `vpc/fixed-pins`), on the canvas of
  the render pass `pass` (see `rwv/use-viewer-wasm-viewport!`). The ones
  that scroll (`scrolls`) move past an end of a touch screen (see
  `.in-device[data-overscroll]` in the viewer styles); the fixed ones
  (`fixed-layer`) stay (the wheel over their containers maps through
  their hotspots, see `app.main.ui.viewer.container-scroll`)."
  [{:keys [canvas-ref pass scale size vbox
           posed prepared-all prepared-frame class shape-filter
           objects frame scroll-ref scrolls fixed-layer pins sheet-root]}]
  (let [{:keys [width height]} (canvas-dimensions scale size)]
    [:div {:data-scroll-layer scrolls
           :data-fixed-layer fixed-layer
           :style {:position "absolute"
                   :top 0
                   :left 0
                   :width (:width size)
                   :height (:height size)}}
     [:canvas {:ref canvas-ref
               :data-pass pass
               :width width
               :height height
               :style {:width "100%"
                       :height "100%"
                       :background "transparent"
                       :pointer-events "none"}}]
     [:> wasm-hotspots-svg* {:vbox vbox
                             :size size
                             :class class
                             :posed posed
                             :prepared-all prepared-all
                             :prepared-frame prepared-frame
                             :shape-filter shape-filter
                             :objects objects
                             :frame frame
                             :scroll-ref scroll-ref
                             :pins pins
                             :sheet-root sheet-root}]]))

(defn- layer-masks
  "What each render pass of the board `frame-id` of `objects` draws (see
  `rwv/use-viewer-wasm-viewport!`): `:fixed-mask-set`, the layers fixed
  when scrolling (with `has-fixed?`, see `vpc/frame-fixed-mask-ids`);
  `:sticky-masks`, `[id ids]` of each of the layers `sticky-ids` that
  stick at the top of the screen, all within it; `:apart`, those the pass
  of what scrolls leaves to the others, and the include and clear ids of
  that one and the fixed one."
  [objects frame-id has-fixed? sticky-ids]
  (let [fixed-mask-set (or (when has-fixed? (vpc/frame-fixed-mask-ids objects frame-id)) #{})
        sticky-masks   (mapv (fn [id] [id (into #{} (cfh/get-children-ids-with-self objects id))])
                             sticky-ids)
        apart          (into fixed-mask-set (mapcat second) sticky-masks)]
    {:fixed-mask-set fixed-mask-set
     :sticky-masks sticky-masks
     :apart apart
     :not-fixed-include-ids
     (when (seq apart)
       (into []
             (distinct)
             (conj (->> (cfh/get-children-ids-with-self objects frame-id)
                        (remove #(contains? apart %)))
                   frame-id)))
     :fixed-include-ids (when has-fixed? (vec (conj fixed-mask-set frame-id)))
     :fixed-clear-fills-ids (when has-fixed? #{frame-id})}))

(mf/defc wasm-sticky-box*
  "A layer that sticks at the top of the screen (see
  `vpc/screen-sticky-layer?`): a box the browser holds at the top of the
  section as it scrolls, as CSS `position: sticky` does, in the span of
  its board (`span`, see `vpc/sticky-span`), with the picture of the
  layer as large as the board (`layer-props`, see `wasm-layer*`) hanging
  from it, its hotspots and the boxes that blur what scrolls under its
  layers (`mask`, see `wasm-backdrops*`). A scroll event would come a
  frame late."
  [{:keys [id span mask prepared size scale layer-props]}]
  (let [{:keys [edge height top bottom]} span]
    [:div {:style {:position "absolute"
                   :top top
                   :left 0
                   :width 0
                   :height (- bottom top)
                   :pointer-events "none"}}
     [:div {:style {:height (- edge top)}}]
     [:div {:data-sticky-layer (dm/str id)
            :style {:position "sticky"
                    :top 0
                    :width 0
                    :height height}}
      [:div {:style {:position "absolute"
                     :top (- edge)
                     :left 0
                     :width 0
                     :height 0}}
       [:> wasm-backdrops* {:prepared prepared
                            :mask-set mask
                            :size size
                            :scale scale}]
       [:> wasm-layer* layer-props]]]]))

(mf/defc viewport-wasm*
  "The board `frame` of `page` rendered by WASM. `modifiers-ref` holds
  the animation it plays, which shows over its shapes as they are loaded
  (see `rwv/use-viewer-wasm-viewport!`); `is-snapshot` draws nothing on
  it (see `interactions/viewport*`). On a screen `fixed-drop` px lower
  than its first one, its layers fixed when scrolling stay where their
  vertical constraint holds them (see `vpc/fixed-pins`). `scroll-ref`
  holds the scroll of its containers (see `rwv/use-viewer-wasm-viewport!`);
  `is-overlay`, the root of an overlay, scrolls as one, drawn at its box
  when the view mode grew it (see `dvs/scroll-entries`)."
  {::mf/wrap [mf/memo]}
  [{:keys [page frame base offset size delta is-fixed modifiers-ref scroll-ref is-snapshot fixed-drop
           is-overlay]}]
  (let [;; the same point each render, or the layers would render again
        delta     (mf/with-memo [delta] (or delta (gpt/point 0 0)))
        vbox      (:vbox size)
        is-fixed  (true? is-fixed)

        not-fixed-wasm-ref   (mf/use-ref nil)
        fixed-wasm-ref       (mf/use-ref nil)
        ;; {id canvas} of the layers that stick at the top of the screen
        sticky-canvases      (mf/use-ref {})

        objects  (:objects page)
        frame-id (:id frame)
        ;; the root of an overlay grown to its content scrolls as one
        sheet-root (when (and (true? is-overlay) (some? (dvs/screen-h (get objects frame-id))))
                     frame-id)
        scale    (vpc/viewer-scale size)
        page-id  (:id page)

        frame   (cond-> frame is-fixed (assoc :fixed-scroll true))
        objects (cond-> objects is-fixed (assoc-in [frame-id :fixed-scroll] true))

        has-fixed?
        (and (not is-fixed)
             (some #(vpc/screen-fixed-layer? objects (get objects %))
                   (cfh/get-children-ids objects frame-id)))

        prepared
        (mf/with-memo [objects frame size delta]
          (vpc/prepare-objects frame size delta objects))

        prepared-all
        (mf/with-memo [objects size delta]
          (vpc/prepare-page-objects objects size delta))

        ;; nothing of an overlay sticks to the section: the root of one
        ;; that scrolls holds its sticky layers (see `dvs/view-shifts`)
        sticky-ids
        (mf/with-memo [objects frame-id is-fixed is-overlay]
          (when-not (or is-fixed (true? is-overlay))
            (not-empty (vpc/screen-sticky-ids objects frame-id))))

        {:keys [fixed-mask-set sticky-masks apart] :as masks}
        (mf/with-memo [objects frame-id has-fixed? sticky-ids]
          (layer-masks objects frame-id has-fixed? sticky-ids))

        prepared-frame (get prepared frame-id)

        ;; the posed shapes of all the hotspots: a board without hotspots
        ;; does not follow the animation
        hotspots? (mf/with-memo [prepared frame-id]
                    (boolean (seq (shapes/hotspot-shapes prepared frame-id nil))))
        posed     (use-posed-objects modifiers-ref hotspots? prepared objects frame size delta)

        pins (mf/with-memo [objects frame-id has-fixed? fixed-drop]
               (when has-fixed? (vpc/fixed-pins objects frame-id fixed-drop)))

        passes
        (mf/with-memo [masks frame-id pins]
          (let [{:keys [not-fixed-include-ids fixed-include-ids fixed-clear-fills-ids]} masks]
            (cond-> (into [{:tag "scroll"
                            :canvas #(mf/ref-val not-fixed-wasm-ref)
                            :include not-fixed-include-ids}]
                          (map (fn [[id mask]]
                                 {:tag (dm/str "sticky:" id)
                                  :canvas #(get (mf/ref-val sticky-canvases) id)
                                  :include (into [frame-id] mask)
                                  :clear #{frame-id}
                                  :still true}))
                          (:sticky-masks masks))
              (seq fixed-include-ids)
              (conj {:tag "fixed"
                     :canvas #(mf/ref-val fixed-wasm-ref)
                     :include fixed-include-ids
                     :clear fixed-clear-fills-ids
                     :pins pins
                     :still true}))))

        sticky-boxes
        (mf/with-memo [sticky-masks objects prepared frame-id scale]
          (mapv (fn [[id mask]]
                  {:id id
                   :mask mask
                   ;; its hotspots follow the scroll of the containers
                   ;; only with one in it: each step would render them
                   :scrolls? (boolean (some #(dvs/container-axes objects (get objects %)) mask))
                   :span (vpc/sticky-span prepared frame-id id scale)
                   :ref (fn [node]
                          (let [canvases (mf/ref-val sticky-canvases)]
                            (mf/set-ref-val! sticky-canvases
                                             (if (some? node)
                                               (assoc canvases id node)
                                               (dissoc canvases id)))))})
                sticky-masks))

        not-fixed-props
        (mf/props {:posed posed
                   :prepared-all prepared-all
                   :prepared-frame prepared-frame
                   :objects objects
                   :frame frame
                   :scroll-ref scroll-ref
                   :sheet-root sheet-root
                   :canvas-ref not-fixed-wasm-ref
                   :pass "scroll"
                   :scrolls (not is-fixed)
                   :scale scale
                   :size size
                   :vbox vbox
                   :class (if (seq apart)
                            (stl/css :not-fixed)
                            (when is-fixed (stl/css :fixed)))
                   :shape-filter (when (seq apart)
                                   #(not (contains? apart %)))})

        fixed-props
        (mf/props {:posed posed
                   :prepared-all prepared-all
                   :prepared-frame prepared-frame
                   :objects objects
                   :frame frame
                   :scroll-ref scroll-ref
                   :sheet-root sheet-root
                   :canvas-ref fixed-wasm-ref
                   :pass "fixed"
                   :fixed-layer true
                   :scale scale
                   :size size
                   :vbox vbox
                   :class (stl/css :not-fixed)
                   :shape-filter #(contains? fixed-mask-set %)
                   :pins pins})]

    (rwv/use-viewer-wasm-viewport!
     page-id objects size scale frame-id passes
     delta modifiers-ref scroll-ref
     sheet-root
     (true? is-snapshot))

    [:& (mf/provider shapes/base-frame-ctx) {:value (get prepared-all (:id base))}
     [:& (mf/provider shapes/frame-offset-ctx) {:value offset}
      [:*
       [:> wasm-layer* not-fixed-props]

       ;; The layers that stick at the top of the screen, over what
       ;; scrolls, under what stays (see `wasm-sticky-box*`)
       (for [{:keys [id mask span ref scrolls?]} sticky-boxes]
         [:> wasm-sticky-box*
          {:key (dm/str id)
           :id id
           :span span
           :mask mask
           :prepared prepared
           :size size
           :scale scale
           :layer-props (mf/spread-props not-fixed-props
                                         {:canvas-ref ref
                                          :scroll-ref (when scrolls? scroll-ref)
                                          :pass (dm/str "sticky:" id)
                                          :scrolls true
                                          :class (stl/css :not-fixed)
                                          :shape-filter #(contains? mask %)})}])

       ;; The layers fixed when scrolling: the scroll of the section moves
       ;; the others, this one sticks to its top left corner (the browser
       ;; holds it there as it scrolls, a scroll event would come a frame
       ;; late). It is as large as nothing, its picture as large as the
       ;; board hangs from it. Only its hotspots take a click, the rest goes
       ;; to the layers that scroll below. Nothing of an overlay scrolls
       ;; with the section: there it lies where its element starts, which
       ;; the margin of a shadow can put before the screen.
       (when has-fixed?
         [:div {:style {:position (if (true? is-overlay) "absolute" "sticky")
                        :top 0
                        :left 0
                        :width 0
                        :height 0
                        :pointer-events "none"}}
          [:> wasm-backdrops* {:prepared prepared
                               :mask-set fixed-mask-set
                               :size size
                               :scale scale
                               :pins pins}]
          [:> wasm-layer* fixed-props]])]]]))
