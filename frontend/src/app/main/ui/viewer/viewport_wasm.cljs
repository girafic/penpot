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
   [app.main.render-viewer-wasm :as rwv]
   [app.main.ui.viewer.shapes :as shapes]
   [app.main.ui.viewer.viewport-common :as vpc]
   [app.render-wasm.api :as wasm.api]
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

(mf/defc wasm-hotspots-svg*
  "The hotspots of the board, over its WASM picture, where its shapes
  show: as the animation of `modifiers-ref` poses them (see
  `use-posed-modifiers`), tilted and in depth (see `shapes/hotspot*`).
  A board without hotspots does not follow the animation: posing all
  its shapes takes a while. The layers of `pins` show that far up (see
  `vpc/fixed-pins`)."
  [{:keys [vbox size class prepared prepared-all prepared-frame shape-filter
           objects frame delta modifiers-ref pins]}]
  (let [hotspots? (mf/with-memo [prepared prepared-frame shape-filter]
                    (boolean (seq (shapes/hotspot-shapes prepared (:id prepared-frame) shape-filter))))
        modifiers (use-posed-modifiers modifiers-ref hotspots?)
        prepared  (mf/with-memo [prepared modifiers objects frame size delta pins]
                    (cond-> (if (and (seq modifiers) (some? objects))
                              (->> (gsh/apply-objects-modifiers objects (gm/set-objects-modifiers modifiers objects))
                                   (vpc/prepare-objects frame size delta))
                              prepared)
                      (seq pins) (vpc/pin-objects pins)))
        prepared-frame (or (get prepared (:id prepared-frame)) prepared-frame)]
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
     [:> shapes/frame-hotspots*
      {:objects prepared
       :all-objects prepared-all
       :frame prepared-frame
       :shape-filter shape-filter}]]))

(mf/defc wasm-backdrops*
  "Under the layers fixed when scrolling (`fixed-mask-set`), boxes that
  blur the layers that scroll where a fixed one has a background blur:
  the fixed layers draw on a canvas of their own, over the one of the
  others, so their background blur there finds nothing under it (see
  `vpc/backdrop-style`). The layers of `pins` that far up (see
  `vpc/fixed-pins`)."
  [{:keys [prepared fixed-mask-set size scale pins]}]
  (let [shapes (mf/with-memo [prepared fixed-mask-set pins]
                 (let [objects (cond-> prepared (seq pins) (vpc/pin-objects pins))]
                   (into []
                         (comp (keep #(get objects %))
                               (filter vpc/background-blur?))
                         fixed-mask-set)))]
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
  those of `pins` that far up (see `vpc/fixed-pins`). The ones that
  scroll (`scrolls`) move past an end of a touch screen (see
  `.in-device[data-overscroll]` in the viewer styles); the fixed ones
  (`fixed-layer`) stay (the wheel over their containers maps through
  their hotspots, see `app.main.ui.viewer.container-scroll`)."
  [{:keys [canvas-ref scale size vbox
           prepared prepared-all prepared-frame class shape-filter
           objects frame delta modifiers-ref scrolls fixed-layer pins]}]
  (let [{:keys [width height]} (canvas-dimensions scale size)]
    [:div {:data-scroll-layer scrolls
           :data-fixed-layer fixed-layer
           :style {:position "absolute"
                   :top 0
                   :left 0
                   :width (:width size)
                   :height (:height size)}}
     [:canvas {:ref canvas-ref :width width :height height :style {:width "100%"
                                                                   :height "100%"
                                                                   :background "transparent"
                                                                   :pointer-events "none"}}]
     [:> wasm-hotspots-svg* {:vbox vbox
                             :size size
                             :class class
                             :prepared prepared
                             :prepared-all prepared-all
                             :prepared-frame prepared-frame
                             :shape-filter shape-filter
                             :objects objects
                             :frame frame
                             :delta delta
                             :modifiers-ref modifiers-ref
                             :pins pins}]]))

(defn- fixed-scroll-layer-ids
  [objects frame-id has-fixed?]
  (let [frame-subtree-ids (into #{} (cfh/get-children-ids-with-self objects frame-id))
        fixed-mask-ids (when has-fixed? (vpc/frame-fixed-mask-ids objects frame-id))
        fixed-mask-set (or fixed-mask-ids #{})
        not-fixed-include-ids
        (when has-fixed?
          (into []
                (distinct
                 (conj (->> frame-subtree-ids
                            (remove #(contains? fixed-mask-set %)))
                       frame-id))))
        fixed-include-ids
        (when has-fixed?
          (vec (conj (or fixed-mask-ids #{}) frame-id)))
        fixed-clear-fills-ids (when has-fixed? #{frame-id})]
    {:fixed-mask-set fixed-mask-set
     :not-fixed-include-ids not-fixed-include-ids
     :fixed-include-ids fixed-include-ids
     :fixed-clear-fills-ids fixed-clear-fills-ids}))

(mf/defc viewport-wasm*
  "The board `frame` of `page` rendered by WASM. `modifiers-ref` holds
  the animation it plays, which shows over its shapes as they are loaded
  (see `rwv/use-viewer-wasm-viewport!`); `is-snapshot` draws nothing on
  it (see `interactions/viewport*`). On a screen `fixed-drop` px lower
  than its first one, its layers fixed when scrolling stay where their
  vertical constraint holds them (see `vpc/fixed-pins`). `scroll-ref`
  holds the scroll of its containers (see `rwv/use-viewer-wasm-viewport!`)."
  {::mf/wrap [mf/memo]}
  [{:keys [page frame base offset size delta is-fixed modifiers-ref scroll-ref is-snapshot fixed-drop]}]
  (let [;; the same point each render, or the layers would render again
        delta     (mf/with-memo [delta] (or delta (gpt/point 0 0)))
        vbox      (:vbox size)
        is-fixed  (true? is-fixed)

        not-fixed-wasm-ref   (mf/use-ref nil)
        fixed-wasm-ref       (mf/use-ref nil)

        objects  (:objects page)
        frame-id (:id frame)
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

        {:keys [fixed-mask-set not-fixed-include-ids fixed-include-ids fixed-clear-fills-ids]}
        (mf/with-memo [objects frame-id has-fixed?]
          (fixed-scroll-layer-ids objects frame-id has-fixed?))

        prepared-frame (get prepared frame-id)

        pins (mf/with-memo [objects frame-id has-fixed? fixed-drop]
               (when has-fixed? (vpc/fixed-pins objects frame-id fixed-drop)))

        not-fixed-props
        (mf/props {:prepared prepared
                   :prepared-all prepared-all
                   :prepared-frame prepared-frame
                   :objects objects
                   :frame frame
                   :delta delta
                   :modifiers-ref modifiers-ref
                   :canvas-ref not-fixed-wasm-ref
                   :scrolls (not is-fixed)
                   :scale scale
                   :size size
                   :vbox vbox
                   :class (if has-fixed?
                            (stl/css :not-fixed)
                            (when is-fixed (stl/css :fixed)))
                   :shape-filter (when has-fixed?
                                   #(not (contains? fixed-mask-set %)))})

        fixed-props
        (mf/props {:prepared prepared
                   :prepared-all prepared-all
                   :prepared-frame prepared-frame
                   :objects objects
                   :frame frame
                   :delta delta
                   :modifiers-ref modifiers-ref
                   :canvas-ref fixed-wasm-ref
                   :fixed-layer true
                   :scale scale
                   :size size
                   :vbox vbox
                   :class (stl/css :not-fixed)
                   :shape-filter #(contains? fixed-mask-set %)
                   :pins pins})]

    (rwv/use-viewer-wasm-viewport!
     page-id objects size scale frame-id
     not-fixed-wasm-ref fixed-wasm-ref
     not-fixed-include-ids fixed-include-ids fixed-clear-fills-ids pins
     delta modifiers-ref scroll-ref nil (true? is-snapshot))

    [:& (mf/provider shapes/base-frame-ctx) {:value (get prepared-all (:id base))}
     [:& (mf/provider shapes/frame-offset-ctx) {:value offset}
      [:*
       [:> wasm-layer* not-fixed-props]

       ;; The layers fixed when scrolling: the scroll of the section moves
       ;; the others, this one sticks to its top left corner (the browser
       ;; holds it there as it scrolls, a scroll event would come a frame
       ;; late). It is as large as nothing, its picture as large as the
       ;; board hangs from it. Only its hotspots take a click, the rest goes
       ;; to the layers that scroll below.
       (when has-fixed?
         [:div {:style {:position "sticky"
                        :top 0
                        :left 0
                        :width 0
                        :height 0
                        :pointer-events "none"}}
          [:> wasm-backdrops* {:prepared prepared
                               :fixed-mask-set fixed-mask-set
                               :size size
                               :scale scale
                               :pins pins}]
          [:> wasm-layer* fixed-props]])]]]))
