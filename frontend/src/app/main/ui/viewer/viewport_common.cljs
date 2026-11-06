;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer.viewport-common
  "Shared object preparation for viewer viewports (SVG and WASM)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.types.modifiers :as ctm]
   [app.common.uuid :as uuid]
   [app.main.data.viewer.scroll :as dvs]))

(defn prepare-objects
  [frame size delta objects]
  (let [frame-id  (:id frame)
        vector  (-> (gpt/point (:x size) (:y size))
                    (gpt/add delta)
                    (gpt/negate))
        update-fn #(d/update-when %1 %2 gsh/transform-shape (ctm/move-modifiers vector))]
    (->> (cfh/get-children-ids objects frame-id)
         (into [frame-id])
         (reduce update-fn objects))))

(defn get-fixed-ids
  ([objects]
   (get-fixed-ids objects cfh/fixed-scroll?))
  ([objects fixed?]
   (let [fixed-ids (filter fixed? (vals objects))

         fixed-children-ids
         (into #{} (mapcat #(cfh/get-children-ids objects (:id %)) fixed-ids))

         parent-children-ids
         (->> fixed-ids
              (mapcat #(cons (:id %) (cfh/get-parent-ids objects (:id %))))
              (remove #(= % uuid/zero)))

         fixed-ids
         (concat fixed-children-ids parent-children-ids)]
     fixed-ids)))

(defn in-container?
  "Whether the layer `id` of `objects` is in a board that scrolls its
  content in the view mode (see `dvs/container-axes`): fixed when
  scrolling there, it stays with that board, not with the screen."
  [objects id]
  (boolean (some #(dvs/container-axes objects (get objects %))
                 (cfh/get-parent-ids-seq objects id))))

(defn screen-fixed-layer?
  "Whether the layer `shape` of `objects` stays on screen as it scrolls:
  fixed when scrolling, and not in a container (see `in-container?`)."
  [objects shape]
  (and (cfh/fixed-scroll? shape)
       (not (in-container? objects (:id shape)))))

(defn screen-fixed?
  "Whether the layer `id` of `objects`, one it holds or one it is in
  below its top board stays on screen as it scrolls (see
  `screen-fixed-layer?`), as `cfh/fixed?` asks but for the layers fixed
  in a container."
  [objects id]
  (boolean
   (some #(screen-fixed-layer? objects (get objects %))
         (concat [id]
                 (cfh/get-children-ids objects id)
                 (->> (cfh/get-parent-ids objects id)
                      (take-while #(and (not= % uuid/zero) (not (cfh/root-frame? objects %)))))))))

(defn screen-sticky-layer?
  "Whether the layer `shape` of `objects` sticks at the top of the screen
  as it scrolls (see `cfh/sticky-scroll?`), in a box of its own (see
  `sticky-span`): not in a container (it sticks in that one, see
  `dvs/scroll-shifts`), not in or holding a layer that stays on screen
  (see `screen-fixed?`), and not one that starts above its top board (it
  would stick at once)."
  [objects shape]
  (and (cfh/sticky-scroll? shape)
       (not (in-container? objects (:id shape)))
       (not (screen-fixed? objects (:id shape)))
       (let [root (cfh/get-root-frame objects (:id shape))]
         (>= (dm/get-in shape [:selrect :y1]) (dm/get-in root [:selrect :y1])))))

(defn screen-sticky-ids
  "The layers of the board `frame-id` of `objects` that stick at the top
  of the screen (see `screen-sticky-layer?`), in the order they draw; not
  one in another, which sticks with it, nor a hidden one or one in it."
  [objects frame-id]
  (letfn [(walk [ids]
            (mapcat (fn [id]
                      (when-let [shape (get objects id)]
                        (cond
                          (:hidden shape)                      nil
                          (screen-sticky-layer? objects shape) [id]
                          :else                                (walk (:shapes shape)))))
                    ids))]
    (vec (walk (dm/get-in objects [frame-id :shapes])))))

(defn sticky-span
  "Where the sticky layer `id` of the prepared `objects` of the board
  `frame-id` (see `screen-sticky-layer?`) shows on its canvas, `scale` px
  a unit: its `:edge` (top) and `:height`, and the span it sticks in, as
  the box of CSS `position: sticky` does in its parent: from `:top`, the
  top of its board, to `:bottom`, where it stops sticking (see
  `dvs/sticky-bound`), the bottom of the board `frame-id` at most."
  [objects frame-id id scale]
  (let [shape  (get objects id)
        {:keys [y1 y2]} (:selrect shape)
        limit  (dm/get-in objects [frame-id :selrect :y2])
        end    (min limit (or (dvs/sticky-bound objects shape frame-id #{:y}) limit))]
    {:edge   (* scale y1)
     :height (* scale (- y2 y1))
     :top    (* scale (min y1 (dm/get-in objects [(:parent-id shape) :selrect :y1])))
     :bottom (* scale (max end y2))}))

(defn screen-stuck-move
  "How far down from its place the sticky layer of the screen `screen-id`
  (see `screen-sticky-layer?`) that is or holds the layer `id` of
  `objects` shows, its page scrolled `scroll` units down, as its box
  sticks (see `dvs/sticky-edge`): `{:id :dy}`, nil when it shows at its
  place. A layer of another top board, an overlay on it, sticks not to
  the page of the screen (see
  `app.main.ui.viewer.viewport-wasm/viewport-wasm*`)."
  [objects screen-id id scroll]
  (when-let [sticky-id (when (contains? objects id)
                         (->> (cons id (cfh/get-parent-ids-seq objects id))
                              (filter #(screen-sticky-layer? objects (get objects %)))
                              (last)))]
    (let [shape (get objects sticky-id)
          root  (cfh/get-root-frame objects sticky-id)]
      (when (= (:id root) screen-id)
        (let [{:keys [y1 y2]} (:selrect shape)
              shown (dvs/sticky-edge y1 (- y2 y1) scroll
                                     {:side  :start
                                      :line  (dm/get-in root [:selrect :y1])
                                      :bound (dvs/sticky-bound objects shape (:id root) #{:y})})
              dy    (- (+ shown scroll) y1)]
          (when (> dy 0.01)
            {:id sticky-id :dy dy}))))))

(defn frame-fixed-mask-ids
  "Fixed-layer shape ids inside `frame-id` (same rules as
  `get-fixed-ids`): the layers that stay on screen as it scrolls (see
  `screen-fixed-layer?`)."
  [objects frame-id]
  (when frame-id
    (let [subtree (into #{} (cfh/get-children-ids-with-self objects frame-id))]
      (into #{}
            (filter #(contains? subtree %)
                    (get-fixed-ids objects #(screen-fixed-layer? objects %)))))))

(defn fixed-pins
  "How far up each layer of the board `frame-id` of `objects` fixed when
  scrolling goes on a screen `drop` px of the board lower than its first
  screen (see `viewer/fixed-drop`), to stay where its vertical
  constraint holds it on the screen: as far at the bottom, half as far
  in the middle, the others where they are. `{id distance}`, nil when
  the screen is as high."
  [objects frame-id drop]
  (when (and (number? drop) (pos? drop))
    (not-empty
     (into {}
           (keep (fn [id]
                   (let [shape (get objects id)]
                     (when (cfh/fixed-scroll? shape)
                       (case (:constraints-v shape)
                         :bottom [id drop]
                         :center [id (/ drop 2)]
                         nil)))))
           (dm/get-in objects [frame-id :shapes])))))

(defn pin-objects
  "`objects` with each layer of `pins` (see `fixed-pins`), and the layers
  in it, moved up as far."
  [objects pins]
  (reduce-kv (fn [objects id distance]
               (let [modifiers (ctm/move-modifiers 0 (- distance))]
                 (reduce #(d/update-when %1 %2 gsh/transform-shape modifiers)
                         objects
                         (cons id (cfh/get-children-ids objects id)))))
             objects
             pins))

(defn pin-modifiers
  "The modif-tree `modif-tree` (an animation at a time, or nil) with each
  layer of `pins` (see `fixed-pins`) moved up as far after it."
  [modif-tree pins]
  (reduce-kv (fn [tree id distance]
               (update-in tree [id :modifiers] ctm/add-modifiers (ctm/move-modifiers 0 (- distance))))
             (or modif-tree {})
             pins))

(defn prepare-page-objects
  "Transform all page objects into vbox-space (for overlay positioning)."
  [objects size delta]
  (let [vector (-> (gpt/point (:x size) (:y size))
                   (gpt/add delta)
                   (gpt/negate))
        update-fn #(d/update-when %1 %2 gsh/transform-shape (ctm/move-modifiers vector))
        ids (->> (keys objects) (remove #(= % uuid/zero)))]
    (reduce update-fn objects ids)))

(defn viewer-scale
  [size]
  (if (and (:base-width size) (pos? (:base-width size)))
    (/ (:width size) (:base-width size))
    1))

(def ^:private blur-sigma-scale
  "Skia's scale from a blur radius to its sigma (`radius_to_sigma` in
  render-wasm)."
  0.57735027)

(defn background-blur?
  "Whether `shape` shows a background blur."
  [shape]
  (let [blur (:background-blur shape)]
    (and (some? blur)
         (not (:hidden blur))
         (not (:hidden shape))
         (pos? (:value blur 0)))))

(defn backdrop-style
  "The style of a box that blurs what shows under `shape` (in the space
  of the view box, see `prepare-objects`) as its background blur does on
  the canvas, at `scale` CSS px to a px of the board: over its box, turned
  as it is, round at its corners or oval, as blurred."
  [shape scale]
  (let [{:keys [x y width height]} (:selrect shape)
        px     (fn [v] (dm/str (* (or v 0) scale) "px"))
        sigma  (+ (* blur-sigma-scale (dm/get-in shape [:background-blur :value])) 0.5)
        blur   (dm/str "blur(" (mth/precision (* sigma scale) 2) "px)")
        radius (cond
                 (cfh/circle-shape? shape) "50%"
                 (some? (:r1 shape))
                 (dm/str (px (:r1 shape)) " " (px (:r2 shape)) " " (px (:r3 shape)) " " (px (:r4 shape))))
        mtx    (:transform shape)]
    (clj->js
     (cond-> {:position "absolute"
              :left (px x)
              :top (px y)
              :width (px width)
              :height (px height)
              :backdropFilter blur
              :WebkitBackdropFilter blur}
       (some? radius)
       (assoc :borderRadius radius)

       (and (some? mtx) (not (gmt/unit? mtx)))
       (assoc :transform (dm/str "matrix(" (:a mtx) ", " (:b mtx) ", " (:c mtx) ", " (:d mtx) ", 0, 0)"))))))
