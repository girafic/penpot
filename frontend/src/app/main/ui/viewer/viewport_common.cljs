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
   [app.common.uuid :as uuid]))

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
  [objects]
  (let [fixed-ids (filter cfh/fixed-scroll? (vals objects))

        fixed-children-ids
        (into #{} (mapcat #(cfh/get-children-ids objects (:id %)) fixed-ids))

        parent-children-ids
        (->> fixed-ids
             (mapcat #(cons (:id %) (cfh/get-parent-ids objects (:id %))))
             (remove #(= % uuid/zero)))

        fixed-ids
        (concat fixed-children-ids parent-children-ids)]
    fixed-ids))

(defn frame-fixed-mask-ids
  "Fixed-layer shape ids inside `frame-id` (same rules as `get-fixed-ids`)."
  [objects frame-id]
  (when frame-id
    (let [subtree (into #{} (cfh/get-children-ids-with-self objects frame-id))]
      (into #{}
            (filter #(contains? subtree %)
                    (get-fixed-ids objects))))))

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
