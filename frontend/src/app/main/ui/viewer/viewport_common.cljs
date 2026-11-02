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
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
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
