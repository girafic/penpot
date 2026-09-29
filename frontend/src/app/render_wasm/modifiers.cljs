;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.render-wasm.modifiers
  "Modif-trees (see `app.common.types.modifiers`) as the WASM renderer
  takes them: geometry and structure entries, and the attributes they
  change as shape properties. The workspace transforms and the view mode
  playing an animation both show modif-trees this way."
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.types.modifiers :as ctm]
   [app.common.uuid :as uuid]))

(defn parse-structure-modifiers
  [modif-tree]
  (into
   []
   (comp
    (mapcat
     (fn [[parent-id data]]
       (when (ctm/has-structure? (:modifiers data))
         (->> (concat
               (get-in data [:modifiers :structure-parent])
               (get-in data [:modifiers :structure-child]))
              (mapcat
               (fn [modifier]
                 (case (:type modifier)
                   :remove-children
                   (->> (:value modifier)
                        (map (fn [child-id]
                               {:type :remove-children
                                :parent parent-id
                                :id child-id
                                :index 0
                                :value 0})))

                   :add-children
                   (->> (:value modifier)
                        (map (fn [child-id]
                               {:type :add-children
                                :parent parent-id
                                :id child-id
                                :index (:index modifier)
                                :value 0})))

                   :scale-content
                   [{:type :scale-content
                     :parent parent-id
                     :id parent-id
                     :index 0
                     :value (:value modifier)}]
                   nil)))))))
    (filter (fn [{:keys [id parent]}]
              (and (some? id) (some? parent)))))
   modif-tree))

(def ^:private xf:parse-geometry-modifier
  (let [default-transform (gmt/matrix)]
    (keep (fn [[id data]]
            (cond
              (or (nil? id) (= id uuid/zero))
              nil

              (ctm/has-geometry? (:modifiers data))
              (let [parent (:geometry-parent (:modifiers data))
                    kind (if (d/not-empty? parent) :parent :child)]
                (d/vec2 id {:transform (ctm/modifiers->transform (:modifiers data)) :kind kind}))

              ;; Unit matrix is used for reflowing
              :else
              (d/vec2 id {:transform default-transform :kind :parent}))))))

(defn parse-geometry-modifiers
  [modif-tree]
  (into [] xf:parse-geometry-modifier modif-tree))

(defn extract-property-changes
  [modif-tree]
  (->> modif-tree
       (mapcat (fn [[id {:keys [modifiers]}]]
                 (->> (:structure-parent modifiers)
                      (map #(vector id %)))))
       (filter (fn [[_ {:keys [type]}]]
                 (= type :change-property)))))

(defn property-changes
  "The shapes of `objects` with the attribute changes `changes` (`[id
  change]`, see `extract-property-changes`) and, first, the ones of
  `prev` set back to how they are in `objects`: `[objects shape-changes]`,
  `shape-changes` the properties that change, by shape id."
  [objects prev changes]
  (let [;; Set old value for previous properties
        clean-props
        (->> prev
             (map (fn [[id {:keys [property] :as change}]]
                    (let [shape (get objects id)]
                      [id (assoc change :value (get shape property))]))))

        changes
        (concat clean-props changes)

        ;; Stores a map shape -> set of properties changed
        ;; this is the standard format used by process-shape-changes
        shape-changes
        (-> (group-by first changes)
            (update-vals #(into #{} (map (comp :property second)) %)))

        ;; Create a new objects only with the temporary modifications
        objects-changed
        (->> changes
             (group-by first)
             (reduce
              (fn [objects [id properties]]
                (let [shape
                      (->> properties
                           (reduce
                            (fn [shape [_ operation]]
                              (ctm/apply-modifier shape operation))
                            (get objects id)))]
                  (assoc objects id shape)))
              objects))]
    [objects-changed shape-changes]))
