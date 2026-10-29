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

;; What `changes-by-shape` made of the changes it was given, by those
;; changes: the changes of a frame are what the next one compares with.
(defonce ^:private by-shape-cache (js/WeakMap.))

(defn- changes-by-shape
  "`{id {property change}}` of the attribute changes `changes` (`[id
  change]`), the last change of a property of a shape winning."
  [changes]
  (if (nil? changes)
    {}
    (or (.get by-shape-cache changes)
        (let [by-shape (reduce (fn [acc [id change]]
                                 (assoc acc id (assoc (get acc id) (:property change) change)))
                               {}
                               changes)]
          (.set by-shape-cache changes by-shape)
          by-shape))))

(defn- same-values?
  "Whether the attribute changes `after` of a shape (`{property change}`)
  give the properties `before` changed the values it gave them."
  [before after]
  (and (= (count before) (count after))
       (every? (fn [[property change]]
                 (when-let [old (get before property)]
                   (= (:value old) (:value change))))
               after)))

(defn property-changes
  "The shapes of `objects` with the attribute changes `changes` (`[id
  change]`, see `extract-property-changes`) and the properties `prev`
  changed and `changes` no longer do set back to how they are in
  `objects`: `[objects shape-changes]`, `shape-changes` the properties
  that change, by shape id. With `shown?` (the renderer shows `prev`),
  the shapes that keep the values `prev` gave them are left out: an
  animation holds most of its layers still most of the time."
  ([objects prev changes]
   (property-changes objects prev changes false))
  ([objects prev changes shown?]
   (let [prev    (changes-by-shape prev)
         changes (changes-by-shape changes)
         change  (fn [[objects shape-changes :as acc] id after]
                   (let [before (get prev id)]
                     (if (and shown? (some? after) (same-values? before after))
                       acc
                       [(cond-> objects
                          (some? after)
                          (assoc id (reduce ctm/apply-modifier (get objects id) (vals after))))
                        (assoc shape-changes id (-> #{} (into (keys before)) (into (keys after))))])))]
     (as-> [objects {}] $
       (reduce-kv change $ changes)
       ;; the shapes `prev` changed that `changes` no longer do
       (reduce-kv (fn [acc id _]
                    (cond-> acc
                      (not (contains? changes id)) (change id nil)))
                  $
                  prev)))))
