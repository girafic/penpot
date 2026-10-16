;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.svg-upload
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.files.shapes-builder :as sb]
   [app.common.svg :as csvg]
   [app.common.types.shape-tree :as ctst]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.main.data.workspace.wasm-text :as dwwt]
   [app.main.features :as features]
   [app.main.fonts :as fonts]
   [app.main.repo :as rp]
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(defn extract-name [href]
  (let [query-idx (str/last-index-of href "?")
        href (if (> query-idx 0) (subs href 0 query-idx) href)
        filename (->> (str/split href "/") (last))
        ext-idx (str/last-index-of filename ".")]
    (if (> ext-idx 0) (subs filename 0 ext-idx) filename)))

(defn upload-images
  "Extract all bitmap images inside the svg data, and upload them, associated to the file.
  Return a map {<href> <image-data>}."
  [svg-data file-id]
  (->> (rx/from (csvg/collect-images svg-data))
       (rx/map (fn [{:keys [href] :as item}]
                 (let [item (-> item
                                (assoc :file-id file-id)
                                (assoc :is-local true)
                                (assoc :name "image"))]
                   (if (str/starts-with? href "data:")
                     (assoc item :content (wapi/data-uri->blob href))
                     (-> item
                         (assoc :name (extract-name href))
                         (assoc :url href))))))
       (rx/filter (fn [item]
                    (or (contains? item :content)
                        (let [url (:url item)]
                          (or (str/starts-with? url "http://")
                              (str/starts-with? url "https://"))))))
       (rx/mapcat (fn [item]
                    ;; The upload commands use closed params schemas, so
                    ;; drop the svg-only attrs (:href :width :height) that
                    ;; `svg/collect-images` attaches.
                    (->> (rp/cmd! (if (contains? item :content)
                                    :upload-file-media-object
                                    :create-file-media-object-from-url)
                                  (dissoc item :href :width :height))
                         ;; When the image uploaded fail we skip the shape
                         ;; returning `nil` will afterward not create the shape.
                         (rx/catch #(rx/of nil))
                         (rx/map #(vector (:href item) %)))))
       (rx/reduce conj {})))

(defn resolve-text-fonts
  "The text `shape` an SVG import made with the fonts its nodes ask for (a
  family, a weight and a style, see `sb/create-text-shape`) as the fonts
  installed that have them, else the default one, at the nearest weight."
  [shape]
  (if-not (cfh/text-shape? shape)
    shape
    (update shape :content
            (fn [content]
              (txt/transform-nodes
               txt/is-text-node?
               (fn [node]
                 (let [font    (or (some-> (:font-family node) fonts/find-font-family)
                                   (fonts/get-font-data "sourcesanspro"))
                       variant (or (fonts/find-closest-variant font (:font-weight node) (:font-style node))
                                   (fonts/get-default-variant font))]
                   (cond-> node
                     (some? font)
                     (assoc :font-id (:id font) :font-family (:family font))

                     (some? variant)
                     (assoc :font-variant-id (:id variant)
                            :font-weight (str (:weight variant))
                            :font-style (:style variant)))))
               content)))))

(defn text-layout
  "The events that lay out the texts `ids` an import added to the page
  `page-id` in the undo transaction `undo-id`: before its changes
  (`:before`), and after them (`:after`), which commit the transaction,
  with the resize of the texts in it when the new renderer lays them
  out."
  [state page-id ids undo-id]
  {:before (when (seq ids) [(ptk/data-event :text/reflow {:ids ids :page-id page-id})])
   :after  (if (and (seq ids) (features/active-feature? state "render-wasm/v1"))
             [(dwwt/resize-wasm-text-all ids {:undo-id undo-id})]
             [(dwu/commit-undo-transaction undo-id)])})

(defn add-svg-shapes
  ([svg-data position]
   (add-svg-shapes nil svg-data position nil))

  ([id svg-data position {:keys [change-selection? ignore-selection?]
                          :or {ignore-selection? false change-selection? true}}]
   (ptk/reify ::add-svg-shapes
     ptk/WatchEvent
     (watch [it state _]
       (try
         (let [id              (d/nilv id (uuid/next))
               page-id         (:current-page-id state)
               objects         (dsh/lookup-page-objects state page-id)
               selected        (if ignore-selection? #{} (dsh/lookup-selected state))
               base            (cfh/get-base-shape objects selected)

               selected-id     (first selected)
               selected-frame? (and (= 1 (count selected))
                                    (= :frame (dm/get-in objects [selected-id :type])))

               base-id         (:parent-id base)

               frame-id        (if (or selected-frame? (empty? selected)
                                       (not= :frame (dm/get-in objects [base-id :type])))
                                 (ctst/top-nested-frame objects position)
                                 base-id)

               parent-id       (if (or selected-frame? (empty? selected) (nil? base))
                                 frame-id
                                 base-id)

               [new-shape new-children]
               (sb/create-svg-shapes id svg-data position objects frame-id parent-id selected true)

               new-children    (mapv resolve-text-fonts new-children)

               changes         (-> (pcb/empty-changes it page-id)
                                   (pcb/with-objects objects)
                                   (pcb/add-object new-shape))

               changes         (reduce (fn [changes new-child]
                                         (pcb/add-object changes new-child))
                                       changes
                                       new-children)

               changes         (pcb/resize-parents changes
                                                   (->> (:redo-changes changes)
                                                        (filter #(= :add-obj (:type %)))
                                                        (map :id)
                                                        (reverse)
                                                        (vec)))
               undo-id         (js/Symbol)
               layout          (text-layout state page-id
                                            (into [] (comp (filter cfh/text-shape?) (map :id)) new-children)
                                            undo-id)]

           (rx/concat
            (rx/of (dwu/start-undo-transaction undo-id))
            (rx/from (:before layout))
            (rx/of (dch/commit-changes changes)
                   (when change-selection?
                     (dws/select-shapes (d/ordered-set (:id new-shape))))
                   (ptk/data-event :layout/update {:ids [(:id new-shape)]}))
            (rx/from (:after layout))))

         (catch :default cause
           (js/console.error cause)
           (rx/throw (ex/error :type :svg-parser
                               :hint (ex-message cause)))))))))

