;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.tokens.remapping
  "Core logic for token remapping functionality"
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.tokens :as cft]
   [app.common.logging :as log]
   [app.common.logic.libraries :as cll]
   [app.common.logic.timelines :as cltl]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.types.container :refer [shapes-seq]]
   [app.common.types.file :refer [object-containers-seq]]
   [app.common.types.token :as cto]
   [app.common.types.tokens-lib :as ctob]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dh]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

;; Change this to :info :debug or :trace to debug this module, or :warn to reset to default
(log/set-level! :warn)

;; Token Reference Scanning
;; ========================

(defn scan-shape-applied-tokens
  "Scan a shape for applied token references to a specific token name"
  [shape token-name container]
  (when-let [applied-tokens (:applied-tokens shape)]
    (for [[attribute applied-token-name] applied-tokens
          :when (= applied-token-name token-name)]
      {:type :applied-token
       :shape-id (:id shape)
       :attribute attribute
       :token-name applied-token-name
       :container container})))

(defn scan-token-value-references
  "Scan a token value for references to a specific token name (alias), supporting complex token values."
  [token token-name]
  (letfn [(find-all-token-value-references [token-value]
            (cond
              (string? token-value)
              (filter #(= % token-name) (cto/find-token-value-references token-value))

              (map? token-value)
              (mapcat find-all-token-value-references (vals token-value))

              (sequential? token-value)
              (mapcat find-all-token-value-references token-value)

              :else
              []))]
    (when-let [value (:value token)]
      (for [referenced-token-name (find-all-token-value-references value)]
        {:type :token-alias
         :source-token-id (:id token)
         :referenced-token-name referenced-token-name}))))

(defn scan-animation-style-references
  "Scan the animation styles of the file for entries whose settings bind
  the token `token-name` (see `cta/schema:style-entry-tokens`)."
  [file-data token-name]
  (for [style (vals (:animation-styles file-data))
        :when (some (fn [entry]
                      (some #(= token-name %) (vals (:applied-tokens entry))))
                    (:entries style))]
    {:type :animation-style
     :style-id (:id style)}))

(defn scan-animation-timeline-references
  "Scan the timelines of the pages of the file for presets and keyframes
  whose settings bind the token `token-name` (the uses of styles take the
  names of their style): one reference by preset or keyframe."
  [file-data token-name]
  (for [page      (vals (:pages-index file-data))
        [board-id timeline] (:timelines page)
        track     (vals (:tracks timeline))
        item      (concat (remove cta/style-use? (:animations track)) (:keyframes track))
        :when (some #(= token-name %) (vals (:applied-tokens item)))]
    {:type :animation-timeline
     :page-id (:id page)
     :board-id board-id}))

(defn scan-workspace-token-references
  "Scan entire workspace for all token references to a specific token"
  [file-data old-token-name]
  (let [tokens-lib (:tokens-lib file-data)
        containers (object-containers-seq file-data)

        ;; Scan all shapes for applied token references to the specific token
        matching-applied (mapcat (fn [container]
                                   (let [shapes (shapes-seq container)]
                                     (mapcat #(scan-shape-applied-tokens % old-token-name container) shapes)))
                                 containers)

        ;; Scan tokens library for alias references to the specific token
        matching-aliases (if tokens-lib
                           (let [all-tokens (ctob/get-all-tokens tokens-lib)]
                             (mapcat #(scan-token-value-references % old-token-name) all-tokens))
                           [])

        ;; Scan the animation styles of the file for settings bound to it
        matching-styles (scan-animation-style-references file-data old-token-name)

        ;; and the presets and keyframes of its timelines
        matching-timelines (scan-animation-timeline-references file-data old-token-name)]
    (log/info :hint "token-scan-details"
              :token-name old-token-name
              :containers-count (count containers)
              :total-applied-refs (count matching-applied)
              :matching-applied (count matching-applied)
              :total-alias-refs (count matching-aliases)
              :matching-aliases (count matching-aliases))

    {:applied-tokens matching-applied
     :token-aliases matching-aliases
     :animation-styles matching-styles
     :animation-timelines matching-timelines
     :total-references (+ (count matching-applied) (count matching-aliases) (count matching-styles)
                          (count matching-timelines))}))

;; Token Remapping Core Logic
;; ==========================

(defn build-remap-changes
  "Build the pending changes required to rename a token from `old-token-name`
   to `new-token-name`, covering applied-token references on shapes and alias
   references in other tokens. Updates to copy shapes are committed with
   `:ignore-touched true` so the rename does not flip sync groups into the
   :touched set and silently break later main→copy propagation."
  [file-data old-token-name new-token-name & {:keys [undo-group]}]
  (let [scan-results     (scan-workspace-token-references file-data old-token-name)
        tokens-lib       (:tokens-lib file-data)
        sets             (ctob/get-sets tokens-lib)
        tokens-with-sets (mapcat (fn [set]
                                   (map (fn [token]
                                          {:token token :set set})
                                        (vals (ctob/get-tokens tokens-lib (ctob/get-id set)))))
                                 sets)

        ;; Group applied token references by container
        refs-by-container (group-by :container (:applied-tokens scan-results))

        ;; Use apply-token logic to update shapes for both direct and alias references
        shape-changes
        (reduce-kv
         (fn [changes container refs]
           (let [;; Each shape keeps only the attributes it already had the
                 ;; token on, so the rename never applies the token to new
                 ;; attributes (e.g. a stroke-width token on a single side).
                 attributes-by-shape (update-vals (group-by :shape-id refs)
                                                  #(into #{} (map :attribute) %))
                 ;; Find the correct token to apply (new or alias)
                 token               (or (some #(when (= (:name (:token %)) new-token-name) %) tokens-with-sets)
                                         (some #(when (= (:name (:token %)) old-token-name) %) tokens-with-sets))]
             (if token
               ;; Create a new independent changes so we can call `with-file-data` after `with-container`
               ;; otherwise it causes probelms looking up for objects
               (let [container-changes
                     (-> (pcb/empty-changes)
                         (pcb/with-container container)
                         (pcb/with-file-data file-data)
                         (pcb/update-shapes (keys attributes-by-shape)
                                            (fn [shape]
                                              (let [attributes (get attributes-by-shape (:id shape))]
                                                (update shape :applied-tokens
                                                        #(merge % (cft/attributes-map attributes (:token token))))))
                                            {:ignore-touched true}))]
                 (pcb/concat-changes changes container-changes))
               changes)))
         (-> (pcb/empty-changes)
             (pcb/with-library-data file-data))
         refs-by-container)

        ;; Create changes for updating token alias references
        token-changes
        (reduce
         (fn [changes ref]
           (let [source-token-id (:source-token-id ref)]
             (when-let [{:keys [token set]} (some #(when (= (:id (:token %)) source-token-id) %) tokens-with-sets)]
               (let [old-value (:value token)
                     new-value (cto/update-token-value-references old-value old-token-name new-token-name)]
                 (pcb/set-token changes (ctob/get-id set) (:id token)
                                (assoc token :value new-value))))))
         shape-changes
         (:token-aliases scan-results))

        rename
        #(if (= % old-token-name) new-token-name %)

        ;; Rename it in the presets and keyframes of the timelines that
        ;; bind it
        timeline-page-ids
        (into #{} (map :page-id) (:animation-timelines scan-results))

        timeline-changes
        (if (seq timeline-page-ids)
          (cltl/commit-page-timelines token-changes
                                      (keep #(get-in file-data [:pages-index %]) timeline-page-ids)
                                      #(cta/rename-timeline-tokens % rename))
          token-changes)

        ;; and in the animation styles that bind it, then sync their uses,
        ;; which keep the names of the tokens too: last, from the pages with
        ;; the timelines renamed, so the sync writes over those changes
        renamed-styles
        (when (seq (:animation-styles scan-results))
          (ctas/token-renamed-styles (:animation-styles file-data) rename))

        changes
        (if (seq renamed-styles)
          (let [file-id    (:id file-data)
                file-data' (-> file-data
                               (update :animation-styles
                                       (fn [styles] (reduce #(assoc %1 (:id %2) %2) styles renamed-styles)))
                               (update :pages-index
                                       (fn [pages]
                                         (reduce (fn [pages page-id]
                                                   (d/update-in-when pages [page-id :timelines]
                                                                     d/update-vals #(cta/rename-timeline-tokens % rename)))
                                                 pages
                                                 timeline-page-ids))))]
            (-> (reduce pcb/update-animation-style timeline-changes renamed-styles)
                (cll/generate-sync-timelines file-id nil file-id {file-id {:data file-data'}})))
          timeline-changes)]

    (cond-> changes
      (some? undo-group)
      (assoc :undo-group undo-group))))

(defn remap-tokens
  "Main function to remap all token references when a token name changes"
  [old-token-name new-token-name & {:keys [undo-group]}]
  (ptk/reify ::remap-tokens
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-data     (dh/lookup-file-data state)
            token-changes (build-remap-changes file-data old-token-name new-token-name :undo-group undo-group)]
        (log/info :hint "token-remapping"
                  :old-name old-token-name
                  :new-name new-token-name)
        (rx/of (dch/commit-changes token-changes))))))

(defn bulk-remap-tokens
  "Helper function to remap a batch of tokens, used for node renaming"
  [tokens-in-path new-tokens & {:keys [undo-group]}]
  (ptk/reify ::bulk-remap-tokens
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/concat
       (map (fn [old-token new-token]
              (remap-tokens (:name old-token) (:name new-token) :undo-group undo-group))
            tokens-in-path
            new-tokens)))))

(defn validate-token-remapping
  "Validate that a token remapping operation is safe to perform"
  [old-name new-name]
  (cond
    (str/blank? new-name)
    {:valid? false
     :error :invalid-name
     :message "Token name cannot be empty"}
    (= old-name new-name)
    {:valid? false
     :error :no-change
     :message "New name is the same as current name"}
    :else
    {:valid? true}))

(defn count-token-references
  "Count the number of references to a token in the workspace"
  [file-data token-name]
  (let [scan-results (scan-workspace-token-references file-data token-name)]
    (log/info :hint "token-reference-scan"
              :token-name token-name
              :applied-refs (count (:applied-tokens scan-results))
              :alias-refs (count (:token-aliases scan-results))
              :style-refs (count (:animation-styles scan-results))
              :total (:total-references scan-results))
    (:total-references scan-results)))
