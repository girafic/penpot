;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation-style
  "Animation styles: a library asset holding a list of preset animations,
  its entries (see `cta/schema:style-entry`), that layers play through
  uses of it (see `cta/schema:style-instance`). A use carries a copy of
  the entries, so it plays without the style; the style drives the sync
  of its uses, as a typography does for the texts that use it."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.time :as ct]
   [app.common.types.animation :as cta]
   [app.common.types.plugins :as ctpg]
   [app.common.uuid :as uuid]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCHEMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema:animation-style
  [:map {:title "AnimationStyle"}
   [:id ::sm/uuid]
   [:name :string]
   [:path {:optional true} [:maybe :string]]
   [:description {:optional true} [:maybe :string]]
   [:modified-at {:optional true} ::ct/inst]
   [:entries [:vector {:min 1 :max cta/max-style-entries :gen/max 4} cta/schema:style-entry]]
   [:plugin-data {:optional true} ctpg/schema:plugin-data]])

(def check-animation-style
  (sm/check-fn schema:animation-style))

(def valid-animation-style?
  (sm/lazy-validator schema:animation-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-animation-style
  "An animation style from `params`, its entries made valid (see
  `cta/make-style-entry`, which keeps their ids)."
  [{:keys [id name path description entries]}]
  (d/without-nils
   {:id (or id (uuid/next))
    :name (or name "Animation style")
    :path path
    :description description
    :entries (mapv cta/make-style-entry entries)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST OF A FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn animation-styles-seq
  [file-data]
  (vals (:animation-styles file-data)))

(defn get-animation-style
  [file-data style-id]
  (dm/get-in file-data [:animation-styles style-id]))

(defn- touch
  [style]
  (assoc style :modified-at (ct/now)))

(defn add-animation-style
  [file-data style]
  (update file-data :animation-styles assoc (:id style) (touch style)))

(defn set-animation-style
  "Put `style` in place of the one of its id, when there is one: the
  whole style, so keys it leaves out go."
  [file-data style]
  (d/assoc-in-when file-data [:animation-styles (:id style)] (touch style)))

(defn update-animation-style
  [file-data style-id f & args]
  (d/update-in-when file-data [:animation-styles style-id] #(-> (apply f % args)
                                                                (touch))))

(defn delete-animation-style
  [file-data style-id]
  (d/update-when file-data :animation-styles dissoc style-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- taken?
  "Whether a new style takes `animation` from its track (see
  `animations->entries`): it is neither locked nor hidden."
  [animation]
  (not (or (:locked animation) (:hidden animation))))

(defn- taken-row?
  "Whether a new style takes the keyframes of `keyframe`'s row of `track`
  (see `animations->entries`): the row is neither hidden, locked nor
  looping (a loop repeats over the duration of a timeline, which a style
  does not have), nor the outline of a path (see `cta/style-properties`)."
  [track {:keys [property index]}]
  (let [key (cta/slot-key property index)]
    (and (contains? cta/style-properties property)
         (not (some #(contains? (get track %) key) [:hidden :locked :loops])))))

(defn keyframe-count
  "How many keyframes the keyframe entries of `entries` hold together (see
  `cta/max-style-keyframes`)."
  [entries]
  (transduce (comp (filter cta/keys-entry?) (map #(count (:keyframes %)))) + 0 entries))

(defn animations->entries
  "The entries of a new style combining what `track` plays, the track of
  a layer resting at `pose` (see `cta/rest-pose`): its presets and the
  entries of its uses (see `cta/track-animations`) that are neither
  locked nor hidden, and a keyframe entry of each row of its keyframes
  that is neither hidden, locked nor looping (see
  `cta/keyframes->keys-entry`; with the keyframe entries of the uses in
  that row, of what the row plays), by start, the first one starting at 0,
  with new ids. `{:entries [...] :start ms}`, `:start` being where the
  first one starts in the track, so that a use of the new style from
  there plays as they do; or `{:error :too-many-entries :count n}` above
  `cta/max-style-entries`, `{:error :too-many-keyframes :count n}` above
  `cta/max-style-keyframes` and `{:error :no-entries :count 0}` without
  any."
  [track pose]
  (let [presets  (cta/track-animations {:animations (filterv taken? (:animations track))})
        slot     (juxt :property :index)
        own-rows (->> (:keyframes track)
                      (filter #(taken-row? track %))
                      (group-by slot))
        ;; a row with own keyframes and keyframe entries of uses becomes
        ;; one entry of what it plays (see `cta/row-with-entries`)
        merged?  #(and (cta/keys-entry? %) (contains? own-rows (slot %)))
        rows     (map (fn [[row own]]
                        (cta/keyframes->keys-entry
                         (cta/row-with-entries (cta/sort-keyframes own)
                                               (filter #(and (cta/keys-entry? %) (= row (slot %))) presets)
                                               pose)
                         pose))
                      own-rows)
        entries  (sort-by :start (concat (remove merged? presets) rows))
        n       (count entries)
        start   (:start (first entries))]
    (cond
      (zero? n)
      {:error :no-entries :count 0}

      (> n cta/max-style-entries)
      {:error :too-many-entries :count n}

      (> (keyframe-count entries) cta/max-style-keyframes)
      {:error :too-many-keyframes :count (keyframe-count entries)}

      :else
      {:entries (mapv #(cta/make-style-entry (-> % (dissoc :id) (update :start - start))) entries)
       :start start})))

(defn entries-error
  "Why a new style cannot combine what `track` plays (see
  `animations->entries`), as `{:error :count}`, or nil when it can: what
  the layer menu tells before it saves one, which does not need where the
  layer rests."
  [track]
  (let [result (animations->entries track {:x 0 :y 0 :width 1 :height 1 :rotation 0})]
    (when (some? (:error result))
      result)))

(defn replace-with-use
  "`track` with `use` in place of what a new style takes from it (see
  `animations->entries`): at the row of the first animation it takes,
  the others and the keyframes of the rows it takes removed. Locked and
  hidden rows keep their places. Without any animation to replace, `use`
  goes last."
  [track use]
  (let [animations (get track :animations [])
        index      (d/index-of-pred animations taken?)]
    (-> track
        (update :keyframes (partial filterv #(not (taken-row? track %))))
        (assoc :animations
               (if (nil? index)
                 (conj animations use)
                 (into []
                       (keep-indexed (fn [i animation]
                                       (cond
                                         (= i index)         use
                                         (taken? animation)  nil
                                         :else               animation)))
                       animations))))))

(declare make-use)

(defn update-from-track
  "`style`, the style of the file `library-id` that the use `use-id` of
  `track` plays, with the entries of what the layer of `track`, resting
  at `pose` (see `cta/rest-pose`), plays now (see `animations->entries`:
  its presets, the entries of its uses, that one too even locked or
  hidden, and its rows of keyframes), back in the time of the style when
  that use is stretched (see `cta/expand-style`); and `track` with a use
  of the new style, still `use-id` and as stretched, in place of what the
  style took (see `replace-with-use`), so that the layer plays as it did.
  `{:style :track}`, or the errors of `animations->entries`."
  [track use-id pose style library-id]
  (let [track  (update track :animations
                       (partial mapv #(cond-> % (= use-id (:id %)) (dissoc :hidden :locked))))
        use    (d/seek #(= use-id (:id %)) (:animations track))
        length (cta/style-length (:entries use))
        k      (if (and (some? use) (pos? length)) (/ (:duration use) length) 1)
        result (animations->entries track pose)]
    (if (some? (:error result))
      result
      (let [length  (cta/style-length (:entries result))
            entries (cta/expand-style {:start 0
                                       :duration (max 1 (mth/round (/ length k)))
                                       :entries (:entries result)})
            style   (assoc style :entries entries)
            use     (-> (make-use style library-id (:start result))
                        (assoc :id use-id
                               :duration (max 1 (mth/round (* k (cta/style-length entries))))))]
        {:style style
         :track (replace-with-use track use)}))))

(defn make-use
  "A use, from `start`, of `style` of the file `library-id`: a copy of all
  its entries, as long as they play. A use keeps every entry, whatever
  layer it goes on."
  [style library-id start]
  (cta/make-animation {:type :style
                       :style-ref-id (:id style)
                       :style-ref-file library-id
                       :name (:name style)
                       :start start
                       :entries (:entries style)}))

(defn uses-style?
  "Whether `animation` is a use of the style `style-id` of the file
  `library-id`."
  [animation library-id style-id]
  (and (cta/style-use? animation)
       (= library-id (:style-ref-file animation))
       (= style-id (:style-ref-id animation))))

(defn detach-use
  "The presets `use` plays (see `cta/expand-style`), as plain presets with
  new ids and its flags: what is left of it once it no longer links to
  its style, but for its keyframe entries, which become keyframes of its
  layer (see `detach-keys`)."
  [{:keys [hidden locked] :as use}]
  (into []
        (comp (remove cta/keys-entry?)
              (map #(cta/make-animation (assoc % :id (uuid/next) :hidden hidden :locked locked))))
        (cta/expand-style use)))

(defn- entry-slot
  [{:keys [property index]}]
  (cta/slot-key property index))

(defn- detach-keys
  "`track` with the keyframe entries of its style use `use` turned into
  keyframes of its layer, which rests at `pose` (see `cta/rest-pose`),
  over a timeline `duration` ms long, as they played (see
  `cta/take-over-keys`): no own keyframe of the layer goes. A hidden use,
  which played nothing, only gives the layer the rows it has none of; the
  rows it gives get the flags of `use`, those the layer had keep theirs
  (a row the user hid stays hidden)."
  [track use pose duration]
  (let [own-slots (into #{} (map entry-slot) (:keyframes track))
        entries   (cond->> (filterv cta/keys-entry? (cta/expand-style use))
                    (true? (:hidden use))
                    (filterv #(not (contains? own-slots (entry-slot %)))))]
    (if (empty? entries)
      track
      (let [new-slots (into #{} (comp (map entry-slot) (remove own-slots)) entries)
            flag      (fn [track flag]
                        (cond-> track
                          (and (true? (get use flag)) (seq new-slots))
                          (update flag (fnil into #{}) new-slots)))]
        (-> (cta/take-over-keys track entries pose duration)
            (flag :hidden)
            (flag :locked))))))

(defn detach-track-use
  "`track` of `timeline`, of a layer of `objects`, with its style use
  `use-id` replaced, in place, by what is left of it once it no longer
  links to its style: its presets (see `detach-use`), its keyframe
  entries turned into keyframes of the layer (see `detach-keys`)."
  [track use-id timeline objects]
  (if-let [use (d/seek #(= use-id (:id %)) (:animations track))]
    (-> track
        (detach-keys use (cta/rest-pose timeline objects (:shape-id track)) (:duration timeline))
        (assoc :animations (into [] (mapcat #(if (= use-id (:id %)) (detach-use %) [%])) (:animations track))))
    track))

(defn sync-use
  "`use` with the entries and the name of `style`, now. It lasts as long
  as the new entries play, unless it was stretched (its duration was not
  the length of its entries): then the new entries scale into it. With
  `style` nil (deleted), the presets of `use` (see `detach-use`)."
  [use style]
  (if (nil? style)
    (detach-use use)
    (let [stretched? (not= (:duration use) (cta/style-length (:entries use)))]
      (cta/make-animation
       (assoc use
              :name (:name style)
              :entries (:entries style)
              :duration (if stretched?
                          (:duration use)
                          (cta/style-length (:entries style))))))))

(defn- rewrite-uses
  "`timeline`, of the layers of `objects`, with each animation that
  `rewrite?` in its tracks replaced, in place, by what `f` gives for it:
  one animation, or `{:detach presets}` for a use it detaches (see
  `detach-use`), whose keyframe entries then become keyframes of its
  layer (see `detach-keys`). `[timeline rewritten]`, `rewritten` being
  every animation `f` gave."
  [timeline objects rewrite? f]
  (let [rewritten (volatile! [])

        rewrite-track
        (fn [track]
          (if (some rewrite? (:animations track))
            (let [detached (volatile! [])
                  track    (update track :animations
                                   (fn [animations]
                                     (into []
                                           (mapcat (fn [animation]
                                                     (if (rewrite? animation)
                                                       (let [result (f animation)
                                                             detach (contains? result :detach)
                                                             result (if detach (:detach result) [result])]
                                                         (when detach
                                                           (vswap! detached conj animation))
                                                         (vswap! rewritten into result)
                                                         result)
                                                       [animation])))
                                           animations)))
                  pose     (when (seq @detached)
                             (cta/rest-pose timeline objects (:shape-id track)))]
              (reduce #(detach-keys %1 %2 pose (:duration timeline)) track @detached))
            track))

        timeline (d/update-when timeline :tracks d/update-vals rewrite-track)]
    [timeline @rewritten]))

(defn sync-timeline
  "`timeline`, of the layers of `objects`, with the uses of the styles of
  the file `library-id` (of the style `asset-id` only, when given) in its
  tracks synced with `styles`, the styles of that file by id (see
  `sync-use`): they keep their rows, and the presets of a use whose style
  is gone take its place, its keyframe entries becoming keyframes of its
  layer. The duration grows to the end of the latest of them, never
  shrinks. Nil `styles` means the styles of that file are not known (its
  data is not loaded): then nothing changes."
  [timeline objects library-id styles asset-id]
  (if (nil? styles)
    timeline
    (let [synced? (fn [animation]
                    (and (cta/style-use? animation)
                         (= library-id (:style-ref-file animation))
                         (or (nil? asset-id) (= asset-id (:style-ref-id animation)))))

          [timeline' rewritten]
          (rewrite-uses timeline objects synced?
                        #(if-let [style (get styles (:style-ref-id %))]
                           (sync-use % style)
                           {:detach (detach-use %)}))]
      (if (= timeline timeline')
        timeline
        (update timeline' :duration max (reduce max 0 (map cta/animation-end rewritten)))))))

(defn- timeline-uses
  "The uses of animation styles in the tracks of `timeline`, each with
  the id of the shape of its track as `::shape-id`."
  [timeline]
  (for [track     (vals (:tracks timeline))
        animation (:animations track)
        :when     (cta/style-use? animation)]
    (assoc animation ::shape-id (:shape-id track))))

(defn page-uses
  "The uses of animation styles in the timelines of `page`."
  [page]
  (mapcat timeline-uses (vals (:timelines page))))

(defn drifted?
  "Whether a use in `timeline` of a style of its own file `file-id` holds
  other entries or another name than the style, in `styles`, or holds a
  style that is gone: the uses a sync missed, as when a whole track was
  written with old entries after the style changed."
  [timeline file-id styles]
  (boolean
   (some (fn [{:keys [style-ref-id] :as use}]
           (and (= file-id (:style-ref-file use))
                (let [style (get styles style-ref-id)]
                  (or (nil? style)
                      (not= (:entries style) (:entries use))
                      (not= (:name style) (:name use))))))
         (timeline-uses timeline))))

(defn detach-foreign-uses
  "`timeline`, of the layers of `objects`, with every use of a style of a
  file other than `file-id` replaced by its presets, its keyframe entries
  becoming keyframes of its layer (see `detach-use`)."
  [timeline objects file-id]
  (first (rewrite-uses timeline
                       objects
                       #(and (cta/style-use? %) (not= file-id (:style-ref-file %)))
                       #(hash-map :detach (detach-use %)))))

(defn used-styles-changed-since
  "The uses, in the timelines of `page`, of styles of `library` (a file
  with its `:data`) changed after `since-date` (an instant or ms), or
  gone from its data: `{:shape-id :asset-id :asset-type :container-id}`
  each, as `ctf/used-assets-changed-since` gives the other assets, and
  the `:name` the use copied, which is all that is left of a style gone."
  [page library since-date]
  (let [library-id (:id library)
        loaded?    (some? (:data library))
        styles     (dm/get-in library [:data :animation-styles])
        since      (inst-ms (ct/inst since-date))
        changed?   (fn [{:keys [modified-at]}]
                     (or (nil? modified-at)
                         (>= (inst-ms modified-at) since)))]
    (for [use   (page-uses page)
          :when (= library-id (:style-ref-file use))
          :let  [style (get styles (:style-ref-id use))]
          :when (if (some? style) (changed? style) loaded?)]
      {:shape-id   (::shape-id use)
       :asset-id   (:style-ref-id use)
       :asset-type :animation-style
       :container-id (:id page)
       :name       (:name use)})))

(defn remap-style-refs
  "`timelines` (of a page, by board) with the uses of the styles
  `style-ids` of the file `library-id` linked to the file `file-id`
  instead, which now holds those styles (see `ctf/absorb-assets`)."
  [timelines library-id file-id style-ids]
  (d/update-vals timelines
                 (fn [timeline]
                   (first (rewrite-uses timeline
                                        nil
                                        #(and (cta/style-use? %)
                                              (= library-id (:style-ref-file %))
                                              (contains? style-ids (:style-ref-id %)))
                                        #(assoc % :style-ref-file file-id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESIGN TOKENS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn resolve-style-tokens
  "`style` with the settings the design tokens of its entries set taken
  from `values`, the resolved values of tokens by name (see
  `cta/resolve-animation-tokens`), the easings of the keyframes of its
  keyframe entries too."
  [style values]
  (update style :entries (partial mapv #(cta/resolve-animation-tokens % values))))

(defn token-resolved-styles
  "The animation styles of `styles` (by id) that the resolved token
  `values` change, as they become: the styles of a file to commit when
  its tokens or its active themes change. The uses follow by sync, so
  only the file of a style resolves its tokens."
  [styles values]
  (into []
        (keep (fn [style]
                (let [style' (resolve-style-tokens style values)]
                  (when (not= style style')
                    style'))))
        (vals styles)))

(defn token-renamed-styles
  "The animation styles of `styles` (by id) whose entries name a design
  token that `rename` (a function of the old name, returning it when it
  stays) renames, with the new names."
  [styles rename]
  (into []
        (keep (fn [style]
                (let [style' (update style :entries
                                     (partial mapv #(cta/rename-animation-tokens % rename)))]
                  (when (not= style style')
                    style'))))
        (vals styles)))
