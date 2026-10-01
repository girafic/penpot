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

(defn animations->entries
  "The entries of a new style combining `animations`, those of a track:
  the presets and the entries of the uses (see `cta/track-animations`)
  that are neither locked nor hidden, by start, the first one starting at
  0, with new ids. `{:entries [...] :start ms}`, `:start` being where the
  first one starts in the track, so that a use of the new style from
  there plays as they do; or `{:error :too-many-entries :count n}` above
  `cta/max-style-entries` and `{:error :no-entries :count 0}` without
  any."
  [animations]
  (let [presets (sort-by :start (cta/track-animations {:animations (filterv taken? animations)}))
        n       (count presets)
        start   (:start (first presets))]
    (cond
      (zero? n)
      {:error :no-entries :count 0}

      (> n cta/max-style-entries)
      {:error :too-many-entries :count n}

      :else
      {:entries (mapv #(cta/make-style-entry (-> % (dissoc :id) (update :start - start))) presets)
       :start start})))

(defn replace-with-use
  "`track` with `use` in place of the animations a new style takes from it
  (see `animations->entries`): at the row of the first one, the others
  removed. Locked and hidden rows keep their places. Without any to
  replace, `use` goes last."
  [track use]
  (let [animations (get track :animations [])
        index      (d/index-of-pred animations taken?)]
    (assoc track :animations
           (if (nil? index)
             (conj animations use)
             (into []
                   (keep-indexed (fn [i animation]
                                   (cond
                                     (= i index)         use
                                     (taken? animation)  nil
                                     :else               animation)))
                   animations)))))

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
  its style."
  [{:keys [hidden locked] :as use}]
  (mapv #(cta/make-animation (assoc % :id (uuid/next) :hidden hidden :locked locked))
        (cta/expand-style use)))

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
  "`timeline` with each animation that `rewrite?` in its tracks replaced,
  in place, by what `f` gives for it: one animation or a vector of them.
  `[timeline rewritten]`, `rewritten` being every animation `f` gave."
  [timeline rewrite? f]
  (let [rewritten (volatile! [])

        rewrite-track
        (fn [track]
          (if (some rewrite? (:animations track))
            (update track :animations
                    (fn [animations]
                      (into []
                            (mapcat (fn [animation]
                                      (if (rewrite? animation)
                                        (let [result (f animation)
                                              result (if (map? result) [result] result)]
                                          (vswap! rewritten into result)
                                          result)
                                        [animation])))
                            animations)))
            track))

        timeline (d/update-when timeline :tracks d/update-vals rewrite-track)]
    [timeline @rewritten]))

(defn sync-timeline
  "`timeline` with the uses of the styles of the file `library-id` (of the
  style `asset-id` only, when given) in its tracks synced with `styles`,
  the styles of that file by id (see `sync-use`): they keep their rows,
  and the presets of a use whose style is gone take its place. The
  duration grows to the end of the latest of them, never shrinks. Nil
  `styles` means the styles of that file are not known (its data is not
  loaded): then nothing changes."
  [timeline library-id styles asset-id]
  (if (nil? styles)
    timeline
    (let [synced? (fn [animation]
                    (and (cta/style-use? animation)
                         (= library-id (:style-ref-file animation))
                         (or (nil? asset-id) (= asset-id (:style-ref-id animation)))))

          [timeline' rewritten]
          (rewrite-uses timeline synced? #(sync-use % (get styles (:style-ref-id %))))]
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
  "`timeline` with every use of a style of a file other than `file-id`
  replaced by its presets (see `detach-use`)."
  [timeline file-id]
  (first (rewrite-uses timeline
                       #(and (cta/style-use? %) (not= file-id (:style-ref-file %)))
                       detach-use)))

(defn used-styles-changed-since
  "The uses, in the timelines of `page`, of styles of `library` (a file
  with its `:data`) changed after `since-date`, or gone from its data:
  `{:shape-id :asset-id :asset-type :container-id}` each, as
  `ctf/used-assets-changed-since` gives the other assets."
  [page library since-date]
  (let [library-id (:id library)
        loaded?    (some? (:data library))
        styles     (dm/get-in library [:data :animation-styles])
        changed?   (fn [{:keys [modified-at]}]
                     (or (nil? modified-at)
                         (>= (inst-ms modified-at) (inst-ms since-date))))]
    (for [timeline (vals (:timelines page))
          use      (timeline-uses timeline)
          :when    (= library-id (:style-ref-file use))
          :let     [style (get styles (:style-ref-id use))]
          :when    (if (some? style) (changed? style) loaded?)]
      {:shape-id   (::shape-id use)
       :asset-id   (:style-ref-id use)
       :asset-type :animation-style
       :container-id (:id page)})))

(defn remap-style-refs
  "`timelines` (of a page, by board) with the uses of the styles
  `style-ids` of the file `library-id` linked to the file `file-id`
  instead, which now holds those styles (see `ctf/absorb-assets`)."
  [timelines library-id file-id style-ids]
  (d/update-vals timelines
                 (fn [timeline]
                   (first (rewrite-uses timeline
                                        #(and (cta/style-use? %)
                                              (= library-id (:style-ref-file %))
                                              (contains? style-ids (:style-ref-id %)))
                                        #(assoc % :style-ref-file file-id))))))
