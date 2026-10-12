;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.audio-clip
  "The audio clips of Penpot Motion: the sound files the timeline of a
  board plays, each from its `:start` on the timeline for `:duration` ms,
  its file from `:offset` (trimmed at either edge), at a `:speed` (2 plays
  it twice as fast, and higher, as a tape does), on a `:lane` of the
  dock, with a volume, mute and fades. Times on the timeline (`:start`,
  `:duration`, the fades) are ms of the timeline; `:offset` and
  `:source-duration` are ms of the file. A clip points to its file, a
  file media object, by `:audio-id`.

  The operations are pure and read and write only the `:audio-clips`
  and `:duration` of a timeline: what plays when is
  `app.common.types.animation/audio-plan`. Adding, moving or trimming a
  clip never changes the length of the timeline, which is also its loop
  period and its export length; `fit-duration-to-audio` does, when asked.
  A locked clip takes no edit but its unlocking and a new name."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.uuid :as uuid]
   [cuerdas.core :as str]))

(def max-audio-duration
  "The longest audio file a clip may play, in ms: 5 minutes, about 115 MB
  decoded at 48 kHz in stereo, which every collaborator and viewer
  decodes. The server cannot check it: the client does, before upload
  and before decoding."
  (* 5 60 1000))

(def min-clip-duration
  "The shortest a clip gets by trimming, in ms."
  10)

(def max-volume
  "The loudest a clip plays its file: four times as loud (+12 dB)."
  4)

(def min-speed
  "The slowest a clip plays its file: a quarter of its speed."
  0.25)

(def max-speed
  "The fastest a clip plays its file: four times its speed."
  4)

(def schema:audio-clip
  [:map {:title "AudioClip"}
   [:id ::sm/uuid]
   [:audio-id ::sm/uuid]
   [:name :string]
   [:source-duration [::sm/int {:min min-clip-duration :max max-audio-duration}]]
   ;; before 0 it does not play: it sounds from the start of the
   ;; timeline on, as far into its file as it is there
   [:start ::sm/int]
   [:offset [::sm/int {:min 0}]]
   [:duration [::sm/int {:min min-clip-duration}]]
   [:lane [::sm/int {:min 0}]]
   [:volume {:optional true} [::sm/number {:min 0 :max max-volume}]]
   [:speed {:optional true} [::sm/number {:min min-speed :max max-speed}]]
   [:muted {:optional true} :boolean]
   [:fade-in {:optional true} [::sm/int {:min 0}]]
   [:fade-out {:optional true} [::sm/int {:min 0}]]
   [:locked {:optional true} :boolean]])

(def check-audio-clip
  (sm/check-fn schema:audio-clip))

(def valid-audio-clip?
  (sm/lazy-validator schema:audio-clip))

;; CLIPS

(defn- whole
  [n]
  (int (mth/round n)))

(defn clip-speed
  "How fast `clip` plays its file: 1, or its `:speed`."
  [clip]
  (or (:speed clip) 1))

(defn make-audio-clip
  "A valid clip from `params`, without the keys it does not know: a new
  `:id` when it has none, whole ms, the speed within `min-speed` and
  `max-speed` (1 left out), the part it plays within its file and at
  least `min-clip-duration` long, lane 0 when it has none, the volume
  within 0 and `max-volume` (1, the one of its file, left out) and each
  fade at most half the clip, so they never overlap."
  [{:keys [id audio-id name source-duration start offset duration lane volume speed muted fade-in fade-out locked]}]
  (let [source   (whole (mth/clamp (or source-duration min-clip-duration) min-clip-duration max-audio-duration))
        speed    (if (number? speed) (mth/clamp speed min-speed max-speed) 1)
        offset   (whole (mth/clamp (or offset 0) 0 (max 0 (- source (* min-clip-duration speed)))))
        ;; the longest it plays from there, in ms of the timeline
        room     (mth/floor (/ (- source offset) speed))
        duration (whole (mth/clamp (or duration room) (min min-clip-duration room) room))
        fade     #(when (and (number? %) (pos? %)) (whole (min % (/ duration 2))))]
    (d/without-nils
     {:id (or id (uuid/next))
      :audio-id audio-id
      :name (or name "Audio")
      :source-duration source
      :start (whole (or start 0))
      :offset offset
      :duration duration
      :lane (whole (max 0 (or lane 0)))
      :volume (when (and (number? volume) (not= volume 1)) (mth/clamp volume 0 max-volume))
      :speed (when (not= speed 1) speed)
      :muted (when (true? muted) true)
      :fade-in (fade fade-in)
      :fade-out (fade fade-out)
      :locked (when (true? locked) true)})))

(defn clip-end
  "Where `clip` ends on the timeline, in ms."
  [{:keys [start duration]}]
  (+ start duration))

(defn audio-clips
  "The clips of `timeline`, by lane and start."
  [timeline]
  (sort-by (juxt :lane :start) (vals (:audio-clips timeline))))

(defn get-audio-clip
  [timeline clip-id]
  (dm/get-in timeline [:audio-clips clip-id]))

(defn- overlaps?
  [clip start end]
  (and (< (:start clip) end) (< start (clip-end clip))))

(defn lane-takes?
  "Whether `lane` of `timeline` may hold a clip of the file `audio-id`: a
  lane plays a single file (its clips are parts of it), so it holds no
  clip of another one (but those of `except-ids`)."
  [timeline lane audio-id & {:keys [except-ids]}]
  (not-any? #(and (= lane (:lane %))
                  (not= audio-id (:audio-id %))
                  (not (contains? except-ids (:id %))))
            (vals (:audio-clips timeline))))

(defn free-lane
  "The first lane of `timeline` with no clip between `start` and `end`
  (but the clips of `except-ids`) and, given its file `audio-id`, none of
  another file (see `lane-takes?`)."
  [timeline start end & {:keys [except-ids audio-id]}]
  (let [taken (into #{}
                    (comp (remove #(contains? except-ids (:id %)))
                          (filter #(or (overlaps? % start end)
                                       (and (some? audio-id) (not= audio-id (:audio-id %)))))
                          (map :lane))
                    (vals (:audio-clips timeline)))]
    (d/seek #(not (contains? taken %)) (range))))

(defn add-audio-clip
  "`timeline` with `clip` (see `make-audio-clip`): on its lane when it has
  one that plays no other file (see `lane-takes?`), else on the first
  lane free over its span that plays no other file. The duration of the
  timeline stays."
  [timeline clip]
  (let [lane (:lane clip)
        clip (make-audio-clip clip)
        own  #{(:id clip)}
        clip (if (and (some? lane) (lane-takes? timeline lane (:audio-id clip) :except-ids own))
               clip
               (assoc clip :lane (free-lane timeline (:start clip) (clip-end clip)
                                            :audio-id (:audio-id clip)
                                            :except-ids own)))]
    (assoc-in timeline [:audio-clips (:id clip)] clip)))

(defn- edit-clip
  "`timeline` with the clip `clip-id` changed by `f` and made valid again,
  unless it is locked or missing."
  [timeline clip-id f]
  (let [clip (get-audio-clip timeline clip-id)]
    (if (or (nil? clip) (:locked clip))
      timeline
      (assoc-in timeline [:audio-clips clip-id] (make-audio-clip (f clip))))))

(defn move-audio-clip
  "`timeline` with the clip `clip-id` from `start`, on `lane` when given and
  it plays no other file (see `lane-takes?`), else on its own."
  [timeline clip-id start & {:keys [lane]}]
  (let [audio-id (:audio-id (get-audio-clip timeline clip-id))
        lane     (when (and (some? lane) (lane-takes? timeline lane audio-id :except-ids #{clip-id}))
                   lane)]
    (edit-clip timeline clip-id #(cond-> (assoc % :start start)
                                   (some? lane) (assoc :lane lane)))))

(defn trim-audio-clip-start
  "`timeline` with the clip `clip-id` starting at `start`, its end kept:
  it plays from earlier or later in its file (at its speed), never before
  the file starts nor shorter than `min-clip-duration`."
  [timeline clip-id start]
  (edit-clip timeline clip-id
             (fn [{:keys [offset duration] :as clip}]
               (let [speed (clip-speed clip)
                     end   (clip-end clip)
                     start (mth/clamp start (- (:start clip) (/ offset speed)) (- end min-clip-duration))
                     delta (- start (:start clip))]
                 (assoc clip :start start :offset (+ offset (* delta speed)) :duration (- duration delta))))))

(defn trim-audio-clip-end
  "`timeline` with the clip `clip-id` ending at `end`: it plays less or
  more of its file, never past the end of the file nor shorter than
  `min-clip-duration` (see `make-audio-clip`)."
  [timeline clip-id end]
  (edit-clip timeline clip-id #(assoc % :duration (- end (:start %)))))

(defn split-audio-clip
  "`timeline` with the clip `clip-id` cut at `time` into two clips on the
  same lane and file: the left one keeps its id and fade in, the right
  one, `new-id`, its fade out. Nothing changes for a time at or outside
  its edges, nor when a part would be shorter than `min-clip-duration`."
  [timeline clip-id time new-id]
  (let [clip (get-audio-clip timeline clip-id)
        cut  (some-> clip :start (#(- time %)))]
    (if (or (nil? clip) (:locked clip)
            (< cut min-clip-duration)
            (< (- (:duration clip) cut) min-clip-duration))
      timeline
      (let [cut   (whole cut)
            left  (-> clip (assoc :duration cut) (dissoc :fade-out))
            right (-> clip
                      (assoc :id new-id
                             :start (+ (:start clip) cut)
                             :offset (+ (:offset clip) (* cut (clip-speed clip)))
                             :duration (- (:duration clip) cut))
                      (dissoc :fade-in))]
        (-> timeline
            (assoc-in [:audio-clips clip-id] (make-audio-clip left))
            (assoc-in [:audio-clips new-id] (make-audio-clip right)))))))

(defn set-audio-clip-offset
  "`timeline` with the clip `clip-id` playing its file from `offset` ms,
  its start and duration kept, but shorter when the file ends first."
  [timeline clip-id offset]
  (edit-clip timeline clip-id #(assoc % :offset offset)))

(defn set-audio-clip-speed
  "`timeline` with the clip `clip-id` playing its file at `speed` (see
  `min-speed`, `max-speed`): the same part of it, so it lasts longer or
  shorter on the timeline."
  [timeline clip-id speed]
  (edit-clip timeline clip-id
             (fn [clip]
               (let [speed (mth/clamp speed min-speed max-speed)]
                 (assoc clip
                        :speed speed
                        :duration (* (:duration clip) (/ (clip-speed clip) speed)))))))

(defn set-audio-clip-volume
  "`timeline` with the volume of the clip `clip-id` at `volume`, from 0 to
  `max-volume` (1 plays its file as it is)."
  [timeline clip-id volume]
  (edit-clip timeline clip-id #(assoc % :volume volume)))

(defn set-audio-clip-muted
  [timeline clip-id muted?]
  (edit-clip timeline clip-id #(assoc % :muted muted?)))

(defn set-audio-clip-fades
  "`timeline` with the fades of the clip `clip-id`, in ms (nil or 0 for
  none)."
  [timeline clip-id fade-in fade-out]
  (edit-clip timeline clip-id #(assoc % :fade-in fade-in :fade-out fade-out)))

(defn rename-audio-file
  "`timeline` with the clips that play the file `audio-id` named `name`,
  trimmed (nothing when blank): the name shown on them and on their
  lane. Locked ones too, as a locked layer takes a new name."
  [timeline audio-id name]
  (let [name (str/trim (or name ""))]
    (if (str/blank? name)
      timeline
      (update timeline :audio-clips
              (fn [clips]
                (d/update-vals clips #(cond-> % (= audio-id (:audio-id %)) (assoc :name name))))))))

(defn set-audio-clip-locked
  "`timeline` with the clip `clip-id` locked or not: with a new name (see
  `rename-audio-file`), the one change a locked clip takes."
  [timeline clip-id locked?]
  (if (some? (get-audio-clip timeline clip-id))
    (update-in timeline [:audio-clips clip-id]
               #(make-audio-clip (assoc % :locked locked?)))
    timeline))

(defn remove-audio-clip
  "`timeline` without the clip `clip-id`, unless it is locked."
  [timeline clip-id]
  (let [clip (get-audio-clip timeline clip-id)]
    (if (or (nil? clip) (:locked clip))
      timeline
      (let [clips (dissoc (:audio-clips timeline) clip-id)]
        (if (empty? clips)
          (dissoc timeline :audio-clips)
          (assoc timeline :audio-clips clips))))))

(defn move-lane
  "`timeline` with the clips of `lane` on the `to`th of the lanes that hold
  clips, the others in their order around it, and those lanes numbered
  from 0 again. The order of the lanes is not an edit of the clips: a
  lock does not keep a clip from it."
  [timeline lane to]
  (let [clips (:audio-clips timeline)
        lanes (vec (sort (distinct (map :lane (vals clips)))))]
    (if-not (some #{lane} lanes)
      timeline
      (let [others (filterv #(not= lane %) lanes)
            to     (mth/clamp to 0 (count others))
            order  (into (conj (subvec others 0 to) lane) (subvec others to))
            index  (into {} (map-indexed (fn [i l] [l i])) order)]
        (assoc timeline :audio-clips
               (into {} (map (fn [[id clip]] [id (assoc clip :lane (index (:lane clip)))])) clips))))))

(defn fit-duration-to-audio
  "`timeline` as long as its clips play: until the end of the last one.
  A timeline without clips stays."
  [timeline]
  (if-let [clips (seq (vals (:audio-clips timeline)))]
    (assoc timeline :duration (reduce max (map clip-end clips)))
    timeline))

(defn clip-times
  "The starts and ends of the clips of `timeline` but those of
  `clip-ids`, the ones being dragged: what drags snap to."
  [timeline & {:keys [clip-ids]}]
  (for [clip (vals (:audio-clips timeline))
        :when (not (contains? clip-ids (:id clip)))
        time [(:start clip) (clip-end clip)]]
    time))

;; GAIN

(defn- gain-at
  "The gain of `clip` `t` ms into it: its volume, ramped up from 0 over
  its fade in and down to 0 over its fade out."
  [{:keys [duration volume fade-in fade-out]} t]
  (let [volume (double (or volume 1))
        in     (if (and fade-in (pos? fade-in)) (min 1.0 (/ (double t) fade-in)) 1.0)
        out    (if (and fade-out (pos? fade-out)) (min 1.0 (/ (double (- duration t)) fade-out)) 1.0)]
    (* volume (max 0.0 (min in out)))))

(defn gain-points
  "The gain of `clip` over the `length` ms it plays from `from` ms into
  it, as `[[ms gain] ...]` with the ms counted from `from`: its first and
  last ms, and the ends of its fades between them (the gain is linear
  between two points)."
  [{:keys [duration fade-in fade-out] :as clip} from length]
  (let [to    (+ from length)
        knots (->> [(when fade-in fade-in)
                    (when fade-out (- duration fade-out))]
                   (filter #(and (some? %) (< from % to))))]
    (->> (concat [from] knots [to])
         (distinct)
         (sort)
         (mapv (fn [t] [(- t from) (gain-at clip t)])))))
