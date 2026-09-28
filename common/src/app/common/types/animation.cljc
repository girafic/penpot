;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation
  "Keyframe based timeline animations (a.k.a. Penpot Motion).

  A `timeline` is a page level, uuid-keyed object (stored on the page
  under `:timelines`, mirroring `:flows`). Each timeline holds a map of
  `tracks` keyed by the target shape-id, and each track holds a vector of
  `keyframes`. A keyframe animates a single property of a shape at a given
  time.

  This namespace also contains the pure interpolation engine
  (`values-at`, `timeline->modif-tree`, `timeline->opacity`) shared by the
  editor preview, the viewer playback and the exporter, so the animation
  is computed identically everywhere."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.color :as clr]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.shape.interactions :as cti]
   [app.common.types.shape.layout :as ctl]
   [app.common.uuid :as uuid]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCHEMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Properties that a keyframe can animate. Reuses the existing easing
;; presets from interactions plus a custom cubic-bezier curve.
(def animatable-properties
  #{:x :y :width :height :opacity :rotation :scale-x :scale-y
    :r1 :r2 :r3 :r4
    :fill-color :fill-opacity
    :stroke-color :stroke-opacity :stroke-width
    :shadow-offset-x :shadow-offset-y :shadow-blur
    :shadow-spread :shadow-color :shadow-opacity
    :blur :background-blur
    ;; path trim: the part of the outline the strokes are drawn along
    :trim-start :trim-end :trim-offset})

(def trim-properties
  "Path trim, as fractions of the outline: the strokes are drawn from
  `:trim-start` to `:trim-end`, moved along by `:trim-offset`. A shape has
  no trim of its own: all of it is drawn."
  {:trim-start 0 :trim-end 1 :trim-offset 0})

(def color-properties
  #{:fill-color :stroke-color :shadow-color})

(def indexed-properties
  #{:fill-color :fill-opacity
    :stroke-color :stroke-opacity :stroke-width
    :shadow-offset-x :shadow-offset-y :shadow-blur
    :shadow-spread :shadow-color :shadow-opacity})

(defn color-property?
  [property]
  (contains? color-properties property))

(defn indexed-property?
  [property]
  (contains? indexed-properties property))

(defn slot-key
  "The key of the keyframes of `property` (and optional `index`) in the
  `:loops`, `:hidden` and `:locked` sets of a track."
  [property index]
  (if (some? index)
    [property index]
    property))

(def interpolation-types
  #{:linear :bezier :step})

;; A custom cubic-bezier easing curve, expressed as the two control
;; points (x1 y1 x2 y2) like the CSS `cubic-bezier()` function.
(def schema:bezier-easing
  [:map {:title "BezierEasing"}
   [:type [:= :bezier]]
   [:curve [:tuple ::sm/safe-number ::sm/safe-number ::sm/safe-number ::sm/safe-number]]])

;; A damped spring (stiffness, damping and mass, like Figma's), stretched
;; so it settles at the end of the segment.
(def schema:spring-easing
  [:map {:title "SpringEasing"}
   [:type [:= :spring]]
   [:stiffness ::sm/safe-number]
   [:damping ::sm/safe-number]
   [:mass ::sm/safe-number]])

(def schema:easing
  ;; JSON gives the type as a string, and decoding picks the branch before
  ;; turning it into a keyword.
  [:multi {:dispatch (fn [v] (if (map? v) (some-> (:type v) keyword) :preset))
           :title "KeyframeEasing"}
   [:preset [::sm/one-of cti/easing-types]]
   [:bezier schema:bezier-easing]
   [:spring schema:spring-easing]])

(def schema:loop-key
  [:or
   [::sm/one-of animatable-properties]
   [:tuple [::sm/one-of animatable-properties] ::sm/safe-int]])

(def schema:keyframe
  [:map {:title "Keyframe"}
   [:id ::sm/uuid]
   [:time ::sm/safe-int]
   [:property [::sm/one-of animatable-properties]]
   [:index {:optional true} ::sm/safe-int]
   [:value [:or ::sm/safe-number clr/schema:hex-color]]
   [:easing {:optional true} schema:easing]
   [:interpolation {:optional true} [::sm/one-of interpolation-types]]
   ;; Motion path: for `:x` and `:y`, how far along its axis the curve
   ;; leaves the keyframe towards the next one (`:path-out`) and comes into
   ;; it from the previous one (`:path-in`), like the handles of a bezier
   ;; node. Without them the position moves in a straight line.
   [:path-in {:optional true} ::sm/safe-number]
   [:path-out {:optional true} ::sm/safe-number]])

;; A preset animation (like Figma's animation styles): an effect over a
;; span of time that stays editable. `:in` plays from the effect to the
;; layer's own values, `:out` from them to the effect. Within its span it
;; takes over the keyframes of the properties it animates.
(def animation-types
  #{:fade :move :scale :rotate})

(def animation-directions
  #{:in :out})

(def schema:animation
  [:map {:title "PresetAnimation"}
   [:id ::sm/uuid]
   [:type [::sm/one-of animation-types]]
   [:direction [::sm/one-of animation-directions]]
   [:start ::sm/safe-int]
   [:duration ::sm/safe-int]
   [:easing {:optional true} schema:easing]
   ;; fade: opacity factor, scale: scale factor, rotate: degrees
   [:amount {:optional true} ::sm/safe-number]
   ;; move: offset in px
   [:offset-x {:optional true} ::sm/safe-number]
   [:offset-y {:optional true} ::sm/safe-number]
   ;; left out of playback and exports / kept from edits
   [:hidden {:optional true} :boolean]
   [:locked {:optional true} :boolean]])

(def schema:track
  [:map {:title "AnimationTrack"}
   [:shape-id ::sm/uuid]
   [:keyframes [:vector {:gen/max 8} schema:keyframe]]
   [:animations {:optional true} [:vector {:gen/max 4} schema:animation]]
   ;; Properties (or `[property index]`) whose keyframes repeat, are left
   ;; out of playback and exports, or are kept from edits.
   [:loops {:optional true} [::sm/set schema:loop-key]]
   [:hidden {:optional true} [::sm/set schema:loop-key]]
   [:locked {:optional true} [::sm/set schema:loop-key]]
   ;; Normalized pivot (0–1) for scale and rotation. Absent is center.
   [:origin {:optional true}
    [:map
     [:x ::sm/safe-number]
     [:y ::sm/safe-number]]]])

;; What playback does at the end of a timeline: stop (`:once`), start
;; over (`:loop`) or play back to the start and on (`:ping-pong`).
(def playback-modes
  #{:once :loop :ping-pong})

;; A timeline is scoped to a board (top-level frame). The page-level
;; `:timelines` map is keyed by `:board-id`, so each board owns at most
;; one timeline (like Figma Motion).
(def schema:marker
  "A named moment of a timeline, see `add-marker`."
  [:map {:title "AnimationMarker"}
   [:id ::sm/uuid]
   [:time ::sm/safe-int]
   [:name :string]])

(def export-formats
  #{:mp4 :webm :gif :avif :svg :lottie})

(def schema:export
  "How the board of a timeline is exported as an animation, as last set
  in the export panel, which offers it again."
  [:map {:title "AnimationExport"}
   [:format {:optional true} [::sm/one-of export-formats]]
   [:quality {:optional true} [::sm/one-of #{:low :medium :high}]]
   [:fps {:optional true} ::sm/safe-int]
   [:size {:optional true} :string]
   [:loop {:optional true} :boolean]])

(def schema:timeline
  [:map {:title "AnimationTimeline"}
   [:board-id ::sm/uuid]
   [:name :string]
   [:duration ::sm/safe-int]
   [:playback {:optional true} [::sm/one-of playback-modes]]
   ;; Timelines saved before `:playback` only say whether they loop.
   [:loop {:optional true} :boolean]
   [:loop-count {:optional true} [:maybe ::sm/safe-int]]
   [:markers {:optional true} [:vector {:gen/max 3} schema:marker]]
   [:export {:optional true} schema:export]
   [:tracks [:map-of {:gen/max 3} ::sm/uuid schema:track]]])

(def schema:timeline-attrs
  "Settings of a timeline to change, its board and tracks aside. A nil
  value removes an optional one."
  [:map {:title "AnimationTimelineAttrs"}
   [:name {:optional true} :string]
   [:duration {:optional true} ::sm/safe-int]
   [:playback {:optional true} [:maybe [::sm/one-of playback-modes]]]
   [:loop {:optional true} [:maybe :boolean]]
   [:loop-count {:optional true} [:maybe ::sm/safe-int]]
   [:markers {:optional true} [:maybe [:vector {:gen/max 3} schema:marker]]]
   [:export {:optional true} [:maybe schema:export]]])

(def schema:timelines
  [:map-of {:gen/max 2} ::sm/uuid schema:timeline])

(sm/register! ::keyframe schema:keyframe)
(sm/register! ::animation schema:animation)
(sm/register! ::track schema:track)
(sm/register! ::timeline schema:timeline)

(def check-timeline
  (sm/check-fn schema:timeline))

(def valid-timeline?
  (sm/lazy-validator schema:timeline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-duration 1000)

(def max-export-frames 1800)

(def ^:private ref-export-pixels (* 1920 1080))

(def ^:private quality-bitrate
  {:low 2500000
   :medium 5000000
   :high 8000000})

(defn frame-times
  "Sample times in ms from 0 through `duration-ms` at `fps`, inclusive.
  Duration or fps of 0 yields `[0]`."
  [duration-ms fps]
  (let [duration (max 0 (int duration-ms))
        fps      (max 0 (int fps))]
    (if (or (zero? duration) (zero? fps))
      [0]
      (let [steps (max 1 (mth/round (* (/ (double duration) 1000.0) fps)))]
        (mapv (fn [i]
                (mth/round (* i (/ (double duration) steps))))
              (range (inc steps)))))))

(defn ping-pong-frames
  "`frames` followed by the same frames backwards, for a ping-pong
  export. A loop leaves out the first frame at the end, as it shows it
  next."
  [frames loop?]
  (let [frames (vec frames)
        back   (rest (rseq frames))]
    (into frames (if loop? (butlast back) back))))

(defn video-bitrate
  "Target bits per second for `quality`, scaled from 1920×1080."
  [width height quality]
  (let [base (get quality-bitrate quality (:medium quality-bitrate))
        area (max 1 (* (max 1 (int width)) (max 1 (int height))))]
    (max 1 (mth/round (* base (/ (double area) ref-export-pixels))))))

(defn gif-colors
  "Palette size for a GIF at `quality`."
  [quality]
  (case quality
    :low 64
    :high 256
    128))

(defn estimated-video-bytes
  [width height quality duration-ms]
  (let [bps  (video-bitrate width height quality)
        secs (/ (max 0 (int duration-ms)) 1000.0)]
    (mth/round (* bps secs 0.125))))

(defn estimated-gif-bytes
  [width height frame-count]
  (mth/round (* (max 1 (int width))
                (max 1 (int height))
                (max 1 (int frame-count))
                0.35)))

(defn- painted-children
  "The ids of the children of `id` in the order the canvas paints them,
  the bottom-most first: as `:shapes` lists them, but a flex or grid
  board sorts them by z-index and paints its first one on top."
  [objects id]
  (let [shape (get objects id)
        ids   (get shape :shapes [])]
    (if (ctl/any-layout? shape)
      (mapv :id (ctl/sort-layout-children-z-index (keep #(get objects %) ids)
                                                  (and (ctl/flex-layout? shape) (ctl/reverse? shape))))
      (vec ids))))

(defn export-layer-ids
  "Shape ids that become their own export layer: every tracked shape
  plus its siblings, in paint order (see `painted-children`), each
  before its descendants."
  [objects board-id tracks]
  (let [tracked (set (keys tracks))
        parents (into #{} (keep #(get-in objects [% :parent-id]) tracked))
        layer?  (into #{}
                      (mapcat #(get-in objects [% :shapes] []))
                      parents)]
    (->> (tree-seq (fn [id] (seq (get-in objects [id :shapes])))
                   (fn [id] (painted-children objects id))
                   board-id)
         (remove #{board-id})
         (filter layer?)
         vec)))

(defn make-timeline
  "Create a timeline for `board-id` (a top-level frame). The page-level
  `:timelines` map is keyed by this same board-id."
  [{:keys [board-id name duration playback loop-count]}]
  (cond-> {:board-id board-id
           :name (or name "Animation")
           :duration (or duration default-duration)
           :tracks {}}
    (some? playback) (assoc :playback playback)
    (some? loop-count) (assoc :loop-count loop-count)))

(defn playback-mode
  "What playback of `timeline` does at its end, one of `playback-modes`."
  [timeline]
  (or (:playback timeline)
      (if (:loop timeline) :loop :once)))

(defn cycle-duration
  "Time in ms until playback of `timeline` is back at its start: twice
  the duration in ping-pong."
  [timeline]
  (cond-> (:duration timeline)
    (= :ping-pong (playback-mode timeline)) (* 2)))

(defn playback-time
  "The time of `timeline` shown `elapsed` ms after playing from its
  start: held at the end when played once, wrapped when looping, going
  back and forth in ping-pong."
  [timeline elapsed]
  (let [duration (max 1 (:duration timeline))]
    (case (playback-mode timeline)
      :loop      (mod elapsed duration)
      :ping-pong (let [t (mod elapsed (* 2 duration))]
                   (if (> t duration) (- (* 2 duration) t) t))
      (min elapsed duration))))

(defn playback-ended?
  "Whether playback of `timeline` has stopped `elapsed` ms after its
  start, which only happens when it plays once."
  [timeline elapsed]
  (and (= :once (playback-mode timeline))
       (> elapsed (:duration timeline))))

(defn advance-playback
  "Where playback of `timeline` is `dt` ms after showing `time` going
  forward (`direction` 1) or, in ping-pong, back (-1): `{:time
  :direction}`, with `:ended?` when it stops at the end of a timeline
  played once. It only reads the timeline as it is now, so a change of
  its duration or playback mode while playing applies right away."
  [timeline time direction dt]
  (let [duration  (max 1 (:duration timeline))
        mode      (playback-mode timeline)
        direction (if (= mode :ping-pong) direction 1)
        t         (+ time (* direction dt))]
    (case mode
      :loop
      {:time (mod t duration) :direction 1}

      :ping-pong
      (cond
        (> t duration) {:time (max 0 (- (* 2 duration) t)) :direction -1}
        (neg? t)       {:time (min duration (- t)) :direction 1}
        :else          {:time t :direction direction})

      (if (< t duration)
        {:time t :direction 1}
        {:time duration :direction 1 :ended? true}))))

(defn make-keyframe
  [{:keys [id time property value easing interpolation index path-in path-out]}]
  (cond-> {:id (or id (uuid/next))
           :time (or time 0)
           :property property
           :value value}
    (some? easing) (assoc :easing easing)
    (some? interpolation) (assoc :interpolation interpolation)
    (some? path-in) (assoc :path-in path-in)
    (some? path-out) (assoc :path-out path-out)
    (some? index) (assoc :index index)))

(defn- keyframe-slot
  [keyframe]
  [(:property keyframe) (:index keyframe)])

(defn same-slot?
  "Whether `keyframe` is the same property (and index) as `property` /
  `index`."
  [keyframe property index]
  (and (= (:property keyframe) property)
       (= (:index keyframe) index)))

(defn sort-keyframes
  "Return keyframes sorted by `:time`, then `:property`, then `:index`."
  [keyframes]
  (vec (sort-by (juxt :time #(name (:property %)) :index) keyframes)))

(defn get-track
  [timeline shape-id]
  (dm/get-in timeline [:tracks shape-id]))

;; The keyframes of a property (and index) can repeat over the timeline
;; (`:loops`), be left out of playback and exports (`:hidden`) or be kept
;; from edits (`:locked`). Each flag is a set of slot keys in the track.

(def ^:private slot-flags
  [:loops :hidden :locked])

(defn slot-flag?
  "Whether `flag` (`:loops`, `:hidden` or `:locked`) is set on the
  keyframes of `property` (and optional `index`) of `shape-id`."
  ([timeline shape-id flag property]
   (slot-flag? timeline shape-id flag property nil))
  ([timeline shape-id flag property index]
   (contains? (dm/get-in timeline [:tracks shape-id flag]) (slot-key property index))))

(defn- locked-keyframe?
  [timeline shape-id keyframe]
  (slot-flag? timeline shape-id :locked (:property keyframe) (:index keyframe)))

(defn- update-slot-flag
  "Apply `f` (`conj` or `disj`) with the slot `key` to the `flag` set of
  `track`, dropping the set once empty."
  [track flag f key]
  (let [keys (f (get track flag #{}) key)]
    (if (empty? keys)
      (dissoc track flag)
      (assoc track flag keys))))

(defn- prune-slot-flags
  "`track` without the flags of the properties it has no keyframes of."
  [track]
  (let [keys (into #{} (map #(slot-key (:property %) (:index %))) (:keyframes track))]
    (reduce (fn [track flag]
              (let [kept (into #{} (filter keys) (get track flag))]
                (if (empty? kept)
                  (dissoc track flag)
                  (assoc track flag kept))))
            track
            slot-flags)))

(defn toggle-slot-flag
  "Set or clear `flag` (see `slot-flag?`) on the keyframes of `property`
  (and optional `index`) of `shape-id`."
  ([timeline shape-id flag property]
   (toggle-slot-flag timeline shape-id flag property nil))
  ([timeline shape-id flag property index]
   (let [f (if (slot-flag? timeline shape-id flag property index) disj conj)]
     (d/update-in-when timeline [:tracks shape-id]
                       update-slot-flag flag f (slot-key property index)))))

(def default-transform-origin
  {:x 0.5 :y 0.5})

(def origin-cells
  "Nine snap points for the origin picker, row-major from top-left."
  [[0 0] [0.5 0] [1 0]
   [0 0.5] [0.5 0.5] [1 0.5]
   [0 1] [0.5 1] [1 1]])

(defn track-origin
  "Normalized `{ :x :y }` pivot of `track`. Missing values are center."
  [track]
  (let [origin (:origin track)]
    {:x (double (or (:x origin) 0.5))
     :y (double (or (:y origin) 0.5))}))

(defn origin-point
  "Absolute point on `shape` for the normalized `origin`."
  [shape origin]
  (let [sr (:selrect shape)
        ox (or (:x origin) 0.5)
        oy (or (:y origin) 0.5)]
    (gpt/point (+ (or (:x sr) 0) (* (or (:width sr) 0) ox))
               (+ (or (:y sr) 0) (* (or (:height sr) 0) oy)))))

(defn- ensure-track
  [timeline shape-id]
  (if (get-track timeline shape-id)
    timeline
    (assoc-in timeline [:tracks shape-id] {:shape-id shape-id :keyframes []})))

(defn- empty-track?
  "A track with nothing left: no keyframes, animations or origin."
  [track]
  (and (empty? (:keyframes track))
       (empty? (:animations track))
       (nil? (:origin track))))

(defn set-track-origin
  "Set the scale/rotation pivot of `shape-id`. Values are 0–1 and
  clamped. The default (center) is stored as absence."
  [timeline shape-id ox oy]
  (let [ox       (mth/clamp (double ox) 0.0 1.0)
        oy       (mth/clamp (double oy) 0.0 1.0)
        timeline (ensure-track timeline shape-id)]
    (if (and (mth/close? ox 0.5) (mth/close? oy 0.5))
      (d/update-in-when timeline [:tracks shape-id] dissoc :origin)
      (assoc-in timeline [:tracks shape-id :origin] {:x ox :y oy}))))

(defn- store-track
  "Put back `track` of `shape-id` after something was removed from it:
  without the flags of the properties it has no keyframes of anymore, or
  dropped when nothing is left (no keyframes, animations nor origin)."
  [timeline shape-id track]
  (let [track (prune-slot-flags track)]
    (if (empty-track? track)
      (update timeline :tracks dissoc shape-id)
      (assoc-in timeline [:tracks shape-id] track))))

(defn add-keyframe
  "Add a keyframe to the track of `shape-id`, creating the track if
  needed. If a keyframe for the same property/time already exists it is
  replaced (so re-recording a value at the same instant updates it). A
  locked property takes none."
  [timeline shape-id keyframe]
  (if (locked-keyframe? timeline shape-id keyframe)
    timeline
    (let [keyframe (make-keyframe keyframe)
          track    (or (get-track timeline shape-id) {:shape-id shape-id :keyframes []})
          kept     (->> (:keyframes track)
                        (remove #(and (= (:time %) (:time keyframe))
                                      (same-slot? % (:property keyframe) (:index keyframe)))))
          track    (assoc track :keyframes (sort-keyframes (conj (vec kept) keyframe)))]
      (assoc-in timeline [:tracks shape-id] track))))

(defn update-keyframe
  "Apply `f` to a keyframe, unless its property is locked."
  [timeline shape-id keyframe-id f]
  (d/update-in-when timeline [:tracks shape-id :keyframes]
                    (fn [keyframes]
                      (sort-keyframes
                       (mapv #(if (and (= (:id %) keyframe-id)
                                       (not (locked-keyframe? timeline shape-id %)))
                                (f %)
                                %)
                             keyframes)))))

(defn remove-keyframe
  "Remove a keyframe, unless its property is locked. If it leaves the
  track empty (no animations nor origin either), drop the track too."
  [timeline shape-id keyframe-id]
  (let [track    (get-track timeline shape-id)
        keyframe (d/seek #(= (:id %) keyframe-id) (:keyframes track))]
    (if (or (nil? keyframe) (locked-keyframe? timeline shape-id keyframe))
      timeline
      (store-track timeline shape-id
                   (update track :keyframes (partial filterv #(not= (:id %) keyframe-id)))))))

(defn- find-keyframe
  [timeline {:keys [shape-id keyframe-id]}]
  (d/seek #(= keyframe-id (:id %))
          (dm/get-in timeline [:tracks shape-id :keyframes])))

(defn- drop-landed-on
  "`keyframes` without the ones a keyframe of `moved-ids` landed on: the
  same property (and index) at the same time."
  [keyframes moved-ids]
  (let [slot-time (juxt :property :index :time)
        landed    (into #{}
                        (comp (filter #(contains? moved-ids (:id %)))
                              (map slot-time))
                        keyframes)]
    (filterv #(or (contains? moved-ids (:id %))
                  (not (contains? landed (slot-time %))))
             keyframes)))

(defn shift-keyframes
  "`timeline` with the keyframes of `keyframes` (`{:shape-id
  :keyframe-id}` each) moved `dt` ms along, all of them as far and none
  before the start. Locked ones stay where they are. One that lands on
  another keyframe of its property takes its place, as a keyframe added
  there does (see `add-keyframe`). The duration grows to hold them."
  [timeline keyframes dt]
  (let [moving (filterv (fn [{:keys [shape-id] :as ref}]
                          (when-let [keyframe (find-keyframe timeline ref)]
                            (not (locked-keyframe? timeline shape-id keyframe))))
                        keyframes)
        times  (mapv #(:time (find-keyframe timeline %)) moving)
        dt     (if (seq times) (max dt (- (reduce min times))) 0)]
    (if (zero? dt)
      timeline
      (let [moved-ids (into #{} (map :keyframe-id) moving)
            shifted   (reduce (fn [timeline {:keys [shape-id keyframe-id]}]
                                (update-keyframe timeline shape-id keyframe-id #(update % :time + dt)))
                              timeline
                              moving)]
        (-> (reduce (fn [timeline shape-id]
                      (update-in timeline [:tracks shape-id :keyframes] drop-landed-on moved-ids))
                    shifted
                    (into #{} (map :shape-id) moving))
            (update :duration max (+ (reduce max times) dt)))))))

(defn property-keyframes
  "The (time-sorted) keyframes of `property` in the track of `shape-id`.
  When `index` is given, only that slot (fill, stroke or shadow)."
  ([timeline shape-id property]
   (property-keyframes timeline shape-id property nil))
  ([timeline shape-id property index]
   (filterv #(same-slot? % property index)
            (dm/get-in timeline [:tracks shape-id :keyframes]))))

(defn split-timing
  "The timing (`:easing`, `:interpolation`) for a keyframe of `property`
  (and optional `index`) of `shape-id` added at `time`: the one of the
  segment it splits, so a linear or hold segment stays the same with the
  value shown there. Before the first keyframe it eases."
  [timeline shape-id property index time]
  (let [previous (->> (property-keyframes timeline shape-id property index)
                      (filter #(< (:time %) time))
                      (last))]
    (if (some? previous)
      (select-keys previous [:easing :interpolation])
      {:easing :ease})))

(defn looping?
  "Whether the keyframes of `property` (and optional `index`) of
  `shape-id` repeat over the timeline."
  ([timeline shape-id property]
   (looping? timeline shape-id property nil))
  ([timeline shape-id property index]
   (slot-flag? timeline shape-id :loops property index)))

(defn toggle-loop
  "Make the keyframes of `property` (and optional `index`) of `shape-id`
  repeat over the timeline, or stop them, unless they are locked."
  ([timeline shape-id property]
   (toggle-loop timeline shape-id property nil))
  ([timeline shape-id property index]
   (if (slot-flag? timeline shape-id :locked property index)
     timeline
     (toggle-slot-flag timeline shape-id :loops property index))))

(defn remove-property
  "Remove every keyframe of `property` (and optional `index`) from the
  track of `shape-id`, unless they are locked. If it leaves the track
  empty (no animations nor origin either), drop the track too."
  ([timeline shape-id property]
   (remove-property timeline shape-id property nil))
  ([timeline shape-id property index]
   (let [track (get-track timeline shape-id)]
     (if (or (nil? track) (slot-flag? timeline shape-id :locked property index))
       timeline
       (store-track timeline shape-id
                    (update track :keyframes
                            (partial filterv #(not (same-slot? % property index)))))))))

(defn remove-track
  "Remove the keyframes and animations of `shape-id` but the locked ones,
  and the whole track when none is locked."
  [timeline shape-id]
  (let [track      (get-track timeline shape-id)
        locked     (get track :locked #{})
        keyframes  (filterv #(contains? locked (slot-key (:property %) (:index %)))
                            (:keyframes track))
        animations (filterv :locked (:animations track))]
    (if (and (empty? keyframes) (empty? animations))
      (update timeline :tracks dissoc shape-id)
      (store-track timeline shape-id
                   (cond-> (assoc track :keyframes keyframes)
                     (empty? animations) (dissoc :animations)
                     (seq animations)    (assoc :animations animations))))))

(defn animation-end
  [{:keys [start duration]}]
  (+ start duration))

(defn- track-times
  "Keyframe times and animation starts and ends of `track`."
  [track]
  (concat (map :time (:keyframes track))
          (mapcat (juxt :start animation-end) (:animations track))))

(defn keyframes-range
  "`[first last]` time of the keyframes and animations of the tracks of
  `shape-ids`, or nil when they have none."
  [timeline shape-ids]
  (let [times (into [] (mapcat #(track-times (get-track timeline %))) shape-ids)]
    (when (seq times)
      [(reduce min times) (reduce max times)])))

(defn retime-keyframes
  "Map the keyframe and animation times of the tracks of `shape-ids`
  linearly from the span `from` to the span `to` (both `[start end]`):
  moves them when both spans have the same length, stretches them
  otherwise."
  [timeline shape-ids [from-start from-end] [to-start to-end]]
  (let [span  (- from-end from-start)
        scale (if (pos? span) (/ (- to-end to-start) span) 1)
        remap #(max 0 (mth/round (+ to-start (* (- % from-start) scale))))

        retime-animation
        (fn [animation]
          (let [start (remap (:start animation))]
            (assoc animation
                   :start start
                   :duration (max 1 (- (remap (animation-end animation)) start)))))]
    ;; Locked keyframes and animations stay where they are.
    (reduce (fn [timeline shape-id]
              (let [locked (dm/get-in timeline [:tracks shape-id :locked] #{})]
                (-> timeline
                    (d/update-in-when [:tracks shape-id :keyframes]
                                      (fn [keyframes]
                                        (sort-keyframes
                                         (mapv #(cond-> %
                                                  (not (contains? locked (slot-key (:property %) (:index %))))
                                                  (update :time remap))
                                               keyframes))))
                    (d/update-in-when [:tracks shape-id :animations]
                                      (partial mapv #(cond-> % (not (:locked %)) retime-animation))))))
            timeline
            shape-ids)))

(defn paste-keyframes
  "Add copies of `keyframes` (any properties) to the track of `shape-id`,
  shifted so the earliest one lands at `time`, and make the properties in
  `loops` repeat. Locked properties of `shape-id` take none. The duration
  grows to fit the pasted keyframes."
  [timeline shape-id keyframes loops time]
  (let [locked    (dm/get-in timeline [:tracks shape-id :locked] #{})
        offset    (when (seq keyframes)
                    (- time (reduce min (map :time keyframes))))
        keyframes (filterv #(not (contains? locked (slot-key (:property %) (:index %))))
                           keyframes)]
    (if (empty? keyframes)
      timeline
      (let [timeline (reduce (fn [timeline keyframe]
                               (add-keyframe timeline shape-id
                                             (-> keyframe
                                                 (dissoc :id)
                                                 (update :time + offset))))
                             timeline
                             keyframes)]
        (-> (reduce (fn [timeline key]
                      (d/update-in-when timeline [:tracks shape-id] update-slot-flag :loops conj key))
                    timeline
                    (remove locked loops))
            (update :duration max (+ offset (reduce max (map :time keyframes)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRESET ANIMATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-animation-duration 400)

(def overshoot-easing
  "Overshoots, then settles: things pop into place."
  {:type :bezier :curve [0.34 1.56 0.64 1.0]})

(def animation-defaults
  "The settings of a new animation of each type."
  {:fade   {:amount 0}
   :move   {:offset-x 0 :offset-y 40}
   :scale  {:amount 0.5}
   :rotate {:amount -90}})

(def animation-styles
  "Composite presets and the animations each one adds."
  {:slide-up    [{:type :fade} {:type :move :offset-x 0 :offset-y 40}]
   :slide-down  [{:type :fade} {:type :move :offset-x 0 :offset-y -40}]
   :slide-left  [{:type :fade} {:type :move :offset-x 40 :offset-y 0}]
   :slide-right [{:type :fade} {:type :move :offset-x -40 :offset-y 0}]
   :pop         [{:type :fade} {:type :scale :amount 0.5 :easing overshoot-easing}]
   :zoom        [{:type :fade} {:type :scale :amount 1.5}]
   :spin        [{:type :fade} {:type :scale :amount 0.5} {:type :rotate :amount -180}]})

(defn animation-properties
  "The properties an animation of `type` takes over."
  [type]
  (case type
    :fade   [:opacity]
    :move   [:x :y]
    :scale  [:scale-x :scale-y]
    :rotate [:rotation]
    []))

(defn make-animation
  "A valid animation from `params`; missing settings take the defaults of
  its type."
  [{:keys [id type direction start duration hidden locked] :as params}]
  (cond-> (merge (get animation-defaults type)
                 (d/without-nils (select-keys params [:easing :amount :offset-x :offset-y]))
                 {:id (or id (uuid/next))
                  :type type
                  :direction (or direction :in)
                  :start (max 0 (int (or start 0)))
                  :duration (max 1 (int (or duration default-animation-duration)))})
    (true? hidden) (assoc :hidden true)
    (true? locked) (assoc :locked true)))

(defn get-animation
  [timeline shape-id animation-id]
  (d/seek #(= (:id %) animation-id)
          (dm/get-in timeline [:tracks shape-id :animations])))

(defn add-animation
  "Add `animation` (see `make-animation`) after the others of the track of
  `shape-id`, creating the track if needed. They keep that order, so
  moving one in time does not move its row. The duration grows to fit
  it."
  [timeline shape-id animation]
  (let [animation (make-animation animation)]
    (-> (ensure-track timeline shape-id)
        (update-in [:tracks shape-id :animations] (fnil conj []) animation)
        (update :duration max (animation-end animation)))))

(defn update-animation
  "Apply `f` to the animation `animation-id` of `shape-id`, keeping it
  valid, unless it is locked. The duration grows to fit it."
  [timeline shape-id animation-id f]
  (let [timeline (d/update-in-when timeline [:tracks shape-id :animations]
                                   (partial mapv #(if (and (= (:id %) animation-id)
                                                           (not (:locked %)))
                                                    (make-animation (f %))
                                                    %)))]
    (if-let [animation (get-animation timeline shape-id animation-id)]
      (update timeline :duration max (animation-end animation))
      timeline)))

(defn remove-animation
  "Remove an animation, unless it is locked. If it leaves the track empty,
  drop the track too."
  [timeline shape-id animation-id]
  (let [track     (get-track timeline shape-id)
        animation (get-animation timeline shape-id animation-id)]
    (if (or (nil? animation) (:locked animation))
      timeline
      (let [animations (filterv #(not= (:id %) animation-id) (:animations track))]
        (store-track timeline shape-id
                     (if (empty? animations)
                       (dissoc track :animations)
                       (assoc track :animations animations)))))))

(defn snap-times
  "The times a drag on `timeline` snaps to: its start and end, its
  markers, its keyframes and the starts and ends of its preset
  animations, but for the keyframes of `keyframe-ids`, the animations of
  `animation-ids` and the markers of `marker-ids`, the ones being
  dragged, and for its end with `end?` false, when the end is what is
  dragged."
  [timeline & {:keys [keyframe-ids animation-ids marker-ids end?] :or {end? true}}]
  (let [tracks (vals (:tracks timeline))]
    (into (cond-> (sorted-set 0) end? (conj (:duration timeline)))
          (concat
           (for [marker (:markers timeline)
                 :when (not (contains? marker-ids (:id marker)))]
             (:time marker))
           (for [track    tracks
                 keyframe (:keyframes track)
                 :when (not (contains? keyframe-ids (:id keyframe)))]
             (:time keyframe))
           (for [track     tracks
                 animation (:animations track)
                 :when (not (contains? animation-ids (:id animation)))
                 time      [(:start animation) (animation-end animation)]]
             time)))))

(defn snap-time
  "The one of `times` nearest to `time`, within `threshold` of it, or nil."
  [times time threshold]
  (reduce (fn [best t]
            (let [distance (mth/abs (- t time))]
              (if (and (<= distance threshold)
                       (or (nil? best) (< distance (mth/abs (- best time)))))
                t
                best)))
          nil
          times))

(defn add-marker
  "`timeline` with a new marker at `time`, named `name` (`Marker N` when
  not given). Markers name moments of an animation: drags snap to them,
  the playhead jumps between them, and a Lottie player plays the stretch
  from one to the next by its name. The duration grows to hold it."
  [timeline {:keys [id time name]}]
  (let [time (max 0 (int time))
        name (or name (str "Marker " (inc (count (:markers timeline)))))]
    (-> timeline
        (update :markers (fn [markers]
                           (->> {:id (or id (uuid/next)) :time time :name name}
                                (conj (vec markers))
                                (sort-by :time)
                                (vec))))
        (update :duration max time))))

(defn update-marker
  "`timeline` with `f` applied to its marker `marker-id`, kept at or past
  the start and in time order. The duration grows to hold it."
  [timeline marker-id f]
  (let [markers (mapv (fn [marker]
                        (if (= marker-id (:id marker))
                          (update (f marker) :time #(max 0 (int %)))
                          marker))
                      (:markers timeline))]
    (cond-> timeline
      (seq markers)
      (-> (assoc :markers (vec (sort-by :time markers)))
          (update :duration max (reduce max (map :time markers)))))))

(defn remove-marker
  [timeline marker-id]
  (let [markers (filterv #(not= marker-id (:id %)) (:markers timeline))]
    (if (empty? markers)
      (dissoc timeline :markers)
      (assoc timeline :markers markers))))

(defn marker-after
  "The first marker of `timeline` past `time`, or nil."
  [timeline time]
  (d/seek #(> (:time %) time) (:markers timeline)))

(defn marker-before
  "The last marker of `timeline` before `time`, or nil."
  [timeline time]
  (d/seek #(< (:time %) time) (rseq (vec (:markers timeline)))))

(defn toggle-animation-flag
  "Set or clear `flag` of an animation: `:hidden` (left out of playback
  and exports) or `:locked` (kept from edits)."
  [timeline shape-id animation-id flag]
  (d/update-in-when timeline [:tracks shape-id :animations]
                    (partial mapv #(cond
                                     (not= (:id %) animation-id) %
                                     (get % flag)                (dissoc % flag)
                                     :else                       (assoc % flag true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERPOLATION ENGINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cubic-bezier presets matching the CSS easing keywords, so a preset
;; and a custom curve are evaluated by the same solver.
(def ^:private preset-curves
  {:linear      [0.0 0.0 1.0 1.0]
   :ease        [0.25 0.1 0.25 1.0]
   :ease-in     [0.42 0.0 1.0 1.0]
   :ease-out    [0.0 0.0 0.58 1.0]
   :ease-in-out [0.42 0.0 0.58 1.0]})

(defn- cubic-bezier-component
  [t p1 p2]
  (let [mt (- 1.0 t)]
    (+ (* 3 mt mt t p1)
       (* 3 mt t t p2)
       (* t t t))))

(defn cubic-bezier
  "Evaluate a cubic-bezier easing `[x1 y1 x2 y2]` at progress `t`
  (0..1), returning the eased progress. Solves x(s)=t with the
  bisection method (no derivatives needed, robust for any monotonic
  curve) and returns y(s)."
  [[x1 y1 x2 y2] t]
  (cond
    (<= t 0.0) 0.0
    (>= t 1.0) 1.0
    :else
    (loop [lo 0.0 hi 1.0 i 0]
      (let [s  (/ (+ lo hi) 2.0)
            x  (cubic-bezier-component s x1 x2)]
        (if (or (> i 24) (< (mth/abs (- x t)) 0.0005))
          (cubic-bezier-component s y1 y2)
          (if (< x t)
            (recur s hi (inc i))
            (recur lo s (inc i))))))))

(defn- exp
  [x]
  #?(:clj (Math/exp x) :cljs (js/Math.exp x)))

(defn- ln
  [x]
  #?(:clj (Math/log x) :cljs (js/Math.log x)))

(defn- spring-motion
  "Natural frequency (rad/s) and damping ratio of `spring`."
  [{:keys [stiffness damping mass]}]
  (let [k (max 0.01 (double stiffness))
        m (max 0.01 (double mass))
        c (max 0.0 (double damping))]
    [(mth/sqrt (/ k m)) (/ c (* 2.0 (mth/sqrt (* k m))))]))

(defn- spring-displacement
  "Displacement at `t` seconds of a spring let go at 1 with no velocity,
  settling at 0."
  [spring t]
  (let [[w0 zeta] (spring-motion spring)]
    (cond
      (< zeta 0.999)
      (let [wd (* w0 (mth/sqrt (- 1.0 (* zeta zeta))))]
        (* (exp (- (* zeta w0 t)))
           (+ (mth/cos (* wd t))
              (* (/ (* zeta w0) wd) (mth/sin (* wd t))))))

      (<= zeta 1.001)
      (* (exp (- (* w0 t))) (+ 1.0 (* w0 t)))

      :else
      (let [s  (mth/sqrt (- (* zeta zeta) 1.0))
            r1 (* (- w0) (- zeta s))
            r2 (* (- w0) (+ zeta s))]
        (/ (- (* r2 (exp (* r1 t))) (* r1 (exp (* r2 t))))
           (- r2 r1))))))

(def ^:private spring-rest
  "The share of its travel within which a spring counts as settled."
  0.001)

(defn- settle-time
  "Seconds `spring` takes to settle: a bouncing one once the envelope of
  its swings stays within `spring-rest`, the others once their slower
  motion does."
  [spring]
  (let [[w0 zeta] (spring-motion spring)
        zeta      (max zeta 0.01)]
    (cond
      (< zeta 0.999)
      (/ (ln (/ 1.0 (* spring-rest (mth/sqrt (- 1.0 (* zeta zeta))))))
         (* zeta w0))

      ;; e^-u (1 + u) = 0.001
      (<= zeta 1.001)
      (/ 9.2335 w0)

      :else
      (let [s  (mth/sqrt (- (* zeta zeta) 1.0))
            r1 (* (- w0) (- zeta s))
            r2 (* (- w0) (+ zeta s))]
        (/ (ln (* spring-rest (/ (- r2 r1) r2))) r1)))))

(defn spring-progress
  "Eased progress of `spring` at the linear progress `t` (0..1): its motion
  stretched so it settles at the end."
  [spring t]
  (cond
    (<= t 0.0) 0.0
    (>= t 1.0) 1.0
    :else      (- 1.0 (spring-displacement spring (* t (settle-time spring))))))

;; Stretched to the segment, a spring only keeps the shape its damping
;; ratio gives, so like Figma it is edited as one value: the bounce, 1
;; minus the damping ratio (0 settles without overshooting, towards 1 it
;; bounces more; below 0 it creeps in).

(def max-spring-bounce 0.97)

(defn spring-bounce
  [spring]
  (- 1.0 (second (spring-motion spring))))

(defn spring-with-bounce
  "`spring` with the damping that gives it `bounce`."
  [{:keys [stiffness mass] :as spring} bounce]
  (let [bounce (mth/clamp bounce -1.0 max-spring-bounce)]
    (assoc spring :damping
           (mth/precision (* 2.0 (- 1.0 bounce)
                             (mth/sqrt (* (max 0.01 (double stiffness))
                                          (max 0.01 (double mass)))))
                          3))))

(defn- peak-position
  "When, in 0..1 of its segment, a spring of damping ratio `zeta` (< 1)
  first overshoots: half a swing over the time it takes to settle."
  [zeta]
  (let [s (mth/sqrt (- 1.0 (* zeta zeta)))]
    (/ (* mth/PI zeta)
       (* s (ln (/ 1.0 (* spring-rest s)))))))

(defn spring-peak
  "`[t progress]` of the first overshoot of `spring`, or nil when it does
  not overshoot within its segment."
  [spring]
  (let [[_ zeta] (spring-motion spring)]
    (when (< zeta 0.999)
      (let [t (peak-position zeta)]
        (when (<= t 1.0)
          [t (+ 1.0 (exp (- (/ (* mth/PI zeta) (mth/sqrt (- 1.0 (* zeta zeta)))))))])))))

(defn bounce-for-peak
  "The bounce of the spring that first overshoots at `t` (0..1) of its
  segment: the more it bounces, the sooner. From the end on, no bounce."
  [t]
  (let [least (- 1.0 max-spring-bounce)]
    (cond
      (<= t (peak-position least)) max-spring-bounce
      (>= t 1.0)                   0.0
      :else
      (loop [lo least hi 0.999 i 0]
        (let [mid (/ (+ lo hi) 2.0)]
          (cond
            (> i 40)                  (- 1.0 mid)
            (< (peak-position mid) t) (recur mid hi (inc i))
            :else                     (recur lo mid (inc i))))))))

(defn- spring-cycles
  "How many times `spring` swings before it settles."
  [spring]
  (let [[w0 zeta] (spring-motion spring)]
    (if (>= zeta 0.999)
      0
      (/ (* w0 (mth/sqrt (- 1.0 (* zeta zeta))) (settle-time spring))
         (* 2.0 mth/PI)))))

(defn spring-samples
  "Points to follow `spring` smoothly (16 per swing), at least `least`."
  [spring least]
  (-> (mth/ceil (* 16 (spring-cycles spring)))
      (int)
      (max least)
      (min 2000)))

(defn easing-progress
  "Eased progress of `easing` (a preset keyword, a bezier or a spring) at
  the linear progress `t` (0..1). Linear is exact (no solver error)."
  [easing t]
  (cond
    (or (nil? easing) (= easing :linear)) t
    (keyword? easing)                     (cubic-bezier (preset-curves easing (preset-curves :linear)) t)
    (= :spring (:type easing))            (spring-progress easing t)
    (= :bezier (:type easing))            (cubic-bezier (:curve easing) t)
    :else                                 t))

(def bezier-presets
  "The named curves the easing editor offers."
  [{:id :linear :easing :linear}
   {:id :ease :easing :ease}
   {:id :ease-in :easing :ease-in}
   {:id :ease-out :easing :ease-out}
   {:id :ease-in-out :easing :ease-in-out}
   {:id :ease-in-back :easing {:type :bezier :curve [0.36 0.0 0.66 -0.56]}}
   {:id :ease-out-back :easing {:type :bezier :curve [0.34 1.56 0.64 1.0]}}
   {:id :ease-in-out-back :easing {:type :bezier :curve [0.68 -0.6 0.32 1.6]}}])

(def spring-presets
  "The named springs the easing editor offers (Figma's values)."
  [{:id :gentle :easing {:type :spring :stiffness 100 :damping 15 :mass 1}}
   {:id :quick :easing {:type :spring :stiffness 300 :damping 20 :mass 1}}
   {:id :bouncy :easing {:type :spring :stiffness 600 :damping 15 :mass 1}}
   {:id :slow :easing {:type :spring :stiffness 80 :damping 20 :mass 1}}])

(defn easing-curve
  "The `[x1 y1 x2 y2]` control points of a preset keyword or a bezier, or
  nil for a spring."
  [easing]
  (cond
    (keyword? easing)          (preset-curves easing (preset-curves :linear))
    (= :bezier (:type easing)) (:curve easing)
    (nil? easing)              (preset-curves :linear)
    :else                      nil))

(defn- segment-progress
  "Eased progress in [0,1] for the segment that STARTS at `from-keyframe`,
  given the linear progress `t` in [0,1]."
  [from-keyframe t]
  (easing-progress (:easing from-keyframe) t))

(defn- lerp
  [a b t]
  (+ a (* (- b a) t)))

(defn- clamp-byte
  [n]
  (-> n mth/round (max 0) (min 255) int))

(defn- lerp-color
  "Mix two hex colours in RGB. A step (`t` unused by the caller) is
  handled before this is called."
  [from to t]
  (let [[fr fg fb] (clr/hex->rgb from)
        [tr tg tb] (clr/hex->rgb to)]
    (clr/rgb->hex [(clamp-byte (lerp fr tr t))
                   (clamp-byte (lerp fg tg t))
                   (clamp-byte (lerp fb tb t))])))

(defn- lerp-value
  [from to t]
  (if (or (string? from) (string? to))
    (lerp-color from to t)
    (lerp from to t)))

(defn- bezier-value
  "The cubic bezier through `p0`, `p1`, `p2` and `p3` at `t`."
  [p0 p1 p2 p3 t]
  (let [u (- 1 t)]
    (+ (* u u u p0)
       (* 3 u u t p1)
       (* 3 u t t p2)
       (* t t t p3))))

(defn curved?
  "Whether the segment from the keyframe `from` to `to` follows a motion
  path (see `schema:keyframe`)."
  [from to]
  (and (not= :step (:interpolation from :linear))
       (or (some? (:path-out from)) (some? (:path-in to)))))

(defn- segment-value
  [from to t]
  (if (curved? from to)
    (let [v0 (:value from)
          v1 (:value to)]
      (bezier-value v0 (+ v0 (:path-out from 0)) (+ v1 (:path-in to 0)) v1 t))
    (lerp-value (:value from) (:value to) t)))

(defn- property-value-at
  "Compute the value of a single property from its (time-sorted)
  keyframes at `time`, or nil when there are no keyframes for it."
  [keyframes time]
  (when (seq keyframes)
    (let [first-kf (first keyframes)
          last-kf  (last keyframes)]
      (cond
        (<= time (:time first-kf)) (:value first-kf)
        (>= time (:time last-kf))  (:value last-kf)
        :else
        (let [[from to] (->> (partition 2 1 keyframes)
                             (some (fn [[a b]]
                                     (when (and (<= (:time a) time) (< time (:time b)))
                                       [a b]))))]
          ;; `:step` interpolation holds the source value (exact, keeps type)
          (if (= :step (:interpolation from :linear))
            (:value from)
            (let [span (- (:time to) (:time from))
                  t    (if (zero? span) 0.0 (/ (double (- time (:time from))) span))]
              (segment-value from to (segment-progress from t)))))))))

(defn- loop-time
  "Fold `time` into the span between the first and the last of the
  (time-sorted) `keyframes` of a looping property, so the span repeats."
  [keyframes time]
  (let [start (:time (first keyframes))
        span  (- (:time (last keyframes)) start)]
    (if (and (pos? span) (> time start))
      (+ start (mod (- time start) span))
      time)))

(defn- store-value
  "Put `v` on `m` at `property`, nested under `index` when the property
  is a fill, stroke or shadow slot."
  [m property index v]
  (if (some? index)
    (assoc-in m [property index] v)
    (assoc m property v)))

(defn values-at
  "Return `{shape-id {property value}}` with the interpolated value of
  every animated property of every track of `timeline` at `time` (ms).
  Indexed properties (fill, stroke, shadow) nest as `{index value}`."
  [timeline time]
  (reduce-kv
   (fn [acc shape-id track]
     (let [loops    (get track :loops #{})
           by-slot  (group-by keyframe-slot (:keyframes track))
           values   (reduce-kv
                     (fn [m [property index] keyframes]
                       (let [keyframes (sort-keyframes keyframes)
                             time      (if (contains? loops (slot-key property index))
                                         (loop-time keyframes time)
                                         time)
                             v         (property-value-at keyframes time)]
                         (cond-> m (some? v) (store-value property index v))))
                     {}
                     by-slot)]
       (cond-> acc (seq values) (assoc shape-id values))))
   {}
   (:tracks timeline)))

(def ^:private max-loop-repeats 1000)

(defn- expand-property-loop
  "Unroll the (time-sorted) `keyframes` of a looping property into
  explicit repetitions up to `duration`, closed by a keyframe at
  `duration` with the value `values-at` gives there."
  [keyframes duration]
  (let [start (:time (first keyframes))
        span  (- (:time (last keyframes)) start)]
    (if (or (not (pos? span)) (>= (+ start span) duration))
      keyframes
      (let [repeats (for [i (range 1 (inc max-loop-repeats))
                          :while (< (+ start (* i span)) duration)
                          [j keyframe] (map-indexed vector keyframes)]
                      ;; Each repetition restarts 1ms after the previous
                      ;; one ends, so both end values are kept.
                      (-> keyframe
                          (assoc :id (uuid/next))
                          (update :time + (* i span) (if (zero? j) 1 0))))
            kept    (filterv #(<= (:time %) duration)
                             (into (vec keyframes) repeats))]
        (cond-> kept
          (< (:time (peek kept)) duration)
          (conj (make-keyframe
                 {:time duration
                  :property (:property (first keyframes))
                  :index (:index (first keyframes))
                  :value (property-value-at keyframes (loop-time keyframes duration))})))))))

(defn expand-loops
  "Return `timeline` with its looping properties unrolled into explicit
  keyframes, for the exports that only read keyframes."
  [timeline]
  (let [duration (:duration timeline)]
    (update timeline :tracks d/update-vals
            (fn [{:keys [loops keyframes] :as track}]
              (if (empty? loops)
                track
                (-> track
                    (dissoc :loops)
                    (assoc :keyframes
                           (->> (group-by keyframe-slot keyframes)
                                (mapcat (fn [[[property index] keyframes]]
                                          (cond-> (sort-keyframes keyframes)
                                            (contains? loops (slot-key property index))
                                            (expand-property-loop duration))))
                                (sort-keyframes)))))))))

(def ^:private path-steps
  "Straight steps a curved segment of a motion path is drawn with."
  24)

(defn- expand-property-path
  "Replace each curved segment (see `curved?`) of the (time-sorted)
  `keyframes` of one property by straight steps along the curve, eased as
  the segment was."
  [keyframes]
  (into []
        (comp
         (mapcat (fn [[from to]]
                   (if (and (some? to) (curved? from to))
                     (let [span  (- (:time to) (:time from))
                           steps (for [i (range 1 path-steps)
                                       :let [time (+ (:time from) (mth/round (* span (/ i path-steps))))]]
                                   (make-keyframe
                                    {:time time
                                     :property (:property from)
                                     :index (:index from)
                                     :value (property-value-at [from to] time)}))]
                       (cons (dissoc from :easing)
                             ;; A short segment rounds some steps to the
                             ;; same time; keep one of each.
                             (->> steps
                                  (filter #(< (:time from) (:time %) (:time to)))
                                  (partition-by :time)
                                  (map first))))
                     [from])))
         (map #(dissoc % :path-in :path-out)))
        (partition-all 2 1 keyframes)))

(defn expand-paths
  "Return `timeline` with its motion paths unrolled into straight steps,
  for the exports that only draw straight lines between keyframes."
  [timeline]
  (update timeline :tracks d/update-vals
          (fn [{:keys [keyframes] :as track}]
            (if (some #(or (:path-in %) (:path-out %)) keyframes)
              (assoc track :keyframes
                     (->> (group-by keyframe-slot keyframes)
                          (mapcat (fn [[_ keyframes]]
                                    (expand-property-path (sort-keyframes keyframes))))
                          (sort-keyframes)))
              track))))

(defn- expand-property-springs
  "Replace each spring segment of the (time-sorted) `keyframes` of one
  property by linear steps along the spring."
  [keyframes]
  (into []
        (mapcat (fn [[from to]]
                  (if (and (some? to)
                           (= :spring (:type (:easing from)))
                           (not= :step (:interpolation from)))
                    (let [span    (- (:time to) (:time from))
                          samples (spring-samples (:easing from) 16)
                          steps   (for [i (range 1 samples)
                                        :let [t (/ i samples)]]
                                    (make-keyframe
                                     {:time (+ (:time from) (mth/round (* t span)))
                                      :property (:property from)
                                      :index (:index from)
                                      :value (lerp-value (:value from) (:value to)
                                                         (spring-progress (:easing from) t))}))]
                      (cons (dissoc from :easing)
                            ;; A short segment rounds some steps to the
                            ;; same time; keep one of each.
                            (->> steps
                                 (filter #(< (:time from) (:time %) (:time to)))
                                 (partition-by :time)
                                 (map first))))
                    [from])))
        (partition-all 2 1 keyframes)))

(defn expand-springs
  "Return `timeline` with its spring easings unrolled into linear
  keyframes, for the exports that only know bezier easings."
  [timeline]
  (update timeline :tracks d/update-vals
          (fn [{:keys [keyframes] :as track}]
            (if (some #(= :spring (:type (:easing %))) keyframes)
              (assoc track :keyframes
                     (->> (group-by keyframe-slot keyframes)
                          (mapcat (fn [[_ keyframes]]
                                    (expand-property-springs (sort-keyframes keyframes))))
                          (sort-keyframes)))
              track))))

(defn- reverse-easing
  "The easing that plays `easing` backwards: its bezier turned around
  the centre. Linear stays nil; springs are unrolled before (see
  `expand-springs`)."
  [easing]
  (when-not (or (nil? easing) (= easing :linear))
    (when-let [[x1 y1 x2 y2] (easing-curve easing)]
      {:type :bezier :curve [(- 1 x2) (- 1 y2) (- 1 x1) (- 1 y1)]})))

(defn- mirror-property-keyframes
  "The (time-sorted) `keyframes` of one property up to `duration`,
  followed by their mirror image up to twice `duration`. A mirrored
  segment takes the reversed easing of the one it mirrors; a mirrored
  step jumps 1ms after the keyframe it leaves."
  [keyframes duration]
  (let [end       (* 2 duration)
        cut       (filterv #(<= (:time %) duration) keyframes)
        keyframes (cond-> cut
                    ;; Keyframes past the end are cut at the end.
                    (and (< (count cut) (count keyframes))
                         (not= duration (:time (peek cut))))
                    (conj (make-keyframe
                           {:time duration
                            :property (:property (first keyframes))
                            :index (:index (first keyframes))
                            :value (property-value-at keyframes duration)})))
        forward   (filterv #(< (:time %) duration) keyframes)
        backward  (mapcat
                   ;; Played backwards, the segment `from` → `to` goes
                   ;; from the mirror of `to` to the mirror of `from`.
                   (fn [[to from]]
                     (let [time (- end (:time to))
                           base (-> (dissoc to :easing :interpolation)
                                    (assoc :id (uuid/next) :time time))]
                       (cond
                         (nil? from)
                         [base]

                         (= :step (:interpolation from))
                         (cond-> [(assoc base :interpolation :step)]
                           (< (inc time) (- end (:time from)))
                           (conj (assoc base
                                        :id (uuid/next)
                                        :time (inc time)
                                        :value (:value from)
                                        :interpolation :step)))

                         :else
                         (let [easing (reverse-easing (:easing from))]
                           [(cond-> base
                              (some? easing) (assoc :easing easing)
                              (some? (:interpolation from)) (assoc :interpolation (:interpolation from)))]))))
                   (partition-all 2 1 (rseq keyframes)))]
    (into forward backward)))

(defn expand-ping-pong
  "Return a ping-pong `timeline` as a loop of twice its duration: its
  keyframes followed by their mirror image, for the exports that can
  only loop. Run it after `expand-loops`. Other timelines are returned
  unchanged."
  [timeline]
  (if (not= :ping-pong (playback-mode timeline))
    timeline
    (let [duration (:duration timeline)]
      (-> timeline
          (assoc :duration (* 2 duration) :playback :loop)
          (update :tracks d/update-vals
                  (fn [track]
                    (update track :keyframes
                            (fn [keyframes]
                              (->> (group-by keyframe-slot keyframes)
                                   (mapcat (fn [[_ keyframes]]
                                             (mirror-property-keyframes (sort-keyframes keyframes) duration)))
                                   (sort-keyframes))))))))))

(defn- has? [values & props]
  (some #(contains? values %) props))

(defn value-of
  "The interpolated value of `property` in `values`, nested under
  `index` when the property is a fill, stroke or shadow slot."
  ([values property]
   (value-of values property nil))
  ([values property index]
   (if (some? index)
     (get-in values [property index])
     (get values property))))

(defn- as-vec
  [items]
  (if (vector? items) items (vec items)))

(def appearance-attrs
  "The attributes of a shape the animation changes with `:change-property`
  modifiers (see `shape->modifiers`): how it looks. Its geometry takes the
  transform path."
  #{:opacity :r1 :r2 :r3 :r4 :fills :strokes :shadow :blur :background-blur
    :trim-start :trim-end :trim-offset})

(defn appearance-changes
  "`{attr value}` of the `appearance-attrs` that `modifiers` change, or nil.
  Other structure changes, like those of the layout during an edit, are
  left out."
  [modifiers]
  (not-empty
   (reduce (fn [changes {:keys [type property value]}]
             (cond-> changes
               (and (= :change-property type) (contains? appearance-attrs property))
               (assoc property value)))
           {}
           (:structure-parent modifiers))))

(defn apply-appearance-modifiers
  "Apply the appearance half of `modifiers` to `objects`, so SVG can paint
  the animated fills, strokes, shadows, blurs and trims. Geometry stays on
  the transform path."
  [objects modifiers]
  (reduce-kv
   (fn [objects id {:keys [modifiers]}]
     (if-let [changes (appearance-changes modifiers)]
       (d/update-when objects id merge changes)
       objects))
   objects
   modifiers))

(defn solid-color-fill?
  "A fill that is a flat colour (not a gradient or an image)."
  [fill]
  (and (some? (:fill-color fill))
       (nil? (:fill-color-gradient fill))
       (nil? (:fill-image fill))))

(defn solid-color-stroke?
  "A stroke that is a flat colour (not a gradient or an image)."
  [stroke]
  (and (some? (:stroke-color stroke))
       (nil? (:stroke-color-gradient stroke))
       (nil? (:stroke-image stroke))))

(defn appearance-slots
  "The fill, stroke, shadow and blur slots of `shape` that can take
  keyframes. Gradients and images are skipped."
  [shape]
  (let [fills   (as-vec (:fills shape))
        strokes (as-vec (:strokes shape))
        shadows (as-vec (:shadow shape))]
    (vec
     (concat
      (keep-indexed
       (fn [index fill]
         (when (solid-color-fill? fill)
           {:group :fill :index index
            :properties [:fill-color :fill-opacity]}))
       fills)
      (keep-indexed
       (fn [index stroke]
         (when (solid-color-stroke? stroke)
           {:group :stroke :index index
            :properties [:stroke-color :stroke-opacity :stroke-width]}))
       strokes)
      (map-indexed
       (fn [index _]
         {:group :shadow :index index
          :properties [:shadow-offset-x :shadow-offset-y :shadow-blur
                       :shadow-spread :shadow-color :shadow-opacity]})
       shadows)
      (when (:blur shape)
        [{:group :blur :index nil :properties [:blur]}])
      (when (:background-blur shape)
        [{:group :background-blur :index nil :properties [:background-blur]}])))))

(defn- patch-indexed
  "Replace fields of each item in `items` from the `{index value}` maps
  in `by-field`. `solid?` skips items that cannot take a colour."
  [items by-field solid?]
  (if (empty? by-field)
    items
    (let [items (as-vec items)]
      (reduce-kv
       (fn [items field values]
         (reduce-kv
          (fn [items index value]
            (if-let [item (get items index)]
              (if (or (nil? solid?) (solid? item))
                (assoc items index (assoc item field value))
                items)
              items))
          items
          values))
       items
       by-field))))

(defn- apply-fills
  [fills values]
  (patch-indexed fills
                 (cond-> {}
                   (contains? values :fill-color) (assoc :fill-color (:fill-color values))
                   (contains? values :fill-opacity) (assoc :fill-opacity (:fill-opacity values)))
                 solid-color-fill?))

(defn- apply-strokes
  [strokes values]
  (-> strokes
      (patch-indexed
       (cond-> {}
         (contains? values :stroke-color) (assoc :stroke-color (:stroke-color values))
         (contains? values :stroke-opacity) (assoc :stroke-opacity (:stroke-opacity values)))
       solid-color-stroke?)
      (patch-indexed
       (cond-> {}
         (contains? values :stroke-width) (assoc :stroke-width (:stroke-width values)))
       nil)))

(defn- apply-shadow-color
  [shadow index colors opacities]
  (cond-> shadow
    (contains? colors index)
    (assoc-in [:color :color] (get colors index))
    (contains? opacities index)
    (assoc-in [:color :opacity] (get opacities index))))

(defn- apply-shadows
  [shadows values]
  (let [shadows (as-vec shadows)
        fields  (select-keys values [:shadow-offset-x :shadow-offset-y
                                     :shadow-blur :shadow-spread
                                     :shadow-color :shadow-opacity])]
    (if (empty? fields)
      shadows
      (let [ox (:shadow-offset-x values)
            oy (:shadow-offset-y values)
            bl (:shadow-blur values)
            sp (:shadow-spread values)
            cl (:shadow-color values)
            op (:shadow-opacity values)
            idxs (into #{} (mapcat keys) [ox oy bl sp cl op])]
        (reduce
         (fn [shadows index]
           (if-let [shadow (get shadows index)]
             (assoc shadows index
                    (-> shadow
                        (cond-> (contains? ox index) (assoc :offset-x (get ox index)))
                        (cond-> (contains? oy index) (assoc :offset-y (get oy index)))
                        (cond-> (contains? bl index) (assoc :blur (get bl index)))
                        (cond-> (contains? sp index) (assoc :spread (get sp index)))
                        (apply-shadow-color index cl op)))
             shadows))
         shadows
         idxs)))))

(defn- apply-blur
  [blur value]
  (cond-> blur
    (and (some? blur) (some? value))
    (assoc :value value)))

(defn position-origin
  "Keyframe positions (`:x`/`:y`) are relative to the board of the
  timeline, so moving the board keeps its animation; the positions of the
  board itself are relative to the canvas."
  [timeline objects shape-id]
  (let [board-id (:board-id timeline)
        selrect  (when (not= shape-id board-id)
                   (dm/get-in objects [board-id :selrect]))]
    (gpt/point (or (:x selrect) 0) (or (:y selrect) 0))))

(defn- axis-scale
  "Scale for one axis. An absolute `:width` or `:height` is the target
  size over the current size; a scale keyframe multiplies that."
  [base values size-key scale-key]
  (let [size  (get values size-key)
        scale (get values scale-key 1)
        from-size (if (and (some? size) (pos? base))
                    (/ (double size) (double base))
                    1.0)]
    (* from-size scale)))

(defn- shape->modifiers
  "Build a single modifiers record for `shape` from the interpolated
  `values` (positions relative to `origin`). Position/scale/rotation
  become geometry modifiers; opacity becomes a `:change-property`
  structure modifier so it rides the same modifier pipeline in every
  renderer (SVG via `transform-shape`, WASM via `set-shape-opacity`) and
  in export."
  [shape values origin pivot]
  (let [selrect (:selrect shape)
        base-x  (:x selrect)
        base-y  (:y selrect)
        base-r  (or (:rotation shape) 0)
        target-x (if (contains? values :x) (+ (:x origin) (:x values)) base-x)
        target-y (if (contains? values :y) (+ (:y origin) (:y values)) base-y)
        base-w   (or (:width selrect) 0)
        base-h   (or (:height selrect) 0)
        sx       (axis-scale base-w values :width :scale-x)
        sy       (axis-scale base-h values :height :scale-y)
        target-r (get values :rotation base-r)
        pivot    (or pivot (origin-point shape default-transform-origin))]
    ;; The shape scales and turns around its pivot where it is, then
    ;; moves, like the CSS and Lottie exports: moving first would make it
    ;; turn around the place it left.
    (cond-> (ctm/empty)
      (has? values :scale-x :scale-y :width :height)
      (ctm/resize (gpt/point sx sy) pivot)

      (has? values :rotation)
      (ctm/rotation pivot (- target-r base-r))

      (has? values :x :y)
      (ctm/move (gpt/point (- target-x base-x) (- target-y base-y)))

      (has? values :opacity)
      (ctm/change-property :opacity (get values :opacity))

      (contains? values :r1)
      (ctm/change-property :r1 (:r1 values))

      (contains? values :r2)
      (ctm/change-property :r2 (:r2 values))

      (contains? values :r3)
      (ctm/change-property :r3 (:r3 values))

      (contains? values :r4)
      (ctm/change-property :r4 (:r4 values))

      (has? values :fill-color :fill-opacity)
      (ctm/change-property :fills (apply-fills (:fills shape) values))

      (has? values :stroke-color :stroke-opacity :stroke-width)
      (ctm/change-property :strokes (apply-strokes (:strokes shape) values))

      (has? values :shadow-offset-x :shadow-offset-y :shadow-blur
            :shadow-spread :shadow-color :shadow-opacity)
      (ctm/change-property :shadow (apply-shadows (:shadow shape) values))

      (has? values :blur)
      (ctm/change-property :blur (apply-blur (:blur shape) (:blur values)))

      (has? values :background-blur)
      (ctm/change-property :background-blur
                           (apply-blur (:background-blur shape) (:background-blur values)))

      ;; A trim shows as a whole, so all three go when one is animated.
      (has? values :trim-start :trim-end :trim-offset)
      (as-> $ (reduce-kv (fn [modifiers property rest]
                           (ctm/change-property modifiers property (get values property rest)))
                         $
                         trim-properties)))))

(defn- rest-value
  "The value of `property` that `shape` has without keyframes, positions
  relative to `origin`."
  [shape property origin]
  (case property
    :x        (- (dm/get-in shape [:selrect :x] 0) (:x origin))
    :y        (- (dm/get-in shape [:selrect :y] 0) (:y origin))
    :opacity  (or (:opacity shape) 1)
    :rotation (or (:rotation shape) 0)
    :scale-x  1
    :scale-y  1
    nil))

(defn- effect-value
  "`base` with the effect of `animation` on `property` fully applied."
  [{:keys [type amount offset-x offset-y]} property base]
  (case type
    :fade   (* base (or amount 0))
    :move   (+ base (or (if (= property :x) offset-x offset-y) 0))
    :scale  (* base (or amount 1))
    :rotate (+ base (or amount 0))
    base))

(defn- animation-keyframes
  "The two keyframes of `animation` for `property`: from the effect to the
  value `base-at` gives, or the other way round for an `:out` animation.
  Entrances ease out and exits ease in, unless an easing is set."
  [{:keys [start direction easing] :as animation} property base-at]
  (let [end  (animation-end animation)
        out? (= direction :out)
        from (cond->> (base-at start) (not out?) (effect-value animation property))
        to   (cond->> (base-at end) out? (effect-value animation property))]
    [(make-keyframe {:time start
                     :property property
                     :value from
                     :easing (or easing (if out? :ease-in :ease-out))})
     (make-keyframe {:time end :property property :value to})]))

(defn- resolve-property
  "The keyframes of `property` once `animations` (sorted by start) take
  over their spans: keyframes inside a span give way to the two of the
  animation, the later animation winning where two overlap."
  [keyframes animations property base-at]
  (sort-keyframes
   (reduce (fn [keyframes animation]
             (let [start (:start animation)
                   end   (animation-end animation)]
               (into (filterv #(not (<= start (:time %) end)) keyframes)
                     (animation-keyframes animation property base-at))))
           keyframes
           animations)))

(defn- resolve-track
  [timeline objects shape-id {:keys [animations keyframes] :as track}]
  (let [shape (get objects shape-id)]
    (if (or (empty? animations) (nil? shape))
      (dissoc track :animations)
      (let [origin     (position-origin timeline objects shape-id)
            loops      (get track :loops #{})
            animations (sort-by :start animations)
            properties (into #{} (mapcat (comp animation-properties :type)) animations)
            by-slot    (group-by keyframe-slot keyframes)

            resolve
            (fn [property]
              ;; A looping property is unrolled first, its repeats being
              ;; plain keyframes the animation can take over.
              (let [own     (sort-keyframes (get by-slot [property nil]))
                    own     (cond-> own
                              (and (seq own) (contains? loops property))
                              (expand-property-loop (:duration timeline)))
                    base-at #(or (property-value-at own %)
                                 (rest-value shape property origin))
                    covers  (filter #(some #{property} (animation-properties (:type %))) animations)]
                (resolve-property own covers property base-at)))]
        (-> (reduce #(update-slot-flag %1 :loops disj %2) track properties)
            (dissoc :animations)
            (assoc :keyframes
                   (sort-keyframes
                    (into (filterv #(not (contains? properties (:property %))) keyframes)
                          (mapcat resolve)
                          properties))))))))

(defn- visible-track
  "`track` without its hidden keyframes and animations."
  [{:keys [hidden animations] :as track}]
  (cond-> (dissoc track :hidden)
    (seq hidden)
    (update :keyframes (partial filterv #(not (contains? hidden (slot-key (:property %) (:index %))))))

    (some :hidden animations)
    (update :animations (partial filterv (complement :hidden)))))

(defn resolve-animations
  "Return `timeline` as it plays: its hidden rows left out and its preset
  animations turned into the keyframes they stand for. Everything that
  reads keyframes uses it: the preview, the playback and the exports."
  [timeline objects]
  (if (some #(or (seq (:animations %)) (seq (:hidden %))) (vals (:tracks timeline)))
    (update timeline :tracks
            (fn [tracks]
              (reduce-kv (fn [tracks shape-id track]
                           (let [track (resolve-track timeline objects shape-id (visible-track track))]
                             ;; Nothing left to play once all of it is hidden.
                             (if (empty? (:keyframes track))
                               (dissoc tracks shape-id)
                               (assoc tracks shape-id track))))
                         tracks
                         tracks)))
    timeline))

(defn timeline->modif-tree
  "Compute a transient modif-tree `{shape-id {:modifiers ...}}` for
  `timeline` at `time`, covering transforms (x/y/scale/rotation) and
  opacity (as a change-property modifier). Designed to be fed to
  `set-modifiers` / `set-wasm-modifiers` (editor preview) or applied
  through `transform-shape` (viewer/export)."
  [timeline objects time]
  (reduce-kv
   (fn [tree shape-id values]
     (if-let [shape (get objects shape-id)]
       (let [board-origin (position-origin timeline objects shape-id)
             pivot        (origin-point shape (track-origin (get-track timeline shape-id)))
             modifiers    (shape->modifiers shape values board-origin pivot)]
         (if (ctm/empty? modifiers)
           tree
           (assoc tree shape-id {:modifiers modifiers})))
       tree))
   {}
   (values-at (resolve-animations timeline objects) time)))

(defn timeline->opacity
  "Compute `{shape-id opacity}` for every shape that animates opacity at
  `time` (opacity is applied as a transient style override, not a
  modifier)."
  [timeline time]
  (reduce-kv
   (fn [acc shape-id values]
     (if (contains? values :opacity)
       (assoc acc shape-id (:opacity values))
       acc))
   {}
   (values-at timeline time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITS ON THE CANVAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private edit-properties
  "Properties an edit of a shape records as keyframes. Scale stays a
  timeline value; width and height follow a resize."
  [:x :y :width :height :rotation :opacity :r1 :r2 :r3 :r4])

(def ^:private edit-absolute-properties
  "Properties an edit records as they end up, not as a change of the
  value shown."
  #{:opacity :width :height :r1 :r2 :r3 :r4})

(defn- angle-delta
  "The turn from `from` to `to` degrees the short way round: a shape
  keeps its rotation within 0-360, so 350 to 10 is a turn of 20."
  [from to]
  (let [delta (mod (- to from) 360)]
    (if (> delta 180) (- delta 360) delta)))

(defn- close-value?
  [property old new]
  (cond
    (or (nil? old) (nil? new)) true
    (color-property? property) (= old new)
    :else (mth/close? old new)))

(defn- appearance-slot-values
  "The current fill, stroke, shadow and blur values of `shape` as
  `{[property index] value}` (index is nil for blur)."
  [shape]
  (let [put (fn [m property index value]
              (cond-> m (some? value) (assoc [property index] value)))]
    (as-> {} $
      (reduce-kv
       (fn [m index fill]
         (if (solid-color-fill? fill)
           (-> m
               (put :fill-color index (:fill-color fill))
               (put :fill-opacity index (or (:fill-opacity fill) 1)))
           m))
       $
       (as-vec (:fills shape)))
      (reduce-kv
       (fn [m index stroke]
         (if (solid-color-stroke? stroke)
           (-> m
               (put :stroke-color index (:stroke-color stroke))
               (put :stroke-opacity index (or (:stroke-opacity stroke) 1))
               (put :stroke-width index (or (:stroke-width stroke) 0)))
           m))
       $
       (as-vec (:strokes shape)))
      (reduce-kv
       (fn [m index shadow]
         (-> m
             (put :shadow-offset-x index (or (:offset-x shadow) 0))
             (put :shadow-offset-y index (or (:offset-y shadow) 0))
             (put :shadow-blur index (or (:blur shadow) 0))
             (put :shadow-spread index (or (:spread shadow) 0))
             (put :shadow-color index (get-in shadow [:color :color]))
             (put :shadow-opacity index (or (get-in shadow [:color :opacity]) 1))))
       $
       (as-vec (:shadow shape)))
      (cond-> $
        (:blur shape)
        (put :blur nil (or (get-in shape [:blur :value]) 0))
        (:background-blur shape)
        (put :background-blur nil (or (get-in shape [:background-blur :value]) 0))))))

(defn- edit-value
  "The value of `property` of the shape `shape-id` of `objects`, with
  positions relative to the board of the timeline, so moving the board
  itself records nothing for its layers."
  [objects board-id shape-id property]
  (let [shape    (get objects shape-id)
        relative (fn [axis]
                   (let [own   (dm/get-in shape [:selrect axis])
                         board (if (= shape-id board-id) 0 (dm/get-in objects [board-id :selrect axis]))]
                     (when (and (some? own) (some? board))
                       (- own board))))]
    (case property
      :x        (relative :x)
      :y        (relative :y)
      :width    (dm/get-in shape [:selrect :width])
      :height   (dm/get-in shape [:selrect :height])
      :rotation (or (:rotation shape) 0)
      :opacity  (or (:opacity shape) 1)
      :r1       (or (:r1 shape) 0)
      :r2       (or (:r2 shape) 0)
      :r3       (or (:r3 shape) 0)
      :r4       (or (:r4 shape) 0))))

(defn- keyframe-easing-at
  "The easing of the keyframe of the slot of `property` and `index` of
  `shape-id` at `time`, for a keyframe that takes its place."
  [timeline shape-id property index time]
  (or (:easing (d/seek #(and (same-slot? % property index) (= (:time %) time))
                       (dm/get-in timeline [:tracks shape-id :keyframes])))
      :ease))

(defn record-edit
  "`timeline` with an edit of its shapes recorded at `time`, as motion
  mode does for an edit on the canvas: every animated property the edit
  changes gets a keyframe there. `before` and `after` are the objects
  before and after the edit. The keyframe takes the value shown at `time`
  plus the change (for opacity, sizes, radii and the appearance, the new
  value). Hidden and locked rows record nothing: the edit stays on the
  shape."
  [timeline before after time]
  (let [board-id (:board-id timeline)
        shown    (values-at (resolve-animations timeline before) time)

        record
        (fn [timeline shape-id property]
          (let [old (edit-value before board-id shape-id property)
                new (edit-value after board-id shape-id property)]
            (if (or (nil? old) (nil? new) (mth/close? old new))
              timeline
              (add-keyframe timeline shape-id
                            {:time time
                             :property property
                             :value (cond
                                      (contains? edit-absolute-properties property)
                                      new

                                      (= property :rotation)
                                      (+ (get-in shown [shape-id property]) (angle-delta old new))

                                      :else
                                      (+ (get-in shown [shape-id property]) (- new old)))
                             :easing (keyframe-easing-at timeline shape-id property nil time)}))))

        record-slot
        (fn [timeline shape-id property index old new]
          (if (close-value? property old new)
            timeline
            (add-keyframe timeline shape-id
                          (cond-> {:time time
                                   :property property
                                   :value new
                                   :easing (keyframe-easing-at timeline shape-id property index time)}
                            (some? index) (assoc :index index)))))]

    (reduce-kv
     (fn [timeline shape-id track]
       (let [muted    (into (get track :hidden #{}) (get track :locked))
             recorded (remove #(contains? muted (slot-key (:property %) (:index %)))
                              (:keyframes track))
             animated (into #{} (map :property) recorded)
             slots    (into #{} (map (juxt :property :index)) recorded)
             timeline (->> edit-properties
                           (filter animated)
                           (reduce #(record %1 shape-id %2) timeline))
             old      (appearance-slot-values (get before shape-id))
             new      (appearance-slot-values (get after shape-id))]
         (reduce (fn [timeline [property index]]
                   (record-slot timeline shape-id property index
                                (get old [property index])
                                (get new [property index])))
                 timeline
                 (filter slots (keys new)))))
     timeline
     (:tracks timeline))))

(defn- same-paint-layers?
  "Whether `a` and `b` have as many fills, strokes and shadows, so an
  index names the same one in both."
  [a b]
  (every? #(= (count (get a %)) (count (get b %))) [:fills :strokes :shadow]))

(defn record-unanimated
  "`timeline` with an edit of the shapes `shape-ids` recorded at `time`
  in the properties it changes that are not animated yet, as motion mode
  does with auto-keyframe on (`record-edit` takes the animated ones).
  Each gets a keyframe with its new value there and, past the start, one
  with the value before at 0, so the change plays. `before` and `after`
  are the objects before and after the edit. A shape whose path or text
  changes records nothing, nor do fills, strokes and shadows added or
  removed."
  [timeline before after shape-ids time]
  (let [board-id (:board-id timeline)

        record
        (fn [timeline shape-id property index old new]
          (let [keyframe (cond-> {:property property}
                           (some? index) (assoc :index index))]
            (cond-> (add-keyframe timeline shape-id (assoc keyframe :time time :value new))
              (pos? time)
              (add-keyframe shape-id (assoc keyframe :time 0 :value old)))))]

    (reduce
     (fn [timeline shape-id]
       (let [old-shape (get before shape-id)
             new-shape (get after shape-id)
             animated  (into #{}
                             (map (juxt :property :index))
                             (dm/get-in timeline [:tracks shape-id :keyframes]))]
         (if (or (nil? old-shape) (nil? new-shape)
                 (not= (:content old-shape) (:content new-shape)))
           timeline
           (let [timeline
                 (reduce (fn [timeline property]
                           (let [old (edit-value before board-id shape-id property)
                                 new (edit-value after board-id shape-id property)]
                             (cond
                               (or (contains? animated [property nil])
                                   (close-value? property old new))
                               timeline

                               ;; it turns the short way, as it does on the canvas
                               (= property :rotation)
                               (record timeline shape-id property nil old (+ old (angle-delta old new)))

                               :else
                               (record timeline shape-id property nil old new))))
                         timeline
                         edit-properties)
                 old (appearance-slot-values old-shape)
                 new (appearance-slot-values new-shape)]
             (if (same-paint-layers? old-shape new-shape)
               (reduce (fn [timeline [property index :as slot]]
                         (if (or (contains? animated slot)
                                 (close-value? property (get old slot) (get new slot)))
                           timeline
                           (record timeline shape-id property index (get old slot) (get new slot))))
                       timeline
                       (keys new))
               timeline)))))
     timeline
     shape-ids)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAINTENANCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-shapes
  "Drop the tracks targeting any of `shape-ids` (used when shapes are
  deleted). Returns the updated timeline."
  [timeline shape-ids]
  (update timeline :tracks #(apply dissoc % shape-ids)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS EXPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- fmt
  "Format a number for CSS: rounded to 3 decimals, integers without a
  trailing `.0`."
  [v]
  (let [r (mth/precision v 3)]
    (if (== r (mth/round r))
      (str (long r))
      (str r))))

(defn origin-css
  "CSS `transform-origin` value for a normalized `{ :x :y }` origin."
  [origin]
  (str (fmt (* 100.0 (:x origin))) "% "
       (fmt (* 100.0 (:y origin))) "%"))

(defn easing->css
  "Render a keyframe easing as a CSS timing-function string. A spring
  becomes a `linear()` function through points of its curve."
  [easing]
  (cond
    (= :spring (:type easing))
    (let [stops (spring-samples easing 40)]
      (str "linear("
           (->> (range (inc stops))
                (map (fn [i]
                       (let [t (/ i stops)]
                         (str (fmt (spring-progress easing t)) " " (fmt (* 100.0 t)) "%"))))
                (str/join ", "))
           ")"))

    (map? easing)
    (let [[a b c d] (:curve easing)]
      (str "cubic-bezier(" (fmt a) ", " (fmt b) ", " (fmt c) ", " (fmt d) ")"))

    (keyword? easing)
    (case easing
      :linear "linear"
      :ease "ease"
      :ease-in "ease-in"
      :ease-out "ease-out"
      :ease-in-out "ease-in-out"
      "linear")

    :else "linear"))

(defn shape-css-class
  "CSS class used by `timeline->css` for `id`. The whole id: the ids of
  the shapes of a file begin alike."
  [id]
  (str "penpot-shape-" id))

(defn- css-color
  [hex opacity]
  (if (or (nil? hex) (nil? opacity) (= opacity 1))
    (or hex "transparent")
    (let [[r g b] (clr/hex->rgb hex)]
      (str "rgba(" (int r) ", " (int g) ", " (int b) ", " (fmt opacity) ")"))))

(defn- css-box-shadow
  [shadow]
  (str (when (= :inner-shadow (:style shadow)) "inset ")
       (fmt (:offset-x shadow)) "px "
       (fmt (:offset-y shadow)) "px "
       (fmt (:blur shadow)) "px "
       (fmt (:spread shadow)) "px "
       (css-color (get-in shadow [:color :color])
                  (get-in shadow [:color :opacity]))))

(defn- values->declarations
  "Build the CSS declarations (transform, opacity, fill, stroke, shadow,
  blur) for `shape` given the interpolated `values` at one keyframe stop."
  [shape values origin]
  (let [selrect (:selrect shape)
        base-x  (:x selrect)
        base-y  (:y selrect)
        base-r  (or (:rotation shape) 0)
        dx      (if (contains? values :x) (- (+ (:x origin) (:x values)) base-x) 0)
        dy      (if (contains? values :y) (- (+ (:y origin) (:y values)) base-y) 0)
        sx      (axis-scale (or (:width selrect) 0) values :width :scale-x)
        sy      (axis-scale (or (:height selrect) 0) values :height :scale-y)
        rot     (- (get values :rotation base-r) base-r)
        transform (str "translate(" (fmt dx) "px, " (fmt dy) "px) "
                       "rotate(" (fmt rot) "deg) "
                       "scale(" (fmt sx) ", " (fmt sy) ")")
        fills   (apply-fills (:fills shape) values)
        strokes (apply-strokes (:strokes shape) values)
        shadows (apply-shadows (:shadow shape) values)
        fill    (d/seek solid-color-fill? fills)
        stroke  (d/seek solid-color-stroke? strokes)
        decls   [(str "transform: " transform ";")]]
    (cond-> decls
      (contains? values :opacity)
      (conj (str "opacity: " (fmt (:opacity values)) ";"))

      (has? values :r1 :r2 :r3 :r4)
      (conj (str "border-radius: "
                 (fmt (get values :r1 (:r1 shape))) "px "
                 (fmt (get values :r2 (:r2 shape))) "px "
                 (fmt (get values :r3 (:r3 shape))) "px "
                 (fmt (get values :r4 (:r4 shape))) "px;"))

      (and fill (has? values :fill-color :fill-opacity))
      (conj (str "background-color: "
                 (css-color (:fill-color fill) (or (:fill-opacity fill) 1)) ";"))

      (and stroke (has? values :stroke-color :stroke-opacity))
      (conj (str "border-color: "
                 (css-color (:stroke-color stroke) (or (:stroke-opacity stroke) 1)) ";"))

      (and stroke (contains? values :stroke-width))
      (conj (str "border-width: " (fmt (or (:stroke-width stroke) 0)) "px;"))

      (has? values :shadow-offset-x :shadow-offset-y :shadow-blur
            :shadow-spread :shadow-color :shadow-opacity)
      (conj (str "box-shadow: "
                 (->> shadows
                      (remove :hidden)
                      (map css-box-shadow)
                      (str/join ", "))
                 ";"))

      (contains? values :blur)
      (conj (str "filter: blur(" (fmt (:blur values)) "px);"))

      (contains? values :background-blur)
      (conj (str "backdrop-filter: blur(" (fmt (:background-blur values)) "px);")))))

(defn- keyframe-timing
  "The CSS timing function of the segment starting at `keyframe`, or nil
  when it has no easing: a hold jumps at its end."
  [keyframe]
  (cond
    (= :step (:interpolation keyframe)) "steps(1, end)"
    (some? (:easing keyframe))          (easing->css (:easing keyframe))))

(defn- track->keyframes-css
  [timeline shape track kf-name origin]
  (let [duration (max 1 (:duration timeline))
        sid      (:shape-id track)
        kfs      (:keyframes track)
        times    (-> (into (sorted-set 0 duration) (map :time kfs)) vec)
        stops    (for [t times]
                   (let [values     (get (values-at timeline t) sid {})
                         pct        (fmt (* 100.0 (/ (double t) duration)))
                         seg-timing (some #(when (= (:time %) t) (keyframe-timing %)) kfs)
                         decls      (cond-> (values->declarations shape values origin)
                                      (and seg-timing (< t duration))
                                      (conj (str "animation-timing-function: " seg-timing ";")))]
                     (str "  " pct "% { " (str/join " " decls) " }")))]
    (str "@keyframes " kf-name " {\n" (str/join "\n" stops) "\n}")))

(defn timeline->css
  "Generate a CSS string (`@keyframes` blocks + per-shape `animation`
  rules) for `timeline`, using the base geometry from `objects`. Pure.
  Played once, a shape keeps the last frame. With reduced motion asked
  for, the shapes stay as designed. Options: `:ids`, the shapes to write
  (all by default), `:selector`, the CSS selector of a shape
  (`shape-css-class` by default), and `:origin`, the `transform-origin`
  of a shape and its track (`origin-css` of the track origin, a share of
  the shape's box, by default)."
  ([timeline objects]
   (timeline->css timeline objects nil))
  ([timeline objects {:keys [ids selector origin]}]
   (let [timeline (-> timeline (resolve-animations objects) expand-loops expand-paths)
         duration (max 1 (:duration timeline))
         iter     (case (playback-mode timeline)
                    :loop      "infinite"
                    :ping-pong "infinite alternate"
                    "1 both")
         selector (or selector #(str "." (shape-css-class (:id %))))
         origin   (or origin (fn [_ track] (origin-css (track-origin track))))
         rules    (into []
                        (keep (fn [[sid track]]
                                (when-let [shape (and (or (nil? ids) (contains? ids sid))
                                                      (get objects sid))]
                                  (let [kf-name  (str "penpot-anim-" sid)
                                        selector (selector shape)
                                        comment  (str "/* " (or (:name shape) (str sid)) " */")]
                                    [selector
                                     (str comment "\n"
                                          (track->keyframes-css timeline shape track kf-name
                                                                (position-origin timeline objects sid)) "\n\n"
                                          selector " {\n  animation: " kf-name " "
                                          duration "ms linear " iter ";\n"
                                          "  transform-origin: "
                                          (origin shape track) ";\n}")]))))
                        (:tracks timeline))]
     (if (empty? rules)
       ""
       (str (str/join "\n\n" (map second rules))
            "\n\n@media (prefers-reduced-motion: reduce) {\n  "
            (str/join ",\n  " (map first rules))
            " {\n    animation: none;\n  }\n}")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOTTIE (bodymovin JSON) EXPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Builds a Lottie animation document (a plain map ready to be
;; JSON-encoded). The board (the timeline's `:board-id`) is the
;; composition; each tracked shape becomes a shape layer whose transform
;; (position/scale/rotation/opacity) is animated with native Lottie
;; keyframes + cubic-bezier easing handles. Rectangles, ellipses, paths
;; and booleans, and groups and boards of them, are exported as vectors,
;; with their fills, strokes, trim, drop shadows and blur (see
;; `lottie-vector-layer?`); other shapes as images or, without them, as a
;; rectangle of their bounds in their first fill colour.

(def ^:private lottie-fps 60)

(defn- ms->frames
  [ms]
  (mth/precision (* (/ (double ms) 1000.0) lottie-fps) 3))

(defn- easing->bezier-curve
  "Control points of a keyframe easing; springs are unrolled before (see
  `expand-springs`), anything else is linear."
  [easing]
  (or (easing-curve easing) (preset-curves :linear)))

(defn- hex->rgb01
  "Parse a `#rrggbb` colour into normalized [r g b] (0..1). Falls back to a
  neutral grey."
  [hex]
  (let [h (cond
            (and (string? hex) (str/starts-with? hex "#")) (subs hex 1)
            (string? hex) hex
            :else nil)]
    (if (and h (= 6 (count h)))
      (let [pp (fn [s]
                 (/ (double #?(:clj (Integer/parseInt s 16)
                               :cljs (js/parseInt s 16)))
                    255.0))]
        [(pp (subs h 0 2)) (pp (subs h 2 4)) (pp (subs h 4 6))])
      [0.694 0.698 0.710])))

(defn- lottie-keyframes
  "Build a Lottie keyframe vector for a single-property keyframe list.
  `val-fn` maps a keyframe value to its Lottie `s` array. The segment
  easing is carried on the starting keyframe via `:o`/`:i` (or `:h 1` for
  a stepped hold)."
  [kfs val-fn]
  (let [kfs (sort-keyframes kfs)
        n   (count kfs)]
    (vec (map-indexed
          (fn [i kf]
            (let [base {:t (ms->frames (:time kf)) :s (val-fn (:value kf))}]
              (cond
                (= i (dec n))
                base

                (= :step (:interpolation kf :linear))
                (assoc base :h 1)

                :else
                (let [[x1 y1 x2 y2] (easing->bezier-curve (:easing kf))]
                  (assoc base :o {:x [x1] :y [y1]} :i {:x [x2] :y [y2]})))))
          kfs))))

(defn- lottie-prop-1d
  "A 1D Lottie property (split position x/y, rotation, opacity). Static
  values are scalars, as is a single keyframe: Lottie players draw no
  layer with an animated value of one keyframe. Animated values use
  keyframes with `s` of `[v]`."
  [kfs scalar-fn default-scalar]
  (case (count kfs)
    0 {:a 0 :k default-scalar}
    1 {:a 0 :k (scalar-fn (:value (first kfs)))}
    {:a 1 :k (lottie-keyframes kfs (fn [v] [(scalar-fn v)]))}))

(defn- lottie-pair-prop
  "One 2D Lottie property from two (separate) 1D tracks, `a-kfs` and
  `b-kfs`; `val-fn` gives the Lottie value of their values, `a` and `b`
  when a track has none."
  [a-kfs b-kfs a b val-fn]
  (let [ak    (sort-keyframes a-kfs)
        bk    (sort-keyframes b-kfs)
        times (->> (concat ak bk) (map :time) distinct sort vec)
        n     (count times)
        at    (fn [kfs t] (some #(when (= (:time %) t) %) kfs))]
    (if (< n 2)
      ;; static for no keyframe or one instant (see `lottie-prop-1d`)
      {:a 0 :k (val-fn (or (:value (first ak)) a) (or (:value (first bk)) b))}
      {:a 1
       :k (vec (map-indexed
                (fn [i t]
                  (let [va   (or (property-value-at ak t) a)
                        vb   (or (property-value-at bk t) b)
                        kf   (or (at ak t) (at bk t))
                        base {:t (ms->frames t) :s (val-fn va vb)}]
                    (cond
                      (= i (dec n))
                      base

                      (= :step (:interpolation kf :linear))
                      (assoc base :h 1)

                      :else
                      (let [[x1 y1 x2 y2] (easing->bezier-curve (:easing kf))]
                        (assoc base :o {:x [x1] :y [y1]} :i {:x [x2] :y [y2]})))))
                times))})))

(defn- lottie-scale-prop
  "Combine the (separate) scale-x and scale-y tracks into one 2D Lottie
  scale property (percentages)."
  [sx-kfs sy-kfs]
  (lottie-pair-prop sx-kfs sy-kfs 1 1 (fn [x y] [(* 100.0 x) (* 100.0 y) 100])))

(defn- slot-kfs
  "Keyframes of `property` at `index` (nil = the unindexed property)."
  [by-prop property index]
  (filterv #(= (:index %) index) (by-prop property)))

(defn- lottie-color-prop
  "A Lottie colour: static for no keyframe or one (see `lottie-prop-1d`)."
  [kfs default-rgb]
  (let [rgba (fn [v]
               (let [[r g b] (hex->rgb01 v)]
                 [r g b 1]))]
    (case (count kfs)
      0 {:a 0 :k (conj (vec default-rgb) 1)}
      1 {:a 0 :k (rgba (:value (first kfs)))}
      {:a 1 :k (lottie-keyframes kfs rgba)})))

(defn- lottie-dashes
  "The dash pattern of a stroke of `style` and `width`, as the canvas
  draws it."
  [{:keys [stroke-style stroke-width stroke-dash stroke-gap]}]
  (let [w       (or stroke-width 1)
        pattern (case stroke-style
                  :dotted [0 (+ w 5)]
                  :dashed [(or stroke-dash (+ w 10)) (or stroke-gap (+ w 10))]
                  :mixed  [(+ w 5) (+ w 5) (+ w 1) (+ w 5)]
                  nil)]
    (when (seq pattern)
      (conj (vec (map-indexed (fn [i length]
                                (if (even? i)
                                  {:n "d" :nm "dash" :v {:a 0 :k length}}
                                  {:n "g" :nm "gap" :v {:a 0 :k length}}))
                              pattern))
            {:n "o" :nm "offset" :v {:a 0 :k 0}}))))

(defn- lottie-stroke-items
  "The Lottie strokes of `shape`, top-most first, each with the keyframes
  of its index. Lottie draws them centered on the outline."
  [shape by-prop]
  (->> (as-vec (:strokes shape))
       (map-indexed
        (fn [index stroke]
          (let [color-kfs (slot-kfs by-prop :stroke-color index)
                opac-kfs  (slot-kfs by-prop :stroke-opacity index)
                width-kfs (slot-kfs by-prop :stroke-width index)]
            (when (or (solid-color-stroke? stroke) (seq color-kfs))
              (let [cap    (:stroke-cap-start stroke)
                    dashes (lottie-dashes stroke)]
                (cond-> {:ty "st" :nm (str "stroke " index)
                         :c (lottie-color-prop color-kfs (hex->rgb01 (or (:stroke-color stroke)
                                                                         (:value (first color-kfs))
                                                                         "#000000")))
                         :o (lottie-prop-1d opac-kfs (fn [v] (* 100.0 v))
                                            (* 100.0 (or (:stroke-opacity stroke) 1)))
                         :w (lottie-prop-1d width-kfs identity (or (:stroke-width stroke) 1))
                         :lc (cond
                               (= :dotted (:stroke-style stroke))           2
                               (not= cap (:stroke-cap-end stroke))          1
                               :else (case cap :round 2 :square 3 1))
                         :lj 1 :ml 4}
                  (seq dashes) (assoc :d dashes)))))))
       (remove nil?)
       (vec)))

(defn- fill-rule
  "Lottie fill rule of `shape`: 2 (even-odd) when it says so, else 1."
  [shape]
  (if (= "evenodd" (or (dm/get-in shape [:svg-attrs :fill-rule])
                       (dm/get-in shape [:svg-attrs :fillRule])))
    2
    1))

(defn- lottie-gradient-fill
  "A Lottie gradient fill of `gradient` over the box `[x y width height]`,
  where the gradient points are fractions of the box."
  [{:keys [type start-x start-y end-x end-y stops]} opacity rule [x y width height]]
  (let [stops (sort-by :offset stops)
        point (fn [px py] [(+ x (* px width)) (+ y (* py height))])]
    {:ty "gf" :nm "gradient" :r rule
     :t (if (= type :radial) 2 1)
     :s {:a 0 :k (point start-x start-y)}
     :e {:a 0 :k (point end-x end-y)}
     :g {:p (count stops)
         :k {:a 0 :k (vec (concat (mapcat (fn [{:keys [offset color]}]
                                            (cons offset (hex->rgb01 color)))
                                          stops)
                                  (mapcat (fn [{:keys [offset opacity]}]
                                            [offset (or opacity 1)])
                                          stops)))}}
     :o {:a 0 :k (* 100.0 (or opacity 1))}}))

(defn- lottie-fill-items
  "The Lottie fills of `shape`, top-most first: flat colours with the
  keyframes of their index, and gradients over `box` (see
  `lottie-gradient-fill`)."
  [shape by-prop box]
  (let [rule (fill-rule shape)]
    (->> (as-vec (:fills shape))
         (map-indexed
          (fn [index fill]
            (cond
              (solid-color-fill? fill)
              {:ty "fl" :nm (str "fill " index) :r rule
               :c (lottie-color-prop (slot-kfs by-prop :fill-color index) (hex->rgb01 (:fill-color fill)))
               :o (lottie-prop-1d (slot-kfs by-prop :fill-opacity index) (fn [v] (* 100.0 v))
                                  (* 100.0 (or (:fill-opacity fill) 1)))}

              (some? (:fill-color-gradient fill))
              (lottie-gradient-fill (:fill-color-gradient fill) (:fill-opacity fill) rule box))))
         (remove nil?)
         (vec))))

(defn- lottie-trim-item
  "Lottie trim paths of the trim keyframes, or nil without any: start and
  end in percent, the offset in degrees (a whole turn is the outline)."
  [by-prop]
  (when (some #(seq (by-prop %)) (keys trim-properties))
    {:ty "tm" :nm "trim" :m 1
     :s (lottie-prop-1d (by-prop :trim-start) (fn [v] (* 100.0 v)) 0)
     :e (lottie-prop-1d (by-prop :trim-end) (fn [v] (* 100.0 v)) 100)
     :o (lottie-prop-1d (by-prop :trim-offset) (fn [v] (* 360.0 v)) 0)}))

(defn- lottie-blur-effect
  [shape by-prop]
  (let [kfs (by-prop :blur)]
    (when (or (seq kfs) (some? (:blur shape)))
      [{:ty 29
        :nm "Gaussian Blur"
        :en 1
        :ef [{:ty 0 :nm "Sigma"
              :v (lottie-prop-1d kfs identity (or (get-in shape [:blur :value]) 0))}]}])))

(defn- lottie-pair-1d
  "A 1D Lottie property of two 1D tracks (see `lottie-pair-prop`), `val-fn`
  giving its value of theirs."
  [a-kfs b-kfs a b val-fn]
  (let [prop (lottie-pair-prop a-kfs b-kfs a b (fn [va vb] [(val-fn va vb)]))]
    (cond-> prop
      (zero? (:a prop)) (update :k first))))

(defn- drop-shadow?
  [shadow]
  (and (= :drop-shadow (:style shadow)) (not (:hidden shadow))))

(defn- lottie-shadow-effects
  "Lottie drop shadows of the drop shadows of `shape`, each with the
  keyframes of its index. Lottie has no spread."
  [shape by-prop]
  (->> (as-vec (:shadow shape))
       (map-indexed
        (fn [index shadow]
          (when (drop-shadow? shadow)
            (let [kfs      #(slot-kfs by-prop % index)
                  x        (or (:offset-x shadow) 0)
                  y        (or (:offset-y shadow) 0)
                  offset   #(lottie-pair-1d (kfs :shadow-offset-x) (kfs :shadow-offset-y) x y %)]
              {:ty 25 :nm "Drop Shadow" :en 1 :np 8
               :ef [{:ty 2 :nm "Shadow Color"
                     :v (lottie-color-prop (kfs :shadow-color)
                                           (hex->rgb01 (or (dm/get-in shadow [:color :color]) "#000000")))}
                    {:ty 0 :nm "Opacity"
                     :v (lottie-prop-1d (kfs :shadow-opacity) (fn [v] (* 255.0 v))
                                        (* 255.0 (or (dm/get-in shadow [:color :opacity]) 1)))}
                    ;; 0 is up, 90 to the right
                    {:ty 0 :nm "Direction" :v (offset (fn [dx dy] (mth/degrees (mth/atan2 dx (- dy)))))}
                    {:ty 0 :nm "Distance" :v (offset mth/hypot)}
                    {:ty 0 :nm "Softness"
                     :v (lottie-prop-1d (kfs :shadow-blur) (fn [v] (* 2.0 v)) (* 2.0 (or (:blur shadow) 0)))}
                    {:ty 7 :nm "Shadow Only" :v {:a 0 :k 0}}]}))))
       (remove nil?)
       (vec)))

(defn- lottie-anchor
  "Layer-space point of the track origin on `shape`."
  [shape ox oy origin]
  (let [sr (:selrect shape)
        sw (double (or (:width sr) 0))
        sh (double (or (:height sr) 0))
        nx (or (:x origin) 0.5)
        ny (or (:y origin) 0.5)]
    [(+ (- (or (:x sr) 0) ox) (* sw nx))
     (+ (- (or (:y sr) 0) oy) (* sh ny))]))

(defn- lottie-ks
  [shape track ox oy origin]
  (let [by-prop (group-by :property (:keyframes track))
        pivot   (track-origin track)
        [ax ay] (lottie-anchor shape ox oy pivot)
        nx      (:x pivot)
        ny      (:y pivot)
        sw      (double (or (:width (:selrect shape)) 0))
        sh      (double (or (:height (:selrect shape)) 0))
        base-r  (or (:rotation shape) 0)]
    {:o (lottie-prop-1d (by-prop :opacity) (fn [v] (* 100.0 v)) (* 100.0 (or (:opacity shape) 1)))
     :r (lottie-prop-1d (by-prop :rotation) identity base-r)
     :p {:s true
         :x (lottie-prop-1d (by-prop :x) (fn [v] (+ (- (+ (:x origin) v) ox) (* sw nx))) ax)
         :y (lottie-prop-1d (by-prop :y) (fn [v] (+ (- (+ (:y origin) v) oy) (* sh ny))) ay)}
     :a {:a 0 :k [ax ay 0]}
     :s (lottie-scale-prop (by-prop :scale-x) (by-prop :scale-y))}))

(defn- shape->lottie-image-layer
  "An image layer of `shape`, whose image is rasterized opaque: the layer
  has its opacity. The image begins at the layer's origin: the anchor is
  the track origin on it, not on the composition."
  [ind shape track ox oy origin asset duration]
  {:ddd 0
   :ind (inc ind)
   :ty 2
   :nm (or (:name shape) (str "layer-" (inc ind)))
   :refId (:id asset)
   :sr 1
   :ks (assoc (lottie-ks shape track ox oy origin)
              :a {:a 0 :k (conj (lottie-anchor (update shape :selrect assoc :x 0 :y 0) 0 0
                                               (track-origin track))
                                0)})
   :ao 0
   :ip 0
   :op (ms->frames (max 1 duration))
   :st 0
   :bm 0})

(def ^:private lottie-group-transform
  {:ty "tr" :nm "transform"
   :p {:a 0 :k [0 0]}
   :a {:a 0 :k [0 0]}
   :s {:a 0 :k [100 100]}
   :r {:a 0 :k 0}
   :o {:a 0 :k 100}})

(defn- lottie-group
  [name items]
  (let [items (conj (vec items) lottie-group-transform)]
    {:ty "gr" :nm name :np (count items) :it items}))

(defn- lottie-paints?
  "Whether the fills and strokes of `shape` are Lottie paints: flat or
  gradient fills, flat strokes."
  [shape]
  (and (every? #(or (solid-color-fill? %) (some? (:fill-color-gradient %)))
               (as-vec (:fills shape)))
       (every? solid-color-stroke? (as-vec (:strokes shape)))))

(declare lottie-content?)

(defn- lottie-outline?
  "Whether the kind of `shape` draws as Lottie vectors, the layers of a
  group or board as `lottie-content?` says, but for those in `layer-ids`,
  which draw on their own."
  [objects layer-ids shape]
  (case (:type shape)
    (:rect :circle) true
    (:path :bool)   (some? (:content shape))
    :group          (and (not (:masked-group shape))
                         (every? #(or (contains? layer-ids %)
                                      (lottie-content? objects layer-ids (get objects %)))
                                 (:shapes shape)))
    :frame          (every? #(or (contains? layer-ids %)
                                 (lottie-content? objects layer-ids (get objects %)))
                            (:shapes shape))
    false))

(defn- lottie-content?
  "Whether `shape`, drawn inside a layer, draws as Lottie vectors (a hidden
  one draws nothing). Effects go with a whole layer, so no shadow nor blur,
  and no board clipping its content."
  [objects layer-ids shape]
  (or (true? (:hidden shape))
      (and (some? shape)
           (lottie-paints? shape)
           (empty? (as-vec (:shadow shape)))
           (nil? (:blur shape))
           (or (not= :frame (:type shape)) (true? (:show-content shape)))
           (lottie-outline? objects layer-ids shape))))

(defn lottie-vector-layer?
  "Whether the layer `id` of a Lottie export draws as vectors (`layer-ids`
  are all the layers of the export): a rectangle, ellipse, path or
  boolean, or a group or board of such, with flat or gradient fills, flat
  strokes, and drop shadows and blur of its own at most. Anything else
  draws as an image (see `timeline->lottie`)."
  [objects layer-ids id]
  (let [shape (get objects id)]
    (and (some? shape)
         (lottie-paints? shape)
         (every? #(or (drop-shadow? %) (:hidden %)) (as-vec (:shadow shape)))
         (lottie-outline? objects layer-ids shape))))

(defn- lottie-path-shapes
  "Lottie paths (`sh`) of path `content`, one per subpath, moved by
  `dx`, `dy`. Tangents are relative to their vertex."
  [content dx dy]
  (let [point  (fn [x y] [(+ x dx) (+ y dy)])
        vertex (fn [[x y]] {:v [x y] :i [0 0] :o [0 0]})
        item   (fn [vertices closed?]
                 (let [first-v  (first vertices)
                       last-v   (peek vertices)
                       back?    (and (> (count vertices) 2)
                                     (mth/close? ((:v first-v) 0) ((:v last-v) 0))
                                     (mth/close? ((:v first-v) 1) ((:v last-v) 1)))
                       ;; an outline back on its first vertex closes on it,
                       ;; as the ones `path/convert-to-path` gives do
                       closed?  (or closed? back?)
                       vertices (if back?
                                  (assoc (pop vertices) 0 (assoc first-v :i (:i last-v)))
                                  vertices)]
                   {:ty "sh" :nm "path"
                    :ks {:a 0 :k {:c closed?
                                  :v (mapv :v vertices)
                                  :i (mapv :i vertices)
                                  :o (mapv :o vertices)}}}))
        done   (fn [items vertices closed?]
                 (cond-> items (> (count vertices) 1) (conj (item vertices closed?))))]
    (loop [segments (seq (vec content))
           vertices []
           items    []]
      (if-let [{:keys [command params]} (first segments)]
        (let [{:keys [x y c1x c1y c2x c2y]} params
              p (when (some? x) (point x y))]
          (case command
            :move-to
            (recur (rest segments) [(vertex p)] (done items vertices false))

            :line-to
            (recur (rest segments) (conj vertices (vertex p)) items)

            :curve-to
            (let [[px py] (:v (or (peek vertices) (vertex p)))
                  [ax ay] (point c1x c1y)
                  [bx by] (point c2x c2y)
                  prev    (if (seq vertices)
                            (assoc-in vertices [(dec (count vertices)) :o] [(- ax px) (- ay py)])
                            [(vertex [px py])])]
              (recur (rest segments)
                     (conj prev {:v p :i [(- bx (p 0)) (- by (p 1))] :o [0 0]})
                     items))

            :close-path
            ;; what follows without a move starts where this one did
            (recur (rest segments)
                   (if (seq vertices) [(vertex (:v (first vertices)))] [])
                   (done items vertices true))

            (recur (rest segments) vertices items)))
        (done items vertices false)))))

(defn- lottie-outline
  "The outline of `shape` as Lottie items, in board coordinates (the
  board at `ox`, `oy`) and before it turns: the layer turns it. An
  ellipse and a rectangle with one radius take their size (and radius)
  from the keyframes, growing from the top left corner."
  [shape by-prop ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        left   (- x ox)
        top    (- y oy)
        size   (lottie-pair-prop (by-prop :width) (by-prop :height) width height
                                 (fn [w h] [w h]))
        center (lottie-pair-prop (by-prop :width) (by-prop :height) width height
                                 (fn [w h] [(+ left (/ w 2)) (+ top (/ h 2))]))
        rotation (or (:rotation shape) 0)]
    (cond
      (= :circle (:type shape))
      [{:ty "el" :nm "ellipse" :d 1 :s size :p center}]

      (and (contains? #{:rect :frame} (:type shape))
           (= (:r1 shape) (:r2 shape) (:r3 shape) (:r4 shape)))
      [{:ty "rc" :nm "rect" :d 1 :s size :p center
        :r (lottie-prop-1d (by-prop :r1) identity (or (:r1 shape) 0))}]

      (contains? #{:rect :frame} (:type shape))
      (-> (assoc shape :type :rect)
          (dissoc :transform :transform-inverse :flip-x :flip-y)
          (path/convert-to-path)
          (:content)
          (lottie-path-shapes (- ox) (- oy)))

      :else
      (-> (cond-> (:content shape)
            (not (mth/almost-zero? rotation))
            (path/transform-content (gmt/rotate-matrix (- rotation)
                                                       (gpt/point (+ x (/ width 2)) (+ y (/ height 2))))))
          (lottie-path-shapes (- ox) (- oy))))))

(defn- lottie-vector-groups
  "The Lottie groups drawing `shape` as vectors: its outline with its
  strokes over its fills. A trim trims the strokes only, as the canvas
  does: they get a trimmed copy of the outline."
  [shape by-prop ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        outline (lottie-outline shape by-prop ox oy)
        strokes (lottie-stroke-items shape by-prop)
        fills   (lottie-fill-items shape by-prop [(- x ox) (- y oy) width height])
        trim    (lottie-trim-item by-prop)]
    (if (and (some? trim) (seq strokes))
      [(lottie-group "strokes" (concat outline [trim] strokes))
       (lottie-group "fills" (concat outline fills))]
      [(lottie-group "shape" (concat outline strokes fills))])))

(defn- lottie-turned-back
  "`content`, the outline of a shape where it is on the canvas, turned
  back by `rotation` around `center` (the layer turns it) and as Lottie
  paths in the board at `ox`, `oy`."
  [content rotation center ox oy]
  (-> (cond-> content
        (not (mth/almost-zero? rotation))
        (path/transform-content (gmt/rotate-matrix (- rotation) center)))
      (lottie-path-shapes (- ox) (- oy))))

(defn- canvas-outline
  "The outline of `shape` where it is on the canvas, as path content."
  [shape]
  (if (contains? #{:path :bool} (:type shape))
    (:content shape)
    (:content (path/convert-to-path (cond-> shape (= :frame (:type shape)) (assoc :type :rect))))))

(defn- lottie-content-group
  "A Lottie group drawing `shape`, a layer of a group or board drawn in
  one layer that turns by `rotation` around `center`: its outline turned
  back with its strokes over its fills or, for a group or board, the ones
  of its layers (but for those in `layer-ids`, which draw on their own),
  the top-most first, over the board's own. None for a hidden one."
  [objects layer-ids shape rotation center ox oy]
  (when-not (true? (:hidden shape))
    (let [children (when (contains? #{:group :frame} (:type shape))
                     (->> (rseq (painted-children objects (:id shape)))
                          (remove #(contains? layer-ids %))
                          (keep #(lottie-content-group objects layer-ids (get objects %)
                                                       rotation center ox oy))))
          {:keys [x y width height]} (:selrect shape)
          paint    (when (not= :group (:type shape))
                     (concat (lottie-stroke-items shape {})
                             (lottie-fill-items shape {} [(- x ox) (- y oy) width height])))
          items    (cond-> (vec children)
                     (seq paint)
                     (conj (lottie-group "shape" (concat (lottie-turned-back (canvas-outline shape)
                                                                             rotation center ox oy)
                                                         paint))))]
      (when (seq items)
        (-> (lottie-group (or (:name shape) "layer") items)
            (update :it (fn [it]
                          (assoc-in it [(dec (count it)) :o :k] (* 100.0 (or (:opacity shape) 1))))))))))

(defn- lottie-clip-mask
  "A Lottie mask keeping a layer to the outline of the board `shape`."
  [shape ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        center (gpt/point (+ x (/ width 2)) (+ y (/ height 2)))
        path   (first (lottie-turned-back (canvas-outline shape) (or (:rotation shape) 0) center ox oy))]
    {:inv false :mode "a" :nm "clip" :pt (:ks path) :o {:a 0 :k 100} :x {:a 0 :k 0}}))

(defn- lottie-layer-groups
  "The Lottie groups drawing the layer `shape` as vectors: its own shape
  with the keyframes of its track (see `lottie-vector-groups`) or, for a
  group, its layers, and for a board, its layers over its own shape."
  [objects layer-ids shape by-prop ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        center   (gpt/point (+ x (/ width 2)) (+ y (/ height 2)))
        rotation (or (:rotation shape) 0)
        children #(->> (rseq (painted-children objects (:id shape)))
                       (remove (fn [id] (contains? layer-ids id)))
                       (keep (fn [id] (lottie-content-group objects layer-ids (get objects id)
                                                            rotation center ox oy))))]
    (case (:type shape)
      :group (vec (children))
      :frame (into (vec (children)) (lottie-vector-groups shape by-prop ox oy))
      (lottie-vector-groups shape by-prop ox oy))))

(defn- lottie-placeholder-group
  "A rectangle of the bounds of `shape` in its first fill colour, for a
  shape drawn neither as vectors nor as an image."
  [shape by-prop ox oy]
  (let [{:keys [x y width height]} (:selrect shape)
        fill (d/seek solid-color-fill? (as-vec (:fills shape)))]
    (lottie-group
     "shape"
     (concat [{:ty "rc" :d 1 :nm "rect"
               :s {:a 0 :k [width height]}
               :p {:a 0 :k [(+ (- x ox) (/ width 2.0)) (+ (- y oy) (/ height 2.0))]}
               :r {:a 0 :k 0}}]
             (lottie-stroke-items shape by-prop)
             [{:ty "fl" :nm "fill" :r 1
               :c (lottie-color-prop (slot-kfs by-prop :fill-color 0)
                                     (hex->rgb01 (or (:fill-color fill)
                                                     (-> shape :fills first :fill-color))))
               :o (lottie-prop-1d (slot-kfs by-prop :fill-opacity 0)
                                  (fn [v] (* 100.0 v))
                                  (* 100.0 (or (:fill-opacity fill) 1)))}]))))

(defn- shape->lottie-layer
  "Build a Lottie shape layer for `shape`/`track`, as vectors when it can
  be (see `lottie-vector-layer?`, `layer-ids` all the layers), with its
  drop shadows and blur as effects and a board kept to its outline.
  `ox`/`oy` is the board origin so coordinates are relative to the
  composition; keyframe positions are relative to `origin`. Without
  `effects?` or `mask?`, those are left to the precomp holding it."
  [ind objects layer-ids shape track ox oy origin & {:keys [effects? mask?] :or {effects? true mask? true}}]
  (let [by-prop (group-by :property (:keyframes track))
        vector? (lottie-vector-layer? objects layer-ids (:id shape))
        effects (when effects?
                  (concat (when vector? (lottie-shadow-effects shape by-prop))
                          (lottie-blur-effect shape by-prop)))]
    (cond-> {:ddd 0
             :ind (inc ind)
             :ty 4
             :nm (or (:name shape) (str "layer-" (inc ind)))
             :sr 1
             :ks (lottie-ks shape track ox oy origin)
             :ao 0
             :shapes (if vector?
                       (lottie-layer-groups objects layer-ids shape by-prop ox oy)
                       [(lottie-placeholder-group shape by-prop ox oy)])
             :ip 0
             :op (ms->frames (max 1 (:duration track 0)))
             :st 0
             :bm 0}
      (seq effects)
      (assoc :ef (vec effects))

      (and mask? vector? (= :frame (:type shape)) (not (true? (:show-content shape))))
      (assoc :hasMask true :masksProperties [(lottie-clip-mask shape ox oy)]))))

(defn- lottie-markers
  "The Lottie markers of the markers of `timeline`: each names the stretch
  up to the next one, or to the end, so a player plays it by its name."
  [timeline]
  (let [markers (sort-by :time (:markers timeline))
        ends    (concat (map :time (rest markers)) [(:duration timeline)])]
    (mapv (fn [{:keys [time name]} end]
            {:tm (ms->frames time)
             :cm name
             :dr (ms->frames (max 0 (- end time)))})
          markers
          ends)))

(defn- lottie-document
  [timeline objects layers assets]
  (let [board  (get objects (:board-id timeline))
        bsr    (:selrect board)
        width  (or (:width bsr) 100)
        height (or (:height bsr) 100)]
    {:v "5.7.0"
     :fr lottie-fps
     :ip 0
     :op (ms->frames (max 1 (:duration timeline)))
     :w (mth/round width)
     :h (mth/round height)
     :nm (or (:name timeline) "Penpot Animation")
     :ddd 0
     :assets (vec assets)
     :layers (vec layers)}))

(defn- clipping-board?
  [shape]
  (and (= :frame (:type shape)) (not (true? (:show-content shape)))))

(defn- layer-nodes
  "The layers inside `id` as the Lottie and SVG exports nest them, the
  bottom-most first: a layer is `{:id id}`, with `:inner` nodes when
  layers sit inside it, so they follow it; a board that clips layers but
  is none itself is `{:id id :clip? true :inner nodes}`; any other shape
  gives the layers inside it in its place."
  [objects layer-ids id]
  (into []
        (mapcat (fn [cid]
                  (let [inner (layer-nodes objects layer-ids cid)]
                    (cond
                      (contains? layer-ids cid)
                      [(cond-> {:id cid} (seq inner) (assoc :inner inner))]

                      (and (seq inner) (clipping-board? (get objects cid)))
                      [{:id cid :clip? true :inner inner}]

                      :else inner))))
        (painted-children objects id)))

(def ^:private lottie-layer-properties
  "What a layer does as a whole: a precomp does it to all its layers."
  #{:x :y :rotation :scale-x :scale-y :opacity})

(defn- lottie-track
  [{:keys [timeline duration]} id]
  (-> (or (get-in timeline [:tracks id]) {:shape-id id :keyframes []})
      (assoc :duration duration)))

(defn- lottie-numbered
  [layers]
  (into [] (map-indexed (fn [i layer] (assoc layer :ind (inc i)))) layers))

(defn- lottie-shape-layer
  "A layer drawing `shape` with `track`: as vectors when it can be, else
  as its image or a placeholder (see `shape->lottie-layer`)."
  [{:keys [objects layer-ids assets timeline duration ox oy]} shape track
   & {:keys [effects? mask?] :or {effects? true mask? true}}]
  (let [id     (:id shape)
        origin (position-origin timeline objects id)
        asset  (get assets id)]
    (if (and (some? asset) (not (lottie-vector-layer? objects layer-ids id)))
      (shape->lottie-image-layer 0 shape track ox oy origin asset duration)
      (shape->lottie-layer 0 objects layer-ids shape track ox oy origin
                           :effects? effects? :mask? mask?))))

(defn- lottie-content-layer
  "The layer drawing `shape` itself and the shapes inside it that are no
  layers, for the precomp (or the composition) that moves, fades and
  clips it."
  [ctx shape]
  (let [track (update (lottie-track ctx (:id shape)) :keyframes
                      #(into [] (remove (comp lottie-layer-properties :property)) %))]
    (lottie-shape-layer ctx (assoc shape :opacity 1) track :effects? false :mask? false)))

(defn- lottie-draws?
  "Whether the content layer of `shape` draws anything: paint of its own
  or shapes inside it that are no layers."
  [objects layer-ids shape]
  (boolean
   (or (seq (:fills shape))
       (seq (:strokes shape))
       (some #(and (not (contains? layer-ids %))
                   (not (true? (dm/get-in objects [% :hidden]))))
             (:shapes shape)))))

(declare lottie-node-layers)

(defn- lottie-precomp-layer
  "A precomp layer of the node `id` (see `layer-nodes`): it moves, fades
  and clips its layers, with the shape's own drawing at the bottom. The
  precomp itself goes to `comps`."
  [{:keys [objects layer-ids timeline duration width height ox oy comps] :as ctx}
   {:keys [id inner clip?]}]
  (let [shape   (get objects id)
        track   (if clip?
                  {:shape-id id :keyframes [] :duration duration}
                  (lottie-track ctx id))
        content (when (and (not clip?)
                           (or (contains? (:assets ctx) id)
                               (lottie-draws? objects layer-ids shape)))
                  (lottie-content-layer ctx shape))
        ref     (str "comp_" id)
        ;; before adding this precomp: the ones inside it add themselves
        layers  (lottie-numbered (cond-> (lottie-node-layers ctx inner)
                                   (some? content) (conj content)))
        by-prop (group-by :property (:keyframes track))
        effects (when-not clip?
                  (concat (when (lottie-vector-layer? objects layer-ids id)
                            (lottie-shadow-effects shape by-prop))
                          (lottie-blur-effect shape by-prop)))]
    (vswap! comps conj {:id ref :nm (or (:name shape) ref) :layers layers})
    (cond-> {:ddd 0
             :ty 0
             :nm (or (:name shape) ref)
             :refId ref
             :sr 1
             :ks (lottie-ks shape track ox oy (position-origin timeline objects id))
             :ao 0
             :w width
             :h height
             :ip 0
             :op (ms->frames duration)
             :st 0
             :bm 0}
      (seq effects)
      (assoc :ef (vec effects))

      (clipping-board? shape)
      (assoc :hasMask true :masksProperties [(lottie-clip-mask shape ox oy)]))))

(defn- lottie-node-layers
  "The Lottie layers of `nodes` (see `layer-nodes`), the top-most first."
  [ctx nodes]
  (into []
        (keep (fn [{:keys [id inner] :as node}]
                (when-let [shape (get (:objects ctx) id)]
                  (if (seq inner)
                    (lottie-precomp-layer ctx node)
                    (lottie-shape-layer ctx shape (lottie-track ctx id))))))
        (rseq nodes)))

(defn- lottie-layers
  "The layers of the composition of `timeline`, the top-most first, and
  the precomps they use: a layer with layers inside it, or a board that
  clips some, becomes a precomp (see `lottie-precomp-layer`). Layers not
  in the tree of the board go below the others, and the board draws
  itself, and what is no layer, at the bottom."
  [timeline objects layer-ids assets]
  (let [board-id (:board-id timeline)
        board    (get objects board-id)
        bsr      (:selrect board)
        ctx      {:objects objects
                  :layer-ids layer-ids
                  :assets assets
                  :timeline timeline
                  :duration (max 1 (:duration timeline))
                  :ox (or (:x bsr) 0)
                  :oy (or (:y bsr) 0)
                  :width (mth/round (or (:width bsr) 100))
                  :height (mth/round (or (:height bsr) 100))
                  :comps (volatile! [])}
        tree     (layer-nodes objects layer-ids board-id)
        treed    (into #{} (comp (mapcat #(tree-seq :inner :inner %)) (map :id)) tree)
        untreed  (into [] (comp (remove treed)
                                (remove #{board-id})
                                (filter #(contains? objects %))
                                (map (fn [id] {:id id})))
                       layer-ids)
        layers   (lottie-node-layers ctx (into untreed tree))
        board    (cond-> board (true? (:hide-fill-on-export board)) (assoc :fills []))
        content  (when (and (some? board)
                            (or (contains? assets board-id)
                                (lottie-draws? objects layer-ids board)))
                   (lottie-content-layer ctx board))]
    {:layers (lottie-numbered (cond-> layers (some? content) (conj content)))
     :comps  @(:comps ctx)}))

(defn timeline->lottie
  "Generate a Lottie (bodymovin) animation document (a plain map ready for
  JSON encoding) for `timeline`, using `objects` for geometry. The board
  (`:board-id`) is the composition. Pure.

  When `assets` is a non-empty map of `{shape-id {:id :w :h :p :e}}`,
  the export layers (see `export-layer-ids`) are those shapes, which
  become image layers (`ty` 2) unless they export as vectors (see
  `lottie-vector-layer?`); the board's own asset draws it. Without
  assets the animated shapes are the layers, as vectors or rectangles of
  their bounds. Layers holding layers become precomps (see
  `lottie-layers`). The markers become Lottie markers (see
  `lottie-markers`)."
  ([timeline objects]
   (timeline->lottie timeline objects nil))
  ([timeline objects assets]
   (let [markers   (lottie-markers timeline)
         ;; Lottie has no ping-pong, so the way back is part of the document.
         timeline  (-> timeline (resolve-animations objects) expand-loops expand-paths expand-springs expand-ping-pong)
         board-id  (:board-id timeline)
         layer-ids (if (seq assets)
                     (set (export-layer-ids objects board-id (:tracks timeline)))
                     (disj (set (keys (:tracks timeline))) board-id))
         {:keys [layers comps]} (lottie-layers timeline objects layer-ids assets)
         images    (map (fn [[_ asset]]
                          {:id (:id asset)
                           :w (:w asset)
                           :h (:h asset)
                           :u ""
                           :p (:p asset)
                           :e (or (:e asset) 1)})
                        assets)]
     (cond-> (lottie-document timeline objects layers (concat images comps))
       (seq markers) (assoc :markers markers)))))

(defn- svg-escape
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "\"" "&quot;")
      (str/replace "<" "&lt;")))

(defn- svg-clip-id
  [id]
  (str "penpot-clip-" id))

(defn- svg-clip-paths
  "The clip paths of the boards among `nodes` (see `layer-nodes`) that
  clip layers, where they are in the SVG, whose origin is `ox`/`oy`."
  [objects ox oy nodes]
  (->> (rest (tree-seq :inner :inner {:inner nodes}))
       (filter #(and (seq (:inner %)) (clipping-board? (get objects (:id %)))))
       (map (fn [{:keys [id]}]
              (str "<clipPath id=\"" (svg-escape (svg-clip-id id)) "\">"
                   "<path transform=\"translate(" (fmt (- ox)) " " (fmt (- oy)) ")\" d=\""
                   (path/content (canvas-outline (get objects id))) "\"/>"
                   "</clipPath>")))
       (str/join "")))

(defn- emit-svg-node
  "The SVG of a node of `layer-nodes`: a layer is a group its CSS class
  animates, holding its image and the layers inside it, which so follow
  it; a board that clips keeps those to its outline. All images sit
  where they are in the SVG, whose origin is `ox`/`oy`."
  [objects images ox oy {:keys [id inner clip?]}]
  (when-let [shape (get objects id)]
    (let [inner   (str/join "" (keep #(emit-svg-node objects images ox oy %) inner))
          clipped #(if (and (seq inner) (clipping-board? shape))
                     (str "<g clip-path=\"url(#" (svg-escape (svg-clip-id id)) ")\">" % "</g>")
                     %)]
      (if clip?
        (clipped inner)
        (let [img     (get images id)
              sr      (:selrect shape)
              ;; the image is opaque; an animated opacity overrides this
              opacity (or (:opacity shape) 1)]
          (str "<g class=\"" (svg-escape (shape-css-class id)) "\""
               (when (< opacity 1) (str " opacity=\"" (fmt opacity) "\""))
               ">"
               (clipped
                (str "<image href=\"" (svg-escape (or (:href img) ""))
                     "\" x=\"" (fmt (- (or (:x sr) 0) ox))
                     "\" y=\"" (fmt (- (or (:y sr) 0) oy))
                     "\" width=\"" (fmt (or (:width img) (:width sr) 0))
                     "\" height=\"" (fmt (or (:height img) (:height sr) 0)) "\"/>"
                     inner))
               "</g>"))))))

(defn timeline->svg
  "Build an animated SVG: rest-pose `<image>` per export layer plus the
  CSS from `timeline->css`. The layers nest as `layer-nodes` says, so a
  layer follows the one it sits in and a board clips them; each turns
  and scales around its pivot in the SVG. `images` is `{shape-id {:href
  :x :y :width :height}}`; the board's own image, when there, goes below
  them."
  [timeline objects images]
  (let [timeline (-> timeline (resolve-animations objects) expand-loops)
        board-id (:board-id timeline)
        bsr      (:selrect (get objects board-id))
        width    (mth/round (or (:width bsr) 100))
        height   (mth/round (or (:height bsr) 100))
        ox       (or (:x bsr) 0)
        oy       (or (:y bsr) 0)
        ;; SVG takes a share of `transform-origin` of the whole drawing
        css      (timeline->css timeline objects
                                {:origin (fn [shape track]
                                           (let [[x y] (lottie-anchor shape ox oy (track-origin track))]
                                             (str (fmt x) "px " (fmt y) "px")))})
        ids      (export-layer-ids objects board-id (:tracks timeline))
        nodes    (layer-nodes objects (set ids) board-id)
        clips    (svg-clip-paths objects ox oy nodes)
        backdrop (when-let [img (get images board-id)]
                   (str "<image href=\"" (svg-escape (:href img))
                        "\" x=\"0\" y=\"0\" width=\"" width "\" height=\"" height "\"/>"))]
    (str "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" width
         "\" height=\"" height "\" viewBox=\"0 0 " width " " height
         "\" overflow=\"hidden\">"
         (when (seq clips) (str "<defs>" clips "</defs>"))
         "<style>g { transform-box: view-box; }\n" css "</style>"
         backdrop
         (str/join "" (keep #(emit-svg-node objects images ox oy %) nodes))
         "</svg>")))
