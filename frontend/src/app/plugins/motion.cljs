;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.plugins.motion
  "The keyframe animation of boards (Penpot Motion) in the plugin API: the
  timeline of a board, the keyframes of its layers and their preset
  animations (see `app.common.types.animation`). Like the other proxies
  they keep ids only and read the timeline of the page again each time.
  Changing an animation needs the `animation/v1` feature."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.animation :as cta]
   [app.common.types.color :as clr]
   [app.common.types.shape.interactions :as cti]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.workspace.animation :as dwa]
   [app.main.features :as features]
   [app.main.store :as st]
   [app.plugins.format :as format]
   [app.plugins.register :as r]
   [app.plugins.system-events :as se]
   [app.plugins.utils :as u]
   [app.util.object :as obj]
   [clojure.set :as set]
   [cuerdas.core :as str]))

;; Set in `app.plugins`: the shapes need the timelines and the other way
;; round.
(def shape-proxy identity)
(def shape-proxy? identity)

(def property-names
  "The animatable properties as the plugin API names them, camelCase like
  the rest of it."
  {:x "x"
   :y "y"
   :width "width"
   :height "height"
   :rotation "rotation"
   :scale-x "scaleX"
   :scale-y "scaleY"
   :opacity "opacity"
   :r1 "borderRadiusTopLeft"
   :r2 "borderRadiusTopRight"
   :r3 "borderRadiusBottomRight"
   :r4 "borderRadiusBottomLeft"
   :fill-color "fillColor"
   :fill-opacity "fillOpacity"
   :stroke-color "strokeColor"
   :stroke-opacity "strokeOpacity"
   :stroke-width "strokeWidth"
   :shadow-offset-x "shadowOffsetX"
   :shadow-offset-y "shadowOffsetY"
   :shadow-blur "shadowBlur"
   :shadow-spread "shadowSpread"
   :shadow-color "shadowColor"
   :shadow-opacity "shadowOpacity"
   :blur "blur"
   :background-blur "backgroundBlur"
   :trim-start "trimStart"
   :trim-end "trimEnd"
   :trim-offset "trimOffset"})

(def ^:private properties
  (set/map-invert property-names))

(def ^:private playback-names
  #{"once" "loop" "ping-pong"})

(def ^:private style-names
  (into #{} (map name) (keys cta/animation-styles)))

(def ^:private export-qualities
  #{"low" "medium" "high"})

(def ^:private export-sizes
  #{"0.5" "1" "2" "w1920" "h720" "h1080" "h1440" "h2160"})

(def ^:private export-defaults
  "The export settings a board has until they are set, as the export
  panel offers them."
  {:format :mp4 :quality :high :fps 30 :size "1" :loop true})

(defn- format-export
  "The export settings of a timeline (`cta/schema:export`) as the plugin
  API gives them."
  [export]
  (let [export (merge export-defaults export)]
    #js {:format (name (:format export))
         :quality (name (:quality export))
         :fps (:fps export)
         :size (:size export)
         :loop (:loop export)}))

(defn- parse-export
  "The export settings the object `value` of `Timeline.exportSettings`
  sets, or a string saying what is wrong with them."
  [value]
  (let [file-format (obj/get value "format")
        quality     (obj/get value "quality")
        fps         (obj/get value "fps")
        size        (obj/get value "size")
        loops?      (obj/get value "loop")]
    (cond
      (and (some? file-format) (not (contains? cta/export-formats (keyword file-format))))
      (dm/str "The format should be one of " (pr-str (sort (map name cta/export-formats))))

      (and (some? quality) (not (contains? export-qualities quality)))
      "The quality should be 'low', 'medium' or 'high'"

      (and (some? fps) (not (and (sm/valid-safe-int? fps) (<= 1 fps 120))))
      "The frame rate should be a whole number from 1 to 120"

      (and (some? size) (not (contains? export-sizes size)))
      (dm/str "The size should be one of " (pr-str (sort export-sizes)))

      (and (some? loops?) (not (boolean? loops?)))
      "Whether it loops should be true or false"

      :else
      (cond-> {}
        (some? file-format) (assoc :format (keyword file-format))
        (some? quality)     (assoc :quality (keyword quality))
        (some? fps)         (assoc :fps fps)
        (some? size)        (assoc :size size)
        (some? loops?)      (assoc :loop loops?)))))

(defn- enabled?
  []
  (features/active-feature? @st/state "animation/v1"))

(defn- locate-timeline
  [file-id page-id board-id]
  (dm/get-in (u/locate-page file-id page-id) [:timelines board-id]))

(defn- locate-keyframe
  [file-id page-id board-id shape-id keyframe-id]
  (d/seek #(= keyframe-id (:id %))
          (dm/get-in (locate-timeline file-id page-id board-id) [:tracks shape-id :keyframes])))

(defn- locate-marker
  [file-id page-id board-id marker-id]
  (d/seek #(= marker-id (:id %))
          (:markers (locate-timeline file-id page-id board-id))))

(defn- locate-animation
  [file-id page-id board-id shape-id animation-id]
  (some-> (locate-timeline file-id page-id board-id)
          (cta/get-animation shape-id animation-id)))

(defn- valid-time?
  [value]
  (and (sm/valid-safe-number? value) (>= value 0)))

(defn- valid-value?
  [property value]
  (if (cta/color-property? property)
    (and (string? value) (sm/validate clr/schema:hex-color value))
    (sm/valid-safe-number? value)))

(defn format-easing
  "The easing of a keyframe or preset animation as the plugin API gives
  it: a preset name, `hold`, or a bezier or spring object."
  [{:keys [easing interpolation]}]
  (cond
    (= :step interpolation)    "hold"
    (nil? easing)              "linear"
    (keyword? easing)          (name easing)
    (= :bezier (:type easing)) #js {:type "bezier" :curve (into-array (:curve easing))}
    (= :spring (:type easing)) #js {:type "spring"
                                    :stiffness (:stiffness easing)
                                    :damping (:damping easing)
                                    :mass (:mass easing)}
    :else                      "linear"))

(defn parse-easing
  "The `{:easing}` (or, for `hold`, `{:interpolation :step}`) the easing
  `value` of the plugin API stands for, or nil when it is not one. Only a
  keyframe can hold its value (`hold?`)."
  [value hold?]
  (cond
    (= value "hold")
    (when hold? {:interpolation :step})

    (string? value)
    (let [easing (keyword value)]
      (when (contains? cti/easing-types easing)
        {:easing easing}))

    (object? value)
    (let [easing (case (obj/get value "type")
                   "bezier" {:type :bezier :curve (vec (obj/get value "curve"))}
                   "spring" {:type :spring
                             :stiffness (obj/get value "stiffness")
                             :damping (obj/get value "damping")
                             :mass (obj/get value "mass")}
                   nil)]
      (when (and (some? easing) (sm/validate cta/schema:easing easing))
        {:easing easing}))))

(defn- in-board?
  "Whether the shape `shape-id` is the board `board-id` or inside it, so
  its timeline animates it."
  [file-id page-id board-id shape-id]
  (let [objects (u/locate-objects file-id page-id)]
    (and (contains? objects shape-id)
         (or (= shape-id board-id)
             (= board-id (cfh/get-shape-id-root-frame objects shape-id))))))

(defn- write-error
  "Why the plugin may not change the timelines of the page `page-id`, or
  nil."
  [plugin-id page-id]
  (cond
    (not (enabled?))
    "Motion (the animation/v1 feature) is not enabled"

    (not (r/check-permission plugin-id "content:write"))
    "Plugin doesn't have 'content:write' permission"

    (not (u/page-active? page-id))
    "Cannot modify a page that is not currently active"))

(defn- write-timeline!
  "Change the timeline of the board `board-id` with `f` (a new one when it
  has none; nil removes it) and commit it. Nil, reported as `code`, when
  the plugin may not."
  [plugin-id file-id page-id board-id code f]
  (if-let [error (write-error plugin-id page-id)]
    (u/not-valid plugin-id code error)
    (let [page     (u/locate-page file-id page-id)
          board    (dm/get-in page [:objects board-id])
          timeline (or (dm/get-in page [:timelines board-id])
                       (cta/make-timeline {:board-id board-id :name (:name board)}))
          changes  (-> (pcb/empty-changes)
                       (pcb/with-page page)
                       (pcb/change-timeline board-id (f timeline)))]
      (when (seq (:redo-changes changes))
        (st/emit! (-> (dch/commit-changes changes)
                      (se/add-event plugin-id))))
      true)))

(defn- locked?
  [file-id page-id board-id shape-id {:keys [property index]}]
  (cta/slot-flag? (locate-timeline file-id page-id board-id) shape-id :locked property index))

(defn keyframe-proxy? [p]
  (obj/type-of? p "KeyframeProxy"))

(defn keyframe-proxy
  [plugin-id file-id page-id board-id shape-id id]
  (let [locate
        #(locate-keyframe file-id page-id board-id shape-id id)

        update!
        (fn update!
          ([code f]
           (update! code f identity))
          ([code f g]
           (if (locked? file-id page-id board-id shape-id (locate))
             (u/not-valid plugin-id code "The keyframe is locked")
             (write-timeline! plugin-id file-id page-id board-id code
                              #(-> % (cta/update-keyframe shape-id id f) g)))))]

    (obj/reify {:name "KeyframeProxy" :on-error (u/handle-error plugin-id)}
      :$plugin {:enumerable false :get (constantly plugin-id)}
      :$file {:enumerable false :get (constantly file-id)}
      :$page {:enumerable false :get (constantly page-id)}
      :$board {:enumerable false :get (constantly board-id)}
      :$shape {:enumerable false :get (constantly shape-id)}
      :$id {:enumerable false :get (constantly id)}

      :id
      {:get #(dm/str id)}

      :shape
      {:get #(shape-proxy plugin-id file-id page-id shape-id)}

      :property
      {:get #(some-> (locate) :property property-names)}

      :index
      {:get #(:index (locate))}

      :time
      {:get #(:time (locate))
       :set
       (fn [value]
         (if (not (valid-time? value))
           (u/not-valid plugin-id :time value)
           (let [time (mth/round value)]
             ;; the timeline grows to hold the keyframe
             (update! :time #(assoc % :time time) #(update % :duration max time)))))}

      :value
      {:get #(:value (locate))
       :set
       (fn [value]
         (let [property (:property (locate))]
           (if (not (valid-value? property value))
             (u/not-valid plugin-id :value value)
             (update! :value #(assoc % :value value)))))}

      :easing
      {:get #(some-> (locate) format-easing)
       :set
       (fn [value]
         (if-let [easing (parse-easing value true)]
           (update! :easing #(-> % (dissoc :easing :interpolation) (merge easing)))
           (u/not-valid plugin-id :easing value)))}

      :remove
      (fn []
        (if (locked? file-id page-id board-id shape-id (locate))
          (u/not-valid plugin-id :remove "The keyframe is locked")
          (write-timeline! plugin-id file-id page-id board-id :remove
                           #(cta/remove-keyframe % shape-id id)))))))

(defn- valid-name?
  [value]
  (and (string? value) (not (str/blank? value))))

(defn marker-proxy? [p]
  (obj/type-of? p "MarkerProxy"))

(defn marker-proxy
  [plugin-id file-id page-id board-id id]
  (let [locate
        #(locate-marker file-id page-id board-id id)

        update!
        (fn [code f]
          (write-timeline! plugin-id file-id page-id board-id code
                           #(cta/update-marker % id f)))]

    (obj/reify {:name "MarkerProxy" :on-error (u/handle-error plugin-id)}
      :$plugin {:enumerable false :get (constantly plugin-id)}
      :$file {:enumerable false :get (constantly file-id)}
      :$page {:enumerable false :get (constantly page-id)}
      :$board {:enumerable false :get (constantly board-id)}
      :$id {:enumerable false :get (constantly id)}

      :id
      {:get #(dm/str id)}

      :name
      {:get #(:name (locate))
       :set
       (fn [value]
         (if (not (valid-name? value))
           (u/not-valid plugin-id :name value)
           (update! :name #(assoc % :name value))))}

      :time
      {:get #(:time (locate))
       :set
       (fn [value]
         (if (not (valid-time? value))
           (u/not-valid plugin-id :time value)
           ;; the timeline grows to hold the marker
           (update! :time #(assoc % :time (mth/round value)))))}

      :remove
      (fn []
        (write-timeline! plugin-id file-id page-id board-id :remove
                         #(cta/remove-marker % id))))))

(defn preset-animation-proxy? [p]
  (obj/type-of? p "PresetAnimationProxy"))

(defn preset-animation-proxy
  [plugin-id file-id page-id board-id shape-id id]
  (let [locate
        #(locate-animation file-id page-id board-id shape-id id)

        update!
        (fn [code f]
          (if (:locked (locate))
            (u/not-valid plugin-id code "The animation is locked")
            (write-timeline! plugin-id file-id page-id board-id code
                             #(cta/update-animation % shape-id id f))))

        number-setter
        (fn [code attr valid?]
          (fn [value]
            (if (not (valid? value))
              (u/not-valid plugin-id code value)
              (update! code #(assoc % attr value)))))]

    (obj/reify {:name "PresetAnimationProxy" :on-error (u/handle-error plugin-id)}
      :$plugin {:enumerable false :get (constantly plugin-id)}
      :$file {:enumerable false :get (constantly file-id)}
      :$page {:enumerable false :get (constantly page-id)}
      :$board {:enumerable false :get (constantly board-id)}
      :$shape {:enumerable false :get (constantly shape-id)}
      :$id {:enumerable false :get (constantly id)}

      :id
      {:get #(dm/str id)}

      :shape
      {:get #(shape-proxy plugin-id file-id page-id shape-id)}

      :type
      {:get #(some-> (locate) :type name)}

      :direction
      {:get #(some-> (locate) :direction name)
       :set
       (fn [value]
         (if (not (contains? #{"in" "out"} value))
           (u/not-valid plugin-id :direction value)
           (update! :direction #(assoc % :direction (keyword value)))))}

      :start
      {:get #(:start (locate))
       :set (number-setter :start :start valid-time?)}

      :duration
      {:get #(:duration (locate))
       :set (number-setter :duration :duration #(and (sm/valid-safe-number? %) (pos? %)))}

      :easing
      {:get
       ;; without one of its own: entrances ease out, exits ease in
       #(when-let [animation (locate)]
          (if (some? (:easing animation))
            (format-easing animation)
            (if (= :out (:direction animation)) "ease-in" "ease-out")))
       :set
       (fn [value]
         (if-let [{:keys [easing]} (parse-easing value false)]
           (update! :easing #(assoc % :easing easing))
           (u/not-valid plugin-id :easing value)))}

      :amount
      {:get #(:amount (locate))
       :set (number-setter :amount :amount sm/valid-safe-number?)}

      :offsetX
      {:get #(:offset-x (locate))
       :set (number-setter :offsetX :offset-x sm/valid-safe-number?)}

      :offsetY
      {:get #(:offset-y (locate))
       :set (number-setter :offsetY :offset-y sm/valid-safe-number?)}

      :remove
      (fn []
        (if (:locked (locate))
          (u/not-valid plugin-id :remove "The animation is locked")
          (write-timeline! plugin-id file-id page-id board-id :remove
                           #(cta/remove-animation % shape-id id)))))))

(defn- parse-keyframe
  "The keyframe the object `props` of the plugin API describes, or a
  string saying what is wrong with it."
  [props]
  (let [property (get properties (obj/get props "property"))
        time     (obj/get props "time")
        value    (obj/get props "value")
        index    (obj/get props "index")
        easing   (parse-easing (d/nilv (obj/get props "easing") "ease") true)]
    (cond
      (nil? property)
      "Not a property that can be animated"

      (not (valid-time? time))
      "The time should be a number of milliseconds from 0"

      (not (valid-value? property value))
      (if (cta/color-property? property)
        "The value of a color should be an hexadecimal color like '#ff0000'"
        "The value should be a number")

      (and (some? index) (not (cta/indexed-property? property)))
      "Only fills, strokes and shadows take an index"

      (and (some? index) (not (and (sm/valid-safe-int? index) (>= index 0))))
      "The index should be a number from 0"

      (nil? easing)
      "Not a valid easing"

      :else
      (merge {:property property
              :time (mth/round time)
              :value value}
             (when (cta/indexed-property? property)
               {:index (or index 0)})
             easing))))

(defn- parse-animation
  "The preset animation the object `props` of the plugin API describes,
  or a string saying what is wrong with it."
  [props]
  (let [type      (some-> (obj/get props "type") keyword)
        direction (d/nilv (obj/get props "direction") "in")
        start     (d/nilv (obj/get props "start") 0)
        duration  (obj/get props "duration")
        easing    (some-> (obj/get props "easing") (parse-easing false))
        numbers   (->> ["amount" "offsetX" "offsetY"]
                       (keep #(when-let [v (obj/get props %)] [% v]))
                       (into {}))]
    (cond
      (not (contains? cta/animation-types type))
      "The type should be one of 'fade', 'move', 'scale' or 'rotate'"

      (not (contains? #{"in" "out"} direction))
      "The direction should be 'in' or 'out'"

      (not (valid-time? start))
      "The start should be a number of milliseconds from 0"

      (and (some? duration) (not (and (sm/valid-safe-number? duration) (pos? duration))))
      "The duration should be a number of milliseconds above 0"

      (and (some? (obj/get props "easing")) (nil? easing))
      "Not a valid easing"

      (not (every? sm/valid-safe-number? (vals numbers)))
      "The amount and offsets should be numbers"

      :else
      (cond-> {:type type
               :direction (keyword direction)
               :start (mth/round start)}
        (some? duration)              (assoc :duration (mth/round duration))
        (some? easing)                (merge easing)
        (contains? numbers "amount")  (assoc :amount (get numbers "amount"))
        (contains? numbers "offsetX") (assoc :offset-x (get numbers "offsetX"))
        (contains? numbers "offsetY") (assoc :offset-y (get numbers "offsetY"))))))

(defn- track-items
  "`[shape-id item]` of the keyframes (`:keyframes`) or preset animations
  (`:animations`) of every layer of `timeline`, by time."
  [timeline k]
  (->> (:tracks timeline)
       (mapcat (fn [[shape-id track]]
                 (map #(vector shape-id %) (get track k))))
       (sort-by (fn [[_ item]] (or (:time item) (:start item))))))

(defn timeline-proxy? [p]
  (obj/type-of? p "TimelineProxy"))

(defn timeline-proxy
  [plugin-id file-id page-id board-id]
  (let [locate
        #(locate-timeline file-id page-id board-id)

        write!
        (fn [code f]
          (write-timeline! plugin-id file-id page-id board-id code f))

        layer
        (fn [code shape]
          (let [shape-id (when (shape-proxy? shape) (obj/get shape "$id"))]
            (cond
              (nil? shape-id)
              (u/not-valid plugin-id code "Not a shape")

              (not (in-board? file-id page-id board-id shape-id))
              (u/not-valid plugin-id code "The shape is not the board nor inside it")

              :else
              shape-id)))]

    (obj/reify {:name "TimelineProxy" :on-error (u/handle-error plugin-id)}
      :$plugin {:enumerable false :get (constantly plugin-id)}
      :$file {:enumerable false :get (constantly file-id)}
      :$page {:enumerable false :get (constantly page-id)}
      :$id {:enumerable false :get (constantly board-id)}

      :board
      {:get #(shape-proxy plugin-id file-id page-id board-id)}

      :name
      {:get #(:name (locate))
       :set
       (fn [value]
         (if (not (string? value))
           (u/not-valid plugin-id :name value)
           (write! :name #(assoc % :name value))))}

      :duration
      {:get #(:duration (locate))
       :set
       (fn [value]
         (if (not (and (sm/valid-safe-number? value) (pos? value)))
           (u/not-valid plugin-id :duration value)
           (write! :duration #(assoc % :duration (mth/round value)))))}

      :playback
      {:get #(some-> (locate) cta/playback-mode name)
       :set
       (fn [value]
         (if (not (contains? playback-names value))
           (u/not-valid plugin-id :playback value)
           (write! :playback #(-> % (assoc :playback (keyword value)) (dissoc :loop)))))}

      :keyframes
      {:get
       #(->> (track-items (locate) :keyframes)
             (format/format-array
              (fn [[shape-id {:keys [id]}]]
                (keyframe-proxy plugin-id file-id page-id board-id shape-id id))))}

      :animations
      {:get
       #(->> (track-items (locate) :animations)
             (format/format-array
              (fn [[shape-id {:keys [id]}]]
                (preset-animation-proxy plugin-id file-id page-id board-id shape-id id))))}

      :addKeyframe
      (fn [shape props]
        (when-let [shape-id (layer :addKeyframe shape)]
          (let [keyframe (if (object? props) (parse-keyframe props) "No keyframe given")]
            (cond
              (string? keyframe)
              (u/not-valid plugin-id :addKeyframe keyframe)

              (cta/slot-flag? (locate) shape-id :locked (:property keyframe) (:index keyframe))
              (u/not-valid plugin-id :addKeyframe "The property is locked")

              :else
              (let [id (uuid/next)]
                (when (write! :addKeyframe
                              #(-> %
                                   (cta/add-keyframe shape-id (assoc keyframe :id id))
                                   (update :duration max (:time keyframe))))
                  (keyframe-proxy plugin-id file-id page-id board-id shape-id id)))))))

      :addAnimation
      (fn [shape props]
        (when-let [shape-id (layer :addAnimation shape)]
          (let [animation (if (object? props) (parse-animation props) "No animation given")]
            (if (string? animation)
              (u/not-valid plugin-id :addAnimation animation)
              (let [id (uuid/next)]
                (when (write! :addAnimation #(cta/add-animation % shape-id (assoc animation :id id)))
                  (preset-animation-proxy plugin-id file-id page-id board-id shape-id id)))))))

      :addAnimationStyle
      (fn [shape style start]
        (when-let [shape-id (layer :addAnimationStyle shape)]
          (let [start (d/nilv start 0)]
            (cond
              (not (contains? style-names style))
              (u/not-valid plugin-id :addAnimationStyle
                           (dm/str "The style should be one of " (pr-str (sort style-names))))

              (not (valid-time? start))
              (u/not-valid plugin-id :addAnimationStyle "The start should be a number of milliseconds from 0")

              :else
              (let [animations (mapv #(assoc % :id (uuid/next) :start (mth/round start))
                                     (get cta/animation-styles (keyword style)))]
                (when (write! :addAnimationStyle
                              #(reduce (fn [timeline animation]
                                         (cta/add-animation timeline shape-id animation))
                                       %
                                       animations))
                  (format/format-array
                   #(preset-animation-proxy plugin-id file-id page-id board-id shape-id (:id %))
                   animations)))))))

      :markers
      {:get
       #(format/format-array
         (fn [{:keys [id]}]
           (marker-proxy plugin-id file-id page-id board-id id))
         (:markers (locate)))}

      ;; As the export panel sets them and offers them again; a partial
      ;; object changes only what it has.
      :exportSettings
      {:get #(format-export (:export (locate)))
       :set
       (fn [value]
         (let [export (if (object? value) (parse-export value) "No settings given")]
           (if (string? export)
             (u/not-valid plugin-id :exportSettings export)
             (write! :exportSettings #(update % :export merge export)))))}

      :addMarker
      (fn [props]
        (let [time (when (object? props) (obj/get props "time"))
              name (when (object? props) (obj/get props "name"))]
          (cond
            (not (valid-time? time))
            (u/not-valid plugin-id :addMarker "The time should be a number of milliseconds from 0")

            (and (some? name) (not (valid-name? name)))
            (u/not-valid plugin-id :addMarker "The name should be a text")

            :else
            (let [id (uuid/next)]
              (when (write! :addMarker #(cta/add-marker % {:id id :time (mth/round time) :name name}))
                (marker-proxy plugin-id file-id page-id board-id id))))))

      ;; As motion mode with auto-keyframe on: `callback` changes `shapes`,
      ;; and the changes are recorded as keyframes at `time`. Only those
      ;; of `shapes`: changing a group changes its layers too.
      :record
      (fn [time shapes callback]
        (let [ids (when (array? shapes)
                    (mapv #(when (shape-proxy? %) (obj/get % "$id")) shapes))]
          (cond
            (not (valid-time? time))
            (u/not-valid plugin-id :record "The time should be a number of milliseconds from 0")

            (or (nil? ids) (some nil? ids))
            (u/not-valid plugin-id :record "The shapes should be an array of shapes")

            (not-every? #(in-board? file-id page-id board-id %) ids)
            (u/not-valid plugin-id :record "A shape is not the board nor inside it")

            (not (fn? callback))
            (u/not-valid plugin-id :record "The callback should be a function")

            ;; before the callback changes anything
            (some? (write-error plugin-id page-id))
            (u/not-valid plugin-id :record (write-error plugin-id page-id))

            :else
            (let [before (u/locate-objects file-id page-id)
                  _      (callback)
                  after  (merge before (select-keys (u/locate-objects file-id page-id) ids))
                  time   (mth/round time)]
              (write! :record #(-> %
                                   (cta/record-edit before after time)
                                   (cta/record-unanimated before after ids time)))
              nil))))

      :valueAt
      (fn [shape property time index]
        (when-let [shape-id (layer :valueAt shape)]
          (let [property (get properties property)]
            (cond
              (nil? property)
              (u/not-valid plugin-id :valueAt "Not a property that can be animated")

              (not (valid-time? time))
              (u/not-valid plugin-id :valueAt "The time should be a number of milliseconds from 0")

              :else
              (let [objects (u/locate-objects file-id page-id)
                    values  (-> (cta/resolve-animations (locate) objects)
                                (cta/values-at time)
                                (get shape-id))]
                (cta/value-of values property (when (cta/indexed-property? property)
                                                (d/nilv index 0))))))))

      :clear
      (fn [shape]
        (when-let [shape-id (layer :clear shape)]
          (write! :clear #(cta/remove-track % shape-id))))

      ;; With the animations of the component copies in the board, as the
      ;; exports of the app (see `dwa/board-export-timeline`).
      :toCSS
      (fn []
        (when (some? (locate))
          (let [page (u/locate-page file-id page-id)]
            (some-> (dwa/board-export-timeline page (:files @st/state) board-id)
                    (cta/timeline->css (:objects page))))))

      :remove
      (fn []
        (write! :remove (constantly nil))))))

(defn animated-copies
  "The component copies in the board `board-id` that play the animation
  of their mains, as `Board.animatedCopies` gives them: the copy, how long
  a round of it takes and whether it plays once or loops (see
  `cltl/copy-timelines`)."
  [plugin-id file-id page-id board-id]
  (let [page   (u/locate-page file-id page-id)
        copies (cltl/copy-timelines (:objects page) (:timelines page) (:files @st/state) board-id)]
    (->> (group-by (fn [[[_ copy-id]]] copy-id) copies)
         (sort-by key)
         (format/format-array
          (fn [[copy-id entries]]
            (let [timelines (map val entries)]
              #js {:copy (shape-proxy plugin-id file-id page-id copy-id)
                   :duration (reduce max 0 (map cta/cycle-duration timelines))
                   :playback (if (every? #(= :once (cta/playback-mode %)) timelines) "once" "loop")}))))))

(defn timeline-options
  "The settings the object `options` of `Board.addTimeline` gives a
  timeline, or a string saying what is wrong with them."
  [options]
  (let [name     (obj/get options "name")
        duration (obj/get options "duration")
        playback (obj/get options "playback")]
    (cond
      (and (some? name) (not (string? name)))
      "The name should be a string"

      (and (some? duration) (not (and (sm/valid-safe-number? duration) (pos? duration))))
      "The duration should be a number of milliseconds above 0"

      (and (some? playback) (not (contains? playback-names playback)))
      "The playback should be 'once', 'loop' or 'ping-pong'"

      :else
      (cond-> {}
        (some? name)     (assoc :name name)
        (some? duration) (assoc :duration (mth/round duration))
        (some? playback) (assoc :playback (keyword playback))))))

(defn add-timeline!
  "Give the board `board-id` a timeline, or the one it has the settings
  of `options`: the proxy of it, or nil."
  [plugin-id file-id page-id board-id options]
  (let [settings (if (some? options) (timeline-options options) {})]
    (if (string? settings)
      (u/not-valid plugin-id :addTimeline settings)
      (when (write-timeline! plugin-id file-id page-id board-id :addTimeline
                             #(cond-> (merge % settings)
                                (contains? settings :playback) (dissoc :loop)))
        (timeline-proxy plugin-id file-id page-id board-id)))))
