;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.plugins.motion
  "The keyframe animation of boards (Penpot Motion) in the plugin API: the
  timeline of a board, the keyframes of its layers, their preset
  animations, their uses of animation styles and the audio clips of the
  board (see `app.common.types.animation`). Like the other proxies they
  keep ids only and read the timeline of the page again each time.
  Changing an animation needs the `animation/v1` feature."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.media :as cm]
   [app.common.schema :as sm]
   [app.common.types.animation :as cta]
   [app.common.types.animation-code :as ctac]
   [app.common.types.animation-style :as ctas]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.color :as clr]
   [app.common.types.shape.interactions :as cti]
   [app.common.types.text-units :as tu]
   [app.common.types.token :as cto]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.workspace.audio :as dwaa]
   [app.main.data.workspace.tokens.propagation :as dwtp]
   [app.main.features :as features]
   [app.main.store :as st]
   [app.plugins.format :as format]
   [app.plugins.register :as r]
   [app.plugins.system-events :as se]
   [app.plugins.utils :as u]
   [app.util.audio :as audio]
   [app.util.code-gen.motion :as cgm]
   [app.util.object :as obj]
   [beicon.v2.core :as rx]
   [clojure.set :as set]
   [cuerdas.core :as str]
   [promesa.core :as p]))

;; Set in `app.plugins`: the shapes need the timelines and the other way
;; round.
(def shape-proxy identity)
(def shape-proxy? identity)

;; Set in `app.plugins` too: the proxy of an animation style of a library
;; (see `app.plugins.library`), which parses its entries here.
(def lib-animation-style-proxy identity)

(defn- lib-animation-style-proxy?
  [p]
  (obj/type-of? p "LibraryAnimationStyleProxy"))

(def property-names
  "The animatable properties as the plugin API names them, camelCase like
  the rest of it (see `ctac/property-names`, which the code of the
  Inspect shares)."
  ctac/property-names)

(def ^:private properties
  (set/map-invert property-names))

(def ^:private playback-names
  #{"once" "loop" "ping-pong"})

(def ^:private style-names
  (into #{} (map name) (keys cta/builtin-styles)))

(def ^:private export-qualities
  #{"low" "medium" "high"})

(def ^:private export-defaults
  "The export settings a board has until they are set, as the export
  panel offers them."
  {:format :mp4 :quality :high :fps 30 :size "1" :loop true :audio true :ids false :bbox false
   :outline false})

(defn- format-export
  "The export settings of a timeline (`cta/schema:export`) as the plugin
  API gives them."
  [export]
  (let [export (merge export-defaults export)]
    #js {:format (name (:format export))
         :quality (name (:quality export))
         :fps (:fps export)
         :size (:size export)
         :loop (:loop export)
         :audio (:audio export)
         :includeIds (:ids export)
         :includeBoundingBox (:bbox export)
         :outlineText (:outline export)}))

(defn- parse-export
  "The export settings the object `value` of `Timeline.exportSettings`
  sets, or a string saying what is wrong with them."
  [value]
  (let [file-format (obj/get value "format")
        quality     (obj/get value "quality")
        fps         (obj/get value "fps")
        size        (obj/get value "size")
        loops?      (obj/get value "loop")
        audio?      (obj/get value "audio")
        ids?        (obj/get value "includeIds")
        bbox?       (obj/get value "includeBoundingBox")
        outline?    (obj/get value "outlineText")]
    (cond
      (and (some? file-format) (not (contains? cta/export-formats (keyword file-format))))
      (dm/str "The format should be one of " (pr-str (sort (map name cta/export-formats))))

      (and (some? quality) (not (contains? export-qualities quality)))
      "The quality should be 'low', 'medium' or 'high'"

      (and (some? fps) (not (and (sm/valid-safe-int? fps) (<= 1 fps 120))))
      "The frame rate should be a whole number from 1 to 120"

      (and (some? size) (nil? (cta/parse-export-size size)))
      (dm/str "The size should be a factor of the board up to " cta/max-export-size-factor
              " ('0.5', '2', '3.5') or a width or height in pixels up to "
              cta/max-export-size-pixels " ('w1920', 'h720')")

      (and (some? loops?) (not (boolean? loops?)))
      "Whether it loops should be true or false"

      (and (some? audio?) (not (boolean? audio?)))
      "Whether it has sound should be true or false"

      (and (some? ids?) (not (boolean? ids?)))
      "Whether an SVG has the ids of the layers should be true or false"

      (and (some? bbox?) (not (boolean? bbox?)))
      "Whether an SVG has a bounding box should be true or false"

      (and (some? outline?) (not (boolean? outline?)))
      "Whether an SVG has its texts as outlines should be true or false"

      :else
      (cond-> {}
        (some? file-format) (assoc :format (keyword file-format))
        (some? quality)     (assoc :quality (keyword quality))
        (some? fps)         (assoc :fps fps)
        (some? size)        (assoc :size (cta/parse-export-size size))
        (some? loops?)      (assoc :loop loops?)
        (some? audio?)      (assoc :audio audio?)
        (some? ids?)        (assoc :ids ids?)
        (some? bbox?)       (assoc :bbox bbox?)
        (some? outline?)    (assoc :outline outline?)))))

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

(defn- locate-clip
  [file-id page-id board-id clip-id]
  (some-> (locate-timeline file-id page-id board-id)
          (ctau/get-audio-clip clip-id)))

(defn- locate-animation
  [file-id page-id board-id shape-id animation-id]
  (some-> (locate-timeline file-id page-id board-id)
          (cta/get-animation shape-id animation-id)))

(defn- valid-time?
  [value]
  (and (sm/valid-safe-number? value) (>= value 0)))

(defn- valid-timeline-time?
  "A time on a timeline, in ms: before 0 too, where nothing plays, as the
  timeline dock allows."
  [value]
  (sm/valid-safe-number? value))

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

(defn- split-error
  "Why the layer `shape-id` cannot play an animation by `kind` of units
  (see `cta/unit-split-kind`), the animation `animation-id` of it aside,
  or nil: only a text splits, and a text plays one kind, its split
  animations and the split entries of its style uses alike."
  [file-id page-id timeline shape-id kind animation-id]
  (when (some? kind)
    (let [shape (get (u/locate-objects file-id page-id) shape-id)
          other (->> (cta/track-animations (cta/get-track timeline shape-id))
                     (remove #(= animation-id (:id %)))
                     (keep :split)
                     (first))]
      (cond
        (not (cfh/text-shape? shape))
        "Only a text plays its characters, words or lines one by one"

        (and (some? other) (not= other kind))
        (dm/str "The text already plays its units by '" (name other) "': a text plays one kind")))))

(defn- parse-split
  "The kind of units the plugin API names `value` (nil for none), or
  `::invalid`."
  [value]
  (cond
    (nil? value)                                                 nil
    (and (string? value) (contains? tu/split-kinds (keyword value))) (keyword value)
    :else                                                        ::invalid))

(defn- valid-spread?
  [value]
  (and (sm/valid-safe-number? value) (<= 0 value 1)))

(defn- valid-order?
  [value]
  (and (string? value) (contains? tu/orders (keyword value))))

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

(defn- paste-animation!
  "Paste the animation of the layer `shape-id` onto the layers
  `target-ids`, in that order, of the page `page-id`, in the boards they
  are in, and commit it, as pasting a copied animation does (see
  `cltl/paste-animation` for `opts`): `{:targets :skipped :pivot-kept}`.
  Nil, reported as `code`, when the plugin may not, the layer has nothing
  to copy or it would paste too much."
  [plugin-id file-id page-id code shape-id target-ids {:keys [properties presets?] :as opts}]
  (if-let [error (write-error plugin-id page-id)]
    (u/not-valid plugin-id code error)
    (let [page  (u/locate-page file-id page-id)
          clip  (cltl/animation-clip file-id page (:objects page) (:files @st/state) [shape-id]
                                     :properties properties
                                     :presets? presets?
                                     :whole-layer? true)
          layer (first (:layers clip))]
      (if (nil? layer)
        (u/not-valid plugin-id code "The shape has no animation to copy")
        (let [pairs  (mapv #(hash-map :layer layer :target-id %) target-ids)
              result (cltl/generate-paste-animation (pcb/empty-changes) file-id page clip pairs
                                                    (assoc opts :skip-source? true))]
          (if (some? (:error result))
            (u/not-valid plugin-id code
                         (dm/str "Too much to paste: at most " cltl/max-paste-targets
                                 " layers and " cltl/max-paste-keyframes " keyframes in all"))
            (do
              (when (seq (:redo-changes (:changes result)))
                (st/emit! (-> (dch/commit-changes (:changes result))
                              (se/add-event plugin-id))))
              result)))))))

(defn- locked?
  [file-id page-id board-id shape-id {:keys [property index]}]
  (cta/slot-flag? (locate-timeline file-id page-id board-id) shape-id :locked property index))

(def ^:private style-token-settings
  "The settings of a style entry a design token sets, by the names the
  plugin API gives them, those of the code too (see
  `cta/schema:animation-tokens`)."
  ctac/token-setting-names)

(defn- parse-entry-tokens
  "The `:applied-tokens` of a style entry of the preset type `type` that
  the object `tokens` of the plugin API (settings to token names)
  describes, nil without it, or a string saying what is wrong with it."
  [tokens type]
  (when (some? tokens)
    (if-not (object? tokens)
      "The applied tokens should be an object of setting names to token names"
      (let [by-name (set/map-invert style-token-settings)
            pairs   (mapv (fn [setting] [setting (obj/get tokens setting)]) (js-keys tokens))]
        (or (some (fn [[setting token-name]]
                    (let [key (get by-name setting)]
                      (cond
                        (nil? key)
                        (dm/str "No token sets '" setting "'")

                        (not (sm/validate cto/schema:token-name token-name))
                        (dm/str "The token of '" setting "' should be a token name")

                        (empty? (cta/animation-token-types type key))
                        (dm/str "No token sets '" setting "' of a " (name type) " animation"))))
                  pairs)
            (into {} (map (fn [[setting token-name]] [(get by-name setting) token-name])) pairs))))))

(defn- format-applied-tokens
  "The design tokens `applied-tokens` (setting to name) as the plugin API
  gives them: an object of the plugin names of the settings."
  [applied-tokens]
  (reduce-kv (fn [tokens key token-name]
               (obj/set! tokens (get style-token-settings key) token-name))
             #js {}
             applied-tokens))

(defn- parse-keyframe-tokens
  "The `:applied-tokens` of a keyframe that the object `tokens` of the
  plugin API describes (only its `easing`), nil without it (or an empty
  one), or a string saying what is wrong with it."
  [tokens]
  (when (some? tokens)
    (cond
      (not (object? tokens))
      "The applied tokens should be an object of setting names to token names"

      (empty? (js-keys tokens))
      nil

      (some #(not= "easing" %) (js-keys tokens))
      "A keyframe binds its easing only"

      (not (sm/validate cto/schema:token-name (obj/get tokens "easing")))
      "The token of 'easing' should be a token name"

      :else
      {:easing (obj/get tokens "easing")})))

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

      ;; not enumerable: the shape leads back here through its board's
      ;; timeline, and serializing it would never end
      :shape
      {:enumerable false
       :get #(shape-proxy plugin-id file-id page-id shape-id)}

      :property
      {:get #(some-> (locate) :property property-names)}

      :index
      {:get #(:index (locate))}

      :time
      {:get #(:time (locate))
       :set
       (fn [value]
         (if (not (valid-timeline-time? value))
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
         ;; set by hand, no token sets it
         (if-let [easing (parse-easing value true)]
           (update! :easing #(-> % (dissoc :easing :interpolation :applied-tokens) (merge easing)))
           (u/not-valid plugin-id :easing value)))}

      :appliedTokens
      {:get #(some-> (locate) :applied-tokens format-applied-tokens)
       :set
       (fn [value]
         (let [tokens (parse-keyframe-tokens value)]
           (if (string? tokens)
             (u/not-valid plugin-id :appliedTokens tokens)
             (when (update! :appliedTokens #(cond-> (dissoc % :applied-tokens)
                                              (some? tokens) (assoc :applied-tokens tokens)))
               (when (some? tokens)
                 (st/emit! (dwtp/propagate-style-tokens)))))))}

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

(defn audio-clip-proxy? [p]
  (obj/type-of? p "AudioClipProxy"))

(defn- valid-lane?
  [value]
  (and (sm/valid-safe-int? value) (>= value 0)))

(defn audio-clip-proxy
  [plugin-id file-id page-id board-id id]
  (let [locate
        #(locate-clip file-id page-id board-id id)

        ;; A locked clip takes no write but its unlocking.
        update!
        (fn [code f]
          (if (:locked (locate))
            (u/not-valid plugin-id code "The clip is locked")
            (write-timeline! plugin-id file-id page-id board-id code f)))]

    (obj/reify {:name "AudioClipProxy" :on-error (u/handle-error plugin-id)}
      :$plugin {:enumerable false :get (constantly plugin-id)}
      :$file {:enumerable false :get (constantly file-id)}
      :$page {:enumerable false :get (constantly page-id)}
      :$board {:enumerable false :get (constantly board-id)}
      :$id {:enumerable false :get (constantly id)}

      :id
      {:get #(dm/str id)}

      :name
      {:get #(:name (locate))}

      :sourceDuration
      {:get #(:source-duration (locate))}

      :start
      {:get #(:start (locate))
       :set
       (fn [value]
         (if (not (valid-timeline-time? value))
           (u/not-valid plugin-id :start value)
           (update! :start #(ctau/move-audio-clip % id (mth/round value)))))}

      :offset
      {:get #(:offset (locate))
       :set
       (fn [value]
         (if (not (valid-time? value))
           (u/not-valid plugin-id :offset value)
           (update! :offset #(ctau/set-audio-clip-offset % id (mth/round value)))))}

      :duration
      {:get #(:duration (locate))
       :set
       (fn [value]
         (if (not (and (sm/valid-safe-number? value) (pos? value)))
           (u/not-valid plugin-id :duration value)
           (update! :duration
                    (fn [timeline]
                      (let [start (:start (ctau/get-audio-clip timeline id))]
                        (ctau/trim-audio-clip-end timeline id (+ start (mth/round value))))))))}

      :lane
      {:get #(:lane (locate))
       :set
       (fn [value]
         (cond
           (not (valid-lane? value))
           (u/not-valid plugin-id :lane value)

           ;; a lane plays a single file
           (not (ctau/lane-takes? (locate-timeline file-id page-id board-id) value
                                  (:audio-id (locate)) :except-ids #{id}))
           (u/not-valid plugin-id :lane "The lane plays another audio file")

           :else
           (update! :lane
                    (fn [timeline]
                      (let [start (:start (ctau/get-audio-clip timeline id))]
                        (ctau/move-audio-clip timeline id start :lane value))))))}

      :speed
      {:get #(some-> (locate) ctau/clip-speed)
       :set
       (fn [value]
         (if (not (and (sm/valid-safe-number? value) (<= ctau/min-speed value ctau/max-speed)))
           (u/not-valid plugin-id :speed value)
           (update! :speed #(ctau/set-audio-clip-speed % id value))))}

      :volume
      {:get #(or (:volume (locate)) 1)
       :set
       (fn [value]
         (if (not (and (sm/valid-safe-number? value) (<= 0 value ctau/max-volume)))
           (u/not-valid plugin-id :volume value)
           (update! :volume #(ctau/set-audio-clip-volume % id value))))}

      :muted
      {:get #(true? (:muted (locate)))
       :set
       (fn [value]
         (if (not (boolean? value))
           (u/not-valid plugin-id :muted value)
           (update! :muted #(ctau/set-audio-clip-muted % id value))))}

      :fadeIn
      {:get #(or (:fade-in (locate)) 0)
       :set
       (fn [value]
         (if (not (valid-time? value))
           (u/not-valid plugin-id :fadeIn value)
           (update! :fadeIn
                    (fn [timeline]
                      (let [fade-out (:fade-out (ctau/get-audio-clip timeline id))]
                        (ctau/set-audio-clip-fades timeline id (mth/round value) fade-out))))))}

      :fadeOut
      {:get #(or (:fade-out (locate)) 0)
       :set
       (fn [value]
         (if (not (valid-time? value))
           (u/not-valid plugin-id :fadeOut value)
           (update! :fadeOut
                    (fn [timeline]
                      (let [fade-in (:fade-in (ctau/get-audio-clip timeline id))]
                        (ctau/set-audio-clip-fades timeline id fade-in (mth/round value)))))))}

      :locked
      {:get #(true? (:locked (locate)))
       :set
       (fn [value]
         (if (not (boolean? value))
           (u/not-valid plugin-id :locked value)
           (write-timeline! plugin-id file-id page-id board-id :locked
                            #(ctau/set-audio-clip-locked % id value))))}

      ;; As S in the timeline dock, at a time of the timeline.
      :split
      (fn [time]
        (if (not (valid-time? time))
          (u/not-valid plugin-id :split "The time should be a number of milliseconds from 0")
          (let [new-id (uuid/next)]
            (when (update! :split #(ctau/split-audio-clip % id (mth/round time) new-id))
              (when (some? (locate-clip file-id page-id board-id new-id))
                (audio-clip-proxy plugin-id file-id page-id board-id new-id))))))

      :remove
      (fn []
        (update! :remove #(ctau/remove-audio-clip % id))))))

(defn- audio-bytes
  "The bytes of the `data` a plugin gives as an audio file: an
  `Uint8Array`, an `ArrayBuffer` or an array of numbers; nil for anything
  else."
  [data]
  (cond
    (instance? js/Uint8Array data)  data
    (instance? js/ArrayBuffer data) (js/Uint8Array. data)
    (array? data)                   (js/Uint8Array.from data)
    :else                           nil))

(defn- add-audio-clip!
  "A promise of the proxy of a clip of the board `board-id` playing the
  audio file `name` of `bytes` of the type `mtype`, from `start` ms on
  `lane` (on a free one when nil): the file decoded first, as a drop in
  the workspace does, then uploaded and the clip added. It rejects what
  does not decode or lasts longer than `ctau/max-audio-duration` before
  any upload, and a board that lost its timeline, or a page left, by the
  time the upload is done."
  [plugin-id file-id page-id board-id name bytes mtype start lane]
  (js/Promise.
   (fn [resolve reject]
     (let [not-valid #(u/reject-not-valid reject :addAudioClip %)

           add!
           (fn [{:keys [id]} {:keys [buffer duration]}]
             (audio/remember! id buffer)
             (if-let [error (or (write-error plugin-id page-id)
                                (when (nil? (locate-timeline file-id page-id board-id))
                                  "The board has no timeline"))]
               (not-valid error)
               (let [clip-id (uuid/next)
                     clip    (cond-> {:id clip-id
                                      :audio-id id
                                      :name name
                                      :source-duration duration
                                      :start start
                                      :offset 0
                                      :duration duration}
                               (some? lane) (assoc :lane lane))]
                 (write-timeline! plugin-id file-id page-id board-id :addAudioClip
                                  #(ctau/add-audio-clip % clip))
                 (resolve (audio-clip-proxy plugin-id file-id page-id board-id clip-id)))))]

       (-> (dwaa/prepare (js/File. #js [bytes] name #js {:type mtype}) audio/decode)
           (p/then
            (fn [{:keys [error] :as item}]
              (case error
                :unsupported (not-valid "The type is not of audio a clip plays")
                :undecodable (not-valid "The audio does not decode in this browser")
                :too-long    (not-valid (dm/str "The audio lasts longer than "
                                                (/ ctau/max-audio-duration 60000) " minutes"))
                (->> (dwaa/upload file-id item)
                     (rx/take 1)
                     (rx/subs!
                      (fn [media]
                        (try
                          (add! media item)
                          (catch :default cause
                            (reject cause))))
                      (fn [cause]
                        (if (= :media-type-not-allowed (:code (ex-data cause)))
                          (not-valid "The server takes no audio: Motion is not enabled on it")
                          (reject cause))))))))
           (p/catch reject))))))

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

        ;; set by hand, no token sets it (see `cta/set-by-hand`)
        number-setter
        (fn [code attr valid?]
          (fn [value]
            (if (not (valid? value))
              (u/not-valid plugin-id code value)
              (update! code #(cta/set-by-hand % {attr value})))))

        ;; the settings of a split animation
        split-setter
        (fn [code attr valid? parse]
          (fn [value]
            (cond
              (nil? (:split (locate)))
              (u/not-valid plugin-id code "Only an animation with a split has it")

              (not (valid? value))
              (u/not-valid plugin-id code value)

              :else
              (update! code #(assoc % attr (parse value))))))]

    (obj/reify {:name "PresetAnimationProxy" :on-error (u/handle-error plugin-id)}
      :$plugin {:enumerable false :get (constantly plugin-id)}
      :$file {:enumerable false :get (constantly file-id)}
      :$page {:enumerable false :get (constantly page-id)}
      :$board {:enumerable false :get (constantly board-id)}
      :$shape {:enumerable false :get (constantly shape-id)}
      :$id {:enumerable false :get (constantly id)}

      :id
      {:get #(dm/str id)}

      ;; not enumerable: the shape leads back here through its board's
      ;; timeline, and serializing it would never end
      :shape
      {:enumerable false
       :get #(shape-proxy plugin-id file-id page-id shape-id)}

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
       :set (number-setter :start :start valid-timeline-time?)}

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
           (update! :easing #(cta/set-by-hand % {:easing easing}))
           (u/not-valid plugin-id :easing value)))}

      :appliedTokens
      {:get #(some-> (locate) :applied-tokens format-applied-tokens)
       :set
       (fn [value]
         (let [tokens (if (nil? value) nil (parse-entry-tokens value (:type (locate))))]
           (if (string? tokens)
             (u/not-valid plugin-id :appliedTokens tokens)
             (when (update! :appliedTokens #(cond-> (dissoc % :applied-tokens)
                                              (seq tokens) (assoc :applied-tokens tokens)))
               (when (seq tokens)
                 (st/emit! (dwtp/propagate-style-tokens)))))))}

      :amount
      {:get #(:amount (locate))
       :set (number-setter :amount :amount sm/valid-safe-number?)}

      :offsetX
      {:get #(:offset-x (locate))
       :set (number-setter :offsetX :offset-x sm/valid-safe-number?)}

      :offsetY
      {:get #(:offset-y (locate))
       :set (number-setter :offsetY :offset-y sm/valid-safe-number?)}

      :split
      {:get #(some-> (locate) :split name)
       :set
       (fn [value]
         (let [kind (parse-split value)]
           (if (= ::invalid kind)
             (u/not-valid plugin-id :split value)
             (if-let [error (split-error file-id page-id (locate-timeline file-id page-id board-id)
                                         shape-id kind id)]
               (u/not-valid plugin-id :split error)
               (update! :split #(assoc % :split kind))))))}

      :spread
      {:get #(:spread (locate))
       :set (split-setter :spread :spread valid-spread? identity)}

      :order
      {:get #(some-> (locate) :order name)
       :set (split-setter :order :order valid-order? keyword)}

      :seed
      {:get #(:seed (locate))
       :set (split-setter :seed :seed sm/valid-safe-int? identity)}

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
        easing   (parse-easing (d/nilv (obj/get props "easing") "ease") true)
        tokens   (parse-keyframe-tokens (obj/get props "appliedTokens"))]
    (cond
      (nil? property)
      "Not a property that can be animated"

      (not (valid-timeline-time? time))
      "The time should be a number of milliseconds"

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

      (string? tokens)
      tokens

      :else
      (merge {:property property
              :time (mth/round time)
              :value value}
             (when (cta/indexed-property? property)
               {:index (or index 0)})
             easing
             (when (some? tokens)
               {:applied-tokens tokens})))))

(defn- parse-animation
  "The preset animation the object `props` of the plugin API describes,
  or a string saying what is wrong with it. With `before-start?`, one of
  a timeline, it may start before 0; an entry of a style starts with the
  style or later."
  ([props]
   (parse-animation props false))
  ([props before-start?]
   (let [type      (some-> (obj/get props "type") keyword)
         direction (d/nilv (obj/get props "direction") "in")
         start     (d/nilv (obj/get props "start") 0)
         duration  (obj/get props "duration")
         easing    (some-> (obj/get props "easing") (parse-easing false))
         numbers   (->> ["amount" "offsetX" "offsetY"]
                        (keep #(when-let [v (obj/get props %)] [% v]))
                        (into {}))
         split     (parse-split (obj/get props "split"))
         spread    (obj/get props "spread")
         order     (obj/get props "order")
         seed      (obj/get props "seed")
         tokens    (when (contains? cta/animation-types type)
                     (parse-entry-tokens (obj/get props "appliedTokens") type))]
     (cond
       (not (contains? cta/animation-types type))
       "The type should be one of 'fade', 'move', 'scale', 'rotate' or 'blur'"

       (not (contains? #{"in" "out"} direction))
       "The direction should be 'in' or 'out'"

       (not (if before-start? (valid-timeline-time? start) (valid-time? start)))
       (if before-start?
         "The start should be a number of milliseconds"
         "The start should be a number of milliseconds from 0")

       (and (some? duration) (not (and (sm/valid-safe-number? duration) (pos? duration))))
       "The duration should be a number of milliseconds above 0"

       (and (some? (obj/get props "easing")) (nil? easing))
       "Not a valid easing"

       (not (every? sm/valid-safe-number? (vals numbers)))
       "The amount and offsets should be numbers"

       (= ::invalid split)
       "The split should be 'char', 'word' or 'line'"

       (and (nil? split) (some some? [spread order seed]))
       "The spread, order and seed go with a split"

       (and (some? spread) (not (valid-spread? spread)))
       "The spread should be a number from 0 to 1"

       (and (some? order) (not (valid-order? order)))
       "The order should be 'forward', 'backward', 'center', 'edges' or 'random'"

       (and (some? seed) (not (sm/valid-safe-int? seed)))
       "The seed should be a whole number"

       (string? tokens)
       tokens

       :else
       (cond-> {:type type
                :direction (keyword direction)
                :start (mth/round start)}
         (some? duration)              (assoc :duration (mth/round duration))
         (some? easing)                (merge easing)
         (contains? numbers "amount")  (assoc :amount (get numbers "amount"))
         (contains? numbers "offsetX") (assoc :offset-x (get numbers "offsetX"))
         (contains? numbers "offsetY") (assoc :offset-y (get numbers "offsetY"))
         (some? split)                 (assoc :split split)
         (some? spread)                (assoc :spread spread)
         (some? order)                 (assoc :order (keyword order))
         (some? seed)                  (assoc :seed seed)
         (seq tokens)                  (assoc :applied-tokens tokens))))))

(defn- parse-entry-keyframe
  "The keyframe of a keyframe entry of `property` that the object `props`
  of the plugin API describes (`time` from the start of the entry, the
  `value` relative to the layer, see `cta/schema:keys-entry`), or a
  string saying what is wrong with it."
  [property props]
  (if-not (object? props)
    "It should be an object"
    (let [time     (obj/get props "time")
          value    (obj/get props "value")
          easing   (parse-easing (d/nilv (obj/get props "easing") "ease") true)
          tokens   (parse-keyframe-tokens (obj/get props "appliedTokens"))
          path-in  (obj/get props "pathIn")
          path-out (obj/get props "pathOut")]
      (cond
        (not (valid-time? time))
        "The time should be a number of milliseconds from the start of the entry"

        (not (valid-value? property value))
        (if (cta/color-property? property)
          "The value of a color should be an hexadecimal color like '#ff0000'"
          "The value should be a number")

        (nil? easing)
        "Not a valid easing"

        (string? tokens)
        tokens

        (not (every? #(or (nil? %) (sm/valid-safe-number? %)) [path-in path-out]))
        "The handles of a motion path should be numbers"

        :else
        (cond-> (merge {:time (mth/round time) :value value} easing)
          (some? tokens)   (assoc :applied-tokens tokens)
          (some? path-in)  (assoc :path-in path-in)
          (some? path-out) (assoc :path-out path-out))))))

(defn- parse-keys-entry
  "The keyframe entry of an animation style that the object `props` of
  the plugin API describes (`type: 'keyframes'`, see
  `cta/schema:keys-entry`), or a string saying what is wrong with it."
  [props]
  (let [property  (get properties (obj/get props "property"))
        index     (obj/get props "index")
        start     (d/nilv (obj/get props "start") 0)
        duration  (obj/get props "duration")
        keyframes (obj/get props "keyframes")]
    (cond
      (nil? property)
      "Not a property that can be animated"

      (and (some? index) (not (cta/indexed-property? property)))
      "Only fills, strokes and shadows take an index"

      (and (some? index) (not (and (sm/valid-safe-int? index) (>= index 0))))
      "The index should be a number from 0"

      (not (valid-time? start))
      "The start should be a number of milliseconds from 0"

      (and (some? duration) (not (and (sm/valid-safe-number? duration) (pos? duration))))
      "The duration should be a number of milliseconds above 0"

      (not (and (array? keyframes) (pos? (alength keyframes))))
      "The keyframes should be an array of one or more"

      (> (alength keyframes) cta/max-style-keyframes)
      (dm/str "A style holds up to " cta/max-style-keyframes " keyframes")

      :else
      (let [parsed (into []
                         (map-indexed (fn [i props]
                                        (let [keyframe (parse-entry-keyframe property props)]
                                          (if (string? keyframe)
                                            (dm/str "Keyframe " (inc i) ": " keyframe)
                                            keyframe))))
                         keyframes)]
        (cond
          (some string? parsed)
          (d/seek string? parsed)

          (and (some? duration) (some #(> (:time %) duration) parsed))
          "The keyframes should be within the duration"

          :else
          (cond-> {:type :keys
                   :property property
                   :start (mth/round start)
                   :keyframes parsed}
            (some? index)    (assoc :index index)
            (some? duration) (assoc :duration (mth/round duration))))))))

(defn- format-entry-keyframe
  "A keyframe of a keyframe entry as the plugin API gives it."
  [{:keys [time value path-in path-out applied-tokens] :as keyframe}]
  (cond-> #js {:time time
               :value value
               :easing (format-easing keyframe)}
    (some? path-in)      (obj/set! "pathIn" path-in)
    (some? path-out)     (obj/set! "pathOut" path-out)
    (seq applied-tokens) (obj/set! "appliedTokens" (format-applied-tokens applied-tokens))))

(defn- format-preset-entry
  [{:keys [type direction start duration amount offset-x offset-y split spread order seed
           applied-tokens] :as entry}]
  (cond-> #js {:type (name type)
               :direction (name (or direction :in))
               :start start
               :duration duration
               :easing (if (some? (:easing entry))
                         (format-easing entry)
                         (if (= :out direction) "ease-in" "ease-out"))}
    (some? amount)   (obj/set! "amount" amount)
    (some? offset-x) (obj/set! "offsetX" offset-x)
    (some? offset-y) (obj/set! "offsetY" offset-y)
    (some? split)    (obj/set! "split" (name split))
    (some? spread)   (obj/set! "spread" spread)
    (some? order)    (obj/set! "order" (name order))
    (some? seed)     (obj/set! "seed" seed)
    (seq applied-tokens)
    (obj/set! "appliedTokens" (format-applied-tokens applied-tokens))))

(defn format-style-entry
  "The entry `entry` of an animation style as the plugin API gives it: an
  object with the fields of `PresetAnimationProps` and, for the settings
  design tokens set, their names in `appliedTokens`; or, for a keyframe
  entry, `type: 'keyframes'` with its property, span and keyframes."
  [{:keys [property index start duration keyframes] :as entry}]
  (if (cta/keys-entry? entry)
    (cond-> #js {:type "keyframes"
                 :property (get property-names property)
                 :start start
                 :duration duration
                 :keyframes (into-array (map format-entry-keyframe keyframes))}
      (some? index) (obj/set! "index" index))
    (format-preset-entry entry)))

(defn binds-tokens?
  "Whether the style `entries` bind design tokens: in their settings, or in
  the easings of the keyframes of keyframe entries."
  [entries]
  (boolean (some #(or (seq (:applied-tokens %)) (some :applied-tokens (:keyframes %))) entries)))

(defn parse-style-entries
  "The entries of an animation style that the array `entries` of the
  plugin API describes (see `parse-animation` and `parse-keys-entry`),
  with the design tokens of their `appliedTokens`, or a string saying
  what is wrong with them."
  [entries]
  (cond
    (not (array? entries))
    "The entries should be an array"

    (not (<= 1 (alength entries) cta/max-style-entries))
    (dm/str "A style holds from 1 to " cta/max-style-entries " entries")

    :else
    (let [parsed (into []
                       (map-indexed (fn [index props]
                                      (let [entry (cond
                                                    (not (object? props))
                                                    "It should be an object"

                                                    (= "keyframes" (obj/get props "type"))
                                                    (parse-keys-entry props)

                                                    :else
                                                    (parse-animation props))]
                                        (if (string? entry)
                                          (dm/str "Entry " (inc index) ": " entry)
                                          entry))))
                       entries)]
      (cond
        (some string? parsed)
        (d/seek string? parsed)

        (> (ctas/keyframe-count parsed) cta/max-style-keyframes)
        (dm/str "A style holds up to " cta/max-style-keyframes " keyframes")

        :else
        parsed))))

(defn- copy-options
  "The settings the object `options` of `Timeline.copyAnimation` gives
  `cltl/paste-animation`, or a string saying what is wrong with them."
  [options]
  (let [names    (obj/get options "properties")
        presets? (obj/get options "presets")
        values   (obj/get options "values")
        start    (obj/get options "start")
        stagger  (obj/get options "stagger")
        replace? (obj/get options "replace")]
    (cond
      (and (some? names) (not (and (array? names) (every? #(contains? properties %) names))))
      "The properties should be an array of properties that can be animated"

      (and (some? presets?) (not (boolean? presets?)))
      "Whether it copies the preset animations should be true or false"

      (and (some? values) (not (contains? #{"relative" "exact"} values)))
      "The values should be 'relative' or 'exact'"

      (and (some? start) (not (valid-timeline-time? start)))
      "The start should be a number of milliseconds"

      (and (some? stagger) (not (valid-time? stagger)))
      "The stagger should be a number of milliseconds from 0"

      (and (some? replace?) (not (boolean? replace?)))
      "Whether it replaces the animation of the targets should be true or false"

      :else
      {:properties (some->> names (into #{} (map properties)))
       :presets?   (d/nilv presets? true)
       :exact?     (= "exact" values)
       :time       (some-> start mth/round)
       :stagger    (mth/round (d/nilv stagger 0))
       :mode       (if (false? replace?) :merge :replace)})))

(defn applied-style-proxy? [p]
  (obj/type-of? p "AppliedAnimationStyleProxy"))

(defn applied-style-proxy
  [plugin-id file-id page-id board-id shape-id id]
  (let [locate
        #(locate-animation file-id page-id board-id shape-id id)

        update!
        (fn [code f]
          (if (:locked (locate))
            (u/not-valid plugin-id code "The animation is locked")
            (write-timeline! plugin-id file-id page-id board-id code
                             #(cta/update-animation % shape-id id f))))]

    (obj/reify {:name "AppliedAnimationStyleProxy" :on-error (u/handle-error plugin-id)}
      :$plugin {:enumerable false :get (constantly plugin-id)}
      :$file {:enumerable false :get (constantly file-id)}
      :$page {:enumerable false :get (constantly page-id)}
      :$board {:enumerable false :get (constantly board-id)}
      :$shape {:enumerable false :get (constantly shape-id)}
      :$id {:enumerable false :get (constantly id)}

      :id
      {:get #(dm/str id)}

      ;; not enumerable: the shape leads back here through its board's
      ;; timeline, and serializing it would never end
      :shape
      {:enumerable false
       :get #(shape-proxy plugin-id file-id page-id shape-id)}

      ;; while its style is in the file or in a library of it
      :style
      {:get
       #(when-let [{:keys [style-ref-file style-ref-id]} (locate)]
          (when (some? (u/locate-library-animation-style style-ref-file style-ref-id))
            (lib-animation-style-proxy plugin-id style-ref-file style-ref-id)))}

      :name
      {:get #(:name (locate))}

      :start
      {:get #(:start (locate))
       :set
       (fn [value]
         (if (not (valid-timeline-time? value))
           (u/not-valid plugin-id :start value)
           (update! :start #(assoc % :start (mth/round value)))))}

      :duration
      {:get #(:duration (locate))
       :set
       (fn [value]
         (if (not (and (sm/valid-safe-number? value) (pos? value)))
           (u/not-valid plugin-id :duration value)
           (update! :duration #(assoc % :duration (mth/round value)))))}

      ;; the presets it plays take its place, with ids of their own, and
      ;; its keyframe entries become keyframes of the layer
      :detach
      (fn []
        (when-let [use (locate)]
          (if (:locked use)
            (u/not-valid plugin-id :detach "The animation is locked")
            (let [objects (u/locate-objects file-id page-id)
                  before  (into #{} (map :id) (:animations (cta/get-track (locate-timeline file-id page-id board-id) shape-id)))]
              (when (write-timeline! plugin-id file-id page-id board-id :detach
                                     #(update-in % [:tracks shape-id] ctas/detach-track-use id % objects))
                (format/format-array
                 #(preset-animation-proxy plugin-id file-id page-id board-id shape-id (:id %))
                 (->> (cta/get-track (locate-timeline file-id page-id board-id) shape-id)
                      :animations
                      (remove #(contains? before (:id %))))))))))

      :remove
      (fn []
        (if (:locked (locate))
          (u/not-valid plugin-id :remove "The animation is locked")
          (write-timeline! plugin-id file-id page-id board-id :remove
                           #(cta/remove-animation % shape-id id)))))))

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

      ;; not enumerable: the board leads back here, and serializing it
      ;; would never end
      :board
      {:enumerable false
       :get #(shape-proxy plugin-id file-id page-id board-id)}

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

      :autoplay
      {:get #(some-> (locate) cta/autoplay?)
       :set
       (fn [value]
         (if (not (boolean? value))
           (u/not-valid plugin-id :autoplay value)
           (write! :autoplay #(cta/with-autoplay % value))))}

      :keyframes
      {:get
       #(->> (track-items (locate) :keyframes)
             (format/format-array
              (fn [[shape-id {:keys [id]}]]
                (keyframe-proxy plugin-id file-id page-id board-id shape-id id))))}

      :animations
      {:get
       #(->> (track-items (locate) :animations)
             (remove (fn [[_ animation]] (cta/style-use? animation)))
             (format/format-array
              (fn [[shape-id {:keys [id]}]]
                (preset-animation-proxy plugin-id file-id page-id board-id shape-id id))))}

      :appliedStyles
      {:get
       #(->> (track-items (locate) :animations)
             (filter (fn [[_ animation]] (cta/style-use? animation)))
             (format/format-array
              (fn [[shape-id {:keys [id]}]]
                (applied-style-proxy plugin-id file-id page-id board-id shape-id id))))}

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
                  (when (contains? keyframe :applied-tokens)
                    (st/emit! (dwtp/propagate-style-tokens)))
                  (keyframe-proxy plugin-id file-id page-id board-id shape-id id)))))))

      :addAnimation
      (fn [shape props]
        (when-let [shape-id (layer :addAnimation shape)]
          (let [animation (if (object? props) (parse-animation props true) "No animation given")
                error     (if (string? animation)
                            animation
                            (split-error file-id page-id (locate) shape-id (:split animation) nil))]
            (if (some? error)
              (u/not-valid plugin-id :addAnimation error)
              (let [id (uuid/next)]
                (when (write! :addAnimation #(cta/add-animation % shape-id (assoc animation :id id)))
                  (when (contains? animation :applied-tokens)
                    (st/emit! (dwtp/propagate-style-tokens)))
                  (preset-animation-proxy plugin-id file-id page-id board-id shape-id id)))))))

      :addAnimationStyle
      (fn [shape style start]
        (when-let [shape-id (layer :addAnimationStyle shape)]
          (let [start (d/nilv start 0)
                kind  (some :split (get cta/builtin-styles (keyword style)))
                error (when (contains? style-names style)
                        (split-error file-id page-id (locate) shape-id kind nil))]
            (cond
              (not (contains? style-names style))
              (u/not-valid plugin-id :addAnimationStyle
                           (dm/str "The style should be one of " (pr-str (sort style-names))))

              (not (valid-timeline-time? start))
              (u/not-valid plugin-id :addAnimationStyle "The start should be a number of milliseconds")

              (some? error)
              (u/not-valid plugin-id :addAnimationStyle error)

              :else
              (let [animations (mapv #(assoc % :id (uuid/next) :start (mth/round start))
                                     (get cta/builtin-styles (keyword style)))]
                (when (write! :addAnimationStyle
                              #(reduce (fn [timeline animation]
                                         (cta/add-animation timeline shape-id animation))
                                       %
                                       animations))
                  (format/format-array
                   #(preset-animation-proxy plugin-id file-id page-id board-id shape-id (:id %))
                   animations)))))))

      ;; A style of the file or of a library of it: a use that plays a
      ;; copy of its entries and follows the edits of the style.
      :applyAnimationStyle
      (fn [shape style start]
        (when-let [shape-id (layer :applyAnimationStyle shape)]
          (let [start      (d/nilv start 0)
                style-file (when (lib-animation-style-proxy? style) (obj/get style "$file"))
                style-data (when (some? style-file)
                             (u/locate-library-animation-style style-file (obj/get style "$id")))]
            (cond
              (not (lib-animation-style-proxy? style))
              (u/not-valid plugin-id :applyAnimationStyle "Not an animation style of a library")

              (nil? style-data)
              (u/not-valid plugin-id :applyAnimationStyle
                           "The style is neither in the file nor in a library of it")

              (not (valid-timeline-time? start))
              (u/not-valid plugin-id :applyAnimationStyle "The start should be a number of milliseconds")

              :else
              (let [use (ctas/make-use style-data style-file (mth/round start))]
                (when (write! :applyAnimationStyle #(cta/add-animation % shape-id use))
                  (applied-style-proxy plugin-id file-id page-id board-id shape-id (:id use))))))))

      :markers
      {:get
       #(format/format-array
         (fn [{:keys [id]}]
           (marker-proxy plugin-id file-id page-id board-id id))
         (:markers (locate)))}

      :audioClips
      {:get
       #(format/format-array
         (fn [{:keys [id]}]
           (audio-clip-proxy plugin-id file-id page-id board-id id))
         (ctau/audio-clips (locate)))}

      ;; As a drop of an audio file in the workspace: decoded, uploaded and
      ;; played by a new clip. The duration of the timeline stays.
      :addAudioClip
      (fn [name data mime-type props]
        (let [bytes (audio-bytes data)
              start (when (object? props) (obj/get props "start"))
              lane  (when (object? props) (obj/get props "lane"))
              error (cond
                      (some? (write-error plugin-id page-id))
                      (write-error plugin-id page-id)

                      (not (valid-name? name))
                      "The name should be a text"

                      (nil? bytes)
                      "The data should be an Uint8Array or an ArrayBuffer"

                      (not (and (string? mime-type) (some? (cm/canonical-audio-type mime-type))))
                      (dm/str "The type should be one of " (pr-str (sort cm/audio-canonical-types)))

                      (and (some? props) (not (object? props)))
                      "The options should be an object"

                      (and (some? start) (not (valid-timeline-time? start)))
                      "The start should be a number of milliseconds"

                      (and (some? lane) (not (valid-lane? lane)))
                      "The lane should be a whole number from 0"

                      ;; a lane plays a single file, and this one is new
                      (and (some? lane) (some #(= lane (:lane %)) (vals (:audio-clips (locate)))))
                      "The lane plays another audio file"

                      (nil? (locate))
                      "The board has no timeline")]
          (if (some? error)
            (js/Promise. (fn [_ reject] (u/reject-not-valid reject :addAudioClip error)))
            (add-audio-clip! plugin-id file-id page-id board-id name bytes
                             (cm/canonical-audio-type mime-type)
                             (mth/round (d/nilv start 0)) lane))))

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

      ;; As copying the animation of a layer on the canvas and pasting it
      ;; onto others, in the boards of the page they are in.
      :copyAnimation
      (fn [shape targets options]
        (when-let [shape-id (layer :copyAnimation shape)]
          (let [ids  (when (array? targets)
                       (mapv #(when (and (shape-proxy? %)
                                         (= file-id (obj/get % "$file"))
                                         (= page-id (obj/get % "$page")))
                                (obj/get % "$id"))
                             targets))
                opts (cond
                       (nil? options)    (copy-options #js {})
                       (object? options) (copy-options options)
                       :else             "The options should be an object")]
            (cond
              (or (nil? ids) (some nil? ids))
              (u/not-valid plugin-id :copyAnimation "The targets should be an array of shapes of the page of the timeline")

              (string? opts)
              (u/not-valid plugin-id :copyAnimation opts)

              :else
              (when-let [{:keys [targets skipped pivot-kept]}
                         (paste-animation! plugin-id file-id page-id :copyAnimation shape-id
                                           (distinct ids) opts)]
                #js {:targets (count targets)
                     :skipped skipped
                     :pivotKept pivot-kept})))))

      ;; The code of the Motion tab of the Inspect with the board selected
      ;; (see `app.util.code-gen.motion`): the board's own animation and,
      ;; each on its own clock, those of the component copies in it, with
      ;; the classes of the code of the Inspect; split animations play as
      ;; animations of their whole texts.
      :toCSS
      (fn []
        (when (some? (locate))
          (let [page (u/locate-page file-id page-id)]
            (or (some-> (cgm/code-timelines page (:files @st/state) board-id)
                        (vector)
                        (cgm/codes-css (:objects page) nil))
                ""))))

      :toJSON
      (fn []
        (when (some? (locate))
          (let [page (u/locate-page file-id page-id)]
            (some-> (cgm/code-timelines page (:files @st/state) board-id)
                    (ctac/motion-json (:objects page))
                    (clj->js)))))

      :toReact
      (fn []
        (when (some? (locate))
          (let [page (u/locate-page file-id page-id)]
            (or (some-> (cgm/code-timelines page (:files @st/state) board-id)
                        (vector)
                        (cgm/codes-react (:objects page) nil))
                ""))))

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
        playback (obj/get options "playback")
        autoplay (obj/get options "autoplay")]
    (cond
      (and (some? name) (not (string? name)))
      "The name should be a string"

      (and (some? duration) (not (and (sm/valid-safe-number? duration) (pos? duration))))
      "The duration should be a number of milliseconds above 0"

      (and (some? playback) (not (contains? playback-names playback)))
      "The playback should be 'once', 'loop' or 'ping-pong'"

      (and (some? autoplay) (not (boolean? autoplay)))
      "The autoplay should be true or false"

      :else
      (cond-> {}
        (some? name)     (assoc :name name)
        (some? duration) (assoc :duration (mth/round duration))
        (some? playback) (assoc :playback (keyword playback))
        (some? autoplay) (assoc :autoplay autoplay)))))

(defn add-timeline!
  "Give the board `board-id` a timeline, or the one it has the settings
  of `options`: the proxy of it, or nil."
  [plugin-id file-id page-id board-id options]
  (let [settings (if (some? options) (timeline-options options) {})]
    (if (string? settings)
      (u/not-valid plugin-id :addTimeline settings)
      (when (write-timeline! plugin-id file-id page-id board-id :addTimeline
                             #(cond-> (merge % (dissoc settings :autoplay))
                                (contains? settings :playback) (dissoc :loop)
                                (contains? settings :autoplay) (cta/with-autoplay (:autoplay settings))))
        (timeline-proxy plugin-id file-id page-id board-id)))))
