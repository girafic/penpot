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
  (`values-at`, `timeline->opacity`) shared by the editor preview, the
  viewer playback and the exporter, so the animation is computed
  identically everywhere; the layers that follow others and the chains
  that reach play in `app.common.types.animation-follow`, with the
  modifiers of a timeline at a time (`timeline->modif-tree`); the
  outlines of a path as keyframes are held in
  `app.common.types.animation-outline`. The exports that write it out
  are `app.common.animation.css`, `.svg` and `.lottie`."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.color :as clr]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.shape.interactions :as cti]
   [app.common.types.shape.layout :as ctl]
   [app.common.types.shape.radius :as ctsr]
   [app.common.types.text :as txt]
   [app.common.types.text-units :as tu]
   [app.common.types.token :as cto]
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
    :trim-start :trim-end :trim-offset
    ;; the outline of a path, in its own frame (see `ctao/outline-of`)
    :content})

(def style-properties
  "The properties the keyframe entries of an animation style animate: all
  but the outline of a path, which no value relative to a layer holds."
  (disj animatable-properties :content))

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

(def schema:animation-tokens
  "The design tokens that set settings of a preset animation or of a style
  entry, by their names (see `animation-token-types`). The file of the
  timeline or of the style resolves them and writes their values into the
  settings, which play those values; the uses of a style keep the names,
  as they keep the values."
  [:map {:title "AnimationTokens"}
   [:start {:optional true} cto/schema:token-name]
   [:duration {:optional true} cto/schema:token-name]
   [:easing {:optional true} cto/schema:token-name]
   [:amount {:optional true} cto/schema:token-name]
   [:offset-x {:optional true} cto/schema:token-name]
   [:offset-y {:optional true} cto/schema:token-name]])

(def schema:keyframe-tokens
  "The design token that sets the easing of the segment a keyframe starts,
  by its name: a cubic bezier or a spring token (see `token-setting`)."
  [:map {:title "KeyframeTokens"}
   [:easing {:optional true} cto/schema:token-name]])

(def schema:loop-key
  [:or
   [::sm/one-of animatable-properties]
   [:tuple [::sm/one-of animatable-properties] ::sm/safe-int]])

(def ^:private keyframe-entries
  "The entries of a keyframe but its property and value (see
  `schema:keyframe`)."
  [[:id ::sm/uuid]
   [:time ::sm/safe-int]
   [:index {:optional true} ::sm/safe-int]
   [:easing {:optional true} schema:easing]
   [:interpolation {:optional true} [::sm/one-of interpolation-types]]
   [:applied-tokens {:optional true} schema:keyframe-tokens]])

(def schema:keyframe
  "A keyframe: the value of a property at a time. The outline of a path
  (`:content`) holds path content and no motion path; the colours a hex
  colour, the others a number. JSON gives the property as a string, and
  decoding picks the branch before turning it into a keyword."
  [:multi {:title "Keyframe"
           :dispatch (fn [v]
                       (if (and (map? v) (= "content" (some-> (:property v) name)))
                         :content
                         :value))}
   [:content
    (into [:map {:title "OutlineKeyframe"}
           [:property [::sm/one-of #{:content}]]
           [:value ctao/schema:outline]
           ;; its first point and way round set by hand: not turned to
           ;; fit the keyframe before (see `ctao/matched-outlines`)
           [:fixed-start {:optional true} :boolean]
           ;; no motion path
           [:path-in {:optional true} :nil]
           [:path-out {:optional true} :nil]]
          keyframe-entries)]
   [:value
    (into [:map {:title "ValueKeyframe"}
           [:property [::sm/one-of style-properties]]
           [:value [:or ::sm/safe-number clr/schema:hex-color]]
           ;; Motion path: for `:x` and `:y`, how far along its axis the
           ;; curve leaves the keyframe towards the next one (`:path-out`)
           ;; and comes into it from the previous one (`:path-in`), like the
           ;; handles of a bezier node. Without them the position moves in
           ;; a straight line.
           [:path-in {:optional true} ::sm/safe-number]
           [:path-out {:optional true} ::sm/safe-number]]
          keyframe-entries)]])

;; A preset animation (like Figma's animation styles): an effect over a
;; span of time that stays editable. `:in` plays from the effect to the
;; layer's own values, `:out` from them to the effect. Within its span it
;; takes over the keyframes of the properties it animates.
(def animation-types
  #{:fade :move :scale :rotate :blur})

(def animation-directions
  #{:in :out})

(def schema:preset-animation
  [:map {:title "PresetAnimation"}
   [:id ::sm/uuid]
   [:type [::sm/one-of animation-types]]
   [:direction [::sm/one-of animation-directions]]
   [:start ::sm/safe-int]
   [:duration ::sm/safe-int]
   [:easing {:optional true} schema:easing]
   ;; fade: opacity factor, scale: scale factor, rotate: degrees, blur:
   ;; the radius of a layer blur in px
   [:amount {:optional true} ::sm/safe-number]
   ;; move: offset in px
   [:offset-x {:optional true} ::sm/safe-number]
   [:offset-y {:optional true} ::sm/safe-number]
   ;; on a text, played by its characters, words or lines one after the
   ;; other (see `app.common.types.text-units`): how far apart they start
   ;; (0..1), in which order and, for a random one, its seed
   [:split {:optional true} [::sm/one-of tu/split-kinds]]
   [:spread {:optional true} ::sm/safe-number]
   [:order {:optional true} [::sm/one-of tu/orders]]
   [:seed {:optional true} ::sm/safe-int]
   ;; the design tokens that set some of its settings
   [:applied-tokens {:optional true} schema:animation-tokens]
   ;; left out of playback and exports / kept from edits
   [:hidden {:optional true} :boolean]
   [:locked {:optional true} :boolean]])

(def schema:style-entry-tokens
  "The design tokens of a style entry (see `schema:animation-tokens`)."
  schema:animation-tokens)

(def max-style-keyframes
  "The most keyframes the keyframe entries of an animation style hold
  together."
  256)

(def schema:entry-keyframe
  "A keyframe of a keyframe entry (see `schema:keys-entry`): as one of a
  track, without its id, property and index, which its entry gives, its
  `:time` counted from the start of the entry."
  [:map {:title "AnimationStyleEntryKeyframe"}
   [:time ::sm/safe-int]
   [:value [:or ::sm/safe-number clr/schema:hex-color]]
   [:easing {:optional true} schema:easing]
   [:interpolation {:optional true} [::sm/one-of interpolation-types]]
   [:path-in {:optional true} ::sm/safe-number]
   [:path-out {:optional true} ::sm/safe-number]
   [:applied-tokens {:optional true} schema:keyframe-tokens]])

(def schema:keys-entry
  "A keyframe entry of an animation style: the keyframes of one property
  (and index, for fills, strokes and shadows) over `:duration` ms from
  `:start`, their times counted from the start of the entry. Their values
  are relative to the layer that plays them (see `keys-entry-keyframes`):
  positions and rotation as offsets from where it rests, sizes as factors
  of its own, the other properties as they are."
  [:map {:title "AnimationStyleKeysEntry"}
   [:id ::sm/uuid]
   [:type [:= :keys]]
   [:property [::sm/one-of style-properties]]
   [:index {:optional true} ::sm/safe-int]
   [:start ::sm/safe-int]
   [:duration ::sm/safe-int]
   [:keyframes [:vector {:min 1 :max max-style-keyframes :gen/max 4} schema:entry-keyframe]]])

(def schema:preset-entry
  "A preset entry of an animation style (see `schema:style-entry`)."
  [:map {:title "AnimationStylePresetEntry"}
   [:id ::sm/uuid]
   [:type [::sm/one-of animation-types]]
   [:direction [::sm/one-of animation-directions]]
   [:start ::sm/safe-int]
   [:duration ::sm/safe-int]
   [:easing {:optional true} schema:easing]
   [:amount {:optional true} ::sm/safe-number]
   [:offset-x {:optional true} ::sm/safe-number]
   [:offset-y {:optional true} ::sm/safe-number]
   [:split {:optional true} [::sm/one-of tu/split-kinds]]
   [:spread {:optional true} ::sm/safe-number]
   [:order {:optional true} [::sm/one-of tu/orders]]
   [:seed {:optional true} ::sm/safe-int]
   [:applied-tokens {:optional true} schema:style-entry-tokens]])

(def schema:style-entry
  "An entry of an animation style (see `app.common.types.animation-style`)
  and of its uses, its `:start` counted from the start of the style: a
  preset animation without the flags of a row, with the design tokens
  that set some of its settings, or a keyframe entry (see
  `schema:keys-entry`). JSON gives the type as a string."
  [:multi {:title "AnimationStyleEntry"
           :dispatch (fn [v]
                       (if (and (map? v)
                                (or (keyword? (:type v)) (string? (:type v)))
                                (= :keys (keyword (:type v))))
                         :keys
                         :preset))}
   [:keys schema:keys-entry]
   [:preset schema:preset-entry]])

(def max-style-entries
  "The most entries an animation style holds."
  32)

(def schema:style-instance
  "A use of an animation style on a track, one of its `:animations`: the
  style it links to, a copy of its name and entries, so it plays without
  the style (as a text keeps the values of its typography), and when it
  plays. Its entries scale in time to its `:duration` (see
  `expand-style`)."
  [:map {:title "AnimationStyleUse"}
   [:id ::sm/uuid]
   [:type [::sm/one-of #{:style}]]
   [:style-ref-id ::sm/uuid]
   [:style-ref-file ::sm/uuid]
   [:name :string]
   [:start ::sm/safe-int]
   [:duration ::sm/safe-int]
   [:entries [:vector {:min 1 :max max-style-entries :gen/max 4} schema:style-entry]]
   [:hidden {:optional true} :boolean]
   [:locked {:optional true} :boolean]])

(def schema:animation
  "An animation of a track: a preset or a use of an animation style. JSON
  gives the type as a string, and a type that is no string or keyword
  takes the preset branch, where it fails as an invalid value."
  [:multi {:title "TrackAnimation"
           :dispatch (fn [v]
                       (if (and (map? v)
                                (or (keyword? (:type v)) (string? (:type v)))
                                (= :style (keyword (:type v))))
                         :style
                         :preset))}
   [:style schema:style-instance]
   [:preset schema:preset-animation]])

(def schema:follow
  "How a layer follows another one of its board, its leader (see
  `follow-of`): it takes what the leader does `:delay` ms later, from
  its first frame on (`:from` `:start`) or from where it rests (`:rest`,
  as a layer of a group does), the parts it takes: where its pivot goes
  (`:position`), how it turns and scales around that pivot
  (`:rotation`, `:scale`) and its opacity (`:opacity`). The defaults
  (from the first frame; position, rotation and scale; no opacity; no
  delay) are stored as absence."
  [:map {:title "AnimationFollow"}
   [:leader ::sm/uuid]
   [:from {:optional true} [:enum :start :rest]]
   [:position {:optional true} :boolean]
   [:rotation {:optional true} :boolean]
   [:scale {:optional true} :boolean]
   [:opacity {:optional true} :boolean]
   [:delay {:optional true} ::sm/safe-int]])

(def schema:ik
  "How the layer at the end of a chain of two follows (an arm: the hand
  follows the forearm, which follows the upper arm, see `ctaf/ik-chains`)
  reaches the layer `:goal`: the two links turn so its pivot lands on
  the goal's (inverse kinematics), the elbow bending clockwise (`:cw`)
  or not (`:ccw`), the layer itself turning with the chain (`:chain`),
  keeping its own angle (`:keep`) or taking the turn of the goal
  (`:goal`). The defaults (`:ccw`, `:chain`) are stored as absence."
  [:map {:title "AnimationIk"}
   [:goal ::sm/uuid]
   [:bend {:optional true} [:enum :cw :ccw]]
   [:turn {:optional true} [:enum :chain :keep :goal]]])

(def schema:track
  [:map {:title "AnimationTrack"}
   [:shape-id ::sm/uuid]
   ;; its outlines within `ctao/max-outline-row-segments` together
   [:keyframes [:and
                [:vector {:gen/max 8} schema:keyframe]
                [:fn {:error/message (str "The outline keyframes of a row hold at most "
                                          ctao/max-outline-row-segments " segments")}
                 ctao/outline-row-fits?]]]
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
     [:y ::sm/safe-number]]]
   [:follow {:optional true} schema:follow]
   [:ik {:optional true} schema:ik]
   ;; A helper layer the exports leave out, as a goal ring (see `set-guide`).
   [:guide {:optional true} :boolean]
   ;; The size of the box the outlines of the `:content` keyframes were
   ;; made for, which they scale with (see `ctao/outline->content`).
   [:outline-box {:optional true}
    [:map
     [:width ::sm/safe-number]
     [:height ::sm/safe-number]]]])

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
  #{:mp4 :webm :gif :avif :svg :lottie :dotlottie})

(def schema:export
  "How the board of a timeline is exported as an animation, as last set
  in the export panel, which offers it again."
  [:map {:title "AnimationExport"}
   [:format {:optional true} [::sm/one-of export-formats]]
   [:quality {:optional true} [::sm/one-of #{:low :medium :high}]]
   [:fps {:optional true} ::sm/safe-int]
   [:size {:optional true} :string]
   [:loop {:optional true} :boolean]
   ;; whether MP4 and WebM carry the audio clips; absent is yes
   [:audio {:optional true} :boolean]
   ;; SVG: the names of the layers as their `id`s, and a bounding box (see
   ;; `timeline->svg`); absent is no
   [:ids {:optional true} :boolean]
   [:bbox {:optional true} :boolean]
   ;; SVG: texts as the outlines of their glyphs, not as text; absent is no
   [:outline {:optional true} :boolean]])

(def max-export-size-factor
  "The largest factor of the board an animation export takes as size."
  10)

(def max-export-size-pixels
  "The largest width or height in pixels an animation export takes as
  size."
  8192)

(defn parse-export-size
  "The `:size` of `schema:export` that `text` names, as the export panel
  takes it: a factor of the board, `2`, `3.5x` or `0,5`, as its number
  (`\"3.5\"`, up to `max-export-size-factor`); a width in pixels, `512w`
  or `w512`, as `\"w512\"`; a height, `720h` or `h720`, as `\"h720\"` (up
  to `max-export-size-pixels`). Nil when it names none."
  [text]
  (let [t      (-> (str text)
                   (str/lower-case)
                   (str/replace "," ".")
                   (str/replace #"\s+" ""))
        pixels (fn [prefix digits]
                 (let [n (d/parse-integer digits)]
                   (when (and (some? n) (<= 1 n max-export-size-pixels))
                     (str prefix n))))]
    (if-let [[_ a b] (re-matches #"(\d+)w|w(\d+)" t)]
      (pixels "w" (or a b))
      (if-let [[_ a b] (re-matches #"(\d+)h|h(\d+)" t)]
        (pixels "h" (or a b))
        (when-let [[_ a b] (re-matches #"(\d*\.?\d+)x?|x(\d*\.?\d+)" t)]
          (let [factor (d/parse-double (or a b))
                ;; three decimals at most
                factor (when (some? factor)
                         (/ (mth/round (* factor 1000)) 1000.0))]
            (when (and (some? factor) (< 0 factor) (<= factor max-export-size-factor))
              (if (== factor (mth/floor factor))
                (str (long factor))
                (str factor)))))))))

(defn export-size-scale
  "The pixel scale of the export size `size` (see `parse-export-size`)
  for a board of `width` × `height`: 1 for a size it does not name."
  [size width height]
  (let [size (or (parse-export-size size) "1")]
    (cond
      (str/starts-with? size "w") (/ (d/parse-double (subs size 1)) (max 1 width))
      (str/starts-with? size "h") (/ (d/parse-double (subs size 1)) (max 1 height))
      :else (d/parse-double size))))

(defn export-size-label
  "How the export panel shows the export size `size` (see
  `parse-export-size`): `3.5x`, `512w`, `720h`."
  [size]
  (cond
    (str/starts-with? size "w") (str (subs size 1) "w")
    (str/starts-with? size "h") (str (subs size 1) "h")
    :else (str size "x")))

(def min-copy-speed
  "The slowest a component copy plays the animation of its main: a
  quarter of its pace."
  0.25)

(def max-copy-speed
  "The fastest a component copy plays the animation of its main: four
  times its pace."
  4)

(def schema:copy-clock
  "When and how fast a component copy in a board plays the animation of
  its main (see `copy-clock`), as a precomposition in After Effects:
  from `:start` ms of the board on, at `:speed` times its pace."
  [:map {:title "AnimationCopyClock"}
   [:start {:optional true} ::sm/safe-int]
   [:speed {:optional true} [::sm/number {:min min-copy-speed :max max-copy-speed}]]])

(def schema:timeline
  [:map {:title "AnimationTimeline"}
   [:board-id ::sm/uuid]
   [:name :string]
   [:duration ::sm/safe-int]
   [:playback {:optional true} [::sm/one-of playback-modes]]
   ;; Timelines saved before `:playback` only say whether they loop.
   [:loop {:optional true} :boolean]
   [:loop-count {:optional true} [:maybe ::sm/safe-int]]
   ;; `false`: the prototype shows the board at rest until it is played
   ;; (see `autoplay?`)
   [:autoplay {:optional true} :boolean]
   [:markers {:optional true} [:vector {:gen/max 3} schema:marker]]
   [:export {:optional true} schema:export]
   [:tracks [:map-of {:gen/max 3} ::sm/uuid schema:track]]
   [:audio-clips {:optional true} [:map-of {:gen/max 2} ::sm/uuid ctau/schema:audio-clip]]
   ;; the clocks of the component copies in the board, by the id of the
   ;; head of each (see `copy-clock`)
   [:copies {:optional true} [:map-of {:gen/max 2} ::sm/uuid schema:copy-clock]]])

(def schema:timeline-attrs
  "Settings of a timeline to change, its board and tracks aside. A nil
  value removes an optional one."
  [:map {:title "AnimationTimelineAttrs"}
   [:name {:optional true} :string]
   [:duration {:optional true} ::sm/safe-int]
   [:playback {:optional true} [:maybe [::sm/one-of playback-modes]]]
   [:loop {:optional true} [:maybe :boolean]]
   [:loop-count {:optional true} [:maybe ::sm/safe-int]]
   [:autoplay {:optional true} [:maybe :boolean]]
   [:markers {:optional true} [:maybe [:vector {:gen/max 3} schema:marker]]]
   [:export {:optional true} [:maybe schema:export]]
   [:copies {:optional true} [:maybe [:map-of {:gen/max 2} ::sm/uuid schema:copy-clock]]]])

(def schema:timelines
  [:map-of {:gen/max 2} ::sm/uuid schema:timeline])

(sm/register! ::keyframe schema:keyframe)
(sm/register! ::preset-animation schema:preset-animation)
(sm/register! ::style-entry schema:style-entry)
(sm/register! ::style-instance schema:style-instance)
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

(defn painted-children
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
  plus its siblings, and the children of `holders`, in paint order (see
  `painted-children`), each before its descendants."
  ([objects board-id tracks]
   (export-layer-ids objects board-id tracks nil))
  ([objects board-id tracks holders]
   (let [tracked (set (keys tracks))
         parents (into (set holders) (keep #(get-in objects [% :parent-id])) tracked)
         layer?  (into #{}
                       (mapcat #(get-in objects [% :shapes] []))
                       parents)]
     (->> (tree-seq (fn [id] (seq (get-in objects [id :shapes])))
                    (fn [id] (painted-children objects id))
                    board-id)
          (remove #{board-id})
          (filter layer?)
          vec))))

(defn make-timeline
  "Create a timeline for `board-id` (a top-level frame). The page-level
  `:timelines` map is keyed by this same board-id."
  [{:keys [board-id name duration playback loop-count autoplay]}]
  (cond-> {:board-id board-id
           :name (or name "Animation")
           :duration (or duration default-duration)
           :tracks {}}
    (some? playback) (assoc :playback playback)
    (some? loop-count) (assoc :loop-count loop-count)
    (false? autoplay) (assoc :autoplay false)))

(defn playback-mode
  "What playback of `timeline` does at its end, one of `playback-modes`."
  [timeline]
  (or (:playback timeline)
      (if (:loop timeline) :loop :once)))

(defn autoplay?
  "Whether the prototype (the view mode) plays `timeline` when its board
  shows, from its start: unless it was turned off."
  [timeline]
  (not (false? (:autoplay timeline))))

(defn with-autoplay
  "`timeline` playing when its board shows in the prototype, or not:
  only the exception is kept (see `autoplay?`)."
  [timeline autoplay]
  (if autoplay
    (dissoc timeline :autoplay)
    (assoc timeline :autoplay false)))

(defn cycle-duration
  "Time in ms until playback of `timeline` is back at its start: twice
  the duration in ping-pong."
  [timeline]
  (cond-> (:duration timeline)
    (= :ping-pong (playback-mode timeline)) (* 2)))

(defn copy-clock
  "When and how fast the board of `timeline` plays the animation of the
  main of its component copy `id` (see `schema:copy-clock`): `{:start
  :speed}`, from 0 at its own pace unless set."
  [timeline id]
  (let [{:keys [start speed]} (dm/get-in timeline [:copies id])]
    {:start (or start 0)
     :speed (or speed 1)}))

(defn set-copy-clock
  "`timeline` with its component copy `id` playing the animation of its
  main from `start` ms on at `speed` (see `copy-clock`); the defaults
  are left out."
  [timeline id {:keys [start speed]}]
  (let [speed (some-> speed (mth/clamp min-copy-speed max-copy-speed))
        clock (cond-> {}
                (and (some? start) (not (zero? start))) (assoc :start (mth/round start))
                (and (some? speed) (not= 1 speed))      (assoc :speed speed))]
    (if (empty? clock)
      (let [timeline (d/dissoc-in timeline [:copies id])]
        (cond-> timeline (empty? (:copies timeline)) (dissoc :copies)))
      (assoc-in timeline [:copies id] clock))))

(defn- clock-elapsed
  "`elapsed` ms of the board as the clock of the animation of a
  component copy counts them (`:clock-start` and `:clock-speed`, see
  `app.common.logic.timelines/instance-timelines`): 0 until it starts,
  then at its pace. A timeline of a board keeps `elapsed`."
  [timeline elapsed]
  (let [start (:clock-start timeline 0)
        speed (:clock-speed timeline 1)]
    (if (and (zero? start) (= 1 speed))
      elapsed
      (* (max 0 (- elapsed start)) speed))))

(defn clock-end
  "When `timeline` is back at its start (see `cycle-duration`), in ms of
  the board: for the animation of a component copy, from its start and
  at its pace (see `clock-elapsed`)."
  [timeline]
  (let [cycle (cycle-duration timeline)
        speed (:clock-speed timeline 1)]
    (+ (:clock-start timeline 0)
       (if (= 1 speed) cycle (int (mth/ceil (/ cycle (double speed))))))))

(defn playback-time
  "The time of `timeline` shown `elapsed` ms after playing from its
  start: held at the end when played once, wrapped when looping, going
  back and forth in ping-pong. The animation of a component copy keeps
  its own clock (see `clock-elapsed`)."
  [timeline elapsed]
  (let [duration (max 1 (:duration timeline))
        elapsed  (clock-elapsed timeline elapsed)]
    (case (playback-mode timeline)
      :loop      (mod elapsed duration)
      :ping-pong (let [t (mod elapsed (* 2 duration))]
                   (if (> t duration) (- (* 2 duration) t) t))
      (min elapsed duration))))

(defn playback-back?
  "Whether `timeline` plays its way back `elapsed` ms after playing from
  its start: the second half of each round of ping-pong (see
  `playback-time`), on the clock of a component copy too."
  [timeline elapsed]
  (and (= :ping-pong (playback-mode timeline))
       (let [duration (max 1 (:duration timeline))]
         (> (mod (clock-elapsed timeline elapsed) (* 2 duration)) duration))))

(defn playback-ended?
  "Whether playback of `timeline` has stopped `elapsed` ms after its
  start, which only happens when it plays once."
  [timeline elapsed]
  (and (= :once (playback-mode timeline))
       (> (clock-elapsed timeline elapsed) (:duration timeline))))

(defn timeline-empty?
  "Whether `timeline` animates nothing and plays no sound: no tracks and
  no audio clips."
  [timeline]
  (and (empty? (:tracks timeline))
       (empty? (:audio-clips timeline))))

(defn audio-plan
  "The parts of the audio clips of `timeline` that sound during the
  `span` ms of playback from `elapsed`, the ms since playback started at
  the start of the timeline: `{:clip-id :audio-id :at :offset :length
  :speed :points}` each, by `:at`, the ms from `elapsed` when the part
  starts, `:length` how long it plays, `:offset` the ms into its file it
  plays from, at `:speed` (so `:length` times `:speed` ms of the file,
  see `ctau/clip-speed`), `:points` its gain (see `ctau/gain-points`). As the keyframes, the clips play on each cycle of
  a loop and not past the end of the timeline; muted ones do not play,
  and the way back of ping-pong is silent (sound does not play
  backwards). Live playback and the exports both schedule by it."
  [timeline elapsed span]
  (let [duration (max 1 (:duration timeline))
        mode     (playback-mode timeline)
        period   (if (= :ping-pong mode) (* 2 duration) duration)
        end      (+ elapsed span)
        clips    (remove :muted (vals (:audio-clips timeline)))
        cycles   (if (= :once mode)
                   [0]
                   (range (* period (int (mth/floor (/ elapsed period)))) end period))]
    (->> (for [cycle cycles
               clip  clips
               :let  [start (+ cycle (:start clip))
                      ;; a clip does not play past the end of the timeline
                      stop  (+ cycle (min (ctau/clip-end clip) duration))
                      ;; nor before its start: a clip that starts before
                      ;; it sounds from there, as far into its file
                      from  (max start cycle elapsed)
                      to    (min stop end)]
               :when (< from to)]
           {:clip-id (:id clip)
            :audio-id (:audio-id clip)
            :at (- from elapsed)
            :offset (+ (:offset clip) (* (- from start) (ctau/clip-speed clip)))
            :length (- to from)
            :speed (ctau/clip-speed clip)
            :points (ctau/gain-points clip (- from start) (- to from))})
         (sort-by :at)
         (vec))))

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
  "A keyframe from `params`, with the design token that sets its easing
  (`:applied-tokens` `{:easing name}`) when it names one."
  [{:keys [id time property value easing interpolation index path-in path-out applied-tokens fixed-start]}]
  (let [easing-token (:easing applied-tokens)]
    (cond-> {:id (or id (uuid/next))
             :time (or time 0)
             :property property
             :value value}
      (some? easing) (assoc :easing easing)
      (some? interpolation) (assoc :interpolation interpolation)
      (and (some? path-in) (not= :content property)) (assoc :path-in path-in)
      (and (some? path-out) (not= :content property)) (assoc :path-out path-out)
      (some? index) (assoc :index index)
      (and (true? fixed-start) (= :content property)) (assoc :fixed-start true)
      (string? easing-token) (assoc :applied-tokens {:easing easing-token}))))

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

(defn box-point
  "The point at the normalized `origin` of the box of `shape` (its
  `:selrect`) before it turns: where its width and height grow from in
  its own space."
  [shape origin]
  (let [sr (:selrect shape)
        ox (or (:x origin) 0.5)
        oy (or (:y origin) 0.5)]
    (gpt/point (+ (or (:x sr) 0) (* (or (:width sr) 0) ox))
               (+ (or (:y sr) 0) (* (or (:height sr) 0) oy)))))

(defn origin-point
  "The point at the normalized `origin` of `shape` as it lies: its box
  turned as the shape is around its middle (its `:rotation`, as the
  exports turn it), where it turns and scales around."
  [shape origin]
  (let [point    (box-point shape origin)
        rotation (or (:rotation shape) 0)]
    (if (mth/almost-zero? (mod rotation 360))
      point
      (gpt/rotate point (box-point shape default-transform-origin) rotation))))

(defn- ensure-track
  [timeline shape-id]
  (if (get-track timeline shape-id)
    timeline
    (assoc-in timeline [:tracks shape-id] {:shape-id shape-id :keyframes []})))

(defn empty-track?
  "A track with nothing left: no keyframes, animations, origin, follow,
  goal to reach nor guide flag."
  [track]
  (and (empty? (:keyframes track))
       (empty? (:animations track))
       (nil? (:origin track))
       (nil? (:follow track))
       (nil? (:ik track))
       (not (:guide track))))

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

(def ^:private follow-defaults
  {:from :start :position true :rotation true :scale true :opacity false :delay 0})

(defn follow-of
  "How `shape-id` follows another layer of the board of `timeline` (see
  `schema:follow`), with the defaults; nil when it follows none."
  [timeline shape-id]
  (when-let [follow (dm/get-in timeline [:tracks shape-id :follow])]
    (merge follow-defaults follow)))

(declare store-track)

(defn set-follow
  "`timeline` with `shape-id` following the layer `:leader` of `follow`
  (see `schema:follow`), or following none for nil; the defaults are
  left out."
  [timeline shape-id follow]
  (if (some? (:leader follow))
    (let [follow (-> (select-keys follow [:leader :from :position :rotation :scale :opacity :delay])
                     (d/update-when :delay #(int (mth/round %))))
          stored (into {} (remove (fn [[k v]] (= v (get follow-defaults k ::none)))) follow)]
      (-> (ensure-track timeline shape-id)
          (assoc-in [:tracks shape-id :follow] stored)))
    (if-let [track (get-track timeline shape-id)]
      (store-track timeline shape-id (dissoc track :follow))
      timeline)))

(defn- store-track
  "Put back `track` of `shape-id` after something was removed from it:
  without the flags of the properties it has no keyframes of anymore, or
  dropped when nothing is left (no keyframes, animations nor origin)."
  [timeline shape-id track]
  (let [track (prune-slot-flags track)]
    (if (empty-track? track)
      (update timeline :tracks dissoc shape-id)
      (assoc-in timeline [:tracks shape-id] track))))

(def ^:private ik-defaults
  {:bend :ccw :turn :chain})

(defn ik-of
  "How `shape-id`, the end of a chain of two follows, reaches its goal
  (see `schema:ik`), with the defaults; nil when it reaches none."
  [timeline shape-id]
  (when-let [ik (dm/get-in timeline [:tracks shape-id :ik])]
    (merge ik-defaults ik)))

(defn set-ik
  "`timeline` with `shape-id` reaching the layer `:goal` of `ik` (see
  `schema:ik`), or reaching none for nil; the defaults are left out."
  [timeline shape-id ik]
  (if (some? (:goal ik))
    (let [ik     (select-keys ik [:goal :bend :turn])
          stored (into {} (remove (fn [[k v]] (= v (get ik-defaults k ::none)))) ik)]
      (-> (ensure-track timeline shape-id)
          (assoc-in [:tracks shape-id :ik] stored)))
    (if-let [track (get-track timeline shape-id)]
      (store-track timeline shape-id (dissoc track :ik))
      timeline)))

(defn drop-guides
  "`timeline` without the tracks of its helper layers (see `guide?`), for
  the exports that leave them out, once what reaches them plays by
  keyframes (see `ctaf/bake-follows`)."
  [timeline]
  (update timeline :tracks #(into {} (remove (fn [[_ track]] (:guide track))) %)))

(defn guide?
  "Whether `shape-id` is a helper layer of `timeline` the exports leave
  out, as a goal ring (see `schema:ik`)."
  [timeline shape-id]
  (true? (dm/get-in timeline [:tracks shape-id :guide])))

(defn set-guide
  "`timeline` with `shape-id` a helper layer the exports leave out, or
  not (see `guide?`)."
  [timeline shape-id guide?]
  (if guide?
    (-> (ensure-track timeline shape-id)
        (assoc-in [:tracks shape-id :guide] true))
    (if-let [track (get-track timeline shape-id)]
      (store-track timeline shape-id (dissoc track :guide))
      timeline)))

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

(defn add-keyframes
  "Add `keyframes` to the track of `shape-id` as `add-keyframe` adds each
  in turn (a later one at the time and slot of another replaces it), but
  sorted once: for the many keyframes of an import."
  [timeline shape-id keyframes]
  (let [added (into []
                    (comp (remove #(locked-keyframe? timeline shape-id %))
                          (map make-keyframe))
                    keyframes)]
    (if (empty? added)
      timeline
      (let [track (or (get-track timeline shape-id) {:shape-id shape-id :keyframes []})
            ;; the last keyframe of each time and slot
            kept  (reduce (fn [acc keyframe]
                            (assoc acc [(:time keyframe) (keyframe-slot keyframe)] keyframe))
                          {}
                          (concat (:keyframes track) added))]
        (assoc-in timeline [:tracks shape-id]
                  (assoc track :keyframes (sort-keyframes (vals kept))))))))

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
  :keyframe-id}` each) moved `dt` ms along, all of them as far. Locked
  ones stay where they are. One that lands on another keyframe of its
  property takes its place, as a keyframe added there does (see
  `add-keyframe`). The duration stays: they can go before the start or
  past the end, where they do not play, as in Figma."
  [timeline keyframes dt]
  (let [moving (filterv (fn [{:keys [shape-id] :as ref}]
                          (when-let [keyframe (find-keyframe timeline ref)]
                            (not (locked-keyframe? timeline shape-id keyframe))))
                        keyframes)
        dt     (if (seq moving) dt 0)]
    (if (zero? dt)
      timeline
      (let [moved-ids (into #{} (map :keyframe-id) moving)
            shifted   (reduce (fn [timeline {:keys [shape-id keyframe-id]}]
                                (update-keyframe timeline shape-id keyframe-id #(update % :time + dt)))
                              timeline
                              moving)]
        (reduce (fn [timeline shape-id]
                  (update-in timeline [:tracks shape-id :keyframes] drop-landed-on moved-ids))
                shifted
                (into #{} (map :shape-id) moving))))))

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

(defn set-outline-at
  "`timeline` with the outline keyframe of the path `shape` at `time`
  holding `value` (see `ctao/outline-value`): the one there changed, or a
  new one with the timing of the segment it splits; a new row made for
  the box of the path. Unchanged when the row does not take it (see
  `ctao/row-takes?`)."
  [timeline shape time value]
  (let [shape-id  (:id shape)
        keyframes (dm/get-in timeline [:tracks shape-id :keyframes])
        here?     #(and (= :content (:property %)) (= time (:time %)))
        existing  (d/seek here? keyframes)]
    (if-not (ctao/row-takes? keyframes here? value)
      timeline
      (-> (if (some? existing)
            (update-in timeline [:tracks shape-id :keyframes]
                       (partial mapv #(cond-> % (= (:id existing) (:id %)) (assoc :value value))))
            (add-keyframe timeline shape-id
                          (merge {:time time :property :content :value value}
                                 (split-timing timeline shape-id :content nil time))))
          (ctao/ensure-outline-box shape)))))

(defn set-outline-fixed
  "`timeline` with the outline keyframe of `shape-id` at `time` holding
  its first point and way round by hand (`fixed?`, see `:fixed-start`
  of `schema:keyframe`) or matched automatically again."
  [timeline shape-id time fixed?]
  (update-in timeline [:tracks shape-id :keyframes]
             (fn [keyframes]
               (some->> keyframes
                        (mapv #(if (and (= :content (:property %)) (= time (:time %)))
                                 (if fixed? (assoc % :fixed-start true) (dissoc % :fixed-start))
                                 %))))))

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

(defn set-by-hand
  "`animation` (a preset, a style entry or a style use) with `attrs` set
  by hand: the design tokens that bound the settings whose values change
  let go of them (see `schema:animation-tokens`), the others stay bound."
  [animation attrs]
  (let [changed (into [] (keep (fn [[key value]] (when (not= value (get animation key)) key))) attrs)
        tokens  (not-empty (apply dissoc (:applied-tokens animation) changed))]
    (cond-> (dissoc (merge animation attrs) :applied-tokens)
      (some? tokens) (assoc :applied-tokens tokens))))

(defn retime-keyframes
  "Map the keyframe and animation times of the tracks of `shape-ids`
  linearly from the span `from` to the span `to` (both `[start end]`):
  moves them when both spans have the same length, stretches them
  otherwise, before the start too (see `shift-keyframes`). The design
  tokens that bound the times of an animation that change let go of them
  (see `set-by-hand`)."
  [timeline shape-ids [from-start from-end] [to-start to-end]]
  (let [span  (- from-end from-start)
        scale (if (pos? span) (/ (- to-end to-start) span) 1)
        remap #(mth/round (+ to-start (* (- % from-start) scale)))

        retime-animation
        (fn [animation]
          (let [start (remap (:start animation))]
            (set-by-hand animation {:start start
                                    :duration (max 1 (- (remap (animation-end animation)) start))})))]
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
   :rotate {:amount -90}
   :blur   {:amount 8}})

(def builtin-styles
  "The built-in composite presets and the animations each one adds, which
  the UI calls presets (\"slide-up\", \"pop\"...): templates that add plain
  animations, not the animation styles of a library (see
  `app.common.types.animation-style`). The text presets play the
  characters, words or lines of a text one after the other (`:split`)."
  {:slide-up      [{:type :fade} {:type :move :offset-x 0 :offset-y 40}]
   :slide-down    [{:type :fade} {:type :move :offset-x 0 :offset-y -40}]
   :slide-left    [{:type :fade} {:type :move :offset-x 40 :offset-y 0}]
   :slide-right   [{:type :fade} {:type :move :offset-x -40 :offset-y 0}]
   :pop           [{:type :fade} {:type :scale :amount 0.5 :easing overshoot-easing}]
   :zoom          [{:type :fade} {:type :scale :amount 1.5}]
   :spin          [{:type :fade} {:type :scale :amount 0.5} {:type :rotate :amount -180}]
   :typewriter    [{:type :fade :duration 1000 :easing :linear :split :char :spread 1}]
   :rise-words    [{:type :fade :duration 800 :split :word}
                   {:type :move :duration 800 :offset-x 0 :offset-y 24 :split :word}]
   :cascade-lines [{:type :fade :duration 900 :split :line}
                   {:type :move :duration 900 :offset-x 0 :offset-y 40 :split :line}]
   :pop-letters   [{:type :fade :duration 800 :split :char :spread 0.7}
                   {:type :scale :duration 800 :amount 0.3 :easing overshoot-easing
                    :split :char :spread 0.7}]})

(def text-styles
  "The built-in presets that play the units of a text (see
  `builtin-styles`), which go on texts only."
  (into #{} (keep (fn [[preset animations]] (when (some :split animations) preset))) builtin-styles))

(defn animation-properties
  "The properties an animation of `type` takes over."
  [type]
  (case type
    :fade   [:opacity]
    :move   [:x :y]
    :scale  [:scale-x :scale-y]
    :rotate [:rotation]
    :blur   [:blur]
    []))

(defn style-use?
  "Whether `animation` is a use of an animation style (see
  `schema:style-instance`), not a preset."
  [animation]
  (= :style (:type animation)))

(defn keys-entry?
  "Whether the style entry `entry` is a keyframe entry (see
  `schema:keys-entry`), not a preset."
  [entry]
  (= :keys (:type entry)))

(defn style-length
  "How long the `entries` of an animation style play: to the end of the
  last one."
  [entries]
  (reduce max 0 (map animation-end entries)))

(declare make-style-entry)

(defn- random-seed
  []
  (inc (rand-int 2147483646)))

(defn- split-settings
  "The split keys of the preset `params` (see `schema:preset-animation`),
  valid: none without a known `:split`, the spread kept within 0..1, and
  by default half spread and in reading order. A random order without a
  seed gets one, kept from then on, so that its copies, a style made of
  it and the uses of that style play its units in the same order."
  [{:keys [split spread order seed]}]
  (when (contains? tu/split-kinds split)
    (let [order (if (contains? tu/orders order) order :forward)
          seed  (when (int? seed) seed)]
      (cond-> {:split split
               :spread (mth/clamp (if (number? spread) spread tu/default-spread) 0 1)
               :order order}
        (some? seed)                         (assoc :seed seed)
        (and (nil? seed) (= :random order))  (assoc :seed (random-seed))))))

(defn animation-token-types
  "The types of the design tokens that can set the setting `key` of a
  preset animation or a style entry of the preset type `type`, in the
  order a picker offers them; empty for a setting no token sets."
  [type key]
  (case key
    (:start :duration) [:duration]
    :easing [:cubic-bezier :spring]
    :amount (case type
              :fade [:opacity]
              :scale [:number]
              :rotate [:rotation :number]
              :blur [:dimensions :number]
              [])
    (:offset-x :offset-y) (if (= :move type) [:dimensions :number] [])
    []))

(defn- entry-tokens
  "The token names of `tokens` that set a setting of an entry of the
  preset type `type`, or nil."
  [type tokens]
  (when (map? tokens)
    (not-empty
     (into {}
           (filter (fn [[key token-name]]
                     (and (string? token-name)
                          (seq (animation-token-types type key)))))
           tokens))))

(defn make-animation
  "A valid animation from `params`: a preset, missing settings taking the
  defaults of its type, with the design tokens of `:applied-tokens` that
  set a setting of its type, or a use of an animation style (`:type :style`),
  which keeps its ref, name and entries (see `make-style-entry`) and lasts
  as long as its style unless it says otherwise."
  [{:keys [id type direction start duration hidden locked] :as params}]
  (cond-> (if (= :style type)
            (let [entries (mapv make-style-entry (:entries params))]
              {:id (or id (uuid/next))
               :type :style
               :style-ref-id (:style-ref-id params)
               :style-ref-file (:style-ref-file params)
               :name (or (:name params) "")
               :start (int (or start 0))
               :duration (max 1 (int (or duration (style-length entries))))
               :entries entries})
            (let [tokens (entry-tokens type (:applied-tokens params))]
              (cond-> (merge (get animation-defaults type)
                             (d/without-nils (select-keys params [:easing :amount :offset-x :offset-y]))
                             (split-settings params)
                             {:id (or id (uuid/next))
                              :type type
                              :direction (or direction :in)
                              :start (int (or start 0))
                              :duration (max 1 (int (or duration default-animation-duration)))})
                (some? tokens) (assoc :applied-tokens tokens))))
    (true? hidden) (assoc :hidden true)
    (true? locked) (assoc :locked true)))

(declare keyframe-value-fits?)

(defn- make-keys-entry
  "A valid keyframe entry from `params` (see `schema:keys-entry`): its
  keyframes by time, one at each whole ms (the last one given), those
  within 0..`:duration` (as long as they last by default) whose value is
  of the kind of its property, each with the easing and the design token
  it has (see `make-keyframe`); an index for the properties of fills,
  strokes and shadows (0 by default)."
  [{:keys [id property index start duration keyframes]}]
  (let [keyframes (->> keyframes
                       (filter #(and (mth/finite? (:time %))
                                     (keyframe-value-fits? {:property property :value (:value %)})))
                       (reduce #(assoc %1 (int (mth/round (:time %2))) %2) (sorted-map)))
        duration  (max 1 (int (or duration (reduce max 0 (keys keyframes)))))]
    (cond-> {:id (or id (uuid/next))
             :type :keys
             :property property
             :start (max 0 (int (or start 0)))
             :duration duration
             :keyframes (into []
                              (keep (fn [[time keyframe]]
                                      (when (<= 0 time duration)
                                        (-> (make-keyframe (assoc keyframe :time time :property property))
                                            (dissoc :id :property :index)))))
                              keyframes)}
      (indexed-property? property)
      (assoc :index (if (int? index) (max 0 index) 0)))))

(defn make-style-entry
  "A valid entry of an animation style from `params` in style time: a
  preset animation (see `make-animation`, which keeps its design tokens)
  without the flags of a row, or a keyframe entry (`:type :keys`, see
  `make-keys-entry`). Its `:id` is kept (a new one only when it has
  none), so the entries a use plays keep the same ids (see
  `expand-style`)."
  [params]
  (if (= :keys (:type params))
    (make-keys-entry params)
    (-> (make-animation (dissoc params :hidden :locked))
        (dissoc :hidden :locked)
        ;; style time starts at the start of the style
        (update :start max 0))))

(defn- spring-setting
  "The spring easing of the resolved spring token `value`
  (`{:stiffness :damping :mass}`), or nil when it does not fit."
  [value]
  (when (map? value)
    (let [{:keys [stiffness damping mass]} value]
      (when (and (mth/finite? stiffness) (pos? stiffness)
                 (mth/finite? damping) (>= damping 0)
                 (mth/finite? mass) (pos? mass))
        {:type :spring :stiffness stiffness :damping damping :mass mass}))))

(defn token-setting
  "The value a design token resolved to `value` gives the setting `key` of
  a preset animation or a style entry: whole ms for its start and
  duration, an easing for its easing (a bezier from the four numbers of a
  cubic bezier, its times kept within 0..1, or a spring from a spring
  token), the number itself for an amount or an offset; nil when the
  value does not fit."
  [key value]
  (case key
    (:start :duration)
    (when (mth/finite? value)
      (mth/round value))

    :easing
    (if (map? value)
      (spring-setting value)
      (when (and (sequential? value)
                 (= 4 (count value))
                 (every? mth/finite? value))
        (let [[x1 y1 x2 y2] value]
          {:type :bezier
           :curve [(mth/clamp x1 0 1) y1 (mth/clamp x2 0 1) y2]})))

    (:amount :offset-x :offset-y)
    (when (mth/finite? value)
      value)

    nil))

(defn resolve-keyframe-tokens
  "`keyframe` (of a track or of a keyframe entry) with the easing its
  design token sets taken from `values` (see `resolve-animation-tokens`):
  a held keyframe bound to a curve or a spring eases."
  [keyframe values]
  (let [easing (some->> (dm/get-in keyframe [:applied-tokens :easing])
                        (get values)
                        (token-setting :easing))]
    (if (or (nil? easing)
            (and (= easing (:easing keyframe)) (nil? (:interpolation keyframe))))
      keyframe
      (-> keyframe
          (assoc :easing easing)
          (dissoc :interpolation)))))

(defn resolve-animation-tokens
  "The preset animation or style entry `entry` with the settings its
  design tokens set (its `:applied-tokens`, those of the keyframes of a
  keyframe entry) taken from `values`, the resolved values of tokens by
  name. A token missing from `values`, or one whose value does not fit
  (see `token-setting`), leaves its setting as it is."
  [entry values]
  (if (keys-entry? entry)
    (update entry :keyframes (partial mapv #(resolve-keyframe-tokens % values)))
    (let [settings (reduce-kv (fn [settings key token-name]
                                (if-some [value (token-setting key (get values token-name))]
                                  (assoc settings key value)
                                  settings))
                              {}
                              (:applied-tokens entry))]
      (if (empty? settings)
        entry
        (make-animation (merge entry settings))))))

(defn rename-animation-tokens
  "The preset animation, style entry or keyframe `entry` with each token
  name of its `:applied-tokens` (those of the keyframes of a keyframe
  entry) given by `rename`, a function of the old name (returning it
  when it stays)."
  [entry rename]
  (cond-> entry
    (contains? entry :applied-tokens)
    (update :applied-tokens d/update-vals rename)

    (keys-entry? entry)
    (update :keyframes (partial mapv #(rename-animation-tokens % rename)))))

(defn resolve-timeline-tokens
  "`timeline` with the settings its design tokens set taken from `values`,
  the resolved values of tokens by name: those of its presets and of the
  easings of its keyframes. The uses of styles are left alone: they take
  the values of their style by sync. Nothing to change, `timeline` as it
  is."
  [timeline values]
  (update timeline :tracks
          d/update-vals
          (fn [track]
            (cond-> (update track :keyframes (partial mapv #(resolve-keyframe-tokens % values)))
              (contains? track :animations)
              (update :animations (partial mapv #(if (style-use? %) % (resolve-animation-tokens % values))))))))

(defn rename-timeline-tokens
  "`timeline` with each token name its presets and keyframes bind given by
  `rename` (see `rename-animation-tokens`); the uses of styles take the
  names of their style by sync."
  [timeline rename]
  (update timeline :tracks
          d/update-vals
          (fn [track]
            (cond-> (update track :keyframes (partial mapv #(rename-animation-tokens % rename)))
              (contains? track :animations)
              (update :animations (partial mapv #(if (style-use? %) % (rename-animation-tokens % rename))))))))

(defn expand-style
  "The preset animations and keyframe entries the style use `use` plays,
  in timeline time: its entries from its `:start`, scaled to its
  `:duration`, the keyframes of a keyframe entry too. Each keeps the `:id`
  of its entry, so expanding a use twice gives the same presets."
  [{:keys [start duration entries]}]
  (let [length (style-length entries)
        k      (if (pos? length) (/ duration length) 1)]
    (mapv (fn [entry]
            (let [duration (max 1 (mth/round (* (:duration entry) k)))]
              (cond-> (assoc entry
                             :start (+ start (mth/round (* (:start entry) k)))
                             :duration duration)
                (keys-entry? entry)
                (update :keyframes (partial mapv (fn [keyframe]
                                                   (update keyframe :time #(min duration (mth/round (* % k))))))))))
          entries)))

(defn- expand-uses
  "`animations` with each style use replaced, in place, by its presets."
  [animations]
  (into [] (mapcat #(if (style-use? %) (expand-style %) [%])) animations))

(defn track-animations
  "The preset animations `track` plays, in order: the hidden ones left out
  and each style use replaced, in place, by the presets and keyframe
  entries it stands for (see `expand-style`). Everything that plays
  animations reads them through it, so nothing after it knows that
  styles exist."
  [track]
  (expand-uses (remove :hidden (:animations track))))

(def ^:private split-keys
  [:split :spread :order :seed])

(defn- play-whole
  "`animation` playing as an animation of its whole layer, unless it
  splits by a kind of units of `keep` (a set): a preset without its split
  keys, a style use without those of its entries."
  [animation keep]
  (let [whole #(cond-> % (not (contains? keep (:split %))) (as-> a (apply dissoc a split-keys)))]
    (if (style-use? animation)
      (update animation :entries (partial mapv whole))
      (whole animation))))

(defn with-split-kind
  "`animations`, the presets of a track or the entries of a style, with
  the one `animation-id` played by `kind` of units (nil for the whole
  layer), and, a text playing one kind (see `unit-split-kind`), the
  others that split by `kind` too, each remade with `make`
  (`make-animation` or `make-style-entry`). Locked ones and style uses
  stay as they are."
  [animations animation-id kind make]
  (mapv (fn [{:keys [id split locked] :as animation}]
          (cond
            (or locked (style-use? animation))  animation
            (= id animation-id)                 (make (assoc animation :split kind))
            (and (some? kind) (some? split))    (make (assoc animation :split kind))
            :else                               animation))
        animations))

(defn flatten-splits
  "`timeline` with its split animations playing as animations of their
  whole layers, the presets and the entries of the style uses alike, but
  those that split by a kind of units of `keep` (none by default): as an
  export plays the units it cannot play one by one (lines in the code of
  a page, whose browser breaks them, see `unit-timelines`)."
  ([timeline]
   (flatten-splits timeline #{}))
  ([timeline keep]
   (flatten-splits timeline keep (constantly true)))
  ([timeline keep keep?]
   ;; `keep?` of a shape id: whether its split animations may keep theirs
   (update timeline :tracks
           (fn [tracks]
             (reduce-kv (fn [tracks id track]
                          (let [keep (if (keep? id) keep #{})]
                            (assoc tracks id (d/update-when track :animations (partial mapv #(play-whole % keep))))))
                        tracks
                        tracks)))))

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
  valid, unless it is locked. The duration stays: the animation can go
  past the end, where it does not play (see `shift-keyframes`)."
  [timeline shape-id animation-id f]
  (d/update-in-when timeline [:tracks shape-id :animations]
                    (partial mapv #(if (and (= (:id %) animation-id)
                                            (not (:locked %)))
                                     (make-animation (f %))
                                     %))))

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
  dragged. The starts and ends of the audio clips too, but for the clips
  of `clip-ids`."
  [timeline & {:keys [keyframe-ids animation-ids marker-ids clip-ids end?] :or {end? true}}]
  (let [tracks (vals (:tracks timeline))]
    (into (cond-> (sorted-set 0) end? (conj (:duration timeline)))
          (concat
           (ctau/clip-times timeline :clip-ids clip-ids)
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
(def preset-curves
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

(defn spring-free-progress
  "Progress of `spring` played as it is, not stretched, `seconds` after it
  starts: as a physics engine plays it."
  [spring seconds]
  (- 1.0 (spring-displacement spring seconds)))

(defn spring-over
  "The spring that, played as it is, moves in `duration-ms` as `spring`
  stretched over a segment that long does (see `spring-progress`): its
  stiffness times (T/L)² and its damping times T/L, T being its settle
  time and L the segment, in seconds; its mass as it is."
  [spring duration-ms]
  (let [scale (/ (settle-time spring) (/ (max 1 duration-ms) 1000.0))]
    (assoc spring
           :stiffness (* (double (:stiffness spring)) scale scale)
           :damping (* (double (:damping spring)) scale))))

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
  (cond
    ;; outlines of one structure (see `match-outlines`), else the first
    (path/content? from)
    (or (when (path/content? to) (path/lerp from to t)) from)

    (or (string? from) (string? to))
    (lerp-color from to t)

    :else
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
  path (see `schema:keyframe`): never an outline."
  [from to]
  (and (not= :step (:interpolation from :linear))
       (not= :content (:property from))
       (or (some? (:path-out from)) (some? (:path-in to)))))

(defn- segment-value
  [from to t]
  (if (curved? from to)
    (let [v0 (:value from)
          v1 (:value to)]
      (bezier-value v0 (+ v0 (:path-out from 0)) (+ v1 (:path-in to 0)) v1 t))
    (lerp-value (:value from) (:value to) t)))

(defn property-value-at
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
        ;; two keyframes, the most common (every preset): the span between
        (let [[from to] (if (== 2 (count keyframes))
                          [first-kf last-kf]
                          (->> (partition 2 1 keyframes)
                               (some (fn [[a b]]
                                       (when (and (<= (:time a) time) (< time (:time b)))
                                         [a b])))))]
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

(defn- matched-row
  "`keyframes`, the time-sorted keyframes of the row of outlines
  `row-key` (`[shape-id index]`), with their outlines matched to one
  structure (see `ctao/matched-outlines`)."
  [row-key keyframes]
  (let [values  (mapv :value keyframes)
        matched (ctao/matched-outlines row-key values (mapv #(true? (:fixed-start %)) keyframes))]
    (if (= values matched)
      keyframes
      (mapv #(assoc %1 :value %2) keyframes matched))))

(defn match-outlines
  "`timeline` with the outlines of each of its rows of `:content`
  keyframes matched to one structure (see `ctao/matched-outlines`), so that each
  one morphs into the next and every keyframe the exports make of them
  (see `expand-springs`, `cut-timeline`) keeps it."
  [timeline]
  (if (some #(some (fn [kf] (= :content (:property kf))) (:keyframes %)) (vals (:tracks timeline)))
    (update timeline :tracks
            (fn [tracks]
              (reduce-kv
               (fn [tracks shape-id {:keys [keyframes] :as track}]
                 (if (some #(= :content (:property %)) keyframes)
                   (let [{rows true others false} (group-by #(= :content (:property %)) keyframes)
                         rows (->> (group-by :index rows)
                                   (mapcat (fn [[index kfs]] (matched-row [shape-id index] (sort-keyframes kfs)))))]
                     (assoc tracks shape-id (assoc track :keyframes (sort-keyframes (concat others rows)))))
                   tracks))
               tracks
               tracks)))
    timeline))

(defn values-at
  "Return `{shape-id {property value}}` with the interpolated value of
  every animated property of every track of `timeline` at `time` (ms).
  Indexed properties (fill, stroke, shadow) nest as `{index value}`. The
  outlines of a path morph into each other (see `match-outlines`)."
  [timeline time]
  (reduce-kv
   (fn [acc shape-id track]
     (let [loops    (get track :loops #{})
           by-slot  (group-by keyframe-slot (:keyframes track))
           values   (reduce-kv
                     (fn [m [property index] keyframes]
                       (let [keyframes (cond-> (sort-keyframes keyframes)
                                         (= :content property) (->> (matched-row [shape-id index])))
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

(declare cut-keyframes)

(defn loop-track
  "`track` repeating every `period` ms from the start: the keyframes of
  each property up to `period`, closed at 0 and at `period` by the values
  they hold there, looping (see `expand-loops`). So the animation of a
  component copy that loops on its own clock plays in the timeline of
  the board it is in (see `app.common.logic.timelines/with-instance-tracks`)."
  [track period]
  (let [by-slot (group-by keyframe-slot (:keyframes track))]
    (assoc track
           :keyframes
           (->> by-slot
                (mapcat (fn [[[property index] keyframes]]
                          (let [keyframes (sort-keyframes keyframes)
                                at        #(make-keyframe {:time %
                                                           :property property
                                                           :index index
                                                           :value (property-value-at keyframes %)})
                                ;; what is before the start or past the
                                ;; period does not repeat
                                kept      (cut-keyframes keyframes 0 period)
                                kept      (if (zero? (:time (first kept) -1)) kept (into [(at 0)] kept))]
                            (cond-> kept
                              (< (:time (peek kept)) period) (conj (at period))))))
                (sort-keyframes))
           :loops
           (into #{} (map (fn [[property index]] (slot-key property index))) (keys by-slot)))))

(defn retime-track
  "`track` played from `start` ms on at `speed` times its pace: its
  keyframes moved and spread out, so the animation of a component copy
  keeps its clock in the timeline of its board (see `copy-clock`)."
  [track start speed]
  (if (and (zero? start) (= 1 speed))
    track
    (update track :keyframes
            (fn [keyframes]
              (mapv #(update % :time (fn [time] (mth/round (+ start (/ time (double speed))))))
                    keyframes)))))

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

(defn- bezier-param
  "Where along the cubic bezier easing `[x1 y1 x2 y2]` its time is `t`
  (0..1): the parameter of the curve there (see `cubic-bezier`)."
  [[x1 _ x2 _] t]
  (loop [lo 0.0 hi 1.0 i 0]
    (let [s (/ (+ lo hi) 2.0)
          x (cubic-bezier-component s x1 x2)]
      (if (or (> i 40) (< (mth/abs (- x t)) 1e-7))
        s
        (if (< x t)
          (recur s hi (inc i))
          (recur lo s (inc i)))))))

(defn- split-easing
  "The easings of the two parts of a segment eased by `easing` cut at its
  linear progress `u` (0..1), each stretched over a segment of its own:
  `[before after]`, the halves of its bezier (de Casteljau), nil for a
  part that is linear or eases nothing. A spring has no bezier: its
  segments are cut in steps (see `split-keyframes-at`)."
  [easing u]
  (when-let [[x1 y1 x2 y2 :as curve] (when-not (or (nil? easing) (= :linear easing))
                                       (easing-curve easing))]
    (let [s     (bezier-param curve u)
          lp    (fn [a b] (+ a (* s (- b a))))
          ax    (lp 0 x1) ay (lp 0 y1)
          bx    (lp x1 x2) by (lp y1 y2)
          cx    (lp x2 1) cy (lp y2 1)
          dx    (lp ax bx) dy (lp ay by)
          ex    (lp bx cx) ey (lp by cy)
          qx    (lp dx ex) qy (lp dy ey)
          ;; the control points of a part from (x0, y0) to (x3, y3), as
          ;; an easing from 0 to 1 in time and progress
          part  (fn [x0 y0 x3 y3 [px1 py1 px2 py2]]
                  (let [w (- x3 x0)
                        h (- y3 y0)]
                    (when (and (> w 1e-9) (> (mth/abs h) 1e-9))
                      {:type :bezier
                       :curve [(mth/clamp (/ (- px1 x0) w) 0 1) (/ (- py1 y0) h)
                               (mth/clamp (/ (- px2 x0) w) 0 1) (/ (- py2 y0) h)]})))]
      [(part 0 0 qx qy [ax ay dx dy])
       (part qx qy 1 1 [ex ey cx cy])])))

(defn- split-segment
  "The segment of the keyframes `from` and `to` of one property cut at
  `time`, strictly between them, as `[from' at to']`: a keyframe `at`
  there with the value it has then, and each part eased and curved as
  that part of the segment was (see `split-easing` and `curved?`). A
  spring is cut in steps first (see `split-keyframes-at`)."
  [from to time]
  (let [u      (/ (double (- time (:time from))) (- (:time to) (:time from)))
        at     (make-keyframe {:time time
                               :property (:property from)
                               :index (:index from)
                               :value (property-value-at [from to] time)})
        from   (dissoc from :applied-tokens)]
    (if (= :step (:interpolation from :linear))
      [from (assoc at :interpolation :step) to]
      (let [[before after] (split-easing (:easing from) u)
            from' (cond-> (dissoc from :easing) (some? before) (assoc :easing before))
            at    (cond-> at
                    (some? after) (assoc :easing after)
                    (some? (:interpolation from)) (assoc :interpolation (:interpolation from)))]
        (if (curved? from to)
          ;; the motion path is a bezier of the values over the eased
          ;; progress: its halves at the progress there
          (let [p   (segment-progress from u)
                lp  (fn [a b] (+ a (* p (- b a))))
                v0  (:value from)
                v3  (:value to)
                v1  (+ v0 (:path-out from 0))
                v2  (+ v3 (:path-in to 0))
                a   (lp v0 v1) b (lp v1 v2) c (lp v2 v3)
                d   (lp a b) e (lp b c)
                q   (lp d e)]
            [(assoc from' :path-out (- a v0))
             (assoc at :value q :path-in (- d q) :path-out (- e q))
             (assoc to :path-in (- c v3))])
          [from' at to])))))

(defn- split-keyframes-at
  "The (time-sorted) `keyframes` of one property with a keyframe at `time`
  where a segment crosses it (see `split-segment`); a spring segment is
  first unrolled into its steps (see `expand-property-springs`)."
  [keyframes time]
  (let [[before after] (split-with #(< (:time %) time) keyframes)
        from           (last before)
        to             (first after)]
    (cond
      (or (nil? from) (nil? to) (= time (:time to)))
      keyframes

      (and (= :spring (:type (:easing from))) (not= :step (:interpolation from)))
      (recur (into (vec (butlast before)) (concat (expand-property-springs [from to]) (rest after)))
             time)

      :else
      (into (vec (butlast before)) (concat (split-segment from to time) (rest after))))))

(defn cut-keyframes
  "The (time-sorted) `keyframes` of one property as they play from `start`
  to `end`: those in between and, where a segment crosses either, a
  keyframe there that ends or starts the part of it inside, eased and
  curved as that part (see `split-segment`). One that holds a single
  value all along keeps a keyframe of it at `start`."
  [keyframes start end]
  (if (every? #(<= start (:time %) end) keyframes)
    keyframes
    (let [kept (->> (-> keyframes
                        (split-keyframes-at start)
                        (split-keyframes-at end))
                    (filterv #(<= start (:time %) end)))]
      (if (seq kept)
        kept
        [(make-keyframe {:time start
                         :property (:property (first keyframes))
                         :index (:index (first keyframes))
                         :value (property-value-at keyframes start)})]))))

(defn cut-timeline
  "`timeline` with the keyframes of each property cut to what plays, from
  its start to its end (see `cut-keyframes`): what lies before the start
  or past the end does not play, but sets where the animation is at
  either. For the exports that write the keyframes as they are; run it
  after `expand-loops`, whose loops repeat what is cut."
  [timeline]
  (let [duration (:duration timeline 0)]
    (update timeline :tracks d/update-vals
            (fn [{:keys [keyframes] :as track}]
              (if (every? #(<= 0 (:time %) duration) keyframes)
                track
                (assoc track :keyframes
                       (->> (group-by keyframe-slot keyframes)
                            (mapcat (fn [[_ keyframes]]
                                      (cut-keyframes (sort-keyframes keyframes) 0 duration)))
                            (sort-keyframes))))))))

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

(defn has? [values & props]
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

(defn as-vec
  [items]
  (if (vector? items) items (vec items)))

(def appearance-attrs
  "The attributes of a shape the animation changes with `:change-property`
  modifiers (see `shape->modifiers`): how it looks, and the poses of the
  units of a text (`:text-units`, see `text-unit-entries`), and the
  outline of a path (`:content`, where it rests). Its geometry takes the
  transform path."
  #{:opacity :r1 :r2 :r3 :r4 :fills :strokes :shadow :blur :background-blur
    :trim-start :trim-end :trim-offset :text-units :content})

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

(defn with-appearance
  "`shape` with the appearance `changes` (see `appearance-changes`), its
  box fitting the outline when one is its (see `path/update-geometry`),
  so that its filters and paints cover it."
  [shape changes]
  (cond-> (merge shape changes)
    (and (contains? changes :content) (= :path (:type shape)))
    (path/update-geometry)))

(defn apply-appearance-modifiers
  "Apply the appearance half of `modifiers` to `objects`, so SVG can paint
  the animated fills, strokes, shadows, blurs, trims and outlines (see
  `with-appearance`). Geometry stays on the transform path."
  [objects modifiers]
  (reduce-kv
   (fn [objects id {:keys [modifiers]}]
     (if-let [changes (appearance-changes modifiers)]
       (d/update-when objects id with-appearance changes)
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

(defn paint-fills
  "The fills `shape` paints in, which an animation of its fills plays on: a
  text has none of its own, it paints its glyphs in those of its spans, the
  ones of the first (which its design tab shows), or the default ones of a
  text when its spans have none, played on all of them (render-wasm paints
  its spans with them, see
  `app.render-wasm.api/set-shape-text-fills-override`)."
  [shape]
  (if (= :text (:type shape))
    (or (some :fills (txt/node-seq txt/is-text-node? (:content shape)))
        txt/default-text-fills)
    (:fills shape)))

(defn apply-fills
  [fills values]
  (patch-indexed fills
                 (cond-> {}
                   (contains? values :fill-color) (assoc :fill-color (:fill-color values))
                   (contains? values :fill-opacity) (assoc :fill-opacity (:fill-opacity values)))
                 solid-color-fill?))

(defn apply-strokes
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

(defn apply-shadows
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

(defn apply-blur
  "`blur` (a layer blur, or nil) with the radius `value`: a layer without a
  blur gets one while it is blurred, as a blur animation blurs any layer."
  [blur value]
  (cond
    (nil? value)  blur
    (some? blur)  (assoc blur :value value)
    (pos? value)  {:id uuid/zero :type :layer-blur :value value :hidden false}
    :else         nil))

(defn position-origin
  "Keyframe positions (`:x`/`:y`) are relative to the board of the
  timeline, so moving the board keeps its animation; the positions of the
  board itself are relative to the canvas."
  [timeline objects shape-id]
  (let [board-id (:board-id timeline)
        selrect  (when (not= shape-id board-id)
                   (dm/get-in objects [board-id :selrect]))]
    (gpt/point (or (:x selrect) 0) (or (:y selrect) 0))))

(defn axis-scale
  "Scale for one axis. An absolute `:width` or `:height` is the target
  size over the current size; a scale keyframe multiplies that."
  [base values size-key scale-key]
  (let [size  (get values size-key)
        scale (get values scale-key 1)
        from-size (if (and (some? size) (pos? base))
                    (/ (double size) (double base))
                    1.0)]
    (* from-size scale)))

(declare ^:private shape-modifiers*)

(defn shape->modifiers
  "Build a single modifiers record for `shape` from the interpolated
  `values` (positions relative to `origin`), turning and scaling around
  its normalized `pivot` (see `track-origin`; its middle when nil).
  Position/scale/rotation become geometry modifiers; opacity becomes a
  `:change-property` structure modifier so it rides the same modifier
  pipeline in every renderer (SVG via `transform-shape`, WASM via
  `set-shape-opacity`) and in export. The outline of a path, a keyframe
  of the row of `track` (see `ctao/outline->content`), is its content where
  it rests, which the transform moves, turns and scales then."
  ([shape values origin pivot]
   (shape->modifiers shape values origin pivot nil))
  ([shape values origin pivot track]
   (shape-modifiers* shape values origin pivot track)))

(defn- shape-modifiers*
  [shape values origin pivot track]
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
        box      (box-point shape (or pivot default-transform-origin))
        pivot    (origin-point shape (or pivot default-transform-origin))
        turned?  (and (some? (:transform shape)) (not (gmt/unit? (:transform shape))))
        ;; a scale scales what the shape holds, its strokes, shadows and
        ;; text too, as the scale tool does and the exports play it; a
        ;; width or height resizes it, its layers kept by their
        ;; constraints
        scale    (let [x (mth/abs (get values :scale-x 1))
                       y (mth/abs (get values :scale-y 1))]
                   (if (mth/close? x y) x (mth/sqrt (* x y))))]
    ;; The shape scales and turns around its pivot where it is, then
    ;; moves, like the CSS and Lottie exports: moving first would make it
    ;; turn around the place it left. A turned one scales along its own
    ;; sides, as its width and height change in the design tab.
    (cond-> (ctm/empty)
      (and (has? values :scale-x :scale-y :width :height) turned?)
      (ctm/resize (gpt/point sx sy)
                  (gmt/transform-point-center box (gpt/point (+ base-x (/ base-w 2)) (+ base-y (/ base-h 2)))
                                              (:transform shape))
                  (:transform shape)
                  (:transform-inverse shape))

      (and (has? values :scale-x :scale-y :width :height) (not turned?))
      (ctm/resize (gpt/point sx sy) box)

      (and (has? values :scale-x :scale-y) (not (mth/close? scale 1)))
      (ctm/scale-content scale)

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
      (ctm/change-property :fills (apply-fills (paint-fills shape) values))

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
                         trim-properties))

      (and (contains? values :content) (= :path (:type shape)))
      (ctm/change-property :content (ctao/outline->content shape track (:content values))))))

(defn rest-value
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
    :blur     (or (dm/get-in shape [:blur :value]) 0)
    nil))

(defn- effect-value
  "`base` with the effect of `animation` on `property` fully applied."
  [{:keys [type amount offset-x offset-y]} property base]
  (case type
    :fade   (* base (or amount 0))
    :move   (+ base (or (if (= property :x) offset-x offset-y) 0))
    :scale  (* base (or amount 1))
    :rotate (+ base (or amount 0))
    :blur   (+ base (or amount 0))
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

(defn unit-split-kind
  "The kind of units (`:char`, `:word` or `:line`) that the split
  animations among `animations` (presets, see `track-animations`) play one
  by one on `shape`, or nil when they play as animations of the whole
  layer: on a layer that is no text, and on a text with more units than
  `tu/live-unit-cap`. A text plays one kind, that of its first split
  animation; the split animations of another kind play whole."
  [shape animations]
  (when (= :text (:type shape))
    (when-let [kind (some :split animations)]
      (when (<= (tu/unit-count shape kind) tu/live-unit-cap)
        kind))))

(declare rest-pose keys-entry-keyframes)

(defn row-with-entries
  "The keyframes a row (a property and index) of a layer resting at `pose`
  plays, in the time of its track: `own`, its own keyframes (by time, a
  loop unrolled), and those of the keyframe entries `entries` of that row
  (see `keys-entry-keyframes`), the later entry winning where two
  overlap. The own keyframes win in their span, from the first to the
  last of them, so an edit on the layer shows at once; the entries play
  outside it."
  [own entries pose]
  (let [own     (vec own)
        from    (:time (first own))
        to      (:time (peek own))
        inside? #(and (some? from) (<= from (:time %) to))
        played  (reduce (fn [keyframes {:keys [start] :as entry}]
                          (let [end (animation-end entry)]
                            (into (filterv #(not (<= start (:time %) end)) keyframes)
                                  (keys-entry-keyframes entry pose))))
                        []
                        (sort-by :start entries))]
    (sort-keyframes (into own (remove inside?) played))))

(defn take-over-keys
  "`track` with the keyframe entries `entries` (in its time, as its style
  uses play them, see `expand-style`) played in their rows by a layer
  resting at `pose` (see `row-with-entries`: they play in their spans but
  where the row's own keyframes are). A looping row is unrolled over
  `duration` first, and loops no more."
  [track entries pose duration]
  (let [slots   (into #{} (map keyframe-slot) entries)
        loops   (get track :loops #{})
        by-slot (group-by keyframe-slot (:keyframes track))

        take-over
        (fn [[property index :as slot]]
          (let [own (sort-keyframes (get by-slot slot))
                own (cond-> own
                      (and (seq own) (contains? loops (slot-key property index)))
                      (expand-property-loop duration))]
            (row-with-entries own (filter #(= slot (keyframe-slot %)) entries) pose)))]
    (-> (reduce (fn [track [property index]]
                  (update-slot-flag track :loops disj (slot-key property index)))
                track
                slots)
        (assoc :keyframes
               (sort-keyframes
                (into (filterv #(not (contains? slots (keyframe-slot %))) (:keyframes track))
                      (mapcat take-over)
                      slots))))))

(defn- resolve-track
  "`track` of `shape-id` as it plays: the keyframe entries of its style
  uses playing in their rows (see `take-over-keys`), then
  its presets turned into the keyframes they stand for, over those, but
  the split animations that play the units of a text one by one (see
  `unit-split-kind`), which stay its `:animations` (see
  `text-unit-entries`)."
  [timeline objects shape-id track]
  (let [shape      (get objects shape-id)
        animations (track-animations track)
        kind       (unit-split-kind shape animations)
        by-units?  #(and (some? kind) (= kind (:split %)))
        splits     (filterv by-units? animations)
        animations (remove by-units? animations)
        entries    (filterv keys-entry? animations)
        animations (remove keys-entry? animations)
        track      (cond-> track
                     (and (seq entries) (some? shape))
                     (take-over-keys entries (rest-pose timeline objects shape-id) (:duration timeline)))
        keyframes  (:keyframes track)]
    (cond-> (if (or (empty? animations) (nil? shape))
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
                                  properties))))))
      (seq splits)
      (assoc :animations splits))))

(defn visible-track
  "`track` without its hidden keyframes and animations."
  [{:keys [hidden animations] :as track}]
  (cond-> (dissoc track :hidden)
    (seq hidden)
    (update :keyframes (partial filterv #(not (contains? hidden (slot-key (:property %) (:index %))))))

    (some :hidden animations)
    (update :animations (partial filterv (complement :hidden)))))

(defn resolve-animations
  "Return `timeline` as it plays: its hidden rows left out and its preset
  animations turned into the keyframes they stand for, but the split
  animations that play the units of a text one by one, which stay the
  `:animations` of its track (see `text-unit-entries`), and the outlines
  of a path matched to morph into each other (see `match-outlines`).
  Everything that reads keyframes uses it: the preview, the playback and
  the exports."
  [timeline objects]
  (match-outlines
   (if (some #(or (seq (:animations %)) (seq (:hidden %))) (vals (:tracks timeline)))
     (update timeline :tracks
             (fn [tracks]
               (reduce-kv (fn [tracks shape-id track]
                            (let [track (resolve-track timeline objects shape-id (visible-track track))]
                              ;; Nothing left to play once all of it is hidden;
                              ;; a track that follows, reaches or holds the
                              ;; pivot others turn around plays still.
                              (if (and (empty? (:keyframes track)) (empty? (:animations track))
                                       (nil? (:follow track)) (nil? (:ik track)) (nil? (:origin track)))
                                (dissoc tracks shape-id)
                                (assoc tracks shape-id track))))
                          tracks
                          tracks)))
     timeline)))

(def ^:private unit-rest
  "The values of a unit of a text at rest, where the text lays it out,
  which the effects of its split animations start from."
  {:x 0 :y 0 :rotation 0 :scale-x 1 :scale-y 1 :opacity 1 :blur 0})

(defn- resolve-unit
  "`{property keyframes}` of a unit of a text playing `animations`, the
  ones its split animations give it (see `tu/unit-animations`), from its
  rest values, the later one taking over where two overlap, as on a layer
  (see `resolve-property`)."
  [animations]
  (let [animations (sort-by :start animations)]
    (into {}
          (map (fn [property]
                 (let [covers (filter #(some #{property} (animation-properties (:type %))) animations)]
                   [property (resolve-property [] covers property (constantly (get unit-rest property)))])))
          (into #{} (mapcat (comp animation-properties :type)) animations))))

(defn- unit-entry
  "The entry of a unit, `base` (its kind and place, see `unit-plans`),
  with `keyframes` (see `resolve-unit`) at `time`, or nil when it is at
  rest then."
  [base keyframes time]
  (let [value    (fn [property]
                   (if-let [keyframes (get keyframes property)]
                     (property-value-at keyframes time)
                     (get unit-rest property)))
        x        (value :x)
        y        (value :y)
        rotation (value :rotation)
        scale-x  (value :scale-x)
        scale-y  (value :scale-y)
        opacity  (value :opacity)
        blur     (value :blur)]
    (when-not (and (== x 0) (== y 0) (== rotation 0) (== scale-x 1) (== scale-y 1) (== opacity 1)
                   (== blur 0))
      (assoc base :dx x :dy y :rotation rotation :scale-x scale-x :scale-y scale-y :opacity opacity
             :blur blur))))

(defrecord UnitPlan [base keyframes from to before after])

(defn- unit-plan
  "How the unit `base` plays the unit animations `animations` (see
  `tu/unit-animations`): its keyframes (see `resolve-unit`), from when to
  when they change it, and the entries it holds before and after, so a
  frame only works out the units on their way (see `text-unit-entries`)."
  [base animations]
  (let [keyframes (resolve-unit animations)
        times     (into [] (comp (mapcat val) (map :time)) keyframes)]
    (if (empty? times)
      (UnitPlan. base keyframes 0 0 nil nil)
      (let [from (reduce min times)
            to   (reduce max times)]
        (UnitPlan. base keyframes from to (unit-entry base keyframes from) (unit-entry base keyframes to))))))

(defonce ^:private unit-plans-cache
  (atom {}))

(def ^:private unit-plans-cache-size
  "The most texts whose unit plans are kept (see `unit-plans`)."
  256)

(defn- unit-plans
  "The plans (see `unit-plan`) of the units of `kind` of the text
  `shape-id` playing its split `animations`, in unit order: `units`, as
  `tu/units` gives them, for characters and words, `n` lines for lines.
  Kept, one entry per text, until its animations or its units change."
  [shape-id kind units n animations]
  (let [entry (get @unit-plans-cache shape-id)]
    (if (and (= kind (:kind entry))
             (= n (:n entry))
             (= units (:units entry))
             (= animations (:animations entry)))
      (:plans entry)
      (let [by-animation (mapv #(tu/unit-animations % n) animations)
            bases        (if (= :line kind)
                           (mapv (fn [index] {:kind :line :index index :line-count n}) (range n))
                           (mapv (fn [[paragraph start end]]
                                   {:kind kind :paragraph paragraph :start start :end end})
                                 units))
            plans        (mapv (fn [index base]
                                 (unit-plan base (map #(nth % index) by-animation)))
                               (range n)
                               bases)]
        (swap! unit-plans-cache
               (fn [cache]
                 (-> (if (or (contains? cache shape-id) (< (count cache) unit-plans-cache-size))
                       cache
                       {})
                     (assoc shape-id {:kind kind :n n :units units :animations animations :plans plans}))))
        plans))))

(defn text-unit-entries
  "`{shape-id entries}` for every text of `timeline` with split
  animations that play its units one by one (see `unit-split-kind`): the
  units off their rest pose at `time`, as render-wasm poses them (see
  `app.common.render-wasm.serializers.text-units`), `[]` when none is.
  Several split animations of a unit play together, as on a layer. A
  track with a `:split-clock`, the settings of a timeline, plays them at
  the time of that clock (see `playback-time`): the one of the component
  copy it comes from."
  [timeline objects time]
  (reduce-kv
   (fn [result shape-id track]
     (let [shape      (get objects shape-id)
           animations (track-animations track)
           kind       (unit-split-kind shape animations)]
       (if (nil? kind)
         result
         (let [n     (tu/unit-count shape kind)
               units (when (not= :line kind) (tu/units shape kind))
               plans (unit-plans shape-id kind units n (filterv #(= kind (:split %)) animations))
               time  (if-let [clock (:split-clock track)] (playback-time clock time) time)]
           (assoc result shape-id
                  (into []
                        (keep (fn [plan]
                                (cond
                                  (<= time (:from plan)) (:before plan)
                                  (>= time (:to plan))   (:after plan)
                                  :else                  (unit-entry (:base plan) (:keyframes plan) time))))
                        plans))))))
   {}
   (:tracks timeline)))

(defn unit-timelines
  "The units of the texts of `timeline` (as it plays, see
  `resolve-animations`, its copies' tracks too, see
  `app.common.logic.timelines/with-instance-tracks`) whose split
  animations play them one by one (see `unit-split-kind`), as timelines
  of the settings of `timeline`: `{text-id {:kind :n :ids :bases
  :timeline}}`, `:ids` the ids of the tracks of the units in their
  timeline (`[text-id k]`, which no shape has), in unit order, `:bases`
  where they are in the text (see `unit-plans`). Their keyframes are in
  the time of `timeline`, of `:x :y` offsets in px from where a unit
  rests, `:rotation` degrees, `:scale-x :scale-y` and `:opacity` factors
  and `:blur` px; the units of a track with a `:split-clock` (a copy)
  play on that clock, unrolled over `timeline` as its keyframes are, and
  loops are unrolled (see `expand-loops`). What the code and the markup
  exports play the units by (see `timeline->css`)."
  [timeline objects]
  (let [settings (select-keys timeline [:duration :playback :loop])]
    (reduce-kv
     (fn [result text-id track]
       (let [shape      (get objects text-id)
             animations (track-animations track)
             kind       (unit-split-kind shape animations)]
         (if (nil? kind)
           result
           (let [n      (tu/unit-count shape kind)
                 units  (when (not= :line kind) (tu/units shape kind))
                 plans  (unit-plans text-id kind units n (filterv #(= kind (:split %)) animations))
                 ids    (mapv #(vector text-id %) (range n))
                 tracks (zipmap ids
                                (map (fn [id plan]
                                       {:shape-id id
                                        :keyframes (sort-keyframes (into [] (mapcat val) (:keyframes plan)))})
                                     ids
                                     plans))
                 clock  (:split-clock track)
                 units  (assoc timeline :tracks tracks)
                 ;; on the clock of a copy, unrolled as its keyframes are
                 units  (if (nil? clock)
                          units
                          (let [own    (cond-> (merge units clock)
                                         (= :ping-pong (playback-mode (merge units clock)))
                                         (-> expand-springs expand-ping-pong))
                                period (:duration own)]
                            (merge (cond-> own
                                     (= :loop (playback-mode own))
                                     (update :tracks d/update-vals #(loop-track % period)))
                                   settings)))]
             (assoc result text-id {:kind kind
                                    :n n
                                    :ids ids
                                    :bases (mapv :base plans)
                                    :timeline (expand-loops units)})))))
     {}
     (:tracks timeline))))

(defn unit-shape
  "A shape for the unit `id` of a text of `timeline` (see
  `unit-timelines`) to play as a layer in the code and markup exports
  (see `frame-values`): at rest where the positions of its keyframes
  count from, so they are the offsets of the unit, unturned and of no
  size."
  [timeline objects id]
  (let [origin (position-origin timeline objects id)]
    {:id id
     :type :rect
     :selrect {:x (:x origin) :y (:y origin) :width 0 :height 0}
     :rotation 0}))

(defn split-animations?
  "Whether `timeline` plays a text of `objects` by its units (see
  `unit-split-kind`), from a preset or from an entry of a style use: what
  the markup exports play whole (see `flatten-splits`)."
  [timeline objects]
  (boolean
   (some (fn [[shape-id track]]
           (unit-split-kind (get objects shape-id) (track-animations track)))
         (:tracks timeline))))

(defn inside?
  "Whether the layer `id` of `objects` is in the layer `other`, through
  groups and boards."
  [objects id other]
  (loop [at (dm/get-in objects [id :parent-id]), depth 0]
    (cond
      (or (nil? at) (> depth 64)) false
      (= at other)                true
      (= at uuid/zero)            false
      :else                       (recur (dm/get-in objects [at :parent-id]) (inc depth)))))

(defn nested-layers?
  "Whether one of the layers `a` and `b` of `objects` is in the other:
  they move together already, and a follow between them would move one
  twice."
  [objects a b]
  (or (inside? objects a b) (inside? objects b a)))

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
;; RETARGETING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The animation of a layer goes onto another one where that one is:
;; keyframe positions, rotations and sizes are absolute, so they are
;; counted again from where the other layer rests.

(def schema:pose
  "Where a layer rests in the board of its timeline (see `rest-pose`)."
  [:map {:title "AnimationPose"}
   [:x ::sm/safe-number]
   [:y ::sm/safe-number]
   [:width ::sm/safe-number]
   [:height ::sm/safe-number]
   [:rotation ::sm/safe-number]])

(defn rest-pose
  "Where `shape-id` of `objects` rests in the board of `timeline`, as its
  keyframes count it: its position in the board (a board on the canvas,
  see `position-origin`), its size and its rotation."
  [timeline objects shape-id]
  (let [shape   (get objects shape-id)
        origin  (position-origin timeline objects shape-id)
        selrect (:selrect shape)]
    {:x        (- (or (:x selrect) 0) (:x origin))
     :y        (- (or (:y selrect) 0) (:y origin))
     :width    (or (:width selrect) 0)
     :height   (or (:height selrect) 0)
     :rotation (or (:rotation shape) 0)}))

(defn fits-keyframe?
  "Whether `shape` can show `keyframe`: it has the fill, stroke or shadow
  slot the keyframe animates (in a flat colour for the colour and opacity
  of fills and strokes), and the radius, blur or strokes (for a trim) it
  animates. Position, size, rotation, scale and opacity fit any shape."
  [shape {:keys [property index]}]
  (case property
    (:fill-color :fill-opacity)
    (boolean (solid-color-fill? (get (as-vec (:fills shape)) index)))

    (:stroke-color :stroke-opacity)
    (boolean (solid-color-stroke? (get (as-vec (:strokes shape)) index)))

    :stroke-width
    (some? (get (as-vec (:strokes shape)) index))

    (:shadow-offset-x :shadow-offset-y :shadow-blur :shadow-spread :shadow-color :shadow-opacity)
    (some? (get (as-vec (:shadow shape)) index))

    (:r1 :r2 :r3 :r4)
    (ctsr/has-radius? shape)

    :blur
    (some? (:blur shape))

    :background-blur
    (some? (:background-blur shape))

    (:trim-start :trim-end :trim-offset)
    (d/not-empty? (:strokes shape))

    :content
    (= :path (:type shape))

    true))

(defn keyframe-value-fits?
  "Whether the value of `keyframe` is of the kind its property takes: a
  hex colour for a colour, an outline (see `ctao/outline-fits?`) for the
  outline of a path, a finite number for the others."
  [{:keys [property value]}]
  (cond
    (color-property? property) (clr/hex-color-string? value)
    (= :content property)      (ctao/outline-fits? value)
    :else                      (mth/finite? value)))

(defn- relative-value
  "`value` of `property`, of a layer resting at `from`, for a layer
  resting at `to`: positions and rotation moved as far as `to` rests from
  `from`, sizes scaled as much as it is bigger (as the copies of a
  component take the animation of their main). Scales are factors
  already, and the other properties look the same on any layer."
  [property value from to]
  (let [ratio (fn [k]
                (let [base (get from k)]
                  (if (and (number? base) (pos? base)) (/ (double (get to k)) base) 1)))]
    (case property
      :x        (+ value (- (:x to) (:x from)))
      :y        (+ value (- (:y to) (:y from)))
      :rotation (+ value (- (:rotation to) (:rotation from)))
      :width    (* value (ratio :width))
      :height   (* value (ratio :height))
      value)))

(def ^:private neutral-pose
  "The pose the values of keyframe entries are counted from (see
  `schema:keys-entry`): positions and rotation from 0, sizes as factors of
  1."
  {:x 0 :y 0 :rotation 0 :width 1 :height 1})

(defn keyframes->keys-entry
  "A keyframe entry (see `schema:keys-entry`) of `keyframes`, those of one
  row (property and index) of a layer resting at `pose` (see `rest-pose`):
  from the first one, as long as they last, their values relative to the
  layer (see `relative-value`), so that it plays on any layer from where
  that one rests, as a pasted animation does."
  [keyframes pose]
  (let [keyframes (sort-keyframes keyframes)
        {:keys [property index]} (first keyframes)
        start     (:time (first keyframes))]
    ;; made in the time of the entry, then put where the first one is,
    ;; before the start of the timeline too
    (-> (make-style-entry
         {:type :keys
          :property property
          :index index
          :start 0
          :duration (- (:time (peek keyframes)) start)
          :keyframes (mapv (fn [keyframe]
                             (-> keyframe
                                 (update :time - start)
                                 (update :value #(relative-value property % pose neutral-pose))))
                           keyframes)})
        (assoc :start start))))

(defn keys-entry-keyframes
  "The keyframes the keyframe entry `entry` (in the time of a track, see
  `expand-style`) gives a layer resting at `pose` (see `rest-pose`): from
  its start, their values the layer's own (see `relative-value`)."
  [{:keys [property index start keyframes]} pose]
  (mapv (fn [keyframe]
          (make-keyframe (assoc keyframe
                                :time (+ start (:time keyframe))
                                :property property
                                :index index
                                :value (relative-value property (:value keyframe) neutral-pose pose))))
        keyframes))

(defn retarget-track
  "`track`, of a layer resting at `from` (see `rest-pose`), for the shape
  `target` resting at `to`: its positions and rotation counted from where
  `target` rests and its sizes scaled to it (see `relative-value`), or
  its values as they are with `exact?`; its outlines (see `ctao/outline->content`)
  scale to the box of `target`, or keep their size with `exact?`. The
  keyframes `target` cannot
  show (see `fits-keyframe?`) are left out, and so are the split presets
  (see `schema:preset-animation`) when it is no text; a style use goes
  whole, with all its entries. The track gets new keyframe and animation
  ids and keeps the loops of the rows left and its pivot; nothing in it
  is hidden or locked. `{:track :skipped}`, `:skipped` being the number
  of keyframes and presets left out."
  [track from target to & {:keys [exact?]}]
  (let [keyframes  (:keyframes track)
        fits       (filterv #(fits-keyframe? target %) keyframes)
        animations (:animations track)
        kept       (if (= :text (:type target))
                     animations
                     (filterv #(or (style-use? %) (nil? (:split %))) animations))

        retarget
        (fn [{:keys [property] :as keyframe}]
          (make-keyframe
           (cond-> (dissoc keyframe :id)
             (not exact?) (update :value #(relative-value property % from to)))))

        outlines? (some #(= :content (:property %)) fits)

        track
        (-> track
            (assoc :shape-id (:id target))
            (assoc :keyframes (sort-keyframes (mapv retarget fits)))
            (dissoc :hidden :locked :animations)
            (cond-> (not outlines?) (dissoc :outline-box))
            ;; with their size: made for the box of the target as they
            ;; are; else scaled with it
            (cond-> (and outlines? exact?)
              (assoc :outline-box (ctao/outline-box target)))
            (cond-> (seq kept)
              (assoc :animations (mapv #(make-animation (dissoc % :id :hidden :locked)) kept)))
            (prune-slot-flags))]
    {:track track
     :skipped (+ (- (count keyframes) (count fits))
                 (- (count animations) (count kept)))}))

(def ^:private pivot-properties
  "The properties that scale, resize or turn a layer around its pivot (the
  `:origin` of its track)."
  #{:rotation :scale-x :scale-y :width :height})

(defn- pivot-motion?
  "Whether `track` scales, resizes or turns its layer: what its pivot is
  the centre of. A style use does when one of its entries does, a preset
  or a keyframe entry."
  [{:keys [keyframes animations]}]
  (boolean (or (some #(contains? pivot-properties (:property %)) keyframes)
               (some #(if (keys-entry? %)
                        (contains? pivot-properties (:property %))
                        (contains? #{:scale :rotate} (:type %)))
                     (expand-uses animations)))))

(defn- merge-animation
  "Add `animation` (see `make-animation`) to the track of `shape-id`, in
  place of the one it has of the same type and direction (for a style
  use, of the same style) starting at the same time, as a keyframe takes
  the place of the one of its row at the same time (see `add-keyframe`);
  that one keeps its row. A locked one stays, and `animation` is left
  out."
  [timeline shape-id animation]
  (let [{:keys [type direction start style-ref-id] :as animation} (make-animation animation)

        animations (dm/get-in timeline [:tracks shape-id :animations])
        index      (d/index-of-pred animations #(and (= type (:type %))
                                                     (= direction (:direction %))
                                                     (= start (:start %))
                                                     (= style-ref-id (:style-ref-id %))))]
    (cond
      (nil? index)
      (add-animation timeline shape-id animation)

      (:locked (nth animations index))
      timeline

      :else
      (-> timeline
          (assoc-in [:tracks shape-id :animations index] animation)
          (update :duration max (animation-end animation))))))

(defn- paste-track*
  "Paste `track` (see `retarget-track`) onto `shape-id` of `timeline`.
  `:replace` first removes the unlocked keyframes of the rows `track` has
  keyframes of and, when it has presets or style uses, the unlocked
  presets and uses of `shape-id`; `:merge` removes nothing. Either way a
  keyframe takes the place of the one of its row at the same time, and a
  preset the one of the same type and direction starting at the same
  time (a use, the one of the same style; see `merge-animation`), so
  pasting twice at the same time leaves no doubles. Locked rows and
  presets stay, and take nothing. The rows of `track` loop as they did.
  Its pivot goes on when it scales, resizes or turns and `shape-id` does
  not anymore; otherwise `shape-id` keeps its own, so its own motion
  plays as before. The duration grows to fit it.
  `{:timeline :pivot-kept?}`."
  [timeline shape-id track mode]
  (let [slots   (into #{} (map keyframe-slot) (:keyframes track))

        cleared
        (if (= mode :replace)
          (let [timeline (reduce (fn [timeline [property index]]
                                   (remove-property timeline shape-id property index))
                                 timeline
                                 slots)]
            (if (seq (:animations track))
              (reduce #(remove-animation %1 shape-id (:id %2))
                      timeline
                      (dm/get-in timeline [:tracks shape-id :animations]))
              timeline))
          timeline)

        own         (get-track cleared shape-id)
        locked      (get own :locked #{})
        pivot?      (pivot-motion? track)
        pivot-kept? (and pivot? (pivot-motion? own))
        end         (reduce max 0 (track-times track))

        ;; outlines go into the box of the row they join (see
        ;; `ctao/outline->content`), a new row taking theirs
        box         (:outline-box own)
        keyframes   (cond-> (:keyframes track)
                      (and (some? box) (some? (:outline-box track)))
                      (ctao/rescale-outlines (:outline-box track) box))

        pasted
        (as-> cleared $
          (reduce #(add-keyframe %1 shape-id %2) $ keyframes)
          (cond-> $
            (and (nil? (dm/get-in $ [:tracks shape-id :outline-box])) (some? (:outline-box track)))
            (assoc-in [:tracks shape-id :outline-box] (:outline-box track)))
          (reduce #(merge-animation %1 shape-id %2) $ (:animations track))
          (reduce #(d/update-in-when %1 [:tracks shape-id] update-slot-flag :loops conj %2)
                  $
                  (remove locked (:loops track)))
          (cond-> $
            (and pivot? (not pivot-kept?))
            (set-track-origin shape-id
                              (dm/get-in track [:origin :x] 0.5)
                              (dm/get-in track [:origin :y] 0.5)))
          (update $ :duration max end))]
    {:timeline pasted
     :pivot-kept? pivot-kept?}))

(defn paste-track
  "Paste `track` (see `retarget-track`) onto `shape-id` of `timeline` as
  `paste-track*` does, `{:timeline :pivot-kept? :outlines-left-out}`: a
  merge whose outlines would take the row of `shape-id` past what it
  holds (see `ctao/outline-row-fits?`) leaves the outlines of `track`
  out, which `:outlines-left-out` counts, and pastes the rest."
  [timeline shape-id track mode]
  (let [pasted (paste-track* timeline shape-id track mode)]
    (if (ctao/outline-row-fits? (dm/get-in pasted [:timeline :tracks shape-id :keyframes]))
      (assoc pasted :outlines-left-out 0)
      (let [outline? #(= :content (:property %))]
        (-> (paste-track* timeline shape-id
                          (-> track
                              (update :keyframes #(filterv (complement outline?) %))
                              (dissoc :outline-box))
                          mode)
            (assoc :outlines-left-out (count (filter outline? (:keyframes track)))))))))

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
  are the objects before and after the edit. A shape whose text changes
  records nothing, nor does a path reshaped (moved, turned or resized it
  records as any layer, see `ctao/outline-change`) or a boolean whose outline
  changes, nor fills, strokes and shadows added or removed."
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
                 (if (and (= :path (:type old-shape)) (= :path (:type new-shape)))
                   (and (not= (:content old-shape) (:content new-shape))
                        (= :reshape (ctao/outline-change old-shape new-shape)))
                   (not= (:content old-shape) (:content new-shape))))
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
  "Drop the tracks targeting any of `shape-ids`, the follows of the
  layers that follow them and the goals of those that reach them, and the
  clocks of the component copies among them (used when shapes are
  deleted). Returns the updated timeline."
  [timeline shape-ids]
  (let [removed  (set shape-ids)
        timeline (update timeline :tracks #(apply dissoc % shape-ids))
        timeline (reduce-kv (fn [timeline id track]
                              (cond-> timeline
                                (contains? removed (dm/get-in track [:follow :leader])) (set-follow id nil)
                                (contains? removed (dm/get-in track [:ik :goal]))       (set-ik id nil)))
                            timeline
                            (:tracks timeline))]
    (if (contains? timeline :copies)
      (let [copies (apply dissoc (:copies timeline) shape-ids)]
        (if (empty? copies) (dissoc timeline :copies) (assoc timeline :copies copies)))
      timeline)))
