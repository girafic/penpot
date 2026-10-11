;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.types.animation-code
  "The animation of a board as code shows it (Penpot Motion): the layers
  it animates, each on the clock it plays on (see `code-layers`, on
  `cltl/code-timelines`), which the code of the Inspect (CSS, JSON,
  React), its timeline and the plugin API read, so they cannot drift
  apart. Pure, for the workspace, the view mode and the plugins alike."
  (:require
   [app.common.animation.css :as acss]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.text-units :as tu]
   [cuerdas.core :as str]))

(def property-names
  "The animatable properties as the code and the plugin API name them,
  camelCase."
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

(def token-setting-names
  "The settings of a style entry a design token sets (see
  `cta/schema:animation-tokens`), as the code and the plugin API name
  them."
  {:start "start"
   :duration "duration"
   :easing "easing"
   :amount "amount"
   :offset-x "offsetX"
   :offset-y "offsetY"})

(def property-order
  "The animatable properties in the order of the rows of the dock, which
  the code follows."
  [:x :y :width :height :scale-x :scale-y :rotation
   :opacity :r1 :r2 :r3 :r4
   :fill-color :fill-opacity
   :stroke-color :stroke-opacity :stroke-width
   :shadow-offset-x :shadow-offset-y :shadow-blur
   :shadow-spread :shadow-color :shadow-opacity
   :blur :background-blur
   :trim-start :trim-end :trim-offset])

(defn- clock-layers
  "`layers` (by id) with those `timeline` animates and `layers` has not
  yet: their entries (see `code-layers`), `head-id` the copy whose main's
  animation they play."
  [layers timeline head-id objects splits]
  (let [resolved (cta/resolve-animations timeline objects)
        units    (cta/unit-timelines resolved objects)]
    (reduce-kv
     (fn [layers id track]
       (if (contains? layers id)
         layers
         (assoc layers id
                (cond-> {:id     id
                         :shape  (get objects id)
                         :clock  timeline
                         :source (cta/visible-track (dm/get-in timeline [:tracks id]))
                         :track  track
                         :rest   (cta/rest-pose timeline objects id)
                         :origin (cta/track-origin track)}
                  (some? head-id)       (assoc :copy {:head-id head-id
                                                      :name (dm/get-in objects [head-id :name])})
                  (contains? splits id) (assoc :split (get splits id))
                  (contains? units id)  (assoc :units (get units id))))))
     layers
     (:tracks resolved))))

(defn code-layers
  "The layers `code` (see `cltl/code-timelines`) animates, in the order
  of the layers panel (the board, then its layers top-most first, depth
  first), only those of `ids` when given. Each is `{:id :shape :clock
  :source :track :rest :origin}`: `:clock` the timeline it plays on,
  `:source` its track as designed (keyframes and presets, the hidden ones
  left out), `:track` as it plays (see `cta/resolve-animations`), `:rest`
  its rest pose (see `cta/rest-pose`) and `:origin` its pivot (see
  `cta/track-origin`); a layer of a component copy playing its main's
  animation has `:copy` `{:head-id :name}`, a text the design plays by
  units `:split`, the kind, and one the code plays by units `:units` (see
  `cta/unit-timelines`)."
  [{:keys [board-id timeline copies splits]} objects & {:keys [ids]}]
  (let [layers (reduce (fn [layers {:keys [timeline head-id]}]
                         (clock-layers layers timeline head-id objects splits))
                       (clock-layers {} timeline nil objects splits)
                       copies)]
    (into []
          (keep (fn [id]
                  (when (or (nil? ids) (contains? ids id))
                    (get layers id))))
          (cltl/layer-ids objects board-id))))

;; JSON

(defn- json-number
  "`v` rounded to 3 decimals, a whole one as an integer, so that the JVM
  and JS give equal data."
  [v]
  (let [v (mth/precision (double v) 3)]
    (if (== v (mth/floor v))
      (long v)
      v)))

(defn- json-value
  [v]
  (if (number? v) (json-number v) v))

(defn- json-easing
  "The easing of the segment `keyframe` (or a preset) starts, as the JSON
  gives it: a hold, a spring as stored, or a cubic bezier, named after the
  CSS easing it is; none is linear."
  [{:keys [easing interpolation]}]
  (cond
    (= :step interpolation)
    (d/ordered-map "type" "hold")

    (= :spring (:type easing))
    (d/ordered-map "type" "spring"
                   "stiffness" (json-number (:stiffness easing))
                   "damping" (json-number (:damping easing))
                   "mass" (json-number (:mass easing)))

    :else
    (let [named (if (nil? easing) :linear (when (keyword? easing) easing))]
      (cond-> (d/ordered-map "type" "bezier"
                             "curve" (mapv json-number (cta/easing-curve (or named easing))))
        (some? named) (assoc "name" (name named))))))

(defn- json-keyframes
  "The keyframes of one property, by time: each but the last with the
  easing of the segment it starts, and the token that sets it."
  [keyframes]
  (let [keyframes  (vec (sort-by :time keyframes))
        last-index (dec (count keyframes))]
    (into []
          (map-indexed
           (fn [i {:keys [time value path-in path-out applied-tokens] :as keyframe}]
             (cond-> (d/ordered-map "time" (json-number time)
                                    "value" (json-value value))
               (< i last-index)  (assoc "easing" (json-easing keyframe))
               (and (< i last-index) (:easing applied-tokens))
               (assoc "tokens" (d/ordered-map "easing" (:easing applied-tokens)))
               (some? path-in)   (assoc "pathIn" (json-number path-in))
               (some? path-out)  (assoc "pathOut" (json-number path-out)))))
          keyframes)))

(defn- json-properties
  "The keyframes of `track` by property (and index), in the order of the
  dock; a property that repeats has `loop` and one cycle."
  [{:keys [keyframes loops]}]
  (let [order (into {} (map-indexed (fn [i property] [property i])) property-order)]
    (->> (group-by (juxt :property :index) keyframes)
         (sort-by (fn [[[property index] _]] [(get order property (count order)) (or index -1)]))
         (mapv (fn [[[property index] keyframes]]
                 (cond-> (d/ordered-map "property" (get property-names property (name property)))
                   (some? index)
                   (assoc "index" index)

                   (contains? loops (cta/slot-key property index))
                   (assoc "loop" true)

                   :always
                   (assoc "keyframes" (json-keyframes keyframes))))))))

(defn- json-preset
  "A preset animation as the JSON lists it, with the name of the style it
  comes from and the tokens that set its settings."
  [{:keys [type direction start duration easing amount offset-x offset-y applied-tokens] :as preset}
   style-name]
  (cond-> (d/ordered-map "type" (name type)
                         "direction" (name (or direction :in))
                         "start" (json-number start)
                         "duration" (json-number duration))
    (some? easing)     (assoc "easing" (json-easing preset))
    (some? amount)     (assoc "amount" (json-number amount))
    (some? offset-x)   (assoc "offsetX" (json-number offset-x))
    (some? offset-y)   (assoc "offsetY" (json-number offset-y))
    (some? style-name) (assoc "style" style-name)
    (seq applied-tokens)
    (assoc "tokens" (into (d/ordered-map)
                          (keep (fn [key]
                                  (when-let [token-name (get applied-tokens key)]
                                    [(get token-setting-names key) token-name])))
                          (keys token-setting-names)))))

(defn- json-presets
  "The visible presets of `source`, the track as designed: a use of an
  animation style as the presets it plays, in timeline time. Its keyframe
  entries are no presets: their keyframes are in the properties, which
  give everything as it plays."
  [source]
  (into []
        (mapcat (fn [animation]
                  (if (cta/style-use? animation)
                    (->> (cta/expand-style animation)
                         (remove cta/keys-entry?)
                         (map #(json-preset % (:name animation))))
                    [(json-preset animation nil)])))
        (:animations source)))

(defn- type-name
  "The type of `shape` as the code and the plugin API name it."
  [shape]
  (when-let [type (:type shape)]
    (case type
      :frame  "board"
      :rect   "rectangle"
      :circle "ellipse"
      :bool   "boolean"
      (name type))))

(defn- json-units
  "The units a text plays one by one (see `cta/unit-timelines`): their
  kind, how many, and the keyframes of each, by property, from where it
  rests."
  [{:keys [kind n ids timeline]}]
  (d/ordered-map "kind" (name kind)
                 "count" n
                 "keyframes" (into []
                                   (map-indexed (fn [k id]
                                                  (d/ordered-map "index" k
                                                                 "properties" (json-properties
                                                                               (get-in timeline [:tracks id])))))
                                   ids)))

(defn- json-layer
  [{:keys [id shape clock source track rest origin copy split units]}]
  (cond-> (d/ordered-map "id" (str id)
                         "name" (or (:name shape) "")
                         "type" (type-name shape)
                         "rest" (d/ordered-map "x" (json-number (:x rest))
                                               "y" (json-number (:y rest))
                                               "width" (json-number (:width rest))
                                               "height" (json-number (:height rest))
                                               "rotation" (json-number (:rotation rest)))
                         "origin" (d/ordered-map "x" (json-number (:x origin))
                                                 "y" (json-number (:y origin))))
    (some? split)
    (assoc "split" (name split))

    (some? units)
    (assoc "units" (json-units units))

    (some? copy)
    (assoc "component" (d/ordered-map "copy" (str (:head-id copy))
                                      "name" (or (:name copy) "")
                                      "duration" (json-number (:duration clock))
                                      "playback" (name (cta/playback-mode clock))))

    :always
    (assoc "properties" (json-properties track)
           "presets" (json-presets source))))

(defn- json-clip
  [{:keys [name start duration offset speed volume fade-in fade-out muted]}]
  (d/ordered-map "name" (or name "")
                 "start" (json-number start)
                 "duration" (json-number duration)
                 "offset" (json-number (or offset 0))
                 "speed" (json-number (or speed 1))
                 "volume" (json-number (or volume 1))
                 "fadeIn" (json-number (or fade-in 0))
                 "fadeOut" (json-number (or fade-out 0))
                 "muted" (true? muted)))

(defn motion-json
  "The animation of the board of `code` (see `cltl/code-timelines`) as
  JSON data, version 1: string keys in camelCase, in a fixed order, times
  in ms and numbers rounded to 3 decimals (whole ones as integers), so the
  JVM and JS give the same. The board, its duration, playback and markers,
  its animated layers (those of `ids` only when given; see `code-layers`)
  with their rest pose, pivot, keyframes by property (each segment with
  its easing) and presets (those of style uses named after the style and
  with the tokens that set them), a layer of a component copy with the
  clock of its main, a text the design plays by units with the kind (and
  the keyframes of each unit, when the code plays them), and the audio
  clips of the board. Version 1 only ever gains keys."
  [{:keys [board-id timeline] :as code} objects & {:keys [ids]}]
  (let [board (get objects board-id)]
    (d/ordered-map
     "version" 1
     "generator" "Penpot"
     "board" (d/ordered-map "id" (str board-id)
                            "name" (or (:name board) "")
                            "width" (json-number (or (dm/get-in board [:selrect :width]) 0))
                            "height" (json-number (or (dm/get-in board [:selrect :height]) 0)))
     "duration" (json-number (:duration timeline))
     "playback" (name (cta/playback-mode timeline))
     "markers" (mapv (fn [{:keys [name time]}]
                       (d/ordered-map "name" (or name "") "time" (json-number time)))
                     (sort-by :time (:markers timeline)))
     "layers" (mapv json-layer (code-layers code objects :ids ids))
     "audio" (mapv json-clip (ctau/audio-clips timeline)))))

;; REACT (motion.dev)

(def ^:private react-values
  "The values of motion.dev a layer animates, in this order: their names,
  the properties behind each and the key of `acss/frame-values` they show."
  [["x" #{:x} :translate-x]
   ["y" #{:y} :translate-y]
   ["rotate" #{:rotation} :rotate]
   ["scaleX" #{:width :scale-x} :scale-x]
   ["scaleY" #{:height :scale-y} :scale-y]
   ["opacity" #{:opacity} :opacity]
   ["borderRadius" #{:r1 :r2 :r3 :r4} :border-radius]
   ["backgroundColor" #{:fill-color :fill-opacity} :background-color]
   ["borderColor" #{:stroke-color :stroke-opacity} :border-color]
   ["borderWidth" #{:stroke-width} :border-width]
   ["boxShadow" #{:shadow-offset-x :shadow-offset-y :shadow-blur
                  :shadow-spread :shadow-color :shadow-opacity} :box-shadow]
   ["filter" #{:blur} :blur]
   ["backdropFilter" #{:background-blur} :backdrop-blur]])

(def ^:private segment-samples
  "The steps a segment takes when the properties behind a value ease it
  apart (see `value-segments`)."
  8)

(defn- react-number
  [v]
  (acss/format-number v))

(defn- react-time
  "A share of a clock, `times` of motion.dev: rounded to 4 decimals, a
  whole one without a trailing `.0`."
  [v]
  (let [r (mth/precision v 4)]
    (if (== r (mth/round r))
      (str (long r))
      (str r))))

(defn- react-literal
  "The JS of the frame value `v` of the motion.dev value `value`."
  [value v]
  (case value
    "borderRadius" (str "\"" (str/join " " (map #(str (react-number %) "px") v)) "\"")
    ("backgroundColor" "borderColor" "boxShadow") (str "\"" v "\"")
    ("filter" "backdropFilter") (str "\"blur(" (react-number v) "px)\"")
    (react-number v)))

(def ^:private linear-ease "\"linear\"")
(def ^:private hold-ease "steps(1, \"end\")")

(defn- react-ease
  "The motion.dev easing of the segment `keyframe` starts: a hold, linear,
  or the cubic bezier of any other curve, CSS names included."
  [{:keys [easing interpolation]}]
  (cond
    (= :step interpolation) hold-ease
    (or (nil? easing) (= :linear easing)) linear-ease
    :else (str "[" (str/join ", " (map react-number (cta/easing-curve easing))) "]")))

(defn- span-ease
  "How the keyframes of one property (by time) ease the span from `a` to
  `b`, consecutive times of the keyframes of a value: nil while it stays
  still, the easing of its segment when the span is that segment (or the
  end of a hold), linear for a part of a linear segment, ::partial for a
  part of a curved one."
  [keyframes a b]
  (let [k (last (filter #(<= (:time %) a) keyframes))
        n (first (filter #(>= (:time %) b) keyframes))]
    (cond
      (or (nil? k) (nil? n))                    nil
      (and (== (:time k) a) (== (:time n) b))   (react-ease k)
      (= :step (:interpolation k))              (when (== (:time n) b) hold-ease)
      (= linear-ease (react-ease k))            linear-ease
      :else                                     ::partial)))

(defn- value-segments
  "`[times eases]` of a value over `duration`: the times of the keyframes
  of the properties behind it within the clock, with 0 and the end, and
  the easing of each segment; a segment the properties ease apart (or one
  of them partly) is cut into `segment-samples` linear steps."
  [keyframes-by-property duration]
  (let [times (into (sorted-set 0 duration)
                    (comp cat (map :time) (filter #(<= 0 % duration)))
                    (vals keyframes-by-property))]
    (reduce (fn [[times eases] [a b]]
              (let [found (into #{} (keep #(span-ease % a b)) (vals keyframes-by-property))]
                (if (or (> (count found) 1) (contains? found ::partial))
                  (let [step (/ (- b a) segment-samples)]
                    [(into times (map #(+ a (* step %))) (range 1 (inc segment-samples)))
                     (into eases (repeat segment-samples linear-ease))])
                  [(conj times b) (conj eases (or (first found) linear-ease))])))
            [[(first times)] []]
            (partition 2 1 times))))

(defn- frame-at
  "The frame values (see `acss/frame-values`) of `layer` at `time` of the
  played `timeline`."
  [timeline objects {:keys [id shape]} time]
  (acss/frame-values shape
                     (get (cta/values-at timeline time) id {})
                     (cta/position-origin timeline objects id)))

(defn- repeat-of
  "The repeat options of a clock: none playing once, forever in a loop,
  back and forth in ping-pong."
  [clock]
  (case (cta/playback-mode clock)
    :loop      ["repeat: Infinity"]
    :ping-pong ["repeat: Infinity" "repeatType: \"reverse\""]
    []))

(defn- native-spring
  "The spring motion.dev plays natively for the keyframes of one property,
  `keyframes` (by time), on `clock`: when the clock plays once and they
  are one spring segment within it, `{:from :to :spring :delay}`, the
  spring rescaled to settle in its segment (see `cta/spring-over`)."
  [keyframes clock]
  (let [[k0 k1 :as kfs] keyframes]
    (when (and (= 2 (count kfs))
               (= :once (cta/playback-mode clock))
               (= :spring (:type (:easing k0)))
               (<= (:time k1) (:duration clock)))
      {:from k0
       :to k1
       :spring (cta/spring-over (:easing k0) (- (:time k1) (:time k0)))
       :delay (:time k0)})))

(defn- layer-animation
  "The motion.dev values of `layer` (see `code-layers`): for each value,
  `{:name :initial :animate :transition}` as JS, from its played clock
  `played` (springs kept) and `sampled` (springs sampled)."
  [layer objects played sampled]
  (let [clock    (:clock layer)
        duration (max 1 (:duration clock))
        repeats  (repeat-of clock)
        animated (into #{} (map :property) (:keyframes (get-in played [:tracks (:id layer)])))]
    (into []
          (keep
           (fn [[value-name properties frame-key]]
             (let [properties (filter animated properties)]
               (when (seq properties)
                 (let [by-property (fn [timeline]
                                     (let [kfs (:keyframes (get-in timeline [:tracks (:id layer)]))]
                                       (into {}
                                             (map (fn [p] [p (vec (sort-by :time (filter #(= p (:property %)) kfs)))]))
                                             properties)))
                       spring      (when (= 1 (count properties))
                                     (native-spring (val (first (by-property played))) clock))]
                   (if (some? spring)
                     (let [{:keys [from to delay] :as spring} spring
                           value-at #(get (frame-at played objects layer (:time %)) frame-key)
                           {:keys [stiffness damping mass]} (:spring spring)]
                       {:name value-name
                        :initial (react-literal value-name (value-at from))
                        :animate (react-literal value-name (value-at to))
                        :transition (str "{ type: \"spring\", stiffness: " (react-number stiffness)
                                         ", damping: " (react-number damping)
                                         ", mass: " (react-number mass)
                                         ", delay: " (react-number (/ delay 1000.0)) " }")})
                     (let [[times eases] (value-segments (by-property sampled) duration)
                           values (mapv #(react-literal value-name
                                                        (get (frame-at sampled objects layer %) frame-key))
                                        times)]
                       {:name value-name
                        :initial (first values)
                        :animate (str "[" (str/join ", " values) "]")
                        :hold? (some #{hold-ease} eases)
                        :transition (str "{ "
                                         (str/join ", "
                                                   (concat
                                                    [(str "duration: " (react-number (/ duration 1000.0)))
                                                     (str "times: [" (str/join ", " (map #(react-time (/ % duration)) times)) "]")
                                                     (str "ease: [" (str/join ", " eases) "]")]
                                                    repeats))
                                         " }")}))))))
           react-values))))

(defn- merge-scale
  "`values` with `scaleX` and `scaleY` as one `scale` when they play
  alike."
  [values]
  (let [x (d/seek #(= "scaleX" (:name %)) values)
        y (d/seek #(= "scaleY" (:name %)) values)]
    (if (and x y (= (dissoc x :name) (dissoc y :name)))
      (into [] (comp (remove #(= "scaleY" (:name %)))
                     (map #(cond-> % (= "scaleX" (:name %)) (assoc :name "scale"))))
            values)
      values)))

(defn- component-name
  "The name of the React component of a board: its name in PascalCase,
  then Motion; `Board` first when it starts with a digit (`01 · Intro`
  gives `Board01IntroMotion`) or has none."
  [board-name]
  (let [words (re-seq #"[A-Za-z0-9]+" (or board-name ""))
        base  (str/join "" (map #(str (str/upper (subs % 0 1)) (subs % 1)) words))]
    (str (when-not (re-find #"^[A-Za-z]" base) "Board") base "Motion")))

(defn- component-names
  "The names of the React components of `boards`, in order: each as
  `component-name` gives it, a number after the ones it gave before."
  [boards]
  (first
   (reduce (fn [[names seen] board]
             (let [base (component-name (:name board))
                   n    (inc (get seen base 0))]
               [(conj names (if (= 1 n) base (str base n))) (assoc seen base n)]))
           [[] {}]
           boards)))

(def ^:private transform-template
  "Penpot turns a layer, then scales it; motion.dev the other way round.
  motion.dev gives the template the values it has, a turn with its unit
  (`\"90deg\"`), so the units go only to plain numbers (see `with-unit`)."
  (str "transformTemplate={({ x = 0, y = 0, rotate = 0, scaleX = 1, scaleY = 1 }) =>\n"
       "          `translate(${withUnit(x, \"px\")}, ${withUnit(y, \"px\")}) "
       "rotate(${withUnit(rotate, \"deg\")}) scale(${scaleX}, ${scaleY})`}"))

(def ^:private with-unit
  "The helper of `transform-template`."
  "const withUnit = (value, unit) => (typeof value === \"number\" ? `${value}${unit}` : value);")

(defn- react-text
  "The JSX of the text `text`: an expression of a JS string."
  [text]
  (str "{\"" (-> text
                 (str/replace "\\" "\\\\")
                 (str/replace "\"" "\\\"")
                 (str/replace "\n" "\\n"))
       "\"}"))

(defn- react-props
  "The motion.dev props of the animated `values`, indented by `indent`."
  [values indent]
  (str indent "initial={{ " (str/join ", " (map #(str (:name %) ": " (:initial %)) values)) " }}\n"
       indent "animate={{ " (str/join ", " (map #(str (:name %) ": " (:animate %)) values)) " }}\n"
       indent "transition={{\n"
       (str/join "" (map #(str indent "  " (:name %) ": " (:transition %) ",\n") values))
       indent "}}\n"))

(defn- react-units
  "The `motion.span` of each unit of the text of `layer`, which the code
  plays one by one (see `cta/unit-timelines`), with the text between them
  as it is, a `br` between its paragraphs: `{:hold? :code}`."
  [{:keys [shape units]} objects]
  (let [{units-tl :timeline :keys [ids bases]} units
        played  (-> units-tl cta/expand-paths cta/cut-timeline)
        sampled (cta/expand-springs played)
        spans   (mapv (fn [k id]
                        (let [unit   {:id id :shape (cta/unit-shape units-tl objects id) :clock units-tl}
                              values (merge-scale (layer-animation unit objects played sampled))]
                          {:hold? (some :hold? values)
                           :values values
                           :class (str "penpot-unit penpot-unit-" k)}))
                      (range)
                      ids)
        texts   (tu/paragraph-texts (:content shape))
        ;; the pieces of each paragraph: its units, the text between them
        pieces  (for [[p text] (map-indexed vector texts)]
                  (let [in-p (filter (fn [[_ base]] (= p (:paragraph base)))
                                     (map vector (range) bases))]
                    (loop [at 0 units in-p out []]
                      (if-let [[[k {:keys [start end]}] & more] (seq units)]
                        (recur end more (cond-> out
                                          (< at start) (conj (subs text at start))
                                          :always      (conj [k (subs text start end)])))
                        (cond-> out (< at (count text)) (conj (subs text at)))))))
        piece   (fn [item]
                  (if (string? item)
                    (str "        " (react-text item) "\n")
                    (let [[k text] item
                          {:keys [values class]} (nth spans k)]
                      (str "        <motion.span\n"
                           "          className=\"" class "\"\n"
                           "          style={{ display: \"inline-block\" }}\n"
                           (react-props values "          ")
                           "        >" (react-text text) "</motion.span>\n"))))]
    {:hold? (some :hold? spans)
     :code  (str/join "        <br />\n" (map #(str/join "" (map piece %)) pieces))}))

(defn- react-layer
  "The `motion.div` of `layer`, holding the `motion.span` of each unit of
  a text the code plays by units (see `react-units`)."
  [layer objects played sampled class-name]
  (let [values (merge-scale (layer-animation layer objects played sampled))
        names  (into #{} (map :name) values)
        origin (:origin layer)
        units  (when (:units layer) (react-units layer objects))
        template? (and (contains? names "rotate") (contains? names "scaleX"))]
    {:hold? (or (some :hold? values) (:hold? units))
     :template? template?
     :code
     (str "      {/* " (or (get-in layer [:shape :name]) "Layer") " */}\n"
          "      <motion.div\n"
          "        className=\"" (class-name (:shape layer)) "\"\n"
          "        style={{ originX: " (react-number (:x origin)) ", originY: " (react-number (:y origin)) " }}\n"
          (when template?
            (str "        " transform-template "\n"))
          (react-props values "        ")
          (if (some? units)
            (str "      >\n" (:code units) "      </motion.div>")
            "      />"))}))

(defn- react-board
  "The React component `component` of the board of `code`: its animated
  layers (those of `ids` only when given)."
  [{:keys [board-id timeline] :as code} component objects ids class-name]
  (let [board   (get objects board-id)
        played  (memoize (fn [clock]
                           (-> (cta/resolve-animations clock objects) cta/expand-loops cta/expand-paths cta/cut-timeline)))
        layers  (code-layers code objects :ids ids)
        divs    (mapv (fn [layer]
                        (let [p (played (:clock layer))]
                          (react-layer layer objects p (cta/expand-springs p) class-name)))
                      layers)
        markers (sort-by :time (:markers timeline))]
    (when (seq divs)
      {:hold? (some :hold? divs)
       :template? (some :template? divs)
       :code
       (str "/* " (or (:name board) "Board") ": " (:duration timeline) " ms, "
            (name (cta/playback-mode timeline)) "."
            (when (seq markers)
              (str " Markers: " (str/join ", " (map #(str (:name %) " " (:time %) " ms") markers)) "."))
            " */\n"
            "export function " component "() {\n"
            "  return (\n"
            "    <MotionConfig reducedMotion=\"user\">\n"
            (str/join "\n" (map :code divs)) "\n"
            "    </MotionConfig>\n"
            "  );\n"
            "}")})))

(defn motion-react
  "The animation of the boards of `codes` (see `cltl/code-timelines`; one
  or several) as React code for motion.dev: one component by board, one
  `motion.div` by animated layer (those of `ids` only when given), its
  class from `class-name` (`acss/shape-css-class` of its id by default) and
  each value with its keyframes, `times` and an easing by segment, the
  clock of the layer (a copy's, its main's) repeating it. A value that is
  one spring segment on a clock playing once is a spring of motion.dev,
  rescaled to settle where Penpot's does; other springs, motion paths and
  segments the properties behind a value ease apart are sampled. Nil when
  nothing animates."
  [codes objects & {:keys [ids class-name]}]
  (let [codes      (if (map? codes) [codes] codes)
        class-name (or class-name #(acss/shape-css-class (:id %)))
        names      (component-names (map #(get objects (:board-id %)) codes))
        boards     (into [] (keep identity)
                         (map #(react-board %1 %2 objects ids class-name) codes names))]
    (when (seq boards)
      (str "import { MotionConfig, motion } from \"motion/react\";\n"
           (when (some :hold? boards)
             "import { steps } from \"motion\";\n")
           "\n"
           (when (some :template? boards)
             (str with-unit "\n\n"))
           (str/join "\n\n" (map :code boards))
           "\n"))))
