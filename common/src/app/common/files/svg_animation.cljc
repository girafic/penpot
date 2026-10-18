;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.files.svg-animation
  "The shapes and the timeline of an animated SVG, so that it plays and
  can be edited in Penpot Motion: a board with the layers of the SVG, as
  the static import makes them (see `sb/create-svg-shapes`), and the
  keyframes of its animations.

  It reads the SMIL animations (`animate`, `animateTransform`,
  `animateMotion`, `set`) and the CSS animations (the `@keyframes` of
  `style` elements, given to elements by rules of simple selectors or by
  their `style`), with their timing: begin, duration, repeats, direction,
  fill and easing. The static declarations of those rules are applied
  too, which the static import leaves out.

  An element that moves, turns, scales or fades is held by a transparent
  board centred on the point it turns or scales around, which carries
  those keyframes (`:x` `:y` `:rotation` `:scale-x` `:scale-y`
  `:opacity`); its paints (the colour and opacity of its fill and
  stroke, the width of its stroke, the dash offset of a line that draws
  itself as its trim) are keyframes of its shapes. The keyframes follow
  the values of the animation: where an easing cannot give them, the
  segment is cut until it does (see `fit`).

  A board has one duration: animations that repeat for ever repeat in
  the loop of the board, as many times as fit in it. What a board cannot
  play is left out and counted (see `svg->shapes`)."
  (:require
   [app.common.data :as d]
   [app.common.files.helpers :as cfh]
   [app.common.files.shapes-builder :as sb]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.svg :as csvg]
   [app.common.types.animation :as cta]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.color :as clr]
   [app.common.types.path :as path]
   [app.common.types.path.morph :as morph]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [clojure.string :as cstr]
   [cuerdas.core :as str]))

;; --- Values -----------------------------------------------------------------

(def ^:private number-re
  #"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?")

(defn- numbers
  "The numbers in the text `s`."
  [s]
  (if (string? s)
    (into [] (keep #(d/parse-double %)) (re-seq number-re s))
    []))

(defn- number
  [s]
  (first (numbers s)))

(defn- clock
  "The milliseconds of the SMIL or CSS clock value `s` (`2s`, `150ms`,
  `1min`, `0:01.5`, or seconds without a unit), nil for none."
  [s]
  (let [s (some-> s str str/trim str/lower)]
    (when-not (or (str/blank? s) (= "indefinite" s))
      (cond
        (str/includes? s ":")
        (* 1000 (reduce #(+ (* %1 60) %2) 0 (map #(d/parse-double % 0) (str/split s ":"))))

        (str/ends-with? s "ms")  (number s)
        (str/ends-with? s "min") (some-> (number s) (* 60000))
        (str/ends-with? s "h")   (some-> (number s) (* 3600000))
        :else                    (some-> (number s) (* 1000))))))

;; --- Limits: an SVG comes from anyone, the import must end

(def ^:private min-dur
  "The shortest cycle (ms) of an animation the import plays."
  10)

(def ^:private max-board
  "The longest board (ms) the import makes."
  600000)

(def ^:private max-steps
  "The most steps of a CSS `steps()` easing; more play as a line."
  1000)

(def ^:private max-starts
  "The most starts of segments of one animation in its board."
  10000)

(defn- rgb
  "The `[r g b]` of the CSS colour `s`, nil for none."
  [s]
  (let [s (some-> s str str/trim str/lower)]
    (cond
      (or (str/blank? s) (contains? #{"none" "transparent" "currentcolor"} s))
      nil

      (str/starts-with? s "rgb")
      (let [[r g b] (numbers s)
            channel (fn [v] (mth/clamp (if (str/includes? s "%") (* 2.55 v) v) 0 255))]
        (when (and r g b) [(channel r) (channel g) (channel b)]))

      (clr/color-string? s)
      (clr/hex->rgb (clr/parse s))

      :else nil)))

(defn- hex
  [[r g b]]
  (clr/rgb->hex [(mth/round r) (mth/round g) (mth/round b)]))

(defn- split-list
  "The items of `s` split at the character `sep` where no parenthesis is
  open, trimmed, without the blank ones."
  [s sep]
  (let [n    (count s)
        item #(str/trim (subs s %1 %2))]
    (loop [i 0 depth 0 start 0 acc []]
      (if (>= i n)
        (let [last (item start n)] (cond-> acc (not (str/blank? last)) (conj last)))
        (let [ch (str (nth s i))]
          (cond
            (= ch "(") (recur (inc i) (inc depth) start acc)
            (= ch ")") (recur (inc i) (max 0 (dec depth)) start acc)

            (and (= ch sep) (zero? depth))
            (recur (inc i) depth (inc i) (let [it (item start i)] (cond-> acc (not (str/blank? it)) (conj it))))

            :else (recur (inc i) depth start acc)))))))

;; --- CSS --------------------------------------------------------------------

(defn- declarations
  "The declarations of the CSS block `text`, in order, as pairs of the
  property (in lower case) and its value."
  [text]
  (into []
        (keep (fn [decl]
                (let [i (cstr/index-of decl ":")]
                  (when (and (some? i) (pos? i))
                    [(str/lower (str/trim (subs decl 0 i)))
                     (-> (subs decl (inc i)) (str/replace #"(?i)!important" "") (str/trim))]))))
        (split-list (or text "") ";")))

(defn- css-blocks
  "The top-level blocks of `css`: pairs of the text before each brace (a
  selector or an at-rule) and the text in it."
  [css]
  (let [n (count css)]
    (loop [i 0 depth 0 start 0 prelude nil acc []]
      (if (>= i n)
        acc
        (let [ch (str (nth css i))]
          (cond
            (= ch "{") (if (zero? depth)
                         (recur (inc i) 1 (inc i) (str/trim (subs css start i)) acc)
                         (recur (inc i) (inc depth) start prelude acc))
            (= ch "}") (cond
                         (= 1 depth)   (recur (inc i) 0 (inc i) nil (conj acc [prelude (subs css start i)]))
                         (zero? depth) (recur (inc i) 0 (inc i) nil acc)
                         :else         (recur (inc i) (dec depth) start prelude acc))
            (and (= ch ";") (zero? depth)) (recur (inc i) 0 (inc i) nil acc)
            :else (recur (inc i) depth start prelude acc)))))))

(defn- compound
  "The tag, id and classes of the compound selector `s`, nil for one with
  more than those (pseudo-classes, attributes)."
  [s]
  (when (re-matches #"[a-zA-Z0-9_\-#.*]+" s)
    {:tag     (second (re-find #"^([a-zA-Z][a-zA-Z0-9_\-]*)" s))
     :id      (second (re-find #"#([a-zA-Z0-9_\-]+)" s))
     :classes (into #{} (map second) (re-seq #"\.([a-zA-Z0-9_\-]+)" s))}))

(defn- selector
  "The compound selectors of the CSS selector `s`, the right-most last;
  nil for one this import does not match: only tags, ids and classes, of
  descendants or children."
  [s]
  (let [compounds (->> (str/split (str/replace (str/trim s) #"\s*>\s*" " ") #"\s+")
                       (remove str/blank?)
                       (mapv compound))]
    (when (and (seq compounds) (every? some? compounds))
      compounds)))

(defn- specificity
  [compounds]
  [(count (filter :id compounds))
   (reduce + (map (comp count :classes) compounds))
   (count (filter :tag compounds))])

(defn- matches-compound?
  [{:keys [tag id classes]} {:keys [attrs] :as node}]
  (and (or (nil? tag) (= tag (name (:tag node))))
       (or (nil? id) (= id (:id attrs)))
       (or (empty? classes)
           (let [own (set (str/split (str/trim (str (:class attrs ""))) #"\s+"))]
             (every? own classes)))))

(defn- matches?
  "Whether the selector `compounds` matches `node`, under `ancestors`
  (the nearest first)."
  [compounds node ancestors]
  (and (matches-compound? (peek compounds) node)
       (loop [cs (pop compounds) as ancestors]
         (cond
           (empty? cs) true
           (empty? as) false
           (matches-compound? (peek cs) (first as)) (recur (pop cs) (rest as))
           :else (recur cs (rest as))))))

(defn- keyframe-stops
  "The stops of the body of a CSS `@keyframes`, by their offset (0 to 1),
  each with its declarations."
  [body]
  (->> (css-blocks body)
       (mapcat (fn [[sel decls]]
                 (let [decls (declarations decls)]
                   (keep (fn [s]
                           (let [s      (str/lower (str/trim s))
                                 offset (case s
                                          "from" 0.0
                                          "to"   1.0
                                          (some-> (number s) (/ 100)))]
                             (when (some? offset)
                               {:offset (mth/clamp offset 0 1) :decls decls})))
                         (split-list sel ",")))))
       (reduce (fn [m {:keys [offset decls]}] (update m offset (fnil into []) decls)) (sorted-map))
       (mapv (fn [[offset decls]] {:offset offset :decls (into {} decls)}))))

(defn- stylesheet
  "The rules and the keyframes of the CSS text `css`."
  [css]
  (let [css (str/replace (or css "") #"/\*[\s\S]*?\*/" "")]
    (reduce
     (fn [acc [prelude body]]
       (cond
         (re-find #"^@(?:-webkit-|-moz-|-o-)?keyframes" prelude)
         (let [kname (-> (str/replace prelude #"^@\S+\s*" "") (str/trim) (str/replace #"[\"']" ""))]
           (assoc-in acc [:keyframes kname] (keyframe-stops body)))

         (str/starts-with? prelude "@")
         acc

         :else
         (let [decls (declarations body)]
           (reduce (fn [acc s]
                     (if-let [sel (selector s)]
                       (update acc :rules conj {:selector sel
                                                :specificity (specificity sel)
                                                :order (count (:rules acc))
                                                :decls decls})
                       acc))
                   acc
                   (split-list prelude ",")))))
     {:rules [] :keyframes {}}
     (css-blocks css))))

;; --- Easing -----------------------------------------------------------------
;;
;; The easing of a segment: nil (linear), a cubic bezier `[x1 y1 x2 y2]`
;; or `:step` (it holds its first value until its end).

(def ^:private css-easings
  {"linear"      nil
   "ease"        [0.25 0.1 0.25 1]
   "ease-in"     [0.42 0 1 1]
   "ease-out"    [0 0 0.58 1]
   "ease-in-out" [0.42 0 0.58 1]
   "step-start"  [:steps 1 :start]
   "step-end"    [:steps 1 :end]})

(defn- css-easing
  "The easing of the CSS timing function `s` (`ease` when none), with
  `[:steps n :start|:end]` for steps (see `expand-steps`)."
  [s]
  (let [s (str/lower (str/trim (str s)))]
    (cond
      (str/blank? s)
      (get css-easings "ease")

      (contains? css-easings s)
      (get css-easings s)

      (str/starts-with? s "cubic-bezier")
      (let [[x1 y1 x2 y2] (numbers s)]
        (when (and x1 y1 x2 y2) [(mth/clamp x1 0 1) y1 (mth/clamp x2 0 1) y2]))

      (str/starts-with? s "steps")
      (let [n (or (number s) 1)]
        (when (<= n max-steps)
          [:steps (max 1 (int n)) (if (re-find #"start" s) :start :end)]))

      :else
      (get css-easings "ease"))))

(defn- ease
  "The eased progress of `easing` at the progress `u` (0 to 1)."
  [easing u]
  (cond
    (nil? easing)    u
    (= :step easing) (if (< u 1) 0 1)
    :else            (cta/cubic-bezier easing u)))

(defn- reverse-easing
  "The easing of a segment played backwards."
  [easing]
  (if (vector? easing)
    (let [[x1 y1 x2 y2] easing] [(- 1 x2) (- 1 y2) (- 1 x1) (- 1 y1)])
    easing))

(defn- bezier-x
  [s x1 x2]
  (let [m (- 1 s)] (+ (* 3 m m s x1) (* 3 m s s x2) (* s s s))))

(defn- bezier-param
  "The parameter of the curve of `x1` `x2` where its x is `u`."
  [x1 x2 u]
  (cond
    (<= u 0) 0.0
    (>= u 1) 1.0
    :else
    (loop [lo 0.0 hi 1.0 i 0]
      (let [s (/ (+ lo hi) 2)
            x (bezier-x s x1 x2)]
        (if (or (> i 40) (< (mth/abs (- x u)) 1e-7))
          s
          (if (< x u) (recur s hi (inc i)) (recur lo s (inc i))))))))

(defn- split-curve
  "The two halves of the cubic `pts` at the parameter `s`."
  [[p0 p1 p2 p3] s]
  (let [l    (fn [a b] (mapv #(+ %1 (* s (- %2 %1))) a b))
        p01  (l p0 p1) p12 (l p1 p2) p23 (l p2 p3)
        p012 (l p01 p12) p123 (l p12 p23)
        m    (l p012 p123)]
    [[p0 p01 p012 m] [m p123 p23 p3]]))

(defn- sub-easing
  "The easing over the part `u0` to `u1` (0 to 1) of a segment of
  `easing`: the same curve, cut and stretched to the part."
  [easing u0 u1]
  (if (or (not (vector? easing)) (and (<= u0 0) (>= u1 1)))
    easing
    (let [[x1 y1 x2 y2] easing
          s0      (bezier-param x1 x2 u0)
          s1      (bezier-param x1 x2 u1)
          [left]  (split-curve [[0 0] [x1 y1] [x2 y2] [1 1]] s1)
          [_ part] (split-curve left (if (pos? s1) (/ s0 s1) 0))
          [[ax ay] [bx by] [cx cy] [ex ey]] part
          w       (- ex ax)
          h       (- ey ay)]
      (when (and (> (mth/abs w) 1e-9) (> (mth/abs h) 1e-9))
        [(mth/clamp (/ (- bx ax) w) 0 1) (/ (- by ay) h)
         (mth/clamp (/ (- cx ax) w) 0 1) (/ (- cy ay) h)]))))

;; --- Transforms -------------------------------------------------------------

(defn- angle-deg
  "The degrees of the CSS angle `[v unit]`."
  [[v unit]]
  (case unit
    "rad"  (mth/degrees v)
    "turn" (* 360 v)
    "grad" (* 0.9 v)
    v))

(defn- css-args
  "The arguments of a CSS function as `[number unit]` pairs."
  [s]
  (into []
        (keep (fn [arg]
                (when-let [[_ n unit] (re-matches #"\s*([+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)([a-z%]*)\s*" arg)]
                  [(d/parse-double n) unit])))
        (str/split s #"[\s,]+")))

(defn- css-transform
  "The functions of the CSS transform `s`: `[name args]`, args as
  `[number unit]`."
  [s]
  (if (or (str/blank? s) (= "none" (str/trim s)))
    []
    (into []
          (map (fn [[_ fname args]] [(str/lower fname) (css-args args)]))
          (re-seq #"([a-zA-Z0-9]+)\(([^)]*)\)" s))))

(defn- length
  "The pixels of the CSS length `[v unit]`, a percentage of `size`."
  [[v unit] size]
  (if (= "%" unit) (* size (/ v 100)) v))

(defn- function-matrix
  "The matrix of the CSS transform function `[fname args]`, the
  percentages of the box `[w h]`; nil for one a board cannot play."
  [[fname args] [w h]]
  (let [arg   #(get args %)
        len   (fn [i size] (if-let [a (arg i)] (length a size) 0))
        deg   (fn [i] (if-let [a (arg i)] (angle-deg a) 0))
        num   (fn [i default] (if-let [a (arg i)] (first a) default))]
    (case fname
      ("translate" "translate3d") (gmt/translate-matrix (len 0 w) (len 1 h))
      "translatex"                (gmt/translate-matrix (len 0 w) 0)
      "translatey"                (gmt/translate-matrix 0 (len 0 h))
      ("scale" "scale3d")         (gmt/scale-matrix (gpt/point (num 0 1) (num 1 (num 0 1))))
      "scalex"                    (gmt/scale-matrix (gpt/point (num 0 1) 1))
      "scaley"                    (gmt/scale-matrix (gpt/point 1 (num 0 1)))
      ("rotate" "rotatez")        (gmt/rotate-matrix (deg 0))
      "skew"                      (gmt/skew-matrix (deg 0) (deg 1))
      "skewx"                     (gmt/skew-matrix (deg 0) 0)
      "skewy"                     (gmt/skew-matrix 0 (deg 0))
      "matrix"                    (let [[a b c d e f] (map first args)]
                                    (if (and a b c d e f) (gmt/matrix a b c d e f) (gmt/matrix)))
      nil)))

(defn- smil-matrix
  "The matrix of the `animateTransform` of `type` with `params`."
  [type [a b c]]
  (case type
    "translate" (gmt/translate-matrix (or a 0) (or b 0))
    "scale"     (gmt/scale-matrix (gpt/point (or a 1) (or b a 1)))
    "rotate"    (gmt/rotate-matrix (or a 0) (gpt/point (or b 0) (or c 0)))
    "skewX"     (gmt/skew-matrix (or a 0) 0)
    "skewY"     (gmt/skew-matrix 0 (or a 0))
    (gmt/matrix)))

(defn- matrix-angle
  [m]
  (mth/degrees (mth/atan2 (:b m) (:a m))))

(defn- decompose
  "The `[tx ty rotation sx sy skew]` of the matrix `m` (scaled, then
  turned, then moved), the rotation turned whole times to be the nearest
  to `nominal`."
  [m nominal]
  (let [{:keys [a b c d e f]} m
        sx   (mth/sqrt (+ (* a a) (* b b)))
        det  (- (* a d) (* b c))
        sy   (if (pos? sx) (/ det sx) 0)
        rot  (mth/degrees (mth/atan2 b a))
        rot  (if (some? nominal) (+ rot (* 360 (mth/round (/ (- nominal rot) 360)))) rot)
        ;; what turns the y axis apart from the x axis
        skew (if (and (pos? sx) (not (zero? sy)))
               (/ (+ (* a c) (* b d)) (* sx sx))
               0)]
    [e f rot sx sy skew]))

;; --- Paths ------------------------------------------------------------------

(defn- polyline
  "The points along the path `content`, its curves cut in short lines."
  [content]
  (let [segments (seq content)]
    (loop [segments segments last nil start nil acc []]
      (if-let [{:keys [command params]} (first segments)]
        (let [pt #(gpt/point (get params %1) (get params %2))]
          (case command
            :move-to  (let [p (pt :x :y)] (recur (rest segments) p p (conj acc p)))
            :line-to  (let [p (pt :x :y)] (recur (rest segments) p start (conj acc p)))
            :curve-to (let [p0 (or last (pt :x :y))
                            c1 (pt :c1x :c1y) c2 (pt :c2x :c2y) p3 (pt :x :y)
                            at (fn [s]
                                 (let [m (- 1 s)
                                       w [(* m m m) (* 3 m m s) (* 3 m s s) (* s s s)]]
                                   (gpt/point (+ (* (w 0) (:x p0)) (* (w 1) (:x c1)) (* (w 2) (:x c2)) (* (w 3) (:x p3)))
                                              (+ (* (w 0) (:y p0)) (* (w 1) (:y c1)) (* (w 2) (:y c2)) (* (w 3) (:y p3))))))]
                        (recur (rest segments) p3 start (into acc (map #(at (/ % 16))) (range 1 17))))
            :close-path (recur (rest segments) start start (cond-> acc (some? start) (conj start)))
            (recur (rest segments) last start acc)))
        acc))))

(defn- path-sampler
  "A function of a fraction of the length of the path `d` to the point
  there and the angle of the path there (degrees), with the length;
  nil for an empty path."
  [d]
  (let [points (try (polyline (path/from-string d)) (catch #?(:clj Throwable :cljs :default) _ []))
        cumul  (reductions + 0 (map gpt/distance points (rest points)))
        total  (last cumul)]
    (when (and (> (count points) 1) (pos? total))
      (let [points (vec points) cumul (vec cumul)]
        {:length total
         :at (fn [f]
               (let [target (* (mth/clamp f 0 1) total)
                     i      (loop [i 1] (if (and (< i (dec (count cumul))) (< (nth cumul i) target)) (recur (inc i)) i))
                     a      (nth points (dec i)) b (nth points i)
                     seg    (- (nth cumul i) (nth cumul (dec i)))
                     u      (if (pos? seg) (/ (- target (nth cumul (dec i))) seg) 0)]
                 [(gpt/point (+ (:x a) (* u (- (:x b) (:x a)))) (+ (:y a) (* u (- (:y b) (:y a)))))
                  (mth/degrees (mth/atan2 (- (:y b) (:y a)) (- (:x b) (:x a))))]))}))))

(defn- outline-length
  "The length of the outline of `shape`, in pixels."
  [shape]
  (case (:type shape)
    :rect   (* 2 (+ (:width shape) (:height shape)))
    :circle (let [a (/ (:width shape) 2) b (/ (:height shape) 2)
                  h (/ (mth/sq (- a b)) (mth/sq (+ a b)))]
              (* mth/PI (+ a b) (+ 1 (/ (* 3 h) (+ 10 (mth/sqrt (- 4 (* 3 h))))))))
    :path   (let [pts (polyline (:content shape))]
              (reduce + (map gpt/distance pts (rest pts))))
    0))

;; --- Reading the SVG --------------------------------------------------------

(def ^:private animation-tags
  #{:animate :animateTransform :animateMotion :set :animateColor})

(def ^:private drawable-tags
  #{:rect :circle :ellipse :path :line :polyline :polygon :text :image})

(def ^:private properties
  "The attributes and CSS properties an animation of a board plays."
  {"opacity" :opacity "fill-opacity" :fill-opacity "stroke-opacity" :stroke-opacity
   "fill" :fill "stroke" :stroke "stroke-width" :stroke-width
   "stroke-dashoffset" :stroke-dashoffset
   "visibility" :visibility "display" :display
   "transform" :transform "motion" :motion
   "x" :x "y" :y "cx" :cx "cy" :cy "r" :r "rx" :rx "ry" :ry
   "width" :width "height" :height
   ;; the outline of a path (see `path-frames`)
   "d" :d "points" :points})

(defn- unplayed
  "What an animation of the attribute or property `attribute` that a
  board cannot play is counted as."
  [attribute]
  (cond
    (contains? #{"d" "points"} attribute) :path-morph
    (contains? #{"offset" "stop-color" "stop-opacity" "x1" "x2" "y1" "y2" "fx" "fy"} attribute) :gradient
    (contains? #{"stddeviation" "dx" "dy" "flood-color" "flood-opacity" "filter"} (str/lower (str attribute))) :filter
    (re-find #"^(font|letter|word|text)" (str attribute)) :text-style
    :else :attribute))

(defn- element?
  [node]
  (and (map? node) (keyword? (:tag node))))

(defn- style-text
  [node]
  (str/join "\n" (filter string? (:content node))))

(defn- index
  "The elements of the SVG `root`, each with the key the import gives it
  (`::sb/element-key`): `{:root :by-key :by-id :css}`; each element in
  `:by-key` with its node, the keys of its ancestors (the nearest first)
  and of its children, and the transform of its ancestors (`:parent-m`)."
  [root]
  (let [state (volatile! {:by-key {} :by-id {} :css [] :next 0})
        walk  (fn walk [node ancestors parent-m]
                (let [k       (:next @state)
                      node    (assoc node ::sb/element-key k)
                      own-m   (csvg/parse-transform (get-in node [:attrs :transform]))
                      _       (vswap! state update :next inc)
                      _       (when (= :style (:tag node)) (vswap! state update :css conj (style-text node)))
                      content (mapv (fn [child]
                                      (if (element? child)
                                        (walk child (cons k ancestors) (gmt/multiply parent-m own-m))
                                        child))
                                    (:content node))
                      node    (assoc node :content content)]
                  (vswap! state update :by-key assoc k
                          {:node node :key k :ancestors ancestors :parent-m parent-m
                           :children (into [] (comp (filter element?) (map ::sb/element-key)) content)})
                  (when-let [id (get-in node [:attrs :id])]
                    (vswap! state update :by-id assoc id k))
                  node))
        root  (walk root () (gmt/matrix))]
    (assoc @state :root root)))

(defn- cascade
  "The declarations of the rules of `sheet` that match the element `k`
  of `idx`, and then of its `style` (which win), in order."
  [sheet idx k]
  (let [{:keys [node ancestors]} (get-in idx [:by-key k])
        nodes (map #(get-in idx [:by-key % :node]) ancestors)
        rules (->> (:rules sheet)
                   (filter #(matches? (:selector %) node nodes))
                   (sort-by (juxt :specificity :order)))]
    (-> []
        (into (mapcat :decls) rules)
        (into (declarations (get-in node [:attrs :style]))))))

(def ^:private inherited
  "The animated properties an element takes from its parent when it does
  not set them."
  #{"fill" "stroke" "fill-opacity" "stroke-opacity" "stroke-width" "stroke-dashoffset" "visibility"})

(def ^:private initial-values
  "The animated properties when nothing sets them, as CSS starts them."
  {"opacity" "1" "fill-opacity" "1" "stroke-opacity" "1" "stroke-width" "1"
   "stroke-dashoffset" "0" "fill" "black" "stroke" "none"
   "visibility" "visible" "display" "inline" "transform" "none"
   "x" "0" "y" "0" "cx" "0" "cy" "0" "r" "0" "rx" "0" "ry" "0"})

(defn- matrix-text
  "The SVG transform `s` as one CSS `matrix()`."
  [s]
  (let [{:keys [a b c d e f]} (csvg/parse-transform s)]
    (str "matrix(" (str/join "," [a b c d e f]) ")")))

(defn- rest-text
  "The text of the property `prop` of the element `k` of `idx` at rest,
  what an animation that does not say starts from: its static CSS (the
  declarations of `cascaded`, by key), else its attribute (a `transform`
  in SVG syntax as a CSS `matrix()`), else for an inherited property the
  one of its parent, else the initial value."
  [idx cascaded k prop]
  (let [own (fn [k]
              (let [css  (some (fn [[p v]] (when (= p prop) v)) (rseq (vec (get cascaded k))))
                    attr (get-in idx [:by-key k :node :attrs (keyword prop)])
                    v    (or css (when (some? attr) (if (= "transform" prop) (matrix-text attr) attr)))]
                (when-not (= "inherit" (some-> v str/trim)) v)))]
    (or (some own (if (contains? inherited prop)
                    (cons k (get-in idx [:by-key k :ancestors]))
                    [k]))
        (get initial-values prop))))

;; --- Animations -------------------------------------------------------------
;;
;; An animation (a "spec") of a property of an element: `:times` (0 to
;; 1) and `:values` of one cycle, the `:easings` of its segments, and its
;; timing: `:begin` `:dur` `:repeat` (`##Inf` for ever) `:end`
;; `:fill-before` `:fill-after` `:direction`.

(defn- even-times
  [n]
  (if (< n 2) [0 1] (mapv #(/ % (dec n)) (range n))))

(defn- smil-begin
  "The milliseconds where the SMIL animation of `attrs` begins, given the
  begins and ends of the others (`synced`, by id); `:event` for one that
  waits for the viewer, nil for one that waits for one not known yet."
  [attrs synced]
  (let [items (split-list (str (:begin attrs "0s")) ";")
        times (map (fn [item]
                     (if-let [[_ id edge offset] (re-matches #"([A-Za-z_][\w\-]*)\.(begin|end)\s*([+-].*)?" item)]
                       (when-let [base (get-in synced [id (keyword edge)])]
                         (+ base (or (some-> offset (str/replace #"\s" "") clock) 0)))
                       (if (re-matches #"[+-]?[\d.].*" item) (clock item) :event)))
                   items)]
    (or (when-let [ns (seq (filter number? times))] (reduce min ns))
        (when (some #{:event} times) :event))))

(defn- smil-timing
  [attrs begin]
  (let [;; a `dur` of 0 or less is an error: no duration, as if indefinite
        dur    (let [d (clock (:dur attrs))] (when (and (some? d) (pos? d)) d))
        rc     (:repeatCount attrs)
        rd     (:repeatDur attrs)
        repeat (cond
                 (= "indefinite" rc) ##Inf
                 (some? rc)          (or (number rc) 1)
                 (= "indefinite" rd) ##Inf
                 (some? rd)          ##Inf
                 :else               1)
        end    (some->> (split-list (str (:end attrs "")) ";") (keep clock) (seq) (reduce min))
        limits (cond-> []
                 (and (some? rd) (not= "indefinite" rd)) (conj (clock rd))
                 (some? end)                            (conj (- end begin)))]
    {:begin       begin
     :dur         dur
     :repeat      repeat
     :limit       (when (seq limits) (reduce min limits))
     :fill-before false
     :fill-after  (if (= "freeze" (:fill attrs)) :freeze :remove)
     :direction   :normal}))

(defn- add-values
  [a b]
  (if (and (vector? a) (vector? b))
    (mapv + a (concat b (repeat 0)))
    (+ a b)))

(defn- smil-values
  "The values of the SMIL animation of `attrs`: of `values`, or from
  `from`, `to` and `by`, from `base` when it has no `from`; parsed by
  `parse`."
  [attrs parse base]
  (let [values (some->> (:values attrs) (#(split-list % ";")) (mapv parse) (filterv some?))]
    (cond
      (seq values) values
      :else
      (let [from (some-> (:from attrs) parse)
            to   (some-> (:to attrs) parse)
            by   (some-> (:by attrs) parse)
            from (or from base)]
        (cond
          (and (some? from) (some? to)) [from to]
          (some? to)                    [to]
          (and (some? from) (some? by)) [from (add-values from by)]
          :else                         nil)))))

(defn- smil-cycle
  "The `:times` `:values` `:easings` of a cycle of the SMIL animation of
  `attrs` with `values`."
  [attrs values default-mode]
  (let [mode    (or (:calcMode attrs) default-mode)
        n       (count values)
        times   (let [kt (numbers (:keyTimes attrs))]
                  (if (= (count kt) n) kt (if (= "discrete" mode) (mapv #(/ % n) (range n)) (even-times n))))
        splines (->> (split-list (str (:keySplines attrs "")) ";") (mapv numbers) (filterv #(= 4 (count %))))]
    (cond
      (= 1 n)
      {:times [0 1] :values [(first values) (first values)] :easings [:step]}

      (= "discrete" mode)
      {:times (conj (vec times) 1) :values (conj (vec values) (peek values)) :easings (vec (repeat n :step))}

      :else
      {:times (vec times)
       :values (vec values)
       :easings (if (and (= "spline" mode) (= (count splines) (dec n)))
                  splines
                  (vec (repeat (dec n) nil)))})))

;; --- Values by property

(defn- parse-value
  "The value of the animated `property` in the text `s`."
  [property s]
  (let [s (str/trim (str s))]
    (case property
      (:opacity :fill-opacity :stroke-opacity)
      (some-> (number s) (cond-> (str/ends-with? s "%") (/ 100)) (mth/clamp 0 1))

      (:fill :stroke)                   (or (rgb s) :none)
      :visibility                       (if (contains? #{"hidden" "collapse"} s) 0 1)
      :display                          (if (= "none" s) 0 1)
      (:stroke-width :stroke-dashoffset
                     :x :y :cx :cy :r :rx :ry
                     :width :height)                  (number s)
      ;; an outline, as the path data it is, out of a CSS `path()`
      (:d :points)                      (let [d (str/trim (or (second (re-find #"^path\(\s*['\"]?(.*?)['\"]?\s*\)$" s)) s))]
                                          (when-not (str/blank? d) d))
      nil)))

(defn- lerp
  [a b u]
  (cond
    (and (number? a) (number? b)) (+ a (* u (- b a)))
    (and (vector? a) (vector? b)) (mapv #(+ %1 (* u (- %2 %1))) a (concat b (repeat 0)))
    :else                         (if (< u 1) a b)))

;; --- Reading the animations of the elements

(defn- animation-list
  "The CSS animations the declarations `decls` give an element, each a
  map of its longhands."
  [decls]
  (let [longhands #{"animation-name" "animation-duration" "animation-timing-function" "animation-delay"
                    "animation-iteration-count" "animation-direction" "animation-fill-mode"}
        shorthand (fn [item]
                    (reduce (fn [acc token]
                              (let [t (str/lower token)]
                                (cond
                                  (re-matches #"[+-]?[\d.]+m?s" t)
                                  (if (contains? acc "animation-duration")
                                    (assoc acc "animation-delay" t)
                                    (assoc acc "animation-duration" t))

                                  (or (contains? css-easings t) (re-find #"^(cubic-bezier|steps)\(" t))
                                  (assoc acc "animation-timing-function" t)

                                  (or (= "infinite" t) (re-matches #"[\d.]+" t))
                                  (assoc acc "animation-iteration-count" t)

                                  (contains? #{"normal" "reverse" "alternate" "alternate-reverse"} t)
                                  (assoc acc "animation-direction" t)

                                  (contains? #{"forwards" "backwards" "both"} t)
                                  (assoc acc "animation-fill-mode" t)

                                  (contains? #{"running" "paused" "none" "initial" "inherit"} t)
                                  acc

                                  :else (assoc acc "animation-name" token))))
                            {}
                            (split-list item " ")))
        lists (reduce (fn [acc [p v]]
                        (cond
                          (= "animation" p) (let [items (mapv shorthand (split-list v ","))]
                                              (reduce (fn [acc lh] (assoc acc lh (mapv #(get % lh) items))) acc longhands))
                          (contains? longhands p) (assoc acc p (split-list v ","))
                          :else acc))
                      {}
                      decls)
        names (get lists "animation-name")]
    (into []
          (keep-indexed
           (fn [i n]
             (when (and (some? n) (not= "none" n))
               (into {"animation-name" n}
                     (keep (fn [lh]
                             (let [l (get lists lh)
                                   v (when (seq l) (nth l (mod i (count l))))]
                               (when (some? v) [lh v]))))
                     (disj longhands "animation-name")))))
          names)))

(defn- css-specs
  "The specs of the CSS animations of the element `k`, given its
  declarations `decls`, the keyframes of the sheet and `rest-of`, the
  text of a property of an element at rest (see `rest-text`)."
  [ctx sheet-keyframes k decls rest-of]
  (let [static (into {} decls)]
    (into []
          (mapcat
           (fn [anim]
             (let [stops     (get sheet-keyframes (get anim "animation-name"))
                   dur       (clock (get anim "animation-duration" "0s"))
                   easing    (css-easing (get anim "animation-timing-function"))
                   iterations (get anim "animation-iteration-count" "1")
                   fill      (get anim "animation-fill-mode" "none")
                   timing    {:begin       (or (clock (get anim "animation-delay")) 0)
                              :dur         dur
                              :repeat      (if (= "infinite" iterations) ##Inf (or (number iterations) 1))
                              :limit       nil
                              :fill-before (contains? #{"backwards" "both"} fill)
                              :fill-after  (if (contains? #{"forwards" "both"} fill) :freeze :remove)
                              :direction   (keyword (get anim "animation-direction" "normal"))}]
               (when (and (seq stops) (some? dur))
                 (for [prop (distinct (mapcat (comp keys :decls) stops))
                       :let [property (get properties prop)]
                       :when (or (some? property)
                                 (do (when-not (contains? #{"animation-timing-function" "transform-origin" "transform-box"} prop)
                                       (ctx :skip (unplayed prop)))
                                     false))
                       :let [own    (filter #(contains? (:decls %) prop) stops)
                             value  (fn [s] (if (= :transform property) s (parse-value property s)))
                             points (mapv (fn [{:keys [offset decls]}]
                                            {:offset offset
                                             :value  (value (get decls prop))
                                             :easing (if-let [tf (get decls "animation-timing-function")] (css-easing tf) easing)})
                                          own)
                             base   (value (rest-of k prop))
                             points (cond-> points
                                      (pos? (:offset (first points)))
                                      (->> (into [{:offset 0 :value base :easing easing}]))

                                      (< (:offset (peek points)) 1)
                                      (conj {:offset 1 :value base :easing easing}))]
                       :when (or (every? some? (map :value points))
                                 (do (ctx :skip (unplayed prop)) false))]
                   (merge timing
                          {:key k :property property :source :css :rest base
                           :origin (get static "transform-origin") :box (get static "transform-box")
                           :times (mapv :offset points)
                           :values (mapv :value points)
                           :easings (mapv :easing (butlast points))})))))
           (animation-list decls)))))

(defn- smil-specs
  "The specs of the SMIL animations of the SVG of `idx`, by the key of
  the element each animates; `rest-of` as in `css-specs`."
  [ctx idx rest-of]
  (let [anims (->> (vals (:by-key idx))
                   (filter #(contains? animation-tags (:tag (:node %))))
                   (sort-by :key))
        ;; begins that wait for other animations, resolved in rounds
        synced (loop [synced {} round 0]
                 (let [next (reduce (fn [acc {:keys [node]}]
                                      (let [attrs (:attrs node)
                                            id    (:id attrs)
                                            begin (smil-begin attrs acc)]
                                        (if (and (some? id) (number? begin))
                                          (let [{:keys [dur repeat limit]} (smil-timing attrs begin)
                                                active (cond-> (if (some? dur) (* dur repeat) ##Inf)
                                                         (some? limit) (min limit))]
                                            (assoc acc id {:begin begin :end (+ begin active)}))
                                          acc)))
                                    synced anims)]
                   (if (or (= next synced) (> round 8)) next (recur next (inc round)))))]
    (into []
          (keep
           (fn [{:keys [node ancestors]}]
             (let [{:keys [tag attrs]} node
                   href     (some-> (or (:href attrs) (:xlink:href attrs)) (str/replace #"^#" ""))
                   target   (if (some? href) (get-in idx [:by-id href]) (first ancestors))
                   attribute (case tag
                               :animateTransform "transform"
                               :animateMotion    "motion"
                               (:attributeName attrs))
                   property (get properties attribute)
                   begin    (smil-begin attrs synced)]
               (cond
                 (nil? target) nil
                 (or (= :event begin) (nil? begin)) (do (ctx :skip :interactive) nil)
                 (nil? property) (do (ctx :skip (unplayed attribute)) nil)

                 :else
                 (let [timing (smil-timing attrs begin)
                       timing (cond-> timing (and (= :set tag) (nil? (:dur timing))) (assoc :dur ##Inf :repeat 1))]
                   (case property
                     :transform
                     (let [type   (:type attrs "translate")
                           parse  #(let [ns (numbers %)] (when (seq ns) ns))
                           ;; SMIL: a by-animation is `values="0;by"`, added
                           by?    (and (nil? (:values attrs)) (nil? (:from attrs)) (some? (:by attrs)))
                           values (smil-values attrs parse (when by? (mapv (constantly 0) (parse (:by attrs)))))]
                       (when (seq values)
                         (merge timing
                                (smil-cycle attrs values "linear")
                                {:key target :property :transform :source :smil :type type
                                 :additive (if (or by? (= "sum" (:additive attrs))) :sum :replace)})))

                     :motion
                     (let [mpath  (some #(when (= :mpath (:tag %)) %) (:content node))
                           d      (or (:path attrs)
                                      (when-let [ref (some-> (or (get-in mpath [:attrs :href]) (get-in mpath [:attrs :xlink:href]))
                                                             (str/replace #"^#" ""))]
                                        (get-in idx [:by-key (get-in idx [:by-id ref]) :node :attrs :d]))
                                      (when-let [pts (some->> (:values attrs) (#(split-list % ";")) (mapv numbers) (filter #(= 2 (count %))) (seq))]
                                        (str "M" (str/join " L" (map #(str/join " " %) pts))))
                                      (when (and (:from attrs) (:to attrs))
                                        (str "M" (str/join " " (numbers (:from attrs))) " L" (str/join " " (numbers (:to attrs))))))
                           sampler (some-> d path-sampler)
                           kp     (numbers (:keyPoints attrs))
                           values (if (seq kp) kp [0 1])]
                       (when (some? sampler)
                         (merge timing
                                (smil-cycle (cond-> attrs (empty? kp) (dissoc :keyTimes)) values "paced")
                                {:key target :property :motion :source :smil :sampler sampler
                                 :rotate (:rotate attrs "0")})))

                     (let [parse  #(parse-value property %)
                           base   (parse (rest-of target attribute))
                           values (smil-values attrs parse base)
                           values (if (and (= :set tag) (seq values)) [(peek values)] values)]
                       (if (seq values)
                         (merge timing
                                (smil-cycle attrs values (if (= :set tag) "discrete" "linear"))
                                {:key target :property property :source :smil :rest base})
                         (do (ctx :skip (unplayed attribute)) nil))))))))
           anims))))

(defn- expand-steps
  "The `spec` with its CSS steps as held segments."
  [{:keys [times values easings] :as spec}]
  (if-not (some #(and (vector? %) (= :steps (first %))) easings)
    spec
    (let [n      (count values)
          parts  (for [i (range (dec n))
                       :let [e (nth easings i)
                             t0 (nth times i) t1 (nth times (inc i))
                             v0 (nth values i) v1 (nth values (inc i))]]
                   (if (and (vector? e) (= :steps (first e)))
                     (let [[_ steps pos] e]
                       (for [j (range steps)]
                         [(+ t0 (* (- t1 t0) (/ j steps)))
                          (lerp v0 v1 (/ (if (= :start pos) (inc j) j) steps))
                          :step]))
                     [[t0 v0 e]]))
          parts  (apply concat parts)]
      (assoc spec
             :times   (conj (mapv first parts) (peek times))
             :values  (conj (mapv second parts) (peek values))
             :easings (mapv #(nth % 2) parts)))))

;; --- Playing the specs ------------------------------------------------------

(defn- active-duration
  [{:keys [dur repeat limit]}]
  (cond-> (if (and (some? dur) (mth/finite? dur)) (* dur repeat) ##Inf)
    (some? limit) (min limit)))

(defn- reversed?
  [direction iteration]
  (case direction
    :reverse           true
    :alternate         (odd? iteration)
    :alternate-reverse (even? iteration)
    false))

(defn- position
  "Where `spec` is at the time `t` of the board, approached from `side`
  (`:left` or `:right`): `[iteration fraction reversed?]`, the fraction
  of its cycle as played forwards; nil where it plays nothing. With
  `steady?` an animation that repeats for ever has always been playing."
  [{:keys [begin dur fill-before fill-after direction] :as spec} t side steady?]
  (let [active (active-duration spec)
        e      (- t begin)]
    (cond
      (not (mth/finite? (or dur ##Inf)))
      (when (or (> e 0) (and (zero? e) (= :right side)))
        [0 0 false])

      (and (or (< e 0) (and (zero? e) (= :left side)))
           (not (and steady? (not (mth/finite? active)))))
      (when fill-before
        [0 0 (reversed? direction 0)])

      (and (mth/finite? active) (or (> e active) (and (== e active) (= :right side))))
      (when (= :freeze fill-after)
        (let [q (/ active dur) i (long (dec (mth/ceil q))) f (- q i)]
          [i f (reversed? direction i)]))

      :else
      (let [q (/ e dur)
            i (long (mth/floor q))
            f (- q i)
            [i f] (if (and (= :left side) (zero? f) (not (zero? e))) [(dec i) 1] [i f])]
        [i f (reversed? direction i)]))))

(defn- segment-at
  "The index of the segment of `times` where the fraction `f` is."
  [times f]
  (let [n (count times)]
    (loop [j 0]
      (if (and (< j (- n 2)) (>= f (nth times (inc j)))) (recur (inc j)) j))))

(defn- cycle-point
  "The segment of the cycle `times` at the fraction `f`, approached from
  `side` (`:left` at a stop: the end of the segment before it), and the
  eased progress there."
  [times easings f side]
  (let [j0 (segment-at times f)
        j  (if (and (= :left side) (pos? j0) (== f (nth times j0))) (dec j0) j0)
        t0 (nth times j) t1 (nth times (inc j) t0)
        u  (if (> t1 t0) (mth/clamp (/ (- f t0) (- t1 t0)) 0 1) 1)
        e  (nth easings j nil)]
    ;; a held segment keeps its value until its end
    [j (if (and (= :step e) (= :left side) (>= u 1)) 0 (ease e u))]))

(defn- cycle-value
  [{:keys [times values easings]} f side]
  (let [[j eased] (cycle-point times easings f side)]
    (lerp (nth values j) (nth values (inc j) (nth values j)) eased)))

(defn- flip
  [side]
  (if (= :left side) :right :left))

(defn- spec-value
  "The value of `spec` at the time `t`, nil where it plays nothing."
  [spec t side steady?]
  (when-let [[_ f rev?] (position spec t side steady?)]
    (if rev?
      (cycle-value spec (- 1 f) (flip side))
      (cycle-value spec f side))))

(defn- spec-times
  "The times in `[0 duration]` where `spec` starts a segment."
  [{:keys [begin dur times] :as spec} duration steady?]
  (let [active (active-duration spec)
        end    (if (mth/finite? active) (+ begin active) ##Inf)
        start  (if (and steady? (not (mth/finite? active))) ##-Inf begin)]
    (if (not (mth/finite? (or dur ##Inf)))
      (filter #(<= 0 % duration) [begin])
      (let [i0     (mth/floor (/ (- (max start 0) begin) dur))
            i1     (mth/ceil (/ (- (min end duration) begin) dur))
            ;; a begin out of reach has no cycles to list
            starts (when (and (mth/finite? i0) (mth/finite? i1))
                     (for [i (range (long i0) (inc (long i1)))
                           f times]
                       (+ begin (* dur (+ i (if (reversed? (:direction spec) i) (- 1 f) f))))))]
        (->> (concat [begin end] starts)
             (filter #(and (<= 0 % duration) (<= start % end))))))))

(defn- segment-starts
  "About how many segments of `spec` a board of `duration` ms plays."
  [{:keys [dur times] :as spec} duration]
  (if (and (some? dur) (mth/finite? dur))
    (* (count times) (inc (/ (min duration (active-duration spec)) dur)))
    (count times)))

(defn- spec-easing
  "The easing of `spec` over the times `a` to `b` of the board, which no
  start of a segment of it cuts."
  [{:keys [begin dur times easings direction] :as spec} a b steady?]
  (when-let [[i fa _] (position spec a :right steady?)]
    (when (and (some? dur) (mth/finite? dur))
      (let [rev? (reversed? direction i)
            fb   (let [q (/ (- b begin) dur)] (- q i))
            fwd  (fn [f] (if rev? (- 1 f) f))
            fa   (fwd fa) fb (fwd (min fb 1))
            [lo hi] (if rev? [fb fa] [fa fb])
            j    (segment-at times (/ (+ lo hi) 2))
            t0   (nth times j) t1 (nth times (inc j))
            span (- t1 t0)
            u    #(if (pos? span) (/ (- % t0) span) 0)
            e    (nth easings j nil)]
        (if rev?
          (sub-easing (reverse-easing e) (- 1 (u hi)) (- 1 (u lo)))
          (sub-easing e (u lo) (u hi)))))))

;; --- Fitting keyframes ------------------------------------------------------

(def ^:private tolerances
  {:x 0.25 :y 0.25 :rotation 0.25 :scale-x 0.002 :scale-y 0.002 :opacity 0.004
   :fill-color 2 :stroke-color 2 :fill-opacity 0.004 :stroke-opacity 0.004
   :stroke-width 0.05 :trim-start 0.002 :trim-end 0.002})

(defn- close?
  [a b tol]
  (cond
    (and (number? a) (number? b)) (<= (mth/abs (- a b)) tol)
    (and (vector? a) (vector? b)) (every? #(<= (mth/abs %) tol) (map - a b))
    :else (= a b)))

(defn- fit
  "The keyframes `[time value easing]` from `a` to `b` of the values of
  the function `f` (of a time and a side), starting with the easing
  `hint`: a segment whose easing misses the values in between by more
  than `tol` is cut in two."
  [f tol a b hint depth]
  (let [va  (f a :right)
        vb  (f b :left)
        ok? (every? (fn [u] (close? (lerp va vb (ease hint u)) (f (+ a (* u (- b a))) :right) tol))
                    [0.25 0.5 0.75])]
    (if (or ok? (>= depth 6) (< (- b a) 8))
      [[a va (if (and (= :step hint) (not (close? va vb tol))) nil hint)]]
      (let [m (/ (+ a b) 2)]
        (into (fit f tol a m (sub-easing hint 0 0.5) (inc depth))
              (fit f tol m b (sub-easing hint 0.5 1) (inc depth)))))))

(defn- simplify
  "The keyframes `frames` without those on the straight line between
  their neighbours, the segments on both sides of them linear."
  [frames tol]
  (reduce (fn [acc kf]
            (let [n (count acc)]
              (if (< n 2)
                (conj acc kf)
                (let [[t0 v0 e0] (nth acc (- n 2))
                      [t1 v1 e1] (peek acc)
                      [t2 v2]    kf
                      u          (if (> t2 t0) (/ (- t1 t0) (- t2 t0)) 0)]
                  (if (and (nil? e0) (nil? e1) (close? (lerp v0 v2 u) v1 tol))
                    (conj (pop acc) kf)
                    (conj acc kf))))))
          []
          frames))

(defn- keyframes
  "The keyframes of a property: the values of `f` (of a time of the board
  and a side) from 0 to `duration`, cut where the specs `contributors`
  start segments (`steady?` as in `position`); nil when they keep
  `rest`."
  [f contributors tol duration at-rest steady?]
  (let [times (->> contributors
                   (mapcat #(spec-times % duration steady?))
                   (concat [0 duration])
                   (map #(mth/round %))
                   (filter #(<= 0 % duration))
                   (distinct)
                   (sort)
                   (vec))
        parts (mapcat (fn [[a b]]
                        (let [changing (filter #(not= (position % a :right steady?)
                                                      (position % b :left steady?))
                                               contributors)
                              hint     (when (= 1 (count changing))
                                         (spec-easing (first changing) a b steady?))]
                          (fit f tol a b hint 0)))
                      (partition 2 1 times))
        ;; at its end a board that plays once shows what follows it
        parts  (concat parts [[duration (f duration (if steady? :left :right)) nil]])
        frames (reduce (fn [acc [t v e]]
                         (let [[pt pv pe] (peek acc)
                               prev-end   (when (some? pt) (f t :left))]
                           (if (and (some? pt) (not= :step pe) (not (close? prev-end v tol)))
                             ;; a jump: a value held until it, or the value
                             ;; before it a moment earlier
                             (if (close? pv prev-end tol)
                               (conj (pop acc) [pt pv :step] [t v e])
                               (cond-> acc
                                 (> (- t pt) 2) (conj [(dec t) prev-end nil])
                                 :always (conj [t v e])))
                             (conj acc [t v e]))))
                       []
                       parts)
        frames (->> frames
                    (map (fn [[t v e]] [(int (mth/round t)) v e]))
                    (reduce (fn [acc [t :as kf]] (if (= t (first (peek acc))) (conj (pop acc) kf) (conj acc kf))) []))]
    (when-not (every? #(close? (second %) at-rest tol) frames)
      (simplify frames tol))))

;; --- Building the board -----------------------------------------------------

(defn- fix-groups
  "`shapes` with the selrects of their groups made of their children, of
  their masks for masked groups that keep what is in them."
  [shapes]
  (let [children (group-by :parent-id shapes)
        updated  (reduce (fn [objects shape]
                           (if (and (= :group (:type shape)) (seq (get children (:id shape))))
                             (let [kids (map #(get objects (:id %)) (get children (:id shape)))]
                               (assoc objects (:id shape)
                                      (if (cfh/mask-bounds-shape? shape)
                                        (gsh/update-mask-selrect shape kids)
                                        (gsh/update-group-selrect shape kids))))
                             objects))
                         (into {} (map (juxt :id identity)) shapes)
                         (reverse shapes))]
    (mapv #(get updated (:id %)) shapes)))

(defn- with-frame-ids
  "`shapes`, parents before their children, each with the board it is in
  as its frame: only a board is one."
  [shapes]
  (first
   (reduce (fn [[acc frames] shape]
             (let [frame-id (get frames (:parent-id shape) (:parent-id shape))]
               [(conj acc (assoc shape :frame-id frame-id))
                (assoc frames (:id shape) (if (= :frame (:type shape)) (:id shape) frame-id))]))
           [[] {}]
           shapes)))

(defn- bounds
  [shape]
  (grc/points->rect (or (:points shape) (grc/rect->points (:selrect shape)))))

(defn- node-shape
  "The transparent board `id` that holds `shape`, centred on `pivot`."
  [id shape pivot]
  (let [{:keys [x1 y1 x2 y2]} (bounds shape)
        px (:x pivot) py (:y pivot)
        hw (max 1 (- px x1) (- x2 px))
        hh (max 1 (- py y1) (- y2 py))]
    (cts/setup-shape
     {:type :frame :id id
      :name (str (:name shape) " motion")
      :x (- px hw) :y (- py hh) :width (* 2 hw) :height (* 2 hh)
      :parent-id (:parent-id shape) :frame-id (:frame-id shape)
      :fills [] :strokes [] :shapes []
      :show-content true})))

(defn- origin-point
  "The point of the CSS `transform-origin` `s` in the box `[x y w h]`."
  [s [x y w h]]
  (let [tokens (str/split (str/trim (or s "0 0")) #"\s+")
        coord  (fn [token size axis]
                 (case token
                   ("left" "top")      0
                   "center"            (/ size 2)
                   ("right" "bottom")  size
                   (let [[[v unit]] (css-args token)]
                     (if (some? v) (length [v unit] size) (if (= :x axis) 0 0)))))
        [a b]  (let [[a b] tokens]
                 ;; a vertical keyword first
                 (if (contains? #{"top" "bottom"} a) [(or b "center") a] [a (or b "center")]))]
    (gpt/point (+ x (coord a w :x)) (+ y (coord b h :y)))))

(defn- fns-matrix
  "The matrix of the CSS transform `fns`, the percentages of the box
  `[w h]`."
  [fns wh]
  (reduce (fn [m f] (gmt/multiply m (or (function-matrix f wh) (gmt/matrix)))) (gmt/matrix) fns))

(defn- css-matrix
  "The matrix of the CSS transform `fns` around `origin`, the
  percentages of `box`, with its nominal rotation."
  [fns origin [_ _ w h]]
  (let [m (fns-matrix fns [w h])]
    [(gmt/multiply (gmt/translate-matrix origin) m (gmt/translate-matrix (gpt/negate origin)))
     (reduce + (map (fn [[fname args]] (if (contains? #{"rotate" "rotatez"} fname) (angle-deg (first args)) 0)) fns))]))

(defn- identity-function
  "The CSS transform function `fname` that changes nothing."
  [[fname args]]
  [fname (if (= "matrix" fname)
           [[1 ""] [0 ""] [0 ""] [1 ""] [0 ""] [0 ""]]
           (mapv (fn [[_ unit]] [(if (str/starts-with? fname "scale") 1 0) unit]) args))])

(defn- pad
  "The CSS transforms `a` and `b`, the shorter one, when the other starts
  with its functions, ended with the others changing nothing."
  [a b]
  (let [n (min (count a) (count b))]
    (if (= (map first (take n a)) (map first (take n b)))
      [(into (vec a) (map identity-function) (drop n b))
       (into (vec b) (map identity-function) (drop n a))]
      [a b])))

(defn- unmatrix
  "The parts of the 2D matrix `m` that CSS interpolates (CSS Transforms,
  decomposing a 2D matrix): `[tx ty sx sy angle m11 m12 m21 m22]`, the
  matrix turned by `angle` and scaled by `sx` `sy` after `m11`..`m22`."
  [{:keys [a b c d e f]}]
  (let [sx        (mth/sqrt (+ (* a a) (* b b)))
        sy        (mth/sqrt (+ (* c c) (* d d)))
        ;; a mirror flips the axis it turns the least
        [sx sy]   (cond (not (neg? (- (* a d) (* b c)))) [sx sy]
                        (< a d)                           [(- sx) sy]
                        :else                             [sx (- sy)])
        [r0x r0y] (if (zero? sx) [a b] [(/ a sx) (/ b sx)])
        [r1x r1y] (if (zero? sy) [c d] [(/ c sy) (/ d sy)])
        angle     (mth/atan2 r0y r0x)
        sn        (- r0y)
        cs        r0x]
    (into [e f sx sy (mth/degrees angle)]
          (if (zero? angle)
            [r0x r0y r1x r1y]
            [(+ (* cs r0x) (* sn r1x)) (+ (* cs r0y) (* sn r1y))
             (- (* cs r1x) (* sn r0x)) (- (* cs r1y) (* sn r0y))]))))

(defn- lerp-matrix
  "The CSS transform between the matrices `ma` and `mb` at `u`, as CSS
  interpolates transforms of other functions: by their parts (see
  `unmatrix`), as a translate, a rotate, a matrix and a scale."
  [ma mb u]
  (let [[tx1 ty1 sx1 sy1 a1 & r1] (unmatrix ma)
        [tx2 ty2 sx2 sy2 a2 & r2] (unmatrix mb)
        ;; one mirrored in x and the other in y: a turn
        [sx1 sy1 a1] (if (or (and (neg? sx1) (neg? sy2)) (and (neg? sy1) (neg? sx2)))
                       [(- sx1) (- sy1) (+ a1 (if (neg? a1) 180 -180))]
                       [sx1 sy1 a1])
        ;; not the long way round
        a1           (if (zero? a1) 360 a1)
        a2           (if (zero? a2) 360 a2)
        [a1 a2]      (cond (<= (mth/abs (- a1 a2)) 180) [a1 a2]
                           (> a1 a2)                    [(- a1 360) a2]
                           :else                        [a1 (- a2 360)])
        at           #(+ %1 (* u (- %2 %1)))
        [m11 m12 m21 m22] (map at r1 r2)]
    [["translate" [[(at tx1 tx2) "px"] [(at ty1 ty2) "px"]]]
     ["rotate" [[(at a1 a2) "deg"]]]
     ["matrix" [[m11 ""] [m12 ""] [m21 ""] [m22 ""] [0 ""] [0 ""]]]
     ["scale" [[(at sx1 sx2) ""] [(at sy1 sy2) ""]]]]))

(defn- lerp-css
  "The CSS transform between `a` and `b` at `u`: function by function
  when they have the same ones (see `pad`), else through their matrices
  (see `lerp-matrix`), the percentages of the box `[w h]`."
  [a b u wh]
  (let [[a b] (pad a b)]
    (if (and (= (map first a) (map first b)) (= (map (comp count second) a) (map (comp count second) b)))
      (mapv (fn [[fname aa] [_ ba]] [fname (mapv (fn [[x unit] [y]] [(+ x (* u (- y x))) unit]) aa ba)]) a b)
      (lerp-matrix (fns-matrix a wh) (fns-matrix b wh) u))))

(defn- transform-spec
  "`spec` of a CSS transform with its values parsed."
  [spec]
  (cond-> spec
    (= :css (:source spec)) (update :values #(mapv css-transform %))))

(defn- transform-value
  "The matrix and nominal rotation of the value of the transform `spec`
  at `t` (nil where it plays nothing), in the space of the parent of its
  element, `box` the box of its percentages and origin."
  [spec t side steady? box]
  (when-let [[_ f rev?] (position spec t side steady?)]
    (let [side (if rev? (flip side) side)
          f    (if rev? (- 1 f) f)]
      (if (= :css (:source spec))
        (let [{:keys [times values easings]} spec
              [j eased] (cycle-point times easings f side)
              fns (lerp-css (nth values j) (nth values (inc j) (nth values j)) eased (drop 2 box))]
          (css-matrix fns (origin-point (:origin spec) box) box))
        (let [params (cycle-value spec f side)]
          [(smil-matrix (:type spec) params) (if (= "rotate" (:type spec)) (first params) 0)])))))

(defn- motion-value
  [spec t side steady?]
  (when-let [f (spec-value spec t side steady?)]
    (let [[pt angle] ((get-in spec [:sampler :at]) f)
          rotate (:rotate spec)
          turn   (case rotate "auto" angle "auto-reverse" (+ 180 angle) (or (number rotate) 0))]
      [(gmt/multiply (gmt/translate-matrix pt) (gmt/rotate-matrix turn)) turn])))

(defn- geometry-matrix
  "The matrix of the animated geometry attributes `values` of the
  element `node`, against their values at rest."
  [node values]
  (let [attrs (:attrs node)
        at-rest #(d/parse-double (get attrs %) 0)
        cur   #(get values % (at-rest %))
        tag   (:tag node)
        dx    (case tag (:circle :ellipse) (- (cur :cx) (at-rest :cx)) (- (cur :x) (at-rest :x)))
        dy    (case tag (:circle :ellipse) (- (cur :cy) (at-rest :cy)) (- (cur :y) (at-rest :y)))
        c     (gpt/point (at-rest :cx) (at-rest :cy))
        ratio (fn [k base] (let [r (at-rest k)] (if (pos? r) (/ (cur k) r) (if (some? base) base 1))))
        [sx sy] (case tag
                  :circle  [(ratio :r 1) (ratio :r 1)]
                  :ellipse [(ratio :rx 1) (ratio :ry 1)]
                  [1 1])]
    (gmt/multiply (gmt/translate-matrix dx dy) (gmt/scale-matrix (gpt/point sx sy) c))))

(defn- node-delta
  "The change of the element `node` at the time `t` from the pose it is
  built in (`built`, its transform), in the space of the board
  (`to-board` takes its parent's to it), with its nominal rotation: the
  transforms of `specs` over its own one `own`, as SMIL and CSS stack
  them, its motion and its animated geometry."
  [node own built specs to-board box t side steady?]
  (let [own-rot  (matrix-angle own)
        [m rot]  (reduce (fn [[m rot] spec]
                           (case (:property spec)
                             :transform
                             (if-let [[v vr] (transform-value spec t side steady? box)]
                               (if (and (= :sum (:additive spec)) (= :smil (:source spec)))
                                 [(gmt/multiply m v) (+ rot vr)]
                                 [v vr])
                               [m rot])
                             [m rot]))
                         [own own-rot]
                         specs)
        [m rot]  (if-let [motion (some #(when (= :motion (:property %)) %) specs)]
                   (if-let [[mm mr] (motion-value motion t side steady?)]
                     [(gmt/multiply mm m) (+ rot mr)]
                     [m rot])
                   [m rot])
        geometry (into {}
                       (keep (fn [spec]
                               (when (contains? #{:x :y :cx :cy :r :rx :ry} (:property spec))
                                 (when-let [v (spec-value spec t side steady?)]
                                   [(:property spec) v]))))
                       specs)
        m        (cond-> m (seq geometry) (gmt/multiply (geometry-matrix node geometry)))
        rest-m   (gmt/multiply to-board built)
        delta    (gmt/multiply (gmt/multiply to-board m) (gmt/inverse rest-m))]
    [delta (- rot own-rot)]))

(defn- fixed-point
  "The point the change `m` keeps where it is, nil for none near."
  [m]
  (let [{:keys [a b c d e f]} m
        ;; (I - A) p = t
        m00 (- 1 a) m01 (- c) m10 (- b) m11 (- 1 d)
        det (- (* m00 m11) (* m01 m10))]
    (when (> (mth/abs det) 1e-6)
      (gpt/point (/ (- (* m11 e) (* m01 f)) det)
                 (/ (- (* m00 f) (* m10 e)) det)))))

(defn- transforms
  "The own transform (`:own`) of each element of the SVG `root`, by its
  key, the one it is built with (`:built`, see `clean`) and the one of
  its ancestors (`:parent-m`); the drawing of a mask in the space of the
  element it masks."
  [root]
  (let [acc    (volatile! {})
        ids    (volatile! {})
        masked (volatile! [])]
    (letfn [(walk [node parent-m]
              (let [built (csvg/parse-transform (get-in node [:attrs :transform]))
                    own   (or (::rest-transform node) built)
                    m     (gmt/multiply parent-m built)]
                (vswap! acc assoc (::sb/element-key node) {:own own :built built :parent-m parent-m})
                (when-let [id (get-in node [:attrs :id])] (vswap! ids assoc id node))
                (when-let [mask (sb/mask-id (:attrs node))] (vswap! masked conj [mask m]))
                (doseq [child (:content node) :when (element? child)]
                  (walk child m))))]
      (walk root (gmt/matrix))
      (doseq [[id m] @masked
              child (:content (get @ids id))
              :when (element? child)]
        (walk child m))
      @acc)))

(def ^:private node-properties
  #{:transform :motion :x :y :cx :cy :r :rx :ry :opacity :visibility :display})

(def ^:private paint-properties
  #{:fill :stroke :fill-opacity :stroke-opacity :stroke-width :stroke-dashoffset})

(defn- inheritors
  "The keys of the elements under the element `k` (itself first) that
  take its `property`: not those that set it themselves."
  [idx k property]
  (let [prop (name property)
        sets? (fn [node] (or (contains? (:attrs node) (keyword prop))
                             (str/includes? (str (get-in node [:attrs :style])) (str prop ":"))))]
    (loop [queue [k] acc []]
      (if-let [k' (first queue)]
        (let [{:keys [node children]} (get-in idx [:by-key k'])]
          (if (and (not= k k') (sets? node))
            (recur (rest queue) acc)
            (recur (into (vec (rest queue)) children) (conj acc k'))))
        acc))))

(defn- inherit-targets
  "The keys of the drawable elements among the `inheritors`."
  [idx k property]
  (filterv #(contains? drawable-tags (get-in idx [:by-key % :node :tag]))
           (inheritors idx k property)))

(defn- entry
  "A keyframe of `property` at `t` with the value `v` and the easing `e`
  (see `fit`)."
  [property t v e index]
  (merge {:property property :time t :value v}
         (when (some? index) {:index index})
         (if (= :step e)
           {:interpolation :step}
           {:easing (if (vector? e) {:type :bezier :curve e} :linear)})))

(def ^:private moving?
  #(re-find #"^(animation|transition|transform|offset)" %))

(defn- node-box
  "The box `[x y w h]` of the element `node` in its own space, from its
  geometry; nil for one the import does not measure."
  [{:keys [tag attrs]}]
  (let [n      #(d/parse-double (get attrs %) 0)
        around (fn [points]
                 (when (seq points)
                   (let [r (grc/points->rect points)] [(:x r) (:y r) (:width r) (:height r)])))]
    (case tag
      (:rect :image)       [(n :x) (n :y) (n :width) (n :height)]
      :circle              (let [r (n :r)] [(- (n :cx) r) (- (n :cy) r) (* 2 r) (* 2 r)])
      :ellipse             [(- (n :cx) (n :rx)) (- (n :cy) (n :ry)) (* 2 (n :rx)) (* 2 (n :ry))]
      :line                (around [(gpt/point (n :x1) (n :y1)) (gpt/point (n :x2) (n :y2))])
      (:polyline :polygon) (around (map (fn [[x y]] (gpt/point x y)) (partition 2 (numbers (:points attrs)))))
      :path                (around (try (polyline (path/from-string (:d attrs)))
                                        (catch #?(:clj Throwable :cljs :default) _ nil)))
      nil)))

(defn- known-blend?
  "Whether the CSS blend mode `v` is one Penpot has."
  [v]
  (contains? cts/blend-modes (keyword (str/lower (str/trim (str v))))))

(defn- clean
  "The element `node` without its animations, scripts and styles, the
  static CSS of each element (`cascaded`, by key) in its style, as the
  static import reads only that, and a static CSS transform as its
  transform, around its `transform-origin` in the view box (`view`,
  `[0 0 w h]`) or the box of the element (`transform-box: fill-box`).
  An element `moving` (keys) animates whose transform is singular at
  rest (a pop-in from `scale(0)`) is built without it, at its full
  size, the transform kept as `::rest-transform` for its animation to
  start from (see `transforms`). What the import cannot apply (a blend
  mode Penpot does not have, a box it does not measure) is counted by
  `ctx` and left out."
  [{:keys [ctx cascaded moving view] :as env} node]
  (let [decls      (get cascaded (::sb/element-key node))
        ;; each property once, its last value, as the cascade ends
        rules      (->> (reverse decls)
                        (remove #(moving? (first %)))
                        (reduce (fn [[seen acc] [p :as decl]]
                                  (if (contains? seen p) [seen acc] [(conj seen p) (conj acc decl)]))
                                [#{} ()])
                        (second))
        decl       (fn [prop] (some (fn [[p v]] (when (= prop p) v)) (reverse decls)))
        unknown?   #(and (some? %) (not (known-blend? %)))
        css-blend  (some (fn [[p v]] (when (= "mix-blend-mode" p) v)) rules)
        attr-blend (get-in node [:attrs :mix-blend-mode])
        rules      (cond->> rules (unknown? css-blend) (remove #(= "mix-blend-mode" (first %))))
        css-t      (decl "transform")
        box        (if (str/includes? (str/lower (str (decl "transform-box"))) "fill") (node-box node) view)
        node       (cond-> node
                     (seq rules)
                     (-> (update :attrs #(apply dissoc % (map (comp keyword first) rules)))
                         (assoc-in [:attrs :style] (str/join ";" (map (fn [[p v]] (str p ":" v)) rules))))

                     (unknown? attr-blend)
                     (update :attrs dissoc :mix-blend-mode)

                     (and (some? css-t) (some? box))
                     (assoc-in [:attrs :transform]
                               (str (first (css-matrix (css-transform css-t)
                                                       (origin-point (decl "transform-origin") box)
                                                       box)))))
        rest-m     (csvg/parse-transform (get-in node [:attrs :transform]))
        node       (cond-> node
                     (and (contains? moving (::sb/element-key node))
                          (< (mth/abs (gmt/determinant rest-m)) 1e-9))
                     (-> (update :attrs dissoc :transform)
                         (assoc ::rest-transform rest-m)))]
    (when (or (unknown? css-blend) (unknown? attr-blend) (and (some? css-t) (nil? box)))
      (ctx :skip :attribute))
    (update node :content
            (fn [content]
              (into []
                    (keep #(cond
                             (not (element? %)) %
                             (contains? (into animation-tags #{:style :script}) (:tag %)) nil
                             :else (clean env %)))
                    content)))))

(defn- hoist-defs
  "The SVG `root` with the content of all its `<defs>` in one at its end:
  they draw nothing, and in place the static import makes layers of them
  as large as the SVG, which would make the boards of the elements they
  are in as large too. The import finds definitions by their ids
  anywhere."
  [root]
  (let [defs (volatile! [])
        walk (fn walk [node]
               (update node :content
                       (fn [content]
                         (into []
                               (keep (fn [child]
                                       (cond
                                         (not (element? child)) child
                                         (= :defs (:tag child))
                                         ;; the defs in it first, which the walk hoists
                                         (let [inner (:content (walk child))]
                                           (vswap! defs into (filter element?) inner)
                                           nil)
                                         :else (walk child))))
                               content))))
        root (walk root)]
    (cond-> root
      (seq @defs) (update :content conj {:tag :defs :attrs {} :content @defs}))))

(defn- read-svg
  "The parsed SVG `svg-data` cleaned (see `clean` and `hoist-defs`), with
  the specs of its animations and the transforms of its elements."
  [ctx svg-data]
  (let [idx       (index svg-data)
        sheet     (stylesheet (str/join "\n" (:css idx)))
        cascaded  (into {} (map (fn [k] [k (cascade sheet idx k)])) (keys (:by-key idx)))
        rest-of   (partial rest-text idx cascaded)
        css       (into [] (mapcat (fn [[k decls]] (css-specs ctx (:keyframes sheet) k decls rest-of))) cascaded)
        ;; a cycle shorter than `min-dur` shows nothing a board can play
        playable? (fn [{:keys [dur]}]
                    (or (nil? dur) (not (mth/finite? dur)) (>= dur min-dur)
                        (do (ctx :skip :timing) false)))
        specs     (->> (into (smil-specs ctx idx rest-of) css)
                       (filter playable?)
                       (map expand-steps)
                       (map #(cond-> % (= :transform (:property %)) (transform-spec)))
                       (vec))
        moving    (into #{} (comp (filter #(contains? #{:transform :motion} (:property %))) (map :key)) specs)
        ;; the box of `transform-origin`: the view box, from its origin
        [_ _ w h] (sb/svg-dimensions svg-data)
        root      (hoist-defs (clean {:ctx ctx :cascaded cascaded :moving moving :view [0 0 w h]} (:root idx)))]
    {:idx idx :specs specs :root root :own-of (transforms root)}))

(defn- board-timing
  "The duration of the board that plays `specs`, and whether some of
  them repeat for ever (the board then loops)."
  [ctx specs]
  (let [loops   (keep (fn [spec]
                        (when (and (not (mth/finite? (active-duration spec))) (some? (:dur spec)) (mth/finite? (:dur spec)))
                          (* (:dur spec) (if (contains? #{:alternate :alternate-reverse} (:direction spec)) 2 1))))
                      specs)
        ends    (keep (fn [spec] (let [a (active-duration spec)] (when (mth/finite? a) (+ (:begin spec) a)))) specs)
        gcd     (fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))
        cycle   (when (seq loops)
                  ;; a loop longer than the longest board plays cut to it
                  (let [ms   (map #(long (mth/clamp (mth/round %) min-dur max-board)) loops)
                        lcm  (reduce (fn [a b] (/ (* a b) (gcd a b))) ms)
                        most (reduce max ms)]
                    (if (and (<= lcm (max 20000 most)) (every? #(<= % max-board) loops))
                      lcm
                      (do (ctx :skip :loop-length) most))))
        finite  (if (seq ends) (reduce max ends) 0)]
    [(int (mth/round (max 100 (if (some? cycle)
                                (* cycle (max 1 (mth/ceil (/ (min finite 60000) cycle))))
                                (min finite max-board)))))
     (some? cycle)]))

(defn- pivot-of
  "The point the changes of an element (`samples`, matrices with their
  rotation) turn or scale around, else the centre of `shape`."
  [samples shape]
  (let [turned (->> samples (sort-by #(- (mth/abs (second %)))) (first))
        scaled (->> samples
                    (sort-by #(let [[_ _ _ sx sy] (decompose (first %) nil)]
                                (- (+ (mth/abs (- 1 sx)) (mth/abs (- 1 sy))))))
                    (first))
        b      (bounds shape)
        center (grc/rect->center b)
        size   (+ (:width b) (:height b) 1)]
    (or (some->> [turned scaled]
                 (keep (fn [[m rot]]
                         (let [[_ _ _ sx sy] (decompose m nil)]
                           (when (or (> (mth/abs rot) 0.5) (> (mth/abs (- 1 sx)) 0.001) (> (mth/abs (- 1 sy)) 0.001))
                             (fixed-point m)))))
                 (first)
                 (#(when (< (gpt/distance % center) (* 10 size)) %)))
        center)))

(defn- element-board
  "The board of the shape `shape-id` of the element `k`, which plays its
  moves and fades (`own`, specs), and its keyframes: `{:node :fade?
  :frames}`; `env` holds what the board is played in."
  [ctx {:keys [idx own-of objects to-board vb-w vb-h bx by duration steady?]} k own shape-id]
  (let [elem     (get-in idx [:by-key k])
        {own-m :own built-m :built parent-m :parent-m} (get own-of k)
        shape    (get objects shape-id)
        moves    (filterv #(contains? #{:transform :motion :x :y :cx :cy :r :rx :ry} (:property %)) own)
        fades    (filterv #(contains? #{:opacity :visibility :display} (:property %)) own)
        board-m  (gmt/multiply to-board parent-m)
        ;; the box of the percentages and the origin of CSS
        local    (let [inv (gmt/inverse (gmt/multiply board-m built-m))
                       r   (grc/points->rect (map #(gpt/transform % inv) (grc/rect->points (bounds shape))))]
                   [(:x r) (:y r) (:width r) (:height r)])
        box      (or (some #(when (= :css (:source %))
                              (if (str/includes? (str (:box %)) "fill") local [0 0 vb-w vb-h]))
                           moves)
                     [0 0 vb-w vb-h])
        delta    (fn [t side] (node-delta (:node elem) own-m built-m moves board-m box t side steady?))
        times    (->> moves (mapcat #(spec-times % duration steady?)) (concat [0 duration]))
        ;; the point its transform turns and scales around
        origin   (some (fn [spec]
                         (when (= :transform (:property spec))
                           (if (= :css (:source spec))
                             (gpt/transform (origin-point (:origin spec) box) board-m)
                             (when (= "rotate" (:type spec))
                               (let [[_ cx cy] (first (:values spec))
                                     space   (if (= :sum (:additive spec)) (gmt/multiply board-m own-m) board-m)]
                                 (gpt/transform (gpt/point (or cx 0) (or cy 0)) space))))))
                       moves)
        samples  (map #(delta % :right) times)
        turns?   (some (fn [[m rot]]
                         (let [[_ _ _ sx sy] (decompose m nil)]
                           (or (> (mth/abs rot) 0.5) (> (mth/abs (- 1 sx)) 0.001) (> (mth/abs (- 1 sy)) 0.001))))
                       samples)
        pivot    (if turns? (or origin (pivot-of samples shape)) (grc/rect->center (bounds shape)))
        node-id  (uuid/next)
        fade?    (boolean (some #(= :opacity (:property %)) fades))
        base-op  (:opacity shape 1)
        ;; it plays the opacity of the element
        node     (cond-> (node-shape node-id shape pivot) fade? (assoc :opacity base-op))
        rest-x   (- (:x node) bx)
        rest-y   (- (:y node) by)
        channel  (fn [property t side]
                   (let [[m rot] (delta t side)
                         moved   (gpt/transform pivot m)
                         [_ _ r sx sy skew] (decompose m rot)]
                     (when (> (mth/abs skew) 0.02) (ctx :skip :skew))
                     (case property
                       :x        (+ rest-x (- (:x moved) (:x pivot)))
                       :y        (+ rest-y (- (:y moved) (:y pivot)))
                       :rotation r
                       :scale-x  sx
                       :scale-y  sy)))
        ;; a fading property plays its last animation that plays then,
        ;; else its value at rest; display and visibility hide as 0
        resting  (fn [property]
                   (if (= :opacity property)
                     (if fade? base-op 1)
                     (or (some #(when (= property (:property %)) (:rest %)) fades) 1)))
        value    (fn [property t side]
                   (or (some #(when (= property (:property %)) (spec-value % t side steady?)) (rseq fades))
                       (resting property)))
        opacity  (fn [t side]
                   (* (value :opacity t side) (value :display t side) (value :visibility t side)))
        moved    (when (seq moves)
                   (for [[property at-rest] [[:x rest-x] [:y rest-y] [:rotation 0] [:scale-x 1] [:scale-y 1]]
                         [t v e] (keyframes #(channel property %1 %2) moves (tolerances property) duration at-rest steady?)]
                     (entry property t v e nil)))
        faded    (when (seq fades)
                   (for [[t v e] (keyframes opacity fades (tolerances :opacity) duration (if fade? base-op 1) steady?)]
                     (entry :opacity t (mth/clamp v 0 1) e nil)))]
    {:node node :fade? fade? :frames (mapv #(vector node-id %) (concat moved faded))}))

(defn- paint-frames
  "The keyframes `[shape-id keyframe]` of the paints of the shape
  `shape-id` of the element `target`, which `spec` animates."
  [ctx {:keys [idx own-of objects to-board duration steady?]} spec target shape-id]
  (let [shape  (get objects shape-id)
        node   (get-in idx [:by-key target :node])
        value  (fn [t side] (spec-value spec t side steady?))
        put    (fn [prop index f at-rest]
                 (for [[t v e] (keyframes f [spec] (tolerances prop) duration at-rest steady?)]
                   [shape-id (entry prop t v e index)]))
        colour (fn [prop at-rest]
                 (for [[t v e] (keyframes #(let [v (value %1 %2)] (if (vector? v) v at-rest))
                                          [spec] (tolerances prop) duration at-rest steady?)]
                   [shape-id (entry prop t (hex v) e 0)]))
        skip   #(do (ctx :skip :paint) nil)]
    (case (:property spec)
      :fill
      (if-let [at-rest (some-> (get-in shape [:fills 0 :fill-color]) clr/hex->rgb)]
        (colour :fill-color at-rest)
        (skip))

      :stroke
      (if-let [at-rest (some-> (get-in shape [:strokes 0 :stroke-color]) clr/hex->rgb)]
        (colour :stroke-color at-rest)
        (skip))

      :fill-opacity
      (if (seq (:fills shape))
        (let [at-rest (get-in shape [:fills 0 :fill-opacity] 1)]
          (put :fill-opacity 0 #(or (value %1 %2) at-rest) at-rest))
        (skip))

      :stroke-opacity
      (if (seq (:strokes shape))
        (let [at-rest (get-in shape [:strokes 0 :stroke-opacity] 1)]
          (put :stroke-opacity 0 #(or (value %1 %2) at-rest) at-rest))
        (skip))

      :stroke-width
      (if (seq (:strokes shape))
        (let [at-rest (get-in shape [:strokes 0 :stroke-width] 1)
              svg     (or (number (or (get-in node [:attrs :stroke-width])
                                      (some->> (get-in node [:attrs :style]) (re-find #"stroke-width\s*:\s*([^;]+)") second)))
                          1)
              ratio   (if (pos? svg) (/ at-rest svg) 1)]
          (put :stroke-width 0 #(* ratio (or (value %1 %2) svg)) at-rest))
        (skip))

      :stroke-dashoffset
      (let [attrs    (:attrs node)
            dash     (numbers (or (:stroke-dasharray attrs)
                                  (some->> (str (:style attrs)) (re-find #"stroke-dasharray\s*:\s*([^;]+)") second)))
            {built-m :built parent-m :parent-m} (get own-of target)
            scale    (mth/sqrt (mth/abs (gmt/determinant (gmt/multiply to-board parent-m built-m))))
            total    (or (number (:pathLength attrs))
                         (when (pos? scale) (/ (outline-length shape) scale))
                         0)]
        (if (and (pos? total) (seq dash) (every? #(>= % (* 0.98 total)) dash) (seq (:strokes shape)))
          (concat (put :trim-start nil #(mth/clamp (/ (- (or (value %1 %2) 0)) total) 0 1) 0)
                  (put :trim-end nil #(mth/clamp (- 1 (/ (or (value %1 %2) 0) total)) 0 1) 1))
          (do (ctx :skip :dash) nil))))))

(def ^:private outline-properties
  #{:d :points})

(defn- outline-numbers
  "The numbers of the segments of `content`, in their order."
  [content]
  (into [] (mapcat (fn [{:keys [params]}]
                     [(:x params 0) (:y params 0) (:c1x params 0) (:c1y params 0) (:c2x params 0) (:c2y params 0)]))
        (vec content)))

(defn- numbers-content
  "The content of the structure of `template` with the numbers `ns` (see
  `outline-numbers`)."
  [template ns]
  (path/from-plain
   (mapv (fn [{:keys [command] :as segment} [x y c1x c1y c2x c2y]]
           (case command
             :close-path segment
             :curve-to   (assoc segment :params {:x x :y y :c1x c1x :c1y c1y :c2x c2x :c2y c2y})
             (assoc segment :params {:x x :y y})))
         (vec template)
         (partition 6 ns))))

(defn- half-way
  "Where a value that cannot morph switches over a segment of `easing`
  (see `ease`): at the progress where the eased one reaches 0.5, as CSS
  plays a discrete value (and Blink a SMIL spline); the middle for a
  linear one."
  [easing]
  (if (nil? easing)
    0.5
    (loop [lo 0.0 hi 1.0 n 0]
      (if (= n 30)
        hi
        (let [m (/ (+ lo hi) 2)]
          (if (< (ease easing m) 0.5)
            (recur m hi (inc n))
            (recur lo m (inc n))))))))

(defn- path-frames
  "The keyframes `[shape-id keyframe]` of the outline (`:content`, see
  `ctao/outline-of`) of the path `shape-id` of the element `target`,
  whose `d` or `points` `spec` animates: its values in the board as the
  element is placed, which morph when they all have one structure, and
  else each held (a step keyframe at each), switching where the easing of
  its interval reaches half way (see `half-way`), as CSS and Blink play
  values that cannot morph (Gecko splits a SMIL animation of more than
  two into equal parts instead).
  Past what an outline row holds (see `ctao/row-takes?`), or changing but
  leaving no keyframe, none, counted as `:path-morph`."
  [ctx {:keys [idx own-of objects to-board duration steady?]} spec target shape-id]
  (let [shape    (get objects shape-id)
        tag      (get-in idx [:by-key target :node :tag])
        {built-m :built parent-m :parent-m} (get own-of target)
        m        (gmt/multiply to-board parent-m built-m)
        content  (fn [s]
                   (when (string? s)
                     (let [d (if (= :points (:property spec))
                               (let [ps (partition 2 (numbers s))]
                                 (when (seq ps)
                                   (str "M" (str/join " L" (map #(str/join "," %) ps)) (when (= :polygon tag) " Z"))))
                               s)]
                       (some-> d path/from-string (path/transform-content m)))))
        values   (mapv content (:values spec))
        template (first values)
        rest     (content (:rest spec))
        same?    #(and (some? %) (morph/same-structure? template %))
        frames   (cond
                   (or (not= :path (:type shape)) (nil? template) (not (every? some? values)))
                   nil

                   ;; one structure: their numbers morph
                   (every? same? values)
                   (let [spec    (assoc spec
                                        :values (mapv outline-numbers values)
                                        :rest (outline-numbers (if (same? rest) rest template)))
                         at-rest (:rest spec)]
                     (for [[t v e] (keyframes #(spec-value spec %1 %2 steady?) [spec] 0.25 duration at-rest steady?)]
                       [shape-id (entry :content t (ctao/outline-of (assoc shape :content (numbers-content template v))) e nil)]))

                   ;; others: each held, switching where the easing of its
                   ;; interval reaches half way (see `half-way`), as CSS
                   ;; and Blink play values that cannot morph (an explicit
                   ;; discrete one as it says)
                   :else
                   (let [discrete? (and (seq (:easings spec)) (every? #(= :step %) (:easings spec)))
                         spec      (if discrete?
                                     (assoc spec :values values)
                                     (let [ts   (vec (:times spec))
                                           es   (concat (:easings spec) (repeat nil))
                                           dur  (:dur spec)
                                           ;; on a whole ms, as the keyframes are cut: a
                                           ;; switch a moment after it would hold the old
                                           ;; value there (see `keyframes`)
                                           snap (fn [f]
                                                  (if (and (number? dur) (pos? dur) (mth/finite? dur))
                                                    (/ (mth/round (* f dur)) dur)
                                                    f))
                                           mid  (map (fn [t0 t1 e]
                                                       (if (= :step e) t1 (snap (+ t0 (* (- t1 t0) (half-way e))))))
                                                     ts (subvec ts 1) es)]
                                       (assoc spec
                                              :times (vec (concat [(first ts)] mid [(peek ts)]))
                                              :values (conj values (peek values))
                                              :easings (vec (repeat (count values) :step)))))
                         spec      (assoc spec :rest (or rest template))
                         at-rest   (:rest spec)]
                     (for [[t v e] (keyframes #(spec-value spec %1 %2 steady?) [spec] 0.25 duration at-rest steady?)]
                       [shape-id (entry :content t (ctao/outline-of (assoc shape :content v)) e nil)])))
        kfs      (mapv second frames)]
    (if (and (some? frames)
             ;; one that changes but leaves no keyframe is counted, never
             ;; dropped without a word
             (or (seq frames) (apply = values))
             (every? #(ctao/outline-fits? (:value %)) kfs)
             (ctao/outline-row-fits? kfs))
      frames
      (do (ctx :skip :path-morph) nil))))

(defn- shown
  "`shape` that its board shows and hides by `properties` (`:display`,
  `:visibility`): not hidden, nor kept hidden by them in its SVG."
  [shape properties]
  (let [props (vec properties)]
    (-> shape
        (cond-> (contains? properties :display) (dissoc :hidden))
        (d/update-when :svg-attrs #(apply dissoc % props))
        (d/update-in-when [:svg-attrs :style] #(apply dissoc % props))
        (d/update-when :svg-attrs #(cond-> % (empty? (:style %)) (dissoc :style))))))

(defn svg->shapes
  "The shapes and the timeline of the animated SVG `svg-data` (parsed
  with `tubax`, its `:name` the name of the file, its `:image-data` the
  uploaded images by their `href`): a board `id` with its top left
  corner at `x`, `y` and the shapes in it, parents before their children
  and the bottom-most first, ready to be added in that order; and the
  timeline of the board. `:skipped` counts what a board cannot play, by
  feature."
  [svg-data {:keys [id x y]}]
  (let [skipped  (volatile! {})
        ctx      (fn [op & args]
                   (case op
                     :skip (vswap! skipped update (first args) (fnil inc 0))))
        {:keys [idx specs root own-of]} (read-svg ctx svg-data)
        [vb-x vb-y vb-w vb-h] (sb/svg-dimensions root)
        board-id (or id (uuid/next))
        bx       (mth/round x)
        by       (mth/round y)
        [root-shape children]
        (sb/create-svg-shapes (uuid/next)
                              (assoc root :name (:name svg-data) :image-data (:image-data svg-data))
                              {:x (- bx vb-x) :y (- by vb-y)}
                              {} board-id board-id #{} false)
        shapes   (fix-groups (into [root-shape] children))
        shapes-of (reduce (fn [acc shape]
                            (if-let [k (or (::sb/element-key (meta shape))
                                           (when (= (:id shape) (:id root-shape)) (::sb/element-key root)))]
                              (update acc k (fnil conj []) (:id shape))
                              acc))
                          {}
                          shapes)
        [duration steady?] (board-timing ctx specs)
        ;; one that starts too many segments in the board is too fast for it
        specs    (filterv #(or (<= (segment-starts % duration) max-starts)
                               (do (ctx :skip :timing) false))
                          specs)
        env      {:idx idx :own-of own-of :objects (into {} (map (juxt :id identity)) shapes)
                  :to-board (gmt/translate-matrix (- bx vb-x) (- by vb-y))
                  :vb-w vb-w :vb-h vb-h :bx bx :by by :duration duration :steady? steady?}
        ;; a masked element moves with its mask, which is in its space
        groups-of (reduce (fn [acc shape]
                            (if-let [k (::sb/mask-of (meta shape))]
                              (update acc k (fnil conj []) (:id shape))
                              acc))
                          {}
                          shapes)
        by-key   (group-by :key specs)
        boards   (into {}
                       (for [[k specs] by-key
                             :let [own (filterv #(contains? node-properties (:property %)) specs)]
                             :when (seq own)
                             shape-id (get groups-of k (get shapes-of k))]
                         [shape-id (element-board ctx env k own shape-id)]))
        paints   (for [[k specs] by-key
                       spec specs
                       :when (contains? paint-properties (:property spec))
                       target (inherit-targets idx k (:property spec))
                       shape-id (get shapes-of target)
                       frame (paint-frames ctx env spec target shape-id)]
                   frame)
        outlines (for [[k specs] by-key
                       spec specs
                       :when (contains? outline-properties (:property spec))
                       shape-id (get shapes-of k)
                       frame (path-frames ctx env spec k shape-id)]
                   frame)
        frames   (-> (into [] (mapcat :frames) (vals boards))
                     (into paints)
                     (into outlines))
        trims    (into #{} (keep (fn [[id kf]] (when (contains? #{:trim-start :trim-end} (:property kf)) id))) frames)
        ;; the properties that show and hide each shape on its board (a
        ;; visibility goes to what the element holds)
        toggled  (->> (for [{:keys [key property]} specs
                            :when (contains? #{:display :visibility} property)
                            k  (if (= :visibility property) (inheritors idx key property) [key])
                            id (get shapes-of k)]
                        [id property])
                      (reduce (fn [acc [id property]] (update acc id (fnil conj #{}) property)) {}))
        shapes   (->> shapes
                      (map (fn [shape]
                             (cond-> shape
                               ;; its board plays its opacity
                               (:fade? (get boards (:id shape)))
                               (dissoc :opacity)

                               ;; its board shows and hides it
                               (contains? toggled (:id shape))
                               (shown (get toggled (:id shape)))

                               ;; a line that draws itself is not dashed
                               (contains? trims (:id shape))
                               (-> (update :strokes (fn [strokes] (mapv #(assoc % :stroke-style :solid) strokes)))
                                   (d/update-when :svg-attrs dissoc :strokeDasharray :strokeDashoffset)
                                   (d/update-in-when [:svg-attrs :style] dissoc :strokeDasharray :strokeDashoffset)))))
                      (mapcat (fn [shape]
                                (if-let [node (:node (get boards (:id shape)))]
                                  [node (assoc shape :parent-id (:id node))]
                                  [shape])))
                      (vec)
                      (fix-groups))
        board    (cts/setup-shape
                  {:type :frame :id board-id
                   :name (or (some-> (:name svg-data) (str/replace #"\.svg$" "")) "SVG")
                   :x bx :y by :width vb-w :height vb-h
                   :parent-id uuid/zero :frame-id uuid/zero
                   :fills [] :strokes [] :shapes []})
        timeline (reduce-kv (fn [timeline shape-id frames]
                              (cta/add-keyframes timeline shape-id (map second frames)))
                            (cta/make-timeline {:board-id board-id
                                                :name (:name board)
                                                :duration duration
                                                :playback (if steady? :loop :once)})
                            (group-by first frames))
        ;; the outlines of each path that morphs are made for its box
        timeline (reduce (fn [timeline shape]
                           (cond-> timeline (= :path (:type shape)) (ctao/ensure-outline-box shape)))
                         timeline
                         shapes)]
    {:shapes   (with-frame-ids (into [board] shapes))
     :timeline (assoc timeline :duration duration)
     :skipped  @skipped}))

(defn animated?
  "Whether the parsed SVG `svg-data` animates: it has SMIL animations or
  CSS keyframes."
  [svg-data]
  (letfn [(walk [node]
            (and (element? node)
                 (or (contains? animation-tags (:tag node))
                     (and (= :style (:tag node)) (str/includes? (style-text node) "@keyframes"))
                     (some walk (:content node)))))]
    (boolean (walk svg-data))))
