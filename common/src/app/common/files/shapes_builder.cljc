;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.files.shapes-builder
  "A SVG to Shapes builder."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.files.helpers :as cfh]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.shapes.common :as gsc]
   [app.common.geom.shapes.transforms :as gst]
   [app.common.math :as mth]
   [app.common.schema :as sm :refer [max-safe-int min-safe-int]]
   [app.common.svg :as csvg]
   [app.common.types.color :as clr]
   [app.common.types.path :as path]
   [app.common.types.path.segment :as path.segm]
   [app.common.types.shape :as cts]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [cuerdas.core :as str]))

(def default-rect
  {:x 0 :y 0 :width 1 :height 1})

(defn- assert-valid-num [attr num]
  (when-not (and (d/num? num)
                 (<= num max-safe-int)
                 (>= num min-safe-int))
    (ex/raise :type :assertion
              :code :data-validation
              :hint (str "invalid numeric value for `" attr "`: " num)))
  (cond
    (and (> num 0) (< num 1))    1
    (and (< num 0) (> num -1))  -1
    :else                       num))

(defn- assert-valid-pos-num
  [attr num]

  (when-not (pos? num)
    (ex/raise :type :assertion
              :code :data-validation
              :hint (str "invalid numeric value for `" attr "`: " num " (should be positive)")))
  num)

(defn- assert-valid-blend-mode
  [mode]
  (let [value (-> mode str/trim str/lower keyword)]

    (when-not (contains? cts/blend-modes value)
      (ex/raise :type :assertion
                :code :data-validation
                :hint (str "unexpected blend mode: " value)))
    value))

(defn svg-dimensions
  "The `[x y width height]` of the viewBox of the parsed SVG `data`, or
  of its width and height without one."
  [{:keys [attrs] :as data}]
  (let [width   (:width attrs 100)
        height  (:height attrs 100)
        viewbox (or (:viewBox attrs)
                    (dm/str "0 0 " width " " height))

        [x y width height] (csvg/parse-numbers viewbox)

        width   (if (= width 0) 1 width)
        height  (if (= height 0) 1 height)]

    [(assert-valid-num :x x)
     (assert-valid-num :y y)
     (assert-valid-pos-num :width width)
     (assert-valid-pos-num :height height)]))

(declare create-svg-root)
(declare create-svg-children)
(declare parse-svg-element)
(declare resolve-element-name)

(defn- process-gradient-stops
  "Processes gradient stops to extract stop-color and stop-opacity from style attributes
   and convert them to direct attributes. This ensures stops with style='stop-color:#...;stop-opacity:1'
   are properly converted to stop-color and stop-opacity attributes."
  [stops]
  (mapv (fn [stop]
          (let [stop-attrs (:attrs stop)
                stop-style (get stop-attrs :style)
                ;; Parse style if it's a string using csvg/parse-style utility
                parsed-style (when (and (string? stop-style) (seq stop-style))
                               (csvg/parse-style stop-style))
                ;; Extract stop-color and stop-opacity from style
                style-stop-color (when parsed-style (:stop-color parsed-style))
                style-stop-opacity (when parsed-style (:stop-opacity parsed-style))
                ;; Merge: use direct attributes first, then style values as fallback
                final-attrs (cond-> stop-attrs
                              (and style-stop-color (not (contains? stop-attrs :stop-color)))
                              (assoc :stop-color style-stop-color)

                              (and style-stop-opacity (not (contains? stop-attrs :stop-opacity)))
                              (assoc :stop-opacity style-stop-opacity)

                              ;; Remove style attribute if we've extracted its values
                              (or style-stop-color style-stop-opacity)
                              (dissoc :style))]
            (assoc stop :attrs final-attrs)))
        stops))

(defn- resolve-gradient-href
  "Resolves xlink:href references in gradients by merging the referenced gradient's
   stops and attributes with the referencing gradient. This ensures gradients that
   reference other gradients (like linearGradient3550 referencing linearGradient3536)
   inherit the stops from the base gradient.
   
   According to SVG spec, when a gradient has xlink:href:
   - It inherits all attributes from the referenced gradient
   - It inherits all stops from the referenced gradient
   - The referencing gradient's attributes override the base ones
   - If the referencing gradient has stops, they replace the base stops
   
   Returns the defs map with all gradient href references resolved."
  [defs]
  (letfn [(resolve-gradient [gradient-id gradient-node defs visited]
            (if (contains? visited gradient-id)
              (do
                #?(:cljs (js/console.warn "[resolve-gradient] Circular reference detected for" gradient-id)
                   :clj nil)
                gradient-node)  ;; Avoid circular references
              (let [attrs (:attrs gradient-node)
                    href-id (or (:href attrs) (:xlink:href attrs))
                    href-id (when (and (string? href-id) (pos? (count href-id)))
                              (subs href-id 1))  ;; Remove leading #

                    base-gradient (when (and href-id (contains? defs href-id))
                                    (get defs href-id))

                    resolved-base (when base-gradient (resolve-gradient href-id base-gradient defs (conj visited gradient-id)))]

                (if resolved-base
                  ;; Merge: base gradient attributes + referencing gradient attributes
                  ;; Use referencing gradient's stops if present, otherwise use base stops
                  (let [base-attrs (:attrs resolved-base)
                        ref-attrs (:attrs gradient-node)

                        ;; Start with base attributes (without id), then merge with ref attributes
                        ;; This ensures ref attributes override base ones
                        base-attrs-clean (dissoc base-attrs :id)
                        ref-attrs-clean (dissoc ref-attrs :href :xlink:href :id)

                        ;; Special handling for gradientTransform: if both have it, combine them
                        base-transform (get base-attrs :gradientTransform)
                        ref-transform (get ref-attrs :gradientTransform)
                        combined-transform (cond
                                             (and base-transform ref-transform)
                                             (str base-transform " " ref-transform)  ;; Apply base first, then ref
                                             :else (or ref-transform base-transform))

                        ;; Merge attributes: base first, then ref (ref overrides)
                        merged-attrs (-> (d/deep-merge base-attrs-clean ref-attrs-clean)
                                         (cond-> combined-transform
                                           (assoc :gradientTransform combined-transform)))

                        ;; If referencing gradient has content (stops), use it; otherwise use base content
                        final-content (if (seq (:content gradient-node))
                                        (:content gradient-node)
                                        (:content resolved-base))

                        ;; Process stops to extract stop-color and stop-opacity from style attributes
                        processed-content (process-gradient-stops final-content)

                        result {:tag (:tag gradient-node)
                                :attrs (assoc merged-attrs :id gradient-id)
                                :content processed-content}]
                    result)
                  ;; Process stops even for gradients without references to extract style attributes
                  (let [processed-content (process-gradient-stops (:content gradient-node))]
                    (assoc gradient-node :content processed-content))))))]
    (let [gradient-tags #{:linearGradient :radialGradient}
          result (reduce-kv
                  (fn [acc id node]
                    (if (contains? gradient-tags (:tag node))
                      (assoc acc id (resolve-gradient id node defs #{}))
                      (assoc acc id node)))
                  {}
                  defs)]
      result)))

(defn- url-id
  "The id of the `url(#id)` reference in `s`."
  [s]
  (some->> s (str) (re-find #"url\(\s*['\"]?#([^)'\"\s]+)") (second)))

(defn mask-id
  "The id of the mask the element with `attrs` refers to."
  [{:keys [mask style]}]
  (or (url-id mask)
      (cond
        (string? style) (some->> style (re-find #"(?:^|;)\s*mask\s*:\s*([^;]+)") (second) (url-id))
        (map? style)    (url-id (:mask style)))))

(defn- white?
  [color]
  (contains? #{"white" "#fff" "#ffffff"} (str/lower (str/trim (str color)))))

(defn- filter-kind
  "What the filter `node` does to the drawing of a mask, as the SVG
  exports of Penpot write it: `:white` or `:black` (it floods it with
  the colour), `:invert` (it inverts its colours); nil for others."
  [node]
  (let [prims  (filterv map? (:content node))
        flood  (some #(when (= :feFlood (:tag %)) (str/lower (str (get-in % [:attrs :flood-color])))) prims)
        matrix (some #(when (= :feColorMatrix (:tag %)) (mapv mth/round (csvg/parse-numbers (get-in % [:attrs :values])))) prims)]
    (cond
      (and (= [:feFlood :feComposite] (mapv :tag prims)) (white? flood))
      :white

      (and (= [:feFlood :feComposite] (mapv :tag prims)) (contains? #{"black" "#000" "#000000"} flood))
      :black

      (and (= [:feColorMatrix] (mapv :tag prims)) (= [-1 0 0 0 1 0 -1 0 0 1 0 0 -1 0 1 0 0 0 1 0] matrix))
      :invert

      :else nil)))

(defn- mask-content
  "The mask mode (see `cts/mask-modes`) of the `<mask>` node `mask`, and
  the nodes it draws: as its mask type says (luminance by default), or
  as the SVG exports of Penpot write the modes (a drawing a filter turns
  white for alpha; a white rect under it, the drawing turned black or
  its colours inverted, for the inverted ones)."
  [mask defs]
  (let [content  (filterv map? (:content mask))
        attrs    (:attrs mask)
        type     (str/lower (str (or (:mask-type attrs)
                                     (some->> (:style attrs) (str) (re-find #"mask-type\s*:\s*(\w+)") (second))
                                     "luminance")))
        filtered (fn [node]
                   (when (= :g (:tag node))
                     (some->> (url-id (get-in node [:attrs :filter])) (get defs) (filter-kind))))
        [base drawn] content
        on-white? (and (= 2 (count content)) (= :rect (:tag base)) (white? (get-in base [:attrs :fill])))]
    (cond
      (and on-white? (= :black (filtered drawn)))  [:alpha-inverted (:content drawn)]
      (and on-white? (= :invert (filtered drawn))) [:luma-inverted (:content drawn)]
      (and (= 1 (count content)) (= :white (filtered base))) [:alpha (:content base)]
      (= "alpha" type)                              [:alpha content]
      :else                                         [:luma content])))

(defn- clip-id
  "The id of the clip path the element with `attrs` refers to."
  [{:keys [style] :as attrs}]
  (or (url-id (:clip-path attrs))
      (cond
        (string? style) (some->> style (re-find #"(?:^|;)\s*clip-path\s*:\s*([^;]+)") (second) (url-id))
        (map? style)    (url-id (:clipPath style)))))

(defn- opaque
  "The SVG `node` and the nodes in it filled opaque white without a
  stroke: what a clip path keeps is where they are."
  [node]
  (if-not (map? node)
    node
    (-> node
        (update :attrs #(-> (dissoc % :style :class :opacity :fill-opacity :stroke :filter)
                            (assoc :fill "#ffffff" :stroke "none")))
        (update :content #(mapv opaque %)))))

(defn- wrap-masked
  "The SVG `node` held by a masked group over the drawing `content`,
  keeping as `mode` says: the group takes its transform, so the drawing
  is in its space, and the key of the element it holds (`::mask-of`), so
  that an animated import moves the outer-most group."
  [node mode content]
  (let [content (filterv map? content)]
    (cond-> {:tag :g
             :attrs (select-keys (:attrs node) [:transform])
             ::mask-mode mode
             ::name (or (::name node) (resolve-element-name (:tag node) (:attrs node)))
             :content [(if (= 1 (count content))
                         (first content)
                         {:tag :g :attrs {} :content content})
                       (-> node
                           (update :attrs dissoc :transform)
                           (dissoc ::mask-of))]}
      (or (contains? node ::mask-of) (contains? node ::element-key))
      (assoc ::mask-of (or (::mask-of node) (::element-key node))))))

(defn- mask-groups
  "The SVG `node` with each element a `<mask>` or a `<clipPath>` of
  `defs` keeps held by a masked group: the drawing of the mask (or the
  clip path, opaque, as an alpha mask) at the bottom, in the space of
  the element, and the element over it, without them. A clip goes
  around a mask."
  [defs node]
  (if-not (map? node)
    node
    (let [node  (cond-> node (seq (:content node)) (update :content #(mapv (partial mask-groups defs) %)))
          attrs (:attrs node)
          mask  (get defs (mask-id attrs))
          mask  (when (and (= :mask (:tag mask))
                           (not= "objectBoundingBox" (get-in mask [:attrs :maskContentUnits])))
                  mask)
          clip  (get defs (clip-id attrs))
          clip  (when (and (= :clipPath (:tag clip))
                           (not= "objectBoundingBox" (get-in clip [:attrs :clipPathUnits])))
                  clip)
          strip (fn [style prop]
                  (if (string? style)
                    (str/replace style (re-pattern (str "(?:^|;)\\s*" prop "\\s*:[^;]*")) "")
                    style))
          bare  (cond-> node
                  (some? mask) (-> (update :attrs dissoc :mask)
                                   (update-in [:attrs :style] strip "mask"))
                  (some? clip) (-> (update :attrs dissoc :clip-path)
                                   (update-in [:attrs :style] strip "clip-path")))]
      (cond-> bare
        (some? mask)
        (as-> n (let [[mode content] (mask-content mask defs)] (wrap-masked n mode content)))

        (some? clip)
        (wrap-masked :alpha
                     (let [content (mapv opaque (:content clip))
                           t       (get-in clip [:attrs :transform])]
                       (if (some? t) [{:tag :g :attrs {:transform t} :content content}] content)))))))

(defn create-svg-shapes
  ([svg-data pos objects frame-id parent-id selected center?]
   (create-svg-shapes (uuid/next) svg-data pos objects frame-id parent-id selected center?))
  ([id svg-data {:keys [x y]} objects frame-id parent-id selected center?]
   (let [[vb-x vb-y vb-width vb-height] (svg-dimensions svg-data)

         unames   (cfh/get-used-names objects)
         svg-name (str/replace (:name svg-data) ".svg" "")

         svg-data (-> svg-data
                      (assoc :x (mth/round
                                 (if center?
                                   (- x vb-x (/ vb-width 2))
                                   x)))
                      (assoc :y (mth/round
                                 (if center?
                                   (- y vb-y (/ vb-height 2))
                                   y)))
                      (assoc :offset-x vb-x)
                      (assoc :offset-y vb-y)
                      (assoc :width vb-width)
                      (assoc :height vb-height)
                      (assoc :name svg-name))

         [def-nodes svg-data]
         (-> svg-data
             (csvg/fix-default-values)
             (csvg/fix-percents)
             (csvg/extract-defs))

         ;; Resolve gradient href references in all defs before processing shapes
         def-nodes (resolve-gradient-href def-nodes)

         ;; In penpot groups have the size of their children. To
         ;; respect the imported svg size and empty space let's create
         ;; a transparent shape as background to respect the imported
         ;; size
         background
         {:tag :rect
          :attrs {:x      (dm/str vb-x)
                  :y      (dm/str vb-y)
                  :width  (dm/str vb-width)
                  :height (dm/str vb-height)
                  :fill   "none"
                  :id     "base-background"}
          :hidden true
          :content []}

         svg-data   (-> svg-data
                        (assoc :defs def-nodes)
                        (assoc :content (into [background]
                                              (map (partial mask-groups def-nodes))
                                              (:content svg-data))))

         root-shape (create-svg-root id frame-id parent-id svg-data)
         root-id    (:id root-shape)

         ;; Create the root shape
         root-attrs (-> (:attrs svg-data)
                        (csvg/format-styles))

         [_ children]
         (reduce (partial create-svg-children objects selected frame-id root-id svg-data)
                 [unames []]
                 (d/enumerate (->> (:content svg-data)
                                   (mapv #(csvg/inherit-attributes root-attrs %)))))

         ;; Collect all defs from children and merge into root shape
         all-defs-from-children (reduce (fn [acc child]
                                          (if-let [child-defs (:svg-defs child)]
                                            (merge acc child-defs)
                                            acc))
                                        {}
                                        children)

         ;; Merge defs from svg-data and children into root shape
         root-shape-with-defs (assoc root-shape :svg-defs (merge def-nodes all-defs-from-children))]

     [root-shape-with-defs children])))

(declare paint-fill)
(declare calculate-rect-metadata)
(declare setup-stroke)
(declare setup-opacity)
(declare setup-other)

(defn- text-prop
  "The text property `k` (a keyword of its CSS name) of the SVG element
  with `attrs`: of its style, or else of its attribute."
  [attrs k]
  (let [style (:style attrs)
        v     (or (if (map? style)
                    (get style k)
                    (some->> (str style)
                             (re-find (re-pattern (str "(?:^|;)\\s*" (name k) "\\s*:\\s*([^;]+)")))
                             (second)))
                  (get attrs k))]
    (when-not (str/blank? (str v))
      (str/trim (str v)))))

(def ^:private text-props
  [:font-family :font-size :font-weight :font-style :letter-spacing
   :text-decoration :text-transform :fill :fill-opacity :text-anchor])

(defn- own-text-props
  [attrs]
  (into {} (keep (fn [k] (when-let [v (text-prop attrs k)] [k v]))) text-props))

(defn- num-str
  "`v` as the text of a number: a whole one without its decimals."
  [v]
  (let [v (mth/precision v 3)]
    (if (== v (mth/round v)) (dm/str (long (mth/round v))) (dm/str v))))

(defn- font-size
  "The pixels of the font size of the text properties `props`, which
  hold it in pixels (see `text-lines`)."
  [props]
  (or (csvg/css-length (:font-size props) 16) 16))

(defn- text-length
  "The pixels of the first of the SVG lengths `s` of a text, `em` of the
  font size `size`."
  [s size]
  (some-> s str str/trim (str/split #"[\s,]+") (first) (csvg/css-length size)))

(defn- text-node
  "The Penpot text node of the text `text` with the SVG text properties
  `props`, its lengths scaled by `scale`; its fill through `defs` (see
  `paint-fill`). The font is the one of the family asked for, by the id
  of a Google font, which the workspace resolves to the fonts it has."
  [text props shape defs scale]
  (let [family  (some-> (:font-family props) (str/split #",") (first) (str/replace #"[\"']" "") (str/trim) (not-empty))
        size    (font-size props)
        weight  (let [w (str/lower (str (:font-weight props "400")))]
                  (case w
                    ("bold" "bolder") "700"
                    "normal"          "400"
                    "lighter"         "300"
                    (if (re-matches #"\d+" w) w "400")))
        italic? (contains? #{"italic" "oblique"} (str/lower (str (:font-style props))))
        none?   (= "none" (:fill props))
        fill    (or (paint-fill (:fill props "#000000") shape defs nil 0) {:fill-color clr/black})
        opacity (d/parse-double (:fill-opacity props) 1)]
    (merge (dissoc txt/default-text-attrs :typography-ref-file :typography-ref-id)
           {:text            text
            :font-id         (if family (dm/str "gfont-" (-> family str/lower (str/replace #"\s+" "-"))) "sourcesanspro")
            :font-family     (or family "sourcesanspro")
            :font-variant-id (dm/str (if (= weight "400") (if italic? "" "regular") weight) (when italic? "italic"))
            :font-size       (num-str (* scale size))
            :font-weight     weight
            :font-style      (if italic? "italic" "normal")
            :letter-spacing  (num-str (* scale (or (text-length (:letter-spacing props) size) 0)))
            :text-transform  (:text-transform props "none")
            :text-decoration (let [d (str (:text-decoration props))]
                               (cond (str/includes? d "underline") "underline"
                                     (str/includes? d "line-through") "line-through"
                                     :else "none"))
            ;; an outlined text has no fill
            :fills           (if none? [] [(update fill :fill-opacity #(* (or % 1) opacity))])})))

(defn- text-lines
  "The lines of the SVG `<text>` elements `nodes`, each with its runs of
  text and their properties, its `:x` `:y` and anchor: a `<tspan>` with
  a position of its own starts one, a `<text>` too. A font size is in
  pixels, one in `em` or `%` of the one it is in."
  [nodes inherited]
  (let [lines (volatile! [])
        line  (fn [x y anchor] (vswap! lines conj {:x x :y y :anchor anchor :runs []}))
        run   (fn [text props] (vswap! lines (fn [ls] (update ls (dec (count ls)) update :runs conj [text props]))))
        walk  (fn walk [node props]
                (let [attrs  (:attrs node)
                      own    (own-text-props attrs)
                      parent (font-size props)
                      props  (cond-> (merge props own)
                               (contains? own :font-size)
                               (assoc :font-size (num-str (or (csvg/css-length (:font-size own) parent) parent))))
                      size   (font-size props)
                      x      (text-length (:x attrs) size)
                      y      (text-length (:y attrs) size)
                      dy     (or (text-length (:dy attrs) size) 0)]
                  (when (or (= :text (:tag node)) (some? y) (not (zero? dy)))
                    (let [prev (peek @lines)]
                      (line (or x (:x prev) 0)
                            (or y (+ (:y prev 0) dy))
                            (:text-anchor props "start"))))
                  (doseq [child (:content node)]
                    (cond
                      (string? child)
                      (let [t (str/replace child #"\s+" " ")]
                        (when-not (str/blank? t) (run t props)))

                      (contains? #{:tspan :a} (:tag child))
                      (walk child props)))))]
    (doseq [node nodes] (walk node inherited))
    (->> @lines
         (keep (fn [l]
                 (let [runs (:runs l)
                       runs (cond-> runs
                              (seq runs) (update 0 update 0 str/ltrim)
                              (seq runs) (update (dec (count runs)) update 0 str/rtrim))]
                   (when (some #(not (str/blank? (first %))) runs)
                     (assoc l :runs runs)))))
         (vec))))

(defn- even-scale
  "The scale of the matrix `m` when it only scales evenly, turns and
  moves, as a Penpot text can (no skew, no mirror), else nil."
  [{:keys [a b c d]}]
  (let [sx (mth/sqrt (+ (* a a) (* b b)))
        sy (mth/sqrt (+ (* c c) (* d d)))]
    (when (and (pos? sx)
               (pos? (- (* a d) (* b c)))
               (mth/close? sx sy (* 1e-3 sx))
               (mth/close? 0 (+ (* a c) (* b d)) (* 1e-3 sx sy)))
      sx)))

(defn create-text-shape
  "The Penpot text of the SVG `<text>` `element`, or of the text container
  of a Penpot export (a `g` with the box of its text and a `<text>` for
  each line in it): a paragraph for each line. A loose text is placed
  by its first baseline, as wide as its text may be: one anchored in its
  middle or end as wide as its text cannot pass, its lines centred or to
  the right. Its transform (with the ones of its groups) turns it and
  scales its font. It keeps its opacity, display, blend mode and stroke;
  nil for a text a Penpot text cannot be: transformed otherwise (see
  `even-scale`) or filtered."
  [name frame-id {:keys [x y defs] :as svg-data} {:keys [attrs] :as element}]
  (let [m          (csvg/parse-transform (:transform attrs))
        scale      (even-scale m)
        container? (contains? attrs :width)
        texts      (if container?
                     (let [fills (some #(when (and (map? %) (str/includes? (str (get-in % [:attrs :class])) "fills")) %)
                                       (:content element))]
                       (filterv #(= :text (:tag %)) (:content (or fills element))))
                     [element])
        ;; a loose text reads its own properties, with the ones of its
        ;; groups (see `csvg/inherit-attributes`)
        inherited  (if container? (own-text-props (dissoc attrs :x :y)) {})
        lines      (text-lines texts inherited)
        filtered?  (not (contains? #{nil "none"} (text-prop attrs :filter)))]
    (when (and (seq lines) (some? scale) (not filtered?))
      (let [first-props (second (first (:runs (first lines))))
            size        (font-size first-props)
            spacing     (or (text-length (:letter-spacing first-props) size) 0)
            anchor      (:anchor (first lines))
            align       (case anchor "middle" "center" "end" "right" "left")
            chars       (reduce max 1 (map #(count (str/join (map first (:runs %)))) lines))
            ;; a guess of the width, which the layout of the text corrects;
            ;; a centred one no glyph is wider than, for it not to wrap
            width       (if (and (not container?) (not= "left" align))
                          (* chars (+ size spacing))
                          (* chars size 0.62))
            height      (* (count lines) size 1.2)
            [bx by bw bh]
            (if container?
              [(d/parse-double (:x attrs) 0) (d/parse-double (:y attrs) 0)
               (d/parse-double (:width attrs) width) (d/parse-double (:height attrs) height)]
              (let [{lx :x ly :y} (first lines)]
                [(case anchor "middle" (- lx (/ width 2)) "end" (- lx width) lx)
                 (- ly (* 0.9 size))
                 width height]))
            ;; its box in its own space, where the SVG draws it
            box         (grc/make-rect (+ bx x) (+ by y) (max bw 1) (max bh 1))
            shape       (cts/setup-shape
                         (-> (calculate-rect-metadata box (gmt/transform-in (gpt/point svg-data) m))
                             (assoc :type :text :name name :frame-id frame-id
                                    :rotation (mod (mth/degrees (mth/atan2 (:b m) (:a m))) 360))))
            paragraphs  (mapv (fn [{:keys [runs]}]
                                (merge txt/default-paragraph-attrs
                                       {:type "paragraph"
                                        :text-align (if container? "left" align)
                                        :children (mapv (fn [[text props]] (text-node text props shape defs scale)) runs)}))
                              lines)]
        (-> shape
            (assoc :grow-type (cond container? :fixed (= "left" align) :auto-width :else :auto-height))
            (assoc :content {:type "root" :vertical-align "top"
                             :children [{:type "paragraph-set" :children paragraphs}]})
            (dissoc :position-data)
            ;; its look, but its fill, which its nodes hold
            (assoc :strokes [])
            (assoc :svg-attrs (-> (select-keys attrs [:opacity :display :mix-blend-mode :stroke :stroke-width
                                                      :stroke-opacity :stroke-dasharray :style])
                                  (csvg/attrs->props)
                                  (d/update-when :style select-keys [:opacity :display :mixBlendMode :stroke
                                                                     :strokeWidth :strokeOpacity :strokeDasharray])))
            (setup-stroke)
            (setup-opacity)
            (setup-other)
            (dissoc :svg-attrs)
            (d/update-when :strokes (partial mapv #(assoc % :stroke-alignment :center))))))))

(defn create-raw-svg
  [name frame-id {:keys [x y width height offset-x offset-y defs] :as svg-data} {:keys [attrs] :as data}]
  (let [props (csvg/attrs->props attrs)
        vbox  (grc/make-rect offset-x offset-y width height)]
    (cts/setup-shape
     {:type :svg-raw
      :name name
      :frame-id frame-id
      :width width
      :height height
      :x x
      :y y
      :content data
      :svg-attrs props
      :svg-viewbox vbox
      :svg-defs defs})))

(defn create-svg-root
  [id frame-id parent-id {:keys [name x y width height offset-x offset-y attrs defs] :as svg-data}]
  (let [props (-> (dissoc attrs :viewBox :view-box :xmlns)
                  (d/without-keys csvg/inheritable-props)
                  (csvg/attrs->props))]
    (cts/setup-shape
     {:id id
      :type :group
      :name name
      :frame-id frame-id
      :parent-id parent-id
      :width width
      :height height
      :x (+ x offset-x)
      :y (+ y offset-y)
      :svg-attrs props
      :svg-defs defs})))

(defn create-svg-children
  [objects selected frame-id parent-id svg-data [unames children] [_index svg-element]]
  (let [[shape new-children] (parse-svg-element frame-id svg-data svg-element unames)]
    (if (some? shape)
      (let [shape-id (:id shape)
            shape    (-> shape
                         (assoc :frame-id frame-id)
                         (assoc :parent-id parent-id)
                         ;; which element made it, for an importer that
                         ;; tags the elements (see `app.common.files.svg-animation`)
                         (cond-> (and (map? svg-element) (contains? svg-element ::element-key))
                           (vary-meta assoc ::element-key (::element-key svg-element)))
                         (cond-> (and (map? svg-element) (contains? svg-element ::mask-of))
                           (vary-meta assoc ::mask-of (::mask-of svg-element))))
            children (conj children shape)
            unames   (conj unames (:name shape))]

        (reduce (partial create-svg-children objects selected frame-id shape-id svg-data)
                [unames children]
                (d/enumerate new-children)))

      [unames children])))


(defn create-group
  [name frame-id {:keys [x y width height offset-x offset-y defs] :as svg-data} {:keys [attrs]}]
  (let [transform (csvg/parse-transform (:transform attrs))
        attrs     (-> attrs
                      (d/without-keys csvg/inheritable-props)
                      (csvg/attrs->props))
        vbox      (grc/make-rect offset-x offset-y width height)]
    (cts/setup-shape
     {:type :group
      :name name
      :frame-id frame-id
      :x (+ x offset-x)
      :y (+ y offset-y)
      :width width
      :height height
      :svg-transform transform
      :svg-attrs attrs
      :svg-viewbox vbox
      :svg-defs defs})))

(defn- stroke-only-svg-path?
  "Returns true when the SVG element renders only a stroke (fill=none).
  Stroke-only paths can have their consecutive touching subpaths safely
  merged into a continuous polyline so that `stroke-linejoin` applies at
  shared endpoints, without affecting any fill-rule semantics."
  [attrs]
  (let [attr-fill  (some-> (:fill attrs) str/trim)
        style-fill (some-> (get-in attrs [:style :fill]) str/trim)]
    (= "none" (or attr-fill style-fill))))

(defn create-path-shape [name frame-id svg-data {:keys [attrs] :as data}]
  (when (and (contains? attrs :d) (seq (:d attrs)))
    (let [transform    (csvg/parse-transform (:transform attrs))
          stroke-only? (stroke-only-svg-path? attrs)
          content      (cond-> (path/from-string (:d attrs))
                         stroke-only?
                         (path/merge-touching-subpaths)

                         (some? transform)
                         (path.segm/transform-content transform))

          selrect    (path.segm/content->selrect content)
          points     (grc/rect->points selrect)
          origin     (gpt/negate (gpt/point svg-data))
          attrs      (-> (dissoc attrs :d :transform)
                         (csvg/attrs->props))]
      (-> (cts/setup-shape
           {:type :path
            :name name
            :frame-id frame-id
            :content content
            :selrect selrect
            :points points
            :svg-viewbox selrect
            :svg-attrs attrs
            :svg-transform transform
            :strokes []
            :fills []})
          (gsh/translate-to-frame origin)))))

(defn calculate-rect-metadata
  [rect transform]
  (let [points    (-> rect
                      (grc/rect->points)
                      (gsh/transform-points transform))
        center    (gsc/points->center points)
        selrect   (gst/calculate-selrect points center)
        transform (gst/calculate-transform points center selrect)]

    {:x (:x selrect)
     :y (:y selrect)
     :width (:width selrect)
     :height (:height selrect)
     :selrect selrect
     :points points
     :transform transform
     :transform-inverse (when (some? transform)
                          (gmt/inverse transform))}))

(defn- parse-rect-attrs
  [{:keys [x y width height]}]
  (grc/make-rect
   (d/parse-double x 0)
   (d/parse-double y 0)
   (d/parse-double width 1)
   (d/parse-double height 1)))

(defn- parse-radius-attrs
  [attrs]
  (if (or (contains? attrs :rx) (contains? attrs :ry))
    (let [rx-val (d/parse-double (:rx attrs) 0)
          ry-val (d/parse-double (:ry attrs) 0)
          radius (cond
                   (and (contains? attrs :rx) (contains? attrs :ry))
                   (min rx-val ry-val)
                   (contains? attrs :rx)
                   rx-val
                   (contains? attrs :ry)
                   ry-val
                   :else 0)]
      {:r1 radius :r2 radius :r3 radius :r4 radius})
    {}))

(defn create-rect-shape [name frame-id svg-data {:keys [attrs] :as data}]
  (let [transform (->> (csvg/parse-transform (:transform attrs))
                       (gmt/transform-in (gpt/point svg-data)))

        origin    (gpt/negate (gpt/point svg-data))
        vbox      (parse-rect-attrs attrs)
        rect      (-> vbox
                      (update :x - (:x origin))
                      (update :y - (:y origin)))

        props     (-> (dissoc attrs :x :y :width :height :rx :ry :transform)
                      (csvg/attrs->props))

        radius-attrs (parse-radius-attrs attrs)]
    (cts/setup-shape
     (-> (calculate-rect-metadata rect transform)
         (assoc :type :rect)
         (assoc :name name)
         (assoc :frame-id frame-id)
         (assoc :svg-viewbox vbox)
         (assoc :svg-attrs props)
         ;; We need to ensure fills are empty on import process
         ;; because setup-shape assings one by default.
         (assoc :fills [])
         (merge radius-attrs)))))

(defn- parse-circle-attrs
  [attrs]
  (into [] (comp (map (d/getf attrs))
                 (map d/parse-double))
        [:cx :cy :r :rx :ry]))

(defn create-circle-shape
  [name frame-id svg-data {:keys [attrs] :as data}]
  (let [[cx cy r rx ry]
        (parse-circle-attrs attrs)

        transform (->> (csvg/parse-transform (:transform attrs))
                       (gmt/transform-in (gpt/point svg-data)))

        rx        (d/nilv r rx)
        ry        (d/nilv r ry)
        rx        (d/nilv rx 0)
        ry        (d/nilv ry 0)

        ;; There are some svg circles in the internet that does not
        ;; have cx and cy attrs, so we default them to 0
        cx        (d/nilv cx 0)
        cy        (d/nilv cy 0)
        origin    (gpt/negate (gpt/point svg-data))

        rect      (grc/make-rect
                   (- cx rx (:x origin))
                   (- cy ry (:y origin))
                   (* 2 rx)
                   (* 2 ry))
        props     (-> (dissoc attrs :cx :cy :r :rx :ry :transform)
                      (csvg/attrs->props))]

    (cts/setup-shape
     (-> (calculate-rect-metadata rect transform)
         (assoc :type :circle)
         (assoc :name name)
         (assoc :frame-id frame-id)
         (assoc :svg-viewbox rect)
         (assoc :svg-attrs props)
         (assoc :fills [])))))

(defn create-image-shape
  [name frame-id svg-data {:keys [attrs] :as data}]
  (let [transform  (->> (csvg/parse-transform (:transform attrs))
                        (gmt/transform-in (gpt/point svg-data)))

        image-url  (or (:href attrs) (:xlink:href attrs))
        image-data (dm/get-in svg-data [:image-data image-url])

        metadata   {:name name
                    :width (:width image-data)
                    :height (:height image-data)
                    :mtype (:mtype image-data)
                    :id (:id image-data)}

        origin     (gpt/negate (gpt/point svg-data))
        rect       (-> (parse-rect-attrs attrs)
                       (update :x - (:x origin))
                       (update :y - (:y origin)))
        props      (-> (dissoc attrs :x :y :width :height :href :xlink:href)
                       (csvg/attrs->props))]

    (when (some? image-data)
      (cts/setup-shape
       (-> (calculate-rect-metadata rect transform)
           (assoc :type :rect)
           (assoc :name name)
           (assoc :frame-id frame-id)
           (assoc :fills [{:fill-opacity 1
                           :fill-image metadata}])
           (assoc :metadata metadata)
           (assoc :svg-viewbox rect)
           (assoc :svg-attrs props))))))

(defn- fraction
  "The number of the SVG length `s` (`50%` is 0.5), `default` for none."
  [s default]
  (let [s (str/trim (str s))]
    (some-> (if (str/ends-with? s "%")
              (some-> (d/parse-double (subs s 0 (dec (count s)))) (/ 100))
              (d/parse-double s default))
            (double))))

(defn- gradient-fill
  "The Penpot gradient of the SVG `<linearGradient>` or `<radialGradient>`
  `node` on `shape`, its points relative to the box of the shape; nil for
  one with a transform. `offset` takes the user space of the SVG to the
  one of the shapes."
  [node shape offset]
  (let [attrs (:attrs node)
        user? (= "userSpaceOnUse" (:gradientUnits attrs))
        {:keys [x y width height]} (:selrect shape)
        rel-x #(if user? (/ (- (+ % (:x offset 0)) x) (max width 1e-6)) %)
        rel-y #(if user? (/ (- (+ % (:y offset 0)) y) (max height 1e-6)) %)
        stops (into []
                    (keep (fn [{:keys [tag attrs]}]
                            (let [color (str/trim (str (:stop-color attrs "#000000")))]
                              (when (and (= :stop tag) (clr/color-string? color))
                                {:offset  (mth/clamp (fraction (:offset attrs) 0) 0 1)
                                 :color   (clr/parse color)
                                 :opacity (mth/clamp (fraction (:stop-opacity attrs) 1) 0 1)}))))
                    (:content node))
        moved? (let [t (str/trim (str (:gradientTransform attrs)))]
                 (and (not (str/blank? t)) (not (gmt/base? (csvg/parse-transform t)))))]
    (when (and (seq stops) (not moved?))
      (case (:tag node)
        :linearGradient
        {:type :linear
         :start-x (rel-x (fraction (:x1 attrs) 0)) :start-y (rel-y (fraction (:y1 attrs) 0))
         :end-x (rel-x (fraction (:x2 attrs) 1)) :end-y (rel-y (fraction (:y2 attrs) 0))
         :width 1 :stops stops}

        ;; in user space, round: Penpot draws it in the box of the shape
        :radialGradient
        (let [cx (rel-x (fraction (:cx attrs) 0.5))
              cy (rel-y (fraction (:cy attrs) 0.5))
              r  (fraction (:r attrs) 0.5)]
          {:type :radial
           :start-x cx :start-y cy :end-x cx :end-y (+ cy (if user? (/ r (max height 1e-6)) r))
           :width (if user? (/ height (max width 1e-6)) 1) :stops stops})

        nil))))

(defn- style-value
  "The value of the CSS property `css-name` (`prop` once parsed) in the
  style `style` of an element, parsed or as text."
  [style css-name prop]
  (if (map? style)
    (get style prop)
    (some->> (str style)
             (re-find (re-pattern (str "(?:^|;)\\s*" css-name "\\s*:\\s*([^;]+)")))
             (second)
             (str/trim))))

(defn- pattern-paint
  "The fill and its opacity of the SVG `<pattern>` `node` that is one
  rect covering it all, as the SVG exports of Penpot write a fill; nil
  for other patterns."
  [node]
  (let [drawn (loop [nodes (filterv map? (:content node))]
                (if (and (= 1 (count nodes)) (= :g (:tag (first nodes))))
                  (recur (filterv map? (:content (first nodes))))
                  nodes))
        {:keys [tag attrs]} (first drawn)
        size  #(d/parse-double (get %1 %2) 0)]
    (when (and (= 1 (count drawn)) (= :rect tag)
               (zero? (size attrs :x)) (zero? (size attrs :y))
               (>= (size attrs :width) (* 0.99 (size (:attrs node) :width)))
               (>= (size attrs :height) (* 0.99 (size (:attrs node) :height))))
      [(or (style-value (:style attrs) "fill" :fill) (:fill attrs))
       (d/parse-double (or (style-value (:style attrs) "fill-opacity" :fillOpacity) (:fill-opacity attrs)) 1)])))

(defn- paint-fill
  "The fill of the SVG paint `paint` on `shape`, through a pattern or a
  gradient of `defs` it refers to (see `pattern-paint`)."
  [paint shape defs offset depth]
  (let [paint (str/trim (str paint))
        id    (url-id paint)
        node  (when (some? id) (get defs id))]
    (cond
      (clr/color-string? paint)
      {:fill-color (clr/parse paint)}

      (and (= :pattern (:tag node)) (< depth 3))
      (when-let [[inner opacity] (pattern-paint node)]
        (some-> (paint-fill inner shape defs offset (inc depth))
                (update :fill-opacity #(* (or % 1) opacity))))

      (contains? #{:linearGradient :radialGradient} (:tag node))
      (some->> (gradient-fill node shape offset) (hash-map :fill-color-gradient))

      :else nil)))

(declare setup-solid-fill)

(defn setup-fill
  ([shape]
   (setup-fill shape (:svg-defs shape) nil))
  ([shape defs offset]
   (let [paint (or (dm/get-in shape [:svg-attrs :style :fill]) (dm/get-in shape [:svg-attrs :fill]))
         fill  (when (some? (url-id paint)) (paint-fill paint shape defs offset 0))]
     (if (some? fill)
       ;; a gradient, or the colour of a pattern of a Penpot export
       (-> shape
           (update :svg-attrs dissoc :fill :fillOpacity)
           (update-in [:svg-attrs :style] dissoc :fill :fillOpacity)
           (assoc :fills [(update fill :fill-opacity
                                  #(* (or % 1)
                                      (d/parse-double (or (dm/get-in shape [:svg-attrs :style :fillOpacity])
                                                          (dm/get-in shape [:svg-attrs :fillOpacity]))
                                                      1)))]))
       (setup-solid-fill shape)))))

(defn- setup-solid-fill
  [shape]
  (let [color-attr  (str/trim (dm/get-in shape [:svg-attrs :fill]))
        color-attr  (if (= color-attr "currentColor") clr/black color-attr)
        color-style (str/trim (dm/get-in shape [:svg-attrs :style :fill]))
        color-style (if (= color-style "currentColor") clr/black color-style)]
    (cond-> shape
      ;; Color present as attribute
      (clr/color-string? color-attr)
      (-> (update :svg-attrs dissoc :fill)
          (update-in [:svg-attrs :style] dissoc :fill)
          (assoc-in [:fills 0 :fill-color] (clr/parse color-attr)))

      ;; Color present as style
      (clr/color-string? color-style)
      (-> (update-in [:svg-attrs :style] dissoc :fill)
          (update :svg-attrs dissoc :fill)
          (assoc-in [:fills 0 :fill-color] (clr/parse color-style)))

      ;; Only create an opacity if the color is set. Otherwise can create problems down the line
      (and (or (clr/color-string? color-attr) (clr/color-string? color-style))
           (dm/get-in shape [:svg-attrs :fillOpacity]))
      (-> (update :svg-attrs dissoc :fillOpacity)
          (update-in [:svg-attrs :style] dissoc :fillOpacity)
          (assoc-in [:fills 0 :fill-opacity] (-> (dm/get-in shape [:svg-attrs :fillOpacity])
                                                 (d/parse-double 1))))

      (and (or (clr/color-string? color-attr) (clr/color-string? color-style))
           (dm/get-in shape [:svg-attrs :style :fillOpacity]))
      (-> (update-in [:svg-attrs :style] dissoc :fillOpacity)
          (update :svg-attrs dissoc :fillOpacity)
          (assoc-in [:fills 0 :fill-opacity] (-> (dm/get-in shape [:svg-attrs :style :fillOpacity])
                                                 (d/parse-double 1)))))))

(defn- dash-period
  "The dash lengths `ns` (an even count) as their shortest period, which
  repeats: `10 5 10 5` is `10 5`."
  [ns]
  (let [n (count ns)]
    (or (some (fn [p]
                (when (and (zero? (mod n p)) (= ns (into [] (take n) (cycle (take p ns)))))
                  (subvec ns 0 p)))
              (range 2 n 2))
        ns)))

(defn- setup-stroke
  [shape]
  (let [attrs   (get shape :svg-attrs)
        style   (get attrs :style)

        stroke  (or (str/trim (:stroke attrs))
                    (str/trim (:stroke style)))

        color   (cond
                  (= stroke "currentColor")  clr/black
                  (= stroke "none")          nil
                  (clr/color-string? stroke) (clr/parse stroke))

        opacity (when (some? color)
                  (d/parse-double
                   (or (:strokeOpacity attrs)
                       (:strokeOpacity style))
                   1))

        width   (when (some? color)
                  (d/parse-double
                   (or (:strokeWidth attrs)
                       (:strokeWidth style))
                   1))

        linecap (or (get attrs :strokeLinecap)
                    (get style :strokeLinecap))
        linecap (some-> linecap str/trim keyword)

        ;; the dashes, as the stroke styles of Penpot draw them: a dash
        ;; and a gap, or dots (no dash); a list of an odd count repeats,
        ;; as in SVG. Other patterns, and dashes that start elsewhere
        ;; (an offset), stay SVG attributes, which draw them exactly.
        dashes  (when (some? color)
                  (let [v (str/trim (str (or (:strokeDasharray style) (:strokeDasharray attrs))))]
                    (when-not (or (str/blank? v) (= "none" v))
                      (let [ns (csvg/parse-numbers v)]
                        (when (and (seq ns) (some pos? ns))
                          (dash-period (vec (if (odd? (count ns)) (concat ns ns) ns))))))))
        offset  (d/parse-double (or (:strokeDashoffset style) (:strokeDashoffset attrs)) 0)
        dashed  (cond
                  (or (nil? dashes) (not (zero? offset)) (> (count dashes) 2)) nil
                  (zero? (first dashes))                                       {:stroke-style :dotted}
                  :else                                                        {:stroke-style :dashed
                                                                                :stroke-dash (first dashes)
                                                                                :stroke-gap (second dashes)})

        attrs
        (-> attrs
            (cond-> (some? color)
              (dissoc :stroke :strokeWidth :strokeOpacity))
            (cond-> (some? dashed)
              (dissoc :strokeDasharray :strokeDashoffset))
            (update
             :style
             (fn [style]
               (-> style
                   (cond-> (some? color)
                     (dissoc :stroke :strokeWidth :strokeOpacity))
                   (cond-> (some? dashed)
                     (dissoc :strokeDasharray :strokeDashoffset)))))
            (d/without-nils))]

    (cond-> (assoc shape :svg-attrs attrs)
      (some? color)
      (assoc-in [:strokes 0 :stroke-color] color)

      (and (some? color) (some? opacity))
      (assoc-in [:strokes 0 :stroke-opacity] opacity)

      (and (some? color) (some? width))
      (assoc-in [:strokes 0 :stroke-width] width)

      (some? dashed)
      (update-in [:strokes 0] merge dashed)

      (and (some? color) (some? linecap) (cfh/path-shape? shape)
           (or (= linecap :round) (= linecap :square)))
      (assoc-in [:strokes 0 :stroke-cap-start] linecap)

      (and (some? color) (some? linecap) (cfh/path-shape? shape)
           (or (= linecap :round) (= linecap :square)))
      (assoc-in [:strokes 0 :stroke-cap-end] linecap))))

(defn setup-opacity [shape]
  (cond-> shape
    (dm/get-in shape [:svg-attrs :opacity])
    (-> (update :svg-attrs dissoc :opacity)
        (assoc :opacity (-> (dm/get-in shape [:svg-attrs :opacity])
                            (d/parse-double 1))))

    (dm/get-in shape [:svg-attrs :style :opacity])
    (-> (update-in [:svg-attrs :style] dissoc :opacity)
        (assoc :opacity (-> (dm/get-in shape [:svg-attrs :style :opacity])
                            (d/parse-double 1))))

    (dm/get-in shape [:svg-attrs :mixBlendMode])
    (-> (update :svg-attrs dissoc :mixBlendMode)
        (assoc :blend-mode (-> (dm/get-in shape [:svg-attrs :mixBlendMode]) assert-valid-blend-mode)))

    (dm/get-in shape [:svg-attrs :style :mixBlendMode])
    (-> (update-in [:svg-attrs :style] dissoc :mixBlendMode)
        (assoc :blend-mode (-> (dm/get-in shape [:svg-attrs :style :mixBlendMode]) assert-valid-blend-mode)))))

(defn setup-other [shape]
  (cond-> shape
    ;; hidden, with no `display` left that would hide it once shown
    (or (= (dm/get-in shape [:svg-attrs :display]) "none")
        (= (dm/get-in shape [:svg-attrs :style :display]) "none"))
    (-> (update :svg-attrs dissoc :display)
        (d/update-in-when [:svg-attrs :style] dissoc :display)
        (assoc :hidden true))))

(defn tag->name
  "Given a tag returns its layer name"
  [tag]
  (let [suffix (cond
                 (string? tag) tag
                 (keyword? tag) (d/name tag)
                 (nil? tag) "node"
                 :else (dm/str tag))]
    (dm/str "svg-" suffix)))

(defn- filter-valid-def-references
  "Filters out false positive references that are not valid def IDs.
   Filters out:
   - Colors in style attributes (hex colors like #f9dd67)
   - Style fragments that contain CSS keywords (like stop-opacity)
   - References that don't exist in defs"
  [ref-ids defs]
  (let [is-style-fragment? (fn [ref-id]
                             (or (clr/hex-color-string? (str "#" ref-id))
                                 (str/includes? ref-id ";")  ;; Contains CSS separator
                                 (str/includes? ref-id "stop-opacity")  ;; CSS keyword
                                 (str/includes? ref-id "stop-color")))]  ;; CSS keyword
    (->> ref-ids
         (remove is-style-fragment?)  ;; Filter style fragments and hex colors
         (filter #(contains? defs %)))))  ;; Only existing defs

(defn resolve-element-name
  "Pick the most user-meaningful name for an SVG element.

  Inkscape (and editors following the same convention) write the
  operator-given label to ``inkscape:label``/``sodipodi:label`` while
  ``id`` holds an auto-generated technical id like ``path1234``.
  Preferring the namespaced label keeps the layer/group/element names
  the operator sees in their source editor across a paste/import
  (#7869); the existing ``id`` and ``(tag->name tag)`` fallbacks keep
  legacy SVGs that don't carry a label working unchanged."
  [tag attrs]
  (or (:inkscape:label attrs)
      (:sodipodi:label attrs)
      (:id attrs)
      (tag->name tag)))

(defn parse-svg-element
  [frame-id svg-data {:keys [tag attrs hidden] :as element} unames]

  ;; FIXME: there are cases where element is directly a string, I
  ;; think we should handle this case early and avoid some code
  ;; execution

  (let [name         (or (::name element) (resolve-element-name tag attrs))
        att-refs     (csvg/find-attr-references attrs)
        defs         (get svg-data :defs)
        valid-refs   (filter-valid-def-references att-refs defs)
        all-refs     (csvg/find-def-references defs valid-refs)
        ;; Filter the final result to ensure all references are valid defs
        ;; This prevents false positives from style attributes in gradient stops
        references   (filter-valid-def-references all-refs defs)

        href-id      (or (:href attrs) (:xlink:href attrs) " ")
        href-id      (if (and (string? href-id)
                              (pos? (count href-id)))
                       (subs href-id 1)
                       href-id)

        use-tag?     (and (= :use tag)
                          (some? href-id)
                          (contains? defs href-id))]

    (if use-tag?
      (let [;; Merge the data of the use definition with the properties passed as attributes
            use-data     (-> (get defs href-id)
                             (update :attrs #(d/deep-merge % (dissoc attrs :xlink:href :href))))
            displacement (gpt/point (d/parse-double (:x attrs "0")) (d/parse-double (:y attrs "0")))
            disp-matrix  (dm/str (gmt/translate-matrix displacement))
            element      (-> element
                             (assoc :tag :g)
                             (update :attrs dissoc :x :y :width :height :href :xlink:href :transform)
                             (update :attrs csvg/add-transform disp-matrix)
                             (assoc :content [use-data]))]
        (parse-svg-element frame-id svg-data element unames))

      (let [;; SVG graphic elements
            ;; :circle :ellipse :image :line :path :polygon :polyline :rect :text :use
            text-box? (and (= :g tag) (str/includes? (str (:class attrs)) "text-container")
                           (contains? attrs :width))
            shape   (case (if text-box? :text tag)
                      :text        (or (create-text-shape name frame-id svg-data element)
                                       (create-raw-svg name frame-id svg-data element))
                      (:g :a :svg) (create-group name frame-id svg-data element)
                      :rect        (create-rect-shape name frame-id svg-data element)
                      (:circle
                       :ellipse)   (create-circle-shape name frame-id svg-data element)
                      :path        (create-path-shape name frame-id svg-data element)
                      :polyline    (create-path-shape name frame-id svg-data (-> element csvg/polyline->path))
                      :polygon     (create-path-shape name frame-id svg-data (-> element csvg/polygon->path))
                      :line        (create-path-shape name frame-id svg-data (-> element csvg/line->path))
                      :image       (create-image-shape name frame-id svg-data element)
                      #_other      (create-raw-svg name frame-id svg-data element))]

        (when (some? shape)
          [(-> shape
               (assoc :svg-defs (select-keys defs references))
               (setup-fill defs svg-data)
               (setup-stroke)
               (setup-opacity)
               (setup-other)
               (update :svg-attrs (fn [attrs]
                                    (if (empty? (:style attrs))
                                      (dissoc attrs :style)
                                      attrs)))
               (cond-> ^boolean hidden
                 (assoc :hidden true))
               (cond-> (some? (::mask-mode element))
                 (assoc :masked-group true))
               (cond-> (contains? #{:alpha-inverted :luma :luma-inverted} (::mask-mode element))
                 (assoc :mask-mode (::mask-mode element))))

           ;; a text holds its lines itself
           (when-not (= :text (:type shape))
             (cond->> (:content element)
               (contains? csvg/parent-tags tag)
               (mapv (partial csvg/inherit-attributes attrs))))])))))
