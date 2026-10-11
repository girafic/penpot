;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.svg-vectors
  "The layers of an animated SVG export as vectors, as Figma writes an
  SVG: each layer as Penpot's SVG renderer draws it at rest, without the
  layers in it, which are layers of their own (see `asvg/timeline->svg`),
  its images and the fonts of its texts in the file as data URIs, or its
  texts as the outlines of their glyphs (see `outlined`)."
  (:require
   ["react-dom/server" :as rds]
   [app.common.animation.svg :as asvg]
   [app.common.data :as d]
   [app.common.files.helpers :as cfh]
   [app.common.types.path :as path]
   [app.common.types.shape.radius :as ctsr]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [app.config :as cfg]
   [app.main.fonts :as fonts]
   [app.main.render :as render]
   [app.main.ui.shapes.attrs :as attrs]
   [app.main.ui.shapes.embed :as embed]
   [app.main.ui.shapes.export :as export]
   [app.main.ui.shapes.text.fontfaces :as ff]
   [app.util.http :as http]
   [beicon.v2.core :as rx]
   [clojure.set :as set]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn without-layers
  "`objects` with the layers `layer-ids` out of the shapes they sit in, so
  a layer, or the board, draws without the layers in it, which draw on
  their own."
  [objects layer-ids]
  (d/update-vals objects
                 (fn [shape]
                   (cond-> shape
                     (seq (:shapes shape))
                     (update :shapes #(into [] (remove layer-ids) %))))))

(def ^:private trim-properties
  #{:trim-start :trim-end :trim-offset})

(defn- rest-shape
  "`shape` as its layer draws it: without its opacity, which the group of
  the layer has, and without what a filter of its own draws over it,
  which would else show twice: its blur, as strong as on the canvas, and
  the shadows its animation, `track`, plays (see `asvg/svg-filter-blur?`,
  `asvg/svg-filter-shadows?`). Its outline a path when SMIL plays its
  corners (see `asvg/svg-smil-corners?`), which are not alike as it plays.
  A board that hides its fill in exports, without it."
  [shape track]
  (let [shadows? (asvg/svg-filter-shadows? shape)]
    (cond-> (dissoc shape :opacity)
      (or shadows? (asvg/svg-filter-blur? shape track))
      (dissoc :blur)

      shadows?
      (assoc :shadow [])

      ;; corners alike draw as a `rect`
      (and (asvg/svg-smil-corners? shape track) (ctsr/all-equal? shape))
      (as-> shape (assoc shape
                         :r1 (or (:r1 shape) 0) :r2 (or (:r2 shape) 0) :r3 (or (:r3 shape) 0)
                         :r4 (+ (or (:r4 shape) 0) 0.0001)))

      (:hide-fill-on-export shape)
      (assoc :fills []))))

(defn- mark-outline
  "`markup` with the elements of the outline of `shape` (see
  `rest-shape`) marked, which SMIL plays the corners on (see
  `asvg/svg-smil-corners?`)."
  [markup shape]
  (if-let [d (.-d (attrs/get-border-props shape))]
    (let [attr (str " d=\"" d "\"")]
      (.join (.split markup attr) (str attr " data-penpot-outline=\"\"")))
    markup))

(defn with-path-length
  "The `markup` of the shape `id` with the outlines its strokes are drawn
  along of length 1 (`pathLength`), so its trim plays as their dash (see
  `asvg/svg-appearance-css`): those after the start of its strokes, the
  group of class `strokes` whose id ends in it, whatever the order of
  its attributes."
  [markup id]
  (let [start (.search markup (js/RegExp. (str "<g(?=[^>]*\\sclass=\"(?:[^\"]*\\s)?strokes(?:\\s[^\"]*)?\")"
                                               "(?=[^>]*\\sid=\"[^\"]*-" id "\")")))]
    (if (neg? start)
      markup
      (str (subs markup 0 start)
           (str/replace (subs markup start)
                        #"<(path|rect|ellipse|circle|polygon|polyline|line) "
                        "<$1 pathLength=\"1\" ")))))

(defn- merged-attrs
  "The attributes of a copy of an element of the attributes
  `element-attrs` a `<use>` of the attributes `use-attrs` draws: both,
  the style of the element over the one of the `<use>`, which passes on
  only what the element has not."
  [element-attrs use-attrs]
  (let [style-re #"(?:^| )style=\"([^\"]*)\""
        style    (->> [use-attrs element-attrs]
                      (keep #(second (re-find style-re %)))
                      (remove str/blank?)
                      (str/join ";"))]
    (->> [(str/replace element-attrs style-re "")
          (str/replace use-attrs style-re "")
          (when-not (str/blank? style) (str "style=\"" style "\""))]
         (map str/trim)
         (remove str/blank?)
         (str/join " "))))

(defn inline-uses
  "`markup` with each `<use>` of an element in it a copy of that element,
  which goes: the elements an SVG draws are those its CSS animates (see
  `asvg/svg-appearance-css`), as Chrome and Firefox do not play the
  animations of an element on the copies `<use>`s draw of it. The SVG
  renderer draws inner and outer strokes so."
  [markup]
  (let [elements (into {}
                       (map (fn [[whole tag before id after]]
                              [id {:whole whole :tag tag :attrs (str before " " after)}]))
                       (re-seq #"<(\w+) ((?:[^>]*? )?)id=\"([^\"]+)\"([^>]*)></\1>" markup))
        used     (volatile! #{})
        copied   (.replace markup
                           (js/RegExp. "<use href=\"#([^\"]+)\"([^>]*)></use>" "g")
                           (fn [whole id attrs]
                             (if-let [{:keys [tag] element-attrs :attrs} (get elements id)]
                               (do (vswap! used conj id)
                                   (str "<" tag " " (merged-attrs element-attrs attrs) "></" tag ">"))
                               whole)))]
    (reduce (fn [markup id] (.join (.split markup (:whole (get elements id))) ""))
            copied
            @used)))

(mf/defc ^:private layer-svg*
  [{:keys [objects shape]}]
  (let [wrapper (mf/with-memo [objects]
                  (render/shape-wrapper-factory objects))]
    [:& (mf/provider export/include-metadata-ctx) {:value false}
     [:& (mf/provider embed/context) {:value true}
      ;; in an `svg`, so its elements are SVG ones; only what is in it
      ;; is kept (see `layer-markup`)
      [:svg
       [:& wrapper {:shape shape}]]]]))

(def ^:private path-arity
  "How many numbers each command of path data takes."
  {"M" 2 "L" 2 "C" 6 "Z" 0})

(defn- short-number
  "`v`, a multiple of 0.01, as briefly as SVG takes it."
  [v]
  (let [text (str (/ (js/Math.round (* v 100)) 100))]
    (cond
      (str/starts-with? text "0.")  (subs text 1)
      (str/starts-with? text "-0.") (str "-" (subs text 2))
      :else text)))

(defn compact-path
  "The path data `d` (absolute `M` `L` `C` `Z`, as the SVG renderer
  writes them) to 0.01 px, relative: a fraction of its length, the
  outlines of glyphs most (see `outlined`). The points are rounded
  before they are made relative, so no error adds up. `d` as it is when
  it has other commands."
  [d]
  (let [tokens (re-seq #"[A-Za-z]|-?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?" d)
        round  #(/ (js/Math.round (* % 100)) 100)]
    (loop [tokens tokens
           cmd    nil
           [cx cy] [0 0]
           [sx sy] [0 0]
           out    []]
      (if-let [token (first tokens)]
        (cond
          (contains? path-arity token)
          (if (= "Z" token)
            (recur (rest tokens) "Z" [sx sy] [sx sy] (conj out "z"))
            (recur (rest tokens) token [cx cy] [sx sy] out))

          (re-matches #"[A-Za-z]" token)
          d

          :else
          (let [cmd     (if (= "Z" cmd) nil cmd)
                n       (get path-arity cmd)
                numbers (take n tokens)]
            (if (or (nil? n) (< (count numbers) n))
              d
              (let [points (partition 2 (map #(round (js/parseFloat %)) numbers))
                    [x y]  (last points)
                    deltas (mapcat (fn [[px py]] [(round (- px cx)) (round (- py cy))]) points)]
                (recur (drop n tokens)
                       ;; numbers after an `M` are lines
                       (if (= "M" cmd) "L" cmd)
                       [x y]
                       (if (= "M" cmd) [x y] [sx sy])
                       (conj out (str (str/lower cmd) (str/join "," (map short-number deltas)))))))))
        (str/join "" out)))))

(defn- compact-paths
  "`markup` with its path data compact (see `compact-path`)."
  [markup]
  (.replace markup (js/RegExp. " d=\"([^\"]*)\"" "g") (fn [_ d] (str " d=\"" (compact-path d) "\""))))

(defn layer-markup
  "The SVG of the shape `id` of `objects` (see `without-layers`) at rest,
  as Penpot's SVG renderer draws it, its images embedded when their data
  URIs are cached (see `load-media`), without metadata, its `<use>`s
  copies (see `inline-uses`), its path data compact (see
  `compact-path`). `track` is its animation (see `rest-shape`)."
  [objects id track]
  (let [shape  (rest-shape (get objects id) track)
        markup (rds/renderToStaticMarkup
                (mf/element layer-svg* #js {:objects objects :shape shape}))]
    (cond-> (-> markup
                (str/replace #"^<svg[^>]*>" "")
                (str/replace #"</svg>$" "")
                (inline-uses)
                (compact-paths))
      (some #(trim-properties (:property %)) (:keyframes track))
      (with-path-length id)

      (asvg/svg-smil-corners? (get objects id) track)
      (mark-outline shape))))

(def ^:private media-fetches
  "How many images `load-media` fetches at once."
  4)

(defn- image-urls
  "The URLs of the images the shapes in the board `board-id` of `objects`
  draw, but the hidden ones."
  [objects board-id]
  (->> (cfh/get-children-with-self objects board-id)
       (remove (some-fn nil? :hidden))
       (mapcat (fn [shape]
                 (cond-> (into [] (keep :fill-image) (:fills shape))
                   (= :image (:type shape)) (conj (:metadata shape))
                   (some? (:fill-image shape)) (conj (:fill-image shape)))))
       (map cfg/resolve-file-media)
       (distinct)))

(defn load-media
  "A promise resolved once the images the shapes in the board `board-id`
  of `objects` draw are cached as data URIs, which the SVG renderer then
  embeds (see `layer-markup`), a few at a time; one that fails stays a
  link."
  [objects board-id]
  (js/Promise.
   (fn [resolve _]
     (->> (rx/from (image-urls objects board-id))
          (rx/map (fn [url] (rx/catch (fn [_] (rx/empty)) (http/fetch-data-uri url))))
          (rx/merge-all media-fetches)
          (rx/reduce (constantly nil) nil)
          (rx/subs! resolve (fn [_] (resolve nil)))))))

(defn- embed-urls
  "`css` with each `url(…)` it loads replaced by the data URI of what it
  loads, a promise; one that fails stays a link."
  [css]
  (let [urls (distinct (fonts/extract-fontface-urls css))]
    (js/Promise.
     (fn [resolve _]
       (->> (rx/from urls)
            (rx/merge-map (fn [url]
                            (let [href (str/trim url "\"' ")]
                              (->> (http/fetch-data-uri href)
                                   (rx/map (fn [data] [url (get data href)]))
                                   (rx/catch (fn [_] (rx/empty)))))))
            (rx/reduce conj [])
            (rx/subs! (fn [pairs]
                        (resolve (reduce (fn [css [url data]]
                                           (cond-> css
                                             (some? data)
                                             (str/replace (str "url(" url ")") (str "url(\"" data "\")"))))
                                         css
                                         pairs)))
                      (fn [_] (resolve css))))))))

(defn fonts-css
  "A promise of the `@font-face` CSS of the fonts the texts among the
  shapes `ids` of `objects` (and in them) use, their files in it as data
  URIs, or nil without texts."
  [objects ids]
  (let [refs (reduce (fn [refs id]
                       (set/union refs (ff/shape->fonts (get objects id) objects)))
                     #{}
                     ids)]
    (if (empty? refs)
      (js/Promise.resolve nil)
      (js/Promise.
       (fn [resolve _]
         (->> (fonts/render-font-styles refs)
              (rx/subs! (fn [css] (-> (embed-urls css) (.then resolve)))
                        (fn [_] (resolve nil)))))))))

(defn text-ids
  "The texts among the shapes `ids` of `objects` and in them."
  [objects ids]
  (into #{}
        (comp (mapcat #(cfh/get-children-ids-with-self objects %))
              (filter #(= :text (get-in objects [% :type]))))
        ids))

(defn text-fills
  "The fills the text `shape` paints all its text with, which its
  outline is painted with (see `outlined`); nil when it paints its text
  in more than one way, which one outline cannot."
  [shape]
  (let [paints (into #{}
                     (comp (remove #(str/blank? (:text %)))
                           (map #(or (:fills %) (txt/get-default-text-fills))))
                     (txt/node-seq txt/is-text-node? (:content shape)))]
    (case (count paints)
      0 (txt/get-default-text-fills)
      1 (vec (first paints))
      nil)))

(defn outlined
  "The text `shape` as a path, `content` the outlines of its glyphs on
  the page (see `wasm.api/shape-to-path`), painted with `fills` (see
  `text-fills`), as Penpot converts a text to a path: what an SVG of
  outlined text draws of it."
  [shape content fills]
  (-> shape
      (assoc :type :path :fills fills)
      (dissoc :position-data :grow-type)
      (d/without-keys [:x :y :width :height :rx :ry :r1 :r2 :r3 :r4 :metadata])
      (path/update-geometry content)))

(defn outlined-view
  "The text `shape` outlined (see `outlined`) as the CSS of an SVG takes
  it (see `asvg/timeline->svg`): a path in the paint of its text, but on
  the box of the text, so its animation turns and scales it around the
  same pivot."
  [shape content fills]
  (-> shape
      (assoc :type :path :fills fills :content content)
      (dissoc :position-data :grow-type)))

(defn span-fills
  "The fills the span `span` of the paragraph `paragraph` of the text
  `shape` paints with, as render-wasm counts them: the paragraphs that
  have spans (see `wasm.api/fonts-from-text-content`)."
  [shape paragraph span]
  (let [paragraphs (filterv (comp seq :children)
                            (get-in shape [:content :children 0 :children]))]
    (or (get-in paragraphs [paragraph :children span :fills])
        (txt/get-default-text-fills))))

(defn unit-markup
  "The SVG of a piece of the split text `shape`, its `runs` (see
  `wasm.api/text-unit-outlines`): each the outlines of the glyphs a span
  paints, a path of the text in the fills of the span (see `outlined`),
  drawn at rest as `layer-markup` draws a layer, `track` the animation
  of the text (see `rest-shape`)."
  [shape runs track]
  (let [id (:id shape)]
    (->> runs
         (map (fn [{:keys [paragraph span content]}]
                (layer-markup {id (outlined shape content (span-fills shape paragraph span))} id track)))
         (str/join ""))))

(defn outlined-group
  "The text `shape` that paints its text in more than one way as a group
  of paths, one by run of `runs` (see `wasm.api/text-unit-outlines`), each
  the outlines of the glyphs a span paints in the fills of that span (see
  `outlined`): `{id shape}` of the group, which keeps the opacity, the
  shadows and the blur of the text, and its paths, which keep its strokes."
  [shape runs]
  (let [id       (:id shape)
        children (mapv (fn [{:keys [paragraph span content]}]
                         (-> (outlined shape content (span-fills shape paragraph span))
                             (assoc :id (uuid/next) :parent-id id)
                             (dissoc :opacity :shadow :blur :blend-mode)))
                       runs)]
    (into {id (-> shape
                  (assoc :type :group :shapes (mapv :id children))
                  (dissoc :content :position-data :grow-type :fills :strokes))}
          (map (juxt :id identity))
          children)))

