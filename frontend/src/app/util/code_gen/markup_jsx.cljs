;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.util.code-gen.markup-jsx
  "The HTML of a board in the code of the Inspect as JSX, for the React
  code of its animation (see `app.util.code-gen.motion/codes-react`): the
  same elements and classes, so the CSS of the Inspect lays it out, with
  the animated layers, the units of the texts the code plays by units and
  the strokes it trims as `motion` elements."
  (:require
   [app.common.data :as d]
   [app.main.data.exports.svg-vectors :as svgv]
   [app.util.code-gen.common :as cgc]
   [app.util.code-gen.markup-html :as html]
   [cuerdas.core :as str]))

(def ^:private token-re
  "The pieces of the markup: comments, tags and the text between them."
  #"<!--[\s\S]*?-->|</?[A-Za-z][^>]*>|[^<]+")

(def ^:private void-tags
  #{"img" "br" "hr" "input" "meta" "link" "source" "wbr"})

(def ^:private entities
  {"amp" "&" "lt" "<" "gt" ">" "quot" "\"" "apos" "'" "nbsp" "\u00a0"})

(defn- decode
  "`s` with the character references of the markup as their characters."
  [s]
  (str/replace s #"&(#x[0-9a-fA-F]+|#[0-9]+|[a-zA-Z]+);"
               (fn [[whole ref]]
                 (cond
                   (str/starts-with? ref "#x") (js/String.fromCodePoint (js/parseInt (subs ref 2) 16))
                   (str/starts-with? ref "#")  (js/String.fromCodePoint (js/parseInt (subs ref 1) 10))
                   :else                       (get entities ref whole)))))

(defn- parse-tag
  "The tag `token` of the markup: `{:closing :tag :self-closing :attrs}`,
  `:closing` true for a closing tag, `:self-closing` for one that holds
  nothing (written so, or a void element of HTML), `:attrs` its
  attributes in order, `[name value]` each (nil for one without a
  value). Nil for no tag."
  [token]
  (when-let [[_ closing tag attrs self] (re-find #"^<(/)?([A-Za-z][\w:.-]*)([\s\S]*?)(/)?>$" token)]
    {:closing (some? closing)
     :tag tag
     :self-closing (or (some? self) (contains? void-tags (str/lower tag)))
     :attrs (into []
                  (map (fn [[_ k v]] [k (some-> v decode)]))
                  (re-seq #"([^\s=/]+)(?:=\"([^\"]*)\")?" attrs))}))

(defn- fold
  "`stack` (of open elements) with its top `n` closed, each into the one
  under it."
  [stack n]
  (reduce (fn [stack _]
            (let [node  (peek stack)
                  stack (pop stack)]
              (update-in stack [(dec (count stack)) :children] conj node)))
          stack
          (range n)))

(defn parse
  "The tree of `markup` (the HTML of the code of the Inspect, SVG in it):
  `{:tag :attrs :children}` elements, `{:text}` texts, `{:comment}`
  comments."
  [markup]
  (loop [tokens (re-seq token-re markup)
         stack  [{:tag ::root :children []}]]
    (if-let [token (first tokens)]
      (let [append #(update-in %1 [(dec (count %1)) :children] conj %2)]
        (cond
          (str/starts-with? token "<!--")
          (recur (rest tokens) (append stack {:comment (subs token 4 (- (count token) 3))}))

          (str/starts-with? token "<")
          (let [{:keys [closing tag self-closing attrs]} (parse-tag token)]
            (cond
              (nil? tag)
              (recur (rest tokens) stack)

              ;; up to the element it closes; one never opened (as an
              ;; `</img>`) closes nothing
              closing
              (if-let [at (d/index-of-pred (vec (reverse (rest stack))) #(= tag (:tag %)))]
                (recur (rest tokens) (fold stack (inc at)))
                (recur (rest tokens) stack))

              self-closing
              (recur (rest tokens) (append stack {:tag tag :attrs attrs :children []}))

              :else
              (recur (rest tokens) (conj stack {:tag tag :attrs attrs :children []}))))

          :else
          (recur (rest tokens) (append stack {:text (decode token)}))))
      (:children (peek (fold stack (dec (count stack))))))))

(def ^:private attr-names
  "The JSX names of the attributes that are not their names in HTML or SVG
  in camelCase."
  {"class" "className"
   "for" "htmlFor"
   "tabindex" "tabIndex"
   "xlink:href" "xlinkHref"
   "xlink:title" "xlinkTitle"
   "xmlns:xlink" "xmlnsXlink"
   "xml:space" "xmlSpace"
   "xml:lang" "xmlLang"})

(defn- jsx-name
  "The JSX name of the attribute `attr` of HTML or SVG: its name of React
  (see `attr-names`), a data or ARIA attribute as it is, one with a dash
  or a colon in camelCase."
  [attr]
  (cond
    (contains? attr-names attr)                                          (get attr-names attr)
    (or (str/starts-with? attr "data-") (str/starts-with? attr "aria-")) attr
    (re-find #"[-:]" attr)                                               (str/camel (str/replace attr ":" "-"))
    :else                                                                attr))

(defn- css-key
  "The key of the CSS property `property` in a style object of React."
  [property]
  (cond
    (str/starts-with? property "--") (js/JSON.stringify property)
    (str/starts-with? property "-")  (let [k (str/camel (subs property 1))]
                                       (str (str/upper (subs k 0 1)) (subs k 1)))
    :else                            (str/camel property)))

(defn- style-object
  "The style object of React of the CSS declarations `style`."
  [style]
  (let [decls (keep (fn [decl]
                      (let [at (str/index-of decl ":")]
                        (when (and (some? at) (pos? at))
                          [(str/trim (subs decl 0 at)) (str/trim (subs decl (inc at)))])))
                    (str/split style #";"))]
    (str "{{ " (str/join ", " (map (fn [[k v]] (str (css-key k) ": " (js/JSON.stringify v))) decls)) " }}")))

(defn- jsx-attr
  "The JSX of the attribute `[attr value]`: its name alone without a
  value, its style an object, a value with a quote, a backslash or a
  brace as a JS string."
  [[attr value]]
  (let [n (jsx-name attr)]
    (cond
      (nil? value)                (str n)
      (= "style" attr)            (str n "=" (style-object value))
      (re-find #"[\"\\{}]" value) (str n "={" (js/JSON.stringify value) "}")
      :else                       (str n "=\"" value "\""))))

(defn- classes
  "The classes of the element of the attributes `attrs`, a set."
  [attrs]
  (or (some-> (some (fn [[k v]] (when (= "class" k) v)) attrs)
              (str/split #"\s+")
              (set))
      #{}))

(declare render-node)

(defn- render-children
  "The lines of `children`, `depth` steps of two spaces in, in `ctx` (see
  `render-node`)."
  [children ctx depth]
  (into [] (mapcat #(render-node % ctx depth)) children))

(defn- render-element
  "The lines of the element `node`: a `motion` element with the props
  `lines` (indented one step in from it) when it plays any."
  [{:keys [tag attrs children]} ctx depth lines]
  (let [indent  (str/repeat "  " depth)
        inner   (str indent "  ")
        tag     (if (some? lines) (str "motion." tag) tag)
        attrs   (map jsx-attr attrs)
        content (render-children children ctx (inc depth))]
    (if (some? lines)
      (-> [(str indent "<" tag)]
          (into (map #(str inner %)) attrs)
          (conj (str/rtrim lines))
          (into (if (seq content)
                  (-> [(str indent ">")] (into content) (conj (str indent "</" tag ">")))
                  [(str indent "/>")])))
      (let [open (str indent "<" tag (when (seq attrs) (str " " (str/join " " attrs))))]
        (if (seq content)
          (-> [(str open ">")] (into content) (conj (str indent "</" tag ">")))
          [(str open " />")])))))

(defn- unit-props
  "The props of the unit the element of the classes `cls` is, in the text
  `text` (an animated layer the code plays by units, see
  `ctac/react-layer`), as a function of the indent; nil for another
  element."
  [text cls]
  (when (some? text)
    (some (fn [c]
            (when-let [[_ k] (re-matches #"penpot-unit-(\d+)" c)]
              (let [k (js/parseInt k 10)]
                #((get-in text [:units :attrs]) k %))))
          cls)))

(defn- trim-props
  "The props of the element of the attributes `attrs` when it paints a
  stroke of `layer` (an animated layer that trims its strokes, see
  `ctac/react-layer`): its trim from where its piece of the outline
  starts, as its marker says (see `svgv/trim-attr`), as a function of
  the indent; nil for another element."
  [layer attrs]
  (when (some? layer)
    (when-let [from (some (fn [[k v]] (when (= svgv/trim-attr k) v)) attrs)]
      (let [from (js/parseFloat from)]
        #((:trim layer) from %)))))

(defn- without-rest-dash
  "`attrs` of an element whose trim motion.dev plays, without the dash of
  its stroke at rest in its style, which would hide the one it plays as
  attributes."
  [attrs]
  (mapv (fn [[k v :as attr]]
          (if (and (= "style" k) (some? v))
            [k (->> (str/split v #";")
                    (remove #(re-find #"^\s*stroke-dash(?:array|offset)\s*:" %))
                    (str/join ";"))]
            attr))
        attrs))

(defn- child-ctx
  "`ctx` for the elements in the one of the animated layer `layer`, if
  any: the text whose units they are, the layer whose strokes they
  paint."
  [ctx layer]
  (cond-> ctx
    (some? layer)
    (assoc :text (when (:units layer) layer)
           :trimmed (when (:trim layer) layer))))

(defn- render-node
  "The lines of `node`, `depth` steps of two spaces in. `ctx` holds
  `:layers`, the animation of the animated layers by the class of their
  element (see `board-jsx`), `:text`, the text whose units the node is
  in, and `:trimmed`, the layer whose strokes it is in."
  [node ctx depth]
  (let [indent (str/repeat "  " depth)]
    (cond
      (contains? node :comment)
      [(str indent "{/* " (str/trim (str/replace (:comment node) "*/" "* /")) " */}")]

      ;; the line breaks and indents `generate-html` puts between elements
      ;; go; any other text stays: a space between two units of a text, the
      ;; no-break space of an empty line
      (contains? node :text)
      (let [text (:text node)]
        (when-not (and (str/blank? text) (str/includes? text "\n"))
          [(str indent "{" (js/JSON.stringify text) "}")]))

      :else
      (let [attrs (:attrs node)
            cls   (classes attrs)
            layer (some (:layers ctx) cls)
            trim  (trim-props (:trimmed ctx) attrs)
            props (or (:attrs layer) (unit-props (:text ctx) cls) trim)
            lines (when (some? props) (props (str indent "  ")))
            node  (cond-> node
                    (and (some? trim) (some? lines)) (update :attrs without-rest-dash))]
        (render-element node (child-ctx ctx layer) depth lines)))))

(defn markup-jsx
  "The JSX of `markup` (see `parse`), indented by `depth` steps of two
  spaces, the elements of the animated layers of `layers` (their
  animation by the class of their element, see `ctac/react-layer`)
  `motion` elements, and so the units of the texts it plays by units and
  the elements that paint the strokes it trims."
  [markup layers depth]
  (str/join "\n" (render-children (parse markup) {:layers layers} depth)))

(defn board-jsx
  "The JSX of the board of `code` (see `cltl/code-timelines`) as the HTML
  of the code of the Inspect writes it from `objects` (marked with what
  the code plays, see `app.util.code-gen.motion/marked-objects`), each
  animated layer of `layers` (by id, see `ctac/react-layer`) a `motion`
  element (see `markup-jsx`), indented by `depth` steps of two spaces."
  [objects {:keys [board-id]} layers & {:keys [depth] :or {depth 3}}]
  (let [by-class (into {}
                       (keep (fn [[id layer]]
                               (when-let [shape (get objects id)]
                                 [(cgc/shape->selector shape) layer])))
                       layers)]
    (markup-jsx (or (html/generate-html objects (get objects board-id)) "") by-class depth)))
