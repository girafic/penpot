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
   [app.common.animation.export :as aexp]
   [app.common.data :as d]
   [app.util.code-gen.common :as cgc]
   [app.util.code-gen.markup-html :as html]
   [cuerdas.core :as str]))

(def ^:private token-re
  "The pieces of the markup: comments, tags and the text between them."
  #"<!--[\s\S]*?-->|</?[A-Za-z][^>]*>|[^<]+")

(def ^:private void-tags
  #{"img" "br" "hr" "input" "meta" "link" "source" "wbr"})

(def ^:private entities
  {"amp" "&" "lt" "<" "gt" ">" "quot" "\"" "apos" "'" "nbsp" " "})

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
  [token]
  (when-let [[_ closing tag attrs self] (re-find #"^<(/)?([A-Za-z][\w:.-]*)([\s\S]*?)(/)?>$" token)]
    {:closing? (some? closing)
     :tag tag
     :self? (or (some? self) (contains? void-tags (str/lower tag)))
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
          (let [{:keys [closing? tag self? attrs]} (parse-tag token)]
            (cond
              (nil? tag)
              (recur (rest tokens) stack)

              ;; up to the element it closes; one never opened (as an
              ;; `</img>`) closes nothing
              closing?
              (if-let [at (d/index-of-pred (vec (reverse (rest stack))) #(= tag (:tag %)))]
                (recur (rest tokens) (fold stack (inc at)))
                (recur (rest tokens) stack))

              self?
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
  [attr]
  (cond
    (contains? attr-names attr)                                     (get attr-names attr)
    (or (str/starts-with? attr "data-") (str/starts-with? attr "aria-")) attr
    (re-find #"[-:]" attr)                                          (str/camel (str/replace attr ":" "-"))
    :else                                                           attr))

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
  [[attr value]]
  (let [n (jsx-name attr)]
    (cond
      (nil? value)               n
      (= "style" attr)           (str n "=" (style-object value))
      (re-find #"[\"\\{}]" value) (str n "={" (js/JSON.stringify value) "}")
      :else                      (str n "=\"" value "\""))))

(defn- classes
  [attrs]
  (some-> (some (fn [[k v]] (when (= "class" k) v)) attrs)
          (str/split #"\s+")
          (set)))

(declare render-node)

(defn- render-children
  [children ctx depth]
  (into [] (mapcat #(render-node % ctx depth)) children))

(defn- render-element
  "The lines of the element `node`: a `motion` element with `props` (a
  function of the indent, see `ctac/react-layer`) when it plays any."
  [{:keys [tag attrs children]} ctx depth props]
  (let [indent  (str/repeat "  " depth)
        inner   (str indent "  ")
        tag     (if (some? props) (str "motion." tag) tag)
        attrs   (map jsx-attr attrs)
        content (render-children children ctx (inc depth))]
    (if (some? props)
      (-> [(str indent "<" tag)]
          (into (map #(str inner %)) attrs)
          (conj (str/rtrim (props inner)))
          (into (if (seq content)
                  (-> [(str indent ">")] (into content) (conj (str indent "</" tag ">")))
                  [(str indent "/>")])))
      (let [open (str indent "<" tag (when (seq attrs) (str " " (str/join " " attrs))))]
        (if (seq content)
          (-> [(str open ">")] (into content) (conj (str indent "</" tag ">")))
          [(str open " />")])))))

(defn- render-node
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
      (let [{:keys [tag attrs]} node
            cls       (or (classes attrs) #{})
            layer     (some (:layers ctx) cls)
            ;; the unit `k` of a text the code plays by units
            unit      (when-let [text (:text-layer ctx)]
                        (some #(when-let [[_ k] (re-matches #"penpot-unit-(\d+)" %)]
                                 (let [k (js/parseInt k 10)]
                                   (fn [indent] ((get-in text [:units :attrs]) k indent))))
                              cls))
            ;; an element painting a trimmed stroke, from where its piece
            ;; of the outline starts
            trim      (when-let [{:keys [layer pieces]} (:trim ctx)]
                        (when (and (:paint? ctx) (not= "defs" tag)
                                   (or (not= :outer (:paint? ctx)) (some #(= "mask" (first %)) attrs)))
                          (let [k    (some #(when-let [[_ k] (re-matches #"trim-piece-(\d+)" %)] (js/parseInt k 10)) cls)
                                from (if (and (some? k) (some? pieces)) (first (nth pieces k [0])) 0)]
                            (fn [indent] ((:trim layer) from indent)))))
            props     (or (:attrs layer) unit trim)
            ctx       (cond-> ctx
                        (some? layer)
                        (assoc :text-layer (when (:units layer) layer)
                               :trim (when (:trim layer)
                                       {:layer layer :pieces (aexp/trim-pieces (:shape layer))}))

                        :always
                        (assoc :paint? (cond
                                         (contains? cls "stroke-shape")       :plain
                                         (contains? cls "inner-stroke-shape") :plain
                                         (contains? cls "outer-stroke-shape") :outer
                                         :else                                nil)))]
        (render-element node ctx depth (when (and (some? props) (some? (props ""))) props))))))

(defn board-jsx
  "The JSX of the board of `code` (see `cltl/code-timelines`) as the HTML
  of the code of the Inspect writes it from `objects` (marked with what
  the code plays, see `app.util.code-gen.motion/marked-objects`), each
  animated layer of `layers` (by id, see `ctac/react-layer`) a `motion`
  element, indented by `depth` steps of two spaces."
  [objects {:keys [board-id]} layers & {:keys [depth] :or {depth 3}}]
  (let [by-class (into {}
                       (keep (fn [[id layer]]
                               (when-let [shape (get objects id)]
                                 [(cgc/shape->selector shape) (assoc layer :shape shape)])))
                       layers)
        markup   (html/generate-html objects (get objects board-id))]
    (str/join "\n" (render-children (parse (or markup "")) {:layers by-class} depth))))
