;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.shapes.text.html-text
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.text :as legacy.txt]
   [app.main.ui.shapes.text.styles :as sts]
   [rumext.v2 :as mf]))

(defn- leaf-pieces
  "The text `text` of a leaf, from `at` in its paragraph, cut at the
  units `units` of the paragraph (`{:start :end :k}`, by start): `[text
  k]` each, `k` nil between units."
  [text at units]
  (let [end (+ at (count text))]
    (loop [pos at units (filter #(and (< (:start %) end) (> (:end %) at)) units) out []]
      (if-let [{:keys [start k] unit-end :end} (first units)]
        (let [from (max start pos)
              to   (min unit-end end)]
          (recur to (rest units) (cond-> out
                                   (< pos from) (conj [(subs text (- pos at) (- from at)) nil])
                                   :always      (conj [(subs text (- from at) (- to at)) k]))))
        (cond-> out
          (< pos end) (conj [(subs text (- pos at)) nil]))))))

(defn- with-unit-pieces
  "`content` (indexed, see `legacy.txt/index-content`) with the leaves of
  the paragraphs of its first paragraph set cut at `bases`, the units the
  code plays one by one (`{:paragraph :start :end}` in unit order, see
  `app.common.types.animation/unit-timelines`): `:unit-pieces` (see
  `leaf-pieces`)."
  [content bases]
  (let [by-paragraph (group-by :paragraph (map-indexed #(assoc %2 :k %1) bases))

        paragraph
        (fn [index node]
          (if-let [units (get by-paragraph index)]
            (update node :children
                    (fn [leaves]
                      (first (reduce (fn [[out at] leaf]
                                       (let [text (or (:text leaf) "")]
                                         [(conj out (assoc leaf :unit-pieces (leaf-pieces text at units)))
                                          (+ at (count text))]))
                                     [[] 0]
                                     leaves))))
            node))]
    (update content :children
            (fn [sets]
              (cond-> (vec sets)
                (seq sets)
                (update 0 update :children #(into [] (map-indexed paragraph) %)))))))

(mf/defc render-text*
  [{:keys [node parent shape is-code]}]
  (let [text   (:text node)
        style  (if (= text "")
                 (sts/generate-text-styles shape parent)
                 (sts/generate-text-styles shape node))
        class  (when is-code (:$id node))
        pieces (:unit-pieces node)]
    [:span.text-node {:style style :class class}
     (cond
       (seq pieces)
       (for [[index [piece k]] (d/enumerate pieces)]
         (if (some? k)
           [:span {:key index :class (dm/str "penpot-unit penpot-unit-" k)} piece]
           [:* {:key index} piece]))

       (= text "")
       "\u00A0"

       :else
       text)]))

(mf/defc render-root*
  [{:keys [node children shape is-code]}]
  (let [style (sts/generate-root-styles shape node is-code)
        class (when is-code (:$id node))]
    [:div.root.rich-text
     {:style style
      :class class
      :xmlns "http://www.w3.org/1999/xhtml"}
     children]))

(mf/defc render-paragraph-set*
  [{:keys [node children shape is-code]}]
  (let [style (when-not is-code (sts/generate-paragraph-set-styles shape))
        class (when is-code (:$id node))]
    [:div.paragraph-set {:style style :class class} children]))

(mf/defc render-paragraph*
  [{:keys [node children shape is-code]}]
  (let [style (when-not is-code (sts/generate-paragraph-styles shape node))
        class (when is-code (:$id node))
        dir   (:text-direction node "auto")]
    [:p.paragraph {:style style :dir dir :class class} children]))

;; -- Text nodes
(mf/defc render-node*
  {::mf/props :obj}
  [{:keys [node is-code] :as props}]
  (let [{:keys [type text children] :as parent} node]
    (if (string? text)
      [:> render-text* props]
      (let [component (case type
                        "root" render-root*
                        "paragraph-set" render-paragraph-set*
                        "paragraph" render-paragraph*
                        nil)]
        (when component
          [:> component props
           (for [[index child-node] (d/enumerate children)]
             [:> render-node*
              (mf/spread-props props
                               {:node child-node
                                :parent parent
                                :index index
                                :key index
                                :is-code is-code})])])))))

(mf/defc text-shape*
  {::mf/forward-ref true}
  [{:keys [shape grow-type is-code]} ref]
  (let [{:keys [id x y width height content]} shape

        ;; the code plays the units in `:code-units` one by one
        content (cond-> content
                  is-code                    (legacy.txt/index-content)
                  (seq (:code-units shape))  (with-unit-pieces (:code-units shape)))

        style
        (when-not is-code
          #js {:position "fixed"
               :left 0
               :top 0
               :background "white"
               :width  (if (#{:auto-width} grow-type) 100000 width)
               :height (if (#{:auto-height :auto-width} grow-type) 100000 height)})]

    [:div.text-node-html
     {:id (dm/str "html-text-node-" id)
      :ref ref
      :data-x x
      :data-y y
      :style style}
     ;; We use a class here because react has a bug that won't use the appropriate selector for
     ;; `background-clip`
     (when (not is-code)
       [:style ".text-node { background-clip: text;
                             -webkit-background-clip: text; }"])
     [:> render-node* {:index 0
                       :shape shape
                       :node content
                       :is-code is-code}]]))
