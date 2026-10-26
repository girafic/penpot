;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation-text-style
  "The type style of a text as it plays (see `app.common.types.animation`):
  the size of its first span, the others keeping their share, letter
  spacing, line height and the face of the played weight."
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.types.text :as txt]))

(def text-style-properties
  "The type style of a text (see `apply-text-style`): the size of its
  first span (px), the others keeping their share of it, and of all its
  spans the spacing of the letters (px), the height of the lines (a share
  of the size) and the weight (100 to 900, the nearest face its font
  has). Only texts have them."
  [:font-size :letter-spacing :line-height :font-weight])

(defn- text-number
  [v]
  (if (number? v) v (d/parse-double v)))

(defn text-style-rest
  "The value of the text style property `property` (see
  `text-style-properties`) of the text `shape` without keyframes: that of
  its first span, or of a text by default."
  [shape property]
  (let [span (first (txt/node-seq txt/is-text-node? (:content shape)))]
    (or (text-number (get span property))
        (text-number (get txt/default-text-attrs property)))))

(defonce ^:private font-weight-face (atom nil))

(defn set-font-weight-face!
  "Have `f` give the face a weight of a text plays in (see
  `apply-text-style`): `(f font-id font-style weight)`, the face of the
  font nearest it in that style, `{:font-variant-id :font-weight
  :font-style}`, or nil for a font it does not know. The hosts know the
  fonts; without it a span takes the weight alone."
  [f]
  (reset! font-weight-face f))

(defn- text-style-value
  "`v` as a text keeps it: a string, to 3 decimals, a whole one without
  them (on the JVM as in JS)."
  [v]
  (let [v (mth/precision (double v) 3)]
    (if (== v (mth/floor v)) (str (long v)) (str v))))

(defn apply-text-style
  "The content of the text `shape` as the text style `values` (see
  `text-style-properties`) play it: each span and paragraph scaled as much
  as its first span is to the played size, their letters spaced and their
  lines as high as played, and each span in the face of its font nearest
  the played weight (see `set-font-weight-face!`)."
  [shape values]
  (let [base   (text-style-rest shape :font-size)
        scale  (when (and (contains? values :font-size) (pos? base))
                 (/ (:font-size values) base))
        weight (some-> (:font-weight values) (mth/round) (mth/clamp 1 1000))
        face   (when (some? weight)
                 (let [f @font-weight-face]
                   (fn [node]
                     (or (when (some? f)
                           (f (or (:font-id node) (:font-id txt/default-text-attrs))
                              (or (:font-style node) "normal")
                              weight))
                         {:font-weight (str weight)}))))
        style  (fn [node]
                 (let [span? (txt/is-text-node? node)
                       size  (text-number (:font-size node))]
                   (cond-> node
                     (and (some? scale) (or span? (some? size)))
                     (assoc :font-size (text-style-value (* scale (or size base))))

                     (contains? values :letter-spacing)
                     (assoc :letter-spacing (text-style-value (:letter-spacing values)))

                     (contains? values :line-height)
                     (assoc :line-height (text-style-value (:line-height values)))

                     (and span? (some? face))
                     (merge (select-keys (face node) [:font-variant-id :font-weight :font-style])))))]
    (txt/transform-nodes #(or (txt/is-text-node? %) (txt/is-paragraph-node? %))
                         style
                         (:content shape))))
