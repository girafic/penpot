;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation-layout
  "The spacing of an auto layout and of the layers in it as it plays (see
  `app.common.types.animation`): gaps, padding, margins and the least
  and most sizes."
  (:require
   [app.common.data.macros :as dm]))

(def layout-container-properties
  "The spacing of a board with an auto layout (flex or grid), px: the
  gaps between its rows and its columns and its padding, top first."
  [:layout-row-gap :layout-column-gap
   :layout-padding-top :layout-padding-right :layout-padding-bottom :layout-padding-left])

(def layout-item-properties
  "The spacing of a layer in an auto layout, px: its margins, top first,
  and its least and most width and height."
  [:layout-margin-top :layout-margin-right :layout-margin-bottom :layout-margin-left
   :layout-min-width :layout-max-width :layout-min-height :layout-max-height])

(def layout-paths
  "Where the layout properties are in a shape (see `apply-layout`)."
  {:layout-row-gap        [:layout-gap :row-gap]
   :layout-column-gap     [:layout-gap :column-gap]
   :layout-padding-top    [:layout-padding :p1]
   :layout-padding-right  [:layout-padding :p2]
   :layout-padding-bottom [:layout-padding :p3]
   :layout-padding-left   [:layout-padding :p4]
   :layout-margin-top     [:layout-item-margin :m1]
   :layout-margin-right   [:layout-item-margin :m2]
   :layout-margin-bottom  [:layout-item-margin :m3]
   :layout-margin-left    [:layout-item-margin :m4]
   :layout-min-width      [:layout-item-min-w]
   :layout-max-width      [:layout-item-max-w]
   :layout-min-height     [:layout-item-min-h]
   :layout-max-height     [:layout-item-max-h]})

(defn layout-rest
  "The value of the layout property `property` of `shape` without
  keyframes: its own, else no gap, padding or margin, and its own width
  or height for a least or most size it has none of."
  [shape property]
  (or (get-in shape (get layout-paths property))
      (case property
        (:layout-min-width :layout-max-width)   (dm/get-in shape [:selrect :width] 0)
        (:layout-min-height :layout-max-height) (dm/get-in shape [:selrect :height] 0)
        0)))

(def ^:private layout-maps
  "The maps of gaps, padding and margins of a shape with no spacing, which
  its own fill in (see `apply-layout`)."
  {:layout-gap         {:row-gap 0 :column-gap 0}
   :layout-padding     {:p1 0 :p2 0 :p3 0 :p4 0}
   :layout-item-margin {:m1 0 :m2 0 :m3 0 :m4 0}})

(defn apply-layout
  "The attributes of `shape` its layout properties in `values` change
  (see `layout-paths`): `{attribute value}`, the maps of its gaps,
  padding and margins whole, a side it has none of at 0 (as it draws)."
  [shape values]
  (reduce-kv (fn [changed property [attr :as path]]
               (if (contains? values property)
                 (assoc changed attr (if (next path)
                                       (assoc (or (get changed attr)
                                                  (merge (get layout-maps attr) (get shape attr)))
                                              (second path)
                                              (get values property))
                                       (get values property)))
                 changed))
             {}
             layout-paths))
