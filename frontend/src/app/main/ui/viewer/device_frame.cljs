;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.device-frame
  "A device round the screen of the view mode (see
  `app.main.data.viewer.devices`). The view mode places three layers in
  one stage: the drawing of the device below, its section of the board
  as the screen (`screen-style`), and the cutouts over it, which let the
  pointer through. The section stays where it is in the view mode, only
  its box changes: what listens to it keeps listening."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.main.data.viewer.devices :as dvd]
   [rumext.v2 :as mf]))

(defn- role-class
  [role]
  (case role
    :band      (stl/css :band)
    :glass     (stl/css :glass)
    :face      (stl/css :face)
    :button    (stl/css :button)
    :base      (stl/css :base)
    :base-dark (stl/css :base-dark)
    :chin      (stl/css :chin)
    :front     (stl/css :front)
    :stand     (stl/css :stand)
    :ring      (stl/css :ring)
    :lens      (stl/css :lens)
    :cutout    (stl/css :cutout)
    :crease    (stl/css :crease)
    :toolbar   (stl/css :toolbar)
    :rule      (stl/css :rule)
    :close     (stl/css :close)
    :minimize  (stl/css :minimize)
    :zoom      (stl/css :zoom)
    :field     (stl/css :field)
    :title     (stl/css :title)))

(defn- fit-text
  "`text` cut with an ellipsis to about `max-w` px at `size` px."
  [text size max-w]
  (let [n (max 1 (mth/floor (/ max-w (* 0.55 size))))]
    (if (> (count text) n)
      (dm/str (subs text 0 (dec n)) "…")
      text)))

(defn- shape-element
  [label index {:keys [type role] :as shape}]
  (case type
    :circle
    (mf/html [:circle {:key index :class (role-class role)
                       :cx (:cx shape) :cy (:cy shape) :r (:r shape)}])
    :label
    (mf/html [:text {:key index :class (role-class role)
                     :x (:x shape) :y (:y shape) :font-size (:size shape)
                     :text-anchor "middle" :dominant-baseline "central"}
              (fit-text (or label "") (:size shape) (:max-w shape))])
    (mf/html [:rect {:key index :class (role-class role)
                     :x (:x shape) :y (:y shape) :width (:w shape) :height (:h shape)
                     :rx (:r shape)}])))

(mf/defc device-drawing*
  "The device `kind` of `layout` (see `dvd/layout`) in its `finish`: its
  body below the screen, or its `cutouts` over it, in the box of the
  device; turned a quarter to the left when it lies on its side. A
  browser shows `label` in its address field."
  [{:keys [layout kind finish cutouts label]}]
  (let [{:keys [x y w h view-w view-h landscape? geometry]} layout]
    [:svg {:class (stl/css :drawing)
           :aria-hidden true
           :data-finish (dvd/parse-finish finish)
           :data-kind (some-> kind name)
           :view-box (dm/str "0 0 " view-w " " view-h)
           :style {:left x :top y :width w :height h}}
     [:g {:transform (when landscape?
                       (dm/str "translate(0 " (:w geometry) ") rotate(-90)"))}
      (map-indexed (partial shape-element label)
                   (if (true? cutouts) (:cutouts geometry) (:parts geometry)))]]))

(defn screen-style
  "The box of the section of the view mode as the screen of the device
  of `layout`, in the stage it shares with the drawing: a style object,
  as React takes it."
  [layout]
  (let [{:keys [x y w h radii]} (:screen layout)
        [tl tr br bl] radii]
    #js {:marginLeft (dm/str x "px")
         :marginTop (dm/str y "px")
         :width (dm/str w "px")
         :height (dm/str h "px")
         :borderRadius (dm/str tl "px " tr "px " br "px " bl "px")}))

(defn use-size
  "The size `[w h]` of the element of `ref` as it renders, nil before;
  `[w h]` again only when it changes. It measures again as `present`
  changes: whether the element is rendered."
  [ref present]
  (let [size* (mf/use-state nil)]
    (mf/with-effect [ref present]
      (when-let [node (mf/ref-val ref)]
        (let [measure! (fn []
                         (let [size [(.-clientWidth node) (.-clientHeight node)]]
                           ;; the same size renders nothing again
                           (swap! size* #(if (= % size) % size))))
              observer (js/ResizeObserver. measure!)]
          (measure!)
          (.observe observer node)
          #(.disconnect observer))))
    (deref size*)))
