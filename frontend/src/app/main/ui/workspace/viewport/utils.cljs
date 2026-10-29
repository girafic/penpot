; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.viewport.utils
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.main.ui.css-cursors :as cur]
   [app.main.ui.formats :refer [format-number]]))

(defn format-viewbox
  "Format a viewbox to a string"
  [vbox]
  (dm/str (format-number (:x vbox 0)) " "
          (format-number (:y vbox 0)) " "
          (format-number (:width vbox 0)) " "
          (format-number (:height vbox 0))))

(defn get-cursor [cursor]
  (case cursor
    :hand (cur/get-static "hand")
    :comments (cur/get-static "comments")
    :create-artboard (cur/get-static "create-artboard")
    :create-rectangle (cur/get-static "create-rectangle")
    :create-ellipse (cur/get-static "create-ellipse")
    :pen (cur/get-static "pen")
    :pencil (cur/get-static "pencil")
    :draw-path (cur/get-static "draw")
    :edit-path (cur/get-static "move")
    :create-shape (cur/get-static "create-shape")
    :duplicate (cur/get-static "duplicate")
    :zoom (cur/get-static "zoom")
    :zoom-in (cur/get-static "zoom-in")
    :zoom-out (cur/get-static "zoom-out")
    :picker (cur/get-static "picker")
    (cur/get-static "pointer-inner")))

;; Ensure that the label has always the same font
;; size, regardless of zoom
;; https://css-tricks.com/transforms-on-svg-elements/
(defn text-transform
  [{:keys [x y]} zoom]
  (let [inv-zoom (/ 1 zoom)]
    (dm/fmt "scale(%, %) translate(%, %)" inv-zoom inv-zoom (* zoom x) (* zoom y))))

(defn left?
  [cur cand]
  (let [closex? (mth/close? (:x cand) (:x cur) 0.01)]
    (cond
      (and closex? (< (:y cand) (:y cur))) cand
      closex?                              cur
      (< (:x cand) (:x cur))               cand
      :else                                cur)))

(defn top?
  [cur cand]
  (let [closey? (mth/close? (:y cand) (:y cur) 0.01)]
    (cond
      (and closey? (< (:x cand) (:x cur))) cand
      closey?                              cur
      (< (:y cand) (:y cur))               cand
      :else                                cur)))

(defn right?
  [cur cand]
  (let [closex? (mth/close? (:x cand) (:x cur) 0.01)]
    (cond
      (and closex? (< (:y cand) (:y cur))) cand
      closex?                              cur
      (> (:x cand) (:x cur))               cand
      :else                                cur)))

(defn title-transform-use-width?
  [{:keys [rotation] :as shape}]
  (let [side       (mth/ceil (/ (- rotation 45) 90))
        use-width? (even? side)]
    use-width?))

(defn title-transform
  [{:keys [points] :as shape} zoom grid-edition?]
  (let [leftmost  (->> points (reduce left?))
        topmost   (->> points (remove #{leftmost}) (reduce top?))
        rightmost (->> points (remove #{leftmost topmost}) (reduce right?))]
    (when (and (some? leftmost) (some? topmost) (some? rightmost))
      (let [left-top (gpt/to-vec leftmost topmost)
            left-top-angle (gpt/angle left-top)

            top-right (gpt/to-vec topmost rightmost)
            top-right-angle (gpt/angle top-right)

            ;; Choose the position that creates the less angle between left-side and top-side
            [label-pos angle h-pos v-pos]
            (if (< (mth/abs left-top-angle) (mth/abs top-right-angle))
              [leftmost left-top-angle left-top (gpt/perpendicular left-top)]
              [topmost top-right-angle top-right (gpt/perpendicular top-right)])

            delta-x (if grid-edition? 40 0)
            delta-y (if grid-edition? 50 10)

            label-pos
            (-> label-pos
                (gpt/subtract (gpt/scale (gpt/unit v-pos) (/ delta-y zoom)))
                (gpt/subtract (gpt/scale (gpt/unit h-pos) (/ delta-x zoom))))]
        (dm/fmt "rotate(% %,%) scale(%, %) translate(%, %)"
                ;; rotate
                angle (:x label-pos) (:y label-pos)
                ;; scale
                (/ 1 zoom) (/ 1 zoom)
                ;; translate
                (* zoom (:x label-pos)) (* zoom (:y label-pos)))))))

(defn with-shown
  "`objects` with the shapes of `ids` where the animation shows them:
  `shown` a delay of `cltl/shown-shapes`, forced only when there are
  `ids`."
  [objects shown ids]
  (if (empty? ids)
    objects
    (let [shown @shown]
      (reduce (fn [objects id]
                (if-let [shape (get shown id)]
                  (assoc objects id shape)
                  objects))
              objects
              ids))))

(defn- apply-wasm-modifiers-to-ids
  "`objects` with the shapes of `id-set` moved by `wasm-modifiers`
  (`{id transform}`). During a WASM drag, `wasm-modifiers` can list every
  propagated descendant (large variants); the SVG outlines only need the
  geometry of the selected, hovered and highlighted shapes, not the
  whole page."
  [objects wasm-modifiers id-set]
  (if (or (empty? wasm-modifiers) (empty? id-set))
    objects
    (reduce
     (fn [objs id]
       (if-let [t (get wasm-modifiers id)]
         (if (contains? objs id)
           (update objs id gsh/apply-transform t)
           objs)
         objs))
     objects
     id-set)))

(defn outline-ids
  "The shapes of `ids`, those the outlines of the WASM viewport draw, and
  the groups and boards they are in, whose tilts tilt them (see
  `app.common.geom.tilt`)."
  [objects ids]
  (into (set ids) (mapcat #(cfh/get-parent-ids objects %)) ids))

(defn outline-objects
  "`objects` with the shapes of `ids`, those the outlines of the WASM
  viewport draw, as it shows them: moved by `wasm-modifiers` while
  dragged and, in motion mode while paused, where the animation shows
  them (`shown`, a delay of `cltl/shown-shapes`, see `with-shown`), a
  path with the outline it has at the playhead. The groups and boards
  they are in too (see `outline-ids`)."
  [objects wasm-modifiers ids shown]
  (let [ids (outline-ids objects ids)]
    (cond-> (apply-wasm-modifiers-to-ids objects wasm-modifiers ids)
      (some? shown) (with-shown shown ids))))
