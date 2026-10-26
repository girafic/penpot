;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation-paint
  "The paint of the animation model (see `app.common.types.animation`):
  colours and gradients as they blend (stops evened out, see
  `pad-stops`), the widths of the sides of a stroke and the dash pattern
  of a dashed one."
  (:require
   [app.common.math :as mth]
   [app.common.types.color :as clr]))

(def gradient-properties
  "The properties whose values are whole gradients (see `lerp-gradient`):
  of a gradient fill and of a gradient stroke."
  #{:fill-gradient :stroke-gradient})

(defn gradient-property?
  [property]
  (contains? gradient-properties property))

(defn lerp
  [a b t]
  (+ a (* (- b a) t)))

(defn- clamp-byte
  [n]
  (-> n mth/round (max 0) (min 255) int))

(defn lerp-color
  "Mix two hex colours in RGB. A step (`t` unused by the caller) is
  handled before this is called."
  [from to t]
  (let [[fr fg fb] (clr/hex->rgb from)
        [tr tg tb] (clr/hex->rgb to)]
    (clr/rgb->hex [(clamp-byte (lerp fr tr t))
                   (clamp-byte (lerp fg tg t))
                   (clamp-byte (lerp fb tb t))])))

(defn gradient-value?
  [v]
  (and (map? v) (contains? v :stops)))

(defn gradient-stops
  "The stops of `gradient` by their offset, each with its opacity."
  [gradient]
  (->> (:stops gradient)
       (map #(update % :opacity (fn [o] (or o 1))))
       (sort-by :offset)
       (vec)))

(defn- stop-slots
  "The slots of the sorted offsets `ref` that the sorted offsets `own`, as
  many or fewer, take: in order, as near their offsets as can be."
  [own ref]
  (let [c    (count own)
        n    (count ref)
        memo (volatile! {})
        best (fn best [i j]
               (if-let [found (find @memo [i j])]
                 (val found)
                 (let [result
                       (cond
                         (= i c)               [0 []]
                         (> (- c i) (- n j))   nil
                         :else
                         (let [take (when-let [[cost slots] (best (inc i) (inc j))]
                                      [(+ cost (mth/abs (- (nth own i) (nth ref j))))
                                       (into [j] slots)])
                               skip (best i (inc j))]
                           (cond
                             (nil? take)                    skip
                             (nil? skip)                    take
                             (<= (first take) (first skip)) take
                             :else                          skip)))]
                   (vswap! memo assoc [i j] result)
                   result)))]
    (second (best 0 0))))

(defn pad-stops
  "The stops of `gradient` (see `gradient-stops`), as many as those of
  `reference` when it has fewer: its own in the slots of the stops of the
  reference nearest them (see `stop-slots`), and in the others stops that
  draw as it does, its colour at the offset of the reference there, kept
  between its own around. So a gradient blends into one of more stops,
  each of its stops going to the nearest of the other."
  [gradient reference]
  (let [own (gradient-stops gradient)
        ref (gradient-stops reference)]
    (if (>= (count own) (count ref))
      own
      (let [slots   (stop-slots (mapv :offset own) (mapv :offset ref))
            by-slot (zipmap slots own)]
        (mapv (fn [j]
                (or (get by-slot j)
                    (let [before (some->> (filter #(< % j) slots) (last) (get by-slot))
                          after  (some->> (filter #(> % j) slots) (first) (get by-slot))
                          offset (mth/clamp (:offset (nth ref j))
                                            (if before (:offset before) 0)
                                            (if after (:offset after) 1))]
                      (-> (clr/interpolate-gradient own offset)
                          (select-keys [:color :opacity :offset])))))
              (range (count ref)))))))

(defn evened-gradient-row
  "`keyframes`, the time-sorted keyframes of a row of gradients, each
  with as many stops as the one with most (see `pad-stops`), so each
  blends into the next stop by stop and every frame has as many."
  [keyframes]
  (let [most (:value (apply max-key #(count (:stops (:value %))) keyframes))
        n    (count (:stops most))]
    (if (every? #(= n (count (:stops (:value %)))) keyframes)
      keyframes
      (mapv (fn [kf]
              (cond-> kf
                (< (count (:stops (:value kf))) n)
                (assoc-in [:value :stops] (pad-stops (:value kf) most))))
            keyframes))))

(defn lerp-gradient
  "The gradient `t` of the way from `from` to `to`: its points and width,
  and stop by stop (as many in each, see `pad-stops`) the colour in RGB,
  the opacity and the offset. One of another type (linear, radial) holds
  until `to`."
  [from to t]
  (if (not= (:type from) (:type to))
    (if (>= t 1) to from)
    (let [a   (pad-stops from to)
          b   (pad-stops to from)
          num   (fn [k] (lerp (get from k 0) (get to k 0) t))]
      (assoc from
             :start-x (num :start-x) :start-y (num :start-y)
             :end-x (num :end-x) :end-y (num :end-y)
             :width (num :width)
             :stops (->> (map (fn [sa sb]
                                {:color   (lerp-color (:color sa) (:color sb) t)
                                 :opacity (mth/clamp (lerp (:opacity sa) (:opacity sb) t) 0 1)
                                 :offset  (mth/clamp (lerp (:offset sa) (:offset sb) t) 0 1)})
                              a b)
                         (sort-by :offset)
                         (vec))))))

(defn dashed-stroke?
  "A stroke drawn in dashes of its own length and gap (`:stroke-dash`,
  `:stroke-gap`), which its dash keyframes play on; dotted and mixed
  strokes take theirs from their width."
  [stroke]
  (= :dashed (:stroke-style stroke)))

(def stroke-side-properties
  "The widths of the sides of a stroke, px, top first: rectangles and
  boards draw them (see `sided-shape?`), with the WASM renderer."
  [:stroke-width-top :stroke-width-right :stroke-width-bottom :stroke-width-left])

(defn sided-shape?
  "Whether `shape` draws the sides of its strokes as wide as each says: a
  rectangle or a board."
  [shape]
  (contains? #{:rect :frame} (:type shape)))

(defn sided-stroke?
  "Whether `stroke` has a width of its own for its sides (the Design tab
  sets all four, see `app.common.types.stroke/materialize-stroke-side-widths`)."
  [stroke]
  (boolean (some #(some? (get stroke %)) stroke-side-properties)))

(defn stroke-side-width
  "The width of the side `property` of `stroke`: its own, or the stroke's."
  [stroke property]
  (or (get stroke property) (:stroke-width stroke) 0))

(defn stroke-sides-even?
  "Whether the sides of `stroke` are all as wide."
  [stroke]
  (apply = (map #(stroke-side-width stroke %) stroke-side-properties)))

(defn sided
  "`stroke` with the widths of its sides that play (`played`, `{side
  width}`) and, when its width plays (`width`, else nil), each other side
  that wide: all four set and `:stroke-width` the top one, as the Design
  tab keeps them, so render-wasm draws the sides, or, all alike, the one
  width."
  [stroke played width]
  (let [sides (into {}
                    (map (fn [side]
                           [side (cond
                                   (contains? played side) (get played side)
                                   (some? width)           width
                                   :else                   (stroke-side-width stroke side))]))
                    stroke-side-properties)]
    (merge stroke sides {:stroke-width (:stroke-width-top sides)})))

(defn stroke-dash
  "The dash and gap of the dashed `stroke`, px: its own, or the width
  plus 10 that a stroke without them draws."
  [stroke]
  (let [width (or (:stroke-width stroke) 1)]
    [(or (:stroke-dash stroke) (+ width 10))
     (or (:stroke-gap stroke) (+ width 10))]))
