;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.geom.offset-lines
  "Offset lines (Penpot Motion): parallel copies of the outline of a
  shape, `:count` of them `:spacing` px apart across it, from one side to
  the other, their colour and opacity going from those of the inner side
  to those of the outer one (the side the outline turns away from), as a
  light painting or an engraving does. A group holding them keeps how
  they are made (`:offset-lines`, see `cts/schema:offset-lines`) and its
  source, hidden; they are paths like any other, which every renderer
  and export draws and a trim of the group draws together or in turn."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.color :as clr]
   [app.common.types.path :as path]))

(def defaults
  "How offset lines are made at first: a band of 24 lines, 4 px apart,
  dim inside and bright outside."
  {:count 24 :spacing 4 :width 2.5
   :inner-color "#5a667e" :inner-opacity 0.3
   :outer-color "#ffffff" :outer-opacity 1})

(def ^:private smoothness
  "How far, px, the lines may leave the points they are fitted through."
  0.25)

(def ^:private max-miter
  "The most a corner lengthens the offset of a point by."
  3)

(defn offsets
  "The offsets of the lines of `spec`, px from the outline, outwards
  positive: centred on it."
  [{:keys [count spacing]}]
  (mapv #(* spacing (- % (/ (dec count) 2))) (range count)))

(defn- normal
  "The normal of the segment from `a` to `b`, of length 1, to its left as
  y goes down: outwards for an outline going clockwise."
  [a b]
  (let [dx  (- (:x b) (:x a))
        dy  (- (:y b) (:y a))
        len (mth/hypot dx dy)]
    (when (> len 1e-9)
      (gpt/point (/ (- dy) len) (/ dx len)))))

(defn- dedupe-points
  [points]
  (reduce (fn [acc p]
            (if (and (seq acc) (< (gpt/distance (peek acc) p) 1e-6)) acc (conj acc p)))
          []
          points))

(defn offset-polyline
  "The points of the polyline `points` (closed when `closed?`) moved `d`
  px along their normals, the normals of their two segments averaged (a
  corner lengthens it, up to `max-miter`)."
  [points closed? d]
  (let [pts (cond-> (dedupe-points points)
              (and closed? (> (count points) 2) (< (gpt/distance (first points) (peek (vec points))) 1e-6))
              pop)
        n   (count pts)]
    (if (< n 2)
      pts
      (mapv (fn [i]
              (let [p      (nth pts i)
                    prev   (cond (pos? i) (nth pts (dec i)) closed? (nth pts (dec n)))
                    next   (cond (< i (dec n)) (nth pts (inc i)) closed? (nth pts 0))
                    n1     (some-> prev (normal p))
                    n2     (some->> next (normal p))
                    [nx ny] (cond
                              (and n1 n2) [(+ (:x n1) (:x n2)) (+ (:y n1) (:y n2))]
                              n1          [(:x n1) (:y n1)]
                              n2          [(:x n2) (:y n2)]
                              :else       [0 0])
                    len    (mth/hypot nx ny)
                    ;; along the bisector, as far as the two segments
                    ;; are apart, at most `max-miter`
                    miter  (if (and n1 n2 (> len 1e-9))
                             (min max-miter (/ 2 len))
                             1)
                    k      (if (> len 1e-9) (/ (* d miter) len) 0)]
                (gpt/point (+ (:x p) (* nx k)) (+ (:y p) (* ny k)))))
            (range n)))))

(defn- outward
  "1 when the polylines of an outline go round clockwise (as y goes
  down), so that their left is outside; -1 otherwise, and for open ones."
  [polylines]
  (let [{:keys [points closed?]} (first polylines)
        pts (vec points)]
    (if (and closed? (> (count pts) 2))
      (let [area (reduce + (map (fn [a b] (- (* (:x a) (:y b)) (* (:x b) (:y a))))
                                pts (conj (subvec pts 1) (first pts))))]
        (if (pos? area) -1 1))
      1)))

(defn- mix
  "The colour `t` of the way from `a` to `b` (hex)."
  [a b t]
  (let [[r1 g1 b1] (clr/hex->rgb a)
        [r2 g2 b2] (clr/hex->rgb b)
        lerp       (fn [x y] (mth/round (+ x (* t (- y x)))))]
    (clr/rgb->hex [(lerp r1 r2) (lerp g1 g2) (lerp b1 b2)])))

(defn lines
  "The lines `spec` makes from the outline of `source`: `{:offset :content
  :color :opacity :width}` each, from the inner side to the outer one, its
  path content fitted smoothly through its points."
  [source spec]
  (let [spec      (merge defaults spec)
        polylines (gtl/outline-polylines source)
        sign      (outward polylines)
        ds        (offsets spec)
        n         (count ds)]
    (into []
          (map-indexed
           (fn [i d]
             (let [t        (if (> n 1) (/ i (dec n)) 1)
                   subpaths (keep (fn [{:keys [points closed?]}]
                                    (let [pts (offset-polyline points closed? (* sign d))]
                                      (when (> (count pts) 1)
                                        (path/smooth-points->content (cond-> pts closed? (conj (first pts))) smoothness))))
                                  polylines)]
               {:offset  d
                :content (path/content (into [] (mapcat seq) subpaths))
                :color   (mix (:inner-color spec) (:outer-color spec) t)
                :opacity (+ (:inner-opacity spec) (* t (- (:outer-opacity spec) (:inner-opacity spec))))
                :width   (:width spec)})))
          ds)))
