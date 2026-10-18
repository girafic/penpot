;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-turn-test
  "The flip and cube transitions of the view mode (Penpot Motion)."
  (:require
   [app.common.math :as mth]
   [app.main.ui.viewer.interactions :as vi]
   [cljs.test :as t :include-macros true]))

(def ^:private size {:width 400 :height 300})

(defn- parse
  [^js keyframe]
  (let [n  "([-\\d.e]+)"
        re (re-pattern (str "scale\\(" n "\\) perspective\\(" n "px\\) translateZ\\(" n "px\\) "
                            "rotate([XY])\\(" n "deg\\) translateZ\\(" n "px\\)"))
        [_ fit persp _ axis deg depth] (re-find re (.-transform keyframe))]
    {:offset (.-offset keyframe)
     :fit    (js/parseFloat fit)
     :persp  (js/parseFloat persp)
     :depth  (js/parseFloat depth)
     :axis   axis
     :deg    (js/parseFloat deg)}))

(defn- corners
  "Where the corners of a board of `size` show, from its middle, as CSS
  draws the transform of a keyframe."
  [{:keys [fit persp depth axis deg]} {:keys [width height]}]
  (let [r (mth/radians deg)
        c (mth/cos r)
        s (mth/sin r)]
    (for [x [(- (/ width 2)) (/ width 2)]
          y [(- (/ height 2)) (/ height 2)]]
      (let [[px py z] (if (= axis "Y")
                        [(+ (* x c) (* depth s)) y (+ (* (- x) s) (* depth (- c 1)))]
                        [x (- (* y c) (* depth s)) (+ (* y s) (* depth (- c 1)))])
            grow      (/ persp (- persp z))]
        [(* fit grow px) (* fit grow py)]))))

(defn- turn
  [type direction]
  (map #(map parse %)
       (vi/turn-keyframes {:animation-type type :direction direction :duration 300 :easing :linear}
                          size size)))

(t/deftest a-turning-board-stays-in-its-box
  (doseq [type [:flip :cube] direction [:right :left :up :down]]
    (t/testing (str (name type) " " (name direction))
      (let [[leaves comes-in] (turn type direction)]
        (t/is (= 13 (count leaves) (count comes-in)))
        (t/is (= [1 0] ((juxt :fit :deg) (first leaves))) "the board that leaves starts flat")
        (t/is (= [1 0] ((juxt :fit :deg) (last comes-in))) "the next one ends flat")
        (doseq [frame (concat leaves comes-in)
                [x y] (corners frame size)]
          (t/is (and (<= (mth/abs x) (+ 200 1e-6)) (<= (mth/abs y) (+ 150 1e-6)))
                (str "a corner at " x " " y " of " (:deg frame) "deg")))))))

(t/deftest a-flip-has-the-next-board-on-its-back
  (let [[leaves comes-in] (turn :flip :right)]
    (t/is (= [0 180] [(:deg (first leaves)) (:deg (last leaves))]))
    (t/is (every? true? (map #(and (= (- (:deg %1) 180) (:deg %2)) (= (:fit %1) (:fit %2)))
                             leaves comes-in)))
    (t/is (= (mth/round (* 0.75 1000)) (mth/round (* 1000 (:fit (nth leaves 6)))))
          "edge on, it is a quarter smaller: its near side does not grow out")))

(t/deftest a-cube-turns-two-sides-that-meet
  (doseq [[direction axis-of sign] [[:right first 1] [:left first -1] [:down second 1] [:up second -1]]]
    (t/testing (name direction)
      (let [[leaves comes-in] (turn :cube direction)
            a (corners (nth leaves 6) size)
            b (corners (nth comes-in 6) size)]
        (t/is (= 2 (count (for [p a q b
                                :when (< (mth/abs (- (first p) (first q))) 1e-6)
                                :when (< (mth/abs (- (second p) (second q))) 1e-6)]
                            p)))
              "two corners in common: an edge of the cube")
        (t/is (pos? (* sign (reduce + (map axis-of a))))
              "the board that leaves goes the way of the direction")))))
