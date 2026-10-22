;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.math.noise
  "Smooth pseudo-random values in [-1, 1] along a line, the same for a
  seed on every runtime (no `rand`, a 32-bit integer hash, exact in both):
  value noise, a random value at each whole `x` and a smooth curve
  through them (Catmull–Rom), as the wiggle of After Effects moves to a
  new random value about once per unit. Neighbouring seeds and whole
  numbers give unrelated values. With a `period`, the values repeat
  every `period` units, so a loop ends where it starts."
  (:require
   [app.common.math :as mth]))

(defn- u32
  "`x` as an unsigned 32-bit integer: its low 32 bits."
  [x]
  #?(:clj  (bit-and (long x) 0xffffffff)
     :cljs (unsigned-bit-shift-right x 0)))

(defn- mul32
  "The low 32 bits of `a` × `b`, two unsigned 32-bit integers."
  [a b]
  #?(:clj  (bit-and (unchecked-multiply (long a) (long b)) 0xffffffff)
     :cljs (unsigned-bit-shift-right (js/Math.imul a b) 0)))

(defn- hash32
  "The unsigned 32-bit integer `x` mixed (lowbias32, C. Wellons): each bit
  of it flips about half the bits of the result."
  [x]
  (let [x (u32 (bit-xor x (unsigned-bit-shift-right x 16)))
        x (mul32 x 0x7feb352d)
        x (u32 (bit-xor x (unsigned-bit-shift-right x 15)))
        x (mul32 x 0x846ca68b)]
    (u32 (bit-xor x (unsigned-bit-shift-right x 16)))))

(defn- lattice
  "The random value of `seed` at the whole number `i`, in [-1, 1]."
  [seed i]
  (let [h (hash32 (u32 (bit-xor (hash32 (u32 seed)) (u32 i))))]
    (- (* 2 (/ h 4294967295.0)) 1)))

(defn- cell
  "The whole number `i` within the `period` (none: as it is)."
  [i period]
  (if (some? period) (mod i period) i))

(defn value
  "The noise of `seed` at `x`, in [-1, 1]: its random value at each whole
  `x` and a smooth curve between them, repeating every `period` units
  when given (a whole number of them, at least 1)."
  ([seed x]
   (value seed x nil))
  ([seed x period]
   (let [i  (mth/floor x)
         t  (- x i)
         p0 (lattice seed (cell (dec i) period))
         p1 (lattice seed (cell i period))
         p2 (lattice seed (cell (inc i) period))
         p3 (lattice seed (cell (+ i 2) period))
         ;; Catmull–Rom through the four, kept within [-1, 1]
         v  (* 0.5 (+ (* 2 p1)
                      (* t (- p2 p0))
                      (* t t (+ (* 2 p0) (* -5 p1) (* 4 p2) (- p3)))
                      (* t t t (+ (- p0) (* 3 p1) (* -3 p2) p3))))]
     (mth/clamp v -1 1))))

(defn fractal
  "The noise of `seed` at `x` with `octaves` layers (1 to 4), each twice
  as fast and half as strong as the one before, as the octaves of the
  wiggle of After Effects; in [-1, 1]. With a `period`, it repeats every
  `period` units."
  ([seed x octaves]
   (fractal seed x octaves nil))
  ([seed x octaves period]
   (loop [k     0
          sum   0
          total 0
          amp   1
          freq  1]
     (if (< k (max 1 octaves))
       (recur (inc k)
              (+ sum (* amp (value (+ seed (* k 7919)) (* x freq) (some-> period (* freq)))))
              (+ total amp)
              (* amp 0.5)
              (* freq 2))
       (/ sum total)))))
