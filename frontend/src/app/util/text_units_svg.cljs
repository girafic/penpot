;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.util.text-units-svg
  "The units of a text that its split animations pose (`:text-units`,
  see `cta/text-unit-entries`) as the SVG renderer draws them: the
  fragments of the text (`:position-data`, a span on a line each) cut
  where the units start and end, every piece a fragment of its own, the
  pieces of a posed unit with the `:pose` of that unit. Where a piece is
  in its fragment comes from the width of the text before it, measured
  in the font of the fragment and scaled to the width of the fragment,
  as the renderer stretches it (`textLength`)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.types.text-units :as tu]
   [cuerdas.core :as str]))

(defonce ^:private context
  (delay
    (when (exists? js/document)
      (some-> (.createElement js/document "canvas") (.getContext "2d")))))

(defonce ^:private widths
  (atom {}))

(def ^:private max-widths
  "The most measured widths kept."
  4096)

(defn- font
  [{:keys [font-style font-weight font-size font-family]}]
  (dm/str (d/nilv font-style "normal") " " (d/nilv font-weight "400") " "
          (d/parse-double font-size 14) "px \"" font-family "\""))

(defn- transformed
  [text text-transform]
  (case text-transform
    "uppercase"  (str/upper text)
    "lowercase"  (str/lower text)
    "capitalize" (.replace ^js text (js/RegExp. "(^|\\s)(\\S)" "g")
                           (fn [_ space letter] (dm/str space (str/upper letter))))
    text))

(defn- measure
  "The width of `text` in the font of the fragment `data`, or, without a
  canvas to measure it with, its length: every character as wide."
  [data text]
  (if-let [^js ctx @context]
    (let [k [(font data) (:letter-spacing data) (:text-transform data) text]]
      (or (get @widths k)
          (let [_     (set! (.-font ctx) (first k))
                _     (set! (.-letterSpacing ctx) (dm/str (d/parse-double (:letter-spacing data) 0) "px"))
                width (.-width (.measureText ctx (transformed text (:text-transform data))))]
            (swap! widths #(assoc (if (< (count %) max-widths) % {}) k width))
            width)))
    (count text)))

(defn- pose
  "`{:transform :opacity}` of the unit `entry` for a piece whose box goes
  from `x0` to `x1` and from `top` to `bottom`: moved, turned and scaled
  around its centre; and its `:filter` when it is blurred."
  [{:keys [dx dy rotation scale-x scale-y opacity blur]} x0 x1 top bottom]
  (let [cx (/ (+ x0 x1) 2)
        cy (/ (+ top bottom) 2)]
    (cond-> {:transform (dm/str "translate(" (+ cx (d/nilv dx 0)) "," (+ cy (d/nilv dy 0)) ")"
                                " rotate(" (d/nilv rotation 0) ")"
                                " scale(" (d/nilv scale-x 1) "," (d/nilv scale-y 1) ")"
                                " translate(" (- cx) "," (- cy) ")")
             :opacity (d/nilv opacity 1)}
      (and (some? blur) (pos? blur))
      (assoc :filter (dm/str "blur(" blur "px)")))))

(defn- fragment-ranges
  "Where each fragment of `position-data` is in the paragraphs `texts`
  (see `tu/paragraph-texts`): `[paragraph start end]`, UTF-16 offsets,
  found in reading order, or nil for one that is not there."
  [texts position-data]
  (first
   (reduce (fn [[ranges paragraph offset] {:keys [text]}]
             (let [text  (d/nilv text "")
                   found (loop [p paragraph from offset]
                           (when (< p (count texts))
                             (let [i (.indexOf ^js (nth texts p) text from)]
                               (if (>= i 0)
                                 [p i (+ i (count text))]
                                 (recur (inc p) 0)))))]
               (if (some? found)
                 [(conj ranges found) (first found) (nth found 2)]
                 [(conj ranges nil) paragraph offset])))
           [[] 0 0]
           position-data)))

(defn- cut
  "The fragment `data`, at `[paragraph start end]` of its text, cut where
  the units of `entries` in it start and end: its pieces, each a
  fragment with its own text, place and width, the ones of a posed unit
  with its `:pose`."
  [data [paragraph start end] entries]
  (let [inside (filterv #(and (= paragraph (:paragraph %)) (< (:start %) end) (> (:end %) start)) entries)]
    (if (empty? inside)
      [data]
      (let [{:keys [x y width height text direction]} data
            length (count text)
            cuts   (into (sorted-set 0 length)
                         (mapcat (fn [entry]
                                   [(max 0 (- (:start entry) start))
                                    (min length (- (:end entry) start))]))
                         inside)
            full   (measure data text)
            scale  (if (pos? full) (/ width full) 0)
            at     (fn [offset] (* scale (measure data (subs text 0 offset))))
            rtl?   (= "rtl" direction)]
        (into []
              (keep (fn [[a b]]
                      (when (< a b)
                        (let [x0    (if rtl? (- (+ x width) (at b)) (+ x (at a)))
                              x1    (if rtl? (- (+ x width) (at a)) (+ x (at b)))
                              entry (d/seek #(and (<= (- (:start %) start) a) (< a (- (:end %) start))) inside)]
                          (cond-> (assoc data :text (subs text a b) :x x0 :width (- x1 x0))
                            (some? entry)
                            (assoc :pose (pose entry x0 x1 (- y height) y)))))))
              (partition 2 1 cuts))))))

(defn- line-fragments
  "`position-data` with the fragments of each posed line (see
  `tu/lines`) given the pose of its line unit, around the centre of the
  line. A line at or past the `:line-count` of the entries takes the
  pose of the last one, as render-wasm does."
  [position-data entries]
  (let [line-count (reduce max 0 (keep :line-count entries))
        by-index   (into {} (map (juxt :index identity)) entries)]
    (reduce (fn [result [index {:keys [fragments span]}]]
              (if-let [entry (get by-index (min index (dec line-count)))]
                (let [x0   (reduce min (map #(:x (nth position-data %)) fragments))
                      x1   (reduce max (map #(let [f (nth position-data %)] (+ (:x f) (:width f))) fragments))
                      pose (pose entry x0 x1 (first span) (second span))]
                  (reduce #(assoc-in %1 [%2 :pose] pose) result fragments))
                result))
            (vec position-data)
            (map-indexed vector (tu/lines position-data)))))

(defn posed-fragments
  "The fragments of the text `shape` to draw with its units posed (see
  the namespace), or nil when its `:text-units` pose none."
  [{:keys [text-units position-data content]}]
  (when (and (seq text-units) (seq position-data))
    (if (= :line (:kind (first text-units)))
      (line-fragments position-data text-units)
      (into []
            cat
            (map (fn [data range]
                   (if (some? range) (cut data range text-units) [data]))
                 position-data
                 (fragment-ranges (tu/paragraph-texts content) position-data))))))
