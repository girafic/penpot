;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.text-units
  "The units of a text layer that a split animation of Penpot Motion
  plays one after the other: its characters, words or lines. They are
  derived from the text when it plays and never stored, so the text stays
  editable: typing just gives more units. Characters and words are UTF-16
  ranges of the text of a paragraph, as render-wasm reads them; lines are
  counted from the position data. And when each unit plays in the span
  of its animation (see `unit-animations`)."
  (:require
   [app.common.math :as mth])
  #?(:clj
     (:import
      java.text.BreakIterator)))

(def split-kinds
  "What a split animation plays one by one."
  #{:char :word :line})

(def orders
  "The order the units of a split animation start in."
  #{:forward :backward :center :edges :random})

(def default-spread
  "How far apart the units of a split animation start by default, as a
  share of its duration (see `unit-animations`)."
  0.5)

(def live-unit-cap
  "The most units a text plays one by one. A text with more plays its
  split animations as whole-layer animations, so a very long text never
  builds thousands of unit poses a frame. Measured on 2026-10-01 (a dev
  build, WASM): a typewriter of 1000 characters works out its units in
  0.16 ms a frame (`text-unit-entries`, which works out only the units on
  their way) and plays at 60 fps; at a spread of 0.5, about 500 units on
  their way, it takes 2.3 ms."
  1000)

;; TEXT

(defn paragraph-texts
  "The text of each paragraph of `content`, as render-wasm lays them out:
  the paragraphs of its first paragraph set, each one the text of its
  leaves joined. A line break inside a leaf does not start a paragraph."
  [content]
  (into []
        (map (fn [paragraph]
               (apply str (map :text (:children paragraph)))))
        (:children (first (:children content)))))

(defn- blank?
  [text]
  (some? (re-matches #?(:clj #"(?U)\s*" :cljs #"\s*") text)))

#?(:clj
   (defn- word-like?
     "Whether `text` holds a letter or a digit (in JS the word segmenter
     says it)."
     [text]
     (some? (re-find #"[\p{L}\p{N}]" text))))

#?(:cljs
   (def ^:private segmenters
     (delay {:grapheme (js/Intl.Segmenter. js/undefined #js {:granularity "grapheme"})
             :word     (js/Intl.Segmenter. js/undefined #js {:granularity "word"})})))

(defn- segments
  "The segments of `text` by `granularity` (`:grapheme` or `:word`), as
  `[start end word-like?]`, UTF-16 offsets."
  [text granularity]
  #?(:cljs
     (let [segmenter (get @segmenters granularity)]
       (into []
             (map (fn [^js segment]
                    (let [start (.-index segment)
                          text  (.-segment segment)]
                      [start (+ start (.-length text))
                       (if (= granularity :word) (true? (.-isWordLike segment)) false)])))
             (js/Array.from (.segment segmenter text))))
     :clj
     (let [iterator (if (= granularity :word)
                      (BreakIterator/getWordInstance)
                      (BreakIterator/getCharacterInstance))]
       (.setText iterator ^String text)
       (loop [start  (.first iterator)
              end    (.next iterator)
              result []]
         (if (= end BreakIterator/DONE)
           result
           (recur end
                  (.next iterator)
                  (conj result [start end (and (= granularity :word)
                                               (word-like? (subs text start end)))])))))))

(defn- paragraph-chars
  "The character units of `text`, the paragraph `paragraph`: its grapheme
  clusters that are not whitespace."
  [paragraph text]
  (into []
        (keep (fn [[start end]]
                (when-not (blank? (subs text start end))
                  [paragraph start end])))
        (segments text :grapheme)))

(defn- paragraph-words
  "The word units of `text`, the paragraph `paragraph`: its word-like
  segments, each with the punctuation right after it, and the punctuation
  that has no word right before it given to the next word (or, at the
  end, to the last one). A paragraph of punctuation alone is one unit."
  [paragraph text]
  (let [{:keys [words pending]}
        (reduce (fn [{:keys [words pending adjacent?] :as acc} [start end word?]]
                  (cond
                    word?
                    {:words (conj words [(or (first pending) start) end])
                     :pending nil
                     :adjacent? true}

                    (blank? (subs text start end))
                    (assoc acc :adjacent? false)

                    (and adjacent? (seq words))
                    (assoc acc :words (assoc-in words [(dec (count words)) 1] end))

                    :else
                    (assoc acc :pending [(or (first pending) start) end])))
                {:words [] :pending nil :adjacent? false}
                (segments text :word))
        words (cond
                (nil? pending)  words
                (seq words)     (assoc-in words [(dec (count words)) 1] (second pending))
                :else           [pending])]
    (mapv (fn [[start end]] [paragraph start end]) words)))

(defn char-units
  "The character units of the paragraphs `texts` (see `paragraph-texts`):
  `[paragraph start end]` each, UTF-16 offsets, in reading order."
  [texts]
  (into [] (comp (map-indexed paragraph-chars) cat) texts))

(defn word-units
  "The word units of the paragraphs `texts` (see `paragraph-texts`):
  `[paragraph start end]` each, UTF-16 offsets, in reading order."
  [texts]
  (into [] (comp (map-indexed paragraph-words) cat) texts))

(defn- same-line?
  "Whether two vertical spans, `[top bottom]`, are on the same line: they
  overlap by more than half the smaller one."
  [[top-a bottom-a] [top-b bottom-b]]
  (> (- (min bottom-a bottom-b) (max top-a top-b))
     (* 0.5 (min (- bottom-a top-a) (- bottom-b top-b)))))

(defn lines
  "The lines `position-data` (of a text layer) shows, from the top: its
  fragments grouped by vertical overlap, the groups without visible text
  left out, as render-wasm groups them. Each one `{:fragments [index
  ...] :span [top bottom]}`, the indices of its fragments in
  `position-data`."
  [position-data]
  (->> (sort-by #(:y (nth position-data %)) (range (count position-data)))
       (reduce (fn [lines index]
                 (let [{:keys [y height text]} (nth position-data index)
                       span [(- y height) y]
                       line (peek lines)]
                   (if (and (some? line) (same-line? (:span line) span))
                     (conj (pop lines) (-> line
                                           (update :span (fn [[top bottom]] [(min top (first span)) (max bottom y)]))
                                           (update :text str text)
                                           (update :fragments conj index)))
                     (conj lines {:span span :text (str text) :fragments [index]}))))
               [])
       (into [] (comp (remove (comp blank? :text))
                      (map #(dissoc % :text))))))

(defn line-count
  "How many lines `position-data` (of a text layer) shows (see `lines`),
  as render-wasm counts them. Nil without position data."
  [position-data]
  (when (seq position-data)
    (count (lines position-data))))

;; UNITS OF A SHAPE

(defonce ^:private cache
  (atom {}))

(defn- shape-units
  [kind {:keys [content position-data]}]
  (case kind
    :char (char-units (paragraph-texts content))
    :word (word-units (paragraph-texts content))
    :line (line-count position-data)))

(defn units
  "The units of `kind` of the text `shape`: for `:char` and `:word`
  `[paragraph start end]` each (see `char-units`), for `:line` how many
  lines it has (nil without position data, see `line-count`). Kept, one
  entry per shape, until its content or position data change."
  [{:keys [id content position-data] :as shape} kind]
  (let [entry (get @cache id)
        fresh (fn [entry]
                (and (some? entry)
                     (= content (:content entry))
                     (= position-data (:position-data entry))))]
    (if (and (fresh entry) (contains? (:units entry) kind))
      (get-in entry [:units kind])
      (let [value (shape-units kind shape)]
        (swap! cache update id
               (fn [entry]
                 (if (fresh entry)
                   (assoc-in entry [:units kind] value)
                   {:content content :position-data position-data :units {kind value}})))
        value))))

(defn unit-count
  "How many units of `kind` the text `shape` plays: a text without
  position data plays its lines as one."
  [shape kind]
  (let [units (units shape kind)]
    (if (= kind :line)
      (or units 1)
      (count units))))

;; TIMING

(defn- next-random
  "The next number of a Park-Miller generator, the same on every runtime."
  [x]
  (mod (* x 48271) 2147483647))

(defn- shuffled-slots
  "The slots 0..n-1 shuffled by `seed` (Fisher-Yates), the same on every
  runtime."
  [n seed]
  (loop [i    (dec n)
         x    (inc (mod (if (neg? seed) (- seed) seed) 2147483646))
         slot (vec (range n))]
    (if (pos? i)
      (let [x (next-random x)
            j (mod x (inc i))]
        (recur (dec i) x (assoc slot i (nth slot j) j (nth slot i))))
      slot)))

(defn id-seed
  "A seed taken from the id of an animation, for the random order of one
  that has none (written before seeds existed, or by another client)."
  [id]
  (let [text (str id)]
    (loop [i 0 h 7]
      (if (< i (count text))
        (recur (inc i) (mod (+ (* h 31) #?(:clj (int (.charAt ^String text i)) :cljs (.charCodeAt text i)))
                            2147483647))
        h))))

(defn- spread-out
  "`distances` (one a unit) as positions in 0..1, the nearest at 0."
  [distances]
  (let [low  (reduce min distances)
        high (reduce max distances)]
    (if (> high low)
      (mapv #(/ (- % low) (- high low)) distances)
      (mapv (constantly 0.0) distances))))

(defn unit-positions
  "Where each of `n` units starts within the span of its animation, as
  0..1, in unit order: `:forward` from the first, `:backward` from the
  last, `:center` from the middle, `:edges` from both ends, `:random` by a
  shuffle of `seed`."
  [order n seed]
  (if (<= n 1)
    (vec (repeat n 0.0))
    (let [last   (double (dec n))
          middle (/ last 2)
          center (mapv #(mth/abs (- % middle)) (range n))]
      (case order
        :backward (mapv #(/ (- last %) last) (range n))
        :center   (spread-out center)
        :edges    (mapv #(- 1 %) (spread-out center))
        :random   (let [slots (shuffled-slots n (or seed 0))]
                    (mapv #(/ (nth slots %) last) (range n)))
        (mapv #(/ % last) (range n))))))

(defn unit-stagger
  "How many ms apart two units next to each other start in reading order,
  for `n` units of a split animation lasting `duration` with `spread`
  (see `unit-animations`): what the spread comes to for a text."
  [duration spread n]
  (if (> n 1)
    (let [spread (mth/clamp (or spread default-spread) 0 1)
          unit   (+ (* duration (- 1 spread)) (/ (* spread duration) n))]
      (/ (- duration unit) (dec n)))
    0))

(defn unit-animations
  "The `n` plain animations the split `animation` plays, one per unit, in
  unit order, all within its span: each unit lasts `d = D(1 - s) + sD/n`
  and starts at `S + p(D - d)`, `S` and `D` being the start and the
  duration of `animation`, `s` its spread and `p` the position of the
  unit (see `unit-positions`). So a spread of 0 plays every unit
  together over the whole span and a spread of 1 one after the other.
  Times are whole milliseconds; no unit ends after the span. The
  animations have no split keys."
  [{:keys [id start duration spread order seed] :as animation} n]
  (let [base      (dissoc animation :split :spread :order :seed)
        spread    (mth/clamp (or spread default-spread) 0 1)
        unit      (+ (* duration (- 1 spread)) (/ (* spread duration) (max n 1)))
        end       (+ start duration)
        positions (unit-positions (or order :forward) n (or seed (id-seed id)))]
    (mapv (fn [position]
            (let [unit-start (min (dec end) (+ start (mth/round (* position (- duration unit)))))]
              (assoc base
                     :start unit-start
                     :duration (max 1 (min (mth/round unit) (- end unit-start))))))
          positions)))
