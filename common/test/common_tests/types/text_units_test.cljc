;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.text-units-test
  (:require
   [app.common.math :as mth]
   [app.common.types.text-units :as tu]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- content
  "Text content of `paragraphs`, each a string (one leaf) or a vector of
  leaf texts."
  [& paragraphs]
  {:type "root"
   :children [{:type "paragraph-set"
               :children (mapv (fn [leaves]
                                 {:type "paragraph"
                                  :children (mapv (fn [text] {:text text})
                                                  (if (string? leaves) [leaves] leaves))})
                               paragraphs)}]})

(defn- chars-of
  [& paragraphs]
  (tu/char-units (tu/paragraph-texts (apply content paragraphs))))

(defn- words-of
  [& paragraphs]
  (tu/word-units (tu/paragraph-texts (apply content paragraphs))))

(t/deftest characters-and-words
  (t/is (= [[0 0 1] [0 1 2] [0 3 4] [0 4 5] [0 5 6]] (chars-of "Hi you")))
  (t/is (= [[0 0 2] [0 3 6]] (words-of "Hi you"))))

(t/deftest punctuation-goes-with-a-word
  (t/is (= [[0 0 3] [0 4 8]] (words-of "Hi, you!")) "after a word")
  (t/is (= [[0 0 4]] (words-of "(hi)")) "before the first word")
  (t/is (= [[0 0 2] [0 3 8]] (words-of "Hi - you")) "alone, to the next word")
  (t/is (= [[0 0 3]] (words-of "!!!")) "without a word, a unit of its own"))

(t/deftest a-character-is-a-grapheme-cluster
  (let [units (chars-of "👍🏽 ok")]
    (t/is (= 3 (count units)))
    (t/is (= [0 0 4] (first units)) "a skin tone emoji is 4 UTF-16 units"))
  (t/is (= [[0 0 8] [0 9 10]] (chars-of "👨‍👩‍👧 a")) "a family joined by ZWJ is one")
  (t/is (= [[0 0 2] [0 2 3]] (chars-of (str "e" \u0301 "a"))) "a letter with a combining accent is one"))

(t/deftest units-count-by-paragraph
  (t/is (= [[0 0 1] [0 1 2] [1 0 1] [1 1 2]] (chars-of "ab" "cd")))
  (t/is (= [[0 0 1] [0 2 3] [1 0 1]] (chars-of ["a\nb"] "c"))
        "a line break in a leaf does not start a paragraph")
  (t/is (= [[0 0 1] [0 1 2]] (chars-of ["a" "b"])) "leaves join")
  (t/is (= [] (chars-of "  ")) "whitespace is no unit"))

#?(:cljs
   (t/deftest a-cjk-paragraph-has-several-words
     (t/is (< 1 (count (words-of "我喜欢猫"))))))

(defn- fragment
  [y height text]
  {:x 0 :y y :width 10 :height height :text text :fills []})

(t/deftest lines-from-the-position-data
  (t/is (= 1 (tu/line-count [(fragment 20 20 "Big") (fragment 18 12 "small")]))
        "fragments of one line with different font sizes")
  (t/is (= 2 (tu/line-count [(fragment 20 20 "one") (fragment 40 20 "two")])))
  (t/is (= 2 (tu/line-count [(fragment 20 20 "one") (fragment 40 20 "  ") (fragment 60 20 "three")]))
        "a blank line is no line")
  (t/is (nil? (tu/line-count nil)))
  (t/is (nil? (tu/line-count []))))

(t/deftest lines-hold-their-fragments
  (t/is (= [{:fragments [1 0] :span [0 20]} {:fragments [3] :span [40 60]}]
           (tu/lines [(fragment 20 20 "Big") (fragment 18 10 "small")
                      (fragment 40 20 " ") (fragment 60 20 "two")]))
        "by their place, from the top, the blank one left out"))

(t/deftest units-of-a-shape
  (let [shape {:id (uuid/next)
               :content (content "Hi you")
               :position-data [(fragment 20 20 "Hi you")]}]
    (t/is (= [[0 0 2] [0 3 6]] (tu/units shape :word)))
    (t/is (= 5 (tu/unit-count shape :char)))
    (t/is (= 1 (tu/unit-count shape :line)))
    (t/is (= 1 (tu/unit-count (dissoc shape :position-data) :line))
          "without position data, the lines are one")
    (t/is (= 2 (tu/unit-count (assoc shape :content (content "a b")) :word)))))

(t/deftest one-cache-entry-per-shape
  (let [cache @#'tu/cache
        id    (uuid/next)]
    (doseq [text ["a" "a b" "a b c"]]
      (tu/units {:id id :content (content text)} :word)
      (tu/units {:id id :content (content text)} :char))
    (t/is (= 3 (tu/unit-count {:id id :content (content "a b c")} :word)))
    (t/is (= (content "a b c") (:content (get @cache id))) "the entry of the last content")
    (t/is (= #{:word :char} (set (keys (:units (get @cache id))))))))

(defn- spans
  [animations]
  (mapv (juxt :start :duration) animations))

(def ^:private fade
  {:id (uuid/next) :type :fade :direction :in :start 100 :duration 1000
   :split :word :spread 0.5 :order :forward})

(t/deftest every-unit-plays-inside-the-span
  (doseq [spread [0 0.5 1]
          order  tu/orders
          n      [1 2 5 13]]
    (let [units (tu/unit-animations (assoc fade :spread spread :order order :seed 7) n)]
      (t/is (= n (count units)))
      (t/is (every? #(and (<= 100 (:start %))
                          (<= (+ (:start %) (:duration %)) 1100)
                          (pos? (:duration %)))
                    units)
            (str spread " " order " " n)))))

(t/deftest the-spread-sets-how-far-apart-units-start
  (t/is (= [[100 250] [350 250] [600 250] [850 250]]
           (spans (tu/unit-animations (assoc fade :spread 1) 4)))
        "back to back")
  (t/is (= [[100 1000] [100 1000] [100 1000]]
           (spans (tu/unit-animations (assoc fade :spread 0) 3)))
        "all together")
  (t/is (= [(dissoc fade :split :spread :order :seed)]
           (tu/unit-animations fade 1))
        "one unit is the animation itself")
  (t/is (= [] (tu/unit-animations fade 0))))

(t/deftest orders
  (let [starts #(mapv :start (tu/unit-animations (assoc fade :spread 1 :order %1) %2))]
    (t/is (= [100 300 500 700 900] (starts :forward 5)))
    (t/is (= [900 700 500 300 100] (starts :backward 5)))
    (t/is (= [900 500 100 500 900] (starts :center 5)) "the middle first")
    (t/is (= [100 500 900 500 100] (starts :edges 5)) "the ends first")
    (t/is (= [100 100] (starts :center 2)))))

(t/deftest random-order-follows-the-seed
  ;; the same on the JVM and in JS
  (t/is (= [2 1 3 6 0 7 4 5] (mapv #(mth/round (* 7 %)) (tu/unit-positions :random 8 42))))
  (t/is (= (tu/unit-positions :random 8 42) (tu/unit-positions :random 8 42)))
  (t/is (not= (tu/unit-positions :random 8 42) (tu/unit-positions :random 8 43)))
  (t/is (= (range 8) (sort (mapv #(mth/round (* 7 %)) (tu/unit-positions :random 8 1))))
        "a shuffle: every slot once")
  (t/is (= (tu/unit-animations (dissoc (assoc fade :order :random) :seed) 6)
           (tu/unit-animations (assoc fade :order :random :seed (tu/id-seed (:id fade))) 6))
        "without a seed, the one of its id"))

(t/deftest the-stagger-of-the-units
  (t/is (= 250 (tu/unit-stagger 1000 1 4)) "back to back")
  (t/is (= 0 (tu/unit-stagger 1000 0 4)) "all together")
  (t/is (= 0 (tu/unit-stagger 1000 1 1)))
  (t/is (mth/close? 125 (tu/unit-stagger 1000 0.5 4))))
