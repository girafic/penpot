;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.text-units-svg-test
  (:require
   ["react-dom/server" :as rds]
   [app.common.types.shape :as cts]
   [app.main.ui.shapes.text.svg-text :as svg-text]
   [app.util.text-units-svg :as tus]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]
   [rumext.v2 :as mf]))

;; Without a canvas to measure the text with (as in these tests), every
;; character is as wide: 10 px in a fragment 120 px wide of 12 of them.

(defn- fragment
  [x y text & {:as attrs}]
  (merge {:x x :y y :width (* 10 (count text)) :height 20 :text text
          :font-family "sourcesanspro" :font-size "16" :font-weight "400" :font-style "normal"
          :fills [{:fill-color "#000000" :fill-opacity 1}]}
         attrs))

(defn- text-shape
  "A text of the paragraphs `texts` drawn as the fragments `fragments`,
  posing `units`."
  [texts fragments units]
  (-> (cts/setup-shape {:type :text :x 10 :y 20 :width 300 :height 60})
      (assoc :content {:type "root"
                       :children [{:type "paragraph-set"
                                   :children (mapv (fn [text] {:type "paragraph" :children [{:text text}]})
                                                   texts)}]}
             :position-data fragments)
      (cond-> (some? units) (assoc :text-units units))))

(def ^:private moved-you
  {:kind :word :paragraph 0 :start 3 :end 6
   :dx 0 :dy 12 :rotation 0 :scale-x 1 :scale-y 1 :opacity 0.5})

(defn- markup
  [shape]
  (rds/renderToStaticMarkup (mf/element svg-text/text-shape #js {:shape shape})))

(defn- count-of
  "How many times `part` is in `text`."
  [text part]
  (loop [from 0 n 0]
    (if-let [i (str/index-of text part from)]
      (recur (+ i (count part)) (inc n))
      n)))

(t/deftest a-posed-word-is-a-piece-of-its-own
  (let [pieces (tus/posed-fragments (text-shape ["Hi you there"] [(fragment 10 40 "Hi you there")] [moved-you]))]
    (t/is (= [["Hi " 10 30] ["you" 40 30] [" there" 70 60]] (mapv (juxt :text :x :width) pieces)))
    (t/is (= [nil {:transform "translate(55,42) rotate(0) scale(1,1) translate(-55,-30)" :opacity 0.5} nil]
             (mapv :pose pieces))
          "around the centre of its box")))

(t/deftest a-posed-word-draws-in-a-group-of-its-own
  (let [svg (markup (text-shape ["Hi you there"] [(fragment 10 40 "Hi you there")] [moved-you]))
        at  (str/index-of svg "<g transform=\"translate(55,42) rotate(0) scale(1,1) translate(-55,-30)\" opacity=\"0.5\">")]
    (t/is (some? at))
    (t/is (= 1 (count-of svg "<g transform=\"translate(")))
    (t/is (str/starts-with? (subs svg (str/index-of svg ">" (str/index-of svg "<text " at))) ">you</text>")
          "around the text of the word")
    (t/is (= 3 (count-of svg "<text ")) "three pieces")))

(t/deftest a-text-at-rest-draws-as-it-did
  (let [shape    (text-shape ["Hi you there"] [(fragment 10 40 "Hi you there")] nil)
        as-it-is (markup shape)]
    (t/is (= as-it-is (markup (assoc shape :text-units []))))
    (t/is (= 1 (count-of as-it-is "<text ")))
    (t/is (nil? (tus/posed-fragments (assoc shape :text-units []))))))

(t/deftest a-word-of-a-later-fragment-or-paragraph
  (let [fragments [(fragment 10 40 "Hi ") (fragment 40 40 "you there") (fragment 10 70 "and on")]
        units     [(assoc moved-you :start 7 :end 12) (assoc moved-you :paragraph 1 :start 4 :end 6)]
        pieces    (tus/posed-fragments (text-shape ["Hi you there" "and on"] fragments units))]
    (t/is (= [["Hi " false] ["you " false] ["there" true] ["and " false] ["on" true]]
             (mapv (juxt :text (comp some? :pose)) pieces)))
    (t/is (= [10 40 80 10 50] (mapv :x pieces)))))

(t/deftest right-to-left-pieces-go-from-the-right
  (let [pieces (tus/posed-fragments (text-shape ["Hi you there"]
                                                [(fragment 10 40 "Hi you there" :direction "rtl")]
                                                [moved-you]))]
    ;; "Hi " is the right end of the line, "you" left of it
    (t/is (= [["Hi " 100 30] ["you" 70 30] [" there" 10 60]] (mapv (juxt :text :x :width) pieces)))))

(t/deftest the-fragments-of-a-line-share-its-pose
  (let [fragments [(fragment 10 40 "Hi ") (fragment 40 40 "you") (fragment 10 70 "there")]
        units     [{:kind :line :index 0 :line-count 2 :dy 5 :opacity 0.2}]
        pieces    (tus/posed-fragments (text-shape ["Hi you" "there"] fragments units))]
    (t/is (= 3 (count pieces)))
    (t/is (= (:pose (first pieces)) (:pose (second pieces))))
    (t/is (= "translate(40,35) rotate(0) scale(1,1) translate(-40,-30)" (:transform (:pose (first pieces))))
          "around the centre of the line")
    (t/is (nil? (:pose (nth pieces 2))))))

(t/deftest a-blurred-unit-draws-blurred
  (let [fragments [(fragment 10 40 "Hi you there")]
        blurred   (assoc moved-you :blur 4)
        pieces    (tus/posed-fragments (text-shape ["Hi you there"] fragments [blurred]))
        sharp     (tus/posed-fragments (text-shape ["Hi you there"] fragments [moved-you]))
        svg       (markup (text-shape ["Hi you there"] fragments [blurred]))]
    (t/is (= "blur(4px)" (:filter (:pose (second pieces)))))
    (t/is (nil? (:filter (:pose (second sharp)))) "none without a blur")
    (t/is (str/includes? svg "filter:blur(4px)"))))
