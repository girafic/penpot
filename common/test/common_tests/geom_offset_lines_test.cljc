;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.geom-offset-lines-test
  (:require
   [app.common.geom.offset-lines :as gol]
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [clojure.test :as t]))

(defn- box
  "The box of the points of `content`: `[x y width height]`, rounded."
  [content]
  (let [{:keys [x y width height]} (path/calc-selrect content)]
    (mapv #(mth/round %) [x y width height])))

(t/deftest offset-lines-run-parallel-to-the-outline
  (let [rect  (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 50})
        lines (gol/lines rect {:count 3 :spacing 10})]
    (t/is (= [-10 0 10] (mapv :offset lines)) "centred on the outline")
    (t/is (= [[10 10 80 30] [0 0 100 50] [-10 -10 120 70]] (mapv (comp box :content) lines))
          "inside, on it and outside it")
    (t/is (= ["#5a667e" "#adb3bf" "#ffffff"] (mapv :color lines)) "from the inner colour to the outer one")
    (t/is (mth/close? 0.65 (:opacity (second lines)) 1e-9)))
  (t/testing "an open path, along it"
    (let [line  (cts/setup-shape {:type :path :content (path/from-string "M 0 0 L 100 0")})
          lines (gol/lines line {:count 2 :spacing 8})]
      (t/is (= [[0 -4 100 0] [0 4 100 0]] (mapv (comp box :content) lines))))))

(t/deftest a-group-of-offset-lines-knows-its-layers-by-their-roles
  (let [objects {:group {:id :group :shapes [:src :l1 :l2 :mine] :offset-lines {:count 2}}
                 :src   {:id :src :offset-line :source}
                 :l1    {:id :l1 :offset-line :line}
                 :l2    {:id :l2 :offset-line :line}
                 :mine  {:id :mine}}]
    (t/is (= {:source :src :lines [:l1 :l2]} (gol/layers-of objects (:group objects)))
          "what else it holds is none of them")
    (t/is (nil? (gol/layers-of (dissoc objects :src) (update (:group objects) :shapes #(remove #{:src} %))))
          "none without a source"))
  (t/testing "a group made before the roles, by the id of its source"
    (let [objects {:group {:id :group :shapes [:src :l1] :offset-lines {:source :src :count 1}}
                   :src   {:id :src}
                   :l1    {:id :l1}}]
      (t/is (= {:source :src :lines [:l1]} (gol/layers-of objects (:group objects))))
      (t/is (nil? (gol/layers-of objects (assoc-in (:group objects) [:offset-lines :source] :gone)))
            "a source that is not in it"))))

(defn- subpath-boxes
  "The boxes of the subpaths of `content`, `[x y width height]`, rounded."
  [content]
  (mapv (fn [{:keys [points]}]
          (let [xs (map :x points) ys (map :y points)
                x  (apply min xs) y (apply min ys)]
            (mapv #(mth/round %) [x y (- (apply max xs) x) (- (apply max ys) y)])))
        (gtl/outline-polylines {:type :path :content content})))

(t/deftest the-rings-past-the-middle-of-a-tight-curve-are-left-out
  (let [circle (cts/setup-shape {:type :circle :x 0 :y 0 :width 40 :height 40})
        lines  (gol/lines circle {})]
    (t/is (= 17 (count lines)) "the 7 that would go past its middle")
    (t/is (every? #(<= (nth (box (:content %)) 2) 40) (filter #(neg? (:offset %)) lines))
          "none inside it wider than it")))

(t/deftest each-subpath-takes-its-own-side
  (t/testing "two squares going round opposite ways"
    (let [squares (cts/setup-shape {:type :path :content (path/from-string "M0 0 L100 0 L100 100 L0 100 Z M200 0 L200 100 L300 100 L300 0 Z")})
          inner   (:content (first (gol/lines squares {:count 3 :spacing 10})))]
      (t/is (= [[10 10 80 80] [210 10 80 80]] (subpath-boxes inner)) "inside both")))
  (t/testing "a hole, either way round: its inner side is in the shape"
    (doseq [hole ["M30 30 L30 70 L70 70 L70 30 Z" "M30 30 L70 30 L70 70 L30 70 Z"]]
      (let [shape (cts/setup-shape {:type :path :content (path/from-string (str "M0 0 L100 0 L100 100 L0 100 Z " hole))})
            inner (:content (first (gol/lines shape {:count 3 :spacing 10})))]
        (t/is (= [[10 10 80 80] [20 20 60 60]] (subpath-boxes inner)) hole)))))

(t/deftest the-lines-of-a-closed-outline-are-closed
  (let [rect  (cts/setup-shape {:type :rect :x 0 :y 0 :width 100 :height 50})
        line  (first (gol/lines rect {:count 1}))]
    (t/is (= :close-path (:command (last (seq (:content line))))))))

(t/deftest offset-line-colours-are-six-hex-digits
  (let [valid? #(sm/validate cts/schema:offset-lines (assoc gol/defaults :inner-color %))]
    (t/is (valid? "#A1B2C3"))
    (t/is (not (valid? "#abc")) "three digits, which the mix misreads")
    (t/is (not (valid? "red")))))

(defn- crossings
  "How many times the polylines of the outline of `content` cross
  themselves: pairs of segments not next to each other that meet."
  [content]
  (let [cross? (fn [a b c d]
                 (let [side #(- (* (- (:x %2) (:x %1)) (- (:y %3) (:y %1)))
                                (* (- (:y %2) (:y %1)) (- (:x %3) (:x %1))))]
                   (and (neg? (* (side a b c) (side a b d)))
                        (neg? (* (side c d a) (side c d b))))))]
    (reduce + (for [{:keys [points closed?]} (gtl/outline-polylines {:type :path :content content})
                    :let [pts  (vec points)
                          segs (vec (partition 2 1 (cond-> pts closed? (conj (first pts)))))
                          n    (count segs)]
                    i (range n)
                    j (range (+ i 2) n)
                    :when (not (and closed? (= i 0) (= j (dec n))))
                    :let [[a b] (nth segs i) [c d] (nth segs j)]
                    :when (cross? a b c d)]
                1))))

(defn- nearest
  "How far the point `p` is from the polyline `pts` (closed when `closed?`)."
  [p pts closed?]
  (let [pts (vec pts)]
    (apply min (for [[a b] (partition 2 1 (cond-> pts closed? (conj (first pts))))]
                 (let [dx (- (:x b) (:x a)) dy (- (:y b) (:y a))
                       l2 (+ (* dx dx) (* dy dy))
                       t  (if (zero? l2) 0 (mth/clamp (/ (+ (* (- (:x p) (:x a)) dx) (* (- (:y p) (:y a)) dy)) l2) 0 1))]
                   (mth/hypot (- (:x p) (+ (:x a) (* t dx))) (- (:y p) (+ (:y a) (* t dy)))))))))

(defn- closest
  "How near the points of the line `line` come to the outline of
  `shape`, against its offset: 1 when none comes nearer than it."
  [shape line]
  (let [{:keys [points closed?]} (first (gtl/outline-polylines shape))]
    (/ (apply min (for [{pts :points} (gtl/outline-polylines {:type :path :content (:content line)})
                        p pts]
                    (nearest p points closed?)))
       (mth/abs (:offset line)))))

(t/deftest the-lines-of-a-concave-outline-keep-their-distance
  ;; a notch 20 px wide: the lines outside it, further than 10 px, would
  ;; run into it and back, closer to the outline than they are apart
  (let [notched (cts/setup-shape {:type :path :content (path/from-string "M0 0 L90 0 L90 40 L110 40 L110 0 L200 0 L200 100 L0 100 Z")})
        lines   (gol/lines notched {:count 12 :spacing 4})]
    (t/is (= 12 (count lines)))
    (t/is (every? #(> % 0.9) (map #(closest notched %) lines))
          (str "nearest by line: " (mapv #(mth/precision (closest notched %) 2) lines)))
    (t/is (every? zero? (map (comp crossings :content) lines)) "no loops"))
  (t/testing "an open path bent tighter than its lines are apart"
    (let [bent  (cts/setup-shape {:type :path :content (path/from-string "M0 0 L100 0 L100 10 L0 10")})
          lines (gol/lines bent {:count 6 :spacing 6})]
      (t/is (every? #(> % 0.9) (map #(closest bent %) lines))
            (str "nearest by line: " (mapv #(mth/precision (closest bent %) 2) lines)))))
  (t/testing "the end of an open path on its own lines, as the bowl of a P on its stem, cuts none"
    (let [p     (cts/setup-shape {:type :path :content (path/from-string "M0 200 L0 0 L100 0 L100 100 L20 100")})
          lines (gol/lines p {:count 2 :spacing 40})]
      (t/is (= [1 1] (mapv #(count (gtl/outline-polylines {:type :path :content (:content %)})) lines)))))
  (t/testing "two notches the lines bridge: one ring over both"
    (let [h     (cts/setup-shape {:type :path :content (path/from-string "M0 0 L80 0 L80 80 L120 80 L120 0 L200 0 L200 200 L120 200 L120 120 L80 120 L80 200 L0 200 Z")})
          outer (last (gol/lines h {:count 12 :spacing 4}))
          rings (gtl/outline-polylines {:type :path :content (:content outer)})]
      (t/is (= 22 (:offset outer)))
      (t/is (= 1 (count rings)))
      (t/is (> (closest h outer) 0.85) "nor through the middle")))
  (t/testing "a waist narrower than the lines go in: a ring on each side"
    (let [waisted (cts/setup-shape {:type :path :content (path/from-string "M0 0 L80 0 L80 40 L120 40 L120 0 L200 0 L200 100 L120 100 L120 60 L80 60 L80 100 L0 100 Z")})
          inner   (last (filter #(neg? (:offset %)) (gol/lines waisted {:count 2 :spacing 24})))]
      (t/is (= -12 (:offset inner)))
      (t/is (= 2 (count (gtl/outline-polylines {:type :path :content (:content inner)})))
            "the waist, 20 px, closes before 12 px in from both sides"))))

