;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.geom-offset-lines-test
  (:require
   [app.common.geom.offset-lines :as gol]
   [app.common.math :as mth]
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
