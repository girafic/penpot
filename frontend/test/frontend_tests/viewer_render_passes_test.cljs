;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-render-passes-test
  "The render passes of a board in the view mode: what scrolls, each
  layer that sticks at the top of the screen and the fixed layers, each
  on a canvas of its own, and which of them a render draws (Penpot
  Motion)."
  (:require
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.render-viewer-wasm :as rwv]
   [app.main.ui.viewer.viewport-common :as vpc]
   [app.main.ui.viewer.viewport-wasm :as vpw]
   [cljs.test :as t :include-macros true]))

(def ^:private board-id (uuid/custom 12 1))
(def ^:private tabs (uuid/custom 12 2))
(def ^:private heading (uuid/custom 12 3))
(def ^:private label (uuid/custom 12 4))
(def ^:private row (uuid/custom 12 5))
(def ^:private chips (uuid/custom 12 6))
(def ^:private chip (uuid/custom 12 7))

(defn- shape
  [id type parent-id x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type type :x x :y y :width w :height h})
      (merge {:id id :parent-id parent-id :frame-id parent-id} attrs)))

(def ^:private objects
  (->> [(shape board-id :frame uuid/zero 0 0 402 1814 :frame-id uuid/zero
               :shapes [heading row chips tabs])
        (shape heading :frame board-id 0 100 402 40 :sticky-scroll true :shapes [label])
        (shape label :rect heading 24 110 100 20)
        (shape row :rect board-id 0 200 402 600)
        (shape chips :frame board-id 0 820 402 40 :scroll-overflow :horizontal :shapes [chip])
        (shape chip :rect chips 0 820 800 40)
        (shape tabs :rect board-id 0 1730 402 84 :fixed-scroll true)]
       (into {} (map (juxt :id identity)))))

(defn- passes
  [objects has-fixed?]
  (vpw/render-passes (vpw/layer-masks objects board-id has-fixed? (vpc/screen-sticky-ids objects board-id))
                     board-id nil))

(t/deftest a-board-renders-what-scrolls-its-sticky-and-its-fixed-layers-apart
  (let [plain (dissoc objects heading label tabs)]
    (t/is (= [{:tag "scroll" :include nil}]
             (passes (assoc-in plain [board-id :shapes] [row chips]) false))
          "without fixed or sticky layers: one pass, of all of it"))
  (let [no-tabs (-> objects
                    (dissoc tabs)
                    (update-in [board-id :shapes] #(vec (remove #{tabs} %))))
        [scroll sticky :as all] (passes no-tabs false)]
    (t/is (= ["scroll" (str "sticky:" heading)] (map :tag all)) "with a sticky one")
    (t/is (= #{board-id row chips chip} (set (:include scroll))) "what scrolls leaves it to its own")
    (t/is (= {:layer heading :include [board-id] :clear #{board-id} :still true}
             (-> sticky
                 (dissoc :tag)
                 (update :include #(vec (take 1 %)))))
          "which draws it over its board, whose fills it clears, and stays as the containers scroll")
    (t/is (= #{board-id heading label} (set (:include sticky))) "with what is in it"))
  (let [all (passes objects true)]
    (t/is (= ["scroll" (str "sticky:" heading) "fixed"] (map :tag all))
          "the fixed layers last, over the others")
    (t/is (= #{board-id tabs} (set (:include (last all)))))
    (t/is (every? :still (rest all)))))

(t/deftest a-render-draws-the-passes-a-scroll-moves
  (let [all    (passes objects true)
        sticky (str "sticky:" heading)
        tags   #(set (map :tag %))]
    (t/is (= #{"scroll"} (rwv/moving-passes objects all)) "the container is in what scrolls")
    (t/is (= #{"scroll" sticky "fixed"} (tags (rwv/due-passes all {:drawn #{sticky "fixed"} :moving #{}})))
          "a render of the animation or of a change draws all")
    (t/is (= #{"scroll"} (tags (rwv/due-passes all {:drawn #{sticky "fixed"} :moving #{} :scroll-only? true})))
          "a scroll of the containers only what scrolls")
    (t/is (= #{"scroll" "fixed"} (tags (rwv/due-passes all {:drawn #{sticky} :moving #{} :scroll-only? true})))
          "and one not drawn yet, or whose render a later one skipped")
    (t/is (= #{"scroll" sticky} (tags (rwv/due-passes all {:drawn #{sticky "fixed"} :moving #{sticky} :scroll-only? true})))
          "and one a container scrolls in")))
