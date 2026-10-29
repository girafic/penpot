;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-hotspots-test
  "The hotspots of the WASM view mode where their shapes show: in the
  order they are drawn, and tilted and in depth (Penpot Motion)."
  (:require
   [app.common.geom.tilt :as gtl]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.ui.viewer.shapes :as vs]
   [cljs.test :as t :include-macros true]))

(def ^:private board-id (uuid/custom 9 1))

(defn- shape
  [id type parent-id x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type type :x x :y y :width w :height h})
      (merge {:id id :parent-id parent-id :frame-id board-id} attrs)))

(defn- objects
  [& shapes]
  (into {} (map (juxt :id identity)) shapes))

(t/deftest hotspots-come-in-the-order-shapes-are-drawn
  (let [[g a b c] (map #(uuid/custom 9 %) [2 3 4 5])
        objs (objects (shape board-id :frame uuid/zero 0 0 400 400 :frame-id uuid/zero :shapes [g c])
                      (shape g :group board-id 0 0 200 200 :shapes [a b])
                      (shape a :rect g 0 0 100 100)
                      (shape b :rect g 100 0 100 100)
                      (shape c :rect board-id 50 50 100 100))]
    (t/is (= [board-id g a b c] (vs/hotspot-ids objs board-id))
          "what a group holds before what is over the group")))

(t/deftest the-layers-of-a-3d-space-come-by-their-depth
  (let [[g near far] (map #(uuid/custom 9 %) [2 3 4])
        objs (objects (shape board-id :frame uuid/zero 0 0 400 400 :frame-id uuid/zero :shapes [g])
                      ;; the near one first in the layers, under the far one
                      (shape g :group board-id 100 100 200 200 :shapes [near far]
                             :preserve-depth true :rotate-y 20)
                      (shape near :rect g 100 100 200 200 :translate-z 80)
                      (shape far :rect g 100 100 200 200 :translate-z -80))]
    (t/is (= [board-id g far near] (vs/hotspot-ids objs board-id)))))

(t/deftest the-layers-of-a-3d-space-group-in-a-3d-space-come-by-their-depth
  (let [[scene cube platform rim lid] (map #(uuid/custom 9 %) [2 3 4 5 6])
        objs (objects (shape board-id :frame uuid/zero 0 0 400 400 :frame-id uuid/zero :shapes [scene])
                      (shape scene :group board-id 0 0 400 400 :shapes [platform rim cube] :preserve-depth true)
                      (shape cube :group scene 150 150 100 100 :shapes [lid] :preserve-depth true)
                      (shape platform :rect scene 0 0 400 400)
                      (shape rim :rect scene 20 20 360 360 :translate-z 10)
                      (shape lid :rect cube 150 150 100 100 :translate-z 50))]
    (t/is (= [board-id scene cube platform rim lid] (vs/hotspot-ids objs board-id))
          "the lid over the rim, the group under what it holds")))

(t/deftest a-hotspot-is-where-its-shape-shows
  (let [r    (uuid/custom 9 2)
        flat (objects (shape board-id :frame uuid/zero 0 0 400 400 :frame-id uuid/zero :shapes [r])
                      (shape r :rect board-id 100 150 200 100))]
    (t/is (= :box (vs/hotspot-outline flat (get flat r))) "flat: its box")
    (let [objs (assoc-in flat [r :rotate-y] 60)]
      (t/is (= (gtl/projected-points objs (get objs r))
               (vs/hotspot-outline objs (get objs r)))
            "tilted: the corners it shows at"))
    (let [objs (update flat r assoc :rotate-y 150 :backface-hidden true)]
      (t/is (nil? (vs/hotspot-outline objs (get objs r))) "its back hidden: nothing to click"))))

(t/deftest only-the-shapes-with-interactions-get-a-hotspot
  ;; a board without them has no hotspot to follow the animation with
  (let [[a b c] (map #(uuid/custom 9 %) [2 3 4])
        click   [{:event-type :click :action-type :navigate :destination board-id}]
        objs    (objects (shape board-id :frame uuid/zero 0 0 400 400 :frame-id uuid/zero :shapes [a b c])
                         (shape a :rect board-id 0 0 100 100 :interactions click)
                         (shape b :rect board-id 100 0 100 100)
                         (shape c :rect board-id 200 0 100 100 :interactions click :hidden true))]
    (t/is (= [a] (map :id (vs/hotspot-shapes objs board-id nil))))
    (t/is (empty? (vs/hotspot-shapes objs board-id #(not= a %))) "the shape filter leaves it out")
    (t/is (empty? (vs/hotspot-shapes (update objs a dissoc :interactions) board-id nil)))))
