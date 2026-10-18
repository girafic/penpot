;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.geom-tilt-test
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

;; The expected corners are those Chromium's DOMMatrix gives for the
;; same CSS: `translate(vp) perspective(d) translate(-vp)` and the turns
;; around the pivot (the same literals are in `render-wasm/src/math/tilt.rs`).

(def ^:private board-id (uuid/custom 1 1))
(def ^:private group-id (uuid/custom 1 2))
(def ^:private rect-id (uuid/custom 1 3))

(defn- board
  [x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type :frame :x x :y y :width w :height h})
      (merge {:id board-id :parent-id uuid/zero :frame-id uuid/zero} attrs)))

(defn- rect
  [x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type :rect :x x :y y :width w :height h})
      (merge {:id rect-id :parent-id board-id :frame-id board-id} attrs)))

(defn- objects
  [& shapes]
  (into {} (map (juxt :id identity)) shapes))

(defn- corners
  [objects id]
  (mapv (fn [p] [(:x p) (:y p)]) (gtl/projected-points objects (get objects id))))

(defn- close-corners?
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map (fn [[ex ey] [ax ay]]
                            (and (mth/close? ex ax 1e-3) (mth/close? ey ay 1e-3)))
                          expected actual))))

(t/deftest a-flat-shape-has-no-projection
  (let [objs (objects (board 0 0 400 400) (rect 100 150 200 100))]
    (t/is (nil? (gtl/tilt-matrix objs (get objs rect-id))))
    (t/is (nil? (gtl/projection objs (get objs rect-id))))
    (t/is (nil? (gtl/projected-points objs (get objs rect-id))))
    (t/is (false? (gtl/back-facing? objs (get objs rect-id))))))

(t/deftest a-tilt-shows-as-css-draws-it
  (t/testing "rotate-y: the right edge goes back"
    (let [objs (objects (board 0 0 400 400) (rect 100 150 200 100 :rotate-y 60))]
      (t/is (close-corners? [[145.2593178650658 145.25931786506578]
                             [246.014985371363 153.98501462863697]
                             [246.014985371363 246.01498537136305]
                             [145.2593178650658 254.74068213493425]]
                            (corners objs rect-id)))))

  (t/testing "rotate-x: the top edge goes back"
    (let [objs (objects (board 0 0 400 400) (rect 100 150 200 100 :rotate-x 30))]
      (t/is (close-corners? [[102.43902439024389 157.75485835197856]
                             [297.56097560975604 157.75485835197856]
                             [302.56410256410254 244.41155916843272]
                             [97.43589743589743 244.41155916843272]]
                            (corners objs rect-id)))))

  (t/testing "rotate-x, then rotate-y, as rotateX() rotateY()"
    (let [objs (objects (board 0 0 400 400) (rect 100 150 200 100 :rotate-x 20 :rotate-y 35))]
      (t/is (close-corners? [[114.9553614615924 130.8534770133438]
                             [276.48481601970104 174.44708427275793]
                             [279.007907225437 264.2382890559766]
                             [111.82436407334525 229.4587175766942]]
                            (corners objs rect-id)))))

  (t/testing "the tilt turns in the axes of a turned layer"
    (let [turned (-> (rect 100 150 200 100)
                     (gsh/transform-shape (ctm/rotation (ctm/empty) (gpt/point 200 200) 30))
                     (assoc :rotate-x 45))
          objs   (objects (board 0 0 400 400) turned)]
      (t/is (close-corners? [[133.42877730132543 122.13434485397467]
                             [300.7192467881137 218.71954244503078]
                             [271.4510468358039 283.5733872265198]
                             [91.89780016361794 179.90827192646196]]
                            (corners objs rect-id)))))

  (t/testing "the vanishing point is the middle of the board"
    (let [objs (objects (board 0 0 1200 400) (rect 1050 150 100 100 :rotate-y 70))]
      (t/is (close-corners? [[1106.7064063828627 147.53494893316707]
                             [1093.8955089082638 152.2438071031022]
                             [1093.8955089082638 247.75619289689791]
                             [1106.7064063828627 252.46505106683296]]
                            (corners objs rect-id))))))

(t/deftest the-layers-of-a-tilted-group-tilt-with-it
  (let [group (-> (cts/setup-shape {:type :group :x 50 :y 50 :width 300 :height 300})
                  (merge {:id group-id :parent-id board-id :frame-id board-id :rotate-x 20}))
        objs  (objects (board 0 0 400 400)
                       group
                       (rect 100 150 200 100 :parent-id group-id :rotate-y 40))]
    (t/is (close-corners? [[119.60258629812793 150.68898608655059]
                           [270.8395390492815 156.55122055748268]
                           [273.1532388209567 244.86786590785678]
                           [116.60923452951954 251.14695867725789]]
                          (corners objs rect-id)))))

(t/deftest a-pivot-turns-from-where-the-layer-rests
  (t/testing "a door turns around its hinge"
    (let [objs (objects (board 0 0 400 400)
                        (rect 100 150 200 100 :rotate-y 50
                              :tilt-pivot {:x 0 :y 0.5 :rest-x 0 :rest-y 0}))]
      (t/is (close-corners? [[100 150]
                             [224.7635291567927 156.64272059187087]
                             [224.7635291567927 243.35727940812916]
                             [100 250]]
                            (corners objs rect-id)))))

  (t/testing "at its values at rest a layer shows as it rests, whatever its pivot"
    (let [rest   [[108.83943118058538 147.3684210526316]
                  [282.47860988423224 152.38095238095238]
                  [282.47860988423224 247.61904761904762]
                  [108.83943118058538 252.63157894736844]]
          at     (fn [pivot]
                   (corners (objects (board 0 0 400 400)
                                     (rect 100 150 200 100 :rotate-y 30 :tilt-pivot pivot))
                            rect-id))]
      (t/is (close-corners? rest (corners (objects (board 0 0 400 400)
                                                   (rect 100 150 200 100 :rotate-y 30))
                                          rect-id)))
      (t/is (close-corners? rest (at {:x 0 :y 0.5 :rest-x 0 :rest-y 30})))
      (t/is (close-corners? rest (at {:x 1 :y 0 :rest-x 0 :rest-y 30})))))

  (t/testing "the lift of a pivot is where the rest tilt takes it"
    (let [shape (rect 100 150 200 100)
          [dx dy dz] (gtl/lift shape 0 0.5 0 30)]
      ;; the left edge, 100 px left of the middle, comes 50 px forward
      (t/is (mth/close? (- 100 (* 100 (mth/cos (mth/radians 30)))) dx 1e-6))
      (t/is (mth/close? 0 dy 1e-6))
      (t/is (mth/close? 50 dz 1e-6)))))

(t/deftest back-faces
  (let [at-vp (fn [ry & {:as attrs}]
                (let [objs (objects (board 0 0 400 400)
                                    (merge (rect 150 150 100 100 :rotate-y ry) attrs))]
                  (gtl/back-facing? objs (get objs rect-id))))]
    (t/testing "a layer at the vanishing point turns over at 90"
      (t/is (false? (at-vp 89)))
      (t/is (true? (at-vp 91))))

    (t/testing "a layer away from the vanishing point turns over earlier, as browsers show it"
      (let [off (fn [ry]
                  (let [objs (objects (board 0 0 1200 400) (rect 1050 150 100 100 :rotate-y ry))]
                    (gtl/back-facing? objs (get objs rect-id))))]
        (t/is (false? (off 60)))
        (t/is (true? (off 65)))))

    (t/testing "a flip in the plane is no back"
      (let [flipped (-> (rect 150 150 100 100)
                        (gsh/transform-shape (ctm/resize (ctm/empty) (gpt/point -1 1) (gpt/point 200 200)))
                        (assoc :rotate-y 30))
            objs    (objects (board 0 0 400 400) flipped)]
        (t/is (false? (gtl/back-facing? objs (get objs rect-id))))
        (t/is (true? (gtl/back-facing? (assoc-in objs [rect-id :rotate-y] 120)
                                       (assoc flipped :rotate-y 120)))))))

  (t/testing "a group turned over turns over what it holds"
    (let [group (fn [ry]
                  (-> (cts/setup-shape {:type :group :x 50 :y 50 :width 300 :height 300})
                      (merge {:id group-id :parent-id board-id :frame-id board-id :rotate-y ry})))
          back  (fn [group-ry rect-ry]
                  (let [objs (objects (board 0 0 400 400)
                                      (group group-ry)
                                      (rect 150 150 100 100 :parent-id group-id :rotate-y rect-ry))]
                    (gtl/back-facing? objs (get objs rect-id))))]
      (t/is (true? (back 180 0)))
      (t/is (false? (back 180 180)))
      (t/is (false? (back 0 0))))))

(t/deftest a-layer-that-reaches-the-viewer-does-not-show
  (let [objs (objects (board 0 0 400 400) (rect -1800 150 4000 100 :rotate-y 89))]
    (t/is (some? (gtl/projection objs (get objs rect-id))))
    (t/is (nil? (gtl/projected-points objs (get objs rect-id))))))

(t/deftest the-tilt-attributes-validate
  (let [shape (rect 0 0 10 10)
        frame (board 0 0 100 100)]
    (t/is (cts/valid-shape? (assoc shape :rotate-x 30 :rotate-y -10 :backface-hidden true)))
    (t/is (not (cts/valid-shape? (assoc shape :rotate-x nil))))
    (t/is (not (cts/valid-shape? (assoc shape :rotate-y "30"))))
    (t/is (cts/valid-shape? (assoc frame :perspective 800)))
    (t/is (not (cts/valid-shape? (assoc frame :perspective 50))))
    (t/is (not (cts/valid-shape? (assoc frame :perspective nil))))))
