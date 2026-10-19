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

(t/deftest the-vanishing-point-of-a-board-moves
  (t/testing "as CSS `perspective-origin: 25% 75%`"
    (let [objs (objects (board 0 0 1200 400 :perspective-x 0.25 :perspective-y 0.75)
                        (rect 1050 150 100 100 :rotate-y 70))]
      (t/is (gpt/close? (gpt/point 300 300) (gtl/vanishing-point (get objs board-id))))
      (t/is (close-corners? [[1121.4967127838604 142.60484679950116]
                             [1080.4326662896508 156.7314213093065]
                             [1080.4326662896508 252.24380710310214]
                             [1121.4967127838604 247.53494893316704]]
                            (corners objs rect-id)))))

  (t/testing "at its fractions as the board lies, turned"
    (let [quarter (-> (board 0 0 200 100 :perspective-x 0 :perspective-y 0)
                      (gsh/transform-shape (ctm/rotation (ctm/empty) (gpt/point 100 50) 90)))
          vp      (gtl/vanishing-point quarter)]
      ;; its top left corner turns to the top right
      (t/is (mth/close? 150 (:x vp) 1e-6))
      (t/is (mth/close? -50 (:y vp) 1e-6)))))

(def ^:private face-id (uuid/custom 1 4))

(defn- cube-corner
  "A board seen from 800 px, a group turned 30° about Y holding a face
  100 px forward and a face turned 90° on its right: a corner of a cube."
  [& {:as group-attrs}]
  (let [group (-> (cts/setup-shape {:type :group :x 100 :y 100 :width 300 :height 200})
                  (merge {:id group-id :parent-id board-id :frame-id board-id
                          :shapes [rect-id face-id] :rotate-y 30}
                         group-attrs))]
    (objects (board 0 0 400 400 :perspective 800)
             group
             (rect 100 100 200 200 :parent-id group-id :translate-z 100)
             (merge (rect 200 100 200 200 :parent-id group-id :rotate-y 90) {:id face-id}))))

(t/deftest a-3d-space-keeps-the-depth-of-what-it-holds
  (t/testing "as CSS draws `transform-style: preserve-3d`"
    (let [objs (cube-corner :preserve-depth true)]
      (t/is (true? (gtl/three-d-space? (get objs group-id))))
      (t/is (close-corners? [[162.52640405506264 74.68621813215825]
                             [355.2565148451803 91.65726539606239]
                             [355.2565148451803 308.34273460393763]
                             [162.52640405506264 325.3137818678418]]
                            (corners objs rect-id)))
      (t/is (close-corners? [[355.2565148451804 91.65726539606241]
                             [238.00013121617312 112.24245605240561]
                             [238.00013121617312 287.7575439475945]
                             [355.2565148451804 308.3427346039377]]
                            (corners objs face-id)))
      (t/is (mth/close? 111.60254037844386 (gtl/depth objs (get objs rect-id)) 1e-6))
      (t/is (mth/close? -25 (gtl/depth objs (get objs face-id)) 1e-6))
      (t/is (= [face-id rect-id] (gtl/depth-sorted objs (get objs group-id) [rect-id face-id]))
            "back to front")
      (t/is (nil? (gtl/own-matrix objs (get objs group-id)))
            "the group passes its tilt to what it holds")))

  (t/testing "a group that is no 3D space flattens what it holds"
    (let [objs (cube-corner)]
      (t/is (false? (gtl/three-d-space? (get objs group-id))))
      (t/is (not (close-corners? [[162.52640405506264 74.68621813215825]
                                  [355.2565148451803 91.65726539606239]
                                  [355.2565148451803 308.34273460393763]
                                  [162.52640405506264 325.3137818678418]]
                                 (corners objs rect-id))))
      (t/is (= [rect-id face-id] (gtl/depth-sorted objs (get objs group-id) [rect-id face-id])))))

  (t/testing "an opacity, a shadow or a blur flattens it, as in CSS"
    (t/is (false? (gtl/three-d-space? (get (cube-corner :preserve-depth true :opacity 0.5) group-id))))
    (t/is (false? (gtl/three-d-space? (get (cube-corner :preserve-depth true :blur {:id (uuid/next) :type :layer-blur :value 4 :hidden false}) group-id))))
    (t/is (false? (gtl/three-d-space? (assoc (board 0 0 10 10) :preserve-depth true))) "a board that clips")
    (t/is (true? (gtl/three-d-space? (assoc (board 0 0 10 10) :preserve-depth true :show-content true))))))

(t/deftest parallel-layers-draw-by-their-planes
  ;; turned 32° the small moon, 34 px in front of the picture but off its
  ;; middle, is less deep by its middle: its plane tells
  (let [group   (-> (cts/setup-shape {:type :group :x 100 :y 100 :width 200 :height 200})
                    (merge {:id group-id :parent-id board-id :frame-id board-id
                            :shapes [rect-id face-id] :rotate-y 32 :preserve-depth true}))
        picture (rect 106 140 188 120 :parent-id group-id :translate-z 36)
        moon    (merge (rect 238 183 34 34 :parent-id group-id :translate-z 70) {:id face-id})
        objs    (objects (board 0 0 400 400 :perspective 800) group picture moon)]
    (t/is (< (gtl/depth objs moon) (gtl/depth objs picture)))
    (t/is (= [rect-id face-id] (gtl/depth-sorted objs (get objs group-id) [face-id rect-id])))
    (t/is (true? (gtl/in-front? [[0 0 0] [10 0 0] [10 10 0] [0 10 0]]
                                [[0 0 10] [10 0 10] [10 10 10] [0 10 10]]
                                [5 5 1000])))
    (t/is (nil? (gtl/in-front? [[0 0 0] [10 0 0] [10 10 0] [0 10 0]]
                               [[0 0 0] [10 0 0] [10 10 0] [0 10 0]]
                               [5 5 1000]))
          "one plane: nothing tells")
    (t/testing "in one plane they keep their order, however the space turns"
      (let [flat (-> objs
                     (update group-id assoc :rotate-x 14 :rotate-y -32)
                     (update rect-id dissoc :translate-z)
                     (update face-id dissoc :translate-z))]
        (t/is (= [face-id rect-id] (gtl/depth-sorted flat (get flat group-id) [face-id rect-id])))
        (t/is (= [rect-id face-id] (gtl/depth-sorted flat (get flat group-id) [rect-id face-id])))))))

(t/deftest the-layers-of-a-3d-space-group-in-a-3d-space-sort-with-its-layers
  ;; a platform at depth 0, a rim at 10 and a cube group whose lid is at 50:
  ;; the cube as one plane lay at 0, under the rim (render-wasm sorts alike)
  (let [[scene-id cube-id rim-id lid-id] (map #(uuid/custom 1 %) [20 21 22 23])
        group    (fn [id parent-id shapes & {:as attrs}]
                   (-> (cts/setup-shape {:type :group :x 0 :y 0 :width 400 :height 400})
                       (merge {:id id :parent-id parent-id :frame-id board-id :shapes shapes
                               :preserve-depth true}
                              attrs)))
        objs     (fn [& {:as cube-attrs}]
                   (objects (board 0 0 400 400 :shapes [scene-id])
                            (group scene-id board-id [rect-id rim-id cube-id])
                            (group cube-id scene-id [lid-id] cube-attrs)
                            (rect 0 0 400 400 :parent-id scene-id)
                            (merge (rect 20 20 360 360 :parent-id scene-id :translate-z 10) {:id rim-id})
                            (merge (rect 150 150 100 100 :parent-id cube-id :translate-z 50) {:id lid-id})))
        sorted   (fn [objs]
                   (let [[layers groups] (gtl/space-layers objs [rect-id rim-id cube-id])]
                     [(gtl/depth-sorted objs (get objs scene-id) layers) groups]))]
    (t/is (= [[rect-id rim-id lid-id] [cube-id]] (sorted (objs))) "back to front, the lid in front")
    (t/is (= [[rect-id cube-id rim-id] []]
             (sorted (objs :fills [{:fill-color "#ff0000" :fill-opacity 1}])))
          "a group whose layers take its fills sorts as one")
    (t/is (= [[rect-id cube-id rim-id] []] (sorted (objs :hidden true))) "nor a hidden one")))

(t/deftest an-orthographic-board-sees-without-perspective
  (let [near? (fn [expected actual]
                (every? true? (map (fn [[ex ey] [ax ay]]
                                     (and (mth/close? ex ax 1e-2) (mth/close? ey ay 1e-2)))
                                   expected actual)))]
    (t/testing "a turn about Y narrows the layer, its edges stay as long"
      (let [objs (objects (board 0 0 400 400 :orthographic true) (rect 100 150 200 100 :rotate-y 60))]
        (t/is (near? [[150 150] [250 150] [250 250] [150 250]] (corners objs rect-id)))))
    (t/testing "turned isometrically, a square is the rhombus of an isometric drawing"
      ;; as CSS rotateX(54.7356deg) rotateZ(45deg) without perspective
      (let [square (-> (rect 150 150 100 100)
                       (gsh/transform-shape (ctm/rotation (ctm/empty) (gpt/point 200 200) 60))
                       (assoc :rotate-x 35.2644 :rotate-y -45))
            objs   (objects (board 0 0 400 400 :orthographic true) square)]
        (t/is (near? [[129.289322 200] [200 159.175161] [200 240.824839] [270.710678 200]]
                     (sort (corners objs rect-id))))))
    (t/is (cts/valid-shape? (assoc (board 0 0 10 10) :orthographic true)))))

(t/deftest a-layer-forward-shows-bigger
  ;; 100 px towards a viewer 1000 px away: 1000/900 as big, from the
  ;; vanishing point
  (let [objs (objects (board 0 0 400 400) (rect 110 110 180 180 :translate-z 100))
        k    (/ 1000 900)]
    (t/is (true? (gtl/three-d? (get objs rect-id))))
    (t/is (false? (gtl/tilted? (get objs rect-id))))
    (t/is (close-corners? [[(- 200 (* 90 k)) (- 200 (* 90 k))]
                           [(+ 200 (* 90 k)) (- 200 (* 90 k))]
                           [(+ 200 (* 90 k)) (+ 200 (* 90 k))]
                           [(- 200 (* 90 k)) (+ 200 (* 90 k))]]
                          (corners objs rect-id)))))

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

(def ^:private flat-lit
  "How a face looking at the viewer is lit from 30° above the plane,
  a fifth of the light all round."
  (+ 0.2 (* 0.8 0.5)))

(t/deftest a-light-shades-the-layers-in-3d
  ;; from the right, 30° above the plane: a face turned to it shows
  ;; brighter, one turned away darker, one looking at the viewer as it is
  (let [lit (fn [& attrs]
              (let [objs (objects (board 0 0 400 400 :light {:angle 90 :height 30 :ambient 0.2})
                                  (apply rect 100 150 200 100 attrs))]
                (gtl/scene-effects objs (get objs rect-id))))]
    (t/is (nil? (lit)) "a layer in the plane")
    (t/is (nil? (lit :translate-z -50)) "a layer off the plane looking at the viewer")
    (t/is (mth/close? (/ (+ 0.2 (* 0.8 (+ (* 0.5 0.8660254037844386) (* 0.8660254037844386 0.5)))) flat-lit)
                      (:shade (lit :rotate-y 30)) 1e-6)
          "its right edge back, it looks to the right")
    (t/is (mth/close? (/ 0.2 flat-lit) (:shade (lit :rotate-y -60)) 1e-6)
          "its left edge back, away from the light: what comes from all round")
    (t/is (nil? (lit :rotate-y 180)) "its back looks at the viewer as its face did")
    (t/is (nil? (let [objs (objects (board 0 0 400 400) (rect 100 150 200 100 :rotate-y 30))]
                  (gtl/scene-effects objs (get objs rect-id))))
          "a board with no light")))

(t/deftest a-3d-space-group-lights-what-it-holds
  (let [objs (-> (cube-corner :preserve-depth true)
                 (update board-id assoc :light {:angle 90 :height 30 :ambient 0.2}))]
    (t/is (nil? (gtl/scene-effects objs (get objs group-id))) "it draws nothing of its own")
    (t/is (mth/close? (/ (+ 0.2 (* 0.8 0.8660254037844386)) flat-lit)
                      (:shade (gtl/scene-effects objs (get objs rect-id))) 1e-6)
          "turned 30° to the light with the group")
    (t/is (mth/close? (/ 0.2 flat-lit) (:shade (gtl/scene-effects objs (get objs face-id))) 1e-6)
          "turned 120°: it shows its back, which looks away from the light"))
  (t/testing "a group that flattens what it holds is shaded as one"
    (let [objs (-> (cube-corner)
                   (update board-id assoc :light {:angle 90 :height 30 :ambient 0.2}))]
      (t/is (mth/close? (/ (+ 0.2 (* 0.8 0.8660254037844386)) flat-lit)
                        (:shade (gtl/scene-effects objs (get objs group-id))) 1e-6))
      (t/is (nil? (gtl/scene-effects objs (get objs rect-id))) "in the plane of the group"))))

(t/deftest a-light-casts-shadows-away-from-it
  (let [cast (fn [angle ox oy] ((juxt :offset-x :offset-y) (gtl/cast-shadow {:angle angle} {:offset-x ox :offset-y oy})))]
    (t/is (= [4.0 4.0] (cast 315 4 4)) "from the top left, to the bottom right, as it was")
    (t/is (= [-4.0 4.0] (cast 45 4 4)) "from the top right, to the bottom left")
    (t/is (= [0.0 -5.66] (cast 180 4 4)) "from below, up, as long")
    (t/is (= [0.0 0.0] (cast 90 0 0)) "none stays none")))

(t/deftest depth-cues-fade-and-blur-the-layers-in-3d
  (let [cued (fn [cues & attrs]
               (let [objs (objects (apply board 0 0 400 400 (mapcat identity cues))
                                   (apply rect 100 150 200 100 attrs))]
                 (gtl/scene-effects objs (get objs rect-id))))]
    (t/is (mth/close? 0.5 (:fade (cued {:depth-fade 0.5} :translate-z -1000)) 1e-9)
          "half gone 1000 px behind")
    (t/is (mth/close? (mth/sqrt 0.5) (:fade (cued {:depth-fade 0.5} :translate-z -500)) 1e-9))
    (t/is (nil? (cued {:depth-fade 0.5} :translate-z 200)) "none in front")
    (t/is (mth/close? 0.5 (:fade (cued {:depth-fade 0.5 :depth-focus 150} :translate-z -850)) 1e-9)
          "behind its focus")
    (t/is (nil? (cued {:depth-fade 0.5 :depth-blur 2})) "none in the plane")
    (t/is (mth/close? 5 (:blur (cued {:depth-blur 2} :translate-z -250)) 1e-9)
          "2 px each 100 px off the plane")
    (t/is (mth/close? 4 (:blur (cued {:depth-blur 2 :depth-focus -50} :translate-z 150)) 1e-9)
          "and off a focus")
    (t/is (nil? (cued {:depth-blur 2 :depth-focus -250} :translate-z -250)) "sharp at its focus")
    (t/is (= gtl/max-depth-blur (:blur (cued {:depth-blur 100} :translate-z -5000))) "up to a limit")))

(t/deftest the-tilt-attributes-validate
  (let [shape (rect 0 0 10 10)
        frame (board 0 0 100 100)]
    (t/is (cts/valid-shape? (assoc shape :rotate-x 30 :rotate-y -10 :backface-hidden true)))
    (t/is (not (cts/valid-shape? (assoc shape :rotate-x nil))))
    (t/is (not (cts/valid-shape? (assoc shape :rotate-y "30"))))
    (t/is (cts/valid-shape? (assoc frame :perspective 800)))
    (t/is (cts/valid-shape? (assoc frame :perspective-x 0.25 :perspective-y 1.5)))
    (t/is (cts/valid-shape? (assoc shape :translate-z -40)))
    (t/is (cts/valid-shape? (assoc frame :preserve-depth true)))
    (t/is (not (cts/valid-shape? (assoc frame :preserve-depth "yes"))))
    (t/is (not (cts/valid-shape? (assoc frame :perspective-x "0.5"))))
    (t/is (not (cts/valid-shape? (assoc frame :perspective 50))))
    (t/is (not (cts/valid-shape? (assoc frame :perspective nil))))
    (t/is (cts/valid-shape? (assoc frame :light gtl/default-light :depth-fade 0.4 :depth-blur 3 :depth-focus -120)))
    (t/is (not (cts/valid-shape? (assoc frame :light {:angle 0 :height 120 :ambient 0}))))
    (t/is (not (cts/valid-shape? (assoc frame :light {:angle 0}))))
    (t/is (not (cts/valid-shape? (assoc frame :depth-fade 2))))))
