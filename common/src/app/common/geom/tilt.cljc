;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.geom.tilt
  "The 3D rotation of layers (Penpot Motion): a layer tilts about its own
  X and Y axes (`:rotate-x`, `:rotate-y`, degrees), seen with the
  perspective of its board (`:perspective`, px). It is drawn, not kept in
  its geometry: `:transform`, `:points` and `:selrect` stay flat.

  As CSS draws `perspective(d) rotate(r) rotateX(a) rotateY(b)`: the tilt
  turns the layer in its own axes, before its turn in the plane, the top
  edge going back for a positive `:rotate-x`, the right edge for a
  positive `:rotate-y`; a point `z` towards the viewer shows at
  `vp + (p - vp) * d / (d - z)`, `vp` the vanishing point of the board
  (`:perspective-x`, `:perspective-y`, fractions of it, its middle
  without; CSS `perspective-origin`).

  A layer also comes `:translate-z` px towards the viewer, in the space
  of its parent: nearer, it shows bigger, away from the vanishing point.

  What a shape is drawn through is a homography of the page (`own-matrix`),
  a vector of 9 numbers by rows mapping `[x y 1]`. The layers in a tilted
  group or board are drawn through its homography too, after their own
  (`projection`): each tilted layer flattens what it holds, as CSS does
  without `preserve-3d`. A group that is a 3D space (`:preserve-depth`, see
  `three-d-space?`) does not: what it holds keeps its depth in its space,
  its tilt and depth in theirs, seen once (CSS `transform-style:
  preserve-3d`); the layers of a 3D space are drawn back to front
  (`depth`). A board that is a 3D space draws what it holds back to front
  too, seen with its own perspective. The WASM renderer does the same
  sums (`render-wasm/src/math/tilt.rs`)."
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes.common :as gco]
   [app.common.geom.shapes.transforms :as gtr]
   [app.common.math :as mth]
   [app.common.types.path.helpers :as path.helpers]
   [app.common.uuid :as uuid]))

(def default-perspective
  "The distance of the viewer, px, of a board with no `:perspective`."
  1000)

(def min-perspective 100)
(def max-perspective 100000)

(def orthographic-distance
  "How far the viewer of the tilted layers of an orthographic board (see
  `perspective-of`) is, px: far enough to see them without perspective,
  and finite, so that the planes of the depth sort stay numbers."
  1e7)

(def ^:private min-w
  "The least depth factor a point shows at: a point nearer the viewer
  (or behind) does not show."
  0.01)

(defn tilted?
  "Whether `shape` tilts out of the plane."
  [shape]
  (or (not (mth/almost-zero? (or (:rotate-x shape) 0)))
      (not (mth/almost-zero? (or (:rotate-y shape) 0)))))

(defn lifted?
  "Whether `shape` comes out of the plane of its parent, towards the
  viewer or away (`:translate-z`)."
  [shape]
  (not (mth/almost-zero? (or (:translate-z shape) 0))))

(defn three-d?
  "Whether `shape` leaves the plane: it tilts or comes out of it."
  [shape]
  (or (tilted? shape) (lifted? shape)))

(defn three-d-space?
  "Whether `shape` is a 3D space: a group or a board that does not clip,
  set `:preserve-depth`, with nothing that would flatten what it holds into
  its plane, as CSS flattens a `preserve-3d` element with a clip, an
  opacity, a filter, a blend or a mask."
  [shape]
  (and (true? (:preserve-depth shape))
       (case (:type shape)
         :group (not (:masked-group shape))
         :frame (true? (:show-content shape))
         false)
       (>= (or (:opacity shape) 1) 0.999)
       (not (some #(not (:hidden %)) (:shadow shape)))
       (not (and (some? (:blur shape)) (not (:hidden (:blur shape))) (pos? (:value (:blur shape) 0))))
       (contains? #{nil :normal} (:blend-mode shape))))

(defn- space-group?
  "Whether `shape` is a group that is a 3D space (see `three-d-space?`):
  it draws nothing of its own, its tilt and depth go into those of what
  it holds."
  [shape]
  (and (= :group (:type shape)) (three-d-space? shape)))

;; 4×4 matrices, by rows, turning column vectors [x y z 1]

(defn- mul44
  [a b]
  (vec (for [r (range 4) c (range 4)]
         (+ (* (nth a (* r 4)) (nth b c))
            (* (nth a (+ (* r 4) 1)) (nth b (+ 4 c)))
            (* (nth a (+ (* r 4) 2)) (nth b (+ 8 c)))
            (* (nth a (+ (* r 4) 3)) (nth b (+ 12 c)))))))

(defn- translate44
  [x y z]
  [1 0 0 x
   0 1 0 y
   0 0 1 z
   0 0 0 1])

(defn- rotate-x44
  [deg]
  (let [r (mth/radians deg)
        c (mth/cos r)
        s (mth/sin r)]
    [1 0 0 0
     0 c (- s) 0
     0 s c 0
     0 0 0 1]))

(defn- rotate-y44
  [deg]
  (let [r (mth/radians deg)
        c (mth/cos r)
        s (mth/sin r)]
    [c 0 s 0
     0 1 0 0
     (- s) 0 c 0
     0 0 0 1]))

(defn- perspective44
  [d]
  [1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 (/ -1 d) 1])

(defn- matrix->m44
  "The plane matrix `m` (`gmt/matrix`) in space: depth stays."
  [m]
  [(dm/get-prop m :a) (dm/get-prop m :c) 0 (dm/get-prop m :e)
   (dm/get-prop m :b) (dm/get-prop m :d) 0 (dm/get-prop m :f)
   0 0 1 0
   0 0 0 1])

(defn- apply44
  "`[x y z]` moved by the affine `m`."
  [m [x y z]]
  [(+ (* (nth m 0) x) (* (nth m 1) y) (* (nth m 2) z) (nth m 3))
   (+ (* (nth m 4) x) (* (nth m 5) y) (* (nth m 6) z) (nth m 7))
   (+ (* (nth m 8) x) (* (nth m 9) y) (* (nth m 10) z) (nth m 11))])

(defn- restrict
  "The homography `m` makes of the plane `z = 0`: its rows and columns of
  x, y and w."
  [m]
  [(nth m 0) (nth m 1) (nth m 3)
   (nth m 4) (nth m 5) (nth m 7)
   (nth m 12) (nth m 13) (nth m 15)])

;; homographies

(defn mul
  "The homography of `a` after `b`; either nil is no change."
  [a b]
  (cond
    (nil? a) b
    (nil? b) a
    :else
    (vec (for [r (range 3) c (range 3)]
           (+ (* (nth a (* r 3)) (nth b c))
              (* (nth a (+ (* r 3) 1)) (nth b (+ 3 c)))
              (* (nth a (+ (* r 3) 2)) (nth b (+ 6 c))))))))

(defn det
  "The determinant of the homography `h`."
  [h]
  (let [[a b c d e f g h' i] h]
    (- (+ (* a (- (* e i) (* f h')))
          (* c (- (* d h') (* e g))))
       (* b (- (* d i) (* f g))))))

(defn project-point
  "`point` through the homography `h`, nil where it reaches the viewer."
  [h point]
  (let [x (dm/get-prop point :x)
        y (dm/get-prop point :y)
        w (+ (* (nth h 6) x) (* (nth h 7) y) (nth h 8))]
    (when (> w min-w)
      (gpt/point (/ (+ (* (nth h 0) x) (* (nth h 1) y) (nth h 2)) w)
                 (/ (+ (* (nth h 3) x) (* (nth h 4) y) (nth h 5)) w)))))

;; layers

(defn vanishing-point
  "The vanishing point of the tilted layers of `board`: the point at the
  fractions `:perspective-x`, `:perspective-y` of its box (its middle
  without), as the board lies."
  [board]
  (let [sr (:selrect board)
        fx (or (:perspective-x board) 0.5)
        fy (or (:perspective-y board) 0.5)]
    (gpt/transform (gpt/point (+ (:x sr) (* (:width sr) fx)) (+ (:y sr) (* (:height sr) fy)))
                   (gtr/transform-matrix board))))

(defn perspective-of
  "How the tilted layers of the board of `shape` are seen: the distance
  of the viewer (`:perspective` of the board, `default-perspective`
  without; `orthographic-distance` for an `:orthographic` board, which
  sees them without perspective, `:orthographic?`) and the vanishing
  point (see `vanishing-point`; the middle of `shape` at the root of the
  page)."
  [objects shape]
  (let [board (get objects (:frame-id shape))]
    (if (and (some? board)
             (not= uuid/zero (:id board))
             (= :frame (:type board)))
      {:distance      (if (true? (:orthographic board))
                        orthographic-distance
                        (or (:perspective board) default-perspective))
       :orthographic? (true? (:orthographic board))
       :vp            (vanishing-point board)}
      {:distance default-perspective
       :vp       (gco/shape->center shape)})))

(defn- rotation44
  [rx ry]
  (mul44 (rotate-x44 (or rx 0)) (rotate-y44 (or ry 0))))

(defn- about44
  "`m` around the point `[x y z]`."
  [m [x y z]]
  (mul44 (translate44 x y z) (mul44 m (translate44 (- x) (- y) (- z)))))

(defn- box-point
  "The point at the fractions `fx`, `fy` of the box of `shape` before it
  turns."
  [shape fx fy]
  (let [sr (:selrect shape)]
    [(+ (:x sr) (* (:width sr) fx)) (+ (:y sr) (* (:height sr) fy)) 0]))

(defn lift
  "How far the point at the fractions `fx`, `fy` of the box of `shape`
  (before it turns) moves, `[dx dy dz]` (dz towards the viewer), when it
  tilts by `rest-x`, `rest-y` around the middle: the place an animation
  turns it from, around that point, so that its values at rest show it
  as it rests."
  [shape fx fy rest-x rest-y]
  (let [o      (box-point shape fx fy)
        center (box-point shape 0.5 0.5)
        moved  (apply44 (about44 (rotation44 rest-x rest-y) center) o)]
    (mapv - moved o)))

(defn- tilt44
  "The tilt of `shape` in the space of its box: around its middle, or,
  while an animation plays it (`:tilt-pivot`, fractions of its box and
  the tilt it rests at), around its pivot placed where it rests."
  [shape]
  (let [rotation (rotation44 (:rotate-x shape) (:rotate-y shape))]
    (if-let [{:keys [x y rest-x rest-y]} (:tilt-pivot shape)]
      (let [o      (box-point shape x y)
            [dx dy dz] (lift shape x y rest-x rest-y)]
        (mul44 (translate44 (+ (nth o 0) dx) (+ (nth o 1) dy) dz)
               (mul44 rotation (translate44 (- (nth o 0)) (- (nth o 1)) 0))))
      (about44 rotation (box-point shape 0.5 0.5)))))

(defn- model44
  "Where `shape` takes the points of the page in space: its tilt (in its
  own axes, after its transform) and then its depth, nil when it stays in
  the plane."
  [shape]
  (when (three-d? shape)
    (let [z (or (:translate-z shape) 0)]
      (reduce mul44
              [(translate44 0 0 z)
               (matrix->m44 (gtr/transform-matrix shape))
               (tilt44 shape)
               (matrix->m44 (gtr/inverse-transform-matrix shape))]))))

(defn- seen
  "The homography of the page the points of `m44` show at, seen as
  `perspective` says (see `perspective-of`)."
  [m44 {:keys [distance vp]}]
  (let [vx (dm/get-prop vp :x)
        vy (dm/get-prop vp :y)]
    (restrict (mul44 (about44 (perspective44 distance) [vx vy 0]) m44))))

(defn seen-tilt-matrix
  "The homography of the page `shape` is drawn through for its own tilt
  and depth, seen as `perspective` says (see `perspective-of`), nil when
  it stays in the plane."
  [shape perspective]
  (some-> (model44 shape) (seen perspective)))

(defn- space-chain
  "`[m44 top]`: where `shape` and the 3D space groups it is in (see
  `space-group?`) take the points of the page, nil when all stay in the
  plane, and the outermost of them."
  [objects shape]
  (loop [m   (model44 shape)
         top shape]
    (let [parent (get objects (:parent-id top))]
      (if (and (some? parent) (not= uuid/zero (:id parent)) (space-group? parent))
        (recur (if-let [pm (model44 parent)] (if m (mul44 pm m) pm) m) parent)
        [m top]))))

(defn own-matrix
  "The homography of the page `shape` of `objects` is drawn through in
  the plane of the layer it is in: its tilt and depth and those of the 3D
  space groups it is in (see `space-chain`), seen from the board of the
  outermost (see `perspective-of`); nil when it stays in the plane, and
  for a 3D space group, which passes its own to what it holds."
  [objects shape]
  (when-not (space-group? shape)
    (let [[m top] (space-chain objects shape)]
      (when (some? m)
        (seen m (perspective-of objects top))))))

(defn tilt-matrix
  "The homography of the page `shape` of `objects` is drawn through for
  its own tilt and depth (see `own-matrix`)."
  [objects shape]
  (own-matrix objects shape))

(defn projection
  "The homography of the page `shape` is drawn through: its own (see
  `own-matrix`), then that of each group or board it is in, nil when
  none leaves the plane."
  [objects shape]
  (loop [h      (own-matrix objects shape)
         parent (get objects (:parent-id shape))]
    (if (or (nil? parent) (= uuid/zero (:id parent)))
      h
      (recur (mul (own-matrix objects parent) h)
             (get objects (:parent-id parent))))))

(defn depth
  "How far the middle of `shape` comes towards the viewer in the 3D space
  it is in (see `space-chain`), px: what draws it before or after the
  other layers of a 3D space."
  [objects shape]
  (let [[m _] (space-chain objects shape)
        c     (gco/shape->center shape)]
    (if (some? m)
      (nth (apply44 m [(:x c) (:y c) 0]) 2)
      0)))

(defn- sub3 [[ax ay az] [bx by bz]] [(- ax bx) (- ay by) (- az bz)])
(defn- dot3 [[ax ay az] [bx by bz]] (+ (* ax bx) (* ay by) (* az bz)))
(defn- cross3
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])

(defn- face
  "`[corners eye]`: the corners of `shape` in the 3D space it is in (see
  `space-chain`), and where the viewer of that space is."
  [objects shape]
  (let [[m top] (space-chain objects shape)
        {:keys [distance vp]} (perspective-of objects top)]
    [(mapv (fn [p] (let [q [(:x p) (:y p) 0]] (if m (apply44 m q) q))) (:points shape))
     [(dm/get-prop vp :x) (dm/get-prop vp :y) distance]]))

(def ^:private plane-tolerance
  "How far, px, a corner can be off a plane and count as in it."
  0.01)

(defn- eye-sides
  "How far `points` are from the plane of the layer with the corners
  `corners`, px, positive on the side of `eye`; nil for a layer with no
  area."
  [[c0 c1 _ c3] points eye]
  (let [n   (cross3 (sub3 c1 c0) (sub3 c3 c0))
        len (mth/sqrt (dot3 n n))]
    (when (> len 1e-9)
      (let [side (fn [p] (/ (dot3 n (sub3 p c0)) len))
            e    (side eye)]
        (when-not (mth/almost-zero? e)
          (mapv #(if (pos? e) (side %) (- (side %))) points))))))

(defn- coplanar?
  "Whether the layers with the corners `a` and `b` lie in one plane."
  [[c0 c1 _ c3] b]
  (let [n   (cross3 (sub3 c1 c0) (sub3 c3 c0))
        len (mth/sqrt (dot3 n n))]
    (and (> len 1e-9)
         (every? #(< (mth/abs (/ (dot3 n (sub3 % c0)) len)) plane-tolerance) b))))

(defn in-front?
  "Whether the layer with the corners `b` shows in front of the one with
  the corners `a`, seen from `eye`: true when `b` is all on the side of
  the plane of `a` the viewer is on, or `a` all behind the plane of `b`;
  false the other way round; nil when they cross or lie in one plane."
  [a b eye]
  (let [on-eye-side? (fn [sides] (and (some? sides)
                                      (every? #(> % (- plane-tolerance)) sides)
                                      (some #(> % plane-tolerance) sides)))
        behind?      (fn [sides] (and (some? sides)
                                      (every? #(< % plane-tolerance) sides)
                                      (some #(< % (- plane-tolerance)) sides)))
        b-to-a       (eye-sides a b eye)
        a-to-b       (eye-sides b a eye)]
    (cond
      (or (on-eye-side? b-to-a) (behind? a-to-b)) true
      (or (behind? b-to-a) (on-eye-side? a-to-b)) false
      :else nil)))

(def ^:private max-plane-sorted
  "The most layers a 3D space sorts by their planes; more go by the depth
  of their middle only."
  128)

(defn depth-sorted
  "`ids`, layers of `objects` in the parent `parent` from the back to the
  front, as they show: in a 3D space (see `three-d-space?`) each, from the
  back, goes in front of those it shows in front of (see `in-front?`), as
  layers parallel to each other need; where two lie in one plane as they
  are, where they cross by how far their middle comes towards the viewer
  (see `depth`); else as they are."
  [objects parent ids]
  (if (three-d-space? parent)
    (let [layers (mapv (fn [id]
                         (let [shape (get objects id)]
                           {:id id :depth (depth objects shape) :face (face objects shape)}))
                       ids)]
      (if (> (count layers) max-plane-sorted)
        (->> (map-indexed vector layers)
             (sort-by (fn [[i l]] [(:depth l) i]))
             (mapv (comp :id second)))
        (let [ahead? (fn [x y]
                       (let [[fx eye] (:face x)
                             [fy _]   (:face y)
                             ahead    (in-front? fy fx eye)]
                         (cond
                           (some? ahead)     ahead
                           (coplanar? fy fx) false
                           :else             (> (:depth x) (:depth y)))))]
          (->> layers
               (reduce (fn [order layer]
                         (let [at (loop [at (count order)]
                                    (if (and (pos? at) (ahead? (nth order (dec at)) layer))
                                      (recur (dec at))
                                      at))]
                           (into (conj (subvec order 0 at) layer) (subvec order at))))
                       [])
               (mapv :id)))))
    ids))

(defn- opens-into-space?
  "Whether a walk of a 3D space goes through `shape` to what it holds: a
  3D space group (see `space-group?`) draws nothing of its own, so what it
  holds sorts with the other layers of the space it is in, as CSS sorts
  the elements of one 3D rendering context. Not one that is hidden, nor
  one whose fills its layers take."
  [shape]
  (and (space-group? shape) (not (:hidden shape)) (empty? (:fills shape))))

(defn space-layers
  "`ids`, layers of a 3D space of `objects`, with each 3D space group
  among them that opens into it (see `opens-into-space?`) replaced by what
  it holds, as deep as they go, in their order, for `depth-sorted`: as
  render-wasm sorts them (`space_layers`). Returns `[layers groups]`, the
  groups gone through too."
  [objects ids]
  (reduce (fn [[layers groups] id]
            (let [shape (get objects id)]
              (if (and (some? shape) (opens-into-space? shape))
                (let [[inner deeper] (space-layers objects (:shapes shape))]
                  [(into layers inner) (into (conj groups id) deeper)])
                [(conj layers id) groups])))
          [[] []]
          ids))

(defn- shown-projection
  "The homography the box of `shape` shows through (see `projection`); of
  a 3D space group, which draws nothing itself, as if it drew, through
  its own tilt and depth too."
  [objects shape]
  (if (space-group? shape)
    (let [[m top] (space-chain objects shape)
          parent  (get objects (:parent-id top))]
      (mul (when (and (some? parent) (not= uuid/zero (:id parent)))
             (projection objects parent))
           (some-> m (seen (perspective-of objects top)))))
    (projection objects shape)))

(defn project-outline
  "`points` of the flat `shape` as they show, tilted (see `projection`):
  nil when nothing tilts it or one reaches the viewer."
  [objects shape points]
  (when-let [h (shown-projection objects shape)]
    (let [projected (mapv #(project-point h %) points)]
      (when (every? some? projected)
        projected))))

(defn projected-points
  "The corners of `shape` as they show, tilted (see `project-outline`)."
  [objects shape]
  (project-outline objects shape (:points shape)))

(defn back-facing?
  "Whether `shape` shows its back: what tilts it (see `projection`)
  turns it over. A flip in the plane is no back."
  [objects shape]
  (boolean
   (when-let [h (projection objects shape)]
     (neg? (det h)))))

;; outlines

(def ^:private curve-steps 12)
(def ^:private ellipse-steps 48)

(defn- ellipse-polyline
  [shape]
  (let [{:keys [x y width height]} (:selrect shape)
        rx  (/ width 2)
        ry  (/ height 2)
        cx  (+ x rx)
        cy  (+ y ry)
        mtx (gtr/transform-matrix shape)]
    {:points (mapv (fn [i]
                     (let [a (/ (* 2 mth/PI i) ellipse-steps)]
                       (gpt/transform (gpt/point (+ cx (* rx (mth/cos a))) (+ cy (* ry (mth/sin a)))) mtx)))
                   (range ellipse-steps))
     :closed? true}))

(defn- path-polylines
  "The subpaths of the path `content` as polylines, their curves in
  `curve-steps` lines."
  [content]
  (let [finish (fn [lines line] (cond-> lines (seq (:points line)) (conj line)))
        [lines line]
        (reduce
         (fn [[lines line] {:keys [command params]}]
           (let [pt (when (some? (:x params)) (gpt/point (:x params) (:y params)))]
             (case command
               :move-to    [(finish lines line) {:points [pt] :closed? false}]
               :line-to    [lines (update line :points (fnil conj []) pt)]
               :curve-to   (let [start (or (peek (:points line)) pt)
                                 h1    (gpt/point (:c1x params) (:c1y params))
                                 h2    (gpt/point (:c2x params) (:c2y params))]
                             [lines (update line :points (fnil into [])
                                            (map #(path.helpers/curve-values start pt h1 h2 (/ % curve-steps)))
                                            (range 1 (inc curve-steps)))])
               :close-path [lines (assoc line :closed? true)]
               [lines line])))
         [[] {:points [] :closed? false}]
         (seq content))]
    (finish lines line)))

(defn outline-polylines
  "The outline of `shape` as it lies flat, polylines of points of the
  page (`{:points :closed?}`): the corners of its box, the points of an
  ellipse, the subpaths of a path."
  [shape]
  (case (:type shape)
    :circle [(ellipse-polyline shape)]
    :path   (path-polylines (:content shape))
    [{:points (vec (:points shape)) :closed? true}]))

(defn projected-outline
  "The outline of `shape` (see `outline-polylines`) as it shows, tilted:
  nil when nothing tilts it or a point reaches the viewer."
  [objects shape]
  (when-let [h (shown-projection objects shape)]
    (let [lines (mapv (fn [line] (update line :points (fn [pts] (mapv #(project-point h %) pts))))
                      (outline-polylines shape))]
      (when (every? (fn [line] (every? some? (:points line))) lines)
        lines))))
