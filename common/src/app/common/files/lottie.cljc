;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.files.lottie
  "The shapes and the timeline of a Lottie (Bodymovin) document, so an
  animation made elsewhere plays and can be edited in Penpot Motion: a
  board with the layers of the composition and their keyframes.

  It reads what the layers draw: the paths, rectangles and ellipses of
  shape layers with their fills, gradients, strokes and trims, images,
  solids, precompositions, null layers and parents, a clip mask, drop
  shadows and blur. It animates the layers, their groups and their
  paints, with the easing and the motion paths of the keyframes. What a
  Penpot board cannot show is left out and counted (see `lottie->shapes`).

  A layer draws where the Lottie draws it at rest: its transform and the
  ones of its groups are baked into the outlines. A layer, group or parent
  that moves is a transparent board centred on its anchor, which carries
  the keyframes: its position as the change from rest, its scale and
  rotation around the anchor, its opacity. The in and out points of a
  layer are held opacity keyframes."
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.color :as clr]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]))

;; --- Values -----------------------------------------------------------------

(defn- animated?
  [prop]
  (and (map? prop) (= 1 (:a prop))))

(defn- keyframes-of
  "The keyframes of an animated `prop`, each with its value `:s` (older
  files give a keyframe only the end value `:e` of the one before)."
  [prop]
  (loop [kfs (seq (:k prop)) prev nil out []]
    (if-let [kf (first kfs)]
      (let [s (if (contains? kf :s) (:s kf) (:e prev))]
        (recur (rest kfs) kf (cond-> out (some? s) (conj (assoc kf :s s)))))
      out)))

(defn- rest-value
  "The value of `prop` at rest: its static value, or its first keyframe."
  [prop default]
  (cond
    (nil? prop)       default
    (animated? prop)  (or (:s (first (keyframes-of prop))) default)
    (map? prop)       (let [k (:k prop)] (if (some? k) k default))
    :else             prop))

(defn- scalar
  [v]
  (let [v (if (sequential? v) (first v) v)]
    (if (number? v) v 0)))

(defn- pair
  [v]
  (if (sequential? v)
    [(scalar (first v)) (scalar (if (> (count v) 1) (second v) (first v)))]
    [(scalar v) (scalar v)]))

(defn- clamp01
  [v]
  (mth/clamp v 0 1))

(defn- rgb01
  "The red, green, blue (and alpha) of a Lottie colour, from 0 to 1."
  [c]
  (let [c (if (sequential? c) (vec c) [0 0 0 1])]
    (if (some #(> % 1) (take 3 c))
      (mapv #(/ % 255) c)
      c)))

(defn- hex
  [c]
  (let [[r g b] (rgb01 c)]
    (clr/rgb->hex [(mth/round (* 255 (clamp01 r)))
                   (mth/round (* 255 (clamp01 g)))
                   (mth/round (* 255 (clamp01 b)))])))

;; --- 2D affine transforms: [a b c d e f] takes x, y to a x + c y + e, b x + d y + f

(defn- mult
  [[a b c d e f] [a' b' c' d' e' f']]
  [(+ (* a a') (* c b'))
   (+ (* b a') (* d b'))
   (+ (* a c') (* c d'))
   (+ (* b c') (* d d'))
   (+ (* a e') (* c f') e)
   (+ (* b e') (* d f') f)])

(defn- move-point
  [[a b c d e f] [x y]]
  [(+ (* a x) (* c y) e)
   (+ (* b x) (* d y) f)])

(defn- turn-vector
  [[a b c d] [x y]]
  [(+ (* a x) (* c y))
   (+ (* b x) (* d y))])

(defn- scale-of
  [[a b c d]]
  (mth/sqrt (mth/abs (- (* a d) (* b c)))))

(defn- translation?
  [[a b c d]]
  (and (mth/close? a 1) (mth/close? d 1) (mth/close? b 0) (mth/close? c 0)))

(defn- trs
  "The Lottie transform: to `p`, turned by `rotation` degrees and scaled
  by `scale` percents around the anchor `a`."
  [[px py] [ax ay] [sx sy] rotation]
  (let [r   (mth/radians rotation)
        cos (mth/cos r)
        sin (mth/sin r)
        sx  (/ sx 100)
        sy  (/ sy 100)
        m   [(* cos sx) (* sin sx) (- (* sin sy)) (* cos sy) 0 0]
        [tx ty] (move-point m [(- ax) (- ay)])]
    [(m 0) (m 1) (m 2) (m 3) (+ tx px) (+ ty py)]))

;; --- Boxes

(def ^:private empty-box
  {:x0 ##Inf :y0 ##Inf :x1 ##-Inf :y1 ##-Inf})

(defn- add-point
  [box [x y]]
  (-> box
      (update :x0 min x) (update :y0 min y)
      (update :x1 max x) (update :y1 max y)))

(defn- merge-boxes
  [a b]
  {:x0 (min (:x0 a) (:x0 b)) :y0 (min (:y0 a) (:y0 b))
   :x1 (max (:x1 a) (:x1 b)) :y1 (max (:y1 a) (:y1 b))})

(defn- finite-box?
  [{:keys [x0 x1]}]
  (and (mth/finite? x0) (mth/finite? x1)))

(defn- selrect-box
  [{:keys [selrect]}]
  {:x0 (:x selrect) :y0 (:y selrect)
   :x1 (+ (:x selrect) (:width selrect)) :y1 (+ (:y selrect) (:height selrect))})

;; --- Outlines

(def ^:private kappa 0.5523)

(defn- subpaths
  "The subpaths (`:v` vertices, `:i` and `:o` tangents relative to them,
  `:c` closed) of an outline item at rest."
  [ctx {:keys [ty] :as item}]
  (case ty
    "sh"
    (do (when (animated? (:ks item))
          (ctx :skip :path-morph))
        (let [k (rest-value (:ks item) nil)
              k (if (sequential? k) (first k) k)]
          (if (seq (:v k)) [k] [])))

    "rc"
    (let [[w h]   (pair (rest-value (:s item) [0 0]))
          [cx cy] (pair (rest-value (:p item) [0 0]))
          x       (- cx (/ w 2))
          y       (- cy (/ h 2))
          r       (max 0 (min (scalar (rest-value (:r item) 0)) (/ w 2) (/ h 2)))
          k       (* r kappa)
          z       [0 0]]
      (if (zero? r)
        [{:c true :v [[x y] [(+ x w) y] [(+ x w) (+ y h)] [x (+ y h)]] :i [z z z z] :o [z z z z]}]
        [{:c true
          :v [[(+ x r) y] [(- (+ x w) r) y] [(+ x w) (+ y r)] [(+ x w) (- (+ y h) r)]
              [(- (+ x w) r) (+ y h)] [(+ x r) (+ y h)] [x (- (+ y h) r)] [x (+ y r)]]
          :i [[(- k) 0] z [0 (- k)] z [k 0] z [0 k] z]
          :o [z [k 0] z [0 k] z [(- k) 0] z [0 (- k)]]}]))

    "el"
    (let [[w h]   (pair (rest-value (:s item) [0 0]))
          [cx cy] (pair (rest-value (:p item) [0 0]))
          rx      (/ w 2)
          ry      (/ h 2)
          kx      (* rx kappa)
          ky      (* ry kappa)]
      [{:c true
        :v [[cx (- cy ry)] [(+ cx rx) cy] [cx (+ cy ry)] [(- cx rx) cy]]
        :i [[(- kx) 0] [0 (- ky)] [kx 0] [0 ky]]
        :o [[kx 0] [0 ky] [(- kx) 0] [0 (- ky)]]}])

    []))

(defn- move-subpath
  [m {:keys [v i o c]}]
  (let [v' (mapv #(move-point m %) v)
        tangent (fn [ts] (mapv (fn [p [tx ty] p']
                                 (let [[qx qy] (move-point m [(+ (p 0) tx) (+ (p 1) ty)])]
                                   [(- qx (p' 0)) (- qy (p' 1))]))
                               v ts v'))]
    {:c (boolean c) :v v' :i (tangent i) :o (tangent o)}))

(defn- subpath-box
  [box {:keys [v i o]}]
  (reduce (fn [box [[x y] [ix iy] [ox oy]]]
            (-> box
                (add-point [x y])
                (add-point [(+ x ix) (+ y iy)])
                (add-point [(+ x ox) (+ y oy)])))
          box
          (map vector v i o)))

(defn- subpath-segments
  [{:keys [v i o c]}]
  (when (seq v)
    (let [n   (count v)
          seg (fn [a b]
                (let [[ax ay]   (nth v a)
                      [bx by]   (nth v b)
                      [oax oay] (nth o a)
                      [ibx iby] (nth i b)]
                  (if (and (zero? oax) (zero? oay) (zero? ibx) (zero? iby))
                    {:command :line-to :params {:x bx :y by}}
                    {:command :curve-to
                     :params {:x bx :y by
                              :c1x (+ ax oax) :c1y (+ ay oay)
                              :c2x (+ bx ibx) :c2y (+ by iby)}})))
          [x y] (first v)]
      (cond-> (into [{:command :move-to :params {:x x :y y}}]
                    (map #(seg (dec %) %))
                    (range 1 n))
        c (conj (seg (dec n) 0) {:command :close-path :params {}})))))

;; --- Paints

(defn- dash-style
  [stroke]
  (cond
    (empty? (:d stroke))                          :solid
    (zero? (scalar (rest-value (:v (first (:d stroke))) 1))) :dotted
    :else                                         :dashed))

(defn- stroke
  [ctx item scale]
  (when (= "gs" (:ty item))
    (ctx :skip :gradient-stroke))
  (let [style (dash-style item)
        cap   (case (:lc item) 2 :round 3 :square nil)]
    (cond-> {:stroke-color     (hex (rest-value (:c item) [0 0 0 1]))
             :stroke-opacity   (clamp01 (/ (scalar (rest-value (:o item) 100)) 100))
             :stroke-width     (* scale (scalar (rest-value (:w item) 1)))
             :stroke-alignment :center
             :stroke-style     style}
      (and (some? cap) (= :solid style))
      (assoc :stroke-cap-start cap :stroke-cap-end cap))))

(defn- fill
  [item box]
  (case (:ty item)
    "fl"
    (let [c (rgb01 (rest-value (:c item) [0 0 0 1]))]
      {:fill-color   (hex c)
       :fill-opacity (clamp01 (* (/ (scalar (rest-value (:o item) 100)) 100) (nth c 3 1)))})

    "gf"
    (let [n        (:p (:g item))
          k        (vec (rest-value (:k (:g item)) []))
          alphas?  (>= (count k) (* n 6))
          stops    (mapv (fn [j]
                           {:offset  (clamp01 (nth k (* j 4) 0))
                            :color   (hex (subvec k (inc (* j 4)) (+ 4 (* j 4))))
                            :opacity (clamp01 (if alphas? (nth k (+ (* n 4) (* j 2) 1) 1) 1))})
                         (range n))
          [sx sy]  (pair (rest-value (:s item) [0 0]))
          [ex ey]  (pair (rest-value (:e item) [0 0]))
          {:keys [x0 y0 x1 y1]} box
          w        (max 1e-6 (- x1 x0))
          h        (max 1e-6 (- y1 y0))]
      {:fill-opacity (clamp01 (/ (scalar (rest-value (:o item) 100)) 100))
       :fill-color-gradient
       {:type    (if (= 2 (:t item)) :radial :linear)
        :start-x (/ (- sx x0) w) :start-y (/ (- sy y0) h)
        :end-x   (/ (- ex x0) w) :end-y   (/ (- ey y0) h)
        :width   1
        :stops   stops}})
    nil))

(def ^:private outline-types #{"sh" "rc" "el" "sr"})
(def ^:private paint-types #{"fl" "gf" "st" "gs"})

(def ^:private unsupported-items
  {"sr" :polystar "rp" :repeater "mm" :merge-paths "rd" :round-corners
   "zz" :zig-zag "pb" :pucker-bloat "tw" :twist "op" :offset-path})

;; --- Nodes of the shapes of a layer

(declare items-nodes)

(defn- group-transform
  [items]
  (d/seek #(= "tr" (:ty %)) items))

(defn- rest-transform
  "The rest matrix of a Lottie transform `tr` (of a layer or a group):
  what moves is at rest where its keyframes start, unscaled and unturned."
  [tr]
  (let [position (:p tr)
        split?   (and (map? position) (true? (:s position)))
        p        (if split?
                   [(scalar (rest-value (:x position) 0)) (scalar (rest-value (:y position) 0))]
                   (pair (rest-value position [0 0])))
        a        (pair (rest-value (:a tr) [0 0]))
        s        (if (animated? (:s tr)) [100 100] (pair (rest-value (:s tr) [100 100])))
        r        (if (animated? (or (:r tr) (:rz tr))) 0 (scalar (rest-value (or (:r tr) (:rz tr)) 0)))]
    {:p p :a a :split? split? :m (trs p a s r)}))

(defn- moves?
  [tr]
  (let [p (:p tr)]
    (or (some animated? [(:s tr) (:r tr) (:rz tr) (:o tr)])
        (animated? p)
        (and (map? p) (true? (:s p)) (or (animated? (:x p)) (animated? (:y p)))))))

(defn- group-nodes
  [ctx group m opacity trim]
  (let [items (vec (remove :hd (:it group)))
        tr    (group-transform items)]
    (if (and (some? tr) (moves? tr))
      (let [{rest-m :m p :p :as rest} (rest-transform tr)
            m' (mult m rest-m)]
        [{:kind     :group
          :name     (:nm group)
          :tr       tr
          :rest     rest
          :parent-m m
          :pivot    (move-point m p)
          :children (items-nodes ctx items m' 1 trim)
          :opacity  (cond-> opacity
                      (not (animated? (:o tr)))
                      (* (/ (scalar (rest-value (:o tr) 100)) 100)))}])
      (let [m' (if (some? tr) (mult m (:m (rest-transform tr))) m)
            o  (if (some? tr) (* opacity (/ (scalar (rest-value (:o tr) 100)) 100)) opacity)]
        (items-nodes ctx items m' o trim)))))

(defn- items-nodes
  "The nodes of the shape `items` of a Lottie layer or group (the top-most
  first), the bottom-most first: a `:drawable` for the outlines and paints
  of a level, a `:group` for a group that moves. `m` takes their outlines
  to the board."
  [ctx items m opacity trim]
  (let [items    (vec (remove :hd items))
        trim     (or (d/seek #(= "tm" (:ty %)) items) trim)
        outlines (filterv #(outline-types (:ty %)) items)
        paints   (filterv #(paint-types (:ty %)) items)
        groups   (filterv #(= "gr" (:ty %)) items)
        _        (doseq [item items]
                   (when-let [feature (unsupported-items (:ty item))]
                     (ctx :skip feature)))
        own      (when (and (seq outlines) (seq paints))
                   (let [paths (into [] (comp (mapcat #(subpaths ctx %))
                                              (map #(move-subpath m %)))
                                     outlines)]
                     (when (seq paths)
                       {:kind     :drawable
                        :outlines outlines
                        :paints   paints
                        :paths    paths
                        :m        m
                        :opacity  opacity
                        :trim     trim
                        :box      (reduce subpath-box empty-box paths)})))
        ;; Lottie draws the end of a list first; the level's own paint
        ;; goes under its groups when it follows them.
        below?   (or (empty? groups)
                     (nil? own)
                     (> (d/index-of items (first paints))
                        (d/index-of items (peek groups))))
        nested   (into [] (mapcat #(group-nodes ctx % m opacity trim)) (rseq groups))]
    (cond
      (nil? own) nested
      below?     (into [own] nested)
      :else      (conj nested own))))

;; --- Shapes

(defn- frame-shape
  "A transparent board `id` centred on `pivot` around `box`, so it scales
  and turns around it like the Lottie layer; or the box of a `clip`."
  [id name parent-id pivot box clip]
  (let [[x y w h] (if (some? clip)
                    [(:x0 clip) (:y0 clip) (- (:x1 clip) (:x0 clip)) (- (:y1 clip) (:y0 clip))]
                    (let [[px py] pivot
                          ok?     (finite-box? box)
                          hw      (max 1 (if ok? (max (- px (:x0 box)) (- (:x1 box) px)) 1))
                          hh      (max 1 (if ok? (max (- py (:y0 box)) (- (:y1 box) py)) 1))]
                      [(- px hw) (- py hh) (* 2 hw) (* 2 hh)]))
        r (or (:r clip) 0)]
    (cts/setup-shape
     (cond-> {:type :frame
              :id id
              :name (or name "Layer")
              :x x :y y :width (max 1 w) :height (max 1 h)
              :parent-id parent-id
              :frame-id parent-id
              :fills []
              :strokes []
              :shapes []
              :show-content (nil? clip)}
       (pos? r) (assoc :r1 r :r2 r :r3 r :r4 r)))))

(defn- drawable-shape
  "The shape of a `:drawable` node: a rectangle or an ellipse while it is
  only moved, else a path."
  [ctx id parent-id {:keys [outlines paints paths m opacity]}]
  (let [single (when (= 1 (count outlines)) (first outlines))
        base   {:id id
                :name (or (:nm single) "Path")
                :parent-id parent-id
                :frame-id parent-id
                :fills []
                :strokes []}
        shape  (cond
                 (and (= "rc" (:ty single)) (translation? m))
                 (let [kf0     (when (animated? (:s single)) (first (keyframes-of (:s single))))
                       [w h]   (pair (if kf0 (:s kf0) (rest-value (:s single) [0 0])))
                       [cx cy] (pair (if (and kf0 (animated? (:p single)))
                                       (:s (first (keyframes-of (:p single))))
                                       (rest-value (:p single) [0 0])))
                       [x y]   (move-point m [(- cx (/ w 2)) (- cy (/ h 2))])
                       r       (max 0 (scalar (rest-value (:r single) 0)))]
                   (cts/setup-shape (assoc base :type :rect :x x :y y
                                           :width (max 0.01 w) :height (max 0.01 h)
                                           :r1 r :r2 r :r3 r :r4 r)))

                 (and (= "el" (:ty single)) (translation? m) (not (animated? (:s single))))
                 (let [[w h]   (pair (rest-value (:s single) [0 0]))
                       [cx cy] (pair (rest-value (:p single) [0 0]))
                       [x y]   (move-point m [(- cx (/ w 2)) (- cy (/ h 2))])]
                   (cts/setup-shape (assoc base :type :circle :x x :y y
                                           :width (max 0.01 w) :height (max 0.01 h))))

                 :else
                 (cts/setup-shape (assoc base :type :path
                                         :content (path/from-plain (into [] (mapcat subpath-segments) paths)))))
        box    (selrect-box shape)
        fills  (into [] (keep #(fill % box)) (filter #(#{"fl" "gf"} (:ty %)) paints))
        strokes (into [] (map #(stroke ctx % (scale-of m))) (filter #(#{"st" "gs"} (:ty %)) paints))]
    (cond-> (assoc shape :fills fills :strokes strokes)
      (< opacity 1) (assoc :opacity opacity))))

(defn- effects
  [ctx layer]
  (reduce (fn [acc {:keys [ty ef]}]
            (let [v (into {} (map (juxt :nm :v)) ef)]
              (case ty
                25 (let [dir  (mth/radians (scalar (rest-value (get v "Direction") 0)))
                         dist (scalar (rest-value (get v "Distance") 0))]
                     (when (some animated? (vals v)) (ctx :skip :animated-effect))
                     (update acc :shadows conj
                             {:id (uuid/next)
                              :style :drop-shadow
                              :offset-x (* dist (mth/sin dir))
                              :offset-y (- (* dist (mth/cos dir)))
                              :blur (/ (scalar (rest-value (get v "Softness") 0)) 2)
                              :spread 0
                              :hidden false
                              :color {:color (hex (rest-value (get v "Shadow Color") [0 0 0 1]))
                                      :opacity (clamp01 (/ (scalar (rest-value (get v "Opacity") 255)) 255))}}))
                29 (assoc acc :blur {:id (uuid/next)
                                     :type :layer-blur
                                     :value (scalar (rest-value (or (get v "Blurriness") (get v "Sigma")) 0))
                                     :hidden false})
                (do (ctx :skip :effect) acc))))
          {:shadows []}
          (:ef layer)))

(defn- mask-box
  "The box of the clip mask of `layer`, taken to the board by `m`, and the
  radius of its corners when it is a rounded rectangle."
  [ctx layer m]
  (let [masks (remove #(= "n" (:mode %)) (:masksProperties layer))]
    (when (seq masks)
      (when (or (> (count masks) 1)
                (not= "a" (:mode (first masks)))
                (:inv (first masks)))
        (ctx :skip :complex-mask))
      (let [pt    (rest-value (:pt (first masks)) nil)
            pt    (if (sequential? pt) (first pt) pt)
            sp    (move-subpath m pt)
            box   (subpath-box empty-box sp)
            top   (->> (:v sp)
                       (filter #(mth/close? (second %) (:y0 box)))
                       (map #(- (first %) (:x0 box)))
                       (filter #(> % 0.5)))]
        (assoc box :r (if (seq top) (reduce min top) 0))))))

;; --- Keyframes

(defn- easing
  [kf]
  (cond
    (= 1 (:h kf))
    {:interpolation :step}

    (and (map? (:o kf)) (map? (:i kf)))
    {:easing {:type :bezier
              :curve [(scalar (:x (:o kf))) (scalar (:y (:o kf)))
                      (scalar (:x (:i kf))) (scalar (:y (:i kf)))]}}

    :else
    {:easing :linear}))

(defn- finite
  [v]
  (if (and (number? v) (mth/finite? v)) v 0))

(defn- add-keyframes
  "Keyframes of `property` of the shape `id` from the Lottie `prop`, their
  value given by `value-fn`."
  ([ctx id prop property value-fn]
   (add-keyframes ctx id prop property value-fn nil))
  ([ctx id prop property value-fn index]
   (when (animated? prop)
     (doseq [kf (keyframes-of prop)]
       (let [value (value-fn (:s kf))]
         (ctx :keyframe id (merge {:property property
                                   :time (ctx :time (:t kf))
                                   :value (if (number? value) (finite value) value)}
                                  (easing kf)
                                  (when (some? index) {:index index}))))))))

(defn- add-position-keyframes
  "Keyframes of `:x` and `:y` of the node `id`, at `rest-x`, `rest-y` when
  the Lottie position `p` is at `rest-p`; `m` takes changes of it to the
  board. The tangents of a joint position are motion paths."
  [ctx id p rest-p m rest-x rest-y]
  (let [[rx ry] rest-p
        delta   (fn [dx dy] (turn-vector m [dx dy]))]
    (cond
      (and (map? p) (true? (:s p)))
      (do (add-keyframes ctx id (:x p) :x #(+ rest-x (first (delta (- (scalar %) rx) 0))))
          (add-keyframes ctx id (:y p) :y #(+ rest-y (second (delta 0 (- (scalar %) ry))))))

      (animated? p)
      (let [kfs (keyframes-of p)]
        (doseq [[j kf] (d/enumerate kfs)]
          (let [[x y]   (pair (:s kf))
                [dx dy] (delta (- x rx) (- y ry))
                prev    (when (pos? j) (nth kfs (dec j)))
                [ox oy] (when (sequential? (:to kf)) (delta (scalar (first (:to kf))) (scalar (second (:to kf)))))
                [ix iy] (when (and prev (sequential? (:ti prev)))
                          (delta (scalar (first (:ti prev))) (scalar (second (:ti prev)))))
                base    (merge {:time (ctx :time (:t kf))} (easing kf))
                handles (fn [out in]
                          (cond-> {}
                            (and (some? out) (not (zero? out)) (< j (dec (count kfs)))) (assoc :path-out (finite out))
                            (and (some? in) (not (zero? in))) (assoc :path-in (finite in))))]
            (ctx :keyframe id (merge base {:property :x :value (finite (+ rest-x dx))} (handles ox ix)))
            (ctx :keyframe id (merge base {:property :y :value (finite (+ rest-y dy))} (handles oy iy)))))))))

(defn- add-transform-keyframes
  "The keyframes of the Lottie transform `tr` (of a layer or a group) on
  the node `id`: see `add-position-keyframes`."
  [ctx id tr rest parent-m node-x node-y]
  (add-keyframes ctx id (:o tr) :opacity #(clamp01 (/ (scalar %) 100)))
  (add-keyframes ctx id (:s tr) :scale-x #(max 0.001 (/ (first (pair %)) 100)))
  (add-keyframes ctx id (:s tr) :scale-y #(max 0.001 (/ (second (pair %)) 100)))
  (add-keyframes ctx id (or (:r tr) (:rz tr)) :rotation scalar)
  (add-position-keyframes ctx id (:p tr) (:p rest) parent-m node-x node-y))

(defn- add-paint-keyframes
  "Keyframes of the fills, strokes and trim of the shape of a drawable."
  [ctx id {:keys [paints trim]}]
  (doseq [[index item] (d/enumerate (filter #(#{"fl" "gf"} (:ty %)) paints))]
    (when (= "fl" (:ty item))
      (add-keyframes ctx id (:c item) :fill-color hex index)
      (add-keyframes ctx id (:o item) :fill-opacity #(clamp01 (/ (scalar %) 100)) index)))
  (let [strokes (filter #(#{"st" "gs"} (:ty %)) paints)]
    (doseq [[index item] (d/enumerate strokes)]
      (add-keyframes ctx id (:c item) :stroke-color hex index)
      (add-keyframes ctx id (:o item) :stroke-opacity #(clamp01 (/ (scalar %) 100)) index)
      (add-keyframes ctx id (:w item) :stroke-width scalar index))
    (when (and (some? trim) (seq strokes))
      (add-keyframes ctx id (:s trim) :trim-start #(clamp01 (/ (scalar %) 100)))
      (add-keyframes ctx id (:e trim) :trim-end #(clamp01 (/ (scalar %) 100)))
      (add-keyframes ctx id (:o trim) :trim-offset #(/ (scalar %) 360))
      (when (and (not (animated? (:s trim))) (not (animated? (:e trim)))
                 (or (not (zero? (scalar (rest-value (:s trim) 0))))
                     (not= 100 (scalar (rest-value (:e trim) 100)))))
        (ctx :skip :static-trim)))))

;; --- Layers

(declare comp-layers)

(defn- relative-x
  [ctx shape]
  (- (:x (:selrect shape)) (ctx :origin-x)))

(defn- relative-y
  [ctx shape]
  (- (:y (:selrect shape)) (ctx :origin-y)))

(defn- add-size-keyframes
  "Width and height keyframes of a rectangle whose size the Lottie
  animates."
  [ctx id shape {:keys [outlines]}]
  (let [single (when (= 1 (count outlines)) (first outlines))]
    (when (and (= :rect (:type shape)) (animated? (:s single)))
      (add-keyframes ctx id (:s single) :width #(max 0.01 (first (pair %))))
      (add-keyframes ctx id (:s single) :height #(max 0.01 (second (pair %)))))))

(defn- add-nodes
  "Add the shapes of `nodes` (see `items-nodes`) to the board `parent-id`,
  the bottom-most first."
  [ctx parent-id nodes]
  (doseq [node nodes]
    (if (= :group (:kind node))
      (let [id    (uuid/next)
            slot  (ctx :reserve)
            _     (add-nodes ctx id (:children node))
            board (cond-> (frame-shape id (:name node) parent-id (:pivot node) (ctx :box-since slot) nil)
                    (< (:opacity node) 1) (assoc :opacity (:opacity node)))]
        (ctx :place slot board)
        (add-transform-keyframes ctx id (:tr node) (:rest node) (:parent-m node)
                                 (relative-x ctx board) (relative-y ctx board)))
      (let [id    (uuid/next)
            shape (drawable-shape ctx id parent-id node)]
        (ctx :add shape)
        (add-paint-keyframes ctx id node)
        (add-size-keyframes ctx id shape node)))))

(defn- layer-shape-nodes
  "The nodes (see `items-nodes`) of what `layer` draws itself, `m` taking it
  to the board."
  [ctx layer m]
  (case (:ty layer)
    4 (items-nodes ctx (:shapes layer) m 1 nil)
    1 (items-nodes ctx [{:ty "rc" :s {:k [(:sw layer) (:sh layer)]}
                         :p {:k [(/ (:sw layer) 2) (/ (:sh layer) 2)]} :r {:k 0}}
                        {:ty "fl" :c {:k (let [c (clr/hex->rgb (or (:sc layer) "#000000"))]
                                           (conj (mapv #(/ % 255) c) 1))}
                         :o {:k 100}}]
                   m 1 nil)
    []))

(defn- span?
  "Whether `layer` shows only from its in point to its out point, not all
  the time of its composition."
  [ctx layer]
  (let [{:keys [comp-ip comp-op]} (ctx :span)]
    (or (> (:ip layer comp-ip) comp-ip)
        (< (:op layer comp-op) comp-op))))

(defn- add-span-keyframes
  "Held opacity keyframes of the shape `id` that show it, at `opacity`, only
  from the in point of `layer` to its out point."
  [ctx id layer opacity]
  (let [{:keys [comp-ip comp-op]} (ctx :span)
        ip (:ip layer comp-ip)
        op (:op layer comp-op)]
    (when (> ip comp-ip)
      (ctx :keyframe id {:property :opacity :time 0 :value 0 :interpolation :step}))
    (ctx :keyframe id {:property :opacity :time (ctx :time (max ip comp-ip)) :value opacity
                       :interpolation :step})
    (when (< op comp-op)
      (ctx :keyframe id {:property :opacity :time (ctx :time op) :value 0 :interpolation :step}))))

(defn- centred?
  [{:keys [box]} [px py]]
  (let [{:keys [x0 y0 x1 y1]} box]
    (and (mth/close? (/ (+ x0 x1) 2) px 0.5)
         (mth/close? (/ (+ y0 y1) 2) py 0.5))))

(defn- with-effects
  [shape {:keys [shadows blur]}]
  (cond-> shape
    (seq shadows) (assoc :shadow shadows)
    (some? blur)  (assoc :blur blur)))

(defn- add-own-drawing
  "Add what `layer` draws itself to the board `parent-id`: its shapes, its
  image or its precomposition, `m` taking it to the board."
  [ctx layer own-nodes parent-id m]
  (add-nodes ctx parent-id own-nodes)
  (case (:ty layer)
    2 (if-let [image (ctx :image (:refId layer))]
        (let [[x y] (move-point m [0 0])
              k     (scale-of m)]
          (ctx :add (cts/setup-shape
                     {:type :rect :id (uuid/next) :name (or (:nm layer) "Image")
                      :parent-id parent-id :frame-id parent-id
                      :x x :y y :width (* k (:w image)) :height (* k (:h image))
                      :fills [{:fill-opacity 1
                               :fill-image {:id (:id image) :width (:width image)
                                            :height (:height image) :mtype (:mtype image)
                                            :keep-aspect-ratio false}}]
                      :strokes []})))
        (ctx :skip :missing-image))
    0 (comp-layers ctx (:refId layer) parent-id m (+ (ctx :offset) (:st layer 0)))
    nil))

(defn- inner-roles
  "What only the drawing of a layer does, as boards inside its node, the
  outer-most first, and what the node itself does of it (`on-node`): the
  time it shows (`:span`), its opacity, its clip mask and its effects.
  With `parent?` all of them go inside, so the layers it parents do not
  fade nor hide with it."
  [{:keys [parent? span? fades? opacity clip? effects?]}]
  (let [on-node (when-not parent?
                  (cond span? :span
                        (or fades? (< opacity 1)) :opacity))
        roles   (cond-> []
                  (and span? (not= :span on-node))
                  (conj :span)

                  ;; a span shows a static opacity itself
                  (and (or fades? (and (< opacity 1) (not span?)))
                       (not= :opacity on-node))
                  (conj :opacity)

                  clip?
                  (conj :clip))
        ;; the effects of a parent do not reach its children either
        roles   (cond-> roles (and effects? parent? (empty? roles)) (conj :effects))]
    {:on-node on-node :roles roles}))

(defn- add-layer
  "Add the shapes of `layer` (with the layers it parents, `children`) to
  the board `parent-id`; `parent-m` takes its parent's space to the board.

  The layer's node, a board centred on its anchor (or, for a single shape
  that turns or scales only around its middle, the shape), carries the
  keyframes of its transform. What only its drawing does goes on boards
  inside it when needed (see `inner-roles`); a clip keeps to its own box."
  [ctx layer children parent-id parent-m]
  (let [tr        (:ks layer)
        rest      (rest-transform tr)
        m         (mult parent-m (:m rest))
        pivot     (move-point parent-m (:p rest))
        fx        (effects ctx layer)
        effects?  (boolean (or (seq (:shadows fx)) (some? (:blur fx))))
        clip      (when (:hasMask layer) (mask-box ctx layer m))
        opacity   (clamp01 (/ (scalar (rest-value (:o tr) 100)) 100))
        fades?    (animated? (:o tr))
        span?     (span? ctx layer)
        parent?   (boolean (seq children))
        own-nodes (layer-shape-nodes ctx layer m)
        single    (when (and (= 1 (count own-nodes)) (= :drawable (:kind (first own-nodes))))
                    (first own-nodes))
        turns?    (or (animated? (:s tr)) (animated? (or (:r tr) (:rz tr))))
        direct?   (and (some? single) (not parent?) (nil? clip) (not effects?)
                       (not (and span? fades?))
                       (or (not turns?) (centred? single pivot)))]
    (if direct?
      (let [id    (uuid/next)
            shape (drawable-shape ctx id parent-id single)
            shape (cond-> (assoc shape :name (or (:nm layer) (:name shape)))
                    (and (not fades?) (not span?) (< opacity 1))
                    (assoc :opacity (* opacity (:opacity shape 1))))]
        (ctx :add shape)
        (add-paint-keyframes ctx id single)
        (add-size-keyframes ctx id shape single)
        (add-transform-keyframes ctx id tr rest parent-m (relative-x ctx shape) (relative-y ctx shape))
        (when span? (add-span-keyframes ctx id layer opacity)))

      (let [id      (uuid/next)
            slot    (ctx :reserve)
            {:keys [on-node roles]}
            (inner-roles {:parent? parent? :span? span? :fades? fades? :opacity opacity
                          :clip? (some? clip) :effects? effects?})
            boards  (loop [roles roles parent-id id acc []]
                      (if-let [role (first roles)]
                        (let [board {:role role :id (uuid/next) :parent-id parent-id :slot (ctx :reserve)}]
                          (recur (rest roles) (:id board) (conj acc board)))
                        acc))
            drawing (or (:id (peek boards)) id)]
        (add-own-drawing ctx layer own-nodes drawing m)
        (doseq [[child grandchildren] children]
          (add-layer ctx child grandchildren id m))

        ;; the boards inside the node, the inner-most first: each around
        ;; what it holds
        (doseq [{:keys [role id parent-id slot]} (rseq boards)]
          (let [board (frame-shape id (str (or (:nm layer) "Layer") " " (name role))
                                   parent-id pivot (ctx :box-since slot)
                                   (when (= :clip role) clip))
                board (cond-> board
                        (= role (peek roles))
                        (with-effects fx)

                        (and (= :opacity role) (not fades?))
                        (assoc :opacity opacity))]
            (ctx :place slot board)
            (case role
              :span    (add-span-keyframes ctx id layer
                                           (if (or fades? (some #{:opacity} roles)) 1 opacity))
              :opacity (add-keyframes ctx id (:o tr) :opacity #(clamp01 (/ (scalar %) 100)))
              nil)))

        (let [node (frame-shape id (:nm layer) parent-id pivot (ctx :box-since slot) nil)
              node (cond-> node
                     (empty? roles)
                     (with-effects fx)

                     (and (= :opacity on-node) (not fades?))
                     (assoc :opacity opacity))]
          (ctx :place slot node)
          (add-transform-keyframes ctx id (cond-> tr (not= :opacity on-node) (dissoc :o))
                                   rest parent-m (relative-x ctx node) (relative-y ctx node))
          (when (= :span on-node)
            (add-span-keyframes ctx id layer (if fades? 1 opacity))))))))

(def ^:private unsupported-layers
  {5 :text 6 :audio 7 :video-placeholder 13 :camera 15 :data})

(defn- layer-tree
  "The layers of a composition as a tree by their parents, each a pair of
  the layer and its children, the bottom-most first."
  [layers]
  (let [by-ind   (into {} (keep (fn [l] (when (some? (:ind l)) [(:ind l) l]))) layers)
        children (group-by :parent (filter #(contains? by-ind (:parent %)) layers))
        node     (fn node [layer seen]
                   (if (contains? seen (:ind layer))
                     [layer []]
                     [layer (mapv #(node % (conj seen (:ind layer)))
                                  (reverse (get children (:ind layer))))]))]
    (->> (reverse layers)
         (remove #(contains? by-ind (:parent %)))
         (mapv #(node % #{})))))

(defn- drawn?
  [ctx layer]
  (cond
    (:hd layer) false
    (:td layer) (do (ctx :skip :matte) false)
    (contains? unsupported-layers (:ty layer)) (do (ctx :skip (unsupported-layers (:ty layer))) false)
    :else (do (when (:tt layer) (ctx :skip :matte))
              (when (:tm layer) (ctx :skip :time-remap))
              (when (and (some? (:sr layer)) (not= 1 (:sr layer))) (ctx :skip :time-stretch))
              (when (= 1 (:ddd layer)) (ctx :skip :three-d))
              true)))

(defn- prune
  [ctx tree]
  (into []
        (keep (fn [[layer children]]
                (let [children (prune ctx children)]
                  (when (or (drawn? ctx layer) (seq children))
                    [layer children]))))
        tree))

(defn- comp-layers
  "Add the layers of the precomposition `ref-id` (the document's own ones
  for nil) to the board `parent-id`, `m` taking its space to the board,
  its times `offset` frames later."
  [ctx ref-id parent-id m offset]
  (let [layers (if (nil? ref-id)
                 (:layers (ctx :doc))
                 (:layers (ctx :comp ref-id)))]
    (when (and (some? ref-id) (nil? layers))
      (ctx :skip :missing-precomp))
    (ctx :with-offset offset
         (fn []
           (doseq [[layer children] (prune ctx (layer-tree layers))]
             (add-layer ctx layer children parent-id m))))))

;; --- Document

(defn images
  "The image assets of the Lottie document `doc` the board needs, as maps
  with their `:id`, their size (`:w`, `:h`), and either the data URI of
  an embedded one (`:data-uri`) or the path of a file (`:path`)."
  [doc]
  (into []
        (keep (fn [{:keys [id w h u p e] :as asset}]
                (when (and (string? p) (not (contains? asset :layers)))
                  (cond-> {:id id :w w :h h}
                    (or (= 1 e) (re-find #"^data:" p)) (assoc :data-uri p)
                    (not (or (= 1 e) (re-find #"^data:" p))) (assoc :path (str (or u "") p))))))
        (:assets doc)))

(defn lottie->shapes
  "The shapes and the timeline of the Lottie document `doc` (parsed JSON
  with keyword keys): a board `id` named `name` with its top left corner
  at `x`, `y`, and the shapes inside it, parents before their children
  and the bottom-most first, ready to be added in that order; and the
  timeline of the board. `images` maps the id of an image asset to the
  uploaded media object (`:id`, `:width`, `:height`, `:mtype`) with its
  Lottie size (`:w`, `:h`). `:skipped` counts what a board cannot show,
  by feature."
  [doc {:keys [id name x y images background]}]
  (let [id       (or id (uuid/next))
        fr       (let [fr (:fr doc)] (if (and (number? fr) (pos? fr)) fr 30))
        comp-ip  (scalar (:ip doc))
        comp-op  (max (inc comp-ip) (scalar (:op doc)))
        width    (max 1 (scalar (:w doc)))
        height   (max 1 (scalar (:h doc)))
        comps    (into {} (keep (fn [a] (when (:layers a) [(:id a) a]))) (:assets doc))
        state    (volatile! {:shapes [] :keyframes [] :skipped {} :offset 0})
        time     (fn [frame]
                   (-> (/ (* 1000 (- (+ frame (:offset @state)) comp-ip)) fr)
                       (mth/round)
                       (max 0)
                       (int)))
        ;; what was added after a slot is what the board of the slot holds
        box-since (fn [slot]
                    (transduce (comp (remove nil?) (map selrect-box))
                               (completing merge-boxes)
                               empty-box
                               (subvec (:shapes @state) (inc slot))))
        ctx      (fn ctx [op & args]
                   (case op
                     :skip        (vswap! state update-in [:skipped (first args)] (fnil inc 0))
                     :keyframe    (vswap! state update :keyframes conj args)
                     :add         (vswap! state update :shapes conj (first args))
                     :reserve     (let [slot (count (:shapes @state))]
                                    (vswap! state update :shapes conj nil)
                                    slot)
                     :place       (vswap! state assoc-in [:shapes (first args)] (second args))
                     :box-since   (box-since (first args))
                     :time        (time (first args))
                     :image       (get images (first args))
                     :comp        (get comps (first args))
                     :doc         doc
                     :span        {:comp-ip comp-ip :comp-op comp-op}
                     :offset      (:offset @state)
                     :with-offset (let [[offset f] args
                                        before     (:offset @state)]
                                    (vswap! state assoc :offset offset)
                                    (f)
                                    (vswap! state assoc :offset before))
                     :origin-x    x
                     :origin-y    y))
        board    (cts/setup-shape
                  {:type :frame
                   :id id
                   :name (or name (:nm doc) "Lottie")
                   :x x :y y :width width :height height
                   :parent-id uuid/zero
                   :frame-id uuid/zero
                   :fills (if (some? background) [{:fill-color background :fill-opacity 1}] [])
                   :strokes []
                   :shapes []})]
    (comp-layers ctx nil id [1 0 0 1 x y] 0)
    (let [{:keys [shapes keyframes skipped]} @state
          timeline (reduce (fn [timeline [shape-id kf]]
                             (cta/add-keyframe timeline shape-id kf))
                           (cta/make-timeline {:board-id id
                                               :name (or (:nm doc) name "Lottie")
                                               :duration (time comp-op)
                                               :playback :loop})
                           keyframes)
          timeline (reduce (fn [timeline {:keys [tm cm]}]
                             (cta/add-marker timeline {:time (time (scalar tm)) :name (str cm)}))
                           timeline
                           (:markers doc))]
      {:shapes   (into [board] (remove nil?) shapes)
       :timeline (assoc timeline :duration (time comp-op))
       :skipped  skipped})))
