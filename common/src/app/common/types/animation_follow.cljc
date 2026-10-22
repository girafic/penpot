;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation-follow
  "How a timeline plays its layers that follow others (see
  `cta/follow-of`), the chains that reach a goal (two-bone IK, see
  `cta/ik-of`) and the layers that ride the outline of a shape (see
  `cta/path-follow-of`): the rules of these, the modifiers of a timeline
  at a time with them (`timeline->modif-tree`), which the editor
  preview, the viewer playback and the exports read, and their bake into
  keyframes (`bake-follows`). Their data (the `:follow`, `:ik` and
  `:path-follow` of a track) and the interpolation of keyframes are in
  `app.common.types.animation`."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.path-walk :as gpw]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.modifiers :as ctm]))

(defn- own-modifiers
  "`{shape-id modifiers}` of the shapes of `objects` that `timeline` (as
  it plays, see `cta/resolve-animations`) animates, at `time`."
  [timeline objects time]
  (reduce-kv
   (fn [acc shape-id values]
     (if-let [shape (get objects shape-id)]
       (let [board-origin (cta/position-origin timeline objects shape-id)
             track        (cta/get-track timeline shape-id)
             pivot        (cta/track-origin track)]
         (assoc acc shape-id (cta/shape->modifiers shape values board-origin pivot track)))
       acc))
   {}
   (cta/values-at timeline time)))

(defn- moved-by
  "The layers the layer `id` of `timeline` moves with: the leader it
  follows and the shape whose outline it rides."
  [timeline id]
  (let [track (dm/get-in timeline [:tracks id])]
    (keep identity [(dm/get-in track [:follow :leader]) (dm/get-in track [:path-follow :path])])))

(defn- cyclic?
  "Whether the layer `id` of `timeline` moves with itself, through the
  leaders and the paths of the layers it moves with (see `moved-by`): a
  cycle plays nothing."
  [timeline id]
  (loop [front (moved-by timeline id), seen #{}]
    (if-let [at (first front)]
      (cond
        (= at id)           true
        (contains? seen at) (recur (rest front) seen)
        :else               (recur (into (vec (rest front)) (moved-by timeline at)) (conj seen at)))
      false)))

(defn followers
  "The layers of `timeline` that follow a leader of `objects` (see
  `cta/follow-of`), but those that move with themselves through their
  leaders and the paths they ride (a cycle follows nothing, see
  `cyclic?`) and those in their leader or holding it, which move with it
  already."
  [timeline objects]
  (let [leader-of (fn [id] (dm/get-in timeline [:tracks id :follow :leader]))]
    (into #{}
          (filter #(and (contains? objects %)
                        (contains? objects (leader-of %))
                        (not (cyclic? timeline %))
                        (not (cta/nested-layers? objects % (leader-of %)))))
          (keys (:tracks timeline)))))

(defn riders
  "The layers of `timeline` that ride the outline of a shape of `objects`
  (see `cta/path-follow-of`), but those that move with themselves (see
  `cyclic?`), those in their path or holding it, and those whose path
  has no outline (see `gpw/walker`)."
  [timeline objects]
  (into #{}
        (filter (fn [id]
                  (let [path (dm/get-in timeline [:tracks id :path-follow :path])]
                    (and (contains? objects id)
                         (contains? objects path)
                         (not= id path)
                         (not (cyclic? timeline id))
                         (not (cta/nested-layers? objects id path))
                         (some? (gpw/walker (get objects path)))))))
        (keep (fn [[id track]] (when (some? (:path-follow track)) id)) (:tracks timeline))))

(defn- ik-links*
  "`{:a :b}`, the links of the chain of two follows that ends at `id` in
  `timeline`, `follows` its followers (see `followers`): `id` follows
  `:b` and `:b` follows `:a`, each at once (no delay), taking where its
  leader's pivot goes and how it turns, as joints do. Nil without such a
  chain."
  [timeline follows id]
  (let [joint (fn [id]
                (when (contains? follows id)
                  (let [{:keys [leader position rotation delay]} (cta/follow-of timeline id)]
                    (when (and position rotation (zero? delay))
                      leader))))]
    (when-let [b (joint id)]
      (when-let [a (joint b)]
        {:a a :b b}))))

(defn ik-links
  "`{:a :b}`, the links of the chain of two follows that ends at the
  layer `id` of `timeline` (of `objects`), see `cta/schema:ik`; nil without
  such a chain."
  [timeline objects id]
  (ik-links* timeline (followers timeline objects) id))

(defn- ik-verdicts
  "`{effector [problem chain]}` for each layer of `timeline` (of
  `objects`) with a goal, `follows` its followers (see `followers`): its
  chain (see `ik-chains`) when it plays, else the first rule it breaks
  (see `ik-problem`)."
  [timeline objects follows]
  (let [board-id (:board-id timeline)
        layers   (juxt :a :b :e)
        verdict  (fn [id goal]
                   (if-let [{:keys [a b]} (ik-links* timeline follows id)]
                     (if (and (contains? objects goal)
                              (cta/inside? objects goal board-id)
                              (not (contains? #{a b id} goal))
                              (not-any? #(cta/nested-layers? objects goal %) [a b id]))
                       [nil (merge {:a a :b b :e id} (cta/ik-of timeline id))]
                       [:goal nil])
                     [:chain nil]))
        verdicts (into {}
                       (keep (fn [[id track]]
                               (when-let [goal (dm/get-in track [:ik :goal])]
                                 [id (verdict id goal)])))
                       (:tracks timeline))
        placed   (into {} (keep (fn [[id [problem chain]]] (when (nil? problem) [id chain]))) verdicts)
        counts   (frequencies (mapcat layers (vals placed)))
        shared?  (fn [chain] (not-every? #(= 1 (get counts %)) (layers chain)))
        unshared (into {} (remove (comp shared? val)) placed)
        goal-of  (into {}
                       (mapcat (fn [chain] (map (fn [id] [id (:goal chain)]) (layers chain))))
                       (vals unshared))
        leader-of #(when (contains? follows %) (dm/get-in timeline [:tracks % :follow :leader]))
        ;; whether `from` leads to one of `targets`, through follows and goals
        reaches?  (fn [from targets]
                    (loop [front [from] seen #{}]
                      (if-let [id (peek front)]
                        (cond
                          (contains? targets id) true
                          (contains? seen id)    (recur (pop front) seen)
                          :else                  (recur (cond-> (pop front)
                                                          (some? (leader-of id)) (conj (leader-of id))
                                                          (some? (goal-of id))   (conj (goal-of id)))
                                                        (conj seen id)))
                        false)))]
    (reduce-kv (fn [verdicts id chain]
                 (assoc verdicts id
                        (cond
                          (shared? chain)                                 [:shared nil]
                          (reaches? (:goal chain) (set (layers chain)))   [:loop nil]
                          :else                                           [nil chain])))
               verdicts
               placed)))

(defn ik-chains
  "The chains of `timeline` (of `objects`) whose end reaches a goal (see
  `cta/schema:ik`) as they play: `{effector {:a :b :e :goal :bend :turn}}`,
  `:a` and `:b` its links (see `ik-links`). A goal must be another layer
  of the board, out of the chain, neither in a layer of it nor holding
  one; a layer may be in one chain only; and no goal may be led by a
  layer of its chain, through follows and the goals of other chains.
  The others play without reaching, every frame, as two edits from two
  places may make a loop together (see `ik-problem`). `follows`, its
  followers (see `followers`), when the caller has them."
  ([timeline objects]
   (ik-chains timeline objects (followers timeline objects)))
  ([timeline objects follows]
   (into {}
         (keep (fn [[id [problem chain]]] (when (nil? problem) [id chain])))
         (ik-verdicts timeline objects follows))))

(defn ik-problem
  "Why the layer `id` of `timeline` (of `objects`) reaches its goal (see
  `cta/schema:ik`) no more as it plays: it ends no chain of two links that
  follow at once (`:chain`, see `ik-links`), its goal is off the board,
  in its chain or in or around one of its links (`:goal`), a link of its
  chain is in another chain too (`:shared`), or its goal is led by its
  chain, through follows and the goals of other chains (`:loop`). Nil
  when it reaches, or has no goal."
  [timeline objects id]
  (first (get (ik-verdicts timeline objects (followers timeline objects)) id)))

(defn- modifiers-value
  "The value of `prop` `modifiers` give a shape, `default` when they give
  none."
  [modifiers prop default]
  (or (some (fn [{:keys [type property value]}]
              (when (and (= :change-property type) (= prop property)) value))
            (rseq (vec (:structure-parent (or modifiers (ctm/empty))))))
      default))

(defn- modifiers-opacity
  "The opacity `modifiers` give a shape, `default` when they give none."
  [modifiers default]
  (modifiers-value modifiers :opacity default))

(defn- follow-tilt
  "`mods` of the layer `shape` (its track `track`) with the change of the
  tilt of its leader `leader` since `start` (the modifiers it counts
  from, nil for where it rests) to `played` (see
  `app.common.geom.tilt`): its own tilt and that change, around its own
  pivot."
  [mods shape track leader played start]
  (let [tilt   (fn [modifiers shape property]
                 (modifiers-value modifiers property (or (get shape property) 0)))
        dx     (- (tilt played leader :rotate-x) (tilt start leader :rotate-x))
        dy     (- (tilt played leader :rotate-y) (tilt start leader :rotate-y))]
    (if (and (mth/almost-zero? dx) (mth/almost-zero? dy))
      mods
      (let [{:keys [x y]} (cta/track-origin track)]
        (-> mods
            (ctm/change-property :rotate-x (+ (tilt mods shape :rotate-x) dx))
            (ctm/change-property :rotate-y (+ (tilt mods shape :rotate-y) dy))
            (ctm/change-property :tilt-pivot {:x x :y y
                                              :rest-x (or (:rotate-x shape) 0)
                                              :rest-y (or (:rotate-y shape) 0)}))))))

(defn- change-turn
  "The turn (degrees) nearest to the linear part of the matrix `change`,
  a mirror taken out first (along x), so a mirrored leader still turns
  its follower: none for a stretch along turned axes, which only skews."
  [{:keys [a b c d]}]
  (if (neg? (- (* a d) (* b c)))
    (mth/degrees (mth/atan2 (- (- b) c) (- d a)))
    (mth/degrees (mth/atan2 (- b c) (+ a d)))))

(defn- change-least-scale
  "The least scale of the matrix `change` along any axis (its smaller
  singular value): near 0 when it squashes a layer flat along one."
  [{:keys [a b c d]}]
  (let [f   (+ (* a a) (* b b) (* c c) (* d d))
        det (- (* a d) (* b c))]
    (mth/sqrt (max 0 (/ (- f (mth/sqrt (max 0 (- (* f f) (* 4 det det))))) 2)))))

(defn- unscaled
  "The matrix `m` without its scale: its turn (see `change-turn`) around
  `pivot`, which goes where `m` takes it."
  [m pivot]
  (let [turn (mth/radians (change-turn m))
        cos  (mth/cos turn)
        sin  (mth/sin turn)
        to   (gpt/transform pivot m)]
    (gmt/matrix cos sin (- sin) cos
                (- (:x to) (- (* cos (:x pivot)) (* sin (:y pivot))))
                (- (:y to) (+ (* sin (:x pivot)) (* cos (:y pivot)))))))

(defn- change-scale
  "The even scale of the matrix `change`: the one that keeps its area."
  [{:keys [a b c d]}]
  (mth/sqrt (mth/abs (- (* a d) (* b c)))))

(defn- follow-modifiers
  "The modifiers that make a layer take `change`, what its leader does
  since its first frame (a matrix), around `pivot`, where the pivot of
  the leader is then: as `follow` says, it scales evenly and turns
  around that pivot (the nearest turn and the scale of the area: a
  stretch of the leader is left out, as no shape could take it) and
  moves as the pivot does."
  [{:keys [position rotation scale]} change pivot]
  (let [s     (change-scale change)
        turn  (change-turn change)
        moved (gpt/transform pivot change)]
    (cond-> (ctm/empty)
      (and scale (not (mth/close? s 1)))
      (-> (ctm/resize (gpt/point s s) pivot)
          (ctm/scale-content s))

      (and rotation (not (mth/close? turn 0)))
      (ctm/rotation pivot turn)

      position
      (ctm/move (gpt/subtract moved pivot)))))

(defn- follow-chain
  "The layers `follows` of `timeline` and the leaders they follow and the
  paths they ride (see `moved-by`), through a chain of them."
  [timeline follows]
  (loop [ids (set follows), front follows]
    (let [leaders (into #{} (mapcat #(moved-by timeline %)) front)
          new     (remove ids leaders)]
      (if (empty? new)
        ids
        (recur (into ids new) new)))))

;; The modifiers of the follow chains of the timelines played (see
;; `followed-modifiers`) at their first frame, which every frame reads:
;; `[objects modifiers]` by timeline, gone with a timeline no longer
;; played. Playback runs in the browser only.
#?(:cljs
   (defonce ^:private first-frame-cache
     (js/WeakMap.)))

(defn- first-frame-modifiers
  "The modifiers of the layers of `chain`, a timeline of the follow
  chains of the timeline `key` (see `follow-chain`), at its first
  frame: the same as on the frame before while `key` and `objects` are."
  [key objects chain]
  #?(:cljs
     (let [[objs modifiers] (when (some? key) (.get first-frame-cache key))]
       (if (and (some? objs) (identical? objs objects))
         modifiers
         (let [modifiers (own-modifiers chain objects 0)]
           (when (some? key)
             (.set first-frame-cache key [objects modifiers]))
           modifiers)))
     :clj
     (do key (own-modifiers chain objects 0))))

(defn- normal-turn
  "`degrees` as a turn from -180 to 180."
  [degrees]
  (- (mod (+ degrees 180) 360) 180))

(defn- two-bone-turns
  "How the links of a chain of two (see `cta/schema:ik`) turn so that its end
  reaches `g`: `s`, `e` and `w` the pivots of its upper link, of its
  lower link and of its end as they are, `bend` the side the elbow
  goes. `{:upper :lower}` turns (degrees) around the shoulder `s` and
  then around the elbow where it goes (`:elbow`), the end then at
  `:reach`, `g` or the nearest point out of reach. Nil for a link or
  a distance of (almost) nothing, which no turn solves."
  [s e w g bend]
  (let [l1 (gpt/distance s e)
        l2 (gpt/distance e w)
        d  (gpt/distance s g)]
    (when (and (> l1 1e-6) (> l2 1e-6) (> d 1e-6))
      (let [d*     (mth/clamp d (mth/abs (- l1 l2)) (+ l1 l2))
            cos-a  (mth/clamp (/ (- (+ (* l1 l1) (* d* d*)) (* l2 l2)) (* 2 l1 d*)) -1 1)
            angle  #(mth/degrees (mth/atan2 (- (:y %2) (:y %1)) (- (:x %2) (:x %1))))
            toward (angle s g)
            ;; Penpot turns clockwise for positive angles
            elbow* (+ toward (* (if (= :cw bend) 1 -1) (mth/degrees (mth/acos cos-a))))
            upper  (normal-turn (- elbow* (angle s e)))
            at     #(gpt/add s (gpt/scale (gpt/point (mth/cos (mth/radians %1)) (mth/sin (mth/radians %1))) %2))
            elbow  (at elbow* l1)
            reach  (at toward d*)
            lower  (normal-turn (- (angle elbow reach) (angle elbow (gpt/rotate w s upper))))]
        {:upper upper :lower lower :elbow elbow :reach reach}))))

(defn- followed-modifiers
  "`own` (see `own-modifiers`) with the layers of `timeline` that follow
  a leader (see `cta/follow-of`) moved as it: what the leader does from its
  first frame on, or from where it rests (`:from` `:rest`), `:delay` ms
  later, after their own animation; a chain of followers follows
  through. Opacity multiplies by the leader's, as in a group. The delay
  counts in the time played: in ping-pong, on the way back (`back?`,
  see `cta/playback-back?`) a follower is where its leader was on its way
  back, or there, a moment before. Before its delay a follower waits at
  the leader's first frame, or, when `timeline` loops or plays
  ping-pong, takes the end of the round before. A leader whose
  first frame squashes it to (almost) nothing along an axis, as a pop
  in from 0, counts its scale from rest (its place and turn from that
  frame).

  The links of a chain whose end reaches a goal (see `ik-chains`) turn
  so that it does (see `two-bone-turns`), from the pose they play
  without it (`:fk`), and what follows them goes along: a follow counts
  from where its leader is at its first frame with no reaching at all
  (`:none`), so a turn there moves its followers too.

  A layer that rides the outline of a shape (see `riders`) has its pivot
  where its progress along the outline as it plays then puts it, turned
  along the outline when it orients, its own turn added; what follows it
  goes along.

  The other times play the follow chains, goals and paths only, their
  first frame read once while `key` (the timeline the caller keeps,
  before it is resolved) stays."
  [timeline objects time own & {:keys [key back?]}]
  (let [follows (followers timeline objects)
        rides   (riders timeline objects)]
    (if (and (empty? follows) (empty? rides))
      own
      (let [duration   (:duration timeline 0)
            mode       (cta/playback-mode timeline)
            ping-pong? (and (= :ping-pong mode) (pos? duration))
            ;; the time played in a round: ping-pong goes there and back
            round      (if ping-pong? (* 2 duration) duration)
            wraps?     (and (contains? #{:loop :ping-pong} mode) (pos? duration))
            fold       (fn [u] (if (and ping-pong? (> u duration)) (- round u) u))
            now        (if (and ping-pong? back?) (- round time) time)
            earlier    (fn [u delay]
                         (let [u (- u delay)]
                           (cond (>= u 0) u
                                 wraps?   (mod u round)
                                 :else    0)))
            chains     (ik-chains timeline objects follows)
            chain-of   (into {}
                             (mapcat (fn [chain] (map (fn [id] [id chain]) [(:a chain) (:b chain) (:e chain)])))
                             (vals chains))
            goals      (into #{} (map :goal) (vals chains))
            chain      (update timeline :tracks select-keys (follow-chain timeline (into follows (concat goals rides))))
            ;; the progress of the riders along their paths
            ridden     (update timeline :tracks select-keys rides)
            own-at     (memoize #(if (zero? %)
                                   (first-frame-modifiers key objects chain)
                                   (own-modifiers chain objects %)))
            matrix     #(if (some? %) (ctm/modifiers->transform %) (gmt/matrix))
            rested     (fn [id] (cta/origin-point (get objects id) (cta/track-origin (cta/get-track timeline id))))
            cache      (volatile! {})
            cached     (fn [k f]
                         (if (contains? @cache k)
                           (get @cache k)
                           (let [v (f)] (vswap! cache assoc k v) v)))]
        (letfn [;; the modifiers of the layer `id` at `u`, the time played in
                ;; the round: as it plays (`:all`), without the reaching of
                ;; its chain (`:fk`) or with no reaching at all (`:none`)
                (played [id u mode]
                  (let [mode (cond (empty? chains)                                  :all
                                   (and (= :fk mode) (not (contains? chain-of id))) :all
                                   :else                                            mode)]
                    (cached
                     [id u mode]
                     (fn []
                       (if (and (= :all mode) (contains? chain-of id))
                         (reached id u (played id u :fk))
                         (let [mods (cond->> (get (if (= u now) own (own-at (fold u))) id)
                                      (contains? rides id) (ride id u))]
                           (if-not (contains? follows id)
                             mods
                             (let [follow  (cta/follow-of timeline id)
                                   leader  (:leader follow)
                                   lmode   (if (and (= :fk mode) (not= (chain-of leader) (chain-of id))) :all mode)
                                   lplayed (played leader (earlier u (:delay follow)) lmode)
                                   at      (matrix lplayed)
                                   ;; from where it rests, as in a group,
                                   ;; or from its first frame with no
                                   ;; reaching; a first frame scaled to
                                   ;; (almost) nothing holds no change to
                                   ;; count from
                                   lshape  (get objects leader)
                                   lpivot  (rested leader)
                                   first   (matrix (played leader 0 :none))
                                   start   (cond
                                             (= :rest (:from follow)) (gmt/matrix)
                                             ;; squashed flat: its place and
                                             ;; turn count, not its scale
                                             (< (change-least-scale first) 0.01) (unscaled first lpivot)
                                             :else first)
                                   change  (if-let [inv (gmt/inverse start)] (gmt/multiply at inv) at)
                                   pivot   (gpt/transform lpivot start)
                                   moved   (ctm/add-modifiers mods (follow-modifiers follow change pivot))]
                               (cond-> moved
                                 (:tilt follow)
                                 (follow-tilt (get objects id) (cta/get-track timeline id) lshape lplayed
                                              (when-not (= :rest (:from follow))
                                                (or (played leader 0 :none) (ctm/empty))))

                                 (:opacity follow)
                                 (ctm/change-property
                                  :opacity
                                  (* (modifiers-opacity mods (:opacity (get objects id) 1))
                                     (modifiers-opacity lplayed (:opacity lshape 1)))))))))))))

                ;; `mods` of the layer `id` riding its path at `u`: its pivot
                ;; at its progress along the outline as it plays then
                (ride [id u mods]
                  (let [{:keys [path orient turn progress]} (cta/path-follow-of timeline id)
                        pmods (played path u :all)
                        walk  (gpw/walker (cond-> (get objects path)
                                            (some? pmods) (gsh/transform-shape pmods)))]
                    (if (nil? walk)
                      mods
                      (let [values (cached [::ridden u] #(cta/values-at ridden (fold u)))
                            p      (or (dm/get-in values [id :path-progress]) progress)
                            ;; round again past the end of a closed outline
                            share  (if (:closed? walk) (- p (mth/floor p)) (mth/clamp p 0 1))
                            {:keys [point angle]} ((:at walk) share)
                            mods   (or mods (ctm/empty))
                            pivot  (gpt/transform (rested id) (matrix mods))]
                        (-> mods
                            (cond-> orient (ctm/add-modifiers (ctm/rotation nil pivot (+ angle turn))))
                            (ctm/add-modifiers (ctm/move nil (gpt/subtract point pivot))))))))

                ;; where the layer `id` turns around at `u`, played in `mode`
                (pivot-at [id u mode]
                  (gpt/transform (rested id) (matrix (played id u mode))))

                ;; how the chain ending at `e` turns at `u` (see `two-bone-turns`)
                (solved [{:keys [a b e goal bend]} u]
                  (cached
                   [::solved e u]
                   (fn []
                     (let [shoulder (pivot-at a u :fk)
                           wrist    (pivot-at e u :fk)]
                       (some-> (two-bone-turns shoulder (pivot-at b u :fk) wrist (pivot-at goal u :all) bend)
                               (assoc :shoulder shoulder :wrist wrist))))))

                ;; `mods` of the layer `id` of a chain, as it plays without
                ;; reaching, with the turns that make it reach at `u`
                (reached [id u mods]
                  (let [{:keys [a e goal turn] :as chain} (chain-of id)]
                    (if-let [{:keys [upper lower shoulder elbow reach wrist]} (solved chain u)]
                      (let [upper-turn (ctm/rotation nil shoulder upper)]
                        (cond
                          (= id a)
                          (ctm/add-modifiers mods upper-turn)

                          (or (not= id e) (= :chain turn))
                          (-> (ctm/add-modifiers mods upper-turn)
                              (ctm/add-modifiers (ctm/rotation nil elbow lower)))

                          :else
                          (cond-> (ctm/add-modifiers mods (ctm/move nil (gpt/subtract reach wrist)))
                            (= :goal turn)
                            (ctm/add-modifiers (ctm/rotation nil reach (change-turn (matrix (played goal u :all))))))))
                      mods)))]
          (reduce (fn [acc id]
                    (if-let [mods (played id now :all)]
                      (assoc acc id mods)
                      acc))
                  own
                  (-> follows (into (keys chain-of)) (into rides))))))))

(defn timeline->modif-tree
  "Compute a transient modif-tree `{shape-id {:modifiers ...}}` for
  `timeline` at `time`, covering transforms (x/y/scale/rotation), opacity
  and the other looks (as change-property modifiers), and the poses of
  the units of the texts that split animations play one by one
  (`:text-units`, see `cta/text-unit-entries`): there at every time, `[]` at
  rest, so that every renderer clears a pose. Designed to be fed to
  `set-modifiers` / `set-wasm-modifiers` (editor preview) or applied
  through `transform-shape` (viewer/export). With `back?`, `time` is on
  the way back of ping-pong (see `cta/playback-back?`), where the layers
  that follow others lag behind their leaders as they go back."
  [timeline objects time & {:keys [back?]}]
  (let [resolved (cta/resolve-animations timeline objects)
        own      (own-modifiers resolved objects time)
        ;; the timeline as given stays the same from frame to frame
        played   (followed-modifiers resolved objects time own :key timeline :back? back?)
        tree     (reduce-kv (fn [tree shape-id modifiers]
                              (cond-> tree
                                (not (ctm/empty? modifiers))
                                (assoc shape-id {:modifiers modifiers})))
                            {}
                            played)]
    (reduce-kv (fn [tree shape-id entries]
                 (update-in tree [shape-id :modifiers] ctm/change-property :text-units entries))
               tree
               (cta/text-unit-entries resolved objects time))))

(defn natural-bend
  "The side the chain of two follows that ends at the layer `id` of
  `timeline` (see `ik-links`) is drawn bent to at its first frame, as it
  plays without reaching: `:cw` when its elbow is clockwise of the line
  from its shoulder to its wrist (the pivots of its links and of `id`),
  else `:ccw`, a straight chain too. Nil without such a chain."
  [timeline objects id]
  (when-let [{:keys [a b]} (ik-links timeline objects id)]
    (let [free  (update timeline :tracks d/update-vals #(dissoc % :ik))
          tree  (timeline->modif-tree free objects 0)
          pivot (fn [x]
                  (let [mods  (dm/get-in tree [x :modifiers])
                        point (cta/origin-point (get objects x) (cta/track-origin (cta/get-track timeline x)))]
                    (cond-> point
                      (some? mods) (gpt/transform (ctm/modifiers->transform mods)))))
          s     (pivot a)
          e     (pivot b)
          w     (pivot id)
          cross (- (* (- (:x e) (:x s)) (- (:y w) (:y s)))
                   (* (- (:y e) (:y s)) (- (:x w) (:x s))))
          ;; the sine of the bend: straight within a tenth of a degree
          sine  (/ cross (max 1e-9 (* (gpt/distance s e) (gpt/distance s w))))]
      (if (< sine -2e-3) :cw :ccw))))

;; --- Baking follows ------------------------------------------------------

(def ^:private bake-tolerances
  "How far the keyframes a follow bakes into (see `bake-follows`) may miss
  it: a quarter pixel or degree, a little scale and opacity."
  {:x 0.25 :y 0.25 :rotation 0.25 :scale-x 0.002 :scale-y 0.002 :opacity 0.004
   :rotate-x 0.25 :rotate-y 0.25})

(defn- thin-samples
  "The fewest of `samples` (`[time value]`, by time) that linear
  keyframes through them pass within `tol` of all the others: the first
  and last, and recursively the one missed most (Ramer-Douglas-Peucker)."
  [samples tol]
  (let [v     (vec samples)
        n     (count v)
        inner (fn inner [i j]
                (let [[ti vi] (nth v i)
                      [tj vj] (nth v j)
                      [k dev] (reduce (fn [[_ most :as acc] k]
                                        (let [[tk vk] (nth v k)
                                              f       (if (= ti tj) 0 (/ (- tk ti) (- tj ti)))
                                              miss    (mth/abs (- vk (+ vi (* f (- vj vi)))))]
                                          (if (> miss most) [k miss] acc)))
                                      [nil 0]
                                      (range (inc i) j))]
                  (if (and (some? k) (> dev tol))
                    (concat (inner i k) [k] (inner k j))
                    [])))]
    (if (<= n 2)
      v
      (mapv v (concat [0] (inner 0 (dec n)) [(dec n)])))))

(defn- unwrapped
  "`angles` (degrees) moved by whole turns to be nearest the one before
  each: a turn past 180 degrees goes on, not back."
  [angles]
  (reduce (fn [acc a]
            (if-let [prev (peek acc)]
              (conj acc (+ a (* 360 (mth/round (/ (- prev a) 360)))))
              (conj acc a)))
          []
          angles))

(defn- followed-pose
  "The values of `values`' properties that show the follower `id` of
  `timeline` as it plays with its follow: its own `values` at a time,
  `own` its own modifiers then and `played` those with its follow (see
  `followed-modifiers`). What the follow adds since its own pose turns
  and scales it (the nearest, for a skew) and moves its pivot."
  [timeline objects id values own played]
  (let [shape   (get objects id)
        origin  (cta/position-origin timeline objects id)
        origin* (cta/track-origin (cta/get-track timeline id))
        pivot   (cta/origin-point shape origin*)
        matrix  #(if (some? %) (ctm/modifiers->transform %) (gmt/matrix))
        total   (matrix played)
        follow  (if-let [inv (gmt/inverse (matrix own))] (gmt/multiply total inv) total)
        scale   (change-scale follow)
        rest-x  (cta/rest-value shape :x origin)
        rest-y  (cta/rest-value shape :y origin)
        values  (-> (merge {:rotation (cta/rest-value shape :rotation origin) :scale-x 1 :scale-y 1} values)
                    (update :rotation + (change-turn follow))
                    (update :scale-x * scale)
                    (update :scale-y * scale)
                    (assoc :x rest-x :y rest-y))
        ;; where its pivot is without a move, and where it goes
        probe   (gpt/transform pivot (matrix (cta/shape->modifiers shape (dissoc values :content) origin origin*)))
        target  (gpt/transform pivot total)]
    (cond-> (assoc values
                   :x (+ rest-x (- (:x target) (:x probe)))
                   :y (+ rest-y (- (:y target) (:y probe)))
                   :opacity (modifiers-opacity played (:opacity shape 1))
                   :rotate-x (modifiers-value played :rotate-x (or (:rotate-x shape) 0))
                   :rotate-y (modifiers-value played :rotate-y (or (:rotate-y shape) 0)))
      ;; shrunk to nothing (a leader flipping through 0), it has no turn
      (< scale 1e-6) (assoc :rotation nil))))

(defn- unridden
  "`track` riding no path (see `cta/schema:path-follow`), and without the
  keyframes of its progress along one, which no export plays."
  [track]
  (let [progress? #(= :path-progress (:property %))]
    (cond-> (dissoc track :path-follow)
      (some progress? (:keyframes track)) (update :keyframes (partial filterv (complement progress?)))
      (some? (:loops track))              (update :loops disj :path-progress))))

(defn- baked-track
  "The track of the follower `id` of `timeline` playing its `poses` (see
  `followed-pose`, one at each of `times`) as linear keyframes, from its
  own track as it plays: the properties the poses change, or that it
  animated, get the keyframes that keep within `bake-tolerances` of them."
  [timeline objects id times poses]
  (let [shape    (get objects id)
        origin   (cta/position-origin timeline objects id)
        track    (cta/get-track timeline id)
        animated (into #{} (map :property) (:keyframes track))
        at-rest  {:x (cta/rest-value shape :x origin)
                  :y (cta/rest-value shape :y origin)
                  :rotation (cta/rest-value shape :rotation origin)
                  :scale-x 1 :scale-y 1
                  :opacity (:opacity shape 1)
                  :rotate-x (or (:rotate-x shape) 0)
                  :rotate-y (or (:rotate-y shape) 0)}
        baked    (into {}
                       (keep (fn [property]
                               (let [tol     (get bake-tolerances property)
                                     ;; the times a pose has a value for it
                                     samples (filterv (comp some? second) (map vector times (map property poses)))
                                     values  (cond-> (mapv second samples)
                                               (= :rotation property) unwrapped)]
                                 (when (and (seq values)
                                            (or (contains? animated property)
                                                (some #(> (mth/abs (- % (get at-rest property))) tol) values)))
                                   [property (thin-samples (map vector (map first samples) values) tol)]))))
                       (keys bake-tolerances))]
    (-> (unridden track)
        (dissoc :follow :ik)
        (update :keyframes (fn [keyframes]
                             (->> (remove #(and (contains? baked (:property %)) (nil? (:index %))) keyframes)
                                  (concat (for [[property samples] baked
                                                [time value] samples]
                                            (cta/make-keyframe {:time time :property property :value value
                                                                :easing :linear})))
                                  (cta/sort-keyframes)
                                  (vec))))
        (d/update-when :loops #(into #{} (remove (fn [k] (contains? baked k))) %)))))

(defn follows?
  "Whether a layer of `timeline` follows another (see `cta/follow-of`) or
  rides the outline of a shape (see `cta/path-follow-of`)."
  [timeline]
  (boolean (some #(or (:follow %) (:path-follow %)) (vals (:tracks timeline)))))

(defn- round-trip
  "The ping-pong `timeline` as the loop of its way there and back (see
  `cta/expand-ping-pong`), what it plays turned into keyframes first; the
  split animations of its texts keep its clock (see
  `cta/text-unit-entries`)."
  [timeline objects]
  (let [clock (select-keys timeline [:duration :playback])]
    (-> timeline
        (cta/resolve-animations objects)
        (cta/expand-loops)
        (cta/expand-paths)
        (cta/expand-springs)
        (cta/cut-timeline)
        (cta/expand-ping-pong)
        (update :tracks d/update-vals
                (fn [track]
                  (cond-> track
                    (and (seq (:animations track)) (nil? (:split-clock track)))
                    (assoc :split-clock clock)))))))

(defn bake-follows
  "`timeline` with its follows (see `cta/follow-of`) played by keyframes of
  the followers, for the exports that read keyframes: the pose of each
  follower `fps` times a second through the duration, as the canvas
  shows it (see `timeline->modif-tree`), kept where linear keyframes
  between the others would miss it by more than a quarter pixel or
  degree. The tracks of the followers come as they play (see
  `cta/resolve-animations`), the others as they are. A ping-pong timeline
  turns into the loop of its way there and back first (see
  `round-trip`), where the followers lag behind both ways. A follow
  that plays nothing (a cycle, a leader gone) is left out. The layers
  that ride a path bake so too, their position and turn, and none keeps
  its path or the keyframes of its progress. The layers
  `keep?` is true of keep their follows, as the Lottie export plays
  some by parenting (see `app.common.animation.lottie/parent-candidates`)."
  [timeline objects & {:keys [fps keep?] :or {fps 60 keep? (constantly false)}}]
  (if-not (follows? timeline)
    timeline
    (let [timeline (cond-> timeline
                     (= :ping-pong (cta/playback-mode timeline)) (round-trip objects))
          resolved (cta/resolve-animations timeline objects)
          chains   (vals (ik-chains resolved objects))
          ;; the followers, and the upper links of chains that reach
          follows  (into #{} (remove keep?) (concat (followers resolved objects) (map :a chains)
                                                    (riders resolved objects)))
          ids      (follow-chain resolved (into follows (map :goal) chains))
          played   (update resolved :tracks select-keys ids)
          times    (cta/frame-times (:duration timeline) fps)
          frames   (mapv (fn [t]
                           (let [own (own-modifiers played objects t)]
                             {:values (cta/values-at played t)
                              :own    own
                              :played (followed-modifiers played objects t own :key played)}))
                         times)
          baked    (into {}
                         (map (fn [id]
                                [id (baked-track played objects id times
                                                 (mapv (fn [{:keys [values own played]}]
                                                         (followed-pose resolved objects id (get values id)
                                                                        (get own id) (get played id)))
                                                       frames))]))
                         follows)]
      (update timeline :tracks
              (fn [tracks]
                (as-> tracks tracks
                  (reduce-kv (fn [tracks id track]
                               (let [track (or (get baked id)
                                               (cond-> (unridden track) (not (keep? id)) (dissoc :follow :ik)))]
                                 (if (cta/empty-track? track)
                                   (dissoc tracks id)
                                   (assoc tracks id track))))
                             tracks
                             tracks)
                  ;; an upper link that only turns around its centre
                  (reduce-kv (fn [tracks id track]
                               (cond-> tracks
                                 (and (not (contains? tracks id)) (not (cta/empty-track? track)))
                                 (assoc id (assoc track :shape-id id))))
                             tracks
                             baked)))))))
