;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.logic.timelines
  "Keep the animation of layers (see `app.common.types.animation`) with
  them when they are duplicated, pasted or moved to another board. The
  timelines of a page are keyed by board and keyframe positions are
  relative to the board (see `cta/position-origin`), so a track goes to
  the timeline of the board its layer ends up in, its positions moved by
  as much as the layer moved in its board."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.geom.modifiers :as gm]
   [app.common.geom.shapes :as gsh]
   [app.common.types.animation :as cta]
   [app.common.types.component :as ctk]
   [app.common.types.components-list :as ctkl]
   [app.common.types.modifiers :as ctm]
   [app.common.uuid :as uuid]))

(defn- in-board
  "Where `shape-id` is in the board `board-id`: the origin of its keyframe
  positions. A board is relative to the canvas."
  [objects shape-id board-id]
  (let [origin (when (not= shape-id board-id)
                 (dm/get-in objects [board-id :selrect]))]
    {:x (- (dm/get-in objects [shape-id :selrect :x] 0) (:x origin 0))
     :y (- (dm/get-in objects [shape-id :selrect :y] 0) (:y origin 0))}))

(defn- offset
  [from to]
  {:x (- (:x to) (:x from))
   :y (- (:y to) (:y from))})

(defn find-track
  "`[board-id track]`: the board whose timeline animates `shape-id`, and
  the track."
  [timelines shape-id]
  (some (fn [[board-id timeline]]
          (when-let [track (dm/get-in timeline [:tracks shape-id])]
            [board-id track]))
        timelines))

(defn- settings
  "A timeline without its board and tracks."
  [timeline]
  (dissoc timeline :board-id :tracks))

(defn animation-sources
  "What copying the animation of the shapes of `ids` of `page` (with its
  `objects`) takes: their tracks, with where the shapes are in their
  board, and the settings of the timelines of the boards among them."
  [page objects ids]
  (let [timelines (:timelines page)]
    {:tracks
     (into {}
           (keep (fn [id]
                   (when-let [[board-id track] (find-track timelines id)]
                     [id {:board board-id
                          :track track
                          :at (in-board objects id board-id)}])))
           ids)
     :settings
     (into {}
           (keep (fn [id]
                   (when-let [timeline (get timelines id)]
                     [id (settings timeline)])))
           ids)}))

(defn- moved-track
  "`track` for `shape-id`, its positions moved by `delta`. A copy gets
  new keyframe and animation ids."
  [track shape-id {dx :x dy :y} copy?]
  (let [new-id #(cond-> % copy? (assoc :id (uuid/next)))]
    (-> track
        (assoc :shape-id shape-id)
        (update :keyframes
                (partial mapv (fn [keyframe]
                                (cond-> (new-id keyframe)
                                  (= :x (:property keyframe)) (update :value + dx)
                                  (= :y (:property keyframe)) (update :value + dy)))))
        (d/update-when :animations (partial mapv new-id)))))

(defn- track-end
  [track]
  (reduce max 0 (concat (map :time (:keyframes track))
                        (map cta/animation-end (:animations track)))))

(defn- add-track
  "`timelines` with `track` in the timeline of `board-id`, a new one with
  `settings` when the board has none. The duration grows to fit it."
  [timelines board-id track settings]
  (let [timeline (or (get timelines board-id)
                     (merge (cta/make-timeline {:board-id board-id}) settings))]
    (assoc timelines board-id
           (-> timeline
               (assoc :board-id board-id)
               (assoc-in [:tracks (:shape-id track)] track)
               (update :duration max (track-end track))))))

(defn- commit-timelines
  "Changes turning the timelines of `page` into `timelines`."
  [changes page timelines]
  (let [old (:timelines page)]
    (reduce (fn [changes board-id]
              (let [timeline (get timelines board-id)]
                (cond-> changes
                  (not= timeline (get old board-id))
                  (pcb/change-timeline board-id timeline))))
            (pcb/with-page changes page)
            (into (set (keys old)) (keys timelines)))))

(defn generate-copy-timelines
  "Give the copies of `ids-map` (old id -> new id) the animation of their
  originals, `sources` (see `animation-sources`); `changes` holds the
  copies on `page`. A copy outside of any board (an alt-drag duplicate
  before it is dropped) keeps the board of its original when
  `keep-board?`."
  [changes page ids-map {:keys [tracks settings]} & {:keys [keep-board?]}]
  (if (empty? tracks)
    changes
    (let [objects   (pcb/get-objects changes)
          timelines
          (reduce-kv
           (fn [timelines old-id {:keys [board track at]}]
             (let [new-id    (get ids-map old-id)
                   new-board (when (contains? objects new-id)
                               (or (cfh/get-shape-id-root-frame objects new-id)
                                   (when keep-board? board)))]
               (if (nil? new-board)
                 timelines
                 (add-track timelines new-board
                            (moved-track track new-id
                                         (offset at (in-board objects new-id new-board))
                                         true)
                            (or (get settings (d/seek #(= new-board (get ids-map %)) (keys settings)))
                                (get settings board))))))
           (:timelines page)
           tracks)]
      (commit-timelines changes page timelines))))

(defn generate-move-timelines
  "Keep the animation of the shapes of `ids` (and their children) moved
  to another board: their tracks go to its timeline, their positions
  following, as the shapes stay where they are on the canvas. A board
  moved into another one gives its tracks to it. `changes` holds the
  move; `objects` are the shapes before it."
  [changes page objects ids]
  (let [moved     (pcb/get-objects changes)
        timelines (:timelines page)
        all-ids   (into [] (mapcat #(cfh/get-children-ids-with-self moved %)) ids)
        result
        (reduce
         (fn [timelines id]
           (let [[board track] (find-track timelines id)
                 new-board     (cfh/get-shape-id-root-frame moved id)]
             ;; A shape left on the canvas, out of any board, keeps its
             ;; track for when it joins one again.
             (if (or (nil? track) (nil? new-board) (= board new-board))
               timelines
               (-> timelines
                   (d/update-in-when [board :tracks] dissoc id)
                   (add-track new-board
                              (moved-track track id
                                           (offset (in-board objects id board)
                                                   (in-board moved id new-board))
                                           false)
                              (settings (get timelines board)))))))
         timelines
         all-ids)

        result
        (reduce (fn [timelines id]
                  (cond-> timelines
                    (and (contains? timelines id)
                         (not (cfh/root-frame? moved id)))
                    (dissoc id)))
                result
                ids)]
    (cond-> changes
      (not= result timelines)
      (commit-timelines page result))))

(defn main-page-of
  "A function giving, for a component head, `{:objects :timelines}` of the
  page of the main of its component, one of `files` (by id)."
  [files]
  (fn [head]
    (let [fdata (dm/get-in files [(:component-file head) :data])]
      (when-let [component (ctkl/get-component fdata (:component-id head))]
        (when-let [page (dm/get-in fdata [:pages-index (:main-instance-page component)])]
          {:objects (:objects page) :timelines (:timelines page)})))))

(defn- copy-heads
  "Ids of the top component copies in the board `board-id`, itself
  included: the heads that refer to a main, not inside another one."
  [objects board-id]
  (letfn [(walk [id]
            (when-let [shape (get objects id)]
              (if (and (ctk/instance-head? shape) (some? (:shape-ref shape)))
                [id]
                (mapcat walk (:shapes shape)))))]
    (walk board-id)))

(defn- head-of
  "The nearest component head at or above `id` in `objects`."
  [objects id]
  (loop [id id, depth 0]
    (when-let [shape (get objects id)]
      (cond
        (ctk/instance-head? shape) shape
        (or (> depth 64) (= id uuid/zero) (= id (:parent-id shape))) nil
        :else (recur (:parent-id shape) (inc depth))))))

(defn- ratio
  [a b]
  (if (and (number? a) (number? b) (pos? b)) (/ a b) 1))

(defn- copy-map
  "How absolute positions in the main `main` map to its copy `copy`:
  `{:sx :sy :ox :oy}`, a position `p` going to `o + p × s`, scaled as
  much as the copy is."
  [copy main]
  (let [c  (:selrect copy)
        m  (:selrect main)
        sx (ratio (:width c) (:width m))
        sy (ratio (:height c) (:height m))]
    {:sx sx
     :sy sy
     :ox (- (:x c 0) (* sx (:x m 0)))
     :oy (- (:y c 0) (* sy (:y m 0)))}))

(defn- then-map
  "The map (see `copy-map`) applying `inner`, then `outer`."
  [outer inner]
  {:sx (* (:sx outer) (:sx inner))
   :sy (* (:sy outer) (:sy inner))
   :ox (+ (:ox outer) (* (:sx outer) (:ox inner)))
   :oy (+ (:oy outer) (* (:sy outer) (:oy inner)))})

(def ^:private identity-map
  {:sx 1 :sy 1 :ox 0 :oy 0})

(defn- origin
  "Where the keyframe positions of `shape-id` in the board `board-id`
  are counted from: the board for its layers, the canvas for itself."
  [objects shape-id board-id]
  (if (= shape-id board-id)
    {:x 0 :y 0}
    (let [rect (dm/get-in objects [board-id :selrect])]
      {:x (:x rect 0) :y (:y rect 0)})))

(defn- main-track
  "What animates the copy `shape` under the copy head `head` from a main:
  following what it refers to (`:shape-ref`) from main to main, a
  component in a component included, the first of them that has a track.
  `{:board :timeline :track :ref-id :objects :map}`, `:map` taking the
  absolute positions of that main to the copy (see `copy-map`); nil when
  none of them is animated."
  [shape head main-page]
  (loop [shape shape, head head, cmap identity-map, depth 0]
    (let [page   (when (< depth 8) (main-page head))
          mobjs  (:objects page)
          ref-id (:shape-ref shape)
          main   (get mobjs (:shape-ref head))]
      (when (and (some? page) (some? ref-id) (some? main))
        (let [cmap (then-map cmap (copy-map head main))]
          (if-let [[board track] (find-track (:timelines page) ref-id)]
            {:board board
             :timeline (get (:timelines page) board)
             :track track
             :ref-id ref-id
             :objects mobjs
             :map cmap}
            (let [ref       (get mobjs ref-id)
                  next-head (when (some? (:shape-ref ref))
                              (head-of mobjs ref-id))]
              (when (some? next-head)
                (recur ref next-head cmap (inc depth))))))))))

(defn- mapped-track
  "`track` of `ref-id` (of `main-objects`, in its board `main-board`) for
  `shape-id` (of `objects`, in the board `board-id`), its positions and
  sizes taken there by `cmap` (see `main-track`)."
  [track ref-id main-objects main-board shape-id objects board-id {:keys [sx sy ox oy]}]
  (let [from (origin main-objects ref-id main-board)
        to   (origin objects shape-id board-id)
        bx   (- (+ ox (* sx (:x from))) (:x to))
        by   (- (+ oy (* sy (:y from))) (:y to))
        move (fn [keyframe scale offset]
               (-> keyframe
                   (update :value #(+ offset (* scale %)))
                   (d/update-when :path-in * scale)
                   (d/update-when :path-out * scale)))]
    (-> track
        (assoc :shape-id shape-id)
        (update :keyframes
                (partial mapv (fn [keyframe]
                                (case (:property keyframe)
                                  :x      (move keyframe sx bx)
                                  :y      (move keyframe sy by)
                                  :width  (update keyframe :value * sx)
                                  :height (update keyframe :value * sy)
                                  keyframe)))))))

(defn instance-timelines
  "The timelines playing, in the board `board-id` of `objects`, the
  animations of the component copies in it as their mains play them: the
  tracks of the shapes the copies refer to (`:shape-ref`) in the pages of
  their mains, which `main-page` gives for a component head as
  `{:objects :timelines}`, through a component in a component too (see
  `main-track`), taken to where the copies are in the board and scaled as
  much as they are. One by copy and main board, keyed `[:copy id board]`,
  with the settings of the timeline of that main, so that each plays on
  its own clock (see `cta/playback-time`). A shape `timeline`, the one of
  the board, animates itself keeps that animation."
  [objects board-id timeline main-page]
  (let [own (:tracks timeline)]
    (reduce
     (fn [result head-id]
       (let [head (get objects head-id)]
         (reduce
          (fn [result id]
            (if-let [{:keys [board track ref-id] main :timeline mobjs :objects cmap :map}
                     (when-not (contains? own id)
                       (main-track (get objects id) head main-page))]
              (let [k [:copy head-id board]]
                (-> result
                    (update k #(or % (assoc (settings main) :board-id board-id :tracks {})))
                    (assoc-in [k :tracks id]
                              (mapped-track track ref-id mobjs board id objects board-id cmap))))
              result))
          result
          (cfh/get-children-ids-with-self objects head-id))))
     {}
     (copy-heads objects board-id))))

(defn copy-timelines
  "The timelines of the component copies in the board `board-id` (see
  `instance-timelines`), for the pages with `objects` and `timelines`;
  `files` holds the files of the components, by id."
  [objects timelines files board-id]
  (instance-timelines objects board-id (get timelines board-id) (main-page-of files)))

(defn with-copies-clock
  "`timeline`, of a board, keeping time with the animations of the
  component copies in it, `copies` (see `instance-timelines`), while it
  has no tracks itself: as long as the longest of them, or as the board
  when it is `stored?` (its timeline was saved, with the duration set on
  it) and longer, looping when one of them does."
  [timeline copies stored?]
  (if (or (seq (:tracks timeline)) (empty? copies))
    timeline
    (let [longest (reduce max 1 (map cta/cycle-duration copies))]
      (assoc timeline
             :duration (if stored? (max longest (:duration timeline)) longest)
             :playback (if (every? #(= :once (cta/playback-mode %)) copies) :once :loop)))))

(defn with-instance-tracks
  "`timeline`, of a board, with the animations of the component copies
  in it, `copies` (see `instance-timelines`), for the exports, which read
  one timeline: each as it plays on its own clock, over and over when it
  loops (see `cta/loop-track`). A shape `timeline` animates itself keeps
  that animation."
  [timeline copies objects]
  (reduce
   (fn [timeline copy]
     (let [copy   (-> copy (cta/resolve-animations objects) cta/expand-loops)
           ;; ping-pong turns into a loop of there and back
           copy   (cond-> copy
                    (= :ping-pong (cta/playback-mode copy))
                    (-> cta/expand-paths cta/expand-springs cta/expand-ping-pong))
           loop?  (= :loop (cta/playback-mode copy))
           period (:duration copy)]
       (update timeline :tracks
               (fn [tracks]
                 (reduce-kv (fn [tracks id track]
                              (cond-> tracks
                                (not (contains? tracks id))
                                (assoc id (cond-> track loop? (cta/loop-track period)))))
                            tracks
                            (:tracks copy))))))
   timeline
   copies))

(defn animation-tree
  "The modif-tree showing the animations of `timelines` (by board) at
  `time`: every board as it plays then (see `cta/playback-time`)."
  [timelines objects time]
  (reduce-kv (fn [tree _ timeline]
               (into tree (cta/timeline->modif-tree timeline objects (cta/playback-time timeline time))))
             {}
             timelines))

(defn shown-shapes
  "The shapes the animations of `timelines` move at `time`, by id, as the
  canvas shows them: with the children they carry along."
  [timelines objects time]
  (let [modif-tree (animation-tree timelines objects time)]
    (when (seq modif-tree)
      (into {}
            (keep (fn [[id {:keys [modifiers]}]]
                    (when-let [shape (get objects id)]
                      [id (gsh/transform-shape shape modifiers)])))
            (gm/set-objects-modifiers modif-tree objects)))))

(defn edit-preview
  "The modif-tree showing `edit`, a modif-tree that changes shapes of
  `objects`, over the animations of `timelines` at `time` in motion
  mode: the shapes as the edit leaves them and then, with the edit
  recorded in the timeline of the board `board-id` (see
  `cta/record-edit`), as the animations show them there. So during a
  drag the canvas shows what it will show once the shapes are dropped,
  and the animated shapes the edit leaves alone stay where they are."
  [timelines board-id objects time edit]
  (let [after     (reduce-kv (fn [objects id {:keys [modifiers]}]
                               (d/update-when objects id gsh/transform-shape modifiers))
                             objects
                             (gm/set-objects-modifiers edit objects))
        timelines (d/update-when timelines board-id cta/record-edit objects after time)]
    (merge-with (fn [edit shown]
                  (update edit :modifiers ctm/add-modifiers (:modifiers shown)))
                edit
                (animation-tree timelines after time))))
