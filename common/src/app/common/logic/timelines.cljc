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

(defn shown-shapes
  "The shapes the animation of `timeline` moves at `time`, by id, as the
  canvas shows them: with the children they carry along."
  [timeline objects time]
  (let [modif-tree (cta/timeline->modif-tree timeline objects time)]
    (when (seq modif-tree)
      (into {}
            (keep (fn [[id {:keys [modifiers]}]]
                    (when-let [shape (get objects id)]
                      [id (gsh/transform-shape shape modifiers)])))
            (gm/set-objects-modifiers modif-tree objects)))))

(defn edit-preview
  "The modif-tree showing `edit`, a modif-tree that changes shapes of
  `objects`, over the animation of `timeline` at `time` in motion mode:
  the shapes as the edit leaves them and then, with the edit recorded
  (see `cta/record-edit`), as the timeline shows them there. So during a
  drag the canvas shows what it will show once the shapes are dropped,
  and the animated shapes the edit leaves alone stay where they are."
  [timeline objects time edit]
  (let [after   (reduce-kv (fn [objects id {:keys [modifiers]}]
                             (d/update-when objects id gsh/transform-shape modifiers))
                           objects
                           (gm/set-objects-modifiers edit objects))
        shown   (-> (cta/record-edit timeline objects after time)
                    (cta/timeline->modif-tree after time))]
    (merge-with (fn [edit shown]
                  (update edit :modifiers ctm/add-modifiers (:modifiers shown)))
                edit
                shown)))
