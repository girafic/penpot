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

(defn- copy-roots
  "Ids of the top copies of components in the board `board-id`, itself
  included."
  [objects board-id]
  (filter (fn [id]
            (let [shape (get objects id)]
              (and (ctk/instance-root? shape)
                   (not (ctk/main-instance? shape)))))
          (cfh/get-children-ids-with-self objects board-id)))

(defn instance-timelines
  "The timelines playing, in the board `board-id` of `objects`, the
  animation of the component copies in it as their mains play it: the
  tracks of the shapes the copies refer to (`:shape-ref`) in the page of
  the main of their component, which `main-page` gives for a copy as
  `{:objects :timelines}`, moved to where the copies are in the board.
  One by copy, keyed `[:copy id]`, with the settings of the timeline of
  its main, so that it plays on its own clock (see `cta/playback-time`).
  A shape `timeline`, the one of the board, animates itself keeps that
  animation."
  [objects board-id timeline main-page]
  (let [own (:tracks timeline)]
    (reduce
     (fn [result root-id]
       (if-let [{main-objects :objects main-timelines :timelines} (main-page (get objects root-id))]
         (let [[main tracks]
               (reduce (fn [[main tracks] id]
                         (let [ref-id (dm/get-in objects [id :shape-ref])
                               [main-board track]
                               (when (and (some? ref-id) (not (contains? own id)))
                                 (find-track main-timelines ref-id))]
                           (if (some? track)
                             [(or main (get main-timelines main-board))
                              (assoc tracks id
                                     (moved-track track id
                                                  (offset (in-board main-objects ref-id main-board)
                                                          (in-board objects id board-id))
                                                  false))]
                             [main tracks])))
                       [nil {}]
                       (cfh/get-children-ids-with-self objects root-id))]
           (cond-> result
             (seq tracks)
             (assoc [:copy root-id] (assoc (settings main) :board-id board-id :tracks tracks))))
         result))
     {}
     (copy-roots objects board-id))))

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
