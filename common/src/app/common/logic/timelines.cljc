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
  as much as the layer moved in its board. The audio clips of a timeline
  belong to its board: they go with the board when it is duplicated,
  pasted or put into another board, never with a layer."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.geom.modifiers :as gm]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
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

(defn- find-clock
  "`[board-id clock]`: the board whose timeline gives the component copy
  `id` a clock of its own (see `cta/copy-clock`), and the clock."
  [timelines id]
  (some (fn [[board-id timeline]]
          (when-let [clock (dm/get-in timeline [:copies id])]
            [board-id clock]))
        timelines))

(defn- settings
  "A timeline without its board, tracks, audio clips and the clocks of
  its component copies."
  [timeline]
  (dissoc timeline :board-id :tracks :audio-clips :copies))

(defn animation-sources
  "What copying the animation of the shapes of `ids` of `page` (with its
  `objects`) takes: their tracks, with where the shapes are in their
  board, the clocks of the component copies among them, and the
  settings and the audio clips of the timelines of the boards among
  them."
  [page objects ids]
  (let [timelines (:timelines page)
        clocks    (into {}
                        (keep (fn [id]
                                (when-let [[board-id clock] (find-clock timelines id)]
                                  [id {:board board-id :clock clock}])))
                        ids)]
    (cond-> {:tracks
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
                   ids)}
      (seq clocks)
      (assoc :clocks clocks)

      (some #(seq (dm/get-in timelines [% :audio-clips])) ids)
      (assoc :audio
             (into {}
                   (keep (fn [id]
                           (when-let [clips (not-empty (dm/get-in timelines [id :audio-clips]))]
                             [id clips])))
                   ids)))))

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

(defn- copied-follow
  "`track` of a copy in `new-board` of `objects`, following (see
  `cta/follow-of`) the copy of its leader when that was copied too (in
  `ids-map`), its leader still when that is in the same board, else
  nothing."
  [track ids-map objects new-board]
  (if-let [leader (dm/get-in track [:follow :leader])]
    (cond
      (contains? ids-map leader)
      (assoc-in track [:follow :leader] (get ids-map leader))

      (and (contains? objects leader)
           (= new-board (cfh/get-shape-id-root-frame objects leader)))
      track

      :else
      (dissoc track :follow))
    track))

(defn- strayed-follows-dropped
  "`timelines` (of the shapes `objects`) without the follows of layers
  whose leaders are gone, or out of the board of their timeline: a
  layer follows one of its own board only."
  [timelines objects]
  (reduce-kv
   (fn [timelines board-id timeline]
     (reduce-kv (fn [timelines id track]
                  (let [leader (dm/get-in track [:follow :leader])]
                    (if (and (some? leader)
                             (not (and (contains? objects leader)
                                       (= board-id (cfh/get-shape-id-root-frame objects leader)))))
                      (update timelines board-id cta/set-follow id nil)
                      timelines)))
                timelines
                (:tracks timeline)))
   timelines
   timelines))

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

(defn- add-clock
  "`timelines` with `clock` for the component copy `id` in the timeline
  of `board-id`, a new one with `settings` when the board has none."
  [timelines board-id id clock settings]
  (let [timeline (or (get timelines board-id)
                     (merge (cta/make-timeline {:board-id board-id}) settings))]
    (assoc timelines board-id
           (-> timeline
               (assoc :board-id board-id)
               (cta/set-copy-clock id clock)))))

(defn- add-clips
  "`timelines` with the audio `clips` of another board in the timeline of
  `board-id`, a new one with `settings` when the board has none: at the
  same starts, on lanes after the ones the board uses, with new ids when
  they are copies (`copy?`). Its duration stays."
  [timelines board-id clips settings copy?]
  (let [timeline (or (get timelines board-id)
                     (merge (cta/make-timeline {:board-id board-id}) settings))
        lanes    (->> (vals (:audio-clips timeline))
                      (map (comp inc :lane))
                      (reduce max 0))]
    (assoc timelines board-id
           (reduce (fn [timeline clip]
                     (let [clip (cond-> (update clip :lane + lanes)
                                  copy? (assoc :id (uuid/next)))]
                       (assoc-in timeline [:audio-clips (:id clip)] clip)))
                   (assoc timeline :board-id board-id)
                   (sort-by (juxt :lane :start) (vals clips))))))

(defn commit-timelines
  "Changes turning the timelines of `page` into `timelines`, one
  `pcb/change-timeline` per board that changed. Call it once per change
  set, with the whole `:timelines` map of the page and every edit in it:
  a board missing from `timelines` loses its timeline, and a second call
  for the same board would write wrong undo changes (each call takes the
  old timeline from the page as it was)."
  [changes page timelines]
  (let [old (:timelines page)]
    (reduce (fn [changes board-id]
              (let [timeline (get timelines board-id)]
                (cond-> changes
                  (not= timeline (get old board-id))
                  (pcb/change-timeline board-id timeline))))
            (pcb/with-page changes page)
            (into (set (keys old)) (keys timelines)))))

(defn commit-page-timelines
  "Changes turning every timeline of `pages` into what `f` gives of it,
  for the timelines `f` changes (see `commit-timelines`): one change set
  over several pages, as when a design token changes the settings it
  binds in all of them."
  [changes pages f]
  (reduce (fn [changes page]
            (let [timelines  (:timelines page)
                  timelines' (d/update-vals timelines f)]
              (if (= timelines timelines')
                changes
                (commit-timelines changes page timelines'))))
          changes
          pages))

(defn commit-track-animations
  "Changes turning the timelines of `page` into `timelines` (its whole
  `:timelines` map, edited) when mostly the animations of their tracks
  and their durations change, as in a style sync: the animations of each
  track that changed (see `pcb/set-timeline-track-animations`) and each
  changed duration, so that keyframes other users add meanwhile to the
  same layers stay; a track whose keyframes or flags change too (a use
  whose keyframe entries turn into keyframes, see
  `ctas/detach-track-use`) is written whole. A board without a timeline
  on `page` is left out."
  [changes page timelines]
  (let [old (:timelines page)]
    (reduce-kv
     (fn [changes board-id timeline]
       (let [before (get old board-id)]
         (if (or (nil? before) (= timeline before))
           changes
           (as-> changes $
             (reduce-kv (fn [changes shape-id track]
                          (let [old-track (get-in before [:tracks shape-id])]
                            (cond
                              (= (dissoc track :animations) (dissoc old-track :animations))
                              (cond-> changes
                                (not= (:animations track) (:animations old-track))
                                (pcb/set-timeline-track-animations board-id shape-id (:animations track)))

                              :else
                              (pcb/set-timeline-track changes board-id shape-id track))))
                        $
                        (:tracks timeline))
             (cond-> $
               (not= (:duration timeline) (:duration before))
               (pcb/update-timeline board-id {:duration (:duration timeline)}))))))
     (pcb/with-page changes page)
     timelines)))

(defn generate-copy-timelines
  "Give the copies of `ids-map` (old id -> new id) the animation of their
  originals, `sources` (see `animation-sources`); `changes` holds the
  copies on `page`. A copy outside of any board (an alt-drag duplicate
  before it is dropped) keeps the board of its original when
  `keep-board?`."
  [changes page ids-map {:keys [tracks settings audio clocks]} & {:keys [keep-board?]}]
  (if (and (empty? tracks) (empty? audio) (empty? clocks))
    changes
    (let [objects     (pcb/get-objects changes)
          root-of     (fn [new-id]
                        (when (contains? objects new-id)
                          (cfh/get-shape-id-root-frame objects new-id)))
          ;; the board of the copy `new-id` of a layer of `board`, and
          ;; the settings of a timeline made for it
          board-of    (fn [new-id board]
                        (or (root-of new-id)
                            (when (and keep-board? (contains? objects new-id)) board)))
          settings-of (fn [new-board board]
                        (or (get settings (d/seek #(= new-board (get ids-map %)) (keys settings)))
                            (get settings board)))
          timelines
          (reduce-kv
           (fn [timelines old-id {:keys [board track at]}]
             (let [new-id    (get ids-map old-id)
                   new-board (board-of new-id board)]
               (if (nil? new-board)
                 timelines
                 (add-track timelines new-board
                            (-> (moved-track track new-id
                                             (offset at (in-board objects new-id new-board))
                                             true)
                                (copied-follow ids-map objects new-board))
                            (settings-of new-board board)))))
           (:timelines page)
           tracks)

          ;; a copy of a component copy plays on its clock
          timelines
          (reduce-kv
           (fn [timelines old-id {:keys [board clock]}]
             (let [new-id    (get ids-map old-id)
                   new-board (board-of new-id board)]
               (if (nil? new-board)
                 timelines
                 (add-clock timelines new-board new-id clock (settings-of new-board board)))))
           timelines
           clocks)

          ;; the clips of a board go to the board of its copy: the copy
          ;; itself, or the board it is pasted into
          timelines
          (reduce-kv
           (fn [timelines old-board clips]
             (if-let [new-board (root-of (get ids-map old-board))]
               (add-clips timelines new-board clips (get settings old-board) true)
               timelines))
           timelines
           audio)]
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

        ;; a component copy takes its clock along
        result
        (reduce
         (fn [timelines id]
           (let [[board clock] (find-clock timelines id)
                 new-board     (cfh/get-shape-id-root-frame moved id)]
             (if (or (nil? clock) (nil? new-board) (= board new-board))
               timelines
               (-> timelines
                   ;; no clock is the clock of the defaults
                   (update board cta/set-copy-clock id {})
                   (add-clock new-board id clock (settings (get timelines board)))))))
         result
         all-ids)

        ;; a board put into another one gives it its clips too
        result
        (reduce (fn [timelines id]
                  (if (and (contains? timelines id)
                           (not (cfh/root-frame? moved id)))
                    (let [clips (dm/get-in timelines [id :audio-clips])
                          host  (cfh/get-shape-id-root-frame moved id)]
                      (cond-> (dissoc timelines id)
                        (and (seq clips) (some? host) (not= host id))
                        (add-clips host clips (settings (get timelines id)) false)))
                    timelines))
                result
                ids)

        ;; a layer follows one of its own board only
        result (strayed-follows-dropped result moved)]
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

(defn- copy-follows
  "`result` (see `instance-timelines`) with the followers in the
  timelines of the copy `head-id` following the copies of their leaders,
  by `refs` (the main shape each copy shape plays, to that copy shape);
  a follow whose leader plays nothing there is dropped."
  [result head-id refs]
  (reduce-kv
   (fn [result k entry]
     (if (and (vector? k) (= :copy (first k)) (= head-id (second k)))
       (assoc result k
              (update entry :tracks
                      (fn [tracks]
                        (reduce-kv
                         (fn [tracks id track]
                           (if-let [leader (dm/get-in track [:follow :leader])]
                             (let [copy (get refs leader)]
                               (assoc tracks id
                                      (if (contains? tracks copy)
                                        (assoc-in track [:follow :leader] copy)
                                        (dissoc track :follow))))
                             tracks))
                         tracks
                         tracks))))
       result))
   result
   result))

(defn instance-timelines
  "The timelines playing, in the board `board-id` of `objects`, the
  animations of the component copies in it as their mains play them: the
  tracks of the shapes the copies refer to (`:shape-ref`) in the pages of
  their mains, which `main-page` gives for a component head as
  `{:objects :timelines}`, through a component in a component too (see
  `main-track`), taken to where the copies are in the board and scaled as
  much as they are. One by copy and main board, keyed `[:copy id board]`,
  with the settings of the timeline of that main, so that each plays on
  its own clock (see `cta/playback-time`). The board that is the main of
  a copy gives it its audio clips too, an entry of their own when it
  animates nothing (see `with-instance-clips`). A shape `timeline`, the
  one of the board, animates itself keeps that animation. Each plays on
  the clock `timeline` gives the copy (see `cta/copy-clock`), as
  `:clock-start` and `:clock-speed` when they are not the defaults. A
  follower there follows the copy of its leader (see `cta/follow-of`)."
  [objects board-id timeline main-page]
  (let [own (:tracks timeline)]
    (reduce
     (fn [result head-id]
       (let [head    (get objects head-id)
             main-id (:shape-ref head)
             main    (some-> (main-page head) :timelines (get main-id))
             {:keys [start speed]} (cta/copy-clock timeline head-id)
             entry   (fn [settings]
                       (cond-> (assoc settings :board-id board-id :tracks {})
                         (not (zero? start)) (assoc :clock-start start)
                         (not= 1 speed)      (assoc :clock-speed speed)))
             refs    (volatile! {})
             result  (reduce
                      (fn [result id]
                        (if-let [{:keys [board track ref-id] main :timeline mobjs :objects cmap :map}
                                 (when-not (contains? own id)
                                   (main-track (get objects id) head main-page))]
                          (let [k [:copy head-id board]]
                            (vswap! refs assoc ref-id id)
                            (-> result
                                (update k #(or % (entry (settings main))))
                                (assoc-in [k :tracks id]
                                          (mapped-track track ref-id mobjs board id objects board-id cmap))))
                          result))
                      result
                      (cfh/get-children-ids-with-self objects head-id))
             result  (copy-follows result head-id @refs)]
         (if-let [clips (not-empty (:audio-clips main))]
           (let [k [:copy head-id main-id]]
             (-> result
                 (update k #(or % (entry (settings main))))
                 (assoc-in [k :audio-clips] clips)))
           result)))
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
  has no tracks and no audio clips itself (see `cta/timeline-empty?`): as
  long as the longest of them, or as the board when it is `stored?` (its
  timeline was saved, with the duration set on it) and longer, looping
  when one of them does."
  [timeline copies stored?]
  (if (or (not (cta/timeline-empty? timeline)) (empty? copies))
    timeline
    (let [longest (reduce max 1 (map cta/clock-end copies))]
      (assoc timeline
             :duration (if stored? (max longest (:duration timeline)) longest)
             :playback (if (every? #(= :once (cta/playback-mode %)) copies) :once :loop)))))

(defn with-instance-tracks
  "`timeline`, of a board, with the animations of the component copies
  in it, `copies` (see `instance-timelines`), for the exports, which read
  one timeline: each as it plays on its own clock, over and over when it
  loops (see `cta/loop-track`). The split animations of a text of a copy
  stay on its track with the clock of the copy, `:split-clock` (see
  `cta/text-unit-entries`), or, with `text-split` `:whole`, play as
  animations of the whole text, as the markup exports play them (see
  `cta/flatten-splits`). A shape `timeline` animates itself keeps that
  animation. The layers of copies that follow others play keyframes
  (see `cta/bake-follows`). `:copy-loops` holds the cycles (ms) of the copies that loop,
  whose clock the exports bind to the board's (see
  `app.main.data.exports.animation/export-issues`)."
  [timeline copies objects & {:keys [text-split]}]
  (reduce
   (fn [timeline copy]
     (let [clock  (select-keys copy [:duration :playback :loop :clock-start :clock-speed])
           start  (:clock-start copy 0)
           speed  (:clock-speed copy 1)
           copy   (-> (cond-> copy (= :whole text-split) cta/flatten-splits)
                      (cta/bake-follows objects)
                      (cta/resolve-animations objects)
                      (cta/expand-loops))
           ;; ping-pong turns into a loop of there and back
           copy   (cond-> copy
                    (= :ping-pong (cta/playback-mode copy))
                    (-> cta/expand-paths cta/expand-springs cta/expand-ping-pong))
           loop?  (= :loop (cta/playback-mode copy))
           period (:duration copy)]
       (cond-> timeline
         loop? (update :copy-loops (fnil conj []) (if (= 1 speed) period (/ period (double speed))))
         :always
         (update :tracks
                 (fn [tracks]
                   (reduce-kv (fn [tracks id track]
                                (cond-> tracks
                                  (not (contains? tracks id))
                                  (assoc id (cond-> track
                                              loop?                     (cta/loop-track period)
                                              :always                   (cta/retime-track start speed)
                                              (seq (:animations track)) (assoc :split-clock clock)))))
                              tracks
                              (:tracks copy)))))))
   timeline
   copies))

(defn- copy-cycles
  "The starts of the cycles of `copy` (see `instance-timelines`) whose
  sound plays within `duration` ms: the first only when it plays once,
  each one when it loops, the way there of each when it plays ping-pong
  (the way back is silent, see `cta/audio-plan`)."
  [copy duration]
  (if (= :once (cta/playback-mode copy))
    [0]
    (range 0 duration (max 1 (cta/cycle-duration copy)))))

(defn- cycle-clip
  "`clip` of a copy as it plays in the cycle of the copy from `at` ms of
  the board, on `lane`, with the id `id`: cut `length` ms into the cycle,
  where it or the board ends (its fade out, past it, goes), nil when it
  starts there or later."
  [clip id at lane length]
  (when (< (:start clip) length)
    (cond-> (assoc clip :id id :start (+ at (:start clip)) :lane lane)
      (> (ctau/clip-end clip) length)
      (-> (assoc :duration (- length (:start clip)))
          (dissoc :fade-out)))))

(defn at-pace
  "`copy` (see `instance-timelines`) as it sounds at its pace: its
  duration and the timing of its audio clips in ms of the board, the
  clips playing their files that much faster, and no clock of its own
  left; its start stays to the caller (see `with-instance-clips`)."
  [copy]
  (let [speed (:clock-speed copy 1)
        pace  #(some-> % (/ (double speed)) mth/round)]
    (cond-> (dissoc copy :clock-start :clock-speed)
      (not= 1 speed)
      (-> (update :duration pace)
          (update :audio-clips
                  (fn [clips]
                    (when (some? clips)
                      (update-vals clips
                                   (fn [clip]
                                     (-> clip
                                         (update :start pace)
                                         (update :duration #(max ctau/min-clip-duration (pace %)))
                                         (d/update-when :fade-in pace)
                                         (d/update-when :fade-out pace)
                                         (assoc :speed (mth/clamp (* speed (ctau/clip-speed clip))
                                                                  ctau/min-speed ctau/max-speed))))))))))))

(defn sounding-mains
  "The main boards whose audio clips the component copies `copies` (by
  `[:copy id main-board]`, see `instance-timelines`) play, each once,
  however many copies of it there are (they keep the same clock): `[main
  copy]`, by main board, `copy` the timeline of one of them."
  [copies]
  (->> copies
       (filter (fn [[_ copy]] (seq (:audio-clips copy))))
       (group-by (fn [[[_ _ main-id] _]] main-id))
       (sort-by (comp str key))
       (mapv (fn [[main-id entries]]
               [main-id (val (first (sort-by (comp str key) entries)))]))))

(defn with-instance-clips
  "`timeline`, of a board, with the audio clips of the mains of the
  component copies in it, `copies` (see `sounding-mains`), as the copies
  play them over the duration of the board: on each of their cycles
  (see `copy-cycles`), on lanes after the ones of the board, with ids
  that stay the same from call to call, so that live playback goes on
  (see `app.util.audio/follow!`). What the workspace plays and the video
  exports mix, which play one timeline."
  [timeline copies]
  (let [duration (:duration timeline)
        mains    (sounding-mains copies)
        lanes    (->> (vals (:audio-clips timeline))
                      (map (comp inc :lane))
                      (reduce max 0))]
    (first
     (reduce
      (fn [[timeline lanes] [main-id copy]]
        (let [start  (:clock-start copy 0)
              copy   (at-pace copy)
              clips  (sort-by (juxt :lane :start) (vals (:audio-clips copy)))
              length (:duration copy)
              clips  (for [[index at] (map-indexed vector
                                                   (map #(+ start %) (copy-cycles copy (- duration start))))
                           clip       clips
                           :let       [id (uuid/custom (long (hash [main-id (:id clip)])) (long index))]
                           :when      (< at duration)]
                       (cycle-clip clip id at (+ lanes (:lane clip)) (min length (- duration at))))]
          [(reduce (fn [timeline clip]
                     (cond-> timeline
                       (some? clip) (assoc-in [:audio-clips (:id clip)] clip)))
                   timeline
                   clips)
           (+ lanes (reduce max 0 (map (comp inc :lane) (vals (:audio-clips copy)))))]))
      [timeline lanes]
      mains))))

(defn layer-ids
  "The ids of `id` and the layers in it, in the order of the layers panel
  and the dock: the shape, then its layers top-most first, depth first."
  [objects id]
  (letfn [(walk [id]
            (when (contains? objects id)
              (cons id (mapcat walk (rseq (vec (dm/get-in objects [id :shapes])))))))]
    (walk id)))

(defn follow-leaders
  "The layers of the board of `timeline` (of `objects`) that `shape-id`
  can follow (see `cta/follow-of`), in the order of the layers panel:
  not itself, a layer it is in or that is in it (they move together
  already), nor one that follows it, through others."
  [objects timeline shape-id]
  (let [above       (set (cfh/get-parent-ids objects shape-id))
        inside      (set (cfh/get-children-ids objects shape-id))
        leader-of   #(dm/get-in timeline [:tracks % :follow :leader])
        follows-it? (fn [id]
                      (loop [at (leader-of id), seen #{id}]
                        (cond (nil? at)           false
                              (= at shape-id)     true
                              (contains? seen at) false
                              :else               (recur (leader-of at) (conj seen at)))))]
    (into []
          (remove #(or (= % shape-id) (contains? above %) (contains? inside %) (follows-it? %)))
          (rest (layer-ids objects (:board-id timeline))))))

(defn- split-kinds
  "`{text-id kind}`: the kind of units each text of the tracks of
  `timelines` plays its split animations by (see `cta/unit-split-kind`)."
  [timelines objects]
  (reduce (fn [splits timeline]
            (reduce-kv (fn [splits id track]
                         (if-let [kind (cta/unit-split-kind (get objects id) (cta/track-animations track))]
                           (assoc splits id kind)
                           splits))
                       splits
                       (:tracks timeline)))
          {}
          timelines))

(defn code-timelines
  "What the code of the animation of the board `board-id` of `page`
  shows (the Inspect, the plugin API; `files` holds the files of the
  components, by id): `:timeline`, its own timeline (a new one named after
  it while it has none), keeping time with the animated component copies
  in it while it has no tracks (see `with-copies-clock`); `:copies`, the
  timelines of those copies (`{:head-id :timeline}`, in layer order),
  each on its own clock, as the workspace and the view mode play them;
  the split animations of both played whole (see `cta/flatten-splits`),
  but those by characters or words of the texts `units?` (a function of
  a text) is true of, which the code plays by units (see
  `cta/unit-timelines`); and `:splits`, the texts the design plays by
  units, with their kind. The layers that follow others play keyframes
  (see `cta/bake-follows`). Nil when the board plays nothing."
  [page files board-id & {:keys [units?] :or {units? (constantly false)}}]
  (let [objects   (:objects page)
        timelines (:timelines page)
        stored    (get timelines board-id)
        copies    (copy-timelines objects timelines files board-id)]
    (when (or (some? stored) (seq copies))
      (let [own    (or stored (cta/make-timeline {:board-id board-id
                                                  :name (dm/get-in objects [board-id :name])}))
            order  (into {} (map-indexed (fn [index id] [id index])) (layer-ids objects board-id))
            copies (->> copies
                        (sort-by (fn [[[_ head-id] _]] (get order head-id 0)))
                        (mapv (fn [[[_ head-id] timeline]] {:head-id head-id :timeline timeline})))
            splits (split-kinds (cons own (map :timeline copies)) objects)
            ;; lines play whole: a page breaks them as its width says
            flat   #(-> (cta/bake-follows % objects)
                        (cta/flatten-splits #{:char :word} (fn [id] (units? (get objects id)))))
            copies (mapv #(update % :timeline flat) copies)]
        {:board-id board-id
         :timeline (-> (flat own)
                       (with-copies-clock (map :timeline copies) (some? stored)))
         :copies   copies
         :splits   splits}))))

(defn animation-tree
  "The modif-tree showing the animations of `timelines` (by board) at
  `time`: every board as it plays then (see `cta/playback-time`)."
  [timelines objects time]
  (reduce-kv (fn [tree _ timeline]
               (into tree (cta/timeline->modif-tree timeline objects (cta/playback-time timeline time))))
             {}
             timelines))

(defn view-animation-tree
  "The modif-tree of the board `board-id` of `page` as the view mode plays
  it: its own timeline at its playback `time`, and the animated component
  copies in it (see `copy-timelines`) each on its own clock, `elapsed` ms
  after playback started, so a copy that loops goes on once the board's
  own animation has ended. `files` holds the files of the components, by
  id."
  [page files board-id time elapsed]
  (let [objects   (:objects page)
        timelines (:timelines page)]
    (merge (animation-tree (copy-timelines objects timelines files board-id) objects elapsed)
           (animation-tree (select-keys timelines [board-id]) objects time))))

(defn view-playback-ended?
  "Whether the view mode stops playing a board `elapsed` ms after it
  started: when its `clock` (see `with-copies-clock`) and every
  animated component copy in it, `copies`, have played once to their
  end; never while one of them loops."
  [clock copies elapsed]
  (every? #(cta/playback-ended? % elapsed) (cons clock copies)))

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

(defn poses-units?
  "Whether `modif-tree` poses the units of the text `id` (see
  `cta/text-unit-entries`)."
  [modif-tree id]
  (boolean (some #(= :text-units (:property %))
                 (dm/get-in modif-tree [id :modifiers :structure-parent]))))

(defn units-at-rest
  "`modif-tree` with the units of the texts of `ids` at rest
  (`:text-units []`), as a text shows while it is edited, so that its
  caret and its glyphs line up."
  [modif-tree ids]
  (reduce (fn [tree id]
            (update-in tree [id :modifiers]
                       (fn [modifiers]
                         (-> (or modifiers (ctm/empty))
                             (update :structure-parent
                                     (partial filterv #(not= :text-units (:property %))))
                             (ctm/change-property :text-units [])))))
          modif-tree
          ids))

(defn edited-objects
  "`objects` as `edit`, a modif-tree that changes some of them, leaves
  them."
  [objects edit]
  (reduce-kv (fn [objects id {:keys [modifiers]}]
               (d/update-when objects id gsh/transform-shape modifiers))
             objects
             (gm/set-objects-modifiers edit objects)))

(defn edit-preview
  "The modif-tree showing `edit`, a modif-tree that changes shapes of
  `objects`, over the animations of `timelines` at `time` in motion
  mode: the shapes as the edit leaves them and then, with the edit
  recorded in the timeline of the board `board-id` (see
  `cta/record-edit`), as the animations show them there. So during a
  drag the canvas shows what it will show once the shapes are dropped,
  and the animated shapes the edit leaves alone stay where they are."
  [timelines board-id objects time edit]
  (let [after     (edited-objects objects edit)
        timelines (d/update-when timelines board-id cta/record-edit objects after time)]
    (merge-with (fn [edit shown]
                  (update edit :modifiers ctm/add-modifiers (:modifiers shown)))
                edit
                (animation-tree timelines after time))))

;; COPY AND PASTE OF ANIMATION
;;
;; Copying the animation of layers takes an animation clip: the track of
;; each layer and where the layer rests in its board (see
;; `cta/rest-pose`), so that pasting it counts the positions, rotations
;; and sizes again from where the layers it goes onto rest (see
;; `cta/retarget-track`).

(def max-clip-layers
  "The most layers an animation clip holds."
  100)

(def max-paste-targets
  "The most layers one paste of an animation clip goes onto."
  500)

(def max-paste-keyframes
  "The most keyframes one paste of an animation clip writes, over all its
  targets; each preset entry of a style use counts as one, a keyframe
  entry as its keyframes."
  10000)

(defn- clip-entry?
  "Whether the entry `entry` of a style use can be pasted: it does not
  start before its style, and the keyframes of a keyframe entry are
  within its span, with values of the kind its property takes."
  [{:keys [start duration property keyframes] :as entry}]
  (and (>= start 0)
       (or (not (cta/keys-entry? entry))
           (every? #(and (<= 0 (:time %) duration)
                         (cta/keyframe-value-fits? {:property property :value (:value %)}))
                   keyframes))))

(defn- clip-track?
  "Whether the keyframes and presets of `track` can be pasted: the values
  of the kind their properties take, and no entry of a style use before
  the start of its style (see `clip-entry?`). They may lie before the
  start of the timeline, as on the timeline itself."
  [{:keys [keyframes animations]}]
  (and (every? cta/keyframe-value-fits? keyframes)
       (every? #(every? clip-entry? (:entries %)) animations)))

(defn- clip-size
  "How much a paste of `track` writes: its keyframes and the entries of
  its style uses, a keyframe entry counting as its keyframes."
  [{:keys [keyframes animations]}]
  (+ (count keyframes)
     (transduce (mapcat :entries)
                (completing #(+ %1 (if (cta/keys-entry? %2) (count (:keyframes %2)) 1)))
                0
                animations)))

(def max-clip-audio
  "The most audio clips an animation clip holds."
  100)

(def schema:animation-clip
  "An animation clip (see `animation-clip`), with the audio clips of the
  board `:board-id` copied along (see `audio-copy`). It comes from the
  clipboard, so it is checked more strictly than a stored track, and its
  size is capped."
  [:map {:title "AnimationClip"}
   [:file-id {:optional true} ::sm/uuid]
   [:page-id {:optional true} ::sm/uuid]
   [:board-id {:optional true} ::sm/uuid]
   [:layers [:vector {:max max-clip-layers}
             [:map {:title "AnimationClipLayer"}
              [:shape-id ::sm/uuid]
              [:pose cta/schema:pose]
              [:track [:and cta/schema:track [:fn clip-track?]]]]]]
   [:audio {:optional true} [:vector {:max max-clip-audio} ctau/schema:audio-clip]]])

(def animation-clip-valid?
  (sm/lazy-validator schema:animation-clip))

(defn- played-track
  "What animates the layer `shape-id` of `page` (with its `objects`):
  `{:timeline :track}`, its own track or, in a component copy without
  one, the track it plays of its main (see `copy-timelines`; `files`
  holds the files of the components, by id). Nil when nothing does."
  [page objects files shape-id]
  (let [timelines (:timelines page)]
    (if-let [[board-id track] (find-track timelines shape-id)]
      {:timeline (get timelines board-id) :track track}
      (when-let [board-id (cfh/get-shape-id-root-frame objects shape-id)]
        (some (fn [timeline]
                (when-let [track (cta/get-track timeline shape-id)]
                  {:timeline timeline :track track}))
              (vals (copy-timelines objects timelines files board-id)))))))

(defn- plays?
  "Whether `track` holds keyframes or presets."
  [track]
  (or (seq (:keyframes track)) (seq (:animations track))))

(defn- clip-track
  "`track` as an animation clip takes it (see `animation-clip`)."
  [track {:keys [properties keyframe-ids presets? whole-layer?]}]
  (let [slot? (fn [{:keys [property index]}]
                (or (contains? properties property)
                    (contains? properties [property index])))]
    (cond-> (dissoc (cond-> track whole-layer? cta/visible-track) :hidden :locked)
      (some? properties)
      (update :keyframes (partial filterv slot?))

      (some? keyframe-ids)
      (update :keyframes (partial filterv #(contains? keyframe-ids (:id %))))

      (not presets?)
      (dissoc :animations))))

(defn animation-clip
  "What copying the animation of the layers `shape-ids` of `page` (with
  its `objects`) of the file `file-id` takes: for each animated one, its
  track and where it rests in its board (see `cta/rest-pose`). A layer of
  a component copy without a track of its own gives the one it plays of
  its main, as it plays it (`files` holds the files of the components, by
  id). With `whole-layer?`, what plays: no hidden rows or presets.
  Otherwise, hidden or not, the rows of `properties` (properties, or
  `[property index]` slots) and the keyframes of `keyframe-ids`, when
  given. Presets unless not `presets?`. Never locks."
  [file-id page objects files shape-ids
   & {:keys [properties keyframe-ids presets? whole-layer?] :or {presets? true}}]
  (let [opts {:properties   (some-> properties set)
              :keyframe-ids (some-> keyframe-ids set)
              :presets?     presets?
              :whole-layer? whole-layer?}]
    {:file-id file-id
     :page-id (:id page)
     :layers
     (into []
           (keep (fn [shape-id]
                   (when-let [{:keys [timeline track]} (played-track page objects files shape-id)]
                     (let [track (clip-track track opts)]
                       (when (plays? track)
                         {:shape-id shape-id
                          :pose     (cta/rest-pose timeline objects shape-id)
                          :track    track})))))
           shape-ids)}))

(defn audio-copy
  "The audio clips `clip-ids` of `timeline` as an animation clip takes
  them (see `schema:animation-clip`), by start: never locked, at most
  `max-clip-audio`."
  [timeline clip-ids]
  (into []
        (comp (keep #(dm/get-in timeline [:audio-clips %]))
              (map #(dissoc % :locked))
              (take max-clip-audio))
        (sort-by (fn [id] (dm/get-in timeline [:audio-clips id :start] 0))
                 (set clip-ids))))

(defn paste-start
  "The earliest start of what the animation clip `clip` holds: the
  keyframes and presets of its layers, its audio clips. A paste lands
  it at the playhead, the rest at the same distance from it."
  [clip]
  (let [times (concat (mapcat (fn [{:keys [track]}]
                                (concat (map :time (:keyframes track))
                                        (map :start (:animations track))))
                              (:layers clip))
                      (map :start (:audio clip)))]
    (if (seq times) (reduce min times) 0)))

(defn paste-audio
  "`timeline` with the audio clips `clips` (see `audio-copy`) `dt` ms
  later: new ids, each on the first lane free there and
  of its file (see `ctau/free-lane`), the duration as it is. `{:timeline
  :ids}`, the ids of the pasted clips."
  [timeline clips dt]
  (reduce (fn [{:keys [timeline] :as result} clip]
            (let [start (mth/round (+ (:start clip) dt))
                  lane  (ctau/free-lane timeline start (+ start (:duration clip)) :audio-id (:audio-id clip))
                  clip  (assoc clip :id (uuid/next) :start start :lane lane)]
              (-> result
                  (assoc :timeline (assoc-in timeline [:audio-clips (:id clip)] clip))
                  (update :ids conj (:id clip)))))
          {:timeline timeline :ids []}
          (sort-by (juxt :start :lane) clips)))

(defn animated-layer?
  "Whether the layer `shape-id` of `page` plays an animation, its own or
  the one of its main (see `animation-clip`)."
  [page files shape-id]
  (boolean
   (when-let [{:keys [track]} (played-track page (:objects page) files shape-id)]
     (plays? (cta/visible-track track)))))

(defn- layer-order
  "The ids of `objects` from the top-most layer down, as the layers panel
  lists them."
  [objects]
  (letfn [(walk [id]
            (cons id (mapcat walk (reverse (dm/get-in objects [id :shapes])))))]
    (walk uuid/zero)))

(defn paste-targets
  "The layers of `ids` (of `objects`) to paste an animation onto, from
  the top-most down: none inside another one of them, whose animation
  carries it along."
  [objects ids]
  (let [ids (cfh/clean-loops objects ids)]
    (filterv #(contains? ids %) (layer-order objects))))

(defn- clip-start
  "The earliest keyframe or preset of the tracks of `layers`."
  [layers]
  (let [times (mapcat (fn [{:keys [track]}]
                        (concat (map :time (:keyframes track))
                                (map :start (:animations track))))
                      layers)]
    (if (seq times) (reduce min times) 0)))

(defn- shift-track
  "`track` with its keyframes and presets `dt` ms later."
  [track dt]
  (cond-> track
    (not (zero? dt))
    (-> (update :keyframes (partial mapv #(update % :time + dt)))
        (d/update-when :animations (partial mapv #(update % :start + dt))))))

(defn paste-animation
  "Paste the animation clip `clip` (see `animation-clip`) onto the layers
  of `pairs`, `[{:layer clip-layer :target-id id}]` in order, of the page
  `page-id` (with its `timelines` and `objects`) of the file `file-id`.
  Each target takes the track of its clip layer counted from where it
  rests (see `cta/retarget-track`, the values as they are with `exact?`),
  in the timeline of its board (see `cta/paste-track` for `mode`,
  `:replace` or `:merge`). With `time` nil the clip keeps its timing;
  otherwise its earliest keyframe or preset lands at `time`. The n-th
  target starts n × `stagger` ms later. A target out of any board is
  left out, and so is the layer the clip was taken from (same file,
  page and id) with `skip-source?`. A board without a timeline gets one,
  named after it.

  `{:timelines :skipped :pivot-kept :targets :grown}`: the timelines of
  the page after it, how many keyframes and split presets the targets
  could not take (see `cta/retarget-track`) and how many targets kept
  their pivot, the targets pasted onto, and the boards whose timeline
  grew longer. `{:error :too-large}`, and nothing pasted, onto more than
  `max-paste-targets` layers or for more than `max-paste-keyframes`
  keyframes in all (see `clip-size`)."
  [file-id page-id timelines objects clip pairs
   {:keys [mode time stagger exact? skip-source?] :or {mode :replace stagger 0}}]
  (let [keyframes (transduce (map #(clip-size (dm/get-in % [:layer :track]))) + 0 pairs)]
    (if (or (> (count pairs) max-paste-targets) (> keyframes max-paste-keyframes))
      {:error :too-large}
      (let [source?
            (fn [{:keys [layer target-id]}]
              (and skip-source?
                   (= file-id (:file-id clip))
                   (= page-id (:page-id clip))
                   (= target-id (:shape-id layer))))

            pairs
            (into []
                  (keep (fn [{:keys [target-id] :as pair}]
                          (when (and (contains? objects target-id) (not (source? pair)))
                            (when-let [board-id (cfh/get-shape-id-root-frame objects target-id)]
                              (assoc pair :board-id board-id)))))
                  pairs)

            offset (if (some? time) (- time (clip-start (map :layer pairs))) 0)

            result
            (reduce
             (fn [result [n {:keys [layer target-id board-id]}]]
               (let [timeline (or (get-in result [:timelines board-id])
                                  (cta/make-timeline {:board-id board-id
                                                      :name (dm/get-in objects [board-id :name])}))
                     to       (cta/rest-pose timeline objects target-id)

                     {:keys [track skipped]}
                     (cta/retarget-track (:track layer) (:pose layer) (get objects target-id) to
                                         :exact? exact?)

                     {:keys [timeline pivot-kept?]}
                     (cta/paste-track timeline target-id
                                      (shift-track track (mth/round (+ offset (* n stagger))))
                                      mode)]
                 (-> result
                     (assoc-in [:timelines board-id] timeline)
                     (update :skipped + skipped)
                     (update :pivot-kept + (if pivot-kept? 1 0))
                     (update :targets conj target-id))))
             {:timelines timelines :skipped 0 :pivot-kept 0 :targets #{}}
             (map-indexed vector pairs))]
        (assoc result :grown
               (into #{}
                     (filter (fn [board-id]
                               (when-let [before (get timelines board-id)]
                                 (> (dm/get-in result [:timelines board-id :duration])
                                    (:duration before)))))
                     (keys (:timelines result))))))))

(defn generate-paste-animation
  "Paste the animation clip `clip` onto the layers of `pairs` of `page`
  of the file `file-id` (see `paste-animation`): what it pasted and left
  out, with the `:changes` doing it, one undo step for every board. The
  `{:error ...}` of `paste-animation`, without changes, when it pasted
  nothing."
  [changes file-id page clip pairs opts]
  (let [result (paste-animation file-id (:id page) (:timelines page) (:objects page) clip pairs opts)]
    (cond-> result
      (nil? (:error result))
      (assoc :changes (commit-timelines changes page (:timelines result))))))
