;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.animation
  "Events for the keyframe based timeline animation feature (Penpot
  Motion). Persistent edits go through the `:set-timeline` change so they
  are undoable & synced; playhead / playback state is transient and lives
  under `:workspace-animation`."
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.types.animation :as cta]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.layout :as layout]
   [app.main.data.workspace.modifiers :as dwm]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(def ^:private frame-step
  "Playback tick in ms (~60fps)."
  16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-timelines
  [state]
  (get (dsh/lookup-page state) :timelines))

(defn current-timeline-id
  [state]
  (dm/get-in state [:workspace-animation :current-id]))

(defn current-timeline
  [state]
  (get (get-timelines state) (current-timeline-id state)))

(defn playhead
  [state]
  (dm/get-in state [:workspace-animation :playhead] 0))

(defn- shape-property-value
  "Capture the current value of an animatable property of `shape` so it
  can be stored as a keyframe. Scale defaults to 1 (authored by editing
  the keyframe value)."
  [shape property]
  (case property
    :x        (-> shape :selrect :x)
    :y        (-> shape :selrect :y)
    :rotation (or (:rotation shape) 0)
    :opacity  (or (:opacity shape) 1)
    :scale-x  1
    :scale-y  1
    nil))

(declare select-timeline)
(declare apply-preview)

(defn- commit-timeline
  "Build a change that sets (or, when `timeline` is nil, deletes) a
  timeline on the current page."
  [it state id timeline]
  (let [page (dsh/lookup-page state)]
    (dch/commit-changes
     (-> (pcb/empty-changes it)
         (pcb/with-page page)
         (pcb/set-timeline id timeline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TIMELINE CRUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-timeline
  ([] (create-timeline nil))
  ([opts]
   (ptk/reify ::create-timeline
     ptk/WatchEvent
     (watch [it state _]
       (let [timeline (cta/make-timeline (or opts {}))]
         (rx/of (commit-timeline it state (:id timeline) timeline)
                (select-timeline (:id timeline))
                (layout/toggle-layout-flag :animation-timeline :force? true)))))))

(defn select-timeline
  [id]
  (ptk/reify ::select-timeline
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-animation :current-id] id)
          (assoc-in [:workspace-animation :playhead] 0)
          (assoc-in [:workspace-animation :playing?] false)
          (update :workspace-animation dissoc :selected-kf)))))

(defn delete-timeline
  [id]
  (ptk/reify ::delete-timeline
    ptk/WatchEvent
    (watch [it state _]
      (rx/of (commit-timeline it state id nil)
             (dwm/clear-local-transform)))))

(defn- update-current-timeline
  "Apply `f` to the current timeline and commit the result."
  [f]
  (ptk/reify ::update-current-timeline
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (f tl)]
          (rx/of (commit-timeline it state (:id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn set-duration
  [duration]
  (update-current-timeline #(assoc % :duration (max 1 (int duration)))))

(defn rename-timeline
  [name]
  (update-current-timeline #(assoc % :name name)))

(defn set-loop
  [loop?]
  (update-current-timeline #(assoc % :loop (boolean loop?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYFRAME CRUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-keyframe
  "Add a keyframe at the playhead for `property` on every selected shape,
  capturing each shape's current value."
  [property]
  (ptk/reify ::add-keyframe
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [objects  (dsh/lookup-page-objects state)
              selected (dsh/lookup-selected state)
              time     (playhead state)
              tl'      (reduce
                        (fn [tl shape-id]
                          (let [shape (get objects shape-id)
                                value (shape-property-value shape property)]
                            (cond-> tl
                              (some? value)
                              (cta/add-keyframe shape-id
                                                {:time time
                                                 :property property
                                                 :value value
                                                 :easing :ease}))))
                        tl
                        selected)]
          (rx/of (commit-timeline it state (:id tl') tl')))
        (rx/empty)))))

(defn move-keyframe
  [shape-id keyframe-id time]
  (ptk/reify ::move-keyframe
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/update-keyframe tl shape-id keyframe-id
                                       #(assoc % :time (max 0 (int time))))]
          (rx/of (commit-timeline it state (:id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn set-keyframe-value
  [shape-id keyframe-id value]
  (ptk/reify ::set-keyframe-value
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/update-keyframe tl shape-id keyframe-id
                                       #(assoc % :value value))]
          (rx/of (commit-timeline it state (:id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn set-keyframe-easing
  [shape-id keyframe-id easing]
  (ptk/reify ::set-keyframe-easing
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/update-keyframe tl shape-id keyframe-id
                                       #(assoc % :easing easing))]
          (rx/of (commit-timeline it state (:id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn delete-keyframe
  [shape-id keyframe-id]
  (ptk/reify ::delete-keyframe
    ptk/WatchEvent
    (watch [it state _]
      (if-let [tl (current-timeline state)]
        (let [tl' (cta/remove-keyframe tl shape-id keyframe-id)]
          (rx/of (commit-timeline it state (:id tl') tl')
                 (apply-preview)))
        (rx/empty)))))

(defn select-keyframe
  "Mark a keyframe as selected (for the easing editor). `nil` clears it."
  [shape-id keyframe-id]
  (ptk/reify ::select-keyframe
    ptk/UpdateEvent
    (update [_ state]
      (if (and shape-id keyframe-id)
        (assoc-in state [:workspace-animation :selected-kf]
                  {:shape-id shape-id :keyframe-id keyframe-id})
        (update state :workspace-animation dissoc :selected-kf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAYBACK & PREVIEW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-preview
  "Recompute and apply the transient transform modifiers for the current
  timeline at the playhead (editor scrubbing/playback preview). Opacity
  is animated in viewer/export; the editor preview covers the transform
  properties through the existing workspace-modifiers merge."
  []
  (ptk/reify ::apply-preview
    ptk/WatchEvent
    (watch [_ state _]
      (if-let [tl (current-timeline state)]
        (let [objects    (dsh/lookup-page-objects state)
              time       (playhead state)
              modif-tree (cta/timeline->modif-tree tl objects time)]
          (rx/of (dwm/set-modifiers modif-tree)))
        (rx/empty)))))

(defn set-playhead
  [time]
  (ptk/reify ::set-playhead
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :playhead] (max 0 (int time))))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (apply-preview)))))

(declare pause)

(defn play
  []
  (ptk/reify ::play
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :playing?] true))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [tl       (current-timeline state)
            duration (or (:duration tl) 0)
            loop?    (boolean (:loop tl))
            start    (let [p (playhead state)] (if (>= p duration) 0 p))
            stopper  (rx/filter (ptk/type? ::pause) stream)]
        (if (or (nil? tl) (<= duration 0))
          (rx/empty)
          (rx/concat
           (->> (rx/interval frame-step)
                (rx/map (fn [i] (+ start (* (inc i) frame-step))))
                (rx/map (fn [t] (if loop? (mod t duration) t)))
                (rx/take-while (fn [t] (or loop? (<= t duration))))
                (rx/take-until stopper)
                (rx/map set-playhead))
           (rx/of (pause)))))))) ;; auto-pause when reaching the end

(defn pause
  []
  (ptk/reify ::pause
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-animation :playing?] false))))

(defn toggle-play
  []
  (ptk/reify ::toggle-play
    ptk/WatchEvent
    (watch [_ state _]
      (if (dm/get-in state [:workspace-animation :playing?])
        (rx/of (pause))
        (rx/of (play))))))

(defn stop
  "Stop playback and rewind to the start."
  []
  (ptk/reify ::stop
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (pause) (set-playhead 0)))))

(defn close-timeline
  "Hide the timeline dock and clear any preview modifiers."
  []
  (ptk/reify ::close-timeline
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (pause)
             (dwm/clear-local-transform)
             (layout/remove-layout-flag :animation-timeline)))))

(defn toggle-auto-keyframe
  []
  (ptk/reify ::toggle-auto-keyframe
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:workspace-animation :auto-key?] not))))
