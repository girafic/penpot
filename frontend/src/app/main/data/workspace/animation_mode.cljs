;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.animation-mode
  "Where motion mode stands in the store: whether it is on, the board its
  timeline targets, and whether the timeline covers the workspace with the
  canvas in a small window over it. Kept apart from
  `app.main.data.workspace.animation`, so that what draws the canvas (its
  rulers, see `app.render-wasm.rulers-state`) can ask too."
  (:require
   [app.common.files.helpers :as cfh]
   [app.main.data.helpers :as dsh]))

(defn motion-mode?
  "Whether the workspace is in motion mode, with the timeline dock."
  [state]
  (contains? (:workspace-layout state) :animation-timeline))

(defn selection-board-id
  "The board (top-level frame) of the selection `selected` (ids) of the
  page `objects`: the selected board itself, or the root frame holding
  the selected shapes. `nil` when nothing is selected."
  [objects selected]
  (when-let [shape-id (first selected)]
    (cfh/get-shape-id-root-frame objects shape-id)))

(defn target-board-id
  "The board the timeline dock targets for the selection `selected` of
  the page `objects` and the animation state `anim`: the board of the
  selection or, when nothing is selected, the last targeted one (see
  `set-active-board`), so the timeline stays visible and playable."
  [objects selected anim]
  (or (selection-board-id objects selected)
      (:board-id anim)))

(defn active-board-id
  "The board the timeline dock currently targets (see `target-board-id`)."
  [state]
  (target-board-id (dsh/lookup-page-objects state)
                   (dsh/lookup-selected state)
                   (:workspace-animation state)))

(defn timeline-maximized?
  "Whether the timeline dock covers the whole workspace, the sidebars
  too, like a maximized area in Blender, with the canvas in a small
  window over it; `anim` is the `:workspace-animation` state."
  [anim]
  (true? (:maximized? anim)))

(defn canvas-in-window?
  "Whether the canvas is in its small window over the maximized timeline:
  in motion mode, maximized, with a board on the page for the timeline to
  show. Without one, the timeline docks (see `app.main.ui.workspace.timeline`)."
  [state]
  (and (motion-mode? state)
       (timeline-maximized? (:workspace-animation state))
       (some? (dsh/lookup-shape state (active-board-id state)))))
