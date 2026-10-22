;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.prototype-preview
  "The preview of the prototype in a window over the workspace (Shift +
  Space, see `app.main.ui.workspace.prototype-preview`): the view mode of
  the page, from the board of the selection."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn preview
  "The preview open in `state`, `{:page-id :frame-id}`; nil while it is
  closed."
  [state]
  (dm/get-in state [:workspace-local :prototype-preview]))

(defn start-frame-id
  "The board of the page the preview starts at: the first board of the
  selection or the board the first selected shape is in; nil to start
  where the view mode starts (its first flow, or its first board)."
  [objects selected]
  (some (fn [id]
          (let [frame-id (cfh/get-shape-id-root-frame objects id)]
            (when (and (some? frame-id) (not= uuid/zero frame-id))
              frame-id)))
        selected))

(defn open-preview
  "The preview open, from the board of the selection (see
  `start-frame-id`)."
  []
  (ptk/reify ::open-preview
    ptk/UpdateEvent
    (update [_ state]
      (let [objects  (dsh/lookup-page-objects state)
            selected (dm/get-in state [:workspace-local :selected])]
        (assoc-in state [:workspace-local :prototype-preview]
                  {:page-id (:current-page-id state)
                   :frame-id (start-frame-id objects selected)})))))

(defn close-preview
  []
  (ptk/reify ::close-preview
    ptk/UpdateEvent
    (update [_ state]
      (d/update-when state :workspace-local dissoc :prototype-preview))))

(defn toggle-preview
  "The preview closed when it is open, or opened."
  []
  (ptk/reify ::toggle-preview
    ptk/WatchEvent
    (watch [_ state _]
      (rx/of (if (some? (preview state)) (close-preview) (open-preview))))))
