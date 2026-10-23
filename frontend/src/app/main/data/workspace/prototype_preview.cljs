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
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(def play-modes
  "What the play button of the workspace header can do: open the view
  mode, or this preview."
  #{:view-mode :preview})

(defn play-mode
  "What the play button does, as picked last in its menu (kept in the
  browser): `:view-mode` until then."
  []
  (let [mode (get @storage/user ::play-mode)]
    (if (contains? play-modes mode) mode :view-mode)))

(defn set-play-mode!
  "Keep `mode` (one of `play-modes`) as what the play button does."
  [mode]
  (when (contains? play-modes mode)
    (swap! storage/user assoc ::play-mode mode)))

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

(defn pin-step
  "What the pin of the preview does for `board`, the board of the
  selection (see `start-frame-id`): nil, nothing, for none or the one it
  asked for already (`requested`); `:open`, start the preview again on
  the page shown (`current-page`), not the one of the preview (`page`);
  `:start`, no view mode yet (not `ready?`): start it there; `:go`, the
  view mode `listening?`: send it there; `:hold`, the view mode loads:
  send it there once it listens (see `ready-messages`)."
  [{:keys [board requested current-page page ready? listening?]}]
  (cond
    (or (nil? board) (= board requested)) nil
    (not= current-page page)              :open
    (not ready?)                          :start
    listening?                            :go
    :else                                 :hold))

(def interactions-modes
  "How the view mode of the preview shows the interactions of the
  prototype, as its settings pick it (see `dv/set-interactions-mode`)."
  #{"hide" "show" "show-on-click"})

(defn ready-messages
  "What the preview tells a view mode that says it listens, as
  `[[type data] ...]`: its `zoom`, how it shows the `interactions`, and,
  while pinned (`follow?`), to go to the board `unsent` the pin held for
  it (see `pin-step`)."
  [{:keys [zoom interactions unsent follow?]}]
  (cond-> [["penpot:preview-zoom" {:mode zoom}]]
    (contains? interactions-modes interactions)
    (conj ["penpot:preview-interactions" {:mode interactions}])

    (and follow? (some? unsent))
    (conj ["penpot:preview-go" {:frameId (dm/str unsent)}])))

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
