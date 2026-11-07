;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.phone
  "The workspace as a view mode on a phone follows it (see
  `app.main.data.viewer.follow`): once it heard the view mode of its
  file, it tells the board of its selection as it changes, for
  `dvf/followed-ms` after the last time it heard it."
  (:require
   [app.common.data.macros :as dm]
   [app.common.time :as ct]
   [app.main.data.viewer.follow :as dvf]
   [app.main.data.websocket :as dws]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn followed?
  "Whether a view mode follows the workspace in `state` at the time
  `now` (ms)."
  [state now]
  (> (or (dm/get-in state [:workspace-local ::followed-until]) 0) now))

(defn board-selected
  "The board `frame-id` of the page `page-id` selected in the workspace
  (nil for none): told to the view modes that follow it, unless it told
  it last."
  [page-id frame-id]
  (let [tell? (volatile! false)]
    (ptk/reify ::board-selected
      ptk/UpdateEvent
      (update [_ state]
        (let [told (dm/get-in state [:workspace-local ::told])]
          (if (and (some? frame-id)
                   (followed? state (inst-ms (ct/now)))
                   (not= told [page-id frame-id]))
            (do (vreset! tell? true)
                (assoc-in state [:workspace-local ::told] [page-id frame-id]))
            state)))

      ptk/WatchEvent
      (watch [_ state _]
        (if @tell?
          (rx/of (dws/send (dvf/board-message (:current-file-id state) page-id frame-id)))
          (rx/empty))))))

(defn view-mode-heard
  "A view mode of the file `file-id` says it follows (see
  `dvf/hello-message`): the workspace of that file tells it the board of
  its selection now when none followed it, and as it changes until
  `dvf/followed-ms` after."
  [{:keys [file-id]}]
  (ptk/reify ::view-mode-heard
    ptk/UpdateEvent
    (update [_ state]
      (if (= file-id (:current-file-id state))
        (let [now (inst-ms (ct/now))]
          (cond-> (assoc-in state [:workspace-local ::followed-until] (+ now dvf/followed-ms))
            ;; a view mode that starts gets the board selected
            (not (followed? state now))
            (update :workspace-local dissoc ::told)))
        state))))
