;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.phone
  "The board of the selection, told to a view mode on a phone that
  follows the workspace (see `app.main.data.workspace.phone`)."
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.workspace.phone :as dwph]
   [app.main.data.workspace.prototype-preview :as dwpp]
   [app.main.store :as st]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:followed-until
  (l/derived #(dm/get-in % [:workspace-local ::dwph/followed-until]) st/state))

(mf/defc phone-follow*
  "Nothing on screen: once a view mode followed the workspace, the board
  of the `selected` shapes of the `page` as it changes."
  [{:keys [page selected]}]
  (let [followed-until (mf/deref ref:followed-until)
        page-id        (:id page)
        board-id       (when (some? followed-until)
                         (dwpp/start-frame-id (:objects page) selected))]
    (mf/with-effect [followed-until page-id board-id]
      (when (some? board-id)
        (st/emit! (dwph/board-selected page-id board-id))))
    nil))
