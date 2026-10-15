;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.team-container-test
  (:require
   [app.common.uuid :as uuid]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui :as ui]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [frontend-tests.helpers.mock :as mock]
   [rumext.v2 :as mf]))

(t/deftest the-team-container-renders-whatever-the-team-id
  ;; It calls the same React hooks on every render, as React needs: the
  ;; id of a team that is not loaded yet, as a route has while it
  ;; changes, renders nothing, then the team again. (An id that is no
  ;; uuid raises `:invalid-team-id`.)
  (let [team-id (uuid/next)]
    (with-redefs [refs/team (atom {:id team-id :permissions {:can-edit true}})
                  st/emit!  mock/noop]
      (thd/with-dom
        (fn [{:keys [root document]}]
          (let [render! (fn [id]
                          (thd/render! root (mf/element @#'ui/team-container*
                                                        #js {:teamId id :children (mf/html [:p "team"])})))
                shown?  #(some? (.querySelector document "p"))]
            (render! team-id)
            (t/is (shown?))
            (render! (uuid/next))
            (t/is (not (shown?)) "none for a team not loaded")
            (render! team-id)
            (t/is (shown?) "the team again")))))))
