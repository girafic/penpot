;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.render-wasm.render-options-test
  (:require
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.store :as st]
   [app.render-wasm.api :as api]
   [cljs.test :as t :include-macros true]))

(def ^:private info-flag
  "The render option that labels the canvas with its renderer."
  2r1000)

(defn- canvas-window-state
  "A page holding a board that the maximized timeline of motion mode
  shows, the canvas in its small window over it."
  []
  (let [file-id  (uuid/next)
        page-id  (uuid/next)
        board-id (uuid/next)
        board    (cts/setup-shape {:id board-id :type :frame :x 0 :y 0 :width 100 :height 100})]
    {:workspace-layout #{:animation-timeline}
     :workspace-animation {:board-id board-id :maximized? true}
     :current-file-id file-id
     :current-page-id page-id
     :files {file-id {:id file-id
                      :data {:pages-index {page-id {:id page-id
                                                    :objects {board-id board}}}}}}}))

(defn- labelled?
  "Whether the canvas of `state` is labelled with its renderer."
  [state]
  (with-redefs [cf/flags (conj cf/flags :render-wasm-info)
                st/state (atom state)]
    (pos? (bit-and info-flag (#'api/debug-flags)))))

(t/deftest the-canvas-window-has-no-renderer-label
  ;; It shows the animation alone, over the maximized timeline.
  (let [state (canvas-window-state)]
    (t/is (not (labelled? state)))
    (t/is (labelled? (update state :workspace-layout disj :animation-timeline))
          "the canvas of the workspace has")
    (t/is (labelled? (assoc-in state [:workspace-animation :maximized?] false))
          "and with the timeline docked")))
