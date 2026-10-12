;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline.audio-lane
  "What the labels of an audio lane share, in the timeline dock and in the
  audio of the design tab: renaming the file the lane plays (see
  `dwaa/rename-lane`)."
  (:require
   [app.main.data.workspace.audio :as dwaa]
   [app.main.store :as st]
   [rumext.v2 :as mf]))

(defn use-lane-rename
  "Renaming the file of the audio `lane` from its label, a double click
  starting it: `[renaming? on-start on-cancel on-rename]`, `on-rename`
  taking the new name."
  [lane]
  (let [renaming* (mf/use-state false)

        on-start
        (mf/use-fn #(reset! renaming* true))

        on-cancel
        (mf/use-fn #(reset! renaming* false))

        on-rename
        (mf/use-fn
         (mf/deps lane)
         (fn [value]
           (reset! renaming* false)
           (st/emit! (dwaa/rename-lane lane value))))]
    [(deref renaming*) on-start on-cancel on-rename]))
