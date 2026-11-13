;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.export-job
  "The animated export of a board from the view mode (Penpot Motion), in
  the tab the workspace exports in (see
  `app.main.data.exports.animation-tab`), which loads the file itself.
  The view mode has nothing to save: its jobs say so (`:saved?`), and
  their tab loads at once, waiting for nobody. The view mode follows its
  jobs under `:viewer-export-animation` as the workspace does (see
  `app.main.data.exports.animation-follow`): the job lives in the store,
  and the panel may go and come back meanwhile. Light: none of the
  encoders, nor the workspace."
  (:require
   [app.main.data.exports.animation-follow :as follow]
   [app.main.data.exports.animation-job :as job]
   [app.main.router :as rt]
   [app.util.dom :as dom]))

(defn board-job
  "The job (see `job/schema:params`) exporting the board `board-id` of
  the page `page-id` of the file `file-id` shown, or the `boards` of
  `params` into a ZIP, as `params` (see `job/job-params`), with the
  renderer `wasm?`, and nothing to save first."
  [{:keys [file-id page-id board-id wasm?]} params]
  (job/job-params {:file-id file-id :page-id page-id :board-id board-id :wasm? wasm? :saved? true}
                  params))

(defn open-job-tab
  "Open the tab of `job` with `noopener`, at the address `router` gives
  it, in the click that asks for it (a tab opened later is blocked): it
  says it is there on the channel of the job. `open` replaces
  `dom/open-new-window` (in tests)."
  [router job & {:keys [open]}]
  (when-let [uri (rt/resolve-uri router :export-animation (job/encode-params job))]
    ((or open dom/open-new-window) uri "_blank" "noopener")))

(defn start-job
  "Follow the tab of `job` (see `board-job`), opened by the click (see
  `open-job-tab`), until the export ends (see `follow/start-job`, which
  takes `opts`)."
  [job & {:as opts}]
  (follow/start-job :viewer-export-animation job opts))

(defn job-tab-reopened
  "The panel opened the tab of the job `job-id` again (see
  `follow/job-tab-reopened`, which takes `opts`)."
  [job-id & {:as opts}]
  (follow/job-tab-reopened :viewer-export-animation job-id opts))

(defn cancel-job
  "Cancel the export in its tab (see `follow/cancel-job`)."
  []
  (follow/cancel-job :viewer-export-animation))
