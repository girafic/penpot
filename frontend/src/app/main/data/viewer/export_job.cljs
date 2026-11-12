;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.export-job
  "The animated export of a board from the view mode (Penpot Motion), in
  the tab the workspace exports in (see
  `app.main.data.exports.animation-tab`), which loads the file itself.
  The view mode has nothing to save: it answers the tab at once
  (`:saved`), then mirrors how far the tab is as the workspace does (see
  `deaw/start-export-job`), until it ends. Light: none of the encoders,
  nor the workspace."
  (:require
   [app.main.data.exports.animation-job :as job]
   [app.main.router :as rt]
   [app.util.dom :as dom]))

(defn job-of
  "The job of exporting the board `board-id` of the page `page-id` of the
  file `file-id` shown, as `params` (see `job/job-params`), with the
  renderer `wasm?`."
  [file-id page-id board-id wasm? params]
  (job/job-params {:file-id file-id :page-id page-id :board-id board-id :wasm? wasm?}
                  params))

(defn started
  "How far `job` is as its tab opens (see `uea/export-button-state`)."
  [job]
  {:in-progress true
   :in-tab? true
   :job job
   :format (:format job)
   :current 0
   :total 0
   :percent 0
   :cancelling? false
   :paused? false
   :opened? false
   :offer? false})

(defn heard
  "How far an export is, `progress`, once its tab said `message`: nil
  once it ended (done, failed, cancelled, the tab closed)."
  [progress {:keys [type] :as message}]
  (when (some? progress)
    (case type
      :hello    (assoc progress :opened? true)
      :progress (merge progress (select-keys message [:phase :current :total :percent]))
      :paused   (assoc progress :paused? true)
      :resumed  (assoc progress :paused? false)
      (:done :error :cancelled :closed) nil
      progress)))

(defn open!
  "Open the tab of `job` in the click that asks for it (a tab opened later
  is blocked), at the address `router` gives it, its channel open first:
  what the tab says goes to `on-message`, and each `:hello` is answered
  with `:saved`. The channel (`{:post :close}`), nil without one (no
  `BroadcastChannel`: no export then). `channel-ctor` (nil for none) and
  `open` replace `BroadcastChannel` and `dom/open-new-window` (in tests)."
  [router job on-message & {:keys [open] :as opts}]
  (let [post*   (volatile! nil)
        ctor    (if (contains? opts :channel-ctor)
                  (:channel-ctor opts)
                  (job/default-channel-ctor))
        channel (job/open-channel (:job-id job)
                                  (fn [message]
                                    (when (= :hello (:type message))
                                      (some-> @post* (apply [{:type :saved}])))
                                    (on-message message))
                                  ctor)]
    (when (some? channel)
      (vreset! post* (:post channel))
      (when-let [uri (rt/resolve-uri router :export-animation (job/encode-params job))]
        ((or open dom/open-new-window) uri "_blank" "noopener"))
      channel)))
