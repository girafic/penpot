;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.animation-workspace
  "The workspace's side of an animated export run in a tab of its own
  (see `app.main.data.exports.animation-tab`): the panel's Export opens
  the tab, saves what is pending, and mirrors in the panel how far the
  tab is, until the export ends, is cancelled or the tab goes. A version
  preview, and a browser without what the tab needs, export in place
  (`dea/export-animation`)."
  (:require
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-follow :as follow]
   [app.main.data.exports.animation-job :as job]
   [app.main.data.notifications :as ntf]
   [app.main.data.persistence :as dps]
   [app.main.data.workspace.animation :as dwa]
   [app.main.features :as features]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn export-in-tab?
  "Whether the workspace in `state` exports in a tab of its own (see
  `start-export-job`): not while it previews a version, which the tab
  would not load (it loads the file as saved), nor in a browser the tab
  could not talk to (`BroadcastChannel`) or know gone (Web Locks, which
  only secure contexts have). `globals` replaces `globalThis` (in
  tests)."
  [state & {:keys [globals] :or {globals js/globalThis}}]
  (and (nil? (get-in state [:workspace-global :preview-id]))
       (some? (unchecked-get globals "BroadcastChannel"))
       (some? (some-> (unchecked-get globals "navigator") (unchecked-get "locks")))))

(defn export-job
  "The job (see `job/schema:params`) exporting the active board of the
  workspace in `state`, or the `boards` (`[{:board-id :scale}]`) into a
  ZIP, as `params` (see `dea/export-animation`) in a tab of its own, with
  the renderer of the workspace."
  [state params]
  (job/job-params {:file-id (:current-file-id state)
                   :page-id (:current-page-id state)
                   :board-id (dwa/active-board-id state)
                   :wasm? (features/active-feature? state "render-wasm/v1")}
                  params))

(defn open-job-tab!
  "Open the tab of `job` (see `export-job`) with `noopener`, so the
  browser may give it a process of its own, in the click that asks for
  it (a tab opened later is blocked). `window.open` tells nothing then:
  the tab says it is there on the channel of the job."
  [state job]
  (when-let [uri (rt/resolve-uri (:router state) :export-animation (job/encode-params job))]
    (dom/open-new-window uri "_blank" "noopener")))

(defn- job-saved
  "The workspace has saved what was pending before the job `job-id`
  (`outcome` `:saved`), or could not (`:save-failed`, which ends it): the
  tab hears it now, and at its next `:hello` (`save`, see
  `start-export-job`)."
  [job-id save outcome]
  (ptk/reify ::job-saved
    ptk/WatchEvent
    (watch [_ state _]
      (if (and (= :save-failed outcome)
               (= job-id (get-in state [:export-animation :job :job-id])))
        (rx/of (ntf/error (tr "export-animation.error.save-failed"))
               (follow/job-ended :export-animation job-id {:type :save-failed}))
        (rx/empty)))

    ptk/EffectEvent
    (effect [_ _ _]
      (when (= :saved outcome)
        (vreset! save outcome)
        (follow/post job-id {:type outcome})))))

(defn start-export-job
  "Export as `job` (see `export-job`) in the tab the click opened (see
  `open-job-tab!`): the workspace saves what is pending and tells the
  tab, at once and at each `:hello` of it, then follows it under
  `:export-animation` (see `follow/start-job`, which takes `opts`), the
  notes of a done export shown. When the tab says nothing, the panel
  offers to open it again (see `job-tab-reopened`) or to export here
  (see `export-job-here`)."
  [job & {:as opts}]
  (let [save (volatile! nil)]
    (follow/start-job
     :export-animation job
     (assoc opts
            :extra (fn [job-id ended]
                     (rx/merge
                      (rx/of ::dps/force-persist)
                      (->> (dps/wait-persisted-or-error)
                           (rx/map (fn [_] (job-saved job-id save :saved)))
                           (rx/catch (fn [_] (rx/of (job-saved job-id save :save-failed))))
                           (rx/take-until ended))))
            :on-hello (fn [job-id _]
                        (when-let [outcome @save]
                          (follow/post job-id {:type outcome})))
            :done-events (fn [message]
                           (dea/notes-shown (:notes message)))))))

(defn job-tab-reopened
  "The panel opened the tab of the job `job-id` again (see
  `open-job-tab!`, `follow/job-tab-reopened`, which takes `opts`)."
  [job-id & {:as opts}]
  (follow/job-tab-reopened :export-animation job-id opts))

(defn export-job-here
  "Give up the tab of the job `job-id`, which did not open, and export in
  place (see `dea/export-animation`)."
  [job-id]
  (ptk/reify ::export-job-here
    ptk/WatchEvent
    (watch [_ state _]
      (let [job (get-in state [:export-animation :job])]
        (if (= job-id (:job-id job))
          (rx/of (follow/job-ended :export-animation job-id {:type :cancel})
                 (dea/export-animation (select-keys job (into [:format :quality :fps :scale :loop? :audio? :boards]
                                                              (keys job/svg-options)))))
          (rx/empty))))))

(defn export-from-panel!
  "Export the active board, or the `:boards` of `params`, as `params` (see
  `dea/export-animation`), from the click of the panel: in a tab of its
  own, opened now (see `start-export-job`), or in place (see
  `export-in-tab?`)."
  [params]
  (let [state @st/state]
    (if (export-in-tab? state)
      (let [job (export-job state params)]
        (open-job-tab! state job)
        (st/emit! (start-export-job job)))
      (st/emit! (dea/export-animation params)))))

(defn cancel-export-animation
  "Cancel the export: at its next frame in place, or in its tab (see
  `follow/cancel-job`)."
  []
  (ptk/reify ::cancel-export-animation
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (contains? state :export-animation)
        (assoc-in [:export-animation :cancelling?] true)))

    ptk/WatchEvent
    (watch [_ state _]
      (if (get-in state [:export-animation :in-tab?])
        (rx/of (follow/cancel-job :export-animation))
        (rx/empty)))

    ptk/EffectEvent
    (effect [_ state _]
      (when-not (get-in state [:export-animation :in-tab?])
        (dea/cancel-in-place!)))))
