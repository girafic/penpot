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
  their tab loads at once, waiting for nobody. The view mode mirrors
  how far the tab is under `:viewer-export-animation`, as the workspace
  does (see `deaw/start-export-job`, the rules of both in
  `app.main.data.exports.animation-job`), until the export ends, is
  cancelled (or the tab says nothing for `job/cancel-wait-ms`), or the
  tab goes (its lock is free, where the browser has Web Locks). The job
  lives in the store: the panel may go and come back meanwhile. Light:
  none of the encoders, nor the workspace."
  (:require
   [app.main.data.exports.animation-job :as job]
   [app.main.data.notifications :as ntf]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defonce ^:private jobs
  ;; job-id -> {:channel :locks :lock-asked? :cancel-wait-ms}
  (atom {}))

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

(defn- current-job-id
  [state]
  (get-in state [:viewer-export-animation :job :job-id]))

(defn- job-post
  [job-id message]
  (when-let [post (get-in @jobs [job-id :channel :post])]
    (post message)))

(defn- job-ended
  "The job `job-id` is over: the tab hears `message` if any, the channel
  closes (see `start-job`) and the panel is free."
  ([job-id] (job-ended job-id nil))
  ([job-id message]
   (ptk/reify ::job-ended
     IDeref
     (-deref [_] job-id)

     ptk/UpdateEvent
     (update [_ state]
       (cond-> state
         (= job-id (current-job-id state))
         (dissoc :viewer-export-animation)))

     ptk/EffectEvent
     (effect [_ _ _]
       (when message
         (job-post job-id message))))))

(defn- job-ended?
  [job-id event]
  (and (= ::job-ended (ptk/type event))
       (= job-id @event)))

(defn- job-heard
  "A message of the tab of the job `job-id` (see `job/heard`): once it
  says it is there, the view mode asks for the lock the tab holds, which
  it gets when the tab is gone; what ends the job ends it, an error with
  its notification."
  [job-id {:keys [type] :as message}]
  (ptk/reify ::job-heard
    IDeref
    (-deref [_] (assoc message :job-id job-id))

    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= job-id (current-job-id state))
        (update :viewer-export-animation job/heard message)))

    ptk/WatchEvent
    (watch [_ state _]
      (cond
        (not= job-id (current-job-id state)) (rx/empty)
        (= :error type)                      (rx/of (ntf/error (apply tr (job/error-text message)))
                                                    (job-ended job-id))
        (job/ended? message)                 (rx/of (job-ended job-id))
        :else                                (rx/empty)))

    ptk/EffectEvent
    (effect [_ _ _]
      (when (= :hello type)
        (when-let [{:keys [locks lock-asked?]} (get @jobs job-id)]
          (when-not lock-asked?
            (swap! jobs assoc-in [job-id :lock-asked?] true)
            (-> (job/when-released job-id (or locks (job/default-locks)))
                (.then #(st/emit! (job-ended job-id)))
                (.catch (constantly nil)))))))))

(defn- job-unanswered
  [job-id]
  (ptk/reify ::job-unanswered
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (and (= job-id (current-job-id state))
             (not (get-in state [:viewer-export-animation :opened?])))
        (assoc-in [:viewer-export-animation :offer?] true)))))

(defn- hello-wait
  "The offer to open the tab of the job `job-id` again when it says
  nothing within `wait-ms`."
  [job-id wait-ms stream]
  (let [heard (rx/filter (fn [event]
                           (or (job-ended? job-id event)
                               (and (= ::job-heard (ptk/type event))
                                    (let [{:keys [type] :as message} @event]
                                      (and (= :hello type) (= job-id (:job-id message)))))))
                         stream)]
    (->> (rx/timer wait-ms)
         (rx/take-until heard)
         (rx/map (fn [_] (job-unanswered job-id))))))

(defn start-job
  "Mirror how far the tab of `job` (see `board-job`), opened by the
  click (see `open-job-tab`), is, until the export ends. When the tab
  says nothing within `job/hello-wait-ms`, the panel offers to open it
  again (see `job-tab-reopened`). Without a channel (no
  `BroadcastChannel`) the tab exports on its own and the panel is free at
  once. `channel-ctor`, `locks`, `hello-wait-ms` and `cancel-wait-ms`
  replace `BroadcastChannel`, `navigator.locks` and the waits of
  `animation-job` (in tests)."
  [job & {:keys [locks] :as opts}]
  (let [job-id (:job-id job)
        ctor   (if (contains? opts :channel-ctor)
                 (:channel-ctor opts)
                 (job/default-channel-ctor))]
    (ptk/reify ::start-job
      ptk/UpdateEvent
      (update [_ state]
        (assoc state :viewer-export-animation (job/started job)))

      ptk/WatchEvent
      (watch [_ _ stream]
        (if (nil? ctor)
          (rx/of (job-ended job-id))
          (let [ended (rx/filter #(job-ended? job-id %) stream)
                ;; the channel is open while the job lasts
                heard (rx/create
                       (fn [subs]
                         (let [channel (job/open-channel job-id #(rx/push! subs %) ctor)]
                           (swap! jobs assoc job-id {:channel channel
                                                     :locks locks
                                                     :cancel-wait-ms (get opts :cancel-wait-ms job/cancel-wait-ms)})
                           (fn []
                             (when-let [close (:close channel)]
                               (close))
                             (swap! jobs dissoc job-id)))))]
            (rx/merge
             (->> heard
                  (rx/map #(job-heard job-id %))
                  (rx/take-until ended))
             (hello-wait job-id (get opts :hello-wait-ms job/hello-wait-ms) stream))))))))

(defn job-tab-reopened
  "The panel opened the tab of the job `job-id` again (see
  `open-job-tab`): it waits for its word once more (`hello-wait-ms`,
  replaced in tests)."
  [job-id & {:as opts}]
  (ptk/reify ::job-tab-reopened
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= job-id (current-job-id state))
        (assoc-in [:viewer-export-animation :offer?] false)))

    ptk/WatchEvent
    (watch [_ _ stream]
      (hello-wait job-id (get opts :hello-wait-ms job/hello-wait-ms) stream))))

(defn cancel-job
  "Cancel the export in its tab, which says when it stopped (or the panel
  is free after `job/cancel-wait-ms`); a tab that has not said it is
  there yet is given up at once."
  []
  (ptk/reify ::cancel-job
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (contains? state :viewer-export-animation)
        (assoc-in [:viewer-export-animation :cancelling?] true)))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [opened? job]} (:viewer-export-animation state)
            job-id                (:job-id job)]
        (cond
          (nil? job-id) (rx/empty)
          (not opened?) (rx/of (job-ended job-id {:type :cancel}))
          :else
          (let [ended (rx/filter #(job-ended? job-id %) stream)]
            (->> (rx/timer (get-in @jobs [job-id :cancel-wait-ms] job/cancel-wait-ms))
                 (rx/take-until ended)
                 (rx/map (fn [_] (job-ended job-id {:type :cancel}))))))))

    ptk/EffectEvent
    (effect [_ state _]
      (let [{:keys [opened? job]} (:viewer-export-animation state)]
        (when opened?
          (job-post (:job-id job) {:type :cancel}))))))
