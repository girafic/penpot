;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.animation-follow
  "How the side that started an animated export job follows its tab (see
  `app.main.data.exports.animation-tab`): how far it is under a
  `state-key` of the store (see `job/started`, `job/heard`), the offer
  to open the tab again when it says nothing (`job/hello-wait-ms`), the
  end when the tab goes (its lock is free), and a Cancel that frees the
  panel after `job/cancel-wait-ms` when the tab says nothing. The
  workspace (`:export-animation`, see `deaw/start-export-job`) and the
  view mode (`:viewer-export-animation`, see `vej/start-job`) follow
  their jobs by it, each adding what it does of its own through hooks.
  Light: none of the encoders, nor the workspace."
  (:require
   [app.main.data.exports.animation-job :as job]
   [app.main.data.notifications :as ntf]
   [app.main.store :as st]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defonce ^:private jobs
  ;; job-id -> {:channel :locks :lock-asked? :cancel-wait-ms}
  (atom {}))

(defn- current-job-id
  [state state-key]
  (get-in state [state-key :job :job-id]))

(defn post
  "Tell the tab of the job `job-id` `message`, while the job lasts."
  [job-id message]
  (when-let [post (get-in @jobs [job-id :channel :post])]
    (post message)))

(defn job-ended
  "The job `job-id`, followed under `state-key`, is over: the tab hears
  `message` if any, the channel closes (see `start-job`) and the panel is
  free."
  ([state-key job-id] (job-ended state-key job-id nil))
  ([state-key job-id message]
   (ptk/reify ::job-ended
     IDeref
     (-deref [_] job-id)

     ptk/UpdateEvent
     (update [_ state]
       (cond-> state
         (= job-id (current-job-id state state-key))
         (dissoc state-key)))

     ptk/EffectEvent
     (effect [_ _ _]
       (when message
         (post job-id message))))))

(defn- job-ended?
  "Whether `event` ends the job `job-id` (see `job-ended`)."
  [job-id event]
  (and (= ::job-ended (ptk/type event))
       (= job-id @event)))

(defn- job-heard
  "A message of the tab of the job `job-id` (see `job/heard`): at its
  first word the side that follows it asks for the lock the tab holds,
  which it gets when the tab is gone; at each `:hello` it does
  `on-hello` (the job-id and the message); what ends the job ends it, an
  error with its notification, `:done` after `done-events` (of the
  message)."
  [state-key job-id {:keys [type] :as message} {:keys [on-hello done-events]}]
  (ptk/reify ::job-heard
    IDeref
    (-deref [_] (assoc message :job-id job-id))

    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= job-id (current-job-id state state-key))
        (update state-key job/heard message)))

    ptk/WatchEvent
    (watch [_ state _]
      (cond
        (not= job-id (current-job-id state state-key))
        (rx/empty)

        (= :error type)
        (rx/of (ntf/error (apply tr (job/error-text message)))
               (job-ended state-key job-id))

        (= :done type)
        (rx/from (conj (vec (when (fn? done-events) (done-events message)))
                       (job-ended state-key job-id)))

        (job/ended? message)
        (rx/of (job-ended state-key job-id))

        :else
        (rx/empty)))

    ptk/EffectEvent
    (effect [_ _ _]
      (when-let [{:keys [locks lock-asked?]} (get @jobs job-id)]
        (when (and (= :hello type) (fn? on-hello))
          (on-hello job-id message))
        ;; at its first word, whichever: its `:hello` may be lost
        (when (and (not lock-asked?) (not (job/ended? message)))
          (swap! jobs assoc-in [job-id :lock-asked?] true)
          (-> (job/when-released job-id (or locks (job/default-locks)))
              (.then #(st/emit! (job-ended state-key job-id)))
              (.catch (constantly nil))))))))

(defn- job-unanswered
  [state-key job-id]
  (ptk/reify ::job-unanswered
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (and (= job-id (current-job-id state state-key))
             (not (get-in state [state-key :opened?])))
        (assoc-in [state-key :offer?] true)))))

(defn- hello-wait
  "The offer to open the tab of the job `job-id` again when it says
  nothing within `wait-ms`: any word of it stops it."
  [state-key job-id wait-ms stream]
  (let [heard (rx/filter (fn [event]
                           (or (job-ended? job-id event)
                               (and (= ::job-heard (ptk/type event))
                                    (= job-id (:job-id @event)))))
                         stream)]
    (->> (rx/timer wait-ms)
         (rx/take-until heard)
         (rx/map (fn [_] (job-unanswered state-key job-id))))))

(defn start-job
  "Follow under `state-key` the tab of `job`, opened by the click that
  asks for it, until the export ends, is cancelled, or the tab goes (its
  lock is free, where the browser has Web Locks). When the tab says
  nothing within `job/hello-wait-ms`, the panel offers to open it again
  (see `job-tab-reopened`). Without a channel (no `BroadcastChannel`)
  the panel is free at once: the tab exports on its own. `opts`:
  `extra`, a fn of the job-id and the stream of its end giving more
  events to emit as it starts (the workspace saves); `on-hello` and
  `done-events` (see `job-heard`); `channel-ctor` (nil for none),
  `locks`, `hello-wait-ms` and `cancel-wait-ms`, which replace
  `BroadcastChannel`, `navigator.locks` and the waits of `animation-job`
  (in tests)."
  [state-key job {:keys [extra locks] :as opts}]
  (let [job-id (:job-id job)
        ctor   (if (contains? opts :channel-ctor)
                 (:channel-ctor opts)
                 (job/default-channel-ctor))]
    (ptk/reify ::start-job
      ptk/UpdateEvent
      (update [_ state]
        (assoc state state-key (job/started job)))

      ptk/WatchEvent
      (watch [_ _ stream]
        (if (nil? ctor)
          (rx/of (job-ended state-key job-id))
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
                  (rx/map #(job-heard state-key job-id % opts))
                  (rx/take-until ended))
             (if (fn? extra) (extra job-id ended) (rx/empty))
             (hello-wait state-key job-id (get opts :hello-wait-ms job/hello-wait-ms) stream))))))))

(defn job-tab-reopened
  "The panel opened the tab of the job `job-id`, followed under
  `state-key`, again: it waits for its word once more (`hello-wait-ms`
  of `opts`, replaced in tests)."
  [state-key job-id opts]
  (ptk/reify ::job-tab-reopened
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= job-id (current-job-id state state-key))
        (assoc-in [state-key :offer?] false)))

    ptk/WatchEvent
    (watch [_ _ stream]
      (hello-wait state-key job-id (get opts :hello-wait-ms job/hello-wait-ms) stream))))

(defn cancel-job
  "Cancel the export in the tab of the job followed under `state-key`,
  which says when it stopped (or the panel is free after its
  `cancel-wait-ms`); a tab that has not said it is there yet is given up
  at once."
  [state-key]
  (ptk/reify ::cancel-job
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (contains? state state-key)
        (assoc-in [state-key :cancelling?] true)))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [opened? job]} (get state state-key)
            job-id                (:job-id job)]
        (cond
          (nil? job-id) (rx/empty)
          (not opened?) (rx/of (job-ended state-key job-id {:type :cancel}))
          :else
          (let [ended (rx/filter #(job-ended? job-id %) stream)]
            (->> (rx/timer (get-in @jobs [job-id :cancel-wait-ms] job/cancel-wait-ms))
                 (rx/take-until ended)
                 (rx/map (fn [_] (job-ended state-key job-id {:type :cancel}))))))))

    ptk/EffectEvent
    (effect [_ state _]
      (let [{:keys [opened? job]} (get state state-key)]
        (when opened?
          (post (:job-id job) {:type :cancel}))))))
