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

(def hello-wait-ms
  "How long the workspace waits for the export tab to say it is there
  before it offers to open it again or to export in place: the browser
  may have blocked it."
  8000)

(def cancel-wait-ms
  "How long a Cancel waits for the tab to say it stopped before the panel
  is free anyway: a tab that hangs must not block it."
  10000)

(defonce ^:private jobs
  ;; job-id -> {:channel :locks :save :lock-asked? :cancel-wait-ms}
  (atom {}))

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
  [state {:keys [format quality fps scale loop? audio? boards]}]
  (cond-> {:file-id (:current-file-id state)
           :page-id (:current-page-id state)
           :board-id (or (:board-id (first boards)) (dwa/active-board-id state))
           :job-id (job/new-job-id)
           :format format
           :quality (or quality :high)
           :fps (or fps 30)
           :scale (or (:scale (first boards)) scale 1)
           :loop? (not (false? loop?))
           :audio? (not (false? audio?))
           :wasm? (features/active-feature? state "render-wasm/v1")}
    (> (count boards) 1) (assoc :boards (vec boards))))

(defn open-job-tab!
  "Open the tab of `job` (see `export-job`) with `noopener`, so the
  browser may give it a process of its own, in the click that asks for
  it (a tab opened later is blocked). `window.open` tells nothing then:
  the tab says it is there on the channel of the job."
  [state job]
  (when-let [uri (rt/resolve-uri (:router state) :export-animation (job/encode-params job))]
    (dom/open-new-window uri "_blank" "noopener")))

(defn- current-job-id
  [state]
  (get-in state [:export-animation :job :job-id]))

(defn- job-post!
  [job-id message]
  (when-let [post (get-in @jobs [job-id :channel :post])]
    (post message)))

(defn- job-ended
  "The job `job-id` is over: the tab hears `message` if any, the channel
  closes (see `start-export-job`) and the panel is free."
  ([job-id] (job-ended job-id nil))
  ([job-id message]
   (ptk/reify ::job-ended
     IDeref
     (-deref [_] job-id)

     ptk/UpdateEvent
     (update [_ state]
       (cond-> state
         (= job-id (current-job-id state))
         (dissoc :export-animation)))

     ptk/EffectEvent
     (effect [_ _ _]
       (when message
         (job-post! job-id message))))))

(defn- job-ended?
  "Whether `event` ends the job `job-id` (see `job-ended`)."
  [job-id event]
  (and (= ::job-ended (ptk/type event))
       (= job-id @event)))

(defn- job-saved
  "The workspace has saved what was pending before the job `job-id`
  (`outcome` `:saved`), or could not (`:save-failed`, which ends it): the
  tab hears it now, or at its next `:hello`."
  [job-id outcome]
  (ptk/reify ::job-saved
    ptk/WatchEvent
    (watch [_ state _]
      (if (and (= :save-failed outcome) (= job-id (current-job-id state)))
        (rx/of (ntf/error (tr "export-animation.error.save-failed"))
               (job-ended job-id {:type :save-failed}))
        (rx/empty)))

    ptk/EffectEvent
    (effect [_ _ _]
      (when (and (= :saved outcome) (contains? @jobs job-id))
        (swap! jobs assoc-in [job-id :save] outcome)
        (job-post! job-id {:type outcome})))))

(defn- job-heard
  "A message of the tab of the job `job-id`: it says it is there (the
  workspace answers once it has saved, and asks for the lock the tab
  holds, which it gets when the tab is gone), how far it is, that the
  browser froze it, or how it ended."
  [job-id {:keys [type] :as message}]
  (ptk/reify ::job-heard
    IDeref
    (-deref [_] (assoc message :job-id job-id))

    ptk/UpdateEvent
    (update [_ state]
      (if (= job-id (current-job-id state))
        (case type
          :hello    (update state :export-animation assoc :opened? true :offer? false)
          :progress (update state :export-animation merge
                            (select-keys message [:phase :current :total :percent]))
          :paused   (assoc-in state [:export-animation :paused?] true)
          :resumed  (assoc-in state [:export-animation :paused?] false)
          state)
        state))

    ptk/WatchEvent
    (watch [_ state _]
      (if (= job-id (current-job-id state))
        (case type
          :done      (rx/from (conj (vec (dea/notes-shown (:notes message))) (job-ended job-id)))
          :error     (rx/of (ntf/error (apply tr (job/error-text message)))
                            (job-ended job-id))
          :cancelled (rx/of (job-ended job-id))
          :closed    (rx/of (job-ended job-id))
          (rx/empty))
        (rx/empty)))

    ptk/EffectEvent
    (effect [_ _ _]
      (when (= :hello type)
        (when-let [{:keys [save locks lock-asked?]} (get @jobs job-id)]
          (when save
            (job-post! job-id {:type save}))
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
             (not (get-in state [:export-animation :opened?])))
        (assoc-in [:export-animation :offer?] true)))))

(defn- hello-wait
  "The offer of the panel when the tab of the job `job-id` says nothing
  within `wait-ms`."
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

(defn start-export-job
  "Export as `job` (see `export-job`) in the tab the click opened (see
  `open-job-tab!`): the workspace saves what is pending and tells the
  tab, then mirrors in the panel how far the tab is, until the export
  ends, is cancelled, or the tab goes (its lock is free). When the tab
  says nothing within `hello-wait-ms`, the panel offers to open it again
  (see `job-tab-reopened`) or to export here (see `export-job-here`).
  `channel-ctor`, `locks`, `hello-wait-ms` and `cancel-wait-ms` replace
  `BroadcastChannel`, `navigator.locks` and the waits of the same names
  (in tests)."
  [job & {:keys [channel-ctor locks] :as opts}]
  (ptk/reify ::start-export-job
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :export-animation {:in-progress true
                                      :in-tab? true
                                      :job job
                                      :format (:format job)
                                      :current 0
                                      :total 0
                                      :percent 0
                                      :cancelling? false
                                      :paused? false
                                      :opened? false
                                      :offer? false}))

    ptk/WatchEvent
    (watch [_ _ stream]
      (let [job-id (:job-id job)
            ended  (rx/filter #(job-ended? job-id %) stream)
            ;; the channel is open while the job lasts
            heard  (rx/create
                    (fn [subs]
                      (let [on-message #(rx/push! subs %)
                            channel    (job/open-channel job-id on-message
                                                         (or channel-ctor (job/default-channel-ctor)))]
                        (swap! jobs assoc job-id {:channel channel
                                                  :locks locks
                                                  :cancel-wait-ms (get opts :cancel-wait-ms cancel-wait-ms)})
                        (fn []
                          (when-let [close (:close channel)]
                            (close))
                          (swap! jobs dissoc job-id)))))]
        (rx/merge
         (->> heard
              (rx/map #(job-heard job-id %))
              (rx/take-until ended))
         (rx/of ::dps/force-persist)
         (->> (dps/wait-persisted-or-error)
              (rx/map (fn [_] (job-saved job-id :saved)))
              (rx/catch (fn [_] (rx/of (job-saved job-id :save-failed))))
              (rx/take-until ended))
         (hello-wait job-id (get opts :hello-wait-ms hello-wait-ms) stream))))))

(defn job-tab-reopened
  "The panel opened the tab of the job `job-id` again (see
  `open-job-tab!`): it waits for its word once more (`hello-wait-ms`,
  replaced in tests)."
  [job-id & {:as opts}]
  (ptk/reify ::job-tab-reopened
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= job-id (current-job-id state))
        (assoc-in [:export-animation :offer?] false)))

    ptk/WatchEvent
    (watch [_ _ stream]
      (hello-wait job-id (get opts :hello-wait-ms hello-wait-ms) stream))))

(defn export-job-here
  "Give up the tab of the job `job-id`, which did not open, and export in
  place (see `dea/export-animation`)."
  [job-id]
  (ptk/reify ::export-job-here
    ptk/WatchEvent
    (watch [_ state _]
      (let [job (get-in state [:export-animation :job])]
        (if (= job-id (:job-id job))
          (rx/of (job-ended job-id {:type :cancel})
                 (dea/export-animation (select-keys job [:format :quality :fps :scale :loop? :audio? :boards])))
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
  "Cancel the export: at its next frame in place, or in its tab, which
  says when it stopped (or the panel is free after `cancel-wait-ms`); a
  tab that has not said it is there yet is given up at once."
  []
  (ptk/reify ::cancel-export-animation
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (contains? state :export-animation)
        (assoc-in [:export-animation :cancelling?] true)))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [in-tab? opened? job]} (:export-animation state)
            job-id                        (:job-id job)]
        (cond
          (not in-tab?) (rx/empty)
          (not opened?) (rx/of (job-ended job-id {:type :cancel}))
          :else
          (let [ended (rx/filter #(job-ended? job-id %) stream)]
            (->> (rx/timer (get-in @jobs [job-id :cancel-wait-ms] cancel-wait-ms))
                 (rx/take-until ended)
                 (rx/map (fn [_] (job-ended job-id {:type :cancel}))))))))

    ptk/EffectEvent
    (effect [_ state _]
      (let [{:keys [in-tab? opened? job]} (:export-animation state)]
        (cond
          (not in-tab?) (dea/cancel-in-place!)
          opened?       (job-post! (:job-id job) {:type :cancel}))))))
