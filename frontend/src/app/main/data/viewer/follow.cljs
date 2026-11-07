;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.follow
  "The view mode live on a phone (`?follow=true`, the link of \"On your
  phone\" in the workspace): what is saved in the file shows as it comes
  (the websocket of the file, as the workspace gets it), and the board
  selected in a workspace of the same person shows. The view mode says
  so to the workspaces of that person every `hello-interval-ms` (a
  `:broadcast` of the websocket, to all their sessions); a workspace of
  the file that heard it lately tells the board of its selection as it
  changes (see `app.main.data.workspace.phone`)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.uuid :as uuid]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.preview :as dvp]
   [app.main.data.websocket :as dws]
   [app.main.router :as rt]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(def hello-interval-ms
  "How often the view mode tells it follows."
  30000)

(def followed-ms
  "How long a workspace tells its board after it heard the view mode:
  more than two of its hellos, one may get lost."
  75000)

(defn follow?
  "Whether the view mode of the route `params` follows the workspace."
  [params]
  (= "true" (rt/get-query-param params :follow)))

(defn hello-message
  "What the view mode tells the workspaces of the file `file-id`."
  [file-id]
  {:type :broadcast
   :event :viewer-follow
   :file-id file-id})

(defn board-message
  "What a workspace of the file `file-id` tells the view modes that
  follow it: the board `frame-id` of the page `page-id` is selected."
  [file-id page-id frame-id]
  {:type :broadcast
   :event :workspace-board
   :file-id file-id
   :page-id page-id
   :frame-id frame-id})

(defn board-step
  "Where the view mode in `state` goes for the board `frame-id` of the
  page `page-id`: `[:frame frame-id]` on the page it shows, `[:page
  page-id index]` on another one, nil when it shows it already or does
  not list it (hidden in view mode, or not saved yet: it is then kept,
  see `pending`)."
  [state page-id frame-id]
  (let [params  (rt/get-params state)
        current (some-> (rt/get-query-param params :page-id) uuid/parse*)
        index   (some-> (rt/get-query-param params :index) parse-long)
        frames  (dm/get-in state [:viewer :pages page-id :frames])
        target  (d/index-of-pred frames #(= frame-id (:id %)))]
    (cond
      (nil? target)        nil
      (not= page-id current) [:page page-id target]
      (not= target index)  [:frame frame-id]
      :else                nil)))

(defn go-to-board
  "The view mode at the board `frame-id` of the page `page-id`, the one
  selected in the workspace; when it does not list it yet, it goes there
  once the file brings it (see `go-to-pending`)."
  [page-id frame-id]
  (ptk/reify ::go-to-board
    ptk/UpdateEvent
    (update [_ state]
      (let [listed? (some? (d/index-of-pred (dm/get-in state [:viewer :pages page-id :frames])
                                            #(= frame-id (:id %))))]
        (if listed?
          (update state :viewer-local dissoc ::pending)
          (assoc-in state [:viewer-local ::pending] [page-id frame-id]))))

    ptk/WatchEvent
    (watch [_ state _]
      (let [[kind id index] (board-step state page-id frame-id)]
        (case kind
          :frame (rx/of (dv/go-to-frame id))
          :page  (rx/of (rt/nav :viewer (-> (rt/get-params state)
                                            (assoc :page-id id :index index)
                                            (dissoc :frame-id))))
          (rx/empty))))))

(defn- pending
  "The board the view mode waits for, `[page-id frame-id]`."
  [state]
  (dm/get-in state [:viewer-local ::pending]))

(def ^:private go-to-pending
  "The view mode at the board it waits for, once the file brings it."
  (ptk/reify ::go-to-pending
    ptk/WatchEvent
    (watch [_ state _]
      (if-let [[page-id frame-id] (pending state)]
        (rx/of (go-to-board page-id frame-id))
        (rx/empty)))))

(defn start
  "The view mode of the file `file-id` following the workspace: it
  listens to the file, says it follows, and goes where it is told, until
  `stop`."
  [file-id]
  (ptk/reify ::start
    ptk/WatchEvent
    (watch [_ state stream]
      ;; not when the view mode goes to another page: it ends and starts
      ;; again then (see `viewer*`)
      (let [stopper    (rx/filter (ptk/type? ::stop) stream)
            session-id (:session-id state)
            subscribe  {:type :subscribe-file :file-id file-id}
            stale      (rx/subject)
            on-stale   #(rx/push! stale true)
            messages   (->> stream
                            (rx/filter (ptk/type? ::dws/message))
                            (rx/map deref)
                            (rx/filter #(= file-id (:file-id %))))]
        (rx/concat
         (->> (rx/merge
               ;; the file and the hello, now and as the websocket opens
               ;; again
               (rx/of (dws/send subscribe) (dws/send (hello-message file-id)))
               (->> stream
                    (rx/filter (ptk/type? ::dws/opened))
                    (rx/mapcat #(rx/of (dws/send subscribe) (dws/send (hello-message file-id)))))
               (->> (rx/interval hello-interval-ms)
                    (rx/map #(dws/send (hello-message file-id))))

               ;; what is saved, shown as it comes; when it does not apply
               ;; (some got lost), the file fetched again
               (->> messages
                    (rx/filter #(= :file-change (:type %)))
                    (rx/mapcat #(rx/of (dvp/apply-preview-changes (vec (:changes %)) on-stale)
                                       go-to-pending)))
               (->> stale
                    (rx/debounce 500)
                    (rx/map #(dvp/refresh-bundle (constantly nil) (constantly nil))))
               (->> stream
                    (rx/filter (ptk/type? ::dvp/bundle-refreshed))
                    (rx/map (constantly go-to-pending)))

               (->> messages
                    (rx/filter #(and (= :broadcast (:type %))
                                     (= :workspace-board (:event %))
                                     (not= session-id (:session-id %))))
                    (rx/map #(go-to-board (:page-id %) (:frame-id %)))))
              (rx/take-until stopper))
         (rx/of (dws/send {:type :unsubscribe-file :file-id file-id})))))))

(def stop
  "The view mode stops following the workspace."
  (ptk/data-event ::stop {}))
