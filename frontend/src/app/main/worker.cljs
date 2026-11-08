;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.worker
  "Interface to communicate with the web worker"
  (:require
   [app.config :as cf]
   [app.util.worker :as uw]
   [beicon.v2.core :as rx]))

;; Injected from `app.main.errors` to remove circular dependency
(defonce on-error nil)

(defonce instance nil)

(defn init!
  []
  (let [worker (uw/init cf/worker-uri on-error)]
    (uw/ask! worker {:cmd :configure
                     :config {:public-uri cf/public-uri
                              :build-data cf/build-date
                              :version cf/version
                              :version-tag cf/version-tag}})
    (set! instance worker)
    worker))

(defn- ensure-instance!
  "The worker, started on its first use: pages that never ask it (the
  login, the view mode, the settings) never load its code, which costs
  a phone more than 100 MB."
  []
  (or instance
      (when (exists? js/Worker)
        (init!))))

(defn ask!
  ([message]
   (if-let [worker (ensure-instance!)]
     (uw/ask! worker message)
     (rx/empty)))
  ([message transfer]
   (if-let [worker (ensure-instance!)]
     (uw/ask! worker message transfer)
     (rx/empty))))

(defn emit!
  ([message]
   (if-let [worker (ensure-instance!)]
     (uw/emit! worker message)
     (rx/empty)))
  ([message transfer]
   (if-let [worker (ensure-instance!)]
     (uw/emit! worker message transfer)
     (rx/empty))))

(defn ask-buffered!
  ([message]
   (if-let [worker (ensure-instance!)]
     (uw/ask-buffered! worker message)
     (rx/empty)))
  ([message transfer]
   (if-let [worker (ensure-instance!)]
     (uw/ask-buffered! worker message transfer)
     (rx/empty))))

(defn ask-many!
  ([message]
   (if-let [worker (ensure-instance!)]
     (uw/ask-many! worker message)
     (rx/empty)))
  ([message transfer]
   (if-let [worker (ensure-instance!)]
     (uw/ask-many! worker message transfer)
     (rx/empty))))
