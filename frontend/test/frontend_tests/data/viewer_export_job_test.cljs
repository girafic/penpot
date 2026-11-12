;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-export-job-test
  "The animated export of a board from the view mode, in the tab the
  workspace exports in, against a stand-in of the tab: the view mode
  mirrors how far it is until it ends, is cancelled or goes, and is never
  left waiting for it."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.viewer.export-job :as vej]
   [app.main.store :as st]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]
   [frontend-tests.helpers.export-job :as thj]
   [promesa.core :as p]))

(defn- new-job
  []
  (vej/board-job {:file-id (uuid/next) :page-id (uuid/next) :board-id (uuid/next) :wasm? true}
                 {:format :mp4 :quality :high :fps 30 :scale 1}))

(defn- scene
  "A job started in the view mode, with its tab when `tab?`."
  [& {:keys [tab? hello-wait-ms cancel-wait-ms]
      :or {tab? true hello-wait-ms 1000 cancel-wait-ms 10000}}]
  (let [job   (new-job)
        ctor  (thj/broadcast-channels)
        locks (thj/locks)
        tab   (when tab? (thj/fake-tab job ctor locks))]
    (st/emit! (vej/start-job job :channel-ctor ctor :locks locks
                             :hello-wait-ms hello-wait-ms
                             :cancel-wait-ms cancel-wait-ms))
    {:job job :tab tab}))

(defn- export-state
  []
  (:viewer-export-animation @st/state))

(defn- types
  [tab]
  (mapv :type @(:heard tab)))

(defn- hello
  "The tab says it is there, once it holds its lock."
  [{:keys [tab]}]
  (.then (:held tab) (fn [_] ((:post tab) {:type :hello}))))

(defn- fail
  [cause]
  (t/is false (str "unexpected: " (ex-message cause))))

(defn- clean
  "Close the tab, free the panel and the store, then `done`."
  [{:keys [tab]} done]
  (when tab
    ((:close tab))
    (p/resolve (:release tab) nil))
  (st/emit! (vej/cancel-job))
  (swap! st/state dissoc :viewer-export-animation :notification)
  (done)
  nil)

(t/deftest the-job-of-a-board-shown
  (let [[file page board] [(uuid/next) (uuid/next) (uuid/next)]
        job (vej/board-job {:file-id file :page-id page :board-id board :wasm? true}
                           {:format :mp4 :quality :low :fps 24 :scale 2
                            :loop? true :audio? false :ids? true :bbox? false})]
    (t/is (= {:file-id file :page-id page :board-id board :format :mp4 :quality :low :fps 24
              :scale 2 :loop? true :audio? false :wasm? true :ids? true :saved? true}
             (dissoc job :job-id))
          "its settings, the SVG options set only, nothing to save")
    (t/is (uuid? (:job-id job)))
    (t/is (= [{:board-id board :scale 1} {:board-id file :scale 2}]
             (:boards (vej/board-job {:file-id file :page-id page :board-id board :wasm? true}
                                     {:format :gif
                                      :boards [{:board-id board :scale 1}
                                               {:board-id file :scale 2}]})))
          "several boards into a ZIP")))

(t/deftest the-tab-opens-in-the-click
  (let [opened (atom nil)
        job    (new-job)]
    (vej/open-job-tab #{:export-animation} job :open (fn [& args] (reset! opened args)))
    (let [[uri target features] @opened]
      (t/is (str/includes? uri (str (:job-id job))) "the tab of the job")
      (t/is (str/includes? uri "saved=true") "which loads at once")
      (t/is (= ["_blank" "noopener"] [target features])))))

(t/deftest the-panel-mirrors-the-tab
  (t/async
    done
    (let [{:keys [tab] :as scene} (scene)
          post (:post tab)]
      (t/is (= {:in-progress true :in-tab? true :opened? false :offer? false :percent 0}
               (select-keys (export-state) [:in-progress :in-tab? :opened? :offer? :percent])))
      (-> (hello scene)
          (.then (fn [_] (thj/wait-until #(:opened? (export-state)))))
          (.then (fn [_]
                   (post {:type :progress :phase :capture :current 4 :total 10 :percent 40})
                   (thj/wait-until #(= 40 (:percent (export-state))))))
          (.then (fn [_]
                   (post {:type :paused})
                   (thj/wait-until #(:paused? (export-state)))))
          (.then (fn [_]
                   (post {:type :done :name "Hero" :ext ".mp4" :size 10 :notes []})
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_]
                   (t/is (nil? (export-state)) "free once done")
                   (t/is (not (some #{:saved} (types tab))) "nothing to answer")))
          (.catch fail)
          (.finally #(clean scene done))))))

(t/deftest an-error-of-the-tab-is-told-once
  (t/async
    done
    (swap! st/state dissoc :notification)
    (let [{:keys [tab] :as scene} (scene)]
      (-> (hello scene)
          (.then (fn [_]
                   ((:post tab) {:type :error :reason :load})
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_]
                   (t/is (= :error (get-in @st/state [:notification :level])))))
          (.catch fail)
          (.finally #(clean scene done))))))

(t/deftest cancel-before-the-tab-said-hello-gives-it-up
  (t/async
    done
    (let [{:keys [tab] :as scene} (scene)]
      (st/emit! (vej/cancel-job))
      (t/is (nil? (export-state)) "free at once")
      (-> (thj/wait-until #(some #{:cancel} (types tab)))
          (.catch fail)
          (.finally #(clean scene done))))))

(t/deftest cancel-waits-for-the-tab-to-stop
  (t/async
    done
    (let [{:keys [tab] :as scene} (scene)]
      (-> (hello scene)
          (.then (fn [_] (thj/wait-until #(:opened? (export-state)))))
          (.then (fn [_]
                   (st/emit! (vej/cancel-job))
                   (t/is (:cancelling? (export-state)))
                   (thj/wait-until #(some #{:cancel} (types tab)))))
          (.then (fn [_]
                   (t/is (some? (export-state)) "until the tab says it stopped")
                   ((:post tab) {:type :cancelled})
                   (thj/wait-until #(nil? (export-state)))))
          (.catch fail)
          (.finally #(clean scene done))))))

(t/deftest an-unanswered-cancel-frees-the-panel
  (t/async
    done
    (let [scene (scene :cancel-wait-ms 40)]
      (-> (hello scene)
          (.then (fn [_] (thj/wait-until #(:opened? (export-state)))))
          (.then (fn [_]
                   (st/emit! (vej/cancel-job))
                   ;; the tab hangs: it never says it stopped
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_] (t/is (nil? (export-state)))))
          (.catch fail)
          (.finally #(clean scene done))))))

(t/deftest the-tab-gone-frees-the-panel
  (t/async
    done
    (let [{:keys [tab] :as scene} (scene)]
      (-> (hello scene)
          (.then (fn [_] (thj/wait-until #(:opened? (export-state)))))
          (.then (fn [_]
                   ;; the tab crashes: its lock goes, without a word
                   ((:close tab))
                   (p/resolve (:release tab) nil)
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_] (t/is (nil? (export-state)) "the lock freed it")))
          (.catch fail)
          (.finally #(clean scene done))))))

(t/deftest a-tab-that-says-nothing-is-offered-again
  (t/async
    done
    (let [{:keys [job] :as scene} (scene :tab? false :hello-wait-ms 20)]
      (-> (thj/wait-until #(:offer? (export-state)))
          (.then (fn [_]
                   (st/emit! (vej/job-tab-reopened (:job-id job) :hello-wait-ms 20))
                   (t/is (not (:offer? (export-state))) "waits again")
                   (thj/wait-until #(:offer? (export-state)))))
          (.catch fail)
          (.finally #(clean scene done))))))

(t/deftest without-a-channel-the-tab-exports-alone
  (st/emit! (vej/start-job (new-job) :channel-ctor nil))
  (t/is (nil? (export-state)) "the panel is free at once")
  (swap! st/state dissoc :viewer-export-animation))
