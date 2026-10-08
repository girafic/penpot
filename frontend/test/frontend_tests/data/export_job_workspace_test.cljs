;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.export-job-workspace-test
  "The workspace's side of an animated export run in a tab of its own
  (see `deaw/start-export-job`), against a stand-in of the tab."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-job :as job]
   [app.main.data.exports.animation-workspace :as deaw]
   [app.main.data.persistence :as dps]
   [app.main.store :as st]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.export-job :as thj]
   [frontend-tests.helpers.mock :as mock]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(defn- new-job
  []
  {:file-id (uuid/next)
   :page-id (uuid/next)
   :board-id (uuid/next)
   :job-id (job/new-job-id)
   :format :mp4
   :quality :high
   :fps 30
   :scale 1
   :loop? true
   :audio? true
   :wasm? true})

(defn- fake-tab
  "The tab of `job` on `ctor` and `locks`: it holds the lock of the job
  until `:release` resolves, and keeps what it hears in `:heard`."
  [job ctor locks]
  (let [heard   (atom [])
        release (p/deferred)
        channel (job/open-channel (:job-id job) #(swap! heard conj %) ctor)]
    {:heard heard
     :post (:post channel)
     :close (:close channel)
     :release release
     :held (job/hold-lock (:job-id job) release locks)}))

(defn- scene
  "A job started in the workspace, with its tab when `tab?`."
  [& {:keys [tab? hello-wait-ms cancel-wait-ms]
      :or {tab? true hello-wait-ms 50 cancel-wait-ms 10000}}]
  (let [job   (new-job)
        ctor  (thj/broadcast-channels)
        locks (thj/locks)
        tab   (when tab? (fake-tab job ctor locks))]
    (st/emit! (deaw/start-export-job job :channel-ctor ctor :locks locks
                                     :hello-wait-ms hello-wait-ms
                                     :cancel-wait-ms cancel-wait-ms))
    {:job job :ctor ctor :locks locks :tab tab}))

(defn- export-state
  []
  (:export-animation @st/state))

(defn- types
  [tab]
  (mapv :type @(:heard tab)))

(defn- hello!
  "The tab says it is there, once it holds its lock."
  [{:keys [tab]}]
  (.then (:held tab) (fn [_] ((:post tab) {:type :hello}))))

(defn- fail!
  [cause]
  (t/is false (str "unexpected: " (ex-message cause))))

(defn- clean!
  "Close the tab, free the panel and the store, then `done`."
  [{:keys [tab]} done]
  (when tab
    ((:close tab))
    (p/resolve (:release tab) nil))
  (st/emit! (deaw/cancel-export-animation))
  (swap! st/state dissoc :export-animation :persistence :notification)
  (done)
  nil)

(t/deftest the-tab-hears-that-the-workspace-saved
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene)]
      (t/is (= {:in-progress true :in-tab? true :opened? false :offer? false :percent 0}
               (select-keys (export-state) [:in-progress :in-tab? :opened? :offer? :percent])))
      (-> (hello! scene)
          (.then (fn [_] (thj/wait-until #(some #{:saved} (types tab)))))
          (.then (fn [_]
                   (t/is (:opened? (export-state)))
                   (t/is (not (:offer? (export-state))) "no offer once it said hello")))
          (.catch fail!)
          (.finally #(clean! scene done))))))

(t/deftest a-failed-save-ends-the-job
  (t/async
    done
    (mock/with-mocks
      ;; as when the persistence queue fails
      {dps/wait-persisted-or-error
       (mock/stub (fn [& _] (rx/throw (ex-info "Changes could not be saved"
                                               {:type :persistence :code :save-failed}))))}
      (fn [done']
        (let [{:keys [tab] :as scene} (scene)]
          (-> (thj/wait-until #(seq @(:heard tab)))
              (.then (fn [_]
                       (t/is (= [:save-failed] (types tab)))
                       (t/is (nil? (export-state)) "the panel is free")
                       (t/is (= :error (get-in @st/state [:notification :level])))))
              (.catch fail!)
              (.finally #(clean! scene done')))))
      done)))

(t/deftest the-panel-mirrors-the-tab
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene :hello-wait-ms 20)
          post (:post tab)]
      (-> (hello! scene)
          (.then (fn [_]
                   (post {:type :progress :phase :capture :current 30 :total 100 :percent 24})
                   (thj/wait-until #(= 24 (:percent (export-state))))))
          (.then (fn [_]
                   (t/is (= {:current 30 :total 100 :percent 24}
                            (select-keys (export-state) [:current :total :percent])))
                   (post {:type :paused})
                   (thj/wait-until #(:paused? (export-state)))))
          (.then (fn [_]
                   (post {:type :resumed})
                   (thj/wait-until #(false? (:paused? (export-state))))))
          (.then (fn [_]
                   ;; a long phase without a word (encoding) keeps the panel busy
                   (js/Promise. (fn [resolve] (js/setTimeout resolve 100)))))
          (.then (fn [_]
                   (t/is (:in-progress (export-state)))
                   (t/is (not (:offer? (export-state))))
                   (st/emit! (deaw/cancel-export-animation))
                   (thj/wait-until #(some #{:cancel} (types tab)))))
          (.then (fn [_]
                   (t/is (:cancelling? (export-state)) "Cancel reaches the tab")
                   (post {:type :cancelled})
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_] (t/is (nil? (export-state)) "free once the tab stopped")))
          (.catch fail!)
          (.finally #(clean! scene done))))))

(t/deftest an-unanswered-cancel-frees-the-panel
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene :cancel-wait-ms 40)]
      (-> (hello! scene)
          (.then (fn [_] (thj/wait-until #(:opened? (export-state)))))
          (.then (fn [_]
                   (st/emit! (deaw/cancel-export-animation))
                   (t/is (:cancelling? (export-state)))
                   ;; the tab hangs: it never says it stopped
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_] (t/is (some #{:cancel} (types tab)))))
          (.catch fail!)
          (.finally #(clean! scene done))))))

(t/deftest the-tab-done-frees-the-panel-with-its-notes
  (t/async
    done
    (swap! st/state dissoc :persistence :notification)
    (let [{:keys [tab] :as scene} (scene)
          post (:post tab)]
      (-> (hello! scene)
          (.then (fn [_]
                   (post {:type :done :name "Hero" :ext ".mp4" :size 10 :notes [[:audio-dropped]]})
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_]
                   (t/is (= :warning (get-in @st/state [:notification :level])) "the notes")))
          (.catch fail!)
          (.finally #(clean! scene done))))))

(t/deftest the-tab-gone-frees-the-panel
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene)]
      (-> (hello! scene)
          (.then (fn [_] (thj/wait-until #(:opened? (export-state)))))
          (.then (fn [_]
                   ;; the tab crashes: its lock goes, without a word
                   ((:close tab))
                   (p/resolve (:release tab) nil)
                   (thj/wait-until #(nil? (export-state)))))
          (.then (fn [_] (t/is (nil? (export-state)) "the lock freed it")))
          (.catch fail!)
          (.finally #(clean! scene done))))))

(t/deftest a-tab-that-says-nothing-is-offered-again-or-exported-here
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [job] :as scene} (scene :tab? false :hello-wait-ms 20)
          started (atom [])
          sub     (rx/sub! (->> st/stream
                                (rx/filter #(= ::dea/export-animation (ptk/type %))))
                           #(swap! started conj %))]
      (-> (thj/wait-until #(:offer? (export-state)))
          (.then (fn [_]
                   (st/emit! (deaw/job-tab-reopened (:job-id job) :hello-wait-ms 20))
                   (t/is (not (:offer? (export-state))) "waits again")
                   (thj/wait-until #(:offer? (export-state)))))
          (.then (fn [_]
                   (st/emit! (deaw/export-job-here (:job-id job)))
                   (thj/wait-until #(seq @started))))
          (.then (fn [_] (t/is (= 1 (count @started)) "exported in place")))
          (.catch fail!)
          (.finally (fn []
                      (rx/dispose! sub)
                      (clean! scene done)))))))

(t/deftest cancel-before-the-tab-said-hello-gives-it-up
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene :hello-wait-ms 1000)]
      (st/emit! (deaw/cancel-export-animation))
      (t/is (nil? (export-state)) "free at once")
      (-> (thj/wait-until #(some #{:cancel} (types tab)))
          (.catch fail!)
          (.finally #(clean! scene done))))))

(t/deftest cancel-stops-an-export-in-place-there
  (t/async
    done
    (let [stops (atom 0)]
      (mock/with-mocks
        {dea/cancel-in-place! (mock/stub #(swap! stops inc))}
        (fn [done']
          ;; an export in place (a version preview, "Export here")
          (swap! st/state assoc :export-animation {:in-progress true :format :mp4})
          (st/emit! (deaw/cancel-export-animation))
          (t/is (= 1 @stops) "the export in place stops")
          (t/is (:cancelling? (export-state)))
          (swap! st/state dissoc :export-animation)
          ;; a job in a tab is cancelled by its tab, not in place
          (let [{:keys [tab] :as scene} (scene)]
            (-> (hello! scene)
                (.then (fn [_] (thj/wait-until #(:opened? (export-state)))))
                (.then (fn [_]
                         (st/emit! (deaw/cancel-export-animation))
                         (thj/wait-until #(some #{:cancel} (types tab)))))
                (.then (fn [_] (t/is (= 1 @stops) "nothing stops in place")))
                (.catch fail!)
                (.finally #(clean! scene done')))))
        done))))

(t/deftest a-preview-or-a-browser-without-what-tabs-need-exports-in-place
  (let [globals #js {:BroadcastChannel (fn [])
                     :navigator #js {:locks #js {}}}]
    (t/is (deaw/export-in-tab? {} :globals globals) "in a tab, where the tab can talk and be known gone")
    (t/is (not (deaw/export-in-tab? {:workspace-global {:preview-id (uuid/next)}} :globals globals))
          "a version preview is not saved")
    (t/is (not (deaw/export-in-tab? {} :globals #js {:BroadcastChannel (fn []) :navigator #js {}}))
          "no Web Locks (no secure context)")
    (t/is (not (deaw/export-in-tab? {} :globals #js {:navigator #js {:locks #js {}}}))
          "no BroadcastChannel")))

(t/deftest the-job-of-the-active-board
  (let [file-id (uuid/next)
        page-id (uuid/next)
        job     (deaw/export-job {:current-file-id file-id
                                  :current-page-id page-id
                                  :features #{"render-wasm/v1"}}
                                 {:format :gif :quality :low :fps 12 :scale 2 :loop? false})]
    (t/is (= {:file-id file-id :page-id page-id :format :gif :quality :low :fps 12
              :scale 2 :loop? false :audio? true :wasm? true}
             (dissoc job :board-id :job-id)))
    (t/is (uuid? (:job-id job)))))

(t/deftest the-job-of-several-boards
  (let [[a b] [(uuid/next) (uuid/next)]
        boards [{:board-id a :scale 2} {:board-id b :scale 0.5}]
        job    (deaw/export-job {:current-file-id (uuid/next) :current-page-id (uuid/next)}
                                {:format :mp4 :boards boards})]
    (t/is (= [a 2 boards] ((juxt :board-id :scale :boards) job)) "the first is its board")
    (t/is (nil? (:boards (deaw/export-job {} {:format :mp4 :boards (take 1 boards)})))
          "one board is no list")))
