;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.export-job-workspace-test
  "The workspace's side of an animated export run in a tab of its own
  (see `dea/start-export-job`), against a stand-in of the tab."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-job :as job]
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
  [& {:keys [tab? wait-ms] :or {tab? true wait-ms 50}}]
  (let [job   (new-job)
        ctor  (thj/broadcast-channels)
        locks (thj/locks)
        tab   (when tab? (fake-tab job ctor locks))]
    (st/emit! (dea/start-export-job job :channel-ctor ctor :locks locks :wait-ms wait-ms))
    {:job job :ctor ctor :locks locks :tab tab}))

(defn- after
  [ms f]
  (js/setTimeout f ms))

(defn- export-state
  []
  (:export-animation @st/state))

(defn- types
  [tab]
  (mapv :type @(:heard tab)))

(defn- clean!
  [{:keys [tab]}]
  (when tab
    ((:close tab))
    (p/resolve (:release tab) nil))
  (st/emit! (dea/cancel-export-animation))
  (swap! st/state dissoc :export-animation :persistence :notification))

(t/deftest the-tab-hears-that-the-workspace-saved
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene)]
      (t/is (= {:in-progress true :in-tab? true :opened? false :offer? false :percent 0}
               (select-keys (export-state) [:in-progress :in-tab? :opened? :offer? :percent])))
      (-> (:held tab)
          (p/then #((:post tab) {:type :hello})))
      (after 60
             (fn []
               (t/is (some #{:saved} (types tab)) "answered once saved")
               (t/is (:opened? (export-state)))
               (t/is (not (:offer? (export-state))) "no offer once it said hello")
               (clean! scene)
               (done))))))

(t/deftest a-failed-save-ends-the-job
  (t/async
    done
    (mock/with-mocks
      ;; as when the persistence queue fails
      {dps/wait-persisted-or-error
       (fn
         ([] (rx/throw (ex-info "Changes could not be saved" {:type :persistence :code :save-failed})))
         ([_] (rx/throw (ex-info "Changes could not be saved" {:type :persistence :code :save-failed}))))}
      (fn [done']
        (let [{:keys [tab] :as scene} (scene)]
          (after 30
                 (fn []
                   (t/is (= [:save-failed] (types tab)))
                   (t/is (nil? (export-state)) "the panel is free")
                   (t/is (= :error (get-in @st/state [:notification :level])))
                   (clean! scene)
                   (done')))))
      done)))

(t/deftest the-panel-mirrors-the-tab
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene :wait-ms 20)
          post (:post tab)]
      (-> (:held tab)
          (p/then (fn []
                    (post {:type :hello})
                    (post {:type :progress :phase :capture :current 30 :total 100 :percent 24}))))
      (after 40
             (fn []
               (t/is (= {:current 30 :total 100 :percent 24}
                        (select-keys (export-state) [:current :total :percent])))
               (post {:type :paused})
               (after 20
                      (fn []
                        (t/is (:paused? (export-state)))
                        (post {:type :resumed})
                        ;; a long phase without a word (encoding) keeps the panel busy
                        (after 150
                               (fn []
                                 (t/is (false? (:paused? (export-state))))
                                 (t/is (:in-progress (export-state)))
                                 (t/is (not (:offer? (export-state))))
                                 (st/emit! (dea/cancel-export-animation))
                                 (after 20
                                        (fn []
                                          (t/is (some #{:cancel} (types tab)) "Cancel reaches the tab")
                                          (t/is (:cancelling? (export-state)))
                                          (post {:type :cancelled})
                                          (after 20
                                                 (fn []
                                                   (t/is (nil? (export-state)) "free once the tab stopped")
                                                   (clean! scene)
                                                   (done))))))))))))))

(t/deftest the-tab-done-frees-the-panel-with-its-notes
  (t/async
    done
    (swap! st/state dissoc :persistence :notification)
    (let [{:keys [tab] :as scene} (scene)
          post (:post tab)]
      (-> (:held tab)
          (p/then (fn []
                    (post {:type :hello})
                    (post {:type :done :name "Hero" :ext ".mp4" :size 10 :notes [[:audio-dropped]]}))))
      (after 40
             (fn []
               (t/is (nil? (export-state)))
               (t/is (= :warning (get-in @st/state [:notification :level])) "the notes")
               (clean! scene)
               (done))))))

(t/deftest the-tab-gone-frees-the-panel
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene)]
      (-> (:held tab)
          (p/then #((:post tab) {:type :hello})))
      (after 30
             (fn []
               (t/is (:in-progress (export-state)))
               ;; the tab crashes: its lock goes, without a word
               ((:close tab))
               (p/resolve (:release tab) nil)
               (after 30
                      (fn []
                        (t/is (nil? (export-state)) "the lock freed it")
                        (clean! scene)
                        (done))))))))

(t/deftest a-tab-that-says-nothing-is-offered-again-or-exported-here
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [job] :as scene} (scene :tab? false :wait-ms 20)
          started (atom [])
          sub     (rx/sub! (->> st/stream
                                (rx/filter #(= ::dea/export-animation (ptk/type %))))
                           #(swap! started conj %))]
      (after 60
             (fn []
               (t/is (:offer? (export-state)) "no hello: the offer")
               (st/emit! (dea/job-tab-reopened (:job-id job) :wait-ms 20))
               (t/is (not (:offer? (export-state))) "waits again")
               (after 60
                      (fn []
                        (t/is (:offer? (export-state)))
                        (st/emit! (dea/export-job-here (:job-id job)))
                        (after 20
                               (fn []
                                 (t/is (= 1 (count @started)) "exported in place")
                                 (rx/dispose! sub)
                                 (clean! scene)
                                 (done))))))))))

(t/deftest cancel-before-the-tab-said-hello-gives-it-up
  (t/async
    done
    (swap! st/state dissoc :persistence)
    (let [{:keys [tab] :as scene} (scene :wait-ms 1000)]
      (st/emit! (dea/cancel-export-animation))
      (t/is (nil? (export-state)) "free at once")
      (after 30
             (fn []
               (t/is (some #{:cancel} (types tab)))
               (clean! scene)
               (done))))))

(t/deftest a-preview-exports-in-place
  (t/is (= (some? (unchecked-get js/globalThis "BroadcastChannel"))
           (dea/export-in-tab? {}))
        "in a tab, where the tab can talk to the workspace")
  (t/is (not (dea/export-in-tab? {:workspace-global {:preview-id (uuid/next)}}))))

(t/deftest the-job-of-the-active-board
  (let [file-id (uuid/next)
        page-id (uuid/next)
        job     (dea/export-job {:current-file-id file-id
                                 :current-page-id page-id
                                 :features #{"render-wasm/v1"}}
                                {:format :gif :quality :low :fps 12 :scale 2 :loop? false})]
    (t/is (= {:file-id file-id :page-id page-id :format :gif :quality :low :fps 12
              :scale 2 :loop? false :audio? true :wasm? true}
             (dissoc job :board-id :job-id)))
    (t/is (uuid? (:job-id job)))))
