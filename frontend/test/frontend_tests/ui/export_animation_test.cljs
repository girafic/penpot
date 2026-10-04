;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.export-animation-test
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-job :as job]
   [app.main.data.exports.animation-tab :as dat]
   [app.main.data.viewer :as dv]
   [app.main.store :as st]
   [app.main.ui.export-animation :as uea]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.export-job :as thj]
   [frontend-tests.helpers.mock :as mock]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(t/deftest the-page-shows-where-the-export-is
  (let [board  (uuid/next)
        tab    {:params {:board-id board :format :mp4}
                :page {:objects {board {:name "Hero"}}}}
        view   #(uea/page-state (merge tab %))]
    (t/is (= {:text ["export-animation.waiting"] :actions []} (view {:status :waiting})))
    (t/is (= {:text ["export-animation.loading"] :actions []} (view {:status :loading})))
    (t/is (= {:text ["export-animation.exporting" "Hero" "MP4"] :percent 45 :actions [:cancel]}
             (view {:status :exporting :percent 45})))
    (t/is (= [] (:actions (view {:status :exporting :cancelling? true}))) "no second Cancel")
    (t/is (= {:text ["export-animation.done" "Hero.mp4" "1.5 MB"]
              :notes [[:audio-dropped]]
              :actions [:download :close]}
             (view {:status :done :result {:name "Hero" :ext ".mp4" :size (* 1.5 1024 1024)
                                           :notes [[:audio-dropped]]}})))
    (t/is (= [:close] (:actions (view {:status :cancelled}))))
    (t/is (= ["export-animation.stale"] (:text (view {:status :stale}))))
    (t/is (= ["export-animation.error.save-failed"] (:text (view {:status :error :error {:reason :save-failed}}))))
    (t/is (= ["export-animation.error.export" "boom"] (:text (view {:status :error :error {:reason :export :message "boom"}}))))
    (t/is (= ["export-animation.invalid"] (:text (uea/page-state {:status :invalid}))))))

(defn- job-scene
  "The params of a job (a Lottie, which draws without WASM), the bundle of
  its file and the workspace's side of the channel, answering each
  `:hello` with `answer`."
  [answer]
  (let [file-id  (uuid/next)
        page-id  (uuid/next)
        board-id (uuid/next)
        params   {:file-id file-id :page-id page-id :board-id board-id :job-id (job/new-job-id)
                  :format :lottie :quality :high :fps 30 :scale 1 :loop? true :audio? false :wasm? false}
        ctor     (thj/broadcast-channels)
        heard    (atom [])
        ws       (atom nil)]
    (reset! ws (job/open-channel (:job-id params)
                                 (fn [message]
                                   (swap! heard conj message)
                                   (when (and answer (= :hello (:type message)))
                                     ((:post @ws) {:type answer})))
                                 ctor))
    {:params params
     :ctor ctor
     :locks (thj/locks)
     :heard heard
     :ws ws
     :bundle {:file {:id file-id
                     :data {:pages-index {page-id {:id page-id
                                                   :objects {board-id {:id board-id :name "Hero"}}}}}}
              :libraries []
              :fonts []
              :team {:features #{}}}}))

(defn- start!
  [{:keys [params ctor locks]}]
  (st/emit! (dat/initialize (job/encode-params params) :channel-ctor ctor :locks locks)))

(defn- after
  [ms f]
  (js/setTimeout f ms))

(defn- end!
  [{:keys [ws]} done']
  (st/emit! (dat/finalize))
  ((:close @ws))
  (done'))

(t/deftest the-tab-exports-once-the-workspace-has-saved
  (t/async
    done
    (let [{:keys [bundle heard] :as scene} (job-scene :saved)
          loaded    (atom 0)
          downloads (atom 0)
          viewer    (atom 0)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (swap! loaded inc) (rx/of bundle))
         dv/update-page-position-data (fn [& _] (swap! viewer inc) (ptk/data-event ::noop))
         dv/go-to-frame-auto (fn [& _] (swap! viewer inc) (ptk/data-event ::noop))
         dea/run-export (fn [ctx params]
                          (t/is (= "Hero" (get-in ctx [:page :objects (:board-id ctx) :name])) "the page of the file")
                          (t/is (= :lottie (:format params)))
                          ((:on-progress ctx) {:phase :layers :current 1 :total 2})
                          {:promise (js/Promise.resolve {:status :ok :blob (js/Blob. #js ["{}"])
                                                         :name "Hero" :ext ".json" :mtype "application/json"})
                           :cancel (fn [])})
         dea/download-result (fn [_] (swap! downloads inc))}
        (fn [done']
          (start! scene)
          (after 150
                 (fn []
                   (t/is (= :done (get-in @st/state [:export-tab :status])))
                   (t/is (= [1 1] [@loaded @downloads]) "loaded once, downloaded once")
                   (t/is (= 0 @viewer) "nothing of the view mode")
                   (t/is (= [:hello :progress :done] (distinct (map :type @heard))))
                   (t/is (= {:type :done :name "Hero" :ext ".json" :size 2 :notes []}
                            (last @heard)))
                   (end! scene done'))))
        done))))

(t/deftest the-tab-waits-for-the-save
  (t/async
    done
    (let [{:keys [heard] :as scene} (job-scene nil)
          loaded (atom 0)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (swap! loaded inc) (rx/empty))}
        (fn [done']
          (start! scene)
          (after 80
                 (fn []
                   (t/is (= :waiting (get-in @st/state [:export-tab :status])))
                   (t/is (= 0 @loaded) "nothing loads before the save")
                   (t/is (= [{:type :hello}] @heard) "it says it is there")
                   (end! scene done'))))
        done))))

(t/deftest the-tab-tells-a-failed-save
  (t/async
    done
    (let [{:keys [heard] :as scene} (job-scene :save-failed)
          loaded (atom 0)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (swap! loaded inc) (rx/empty))}
        (fn [done']
          (start! scene)
          (after 100
                 (fn []
                   (t/is (= {:status :error :reason :save-failed}
                            {:status (get-in @st/state [:export-tab :status])
                             :reason (get-in @st/state [:export-tab :error :reason])}))
                   (t/is (= 0 @loaded) "a stale file is not exported")
                   (t/is (= :error (:type (last @heard))))
                   (end! scene done'))))
        done))))

(t/deftest the-workspace-cancels-the-export-of-the-tab
  (t/async
    done
    (let [{:keys [bundle heard ws] :as scene} (job-scene :saved)
          running (p/deferred)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [_ _]
                          {:promise running
                           :cancel #(p/reject running (ex-info "cancelled" {:type :export-cancelled}))})
         dea/download-result (fn [_] nil)}
        (fn [done']
          (start! scene)
          (after 80 #((:post @ws) {:type :cancel}))
          (after 160
                 (fn []
                   (t/is (= :cancelled (get-in @st/state [:export-tab :status])))
                   (t/is (= :cancelled (:type (last @heard))))
                   (end! scene done'))))
        done))))

(t/deftest a-tab-opened-by-hand-does-not-start
  (st/emit! (dat/initialize {:format "mp4"}))
  (t/is (= :invalid (get-in @st/state [:export-tab :status])))
  (st/emit! (dat/finalize)))

(t/deftest a-cancel-before-the-export-ends-the-tab-at-once
  (t/async
    done
    (let [{:keys [heard ws] :as scene} (job-scene nil)]
      (start! scene)
      (after 30
             (fn []
               (t/is (= :waiting (get-in @st/state [:export-tab :status])))
               ((:post @ws) {:type :cancel})
               (after 30
                      (fn []
                        (t/is (= :cancelled (get-in @st/state [:export-tab :status])))
                        (t/is (some #{:cancelled} (map :type @heard)))
                        (let [hellos (count (filter #(= :hello (:type %)) @heard))]
                          (after 40
                                 (fn []
                                   (t/is (= hellos (count (filter #(= :hello (:type %)) @heard)))
                                         "no more hellos")
                                   (end! scene done)))))))))))
