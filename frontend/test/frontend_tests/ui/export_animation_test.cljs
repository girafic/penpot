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
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.export-job :as thj]
   [frontend-tests.helpers.mock :as mock]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(t/deftest the-pictures-of-the-frames-show-the-board
  (let [selrect {:x 100 :y 200 :width 400 :height 200}]
    (t/is (= {:width 800 :height 400 :selrect selrect} (uea/frame-view selrect 2))
          "a canvas of the board at the scale of the export")
    (t/is (nil? (uea/frame-view nil 1)))
    (t/is (nil? (uea/frame-view selrect nil)))
    (t/testing "where the board starts in a picture larger than it"
      (t/is (= [0 24] (dea/board-offset selrect {:x 100 :y 188 :width 400 :height 224} 2))))
    (t/testing "while it exports"
      (let [board (uuid/next)]
        (t/is (= {:width 400 :height 200 :selrect selrect}
                 (:frame (uea/page-state {:status :exporting
                                          :params {:board-id board :format :avif}
                                          :page {:objects {board {:name "Spin" :selrect selrect}}}
                                          :frame-scale 1}))))))))

(t/deftest the-title-tells-the-board-and-how-far-it-is
  (let [board (uuid/next)
        tab   {:params {:board-id board :format :mp4}
               :page {:objects {board {:name "Orbit"}}}}]
    (t/is (= {:board "Orbit" :percent 49} (uea/page-title (assoc tab :status :exporting :percent 49))))
    (t/is (= {:board "Orbit"} (uea/page-title (assoc tab :status :done))))
    (t/is (nil? (uea/page-title {:status :loading})) "before it knows the board")))

(t/deftest the-page-shows-where-the-export-is
  (let [board  (uuid/next)
        tab    {:params {:board-id board :format :mp4}
                :page {:objects {board {:name "Hero"}}}}
        view   #(uea/page-state (merge tab %))]
    (t/is (= {:text ["export-animation.waiting"] :actions []} (view {:status :waiting})))
    (t/is (= {:text ["export-animation.loading"] :actions []} (view {:status :loading})))
    (t/is (= {:text ["export-animation.exporting" "Hero" "MP4"] :percent 45 :actions [:cancel]}
             (view {:status :exporting :percent 45})))
    (t/is (= ["export-animation.exporting" "Hero" "WebM"]
             (:text (view {:status :exporting :params {:board-id board :format "webm"}})))
          "the format written as the panel writes it")
    (t/is (= [] (:actions (view {:status :exporting :cancelling? true}))) "no second Cancel")
    (t/is (= {:text ["export-animation.done" "Hero.mp4" "1.5 MB"]
              :notes [[:audio-dropped]]
              :preview {:kind :video :url "blob:hero" :name "Hero.mp4"}
              :actions [:download :close]}
             (view {:status :done :result {:name "Hero" :ext ".mp4" :size (* 1.5 1024 1024)
                                           :notes [[:audio-dropped]] :url "blob:hero"}})))
    (t/is (= [:close] (:actions (view {:status :cancelled}))))
    (t/is (= ["export-animation.stale"] (:text (view {:status :stale}))))
    (t/is (= {:text ["export-animation.duplicate"] :actions [:close]} (view {:status :duplicate}))
          "another tab exports the job")
    (t/is (= ["export-animation.error.save-failed"] (:text (view {:status :error :error {:reason :save-failed}}))))
    (t/is (= ["export-animation.error.export" "boom"] (:text (view {:status :error :error {:reason :export :message "boom"}}))))
    (t/is (= ["export-animation.invalid"] (:text (uea/page-state {:status :invalid}))))
    (t/is (= {:text ["export-animation.waiting"] :actions []} (uea/page-state nil))
          "before the tab starts its job")))

(t/deftest the-done-page-plays-what-the-browser-plays
  (t/is (= [:video :video] (map uea/preview-kind [".mp4" ".webm"])))
  (t/is (= [:image :image :image] (map uea/preview-kind [".gif" ".avif" ".svg"]))
        "an animated image; an SVG runs no script there")
  (t/is (= [:none :none] (map uea/preview-kind [".json" ".lottie"])) "no Lottie player yet"))

;; THE TAB, AGAINST A STAND-IN OF THE WORKSPACE

(defn- job-scene
  "The params of a job (a Lottie without WASM unless `params` says
  otherwise), the bundle of its file and the workspace's side of the
  channel, answering each `:hello` with `answer`. `opts` go to
  `dat/initialize` with the fake channels and locks."
  [answer & {:keys [params opts]}]
  (let [file-id  (uuid/next)
        page-id  (uuid/next)
        board-id (uuid/next)
        params   (merge {:file-id file-id :page-id page-id :board-id board-id :job-id (job/new-job-id)
                         :format :lottie :quality :high :fps 30 :scale 1 :loop? true :audio? false
                         :wasm? false}
                        params)
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
     :opts opts
     :heard heard
     :ws ws
     :bundle {:file {:id file-id
                     :data {:pages-index {page-id {:id page-id
                                                   :objects {board-id {:id board-id :name "Hero"}}}}}}
              :libraries []
              :fonts []
              :team {:features #{}}}}))

(defn- start!
  [{:keys [params ctor locks opts]}]
  (st/emit! (apply dat/initialize (job/encode-params params)
                   (mapcat identity (merge {:channel-ctor ctor :locks locks} opts)))))

(defn- status
  []
  (get-in @st/state [:export-tab :status]))

(defn- types
  [heard]
  (mapv :type @heard))

(defn- ok-result
  []
  {:status :ok :blob (js/Blob. #js ["{}"]) :name "Hero" :ext ".json" :mtype "application/json"})

(defn- fail!
  [cause]
  (t/is false (str "unexpected: " (ex-message cause))))

(defn- end!
  "Close the tab and the workspace's side, then `done'`; returns nothing."
  [{:keys [ws]} done']
  (st/emit! (dat/finalize))
  ((:close @ws))
  (done')
  nil)

(defn- pause
  "A promise resolved after `ms`: only to see that nothing more happens."
  [ms]
  (js/Promise. (fn [resolve] (js/setTimeout resolve ms))))

(t/deftest the-tab-exports-once-the-workspace-has-saved
  (t/async
    done
    (let [{:keys [bundle heard] :as scene} (job-scene :saved)
          loaded    (atom 0)
          downloads (atom 0)
          viewer    (atom 0)
          wasm      (atom 0)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (swap! loaded inc) (rx/of bundle))
         dv/update-page-position-data (fn [& _] (swap! viewer inc) (ptk/data-event ::noop))
         dv/go-to-frame-auto (fn [& _] (swap! viewer inc) (ptk/data-event ::noop))
         dea/run-export (fn [ctx params]
                          (t/is (= "Hero" (get-in ctx [:page :objects (:board-id ctx) :name])) "the page of the file")
                          (t/is (= :lottie (:format params)))
                          ((:on-progress ctx) {:phase :layers :current 1 :total 2})
                          {:promise (js/Promise.resolve (ok-result)) :cancel (fn [])})
         dea/download-result (fn [_] (swap! downloads inc))}
        (fn [done']
          (start! (assoc scene :opts {:start-wasm (fn [_ _] (swap! wasm inc) (js/Promise.resolve nil))}))
          ;; what the workspace hears comes a task later
          (-> (thj/wait-until #(= :done (last (types heard))))
              (.then (fn [_]
                       (t/is (= [1 1] [@loaded @downloads]) "loaded once, downloaded once")
                       (t/is (= 0 @viewer) "nothing of the view mode")
                       (t/is (= 0 @wasm) "no WASM without it in the workspace")
                       (t/is (= [:hello :progress :done] (distinct (types heard))))
                       (t/is (= {:type :done :name "Hero" :ext ".json" :size 2 :notes []}
                                (last @heard)))))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest a-lottie-of-a-wasm-workspace-starts-wasm-first
  (t/async
    done
    (let [{:keys [bundle] :as scene} (job-scene :saved :params {:wasm? true})
          calls (atom [])]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [_ _]
                          (swap! calls conj :export)
                          {:promise (js/Promise.resolve (ok-result)) :cancel (fn [])})
         dea/download-result (fn [_] nil)}
        (fn [done']
          (start! (assoc scene :opts {:start-wasm (fn [_ _] (swap! calls conj :wasm) (js/Promise.resolve nil))}))
          (-> (thj/wait-until #(= :done (status)))
              (.then (fn [_] (t/is (= [:wasm :export] @calls) "its layers that are no vectors are pictures")))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest a-cancel-while-wasm-starts-exports-nothing
  (t/async
    done
    (let [{:keys [bundle heard ws] :as scene} (job-scene :saved :params {:format :mp4 :wasm? true})
          starting  (p/deferred)
          exports   (atom 0)
          downloads (atom 0)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [_ _]
                          (swap! exports inc)
                          {:promise (js/Promise.resolve (ok-result)) :cancel (fn [])})
         dea/download-result (fn [_] (swap! downloads inc))}
        (fn [done']
          (start! (assoc scene :opts {:start-wasm (fn [_ _] starting)}))
          (-> (thj/wait-until #(= :exporting (status)))
              (.then (fn [_]
                       ((:post @ws) {:type :cancel})
                       (thj/wait-until #(get-in @st/state [:export-tab :cancelling?]))))
              (.then (fn [_]
                       (p/resolve starting nil)
                       (thj/wait-until #(= :cancelled (status)))))
              (.then (fn [_]
                       (t/is (= [0 0] [@exports @downloads]) "no export, no download")
                       (t/is (= :cancelled (last (types heard))))))
              (.catch fail!)
              (.finally #(end! scene done'))))
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
          (-> (thj/wait-until #(seq @heard))
              (.then (fn [_]
                       (t/is (= :waiting (status)))
                       (t/is (= 0 @loaded) "nothing loads before the save")
                       (t/is (= [:hello] (types heard)) "it says it is there")))
              (.catch fail!)
              (.finally #(end! scene done'))))
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
          (-> (thj/wait-until #(= :error (last (types heard))))
              (.then (fn [_]
                       (t/is (= [:error :save-failed]
                                [(status) (get-in @st/state [:export-tab :error :reason])]))
                       (t/is (= 0 @loaded) "a stale file is not exported")))
              (.catch fail!)
              (.finally #(end! scene done'))))
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
          (-> (thj/wait-until #(= :exporting (status)))
              (.then (fn [_]
                       ((:post @ws) {:type :cancel})
                       (thj/wait-until #(= :cancelled (status)))))
              (.then (fn [_] (t/is (= :cancelled (last (types heard))))))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest a-tab-opened-by-hand-does-not-start
  (st/emit! (dat/initialize {:format "mp4"}))
  (t/is (= :invalid (status)))
  (st/emit! (dat/finalize)))

(t/deftest a-cancel-before-the-export-ends-the-tab-at-once
  (t/async
    done
    (let [{:keys [heard ws] :as scene} (job-scene nil)]
      (start! scene)
      (-> (thj/wait-until #(seq @heard))
          (.then (fn [_]
                   (t/is (= :waiting (status)))
                   ((:post @ws) {:type :cancel})
                   (thj/wait-until #(= :cancelled (status)))))
          (.then (fn [_] (t/is (some #{:cancelled} (types heard)))))
          (.catch fail!)
          (.finally #(end! scene done))))))

(t/deftest a-second-tab-of-the-job-does-not-export
  (t/async
    done
    (let [{:keys [params locks heard ws] :as scene} (job-scene nil)
          loaded    (atom 0)
          first-tab (p/deferred)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (swap! loaded inc) (rx/empty))}
        (fn [done']
          ;; the first tab of the job holds its lock
          (-> (job/hold-lock (:job-id params) first-tab locks)
              (.then (fn [held?]
                       (t/is (true? held?))
                       (start! scene)
                       (thj/wait-until #(= :duplicate (status)))))
              (.then (fn [_]
                       ((:post @ws) {:type :saved})
                       (pause 30)))
              (.then (fn [_]
                       (t/is (= :duplicate (status)) "the save is not for it")
                       (t/is (= 0 @loaded))
                       (t/is (empty? @heard) "it never says it is there")))
              (.catch fail!)
              (.finally (fn []
                          (p/resolve first-tab nil)
                          (end! scene done')))))
        done))))

(t/deftest a-second-saved-loads-nothing-more
  (t/async
    done
    (let [{:keys [bundle ws] :as scene} (job-scene nil)
          loaded (atom 0)]
      (mock/with-mocks
        {dv/load-bundle (fn [_]
                          (swap! loaded inc)
                          (->> (rx/timer 30) (rx/map (constantly bundle))))
         dea/run-export (fn [_ _] {:promise (js/Promise.resolve (ok-result)) :cancel (fn [])})
         dea/download-result (fn [_] nil)}
        (fn [done']
          (start! scene)
          (-> (thj/wait-until #(= :waiting (status)))
              (.then (fn [_]
                       ;; the end of the save, and the answer to a hello
                       ((:post @ws) {:type :saved})
                       ((:post @ws) {:type :saved})
                       (thj/wait-until #(= :done (status)))))
              (.then (fn [_] (t/is (= 1 @loaded))))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest an-export-error-is-told
  (t/async
    done
    (let [{:keys [bundle heard] :as scene} (job-scene :saved)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [_ _] {:promise (js/Promise.reject (ex-info "boom" {})) :cancel (fn [])})}
        (fn [done']
          (start! scene)
          (-> (thj/wait-until #(= :error (last (types heard))))
              (.then (fn [_]
                       (t/is (= {:type :error :reason :export :message "boom"} (last @heard)))
                       (t/is (= ["export-animation.error.export" "boom"]
                                (:text (uea/page-state (:export-tab @st/state)))))))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest progress-is-told-at-most-every-100-ms
  (t/async
    done
    (let [{:keys [bundle heard] :as scene} (job-scene :saved)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [{:keys [on-progress]} _]
                          (doseq [i (range 1 6)]
                            (on-progress {:phase :layers :current i :total 5}))
                          (on-progress {:phase :write :current 1 :total 1})
                          {:promise (js/Promise.resolve (ok-result)) :cancel (fn [])})
         dea/download-result (fn [_] nil)}
        (fn [done']
          (start! scene)
          (-> (thj/wait-until #(= :done (last (types heard))))
              (.then (fn [_]
                       (t/is (= [18 100] (->> @heard (filter #(= :progress (:type %))) (mapv :percent)))
                             "the first, then the end")))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest download-again-downloads-the-kept-file
  (t/async
    done
    (let [{:keys [bundle] :as scene} (job-scene :saved)
          result    (ok-result)
          downloads (atom [])]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [_ _] {:promise (js/Promise.resolve result) :cancel (fn [])})
         dea/download-result (fn [r] (swap! downloads conj r))}
        (fn [done']
          (start! scene)
          (-> (thj/wait-until #(= :done (status)))
              (.then (fn [_]
                       (st/emit! (dat/download-again))
                       (t/is (= [result result] @downloads))))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest the-file-plays-until-the-tab-closes
  (t/async
    done
    (let [{:keys [bundle] :as scene} (job-scene :saved)
          made (atom 0)]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [_ _] {:promise (js/Promise.resolve (ok-result)) :cancel (fn [])})
         dea/download-result (fn [_] nil)
         wapi/create-uri (mock/stub (fn [_] (swap! made inc) "blob:preview"))
         wapi/revoke-uri (mock/stub mock/revoke-uri-mock)}
        (fn [done']
          (start! scene)
          (-> (thj/wait-until #(= :done (status)))
              (.then (fn [_]
                       (t/is (= 1 @made) "one URL of the file")
                       (t/is (= {:kind :none :url "blob:preview" :name "Hero.json"}
                                (:preview (uea/page-state (:export-tab @st/state))))
                             "a Lottie: no player yet")
                       (st/emit! (dat/finalize))
                       (t/is (= ["blob:preview"] @mock/revoked-uris) "gone with the tab")))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest a-tab-closed-before-it-holds-the-lock-says-nothing
  (t/async
    done
    (let [{:keys [params ctor heard] :as scene} (job-scene :saved)
          grant (atom nil)
          ;; a lock granted only once asked to
          locks #js {:request (fn [name _ callback]
                                (js/Promise.
                                 (fn [resolve _]
                                   (reset! grant #(resolve (callback #js {:name name}))))))}]
      (st/emit! (dat/initialize (job/encode-params params) :channel-ctor ctor :locks locks))
      (-> (thj/wait-until #(some? @grant))
          (.then (fn [_]
                   (st/emit! (dat/finalize))
                   (@grant)
                   (pause 40)))
          (.then (fn [_]
                   (t/is (nil? (:export-tab @st/state)))
                   (t/is (empty? @heard) "no channel, no hello from a closed tab")))
          (.catch fail!)
          (.finally #(end! scene done))))))

(t/deftest a-late-preview-is-for-a-done-export-only
  (st/emit! (dat/initialize {:format "mp4"}))
  (swap! st/state assoc-in [:export-tab :status] :cancelled)
  (st/emit! (#'dat/preview-ready "blob:late"))
  (t/is (nil? (get-in @st/state [:export-tab :result :url])) "nothing plays for a cancelled export")
  (swap! st/state assoc-in [:export-tab :status] :done)
  (st/emit! (#'dat/preview-ready "blob:file"))
  (t/is (= "blob:file" (get-in @st/state [:export-tab :result :url])))
  (st/emit! (dat/finalize)))

(t/deftest the-tab-shows-each-frame-it-takes
  (t/async
    done
    (let [{:keys [bundle] :as scene} (job-scene :saved :params {:format :mp4 :wasm? true})
          running (p/deferred)
          take!   (atom nil)
          drawn   (atom [])]
      (mock/with-mocks
        {dv/load-bundle (fn [_] (rx/of bundle))
         dea/run-export (fn [{:keys [on-frame]} _]
                          (reset! take! on-frame)
                          {:promise running :cancel (fn [])})}
        (fn [done']
          (start! (assoc scene :opts {:start-wasm (fn [_ _] (js/Promise.resolve nil))}))
          (-> (thj/wait-until #(some? @take!))
              (.then (fn [_]
                       (@take! :frame-0 {:x 0 :y -10 :width 100 :height 70} 2)
                       (t/is (= 2 (get-in @st/state [:export-tab :frame-scale])) "the page shows them")
                       ;; the page draws the one taken last, then each one
                       (let [stop (dat/watch-frames! #(swap! drawn conj %&))]
                         (@take! :frame-1 nil 2)
                         (@take! :frame-2 nil 2)
                         (stop)
                         (@take! :frame-3 nil 2))
                       (t/is (= [[:frame-0 {:x 0 :y -10 :width 100 :height 70} 2]
                                 [:frame-1 nil 2]
                                 [:frame-2 nil 2]]
                                @drawn)
                             "none after it stops")
                       (st/emit! (dat/finalize))))
              (.catch fail!)
              (.finally #(end! scene done'))))
        done))))

(t/deftest a-tab-the-workspace-never-answers-goes-stale
  (t/async
    done
    (let [scene (job-scene nil :opts {:save-wait-ms 40})]
      (start! scene)
      (-> (thj/wait-until #(= :stale (status)))
          (.then (fn [_]
                   (t/is (= ["export-animation.stale"] (:text (uea/page-state (:export-tab @st/state)))))))
          (.catch fail!)
          (.finally #(end! scene done))))))
