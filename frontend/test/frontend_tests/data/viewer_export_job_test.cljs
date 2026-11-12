;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-export-job-test
  "The animated export of a board from the view mode, in the tab the
  workspace exports in, against a stand-in of the tab."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.exports.animation-job :as job]
   [app.main.data.viewer.export-job :as vej]
   [cljs.test :as t :include-macros true]
   [clojure.string :as str]
   [frontend-tests.helpers.export-job :as thj]))

(t/deftest the-job-of-a-board-shown
  (let [[file page board] [(uuid/next) (uuid/next) (uuid/next)]
        job (vej/job-of file page board true {:format :mp4 :quality :low :fps 24 :scale 2
                                              :loop? true :audio? false :ids? true :bbox? false})]
    (t/is (= {:file-id file :page-id page :board-id board :format :mp4 :quality :low :fps 24
              :scale 2 :loop? true :audio? false :wasm? true :ids? true}
             (dissoc job :job-id))
          "its settings, the SVG options set only")
    (t/is (uuid? (:job-id job)))
    (t/is (= [{:board-id board :scale 1} {:board-id file :scale 2}]
             (:boards (vej/job-of file page board true {:format :gif
                                                        :boards [{:board-id board :scale 1}
                                                                 {:board-id file :scale 2}]})))
          "several boards into a ZIP")))

(t/deftest how-far-the-tab-is
  (let [job      {:job-id (job/new-job-id) :format :mp4}
        progress (vej/started job)]
    (t/is (= {:in-progress true :in-tab? true :opened? false} (select-keys progress [:in-progress :in-tab? :opened?])))
    (t/is (:opened? (vej/heard progress {:type :hello})) "the tab is there")
    (t/is (= 40 (:percent (vej/heard progress {:type :progress :percent 40 :phase :capture}))))
    (t/is (:paused? (vej/heard progress {:type :paused})))
    (doseq [end [:done :error :cancelled :closed]]
      (t/is (nil? (vej/heard progress {:type end})) (str "over: " end)))
    (t/is (nil? (vej/heard nil {:type :progress :percent 3})) "nothing after its end")))

(t/deftest the-tab-opens-and-is-answered-at-once
  (t/async
    done
    (let [ctor    (thj/broadcast-channels)
          job     (vej/job-of (uuid/next) (uuid/next) (uuid/next) true {:format :lottie})
          heard   (atom [])
          opened  (atom nil)
          channel (vej/open! #{:export-animation} job #(swap! heard conj (:type %))
                             :channel-ctor ctor
                             :open (fn [uri target features] (reset! opened [uri target features])))
          tab-heard (atom [])
          tab     (job/open-channel (:job-id job) #(swap! tab-heard conj (:type %)) ctor)]
      (t/is (some? channel))
      (t/is (str/includes? (first @opened) (str (:job-id job))) "the tab of the job, in the click")
      (t/is (= ["_blank" "noopener"] (rest @opened)))
      ((:post tab) {:type :hello :job-id (:job-id job)})
      (js/setTimeout
       (fn []
         (t/is (= [:hello] @heard) "what the tab says")
         (js/setTimeout
          (fn []
            (t/is (= [:saved] @tab-heard) "answered: nothing to save")
            ((:close channel))
            ((:close tab))
            (done))
          10))
       10))))

(t/deftest no-export-without-a-channel
  (let [opened (atom false)]
    (t/is (nil? (vej/open! #{:export-animation} {:job-id (job/new-job-id)} identity
                           :channel-ctor nil
                           :open (fn [& _] (reset! opened true)))))
    (t/is (false? @opened) "no tab it could not talk to")))
