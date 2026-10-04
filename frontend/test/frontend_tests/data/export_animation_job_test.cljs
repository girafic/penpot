;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.export-animation-job-test
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.exports.animation-job :as job]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.export-job :as thj]
   [promesa.core :as p]))

(defn- job-params
  [& {:as more}]
  (merge {:file-id (uuid/next)
          :page-id (uuid/next)
          :board-id (uuid/next)
          :job-id (job/new-job-id)
          :format :mp4
          :quality :high
          :fps 30
          :scale 1.5
          :loop? true
          :audio? false
          :wasm? true}
         more))

(t/deftest the-settings-of-a-job-travel-in-the-url
  (let [params (job-params)]
    (t/is (= params (job/decode-params (job/encode-params params))))
    (t/is (every? string? (vals (job/encode-params params))) "as query params"))
  (t/testing "a size that fits a small board to a large height"
    (let [params (job-params :scale 21.6)]
      (t/is (= params (job/decode-params (job/encode-params params))))))
  (t/testing "bad ones are refused"
    (let [query (job/encode-params (job-params))]
      (t/is (nil? (job/decode-params (assoc query :format "mov"))))
      (t/is (nil? (job/decode-params (assoc query :fps "0"))))
      (t/is (nil? (job/decode-params (assoc query :fps "500"))))
      (t/is (nil? (job/decode-params (assoc query :scale "0"))))
      (t/is (nil? (job/decode-params (dissoc query :board-id))))
      (t/is (nil? (job/decode-params (assoc query :job-id "nope"))))))
  (t/testing "without WASM, only the Lottie formats of vector layers"
    (t/is (nil? (job/decode-params (job/encode-params (job-params :wasm? false)))))
    (t/is (nil? (job/decode-params (job/encode-params (job-params :wasm? false :format :svg)))))
    (t/is (some? (job/decode-params (job/encode-params (job-params :wasm? false :format :lottie)))))
    (t/is (some? (job/decode-params (job/encode-params (job-params :wasm? false :format :dotlottie)))))))

(t/deftest the-messages-of-a-job
  (let [message {:type :done :name "Hero" :size 1234 :notes [[:fitted 4092 2302]]}]
    (t/is (= message (job/decode-message (job/encode-message message)))))
  (t/is (= {:type :progress :phase :capture :current 3 :total 9 :percent 27}
           (job/decode-message (job/encode-message {:type :progress :phase :capture :current 3 :total 9 :percent 27}))))
  (t/testing "what is no message of a job is ignored"
    (t/is (nil? (job/decode-message (job/encode-message {:type :launch}))))
    (t/is (nil? (job/decode-message "not transit")))
    (t/is (nil? (job/decode-message #js {:type "hello"})))))

(t/deftest the-two-sides-of-a-job-hear-each-other
  (t/async
    done
    (let [ctor     (thj/broadcast-channels)
          id       (job/new-job-id)
          heard-ws (atom [])
          heard-tb (atom [])
          ws       (job/open-channel id #(swap! heard-ws conj %) ctor)
          tab      (job/open-channel id #(swap! heard-tb conj %) ctor)
          other    (job/open-channel (job/new-job-id) #(swap! heard-ws conj [:other %]) ctor)]
      ((:post tab) {:type :hello})
      ((:post ws) {:type :saved})
      (js/setTimeout
       (fn []
         (t/is (= [{:type :hello}] @heard-ws) "not what it posts itself, not another job")
         (t/is (= [{:type :saved}] @heard-tb))
         (doseq [c [ws tab other]] ((:close c)))
         (done))
       20))))

(t/deftest the-workspace-knows-the-tab-is-gone-by-its-lock
  (t/async
    done
    (let [locks   (thj/locks)
          id      (job/new-job-id)
          release (atom nil)
          gone?   (atom false)]
      (-> (job/hold-lock id (js/Promise. (fn [resolve _] (reset! release resolve))) locks)
          (.then (fn [_]
                   (-> (job/when-released id locks)
                       (.then (fn [_] (reset! gone? true))))
                   (js/Promise. (fn [resolve _] (js/setTimeout resolve 20)))))
          (.then (fn [_]
                   (t/is (not @gone?) "while the tab holds it")
                   (@release nil)
                   (js/Promise. (fn [resolve _] (js/setTimeout resolve 20)))))
          (.then (fn [_] (t/is @gone? "once it lets it go")))
          (.finally (fn [] (done) nil))))))

(t/deftest a-second-tab-does-not-get-the-lock-of-the-job
  (t/async
    done
    (let [locks  (thj/locks)
          id     (job/new-job-id)
          first  (p/deferred)
          second (p/deferred)]
      (-> (job/hold-lock id first locks)
          (.then (fn [held?]
                   (t/is (true? held?) "the first tab holds it")
                   (job/hold-lock id second locks)))
          (.then (fn [held?]
                   (t/is (false? held?) "the second does not wait for it")
                   (p/resolve first nil)
                   ;; the lock goes once its holder's promise settles
                   (js/Promise. (fn [resolve] (js/setTimeout resolve 10)))))
          (.then (fn [_] (job/hold-lock id second locks)))
          (.then (fn [held?] (t/is (true? held?) "free once the first tab is gone")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (fn [] (p/resolve second nil) (done) nil))))))
