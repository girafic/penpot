;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.data.svg-upload-test
  "Tests for the media upload triggered from an SVG import."
  (:require
   [app.common.files.helpers :as cfh]
   [app.common.files.svg-animation :as svga]
   [app.common.transit :as tr]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.media :as media]
   [app.main.data.workspace.svg-animation :as dwsa]
   [app.main.data.workspace.svg-upload :as svg]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.async :as async]
   [frontend-tests.helpers.http :as http]
   [frontend-tests.helpers.mock :as mock]
   [potok.v2.core :as ptk]))

(defn- image-node
  [href]
  {:tag :image
   :attrs {:href href :width "10" :height "20"}
   :content []})

(t/deftest upload-images-drops-svg-only-attrs
  (t/testing "media upload params do not carry svg-only attrs"
    (t/async done
      (let [file-id (uuid/next)
            bodies  (atom [])

            fetch-mock
            (fn [url opts]
              (swap! bodies conj {:cmd  (http/url->cmd url)
                                  :body (.-body opts)})
              (js/Promise.resolve
               (http/make-transit-response
                {:id      (uuid/next)
                 :file-id file-id
                 :name    "pic"
                 :mtype   "image/png"
                 :width   10
                 :height  20})))

            orig     (http/install-fetch-mock! fetch-mock)
            svg-data {:tag :svg
                      :attrs {}
                      :content [(image-node "https://example.com/pic.png")]}]

        (->> (svg/upload-images svg-data file-id)
             (rx/subs!
              (fn [_] nil)
              (fn [err]
                (http/restore-fetch! orig)
                (t/is false (str "unexpected error: " (ex-message err)))
                (done))
              (fn []
                (http/restore-fetch! orig)
                (let [{:keys [cmd body]} (first @bodies)
                      params (tr/decode-str body)]
                  (t/is (= :create-file-media-object-from-url cmd))
                  (t/is (= "https://example.com/pic.png" (:url params)))
                  (t/is (not (contains? params :href)))
                  (t/is (not (contains? params :width)))
                  (t/is (not (contains? params :height))))
                (done))))))))

(t/deftest ^:async an-svg-component-gets-fonts-the-workspace-has
  ;; uploaded as a component: no Arial here, so the default font, at the
  ;; nearest weight
  (let [svg-data     {:name "label" :tag :svg :attrs {:viewBox "0 0 100 40" :width "100" :height "40"}
                      :content [{:tag :text :attrs {:x "0" :y "20" :font-family "Arial" :font-weight "bold"}
                                 :content ["Hi"]}]}
        [_ children] (await (async/->promise (media/create-shapes-svg (uuid/next) {} {:x 0 :y 0} svg-data)))
        text         (some #(when (cfh/text-shape? %) %) children)
        node         (get-in text [:content :children 0 :children 0 :children 0])]
    (t/is (= "Hi" (:text node)))
    (t/is (= ["sourcesanspro" "700"] [(:font-id node) (:font-weight node)]))))

(t/deftest ^:async an-animated-svg-that-fails-to-import-is-an-invalid-svg
  ;; as the import of a static one says it, not as an internal error
  (let [error (atom nil)]
    (await
     (mock/with-mocks*
       {svga/svg->shapes (mock/stub (fn [& _] (throw (ex-info "unexpected" {}))))}
       (await (async/observe (ptk/watch (dwsa/svg-animation-imported {:tag :svg :attrs {:viewBox "0 0 10 10"} :content []}
                                                                     {:x 0 :y 0})
                                        {} (rx/empty))
                             :on-error #(reset! error %)))))
    (t/is (= :svg-parser (:type (ex-data @error))))))
