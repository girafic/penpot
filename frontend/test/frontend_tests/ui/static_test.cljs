;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.ui.static-test
  (:require
   [app.main.ui.context :as ctx]
   [app.main.ui.static :as static]
   [app.util.i18n :refer [tr]]
   [app.util.touch :as touch]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [frontend-tests.helpers.mock :as mock]
   [rumext.v2 :as mf]))

(t/deftest screen-kind-classifies-known-screens
  ;; Regression test (penpot/penpot#11224): the exception page shows the
  ;; REQUEST ACCESS modal only when it recognizes the screen the failing
  ;; URL belongs to. The `screen` query param carries the route name, so
  ;; the viewer is "viewer" and every team dashboard is "dashboard-*".
  (t/testing "workspace"
    (t/is (= :workspace (static/screen-kind {:screen "workspace"}))))

  (t/testing "bare dashboard prefix also classifies"
    ;; No such route exists (routes are always "dashboard-*"), and the
    ;; modal still needs loaded team/file info, so this is harmless and
    ;; matches the prefix convention used in routes.cljs.
    (t/is (= :dashboard (static/screen-kind {:screen "dashboard"}))))

  (t/testing "viewer"
    (t/is (= :viewer (static/screen-kind {:screen "viewer"}))))

  (t/testing "dashboard variants"
    (doseq [screen ["dashboard-recent"
                    "dashboard-members"
                    "dashboard-files"
                    "dashboard-settings"]]
      (t/is (= :dashboard (static/screen-kind {:screen screen}))
            (str "screen: " screen)))))

(t/deftest screen-kind-ignores-unknown-screens
  ;; Unknown, missing, or auth screens are not request-access candidates:
  ;; the exception page must keep showing the plain not-found page.
  (doseq [params [nil
                  {}
                  {:screen "auth-login"}
                  {:screen "settings-profile"}
                  ;; The "view" literal never was a real screen name; the
                  ;; exception page compared against it before the fix.
                  {:screen "view"}]]
    (t/is (nil? (static/screen-kind params))
          (str "params: " (pr-str params)))))

(t/deftest screen-kind-tolerates-duplicate-screen-params
  ;; A repeated query param arrives as a vector; the classifier reads the
  ;; scalar the same way the router does instead of crashing on it.
  (t/is (= :dashboard
           (static/screen-kind {:screen ["dashboard-recent" "dashboard-recent"]}))))

;; --- On a phone: the pages a link opens laid out for it, only where the
;; route is a phone's (`touch/phone-layout?`)

(mf/defc on-route*
  {::mf/private true}
  [{:keys [screen children]}]
  [:& (mf/provider ctx/current-route) {:value {:data {:name screen}}}
   children])

(defn- ^:async rendered
  "What `read` takes of the document once `element` rendered on the route
  named `screen` of a phone (`phone?`) or of a desktop."
  [element screen phone? read]
  (let [result (atom nil)]
    (await
     (mock/with-mocks*
       {touch/phone? (constantly phone?)}
       (await
        (thd/with-dom
          (fn [{:keys [root document window]}]
            ;; React loaded before there was a DOM and takes the one of
            ;; jsdom for an old IE's: on the field the login focuses, it
            ;; would watch changes by `attachEvent`
            (let [proto (.. window -HTMLElement -prototype)]
              (set! (.-attachEvent proto) (fn []))
              (set! (.-detachEvent proto) (fn [])))
            (thd/render! root (mf/element on-route* #js {:screen screen :children element}))
            (-> (thd/settle)
                (.then #(reset! result (read document)))))))))
    @result))

(defn- phone-attr
  "The `data-phone` of the first element under `selector`."
  [document selector]
  (some-> (.querySelector document selector) (.getAttribute "data-phone")))

(defn- close-button?
  [document]
  (some? (.querySelector document (str "[aria-label=\"" (tr "labels.close") "\"]"))))

(defn- login
  [document]
  {:phone (phone-attr document "#root > [data-phone]")
   :close (close-button? document)})

(t/deftest ^:async link-login-on-a-phone
  (let [login-modal (mf/element @#'static/login-modal* #js {})]
    (t/is (= {:phone "true" :close false}
             (await (rendered login-modal :viewer true login)))
          "a view mode link of a phone: the login is its page, nothing to close")
    (t/is (= {:phone "false" :close true}
             (await (rendered login-modal :workspace true login)))
          "a workspace link of a phone: a desktop page, the card of a desktop")
    (t/is (= {:phone "false" :close true}
             (await (rendered login-modal :viewer false login)))
          "a view mode link of a desktop")))

