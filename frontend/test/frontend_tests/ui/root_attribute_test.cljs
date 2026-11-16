;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.root-attribute-test
  "The attributes of the root element the pages of a phone set while
  they show, which CSS styles the whole page by instead of a `:has()` on
  `html` or `body` (each change of the DOM restyled all of it)."
  (:require
   [app.main.ui.dashboard.phone-drawer :refer [drawer-toggle*]]
   [app.main.ui.hooks :as hooks]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(mf/defc page*
  [{:keys [attr value]}]
  (hooks/use-root-attribute attr value)
  [:div])

(mf/defc pages*
  "Two pages, each shown or not, asking for the same attribute."
  [{:keys [a b]}]
  [:*
   (when a [:> page* {:attr "data-page-phone" :value a}])
   (when b [:> page* {:attr "data-page-phone" :value b}])])

(defn- attr
  [document]
  (.getAttribute (.-documentElement document) "data-page-phone"))

(t/deftest the-root-holds-the-attribute-while-a-page-shows
  (thd/with-dom
    (fn [{:keys [root document]}]
      (t/is (nil? (attr document)))
      (thd/render! root (mf/element pages* #js {:a "true"}))
      (t/is (= "true" (attr document)) "a page of a phone shows")
      (thd/render! root (mf/element pages* #js {}))
      (t/is (nil? (attr document)) "and is gone"))))

(t/deftest the-root-keeps-it-while-one-of-its-pages-shows
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element pages* #js {:a "true" :b "true"}))
      (t/is (= "true" (attr document)))
      (thd/render! root (mf/element pages* #js {:b "true"}))
      (t/is (= "true" (attr document)) "the other one still shows")
      (thd/render! root (mf/element pages* #js {}))
      (t/is (nil? (attr document))))))

(t/deftest nothing-asks-for-it-without-a-value
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element page* #js {:attr "data-page-phone" :value nil}))
      (t/is (nil? (attr document)) "a page on a computer")
      (thd/render! root (mf/element page* #js {:attr "data-page-phone" :value "true"}))
      (t/is (= "true" (attr document)) "turned to a phone layout")
      (thd/render! root (mf/element page* #js {:attr "data-page-phone" :value nil}))
      (t/is (nil? (attr document)) "and back"))))

(t/deftest a-page-changes-its-value-as-it-shows
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element page* #js {:attr "data-page-phone" :value "a"}))
      (t/is (= "a" (attr document)))
      (thd/render! root (mf/element page* #js {:attr "data-page-phone" :value "b"}))
      (t/is (= "b" (attr document)) "the new value, the old one no longer counted")
      (thd/render! root (mf/element pages* #js {}))
      (t/is (nil? (attr document))))))

(defn- drawer
  [document]
  (.getAttribute (.-documentElement document) "data-page-drawer"))

(t/deftest the-page-behind-an-open-drawer-scrolls-not
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [render! #(thd/render! root (mf/element drawer-toggle* #js {:isOpen %
                                                                       :onOpen (fn [])
                                                                       :onClose (fn [])}))]
        (render! false)
        (t/is (nil? (drawer document)) "closed")
        (render! true)
        (t/is (= "open" (drawer document)) "open: the root stops the page (see `phone_drawer.scss`)")
        (render! false)
        (t/is (nil? (drawer document)) "closed again")))))
