;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.dashboard-file-menu-test
  "The menu of a file of the dashboard: a phone has no tabs and no
  editor, a tap opens its file in the view mode there."
  (:require
   [app.main.repo :as rp]
   [app.main.ui.dashboard.file-menu :refer [file-menu*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.i18n :refer [tr]]
   [app.util.touch :as touch]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [frontend-tests.helpers.mock :as mock]
   [rumext.v2 :as mf]))

(def ^:private file
  {:id #uuid "4a0c5d2e-7b1f-4c3e-9a6d-2f8e1b0c7d51"
   :name "Coffee"
   :project-id #uuid "4a0c5d2e-7b1f-4c3e-9a6d-2f8e1b0c7d52"})

(defn- ^:async open-menu
  "The rows of the open menu of `file` on a phone (`phone?`) or a
  desktop, for a member who may edit it (`can-edit`) or only read it,
  opened from `origin` (the files of a project by default): the text of
  each item, a separator as `:separator`."
  [phone? can-edit & [origin]]
  (let [result (atom nil)]
    (await
     (mock/with-mocks*
       {touch/phone? (constantly phone?)
        rp/cmd!      (mock/stub (fn [& _] (rx/empty)))}
       (await
        (thd/with-dom
          (fn [{:keys [root document]}]
            (thd/render! root
                         (mf/element file-menu*
                                     #js {:files [file]
                                          :isOpen true
                                          :onOpenChange (fn [_])
                                          :canEdit can-edit
                                          :navigate true
                                          :onEdit (fn [])
                                          :origin (or origin :files)
                                          :canRestore false
                                          :trigger (mf/html
                                                    [:> icon-button* {:icon i/menu
                                                                      :variant "ghost"
                                                                      :aria-label "Options"}])}))
            (-> (thd/settle)
                (.then
                 (fn []
                   (reset! result
                           (some->> (.querySelector document "[role=menu]")
                                    (.-children)
                                    (mapv (fn [row]
                                            (if (= "separator" (.getAttribute row "role"))
                                              :separator
                                              (.-textContent row))))))))))))))
    @result))

(t/deftest ^:async a-phone-has-no-way-into-the-editor
  (let [rows (await (open-menu true true))]
    (t/is (seq rows) "the menu opens")
    (t/is (not-any? #{(tr "dashboard.open-in-new-tab")} rows)
          "no tabs on a phone")
    (t/is (= (tr "labels.rename") (first rows))
          "the file's own actions come first")))

(t/deftest ^:async a-phone-searching-starts-with-an-item
  (let [rows (await (open-menu true true :search))]
    (t/is (= [(tr "dashboard.download-binary-file")] rows)
          "from a search, none of the file's own actions: no separator above")))

(t/deftest ^:async a-phone-reading-a-file-starts-with-an-item
  (let [rows (await (open-menu true false))]
    (t/is (= [(tr "dashboard.download-binary-file")] rows)
          "no separator above the one item left")))

(t/deftest ^:async a-desktop-opens-the-file-in-a-new-tab
  (t/is (= (tr "dashboard.open-in-new-tab")
           (first (await (open-menu false true))))))
