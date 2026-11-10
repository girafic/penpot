;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.settings
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.dashboard.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.dashboard.phone-drawer :refer [drawer-toggle*]]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.modal :refer [modal-container*]]
   [app.main.ui.settings.change-email]
   [app.main.ui.settings.delete-account]
   [app.main.ui.settings.feedback :refer [feedback-page*]]
   [app.main.ui.settings.integrations :refer [integrations-page*]]
   [app.main.ui.settings.notifications :refer [notifications-page*]]
   [app.main.ui.settings.options :refer [options-page*]]
   [app.main.ui.settings.password :refer [password-page*]]
   [app.main.ui.settings.profile :refer [profile-page*]]
   [app.main.ui.settings.shortcuts :refer [shortcuts-page*]]
   [app.main.ui.settings.sidebar :refer [sidebar*]]
   [app.main.ui.settings.subscription :refer [subscription-page*]]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.touch :as touch]
   [rumext.v2 :as mf]))

(mf/defc header*
  {::mf/wrap [mf/memo]}
  []
  [:header {:class (stl/css :dashboard-header)
            :data-testid "dashboard-header"}
   [:div {:class (stl/css :dashboard-title)}
    [:h1 {:data-testid "account-title"} (tr "dashboard.your-account-title")]]])

(mf/defc settings*
  [{:keys [route type error-report-id error-href]}]
  (let [section      (get-in route [:data :name])
        profile      (mf/deref refs/profile)

        ;; a phone shows the page alone: the sidebar comes over it as a
        ;; drawer, as in the dashboard
        phone?       (mf/with-memo [] (touch/phone?))
        drawer*      (mf/use-state false)
        drawer?      (deref drawer*)
        open-drawer  (mf/use-fn #(reset! drawer* true))
        close-drawer (mf/use-fn #(reset! drawer* false))]

    (hooks/use-shortcuts ::dashboard sc/shortcuts :dashboard)

    ;; a page chosen in the drawer closes it
    (mf/with-effect [section]
      (reset! drawer* false))

    (mf/with-effect [profile]
      (when (nil? profile)
        (st/emit! (rt/assign-exception {:type :authentication}))))

    [:*
     [:> modal-container*]
     [:section {:class (stl/css :dashboard)
                :data-phone (dm/str phone?)
                :data-drawer (when drawer? "open")}
      (when phone?
        [:> drawer-toggle* {:is-open drawer?
                            :on-open open-drawer
                            :on-close close-drawer}])

      [:> sidebar* {:profile profile
                    :section section}]

      [:div {:class (stl/css :dashboard-content)}
       [:> header*]
       [:div {:class (stl/css :dashboard-container)}
        (case section
          :settings-profile
          [:> profile-page*]

          :settings-feedback
          [:> feedback-page* {:type type
                              :error-report-id error-report-id
                              :error-href error-href}]

          :settings-password
          [:> password-page*]

          :settings-options
          [:> options-page*]

          :settings-subscription
          [:> subscription-page* {:profile profile}]

          :settings-integrations
          [:> integrations-page*]

          :settings-notifications
          [:> notifications-page* {:profile profile}]

          :settings-shortcuts
          [:> shortcuts-page* {:profile profile}])]]]]))

(mf/defc settings-page*
  {::mf/lazy-load true}
  [props]
  [:> settings* props])
