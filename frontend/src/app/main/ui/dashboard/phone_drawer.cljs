;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.dashboard.phone-drawer
  "The dashboard and the settings on a phone show their content alone:
  the sidebar comes over it from the left as a drawer (its styles in
  `_phone-drawer.scss`), by a button at the top left, in front of the
  header of each page."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.hooks :as hooks]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(mf/defc drawer-toggle*
  "The button that opens the drawer, and while it is open the backdrop
  behind it, which closes it."
  [{:keys [is-open on-open on-close]}]
  ;; the page behind it scrolls not (see `phone_drawer.scss`)
  (hooks/use-root-attribute "data-page-drawer" (when is-open "open"))
  [:*
   ;; at the top of the page as its header, not of the screen: the
   ;; rubber of the page at its ends takes it along with the header
   [:div {:class (stl/css :drawer-holder)}
    [:> icon-button* {:variant "ghost"
                      :class (stl/css :drawer-button)
                      :aria-label (tr "dashboard.phone.menu")
                      :aria-expanded is-open
                      :icon i/hamburger
                      :on-click on-open}]]
   (when is-open
     [:div {:class (stl/css :drawer-backdrop)
            :on-click on-close}])])
