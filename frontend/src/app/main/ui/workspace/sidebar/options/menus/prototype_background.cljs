;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.options.menus.prototype-background
  "The background of the prototype of the page in the prototype tab,
  with nothing selected (see `interactions-menu*`): a color or a
  gradient round its boards shown without a device in the view mode
  (see `app.main.ui.viewer.background`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.types.color :as clr]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.undo :as dwu]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.workspace.sidebar.options.rows.color-row :refer [color-row*]]
   [app.util.i18n :refer [tr]]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:prototype-background
  (-> (l/key :prototype-background)
      (l/derived refs/workspace-page)))

(def ^:private added-background
  "The background of the prototype once added: the black the preview of
  the workspace shows round a board without a device."
  "#000000")

(defn- picked-background
  "The background of the prototype that `color`, as the color row gives
  it, sets: its gradient, else its color."
  [{:keys [color gradient]}]
  (if (some? gradient)
    (select-keys gradient clr/gradient-attrs)
    color))

(mf/defc prototype-background-menu*
  "The color or the gradient round the boards of the prototype of the
  page that show without a device in the view mode: added, it shows in
  a row that changes it or removes it, which gives the view mode its
  own color back."
  []
  (let [background (mf/deref ref:prototype-background)
        color      (mf/with-memo [background]
                     (if (map? background)
                       {:gradient background :opacity 1}
                       {:color background :opacity 1}))
        on-add     (mf/use-fn #(st/emit! (dw/change-prototype-background added-background)))
        on-change  (mf/use-fn #(st/emit! (dw/change-prototype-background (picked-background %))))
        on-remove  (mf/use-fn #(st/emit! (dw/change-prototype-background nil)))
        on-open    (mf/use-fn #(st/emit! (dwu/start-undo-transaction :prototype-background)))
        on-close   (mf/use-fn #(st/emit! (dwu/commit-undo-transaction :prototype-background)))]
    [:div {:class (stl/css :section)}
     [:div {:class (stl/css :title)}
      [:> title-bar* {:collapsable false
                      :title (tr "workspace.options.prototype-background")
                      :class (stl/css :title-bar)}
       (when (nil? background)
         [:> icon-button* {:variant "ghost"
                           :aria-label (tr "workspace.options.prototype-background-add")
                           :on-click on-add
                           :icon i/add}])]]
     (when (some? background)
       [:div {:class (stl/css :content)}
        [:> color-row* {:disable-opacity true
                        :disable-image true
                        :title (tr "workspace.options.prototype-background")
                        :color color
                        :on-change on-change
                        :on-remove on-remove
                        :origin :prototype-background
                        :on-open on-open
                        :on-close on-close}]
        [:p {:class (stl/css :background-hint)}
         (tr "workspace.options.prototype-background-hint")]])]))
