;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.options.menus.mask
  "How the mask of a masked group keeps the layers it holds (see
  `cts/mask-modes`), as the track mattes of After Effects."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.workspace.groups :as dwg]
   [app.main.store :as st]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(mf/defc mask-menu*
  "The mask mode `mode` of the masked group `id`: by the opacity of the
  mask or by its brightness, inverted or not."
  [{:keys [id mode]}]
  (let [options
        (mf/with-memo []
          [{:value :alpha :label (tr "workspace.options.mask.alpha")}
           {:value :alpha-inverted :label (tr "workspace.options.mask.alpha-inverted")}
           {:value :luma :label (tr "workspace.options.mask.luma")}
           {:value :luma-inverted :label (tr "workspace.options.mask.luma-inverted")}])

        on-change
        (mf/use-fn
         (mf/deps id)
         (fn [value]
           (st/emit! (dwg/set-mask-mode id value))))]

    [:section {:class (stl/css :element-set)
               :aria-label (tr "workspace.options.mask")}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable false
                      :title (tr "workspace.options.mask")
                      :class (stl/css :title-spacing)}]]
     [:div {:class (stl/css :element-set-content)
            :title (tr "workspace.options.mask-hint")}
      [:& select {:default-value mode
                  :options options
                  :on-change on-change}]]]))
