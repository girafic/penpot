;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.shapes.text
  (:require
   [app.common.data :as d]
   [app.main.data.workspace.texts :as dwt]
   [app.main.refs :as refs]
   [app.main.ui.workspace.sidebar.options.menus.blur :refer [blur-menu]]
   [app.main.ui.workspace.sidebar.options.menus.fill :refer [fill-menu]]
   [app.main.ui.workspace.sidebar.options.menus.measures :refer [measure-attrs measures-menu]]
   [app.main.ui.workspace.sidebar.options.menus.shadow :refer [shadow-menu]]
   [app.main.ui.workspace.sidebar.options.menus.text :refer [text-menu text-fill-attrs root-attrs paragraph-attrs text-attrs]]
   [rumext.alpha :as mf]))

(mf/defc options
  [{:keys [shape] :as props}]
  (let [ids [(:id shape)]
        type (:type shape)

        editors (mf/deref refs/editors)
        editor (get editors (:id shape))

        measure-values (select-keys shape measure-attrs)

        fill-values (dwt/current-text-values
                     {:editor editor
                      :shape shape
                      :attrs text-fill-attrs})

        fill-values (d/update-in-when fill-values [:fill-color-gradient :type] keyword)

        fill-values (cond-> fill-values
                      ;; Keep for backwards compatibility
                      (:fill fill-values) (assoc :fill-color (:fill fill-values))
                      (:opacity fill-values) (assoc :fill-opacity (:fill fill-values)))


        text-values (merge
                     (select-keys shape [:grow-type])
                     (dwt/current-root-values
                      {:editor editor :shape shape
                       :attrs root-attrs})
                     (dwt/current-text-values
                      {:editor editor :shape shape
                       :attrs paragraph-attrs})
                     (dwt/current-text-values
                      {:editor editor :shape shape
                       :attrs text-attrs}))]

    [:*
     [:& measures-menu {:ids ids
                        :type type
                        :values measure-values}]
     [:& fill-menu {:ids ids
                    :type type
                    :values fill-values
                    :editor editor}]
     [:& shadow-menu {:ids ids
                      :values (select-keys shape [:shadow])}]
     [:& blur-menu {:ids ids
                    :values (select-keys shape [:blur])}]
     [:& text-menu {:ids ids
                    :type type
                    :values text-values
                    :editor editor}]]))
