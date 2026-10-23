;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.components.check-menu-item
  "A row of a menu (see `dropdown-menu*`) with a column for its tick."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.shortcuts :as scd]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu-item*]]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.util.dom :as dom]
   [app.util.keyboard :as kbd]
   [rumext.v2 :as mf]))

(defn- on-key-down
  "Enter picks the row, as a click: here, not in the workspace too, whose
  shortcuts take Enter for their own."
  [event]
  (when (kbd/enter? event)
    (dom/prevent-default event)
    (dom/stop-propagation event)
    (dom/click (dom/get-current-target event))))

(mf/defc check-menu-item*
  "A row of `dropdown-menu*` ticked while `checked` is true: one of a
  choice when `radio`, else on or off, and an action when `checked` is
  nil. With an `icon` and the keys of its `shortcut` (see
  `scd/split-sc`) when given; `value` is its `data-value`, for `on-click`
  to read."
  [{:keys [label icon shortcut checked radio value on-click]}]
  (let [role (cond
               (nil? checked)  "menuitem"
               ^boolean radio  "menuitemradio"
               :else           "menuitemcheckbox")]
    [:> dropdown-menu-item* {:class (stl/css :item)
                             :role role
                             :aria-checked checked
                             :data-value value
                             :on-key-down on-key-down
                             :on-click on-click}
     [:span {:class (stl/css :check)}
      (when ^boolean checked
        [:> icon* {:icon-id i/tick :size "s"}])]
     (when (some? icon)
       [:span {:class (stl/css :icon)}
        [:> icon* {:icon-id icon :size "s"}]])
     [:span {:class (stl/css :label)} label]
     (when (some? shortcut)
       [:span {:class (stl/css :shortcut)}
        (for [key (scd/split-sc shortcut)]
          [:span {:key key :class (stl/css :key)} key])])]))
