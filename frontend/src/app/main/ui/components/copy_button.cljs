;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.components.copy-button
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.event :as-alias ev]
   [app.main.data.notifications :as ntf]
   [app.main.store :as st]
   [app.main.ui.ds.foundations.assets.icon :refer [icon*] :as i]
   [app.util.clipboard :as clipboard]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.timers :as tm]
   [rumext.v2 :as mf]))

(defn copy-text!
  "Copy `text`: a clipboard that copies not tells why as a toast, instead
  of the error of the app (a page over plain HTTP, see
  `clipboard/to-clipboard`)."
  [text]
  (-> (clipboard/to-clipboard text)
      (.catch (fn [cause]
                (if-let [message (clipboard/error-message cause)]
                  (st/emit! (ntf/show {:content message
                                       :type :toast
                                       :level :warning
                                       :timeout 5000}))
                  (throw cause))))))

(mf/defc copy-button*
  [{:keys [data on-copied children class aria-label]}]
  (let [active* (mf/use-state false)
        active? (deref active*)

        class   (dm/str class " "
                        (stl/css-case
                         :copy-button  (not (some? children))
                         :copy-wrapper (some? children)))

        on-click
        (mf/use-fn
         (mf/deps data)
         (fn [event]
           (when-not (dom/get-boolean-data event "active")
             (reset! active* true)
             (tm/schedule 1000 #(reset! active* false))
             (when (fn? on-copied) (on-copied event))
             (copy-text! (if (fn? data) (data) data)))))]

    [:button {:class class
              :aria-label (or aria-label (tr "labels.copy"))
              :data-active (dm/str active?)
              :on-click on-click}
     children
     [:span {:class (stl/css :icon-btn)}
      [:> icon* {:icon-id (if active? i/tick i/clipboard)
                 :size "s"
                 :class (stl/css :icon)}]]]))
