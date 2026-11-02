;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.device-menu
  "What picks the device the view mode shows its boards in (see
  `app.main.data.viewer.devices`): the rows of a menu, which the header
  of the view mode and the preview of the prototype share, and the menu
  of the header."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.devices :as dvd]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.store :as st]
   [app.main.ui.components.check-menu-item :refer [check-menu-item*]]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.icons :as deprecated-icon]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(defn device-label
  "The name of the device `choice` (one of `dvd/choices`), or of the
  color of a device (one of `dvd/finishes`), in a menu."
  [choice]
  (case choice
    "auto"          (tr "viewer.device.auto")
    "iphone-island" (tr "viewer.device.iphone-island")
    "iphone-notch"  (tr "viewer.device.iphone-notch")
    "iphone-home"   (tr "viewer.device.iphone-home")
    "iphone-duo-closed" (tr "viewer.device.iphone-duo-closed")
    "iphone-duo-open" (tr "viewer.device.iphone-duo-open")
    "android"       (tr "viewer.device.android")
    "ipad"          (tr "viewer.device.ipad")
    "macbook"       (tr "viewer.device.macbook")
    "watch"         (tr "viewer.device.watch")
    "imac"          (tr "viewer.device.imac")
    "browser"       (tr "viewer.device.browser")
    "dark"          (tr "viewer.device.dark")
    "light"         (tr "viewer.device.light")
    (tr "viewer.device.none")))

(mf/defc device-items*
  "The rows of a menu of the devices, `choice` and `finish` ticked: no
  device, automatic, each device, then its colors once there is one.
  `on-choice` and `on-finish` get what a row picks."
  [{:keys [choice finish on-choice on-finish]}]
  (let [pick-choice (mf/use-fn
                     (mf/deps on-choice)
                     (fn [event]
                       (on-choice (dom/get-data (dom/get-current-target event) "value"))))
        pick-finish (mf/use-fn
                     (mf/deps on-finish)
                     (fn [event]
                       (on-finish (dom/get-data (dom/get-current-target event) "value"))))]
    ;; no device and automatic, then the devices
    [:*
     (for [item (take 2 dvd/choices)]
       [:> check-menu-item* {:key item
                             :label (device-label item)
                             :value item
                             :radio true
                             :checked (= choice item)
                             :on-click pick-choice}])
     [:li {:class (stl/css :separator) :role "separator"}]
     (for [item (drop 2 dvd/choices)]
       [:> check-menu-item* {:key item
                             :label (device-label item)
                             :value item
                             :radio true
                             :checked (= choice item)
                             :on-click pick-choice}])
     (when (not= "none" choice)
       [:*
        [:li {:class (stl/css :separator) :role "separator"}]
        (for [item dvd/finishes]
          [:> check-menu-item* {:key item
                                :label (device-label item)
                                :value item
                                :radio true
                                :checked (= finish item)
                                :on-click pick-finish}])])]))

(mf/defc device-menu*
  "The menu of the devices in the header of the view mode, for the
  device picked, `{:choice :finish}`, and, when the device shown `turns`
  (a phone or a tablet), the button that turns it on its side or stands
  it up again (see `dv/turn-device`)."
  [{:keys [device turns]}]
  (let [{:keys [choice finish]} device
        choice (dvd/parse-choice choice)
        finish (dvd/parse-finish finish)
        open*  (mf/use-state false)
        open?  (deref open*)
        ;; the click that opens it does not close it again
        toggle (mf/use-fn
                (fn [event]
                  (dom/stop-propagation event)
                  (swap! open* not)))
        close  (mf/use-fn #(reset! open* false))
        on-choice (mf/use-fn
                   (mf/deps finish)
                   (fn [choice]
                     (reset! open* false)
                     (st/emit! (dv/set-device choice finish))))
        on-finish (mf/use-fn
                   (mf/deps choice)
                   (fn [finish]
                     (reset! open* false)
                     (st/emit! (dv/set-device choice finish))))
        on-turn   (mf/use-fn #(st/emit! dv/turn-device))]
    [:div {:class (stl/css :device-menu)}
     [:button {:type "button"
               :class (stl/css :trigger)
               :aria-haspopup "menu"
               :aria-expanded open?
               :on-click toggle}
      [:span {:class (stl/css :title)} (tr "viewer.header.device")]
      [:span {:class (stl/css :value)} (device-label choice)]
      [:span {:class (stl/css :arrow)} deprecated-icon/arrow]]
     [:> dropdown-menu* {:show open?
                         :on-close close
                         :class (stl/css :menu)}
      [:> device-items* {:choice choice
                         :finish finish
                         :on-choice on-choice
                         :on-finish on-finish}]]
     (when (true? turns)
       [:> icon-button* {:variant "ghost"
                         :icon i/turn-device
                         :aria-label (tr "viewer.device.turn" (sc/get-tooltip :turn-device))
                         :on-click on-turn}])]))
