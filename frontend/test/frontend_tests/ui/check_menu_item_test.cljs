;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.check-menu-item-test
  "The rows with a tick of the menus of the play button and of the
  preview of the prototype, as the keyboard reaches them."
  (:require
   [app.main.ui.components.check-menu-item :refer [check-menu-item*]]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu*]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(mf/defc zoom-menu*
  {::mf/private true}
  [{:keys [zoom on-pick]}]
  [:> dropdown-menu* {:show true :on-close (fn [] nil)}
   [:> check-menu-item* {:label "Fit" :value "fit" :radio true :checked (= zoom "fit") :on-click on-pick}]
   [:> check-menu-item* {:label "Fill" :value "fill" :radio true :checked (= zoom "fill") :on-click on-pick}]
   [:li {:role "separator"}]
   [:> check-menu-item* {:label "Follow the selection" :checked true :on-click on-pick}]
   [:> check-menu-item* {:label "Resize window to 100%" :on-click on-pick}]])

(t/deftest the-rows-say-what-they-are
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element zoom-menu* #js {:zoom "fit" :onPick (fn [_])}))
      (let [rows (vec (.querySelectorAll document "li[role^=menuitem]"))
            role #(.getAttribute % "role")
            checked #(.getAttribute % "aria-checked")]
        (t/is (= ["menuitemradio" "menuitemradio" "menuitemcheckbox" "menuitem"] (mapv role rows)))
        (t/is (= ["true" "false" "true" nil] (mapv checked rows)))
        (t/is (every? #(= "0" (.getAttribute % "tabindex")) rows) "each takes the focus")))))

(t/deftest the-keyboard-picks-a-row
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [picked (atom [])]
        (thd/render! root (mf/element zoom-menu* #js {:zoom "fit"
                                                      :onPick (fn [event]
                                                                (swap! picked conj (.. event -currentTarget -dataset -value)))}))
        (thd/key-down! window document "ArrowDown")
        (t/is (= "Fit" (.-textContent (.-activeElement document))) "the first row")
        (thd/key-down! window document "ArrowDown")
        (let [reached (atom 0)
              count!  (fn [^js event] (when (= "Enter" (.-key event)) (swap! reached inc)))]
          (.addEventListener document "keydown" count!)
          (thd/key-down! window (.-activeElement document) "Enter")
          (.removeEventListener document "keydown" count!)
          (t/is (= ["fill"] @picked) "the second, picked by Enter")
          (t/is (zero? @reached) "the shortcuts of the workspace do not see it"))))))
