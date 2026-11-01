;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.device-menu-test
  "The menu of the devices in the header of the view mode, and the
  button beside it that turns a phone or a tablet."
  (:require
   [app.main.ui.viewer.device-menu :refer [device-menu*]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(t/deftest a-phone-or-a-tablet-turns-from-the-header
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [render!  #(thd/render! root (mf/element device-menu*
                                                    #js {:device {:choice "iphone-island" :finish "dark"}
                                                         :turns %}))
            buttons  #(.querySelectorAll document "button")]
        (render! false)
        (t/is (= 1 (.-length (buttons))) "the menu alone")
        (render! true)
        (t/is (= 2 (.-length (buttons))))
        (let [named (.querySelector document "[aria-label]")]
          (t/is (some? named) "named by its tooltip")
          (t/is (.contains named (aget (buttons) 1))))))))
