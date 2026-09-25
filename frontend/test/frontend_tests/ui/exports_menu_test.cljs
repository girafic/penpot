;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.exports-menu-test
  (:require
   [app.main.ui.workspace.sidebar.options.menus.exports :as exp]
   [cljs.test :as t :include-macros true]))

(def ^:private board {:id :board :type :frame})
(def ^:private timeline {:board-id :board :tracks {}})

(t/deftest animation-tab-hidden-without-timeline
  (t/is (nil? (exp/animation-export-board :frame [:board] [board] {})))
  (t/is (nil? (exp/animation-export-board :frame [:board] [board] nil))))

(t/deftest animation-tab-shown-when-board-has-timeline
  (t/is (= board (exp/animation-export-board
                  :frame [:board] [board] {:board timeline}))))

(t/deftest animation-tab-hidden-for-non-board-selection
  (t/is (nil? (exp/animation-export-board
               :rect [:board] [board] {:board timeline})))
  (t/is (nil? (exp/animation-export-board
               :frame [:a :b] [board board] {:board timeline}))))
