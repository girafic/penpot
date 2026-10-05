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

(t/deftest the-export-button-follows-the-export
  (t/testing "idle, and again once an export is done"
    (t/is (= {:label ["workspace.options.export.export-format" "MP4"] :action :export :disabled? false}
             (exp/export-button-state nil "MP4" false)))
    (t/is (:disabled? (exp/export-button-state nil "MP4" true))))
  (t/testing "in place"
    (t/is (= {:label ["workspace.options.export.exporting" 3] :action :cancel :disabled? false}
             (exp/export-button-state {:in-progress true :phase :capture :current 3 :total 90 :percent 3}
                                      "MP4" true))
          "how far it is, while it takes the frames")
    (t/is (= {:label ["workspace.options.export.exporting" 85] :action :cancel :disabled? false}
             (exp/export-button-state {:in-progress true :phase :encode :current 2 :total 90 :percent 85}
                                      "MP4" true))
          "then how far it is"))
  (t/testing "in a tab"
    (let [job {:in-progress true :in-tab? true}]
      (t/is (= {:label ["workspace.options.export.opening-tab"] :action :cancel :disabled? false}
               (exp/export-button-state job "MP4" true))
            "until it says hello")
      (t/is (= {:label ["workspace.options.export.in-tab" 45] :action :cancel :disabled? false}
               (exp/export-button-state (assoc job :opened? true :percent 45) "MP4" true)))
      (t/is (:disabled? (exp/export-button-state (assoc job :opened? true :cancelling? true) "MP4" true))
            "no second Cancel")
      (t/is (= ["workspace.options.export.tab-paused"]
               (:note (exp/export-button-state (assoc job :opened? true :paused? true) "MP4" true)))
            "paused while hidden")
      (t/is (= {:label ["labels.cancel"] :action :cancel :disabled? false
                :note ["workspace.options.export.tab-blocked"] :offer? true}
               (exp/export-button-state (assoc job :offer? true) "MP4" true))
            "the offer when it said nothing"))))
