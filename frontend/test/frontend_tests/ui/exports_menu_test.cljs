;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.exports-menu-test
  (:require
   [app.main.ui.workspace.sidebar.options.menus.export-issues :as issues]
   [app.main.ui.workspace.sidebar.options.menus.exports :as exp]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(def ^:private board {:id :board :type :frame})
(def ^:private timeline {:board-id :board :tracks {}})

(t/deftest animation-tab-hidden-without-timeline
  (t/is (nil? (exp/animation-export-boards :frame [board] {})))
  (t/is (nil? (exp/animation-export-boards :frame [board] nil))))

(t/deftest animation-tab-shown-when-board-has-timeline
  (t/is (= [board] (exp/animation-export-boards :frame [board] {:board timeline}))))

(t/deftest animation-tab-shown-for-several-boards
  (let [other {:id :other :type :frame}
        still {:id :still :type :frame}]
    (t/is (= [board other]
             (exp/animation-export-boards :multiple [board still other]
                                          {:board timeline :other (assoc timeline :board-id :other)}))
          "the animated ones")))

(t/deftest animation-tab-hidden-for-non-board-selection
  (t/is (nil? (exp/animation-export-boards :rect [board] {:board timeline})))
  (t/is (nil? (exp/animation-export-boards :multiple [board {:id :rect :type :rect}] {:board timeline}))
        "not with other shapes"))

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

(t/deftest the-animated-size-offers-its-own-size
  (let [labels #(mapv :label (exp/animated-size-options %))]
    (t/is (= ["0.5x" "1x" "2x" "1920w" "720h" "1080h" "1440h" "2160h"] (labels "1")))
    (t/is (= "3.5x" (last (labels "3.5"))) "a typed one after the presets")
    (t/is (= "512w" (last (labels "w512"))))
    (t/is (= 8 (count (labels "nonsense"))))))

(t/deftest tab-goes-round-the-buttons-of-the-issues
  (let [buttons [:close :more]]
    (t/is (= :close (issues/next-focus buttons :dialog false)) "from the dialog, the first")
    (t/is (= :more (issues/next-focus buttons :dialog true)) "or the last, going back")
    (t/is (= :more (issues/next-focus buttons :close false)))
    (t/is (= :close (issues/next-focus buttons :more false)) "round")
    (t/is (= :more (issues/next-focus buttons :close true)) "round, going back")
    (t/is (nil? (issues/next-focus [] :dialog false)) "none")))

(t/deftest the-issues-keep-the-focus-where-it-is
  ;; The list takes the focus once as it opens, keeps it where the user
  ;; put it as the issues are worked out again, and gives it back to its
  ;; pill as it closes.
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [render! #(thd/render! root (mf/element issues/export-issues* #js {:kinds % :formatLabel "SVG"}))
            pill    #(.querySelector document "button[aria-haspopup]")
            popover #(.querySelector document "[role=dialog]")]
        (render! [:no-audio :blend-mode])
        (thd/click! window (pill))
        (t/is (some? (popover)) "it opens")
        (t/is (= (popover) (.-activeElement document)) "with the focus")
        (let [close (.querySelector (popover) "button")]
          (.focus close)
          (render! [:no-audio])
          (t/is (= close (.-activeElement document)) "the issues again, the focus stays"))
        (thd/key-down! window (popover) "Escape")
        (t/is (nil? (popover)) "Esc closes it")
        (t/is (= (pill) (.-activeElement document)) "the focus back on its pill")))))
