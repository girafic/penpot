;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.comments-docked-test
  "A phone shows a new comment at the bottom of its screen, out of the
  board, as wide as it (Penpot Motion)."
  (:require
   [app.main.ui.comments :as cmt]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(defn- form
  "Where the form of a new comment goes as it renders, `docked` or not:
  `[parent-is-body? left-style]`."
  [docked]
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element cmt/comment-floating-thread-draft*
                                    #js {:draft {:position {:x 10 :y 20} :content ""}
                                         :zoom 1
                                         :onCancel (fn [])
                                         :onSubmit (fn [_])
                                         :docked docked}))
      (let [node (.querySelector document "[class*=floating-thread-draft-inner-wrapper]")]
        [(= (.-parentNode node) (.-body document))
         (.. node -style -left)]))))

(t/deftest a-new-comment-docks-on-a-phone
  (let [[in-body? left] (form true)]
    (t/is (true? in-body?) "out of the board")
    (t/is (= "" left) "not placed by its bubble"))
  (let [[in-body? left] (form false)]
    (t/is (false? in-body?) "elsewhere by its bubble")
    (t/is (not= "" left))))
