;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.timeline-recording-test
  (:require
   [app.main.ui.workspace.timeline.recording :as rec]
   [app.main.ui.workspace.timeline.recording-options :refer [recording-options*]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(t/deftest the-time-of-a-recording-counts-down-its-pre-roll
  (t/is (= "0:00" (rec/elapsed-label 0)))
  (t/is (= "0:01" (rec/elapsed-label 1500)))
  (t/is (= "1:01" (rec/elapsed-label 61000)))
  (t/is (= "−0:01" (rec/elapsed-label -700)) "the seconds left, up")
  (t/is (= "−0:02" (rec/elapsed-label -1001))))

(t/deftest the-options-of-recording-take-the-keys-at-once
  ;; Focused as they open, so Escape closes them: not the keys of the
  ;; timeline under them.
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [closed (atom 0)]
        (thd/render! root (mf/element recording-options*
                                      #js {:anchor {:top 500 :left 100 :width 20}
                                           :onClose #(swap! closed inc)}))
        (let [dialog (.querySelector document "[role=dialog]")]
          (t/is (some? dialog))
          (t/is (= dialog (.-activeElement document)) "focused")
          (thd/key-down! window dialog "Escape")
          (t/is (= 1 @closed) "Escape closes them"))))))
