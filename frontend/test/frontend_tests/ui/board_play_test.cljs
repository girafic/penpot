;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.board-play-test
  (:require
   [app.main.ui.workspace.viewport.widgets :as widgets]
   [cljs.test :as t :include-macros true]))

(t/deftest a-tap-on-space-plays-the-animated-board-selected
  (let [animated #{:hero :card}]
    (t/is (= :hero (widgets/space-tap-board #{:hero} animated nil)))
    (t/testing "it stops the one playing, whatever is selected"
      (t/is (= :card (widgets/space-tap-board #{:hero} animated :card)))
      (t/is (= :card (widgets/space-tap-board #{} animated :card))))
    (t/testing "nothing to play: Space only pans"
      (t/is (nil? (widgets/space-tap-board #{:rect} animated nil)) "a board without animation")
      (t/is (nil? (widgets/space-tap-board #{:hero :card} animated nil)) "two boards")
      (t/is (nil? (widgets/space-tap-board #{} animated nil))))))
