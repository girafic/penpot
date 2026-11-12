;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.context-menu-a11y-test
  "A menu beside what opened it stays on the screen: as far as it passes
  its right edge, less a margin, it moves left (as the menu of a font on
  a phone)."
  (:require
   [app.main.ui.components.context-menu-a11y :as cm]
   [cljs.test :as t :include-macros true]))

(t/deftest a-menu-past-the-right-edge-moves-left
  (t/testing "on the screen it stays where it is"
    (t/is (= 0 (cm/offset-on-screen 0 300 375)))
    (t/is (= -20 (cm/offset-on-screen -20 367 375)) "up to the margin"))
  (t/testing "past the edge less the margin it moves left by as much"
    (t/is (= -51 (cm/offset-on-screen 0 418 375)))
    (t/is (= -61 (cm/offset-on-screen -10 418 375)) "from where it was moved before")))
