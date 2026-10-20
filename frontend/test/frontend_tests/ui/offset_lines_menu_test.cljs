;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.offset-lines-menu-test
  "The menu of offset lines (Penpot Motion): the line its + puts an accent
  on."
  (:require
   [app.main.ui.workspace.sidebar.options.menus.offset-lines :as menu]
   [cljs.test :as t :include-macros true]))

(t/deftest a-new-accent-goes-on-a-line-without-one
  (let [free-line #'menu/free-line]
    (t/is (= 3 (free-line 4 [])) "the middle one first")
    (t/is (= 2 (free-line 5 [{:line 3}])) "then the nearest free one, inner first")
    (t/is (= 4 (free-line 5 [{:line 3} {:line 2}])))
    (t/is (nil? (free-line 2 [{:line 1} {:line 2}])) "none when each has one")))
