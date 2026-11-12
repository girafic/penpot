;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.inspect-exports-test
  "Which boards the view mode exports as an animation (Penpot Motion)."
  (:require
   [app.main.ui.inspect.exports :as ie]
   [cljs.test :as t :include-macros true]))

(def ^:private page
  {:timelines {:board {:board-id :board :duration 1000}}})

(t/deftest the-boards-with-a-timeline
  (let [board {:id :board :type :frame}
        still {:id :still :type :frame}]
    (t/is (= [board] (ie/animated-boards [board] page)))
    (t/is (= [board] (ie/animated-boards [board still] page)) "those with one, of several")
    (t/is (nil? (ie/animated-boards [still] page)) "none without")
    (t/is (nil? (ie/animated-boards [board {:id :rect :type :rect}] page)) "only when only boards are selected")
    (t/is (nil? (ie/animated-boards [] page)))))
