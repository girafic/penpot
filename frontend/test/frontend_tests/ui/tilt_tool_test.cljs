;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.tilt-tool-test
  (:require
   [app.main.ui.workspace.sidebar.options.menus.tilt-tool :as tilt-tool]
   [app.main.ui.workspace.viewport.tilt-handles :as tilt-handles]
   [cljs.test :as t :include-macros true]))

(t/deftest a-drag-tilts-as-if-the-card-were-grabbed
  (t/testing "across to the right turns its right edge back, down brings its top forward"
    (t/is (= [-24 36] (tilt-tool/drag-tilt [0 0] 60 40 false)))
    (t/is (= [10 50] (tilt-tool/drag-tilt [10 50] 0 0 false)) "from where it was"))
  (t/testing "to a tenth of a degree"
    (t/is (= [0 0.6] (tilt-tool/drag-tilt [0 0] 1 0 false))))
  (t/testing "with Shift in steps of 15°"
    (t/is (= [-30 45] (tilt-tool/drag-tilt [0 0] 70 40 true)))))

(t/deftest locked-layers-and-boards-show-no-3d-handles
  (let [board {:id :board}]
    (t/is (= #{:tilt :vanishing-point} (tilt-handles/shown-handles {:id :card} board)))
    (t/is (= #{:vanishing-point} (tilt-handles/shown-handles {:id :card :blocked true} board))
          "a locked layer keeps its turn")
    (t/is (= #{:tilt} (tilt-handles/shown-handles {:id :card} (assoc board :blocked true)))
          "a locked board keeps its vanishing point")
    (t/is (= #{:tilt} (tilt-handles/shown-handles {:id :card} nil)) "no board that sees it in 3D")))
