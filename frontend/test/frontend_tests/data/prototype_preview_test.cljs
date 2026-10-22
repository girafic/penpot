;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.prototype-preview-test
  "The preview of the prototype in the workspace: the board it starts
  at, and the history of its screens in the view mode."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.viewer :as dv]
   [app.main.data.workspace.prototype-preview :as dwpp]
   [app.main.router :as rt]
   [cljs.test :as t]
   [potok.v2.core :as ptk]))

(def ^:private board-a (uuid/custom 2 1))
(def ^:private board-b (uuid/custom 2 2))
(def ^:private inner (uuid/custom 2 3))
(def ^:private rect (uuid/custom 2 4))
(def ^:private loose (uuid/custom 2 5))

(def ^:private objects
  {uuid/zero {:id uuid/zero :type :frame :frame-id uuid/zero :parent-id uuid/zero
              :shapes [board-a board-b loose]}
   board-a   {:id board-a :type :frame :frame-id uuid/zero :parent-id uuid/zero :shapes [inner]}
   inner     {:id inner :type :frame :frame-id board-a :parent-id board-a :shapes [rect]}
   rect      {:id rect :type :rect :frame-id inner :parent-id inner}
   board-b   {:id board-b :type :frame :frame-id uuid/zero :parent-id uuid/zero :shapes []}
   loose     {:id loose :type :rect :frame-id uuid/zero :parent-id uuid/zero}})

(t/deftest the-preview-starts-at-the-board-of-the-selection
  (t/is (= board-b (dwpp/start-frame-id objects [board-b])))
  ;; a shape deep in a board: the board of the page it is in
  (t/is (= board-a (dwpp/start-frame-id objects [rect])))
  (t/is (= board-a (dwpp/start-frame-id objects [inner])))
  ;; the first selected shape in a board
  (t/is (= board-b (dwpp/start-frame-id objects [loose board-b])))
  ;; nothing in a board: where the view mode starts
  (t/is (nil? (dwpp/start-frame-id objects [loose])))
  (t/is (nil? (dwpp/start-frame-id objects []))))

(defn- screens
  "The state after the preview showed the screens `indexes`, one after
  the other."
  [state indexes]
  (reduce (fn [state index] (ptk/update (dv/preview-screen index) state)) state indexes))

(t/deftest the-preview-keeps-its-screens-in-a-history-of-its-own
  (let [started (ptk/update (dv/start-preview) {})]
    (t/testing "its screens replace each other in the history of the browser"
      (t/is (true? (::rt/replace-only started)))
      (t/is (dv/preview? started))
      (t/is (not (dv/preview? {}))))

    (t/testing "the screen it leaves goes to its history"
      (let [state (screens started [0 2 2 5])]
        (t/is (= {:back [0 2] :forward [] :index 5}
                 (:viewer-preview state)))))

    (t/testing "back goes to the screen before, which then shows without
                adding to the history, and forth again"
      (let [state (screens started [0 2 5])
            back  (ptk/update (dv/preview-step :back) state)
            _     (t/is (= 2 (get-in back [:viewer-preview :going])))
            back  (screens back [2])
            _     (t/is (= {:back [0] :forward [5] :index 2} (:viewer-preview back)))
            fwd   (screens (ptk/update (dv/preview-step :forward) back) [5])]
        (t/is (= {:back [0 2] :forward [] :index 5} (:viewer-preview fwd)))))

    (t/testing "a new screen after going back drops the screens ahead"
      (let [state (-> (screens started [0 2 5])
                      (as-> s (ptk/update (dv/preview-step :back) s))
                      (screens [2 7]))]
        (t/is (= {:back [0 2] :forward [] :index 7} (:viewer-preview state)))))

    (t/testing "nothing before the first screen"
      (let [state (screens started [0])]
        (t/is (= state (ptk/update (dv/preview-step :back) state)))))

    (t/testing "the view mode outside a preview keeps no history"
      (t/is (= {} (screens {} [0 2]))))))
