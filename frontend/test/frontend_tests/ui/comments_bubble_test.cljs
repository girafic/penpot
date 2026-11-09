;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.comments-bubble-test
  "The view mode drags the bubble of a thread one may move, with a mouse
  or a finger; a tap trembling a little opens it still (Penpot Motion)."
  (:require
   [app.common.geom.point :as gpt]
   [app.main.ui.comments :as cmt]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(def ^:private thread
  {:id (random-uuid)
   :seqn 1
   :owner-id (random-uuid)
   :position (gpt/point 10 20)
   :count-unread-comments 0})

(defn- press!
  "Press the bubble at `from`, move to each point of `path` and let go
  there: what `on-move` and `on-click` got, `{:moved [...] :clicked n}`."
  [movable? from path]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [moved   (atom [])
            clicked (atom 0)]
        (thd/render! root (mf/element cmt/comment-floating-bubble*
                                      #js {:thread thread
                                           :zoom 2
                                           :isOpen false
                                           :origin :viewer
                                           :onClick #(swap! clicked inc)
                                           :onMove (when movable?
                                                     #(swap! moved conj %2))}))
        (let [node   (.querySelector document "[class*=floating-preview-avatar]")
              fire!  (fn [type [x y]]
                       (thd/act #(.dispatchEvent node (new (.-MouseEvent window) type
                                                           #js {:bubbles true :clientX x :clientY y}))))
              end    (or (last path) from)]
          ;; jsdom captures no pointer
          (set! (.-setPointerCapture node) (fn [_]))
          (fire! "pointerdown" from)
          (doseq [point path] (fire! "pointermove" point))
          (fire! "pointerup" end)
          (thd/click! window node)
          {:moved @moved :clicked @clicked})))))

(t/deftest a-bubble-one-may-move-follows-the-pointer
  (let [{:keys [moved clicked]} (press! true [100 100] [[110 100] [130 140]])]
    ;; zoomed twice: 30 and 40 px on the screen are 15 and 20 in the board
    (t/is (= [[25 40]] moved) "dropped where it went")
    (t/is (zero? clicked) "the release of a drag is no tap")))

(t/deftest a-tap-trembling-a-little-opens-the-thread
  (let [{:keys [moved clicked]} (press! true [100 100] [[102 101]])]
    (t/is (empty? moved))
    (t/is (= 1 clicked))))

(t/deftest a-bubble-of-another-stays-in-the-view-mode
  (let [{:keys [moved clicked]} (press! false [100 100] [[130 140]])]
    (t/is (empty? moved) "not dragged")
    (t/is (= 1 clicked) "a click opens it")))
