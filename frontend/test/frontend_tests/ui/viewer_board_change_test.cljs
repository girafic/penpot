;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.viewer-board-change-test
  "What the view mode works out as the board changes (Penpot Motion): a
  phone stopped there for more than a second, and the animation the
  board comes in with skipped its start."
  (:require
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.ui.viewer.thumbnails :refer [thumbnails-panel*]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(def ^:private board
  (merge (cts/setup-shape {:type :frame :x 0 :y 0 :width 100 :height 100})
         {:id (uuid/custom 30 1) :name "Board A" :parent-id uuid/zero :frame-id uuid/zero}))

(def ^:private page
  {:id (uuid/custom 30 2) :objects {(:id board) board}})

(defn- wait
  "A promise resolved once the boards of the panel had the time to draw
  (each waits for an idle moment, 250 ms under node) and React rendered
  them."
  []
  (js/Promise.resolve (thd/act (fn [] (js/Promise. (fn [resolve] (js/setTimeout resolve 400)))))))

(defn- boards-drawn
  "How many boards the panel drew, hidden, shown, then hidden again."
  []
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [seen  (atom [])
            step! (fn [show]
                    (thd/render! root (mf/element thumbnails-panel* #js {:frames [board] :page page
                                                                         :index 0 :show show}))
                    (.then (wait)
                           (fn [_]
                             (swap! seen conj (.-length (.querySelectorAll document "[title=\"Board A\"]"))))))]
        (-> (step! false)
            (.then (fn [_] (step! true)))
            (.then (fn [_] (step! false)))
            (.then (fn [_] @seen)))))))

(t/deftest the-boards-draw-once-the-panel-shows
  (t/async
    done
    (-> (boards-drawn)
        (.then (fn [seen]
                 (t/is (= [0 1 1] seen) "none while hidden; each once shown, kept as it hides again")))
        (.catch (fn [cause] (t/is false (ex-message cause))))
        (.finally (fn [] (done) nil)))))
