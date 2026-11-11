;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.component-svg-test
  "The preview of a component, as the card of its library in the
  dashboard draws it: of a root of any shape, and nothing of a root its
  objects lost, without failing the page."
  (:require
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.render :as render]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(defn- drawn
  "Whether the preview of `root-shape` in `objects` drew its svg, once
  its deferred render ran."
  [objects root-shape]
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element render/component-svg {:objects objects :root-shape root-shape}))
      (-> (js/Promise. (fn [resolve] (js/setTimeout resolve 300)))
          (.then (fn [] (thd/settle)))
          (.then (fn [] (some? (.querySelector document "svg"))))))))

(t/deftest a-component-of-any-shape-and-a-lost-root-draw
  (t/async
    done
    (let [rect (cts/setup-shape {:type :rect :x 10 :y 20 :width 30 :height 40})
          lost (cts/setup-shape {:type :frame :x 0 :y 0 :width 30 :height 40})]
      (-> (drawn {(:id rect) rect} rect)
          (.then (fn [ok]
                   (t/is (true? ok) "a rectangle, as it is")
                   ;; its id another than the key it is under
                   (drawn {(uuid/next) lost} lost)))
          (.then (fn [ok]
                   (t/is (true? ok) "a root its objects lost: an empty preview, no failure")))
          (.catch (fn [cause] (t/is false (ex-message cause))))
          (.finally (fn [] (done) nil))))))
