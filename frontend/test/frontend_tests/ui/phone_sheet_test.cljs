;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.phone-sheet-test
  "The view mode on a phone, its board alone: two fingers held on it open
  the sheet with what its bars hold elsewhere (Penpot Motion)."
  (:require
   [app.main.ui.viewer.phone-sheet :as phone-sheet]
   [app.util.storage :as storage]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(t/deftest two-fingers-hold-still
  (let [start {1 [100 100] 2 [200 300]}]
    (t/is (true? (phone-sheet/held-still? start {1 [104 103] 2 [200 300]})) "a finger trembles")
    (t/is (false? (phone-sheet/held-still? start {1 [100 130] 2 [200 300]})) "it scrolls or pinches")
    (t/is (false? (phone-sheet/held-still? start {1 [100 100]})) "one lifted")
    (t/is (false? (phone-sheet/held-still? start {1 [100 100] 3 [200 300]})) "another came down")))

(mf/defc holder*
  {::mf/private true}
  [{:keys [on-hold]}]
  (phone-sheet/use-two-finger-hold! true on-hold)
  [:div])

(defn- touch!
  "The fingers `points` (`[[x y]...]`) `type` on `window`."
  [^js window type points]
  (let [event   (new (.-Event window) type #js {:bubbles true})
        touches (into-array (map-indexed (fn [i [x y]] #js {:identifier i :clientX x :clientY y}) points))]
    (set! (.-touches event) touches)
    (set! (.-changedTouches event) touches)
    (.dispatchEvent window event)))

(defn- held
  "Whether the sheet opened as two fingers come down and do `then`."
  [then]
  (thd/with-dom
    (fn [{:keys [root window]}]
      (let [held? (atom false)]
        (thd/render! root (mf/element holder* #js {:onHold #(reset! held? true)}))
        (touch! window "touchstart" [[100 100] [200 300]])
        (then window)
        (js/Promise. (fn [resolve]
                       (js/setTimeout #(resolve @held?) (+ phone-sheet/hold-ms 100))))))))

(t/deftest two-fingers-held-open-the-sheet
  (t/async
    done
    ;; one after the other: each takes the DOM globals while it runs
    (-> (held (fn [_]))
        (.then (fn [still]
                 (t/is (true? still) "held still")
                 (held #(touch! % "touchmove" [[100 140] [200 300]]))))
        (.then (fn [moved]
                 (t/is (false? moved) "a finger moved: it scrolls or pinches")
                 (held #(touch! % "touchend" [[100 100]]))))
        (.then (fn [lifted]
                 (t/is (false? lifted) "one lifted before")))
        (.catch (fn [cause] (t/is false (ex-message cause))))
        (.finally (fn [] (done) nil)))))

(t/deftest the-hint-shows-once
  (swap! storage/user dissoc ::phone-sheet/hold-hint)
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (thd/render! root (mf/element phone-sheet/hold-hint* #js {}))
      (let [hint (.querySelector document "[role=status]")]
        (t/is (some? hint) "the first time")
        (thd/click! window hint)
        (t/is (nil? (.querySelector document "[role=status]")) "gone at a tap"))))
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element phone-sheet/hold-hint* #js {}))
      (t/is (nil? (.querySelector document "[role=status]")) "not again"))))
