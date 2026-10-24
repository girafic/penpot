;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.easing-menu-test
  "The entry of the easing menu of preset animations and style entries
  (`easing/easing-entry`), the menu of the range of a link, and the
  editor of a custom curve."
  (:require
   [app.common.types.animation :as cta]
   [app.main.ui.workspace.timeline.easing :as easing]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(t/deftest the-menu-names-an-easing-by-its-curve-or-spring
  (t/testing "the easing of the type and direction"
    (t/is (= :auto (easing/easing-entry nil))))
  (t/testing "named curves, as keywords or as their curves"
    (t/is (= :ease (easing/easing-entry :ease)))
    (t/is (= :linear (easing/easing-entry {:type :bezier :curve [0.0 0.0 1.0 1.0]})))
    (t/is (= :ease-out-back (easing/easing-entry cta/overshoot-easing))
          "the overshoot of the old menu is the ease out back"))
  (t/testing "named springs"
    (t/is (= [:gentle :quick :bouncy :slow]
             (map (comp easing/easing-entry :easing) cta/spring-presets))))
  (t/testing "what no name has is custom"
    (t/is (= :custom-bezier (easing/easing-entry {:type :bezier :curve [0.2 0.7 0.3 1.0]})))
    (t/is (= :custom-spring (easing/easing-entry {:type :spring :stiffness 250 :damping 12 :mass 1})))))

(defn- errors-of
  "The messages of the errors `f` raises in the DOM of `window`, as the
  page would report them."
  [window f]
  (let [errors   (atom [])
        on-error (fn [^js event]
                   (.preventDefault event)
                   (swap! errors conj (.-message event)))]
    (.addEventListener window "error" on-error)
    (try
      (f)
      (finally
        (.removeEventListener window "error" on-error)))
    @errors))

(t/deftest a-handle-of-the-curve-drags-without-callbacks-for-the-drag
  ;; the range of a link once passed none (see `link-item*`)
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [proto (.. window -Element -prototype)]
        ;; jsdom captures no pointer
        (unchecked-set proto "setPointerCapture" (fn [_] nil))
        (unchecked-set proto "releasePointerCapture" (fn [_] nil))
        (thd/render! root (mf/element easing/easing-curve-editor*
                                      #js {:easing {:type :bezier :curve [0.25 0.1 0.25 1.0]}
                                           :onChange (fn [_] nil)}))
        (let [handle  (.querySelector document "[data-handle=p1]")
              pointer #(thd/act (fn [] (.dispatchEvent handle (new (.-MouseEvent window) % #js {:bubbles true}))))]
          (t/is (some? handle))
          (t/is (empty? (errors-of window #(do (pointer "pointerdown") (pointer "pointerup"))))
                "pressed and let go"))))))

(t/deftest the-menu-of-a-link-has-no-auto
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (thd/render! root (mf/element easing/easing-menu* #js {:easing :linear
                                                             :withoutAuto true
                                                             :onChange (fn [_] nil)}))
      (thd/click! window (.querySelector document "button[aria-haspopup=menu]"))
      (let [entries (mapv #(.. % -dataset -entry) (.querySelectorAll document "[data-entry]"))]
        (t/is (= "linear" (first entries)) "linear first")
        (t/is (not-any? #{"auto"} entries))
        (t/is (some #{"bouncy"} entries) "the springs too")))))
