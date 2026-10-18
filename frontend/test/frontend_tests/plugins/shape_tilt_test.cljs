;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.plugins.shape-tilt-test
  "The 3D rotation of the Plugin API (Penpot Motion, see
  `app.common.geom.tilt`)."
  (:require
   [app.common.test-helpers.files :as cthf]
   [app.main.store :as st]
   [app.plugins.api :as api]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]))

(def ^:private plugin-id "00000000-0000-0000-0000-000000000000")

(defn- stored
  "The shape of `proxy` in the live store."
  [store ^js context ^js proxy]
  (let [file-id (aget (. context -currentFile) "$id")
        page-id (aget (. context -currentPage) "$id")]
    (get-in @store [:files file-id :data :pages-index page-id :objects (aget proxy "$id")])))

(t/deftest the-tilt-of-a-shape-reads-and-writes
  (thw/with-wasm-mocks*
    (fn []
      (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
            ^js context (api/create-context plugin-id)
            _           (set! st/state store)
            ^js board   (.createBoard context)
            ^js rect    (.createRectangle context)]
        (.appendChild board rect)
        (set! (.-rotateX rect) 30)
        (set! (.-backfaceHidden rect) true)
        (set! (.-perspective board) 800)
        (t/is (= 30 (.-rotateX rect)))
        (t/is (= 0 (.-rotateY rect)) "flat about Y")
        (t/is (true? (.-backfaceHidden rect)))
        (t/is (= 800 (.-perspective board)))
        (t/is (= 30 (:rotate-x (stored store context rect))))
        (set! (.-rotateX rect) 0)
        (set! (.-backfaceHidden rect) false)
        (set! (.-perspective board) nil)
        (t/is (not (contains? (stored store context rect) :rotate-x)) "flat is no tilt")
        (t/is (not (contains? (stored store context rect) :backface-hidden)))
        (t/is (nil? (.-perspective board)) "the default")))))

(t/deftest the-vanishing-point-of-a-board-reads-and-writes
  (thw/with-wasm-mocks*
    (fn []
      (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
            ^js context (api/create-context plugin-id)
            _           (set! st/state store)
            ^js board   (.createBoard context)]
        (t/is (nil? (.-perspectiveX board)) "its middle by default")
        (set! (.-perspectiveX board) 0.25)
        (set! (.-perspectiveY board) 1.5)
        (t/is (= [0.25 1.5] [(.-perspectiveX board) (.-perspectiveY board)]))
        (t/is (= 0.25 (:perspective-x (stored store context board))))
        (set! (.-perspectiveY board) "middle")
        (t/is (= 1.5 (.-perspectiveY board)) "not a number: left as it is")
        (set! (.-perspectiveX board) nil)
        (t/is (not (contains? (stored store context board) :perspective-x)) "back to its middle")))))
