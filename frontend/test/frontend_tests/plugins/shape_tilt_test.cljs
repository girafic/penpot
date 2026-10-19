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

(t/deftest the-depth-and-the-3d-space-read-and-write
  (thw/with-wasm-mocks*
    (fn []
      (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
            ^js context (api/create-context plugin-id)
            _           (set! st/state store)
            ^js board   (.createBoard context)
            ^js rect    (.createRectangle context)]
        (.appendChild board rect)
        (t/is (= 0 (.-translateZ rect)) "in the plane")
        (set! (.-translateZ rect) 40)
        (t/is (= 40 (:translate-z (stored store context rect))))
        (set! (.-translateZ rect) 0)
        (t/is (not (contains? (stored store context rect) :translate-z)))
        (t/is (false? (.-preserve3d board)))
        (set! (.-preserve3d board) true)
        (t/is (true? (:preserve-depth (stored store context board))))
        (set! (.-preserve3d board) false)
        (t/is (not (contains? (stored store context board) :preserve-depth)))
        (t/is (undefined? (.-preserve3d rect)) "groups and boards only")))))

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

(t/deftest a-board-can-see-without-perspective
  (thw/with-wasm-mocks*
    (fn []
      (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
            ^js context (api/create-context plugin-id)
            _           (set! st/state store)
            ^js board   (.createBoard context)]
        (t/is (false? (.-orthographic board)) "with perspective by default")
        (set! (.-orthographic board) true)
        (t/is (true? (.-orthographic board)))
        (t/is (true? (:orthographic (stored store context board))))
        (set! (.-orthographic board) "yes")
        (t/is (true? (.-orthographic board)) "not a boolean: left as it is")
        (set! (.-orthographic board) false)
        (t/is (not (contains? (stored store context board) :orthographic)) "back to perspective")))))

(t/deftest a-board-lights-its-layers-in-3d
  (thw/with-wasm-mocks*
    (fn []
      (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
            ^js context (api/create-context plugin-id)
            _           (set! st/state store)
            ^js board   (.createBoard context)]
        (t/is (nil? (.-light board)) "no light by default")
        (set! (.-light board) #js {:angle 90})
        (t/is (= {:angle 90 :height 45 :ambient 0.3} (:light (stored store context board)))
              "the rest of the default light")
        (t/is (= 90 (.-angle (.-light board))))
        (set! (.-light board) #js {:angle 0 :height 120 :ambient 0})
        (t/is (= 90 (:angle (:light (stored store context board)))) "too high: left as it is")
        (set! (.-light board) nil)
        (t/is (not (contains? (stored store context board) :light)) "no light")
        (set! (.-depthFade board) 0.4)
        (set! (.-depthBlur board) 2)
        (set! (.-depthFocus board) -100)
        (t/is (= [0.4 2 -100] ((juxt :depth-fade :depth-blur :depth-focus) (stored store context board))))
        (set! (.-depthFade board) 2)
        (t/is (= 0.4 (.-depthFade board)) "more than all of it: left as it is")
        (set! (.-depthBlur board) 0)
        (t/is (not (contains? (stored store context board) :depth-blur)) "none")))))
