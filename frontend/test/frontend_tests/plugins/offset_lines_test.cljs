;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.plugins.offset-lines-test
  "The offset lines of the Plugin API (Penpot Motion, see
  `app.common.geom.offset-lines`)."
  (:require
   [app.common.test-helpers.files :as cthf]
   [app.main.store :as st]
   [app.plugins.api :as api]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]
   [potok.v2.core :as ptk]))

(def ^:private plugin-id "00000000-0000-0000-0000-000000000000")

(defn- stored
  "The shape of `proxy` in the live store."
  [store ^js context ^js proxy]
  (let [file-id (aget (. context -currentFile) "$id")
        page-id (aget (. context -currentPage) "$id")]
    (get-in @store [:files file-id :data :pages-index page-id :objects (aget proxy "$id")])))

(t/deftest a-path-makes-offset-lines
  (thw/with-wasm-mocks*
    (fn []
      (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
            ^js context (api/create-context plugin-id)
            _           (set! st/state store)
            _           (ptk/emit! store #(assoc-in % [:plugins :flags plugin-id :throw-validation-errors] true))
            ^js path    (.createPath context)
            lines       #(-> (stored store context %) :shapes count dec)]
        (set! (.-d path) "M0 0 L100 0 L100 50 L0 50 Z")
        (set! (.-strokes path) #js [#js {:strokeColor "#336699" :strokeOpacity 1 :strokeWidth 2}])
        (let [^js group (.makeOffsetLines path #js {:count 3 :spacing 10})
              ^js spec  (.-offsetLines group)]
          (t/is (= "group" (.-type group)))
          (t/is (= [3 10 "#336699" "#ffffff"] [(.-count spec) (.-spacing spec) (.-innerColor spec) (.-outerColor spec)])
                "the colour of its stroke inside, white outside")
          (t/is (= (.-id path) (.-id (.-source spec))))
          (t/is (= 3 (lines group)) "beside its source")
          (t/is (true? (:hidden (stored store context path))) "which it hides")

          (t/testing "they are made again as they change"
            (set! (.-offsetLines group) #js {:count 5 :outerColor "#FFD27A"})
            (t/is (= 5 (lines group)))
            (t/is (= ["#ffd27a" 10] [(.-outerColor (.-offsetLines group)) (.-spacing (.-offsetLines group))])))

          (t/testing "what is not valid is refused"
            (t/is (thrown? js/Error (set! (.-offsetLines group) #js {:count 0})))
            (t/is (thrown? js/Error (set! (.-offsetLines group) #js {:innerColor "red"})))
            (t/is (thrown? js/Error (.makeOffsetLines path #js {:spacing -1})))
            (t/is (thrown? js/Error (.makeOffsetLines (.createRectangle context)))
                  "only a path makes them")
            (t/is (= 5 (lines group))))

          (t/testing "a group without them has none"
            (let [^js other (.group context #js [(.createRectangle context)])]
              (t/is (nil? (.-offsetLines other)))
              (t/is (thrown? js/Error (set! (.-offsetLines other) #js {:count 3}))))))))))
