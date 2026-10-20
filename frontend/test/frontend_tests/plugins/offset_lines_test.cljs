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
   [app.common.uuid :as uuid]
   [app.main.data.workspace.offset-lines :as dwol]
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
            (t/is (thrown? js/Error (set! (.-offsetLines group) #js {:outerColor "#abc"})) "six digits")
            (t/is (thrown? js/Error (.makeOffsetLines path #js {:spacing -1})))
            (t/is (thrown? js/Error (.makeOffsetLines (.createRectangle context)))
                  "only a path makes them")
            (t/is (= 5 (lines group))))

          (t/testing "a group without them has none"
            (let [^js other (.group context #js [(.createRectangle context)])]
              (t/is (nil? (.-offsetLines other)))
              (t/is (thrown? js/Error (set! (.-offsetLines other) #js {:count 3}))))))))))

(defn- offset-lines-of-a-rect
  "A store, the plugin context and a group of 3 offset lines of a closed
  path: `[store context group path]`."
  []
  (let [store       (ths/setup-store (cthf/sample-file :file1 :page-label :page1))
        ^js context (api/create-context plugin-id)
        _           (set! st/state store)
        _           (ptk/emit! store #(assoc-in % [:plugins :flags plugin-id :throw-validation-errors] true))
        ^js path    (.createPath context)]
    (set! (.-d path) "M0 0 L100 0 L100 50 L0 50 Z")
    (set! (.-strokes path) #js [#js {:strokeColor "#336699" :strokeOpacity 1 :strokeWidth 2}])
    [store context (.makeOffsetLines path #js {:count 3 :spacing 10}) path]))

(defn- roles
  "The roles of the layers of `group` in the live store (see `gol/layers-of`)."
  [store context ^js group]
  (let [objects (get-in @store [:files (aget (. context -currentFile) "$id") :data :pages-index
                                (aget (. context -currentPage) "$id") :objects])]
    (mapv #(get-in objects [% :offset-line] :none) (:shapes (stored store context group)))))

(t/deftest offset-lines-are-made-again-from-their-own-source
  (thw/with-wasm-mocks*
    (fn []
      (let [[store context ^js group ^js path] (offset-lines-of-a-rect)]
        (t/is (= [:source :line :line :line] (roles store context group)) "each layer says what it is")

        (t/testing "a copy makes them from its own source"
          (let [^js copy (.clone group)]
            (set! (.-offsetLines copy) #js {:count 5})
            (t/is (= [:source :line :line :line :line :line] (roles store context copy)))
            (t/is (not= (.-id path) (.-id (.-source (.-offsetLines copy)))) "not from the source of the first")
            (t/is (true? (:hidden (stored store context (.-source (.-offsetLines copy))))) "kept hidden")
            (t/is (= [:source :line :line :line] (roles store context group)) "the first stays as it was")))

        (t/testing "what else the group holds stays"
          (let [^js rect (.createRectangle context)]
            (.appendChild group rect)
            (set! (.-offsetLines group) #js {:count 2})
            (t/is (some #{(aget rect "$id")} (:shapes (stored store context group))))
            (t/is (= 2 (count (filter #{:line} (roles store context group)))))))

        (t/testing "without its source they cannot be made again"
          (.remove path)
          (t/is (nil? (.-source (.-offsetLines group))))
          (t/is (thrown? js/Error (set! (.-offsetLines group) #js {:count 4}))))))))

(t/deftest a-copy-of-a-component-takes-its-offset-lines-from-its-main
  (let [group   {:id (uuid/next) :shapes [:src] :offset-lines {:count 2}}
        objects {:src {:id :src :offset-line :source}}]
    (t/is (true? (dwol/editable? objects group)))
    (t/is (false? (dwol/editable? objects (assoc group :shape-ref (uuid/next)))) "a copy")
    (t/is (false? (dwol/editable? {} group)) "no source")))

(t/deftest offset-lines-take-stripes-and-accents
  (thw/with-wasm-mocks*
    (fn []
      (let [[store ^js context ^js group] (offset-lines-of-a-rect)
            line-color #(-> (some (fn [^js l] (when (= (str "Line " %) (.-name l)) l)) (.-children group))
                            (.-strokes) (aget 0) (.-strokeColor))]
        (set! (.-offsetLines group) #js {:stripeEvery 2 :stripeDim 0.5
                                         :accents #js [#js {:line 3 :color "#FF7A7A" :width 1}]})
        (let [^js spec (.-offsetLines group)]
          (t/is (= [2 0.5] [(.-stripeEvery spec) (.-stripeDim spec)]))
          (t/is (= {:line 3 :color "#ff7a7a" :width 1} (js->clj (aget (.-accents spec) 0) :keywordize-keys true))))
        (t/is (= "#ff7a7a" (line-color 3)) "the accent on its line")
        (t/is (= (:offset-lines (stored store context group))
                 (merge (:offset-lines (stored store context group)) {:stripe-every 2 :stripe-dim 0.5})))
        (set! (.-offsetLines group) #js {:stripeEvery 0 :accents #js []})
        (t/is (nil? (.-stripeEvery (.-offsetLines group))) "none again")
        (t/is (= 0 (.-length (.-accents (.-offsetLines group)))))
        (t/is (not-any? #(contains? (:offset-lines (stored store context group)) %) [:stripe-every :accents])
              "as absence")
        (t/testing "made with them"
          (let [^js path  (.createPath context)
                _         (set! (.-d path) "M0 0 L100 0 L100 50 L0 50 Z")
                ^js other (.makeOffsetLines path #js {:count 3 :spacing 10 :stripeEvery 2
                                                      :accents #js [#js {:line 1 :color "#00FF9C"}]})]
            (t/is (= {:stripe-every 2 :accents [{:line 1 :color "#00ff9c"}]}
                     (select-keys (:offset-lines (stored store context other)) [:stripe-every :accents])))))
        (t/testing "what is not valid is refused"
          (t/is (thrown? js/Error (set! (.-offsetLines group) #js {:stripeEvery 1})))
          (t/is (thrown? js/Error (set! (.-offsetLines group) #js {:accents #js [#js {:line 0 :color "#ffffff"}]})))
          (t/is (thrown? js/Error (set! (.-offsetLines group) #js {:accents "red"}))))))))

