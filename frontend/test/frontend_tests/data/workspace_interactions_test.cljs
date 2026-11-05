;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.data.workspace-interactions-test
  (:require
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.common.types.shape.interactions :as ctsi]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.interactions :as dwi]
   [app.main.ui.workspace.sidebar.options.menus.interactions :as menu]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.state :as ths]))

(t/use-fixtures :each
  {:before cthi/reset-idmap!})

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- make-file
  "A file with two boards (frames)."
  []
  (-> (cthf/sample-file :file1)
      (ctho/add-frame :board1 :name "Board 1")
      (ctho/add-frame :board2 :name "Board 2")))

(defn- navigate-interaction
  "A navigate interaction from `origin-id` to `dest-id` (a flow origin)."
  [origin-id dest-id]
  (-> ctsi/default-interaction
      (ctsi/set-destination dest-id)
      (assoc :position-relative-to origin-id)))

(defn- page-flows
  "The flows map of the current page in `state`."
  [state]
  (let [file-id (:current-file-id state)
        page-id (:current-page-id state)]
    (get-in state [:files file-id :data :pages-index page-id :flows])))

;; ---------------------------------------------------------------------------
;; add-interaction (plugin API path): implicit flow creation must match the
;; editor behavior of creating a flow when an interaction is added outside of
;; any existing flow.
;; ---------------------------------------------------------------------------

(t/deftest add-interaction-creates-implicit-flow-outside-flow
  (t/async
    done
    (let [file        (make-file)
          board1-id   (:id (cths/get-shape file :board1))
          board2-id   (:id (cths/get-shape file :board2))
          page-id     (cthf/current-page-id file)
          interaction (navigate-interaction board1-id board2-id)
          store       (ths/setup-store file)
          events      [(dwi/add-interaction page-id board1-id interaction)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (let [flows (vals (page-flows new-state))]
           (t/is (= 1 (count flows))
                 "an implicit flow is created for the origin board")
           (t/is (= board1-id (:starting-frame (first flows)))
                 "the implicit flow starts at the origin board")))))))

(t/deftest add-interaction-does-not-duplicate-flow-for-same-frame
  (t/async
    done
    (let [file        (make-file)
          board1-id   (:id (cths/get-shape file :board1))
          board2-id   (:id (cths/get-shape file :board2))
          page-id     (cthf/current-page-id file)
          flow-id     (uuid/next)
          ;; board1 is already the starting frame of an explicit flow
          file        (assoc-in file
                                [:data :pages-index page-id :flows flow-id]
                                {:id flow-id
                                 :name "My flow"
                                 :starting-frame board1-id})

          interaction (navigate-interaction board1-id board2-id)
          store       (ths/setup-store file)
          events      [(dwi/add-interaction page-id board1-id interaction)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (let [flows (vals (page-flows new-state))]
           (t/is (= 1 (count flows))
                 "no duplicate flow is created when the frame is already in a flow")
           (t/is (= "My flow" (:name (first flows)))
                 "the existing explicit flow is preserved")))))))

(t/deftest add-interaction-without-destination-does-not-create-flow
  (t/async
    done
    (let [file        (make-file)
          board1-id   (:id (cths/get-shape file :board1))
          page-id     (cthf/current-page-id file)
          ;; An open-url interaction is not a flow origin (no destination)
          interaction (-> ctsi/default-interaction
                          (assoc :action-type :open-url
                                 :url "https://example.com"
                                 :destination nil))
          store       (ths/setup-store file)
          events      [(dwi/add-interaction page-id board1-id interaction)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (t/is (empty? (page-flows new-state))
               "non flow-origin interactions do not create a flow"))))))

;; ---------------------------------------------------------------------------
;; set-scroll-behavior (Penpot Motion): how a layer scrolls in the prototype,
;; as the select of the prototype tab sets and shows it.
;; ---------------------------------------------------------------------------

(t/deftest a-layer-scrolls-with-its-board-fixed-or-sticky
  (t/async
    done
    (let [file     (-> (cthf/sample-file :file1)
                       (ctho/add-frame :board)
                       (cths/add-sample-shape :a :parent-label :board)
                       (cths/add-sample-shape :b :parent-label :board))
          a        (cthi/id :a)
          b        (cthi/id :b)
          objects  #(get-in % [:files (:current-file-id %) :data :pages-index (:current-page-id %) :objects])
          behavior (fn [state & ids] (menu/scroll-behavior (map (objects state) ids)))
          store    (ths/setup-store file)]
      (t/is (= :scroll (menu/scroll-behavior (map #(cths/get-shape file %) [:a :b])))
            "a layer never set scrolls with its board")
      (t/is (nil? (menu/scroll-behavior [(cths/get-shape file :board)])) "a top board is not in a board")
      (t/is (nil? (menu/scroll-behavior (map #(cths/get-shape file %) [:a :board]))))
      (ths/run-store
       store done [(dwi/set-scroll-behavior [a] :sticky)]
       (fn [state]
         (t/is (true? (:sticky-scroll (get (objects state) a))))
         (t/is (= :multiple (behavior state a b))))))))

(t/deftest fixed-and-sticky-exclude-each-other
  (t/async
    done
    (let [file    (-> (cthf/sample-file :file1)
                      (ctho/add-frame :board)
                      (cths/add-sample-shape :a :parent-label :board :sticky-scroll true)
                      (cths/add-sample-shape :b :parent-label :board))
          a       (cthi/id :a)
          b       (cthi/id :b)
          objects #(get-in % [:files (:current-file-id %) :data :pages-index (:current-page-id %) :objects])
          store   (ths/setup-store file)]
      (ths/run-store
       store done [(dwi/set-scroll-behavior [a b] :fixed)]
       (fn [state]
         (t/is (= :fixed (menu/scroll-behavior (map (objects state) [a b]))))
         (t/is (not (contains? (get (objects state) a) :sticky-scroll))))))))

(t/deftest scrolling-with-its-board-takes-both-keys-away
  (t/async
    done
    (let [file    (-> (cthf/sample-file :file1)
                      (ctho/add-frame :board)
                      (cths/add-sample-shape :a :parent-label :board :fixed-scroll true)
                      (cths/add-sample-shape :b :parent-label :board :sticky-scroll true))
          a       (cthi/id :a)
          b       (cthi/id :b)
          objects #(get-in % [:files (:current-file-id %) :data :pages-index (:current-page-id %) :objects])
          store   (ths/setup-store file)]
      (ths/run-store
       store done [(dwi/set-scroll-behavior [a b] :scroll)]
       (fn [state]
         (t/is (= :scroll (menu/scroll-behavior (map (objects state) [a b]))))
         (t/is (not-any? (fn [id] (some #(contains? (get (objects state) id) %) [:fixed-scroll :sticky-scroll]))
                         [a b])
               "neither key"))))))
