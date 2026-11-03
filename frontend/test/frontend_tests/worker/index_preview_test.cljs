;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.worker.index-preview-test
  "The animation preview in the index of the worker (Penpot Motion): the
  shapes as the animation shows them at the playhead, which hovering,
  clicking and snapping find, live as long as the index of their page."
  (:require
   [app.common.files.builder :as fb]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.worker.impl :as impl]
   [app.worker.index :as idx]
   [cljs.test :as t :include-macros true]))

(t/use-fixtures :each
  {:before #(reset! idx/state {:pages-index {}})})

(defn- make-page
  "A page with a rect at 0,0 (100x100)."
  []
  (-> (fb/create-state)
      (fb/add-file {:name "Test"})
      (fb/add-page {:name "Page 1"})
      (fb/add-shape {:type :rect :x 0 :y 0 :width 100 :height 100})
      (fb/get-current-page)))

(defn- the-rect
  [page]
  (->> (vals (:objects page))
       (filter #(= :rect (:type %)))
       (first)))

(defn- set-preview!
  "The animation shows the rect of `page` 500 to the right."
  [page]
  (let [rect (the-rect page)]
    (impl/handler {:cmd :index/set-preview
                   :page-id (:id page)
                   :objects {(:id rect) (gsh/move rect (gpt/point 500 0))}})))

(defn- found-at
  "The ids the selection finds in the 20x20 rect at `x`,`y` of `page`."
  [page x y]
  (set (impl/handler {:cmd :index/query-selection
                      :page-id (:id page)
                      :rect (grc/make-rect x y 20 20)})))

(defn- preview-of
  [page]
  (get-in @idx/state [::idx/preview (:id page)]))

(t/deftest the-preview-moves-what-the-selection-finds
  (let [page (make-page)
        id   (:id (the-rect page))]
    (impl/handler {:cmd :index/initialize :page page})
    (set-preview! page)
    (t/is (contains? (found-at page 540 40) id))
    (t/is (not (contains? (found-at page 40 40) id)))))

(t/deftest finalize-drops-the-preview-of-the-page
  (let [page (make-page)]
    (impl/handler {:cmd :index/initialize :page page})
    (set-preview! page)
    (t/is (some? (preview-of page)))
    (impl/handler {:cmd :index/finalize :page-id (:id page)})
    (t/is (nil? (preview-of page)))))

(t/deftest finalize-drops-only-the-preview-of-that-page
  (let [page-a (make-page)
        page-b (make-page)]
    (doseq [page [page-a page-b]]
      (impl/handler {:cmd :index/initialize :page page})
      (set-preview! page))
    (impl/handler {:cmd :index/finalize :page-id (:id page-a)})
    (t/is (nil? (preview-of page-a)))
    (t/is (some? (preview-of page-b)) "the other page keeps its preview")))

(t/deftest deleting-the-page-drops-its-preview
  (let [page    (make-page)
        page-id (:id page)]
    (impl/handler {:cmd :index/initialize :page page})
    (set-preview! page)
    (impl/handler {:cmd :index/update :page-id page-id
                   :changes [{:type :del-page :id page-id :page-id page-id}]})
    (t/is (nil? (preview-of page)))))

(t/deftest an-update-keeps-the-preview
  (let [page     (make-page)
        page-id  (:id page)
        id       (:id (the-rect page))
        shape-id (uuid/next)]
    (impl/handler {:cmd :index/initialize :page page})
    (set-preview! page)
    (impl/handler {:cmd :index/update :page-id page-id
                   :changes [{:type :add-obj :id shape-id :page-id page-id
                              :parent-id uuid/zero :frame-id uuid/zero
                              :obj (cts/setup-shape {:id shape-id :type :rect
                                                     :x 200 :y 200 :width 50 :height 50})}]})
    (t/is (some? (preview-of page)))
    (t/is (contains? (found-at page 540 40) id) "where the animation shows it")
    (t/is (contains? (found-at page 210 210) shape-id) "the new shape too")))
