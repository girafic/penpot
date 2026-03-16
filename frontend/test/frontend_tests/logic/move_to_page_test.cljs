;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.logic.move-to-page-test
  (:require
   [app.common.test-helpers.components :as cthc]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.common.test-helpers.variants :as cthv]
   [app.common.uuid :as uuid]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.guides :as-alias dwg]
   [app.main.data.workspace.shapes :as dwsh]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.pages :as thp]
   [frontend-tests.helpers.state :as ths]))

(t/use-fixtures :each
  {:before thp/reset-idmap!})

(t/deftest test-move-frame-to-other-page
  (t/async
    done
    (let [file   (-> (cthf/sample-file :file1 :page-label :page1)
                     (cthf/add-sample-page :page2)
                     (cthf/switch-to-page :page1)
                     (ctho/add-frame :frame1))
          store  (ths/setup-store file)
          frame1 (cths/get-shape file :frame1)
          page1-id (cthi/id :page1)
          page2-id (cthi/id :page2)

          events
          [(dwsh/relocate-shapes-to-page
            #{(:id frame1)} page1-id page2-id uuid/zero 0)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (let [file'  (ths/get-file-from-state new-state)
               page1' (cthf/get-page file' :page1)
               page2' (cthf/get-page file' :page2)]
           ;; Frame is gone from page 1
           (t/is (nil? (get (:objects page1') (:id frame1))))
           ;; Frame exists on page 2
           (t/is (some? (get (:objects page2') (:id frame1))))))))))


(t/deftest test-move-frame-with-children
  (t/async
    done
    (let [file   (-> (cthf/sample-file :file1 :page-label :page1)
                     (cthf/add-sample-page :page2)
                     (cthf/switch-to-page :page1)
                     (ctho/add-frame :frame1)
                     (ctho/add-rect :rect1 :parent-label :frame1)
                     (ctho/add-rect :rect2 :parent-label :frame1))
          store  (ths/setup-store file)
          frame1 (cths/get-shape file :frame1)
          rect1  (cths/get-shape file :rect1)
          rect2  (cths/get-shape file :rect2)
          page1-id (cthi/id :page1)
          page2-id (cthi/id :page2)

          events
          [(dwsh/relocate-shapes-to-page
            #{(:id frame1)} page1-id page2-id uuid/zero 0)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (let [file'   (ths/get-file-from-state new-state)
               page1'  (cthf/get-page file' :page1)
               page2'  (cthf/get-page file' :page2)
               frame1' (get (:objects page2') (:id frame1))]
           ;; All shapes gone from page 1
           (t/is (nil? (get (:objects page1') (:id frame1))))
           (t/is (nil? (get (:objects page1') (:id rect1))))
           (t/is (nil? (get (:objects page1') (:id rect2))))
           ;; All shapes present on page 2
           (t/is (some? frame1'))
           (t/is (some? (get (:objects page2') (:id rect1))))
           (t/is (some? (get (:objects page2') (:id rect2))))
           ;; Hierarchy preserved: frame has both children
           (t/is (= 2 (count (:shapes frame1'))))))))))


(t/deftest test-move-frame-transfers-guides
  (t/async
    done
    (let [file   (-> (cthf/sample-file :file1 :page-label :page1)
                     (cthf/add-sample-page :page2)
                     (cthf/switch-to-page :page1)
                     (ctho/add-frame :frame1))
          store  (ths/setup-store file)
          frame1 (cths/get-shape file :frame1)
          page1-id (cthi/id :page1)
          page2-id (cthi/id :page2)

          guide {:axis :x
                 :frame-id (:id frame1)
                 :id (uuid/next)
                 :position 50}

          events
          [(dw/update-guides guide)
           (dwsh/relocate-shapes-to-page
            #{(:id frame1)} page1-id page2-id uuid/zero 0)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (let [file'     (ths/get-file-from-state new-state)
               page1'    (cthf/get-page file' :page1)
               page2'    (cthf/get-page file' :page2)
               p1-guides (vals (:guides page1'))
               p2-guides (vals (:guides page2'))]
           ;; Guide removed from page 1
           (t/is (empty? p1-guides))
           ;; Guide present on page 2 with same properties
           (t/is (= 1 (count p2-guides)))
           (t/is (= (:frame-id (first p2-guides)) (:id frame1)))
           (t/is (= (:position (first p2-guides)) 50))))))))


(t/deftest test-move-component-updates-main-instance-page
  (t/async
    done
    (let [file   (-> (cthf/sample-file :file1 :page-label :page1)
                     (cthf/add-sample-page :page2)
                     (cthf/switch-to-page :page1)
                     (ctho/add-frame :frame1)
                     (cthc/make-component :comp1 :frame1))
          store  (ths/setup-store file)
          frame1 (cths/get-shape file :frame1)
          page1-id (cthi/id :page1)
          page2-id (cthi/id :page2)

          events
          [(dwsh/relocate-shapes-to-page
            #{(:id frame1)} page1-id page2-id uuid/zero 0)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (let [file'     (ths/get-file-from-state new-state)
               comp-id   (cthi/id :comp1)
               component (get-in file' [:data :components comp-id])]
           ;; Component's main-instance-page updated to target page
           (t/is (= page2-id (:main-instance-page component)))))))))


(t/deftest test-move-variant-becomes-standalone-component
  (t/async
    done
    (let [file   (-> (cthf/sample-file :file1 :page-label :page1)
                     (cthv/add-variant :v01 :c01 :m01 :c02 :m02)
                     (cthf/add-sample-page :page2))
          store  (ths/setup-store file)
          m01    (cths/get-shape file :m01 :page-label :page1)
          page1-id (cthi/id :page1)
          page2-id (cthi/id :page2)

          events
          [(dwsh/relocate-shapes-to-page
            #{(:id m01)} page1-id page2-id uuid/zero 0)]]

      (ths/run-store
       store done events
       (fn [new-state]
         (let [file'     (ths/get-file-from-state new-state)
               page1'    (cthf/get-page file' :page1)
               page2'    (cthf/get-page file' :page2)
               shape'    (get (:objects page2') (:id m01))
               comp-id   (cthi/id :c01)
               component (get-in file' [:data :components comp-id])]
           ;; Shape moved to page 2
           (t/is (some? shape'))
           ;; Shape no longer on page 1
           (t/is (nil? (get (:objects page1') (:id m01))))
           ;; Variant metadata removed from shape
           (t/is (nil? (:variant-id shape')))
           (t/is (nil? (:variant-name shape')))
           ;; Shape is now a root component
           (t/is (true? (:component-root shape')))
           ;; Component no longer has variant data
           (t/is (nil? (:variant-id component)))
           (t/is (nil? (:variant-properties component)))
           ;; Component's main-instance-page updated
           (t/is (= page2-id (:main-instance-page component)))))))))
