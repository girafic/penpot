;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.logic.animation-clipboard-test
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cths]
   [app.common.transit :as transit]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.pages :as thp]
   [frontend-tests.helpers.state :as ths]
   [frontend-tests.helpers.wasm :as thw]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before (fn []
             (thp/reset-idmap!)
             (cthi/reset-idmap!)
             (thw/setup-wasm-mocks!))
   :after  thw/teardown-wasm-mocks!})

(defn- setup-file
  "Board A at 100,100 with a rect at 150,200 (50,100 in it) moving right,
  another rect and a group with a child; board B at 1000,100 with a rect
  at 1100,150 (100,50 in it)."
  []
  (let [file   (-> (cthf/sample-file :file1 :page-label :page-1)
                   (ctho/add-frame :board-a :x 100 :y 100 :width 400 :height 300)
                   (cths/add-sample-shape :source :type :rect :parent-label :board-a
                                          :x 150 :y 200 :width 50 :height 50)
                   (cths/add-sample-shape :rect-a :type :rect :parent-label :board-a
                                          :x 300 :y 200 :width 50 :height 50)
                   (ctho/add-group :group :parent-label :board-a)
                   (cths/add-sample-shape :child :type :rect :parent-label :group
                                          :x 120 :y 120 :width 10 :height 10)
                   (ctho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                   (cths/add-sample-shape :rect-b :type :rect :parent-label :board-b
                                          :x 1100 :y 150 :width 100 :height 100))
        source (cthi/id :source)
        tl     (-> (cta/make-timeline {:board-id (cthi/id :board-a) :duration 1000})
                   (cta/add-keyframe source {:time 0 :property :x :value 50})
                   (cta/add-keyframe source {:time 1000 :property :x :value 150}))]
    (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                 (pcb/with-page (cthf/current-page file))
                                 (pcb/set-timeline (cthi/id :board-a) tl)))))

(defn- as-pasted
  "`payload` as the clipboard gives it back."
  [payload]
  (transit/decode-str (transit/encode-str payload {:type :json-verbose})))

(defn- copied
  "What copying the animation of the layer `label` of `file` puts on the
  clipboard."
  [file label]
  (let [page (cthf/current-page file)
        clip (cltl/animation-clip (:id file) page (:objects page) {(:id file) file}
                                  [(cthi/id label)] :whole-layer? true)]
    (as-pasted {:type :copied-animation :version 1 :clip clip})))

(defn- store-of
  "A store with `file`, Motion on unless `motion?` is false."
  ([file] (store-of file true))
  ([file motion?]
   (let [store (ths/setup-store file)]
     (when motion?
       (ptk/emit! store #(update % :features (fnil conj #{}) "animation/v1")))
     store)))

(defn- timelines-of
  [state]
  (:timelines (thp/current-page state)))

(defn- x-values
  [state board-label label]
  (mapv :value (cta/property-keyframes (get (timelines-of state) (cthi/id board-label))
                                       (cthi/id label) :x)))

(defn- close-all?
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map #(mth/close? %1 %2) expected actual))))

(defn- toast
  [state]
  (get-in state [:notification :content]))

(defn- select
  [labels]
  (dws/select-shapes (into (d/ordered-set) (map cthi/id) labels)))

(t/deftest paste-animation-onto-layers-of-two-boards
  (t/async
    done
    (let [file  (setup-file)
          store (store-of file)]
      (ths/run-store
       store done
       [(select [:rect-a :rect-b])
        (dw/paste-animation-data (copied file :source))]
       (fn [state]
         (t/is (close-all? [200 300] (x-values state :board-a :rect-a)))
         (t/is (close-all? [100 200] (x-values state :board-b :rect-b)))
         (t/is (nil? (toast state))))))))

(defn- watch-undo-stack
  "Put the commits on the undo stack, as the workspace does (see
  `app.main.data.workspace/initialize-workspace`), which the test store
  leaves out."
  []
  (ptk/reify ::watch-undo-stack
    ptk/WatchEvent
    (watch [_ _ stream]
      (->> stream
           (rx/filter dch/commit?)
           (rx/map deref)
           (rx/filter #(and (= :local (:source %)) (:save-undo? %) (seq (:undo-changes %))))
           (rx/map #(dwu/append-undo (select-keys % [:undo-changes :redo-changes :undo-group :tags])
                                     false))
           (rx/take-until (rx/filter #(= :the/end %) stream))))))

(t/deftest one-undo-reverts-the-whole-paste
  (t/async
    done
    (let [file  (setup-file)
          store (store-of file)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (select [:rect-a :rect-b])
        (dw/paste-animation-data (copied file :source))
        dwu/undo]
       (fn [state]
         (t/is (= (:timelines (cthf/current-page file)) (timelines-of state))))))))

(t/deftest a-malformed-payload-pastes-nothing
  (t/async
    done
    (let [file    (setup-file)
          store   (store-of file)
          payload (-> (copied file :source)
                      (assoc-in [:clip :layers 0 :track :keyframes 0 :value] "#fff")
                      (as-pasted))]
      (ths/run-store
       store done
       [(select [:rect-a])
        (dw/paste-animation-data payload)]
       (fn [state]
         (t/is (= (:timelines (cthf/current-page file)) (timelines-of state)))
         (t/is (= (tr "workspace.animation.paste-invalid") (toast state))))))))

(t/deftest without-motion-nothing-is-pasted
  (t/async
    done
    (let [file  (setup-file)
          store (store-of file false)]
      (ths/run-store
       store done
       [(select [:rect-a])
        (dw/paste-animation-data (copied file :source))]
       (fn [state]
         (t/is (= (:timelines (cthf/current-page file)) (timelines-of state)))
         (t/is (= (tr "workspace.animation.paste-needs-motion") (toast state))))))))

(t/deftest paste-needs-a-selection-in-a-board
  (t/async
    done
    (let [file  (setup-file)
          store (store-of file)]
      (ths/run-store
       store done
       [(dws/deselect-all)
        (dw/paste-animation-data (copied file :source))]
       (fn [state]
         (t/is (= (:timelines (cthf/current-page file)) (timelines-of state)))
         (t/is (= (tr "workspace.animation.paste-no-target") (toast state))))))))

(t/deftest paste-tells-what-the-layer-cannot-show
  (t/async
    done
    (let [file    (setup-file)
          store   (store-of file)
          payload (-> (copied file :source)
                      (update-in [:clip :layers 0 :track :keyframes] conj
                                 (cta/make-keyframe {:time 0 :property :fill-color :index 1
                                                     :value "#ff0000"}))
                      (as-pasted))]
      (ths/run-store
       store done
       [(select [:rect-a])
        (dw/paste-animation-data payload)]
       (fn [state]
         (t/is (close-all? [200 300] (x-values state :board-a :rect-a)))
         (t/is (= (tr "workspace.animation.paste-skipped" 1) (toast state))))))))

(t/deftest paste-goes-onto-a-group-not-its-child-too
  (t/async
    done
    (let [file  (setup-file)
          store (store-of file)]
      (ths/run-store
       store done
       [(select [:group :child])
        (dw/paste-animation-data (copied file :source))]
       (fn [state]
         (let [tracks (get-in (timelines-of state) [(cthi/id :board-a) :tracks])]
           (t/is (contains? tracks (cthi/id :group)))
           (t/is (not (contains? tracks (cthi/id :child))))))))))

(t/deftest an-oversized-paste-is-refused
  (t/async
    done
    (let [file    (setup-file)
          store   (store-of file)
          payload (-> (copied file :source)
                      (assoc-in [:clip :layers 0 :track :keyframes]
                                (mapv #(cta/make-keyframe {:time % :property :x :value %})
                                      (range (inc cltl/max-paste-keyframes))))
                      (as-pasted))]
      (ths/run-store
       store done
       [(select [:rect-a])
        (dw/paste-animation-data payload)]
       (fn [state]
         (t/is (= (:timelines (cthf/current-page file)) (timelines-of state)))
         (t/is (= (tr "workspace.animation.paste-too-large") (toast state))))))))

(t/deftest an-animation-copied-in-another-file-pastes
  (t/async
    done
    (let [file    (setup-file)
          store   (store-of file)
          payload (-> (copied file :source)
                      (assoc-in [:clip :file-id] (uuid/next))
                      (as-pasted))]
      (ths/run-store
       store done
       [(select [:source :rect-a])
        (dw/paste-animation-data payload)]
       (fn [state]
         ;; from another file, the layer with the source's id is another one
         (t/is (close-all? [50 150] (x-values state :board-a :source)))
         (t/is (close-all? [200 300] (x-values state :board-a :rect-a))))))))
