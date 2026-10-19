;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.data.animation-tilt-test
  "The drags of the 3D tool and of the handles on the canvas (see
  `dwa/tilt-layers` and `dwa/move-vanishing-point`)."
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.shapes :as dwsh]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.async :as async]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before thi/reset-idmap!})

(defn- tilt-file
  "Board A with a card whose turn about Y the timeline plays, and a
  second card."
  []
  (let [file (-> (thf/sample-file :file1)
                 (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (ths/add-sample-shape :card :type :rect :parent-label :board-a
                                       :x 40 :y 40 :width 100 :height 100)
                 (ths/add-sample-shape :other :type :rect :parent-label :board-a
                                       :x 200 :y 40 :width 100 :height 100))
        tl   (-> (cta/make-timeline {:board-id (thi/id :board-a) :duration 1000})
                 (cta/add-keyframe (thi/id :card) {:time 0 :property :rotate-y :value 0})
                 (cta/add-keyframe (thi/id :card) {:time 1000 :property :rotate-y :value 90})
                 (cta/add-keyframe (thi/id :board-a) {:time 0 :property :perspective-x :value 0.5})
                 (cta/add-keyframe (thi/id :board-a) {:time 1000 :property :perspective-x :value 1}))
        page (thf/current-page file)]
    (thf/apply-changes file (-> (pcb/empty-changes nil)
                                (pcb/with-page page)
                                (pcb/set-timeline (thi/id :board-a) tl)))))

(defn- workspace-state
  [file]
  {:current-file-id (:id file)
   :current-page-id (:id (thf/current-page file))
   :files {(:id file) file}
   :permissions {:can-edit true}
   :workspace-layout #{:animation-timeline}
   :workspace-local {:selected #{(thi/id :card)}}
   :workspace-animation {:board-id (thi/id :board-a)}})

(defn- ^:async emitted
  "The types of the events `event` emits on `state`, once its watch ends."
  [event state]
  (let [events (atom [])]
    (await (async/observe (ptk/watch event state (rx/empty)) :on-next #(swap! events conj %)))
    (mapv ptk/type @events)))

(t/deftest ^:async a-tilt-plays-at-the-playhead-where-it-is-animated
  (let [state (workspace-state (tilt-file))]
    (t/is (= [::dwsh/update-shapes]
             (await (emitted (dwa/tilt-layers [(thi/id :card)] nil [10 20]) state)))
          "outside motion mode the layer takes it")
    (t/is (= [::dwsh/update-shapes ::dwa/set-value-at-playhead]
             (await (emitted (dwa/tilt-layers [(thi/id :card)] (thi/id :card) [10 20]) state)))
          "its animated turn about Y at the playhead, X on the layer")
    (t/is (= [::dwsh/update-shapes]
             (await (emitted (dwa/tilt-layers [(thi/id :other)] (thi/id :other) [10 20]) state)))
          "a layer that does not play it takes it")))

(t/deftest ^:async a-vanishing-point-plays-at-the-playhead-where-it-is-animated
  (let [state (workspace-state (tilt-file))]
    (t/is (= [::dwsh/update-shapes]
             (await (emitted (dwa/move-vanishing-point (thi/id :board-a) false [0.25 0.75]) state))))
    (t/is (= [::dwsh/update-shapes ::dwa/set-value-at-playhead]
             (await (emitted (dwa/move-vanishing-point (thi/id :board-a) true [0.25 0.75]) state)))
          "its animated X at the playhead, Y on the board")))

(defn- lit-file
  "Board A lit from the top left, casting the shadows of its layers, with
  a card whose shadow falls straight down and one whose shadow falls
  from the light already."
  []
  (let [shadow (fn [ox oy] [{:id (thi/new-id! :shadow) :style :drop-shadow :offset-x ox :offset-y oy
                             :blur 8 :spread 0 :hidden false :color {:color "#000000" :opacity 0.3}}])]
    (-> (thf/sample-file :file1)
        (tho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
        (ths/update-shape :board-a :light {:angle 315 :height 45 :ambient 0.3 :shadows true})
        (ths/add-sample-shape :down :type :rect :parent-label :board-a :x 40 :y 40 :width 100 :height 100
                              :shadow (shadow 0 10))
        (ths/add-sample-shape :cast :type :rect :parent-label :board-a :x 200 :y 40 :width 100 :height 100
                              :shadow (shadow 4 4)))))

(t/deftest ^:async a-shadow-the-light-does-not-cast-yet-falls-from-it
  (let [state (workspace-state (lit-file))]
    (t/is (= [::dwsh/update-shapes]
             (await (emitted (dwa/cast-shadows-of #{(thi/id :down)} (uuid/next)) state)))
          "a shadow straight down turns")
    (t/is (= []
             (await (emitted (dwa/cast-shadows-of #{(thi/id :cast)} (uuid/next)) state)))
          "one that falls from the light already stays")
    (t/is (= [::dwsh/update-shapes]
             (await (emitted (dwa/cast-shadows-of #{(thi/id :board-a)} (uuid/next)) state)))
          "the layers in a board that changed, too")))

