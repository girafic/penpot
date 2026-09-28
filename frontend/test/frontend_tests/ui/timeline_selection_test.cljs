;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.timeline-selection-test
  (:require
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.animation :as dwa]
   [app.main.ui.workspace.timeline :as timeline]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(def ^:private shape-a (uuid/next))
(def ^:private kf-a (uuid/next))
(def ^:private kf-b (uuid/next))

(defn- hit
  [keyframe-id left top right bottom]
  {:shape-id    shape-a
   :keyframe-id keyframe-id
   :left        left
   :top         top
   :right       right
   :bottom      bottom})

(defn- selected
  [state]
  (get-in state [:workspace-animation :selected-kfs]))

(defn- selected-one
  [state]
  (get-in state [:workspace-animation :selected-kf]))

(t/deftest rect-from-points-normalizes-drag
  (t/is (= {:left 5 :top 8 :right 10 :bottom 20}
           (timeline/rect-from-points 10 20 5 8))))

(t/deftest rects-intersect-when-overlapping
  (t/is (true? (timeline/rects-intersect?
                {:left 0 :top 0 :right 10 :bottom 10}
                {:left 5 :top 5 :right 15 :bottom 15}))))

(t/deftest rects-intersect-when-touching-edges
  (t/is (true? (timeline/rects-intersect?
                {:left 0 :top 0 :right 10 :bottom 10}
                {:left 10 :top 0 :right 20 :bottom 10}))))

(t/deftest rects-do-not-intersect-when-apart
  (t/is (false? (timeline/rects-intersect?
                 {:left 0 :top 0 :right 10 :bottom 10}
                 {:left 11 :top 0 :right 20 :bottom 10}))))

(t/deftest keyframes-in-rect-keeps-overlapping-diamonds
  (let [hits    [(hit kf-a 10 10 20 20)
                 (hit kf-b 100 10 110 20)]
        marquee {:left 0 :top 0 :right 25 :bottom 25}]
    (t/is (= #{{:shape-id shape-a :keyframe-id kf-a}}
             (timeline/keyframes-in-rect hits marquee)))))

(t/deftest keyframes-in-rect-can-keep-several
  (let [hits    [(hit kf-a 10 10 20 20)
                 (hit kf-b 30 12 40 22)]
        marquee {:left 0 :top 0 :right 50 :bottom 30}]
    (t/is (= #{{:shape-id shape-a :keyframe-id kf-a}
               {:shape-id shape-a :keyframe-id kf-b}}
             (timeline/keyframes-in-rect hits marquee)))))

(t/deftest marquee-drag-ignores-small-moves
  (t/is (false? (timeline/marquee-drag? 0 0 2 3))))

(t/deftest marquee-drag-starts-after-threshold
  (t/is (true? (timeline/marquee-drag? 0 0 4 0))))

(t/deftest select-keyframes-replaces-selection
  (let [k1    {:shape-id shape-a :keyframe-id kf-a}
        k2    {:shape-id shape-a :keyframe-id kf-b}
        state {:workspace-animation {:selected-kfs #{k1}}}
        next  (ptk/update (dwa/select-keyframes [k2]) state)]
    (t/is (= #{k2} (selected next)))
    (t/is (= k2 (selected-one next)))))

(t/deftest select-keyframes-clears-on-empty
  (let [k1    {:shape-id shape-a :keyframe-id kf-a}
        state {:workspace-animation {:selected-kfs #{k1} :selected-kf k1}}
        next  (ptk/update (dwa/select-keyframes []) state)]
    (t/is (= #{} (selected next)))
    (t/is (nil? (selected-one next)))))

(t/deftest select-keyframes-drops-single-when-many
  (let [k1   {:shape-id shape-a :keyframe-id kf-a}
        k2   {:shape-id shape-a :keyframe-id kf-b}
        next (ptk/update (dwa/select-keyframes [k1 k2]) {})]
    (t/is (= #{k1 k2} (selected next)))
    (t/is (nil? (selected-one next)))))

(t/deftest a-selected-layer-opens-its-parents
  (let [board    (uuid/next)
        group    (uuid/next)
        rect     (uuid/next)
        other    (uuid/next)
        objects  {uuid/zero {:id uuid/zero :parent-id uuid/zero :shapes [board other]}
                  board     {:id board :parent-id uuid/zero :shapes [group]}
                  group     {:id group :parent-id board :shapes [rect]}
                  rect      {:id rect :parent-id group}
                  other     {:id other :parent-id uuid/zero}}
        still    (cta/make-timeline {:board-id board})
        animated (cta/add-keyframe still rect {:time 0 :property :x :value 0})
        expanded {}]
    (t/is (= {board true group true} (timeline/expand-to {} objects board still #{rect})))
    (t/is (identical? expanded (timeline/expand-to expanded objects board animated #{rect}))
          "animated layers start open")
    (t/is (= {group true} (timeline/expand-to {group false} objects board animated #{rect}))
          "one the user folded opens")
    (t/is (identical? expanded (timeline/expand-to expanded objects board still #{board other}))
          "the board and layers out of it")))

(t/deftest a-click-on-a-property-picks-its-keyframes
  (let [file-id  (uuid/next)
        page-id  (uuid/next)
        board-id (uuid/next)
        w1       (uuid/next)
        w2       (uuid/next)
        h1       (uuid/next)
        timeline (-> (cta/make-timeline {:board-id board-id})
                     (cta/add-keyframe shape-a {:id w1 :time 0 :property :width :value 10})
                     (cta/add-keyframe shape-a {:id w2 :time 500 :property :width :value 20})
                     (cta/add-keyframe shape-a {:id h1 :time 0 :property :height :value 10}))
        state    {:current-file-id file-id
                  :current-page-id page-id
                  :workspace-animation {:board-id board-id
                                        :selected-kfs #{{:shape-id shape-a :keyframe-id kf-a}}}
                  :files {file-id {:data {:pages-index {page-id {:objects {}
                                                                 :timelines {board-id timeline}}}}}}}
        pick     (fn [state property add?]
                   (ptk/update (dwa/select-slot-keyframes shape-a property nil add?) state))
        width    #{{:shape-id shape-a :keyframe-id w1} {:shape-id shape-a :keyframe-id w2}}]
    (t/is (= width (selected (pick state :width false))) "in place of the ones selected")
    (t/is (= (conj width {:shape-id shape-a :keyframe-id h1})
             (selected (pick (pick state :width false) :height true)))
          "Shift adds them")
    (let [locked (update-in state [:files file-id :data :pages-index page-id :timelines board-id]
                            cta/toggle-slot-flag shape-a :locked :width)]
      (t/is (= #{{:shape-id shape-a :keyframe-id kf-a}} (selected (pick locked :width false)))
            "a locked property keeps its keyframes out of it"))))
