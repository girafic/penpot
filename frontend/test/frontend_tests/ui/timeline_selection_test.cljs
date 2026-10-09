;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.timeline-selection-test
  (:require
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
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

(t/deftest only-the-layers-that-animate-have-rows
  (let [board    (uuid/next)
        group    (uuid/next)
        rect     (uuid/next)
        still    (uuid/next)
        text     (uuid/next)
        objects  {uuid/zero {:id uuid/zero :parent-id uuid/zero :shapes [board]}
                  board     {:id board :parent-id uuid/zero :shapes [group still text]}
                  group     {:id group :parent-id board :shapes [rect]}
                  rect      {:id rect :parent-id group}
                  still     {:id still :parent-id board}
                  text      {:id text :parent-id board}}
        timeline (-> (cta/make-timeline {:board-id board})
                     (cta/add-keyframe rect {:time 0 :property :x :value 0})
                     (cta/add-keyframe rect {:time 500 :property :x :value 10})
                     (cta/add-animation text {:type :fade :start 0 :duration 300}))
        layers   #(mapv :id (filter (fn [row] (= :layer (:type row))) %))]
    (t/is (= [board text group rect] (layers (timeline/timeline-rows objects board timeline {} {})))
          "a layer without animation has none, a group holding one has")
    (let [rows (timeline/timeline-rows objects board (cta/make-timeline {:board-id board}) {} {})]
      (t/is (= [board] (layers rows)) "the board always")
      (t/is (false? (:expandable? (first rows)))))))

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

;; KEYS OF THE DOCK

(defn- key-event
  "A key event of `key`, with Cmd and Ctrl (whichever the platform takes)
  when `mod?`, and Shift or Alt when given."
  [key & {:keys [mod? shift? alt?]}]
  #js {:key key :metaKey (boolean mod?) :ctrlKey (boolean mod?)
       :shiftKey (boolean shift?) :altKey (boolean alt?)})

(t/deftest the-keys-of-a-selected-clip-come-first
  (let [kfs    #{{:shape-id shape-a :keyframe-id kf-a}}
        clip   (uuid/next)
        action #(timeline/dock-key-action %1 %2 %3)]
    (t/testing "a clip selected"
      (t/is (= :delete-clip (action (key-event "Delete") #{} clip)))
      (t/is (= :delete-clip (action (key-event "Backspace") #{} clip)) "never the keyframes")
      (t/is (= :split-clip (action (key-event "s") #{} clip)))
      (t/is (= :split-clip (action (key-event "S") #{} clip)))
      (t/is (= :split-clip (action (key-event "b" :mod? true) #{} clip)) "Cmd/Ctrl+B, as Final Cut or Resolve")
      (t/is (= :split-all-clips (action (key-event "B" :mod? true :shift? true) #{} clip)) "Shift too: all of them")
      (t/is (= :nudge-clip (action (key-event "ArrowRight") kfs clip)) "the clip, not the keyframes")
      (t/is (= :nudge-clip (action (key-event "ArrowLeft" :shift? true) #{} clip)))
      (t/is (= :jump-to-marker (action (key-event "ArrowLeft" :alt? true) #{} clip)) "Alt jumps still")
      (t/is (= :copy-selection (action (key-event "c" :mod? true) #{} clip)) "the clips")
      (t/is (= :cut-selection (action (key-event "x" :mod? true) #{} clip)))
      (t/is (= :copy-selection (action (key-event "c" :mod? true) kfs clip)) "with the keyframes")
      (t/is (nil? (action (key-event "s" :mod? true) #{} clip)) "Cmd+S saves as ever"))
    (t/testing "keyframes selected, as before"
      (t/is (= :delete-keyframes (action (key-event "Delete") kfs nil)))
      (t/is (= :copy-selection (action (key-event "c" :mod? true) kfs nil)))
      (t/is (= :cut-selection (action (key-event "x" :mod? true) kfs nil)))
      (t/is (= :shift-keyframes (action (key-event "ArrowLeft") kfs nil)))
      (t/is (nil? (action (key-event "s") kfs nil)) "S splits no keyframe"))
    (t/testing "nothing selected"
      (t/is (nil? (action (key-event "c" :mod? true) #{} nil)) "Cmd/Ctrl+C reaches the workspace")
      (t/is (nil? (action (key-event "b" :mod? true) #{} nil)) "and Cmd/Ctrl+B")
      (t/is (= :split-all-clips (action (key-event "b" :mod? true :shift? true) #{} nil))
            "but the clips under the playhead are cut")
      (t/is (= :add-marker (action (key-event "m") #{} nil)))
      (t/is (nil? (action (key-event "m" :shift? true) #{} nil)) "Shift+M leaves motion mode")
      (t/is (= :jump-to-marker (action (key-event "ArrowRight" :alt? true) #{} nil))))))

(t/deftest a-box-picks-all-kinds-and-whole-tracks
  (let [hits  {:kfs   [{:shape-id 1 :keyframe-id 10 :left 0 :right 10 :top 0 :bottom 10}]
               :anims [{:shape-id 1 :animation-id 20 :left 0 :right 100 :top 20 :bottom 40}]
               :clips [{:clip-id :c :left 50 :right 150 :top 60 :bottom 80}]
               :bars  [{:shape-id 7 :left 0 :right 200 :top 100 :bottom 120}]}
        track {:kfs #{{:shape-id 7 :keyframe-id 70}} :anims #{{:shape-id 8 :animation-id 80}}}
        pick  #(timeline/items-in-rect hits (apply timeline/rect-from-points %) {7 track})]
    (t/is (= {:kfs #{{:shape-id 1 :keyframe-id 10}} :anims #{{:shape-id 1 :animation-id 20}} :clips #{:c}}
             (pick [5 5 60 70]))
          "a keyframe, a block and a clip")
    (t/is (= {:kfs #{{:shape-id 7 :keyframe-id 70}} :anims #{{:shape-id 8 :animation-id 80}} :clips #{}}
             (pick [150 110 160 115]))
          "a bar: all of its track")))

(t/deftest a-box-picks-the-clips-it-touches
  (let [hits [{:clip-id :a :left 10 :right 50 :top 0 :bottom 20}
              {:clip-id :b :left 60 :right 90 :top 30 :bottom 50}]]
    (t/is (= #{:a} (timeline/clips-in-rect hits (timeline/rect-from-points 40 5 50 15))))
    (t/is (= #{:a :b} (timeline/clips-in-rect hits (timeline/rect-from-points 100 100 0 0))))
    (t/is (= #{} (timeline/clips-in-rect hits (timeline/rect-from-points 52 0 57 100))) "between them")))

(t/deftest an-arrow-nudges-by-10-or-100-ms
  (t/is (= 10 (timeline/nudge-step (key-event "ArrowRight"))))
  (t/is (= -10 (timeline/nudge-step (key-event "ArrowLeft"))))
  (t/is (= 100 (timeline/nudge-step (key-event "ArrowRight" :shift? true))))
  (t/is (= -100 (timeline/nudge-step (key-event "ArrowLeft" :shift? true)))))

(t/deftest the-axis-reaches-the-end-of-what-is-past-the-timeline
  (let [tl (-> (cta/make-timeline {:board-id (uuid/next) :duration 2000})
               (cta/add-keyframe shape-a {:time 2500 :property :x :value 0}))]
    (t/is (= 2500 (timeline/content-end tl {})) "a keyframe")
    (t/is (= 3000 (timeline/content-end tl {(uuid/next) 3000})) "an animated copy")
    (t/is (= 60500 (timeline/content-end (ctau/add-audio-clip tl {:audio-id (uuid/next) :name "long.mp3"
                                                                  :source-duration 60000 :start 500})
                                         {}))
          "the end of a long audio clip, to trim it there")))

(t/deftest the-axis-starts-at-what-lies-before-0
  (let [tl (-> (cta/make-timeline {:board-id (uuid/next) :duration 2000})
               (cta/add-keyframe shape-a {:time 300 :property :x :value 0}))]
    (t/is (= 0 (timeline/content-start tl)) "nothing before 0: at 0")
    (t/is (= -400 (timeline/content-start (cta/add-keyframe tl shape-a {:time -400 :property :x :value 5})))
          "a keyframe")
    (t/is (= -250 (timeline/content-start (cta/add-animation tl shape-a {:type :fade :start -250 :duration 500})))
          "a preset")
    (t/is (= -900 (timeline/content-start (ctau/add-audio-clip tl {:audio-id (uuid/next) :name "a.mp3"
                                                                   :source-duration 3000 :start -900})))
          "an audio clip")))
