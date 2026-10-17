;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.timeline-selection-test
  (:require
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.path :as path]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.animation-mode :as dwam]
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

(t/deftest layers-out-of-the-board-keep-their-rows
  ;; A layer dragged out of every board keeps its track in the timeline
  ;; of its board (and plays on the canvas): it shows there, marked.
  (let [board    (uuid/next)
        rect     (uuid/next)
        loose    (uuid/next)
        group    (uuid/next)
        inner    (uuid/next)
        objects  {uuid/zero {:id uuid/zero :parent-id uuid/zero :shapes [board loose group]}
                  board     {:id board :parent-id uuid/zero :shapes [rect]}
                  rect      {:id rect :parent-id board}
                  loose     {:id loose :name "b" :parent-id uuid/zero}
                  group     {:id group :name "a" :parent-id uuid/zero :shapes [inner]}
                  inner     {:id inner :name "c" :parent-id group}}
        timeline (-> (cta/make-timeline {:board-id board})
                     (cta/add-keyframe rect {:time 0 :property :x :value 0})
                     (cta/add-keyframe loose {:time 0 :property :y :value 0})
                     (cta/add-keyframe inner {:time 0 :property :x :value 0}))
        rows     (filterv #(= :layer (:type %)) (timeline/timeline-rows objects board timeline {} {}))]
    (t/is (= [board rect inner loose] (mapv :id rows)) "after the layers of the board, by name")
    (t/is (= [false false true true] (mapv #(true? (:outside? %)) rows)))
    (t/is (= [0 1 1 1] (mapv :depth rows)))
    (t/is (= [board rect] (mapv :id (filter #(= :layer (:type %))
                                            (timeline/timeline-rows objects board
                                                                    (cta/remove-shapes timeline #{loose inner})
                                                                    {} {}))))
          "none without one")))

(t/deftest the-rows-filter-by-name
  ;; As the search of Blender's dope sheet: the properties, animations or
  ;; layers named so, with the layers holding them; all layers open.
  (let [board    (uuid/next)
        group    (uuid/next)
        dot      (uuid/next)
        title    (uuid/next)
        objects  {uuid/zero {:id uuid/zero :parent-id uuid/zero :shapes [board]}
                  board     {:id board :name "Board" :parent-id uuid/zero :shapes [group title]}
                  group     {:id group :name "Card" :parent-id board :shapes [dot]}
                  dot       {:id dot :name "Dot" :parent-id group}
                  title     {:id title :name "Title" :parent-id board}}
        timeline (-> (cta/make-timeline {:board-id board})
                     (cta/add-keyframe dot {:time 0 :property :x :value 0})
                     (cta/add-keyframe dot {:time 0 :property :opacity :value 1})
                     (cta/add-animation title {:type :fade :start 0 :duration 300}))
        rows     (conj (timeline/timeline-rows objects board timeline {group false} {})
                       {:type :audio-group})
        all      (conj (timeline/timeline-rows objects board timeline :all {}) {:type :audio-group})
        shown    #(mapv (fn [{:keys [type id property]}] (or property (when (= :audio-group type) :audio) id))
                        (timeline/filter-rows all %))]
    (t/is (= rows (timeline/filter-rows rows "  ")) "nothing to filter by")
    (t/is (empty? (filter #(= dot (:id %)) rows)) "the card is folded: the dot has no row")
    (t/is (= [board group dot :x :audio] (shown "POS")) "a property, under its folded layer")
    (t/is (= [board title :fade :audio]
             (mapv (fn [{:keys [type id animation]}]
                     (case type :animation (:type animation) :audio-group :audio id))
                   (timeline/filter-rows all "fade")))
          "an animation")
    (t/is (= [board group dot :x :opacity :audio] (shown "card")) "a layer, with all its rows")
    (t/is (= [board :audio] (shown "nothing like it")))
    (t/is (= [board group dot :x :audio] (shown "pos x")) "every word")
    (t/is (= [board group dot :opacity :audio] (shown "dot OPAC")) "with the names of the layers")))

(t/deftest the-dock-follows-a-layer-out-of-its-board
  (let [[board-a board-b loose] [(uuid/next) (uuid/next) (uuid/next)]
        objects   {uuid/zero {:id uuid/zero :type :frame :shapes [board-a board-b loose]}
                   board-a {:id board-a :type :frame :parent-id uuid/zero :frame-id uuid/zero}
                   board-b {:id board-b :type :frame :parent-id uuid/zero :frame-id uuid/zero}
                   loose   {:id loose :type :rect :parent-id uuid/zero :frame-id uuid/zero}}
        timelines {board-a (cta/add-keyframe (cta/make-timeline {:board-id board-a})
                                             loose {:time 0 :property :x :value 0})}
        anim      {:board-id board-b}]
    (t/is (= board-a (dwam/selection-board-id objects [loose] timelines)) "the board holding its track")
    (t/is (= board-a (dwam/target-board-id objects [loose] anim timelines)))
    (t/is (= board-b (dwam/target-board-id objects [loose] (assoc anim :board-id board-b) {}))
          "without a track, the last one")))

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
      (t/is (= :show-properties (action (key-event "s") kfs nil)) "S shows the scale, as no clip is there to split"))
    (t/testing "nothing selected"
      (t/is (nil? (action (key-event "c" :mod? true) #{} nil)) "Cmd/Ctrl+C reaches the workspace")
      (t/is (nil? (action (key-event "b" :mod? true) #{} nil)) "and Cmd/Ctrl+B")
      (t/is (= :split-all-clips (action (key-event "b" :mod? true :shift? true) #{} nil))
            "but the clips under the playhead are cut")
      (t/is (= :add-marker (action (key-event "m") #{} nil)))
      (t/is (nil? (action (key-event "m" :shift? true) #{} nil)) "Shift+M leaves motion mode")
      (t/is (= :jump-to-marker (action (key-event "ArrowRight" :alt? true) #{} nil))))))

(t/deftest the-keys-of-after-effects-show-one-kind-of-property
  ;; P, S, R and T, as in After Effects; with Shift, more than one
  (let [action #(timeline/dock-key-action % #{} nil)
        after  #(timeline/shown-properties %1 %2 %3)]
    (doseq [k ["p" "P" "s" "r" "t" "u" "U"]]
      (t/is (= :show-properties (action (key-event k))) k))
    (t/is (= :show-properties (action (key-event "P" :shift? true))) "with Shift too")
    (t/is (nil? (action (key-event "p" :mod? true))) "but not with Cmd/Ctrl")
    (t/is (= :split-clip (timeline/dock-key-action (key-event "s") #{} (uuid/next))) "S splits a selected clip")
    (t/is (= #{:x :y} (after nil "p" false)) "only the position")
    (t/is (= #{:rotation} (after #{:x :y} "r" false)) "only the rotation, then")
    (t/is (nil? (after #{:x :y} "p" false)) "the same key again: all of them")
    (t/is (= #{:x :y :scale-x :scale-y} (after #{:x :y} "s" true)) "Shift adds the scale")
    (t/is (= #{:scale-x :scale-y} (after #{:x :y :scale-x :scale-y} "p" true)) "and takes the position out")
    (t/is (nil? (after #{:opacity} "t" true)) "nothing left: all of them")
    (t/is (nil? (after #{:opacity} "u" false)) "U shows all of them")))

(t/deftest the-rows-filter-by-property
  (let [board    (uuid/next)
        dot      (uuid/next)
        title    (uuid/next)
        objects  {uuid/zero {:id uuid/zero :parent-id uuid/zero :shapes [board]}
                  board     {:id board :name "Board" :parent-id uuid/zero :shapes [dot title]}
                  dot       {:id dot :name "Position" :parent-id board}
                  title     {:id title :name "Title" :parent-id board}}
        timeline (-> (cta/make-timeline {:board-id board})
                     (cta/add-keyframe dot {:time 0 :property :x :value 0})
                     (cta/add-keyframe dot {:time 0 :property :rotation :value 0})
                     (cta/add-keyframe title {:time 0 :property :opacity :value 1})
                     (cta/add-animation title {:type :fade :start 0 :duration 300}))
        all      (conj (timeline/timeline-rows objects board timeline :all {}) {:type :audio-group})
        shown    #(mapv (fn [{:keys [type id property]}] (or property (when (= :audio-group type) :audio) id))
                        (timeline/filter-rows all %1 %2))]
    (t/is (= [board dot :x :audio] (shown "" #{:x :y})) "the position, under its layer, not the layer named so")
    (t/is (= [board title :opacity dot :rotation :audio] (shown "" #{:rotation :opacity})) "no animation")
    (t/is (= [board title :opacity :audio] (shown "title" #{:rotation :opacity})) "with the text too")))

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

(t/deftest in-frames-an-arrow-nudges-onto-frames
  ;; at 30 fps a frame lasts 33.3 ms: the step counts from the frame
  ;; nearest to where the selection starts, so it lands on frames
  (let [nudge #(+ 67 (timeline/nudge-step (apply key-event %) :frames 30 67))]
    (t/is (= 100 (nudge ["ArrowRight"])) "frame 2 to 3")
    (t/is (= 33 (nudge ["ArrowLeft"])) "back to frame 1")
    (t/is (= 400 (nudge ["ArrowRight" :shift? true])) "10 frames on")
    (t/is (= 10 (timeline/nudge-step (key-event "ArrowRight") :frames 30 nil))
          "nothing selected to count from")))

(t/deftest the-axis-reaches-the-end-of-what-is-past-the-timeline
  (let [tl (-> (cta/make-timeline {:board-id (uuid/next) :duration 2000})
               (cta/add-keyframe shape-a {:time 2500 :property :x :value 0}))]
    (t/is (= 2500 (timeline/content-end tl {})) "a keyframe")
    (t/is (= 3000 (timeline/content-end tl {(uuid/next) [500 3000]})) "an animated copy, by its span")
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

(t/deftest the-dock-targets-the-board-of-the-selection
  ;; The same board for the store (`dwam/active-board-id`) and the dock,
  ;; so the dock maximizes only with a timeline to show.
  (let [[board-a board-b rect] [(uuid/next) (uuid/next) (uuid/next)]
        objects {uuid/zero {:id uuid/zero :type :frame :shapes [board-a board-b]}
                 board-a {:id board-a :type :frame :parent-id uuid/zero :frame-id uuid/zero :shapes [rect]}
                 board-b {:id board-b :type :frame :parent-id uuid/zero :frame-id uuid/zero}
                 rect    {:id rect :type :rect :parent-id board-a :frame-id board-a}}
        anim    {:board-id board-b}]
    (t/is (= board-a (dwam/target-board-id objects [rect] anim)) "the board of what is selected")
    (t/is (= board-a (dwam/target-board-id objects [board-a] anim)) "a selected board itself")
    (t/is (= board-b (dwam/target-board-id objects [] anim)) "the last one, nothing selected")
    (t/is (nil? (dwam/selection-board-id objects [])))))

(t/deftest the-view-never-reaches-past-its-scroll
  ;; The content box of the scroll is fractional at a browser zoom; the
  ;; view takes it rounded down, so it never overflows the scroll and
  ;; brings in scrollbars that would shrink it (the timeline shook).
  (t/is (= {:width 1162 :height 621}
           (timeline/scroll-view-size #js {:width 1162.92 :height 621.54}))
        "rounded down, not to the nearest pixel")
  (t/is (= {:width 1512 :height 820}
           (timeline/scroll-view-size #js {:width 1512 :height 820}))
        "whole pixels as they are"))

(t/deftest the-path-row-counts-the-nodes-of-its-keyframe
  (let [square  (path/from-plain [{:command :move-to :params {:x 0 :y 0}}
                                  {:command :line-to :params {:x 10 :y 0}}
                                  {:command :line-to :params {:x 10 :y 10}}
                                  {:command :line-to :params {:x 0 :y 10}}
                                  {:command :close-path :params {}}])
        matched (path/from-plain (into [{:command :move-to :params {:x 0 :y 0}}]
                                       (map (fn [i] {:command :curve-to
                                                     :params {:x i :y 0 :c1x i :c1y 0 :c2x i :c2y 0}}))
                                       (range 1 20)))]
    (t/is (= 4 (timeline/outline-nodes {:value square} matched)) "on a keyframe, its own nodes")
    (t/is (= 20 (timeline/outline-nodes nil matched)) "between them, those shown")
    (t/is (= 0 (timeline/outline-nodes nil nil)))))
