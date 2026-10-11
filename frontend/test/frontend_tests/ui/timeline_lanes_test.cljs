;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.timeline-lanes-test
  (:require
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.types.audio-clip :as ctau]
   [app.common.uuid :as uuid]
   [app.main.ui.workspace.timeline.lanes :as lanes]
   [cljs.test :as t :include-macros true]))

(def ^:private shape-id (uuid/next))

;; A time axis of 1000 ms over 1000 px: `time` is at x = 12 + time.
(def ^:private geo
  {:width 1000 :height 300 :span 1000 :axis-width 1000 :scroll-x 0 :scroll-y 0})

(def ^:private timeline
  (-> (cta/make-timeline {:board-id (uuid/next) :duration 800})
      (cta/add-keyframe shape-id {:time 100 :property :x :value 0})
      (cta/add-keyframe shape-id {:time 300 :property :x :value 10})
      (cta/add-keyframe shape-id {:time 600 :property :x :value 20})
      (cta/add-keyframe shape-id {:time 200 :property :opacity :value 1})
      (cta/toggle-slot-flag shape-id :locked :opacity)))

;; Row tops: 52, 80, 108, 136 and 164, their middles 14 below.
(def ^:private scene
  {:timeline timeline
   :duration 800
   :rows [{:type :layer :id shape-id :range [100 400]}
          {:type :animation :id shape-id :animation {:id (uuid/next) :start 500 :duration 200}}
          {:type :animation :id shape-id :animation {:id (uuid/next) :start 100 :duration 300 :locked true}}
          {:type :property :id shape-id :property :x :index nil}
          {:type :property :id shape-id :property :opacity :index nil}]})

(defn- hit-at
  [geo x y]
  (when-let [hit (lanes/hit geo scene x y)]
    (cond-> (select-keys hit [:type :row :mode :locked?])
      (some? (:keyframe hit)) (assoc :time (:time (:keyframe hit)))
      (some? (:from hit))     (assoc :from (:time (:from hit))))))

(t/deftest the-time-axis-scrolls-and-zooms
  (t/is (= 1280 (lanes/axis-width 1600 1)))
  (t/is (= 2560 (lanes/axis-width 1600 2)))
  (t/is (= 1600 (lanes/content-width 1280)))
  (t/is (= 112 (lanes/time->x geo 100)))
  (t/is (= 12 (lanes/time->x (assoc geo :scroll-x 100) 100)))
  (t/is (= 100 (lanes/x->time (assoc geo :scroll-x 100) 12)))
  (t/is (= 0 (lanes/x->time geo -100)) "before 0 s")
  (t/is (= 1000 (lanes/x->time geo 5000)) "past the end")
  (t/testing "from before 0, where something lies there"
    (let [early (assoc geo :origin -500 :span 1500 :axis-width 1500)]
      (t/is (= 12 (lanes/time->x early -500)))
      (t/is (= 512 (lanes/time->x early 0)) "0 as far in as 500 ms take")
      (t/is (= -500 (lanes/x->time early 12)))
      (t/is (= -500 (lanes/x->time early -100)) "not before its start"))))

(t/deftest the-rows-scroll-under-the-header
  (t/is (= 136 (lanes/row-y geo 3)))
  (t/is (= 108 (lanes/row-y (assoc geo :scroll-y 28) 3)))
  (t/is (= 3 (lanes/row-index geo 150)))
  (t/is (= 4 (lanes/row-index (assoc geo :scroll-y 28) 150)))
  (t/is (nil? (lanes/row-index geo 40)) "on the header")
  (t/is (= [0 9] (lanes/visible-rows geo 0)))
  (t/is (= [0 13] (lanes/visible-rows (assoc geo :scroll-y 56) 2)))
  (t/is (= [8 21] (lanes/visible-rows (assoc geo :scroll-y 280) 2))))

;; 248 px of rows under the header: rows 0 to 7 in full.
(t/deftest the-rows-scroll-to-rows-out-of-view
  (t/is (nil? (lanes/scroll-to-rows geo [3])) "in view")
  (t/is (nil? (lanes/scroll-to-rows geo [20 2])) "one of them in view")
  (t/is (= 450 (lanes/scroll-to-rows geo [20 21])) "the first in the middle")
  (t/is (= 114 (lanes/scroll-to-rows geo [8])) "half out of view")
  (t/is (= 0 (lanes/scroll-to-rows (assoc geo :scroll-y 400) [1])) "not above the first")
  (t/is (nil? (lanes/scroll-to-rows geo [])) "none"))

(t/deftest content-points-stay-as-the-view-scrolls
  (let [geo (assoc geo :scroll-x 100 :scroll-y 28)]
    (t/is (= [150 126] (lanes/content-point geo 50 150)))
    (t/is (= [100 28] (lanes/content-point geo -5 10)) "kept on the rows")))

(t/deftest the-ruler-scrubs-to-its-end
  (t/is (= {:type :ruler} (hit-at geo 20 10)))
  (t/is (= {:type :ruler} (hit-at geo 812 10)) "the end is not dragged there")
  (t/is (nil? (hit-at geo 20 30)) "the markers are not drawn")
  (t/is (nil? (hit-at geo -1 100)) "off the canvas"))

;; The end of the timeline, at 800 ms, is at x = 812.
(t/deftest the-end-of-the-timeline-is-dragged-on-its-background
  (t/is (= {:type :duration} (hit-at geo 812 66)) "in a layer row")
  (t/is (= {:type :duration} (hit-at geo 816 150)) "in the lane of a property")
  (t/is (= {:type :duration} (hit-at geo 808 200)) "under the rows")
  (t/is (= {:type :row :row 0} (hit-at geo 820 66)) "only close to it")
  (t/is (= {:type :bar :row 0 :mode :end}
           (-> (lanes/hit geo (assoc-in scene [:rows 0 :range] [100 800]) 812 66)
               (select-keys [:type :row :mode])))
        "a bar ending there keeps its end")
  (t/is (= :keyframe
           (let [timeline (cta/add-keyframe timeline shape-id {:time 800 :property :x :value 30})]
             (:type (lanes/hit geo (assoc scene :timeline timeline) 812 150))))
        "and a keyframe there its place"))

(t/deftest bars-and-blocks-have-ends-to-drag
  (t/is (= {:type :bar :row 0 :mode :start} (hit-at geo 110 66)))
  (t/is (= {:type :bar :row 0 :mode :move} (hit-at geo 250 66)))
  (t/is (= {:type :bar :row 0 :mode :end} (hit-at geo 414 66)))
  (t/is (= {:type :row :row 0} (hit-at geo 50 66)))
  (t/is (= {:type :row :row 0} (hit-at geo 250 54)) "above the bar")
  (t/is (= {:type :animation :row 1 :mode :start} (hit-at geo 515 94)))
  (t/is (= {:type :animation :row 1 :mode :move} (hit-at geo 600 94)))
  (t/is (= {:type :animation :row 1 :mode :end} (hit-at geo 710 94)))
  (t/is (= {:type :animation :row 2 :mode :move} (hit-at geo 115 122))
        "a locked block cannot be stretched"))

(t/deftest a-style-use-is-a-block-like-a-preset
  (let [style (ctas/make-animation-style {:name "Pop" :entries [{:type :fade :start 0 :duration 200}]})
        scene (assoc-in scene [:rows 1 :animation] (ctas/make-use style (uuid/next) 500))
        hit   #(select-keys (lanes/hit geo scene %1 %2) [:type :row :mode])]
    (t/is (= {:type :animation :row 1 :mode :start} (hit 515 94)))
    (t/is (= {:type :animation :row 1 :mode :move} (hit 600 94)))
    (t/is (= {:type :animation :row 1 :mode :end} (hit 710 94)) "its end stretches it")))

(t/deftest keyframes-segments-and-easing-buttons
  (t/is (= {:type :keyframe :row 3 :locked? false :time 100} (hit-at geo 112 150)))
  (t/is (= {:type :keyframe :row 3 :locked? false :time 100} (hit-at geo 117 153)))
  (t/is (= {:type :easing :row 3 :locked? false :from 100} (hit-at geo 212 150))
        "in the middle of the segment")
  (t/is (= {:type :segment :row 3 :locked? false :from 100} (hit-at geo 260 150)))
  (t/is (= {:type :segment :row 3 :locked? false :from 300} (hit-at geo 400 150)))
  (t/is (= {:type :lane :row 3 :locked? false} (hit-at geo 800 150)))
  (t/is (= {:type :keyframe :row 4 :locked? true :time 200} (hit-at geo 212 178)))
  (t/is (= {:type :empty} (hit-at geo 212 200)) "under the rows")
  (t/is (= {:type :keyframe :row 3 :locked? false :time 100}
           (hit-at (assoc geo :scroll-x 100 :scroll-y 28) 12 122))
        "where the view is scrolled to"))

(t/deftest the-box-selection-finds-the-keyframes-that-can-be-selected
  (let [boxes (lanes/keyframe-boxes geo scene)]
    (t/is (= (map :id (cta/property-keyframes timeline shape-id :x))
             (map :keyframe-id boxes))
          "not the locked ones")
    (t/is (every? #(= shape-id (:shape-id %)) boxes))
    (let [{:keys [left right top bottom]} (first boxes)]
      (t/is (< 104 left 112 right 120))
      (t/is (< 90 top 98 bottom 106) "in the fourth row, from the top of the rows"))))

(t/deftest audio-clips-have-ends-to-trim
  ;; an audio row after the others (top 192, middle 206): a clip from 100
  ;; to 300 ms and a locked one from 500 to 700 ms
  (let [song   (ctau/make-audio-clip {:audio-id (uuid/next) :name "song" :source-duration 1000
                                      :start 100 :duration 200})
        locked (ctau/make-audio-clip {:audio-id (uuid/next) :name "locked" :source-duration 1000
                                      :start 500 :duration 200 :locked true})
        scene  (update scene :rows conj {:type :audio :lane 0 :clips [song locked]})
        hit    #(let [hit (lanes/hit geo scene %1 %2)]
                  (cond-> (select-keys hit [:type :row :mode :lane])
                    (some? (:clip hit)) (assoc :clip (:name (:clip hit)))))]
    (t/is (= {:type :audio :row 5 :lane 0 :mode :start :clip "song"} (hit 110 206)))
    (t/is (= {:type :audio :row 5 :lane 0 :mode :move :clip "song"} (hit 200 206)))
    (t/is (= {:type :audio :row 5 :lane 0 :mode :end :clip "song"} (hit 310 206)))
    (t/is (= {:type :audio :row 5 :lane 0 :mode :move :clip "locked"} (hit 515 206))
          "a locked clip cannot be trimmed")
    (t/is (= {:type :row :row 5 :lane 0} (hit 400 206)) "between the clips")
    (t/testing "the one on top where they overlap"
      (let [over  (ctau/make-audio-clip {:audio-id (uuid/next) :name "over" :source-duration 1000
                                         :start 200 :duration 200})
            scene (assoc-in scene [:rows 5 :clips] [song over])]
        (t/is (= "over" (:name (:clip (lanes/hit geo scene 250 206)))))))))

(t/deftest audio-clips-have-grips-for-their-fades
  ;; the audio row from 192 (its bar from 197): a clip from 100 to 300 ms
  ;; (x 112 to 312) fading in over 50 ms, so its grip is at x 162, and the
  ;; one of its fade out at its corner, 7 px in (x 305)
  (let [song   (ctau/make-audio-clip {:audio-id (uuid/next) :name "song" :source-duration 1000
                                      :start 100 :duration 200 :fade-in 50})
        locked (ctau/make-audio-clip {:audio-id (uuid/next) :name "locked" :source-duration 1000
                                      :start 500 :duration 200 :locked true})
        short  (ctau/make-audio-clip {:audio-id (uuid/next) :name "short" :source-duration 1000
                                      :start 800 :duration 20})
        scene  (update scene :rows conj {:type :audio :lane 0 :clips [song locked short]})
        mode   #(:mode (lanes/hit geo scene %1 %2))]
    (t/is (= :fade-in (mode 162 198)) "on the top edge where the fade ends")
    (t/is (= :fade-in (mode 158 193)) "from just above the bar")
    (t/is (= :fade-out (mode 305 198)) "at the corner of a clip without one")
    (t/is (= :move (mode 162 206)) "lower in the bar, the clip moves")
    (t/is (= :start (mode 118 206)) "and its edges trim")
    (t/is (= :start (mode 118 198)) "away from the grips too")
    (t/is (= :move (mode 518 198)) "a locked clip has no grips")
    (t/is (= :start (mode 819 198)) "nor one too short for them")))

(t/deftest audio-clips-have-a-volume-line
  ;; the audio row from 192, its bar from 197, 18 px tall: the line at 200
  ;; at full volume, at 206 at half; clips from 100 to 300 ms (x 112 to 312)
  (let [loud   (ctau/make-audio-clip {:audio-id (uuid/next) :name "loud" :source-duration 1000
                                      :start 100 :duration 200})
        quiet  (ctau/make-audio-clip {:audio-id (uuid/next) :name "quiet" :source-duration 1000
                                      :start 400 :duration 200 :volume 0.5})
        locked (ctau/make-audio-clip {:audio-id (uuid/next) :name "locked" :source-duration 1000
                                      :start 700 :duration 200 :locked true})
        louder (ctau/make-audio-clip {:audio-id (uuid/next) :name "louder" :source-duration 1000
                                      :start 950 :duration 40 :volume 2})
        scene  (update scene :rows conj {:type :audio :lane 0 :clips [loud quiet locked louder]})
        hit    #(let [hit (lanes/hit geo scene %1 %2)] [(:mode hit) (:name (:clip hit))])]
    (t/is (= [:volume "loud"] (hit 200 200)) "on its line")
    (t/is (= [:volume "loud"] (hit 200 202)) "close to it")
    (t/is (= [:move "loud"] (hit 200 206)) "away from it, the clip moves")
    (t/is (= [:volume "quiet"] (hit 500 206)) "lower when quieter")
    (t/is (= [:start "loud"] (hit 118 206)) "its ends still trim")
    (t/is (= [:fade-in "loud"] (hit 118 200)) "at its top corner, the grip of its fade")
    (t/is (= [:move "locked"] (hit 800 200)) "a locked clip has none")
    (t/is (= [:volume "louder"] (hit 982 200)) "louder than its file, at the top too")))

(t/deftest the-group-of-the-audio-lanes-is-a-row
  (let [clip  (ctau/make-audio-clip {:audio-id (uuid/next) :name "x" :source-duration 1000
                                     :start 100 :duration 200})
        scene (update scene :rows conj {:type :audio-group :collapsed? true :count 1 :clips [clip]})]
    (t/is (= :row (:type (lanes/hit geo scene 200 206))) "nothing to drag on it")))

(t/deftest clips-have-boxes-for-a-box-selection
  ;; the audio row from 140 in what the view scrolls over (index 5),
  ;; its bars from 145 to 163
  (let [song   (ctau/make-audio-clip {:audio-id (uuid/next) :name "song" :source-duration 1000
                                      :start 100 :duration 200})
        locked (ctau/make-audio-clip {:audio-id (uuid/next) :name "locked" :source-duration 1000
                                      :start 500 :duration 200 :locked true})
        scene  (update scene :rows conj {:type :audio :lane 0 :clips [song locked]})]
    (t/is (= [{:clip-id (:id song) :left 112 :right 312 :top 145 :bottom 163}]
             (lanes/clip-boxes geo scene))
          "but the locked ones")))

(t/deftest blocks-and-tracks-have-boxes-for-a-box-selection
  ;; the rows of `scene`: the layer (top 0, bar from 100 to 400 ms), a
  ;; block (top 28, 500 to 700 ms) and a locked one (top 56)
  (t/is (= [{:shape-id shape-id :left 112 :right 412 :top 5 :bottom 23}]
           (lanes/bar-boxes geo scene)))
  (t/is (= [[512 712 33 51]]
           (mapv (juxt :left :right :top :bottom) (lanes/animation-boxes geo scene)))
        "but the locked ones"))

;; Rows made 48 px tall: their tops at 52, 100, 148, 196 and 244, their
;; middles 24 below, their bars from 5 to 43 px in them.
(t/deftest taller-rows-grow-their-bars
  (let [tall (assoc geo :row-height 48)]
    (t/is (= 28 (lanes/row-height geo)) "as tall as they start")
    (t/is (= 196 (lanes/row-y tall 3)))
    (t/is (= 2 (lanes/row-index tall 150)))
    (t/is (= [0 6] (lanes/visible-rows tall 0)))
    (t/is (= 284 (lanes/scroll-to-rows tall [8])))
    (t/is (= {:type :bar :row 0 :mode :move} (hit-at tall 250 90)) "lower in its bar")
    (t/is (= {:type :row :row 0} (hit-at tall 250 98)) "under its bar")
    (t/is (= {:type :keyframe :row 3 :locked? false :time 100} (hit-at tall 112 220))
          "the keyframes in the middle")
    (t/is (= {:type :easing :row 3 :locked? false :from 100} (hit-at tall 212 215))
          "and the easing button")
    (t/is (= {:type :segment :row 3 :locked? false :from 100} (hit-at tall 212 205)))
    (t/is (= [[112 412 5 43]]
             (mapv (juxt :left :right :top :bottom) (lanes/bar-boxes tall scene))))
    (t/is (= [[512 712 53 91]]
             (mapv (juxt :left :right :top :bottom) (lanes/animation-boxes tall scene))))
    (let [{:keys [top bottom]} (first (lanes/keyframe-boxes tall scene))]
      (t/is (< 160 top 168 bottom 176) "in the middle of the fourth row"))))
