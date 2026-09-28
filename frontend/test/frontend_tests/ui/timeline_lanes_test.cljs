;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.timeline-lanes-test
  (:require
   [app.common.types.animation :as cta]
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
  (t/is (= 1000 (lanes/x->time geo 5000)) "past the end"))

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

(t/deftest the-ruler-and-its-end
  (t/is (= {:type :ruler} (hit-at geo 20 10)))
  (t/is (= {:type :duration} (hit-at geo 812 10)))
  (t/is (= {:type :duration} (hit-at geo 816 10)))
  (t/is (nil? (hit-at geo 20 30)) "the markers are not drawn")
  (t/is (nil? (hit-at geo -1 100)) "off the canvas"))

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
