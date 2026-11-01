;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.drag-scroll-test
  "Dragging the screen of a touch device: when a press drags, where the
  screen scrolls to, and how it coasts on once let go."
  (:require
   [app.common.math :as mth]
   [app.main.ui.viewer.drag-scroll :as dds]
   [cljs.test :as t :include-macros true]))

(t/deftest a-press-drags-once-it-moves-far-enough
  (let [g (dds/start 300 0 0)]
    (t/is (false? (:dragging? (dds/move g (+ 300 dds/threshold) 10))) "a tap trembles")
    (t/is (nil? (:top (dds/move g 303 10))) "and scrolls nothing")
    (let [g (dds/move g 280 20)]
      (t/is (true? (:dragging? g)))
      (t/is (= 20 (:top g)) "the screen follows the pointer up")
      (t/is (true? (:dragging? (dds/move g 298 30))) "and keeps dragging back near the start")
      (t/is (= 2 (:top (dds/move g 298 30)))))))

(t/deftest the-screen-scrolls-from-where-it-was
  (t/is (= 440 (:top (dds/move (dds/start 300 400 0) 260 16)))))

(t/deftest a-drag-lets-go-at-its-speed
  (let [g (reduce (fn [g i] (dds/move g (- 300 (* 2 i)) (* 10 i)))
                  (dds/start 300 0 0)
                  (range 1 20))]
    (t/is (mth/close? 0.2 (dds/speed g 190)) "up 2 px each 10 ms: the screen scrolls down")
    (t/is (mth/close? 0.2 (dds/speed g 250)) "let go just after it moved last")
    (t/is (zero? (dds/speed g 400)) "held still before letting go")))

(t/deftest a-coast-slows-down
  (let [[d1 v1] (dds/coast 1 16)
        [d2 v2] (dds/coast v1 16)]
    (t/is (< 0 d2 d1 16) "less far each frame")
    (t/is (< 0 v2 v1 1))
    (t/is (mth/close? 325 (first (dds/coast 1 1e9)) 0.001) "as far as the time constant at most")
    (t/is (neg? (first (dds/coast -1 16))) "back up too")))

(t/deftest past-an-end-the-screen-gives-like-rubber
  (t/is (= {:scroll 120 :offset 0} (dds/split-top 120 900 800)) "between the ends")
  (let [{:keys [scroll offset]} (dds/split-top -100 900 800)]
    (t/is (zero? scroll) "at the top")
    (t/is (< 50 offset 100) "its content moves down, less far than dragged"))
  (let [{:keys [scroll offset]} (dds/split-top 1100 900 800)]
    (t/is (= 900 scroll) "at the bottom")
    (t/is (< -200 offset -50) "its content moves up"))
  (t/is (< (dds/rubber 100 800) (dds/rubber 200 800) (dds/rubber 10000 800) 800)
        "further and further, never as far as the screen is high")
  (t/is (mth/close? 250 (dds/unrubber (dds/rubber 250 800) 800) 0.001)
        "how far it was dragged, from how far it gave"))

(t/deftest a-wheel-pushes-past-an-end-too
  (t/is (zero? (dds/wheel-past 0 300 900 40)) "between the ends it scrolls")
  (t/is (zero? (dds/wheel-past 0 0 900 40)) "down from the top too")
  (t/is (= -40 (dds/wheel-past 0 0 900 -40)) "up at the top it pushes past")
  (t/is (= 40 (dds/wheel-past 0 900 900 40)) "down at the bottom")
  (t/is (= -70 (dds/wheel-past -40 0 900 -30)) "further")
  (t/is (zero? (dds/wheel-past -40 0 900 100)) "back, never past the other way")
  (t/is (pos? (dds/past-offset -70 800)) "past the top the content moves down")
  (t/is (neg? (dds/past-offset 70 800)) "past the bottom up")
  (t/is (zero? (dds/past-offset 0 800))))

(t/deftest it-springs-back-and-bounces-off-an-end
  (t/is (= 40 (dds/spring-back 40 0)) "from where it was let go")
  (t/is (< 0 (dds/spring-back 40 200) 40) "back, never past the end")
  (t/is (< (dds/spring-back 40 1000) 0.5) "home in a second")
  (t/is (zero? (dds/bounce 2 0)) "a coast runs into the end")
  (t/is (neg? (dds/bounce 2 90)) "running down, its content goes on up past it")
  (t/is (pos? (dds/bounce -2 90)) "running up, down past it")
  (t/is (< (mth/abs (dds/bounce 2 1000)) 0.5) "and comes back")
  (t/is (= (dds/bounce 4 90) (dds/bounce 40 90)) "as far at most"))

(t/deftest taps-act-on-release-only-on-a-touch-device
  (t/is (false? (dds/defer-taps?)) "no device shows")
  (t/is (false? (dds/dragged?))))
