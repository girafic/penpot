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
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

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
    (t/is (mth/close? 500 (first (dds/coast 1 1e9)) 0.001) "as far as the time constant at most, as on iOS")
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

(t/deftest a-press-goes-along-the-axis-it-dragged-more-along
  (t/is (nil? (dds/drag-axis [100 100] [104 103])) "within the threshold, no axis yet")
  (t/is (= :y (dds/drag-axis [100 100] [103 120])))
  (t/is (= :x (dds/drag-axis [100 100] [80 104])) "a chip row dragged sideways"))

(t/deftest a-board-of-the-screen-takes-the-press-from-it
  (t/is (nil? (dds/claim-press)) "without a touch device, nothing to take")
  (t/is (not (dds/claimed?))))

(t/deftest a-finger-that-dragged-makes-no-tap
  (dds/note-touch-start)
  (t/is (not (dds/dragged?)) "a finger comes down")
  (dds/note-touch-drag)
  (t/is (dds/dragged?) "it dragged: its release is no tap")
  (dds/note-touch-start)
  (t/is (not (dds/dragged?)) "the next one starts afresh"))

;; --- A phone (see `dds/use-touch-rubber!`)

(mf/defc rubber-section*
  {::mf/private true}
  []
  (let [section-ref (mf/use-ref nil)]
    (dds/use-touch-rubber! section-ref true "#f5efe6")
    [:div {:ref section-ref :id "section"}]))

(defn- touch!
  "A finger `type` (\"touchstart\", \"touchmove\") at `y` on `node`."
  [^js window node type y]
  (let [event (new (.-Event window) type #js {:bubbles true :cancelable true})
        touch #js {:identifier 1 :clientY y}]
    (set! (.-touches event) #js [touch])
    (set! (.-changedTouches event) #js [touch])
    (thd/act #(.dispatchEvent node event))))

(defn- pulled
  "How far down past its top a section 600 px high that scrolls 400 px
  gives as a finger drags it 200 px down from the top: its offset, nil
  for none. `taken?`: a board in it took the finger (see
  `app.main.ui.viewer.container-scroll`)."
  [taken?]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (thd/render! root (mf/element rubber-section*))
      (let [section (.getElementById document "section")]
        (js/Object.defineProperty section "scrollHeight" #js {:value 1000})
        (js/Object.defineProperty section "clientHeight" #js {:value 600})
        (when taken?
          (.addEventListener section "touchmove" #(.preventDefault %) #js {:capture true}))
        (touch! window section "touchstart" 100)
        (touch! window section "touchmove" 300)
        (when (.hasAttribute section "data-overscroll")
          (js/parseFloat (.. section -style (getPropertyValue "--overscroll"))))))))

(t/deftest a-phone-gives-past-an-end-as-rubber
  ;; the bounce of Safari moved the layers fixed when scrolling with the
  ;; board: only what scrolls moves (see `.viewer-section[data-overscroll]`)
  (let [offset (pulled false)]
    (t/is (mth/close? (dds/rubber 200 600) offset) "down past its top, less than the finger")
    (t/is (< 0 offset 200)))
  (t/is (nil? (pulled true)) "a finger a board in it took is none of its"))


;; --- A finger that scrolled the screen (see `dds/listen-touch-taps!`)

(defn- finger!
  "A finger's pointer event `type` at `[x y]` on `target`."
  [target type [x y]]
  (let [event (js/Event. type)]
    (set! (.-pointerType event) "touch")
    (set! (.-pointerId event) 7)
    (set! (.-clientX event) x)
    (set! (.-clientY event) y)
    (.dispatchEvent target event)))

(defn- scroll! [target]
  (.dispatchEvent target (js/Event. "scroll")))

(defn- tapped?
  "Whether a finger down at `[100 100]` and up at `up`, with `between`
  done to the screen meanwhile, tapped as it lifted. `before`: done to
  the screen before the finger comes down."
  [screen & {:keys [up before between] :or {up [100 100]}}]
  (when before (before screen))
  (finger! screen "pointerdown" [100 100])
  (when between (between screen))
  (finger! screen "pointerup" up)
  (not (dds/dragged?)))

(t/deftest a-finger-that-scrolled-the-screen-taps-nothing
  (let [screen (js/EventTarget.)
        stop!  (dds/listen-touch-taps! screen)]
    (try
      (t/is (tapped? screen :up [103 102]) "a tap trembles")
      (t/is (not (tapped? screen :up [100 40]))
            "it moved: Safari sends no move as the screen scrolls, then lets go")
      (t/is (not (tapped? screen :between #(finger! % "pointermove" [100 60])))
            "it moved, and came back")
      (finger! screen "pointerdown" [100 100])
      (finger! screen "pointercancel" [100 100])
      (t/is (tapped? screen) "a press the browser took away leaves the next alone")
      ;; last: the screen coasts a while after it scrolled
      (t/is (not (tapped? screen :between scroll!)) "the screen scrolled under it")
      (t/is (not (tapped? screen :before scroll!)) "it stopped the screen coasting")
      (finally (stop!)))))

(t/deftest a-tap-once-the-screen-stood-still-acts
  (t/async
    done
    (let [screen (js/EventTarget.)
          stop!  (dds/listen-touch-taps! screen)]
      (scroll! screen)
      (js/setTimeout (fn []
                       (t/is (tapped? screen) "it coasted no more")
                       (stop!)
                       (done))
                     150))))
