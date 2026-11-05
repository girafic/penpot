;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.drag-scroll
  "The screen of a touch device (see `app.main.data.viewer.devices/touch?`)
  scrolls when dragged, as a finger scrolls it: it follows the pointer,
  then coasts on and slows down once let go. Dragged past an end, it
  gives like rubber and springs back; a coast that runs into an end
  bounces off it. A wheel or a trackpad scrolls it as far as an end. A tap there acts when it is let go, and only if it did
  not drag (see `defer-taps?`, read by the hotspots of
  `app.main.ui.viewer.shapes`)."
  (:require
   [app.common.math :as mth]
   [rumext.v2 :as mf]))

(def threshold
  "How far, px, a press moves before it drags."
  6)

(def ^:private window-ms
  "How far back, ms, the speed a drag lets go at is measured."
  100)

(def ^:private time-constant
  "How fast, ms, a coast slows down: it keeps a third of its speed after
  as long. As iOS does, which keeps 0.998 of it each ms (-1 / ln 0.998)."
  500)

(def min-speed
  "The speed, px/ms, a coast stops at."
  0.02)

(def ^:private stretch
  "How stiff the rubber is past an end: the less, the stiffer."
  0.55)

(def spring-ms
  "How fast, ms, the screen springs back to an end, and a bounce turns."
  90)

(def ^:private max-bounce-speed
  "The speed, px/ms, past which a coast bounces no further off an end."
  4)

(defn start
  "A press at `y` px at `t` ms on a screen scrolled `top` px."
  [y top t]
  {:y0 y :top0 top :samples [[t y]] :dragging? false})

(defn move
  "The press `gesture` at `y` at `t`: dragging once it moved further
  than `threshold`, and then the screen scrolled to `:top`, past an end
  too (see `split-top`)."
  [gesture y t]
  (let [dy        (- y (:y0 gesture))
        dragging? (or (:dragging? gesture) (> (mth/abs dy) threshold))]
    (-> gesture
        (assoc :dragging? dragging?
               :top (when dragging? (- (:top0 gesture) dy)))
        (update :samples (fn [samples]
                           (filterv (fn [[st]] (> st (- t window-ms)))
                                    (conj samples [t y])))))))

(defn speed
  "The speed, px/ms, the screen of `gesture` scrolls at when it is let go
  at `t`: the pointer's over the last `window-ms` it moved, the other
  way; 0 once it stood still that long."
  [gesture t]
  (let [[last-t] (peek (:samples gesture))
        samples  (filterv (fn [[st]] (> st (- last-t window-ms))) (:samples gesture))]
    (if (or (< (count samples) 2) (> (- t last-t) window-ms))
      0
      (let [[t0 y0] (first samples)
            [t1 y1] (peek samples)]
        (if (> t1 t0)
          (- (/ (- y1 y0) (- t1 t0)))
          0)))))

(defn coast
  "How far a coast at `v` px/ms goes in `dt` ms, and its speed then:
  `[distance v]`."
  [v dt]
  (let [decay (js/Math.exp (- (/ dt time-constant)))]
    [(* v time-constant (- 1 decay)) (* v decay)]))

(defn rubber
  "How far, px, a screen `size` px high gives when dragged `x` px past an
  end: less and less, never as far as its size."
  [x size]
  (* size (- 1 (/ 1 (+ 1 (/ (* x stretch) size))))))

(defn unrubber
  "How far past an end a screen `size` px high was dragged to give `r` px
  (see `rubber`)."
  [r size]
  (* (/ size stretch) (- (/ 1 (- 1 (/ (min r (* 0.99 size)) size))) 1)))

(defn split-top
  "The screen `size` px high, that scrolls `max` px, dragged to `top`:
  `{:scroll :offset}`, how far it scrolls and how far its content moves
  down (up when less than 0) past an end, giving as rubber."
  [top max size]
  (cond
    (neg? top)  {:scroll 0 :offset (rubber (- top) size)}
    (> top max) {:scroll max :offset (- (rubber (- top max) size))}
    :else       {:scroll top :offset 0}))

(defn spring-back
  "Where content moved `x0` px past an end is `t` ms after it is let go:
  it springs back without swinging past the end."
  [x0 t]
  (* x0 (+ 1 (/ t spring-ms)) (js/Math.exp (- (/ t spring-ms)))))

(defn bounce
  "Where content is `t` ms after a coast at `v` px/ms ran into an end: past
  it, then back."
  [v t]
  (let [v (mth/clamp v (- max-bounce-speed) max-bounce-speed)]
    (* (- v) t (js/Math.exp (- (/ t spring-ms))))))

;; Whether a touch device shows (`some?`), and whether its last press
;; dragged: what the hotspots read.
(defonce ^:private state* (atom nil))

(defn defer-taps?
  "Whether a click acts on release, a touch device showing."
  []
  (some? @state*))

;; Whether the last finger on a real touch screen dragged (see
;; `note-touch-drag`): the screen scrolls by itself there.
(defonce ^:private touch-dragged* (atom false))

(defn note-touch-start
  "A finger on a real touch screen comes down: it dragged nothing yet."
  []
  (reset! touch-dragged* false))

(defn note-touch-drag
  "The finger on a real touch screen dragged: its release is no tap."
  []
  (reset! touch-dragged* true))

(defn dragged?
  "Whether the last press on the screen of a touch device dragged it, or
  the last finger on a real touch screen dragged."
  []
  (or (true? (:dragged? @state*)) @touch-dragged*))

(defn drag-axis
  "The axis a press dragged from `[x0 y0]` to `[x y]` goes along, once it
  moved further than `threshold`: the one it moved more along; nil
  before."
  [[x0 y0] [x y]]
  (let [dx (mth/abs (- x x0))
        dy (mth/abs (- y y0))]
    (when (> (max dx dy) threshold)
      (if (> dx dy) :x :y))))

(defn claim-press
  "Take the press going on from the screen, for a board in it that
  scrolls its content (see `app.main.ui.viewer.container-scroll`): the
  screen leaves it from the move that claims it on, and it counts as
  dragged, so it is no tap. A touch device showing only."
  []
  (when (some? @state*)
    (swap! state* assoc :claimed true :dragged? true)))

(defn claimed?
  "Whether a board took the press going on (see `claim-press`)."
  []
  (true? (:claimed @state*)))

(defn swallow-click!
  "Keep the click that follows a drag from anything: it was no tap."
  []
  (letfn [(swallow [event]
            (.stopPropagation event)
            (.preventDefault event)
            (.removeEventListener js/window "click" swallow true))]
    (.addEventListener js/window "click" swallow true)
    ;; the click comes in the same task as the release, or not at all
    (js/setTimeout #(.removeEventListener js/window "click" swallow true) 0)))

(defn- set-offset!
  "Moves what scrolls in `section` `offset` px down past an end (see the
  styles of `.in-device[data-overscroll]`), over `color`."
  [section offset color]
  (let [style (.-style section)]
    (if (< (mth/abs offset) 0.5)
      (do (.removeProperty style "--overscroll")
          (.removeProperty style "--overscroll-color")
          (.removeAttribute section "data-overscroll"))
      (do (.setProperty style "--overscroll" (str offset "px"))
          (when color (.setProperty style "--overscroll-color" color))
          (.setAttribute section "data-overscroll" "")))))

(defn use-drag-scroll!
  "Lets the element of `section-ref` scroll by dragging it with a mouse
  or a pen while `enabled?` (a touch screen scrolls by itself). Past an
  end it shows `color`, the background of the board."
  [section-ref enabled? color]
  (let [color-ref (mf/use-ref color)]
    (mf/set-ref-val! color-ref color)
    (mf/with-effect [enabled?]
      (when-let [section (when enabled? (mf/ref-val section-ref))]
        (reset! state* {:dragged? false})
        (let [gesture* (volatile! nil)
              frame*   (volatile! nil)
              offset*  (volatile! 0)

              max-scroll #(- (.-scrollHeight section) (.-clientHeight section))

              offset!
              (fn [offset]
                (vreset! offset* offset)
                (set-offset! section offset (mf/ref-val color-ref)))

              stop!
              (fn []
                (some-> @frame* js/cancelAnimationFrame)
                (vreset! frame* nil))

              ;; `f` of the ms since it started, each frame, until it gives nil
              animate!
              (fn [f]
                (stop!)
                (let [t0 (js/performance.now)]
                  (letfn [(step [now]
                            (if (f (- now t0))
                              (vreset! frame* (js/requestAnimationFrame step))
                              (vreset! frame* nil)))]
                    (vreset! frame* (js/requestAnimationFrame step)))))

              spring!
              (fn [x0]
                (animate! (fn [t]
                            (let [x (spring-back x0 t)]
                              (offset! (if (< (mth/abs x) 0.5) 0 x))
                              (>= (mth/abs x) 0.5)))))

              bounce!
              (fn [v]
                (animate! (fn [t]
                            (let [x (bounce v t)
                                  done? (and (> t (* 2 spring-ms)) (< (mth/abs x) 0.5))]
                              (offset! (if done? 0 x))
                              (not done?)))))

              coast!
              (fn [v]
                (let [state (volatile! {:v v :last 0})]
                  (animate! (fn [t]
                              (let [{:keys [v last]} @state
                                    [dist v] (coast v (- t last))
                                    limit    (max-scroll)
                                    top      (+ (.-scrollTop section) dist)]
                                (vreset! state {:v v :last t})
                                (cond
                                  ;; it runs into an end, and bounces off it
                                  (or (neg? top) (> top limit))
                                  (do (set! (.-scrollTop section) (mth/clamp top 0 limit))
                                      (js/setTimeout #(bounce! v) 0)
                                      false)

                                  :else
                                  (do (set! (.-scrollTop section) top)
                                      (> (mth/abs v) min-speed))))))))

              on-move
              (fn [event]
                (when-let [gesture @gesture*]
                  (cond
                    ;; a board in the screen took it (see `claim-press`)
                    (claimed?)
                    (do (vreset! gesture* nil)
                        (set! (.. section -style -cursor) ""))

                    (zero? (.-buttons event))
                    ;; let go where it went unheard
                    (do (vreset! gesture* nil)
                        (set! (.. section -style -cursor) "")
                        (when-not (zero? @offset*) (spring! @offset*)))

                    :else
                    (let [gesture (move gesture (.-clientY event) (.-timeStamp event))]
                      (vreset! gesture* gesture)
                      (when (:dragging? gesture)
                        (when-not (:dragged? @state*)
                          (swap! state* assoc :dragged? true)
                          (set! (.. section -style -cursor) "grabbing"))
                        (let [{:keys [scroll offset]}
                              (split-top (:top gesture) (max-scroll) (.-clientHeight section))]
                          (set! (.-scrollTop section) scroll)
                          (offset! offset)))))))

              on-up
              (fn on-up [event]
                (.removeEventListener js/window "pointermove" on-move true)
                (.removeEventListener js/window "pointerup" on-up true)
                (.removeEventListener js/window "pointercancel" on-up true)
                (when-let [gesture @gesture*]
                  (vreset! gesture* nil)
                  (when (:dragging? gesture)
                    (set! (.. section -style -cursor) "")
                    (swallow-click!)
                    (let [v (speed gesture (.-timeStamp event))]
                      (cond
                        (not (zero? @offset*)) (spring! @offset*)
                        (> (mth/abs v) min-speed) (coast! v))))))

              on-down
              (fn [event]
                (when (and (zero? (.-button event))
                           (not= "touch" (.-pointerType event)))
                  (stop!)
                  (swap! state* assoc :dragged? false :claimed false)
                  ;; caught past an end: the drag goes on from there
                  (let [offset @offset*
                        size   (.-clientHeight section)
                        top    (cond-> (.-scrollTop section)
                                 (pos? offset) (- (unrubber offset size))
                                 (neg? offset) (+ (unrubber (- offset) size)))]
                    (vreset! gesture* (start (.-clientY event) top (.-timeStamp event))))
                  (.addEventListener js/window "pointermove" on-move true)
                  (.addEventListener js/window "pointerup" on-up true)
                  (.addEventListener js/window "pointercancel" on-up true)))

              ;; a wheel or a trackpad scrolls as the browser does, as far
              ;; as an end: it stops a coast, and a screen a drag left
              ;; past an end goes back to it at once
              on-wheel
              (fn [_]
                (stop!)
                (when-not (zero? @offset*)
                  (offset! 0)))]

          (.addEventListener section "pointerdown" on-down)
          (.addEventListener section "wheel" on-wheel #js {:passive false})
          (fn []
            (.removeEventListener section "pointerdown" on-down)
            (.removeEventListener section "wheel" on-wheel #js {:passive false})
            (.removeEventListener js/window "pointermove" on-move true)
            (.removeEventListener js/window "pointerup" on-up true)
            (.removeEventListener js/window "pointercancel" on-up true)
            (stop!)
            (set-offset! section 0 nil)
            (set! (.. section -style -cursor) "")
            (reset! state* nil)))))))
