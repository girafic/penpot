;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.drag-scroll
  "The screen of a touch device (see `app.main.data.viewer.devices/touch?`)
  scrolls when dragged, as a finger scrolls it: it follows the pointer,
  then coasts on and slows down once let go. Dragged past an end, or
  pushed past it with a wheel or a trackpad, it gives like rubber and
  springs back; a coast that runs into an end bounces off it. A tap there acts when it is let go, and only if it did
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
  as long."
  325)

(def ^:private min-speed
  "The speed, px/ms, a coast stops at."
  0.02)

(def ^:private stretch
  "How stiff the rubber is past an end: the less, the stiffer."
  0.55)

(def ^:private spring-ms
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

(def ^:private wheel-reach
  "How far, as a share of its height, a wheel or a trackpad pushes a
  screen past an end at most, as a finger drags it no further than across
  it: the swing of a trackpad goes on long after it is let go."
  0.3)

(defn wheel-past
  "How far, px, a wheel or a trackpad has pushed a screen scrolled `top`
  px of `limit` past an end, `past` before and `dy` more now, `reach` px
  at most: less than 0 past the top, more past the bottom, 0 between the
  ends."
  [past top limit dy reach]
  (cond
    (neg? past) (mth/clamp (+ past dy) (- reach) 0)
    (pos? past) (mth/clamp (+ past dy) 0 reach)
    (and (<= top 0) (neg? dy)) (max (- reach) dy)
    (and (>= top limit) (pos? dy)) (min reach dy)
    :else 0))

(defn past-offset
  "How far the content of a screen `size` px high moves down (up when less
  than 0) pushed `past` px past an end (see `wheel-past`)."
  [past size]
  (cond
    (neg? past) (rubber (- past) size)
    (pos? past) (- (rubber past size))
    :else 0))

(def ^:private wheel-rest-ms
  "How long, ms, a wheel past an end stays still before the screen springs
  back."
  90)

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

(defn dragged?
  "Whether the last press on the screen of a touch device dragged it."
  []
  (true? (:dragged? @state*)))

(defn- swallow-click!
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
              ;; how far a wheel pushed past an end, and when it springs back
              past*    (volatile! 0)
              rest*    (volatile! nil)

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
                  (if (zero? (.-buttons event))
                    ;; let go where it went unheard
                    (do (vreset! gesture* nil)
                        (set! (.. section -style -cursor) "")
                        (when-not (zero? @offset*) (spring! @offset*)))
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
                  (some-> @rest* js/clearTimeout)
                  (vreset! past* 0)
                  (swap! state* assoc :dragged? false)
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

              ;; a wheel or a trackpad past an end gives as a drag does, and
              ;; springs back once it rests
              on-wheel
              (fn [event]
                (stop!)
                (some-> @rest* js/clearTimeout)
                (let [size (.-clientHeight section)
                      past (wheel-past @past* (.-scrollTop section) (max-scroll) (.-deltaY event)
                                       (* wheel-reach size))]
                  (vreset! past* past)
                  (offset! (past-offset past size))
                  (when-not (zero? past)
                    ;; the screen can scroll no further
                    (.preventDefault event)
                    (vreset! rest* (js/setTimeout (fn []
                                                    (vreset! past* 0)
                                                    (spring! @offset*))
                                                  wheel-rest-ms)))))]

          (.addEventListener section "pointerdown" on-down)
          (.addEventListener section "wheel" on-wheel #js {:passive false})
          (fn []
            (.removeEventListener section "pointerdown" on-down)
            (.removeEventListener section "wheel" on-wheel #js {:passive false})
            (some-> @rest* js/clearTimeout)
            (.removeEventListener js/window "pointermove" on-move true)
            (.removeEventListener js/window "pointerup" on-up true)
            (.removeEventListener js/window "pointercancel" on-up true)
            (stop!)
            (set-offset! section 0 nil)
            (set! (.. section -style -cursor) "")
            (reset! state* nil)))))))
