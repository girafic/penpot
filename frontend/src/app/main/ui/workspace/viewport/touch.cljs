;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.viewport.touch
  "Fingers on the canvas (an iPad, a touch screen): two fingers move it
  and zoom it as they spread, two fingers tapped undo and three redo. A
  finger alone waits `hold-ms` (or until it moves `slop-px`) before its
  tool starts, so that a second finger still makes a gesture of it.
  Once a pen was used, a finger alone moves the canvas and only the pen
  draws and selects (see `touch/fingers-pan?`). A mouse and a pen go to
  the viewport as they come."
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [app.main.ui.workspace.viewport.viewport-ref :as uwvv]
   [app.util.mouse :as mse]
   [app.util.timers :as ts]
   [app.util.touch :as touch]
   [rumext.v2 :as mf]))

(def hold-ms
  "How long a finger alone waits before its tool starts."
  120)

(def slop-px
  "How far a finger moves before it counts as moved."
  6)

(def tap-ms
  "How long a tap of fingers lasts at most."
  300)

(def tap-hover-ms
  "How long after it went down a finger tapped reaches its tool at the
  earliest: the shape under it is found meanwhile."
  60)

(defn middle
  "The middle of the `points` (`{:x :y}`)."
  [points]
  (let [n (count points)]
    (gpt/point (/ (reduce + (map :x points)) n)
               (/ (reduce + (map :y points)) n))))

(defn pinch-step
  "From two fingers at `before` to `after` (two points each): how far
  their middle moved, `:dx :dy` (px), how much more they spread,
  `:scale`, and where their middle is now, `:middle`."
  [before after]
  (let [m0     (middle before)
        m1     (middle after)
        d0     (apply gpt/distance before)
        d1     (apply gpt/distance after)]
    {:dx (- (:x m1) (:x m0))
     :dy (- (:y m1) (:y m0))
     :scale (if (and (pos? d0) (pos? d1)) (/ d1 d0) 1)
     :middle m1}))

(defn view-step
  "The view box at `vbox`, `zoom`, after fingers moved it `dx dy` (px)
  and spread it `scale` about `middle` (px from the corner of the
  viewport): `{:x :y}` of its corner, then the point of the canvas to
  zoom about, which stays under the fingers."
  [{:keys [x y]} zoom dx dy middle]
  (let [x' (- x (/ dx zoom))
        y' (- y (/ dy zoom))]
    {:x x'
     :y y'
     :center (gpt/point (+ x' (/ (:x middle) zoom))
                        (+ y' (/ (:y middle) zoom)))}))

(defn- client-point
  [^js event]
  (gpt/point (.-clientX event) (.-clientY event)))

(defn- move-view!
  "The canvas moved `dx dy` (px) and zoomed `scale` about `middle`
  (client px)."
  [dx dy scale middle]
  (let [state  @st/state
        vbox   (dm/get-in state [:workspace-local :vbox])
        zoom   (dm/get-in state [:workspace-local :zoom] 1)
        corner (or (uwvv/point->viewport-relative middle) middle)
        {:keys [x y center]} (view-step vbox zoom dx dy corner)]
    (when (some? vbox)
      (st/emit! (dw/update-viewport-position {:x (constantly x) :y (constantly y)}))
      (when (not (mth/close? scale 1))
        (st/emit! (dw/set-zoom center scale))))))

(defn- schedule-view!
  "The moves of the fingers gathered until the next frame (as the wheel
  does, see `schedule-zoom!` of the viewport actions)."
  [^js g dx dy scale middle]
  (set! (.-dx g) (+ (.-dx g) dx))
  (set! (.-dy g) (+ (.-dy g) dy))
  (set! (.-scale g) (* (.-scale g) scale))
  (set! (.-middle g) middle)
  (when (zero? (.-raf g))
    (set! (.-raf g)
          (ts/raf
           (fn []
             (let [dx (.-dx g) dy (.-dy g) scale (.-scale g) middle (.-middle g)]
               (set! (.-dx g) 0)
               (set! (.-dy g) 0)
               (set! (.-scale g) 1)
               (set! (.-raf g) 0)
               (move-view! dx dy scale middle)))))))

(defn- fingers
  "The points of the fingers down, in the order they came."
  [^js g]
  (vec (vals (.-points g))))

(defn- end-tool!
  "The tool a finger started ended where it is: a second finger makes a
  gesture of it."
  []
  (st/emit! (mse/->MouseEvent :up false false false false)
            (dw/finish-panning)
            (dw/finish-zooming)))

(defn use-touch-handlers
  "The pointer handlers of the viewport, `on-pointer-down`,
  `on-pointer-move` and `on-pointer-up`, and its `on-click`, with the
  fingers in between (see the namespace): `[on-pointer-down
  on-pointer-move on-pointer-up on-pointer-cancel on-click]`. The click
  of a finger tapped waits for its tap to reach the tool."
  [on-pointer-down on-pointer-move on-pointer-up on-click]
  (let [handlers-ref (mf/use-ref nil)
        g-ref        (mf/use-ref nil)
        _            (mf/set-ref-val! handlers-ref #js {:down on-pointer-down
                                                        :move on-pointer-move
                                                        :up on-pointer-up
                                                        :click on-click})
        ^js g        (or (mf/ref-val g-ref)
                         (let [g #js {:points (sorted-map) :order 0 :mode :idle
                                      :held nil :timer nil :start nil :moved false
                                      :count 0 :t0 0 :last nil :tapping false :click nil
                                      :dx 0 :dy 0 :scale 1 :middle nil :raf 0}]
                           (mf/set-ref-val! g-ref g)
                           g))

        handler     (fn [k] (unchecked-get (mf/ref-val handlers-ref) k))

        clear-timer!
        (fn []
          (when-let [timer (.-timer g)]
            (ts/dispose! timer)
            (set! (.-timer g) nil)))

        ;; the finger alone gets its tool: where it went down, then where
        ;; it is
        start-tool!
        (fn [^js latest]
          (clear-timer!)
          (when-let [^js held (.-held g)]
            (set! (.-held g) nil)
            (set! (.-mode g) :tool)
            ((handler "down") held)
            (when (and (some? latest) (not (identical? latest held)))
              ((handler "move") latest))))

        start-gesture!
        (fn []
          (clear-timer!)
          (when (= :tool (.-mode g))
            (end-tool!))
          (set! (.-held g) nil)
          (set! (.-mode g) :gesture)
          (set! (.-last g) (fingers g))
          (set! (.-count g) (max (.-count g) (count (.-points g)))))

        on-down
        (mf/use-fn
         (fn [^js event]
           (if (not= "touch" (.-pointerType event))
             ((handler "down") event)
             (let [id  (.-pointerId event)
                   pt  (client-point event)
                   n   (count (.-points g))]
               (set! (.-points g) (assoc (.-points g) [(.-order g) id] pt))
               (set! (.-order g) (inc (.-order g)))
               (if (zero? n)
                 (do (set! (.-start g) pt)
                     (set! (.-t0 g) (js/Date.now))
                     (set! (.-moved g) false)
                     (set! (.-count g) 1)
                     (if (touch/fingers-pan?)
                       (do (set! (.-mode g) :pan)
                           (set! (.-last g) [pt]))
                       (do (set! (.-mode g) :pending)
                           (set! (.-held g) event)
                           ;; where it is, now: the shape under it is
                           ;; found meanwhile, as under a mouse that moved
                           ;; there
                           ((handler "move") event)
                           (set! (.-timer g) (ts/schedule hold-ms #(start-tool! nil))))))
                 (start-gesture!))))))

        on-move
        (mf/use-fn
         (fn [^js event]
           (if (not= "touch" (.-pointerType event))
             ((handler "move") event)
             (let [id  (.-pointerId event)
                   pt  (client-point event)
                   k   (some #(when (= id (second %)) %) (keys (.-points g)))]
               (when (some? k)
                 (set! (.-points g) (assoc (.-points g) k pt))
                 (when (and (some? (.-start g))
                            (> (gpt/distance pt (.-start g)) slop-px))
                   (set! (.-moved g) true))
                 (case (.-mode g)
                   :pending (when (.-moved g) (start-tool! event))
                   :tool    ((handler "move") event)
                   :pan     (let [[before] (.-last g)]
                              (set! (.-last g) [pt])
                              (schedule-view! g (- (:x pt) (:x before)) (- (:y pt) (:y before)) 1 pt))
                   :gesture (let [before (.-last g)
                                  after  (fingers g)]
                              (set! (.-last g) after)
                              (when (and (= 2 (count before)) (= 2 (count after)))
                                (let [{:keys [dx dy scale middle]} (pinch-step before after)]
                                  (when (or (> (mth/abs dx) 0) (> (mth/abs dy) 0) (not (mth/close? scale 1)))
                                    (when (or (> (mth/abs (- scale 1)) 0.03)
                                              (> (+ (mth/abs dx) (mth/abs dy)) slop-px))
                                      (set! (.-moved g) true))
                                    (schedule-view! g dx dy scale middle)))))
                   nil))))))

        on-up
        (mf/use-fn
         (fn [^js event]
           (if (not= "touch" (.-pointerType event))
             ((handler "up") event)
             (let [id (.-pointerId event)
                   k  (some #(when (= id (second %)) %) (keys (.-points g)))]
               (when (some? k)
                 (set! (.-points g) (dissoc (.-points g) k))
                 (case (.-mode g)
                   ;; a tap: its tool starts and ends at once, once the
                   ;; shape under it is known
                   :pending (let [^js held (.-held g)
                                  wait     (- tap-hover-ms (- (js/Date.now) (.-t0 g)))
                                  tap!     (fn []
                                             ((handler "down") held)
                                             ((handler "up") event)
                                             (set! (.-tapping g) false)
                                             (when-let [click (.-click g)]
                                               (set! (.-click g) nil)
                                               ((handler "click") click)))]
                              (clear-timer!)
                              (set! (.-held g) nil)
                              (if (pos? wait)
                                (do (set! (.-tapping g) true)
                                    (ts/schedule wait tap!))
                                (tap!)))
                   :tool    ((handler "up") event)
                   nil)
                 (if (empty? (.-points g))
                   (do
                     ;; fingers tapped together: two undo, three redo
                     (when (and (= :gesture (.-mode g))
                                (not (.-moved g))
                                (< (- (js/Date.now) (.-t0 g)) tap-ms))
                       (case (.-count g)
                         2 (st/emit! dwu/undo)
                         3 (st/emit! dwu/redo)
                         nil))
                     (clear-timer!)
                     (set! (.-mode g) :idle)
                     (set! (.-held g) nil)
                     (set! (.-start g) nil))
                   ;; one finger left of a gesture: it waits for the others
                   (set! (.-last g) (fingers g))))))))

        ;; the click of a tap that waits comes after it
        on-tap-click
        (mf/use-fn
         (fn [event]
           (if (.-tapping g)
             (set! (.-click g) event)
             ((handler "click") event))))]

    [on-down on-move on-up on-up on-tap-click]))
