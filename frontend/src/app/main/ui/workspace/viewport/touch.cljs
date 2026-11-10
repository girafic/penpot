;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.viewport.touch
  "Fingers on the canvas (an iPad, a touch screen): two fingers move it
  and zoom it as they spread, two fingers tapped undo and three redo, a
  finger held still opens the context menu where it is. A
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

(def long-press-ms
  "How long a finger stays still for its context menu."
  500)

(def hover-wait-ms
  "How long a finger waits at most for the shape under it to be found
  (see `uwvv/hover-answered`) before its tool gets it: the first time,
  the index of the shapes may still be built."
  500)

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
        {:keys [x y center]} (touch/view-step vbox zoom dx dy corner)]
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

(defn- replayed?
  "Whether `event` is the press of a finger given to its target late (see
  `replay!`)."
  [^js event]
  (true? (.-penpotReplay (.-nativeEvent event))))

(defn- replay!
  "The press of a finger on a part of the canvas with its own handler (a
  handle to resize or rotate, a title): given to it as it came, once the
  finger waited (see `use-touch-handlers`)."
  [^js event]
  (let [native (.-nativeEvent event)
        target (.-target native)
        copy   (js/PointerEvent. "pointerdown" native)]
    (set! (.-penpotReplay copy) true)
    (.dispatchEvent target copy)))

(defn- end-tool!
  "The tool a finger started ended where it is: a second finger makes a
  gesture of it."
  []
  (st/emit! (mse/->MouseEvent :up false false false false)
            (dw/finish-panning)
            (dw/finish-zooming)))

(defn use-touch-handlers
  "The pointer handlers of the viewport, `on-pointer-down`,
  `on-pointer-move` and `on-pointer-up`, its `on-click` and its
  `on-context-menu`, with the fingers in between (see the namespace):
  `[on-pointer-down on-pointer-move on-pointer-up on-pointer-cancel
  on-click on-context-menu on-pointer-down-capture]`. The click of a
  finger tapped waits for its tap to reach the tool; a finger held opens
  the context menu itself. A finger on a part of the canvas with its own
  handler (a handle) waits as well (`on-pointer-down-capture`, its press
  replayed to it later): with a second finger it makes a gesture too."
  [on-pointer-down on-pointer-move on-pointer-up on-click on-context-menu]
  (let [handlers-ref (mf/use-ref nil)
        g-ref        (mf/use-ref nil)
        _            (mf/set-ref-val! handlers-ref #js {:down on-pointer-down
                                                        :move on-pointer-move
                                                        :up on-pointer-up
                                                        :click on-click
                                                        :menu on-context-menu})
        ^js g        (or (mf/ref-val g-ref)
                         (let [g #js {:points (sorted-map) :order 0 :mode :idle
                                      :held nil :timer nil :start nil :moved false
                                      :count 0 :t0 0 :last nil :tapping false :click nil
                                      :press nil :press-timer nil :other false :latest nil
                                      :starting nil :lifted nil :rendered 0
                                      :dx 0 :dy 0 :scale 1 :middle nil :raf 0}]
                           (mf/set-ref-val! g-ref g)
                           g))

        _           (set! (.-rendered g) (js/performance.now))

        handler     (fn [k] (unchecked-get (mf/ref-val handlers-ref) k))

        clear-timer!
        (fn []
          (when-let [timer (.-timer g)]
            (ts/dispose! timer)
            (set! (.-timer g) nil)))

        clear-press!
        (fn []
          (when-let [timer (.-press-timer g)]
            (ts/dispose! timer)
            (set! (.-press-timer g) nil)))

        ;; the finger held still: its tool ends, its context menu opens
        ;; where it is (as a right click there would)
        long-press!
        (fn []
          (set! (.-press-timer g) nil)
          (when (and (= 1 (count (.-points g))) (not (.-moved g)))
            (clear-timer!)
            (when (= :tool (.-mode g))
              (end-tool!))
            ;; a tool still waiting for the shape under it never comes
            (set! (.-starting g) nil)
            (set! (.-held g) nil)
            (set! (.-mode g) :menu)
            (touch/menu-opened!)
            ((handler "menu") (.-press g))))

        ;; the press of the finger to what it is on: the canvas, or a
        ;; part of it with its own handler
        deliver-down!
        (fn [^js held]
          (if (.-other g)
            (replay! held)
            ((handler "down") held)))

        ;; once the shape under the finger is found, and the viewport
        ;; drew it as hovered when it changed, its handlers knowing it
        ;; (or it waited too long for it): `f`
        when-hovered
        (fn when-hovered [f]
          (let [[answered changed?] @uwvv/hover-answered
                now (js/performance.now)]
            (if (or (and (>= answered (.-t0 g))
                         (or (not changed?) (>= (.-rendered g) answered)))
                    (> (- now (.-t0 g)) hover-wait-ms))
              (f)
              (ts/schedule 16 #(when-hovered f)))))

        ;; the finger alone gets its tool, once the shape under it is
        ;; known: where it went down, then where it is (its moves wait
        ;; meanwhile)
        start-tool!
        (fn [^js latest]
          (clear-timer!)
          (when-let [^js held (.-held g)]
            (set! (.-held g) nil)
            (set! (.-mode g) :starting)
            (set! (.-starting g) held)
            (set! (.-latest g) latest)
            (set! (.-lifted g) nil)
            (when-hovered
             ;; unless a second finger made a gesture of it meanwhile
             (fn []
               (when (identical? held (.-starting g))
                 (set! (.-starting g) nil)
                 (when (= :starting (.-mode g))
                   (set! (.-mode g) :tool))
                 (deliver-down! held)
                 (when-let [^js latest (.-latest g)]
                   (when-not (identical? latest held)
                     ((handler "move") latest)))
                 (when-let [^js up (.-lifted g)]
                   (set! (.-lifted g) nil)
                   ((handler "up") up)))))))

        start-gesture!
        (fn []
          (clear-timer!)
          (clear-press!)
          (set! (.-starting g) nil)
          (when (= :tool (.-mode g))
            (end-tool!))
          (set! (.-held g) nil)
          (set! (.-mode g) :gesture)
          (set! (.-last g) (fingers g))
          (set! (.-count g) (max (.-count g) (count (.-points g)))))

        ;; a finger down, on the canvas or (`other?`) on a part of it
        ;; with its own handler
        down!
        (fn [^js event other?]
          (let [id  (.-pointerId event)
                pt  (client-point event)
                n   (count (.-points g))]
            (set! (.-points g) (assoc (.-points g) [(.-order g) id] pt))
            (set! (.-order g) (inc (.-order g)))
            (if (zero? n)
              (do (set! (.-start g) pt)
                  (set! (.-t0 g) (js/performance.now))
                  (set! (.-moved g) false)
                  (set! (.-count g) 1)
                  (set! (.-other g) other?)
                  (if (touch/fingers-pan?)
                    (do (set! (.-mode g) :pan)
                        (set! (.-last g) [pt]))
                    (do (set! (.-mode g) :pending)
                        (set! (.-held g) event)
                        (set! (.-press g) event)
                        (set! (.-press-timer g) (ts/schedule long-press-ms long-press!))
                        ;; where it is, now: the shape under it is found
                        ;; meanwhile, as under a mouse that moved there
                        ((handler "move") event)
                        (set! (.-timer g) (ts/schedule hold-ms #(start-tool! nil))))))
              (start-gesture!))))

        on-down
        (mf/use-fn
         (fn [^js event]
           (if (or (not= "touch" (.-pointerType event)) (replayed? event))
             ((handler "down") event)
             (down! event false))))

        ;; a finger on a part of the canvas with its own handler: it waits
        ;; as one on the canvas does
        on-down-capture
        (mf/use-fn
         (fn [^js event]
           (when (and (= "touch" (.-pointerType event))
                      (not (replayed? event))
                      (not (identical? (.-target event) (.-currentTarget event))))
             (.stopPropagation event)
             (down! event true))))

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
                   (set! (.-moved g) true)
                   (clear-press!))
                 (case (.-mode g)
                   :pending  (when (.-moved g) (start-tool! event))
                   :starting (set! (.-latest g) event)
                   :tool     ((handler "move") event)
                   :pan     (let [[before] (.-last g)]
                              (set! (.-last g) [pt])
                              (schedule-view! g (- (:x pt) (:x before)) (- (:y pt) (:y before)) 1 pt))
                   :gesture (let [before (.-last g)
                                  after  (fingers g)]
                              (set! (.-last g) after)
                              (when (and (= 2 (count before)) (= 2 (count after)))
                                (let [{:keys [dx dy scale middle]} (touch/pinch-step before after)]
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
                   :pending (let [^js held (.-held g)]
                              (clear-timer!)
                              (set! (.-held g) nil)
                              (set! (.-tapping g) true)
                              (when-hovered
                               (fn []
                                 (deliver-down! held)
                                 ((handler "up") event)
                                 (set! (.-tapping g) false)
                                 (when-let [click (.-click g)]
                                   (set! (.-click g) nil)
                                   ((handler "click") click)))))
                   ;; lifted before its tool got it: it ends once it starts
                   ;; (see `start-tool!`)
                   :starting (set! (.-lifted g) event)
                   :tool    ((handler "up") event)
                   nil)
                 (if (empty? (.-points g))
                   (do
                     ;; fingers tapped together: two undo, three redo
                     (when (and (= :gesture (.-mode g))
                                (not (.-moved g))
                                (< (- (js/performance.now) (.-t0 g)) tap-ms))
                       (case (.-count g)
                         2 (st/emit! dwu/undo)
                         3 (st/emit! dwu/redo)
                         nil))
                     (clear-timer!)
                     (clear-press!)
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
             ((handler "click") event))))

        ;; a finger held opens the menu itself (see `long-press!`): the
        ;; one a browser opens for it then would come twice
        on-menu
        (mf/use-fn
         (fn [event]
           (if (seq (.-points g))
             (.preventDefault ^js event)
             ((handler "menu") event))))]

    [on-down on-move on-up on-up on-tap-click on-menu on-down-capture]))
