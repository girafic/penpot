;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.container-scroll
  "The wheel over the boards of the view mode that scroll their content (a
  container, see `app.main.data.viewer.scroll`) scrolls them (Penpot
  Motion): the innermost one under the pointer that takes the gesture and
  can still move that way, else the next one out, else the screen. A
  gesture keeps its target while it goes on, so the swing of a trackpad
  does not run on into the screen; past an end the container gives as the
  screen does (see `app.main.ui.viewer.drag-scroll`) and springs back once
  the wheel rests.

  On the screen of a touch device a press that drags along an axis of
  the innermost container under it that takes it scrolls that one: it
  takes the press from the screen (see `dds/claim!`) before the screen
  moves, and keeps it to the end."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.scroll :as dvs]
   [app.main.store :as st]
   [app.main.ui.viewer.drag-scroll :as dds]
   [app.main.ui.viewer.viewport-common :as vpc]
   [app.util.dom.normalize-wheel :as nw]
   [app.util.keyboard :as kbd]
   [rumext.v2 :as mf]))

(def ^:private latch-ms
  "How long, ms, after its last step a wheel gesture keeps its target."
  150)

(defn- svg-point
  "The point of the view box of `svg` under the pointer of `event`, nil
  without `svg`: through its screen matrix, so the zoom, the scroll of the
  section and a turned device count."
  [^js svg ^js event]
  (when-let [ctm (some-> svg .getScreenCTM)]
    (let [pt (.createSVGPoint svg)]
      (set! (.-x pt) (.-clientX event))
      (set! (.-y pt) (.-clientY event))
      (let [p (.matrixTransform pt (.inverse ctm))]
        (gpt/point (.-x p) (.-y p))))))

(defn- shown-scroll
  "The scroll of the containers of the board `screen-id` (`{id {:offset
  :past}}`, see `dv/scroll-container`)."
  [screen-id]
  (let [{:keys [board-id containers]} (dm/get-in @st/state [:viewer-local :scroll])]
    (when (= board-id screen-id) containers)))

(def ^:private fixed-layer-svg
  ":scope > div > [data-fixed-layer] > svg")

(def ^:private scroll-layer-svg
  ":scope > [data-scroll-layer] > svg")

(defn- containers-under
  "The containers of the board `screen-id` of `objects` under the pointer
  of `event` in the element `viewport` (`size` its box, see
  `app.main.ui.viewer/calculate-size`; `fixed-drop` how far its fixed
  layers go up), the innermost first, those of its fixed layers before
  (they show over the others): `[{:id :svg}]`, `:svg` the hotspots it
  maps through."
  [objects screen-id viewport size fixed-drop event]
  (let [offsets (dvs/view-offsets objects (shown-scroll screen-id))
        ids     (dvs/container-ids objects screen-id)
        fixed?  #(vpc/screen-fixed? objects %)
        pins    (vpc/fixed-pins objects screen-id fixed-drop)
        ;; a container in a fixed layer shows where its pin holds it
        extra   (into {}
                      (keep (fn [id]
                              (when-let [up (some #(get pins %) (cfh/get-parent-ids-seq objects id))]
                                [id (gpt/point 0 (- up))])))
                      (filter fixed? ids))
        origin  (gpt/point (:x size) (:y size))
        under   (fn [selector ids]
                  (let [svg (some-> viewport (.querySelector selector))]
                    (when-let [point (svg-point svg event)]
                      (map #(hash-map :id % :svg svg)
                           (dvs/container-at objects ids offsets (gpt/add point origin) :extra extra)))))]
    (vec (concat (under fixed-layer-svg (filter fixed? ids))
                 (under scroll-layer-svg (remove fixed? ids))))))

(defn- target-under
  "The container that takes the wheel `event` moving `[dx dy]` over the
  board `screen-id` (see `containers-under`): `{:id :axes}`, `:id` nil
  for the screen."
  [objects screen-id viewport size fixed-drop event dx dy]
  (let [offsets (dvs/view-offsets objects (shown-scroll screen-id))
        axis    (dvs/gesture-axis [dx dy])
        id      (d/seek (fn [id]
                          (let [frame (get objects id)
                                axes  (dvs/container-axes objects frame)]
                            (and (dvs/takes-gesture? axes axis)
                                 (dvs/can-scroll? (dvs/scroll-range objects frame axes)
                                                  (get offsets id [0 0]) axes [dx dy]))))
                        (map :id (containers-under objects screen-id viewport size fixed-drop event)))]
    {:id id :axes (when (some? id) (dvs/container-axes objects (get objects id)))}))

(defn use-container-scroll!
  "Lets the wheel scroll the containers of the board `frame` of `page`
  shown in the element of `viewport-ref` (`size` its box) in the section
  of `section-ref`, while `enabled?`. `fixed-drop`, how far its fixed
  layers go up (see `vpc/fixed-pins`)."
  [section-ref viewport-ref page frame size fixed-drop enabled?]
  (let [context-ref (mf/use-ref nil)]
    (mf/set-ref-val! context-ref {:objects (:objects page) :frame frame :size size :fixed-drop fixed-drop})
    (mf/with-effect [enabled?]
      (when-let [section (when enabled? (mf/ref-val section-ref))]
        (let [latch* (volatile! nil)
              ;; the press going on, and the container it scrolls
              drag*  (volatile! nil)
              ;; how far the wheel pushed the target past an end
              past*  (volatile! [0 0])
              rest*  (volatile! nil)
              frame* (volatile! nil)

              stop!
              (fn []
                (some-> @frame* js/cancelAnimationFrame)
                (vreset! frame* nil))

              ;; the rubber band back to the end, once the wheel rests
              spring!
              (fn [screen-id id offset [x0 y0]]
                (stop!)
                (let [t0 (js/performance.now)]
                  (letfn [(step [now]
                            (let [t (- now t0)
                                  x (dds/spring-back x0 t)
                                  y (dds/spring-back y0 t)
                                  done? (and (< (mth/abs x) 0.5) (< (mth/abs y) 0.5))]
                              (st/emit! (dv/scroll-container screen-id id offset (if done? [0 0] [x y])))
                              (vreset! frame* (when-not done? (js/requestAnimationFrame step)))))]
                    (vreset! frame* (js/requestAnimationFrame step)))))

              scroll!
              (fn [objects screen-id id axes dx dy]
                (stop!)
                (some-> @rest* js/clearTimeout)
                (let [frame   (get objects id)
                      range   (dvs/scroll-range objects frame axes)
                      [ox oy] (dvs/clamp-offset range (or (dm/get-in (shown-scroll screen-id) [id :offset]) [0 0]))
                      [mx my] range
                      dx      (if (contains? axes :x) dx 0)
                      dy      (if (contains? axes :y) dy 0)
                      {:keys [width height]} (:selrect frame)
                      height  (or (dvs/screen-h frame) height)
                      [px py] @past*
                      px      (dds/wheel-past px ox mx dx (* dds/wheel-reach width))
                      py      (dds/wheel-past py oy my dy (* dds/wheel-reach height))
                      offset  (dvs/clamp-offset range [(+ ox dx) (+ oy dy)])
                      shown   [(- (dds/past-offset px width)) (- (dds/past-offset py height))]]
                  (vreset! past* [px py])
                  (st/emit! (dv/scroll-container screen-id id offset shown))
                  (when (or (not (zero? px)) (not (zero? py)))
                    (vreset! rest* (js/setTimeout (fn []
                                                    (vreset! past* [0 0])
                                                    (spring! screen-id id offset shown))
                                                  dds/wheel-rest-ms)))))

              on-wheel
              (fn [^js event]
                (when-not (kbd/mod? event)
                  (let [{:keys [objects frame size fixed-drop]} (mf/ref-val context-ref)
                        screen-id (:id frame)
                        norm      ^js (nw/normalize-wheel event)
                        dx        (.-pixelX norm)
                        dy        (.-pixelY norm)
                        ;; a mouse turns sideways with shift, a trackpad
                        ;; already sends it so
                        [dx dy]   (if (and (kbd/shift? event) (zero? dx)) [dy 0] [dx dy])
                        now       (.-timeStamp event)
                        latch     @latch*
                        target    (if (and (some? latch) (< (- now (:at latch)) latch-ms))
                                    latch
                                    (do (vreset! past* [0 0])
                                        (target-under objects screen-id (mf/ref-val viewport-ref)
                                                      size fixed-drop event dx dy)))]
                    (vreset! latch* (assoc target :at now))
                    (when-let [id (:id target)]
                      (.preventDefault event)
                      (.stopPropagation event)
                      (scroll! objects screen-id id (:axes target) dx dy)))))]

          (letfn [;; where the pointer of `event` is in the view box of
                  ;; the hotspots the press maps through
                  (drag-point [drag event]
                    (svg-point (:svg drag) event))

                  (follow! [drag event]
                    (let [{:keys [objects]} (mf/ref-val context-ref)
                          {:keys [screen-id id axes start origin]} drag
                          frame (get objects id)
                          range (dvs/scroll-range objects frame axes)
                          point (drag-point drag event)
                          [ox oy] origin
                          dx    (if (contains? axes :x) (- (:x start) (:x point)) 0)
                          dy    (if (contains? axes :y) (- (:y start) (:y point)) 0)]
                      (st/emit! (dv/scroll-container screen-id id (dvs/clamp-offset range [(+ ox dx) (+ oy dy)]) [0 0]))))

                  (on-drag-move [^js event]
                    (when-let [drag @drag*]
                      (if (some? (:id drag))
                        (follow! drag event)
                        (when-let [axis (dds/drag-axis (:client drag) [(.-clientX event) (.-clientY event)])]
                          (let [{:keys [objects]} (mf/ref-val context-ref)
                                hit (d/seek #(dvs/takes-gesture? (dvs/container-axes objects (get objects (:id %))) axis)
                                            (:under drag))]
                            (if (nil? hit)
                              ;; not one of its containers': the screen's
                              (end-drag!)
                              (let [id     (:id hit)
                                    frame  (get objects id)
                                    axes   (dvs/container-axes objects frame)
                                    origin (dvs/clamp-offset (dvs/scroll-range objects frame axes)
                                                             (or (dm/get-in (shown-scroll (:screen-id drag)) [id :offset])
                                                                 [0 0]))
                                    drag   (assoc drag :id id :axes axes :svg (:svg hit) :origin origin
                                                  :start (svg-point (:svg hit) (:down drag)))]
                                (dds/claim!)
                                (set! (.. section -style -cursor) "grabbing")
                                (vreset! drag* drag)
                                (follow! drag event))))))))

                  (on-drag-up [_]
                    (let [drag @drag*]
                      (end-drag!)
                      (when (some? (:id drag))
                        (set! (.. section -style -cursor) "")
                        (dds/swallow-click!))))

                  (end-drag! []
                    (vreset! drag* nil)
                    (.removeEventListener js/window "pointermove" on-drag-move true)
                    (.removeEventListener js/window "pointerup" on-drag-up true)
                    (.removeEventListener js/window "pointercancel" on-drag-up true))

                  ;; in the capture phase: its moves come before the
                  ;; screen's, which leaves them once a container claims
                  ;; the press (see `dds/claim!`)
                  (on-down [^js event]
                    (when (and (zero? (.-button event))
                               (not= "touch" (.-pointerType event))
                               (dds/defer-taps?))
                      (let [{:keys [objects frame size fixed-drop]} (mf/ref-val context-ref)
                            under (containers-under objects (:id frame) (mf/ref-val viewport-ref)
                                                    size fixed-drop event)]
                        (when (seq under)
                          (stop!)
                          (vreset! drag* {:screen-id (:id frame)
                                          :under under
                                          :down event
                                          :client [(.-clientX event) (.-clientY event)]})
                          (.addEventListener js/window "pointermove" on-drag-move true)
                          (.addEventListener js/window "pointerup" on-drag-up true)
                          (.addEventListener js/window "pointercancel" on-drag-up true)))))]

            (.addEventListener section "wheel" on-wheel #js {:capture true :passive false})
            (.addEventListener section "pointerdown" on-down true)
            (fn []
              (.removeEventListener section "wheel" on-wheel #js {:capture true :passive false})
              (.removeEventListener section "pointerdown" on-down true)
              (end-drag!)
              (some-> @rest* js/clearTimeout)
              (stop!))))))))
