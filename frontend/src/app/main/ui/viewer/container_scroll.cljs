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
  the wheel rests."
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

(defn- target-under
  "The container that takes the wheel `event` moving `[dx dy]` over the
  board `screen-id` of `objects` in the element `viewport` (`size` its
  box, see `app.main.ui.viewer/calculate-size`; `fixed-drop` how far its
  fixed layers go up): `{:id :axes}`, `:id` nil for the screen."
  [objects screen-id viewport size fixed-drop event dx dy]
  (let [offsets    (dvs/view-offsets objects (shown-scroll screen-id))
        ids        (dvs/container-ids objects screen-id)
        fixed?     #(vpc/screen-fixed? objects %)
        pins       (vpc/fixed-pins objects screen-id fixed-drop)
        ;; a container in a fixed layer shows where its pin holds it
        extra      (into {}
                         (keep (fn [id]
                                 (when-let [up (some #(get pins %) (cfh/get-parent-ids-seq objects id))]
                                   [id (gpt/point 0 (- up))])))
                         (filter fixed? ids))
        origin     (gpt/point (:x size) (:y size))
        under      (fn [selector ids]
                     (when-let [point (svg-point (some-> viewport (.querySelector selector)) event)]
                       (dvs/container-at objects ids offsets (gpt/add point origin) :extra extra)))
        axis       (dvs/gesture-axis [dx dy])
        candidates (concat (under ":scope > div > [data-fixed-layer] > svg" (filter fixed? ids))
                           (under ":scope > [data-scroll-layer] > svg" (remove fixed? ids)))
        id         (d/seek (fn [id]
                             (let [frame (get objects id)
                                   axes  (dvs/container-axes objects frame)]
                               (and (dvs/takes-gesture? axes axis)
                                    (dvs/can-scroll? (dvs/scroll-range objects frame axes)
                                                     (get offsets id [0 0]) axes [dx dy]))))
                           candidates)]
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

          (.addEventListener section "wheel" on-wheel #js {:capture true :passive false})
          (fn []
            (.removeEventListener section "wheel" on-wheel #js {:capture true :passive false})
            (some-> @rest* js/clearTimeout)
            (stop!)))))))
