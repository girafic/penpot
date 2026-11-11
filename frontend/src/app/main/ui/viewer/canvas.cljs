;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.canvas
  "The canvas of a phone in the view mode: the page as a whole, drawn by
  WASM on the OffscreenCanvas the boards use (see
  `rwv/render-page-view`), at a view of its own (see
  `app.main.data.viewer.canvas`). A finger moves it, coasting on and
  giving as rubber past its edges as the screen of a board does (see
  `app.main.ui.viewer.drag-scroll`), two fingers zoom it about their
  middle, a double tap zooms to a board and back, a tap opens the board
  under it as the prototype; a wheel moves it and zooms it with ctrl, as
  a trackpad does. Its boards show as designed: their animations play
  in the prototype."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.canvas :as dvc]
   [app.main.render-viewer-wasm :as rwv]
   [app.main.store :as st]
   [app.main.ui.viewer.drag-scroll :as dds]
   [app.render-wasm.api :as wasm.api]
   [app.util.dom :as dom]
   [app.util.timers :as ts]
   [app.util.touch :as touch]
   [rumext.v2 :as mf]))

(def ^:private tween-ms
  "How long, ms, a double tap zooms."
  250)

(def ^:private wheel-rest-ms
  "How long, ms, a wheel or a trackpad rests before the canvas renders in
  full."
  150)

(defn- give
  "How far, px, a screen `size` px long shows a view `o` px past an end:
  as rubber (see `dds/rubber`)."
  [o size]
  (if (neg? o)
    (- (dds/rubber (- o) size))
    (dds/rubber o size)))

(defn shown-view
  "How the canvas of `bounds` on a `screen` shows the `view`: past its
  edges, giving as rubber."
  [view bounds screen]
  (let [clamped (dvc/clamp-view view bounds screen)
        [ox oy] (dvc/overshoot view bounds screen)
        zoom    (:zoom clamped)]
    (-> clamped
        (update :x + (/ (give ox (:width screen)) zoom))
        (update :y + (/ (give oy (:height screen)) zoom)))))

(defn- ease-out
  [e]
  (- 1 (mth/pow (- 1 e) 3)))

(defn- use-gestures!
  "The fingers, a wheel and a trackpad on `canvas-ref` move and zoom the
  view of `view-ref` (`{:page-id :screen :view}`) about what `live-ref`
  holds (`{:page :bounds :objects :render!}`): shown from the tiles in
  the cache while it moves (see `rwv/move-page-view`), rendered in full
  by `render!` once it rests. A tap opens the board under it as the
  prototype, once no second tap made it a double tap."
  [canvas-ref view-ref live-ref]
  (mf/with-effect []
    (let [^js canvas (mf/ref-val canvas-ref)
          ;; the fingers down (`{id point}`), what they do (`:pan`,
          ;; `:pinch`), the last tap, and what moves by itself (`:anim`)
          state      (volatile! {:pointers {}})
          raf        (volatile! nil)
          rest-timer (volatile! nil)
          ;; the tap that opens a board unless a second one comes
          tap*       (volatile! nil)

          cancel-tap!
          #(vreset! tap* nil)

          ;; the board under a tap, as the prototype: a step in the
          ;; history, the back gesture returns to the canvas
          open-board!
          (fn [point]
            (let [{:keys [page objects]} (mf/ref-val live-ref)
                  view (:view (mf/ref-val view-ref))]
              (when-let [id (dvc/board-to-open page objects (dvc/page-point view point))]
                (st/emit! (dv/go-to-frame id)))))

          get-view   #(:view (mf/ref-val view-ref))
          get-screen #(:screen (mf/ref-val view-ref))
          get-bounds #(:bounds (mf/ref-val live-ref))
          set-view!  #(mf/set-ref-val! view-ref (assoc (mf/ref-val view-ref) :view %))

          show!
          (fn [view]
            (rwv/move-page-view canvas (:zoom view) (dvc/view-box view (get-screen))))

          ;; at rest, within the edges, in full
          rest!
          (fn []
            (set-view! (dvc/clamp-view (get-view) (get-bounds) (get-screen)))
            ((:render! (mf/ref-val live-ref))))

          end-anim!
          (fn [view]
            (vswap! state dissoc :anim)
            (set-view! view)
            (rest!))

          draw!
          (fn draw! []
            (when (nil? @raf)
              (vreset! raf
                       (ts/raf
                        (fn [now]
                          (vreset! raf nil)
                          (let [{:keys [anim]} @state
                                bounds (get-bounds)
                                screen (get-screen)]
                            (case (:kind anim)
                              ;; let go moving: it goes on, slower and
                              ;; slower, and stops at an edge
                              :coast
                              (let [{[vx vy] :v t :t} anim
                                    [dx vx] (dds/coast vx (- now t))
                                    [dy vy] (dds/coast vy (- now t))
                                    moved   (dvc/pan-view (get-view) dx dy)
                                    [ox oy] (dvc/overshoot moved bounds screen)
                                    vx      (if (mth/close? ox 0 0.01) vx 0)
                                    vy      (if (mth/close? oy 0 0.01) vy 0)
                                    view    (dvc/clamp-view moved bounds screen)]
                                (show! view)
                                (if (and (< (mth/abs vx) dds/min-speed) (< (mth/abs vy) dds/min-speed))
                                  (end-anim! view)
                                  (do (set-view! view)
                                      (vswap! state assoc :anim {:kind :coast :v [vx vy] :t now})
                                      (draw!))))

                              ;; let go past an edge: back to it
                              :spring
                              (let [{:keys [base from t0]} anim
                                    t     (- now t0)
                                    [rx ry] from
                                    zoom  (:zoom base)]
                                (if (> t (* 6 dds/spring-ms))
                                  (do (show! base) (end-anim! base))
                                  (do (show! (-> base
                                                 (update :x + (/ (dds/spring-back rx t) zoom))
                                                 (update :y + (/ (dds/spring-back ry t) zoom))))
                                      (draw!))))

                              ;; a double tap: there smoothly
                              :tween
                              (let [{:keys [from to t0]} anim
                                    e    (min 1 (/ (- now t0) tween-ms))
                                    view (dvc/tween-view from to (ease-out e))]
                                (show! view)
                                (if (>= e 1)
                                  (end-anim! to)
                                  (do (set-view! view) (draw!))))

                              ;; fingers moving it
                              (show! (shown-view (get-view) bounds screen)))))))))

          ;; a finger down stops what moves by itself, where it is: true
          ;; when something moved
          stop-anim!
          (fn []
            (when-let [{:keys [kind base]} (:anim @state)]
              (when (= :spring kind)
                (set-view! base))
              (vswap! state dissoc :anim)
              true))

          point
          (fn [^js event]
            (let [rect (.getBoundingClientRect canvas)]
              (gpt/point (- (.-clientX event) (.-left rect)) (- (.-clientY event) (.-top rect)))))

          fingers
          (fn []
            (let [pointers (:pointers @state)]
              (mapv pointers (sort (keys pointers)))))

          on-down
          (fn [^js event]
            (when (some? (get-view))
              (.setPointerCapture canvas (.-pointerId event))
              (cancel-tap!)
              (some-> @rest-timer js/clearTimeout)
              (let [stopped (stop-anim!)
                    pt      (point event)
                    now     (.-timeStamp event)]
                (vswap! state assoc-in [:pointers (.-pointerId event)] pt)
                (case (count (:pointers @state))
                  ;; a finger that stops a coast taps nothing
                  1 (vswap! state assoc :mode :pan :start pt :moved (true? stopped) :down-t now
                            :samples [[now (:x pt) (:y pt)]])
                  2 (vswap! state assoc :mode :pinch :moved true :before (fingers))
                  nil))))

          on-move
          (fn [^js event]
            (let [id (.-pointerId event)]
              (when-let [prev (get-in @state [:pointers id])]
                (let [pt  (point event)
                      now (.-timeStamp event)]
                  (vswap! state assoc-in [:pointers id] pt)
                  (case (:mode @state)
                    :pan
                    (let [moved (or (:moved @state) (> (gpt/distance pt (:start @state)) dds/threshold))]
                      (vswap! state (fn [s]
                                      (-> s
                                          (assoc :moved moved)
                                          (update :samples #(conj (filterv (fn [[st]] (> st (- now 200))) %)
                                                                  [now (:x pt) (:y pt)])))))
                      (when moved
                        (set-view! (dvc/pan-view (get-view) (- (:x pt) (:x prev)) (- (:y pt) (:y prev))))
                        (draw!)))

                    :pinch
                    (let [after   (fingers)
                          {:keys [dx dy scale middle]} (touch/pinch-step (:before @state) after)
                          [lo hi] (dvc/zoom-range (get-bounds) (get-screen))
                          view    (dvc/pan-view (get-view) dx dy)
                          view    (dvc/zoom-about view middle (mth/clamp (* (:zoom view) scale) lo hi))]
                      (vswap! state assoc :before after)
                      (set-view! view)
                      (draw!))

                    nil)))))

          on-up
          (fn [^js event]
            (let [id (.-pointerId event)]
              (when (some? (get-in @state [:pointers id]))
                (let [now      (.-timeStamp event)
                      {:keys [mode moved samples start down-t]} @state
                      pointers (dissoc (:pointers @state) id)]
                  (vswap! state assoc :pointers pointers)
                  (cond
                    ;; one finger of two lifts: the other moves on
                    (and (= mode :pinch) (= 1 (count pointers)))
                    (let [pt (first (vals pointers))]
                      (vswap! state assoc :mode :pan :moved true :start pt
                              :samples [[now (:x pt) (:y pt)]]))

                    (seq pointers)
                    nil

                    ;; a tap, or the second of a double tap
                    (and (= mode :pan) (not moved))
                    (let [tap {:t now :x (:x start) :y (:y start)}]
                      (vswap! state assoc :mode nil)
                      ;; how soon it came down after the one before ended
                      (if (dvc/double-tap? (:last-tap @state) (assoc tap :t down-t))
                        (let [from (get-view)
                              to   (dvc/double-tap-view from (:objects (mf/ref-val live-ref)) (get-bounds)
                                                        (get-screen) (gpt/point (:x tap) (:y tap)))]
                          (vswap! state assoc :last-tap nil :anim {:kind :tween :from from :to to :t0 now})
                          (draw!))
                        ;; on the frame after: a second tap the page was too
                        ;; busy to take in time comes before it
                        (let [token #js {}]
                          (vswap! state assoc :last-tap tap)
                          (vreset! tap* token)
                          (js/setTimeout #(ts/raf (fn []
                                                    (when (identical? token @tap*)
                                                      (vreset! tap* nil)
                                                      (open-board! start))))
                                         dvc/double-tap-ms))))

                    :else
                    (let [view    (get-view)
                          screen  (get-screen)
                          [ox oy] (dvc/overshoot view (get-bounds) screen)
                          [vx vy] (if (= mode :pan) (dvc/finger-speed samples now) [0 0])]
                      (vswap! state assoc :mode nil :last-tap nil)
                      (cond
                        (or (> (mth/abs ox) 0.5) (> (mth/abs oy) 0.5))
                        (let [base (dvc/clamp-view view (get-bounds) screen)]
                          (set-view! base)
                          (vswap! state assoc :anim {:kind :spring :base base :t0 now
                                                     :from [(give ox (:width screen))
                                                            (give oy (:height screen))]})
                          (draw!))

                        (> (mth/hypot vx vy) dds/min-speed)
                        (do (vswap! state assoc :anim {:kind :coast :v [vx vy] :t now})
                            (draw!))

                        :else
                        (rest!))))))))

          ;; a finger the browser took is no tap
          on-cancel
          (fn [^js event]
            (vswap! state assoc :moved true)
            (on-up event))

          on-wheel
          (fn [^js event]
            (when (some? (get-view))
              (.preventDefault event)
              (stop-anim!)
              (let [bounds  (get-bounds)
                    screen  (get-screen)
                    [lo hi] (dvc/zoom-range bounds screen)
                    view    (get-view)
                    view    (if (.-ctrlKey event)
                              (dvc/zoom-about view (point event)
                                              (mth/clamp (* (:zoom view) (js/Math.exp (- (/ (.-deltaY event) 100))))
                                                         lo hi))
                              (dvc/pan-view view (- (.-deltaX event)) (- (.-deltaY event))))]
                (set-view! (dvc/clamp-view view bounds screen))
                (draw!)
                (some-> @rest-timer js/clearTimeout)
                (vreset! rest-timer (js/setTimeout rest! wheel-rest-ms)))))]

      (.addEventListener canvas "pointerdown" on-down)
      (.addEventListener canvas "pointermove" on-move)
      (.addEventListener canvas "pointerup" on-up)
      (.addEventListener canvas "pointercancel" on-cancel)
      (.addEventListener canvas "wheel" on-wheel #js {:passive false})
      (fn []
        (.removeEventListener canvas "pointerdown" on-down)
        (.removeEventListener canvas "pointermove" on-move)
        (.removeEventListener canvas "pointerup" on-up)
        (.removeEventListener canvas "pointercancel" on-cancel)
        (.removeEventListener canvas "wheel" on-wheel #js {:passive false})
        (some-> @raf ts/cancel-af!)
        (some-> @rest-timer js/clearTimeout)
        (cancel-tap!)))))

(mf/defc canvas-viewport*
  "The `page` as a whole on a screen of `size` (`{:width :height}`, CSS
  px: the section of the view mode), from its start view, about its
  middle as the phone turns, at its start view again on another page.
  `on-drawn` once it shows (and `data-canvas-drawn` on the canvas)."
  [{:keys [page size on-drawn]}]
  (let [canvas-ref (mf/use-ref nil)
        page-id    (:id page)
        objects    (mf/with-memo [page] (dvc/canvas-objects (:objects page)))
        bounds     (mf/with-memo [objects] (dvc/content-bounds objects))
        width      (:width size 0)
        height     (:height size 0)
        dpr        (wasm.api/get-dpr)
        ;; the view, with the page and the screen it is for: it changes
        ;; at each step of a gesture, so it is no state that renders
        view-ref   (mf/use-ref nil)
        ;; what the gestures read as they come
        live-ref   (mf/use-ref nil)

        render!
        (mf/use-fn
         (mf/deps page-id objects on-drawn)
         (fn []
           (let [canvas (mf/ref-val canvas-ref)
                 {:keys [view screen]} (mf/ref-val view-ref)]
             (when (and (some? canvas) (some? view))
               (rwv/render-page-view page-id objects canvas (:zoom view) (dvc/view-box view screen)
                                     (fn []
                                       (dom/set-attribute! canvas "data-canvas-drawn" "true")
                                       (when (fn? on-drawn)
                                         (on-drawn))))))))]

    (mf/set-ref-val! live-ref {:page page :bounds bounds :objects objects :render! render!})

    (mf/with-effect [page-id objects bounds width height dpr]
      (when (and (some? (mf/ref-val canvas-ref)) (some? bounds) (pos? width) (pos? height))
        (let [screen {:width width :height height}
              prev   (mf/ref-val view-ref)
              view   (if (= page-id (:page-id prev))
                       (dvc/resize-view (:view prev) (:screen prev) screen bounds)
                       (dvc/initial-view bounds screen))]
          (mf/set-ref-val! view-ref {:page-id page-id :screen screen :view view})
          (render!))))

    (use-gestures! canvas-ref view-ref live-ref)

    [:canvas {:ref canvas-ref
              :class (stl/css :canvas)
              :data-dpr dpr
              :width (mth/round (* width dpr))
              :height (mth/round (* height dpr))}]))
