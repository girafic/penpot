;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.canvas-window
  "The canvas in a small window over the maximized timeline (see
  `dwa/toggle-timeline-maximized`): where it is, as `{:x :y :w :h}` px of
  the browser window, and the bar and the corners that move and resize it
  and the button that hides it."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.math :as mth]
   [app.main.data.workspace.animation :as dwa]
   [app.main.store :as st]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [goog.events :as events]
   [rumext.v2 :as mf]))

(def ^:private margin
  "Room between the window and the edges of the browser window."
  16)

(def min-width 200)
(def min-height 126)

(def ^:private start-width 400)
(def ^:private start-height 252)

(defn start-frame
  "Where the window starts: at the bottom right of a browser window `vw`
  × `vh` px."
  [vw vh]
  {:x (- vw start-width margin) :y (- vh start-height margin) :w start-width :h start-height})

(defn clamp-frame
  "`frame` kept in a browser window `vw` × `vh` px: no smaller than the
  smallest window, no larger than the browser window, inside it."
  [{:keys [x y w h]} vw vh]
  (let [w (mth/clamp w min-width (max min-width (- vw (* 2 margin))))
        h (mth/clamp h min-height (max min-height (- vh (* 2 margin))))]
    {:x (mth/clamp x 0 (max 0 (- vw w)))
     :y (mth/clamp y 0 (max 0 (- vh h)))
     :w w
     :h h}))

(defn moved
  "`frame` moved by `dx`, `dy`."
  [frame dx dy]
  (-> frame (update :x + dx) (update :y + dy)))

(defn resized
  "`frame` resized by dragging its corner `corner` (`:top-left`,
  `:top-right`, `:bottom-left` or `:bottom-right`) by `dx`, `dy`: the
  opposite corner stays, and it gets no smaller than the smallest window."
  [{:keys [x y w h]} corner dx dy]
  (let [left? (contains? #{:top-left :bottom-left} corner)
        top?  (contains? #{:top-left :top-right} corner)
        w'    (max min-width (if left? (- w dx) (+ w dx)))
        h'    (max min-height (if top? (- h dy) (+ h dy)))]
    {:x (if left? (+ x (- w w')) x)
     :y (if top? (+ y (- h h')) y)
     :w w'
     :h h'}))

(defn use-window-size
  "`[width height]` of the browser window, as it resizes."
  []
  (let [size* (mf/use-state #(vector (.-innerWidth js/window) (.-innerHeight js/window)))]
    (mf/with-effect []
      (let [key (events/listen js/window "resize"
                               #(reset! size* [(.-innerWidth js/window) (.-innerHeight js/window)]))]
        #(events/unlistenByKey key)))
    (deref size*)))

(def ^:private corners
  "Each corner that resizes the window, with its classes."
  [["top-left" (stl/css :corner :top-left)]
   ["top-right" (stl/css :corner :top-right)]
   ["bottom-left" (stl/css :corner :bottom-left)]
   ["bottom-right" (stl/css :corner :bottom-right)]])

(mf/defc canvas-window-handles*
  "The bar on top of the window that moves it (a double click puts it
  back where it starts), with a button that hides it, and its corners
  that resize it, for the window
  at `frame`: `on-change` with the frame as they drag, `on-commit` with
  it once they let go, and whether its size changed."
  [{:keys [frame on-change on-commit]}]
  (let [drag-ref (mf/use-ref nil)

        on-down
        (mf/use-fn
         (mf/deps frame)
         (fn [event]
           (let [target (dom/get-current-target event)]
             (dom/stop-propagation event)
             (dom/prevent-default event)
             (.setPointerCapture target (.-pointerId event))
             (mf/set-ref-val! drag-ref {:mode (keyword (dom/get-data target "mode"))
                                        :frame frame
                                        :last frame
                                        :x (.-clientX event)
                                        :y (.-clientY event)}))))

        on-move
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (when-let [{:keys [mode frame x y] :as drag} (mf/ref-val drag-ref)]
             (let [dx    (- (.-clientX event) x)
                   dy    (- (.-clientY event) y)
                   frame (if (= mode :move) (moved frame dx dy) (resized frame mode dx dy))]
               (mf/set-ref-val! drag-ref (assoc drag :last frame))
               (on-change frame)))))

        on-up
        (mf/use-fn
         (mf/deps on-commit)
         (fn [_]
           (when-let [{:keys [mode last]} (mf/ref-val drag-ref)]
             (mf/set-ref-val! drag-ref nil)
             (on-commit last (not= mode :move)))))

        on-reset
        (mf/use-fn #(st/emit! (dwa/set-canvas-window nil :after-resize)))

        on-hide
        (mf/use-fn #(st/emit! (dwa/toggle-canvas-window)))]

    [:*
     [:div {:class (stl/css :bar)
            :data-mode "move"
            :title (tr "workspace.animation.canvas-window-move")
            :on-pointer-down on-down
            :on-pointer-move on-move
            :on-lost-pointer-capture on-up
            :on-double-click on-reset}
      [:span {:class (stl/css :grip)}]
      ;; a click on it moves nothing
      [:button {:type "button"
                :class (stl/css :hide-btn)
                :title (tr "workspace.animation.canvas-window-hide")
                :aria-label (tr "workspace.animation.canvas-window-hide")
                :on-pointer-down dom/stop-propagation
                :on-double-click dom/stop-propagation
                :on-click on-hide}
       [:> i/icon* {:icon-id i/close-small :size "s"}]]]
     (for [[corner class] corners]
       [:div {:key corner
              :class class
              :data-mode corner
              :title (tr "workspace.animation.canvas-window-resize")
              :on-pointer-down on-down
              :on-pointer-move on-move
              :on-lost-pointer-capture on-up}])]))
