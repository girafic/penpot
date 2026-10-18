;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.options.menus.tilt-tool
  "The 3D rotation tool of the design tab (Penpot Motion, see
  `app.common.geom.tilt`): a card in a pad that tilts the selected layers
  as it is dragged, across about their vertical axis (Y), up and down
  about their horizontal one (X), with Shift in steps of 15°."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [rumext.v2 :as mf]))

(def ^:private degrees-per-px
  "How far a drag of a px across the pad turns the card."
  0.6)

(def ^:private snap-step 15)

(defn drag-tilt
  "The tilt `[rx ry]` a drag of `dx`, `dy` px gives from `[rx0 ry0]`, as
  if the card were grabbed: across to the right turns its right edge back
  (Y grows), down brings its top edge forward (X falls); with `snap?` in
  steps of 15°, else to a tenth of a degree."
  [[rx0 ry0] dx dy snap?]
  (let [round (fn [v]
                (if snap?
                  (* snap-step (mth/round (/ v snap-step)))
                  (mth/precision v 1)))]
    [(round (- rx0 (* dy degrees-per-px)))
     (round (+ ry0 (* dx degrees-per-px)))]))

(defn- number-or-zero
  [v]
  (if (number? v) v 0))

(defn- popover-place
  "Where the tool opens for its button at the client rect `rect`: left of
  the sidebar, level with the button, inside the window."
  [rect sidebar-left]
  (let [height (.-innerHeight js/window)
        right  (+ (- (.-innerWidth js/window) sidebar-left) 12)
        top    (max 8 (min (- (:top rect) 40) (- height 340)))]
    {:right right :top top}))

(mf/defc tilt-tool*
  "The button of the tool and, open, its pad. `rotate-x`, `rotate-y` and
  `backface-hidden` are the values of the selection (`:multiple` when
  they differ); `on-tilt` takes the tilt `[rx ry]` a drag gives,
  `on-drag-start` and `on-drag-end` bracket a drag (one undo step),
  `on-backface-change` the switch of the back face."
  [{:keys [rotate-x rotate-y backface-hidden on-tilt on-drag-start on-drag-end on-backface-change]}]
  (let [place*   (mf/use-state nil)
        place    (deref place*)
        open?    (some? place)
        drag-ref (mf/use-ref nil)
        pop-ref  (mf/use-ref nil)
        btn-ref  (mf/use-ref nil)

        rx       (number-or-zero rotate-x)
        ry       (number-or-zero rotate-y)

        close
        (mf/use-fn
         (fn []
           (reset! place* nil)
           (some-> (mf/ref-val btn-ref) (dom/focus!))))

        on-open
        (mf/use-fn
         (fn [event]
           (let [target  (dom/get-current-target event)
                 rect    (dom/get-bounding-rect target)
                 section (.closest target "[aria-label='shape-measures-section']")
                 sidebar (if (some? section)
                           (dom/get-bounding-rect section)
                           {:left (- (.-innerWidth js/window) 318)})]
             (swap! place* #(if (nil? %) (popover-place rect (:left sidebar)) nil)))))

        on-reset
        (mf/use-fn
         (mf/deps on-tilt on-drag-start on-drag-end)
         (fn [_]
           (on-drag-start)
           (on-tilt [0 0])
           (on-drag-end)))

        on-pointer-down
        (mf/use-fn
         (mf/deps rx ry on-drag-start)
         (fn [event]
           (when (zero? (.-button event))
             (dom/prevent-default event)
             (.setPointerCapture (dom/get-current-target event) (.-pointerId event))
             (mf/set-ref-val! drag-ref {:x (.-clientX event) :y (.-clientY event)
                                        :start [rx ry] :last [rx ry]})
             (on-drag-start))))

        on-pointer-move
        (mf/use-fn
         (mf/deps on-tilt)
         (fn [event]
           (when-let [{:keys [x y start last] :as drag} (mf/ref-val drag-ref)]
             (let [tilt (drag-tilt start (- (.-clientX event) x) (- (.-clientY event) y)
                                   (kbd/shift? event))]
               (when (not= tilt last)
                 (mf/set-ref-val! drag-ref (assoc drag :last tilt))
                 (on-tilt tilt))))))

        on-pointer-up
        (mf/use-fn
         (mf/deps on-drag-end)
         (fn [event]
           (when (some? (mf/ref-val drag-ref))
             (mf/set-ref-val! drag-ref nil)
             (.releasePointerCapture (dom/get-current-target event) (.-pointerId event))
             (on-drag-end))))

        on-key-down
        (mf/use-fn
         (mf/deps close)
         (fn [event]
           (when (kbd/esc? event)
             (dom/stop-propagation event)
             (close))))

        on-backface
        (mf/use-fn
         (mf/deps on-backface-change)
         (fn [event]
           (on-backface-change (-> event dom/get-target dom/checked?))))]

    (mf/with-effect [open?]
      (when open?
        (some-> (mf/ref-val pop-ref) (dom/focus!))))

    [:div {:class (stl/css :tilt-tool)}
     [:button {:type "button"
               :ref btn-ref
               :class (stl/css-case :tilt-button true
                                    :active open?)
               :title (tr "workspace.options.tilt-tool")
               :aria-label (tr "workspace.options.tilt-tool")
               :aria-haspopup "dialog"
               :aria-expanded open?
               :on-click on-open}
      [:svg {:view-box "0 0 16 16" :class (stl/css :tilt-button-icon) :aria-hidden true}
       [:path {:d "M3.5 3.5 L12.5 1.5 L12.5 14.5 L3.5 12.5 Z"}]
       [:path {:d "M8 3 L8 13" :class (stl/css :tilt-button-axis)}]]]

     (when open?
       [:*
        [:div {:class (stl/css :tilt-backdrop)
               :on-pointer-down close}]
        [:div {:class (stl/css :tilt-popover)
               :role "dialog"
               :aria-label (tr "workspace.options.tilt-tool")
               :tab-index -1
               :ref pop-ref
               :style (clj->js (update-vals place #(dm/str % "px")))
               :on-key-down on-key-down}
         [:div {:class (stl/css :tilt-header)}
          [:span {:class (stl/css :tilt-title)} (tr "workspace.options.tilt-tool")]
          [:> icon-button* {:variant "ghost"
                            :icon i/reload
                            :aria-label (tr "workspace.options.tilt-tool.reset")
                            :on-click on-reset}]
          [:> icon-button* {:variant "ghost"
                            :icon i/close
                            :aria-label (tr "labels.close")
                            :on-click close}]]

         [:div {:class (stl/css :tilt-pad)
                :title (tr "workspace.options.tilt-tool.hint")
                :on-pointer-down on-pointer-down
                :on-pointer-move on-pointer-move
                :on-pointer-up on-pointer-up
                :on-lost-pointer-capture on-pointer-up}
          [:div {:class (stl/css :tilt-ghost)}]
          [:div {:class (stl/css-case :tilt-card true
                                      :hides-back (true? backface-hidden))
                 :style #js {:transform (dm/str "rotateX(" rx "deg) rotateY(" ry "deg)")}}
           [:span {:class (stl/css :tilt-dot)}]]]

         [:div {:class (stl/css :tilt-footer)}
          [:span {:class (stl/css :tilt-values)}
           (if (or (= :multiple rotate-x) (= :multiple rotate-y))
             (tr "settings.multiple")
             (dm/str "X " (mth/precision rx 1) "°  ·  Y " (mth/precision ry 1) "°"))]
          [:> checkbox* {:id "tilt-tool-backface-hidden"
                         :label (tr "workspace.options.backface-hidden")
                         :checked (true? backface-hidden)
                         :on-change on-backface}]]]])]))
