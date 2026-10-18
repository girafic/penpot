;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.options.menus.tilt-tool
  "The 3D rotation tool of the design tab (Penpot Motion, see
  `app.common.geom.tilt`): a card in a pad that tilts the selected layers
  as it is dragged, across about their vertical axis (Y), up and down
  about their horizontal one (X), with Shift in steps of 15°; under it
  the fields of the turn about each axis (Z is the turn in the plane),
  the depth, the back face and, of groups and boards, the 3D space."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.main.ui.components.numeric-input :as deprecated-input]
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
        top    (max 8 (min (- (:top rect) 40) (- height 440)))]
    {:right right :top top}))

(mf/defc ^:private axis-field*
  "A number of the tool, `label` before it and `unit` after it, `wide`
  across the row."
  [{:keys [label title unit value on-change wide]}]
  [:div {:class (stl/css-case :tilt-field true
                              :wide (true? wide))
         :title title}
   [:span {:class (stl/css :tilt-field-label)} label]
   [:> deprecated-input/numeric-input*
    {:no-validate true
     :aria-label title
     :placeholder (if (= :multiple value) (tr "settings.multiple") "--")
     :on-change on-change
     :class (stl/css :tilt-field-input)
     :value (if (= :multiple value) value (or value 0))}]
   [:span {:class (stl/css :tilt-field-unit)} unit]])

(mf/defc tilt-tool*
  "The button of the tool and, open, its pad. `rotate-x`, `rotate-y`,
  `rotation`, `translate-z`, `backface-hidden` and `preserve-3d` are the
  values of the selection (`:multiple` when they differ), `space?`
  whether it holds groups or boards; `on-tilt` takes the tilt `[rx ry]` a
  drag gives, `on-drag-start` and `on-drag-end` bracket a drag (one undo
  step), `on-axis-change` an attribute and its value from a field,
  `on-rotation-change` the turn in the plane, `on-backface-change` and
  `on-preserve-change` the switches."
  [{:keys [rotate-x rotate-y rotation translate-z backface-hidden preserve-3d space?
           on-tilt on-drag-start on-drag-end on-axis-change on-rotation-change
           on-backface-change on-preserve-change]}]
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
           (on-backface-change (-> event dom/get-target dom/checked?))))

        on-preserve
        (mf/use-fn
         (mf/deps on-preserve-change)
         (fn [event]
           (on-preserve-change (-> event dom/get-target dom/checked?))))

        on-x     (mf/use-fn (mf/deps on-axis-change) #(on-axis-change :rotate-x %))
        on-y     (mf/use-fn (mf/deps on-axis-change) #(on-axis-change :rotate-y %))
        on-depth (mf/use-fn (mf/deps on-axis-change) #(on-axis-change :translate-z %))]

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
          [:div {:class (stl/css :tilt-fields)}
           [:> axis-field* {:label "X" :unit "°" :value rotate-x
                            :title (tr "workspace.options.rotate-x")
                            :on-change on-x}]
           [:> axis-field* {:label "Y" :unit "°" :value rotate-y
                            :title (tr "workspace.options.rotate-y")
                            :on-change on-y}]
           [:> axis-field* {:label "Z" :unit "°" :value rotation
                            :title (tr "workspace.options.rotation")
                            :on-change on-rotation-change}]
           [:> axis-field* {:label (tr "workspace.options.tilt-tool.depth") :unit "px" :value translate-z
                            :title (tr "workspace.options.depth")
                            :wide true
                            :on-change on-depth}]]
          [:> checkbox* {:id "tilt-tool-backface-hidden"
                         :label (tr "workspace.options.backface-hidden")
                         :checked (true? backface-hidden)
                         :on-change on-backface}]
          (when space?
            [:> checkbox* {:id "tilt-tool-preserve-3d"
                           :label (tr "workspace.options.preserve-3d")
                           :title (tr "workspace.options.preserve-3d.hint")
                           :checked (true? preserve-3d)
                           :on-change on-preserve}])]]])]))
