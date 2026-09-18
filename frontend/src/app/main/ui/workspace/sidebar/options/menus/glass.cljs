;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.glass
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.types.shape.glass :as ctsg]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.workspace.sidebar.options.rows.color-row :refer [color-row*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [rumext.v2 :as mf]))

(defn create-glass
  []
  (assoc ctsg/default-attrs :id (uuid/next)))

(defn normalize-angle
  "Wraps `angle` into the -180..180 range."
  [angle]
  (- (mod (+ angle 180) 360) 180))

(defn point->angle
  "Angle in degrees of the offset (x, y) from the pad center. 0 is up and
  positive angles turn clockwise, like the light angle in the renderer."
  [x y]
  (normalize-angle (mth/round (mth/degrees (mth/atan2 x (- y))))))

(defn angle->position
  "Inline style placing the light marker, in percent of the pad size. It
  must be a JS object: a runtime `:style` value reaches React unconverted."
  [angle]
  (let [rad (mth/radians angle)]
    #js {:left (str (+ 50 (* 36 (mth/sin rad))) "%")
         :top  (str (- 50 (* 36 (mth/cos rad))) "%")}))

(defn slider-fill
  "Start and end of the filled part of a slider track, in percent of the
  track. The fill goes from `origin` (the minimum when nil) to `value`."
  [value min max origin]
  (let [pct  (fn [v] (* 100 (/ (- (mth/clamp v min max) min) (- max min))))
        from (pct (d/nilv origin min))
        to   (pct value)]
    [(mth/min from to) (mth/max from to)]))

(def ^:private advanced-attrs
  [:saturation :brightness :dispersion :splay
   :texture :texture-amount :texture-scale :texture-angle])

(defn advanced-modified?
  "True when a value of the advanced block differs from its default, so
  the block starts open."
  [glass]
  (boolean
   (some (fn [attr]
           (not= (ctsg/get-value glass attr) (get ctsg/default-attrs attr)))
         advanced-attrs)))

(defn- event->angle
  [event node]
  (let [{:keys [left top width height]} (dom/get-bounding-rect node)
        pos (dom/get-client-position event)]
    (point->angle (- (:x pos) left (/ width 2))
                  (- (:y pos) top (/ height 2)))))

(mf/defc light-pad*
  {::mf/private true}
  [{:keys [angle disabled on-change on-change-start on-change-end]}]
  (let [dragging* (mf/use-ref false)
        angle     (d/nilv angle 0)

        on-pointer-down
        (mf/use-fn
         (mf/deps disabled on-change on-change-start)
         (fn [event]
           (when-not disabled
             (let [node (dom/get-current-target event)]
               (dom/capture-pointer event)
               (mf/set-ref-val! dragging* true)
               (on-change-start)
               (on-change (event->angle event node))))))

        on-pointer-move
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (when (mf/ref-val dragging*)
             (on-change (event->angle event (dom/get-current-target event))))))

        on-pointer-up
        (mf/use-fn
         (mf/deps on-change-end)
         (fn [event]
           (when (mf/ref-val dragging*)
             ;; Clear the flag first: releasing the pointer fires
             ;; lostpointercapture, which calls this handler again.
             (mf/set-ref-val! dragging* false)
             (dom/release-pointer event)
             (on-change-end))))

        on-key-down
        (mf/use-fn
         (mf/deps angle disabled on-change)
         (fn [event]
           (when-not disabled
             (let [step (if (kbd/shift? event) 15 1)
                   delta (cond
                           (or (kbd/right-arrow? event) (kbd/up-arrow? event)) step
                           (or (kbd/left-arrow? event) (kbd/down-arrow? event)) (- step)
                           :else nil)]
               (when (some? delta)
                 (dom/prevent-default event)
                 (on-change (normalize-angle (+ angle delta))))))))]

    [:div {:class (stl/css-case :light-pad true
                                :light-pad-disabled disabled)
           :role "slider"
           :tab-index (if disabled -1 0)
           :aria-label (tr "workspace.options.glass-options.light-angle")
           :aria-valuemin -180
           :aria-valuemax 180
           :aria-valuenow angle
           :aria-disabled disabled
           :on-pointer-down on-pointer-down
           :on-pointer-move on-pointer-move
           :on-pointer-up on-pointer-up
           :on-lost-pointer-capture on-pointer-up
           :on-key-down on-key-down}
     [:span {:class (stl/css :light-pad-target)}]
     [:span {:class (stl/css :light-pad-light)
             :style (angle->position angle)}]]))

(mf/defc glass-slider*
  {::mf/private true}
  [{:keys [attr label value default min max input-max step origin disabled
           on-change on-change-start on-change-end]}]
  (let [id        (str "glass-" (d/name attr))
        value     (d/nilv value default)
        step      (d/nilv step 1)
        [from to] (slider-fill value min max origin)

        on-slide
        (mf/use-fn
         (mf/deps attr on-change)
         (fn [event]
           (let [new-value (-> event dom/get-target dom/get-value d/parse-double)]
             (when (d/num? new-value)
               (on-change attr new-value)))))

        on-input
        (mf/use-fn
         (mf/deps attr on-change)
         (fn [new-value]
           (when (d/num? new-value)
             (on-change attr new-value))))]

    [:div {:class (stl/css :slider-row)}
     [:label {:for id :class (stl/css :label)} label]
     [:input {:id id
              :type "range"
              :class (stl/css :slider)
              :style {"--slider-from" (str from "%")
                      "--slider-to" (str to "%")}
              :min min
              :max max
              :step step
              :value (mth/clamp value min max)
              :disabled disabled
              :on-pointer-down on-change-start
              :on-pointer-up on-change-end
              :on-change on-slide}]
     [:> numeric-input* {:class (stl/css :slider-input)
                         :name id
                         :property label
                         :placeholder "--"
                         :min min
                         :max input-max
                         :step step
                         :value value
                         :disabled disabled
                         :on-change on-input}]]))

(mf/defc glass-sliders*
  "Renders one `glass-slider*` per slider definition, see `slider`."
  {::mf/private true}
  [{:keys [sliders value disabled on-change on-change-start on-change-end]}]
  [:*
   (for [{:keys [attr label default min max input-max step origin]} sliders]
     [:> glass-slider* {:key (d/name attr)
                        :attr attr
                        :label label
                        :default default
                        :min min
                        :max max
                        :input-max input-max
                        :step step
                        :origin origin
                        :value (get value attr)
                        :disabled disabled
                        :on-change on-change
                        :on-change-start on-change-start
                        :on-change-end on-change-end}])])

(defn- slider
  "Slider definition for `attr`; the default comes from the glass defaults."
  [attr label & {:as opts}]
  (merge {:attr attr
          :label label
          :default (get ctsg/default-attrs attr)
          :min 0
          :max 100}
         opts))

(mf/defc glass-options*
  "Controls for the glass effect values. `on-change` receives the attribute
  and its new value."
  [{:keys [value disabled on-change]}]
  (let [tx-id       (mf/use-memo #(uuid/next))
        light-tx-id (mf/use-memo #(uuid/next))

        advanced*   (mf/use-state #(advanced-modified? value))
        advanced    (deref advanced*)

        toggle-advanced
        (mf/use-fn #(swap! advanced* not))

        on-change-start
        (mf/use-fn
         (mf/deps tx-id)
         (fn [& _]
           (st/emit! (dwu/start-undo-transaction tx-id))))

        on-change-end
        (mf/use-fn
         (mf/deps tx-id)
         (fn [& _]
           (st/emit! (dwu/commit-undo-transaction tx-id))))

        on-angle-change
        (mf/use-fn
         (mf/deps on-change)
         (fn [angle]
           (when (d/num? angle)
             (on-change :light-angle (normalize-angle angle)))))

        on-intensity-change
        (mf/use-fn
         (mf/deps on-change)
         (fn [intensity]
           (when (d/num? intensity)
             (on-change :light-intensity intensity))))

        on-light-color-change
        (mf/use-fn
         (mf/deps on-change)
         (fn [color & _]
           (when-let [light-color (ctsg/color->light-color color)]
             (on-change :light-color light-color))))

        on-light-color-detach
        (mf/use-fn
         (mf/deps on-change value)
         (fn [& _]
           (when-let [light-color (:light-color value)]
             (on-change :light-color (dissoc light-color :ref-id :ref-file)))))

        on-light-color-open
        (mf/use-fn
         (mf/deps light-tx-id)
         (fn [& _]
           (st/emit! (dwu/start-undo-transaction light-tx-id))))

        on-light-color-close
        (mf/use-fn
         (mf/deps light-tx-id)
         (fn [& _]
           (st/emit! (dwu/commit-undo-transaction light-tx-id))))

        on-texture-change
        (mf/use-fn
         (mf/deps on-change)
         (fn [texture]
           (on-change :texture (keyword texture))))

        main-sliders
        (mf/with-memo []
          [(slider :highlight-width (tr "workspace.options.glass-options.highlight-width") :min 0.5 :max 24 :step 0.5)
           (slider :refraction (tr "workspace.options.glass-options.refraction") :input-max 100)
           (slider :depth (tr "workspace.options.glass-options.depth"))
           (slider :frost (tr "workspace.options.glass-options.frost"))])

        advanced-sliders
        (mf/with-memo []
          [(slider :saturation (tr "workspace.options.glass-options.saturation") :max 200 :input-max 200 :origin 100)
           (slider :brightness (tr "workspace.options.glass-options.brightness") :max 200 :input-max 200 :origin 100)
           (slider :dispersion (tr "workspace.options.glass-options.dispersion") :input-max 100)
           (slider :splay (tr "workspace.options.glass-options.splay") :input-max 100)])

        texture-sliders
        (mf/with-memo []
          [(slider :texture-amount (tr "workspace.options.glass-options.texture-amount") :input-max 100)
           (slider :texture-scale (tr "workspace.options.glass-options.texture-scale") :min 2 :max 64)
           (slider :texture-angle (tr "workspace.options.glass-options.texture-angle") :min -90 :max 90
                   :input-max 180 :origin 0)])

        texture-options
        (mf/with-memo []
          [{:id "none"
            :label (tr "workspace.options.glass-options.texture-none")}
           {:id "reeded"
            :label (tr "workspace.options.glass-options.texture-reeded")}
           {:id "wavy"
            :label (tr "workspace.options.glass-options.texture-wavy")}
           {:id "prismatic"
            :label (tr "workspace.options.glass-options.texture-prismatic")}
           {:id "cross-reeded"
            :label (tr "workspace.options.glass-options.texture-cross-reeded")}
           {:id "hammered"
            :label (tr "workspace.options.glass-options.texture-hammered")}])

        texture (d/name (ctsg/get-value value :texture))]

    [:div {:class (stl/css :glass-options)
           :data-testid "glass-options"}
     [:div {:class (stl/css :light-row)}
      [:span {:class (stl/css :label)}
       (tr "workspace.options.glass-options.light")]
      [:> light-pad* {:angle (:light-angle value)
                      :disabled disabled
                      :on-change on-angle-change
                      :on-change-start on-change-start
                      :on-change-end on-change-end}]
      [:div {:class (stl/css :light-inputs)}
       [:> numeric-input* {:class (stl/css :light-input)
                           :name "glass-light-angle"
                           :property (tr "workspace.options.glass-options.light-angle")
                           :icon i/rotation
                           :placeholder "--"
                           :min -180
                           :max 180
                           :value (:light-angle value)
                           :disabled disabled
                           :on-change on-angle-change}]
       [:> numeric-input* {:class (stl/css :light-input)
                           :name "glass-light-intensity"
                           :property (tr "workspace.options.glass-options.light-intensity")
                           :icon i/percentage
                           :placeholder "--"
                           :min 0
                           :max 100
                           :value (:light-intensity value)
                           :disabled disabled
                           :on-change on-intensity-change}]]]

     [:div {:class (stl/css-case :light-color-row true
                                 :light-color-disabled disabled)
            :data-testid "glass-light-color"}
      [:span {:class (stl/css :label)}
       (tr "workspace.options.glass-options.light-color")]
      [:> color-row* {:class (stl/css :light-color)
                      :color (d/nilv (:light-color value) ctsg/default-light-color)
                      :disable-opacity true
                      :disable-image true
                      :disable-picker disabled
                      :origin :glass
                      :on-change on-light-color-change
                      :on-detach on-light-color-detach
                      :on-open on-light-color-open
                      :on-close on-light-color-close}]]

     [:div {:class (stl/css :sliders)}
      [:> glass-sliders* {:sliders main-sliders
                          :value value
                          :disabled disabled
                          :on-change on-change
                          :on-change-start on-change-start
                          :on-change-end on-change-end}]]

     [:div {:class (stl/css :advanced)}
      [:> title-bar* {:collapsable true
                      :collapsed (not advanced)
                      :on-collapsed toggle-advanced
                      :aria-expanded advanced
                      :title (tr "workspace.options.glass-options.advanced")}]
      (when advanced
        [:div {:class (stl/css :sliders)}
         [:> glass-sliders* {:sliders advanced-sliders
                             :value value
                             :disabled disabled
                             :on-change on-change
                             :on-change-start on-change-start
                             :on-change-end on-change-end}]
         [:div {:class (stl/css :slider-row)}
          [:span {:class (stl/css :label)}
           (tr "workspace.options.glass-options.texture")]
          [:> select* {:key texture
                       ;; The grid item is the wrapper, not the button.
                       :wrapper-class (stl/css :texture-select)
                       :default-selected texture
                       :aria-label (tr "workspace.options.glass-options.texture")
                       :options texture-options
                       :disabled disabled
                       :on-change on-texture-change}]]
         (when (not= texture "none")
           [:> glass-sliders* {:sliders texture-sliders
                               :value value
                               :disabled disabled
                               :on-change on-change
                               :on-change-start on-change-start
                               :on-change-end on-change-end}])])]]))
