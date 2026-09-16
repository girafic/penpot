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
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
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
  [{:keys [attr label value min max input-max disabled on-change on-change-start on-change-end]}]
  (let [id    (str "glass-" (d/name attr))
        value (d/nilv value 0)
        pct   (-> (/ (- (mth/clamp value min max) min) (- max min))
                  (* 100))

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
              :style {"--slider-progress" (str pct "%")}
              :min min
              :max max
              :step 1
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
                         :value value
                         :disabled disabled
                         :on-change on-input}]]))

(mf/defc glass-options*
  "Controls for the glass effect values. `on-change` receives the attribute
  and its new value."
  [{:keys [value disabled on-change]}]
  (let [tx-id (mf/use-memo #(uuid/next))

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

        sliders
        (mf/with-memo []
          [{:attr :refraction :max 100 :input-max 100
            :label (tr "workspace.options.glass-options.refraction")}
           {:attr :depth :max 100
            :label (tr "workspace.options.glass-options.depth")}
           {:attr :dispersion :max 100 :input-max 100
            :label (tr "workspace.options.glass-options.dispersion")}
           {:attr :frost :max 100
            :label (tr "workspace.options.glass-options.frost")}
           {:attr :splay :max 100 :input-max 100
            :label (tr "workspace.options.glass-options.splay")}])]

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

     [:div {:class (stl/css :sliders)}
      (for [{:keys [attr] :as slider} sliders]
        [:> glass-slider* {:key (d/name attr)
                           :attr attr
                           :label (:label slider)
                           :min 0
                           :max (:max slider)
                           :input-max (:input-max slider)
                           :value (get value attr)
                           :disabled disabled
                           :on-change on-change
                           :on-change-start on-change-start
                           :on-change-end on-change-end}])]]))
