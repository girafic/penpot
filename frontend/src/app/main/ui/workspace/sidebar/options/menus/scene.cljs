;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.scene
  "The light of the layers in 3D of a board and their depth cues (Penpot
  Motion, see `app.common.geom.tilt/scene-effects`): rows of the
  perspective section of the Design tab."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.store :as st]
   [app.main.ui.components.numeric-input :as deprecated-input]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.workspace.sidebar.options.menus.motion :refer [keyframe-diamond*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:timeline
  (l/derived dwa/current-timeline st/state))

(mf/defc scene-field*
  "A number of the scene shown with its `label` and `unit`: `value`
  (`:multiple` mixed), `scale` times what it keeps (percent of a share);
  `on-change` gets what it keeps."
  [{:keys [label hint unit value scale min max on-change keyframes]}]
  (let [scale     (or scale 1)
        on-change (mf/use-fn
                   (mf/deps on-change scale)
                   (fn [value]
                     (on-change (when (number? value) (/ value scale)))))]
    [:div {:class (stl/css :scene-field) :title hint}
     [:span {:class (stl/css :scene-label)} label]
     [:> deprecated-input/numeric-input*
      {:no-validate true
       :min min
       :max max
       :aria-label hint
       :placeholder (if (= :multiple value) (tr "settings.multiple") "0")
       :on-change on-change
       :class (stl/css :numeric-input)
       :value (when (number? value) (mth/precision (* scale value) 2))}]
     [:span {:class (stl/css :scene-unit)} unit]
     ;; its diamond, in motion mode, as the camera has
     (when (some? keyframes)
       [:> keyframe-diamond* {:properties [keyframes]}])]))

(def ^:private cue-attrs
  [:depth-fade :depth-blur :depth-focus])

(def ^:private default-cues
  "The depth cues a board gets when they are turned on: a third of the
  opacity gone each 1000 px behind the focus, a px of blur each 100 px
  off it."
  {:depth-fade 0.3 :depth-blur 1})

(mf/defc scene-menu*
  "The light and the depth cues of the boards `ids`, as `values` (their
  values, `:multiple` where they differ) has them: a switch for each,
  with its fields while it is on (the ambient light and the focus beside
  it)."
  [{:keys [ids values]}]
  (let [motion-id (mf/use-ctx ctx/motion-shape-id)
        light  (:light values)
        lit?   (some? light)
        shared (when (map? light) light)
        cued?  (some #(some? (get values %)) cue-attrs)

        on-light-check
        (mf/use-fn
         (mf/deps ids)
         (fn [event]
           (let [on? (-> event dom/get-target dom/checked?)]
             (st/emit! (dwsh/update-shapes ids #(if on?
                                                  (assoc % :light (or (:light %) gtl/default-light))
                                                  (dissoc % :light)))))))

        ;; an animated part of it takes the value at the playhead, as the
        ;; camera does
        on-light-change
        (mf/use-fn
         (mf/deps ids motion-id)
         (fn [attr value]
           (when (number? value)
             (let [value    (case attr
                              :angle   (mod value 360)
                              :height  (mth/clamp value 0 90)
                              :ambient (mth/clamp value 0 1))
                   property (keyword (str "light-" (name attr)))]
               (if (and (some? motion-id)
                        (seq (cta/property-keyframes (deref ref:timeline) motion-id property)))
                 (st/emit! (dwa/set-value-at-playhead motion-id property value))
                 (st/emit! (dwsh/update-shapes ids #(cond-> % (map? (:light %)) (assoc-in [:light attr] value)))
                           ;; the shadows it casts turn with it
                           (dwa/cast-shadows ids)))))))

        on-shadows-check
        (mf/use-fn
         (mf/deps ids)
         (fn [event]
           (let [on? (-> event dom/get-target dom/checked?)]
             (st/emit! (dwsh/update-shapes ids #(cond-> %
                                                  (map? (:light %))
                                                  (update :light (fn [light] (if on? (assoc light :shadows true) (dissoc light :shadows))))))
                       (dwa/cast-shadows ids)))))

        on-cues-check
        (mf/use-fn
         (mf/deps ids)
         (fn [event]
           (let [on? (-> event dom/get-target dom/checked?)]
             (st/emit! (dwsh/update-shapes ids #(if on?
                                                  (merge % default-cues (select-keys % cue-attrs))
                                                  (apply dissoc % cue-attrs)))))))

        ;; while they are on, a cue at 0 stays, so that they stay on
        on-cue-change
        (mf/use-fn
         (mf/deps ids)
         (fn [attr value]
           (when (number? value)
             (let [value (case attr
                           :depth-fade  (mth/clamp value 0 1)
                           :depth-blur  (mth/clamp value 0 100)
                           :depth-focus value)]
               (st/emit! (dwsh/update-shapes ids #(assoc % attr value)))))))

        on-angle   (mf/use-fn (mf/deps on-light-change) #(on-light-change :angle %))
        on-height  (mf/use-fn (mf/deps on-light-change) #(on-light-change :height %))
        on-ambient (mf/use-fn (mf/deps on-light-change) #(on-light-change :ambient %))
        on-fade    (mf/use-fn (mf/deps on-cue-change) #(on-cue-change :depth-fade %))
        on-blur    (mf/use-fn (mf/deps on-cue-change) #(on-cue-change :depth-blur %))
        on-focus   (mf/use-fn (mf/deps on-cue-change) #(on-cue-change :depth-focus %))
        light-of   (fn [attr] (if (= :multiple light) :multiple (get shared attr)))]

    [:*
     [:div {:class (stl/css :scene-check :light-check)
            :title (tr "workspace.options.scene.light.hint")}
      [:> checkbox* {:id "scene-light"
                     :label (tr "workspace.options.scene.light")
                     :checked lit?
                     :on-change on-light-check}]]
     (when lit?
       [:*
        [:div {:class (stl/css :scene-row :light-ambient)}
         [:> scene-field* {:label (tr "workspace.options.scene.ambient")
                           :hint (tr "workspace.options.scene.ambient.hint")
                           :unit "%" :scale 100 :min 0 :max 100
                           :keyframes (when motion-id :light-ambient)
                           :value (light-of :ambient)
                           :on-change on-ambient}]]
        [:div {:class (stl/css :scene-row :light-angle)}
         [:> scene-field* {:label (tr "workspace.options.scene.angle")
                           :hint (tr "workspace.options.scene.angle.hint")
                           :unit "°" :min -360 :max 720
                           :keyframes (when motion-id :light-angle)
                           :value (light-of :angle)
                           :on-change on-angle}]]
        [:div {:class (stl/css :scene-row :light-height)}
         [:> scene-field* {:label (tr "workspace.options.scene.height")
                           :hint (tr "workspace.options.scene.height.hint")
                           :unit "°" :min 0 :max 90
                           :keyframes (when motion-id :light-height)
                           :value (light-of :height)
                           :on-change on-height}]]
        [:div {:class (stl/css :scene-check :shadows-check)
               :title (tr "workspace.options.scene.shadows.hint")}
         [:> checkbox* {:id "scene-shadows"
                        :label (tr "workspace.options.scene.shadows")
                        :checked (true? (:shadows shared))
                        :on-change on-shadows-check}]]])

     [:div {:class (stl/css :scene-check :cues-check)
            :title (tr "workspace.options.scene.depth.hint")}
      [:> checkbox* {:id "scene-depth-cues"
                     :label (tr "workspace.options.scene.depth")
                     :checked (boolean cued?)
                     :on-change on-cues-check}]]
     (when cued?
       [:*
        [:div {:class (stl/css :scene-row :depth-focus)}
         [:> scene-field* {:label (tr "workspace.options.scene.focus")
                           :hint (tr "workspace.options.scene.focus.hint")
                           :unit "px" :min -10000 :max 10000
                           :value (or (:depth-focus values) 0)
                           :on-change on-focus}]]
        [:div {:class (stl/css :scene-row :depth-fade)}
         [:> scene-field* {:label (tr "workspace.options.scene.fade")
                           :hint (tr "workspace.options.scene.fade.hint")
                           :unit "%" :scale 100 :min 0 :max 100
                           :value (or (:depth-fade values) 0)
                           :on-change on-fade}]]
        [:div {:class (stl/css :scene-row :depth-blur)}
         [:> scene-field* {:label (tr "workspace.options.scene.blur")
                           :hint (tr "workspace.options.scene.blur.hint")
                           :unit "px" :min 0 :max 100
                           :value (or (:depth-blur values) 0)
                           :on-change on-blur}]]])]))
