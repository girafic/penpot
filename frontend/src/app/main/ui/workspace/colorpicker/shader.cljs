;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.colorpicker.shader
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.main.data.workspace.colors :as dc]
   [app.main.store :as st]
   [app.main.ui.workspace.colorpicker.shader-presets :as presets]
   [app.render-wasm.api :as api]
   [app.util.dom :as dom]
   [app.util.functions :as fns]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(def ^:private default-shader-colors
  ["#7c3aed" "#0ea5e9" "#f472b6" "#facc15"])

(mf/defc shader-panel*
  "Panel of the colorpicker shader tab: preset selector, SkSL source
  editor with live validation, and color/param uniform inputs."
  [{:keys [shader]}]
  (let [source     (d/nilv (:source shader) "")
        preset     (:preset shader)
        colors     (:colors shader)
        params     (:params shader)

        ;; Local (uncommitted) source while the user is typing
        source*    (mf/use-state source)
        error*     (mf/use-state nil)

        ;; Keep the local editor in sync when the shader changes from
        ;; the outside (e.g. preset selection or undo)
        _          (mf/with-effect [source]
                     (reset! source* source))

        commit-source
        (mf/use-fn
         (fn [value]
           (let [error (api/validate-shader value)]
             (reset! error* error)
             (when (nil? error)
               (st/emit! (dc/update-colorpicker-shader {:source value}))))))

        commit-source-debounced
        (mf/use-memo
         (mf/deps commit-source)
         #(fns/debounce commit-source 300))

        on-change-source
        (mf/use-fn
         (mf/deps commit-source-debounced)
         (fn [event]
           (let [value (dom/get-target-val event)]
             (reset! source* value)
             (commit-source-debounced value))))

        on-change-preset
        (mf/use-fn
         (fn [event]
           (let [name   (dom/get-target-val event)
                 preset (presets/find-preset name)]
             (reset! error* nil)
             (st/emit! (dc/update-colorpicker-shader
                        {:source (:source preset)
                         :preset (:name preset)
                         :colors (:colors preset)
                         :params (:params preset)})))))

        on-change-color
        (mf/use-fn
         (mf/deps colors)
         (fn [event]
           (let [index (-> (dom/get-current-target event)
                           (dom/get-data "index")
                           (d/parse-integer))
                 value (dom/get-target-val event)
                 colors (-> (mapv (fn [i default]
                                    (d/nilv (nth colors i nil) default))
                                  (range 4)
                                  default-shader-colors)
                            (assoc index value))]
             (st/emit! (dc/update-colorpicker-shader {:colors colors})))))

        on-change-param
        (mf/use-fn
         (mf/deps params)
         (fn [event]
           (let [index (-> (dom/get-current-target event)
                           (dom/get-data "index")
                           (d/parse-integer))
                 value (-> (dom/get-target-val event)
                           (d/parse-double 0))
                 params (-> (vec (take 4 (concat params (repeat 0))))
                            (assoc index value))]
             (st/emit! (dc/update-colorpicker-shader {:params params})))))]

    [:div {:class (stl/css :shader-panel)}
     [:div {:class (stl/css :preset-row)}
      [:select {:class (stl/css :preset-select)
                :value (d/nilv preset "")
                :on-change on-change-preset}
       [:option {:value "" :disabled true}
        (tr "workspace.colorpicker.shader.presets")]
       (for [{:keys [name]} presets/presets]
         [:option {:key name :value name}
          (tr (str "workspace.colorpicker.shader.preset." name))])]]

     [:textarea {:class (stl/css :source-editor)
                 :value @source*
                 :spell-check false
                 :rows 12
                 :placeholder presets/default-source
                 :on-change on-change-source}]

     (when-let [error @error*]
       [:div {:class (stl/css :error)
              :title error}
        error])

     [:div {:class (stl/css :uniform-row)}
      (for [index (range 4)]
        [:input {:key (str "shader-color-" index)
                 :class (stl/css :color-input)
                 :type "color"
                 :data-index index
                 :value (d/nilv (nth colors index nil)
                                (nth default-shader-colors index))
                 :title (str "u_color" (inc index))
                 :on-change on-change-color}])]

     [:div {:class (stl/css :uniform-row)}
      (for [index (range 4)]
        [:input {:key (str "shader-param-" index)
                 :class (stl/css :param-input)
                 :type "number"
                 :step 0.1
                 :data-index index
                 :value (d/nilv (nth params index nil) 0)
                 :title (str "u_param" (inc index))
                 :on-change on-change-param}])]

     [:div {:class (stl/css :help)}
      (tr "workspace.colorpicker.shader.help")]]))
