;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.offset-lines
  "Offset lines (Penpot Motion, see `app.common.geom.offset-lines`): of a
  shape, the + that makes them; of a group of them, how they are made,
  and remaking them from their source."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.geom.offset-lines :as gol]
   [app.common.math :as mth]
   [app.main.data.workspace.offset-lines :as dwol]
   [app.main.store :as st]
   [app.main.ui.components.numeric-input :as deprecated-input]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(mf/defc ^:private number-field*
  [{:keys [label hint unit value min max scale on-change]}]
  (let [scale     (or scale 1)
        on-change (mf/use-fn
                   (mf/deps on-change scale)
                   (fn [v] (when (number? v) (on-change (/ v scale)))))]
    [:div {:class (stl/css :field) :title hint}
     [:span {:class (stl/css :field-label)} label]
     [:> deprecated-input/numeric-input*
      {:no-validate true
       :min min
       :max max
       :aria-label hint
       :on-change on-change
       :class (stl/css :field-input)
       :value (mth/precision (* scale value) 2)}]
     [:span {:class (stl/css :field-unit)} unit]]))

(mf/defc ^:private color-field*
  "A colour as hex, `#` and six digits, shown as the colour rows show it:
  six digits."
  [{:keys [label hint value on-change]}]
  (let [value   (str/lower value)
        on-blur (mf/use-fn
                 (mf/deps on-change value)
                 (fn [event]
                   (let [text (str/lower (str/trim (dom/get-value (dom/get-target event))))
                         text (if (str/starts-with? text "#") text (str "#" text))]
                     (when (and (not= text value) (re-matches #"#[0-9a-f]{6}" text))
                       (on-change text)))))
        on-key  (mf/use-fn
                 (fn [event]
                   (when (= "Enter" (.-key event))
                     (dom/blur! (dom/get-target event)))))]
    [:div {:class (stl/css :field) :title hint}
     [:span {:class (stl/css :swatch) :style {:background-color value}}]
     [:span {:class (stl/css :field-label)} label]
     [:input {:class (stl/css :field-text)
              :key value
              :default-value (subs value 1)
              :aria-label hint
              :on-blur on-blur
              :on-key-down on-key}]]))

(mf/defc offset-lines-menu*
  "Of the single selected `shape`: the + that makes offset lines of it,
  or, of a group of them, how they are made."
  [{:keys [shape]}]
  (let [spec   (some->> (:offset-lines shape) (merge gol/defaults))
        id     (:id shape)
        on-add (mf/use-fn (mf/deps id) #(st/emit! (dwol/make-offset-lines id)))
        change (mf/use-fn (mf/deps id) (fn [k v] (st/emit! (dwol/set-offset-lines id {k v}))))
        on-update (mf/use-fn (mf/deps id) #(st/emit! (dwol/set-offset-lines id)))]
    [:section {:class (stl/css :offset-lines)
               :aria-label (tr "workspace.options.offset-lines")}
     [:> title-bar* {:collapsable false
                     :title (tr "workspace.options.offset-lines")
                     :class (stl/css :title)}
      (when (nil? spec)
        [:> icon-button* {:variant "ghost"
                          :aria-label (tr "workspace.options.offset-lines.add")
                          :on-click on-add
                          :icon i/add}])]
     (when (some? spec)
       [:div {:class (stl/css :fields)}
        [:> number-field* {:label (tr "workspace.options.offset-lines.count")
                           :hint (tr "workspace.options.offset-lines.count.hint")
                           :unit "" :min 1 :max 200 :value (:count spec)
                           :on-change #(change :count (mth/round %))}]
        [:> number-field* {:label (tr "workspace.options.offset-lines.spacing")
                           :hint (tr "workspace.options.offset-lines.spacing.hint")
                           :unit "px" :min 0.1 :max 1000 :value (:spacing spec)
                           :on-change #(change :spacing %)}]
        [:> number-field* {:label (tr "workspace.options.offset-lines.width")
                           :hint (tr "workspace.options.offset-lines.width.hint")
                           :unit "px" :min 0.1 :max 1000 :value (:width spec)
                           :on-change #(change :width %)}]
        [:div {:class (stl/css :spacer)}]
        [:> color-field* {:label (tr "workspace.options.offset-lines.inner")
                          :hint (tr "workspace.options.offset-lines.inner.hint")
                          :value (:inner-color spec)
                          :on-change #(change :inner-color %)}]
        [:> number-field* {:label "" :hint (tr "workspace.options.offset-lines.inner.hint")
                           :unit "%" :min 0 :max 100 :scale 100 :value (:inner-opacity spec)
                           :on-change #(change :inner-opacity (mth/clamp % 0 1))}]
        [:> color-field* {:label (tr "workspace.options.offset-lines.outer")
                          :hint (tr "workspace.options.offset-lines.outer.hint")
                          :value (:outer-color spec)
                          :on-change #(change :outer-color %)}]
        [:> number-field* {:label "" :hint (tr "workspace.options.offset-lines.outer.hint")
                           :unit "%" :min 0 :max 100 :scale 100 :value (:outer-opacity spec)
                           :on-change #(change :outer-opacity (mth/clamp % 0 1))}]
        [:div {:class (stl/css :update)}
         [:> button* {:variant "secondary"
                      :title (tr "workspace.options.offset-lines.update.hint")
                      :on-click on-update}
          (tr "workspace.options.offset-lines.update")]]])]))
