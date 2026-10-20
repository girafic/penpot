;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.offset-lines
  "Offset lines (Penpot Motion, see `app.common.geom.offset-lines`): of a
  shape, the + that makes them; of a group of them, how they are made,
  and remaking them from their source; read-only in a copy of a
  component, which takes them from its main."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.geom.offset-lines :as gol]
   [app.common.math :as mth]
   [app.common.schema :as sm]
   [app.common.types.component :as ctk]
   [app.common.types.shape :as cts]
   [app.main.data.workspace.offset-lines :as dwol]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.workspace.sidebar.options.menus.motion-field :refer [labelled-number*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(mf/defc ^:private color-field*
  "A colour as hex, `#` and six digits, shown as the colour rows show it:
  six digits."
  [{:keys [label hint value disabled on-change]}]
  (let [value   (str/lower value)
        on-blur (mf/use-fn
                 (mf/deps on-change value)
                 (fn [event]
                   (let [text (str/lower (str/trim (dom/get-value (dom/get-target event))))
                         text (if (str/starts-with? text "#") text (str "#" text))]
                     (when (and (not= text value) (sm/validate cts/schema:six-digit-hex text))
                       (on-change text)))))
        on-key  (mf/use-fn
                 (fn [event]
                   (when (= "Enter" (.-key event))
                     (dom/blur! (dom/get-target event)))))]
    [:div {:class (stl/css :color-field) :title hint}
     [:span {:class (stl/css :swatch) :style {:background-color value}}]
     [:span {:class (stl/css :color-label)} label]
     [:input {:class (stl/css :color-text)
              :key value
              :default-value (subs value 1)
              :disabled disabled
              :aria-label hint
              :on-blur on-blur
              :on-key-down on-key}]]))

(mf/defc offset-lines-menu*
  "Of the single selected `shape`: the + that makes offset lines of it,
  or, of a group of them, how they are made; in a copy of a component
  read-only, and a word when their source is gone."
  [{:keys [shape]}]
  (let [objects   (mf/deref refs/workspace-page-objects)
        spec      (some->> (:offset-lines shape) (merge gol/defaults))
        id        (:id shape)
        copy?     (ctk/in-component-copy? shape)
        source?   (some? (gol/layers-of objects shape))
        on-add    (mf/use-fn (mf/deps id) #(st/emit! (dwol/make-offset-lines id)))
        change    (mf/use-fn (mf/deps id) (fn [k v] (st/emit! (dwol/set-offset-lines id {k v}))))
        on-update (mf/use-fn (mf/deps id) #(st/emit! (dwol/set-offset-lines id)))]
    [:section {:class (stl/css :offset-lines)
               :aria-label (tr "workspace.options.offset-lines")}
     [:> title-bar* {:collapsable false
                     :title (tr "workspace.options.offset-lines")
                     :class (stl/css :title)}
      (when (and (nil? spec) (not copy?))
        [:> icon-button* {:variant "ghost"
                          :aria-label (tr "workspace.options.offset-lines.add")
                          :on-click on-add
                          :icon i/add}])]
     (cond
       (nil? spec)
       nil

       (not source?)
       [:p {:class (stl/css :hint)} (tr "workspace.options.offset-lines.no-source")]

       :else
       [:div {:class (stl/css :fields)}
        [:> labelled-number* {:label (tr "workspace.options.offset-lines.count")
                              :hint (tr "workspace.options.offset-lines.count.hint")
                              :unit "" :min 1 :max 200 :value (:count spec) :disabled copy?
                              :on-change #(change :count (mth/round %))}]
        [:> labelled-number* {:label (tr "workspace.options.offset-lines.spacing")
                              :hint (tr "workspace.options.offset-lines.spacing.hint")
                              :unit "px" :min 0.1 :max 1000 :value (:spacing spec) :disabled copy?
                              :on-change #(change :spacing %)}]
        [:> labelled-number* {:label (tr "workspace.options.offset-lines.width")
                              :hint (tr "workspace.options.offset-lines.width.hint")
                              :unit "px" :min 0.1 :max 1000 :value (:width spec) :disabled copy?
                              :on-change #(change :width %)}]
        [:div {:class (stl/css :spacer)}]
        [:> color-field* {:label (tr "workspace.options.offset-lines.inner")
                          :hint (tr "workspace.options.offset-lines.inner.hint")
                          :value (:inner-color spec) :disabled copy?
                          :on-change #(change :inner-color %)}]
        [:> labelled-number* {:label "" :hint (tr "workspace.options.offset-lines.inner.hint")
                              :unit "%" :min 0 :max 100 :scale 100 :value (:inner-opacity spec) :disabled copy?
                              :on-change #(change :inner-opacity (mth/clamp % 0 1))}]
        [:> color-field* {:label (tr "workspace.options.offset-lines.outer")
                          :hint (tr "workspace.options.offset-lines.outer.hint")
                          :value (:outer-color spec) :disabled copy?
                          :on-change #(change :outer-color %)}]
        [:> labelled-number* {:label "" :hint (tr "workspace.options.offset-lines.outer.hint")
                              :unit "%" :min 0 :max 100 :scale 100 :value (:outer-opacity spec) :disabled copy?
                              :on-change #(change :outer-opacity (mth/clamp % 0 1))}]
        (if copy?
          [:p {:class (stl/css :hint :wide)} (tr "workspace.options.offset-lines.copy")]
          [:div {:class (stl/css :update)}
           [:> button* {:variant "secondary"
                        :title (tr "workspace.options.offset-lines.update.hint")
                        :on-click on-update}
            (tr "workspace.options.offset-lines.update")]])])]))
