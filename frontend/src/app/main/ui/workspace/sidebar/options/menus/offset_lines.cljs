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
   [app.common.data :as d]
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
   [app.main.ui.workspace.sidebar.options.menus.motion :refer [use-section-open]]
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

(defn- free-line
  "The line of the `n` lines nearest their middle that has none of
  `accents`, or nil when each has one."
  [n accents]
  (let [taken  (into #{} (map :line) accents)
        middle (inc (quot n 2))]
    (->> (range 1 (inc n))
         (sort-by #(mth/abs (- % middle)))
         (d/seek (complement taken)))))

(defn- shown-opacity
  "The opacity the accent `accent` of offset lines `spec` gives its line:
  its own, or the one the line has without it (see `gol/look`)."
  [spec {:keys [line opacity]}]
  (or opacity (:opacity (gol/look (dissoc spec :accents) line (:count spec)))))

(mf/defc ^:private accents*
  "The accents of offset lines `spec` (see `gol/look`): lines of their
  own colour, opacity and width, by number from the inner side, each
  with its −, and a + for one more on a line that has none; `on-change`
  gets them all. An accent without an opacity or a width shows the one
  its line has without it."
  [{:keys [spec disabled on-change]}]
  (let [accents (vec (:accents spec))
        edit    (fn [i k v] (on-change (assoc-in accents [i k] v)))
        free    (free-line (:count spec) accents)
        on-add  (mf/use-fn
                 (mf/deps accents spec free on-change)
                 #(when (some? free)
                    (on-change (conj accents {:line free
                                              :color "#ff7a7a"
                                              :opacity 1
                                              :width (:width spec)}))))]
    [:*
     [:div {:class (stl/css :accents-title)}
      [:span (tr "workspace.options.offset-lines.accents")]
      (when-not disabled
        [:> icon-button* {:variant "ghost"
                          :aria-label (tr "workspace.options.offset-lines.accents.add")
                          :on-click on-add
                          :disabled (nil? free)
                          :icon i/add}])]
     (for [[i {:keys [line color width] :as accent}] (map-indexed vector accents)]
       [:div {:key (str "accent-" i) :class (stl/css :accent)}
        [:> labelled-number* {:label (tr "workspace.options.offset-lines.accent.line")
                              :hint (tr "workspace.options.offset-lines.accent.line.hint")
                              :unit "" :min 1 :max (:count spec) :value line :disabled disabled
                              :on-change #(edit i :line (mth/clamp (mth/round %) 1 (:count spec)))}]
        [:> color-field* {:label "" :hint (tr "workspace.options.offset-lines.accent.color")
                          :value color :disabled disabled
                          :on-change #(edit i :color %)}]
        (if disabled
          ;; its place kept, so the fields sit as they do with it
          [:span]
          [:> icon-button* {:variant "ghost"
                            :aria-label (tr "workspace.options.offset-lines.accents.remove")
                            :on-click #(on-change (into (subvec accents 0 i) (subvec accents (inc i))))
                            :icon i/remove}])
        [:> labelled-number* {:label "" :hint (tr "workspace.options.offset-lines.accent.opacity")
                              :unit "%" :min 0 :max 100 :scale 100
                              :value (shown-opacity spec accent)
                              :disabled disabled
                              :on-change #(edit i :opacity (mth/clamp % 0 1))}]
        [:> labelled-number* {:label "" :hint (tr "workspace.options.offset-lines.accent.width")
                              :unit "px" :min 0.1 :max 1000 :value (or width (:width spec)) :disabled disabled
                              :on-change #(edit i :width %)}]])]))

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
        [shown? toggle-shown show] (use-section-open :offset-lines)
        on-add    (mf/use-fn (mf/deps id show) (fn [] (show) (st/emit! (dwol/make-offset-lines id))))
        change    (mf/use-fn (mf/deps id) (fn [k v] (st/emit! (dwol/set-offset-lines id {k v}))))
        on-update (mf/use-fn (mf/deps id) #(st/emit! (dwol/set-offset-lines id)))]
    [:section {:class (stl/css :offset-lines)
               :aria-label (tr "workspace.options.offset-lines")}
     [:> title-bar* {:collapsable (some? spec)
                     :collapsed (not shown?)
                     :on-collapsed toggle-shown
                     :title (tr "workspace.options.offset-lines")
                     :class (stl/css :title)}
      (when (and (nil? spec) (not copy?))
        [:> icon-button* {:variant "ghost"
                          :aria-label (tr "workspace.options.offset-lines.add")
                          :on-click on-add
                          :icon i/add}])]
     (cond
       (or (nil? spec) (not shown?))
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
        ;; the dark threads: every nth line dimmer
        [:> labelled-number* {:label (tr "workspace.options.offset-lines.stripes")
                              :hint (tr "workspace.options.offset-lines.stripes.hint")
                              :unit "" :min 0 :max 200 :value (or (:stripe-every spec) 0) :disabled copy?
                              :on-change #(change :stripe-every (when (>= % 2) (mth/round %)))}]
        [:> labelled-number* {:label (tr "workspace.options.offset-lines.dim")
                              :hint (tr "workspace.options.offset-lines.dim.hint")
                              :unit "%" :min 0 :max 100 :scale 100 :value (:stripe-dim spec 0) :disabled copy?
                              :on-change #(change :stripe-dim (mth/clamp % 0 1))}]
        [:> accents* {:spec spec :disabled copy? :on-change #(change :accents (not-empty %))}]
        (if copy?
          [:p {:class (stl/css :hint :wide)} (tr "workspace.options.offset-lines.copy")]
          [:div {:class (stl/css :update)}
           [:> button* {:variant "secondary"
                        :title (tr "workspace.options.offset-lines.update.hint")
                        :on-click on-update}
            (tr "workspace.options.offset-lines.update")]])])]))
