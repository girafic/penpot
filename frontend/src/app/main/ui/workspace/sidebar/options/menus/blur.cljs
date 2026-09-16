;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.blur
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.data.workspace :as udw]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.features :as features]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.tooltip.tooltip :refer [tooltip*]]
   [app.main.ui.workspace.sidebar.options.menus.glass :refer [create-glass glass-options*]]
   [app.util.i18n :as i18n :refer [tr]]
   [rumext.v2 :as mf]))

(def blur-attrs [:blur :background-blur :glass])

(def ^:private type->key
  {:layer-blur :blur
   :background-blur :background-blur
   :glass :glass})

(def ^:private key->type
  {:blur :layer-blur
   :background-blur :background-blur
   :glass :glass})

(defn create-blur [type]
  (let [id (uuid/next)]
    {:id id
     :type type
     :value 4
     :hidden false}))

(defn- create-effect
  [key]
  (if (= key :glass)
    (create-glass)
    (create-blur (key->type key))))

(defn- convert-effect
  "Turns the effect `value` into one of `type`. The blur amount carries over
  as the glass frost and back."
  [value type]
  (cond
    (= type :glass)
    (cond-> (create-glass)
      (some? (:value value)) (assoc :frost (:value value))
      (some? (:hidden value)) (assoc :hidden (:hidden value)))

    (= (:type value) :glass)
    (cond-> (create-blur type)
      (some? (:frost value)) (assoc :value (:frost value))
      (some? (:hidden value)) (assoc :hidden (:hidden value)))

    :else
    (assoc value :type type)))

(defn- use-available-keys
  "Effect keys the user can add with the current renderer and flags."
  []
  (let [render-wasm? (features/use-feature "render-wasm/v1")
        bg-blur?     (and render-wasm? (contains? cf/flags :background-blur))
        glass?       (and render-wasm? (contains? cf/flags :glass))]
    (mf/with-memo [bg-blur? glass?]
      (cond-> [:blur]
        bg-blur? (conj :background-blur)
        glass?   (conj :glass)))))

(mf/defc blur-menu-content*
  [{:keys [blur-key value change-fn blur-values available-keys]}]
  (let [is-hidden           (get value :hidden)
        show-more-options*  (mf/use-state false)
        show-more-options   (deref show-more-options*)
        toggle-more-options (mf/use-fn #(swap! show-more-options* not))

        ;; The effect exists but can't be rendered/edited with the current
        ;; renderer (e.g. background blur or glass without render-wasm).
        unsupported?        (not (contains? (set available-keys) blur-key))
        selectable?         (> (count available-keys) 1)

        handle-delete
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn []
           (change-fn #(dissoc % blur-key))))

        handle-toggle-visibility
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn []
           (change-fn #(update-in % [blur-key :hidden] not))))

        handle-change
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn [value]
           (change-fn #(assoc-in % [blur-key :value] value))))

        handle-glass-change
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn [attr value]
           (change-fn #(assoc-in % [blur-key attr] value))))

        handle-type-change
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn [type]
           (let [type-kw    (keyword type)
                 target-key (get type->key type-kw)]
             (change-fn
              (fn [shape]
                (cond
                  ;; same type
                  (= blur-key target-key)
                  shape

                  ;; an effect of the target type already exists
                  (contains? shape target-key)
                  shape

                  ;; the source effect doesn't exist
                  (not (contains? shape blur-key))
                  shape

                  :else
                  (let [effect (get shape blur-key)]
                    (-> shape
                        (dissoc blur-key)
                        (assoc target-key (convert-effect effect type-kw))))))))))

        used-keys
        (mf/with-memo [blur-values]
          (into #{} (map :key) blur-values))

        type-options
        (mf/with-memo [available-keys used-keys blur-key]
          (let [labels {:blur (tr "workspace.options.blur-options.layer-blur")
                        :background-blur (tr "workspace.options.blur-options.background-blur")
                        :glass (tr "workspace.options.blur-options.glass")}]
            (mapv (fn [key]
                    (let [type (d/name (key->type key))]
                      {:value type
                       :id type
                       :label (get labels key)
                       :disabled (and (not= key blur-key)
                                      (contains? used-keys key))}))
                  available-keys)))

        label-ref (mf/use-ref nil)

        label-text
        (cond
          (= blur-key :background-blur)
          (tr "workspace.options.blur-options.background-blur")

          (= blur-key :glass)
          (tr "workspace.options.blur-options.glass")

          selectable?
          (tr "workspace.options.blur-options.layer-blur")

          :else
          (tr "labels.blur"))

        label
        (mf/html [:span {:aria-labelledby "background-blur-disabled-label"
                         :ref label-ref
                         :class (stl/css-case :label true
                                              :disabled-label unsupported?)}
                  label-text])]

    [:*
     [:div {:class (stl/css-case :first-row true
                                 :hidden is-hidden)}
      [:div {:class (stl/css :blur-info)
             :data-testid "blur-info"}
       [:> icon-button* {:class (stl/css-case :show-more true
                                              :selected show-more-options)
                         :on-click toggle-more-options
                         :selected show-more-options
                         :variant "ghost"
                         :disabled (or is-hidden unsupported?)
                         :aria-label (tr "workspace.options.blur-options.toggle-more-options")
                         :icon i/menu}]
       (cond (and selectable? (not unsupported?))
             [:> select*
              {:class (stl/css :blur-type-select)
               :default-selected (d/name (:type value))
               :aria-label (tr "workspace.options.blur-options.blur-type-select")
               :options type-options
               :disabled is-hidden
               :on-change handle-type-change}]
             unsupported?
             [:> tooltip*
              {:trigger-ref label-ref
               :id "background-blur-disabled-label"
               :class (stl/css :disabled-label-tooltip)
               :content (if (= blur-key :glass)
                          (tr "workspace.options.glass-options.disabled-label")
                          (tr "workspace.options.blur-options.disabled-blur-label"))}
              label]
             :else
             label)]

      [:div {:class (stl/css :actions)}
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.options.blur-options.toggle-blur")
                         :on-click handle-toggle-visibility
                         :disabled unsupported?
                         :tooltip-placement "top-left"
                         :icon (if (or is-hidden unsupported?) i/hide i/shown)}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.options.blur-options.remove-blur")
                         :on-click handle-delete
                         :tooltip-placement "top-left"
                         :icon i/remove}]]]

     (when show-more-options
       (if (= blur-key :glass)
         [:> glass-options* {:value value
                             :disabled is-hidden
                             :on-change handle-glass-change}]
         [:div {:class (stl/css :second-row)}
          [:> numeric-input*
           {:class (stl/css :numeric-input)
            :placeholder "--"
            :min 0
            :text-icon "value"
            :on-change handle-change
            :name "blur-value"
            :value (:value value)}]]))]))

(defn get-blurs [values]
  (into []
        (keep (fn [key]
                (when-let [value (get values key)]
                  {:key key :value value})))
        blur-attrs))

(defn- check-blur-menu-props
  [old-props new-props]
  (let [old-values (unchecked-get old-props "values")
        new-values (unchecked-get new-props "values")]
    (and (identical? (unchecked-get old-props "ids")
                     (unchecked-get new-props "ids"))
         (identical? (unchecked-get old-props "type")
                     (unchecked-get new-props "type"))
         (identical? (get old-values :blur)
                     (get new-values :blur))
         (identical? (get old-values :background-blur)
                     (get new-values :background-blur))
         (identical? (get old-values :glass)
                     (get new-values :glass)))))

(mf/defc blur-menu*
  {::mf/wrap [#(mf/memo' % check-blur-menu-props)]}
  [{:keys [ids type values]}]
  (let [available-keys (use-available-keys)
        multi-type?    (> (count available-keys) 1)
        glass?         (contains? (set available-keys) :glass)

        blur-values    (get-blurs values)

        mixed-state (and (or (= :group type)
                             (= :multiple type))
                         (boolean
                          (some #(= :multiple (:value %)) blur-values)))

        state*         (mf/use-state {:show-content true})
        state          (deref state*)
        open?          (:show-content state)

        toggle-content (mf/use-fn #(swap! state* update :show-content not))

        change!
        (mf/use-fn
         (mf/deps ids)
         (fn [update-fn]
           (st/emit! (dwsh/update-shapes ids update-fn)
                     (udw/trigger-bounding-box-cloaking ids))))

        handle-delete-all
        (mf/use-fn
         (mf/deps change!)
         (fn []
           (change! #(apply dissoc % blur-attrs))))

        next-key
        (d/seek #(nil? (get values %)) available-keys)

        handle-add
        (mf/use-fn
         (mf/deps change! next-key)
         (fn []
           (when (some? next-key)
             (change! #(assoc % next-key (create-effect next-key))))))

        title
        (cond
          glass?
          (case type
            :multiple (tr "workspace.options.effects-options.title.multiple")
            :group (tr "workspace.options.effects-options.title.group")
            (tr "workspace.options.effects-options.title"))

          multi-type?
          (case type
            :multiple (tr "workspace.options.blur-effects-options.title.multiple")
            :group (tr "workspace.options.blur-effects-options.title.group")
            (tr "labels.blur-effects"))

          :else
          (case type
            :multiple (tr "workspace.options.blur-options.title.multiple")
            :group (tr "workspace.options.blur-options.title.group")
            (tr "labels.blur")))]

    [:section {:class (stl/css :element-set)
               :hidden (not open?)
               :aria-label (cond
                             glass? (tr "workspace.options.effects-options.title")
                             multi-type? (tr "labels.blur-effects")
                             :else (tr "labels.blur"))}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable  (seq blur-values)
                      :collapsed    (not open?)
                      :on-collapsed toggle-content
                      :aria-expanded open?
                      :aria-controls "blur-content"
                      :title        title
                      :class        (stl/css-case :title-spacing-blur (not (seq blur-values))
                                                  :long-title true)}
       (when (and (not mixed-state)
                  (some? next-key))
         [:> icon-button*
          {:variant "ghost"
           :aria-label (tr "workspace.options.blur-options.add-blur")
           :on-click handle-add
           :icon i/add
           :tooltip-placement "top-left"
           :data-testid "add-blur"}])]]
     (when (and open? (seq blur-values))
       [:div {:class (stl/css :element-set-content)
              :hidden (not open?)
              :id "blur-content"}
        (if mixed-state
          [:div  {:class (stl/css :first-row)}
           [:span {:class (stl/css :mixed-label)}
            (tr "settings.multiple")]
           [:> icon-button* {:variant "ghost"
                             :aria-label (tr "workspace.options.blur-options.remove-blur")
                             :on-click handle-delete-all
                             :tooltip-placement "top-left"
                             :icon i/remove}]]

          (for [{:keys [key value]} blur-values]
            [:> blur-menu-content*
             {:key key
              :blur-key key
              :value value
              :blur-values blur-values
              :available-keys available-keys
              :change-fn change!}]))])]))
