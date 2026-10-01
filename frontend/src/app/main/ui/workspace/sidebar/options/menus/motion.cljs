;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.options.menus.motion
  "Motion mode additions to the design tab: the keyframe diamond beside
  the animatable rows and the preset animations of the selection."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.path-names :as cpn]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.uuid :as uuid]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.animation :as dwa]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu* dropdown-menu-item*]]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.controls.radio-buttons :refer [radio-buttons*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.workspace.sidebar.options.menus.interactions :refer [prototype-pill*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:playhead
  (l/derived dwa/shown-playhead st/state))

(def ^:private ref:timeline
  (l/derived dwa/current-timeline st/state))

(mf/defc keyframe-diamond*
  "Toggle keyframes of `properties` (optional `index`) at the playhead,
  unless they are locked. Renders nothing outside motion mode."
  [{:keys [properties index]}]
  (let [shape-id (mf/use-ctx ctx/motion-shape-id)
        playhead (mf/deref ref:playhead)
        timeline (mf/deref ref:timeline)
        active?  (boolean (and timeline
                               (some #(dwa/keyframe-at timeline shape-id % playhead index)
                                     properties)))
        locked?  (every? #(cta/slot-flag? timeline shape-id :locked % index) properties)
        on-toggle
        (mf/use-fn
         (mf/deps shape-id properties index)
         #(st/emit! (dwa/toggle-keyframes shape-id properties index)))]
    (when shape-id
      [:button {:type "button"
                :class (stl/css-case :keyframe-toggle true
                                     :active active?)
                :title (if active?
                         (tr "workspace.animation.remove-keyframe")
                         (tr "workspace.animation.add-keyframe"))
                :aria-label (if active?
                              (tr "workspace.animation.remove-keyframe")
                              (tr "workspace.animation.add-keyframe"))
                :aria-pressed active?
                :disabled locked?
                :on-click on-toggle}
       [:span {:class (stl/css :diamond)}]])))

;; PATH TRIM

(mf/defc trim-row*
  "Path trim of the strokes of the layer: from where to where along its
  outline they are drawn, and how far that part is moved along. Only in
  motion mode, keyframed: a layer has no trim of its own."
  []
  (let [shape-id (mf/use-ctx ctx/motion-shape-id)
        playhead (mf/deref ref:playhead)
        timeline (mf/deref ref:timeline)
        objects  (mf/deref refs/workspace-page-objects)
        values   (mf/with-memo [timeline objects shape-id playhead]
                   (when (and (some? shape-id) (some? timeline))
                     (-> (cta/resolve-animations timeline objects)
                         (update :tracks select-keys [shape-id])
                         (cta/values-at playhead)
                         (get shape-id))))
        display  (fn [property]
                   (mth/round (dwa/value->display property (get values property (get cta/trim-properties property)))))

        change
        (mf/use-fn
         (mf/deps shape-id)
         (fn [property display]
           (when (number? display)
             (st/emit! (dwa/set-value-at-playhead shape-id property
                                                  (dwa/display->value property display))))))

        on-start  (mf/use-fn (mf/deps change) #(change :trim-start %))
        on-end    (mf/use-fn (mf/deps change) #(change :trim-end %))
        on-offset (mf/use-fn (mf/deps change) #(change :trim-offset %))]

    (when (some? shape-id)
      [:div {:class (stl/css :trim-row)}
       [:span {:class (stl/css :trim-label)} (tr "workspace.animation.path-trim")]
       [:div {:class (stl/css :trim-fields)}
        [:> numeric-input* {:text-icon "%"
                            :property (tr "workspace.animation.trim-start")
                            :min 0
                            :max 100
                            :value (display :trim-start)
                            :on-change on-start}]
        [:> numeric-input* {:text-icon "%"
                            :property (tr "workspace.animation.trim-end")
                            :min 0
                            :max 100
                            :value (display :trim-end)
                            :on-change on-end}]
        [:> numeric-input* {:text-icon "%"
                            :property (tr "workspace.animation.trim-offset")
                            :value (display :trim-offset)
                            :on-change on-offset}]
        [:> keyframe-diamond* {:properties [:trim-start :trim-end :trim-offset]}]]])))

;; PRESET ANIMATIONS

(def ^:private preset-types
  [:fade :move :scale :rotate])

(def ^:private preset-styles
  [:slide-up :slide-down :slide-left :slide-right :pop :zoom :spin])

(defn- preset-label
  [preset]
  (case preset
    :fade        (tr "workspace.animation.preset.fade")
    :move        (tr "workspace.animation.preset.move")
    :scale       (tr "workspace.animation.preset.scale")
    :rotate      (tr "workspace.animation.preset.rotate")
    :slide-up    (tr "workspace.animation.preset.slide-up")
    :slide-down  (tr "workspace.animation.preset.slide-down")
    :slide-left  (tr "workspace.animation.preset.slide-left")
    :slide-right (tr "workspace.animation.preset.slide-right")
    :pop         (tr "workspace.animation.preset.pop")
    :zoom        (tr "workspace.animation.preset.zoom")
    :spin        (tr "workspace.animation.preset.spin")
    (name preset)))

(defn animation-label
  "Name of a preset animation, or of the style a style use plays, as the
  sidebar and the timeline show it."
  [{:keys [type direction] :as animation}]
  (let [out? (= direction :out)]
    (case type
      :style  (:name animation)
      :fade   (if out? (tr "workspace.animation.fade-out") (tr "workspace.animation.fade-in"))
      :move   (if out? (tr "workspace.animation.move-out") (tr "workspace.animation.move-in"))
      :scale  (if out? (tr "workspace.animation.scale-out") (tr "workspace.animation.scale-in"))
      :rotate (if out? (tr "workspace.animation.rotate-out") (tr "workspace.animation.rotate-in"))
      (name type))))

(defn- easing->option
  [easing]
  (cond
    (nil? easing)                   "auto"
    (keyword? easing)               (name easing)
    (= easing cta/overshoot-easing) "overshoot"
    :else                           "custom"))

(defn- option->easing
  [option]
  (case option
    "auto"      nil
    "overshoot" cta/overshoot-easing
    (keyword option)))

(defn- number-handler
  "Call `f` with the number an input gives, skipping empty values."
  [f]
  (fn [value]
    (when (number? value)
      (f value))))

(mf/defc animation-row*
  {::mf/private true}
  [{:keys [label children]}]
  [:div {:class (stl/css :animation-row)}
   [:div {:class (stl/css :animation-row-name)} label]
   [:div {:class (stl/css :animation-row-control)} children]])

(mf/defc animation-item*
  "One preset animation, `animation`: its name and span, and, once
  opened, its settings, each change going to `on-change` as the
  attributes changed; a drag on a number field starts with
  `on-change-start` and ends with `on-change-end`. A locked one shows
  them disabled, and its lock, `on-unlock`, in place of the remove
  button, `on-remove` (disabled when nil); a `read-only` one shows them
  disabled. The layers list and the style editor both use it."
  [{:keys [animation on-change on-remove on-unlock on-change-start on-change-end read-only]}]
  (let [{:keys [id type direction start duration easing amount offset-x offset-y locked]} animation
        read-only? (true? read-only)
        locked?    (or (true? locked) read-only?)

        open*  (mf/use-state false)
        open?  (deref open*)

        on-toggle
        (mf/use-fn #(swap! open* not))

        change
        (mf/use-fn
         (mf/deps on-change)
         (fn [attrs]
           (on-change attrs)))

        on-type
        (mf/use-fn
         (mf/deps change)
         ;; A new type starts from its own settings.
         (fn [value]
           (change {:type (keyword value) :amount nil :offset-x nil :offset-y nil})))

        on-direction
        (mf/use-fn
         (mf/deps change)
         (fn [value]
           (change {:direction (keyword value)})))

        on-easing
        (mf/use-fn
         (mf/deps change)
         (fn [value]
           (change {:easing (option->easing value)})))

        on-start    (mf/use-fn (mf/deps change) (number-handler #(change {:start %})))
        on-duration (mf/use-fn (mf/deps change) (number-handler #(change {:duration %})))
        on-percent  (mf/use-fn (mf/deps change) (number-handler #(change {:amount (/ % 100)})))
        on-angle    (mf/use-fn (mf/deps change) (number-handler #(change {:amount %})))
        on-offset-x (mf/use-fn (mf/deps change) (number-handler #(change {:offset-x %})))
        on-offset-y (mf/use-fn (mf/deps change) (number-handler #(change {:offset-y %})))

        type-options
        (mf/with-memo []
          (mapv (fn [type] {:value (name type) :label (preset-label type)}) preset-types))

        easing-option
        (easing->option easing)

        easing-options
        (mf/with-memo [easing-option]
          (cond-> [{:value "auto" :label (tr "workspace.animation.easing-auto")}
                   {:value "linear" :label (tr "workspace.options.interaction-easing-linear")}
                   {:value "ease" :label (tr "workspace.options.interaction-easing-ease")}
                   {:value "ease-in" :label (tr "workspace.options.interaction-easing-ease-in")}
                   {:value "ease-out" :label (tr "workspace.options.interaction-easing-ease-out")}
                   {:value "ease-in-out" :label (tr "workspace.options.interaction-easing-ease-in-out")}
                   {:value "overshoot" :label (tr "workspace.animation.easing-overshoot")}]
            (= easing-option "custom")
            (conj {:value "custom" :label (tr "workspace.animation.easing-custom") :disabled true})))]

    [:div {:class (stl/css :animation-item)}
     [:> prototype-pill* {:title (animation-label animation)
                          :description (dm/str start "–" (cta/animation-end animation) " ms")
                          :left-button-icon-id i/hsva
                          :left-button-tooltip (tr "labels.options")
                          :is-left-button-active open?
                          :on-left-button-click on-toggle
                          :right-button-icon-id (if (and locked? (not read-only?)) i/lock i/remove)
                          :right-button-tooltip (if (and locked? (not read-only?))
                                                  (tr "workspace.shape.menu.unlock")
                                                  (tr "workspace.animation.remove-animation"))
                          :is-right-button-disabled (or read-only? (nil? (if locked? on-unlock on-remove)))
                          :on-right-button-click (when-not read-only? (if locked? on-unlock on-remove))}]

     (when open?
       [:*
        [:> animation-row* {:label (tr "workspace.animation.type")}
         [:& select {:default-value (name type)
                     :options type-options
                     :disabled locked?
                     :on-change on-type}]]

        [:> animation-row* {:label (tr "workspace.animation.direction")}
         [:> radio-buttons* {:name (dm/str "animation-direction-" id)
                             :selected (name direction)
                             :extended true
                             :options [{:id (dm/str "animation-in-" id)
                                        :value "in"
                                        :label (tr "workspace.options.interaction-in")}
                                       {:id (dm/str "animation-out-" id)
                                        :value "out"
                                        :label (tr "workspace.options.interaction-out")}]
                             :disabled locked?
                             :on-change on-direction}]]

        [:> animation-row* {:label (tr "workspace.animation.start")}
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.start")
                             :min 0
                             :step 1
                             :value start
                             :disabled locked?
                             :on-change-start on-change-start
                             :on-change-end on-change-end
                             :on-change on-start}]]

        [:> animation-row* {:label (tr "workspace.animation.duration")}
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.duration")
                             :min 1
                             :step 1
                             :value duration
                             :disabled locked?
                             :on-change-start on-change-start
                             :on-change-end on-change-end
                             :on-change on-duration}]]

        [:> animation-row* {:label (tr "workspace.animation.easing")}
         [:& select {:default-value easing-option
                     :options easing-options
                     :disabled locked?
                     :on-change on-easing}]]

        (case type
          :fade
          [:> animation-row* {:label (tr "workspace.options.opacity")}
           [:> numeric-input* {:text-icon "%"
                               :property (tr "workspace.options.opacity")
                               :min 0
                               :max 100
                               :value (mth/round (* 100 (or amount 0)))
                               :disabled locked?
                               :on-change-start on-change-start
                               :on-change-end on-change-end
                               :on-change on-percent}]]

          :scale
          [:> animation-row* {:label (tr "workspace.animation.scale")}
           [:> numeric-input* {:text-icon "%"
                               :property (tr "workspace.animation.scale")
                               :min 0
                               :step 10
                               :value (mth/round (* 100 (or amount 1)))
                               :disabled locked?
                               :on-change-start on-change-start
                               :on-change-end on-change-end
                               :on-change on-percent}]]

          :rotate
          [:> animation-row* {:label (tr "workspace.options.rotation")}
           [:> numeric-input* {:icon i/rotation
                               :property (tr "workspace.options.rotation")
                               :step 15
                               :value (or amount 0)
                               :disabled locked?
                               :on-change-start on-change-start
                               :on-change-end on-change-end
                               :on-change on-angle}]]

          :move
          [:> animation-row* {:label (tr "workspace.animation.offset")}
           [:div {:class (stl/css :animation-pair)}
            [:> numeric-input* {:icon i/character-x
                                :property (tr "workspace.animation.offset-x")
                                :value (or offset-x 0)
                                :disabled locked?
                                :on-change-start on-change-start
                                :on-change-end on-change-end
                                :on-change on-offset-x}]
            [:> numeric-input* {:icon i/character-y
                                :property (tr "workspace.animation.offset-y")
                                :value (or offset-y 0)
                                :disabled locked?
                                :on-change-start on-change-start
                                :on-change-end on-change-end
                                :on-change on-offset-y}]]]

          nil)])]))

(mf/defc layer-animation-item*
  "A preset animation of the layer `shape-id`, changed in its timeline."
  {::mf/private true}
  [{:keys [shape-id animation]}]
  (let [id (:id animation)

        on-change
        (mf/use-fn
         (mf/deps shape-id id)
         (fn [attrs]
           (st/emit! (dwa/update-animation shape-id id attrs))))

        on-remove
        (mf/use-fn
         (mf/deps shape-id id)
         #(st/emit! (dwa/remove-animation shape-id id)))

        on-unlock
        (mf/use-fn
         (mf/deps shape-id id)
         #(st/emit! (dwa/toggle-animation-flag shape-id id :locked)))]

    [:> animation-item* {:animation animation
                         :on-change on-change
                         :on-remove on-remove
                         :on-unlock on-unlock}]))

(mf/defc style-use-item*
  "A use of an animation style by the layer `shape-id`: the name of the
  style and the span of the use, and, once opened, its start and length,
  the library the style comes from, and what can be done with the style:
  detach the use from it, or edit it when it is one of this file
  (`local?`)."
  {::mf/private true}
  [{:keys [shape-id animation library-name local?]}]
  (let [{:keys [id start duration locked style-ref-id]} animation
        locked? (true? locked)

        open*   (mf/use-state false)
        open?   (deref open*)

        on-toggle
        (mf/use-fn #(swap! open* not))

        change
        (mf/use-fn
         (mf/deps shape-id id)
         (fn [attrs]
           (st/emit! (dwa/update-animation shape-id id attrs))))

        on-start    (mf/use-fn (mf/deps change) (number-handler #(change {:start %})))
        on-duration (mf/use-fn (mf/deps change) (number-handler #(change {:duration %})))

        on-remove
        (mf/use-fn
         (mf/deps shape-id id)
         #(st/emit! (dwa/remove-animation shape-id id)))

        on-unlock
        (mf/use-fn
         (mf/deps shape-id id)
         #(st/emit! (dwa/toggle-animation-flag shape-id id :locked)))

        on-detach
        (mf/use-fn
         (mf/deps shape-id id)
         #(st/emit! (dwa/detach-animation-style shape-id id)))

        on-edit
        (mf/use-fn
         (mf/deps style-ref-id)
         #(st/emit! (dw/show-animation-style-in-assets style-ref-id)))]

    [:div {:class (stl/css :animation-item)}
     [:> prototype-pill* {:title (animation-label animation)
                          :description (dm/str start "–" (cta/animation-end animation) " ms")
                          :left-button-icon-id i/library
                          :left-button-tooltip (tr "labels.options")
                          :is-left-button-active open?
                          :on-left-button-click on-toggle
                          :right-button-icon-id (if locked? i/lock i/remove)
                          :right-button-tooltip (if locked?
                                                  (tr "workspace.shape.menu.unlock")
                                                  (tr "workspace.animation.remove-animation"))
                          :on-right-button-click (if locked? on-unlock on-remove)}]

     (when open?
       [:*
        (when (some? library-name)
          [:> animation-row* {:label (tr "workspace.animation.style-library")}
           [:span {:class (stl/css :style-library)} library-name]])

        [:> animation-row* {:label (tr "workspace.animation.start")}
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.start")
                             :min 0
                             :step 1
                             :value start
                             :disabled locked?
                             :on-change on-start}]]

        [:> animation-row* {:label (tr "workspace.animation.duration")}
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.duration")
                             :min 1
                             :step 1
                             :value duration
                             :disabled locked?
                             :on-change on-duration}]]

        [:div {:class (stl/css :style-use-actions)}
         [:> button* {:variant "secondary"
                      :disabled locked?
                      :on-click on-detach}
          (tr "workspace.animation.detach-style")]
         (when local?
           [:> button* {:variant "secondary"
                        :on-click on-edit}
            (tr "workspace.animation.edit-style")])]])]))

(defn- style-label
  "The name of an animation style with its groups."
  [{:keys [path name]}]
  (if (str/blank? path) name (cpn/merge-path-item path name)))

(defn- usable-styles
  "The animation styles the file `file-id` can use, by file, its own
  first: `[{:id :name :local? :styles}]`, files without styles left out,
  the styles by their names."
  [files file-id]
  (->> (vals (refs/select-libraries files file-id))
       (keep (fn [file]
               (when-let [styles (not-empty (vals (dm/get-in file [:data :animation-styles])))]
                 {:id     (:id file)
                  :name   (:name file)
                  :local? (= file-id (:id file))
                  :styles (sort-by (comp str/lower style-label) styles)})))
       (sort-by (juxt (complement :local?) :name))))

(mf/defc save-style*
  "\"Save as style\" for the layer `shape-id` with `animations`: a button,
  then a field for the name of the new style. Disabled, with the reason,
  when the presets and uses it would take cannot make a style."
  {::mf/private true}
  [{:keys [shape-id animations style-count]}]
  (let [result  (mf/with-memo [animations]
                  (ctas/animations->entries animations))
        error   (:error result)
        hint    (case error
                  :too-many-entries (tr "workspace.animation.style-too-many-entries"
                                        cta/max-style-entries (:count result))
                  :no-entries       (tr "workspace.animation.save-as-style-empty")
                  nil)

        naming* (mf/use-state false)
        naming? (deref naming*)

        on-start-naming
        (mf/use-fn #(reset! naming* true))

        on-key-down
        (mf/use-fn
         (mf/deps shape-id)
         (fn [event]
           (cond
             (kbd/enter? event)
             (let [name (str/trim (dom/get-value (dom/get-target event)))]
               (reset! naming* false)
               (when-not (str/blank? name)
                 (st/emit! (dwa/save-animation-style shape-id name))))

             (kbd/esc? event)
             (do (dom/stop-propagation event)
                 (reset! naming* false)))))

        on-blur
        (mf/use-fn #(reset! naming* false))]

    [:div {:class (stl/css :save-style)}
     (if naming?
       [:> input* {:variant "dense"
                   :aria-label (tr "workspace.animation.style-name")
                   :placeholder (tr "workspace.animation.style-name")
                   :default-value (tr "workspace.animation.new-style-name" (inc style-count))
                   :auto-focus true
                   :on-focus dom/select-target
                   :on-key-down on-key-down
                   :on-blur on-blur}]
       [:> button* {:variant "secondary"
                    :class (stl/css :save-style-button)
                    :disabled (some? error)
                    :title hint
                    :on-click on-start-naming}
        (tr "workspace.animation.save-as-style")])
     (when (= error :too-many-entries)
       [:div {:class (stl/css :save-style-hint)} hint])]))

(def ^:private ref:stagger
  (l/derived dwa/stagger st/state))

(mf/defc animations-menu*
  "The preset animations of the selection, in motion mode: add one from
  the menu, then open it to change its settings. The list shows for a
  single layer; a preset goes to every selected layer, one after the
  other by the stagger."
  [{:keys [ids]}]
  (let [timeline   (mf/deref ref:timeline)
        stagger    (mf/deref ref:stagger)
        files      (mf/deref refs/files)
        file-id    (mf/use-ctx ctx/current-file-id)
        shape-id   (when (= 1 (count ids)) (first ids))
        animations (when shape-id
                     (dm/get-in timeline [:tracks shape-id :animations]))
        styles     (mf/with-memo [files file-id]
                     (usable-styles files file-id))

        open*      (mf/use-state false)
        open?      (deref open*)

        on-toggle-menu
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (swap! open* not)))

        on-close-menu
        (mf/use-fn #(reset! open* false))

        on-stagger
        (mf/use-fn (number-handler #(st/emit! (dwa/set-stagger %))))

        on-add
        (mf/use-fn
         (mf/deps ids)
         (fn [event]
           (let [preset (-> (dom/get-current-target event)
                            (dom/get-data "preset")
                            (keyword))]
             (reset! open* false)
             (st/emit! (dwa/add-animation-preset ids preset)))))

        on-apply-style
        (mf/use-fn
         (mf/deps ids)
         (fn [event]
           (let [node (dom/get-current-target event)]
             (reset! open* false)
             (st/emit! (dwa/apply-animation-style ids
                                                  (uuid/parse (dom/get-data node "library"))
                                                  (uuid/parse (dom/get-data node "style")))))))]

    [:div {:class (stl/css :animations-menu)}
     [:div {:class (stl/css :animations-title)}
      [:> title-bar* {:collapsable false
                      :title (tr "workspace.animation.animations")}
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.animation.add-animation")
                         :aria-pressed open?
                         :icon i/add
                         :on-click on-toggle-menu}]]

      [:> dropdown-menu* {:show open?
                          :on-close on-close-menu
                          :class (stl/css :presets-menu)}
       (for [preset preset-types]
         [:> dropdown-menu-item* {:key (name preset)
                                  :class (stl/css :presets-menu-item)
                                  :data-preset (name preset)
                                  :on-click on-add}
          (preset-label preset)])
       [:li {:class (stl/css :presets-menu-title)
             :role "presentation"}
        (tr "workspace.animation.presets")]
       (for [preset preset-styles]
         [:> dropdown-menu-item* {:key (name preset)
                                  :class (stl/css :presets-menu-item)
                                  :data-preset (name preset)
                                  :on-click on-add}
          (preset-label preset)])
       (for [{:keys [id name local? styles]} styles]
         [:* {:key (dm/str id)}
          [:li {:class (stl/css :presets-menu-title)
                :role "presentation"}
           (if local?
             (tr "workspace.animation.animation-styles")
             (tr "workspace.animation.library-styles" name))]
          (for [style styles]
            [:> dropdown-menu-item* {:key (dm/str (:id style))
                                     :class (stl/css :presets-menu-item)
                                     :data-library (dm/str id)
                                     :data-style (dm/str (:id style))
                                     :on-click on-apply-style}
             (style-label style)])])]]

     (when (> (count ids) 1)
       [:> animation-row* {:label (tr "workspace.animation.stagger")}
        [:> numeric-input* {:text-icon "ms"
                            :property (tr "workspace.animation.stagger")
                            :min 0
                            :step 10
                            :value stagger
                            :on-change on-stagger}]])

     (when (seq animations)
       [:div {:class (stl/css :animations-list)}
        (for [animation animations]
          (if (cta/style-use? animation)
            (let [library-id (:style-ref-file animation)]
              [:> style-use-item* {:key (dm/str (:id animation))
                                   :shape-id shape-id
                                   :animation animation
                                   :local? (= library-id file-id)
                                   :library-name (when (not= library-id file-id)
                                                   (dm/get-in files [library-id :name]))}])
            [:> layer-animation-item* {:key (dm/str (:id animation))
                                       :shape-id shape-id
                                       :animation animation}]))])

     (when (some? shape-id)
       [:> save-style* {:shape-id shape-id
                        :animations animations
                        :style-count (count (dm/get-in files [file-id :data :animation-styles]))}])]))
