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
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.path-names :as cpn]
   [app.common.types.animation :as cta]
   [app.common.types.animation-follow :as ctaf]
   [app.common.types.animation-style :as ctas]
   [app.common.types.text-units :as tu]
   [app.common.uuid :as uuid]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.animation-styles :as dwas]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu* dropdown-menu-item*]]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.controls.radio-buttons :refer [radio-buttons*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.workspace.sidebar.options.menus.interactions :refer [prototype-pill*]]
   [app.main.ui.workspace.timeline.easing :as easing :refer [easing-menu* easing-curve-editor*]]
   [app.main.ui.workspace.tokens.motion :as motion-tokens]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.motion-labels :as labels]
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
  [:fade :move :scale :rotate :blur])

(def ^:private preset-styles
  [:slide-up :slide-down :slide-left :slide-right :pop :zoom :spin])

(def ^:private text-preset-styles
  "The built-in presets that play the units of a text (see
  `cta/text-styles`), offered when the selection holds a text."
  [:typewriter :rise-words :cascade-lines :pop-letters])

(defn- preset-label
  [preset]
  (case preset
    :fade          (tr "workspace.animation.preset.fade")
    :move          (tr "workspace.animation.preset.move")
    :scale         (tr "workspace.animation.preset.scale")
    :rotate        (tr "workspace.animation.preset.rotate")
    :blur          (tr "workspace.animation.preset.blur")
    :slide-up      (tr "workspace.animation.preset.slide-up")
    :slide-down    (tr "workspace.animation.preset.slide-down")
    :slide-left    (tr "workspace.animation.preset.slide-left")
    :slide-right   (tr "workspace.animation.preset.slide-right")
    :pop           (tr "workspace.animation.preset.pop")
    :zoom          (tr "workspace.animation.preset.zoom")
    :spin          (tr "workspace.animation.preset.spin")
    :typewriter    (tr "workspace.animation.preset.typewriter")
    :rise-words    (tr "workspace.animation.preset.rise-words")
    :cascade-lines (tr "workspace.animation.preset.cascade-lines")
    :pop-letters   (tr "workspace.animation.preset.pop-letters")
    (name preset)))

(def ^:private split-label
  "The name of the units a split animation of a kind plays (see
  `labels/split-label`)."
  labels/split-label)

(defn- order-label
  [order]
  (case order
    :backward (tr "workspace.animation.order.backward")
    :center   (tr "workspace.animation.order.center")
    :edges    (tr "workspace.animation.order.edges")
    :random   (tr "workspace.animation.order.random")
    (tr "workspace.animation.order.forward")))

(def animation-label
  "Name of a preset animation or of the style a style use plays (see
  `labels/animation-label`, which the Inspect shares)."
  labels/animation-label)

(def ^:private noop
  (constantly nil))

(defn split-controls
  "The controls `animation-item*` shows to play `animation` by the units
  of a text, nil without `split-allowed`: `:kind` (`:layer` for the whole
  layer) and, when it splits, `:spread`, `:order`, `:shuffle` for a
  random order and, given the `unit-count` of the text, the `:stagger`
  (ms between two units) and whether it is past the live cap
  (`:capped`)."
  [{:keys [split spread order duration]} split-allowed unit-count]
  (when (true? split-allowed)
    (let [counted? (and (some? split) (some? unit-count))]
      (cond-> {:kind (or split :layer)}
        (some? split)        (assoc :spread (or spread tu/default-spread) :order (or order :forward))
        (= :random order)    (assoc :shuffle true)
        counted?             (assoc :stagger (mth/round (tu/unit-stagger duration spread unit-count)))
        (and counted? (> unit-count tu/live-unit-cap)) (assoc :capped true)))))

(defn- number-handler
  "Call `f` with the number an input gives, skipping empty values."
  [f]
  (fn [value]
    (when (number? value)
      (f value))))

(defn- tokens-of-types
  "The tokens of `tokens` (by type, or a delay of them) of the `types`, in
  that order, for the picker of a setting: an empty map when there are
  none, nil without `tokens` (no picker)."
  [tokens types]
  (when (some? tokens)
    (let [tokens (if (delay? tokens) @tokens tokens)]
      (reduce (fn [acc type]
                (if-let [items (seq (get tokens type))]
                  (assoc acc type (map #(update % :resolved-value motion-tokens/plain-value) items))
                  acc))
              (array-map)
              types))))

(defn- without-token
  "`applied-tokens` without the token of the setting `key`, nil when no
  other is left."
  [applied-tokens key]
  (not-empty (dissoc applied-tokens key)))

(defn- setting-handler
  "A handler of the number field of the setting `key` of a style entry
  or a preset: a number typed in goes into the setting through
  `to-value` and lets go of its token; a token picked (the input gives
  the tokens of its name) binds it and sets the setting to its value
  (see `cta/token-setting`)."
  [key to-value change applied-tokens]
  (fn [value]
    (cond
      (number? value)
      (change (cond-> {key (to-value value)}
                (contains? applied-tokens key)
                (assoc :applied-tokens (without-token applied-tokens key))))

      (sequential? value)
      (when-let [token (first value)]
        (let [setting (cta/token-setting key (:resolved-value token))]
          (change (cond-> {:applied-tokens (assoc applied-tokens key (:name token))}
                    (some? setting) (assoc key setting))))))))

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
  disabled. With `split-allowed`, it can play the characters, words or
  lines of a text one by one: the kind chosen goes to `on-split` (the
  other split animations of the text or style follow it), and, given
  the `unit-count` of the text, the ms between two units shows next to
  the spread. It starts closed, or open with `default-open`. The layers
  list and the style editor both use it. With `tokens` (the active
  design tokens by type), the settings of a style entry or a preset bind
  tokens: each number field and the easing offer those of its types (see
  `cta/animation-token-types`), and its `:applied-tokens` show either way.
  With `before-start`, a preset of a layer, its start may be before 0, as
  on the timeline; an entry of a style starts with the style or later."
  [{:keys [animation on-change on-remove on-unlock on-change-start on-change-end read-only
           split-allowed on-split unit-count default-open tokens before-start]}]
  (let [{:keys [id type direction start duration easing amount offset-x offset-y locked
                applied-tokens]} animation
        read-only? (true? read-only)
        locked?    (or (true? locked) read-only?)

        ;; the number fields take functions or nothing: the layers list
        ;; has no drag of its own to follow
        on-change-start (d/nilv on-change-start noop)
        on-change-end   (d/nilv on-change-end noop)

        open*  (mf/use-state (true? default-open))
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
         (mf/deps change applied-tokens)
         ;; A new type starts from its own settings, and their tokens go.
         (fn [value]
           (change (cond-> {:type (keyword value) :amount nil :offset-x nil :offset-y nil}
                     (some? applied-tokens)
                     (assoc :applied-tokens (not-empty (dissoc applied-tokens :amount :offset-x :offset-y)))))))

        on-direction
        (mf/use-fn
         (mf/deps change)
         (fn [value]
           (change {:direction (keyword value)})))

        ;; the tokens each setting can bind, nil without `tokens`
        time-tokens   (mf/with-memo [tokens] (tokens-of-types tokens [:duration]))
        amount-tokens (mf/with-memo [tokens type] (tokens-of-types tokens (cta/animation-token-types type :amount)))
        offset-tokens (mf/with-memo [tokens type] (tokens-of-types tokens (cta/animation-token-types type :offset-x)))
        easing-tokens (mf/with-memo [tokens type]
                        (->> (tokens-of-types tokens (cta/animation-token-types type :easing))
                             (mapcat val)
                             (sort-by :name)))
        easing-token  (:easing applied-tokens)

        ;; a custom bezier or spring opens its editor under the menu
        custom-open*  (mf/use-state false)

        on-easing
        (mf/use-fn
         (mf/deps change applied-tokens)
         (fn [easing]
           (reset! custom-open* false)
           (change (cond-> {:easing easing}
                     (contains? applied-tokens :easing)
                     (assoc :applied-tokens (without-token applied-tokens :easing))))))

        on-easing-token
        (mf/use-fn
         (mf/deps change applied-tokens easing-tokens)
         (fn [token-name]
           (reset! custom-open* false)
           (let [token   (d/seek #(= token-name (:name %)) easing-tokens)
                 setting (cta/token-setting :easing (:resolved-value token))]
             (change (cond-> {:applied-tokens (assoc applied-tokens :easing token-name)}
                       (some? setting) (assoc :easing setting))))))

        on-easing-custom
        (mf/use-fn #(reset! custom-open* true))

        on-start    (mf/use-fn (mf/deps change applied-tokens) (setting-handler :start identity change applied-tokens))
        on-duration (mf/use-fn (mf/deps change applied-tokens) (setting-handler :duration identity change applied-tokens))
        on-percent  (mf/use-fn (mf/deps change applied-tokens) (setting-handler :amount #(/ % 100) change applied-tokens))
        on-angle    (mf/use-fn (mf/deps change applied-tokens) (setting-handler :amount identity change applied-tokens))
        on-offset-x (mf/use-fn (mf/deps change applied-tokens) (setting-handler :offset-x identity change applied-tokens))
        on-offset-y (mf/use-fn (mf/deps change applied-tokens) (setting-handler :offset-y identity change applied-tokens))
        on-spread   (mf/use-fn (mf/deps change) (number-handler #(change {:spread (/ % 100)})))

        ;; the detach of the token of each setting
        on-detach
        (mf/with-memo [change applied-tokens]
          (into {}
                (map (fn [key]
                       [key (fn [_] (change {:applied-tokens (without-token applied-tokens key)}))]))
                [:start :duration :amount :offset-x :offset-y]))

        on-split-kind
        (mf/use-fn
         (mf/deps change on-split)
         (fn [value]
           (let [kind (when (not= value "layer") (keyword value))]
             (if (some? on-split)
               (on-split kind)
               (change {:split kind})))))

        on-order
        (mf/use-fn
         (mf/deps change)
         (fn [value]
           (change {:order (keyword value)})))

        ;; a new order of the units, at random
        on-shuffle
        (mf/use-fn
         (mf/deps change)
         #(change {:seed (inc (rand-int 2147483646))}))

        split-options
        (mf/with-memo []
          (into [{:value "layer" :label (split-label nil)}]
                (map (fn [kind] {:value (name kind) :label (split-label kind)}))
                [:char :word :line]))

        order-options
        (mf/with-memo []
          (mapv (fn [order] {:value (name order) :label (order-label order)})
                [:forward :backward :center :edges :random]))

        controls
        (split-controls animation split-allowed unit-count)

        type-options
        (mf/with-memo []
          (mapv (fn [type] {:value (name type) :label (preset-label type)}) preset-types))

        ;; the editor of a custom easing, unless a token gives it
        easing-editor?
        (and (nil? easing-token)
             (or (deref custom-open*)
                 (contains? #{:custom-bezier :custom-spring} (easing/easing-entry easing))))]

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

        (when (some? controls)
          [:*
           [:> animation-row* {:label (tr "workspace.animation.split")}
            [:& select {:default-value (name (:kind controls))
                        :options split-options
                        :disabled locked?
                        :on-change on-split-kind}]]

           (when (some? (:spread controls))
             [:*
              [:> animation-row* {:label (tr "workspace.animation.spread")}
               [:div {:class (stl/css :animation-spread)}
                [:> numeric-input* {:text-icon "%"
                                    :property (tr "workspace.animation.spread")
                                    :min 0
                                    :max 100
                                    :step 10
                                    :value (mth/round (* 100 (:spread controls)))
                                    :disabled locked?
                                    :on-change-start on-change-start
                                    :on-change-end on-change-end
                                    :on-change on-spread}]
                (when-let [stagger (:stagger controls)]
                  [:span {:class (stl/css :animation-hint)}
                   (tr "workspace.animation.spread.hint" stagger)])]]

              [:> animation-row* {:label (tr "workspace.animation.order")}
               [:div {:class (stl/css :animation-order)}
                [:& select {:default-value (name (:order controls))
                            :options order-options
                            :disabled locked?
                            :on-change on-order}]
                (when (:shuffle controls)
                  [:> icon-button* {:variant "ghost"
                                    :icon i/reload
                                    :aria-label (tr "workspace.animation.shuffle")
                                    :disabled locked?
                                    :on-click on-shuffle}])]]

              (when (:capped controls)
                [:div {:class (stl/css :animation-note)}
                 (tr "workspace.animation.unit-cap-hint" tu/live-unit-cap)])])])

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
                             :min (when-not (true? before-start) 0)
                             :step 1
                             :value start
                             :tokens time-tokens
                             :applied-token-name (:start applied-tokens)
                             :on-detach (:start on-detach)
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
                             :tokens time-tokens
                             :applied-token-name (:duration applied-tokens)
                             :on-detach (:duration on-detach)
                             :disabled locked?
                             :on-change-start on-change-start
                             :on-change-end on-change-end
                             :on-change on-duration}]]

        [:> animation-row* {:label (tr "workspace.animation.easing")}
         [:> easing-menu* {:easing easing
                           :auto-easing (if (= :out direction) :ease-in :ease-out)
                           :token easing-token
                           :tokens easing-tokens
                           :is-custom easing-editor?
                           :disabled locked?
                           :on-change on-easing
                           :on-token on-easing-token
                           :on-custom on-easing-custom}]]

        (when easing-editor?
          [:> easing-curve-editor* {:easing easing
                                    :disabled locked?
                                    :on-change on-easing
                                    :on-change-start on-change-start
                                    :on-change-end on-change-end}])

        (case type
          :fade
          [:> animation-row* {:label (tr "workspace.options.opacity")}
           [:> numeric-input* {:text-icon "%"
                               :property (tr "workspace.options.opacity")
                               :min 0
                               :max 100
                               :value (mth/round (* 100 (or amount 0)))
                               ;; an opacity token shows as a percent too
                               :name :opacity
                               :tokens amount-tokens
                               :applied-token-name (:amount applied-tokens)
                               :on-detach (:amount on-detach)
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
                               :tokens amount-tokens
                               :applied-token-name (:amount applied-tokens)
                               :on-detach (:amount on-detach)
                               :disabled locked?
                               :on-change-start on-change-start
                               :on-change-end on-change-end
                               :on-change on-percent}]]

          :blur
          [:> animation-row* {:label (tr "workspace.animation.blur")}
           [:> numeric-input* {:text-icon "px"
                               :property (tr "workspace.animation.blur")
                               :min 0
                               :value (or amount 0)
                               :tokens amount-tokens
                               :applied-token-name (:amount applied-tokens)
                               :on-detach (:amount on-detach)
                               :disabled locked?
                               :on-change-start on-change-start
                               :on-change-end on-change-end
                               :on-change on-angle}]]

          :rotate
          [:> animation-row* {:label (tr "workspace.options.rotation")}
           [:> numeric-input* {:icon i/rotation
                               :property (tr "workspace.options.rotation")
                               :step 15
                               :value (or amount 0)
                               :tokens amount-tokens
                               :applied-token-name (:amount applied-tokens)
                               :on-detach (:amount on-detach)
                               :disabled locked?
                               :on-change-start on-change-start
                               :on-change-end on-change-end
                               :on-change on-angle}]]

          :move
          [:> animation-row* {:label (tr "workspace.animation.offset")}
           [:div {:class (stl/css :animation-pair)}
            [:> numeric-input* {:icon i/character-x
                                :class (stl/css :animation-pair-input)
                                :property (tr "workspace.animation.offset-x")
                                :value (or offset-x 0)
                                :tokens offset-tokens
                                :applied-token-name (:offset-x applied-tokens)
                                :on-detach (:offset-x on-detach)
                                :disabled locked?
                                :on-change-start on-change-start
                                :on-change-end on-change-end
                                :on-change on-offset-x}]
            [:> numeric-input* {:icon i/character-y
                                :class (stl/css :animation-pair-input)
                                :align :right
                                :property (tr "workspace.animation.offset-y")
                                :value (or offset-y 0)
                                :tokens offset-tokens
                                :applied-token-name (:offset-y applied-tokens)
                                :on-detach (:offset-y on-detach)
                                :disabled locked?
                                :on-change-start on-change-start
                                :on-change-end on-change-end
                                :on-change on-offset-y}]]]

          nil)])]))

(mf/defc layer-animation-item*
  "A preset animation of the layer `shape-id`, changed in its timeline.
  A `text` can play it by its units (see `animation-item*`), and its
  settings bind the design `tokens` (by type)."
  {::mf/private true}
  [{:keys [shape-id animation text tokens]}]
  (let [id    (:id animation)
        split (:split animation)

        unit-count
        (mf/with-memo [text split]
          (when (and (some? text) (some? split))
            (tu/unit-count text split)))

        on-split
        (mf/use-fn
         (mf/deps shape-id id)
         (fn [kind]
           (st/emit! (dwa/set-animation-split shape-id id kind))))

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
                         :on-unlock on-unlock
                         :split-allowed (some? text)
                         :on-split on-split
                         :unit-count unit-count
                         :tokens tokens
                         :before-start true}]))

(defn- stretch-keys
  "The keyframe entry `entry` from `start` and `duration` ms long, its
  keyframes retimed in proportion, as a stretched use plays them (see
  `cta/expand-style`)."
  [entry start duration]
  (first (cta/expand-style {:start start
                            :duration duration
                            :entries [(assoc entry :start 0)]})))

(mf/defc keys-entry-item*
  "A keyframe entry of an animation style, `entry`: the row it animates,
  its keyframes and span, and, once opened, its start and duration, a new
  duration retiming its keyframes, each change going to `on-change` as
  the attributes changed; a drag on a number field starts with
  `on-change-start` and ends with `on-change-end`. Its remove button,
  `on-remove`, is disabled when nil; a `read-only` one shows them
  disabled. Its values come from a layer that uses the style (see
  `dwas/update-style-from-layer`)."
  [{:keys [entry on-change on-remove on-change-start on-change-end read-only]}]
  (let [{:keys [start duration keyframes]} entry
        read-only?      (true? read-only)
        on-change-start (d/nilv on-change-start noop)
        on-change-end   (d/nilv on-change-end noop)

        open*  (mf/use-state false)
        open?  (deref open*)

        on-toggle
        (mf/use-fn #(swap! open* not))

        on-start
        (mf/use-fn
         (mf/deps on-change)
         (number-handler #(on-change {:start %})))

        on-duration
        (mf/use-fn
         (mf/deps entry on-change)
         (number-handler
          #(on-change (select-keys (stretch-keys entry (:start entry) (max 1 %)) [:duration :keyframes]))))]

    [:div {:class (stl/css :animation-item)}
     [:> prototype-pill* {:title (animation-label entry)
                          :description (tr "workspace.animation.keys-entry.span"
                                           (count keyframes) start (cta/animation-end entry))
                          :left-button-icon-id i/hsva
                          :left-button-tooltip (tr "labels.options")
                          :is-left-button-active open?
                          :on-left-button-click on-toggle
                          :right-button-icon-id i/remove
                          :right-button-tooltip (tr "workspace.animation.remove-animation")
                          :is-right-button-disabled (or read-only? (nil? on-remove))
                          :on-right-button-click (when-not read-only? on-remove)}]

     (when open?
       [:*
        [:> animation-row* {:label (tr "workspace.animation.start")}
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.start")
                             :min 0
                             :step 1
                             :value start
                             :disabled read-only?
                             :on-change-start on-change-start
                             :on-change-end on-change-end
                             :on-change on-start}]]

        [:> animation-row* {:label (tr "workspace.animation.duration")}
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.duration")
                             :min 1
                             :step 1
                             :value duration
                             :disabled read-only?
                             :on-change-start on-change-start
                             :on-change-end on-change-end
                             :on-change on-duration}]]

        [:div {:class (stl/css :animation-note)}
         (tr "workspace.animation.keys-entry.hint")]])]))

(mf/defc style-use-item*
  "A use of an animation style by the layer `shape-id`: the name of the
  style and the span of the use, and, once opened, its start and length,
  the library the style comes from, and what can be done with the style:
  detach the use from it, or, when it is one of this file (`local?`),
  edit it or save into it what the layer plays now."
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
         #(st/emit! (dw/show-animation-style-in-assets style-ref-id)))

        on-update-style
        (mf/use-fn
         (mf/deps shape-id id)
         #(st/emit! (dwas/update-style-from-layer shape-id id)))]

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
         ;; on a layer: before 0 too, as on the timeline
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.start")
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
            (tr "workspace.animation.edit-style")])]
        (when local?
          [:> button* {:variant "secondary"
                       :class (stl/css :style-update)
                       :title (tr "workspace.animation.update-style-from-layer.hint")
                       :on-click on-update-style}
           (tr "workspace.animation.update-style-from-layer")])])]))

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
  "\"Save as style\" for the layer `shape-id` with its `track`: a button,
  then a field for the name of the new style. Disabled, with the reason,
  when the presets, uses and rows of keyframes it would take cannot make
  a style."
  {::mf/private true}
  [{:keys [shape-id track style-count]}]
  (let [result  (mf/with-memo [track]
                  (ctas/entries-error track))
        error   (:error result)
        hint    (case error
                  :too-many-entries   (tr "workspace.animation.style-too-many-entries"
                                          cta/max-style-entries (:count result))
                  :too-many-keyframes (tr "workspace.animation.style-too-many-keyframes"
                                          cta/max-style-keyframes (:count result))
                  :no-entries         (tr "workspace.animation.save-as-style-empty")
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
     (when (contains? #{:too-many-entries :too-many-keyframes} error)
       [:div {:class (stl/css :save-style-hint)} hint])]))

(def ^:private ref:stagger
  (l/derived dwa/stagger st/state))

(def ^:private ref:timelines
  (l/derived #(get % :timelines) refs/workspace-page))

(mf/defc copy-clock-rows*
  "When and how fast the component copy `shape-id` plays the animation of
  its main, as a precomposition in After Effects (see `cta/copy-clock`):
  only for a copy whose main animates."
  {::mf/private true}
  [{:keys [shape-id]}]
  (let [objects   (mf/deref refs/workspace-page-objects)
        timelines (mf/deref ref:timelines)
        files     (mf/deref refs/files)
        board-id  (cfh/get-shape-id-root-frame objects shape-id)
        animated? (mf/with-memo [objects timelines files board-id shape-id]
                    (and (some? board-id)
                         (some (fn [[[_ id]]] (= id shape-id))
                               (dwa/copy-timelines objects timelines files board-id))))
        {:keys [start speed]} (cta/copy-clock (get timelines board-id) shape-id)

        on-start
        (mf/use-fn
         (mf/deps shape-id)
         (number-handler #(st/emit! (dwa/set-copy-clock shape-id {:start (max 0 (mth/round %))}))))

        on-speed
        (mf/use-fn
         (mf/deps shape-id)
         (number-handler #(st/emit! (dwa/set-copy-clock shape-id {:speed (/ % 100)}))))]

    (when animated?
      [:div {:class (stl/css :copy-clock)}
       [:> animation-row* {:label (tr "workspace.animation.copy-start")}
        [:> numeric-input* {:text-icon "ms"
                            :property (tr "workspace.animation.copy-start")
                            :min 0
                            :step 10
                            :value start
                            :on-change on-start}]]
       [:> animation-row* {:label (tr "workspace.animation.copy-speed")}
        [:> numeric-input* {:text-icon "%"
                            :property (tr "workspace.animation.copy-speed")
                            :min (* 100 cta/min-copy-speed)
                            :max (* 100 cta/max-copy-speed)
                            :step 5
                            :value (mth/round (* 100 speed))
                            :on-change on-speed}]]])))

(def ^:private ref:picking
  "The layer a pick is for and what it picks (see `dwa/picking-leader`)."
  (l/derived (fn [state]
               (when-let [id (dwa/picking-leader state)]
                 [id (dwa/picking-purpose state)]))
             st/state =))

(def ^:private follow-parts
  "What a follower can take of its leader, and its label."
  [[:position #(tr "workspace.animation.position")]
   [:rotation #(tr "workspace.options.rotation")]
   [:scale #(tr "workspace.animation.scale")]
   [:opacity #(tr "workspace.options.opacity")]])

(mf/defc follow-menu*
  "Whom the selected layer, a single one in a board, follows (see
  `cta/follow-of`): a layer of its board, chosen in the list or picked on
  the canvas with the picker, and what it takes of it, after a delay. The
  + of the title shows the section; its - follows none."
  [{:keys [ids]}]
  (let [shape-id  (when (= 1 (count ids)) (first ids))
        objects   (mf/deref refs/workspace-page-objects)
        timelines (mf/deref ref:timelines)
        picking   (mf/deref ref:picking)
        board-id  (when (some? shape-id) (cfh/get-shape-id-root-frame objects shape-id))
        layer?    (and (some? board-id) (not= uuid/zero board-id) (not= board-id shape-id))
        timeline  (mf/with-memo [timelines board-id]
                    (or (get timelines board-id) (cta/make-timeline {:board-id board-id})))
        follow    (when layer? (cta/follow-of timeline shape-id))
        leaders   (mf/with-memo [objects timeline shape-id layer?]
                    (when layer? (cltl/follow-leaders objects timeline shape-id)))
        options   (mf/with-memo [leaders objects]
                    (into [{:id "" :label (tr "workspace.animation.follow-choose")}]
                          (map (fn [id] {:id (dm/str id) :label (dm/get-in objects [id :name])}))
                          leaders))
        ;; the layer whose section the + opened
        open*     (mf/use-state nil)
        open?     (or (some? follow) (= shape-id (deref open*)))
        picking?  (and (some? shape-id) (= picking [shape-id :leader]))

        on-add
        (mf/use-fn
         (mf/deps shape-id)
         #(reset! open* shape-id))

        on-remove
        (mf/use-fn
         (mf/deps shape-id)
         (fn []
           (reset! open* nil)
           (st/emit! (dwa/clear-follow shape-id))))

        on-leader
        (mf/use-fn
         (mf/deps shape-id follow)
         (fn [id]
           (when-let [leader (uuid/parse* id)]
             (st/emit! (dwa/set-follow shape-id (assoc follow :leader leader))))))

        on-pick
        (mf/use-fn
         (mf/deps shape-id picking?)
         (fn []
           (st/emit! (if picking?
                       (dwa/cancel-pick-leader)
                       (dwa/pick-leader shape-id)))))

        on-part
        (mf/use-fn
         (mf/deps shape-id)
         (fn [event]
           (let [node (dom/get-current-target event)
                 part (keyword (dom/get-data node "part"))]
             (st/emit! (dwa/update-follow shape-id {part (dom/checked? node)})))))

        on-delay
        (mf/use-fn
         (mf/deps shape-id)
         (number-handler #(st/emit! (dwa/update-follow shape-id {:delay (max 0 (mth/round %))}))))

        on-from
        (mf/use-fn
         (mf/deps shape-id)
         (fn [value]
           (st/emit! (dwa/update-follow shape-id {:from (keyword value)}))))]

    ;; leaving the layer closes its section
    (mf/with-effect [shape-id]
      #(reset! open* nil))

    (when layer?
      [:div {:class (stl/css :follow-menu)}
       [:div {:class (stl/css :animations-title)}
        [:> title-bar* {:collapsable false
                        :title (tr "workspace.animation.follow")}
         (if open?
           [:> icon-button* {:variant "ghost"
                             :aria-label (tr "workspace.animation.remove-follow")
                             :icon i/remove
                             :on-click on-remove}]
           [:> icon-button* {:variant "ghost"
                             :aria-label (tr "workspace.animation.add-follow")
                             :icon i/add
                             :on-click on-add}])]]

       (when open?
         [:div {:class (stl/css :follow-rows)}
          [:> animation-row* {:label (tr "workspace.animation.follow-leader")}
           [:div {:class (stl/css :follow-leader)}
            [:> select* {:class (stl/css :follow-leader-select)
                         :options options
                         :default-selected (if (some? follow) (dm/str (:leader follow)) "")
                         :aria-label (tr "workspace.animation.follow-leader")
                         :on-change on-leader}]
            [:> icon-button* {:variant "ghost"
                              :aria-label (tr "workspace.animation.follow-pick")
                              :aria-pressed picking?
                              :class (stl/css-case :follow-picking picking?)
                              :icon i/picker
                              :on-click on-pick}]]]

          (when (some? follow)
            [:*
             ;; what it takes: what the leader does from its first frame
             ;; on, or all that moves it from where it rests
             [:> animation-row* {:label (tr "workspace.animation.follow-from")}
              [:> radio-buttons* {:name (dm/str "follow-from-" shape-id)
                                  :selected (name (:from follow))
                                  :extended true
                                  :options [{:id (dm/str "follow-from-start-" shape-id)
                                             :value "start"
                                             :label (tr "workspace.animation.follow-from-start")}
                                            {:id (dm/str "follow-from-rest-" shape-id)
                                             :value "rest"
                                             :label (tr "workspace.animation.follow-from-rest")}]
                                  :on-change on-from}]]
             [:div {:class (stl/css :follow-parts)}
              (for [[part label] follow-parts]
                [:> checkbox* {:key (name part)
                               :id (dm/str "follow-" (name part))
                               :label (label)
                               :data-part (name part)
                               :checked (boolean (get follow part))
                               :on-change on-part}])]
             [:> animation-row* {:label (tr "workspace.animation.follow-delay")}
              [:> numeric-input* {:text-icon "ms"
                                  :property (tr "workspace.animation.follow-delay")
                                  :min 0
                                  :step 10
                                  :value (:delay follow)
                                  :on-change on-delay}]]])])])))

(mf/defc reach-menu*
  "The goal the selected layer reaches (see `cta/ik-of`), when it ends a
  chain of two follows (an arm: the hand follows the forearm, which
  follows the upper arm, see `ctaf/ik-links`) or reaches one already: a
  layer of its board, chosen in the list, picked on the canvas or made
  new (`dwa/add-goal`); the side its elbow bends to and how it turns
  itself. A chain that cannot reach keeps its goal, which it reaches
  once mended, and says why (see `ctaf/ik-problem`).
  The + of the title shows the section; its - reaches none."
  [{:keys [ids]}]
  (let [shape-id  (when (= 1 (count ids)) (first ids))
        objects   (mf/deref refs/workspace-page-objects)
        timelines (mf/deref ref:timelines)
        picking   (mf/deref ref:picking)
        board-id  (when (some? shape-id) (cfh/get-shape-id-root-frame objects shape-id))
        layer?    (and (some? board-id) (not= uuid/zero board-id) (not= board-id shape-id))
        timeline  (mf/with-memo [timelines board-id]
                    (or (get timelines board-id) (cta/make-timeline {:board-id board-id})))
        ik        (when layer? (cta/ik-of timeline shape-id))
        chain?    (mf/with-memo [timeline objects shape-id layer?]
                    (and layer? (some? (ctaf/ik-links timeline objects shape-id))))
        ;; why it reaches its goal no more (see `ctaf/ik-problem`)
        problem   (mf/with-memo [timeline objects shape-id ik]
                    (when (some? ik) (ctaf/ik-problem timeline objects shape-id)))
        goals     (mf/with-memo [objects timeline shape-id chain?]
                    (when chain? (cltl/reach-goals objects timeline shape-id)))
        options   (mf/with-memo [goals objects]
                    (into [{:id "" :label (tr "workspace.animation.follow-choose")}]
                          (map (fn [id] {:id (dm/str id) :label (dm/get-in objects [id :name])}))
                          goals))
        ;; the layer whose section the + opened
        open*     (mf/use-state nil)
        open?     (or (some? ik) (= shape-id (deref open*)))
        picking?  (and (some? shape-id) (= picking [shape-id :goal]))

        on-add
        (mf/use-fn
         (mf/deps shape-id)
         #(reset! open* shape-id))

        on-remove
        (mf/use-fn
         (mf/deps shape-id)
         (fn []
           (reset! open* nil)
           (st/emit! (dwa/clear-ik shape-id))))

        on-goal
        (mf/use-fn
         (mf/deps shape-id ik)
         (fn [id]
           (when-let [goal (uuid/parse* id)]
             (st/emit! (dwa/set-ik shape-id (assoc ik :goal goal))))))

        on-pick
        (mf/use-fn
         (mf/deps shape-id picking?)
         (fn []
           (st/emit! (if picking?
                       (dwa/cancel-pick-leader)
                       (dwa/pick-goal shape-id)))))

        on-new-goal
        (mf/use-fn
         (mf/deps shape-id)
         #(st/emit! (dwa/add-goal shape-id)))

        on-bend
        (mf/use-fn
         (mf/deps shape-id)
         (fn [value]
           (st/emit! (dwa/update-ik shape-id {:bend (keyword value)}))))

        on-turn
        (mf/use-fn
         (mf/deps shape-id)
         (fn [value]
           (st/emit! (dwa/update-ik shape-id {:turn (keyword value)}))))]

    ;; leaving the layer closes its section
    (mf/with-effect [shape-id]
      #(reset! open* nil))

    (when (or chain? (some? ik))
      [:div {:class (stl/css :follow-menu)}
       [:div {:class (stl/css :animations-title)}
        [:> title-bar* {:collapsable false
                        :title (tr "workspace.animation.reach")}
         (if open?
           [:> icon-button* {:variant "ghost"
                             :aria-label (tr "workspace.animation.remove-reach")
                             :icon i/remove
                             :on-click on-remove}]
           [:> icon-button* {:variant "ghost"
                             :aria-label (tr "workspace.animation.add-reach")
                             :icon i/add
                             :on-click on-add}])]]

       (when open?
         [:div {:class (stl/css :follow-rows)}
          (when (some? problem)
            [:p {:class (stl/css :reach-broken)}
             (case problem
               :goal   (tr "workspace.animation.reach-broken-goal")
               :shared (tr "workspace.animation.reach-broken-shared")
               :loop   (tr "workspace.animation.reach-broken-loop")
               (tr "workspace.animation.reach-broken"))])
          (when chain?
            [:*
             [:> animation-row* {:label (tr "workspace.animation.reach-goal")}
              [:div {:class (stl/css :follow-leader)}
               [:> select* {:class (stl/css :follow-leader-select)
                            :options options
                            :default-selected (if (some? ik) (dm/str (:goal ik)) "")
                            :aria-label (tr "workspace.animation.reach-goal")
                            :on-change on-goal}]
               [:> icon-button* {:variant "ghost"
                                 :aria-label (tr "workspace.animation.reach-pick")
                                 :aria-pressed picking?
                                 :class (stl/css-case :follow-picking picking?)
                                 :icon i/picker
                                 :on-click on-pick}]]]
             [:> button* {:variant "secondary"
                          :class (stl/css :reach-new-goal)
                          :on-click on-new-goal}
              (tr "workspace.animation.reach-new-goal")]])

          (when (some? ik)
            [:*
             [:> animation-row* {:label (tr "workspace.animation.reach-bend")}
              [:> radio-buttons* {:name (dm/str "reach-bend-" shape-id)
                                  :selected (name (:bend ik))
                                  :extended true
                                  :options [{:id (dm/str "reach-bend-cw-" shape-id)
                                             :value "cw"
                                             :label (tr "workspace.animation.reach-bend-cw")}
                                            {:id (dm/str "reach-bend-ccw-" shape-id)
                                             :value "ccw"
                                             :label (tr "workspace.animation.reach-bend-ccw")}]
                                  :on-change on-bend}]]
             [:> animation-row* {:label (tr "workspace.animation.reach-turn")}
              [:> radio-buttons* {:name (dm/str "reach-turn-" shape-id)
                                  :selected (name (:turn ik))
                                  :extended true
                                  :options [{:id (dm/str "reach-turn-chain-" shape-id)
                                             :value "chain"
                                             :label (tr "workspace.animation.reach-turn-chain")}
                                            {:id (dm/str "reach-turn-keep-" shape-id)
                                             :value "keep"
                                             :label (tr "workspace.animation.reach-turn-keep")}
                                            {:id (dm/str "reach-turn-goal-" shape-id)
                                             :value "goal"
                                             :label (tr "workspace.animation.reach-turn-goal")}]
                                  :on-change on-turn}]]])])])))

(mf/defc animations-menu*
  "The preset animations of the selection, in motion mode: add one from
  the menu, then open it to change its settings. The list shows for a
  single layer; a preset goes to every selected layer, one after the
  other by the stagger. With a text among the `shapes` selected, the
  menu offers the text presets, which go to the texts only, and a single
  text can play its presets by its units."
  [{:keys [ids shapes]}]
  (let [text?      (some #(= :text (:type %)) shapes)
        text       (when (= 1 (count ids))
                     (d/seek #(and (= :text (:type %)) (= (first ids) (:id %))) shapes))
        timeline   (mf/deref ref:timeline)
        stagger    (mf/deref ref:stagger)
        files      (mf/deref refs/files)
        file-id    (mf/use-ctx ctx/current-file-id)
        shape-id   (when (= 1 (count ids)) (first ids))
        animations (when shape-id
                     (dm/get-in timeline [:tracks shape-id :animations]))
        styles     (mf/with-memo [files file-id]
                     (usable-styles files file-id))
        ;; what the settings of its presets bind
        tokens     (mf/use-ctx ctx/active-tokens-by-type)

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
       (for [preset (cond-> preset-styles text? (into text-preset-styles))]
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
       [:div {:class (stl/css :stagger-row)}
        [:> animation-row* {:label (tr "workspace.animation.stagger")}
         [:> numeric-input* {:text-icon "ms"
                             :property (tr "workspace.animation.stagger")
                             :min 0
                             :step 10
                             :value stagger
                             :on-change on-stagger}]]])

     (when (some? shape-id)
       [:> copy-clock-rows* {:shape-id shape-id}])

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
                                       :animation animation
                                       :text text
                                       :tokens tokens}]))])

     (when (some? shape-id)
       [:> save-style* {:shape-id shape-id
                        :track (dm/get-in timeline [:tracks shape-id])
                        :style-count (count (dm/get-in files [file-id :data :animation-styles]))}])]))
