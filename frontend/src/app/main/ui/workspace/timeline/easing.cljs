;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline.easing
  "Easing editor of a timeline segment, like Figma's: a curve (a named one,
  a hold or a cubic bezier with handles to drag) or a spring."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu* dropdown-menu-item*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.layout.tab-switcher :refer [tab-switcher*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

;; PRESETS

(def ^:private curve-menu
  (-> [{:id :hold}]
      (into cta/bezier-presets)
      (conj {:id :custom-bezier})))

(def ^:private spring-menu
  (conj cta/spring-presets {:id :custom-spring}))

(defn- preset-label
  [id]
  (case id
    :hold             (tr "workspace.animation.easing-hold")
    :linear           (tr "workspace.options.interaction-easing-linear")
    :ease             (tr "workspace.options.interaction-easing-ease")
    :ease-in          (tr "workspace.options.interaction-easing-ease-in")
    :ease-out         (tr "workspace.options.interaction-easing-ease-out")
    :ease-in-out      (tr "workspace.options.interaction-easing-ease-in-out")
    :ease-in-back     (tr "workspace.animation.easing-ease-in-back")
    :ease-out-back    (tr "workspace.animation.easing-ease-out-back")
    :ease-in-out-back (tr "workspace.animation.easing-ease-in-out-back")
    :custom-bezier    (tr "workspace.animation.easing-custom-bezier")
    :gentle           (tr "workspace.animation.spring-gentle")
    :quick            (tr "workspace.animation.spring-quick")
    :bouncy           (tr "workspace.animation.spring-bouncy")
    :slow             (tr "workspace.animation.spring-slow")
    :custom-spring    (tr "workspace.animation.spring-custom")
    (name id)))

(defn- spring-params
  [spring]
  (select-keys spring [:stiffness :damping :mass]))

(defn- keyframe-preset
  "The menu entry of the easing of the segment that starts at `keyframe`."
  [keyframe]
  (let [easing (:easing keyframe)]
    (cond
      (= :step (:interpolation keyframe))
      :hold

      (= :spring (:type easing))
      (or (some #(when (= (spring-params easing) (spring-params (:easing %))) (:id %))
                cta/spring-presets)
          :custom-spring)

      :else
      (or (some #(when (= (cta/easing-curve easing) (cta/easing-curve (:easing %))) (:id %))
                cta/bezier-presets)
          :custom-bezier))))

(defn- reverse-curve
  "The curve played backwards: an ease in becomes an ease out."
  [[x1 y1 x2 y2]]
  (mapv #(mth/precision % 2) [(- 1 x2) (- 1 y2) (- 1 x1) (- 1 y1)]))

(defn- format-curve
  [curve]
  (str/join ", " (map #(mth/precision % 2) curve)))

(defn- parse-curve
  "The `[x1 y1 x2 y2]` of a text like `0.3, -0.05, 0.13, 0.55`, or nil.
  The times (x) stay within 0..1."
  [text]
  (let [values (->> (str/split (str/trim text) #"[,\s]+")
                    (map #(d/parse-double % nil)))]
    (when (and (= 4 (count values)) (every? some? values))
      (let [[x1 y1 x2 y2] values]
        [(mth/clamp x1 0 1) y1 (mth/clamp x2 0 1) y2]))))

;; DRAWING

(defn- curve-points
  "`[t progress]` points along `easing`, at least `least` of them (more
  for a spring that swings many times). A hold jumps at the end."
  [easing hold? least]
  (if hold?
    [[0 0] [1 0] [1 1]]
    (let [samples (if (= :spring (:type easing))
                    (cta/spring-samples easing least)
                    least)]
      (map (fn [i]
             (let [t (/ i samples)]
               [t (cta/easing-progress easing t)]))
           (range (inc samples))))))

(defn- sampled-path
  "SVG path of the progress of `easing` over time (see `curve-points`),
  `(to-x t)` and `(to-y progress)` giving the coordinates."
  [easing hold? to-x to-y least]
  (->> (curve-points easing hold? least)
       (map-indexed (fn [i [t v]]
                      (dm/str (if (zero? i) "M" " L") (to-x t) "," (to-y v))))
       (apply str)))

(defn- icon-x [t] (+ 2 (* t 12)))
(defn- icon-y [v] (+ 2 (* (- 1.2 (mth/clamp v -0.2 1.2)) (/ 12 1.4))))

(def ^:private icon-samples 24)

(defn icon-points
  "The `[x y]` points `curve-icon*` draws `easing` through in its 16px
  box, for the lanes of the timeline, which draw it on a canvas."
  [easing hold?]
  (map (fn [[t v]] [(icon-x t) (icon-y v)])
       (curve-points easing hold? icon-samples)))

(mf/defc curve-icon*
  "A small drawing of an easing. `is-hold` jumps at the end."
  [{:keys [easing is-hold class]}]
  (let [d (mf/with-memo [easing is-hold]
            (sampled-path easing is-hold icon-x icon-y icon-samples))]
    [:svg {:class [class (stl/css :curve-icon)]
           :width 16
           :height 16
           :view-box "0 0 16 16"
           :aria-hidden true}
     [:path {:d d}]]))

;; Curves show progress from -0.6 to 1.6, so the back curves fit; springs,
;; like Figma, from -0.15 to 2.15 with the target in the middle, so the
;; most bouncy one fits.
(def ^:private curve-range [-0.6 1.6])
(def ^:private spring-range [-0.15 2.15])

(def ^:private graph-width 216)
(def ^:private graph-height 256)
(def ^:private graph-pad 12)
(def ^:private graph-inner-width (- graph-width (* 2 graph-pad)))
(def ^:private graph-inner-height (- graph-height (* 2 graph-pad)))

(defn- graph-x [t] (+ graph-pad (* t graph-inner-width)))

(defn- graph-y
  [[lo hi] v]
  (+ graph-pad (* (/ (- hi v) (- hi lo)) graph-inner-height)))

(defn- graph-point
  "The curve point under the client position `pos` over the graph `node`
  showing the progress `range`."
  [node [lo hi] pos]
  (let [rect (dom/get-bounding-rect node)
        px   (/ (- (* (- (:x pos) (:left rect)) (/ graph-width (max 1 (:width rect)))) graph-pad)
                graph-inner-width)
        py   (/ (- (* (- (:y pos) (:top rect)) (/ graph-height (max 1 (:height rect)))) graph-pad)
                graph-inner-height)]
    [(mth/precision (mth/clamp px 0 1) 2)
     (mth/precision (mth/clamp (- hi (* py (- hi lo))) lo hi) 2)]))

(defn- spring-handle
  "Where the handle of a spring sits: on its first overshoot or, when it
  does not overshoot, at the end of its target."
  [spring]
  (or (cta/spring-peak spring) [1 1]))

(mf/defc curve-graph*
  "The graph of an easing. A curve shows handles to drag its control
  points; a spring one on its first overshoot: dragged to the left it
  bounces more (and sooner), to the right less."
  {::mf/private true}
  [{:keys [easing is-hold on-curve-change on-spring-change on-drag-start on-drag-end]}]
  (let [svg-ref (mf/use-ref nil)
        drag-ref (mf/use-ref nil)
        spring? (and (not is-hold) (= :spring (:type easing)))
        curve   (when-not (or is-hold spring?) (cta/easing-curve easing))
        [x1 y1 x2 y2] curve
        [sx sy] (when spring? (spring-handle easing))
        y-range (if spring? spring-range curve-range)
        gy      (partial graph-y y-range)

        on-handle-down
        (mf/use-fn
         (mf/deps on-drag-start)
         (fn [event]
           (dom/stop-propagation event)
           (dom/capture-pointer event)
           (mf/set-ref-val! drag-ref (-> (dom/get-current-target event)
                                         (dom/get-data "handle")
                                         (keyword)))
           (on-drag-start)))

        on-handle-move
        (mf/use-fn
         (mf/deps easing curve y-range on-curve-change on-spring-change)
         (fn [event]
           (when-let [handle (mf/ref-val drag-ref)]
             (let [[x y] (graph-point (mf/ref-val svg-ref) y-range (dom/get-client-position event))
                   [x1 y1 x2 y2] curve]
               (case handle
                 :p1     (on-curve-change [x y x2 y2])
                 :p2     (on-curve-change [x1 y1 x y])
                 :spring (on-spring-change
                          (cta/spring-with-bounce easing (mth/precision (cta/bounce-for-peak x) 2))))))))

        on-handle-up
        (mf/use-fn
         (mf/deps on-drag-end)
         (fn [event]
           (when (some? (mf/ref-val drag-ref))
             (mf/set-ref-val! drag-ref nil)
             (dom/release-pointer event)
             (on-drag-end))))]

    [:svg {:ref svg-ref
           :class (stl/css :graph)
           :view-box (dm/str "0 0 " graph-width " " graph-height)
           :role "img"
           :aria-label (tr "workspace.animation.easing")}
     ;; A spring shows its target only, like Figma.
     (when-not spring?
       [:line {:class (stl/css :graph-guide)
               :x1 0 :x2 graph-width :y1 (gy 0) :y2 (gy 0)}])
     [:line {:class (stl/css :graph-guide)
             :x1 0 :x2 graph-width :y1 (gy 1) :y2 (gy 1)}]
     (if (some? curve)
       [:*
        [:path {:class (stl/css :graph-curve)
                :d (dm/str "M" (graph-x 0) "," (gy 0)
                           " C" (graph-x x1) "," (gy y1)
                           " " (graph-x x2) "," (gy y2)
                           " " (graph-x 1) "," (gy 1))}]
        [:line {:class (stl/css :graph-handle-line)
                :x1 (graph-x 0) :y1 (gy 0) :x2 (graph-x x1) :y2 (gy y1)}]
        [:line {:class (stl/css :graph-handle-line)
                :x1 (graph-x 1) :y1 (gy 1) :x2 (graph-x x2) :y2 (gy y2)}]
        (for [[handle x y] [[:p1 x1 y1] [:p2 x2 y2]]]
          [:circle {:key (name handle)
                    :class (stl/css :graph-handle)
                    :cx (graph-x x)
                    :cy (gy y)
                    :r 6
                    :data-handle (name handle)
                    :on-pointer-down on-handle-down
                    :on-pointer-move on-handle-move
                    :on-pointer-up on-handle-up
                    :on-lost-pointer-capture on-handle-up}])]
       [:*
        [:path {:class (stl/css :graph-curve)
                :d (sampled-path easing is-hold graph-x gy 96)}]
        (when spring?
          [:circle {:class (stl/css :graph-handle :spring-handle)
                    :cx (graph-x sx)
                    :cy (gy (min sy (second y-range)))
                    :r 6
                    :data-handle "spring"
                    :on-pointer-down on-handle-down
                    :on-pointer-move on-handle-move
                    :on-pointer-up on-handle-up
                    :on-lost-pointer-capture on-handle-up}])])]))

;; CONTROLS

(mf/defc preset-menu*
  "The named easings of a mode, each with its drawing."
  {::mf/private true}
  [{:keys [selected items easing is-hold on-select]}]
  (let [open* (mf/use-state false)
        open? (deref open*)

        on-toggle
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (swap! open* not)))

        on-close
        (mf/use-fn #(reset! open* false))

        on-item-click
        (mf/use-fn
         (mf/deps on-select)
         (fn [event]
           (reset! open* false)
           (on-select (-> (dom/get-current-target event)
                          (dom/get-data "preset")
                          (keyword)))))]

    [:div {:class (stl/css :preset-menu)}
     [:button {:type "button"
               :class (stl/css :preset-button)
               :aria-haspopup "menu"
               :aria-expanded open?
               :on-click on-toggle}
      [:> curve-icon* {:easing easing :is-hold is-hold}]
      [:span {:class (stl/css :preset-name)} (preset-label selected)]
      [:> i/icon* {:icon-id i/arrow-down :size "s" :class (stl/css :preset-arrow)}]]

     [:> dropdown-menu* {:show open?
                         :on-close on-close
                         :class (stl/css :preset-list)}
      (for [{:keys [id] :as item} items]
        [:> dropdown-menu-item* {:key (name id)
                                 :class (stl/css-case :preset-item true
                                                      :selected (= id selected))
                                 :data-preset (name id)
                                 :on-click on-item-click}
         (if (contains? item :easing)
           [:> curve-icon* {:easing (:easing item)}]
           [:span {:class (stl/css :curve-icon)}])
         [:span (preset-label id)]])]]))

(mf/defc curve-values*
  "The bezier control points as text, to type exact values."
  {::mf/private true}
  [{:keys [curve on-change]}]
  (let [text (format-curve curve)

        submit
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (when-let [curve (parse-curve (dom/get-value (dom/get-target event)))]
             (on-change curve))))

        on-key-down
        (mf/use-fn
         (mf/deps submit)
         (fn [event]
           (when (kbd/enter? event)
             (dom/prevent-default event)
             (submit event)
             (dom/blur! (dom/get-target event)))))]

    ;; Keyed by the value, so it shows the curve again after an edit.
    [:input {:key text
             :type "text"
             :class (stl/css :values-input)
             :default-value text
             :aria-label (tr "workspace.animation.easing-values")
             :on-key-down on-key-down
             :on-blur submit}]))

(mf/defc spring-bounce*
  "The bounce of a spring, like Figma's custom spring."
  {::mf/private true}
  [{:keys [spring on-change]}]
  (let [on-bounce
        (mf/use-fn
         (mf/deps spring on-change)
         (fn [value]
           (when (number? value)
             (on-change (cta/spring-with-bounce spring value)))))]
    [:div {:class (stl/css :spring-field)}
     [:span {:class (stl/css :spring-label)} (tr "workspace.animation.spring-bounce")]
     [:> numeric-input* {:property (tr "workspace.animation.spring-bounce")
                         :min -1
                         :max cta/max-spring-bounce
                         :step 0.05
                         :value (mth/precision (cta/spring-bounce spring) 2)
                         :on-change on-bounce}]]))

(mf/defc easing-editor*
  "Edit the easing of the segment that starts at `keyframe` of `shape-id`."
  [{:keys [shape-id keyframe]}]
  (let [keyframe-id (:id keyframe)
        easing      (:easing keyframe)
        hold?       (= :step (:interpolation keyframe))
        spring?     (and (not hold?) (= :spring (:type easing)))
        selected    (keyframe-preset keyframe)
        undo-ref    (mf/use-ref nil)

        set-easing
        (mf/use-fn
         (mf/deps shape-id keyframe-id)
         (fn [easing]
           (st/emit! (dwa/set-keyframe-easing shape-id keyframe-id easing))))

        on-mode
        (mf/use-fn
         (mf/deps set-easing spring?)
         (fn [mode]
           (cond
             (and (= mode "easing-spring") (not spring?))
             (set-easing (:easing (first cta/spring-presets)))

             (and (= mode "easing-curve") spring?)
             (set-easing :ease-out))))

        on-preset
        (mf/use-fn
         (mf/deps set-easing easing)
         (fn [id]
           (case id
             :hold          (set-easing :hold)
             :custom-bezier (set-easing {:type :bezier
                                         :curve (or (cta/easing-curve easing) [0.0 0.0 1.0 1.0])})
             :custom-spring nil
             (when-let [preset (d/seek #(= id (:id %))
                                       (concat cta/bezier-presets cta/spring-presets))]
               (set-easing (:easing preset))))))

        on-curve-change
        (mf/use-fn
         (mf/deps set-easing)
         (fn [curve]
           (set-easing {:type :bezier :curve curve})))

        on-reverse
        (mf/use-fn
         (mf/deps easing on-curve-change)
         (fn []
           (when-let [curve (cta/easing-curve easing)]
             (on-curve-change (reverse-curve curve)))))

        on-spring-change
        (mf/use-fn
         (mf/deps set-easing)
         (fn [spring]
           (set-easing spring)))

        ;; A drag of a handle is one undo step.
        on-drag-start
        (mf/use-fn
         (fn []
           (let [undo-id (js/Symbol)]
             (mf/set-ref-val! undo-ref undo-id)
             (st/emit! (dwu/start-undo-transaction undo-id)))))

        on-drag-end
        (mf/use-fn
         (fn []
           (when-let [undo-id (mf/ref-val undo-ref)]
             (mf/set-ref-val! undo-ref nil)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))]

    (mf/with-effect []
      on-drag-end)

    [:> tab-switcher* {:class (stl/css :mode-tabs)
                       :tabs [{:id "easing-curve"
                               :label (tr "workspace.animation.easing-curve")}
                              {:id "easing-spring"
                               :label (tr "workspace.animation.easing-spring")}]
                       :selected (if spring? "easing-spring" "easing-curve")
                       :on-change on-mode}
     [:div {:class (stl/css :easing-editor)}
      [:> preset-menu* {:selected selected
                        :items (if spring? spring-menu curve-menu)
                        :easing easing
                        :is-hold hold?
                        :on-select on-preset}]

      [:> curve-graph* {:easing easing
                        :is-hold hold?
                        :on-curve-change on-curve-change
                        :on-spring-change on-spring-change
                        :on-drag-start on-drag-start
                        :on-drag-end on-drag-end}]

      (cond
        spring?
        [:> spring-bounce* {:spring easing :on-change on-spring-change}]

        (not hold?)
        [:div {:class (stl/css :values-row)}
         [:> curve-values* {:curve (cta/easing-curve easing)
                            :on-change on-curve-change}]
         [:> icon-button* {:variant "ghost"
                           :icon i/flip-horizontal
                           :aria-label (tr "workspace.animation.easing-reverse")
                           :on-click on-reverse}]])]]))
