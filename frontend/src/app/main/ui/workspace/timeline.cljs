;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline
  "Bottom timeline dock for the keyframe based animation feature (Penpot
  Motion): per-shape tracks, per-property lanes, draggable keyframes and a
  scrubbable playhead."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.main.data.workspace.animation :as dwa]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:timelines
  (l/derived #(get % :timelines) refs/workspace-page))

(def ^:private properties
  [{:id :x        :label "X"}
   {:id :y        :label "Y"}
   {:id :scale-x  :label "Scale X"}
   {:id :scale-y  :label "Scale Y"}
   {:id :rotation :label "Rotation"}
   {:id :opacity  :label "Opacity"}])

(defn- time->pct
  [time duration]
  (if (pos? duration)
    (str (* 100 (/ (double time) duration)) "%")
    "0%"))

(defn- pointer->time
  "Translate a pointer event over `node` into a time value (ms) clamped to
  [0, duration]."
  [event node duration]
  (let [rect     (dom/get-bounding-rect node)
        x        (:x (dom/get-client-position event))
        fraction (/ (- x (:left rect)) (max 1 (:width rect)))
        fraction (-> fraction (max 0.0) (min 1.0))]
    (int (* fraction duration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUB COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mf/defc keyframe*
  {::mf/private true}
  [{:keys [keyframe shape-id duration lane-node-ref selected?]}]
  (let [time        (:time keyframe)
        kf-id       (:id keyframe)
        dragging-ref (mf/use-ref false)

        on-pointer-down
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (dom/capture-pointer event)
           (mf/set-ref-val! dragging-ref true)))

        on-pointer-move
        (mf/use-fn
         (mf/deps shape-id kf-id duration)
         (fn [event]
           (when (mf/ref-val dragging-ref)
             (when-let [node (mf/ref-val lane-node-ref)]
               (let [t (pointer->time event node duration)]
                 (st/emit! (dwa/move-keyframe shape-id kf-id t)))))))

        on-pointer-up
        (mf/use-fn
         (fn [event]
           (dom/release-pointer event)
           (mf/set-ref-val! dragging-ref false)))

        on-double-click
        (mf/use-fn
         (mf/deps shape-id kf-id)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dwa/delete-keyframe shape-id kf-id))))

        on-click
        (mf/use-fn
         (mf/deps shape-id kf-id)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dwa/select-keyframe shape-id kf-id))))]

    [:div {:class (stl/css-case :keyframe true :selected selected?)
           :style #js {"left" (time->pct time duration)}
           :title (dm/str time " ms")
           :on-pointer-down on-pointer-down
           :on-pointer-move on-pointer-move
           :on-pointer-up on-pointer-up
           :on-click on-click
           :on-double-click on-double-click}]))

(mf/defc property-lane*
  {::mf/private true}
  [{:keys [shape-id keyframes duration selected-kf-id]}]
  (let [node-ref (mf/use-ref nil)]
    [:div {:class (stl/css :property-lane)
           :ref node-ref}
     (for [kf keyframes]
       [:> keyframe* {:key (dm/str (:id kf))
                      :keyframe kf
                      :shape-id shape-id
                      :duration duration
                      :selected? (= (:id kf) selected-kf-id)
                      :lane-node-ref node-ref}])]))

(mf/defc track*
  {::mf/private true}
  [{:keys [track shape-name duration selected-kf]}]
  (let [shape-id  (:shape-id track)
        by-prop   (group-by :property (:keyframes track))
        selected-kf-id (when (= (:shape-id selected-kf) shape-id)
                         (:keyframe-id selected-kf))]
    [:div {:class (stl/css :track)}
     [:div {:class (stl/css :track-header)}
      [:span {:class (stl/css :track-name) :title shape-name} shape-name]]
     [:div {:class (stl/css :track-lanes)}
      (for [{:keys [id label]} properties
            :let [kfs (get by-prop id)]
            :when (seq kfs)]
        [:div {:class (stl/css :lane-row) :key (dm/str shape-id "-" (name id))}
         [:span {:class (stl/css :lane-label)} label]
         [:> property-lane* {:shape-id shape-id
                             :keyframes kfs
                             :duration duration
                             :selected-kf-id selected-kf-id}]])]]))

(mf/defc toolbar*
  {::mf/private true}
  [{:keys [timeline board-name playing? auto-key? selected?]}]
  (let [duration (:duration timeline)

        on-toggle-play
        (mf/use-fn #(st/emit! (dwa/toggle-play)))

        on-stop
        (mf/use-fn #(st/emit! (dwa/stop)))

        on-duration-change
        (mf/use-fn
         (fn [event]
           (let [v (-> (dom/get-target event) (dom/get-value) (d/parse-integer 1000))]
             (st/emit! (dwa/set-duration v)))))

        on-toggle-loop
        (mf/use-fn
         (mf/deps (:loop timeline))
         (fn [_] (st/emit! (dwa/set-loop (not (:loop timeline))))))

        on-toggle-auto-key
        (mf/use-fn #(st/emit! (dwa/toggle-auto-keyframe)))

        on-add-keyframe
        (mf/use-fn
         (fn [event]
           (let [prop (-> (dom/get-current-target event)
                          (dom/get-attribute "data-property")
                          (keyword))]
             (st/emit! (dwa/add-keyframe prop)))))

        on-export-css
        (mf/use-fn #(st/emit! (dwa/export-css)))

        on-close
        (mf/use-fn #(st/emit! (dwa/close-timeline)))]

    [:div {:class (stl/css :toolbar)}
     (when board-name
       [:span {:class (stl/css :board-name) :title board-name} board-name])
     [:div {:class (stl/css :playback-controls)}
      [:button {:class (stl/css-case :play-btn true :active playing?)
                :title (tr "workspace.animation.play")
                :on-click on-toggle-play}
       (if playing?
         [:span {:class (stl/css :pause-icon)}]
         [:> i/icon* {:icon-id i/play}])]
      [:button {:class (stl/css :ctrl-btn)
                :title (tr "workspace.animation.rewind")
                :on-click on-stop}
       [:> i/icon* {:icon-id i/reload}]]
      [:button {:class (stl/css-case :ctrl-btn true :active (:loop timeline))
                :title (tr "workspace.animation.loop")
                :on-click on-toggle-loop}
       [:> i/icon* {:icon-id i/rotation}]]]

     [:div {:class (stl/css :duration-field)}
      [:label (tr "workspace.animation.duration")]
      [:input {:type "number" :min 1 :step 100
               :value duration
               :on-change on-duration-change}]
      [:span "ms"]]

     [:div {:class (stl/css :add-keyframe-group)}
      [:span {:class (stl/css :group-label)} (tr "workspace.animation.add-keyframe")]
      (for [{:keys [id label]} properties]
        [:button {:key (name id)
                  :class (stl/css :prop-btn)
                  :data-property (name id)
                  :disabled (not selected?)
                  :title (if selected?
                           (dm/str (tr "workspace.animation.add-keyframe") " — " label)
                           (tr "workspace.animation.select-shape"))
                  :on-click on-add-keyframe}
         [:> i/icon* {:icon-id i/add}]
         [:span label]])]

     [:button {:class (stl/css-case :ctrl-btn true :active auto-key?)
               :title (tr "workspace.animation.auto-keyframe")
               :on-click on-toggle-auto-key}
      (tr "workspace.animation.rec")]

     [:button {:class (stl/css :ctrl-btn)
               :title (tr "workspace.animation.export-css")
               :on-click on-export-css}
      (tr "workspace.animation.export")]

     [:button {:class (stl/css :close-btn)
               :title (tr "labels.close")
               :on-click on-close}
      [:> i/icon* {:icon-id i/close}]]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EASING EDITOR (cubic-bezier)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private easing-presets
  [{:id :linear      :label "Linear" :curve [0.0 0.0 1.0 1.0]}
   {:id :ease        :label "Ease"   :curve [0.25 0.1 0.25 1.0]}
   {:id :ease-in     :label "In"     :curve [0.42 0.0 1.0 1.0]}
   {:id :ease-out    :label "Out"    :curve [0.0 0.0 0.58 1.0]}
   {:id :ease-in-out :label "In-Out" :curve [0.42 0.0 0.58 1.0]}])

(defn- easing->curve
  [easing]
  (cond
    (map? easing)     (:curve easing)
    (keyword? easing) (or (some #(when (= (:id %) easing) (:curve %)) easing-presets)
                          [0.0 0.0 1.0 1.0])
    :else             [0.0 0.0 1.0 1.0]))

(def ^:private ee-size 132)

(mf/defc easing-editor*
  {::mf/private true}
  [{:keys [shape-id keyframe-id easing]}]
  (let [[x1 y1 x2 y2] (easing->curve easing)
        svg-ref (mf/use-ref nil)
        drag*   (mf/use-state nil)

        emit-curve
        (mf/use-fn
         (mf/deps shape-id keyframe-id)
         (fn [c]
           (st/emit! (dwa/set-keyframe-easing shape-id keyframe-id {:type :bezier :curve c}))))

        on-handle-down
        (mf/use-fn
         (fn [handle event]
           (dom/stop-propagation event)
           (dom/capture-pointer event)
           (reset! drag* handle)))

        on-move
        (mf/use-fn
         (mf/deps x1 y1 x2 y2 emit-curve)
         (fn [event]
           (when-let [handle @drag*]
             (let [node (mf/ref-val svg-ref)
                   rect (dom/get-bounding-rect node)
                   pos  (dom/get-client-position event)
                   px   (-> (/ (- (:x pos) (:left rect)) (max 1 (:width rect))) (max 0.0) (min 1.0))
                   vy   (-> (- 1.0 (/ (- (:y pos) (:top rect)) (max 1 (:height rect)))) (max 0.0) (min 1.0))]
               (emit-curve (if (= handle :p1) [px vy x2 y2] [x1 y1 px vy]))))))

        on-up
        (mf/use-fn
         (fn [event]
           (dom/release-pointer event)
           (reset! drag* nil)))

        ux (fn [v] (* v ee-size))
        uy (fn [v] (* (- 1.0 v) ee-size))]

    [:div {:class (stl/css :easing-editor)}
     [:div {:class (stl/css :easing-presets)}
      (for [{:keys [id label]} easing-presets]
        [:button {:key (name id)
                  :class (stl/css :preset-btn)
                  :on-click #(st/emit! (dwa/set-keyframe-easing shape-id keyframe-id id))}
         label])]
     [:svg {:class (stl/css :easing-svg)
            :ref svg-ref
            :width ee-size :height ee-size}
      [:line {:x1 0 :y1 (uy 0.0) :x2 (ux x1) :y2 (uy y1) :class (stl/css :easing-handle-line)}]
      [:line {:x1 ee-size :y1 (uy 1.0) :x2 (ux x2) :y2 (uy y2) :class (stl/css :easing-handle-line)}]
      [:path {:d (dm/str "M0," (uy 0.0)
                         " C" (ux x1) "," (uy y1)
                         " " (ux x2) "," (uy y2)
                         " " ee-size "," (uy 1.0))
              :class (stl/css :easing-curve)}]
      ;; pointer capture (set on pointer-down) routes move/up to the handle
      [:circle {:cx (ux x1) :cy (uy y1) :r 6 :class (stl/css :easing-handle)
                :on-pointer-down (fn [e] (on-handle-down :p1 e))
                :on-pointer-move on-move
                :on-pointer-up on-up}]
      [:circle {:cx (ux x2) :cy (uy y2) :r 6 :class (stl/css :easing-handle)
                :on-pointer-down (fn [e] (on-handle-down :p2 e))
                :on-pointer-move on-move
                :on-pointer-up on-up}]]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ROOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mf/defc empty-state*
  {::mf/private true}
  [{:keys [board-name]}]
  (let [on-create (mf/use-fn #(st/emit! (dwa/create-timeline)))]
    [:div {:class (stl/css :empty-state)}
     (if board-name
       [:*
        [:p (tr "workspace.animation.empty-board" board-name)]
        [:button {:class (stl/css :create-btn)
                  :on-click on-create}
         (tr "workspace.animation.create")]]
       [:p (tr "workspace.animation.select-board")])]))

(mf/defc timeline*
  []
  (let [anim       (mf/deref refs/workspace-animation)
        timelines  (mf/deref ref:timelines)
        objects    (mf/deref refs/workspace-page-objects)
        selected   (mf/deref refs/selected-shapes)

        ;; The dock targets the board (top-level frame) of the selection.
        board-id   (when-let [sid (:id (first selected))]
                     (cfh/get-shape-id-root-frame objects sid))
        board-name (get-in objects [board-id :name])
        timeline   (get timelines board-id)
        playhead   (get anim :playhead 0)
        playing?   (get anim :playing? false)
        auto-key?  (get anim :auto-key? false)
        duration   (get timeline :duration 1000)
        selected?  (boolean (seq selected))

        selected-kf (:selected-kf anim)
        selected-keyframe
        (when selected-kf
          (->> (get-in timeline [:tracks (:shape-id selected-kf) :keyframes])
               (d/seek #(= (:id %) (:keyframe-id selected-kf)))))

        ruler-ref  (mf/use-ref nil)
        drag-ref   (mf/use-ref false)

        on-scrub
        (mf/use-fn
         (mf/deps duration)
         (fn [event]
           (let [node (mf/ref-val ruler-ref)
                 t    (pointer->time event node duration)]
             (st/emit! (dwa/set-playhead t)))))

        on-ruler-down
        (mf/use-fn
         (mf/deps duration)
         (fn [event]
           (dom/capture-pointer event)
           (mf/set-ref-val! drag-ref true)
           (on-scrub event)))

        on-ruler-move
        (mf/use-fn
         (mf/deps duration)
         (fn [event]
           (when (mf/ref-val drag-ref)
             (on-scrub event))))

        on-ruler-up
        (mf/use-fn
         (fn [event]
           (dom/release-pointer event)
           (mf/set-ref-val! drag-ref false)))]

    [:section {:class (stl/css :timeline-dock)}
     (if (nil? timeline)
       [:> empty-state* {:board-name board-name}]
       [:*
        [:> toolbar* {:timeline timeline
                      :board-name board-name
                      :playing? playing?
                      :auto-key? auto-key?
                      :selected? selected?}]

        [:div {:class (stl/css :timeline-body)}
         [:div {:class (stl/css :timeline-main)}
          [:div {:class (stl/css :ruler)
                 :ref ruler-ref
                 :on-pointer-down on-ruler-down
                 :on-pointer-move on-ruler-move
                 :on-pointer-up on-ruler-up}
           [:span {:class (stl/css :ruler-time)} (dm/str playhead " / " duration " ms")]
           [:div {:class (stl/css :playhead)
                  :style #js {"left" (time->pct playhead duration)}}]]

          [:div {:class (stl/css :tracks)}
           ;; playhead line spanning the tracks area
           [:div {:class (stl/css :playhead-line)
                  :style #js {"left" (time->pct playhead duration)}}]

           (if (seq (:tracks timeline))
             (for [[shape-id track] (:tracks timeline)]
               [:> track* {:key (dm/str shape-id)
                           :track track
                           :shape-name (get-in objects [shape-id :name] "?")
                           :duration duration
                           :selected-kf selected-kf}])
             [:div {:class (stl/css :tracks-hint)}
              (tr "workspace.animation.tracks-hint")])]]

         (when selected-keyframe
           [:div {:class (stl/css :easing-panel)}
            [:span {:class (stl/css :easing-title)} (tr "workspace.animation.easing")]
            [:> easing-editor* {:shape-id (:shape-id selected-kf)
                                :keyframe-id (:keyframe-id selected-kf)
                                :easing (:easing selected-keyframe)}]])]]])]))
