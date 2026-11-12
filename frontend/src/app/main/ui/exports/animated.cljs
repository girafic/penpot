;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.exports.animated
  "The settings and the button of the animated export of boards (Penpot
  Motion): the workspace keeps the settings with the timeline of a board
  (see `app.main.ui.workspace.sidebar.options.menus.exports`), the view
  mode for one export (see `app.main.ui.inspect.exports`). Light: none of
  the encoders, so that the view mode can show it without loading them."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.main.data.exports.animation-formats :as daf]
   [app.main.data.exports.animation-job :as job]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.ds.controls.combobox :refer [combobox*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.main.ui.workspace.sidebar.options.menus.export-issues :refer [export-issues*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(def ^:private size-presets
  ["0.5" "1" "2" "w1920" "h720" "h1080" "h1440" "h2160"])

(defn animated-size-options
  "The sizes the Animated export offers: the presets and, when it is
  another one, the size `size` it has (see `cta/parse-export-size`)."
  [size]
  (mapv (fn [preset] {:id preset :label (cta/export-size-label preset)})
        (cond-> size-presets
          (and (some? (cta/parse-export-size size))
               (not (some #{size} size-presets)))
          (conj size))))

(defn export-button-state
  "What the panel shows of the animated export `progress` (see
  `refs/export-animation`): `{:label :action :disabled? :note :offer?}`,
  the label and the note as `[key & args]` for `tr`, what a click on the
  button does (`:export` `:cancel`), and whether it offers to open the
  export tab again or to export here (the tab said nothing, see
  `deaw/start-export-job`). `format-label` names the format of the export
  and `disabled?` tells that it cannot start."
  [progress format-label disabled?]
  (let [{:keys [in-progress in-tab? opened? offer? paused? cancelling? percent]} progress]
    (cond
      (not in-progress)
      {:label ["workspace.options.export.export-format" format-label]
       :action :export
       :disabled? (boolean disabled?)}

      ;; how far it is, as the export in a tab tells it
      (not in-tab?)
      {:label ["workspace.options.export.exporting" (or percent 0)]
       :action :cancel
       :disabled? (boolean cancelling?)}

      offer?
      {:label ["labels.cancel"]
       :action :cancel
       :disabled? false
       :note ["workspace.options.export.tab-blocked"]
       :offer? true}

      (not opened?)
      {:label ["workspace.options.export.opening-tab"]
       :action :cancel
       :disabled? (boolean cancelling?)}

      :else
      (cond-> {:label ["workspace.options.export.in-tab" (or percent 0)]
               :action :cancel
               :disabled? (boolean cancelling?)}
        paused? (assoc :note ["workspace.options.export.tab-paused"])))))

(defn- board-size
  [board]
  (let [{:keys [width height]} (:selrect board)]
    [(mth/round (or width 0)) (mth/round (or height 0))]))

(mf/defc animated-export*
  "The Animated export of the `boards` (one, or several into a ZIP), with
  their `timelines` (by board id), as the settings `export` say (see
  `cta/schema:export`); `on-change` with what a field changes of them,
  for all the boards. `progress` the export going on (see
  `export-button-state`); `on-export` with the params of the export (see
  `dea/export-animation`), `on-cancel`, and `on-open-tab` and
  `on-export-here` (none where it cannot export in place) when the tab
  of the job said nothing. `formats` it
  offers, `issues` of each, `render-wasm?`, `codec-supported?` (a
  promise of whether the browser encodes a format, see
  `daf/codec-supported?`) and which way its selects open
  (`select-direction`, \"up\" by default, \"auto\" as they have room)."
  [{:keys [boards timelines export formats issues render-wasm? progress
           on-change on-export on-cancel on-open-tab on-export-here
           codec-supported? select-direction]}]
  (let [codec-supported? (or codec-supported? daf/codec-supported?)
        select-direction (case select-direction
                           nil    "up"
                           "auto" nil
                           select-direction)

        board     (first boards)
        timeline  (get timelines (:id board))
        several?  (> (count boards) 1)
        [width height] (board-size board)

        board-ids (mapv :id boards)
        format    (let [format (:format export)]
                    (if (some #{format} formats) format (first formats)))
        quality   (get export :quality :high)
        fps       (get export :fps 30)
        size      (get export :size "1")
        loop?     (get export :loop true)
        ;; MP4 and WebM carry the sound of the audio clips, unless left out
        audio?    (get export :audio true)
        ;; SVG: the names of the layers as ids, a bounding box, texts as
        ;; the outlines of their glyphs (see `job/svg-options`)
        svg-opts  (into {} (map (fn [[option key]] [option (get export key false)])) job/svg-options)
        ids?      (:ids? svg-opts)
        bbox?     (:bbox? svg-opts)
        outline?  (:outline? svg-opts)
        clips?    (boolean (some #(seq (:audio-clips (get timelines %))) board-ids))
        support*  (mf/use-state {:mp4 true :webm true :avif true})
        support   (deref support*)

        raster?   (#{:mp4 :webm :gif :avif} format)
        loops?    (#{:gif :avif} format)
        too-many? (some #(> (count (cta/frame-times (or (:duration (get timelines %)) 0) fps))
                            cta/max-export-frames)
                        board-ids)
        busy?     (boolean (:in-progress progress))
        codec?    (get support format true)
        disabled? (or (nil? timeline) too-many? busy? (not codec?))

        scale     (cta/export-size-scale size width height)
        out-w     (mth/round (* width scale))
        out-h     (mth/round (* height scale))

        ;; each board at the scale the size gives it
        export-boards
        (mapv (fn [board]
                (let [[w h] (board-size board)]
                  {:board-id (:id board) :scale (cta/export-size-scale size w h)}))
              boards)

        estimate  (when (and timeline raster?)
                    (daf/approx-size-label
                     (reduce + (map (fn [board {:keys [scale]}]
                                      (let [[w h] (board-size board)]
                                        (daf/estimated-size format (mth/round (* w scale)) (mth/round (* h scale))
                                                            quality
                                                            (cta/cycle-duration (get timelines (:id board)))
                                                            fps)))
                                    boards export-boards))))

        format-options
        (filterv #(some #{(keyword (:value %))} formats)
                 [{:value "mp4" :label (:mp4 daf/format-labels) :disabled (not (:mp4 support))}
                  {:value "webm" :label (:webm daf/format-labels) :disabled (not (:webm support))}
                  {:value "gif" :label (:gif daf/format-labels)}
                  {:value "avif" :label (:avif daf/format-labels) :disabled (not (:avif support))}
                  {:value "svg" :label (:svg daf/format-labels)}
                  {:value "lottie" :label (:lottie daf/format-labels)}
                  {:value "dotlottie" :label (:dotlottie daf/format-labels)}])

        quality-options
        [{:value "low" :label (tr "workspace.options.export.quality-low")}
         {:value "medium" :label (tr "workspace.options.export.quality-medium")}
         {:value "high" :label (tr "workspace.options.export.quality-high")}]

        fps-options
        [{:value "12" :label "12fps"}
         {:value "15" :label "15fps"}
         {:value "24" :label "24fps"}
         {:value "25" :label "25fps"}
         {:value "30" :label "30fps"}
         {:value "60" :label "60fps"}]

        on-format
        (mf/use-fn
         (mf/deps on-change)
         (fn [value]
           (let [format (keyword value)]
             (on-change (cond-> {:format format}
                          (= format :gif) (assoc :fps 15))))))

        on-quality
        (mf/use-fn (mf/deps on-change) #(on-change {:quality (keyword %)}))

        on-fps
        (mf/use-fn (mf/deps on-change) #(on-change {:fps (d/parse-integer %)}))

        ;; the field shows the size it has after each edit: a typed size
        ;; it does not take goes back, `3,5` shows as `3.5x`
        size-edits* (mf/use-state 0)
        size-ref    (mf/use-ref size)
        _           (mf/set-ref-val! size-ref size)

        on-size
        (mf/use-fn
         (mf/deps on-change)
         (fn [value]
           (let [parsed (cta/parse-export-size value)]
             (if (and (some? parsed) (not= parsed (mf/ref-val size-ref)))
               (on-change {:size parsed})
               (swap! size-edits* inc)))))

        on-loop
        (mf/use-fn (mf/deps on-change) #(on-change {:loop %}))

        on-audio
        (mf/use-fn (mf/deps on-change) #(on-change {:audio %}))

        on-ids
        (mf/use-fn (mf/deps on-change) #(on-change {:ids (dom/checked? (dom/get-target %))}))

        on-bbox
        (mf/use-fn (mf/deps on-change) #(on-change {:bbox (dom/checked? (dom/get-target %))}))

        on-outline
        (mf/use-fn (mf/deps on-change) #(on-change {:outline (dom/checked? (dom/get-target %))}))

        on-export-click
        (mf/use-fn
         (mf/deps on-export format quality fps scale loop? audio? svg-opts several? export-boards)
         (fn []
           (on-export (cond-> (merge {:format format
                                      :quality quality
                                      :fps fps
                                      :scale scale
                                      :loop? loop?
                                      :audio? audio?}
                                     svg-opts)
                        several? (assoc :boards export-boards)))))

        view
        (export-button-state progress
                             (:label (d/seek #(= (name format) (:value %)) format-options))
                             disabled?)]

    (mf/with-effect [codec-supported?]
      (-> (js/Promise.all
           #js [(codec-supported? :mp4)
                (codec-supported? :webm)
                (codec-supported? :avif)])
          (.then (fn [result]
                   (reset! support* {:mp4 (aget result 0)
                                     :webm (aget result 1)
                                     :avif (aget result 2)}))))
      (constantly nil))

    (when timeline
      [:div {:class (stl/css :animated-panel)}
       [:div {:class (stl/css :animated-fields)}
        [:div {:class (stl/css :animated-row)}
         [:span {:class (stl/css :row-label)} (tr "workspace.options.export.format")]
         [:div {:class (stl/css :row-control)}
          [:& select {:default-value (name format)
                      :options format-options
                      :data-direction select-direction
                      :on-change on-format}]]]

        ;; as Figma's options of an SVG
        (when (= :svg format)
          [:*
           [:> checkbox* {:id "animated-export-svg-ids"
                          :label (tr "workspace.options.export.svg-ids")
                          :checked ids?
                          :on-change on-ids}]
           [:> checkbox* {:id "animated-export-svg-outline"
                          :label (tr "workspace.options.export.svg-outline")
                          :checked outline?
                          :on-change on-outline}]
           [:> checkbox* {:id "animated-export-svg-bbox"
                          :label (tr "workspace.options.export.svg-bbox")
                          :checked bbox?
                          :on-change on-bbox}]])

        [:> export-issues* {:kinds (get issues format)
                            :format-label (get daf/format-labels format)}]

        (when raster?
          [:*
           [:div {:class (stl/css :animated-row)}
            [:span {:class (stl/css :row-label)} (tr "workspace.options.export.size")]
            [:div {:class (stl/css :row-control)}
             [:> combobox* {:key (str size "-" @size-edits*)
                            :id "animated-export-size"
                            :default-selected size
                            :options (animated-size-options size)
                            :max-length 8
                            :on-change on-size}]]]
           [:div {:class (stl/css :animated-row)}
            [:span {:class (stl/css :row-label)} (tr "workspace.options.export.quality")]
            [:div {:class (stl/css :row-control)}
             [:& select {:default-value (name quality)
                         :options quality-options
                         :data-direction select-direction
                         :on-change on-quality}]]]
           [:div {:class (stl/css :animated-row)}
            [:span {:class (stl/css :row-label)} (tr "workspace.options.export.framerate")]
            [:div {:class (stl/css :row-control)}
             [:& select {:default-value (str fps)
                         :options fps-options
                         :data-direction select-direction
                         :on-change on-fps}]]]
           (when loops?
             [:div {:class (stl/css :animated-row)}
              [:span {:class (stl/css :row-label)} (tr "workspace.animation.loop")]
              [:> switch* {:aria-label (tr "workspace.animation.loop")
                           :default-checked loop?
                           :on-change on-loop}]])
           (when (and clips? (contains? daf/audio-formats format))
             [:div {:class (stl/css :animated-row)}
              [:span {:class (stl/css :row-label)} (tr "workspace.options.export.include-audio")]
              [:> switch* {:aria-label (tr "workspace.options.export.include-audio")
                           :default-checked audio?
                           :on-change on-audio}]])])]

       [:div {:class (stl/css :animated-meta)}
        [:div {:class (stl/css :animated-row)}
         [:span {:class (stl/css :row-label)} (tr "workspace.options.export.dimensions")]
         [:span {:class (stl/css :row-value)}
          (if several?
            (tr "workspace.options.export.boards-zip" (count boards))
            (str out-w "×" out-h))]]
        (when estimate
          [:div {:class (stl/css :animated-row)}
           [:span {:class (stl/css :row-label)} (tr "workspace.options.export.file-size")]
           [:span {:class (stl/css :row-value)} estimate]])]

       (when too-many?
         [:p {:class (stl/css :hint)} (tr "workspace.options.export.too-many-frames")])

       (when-not codec?
         [:p {:class (stl/css :hint)} (tr "workspace.options.export.codec-unavailable")])

       ;; MP4 and WebM, which carry audio, render through WASM
       (when (and clips? (not render-wasm?))
         [:p {:class (stl/css :hint)} (tr "workspace.options.export.audio-needs-wasm")])

       ;; mounted while the panel shows, so what comes in is read out
       [:span {:class (stl/css :sr-status) :role "status" :aria-live "polite"}
        (some->> (:note view) (apply tr))]

       (when-let [note (:note view)]
         [:p {:class (stl/css :hint)} (apply tr note)])

       (when (:offer? view)
         [:div {:class (stl/css :animated-offer)}
          [:> button* {:variant "primary"
                       :class (stl/css :animated-export-btn)
                       :on-click on-open-tab}
           (tr "workspace.options.export.open-tab")]
          ;; not where it cannot export in place (the view mode)
          (when (fn? on-export-here)
            [:> button* {:variant "secondary"
                         :class (stl/css :animated-export-btn)
                         :on-click on-export-here}
             (tr "workspace.options.export.export-here")])])

       [:> button* {:variant (if (:offer? view) "ghost" "secondary")
                    :class (stl/css :animated-export-btn)
                    :disabled (:disabled? view)
                    :on-click (case (:action view)
                                :export on-export-click
                                on-cancel)}
        (apply tr (:label view))]])))
