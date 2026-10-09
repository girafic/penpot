;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.exports
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.files.helpers :as cfh]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-workspace :as deaw]
   [app.main.data.exports.assets :as de]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.features :as features]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.ds.controls.combobox :refer [combobox*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.layout.tab-switcher :refer [tab-switcher*]]
   [app.main.ui.exports.assets]
   [app.main.ui.workspace.sidebar.options.menus.export-issues :refer [export-issues*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [c tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def exports-attrs
  "Shape attrs that corresponds to exports. Used in other namespaces."
  [:exports])

(defn- check-exports-menu-props
  [old-props new-props]
  (and (identical? (unchecked-get old-props "ids")
                   (unchecked-get new-props "ids"))
       (identical? (unchecked-get old-props "type")
                   (unchecked-get new-props "type"))
       (identical? (unchecked-get old-props "pageId")
                   (unchecked-get new-props "pageId"))
       (identical? (unchecked-get old-props "fileId")
                   (unchecked-get new-props "fileId"))

       ;; NOTE: we explicitly ignore "shapes" prop and use values for
       ;; track if the "value" changes (checking by value equality);
       ;; this prevents rerender the component when no real change is
       ;; made to exports
       (= (unchecked-get old-props "values")
          (unchecked-get new-props "values"))))

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

(defn animation-export-boards
  "The selected boards that have a timeline to export, their own or the
  one of the component copies in them (`timelines` holds them by board,
  see `dwa/board-export-timeline`), when only boards are selected (`type`
  is `:frame` or `:multiple`); else nil. The Animated export tab is shown
  only for them: one, or several into a ZIP."
  [type shapes timelines]
  (when (and (contains? #{:frame :multiple} type)
             (seq shapes)
             (every? cfh/frame-shape? shapes))
    (not-empty (filterv #(contains? timelines (:id %)) shapes))))

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

(mf/defc animated-exports*
  "The Animated export of the `boards` selected (one, or several into a
  ZIP), with their `timelines` (by board id): the settings of the first,
  which a change sets for all."
  {::mf/private true}
  [{:keys [boards timelines formats issues render-wasm?]}]
  (let [progress  (mf/deref refs/export-animation)
        board     (first boards)
        timeline  (get timelines (:id board))
        several?  (> (count boards) 1)
        [width height] (board-size board)

        ;; As last set for the board, kept with its timeline (see
        ;; `dwa/set-export`).
        board-ids (mapv :id boards)
        export    (:export timeline)
        format    (let [format (:format export)]
                    (if (some #{format} formats) format (first formats)))
        quality   (get export :quality :high)
        fps       (get export :fps 30)
        size      (get export :size "1")
        loop?     (get export :loop true)
        ;; MP4 and WebM carry the sound of the audio clips, unless left out
        audio?    (get export :audio true)
        ;; SVG: the names of the layers as ids, a bounding box, texts as
        ;; the outlines of their glyphs
        ids?      (get export :ids false)
        bbox?     (get export :bbox false)
        outline?  (get export :outline false)
        clips?    (boolean (some #(seq (:audio-clips (get timelines %))) board-ids))
        support*  (mf/use-state {:mp4 true :webm true :avif true})
        support   (deref support*)

        raster?   (#{:mp4 :webm :gif :avif} format)
        loops?    (#{:gif :avif} format)
        too-many? (some #(> (count (cta/frame-times (or (:duration (get timelines %)) 0) fps))
                            cta/max-export-frames)
                        board-ids)
        busy?     (boolean (:in-progress progress))
        job-id    (get-in progress [:job :job-id])
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
                    (dea/approx-size-label
                     (reduce + (map (fn [board {:keys [scale]}]
                                      (let [[w h] (board-size board)]
                                        (dea/estimated-size format (mth/round (* w scale)) (mth/round (* h scale))
                                                            quality
                                                            (cta/cycle-duration (get timelines (:id board)))
                                                            fps)))
                                    boards export-boards))))

        format-options
        (filterv #(some #{(keyword (:value %))} formats)
                 [{:value "mp4" :label (:mp4 dea/format-labels) :disabled (not (:mp4 support))}
                  {:value "webm" :label (:webm dea/format-labels) :disabled (not (:webm support))}
                  {:value "gif" :label (:gif dea/format-labels)}
                  {:value "avif" :label (:avif dea/format-labels) :disabled (not (:avif support))}
                  {:value "svg" :label (:svg dea/format-labels)}
                  {:value "lottie" :label (:lottie dea/format-labels)}
                  {:value "dotlottie" :label (:dotlottie dea/format-labels)}])

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

        ;; one change for all the boards, undone at once
        set-export
        (mf/use-fn
         (mf/deps board-ids)
         (fn [export]
           (let [undo-id (js/Symbol)]
             (st/emit! (dwu/start-undo-transaction undo-id))
             (run! #(st/emit! (dwa/set-export % export)) board-ids)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        on-format
        (mf/use-fn
         (mf/deps set-export)
         (fn [value]
           (let [format (keyword value)]
             (set-export (cond-> {:format format}
                           (= format :gif) (assoc :fps 15))))))

        on-quality
        (mf/use-fn (mf/deps set-export) #(set-export {:quality (keyword %)}))

        on-fps
        (mf/use-fn (mf/deps set-export) #(set-export {:fps (d/parse-integer %)}))

        ;; the field shows the size it has after each edit: a typed size
        ;; it does not take goes back, `3,5` shows as `3.5x`
        size-edits* (mf/use-state 0)
        size-ref    (mf/use-ref size)
        _           (mf/set-ref-val! size-ref size)

        on-size
        (mf/use-fn
         (mf/deps set-export)
         (fn [value]
           (let [parsed (cta/parse-export-size value)]
             (if (and (some? parsed) (not= parsed (mf/ref-val size-ref)))
               (set-export {:size parsed})
               (swap! size-edits* inc)))))

        on-loop
        (mf/use-fn (mf/deps set-export) #(set-export {:loop %}))

        on-audio
        (mf/use-fn (mf/deps set-export) #(set-export {:audio %}))

        on-ids
        (mf/use-fn (mf/deps set-export) #(set-export {:ids (dom/checked? (dom/get-target %))}))

        on-bbox
        (mf/use-fn (mf/deps set-export) #(set-export {:bbox (dom/checked? (dom/get-target %))}))

        on-outline
        (mf/use-fn (mf/deps set-export) #(set-export {:outline (dom/checked? (dom/get-target %))}))

        ;; in a tab of its own, opened in this click
        on-export
        (mf/use-fn
         (mf/deps format quality fps scale loop? audio? ids? bbox? outline? several? export-boards)
         (fn []
           (deaw/export-from-panel! (cond-> {:format format
                                             :quality quality
                                             :fps fps
                                             :scale scale
                                             :loop? loop?
                                             :audio? audio?
                                             :ids? ids?
                                             :bbox? bbox?
                                             :outline? outline?}
                                      several? (assoc :boards export-boards)))))

        on-cancel
        (mf/use-fn #(st/emit! (deaw/cancel-export-animation)))

        on-open-tab
        (mf/use-fn
         (mf/deps progress)
         (fn []
           (deaw/open-job-tab! @st/state (:job progress))
           (st/emit! (deaw/job-tab-reopened job-id))))

        on-export-here
        (mf/use-fn (mf/deps job-id) #(st/emit! (deaw/export-job-here job-id)))

        view
        (export-button-state progress
                             (:label (d/seek #(= (name format) (:value %)) format-options))
                             disabled?)]

    (mf/use-effect
     (fn []
       (-> (js/Promise.all
            #js [(dea/codec-supported? :mp4)
                 (dea/codec-supported? :webm)
                 (dea/codec-supported? :avif)])
           (.then (fn [result]
                    (reset! support* {:mp4 (aget result 0)
                                      :webm (aget result 1)
                                      :avif (aget result 2)}))))
       (constantly nil)))

    ;; The section sits at the bottom of the panel, above the timeline
    ;; dock that clips it, so the selects open upwards.
    (when timeline
      [:div {:class (stl/css :animated-panel)}
       [:div {:class (stl/css :animated-fields)}
        [:div {:class (stl/css :animated-row)}
         [:span {:class (stl/css :row-label)} (tr "workspace.options.export.format")]
         [:div {:class (stl/css :row-control)}
          [:& select {:default-value (name format)
                      :options format-options
                      :data-direction "up"
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
                            :format-label (get dea/format-labels format)}]

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
                         :data-direction "up"
                         :on-change on-quality}]]]
           [:div {:class (stl/css :animated-row)}
            [:span {:class (stl/css :row-label)} (tr "workspace.options.export.framerate")]
            [:div {:class (stl/css :row-control)}
             [:& select {:default-value (str fps)
                         :options fps-options
                         :data-direction "up"
                         :on-change on-fps}]]]
           (when loops?
             [:div {:class (stl/css :animated-row)}
              [:span {:class (stl/css :row-label)} (tr "workspace.animation.loop")]
              [:> switch* {:aria-label (tr "workspace.animation.loop")
                           :default-checked loop?
                           :on-change on-loop}]])
           (when (and clips? (contains? dea/audio-formats format))
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
          [:> button* {:variant "secondary"
                       :class (stl/css :animated-export-btn)
                       :on-click on-export-here}
           (tr "workspace.options.export.export-here")]])

       [:> button* {:variant (if (:offer? view) "ghost" "secondary")
                    :class (stl/css :animated-export-btn)
                    :disabled (:disabled? view)
                    :on-click (case (:action view)
                                :export on-export
                                on-cancel)}
        (apply tr (:label view))]])))

(mf/defc exports-menu*
  {::mf/wrap [#(mf/memo' % check-exports-menu-props)]}
  [{:keys [ids type shapes values file-id page-id]}]

  (let [exports (get values :exports [])
        open*   (mf/use-state true)
        open?   (deref open*)

        state   (mf/deref refs/export)

        in-progress?
        (get state :in-progress)

        has-exports?
        (or (= :multiple exports)
            (some? (seq exports)))

        toggle-content
        (mf/use-fn #(swap! open* not))

        shapes-with-exports
        (mf/with-memo [shapes]
          (filter (comp seq :exports) shapes))

        sname
        (when (seqable? exports)
          (let [sname  (-> shapes-with-exports first :name)
                suffix (-> exports first :suffix)]
            (cond-> sname
              (and (= 1 (count exports)) (some? suffix))
              (str suffix))))

        scale-enabled?
        (mf/use-fn
         (fn [export]
           (#{:png :jpeg :webp} (:type export))))

        on-download
        (mf/use-fn
         (mf/deps ids page-id file-id exports)
         (fn [event]
           (dom/prevent-default event)
           (if (= :multiple type)
             ;; I can select multiple shapes all of them with no export settings and one of them with only one
             ;; In that situation we must export it directly
             (if (and (= 1 (count shapes-with-exports)) (= 1 (-> shapes-with-exports first :exports count)))
               (let [shape       (-> shapes-with-exports first)
                     export      (-> shape :exports first)
                     suffix      (:suffix export)
                     sname       (cond-> (:name shape)
                                   (some? suffix)
                                   (str suffix))
                     defaults    {:page-id page-id
                                  :file-id file-id
                                  :name sname
                                  :object-id (:id (first shapes-with-exports))}
                     full-export (merge export defaults)]
                 (st/emit! (de/request-simple-export {:export full-export})
                           (de/export-shapes-event [full-export] "workspace:sidebar")))
               (st/emit!
                (de/show-workspace-export-dialog {:selected (reverse ids) :origin "workspace:sidebar"})))

             ;; In other all cases we only allowed to have a single
             ;; shape-id because multiple shape-ids are handled
             ;; separately by the export-modal.
             (let [defaults {:page-id page-id
                             :file-id file-id
                             :name sname
                             :object-id (first ids)}
                   exports  (mapv #(merge % defaults) exports)]

               (st/emit!
                (de/request-export {:exports exports})
                (de/export-shapes-event exports "workspace:sidebar"))))))


        ;; TODO: maybe move to specific events for avoid to have this logic here?
        add-export
        (mf/use-fn
         (mf/deps ids)
         (fn []
           (let [xspec {:type :png :suffix "" :scale 1}]
             (st/emit! (dwsh/update-shapes ids
                                           (fn [shape]
                                             (assoc shape :exports (into [xspec] (:exports shape)))))))))

        delete-export
        (mf/use-fn
         (mf/deps ids)
         (fn [event]
           (let [value (-> (dom/get-current-target event)
                           (dom/get-data "value")
                           (d/parse-integer))
                 remove-fill-by-index (fn [values index] (->> (d/enumerate values)
                                                              (filterv (fn [[idx _]] (not= idx index)))
                                                              (mapv second)))

                 remove (fn [shape] (update shape :exports remove-fill-by-index value))]
             (st/emit! (dwsh/update-shapes ids remove)))))

        on-scale-change
        (mf/use-fn
         (mf/deps ids)
         (fn [index event]
           (let [scale (d/parse-double event)]
             (st/emit! (dwsh/update-shapes ids
                                           (fn [shape]
                                             (assoc-in shape [:exports index :scale] scale)))))))

        on-suffix-change
        (mf/use-fn
         (mf/deps ids)
         (fn [event]
           (let [value   (dom/get-target-val event)
                 index   (-> (dom/get-current-target event)
                             (dom/get-data "value")
                             (d/parse-integer))]
             (st/emit! (dwsh/update-shapes ids
                                           (fn [shape]
                                             (assoc-in shape [:exports index :suffix] value)))))))

        on-type-change
        (mf/use-fn
         (mf/deps ids)
         (fn [index event]
           (let [type (keyword event)]
             (st/emit! (dwsh/update-shapes ids
                                           (fn [shape]
                                             (assoc-in shape [:exports index :type] type)))))))

        on-remove-all
        (mf/use-fn
         (mf/deps ids)
         (fn []
           (st/emit! (dwsh/update-shapes ids
                                         (fn [shape]
                                           (assoc shape :exports []))))))
        manage-key-down
        (mf/use-fn
         (fn [event]
           (let [esc?   (kbd/esc? event)]
             (when esc?
               (dom/blur! (dom/get-target event))))))

        size-options [{:value "0.5" :label "0.5x"}
                      {:value "0.75" :label "0.75x"}
                      {:value "1" :label "1x"}
                      {:value "1.5" :label "1.5x"}
                      {:value "2" :label "2x"}
                      {:value "3" :label "3x"}
                      {:value "4" :label "4x"}
                      {:value "6" :label "6x"}]

        format-options [{:value "png" :label "PNG"}
                        {:value "jpeg" :label "JPG"}
                        {:value "webp" :label "WEBP"}
                        {:value "svg" :label "SVG"}
                        {:value "pdf" :label "PDF"}]

        render-wasm? (features/use-feature "render-wasm/v1")

        ;; What the selected boards export as animations: their
        ;; timelines with the animations of the component copies in them.
        page      (mf/deref refs/workspace-page)
        files     (mf/deref refs/files)
        board-ids (when (and (contains? #{:frame :multiple} type)
                             (every? cfh/frame-shape? shapes))
                    (mapv :id shapes))
        ;; by value: a new vector each render would make new refs below
        boards-key (str/join "," board-ids)
        timelines (mf/with-memo [page files boards-key]
                    (into {}
                          (keep (fn [id]
                                  (when-let [tl (dwa/board-export-timeline page files id)]
                                    [id tl])))
                          board-ids))
        timeline  (get timelines (first (filter #(contains? timelines %) board-ids)))

        ;; Most formats rasterize through WASM: without it the tab offers
        ;; what is left, for all the boards, and is gone when nothing is.
        formats-ref
        (mf/with-memo [timelines render-wasm?]
          (l/derived (fn [objects]
                       (let [all (map #(dea/export-formats objects % render-wasm?) (vals timelines))]
                         (if (seq all)
                           (filterv (fn [format] (every? #(some #{format} %) all)) (first all))
                           [])))
                     refs/workspace-page-objects =))

        formats (mf/deref formats-ref)

        ;; What the export in each format plays otherwise than the canvas,
        ;; of any board (see `dea/export-issues`).
        issues-ref
        (mf/with-memo [timelines]
          (l/derived (fn [objects]
                       (apply merge-with (comp vec distinct concat)
                              (map #(dea/export-issues objects %) (vals timelines))))
                     refs/workspace-page-objects =))

        issues (mf/deref issues-ref)

        animated-boards
        (when (seq formats)
          (animation-export-boards type shapes timelines))

        animated-board (first animated-boards)
        animated-id    (str/join "," (map :id animated-boards))

        ;; A board exported as an animation opens on that tab: choosing
        ;; it keeps the settings of the board (see `dwa/set-export`).
        tab* (mf/use-state nil)
        tab  (or (deref tab*)
                 (if (some? (:export timeline)) "animated" "static"))

        on-tab
        (mf/use-fn
         (mf/deps animated-boards)
         (fn [id]
           (let [animated? (= id "export-animated")]
             (reset! tab* (if animated? "animated" "static"))
             (when animated?
               (run! #(st/emit! (dwa/set-export (:id %) {})) animated-boards)))))

        export-tabs
        [{:id "export-static" :label (tr "workspace.options.export.static")}
         {:id "export-animated" :label (tr "workspace.options.export.animated")}]]

    ;; Another board opens on its own tab.
    (mf/with-effect [animated-id]
      (reset! tab* nil))

    [:div {:class (stl/css :element-set)}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable  has-exports?
                      :collapsed    (not open?)
                      :on-collapsed toggle-content
                      :title        (tr (if (> (count ids) 1) "workspace.options.export-multiple" "workspace.options.export"))
                      :class        (stl/css-case :title-spacing-export (not has-exports?))}
       (when (or (nil? animated-board) (= tab "static"))
         [:> icon-button* {:variant "ghost"
                           :aria-label (tr "workspace.options.export.add-export")
                           :on-click add-export
                           :icon i/add}])]]
     (when open?
       [:div {:class (stl/css :element-set-content)}
        (when animated-board
          [:> tab-switcher* {:class (stl/css :export-type)
                             :tabs export-tabs
                             :selected (str "export-" tab)
                             :on-change on-tab}
           (when (= tab "animated")
             [:> animated-exports* {:key animated-id
                                    :boards animated-boards
                                    :timelines timelines
                                    :formats formats
                                    :issues issues
                                    :render-wasm? render-wasm?}])])

        (when (or (nil? animated-board) (= tab "static"))
          [:*
           (cond
             (= :multiple exports)
             [:div {:class (stl/css :multiple-exports)}
              [:div {:class (stl/css :label)} (tr "settings.multiple")]
              [:div {:class (stl/css :actions)}
               [:> icon-button* {:variant "ghost"
                                 :aria-label (tr "workspace.options.export.remove-export")
                                 :on-click on-remove-all
                                 :icon i/remove}]]]

             (seq exports)
             [:*
              (for [[index export] (d/enumerate exports)]
                [:div {:class (stl/css :element-group)
                       :key index}
                 [:div {:class (stl/css :input-wrapper)}
                  [:div  {:class (stl/css :format-select)}
                   [:& select
                    {:default-value (d/name (:type export))
                     :options format-options
                     :dropdown-class (stl/css :dropdown-upwards)
                     :on-change (partial on-type-change index)}]]
                  (when (scale-enabled? export)
                    [:div {:class (stl/css :size-select)}
                     [:& select
                      {:default-value (str (:scale export))
                       :options size-options
                       :dropdown-class (stl/css :dropdown-upwards)
                       :on-change (partial on-scale-change index)}]])
                  [:label {:class (stl/css :suffix-input)
                           :for "suffix-export-input"}
                   [:input {:class (stl/css :type-input)
                            :id "suffix-export-input"
                            :type "text"
                            :value (:suffix export)
                            :placeholder (tr "workspace.options.export.suffix")
                            :data-value (str index)
                            :on-change on-suffix-change
                            :on-key-down manage-key-down}]]]

                 [:> icon-button* {:variant "ghost"
                                   :aria-label (tr "workspace.options.export.remove-export")
                                   :on-click delete-export
                                   :data-value index
                                   :icon i/remove}]])])

           (when (or (= :multiple exports) (seq exports))
             [:button
              {:on-click (when-not in-progress? on-download)
               :class (stl/css-case
                       :export-btn true
                       :btn-disabled in-progress?)
               :disabled in-progress?}
              (if in-progress?
                (tr "workspace.options.exporting-object")
                (tr "workspace.options.export-object" (c (count shapes-with-exports))))])])])]))
