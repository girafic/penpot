;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.exports
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.assets :as de]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.features :as features]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.layout.tab-switcher :refer [tab-switcher*]]
   [app.main.ui.exports.assets]
   [app.util.dom :as dom]
   [app.util.i18n :refer [c tr]]
   [app.util.keyboard :as kbd]
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

(defn export-scale
  "Pixel scale for an animated export. `0.5` and `2` are factors.
  `w1920` fits the width, `h720` and the other heights fit the height."
  [preset width height]
  (cond
    (= preset "0.5") 0.5
    (= preset "2") 2.0
    (= preset "w1920") (/ 1920.0 (max 1 width))
    (= preset "h720") (/ 720.0 (max 1 height))
    (= preset "h1080") (/ 1080.0 (max 1 height))
    (= preset "h1440") (/ 1440.0 (max 1 height))
    (= preset "h2160") (/ 2160.0 (max 1 height))
    :else 1.0))

(defn animation-export-board
  "The selected board when it has a timeline to export, its own or the
  one of the component copies in it (`timelines` holds them by board, see
  `dwa/board-export-timeline`), else nil. The Animated export tab is
  shown only in that case."
  [type ids shapes timelines]
  (when (and (= type :frame) (= 1 (count ids)))
    (let [shape (first shapes)]
      (when (and shape (get timelines (:id shape)))
        shape))))

(mf/defc animated-exports*
  {::mf/private true}
  [{:keys [board timeline formats]}]
  (let [progress  (mf/deref refs/export-animation)
        selrect   (:selrect board)
        width     (mth/round (or (:width selrect) 0))
        height    (mth/round (or (:height selrect) 0))

        ;; As last set for the board, kept with its timeline (see
        ;; `dwa/set-export`).
        board-id  (:id board)
        export    (:export timeline)
        format    (let [format (:format export)]
                    (if (some #{format} formats) format (first formats)))
        quality   (get export :quality :high)
        fps       (get export :fps 30)
        size      (get export :size "1")
        loop?     (get export :loop true)
        support*  (mf/use-state {:mp4 true :webm true :avif true})
        support   (deref support*)

        raster?   (#{:mp4 :webm :gif :avif} format)
        loops?    (#{:gif :avif} format)
        times     (cta/frame-times (or (:duration timeline) 0) fps)
        too-many? (> (count times) cta/max-export-frames)
        busy?     (boolean (:in-progress progress))
        codec?    (get support format true)
        disabled? (or (nil? timeline) too-many? busy? (not codec?))

        scale     (export-scale size width height)
        out-w     (mth/round (* width scale))
        out-h     (mth/round (* height scale))

        estimate  (when (and timeline raster?)
                    (dea/approx-size-label
                     (dea/estimated-size format out-w out-h quality
                                         (cta/cycle-duration timeline) fps)))

        format-options
        (filterv #(some #{(keyword (:value %))} formats)
                 [{:value "mp4" :label "MP4" :disabled (not (:mp4 support))}
                  {:value "webm" :label "WebM" :disabled (not (:webm support))}
                  {:value "gif" :label "GIF"}
                  {:value "avif" :label "AVIF" :disabled (not (:avif support))}
                  {:value "svg" :label "SVG"}
                  {:value "lottie" :label "Lottie"}])

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

        size-options
        [{:value "0.5" :label "0.5x"}
         {:value "1" :label "1x"}
         {:value "2" :label "2x"}
         {:value "w1920" :label "1920w"}
         {:value "h720" :label "720h"}
         {:value "h1080" :label "1080h"}
         {:value "h1440" :label "1440h"}
         {:value "h2160" :label "2160h"}]

        set-export
        (mf/use-fn
         (mf/deps board-id)
         #(st/emit! (dwa/set-export board-id %)))

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

        on-size
        (mf/use-fn (mf/deps set-export) #(set-export {:size %}))

        on-loop
        (mf/use-fn (mf/deps set-export) #(set-export {:loop %}))

        on-export
        (mf/use-fn
         (mf/deps format quality fps scale loop?)
         (fn []
           (st/emit! (dea/export-animation {:format format
                                            :quality quality
                                            :fps fps
                                            :scale scale
                                            :loop? loop?}))))

        on-cancel
        (mf/use-fn #(st/emit! (dea/cancel-export-animation)))]

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

        (when raster?
          [:*
           [:div {:class (stl/css :animated-row)}
            [:span {:class (stl/css :row-label)} (tr "workspace.options.export.size")]
            [:div {:class (stl/css :row-control)}
             [:& select {:default-value size
                         :options size-options
                         :data-direction "up"
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
                           :on-change on-loop}]])])]

       [:div {:class (stl/css :animated-meta)}
        [:div {:class (stl/css :animated-row)}
         [:span {:class (stl/css :row-label)} (tr "workspace.options.export.dimensions")]
         [:span {:class (stl/css :row-value)} (str out-w "×" out-h)]]
        (when estimate
          [:div {:class (stl/css :animated-row)}
           [:span {:class (stl/css :row-label)} (tr "workspace.options.export.file-size")]
           [:span {:class (stl/css :row-value)} estimate]])]

       (when too-many?
         [:p {:class (stl/css :hint)} (tr "workspace.options.export.too-many-frames")])

       (when-not codec?
         [:p {:class (stl/css :hint)} (tr "workspace.options.export.codec-unavailable")])

       (if busy?
         [:> button* {:variant "secondary"
                      :class (stl/css :animated-export-btn)
                      :on-click on-cancel}
          (tr "workspace.options.export.progress"
              (c (or (:current progress) 0))
              (c (or (:total progress) 0)))]
         [:> button* {:variant "secondary"
                      :class (stl/css :animated-export-btn)
                      :disabled disabled?
                      :on-click on-export}
          (tr "workspace.options.export.export-format"
              (:label (d/seek #(= (name format) (:value %)) format-options)))])])))

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

        ;; What the selected board exports as an animation: its timeline
        ;; with the animations of the component copies in it.
        page      (mf/deref refs/workspace-page)
        files     (mf/deref refs/files)
        board-id  (when (and (= type :frame) (= 1 (count ids)))
                    (:id (first shapes)))
        timeline  (mf/with-memo [page files board-id]
                    (when (some? board-id)
                      (dwa/board-export-timeline page files board-id)))
        timelines (when (some? timeline)
                    {board-id timeline})

        ;; Most formats rasterize through WASM: without it the tab offers
        ;; what is left, and is gone when nothing is.
        formats-ref
        (mf/with-memo [timeline render-wasm?]
          (l/derived #(dea/export-formats % timeline render-wasm?)
                     refs/workspace-page-objects =))

        formats (mf/deref formats-ref)

        animated-board
        (when (seq formats)
          (animation-export-board type ids shapes timelines))

        animated-id (:id animated-board)

        ;; A board exported as an animation opens on that tab: choosing
        ;; it keeps the settings of the board (see `dwa/set-export`).
        tab* (mf/use-state nil)
        tab  (or (deref tab*)
                 (if (some? (:export timeline)) "animated" "static"))

        on-tab
        (mf/use-fn
         (mf/deps animated-id)
         (fn [id]
           (let [animated? (= id "export-animated")]
             (reset! tab* (if animated? "animated" "static"))
             (when (and animated? (some? animated-id))
               (st/emit! (dwa/set-export animated-id {}))))))

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
             [:> animated-exports* {:key (str animated-id)
                                    :board animated-board
                                    :timeline timeline
                                    :formats formats}])])

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
