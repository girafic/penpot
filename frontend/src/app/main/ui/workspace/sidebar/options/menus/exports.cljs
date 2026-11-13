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
   [app.common.geom.tilt :as gtl]
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
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.layout.tab-switcher :refer [tab-switcher*]]
   [app.main.ui.exports.animated :as uea]
   [app.main.ui.exports.assets]
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

(mf/defc animated-exports*
  "The Animated export of the `boards` selected (one, or several into a
  ZIP), with their `timelines` (by board id), as the workspace has it:
  the settings of the first, kept with its timeline (see
  `dwa/set-export`), which a change sets for all at once, and the export
  in a tab of its own (see `deaw/export-from-panel!`). See
  `uea/animated-export*`."
  {::mf/private true}
  [{:keys [boards timelines formats issues render-wasm?]}]
  (let [progress  (mf/deref refs/export-animation)
        board-ids (mapv :id boards)
        export    (:export (get timelines (:id (first boards))))
        job-id    (get-in progress [:job :job-id])

        ;; one change for all the boards, undone at once
        on-change
        (mf/use-fn
         (mf/deps board-ids)
         (fn [export]
           (let [undo-id (js/Symbol)]
             (st/emit! (dwu/start-undo-transaction undo-id))
             (run! #(st/emit! (dwa/set-export % export)) board-ids)
             (st/emit! (dwu/commit-undo-transaction undo-id)))))

        ;; in a tab of its own, opened in this click
        on-export
        (mf/use-fn #(deaw/export-from-panel! %))

        on-cancel
        (mf/use-fn #(st/emit! (deaw/cancel-export-animation)))

        on-open-tab
        (mf/use-fn
         (mf/deps progress job-id)
         (fn []
           (deaw/open-job-tab! @st/state (:job progress))
           (st/emit! (deaw/job-tab-reopened job-id))))

        on-export-here
        (mf/use-fn (mf/deps job-id) #(st/emit! (deaw/export-job-here job-id)))]

    ;; The section sits at the bottom of the panel, above the timeline
    ;; dock that clips it, so the selects open upwards.
    [:> uea/animated-export* {:boards boards
                              :timelines timelines
                              :export export
                              :formats formats
                              :issues issues
                              :render-wasm? render-wasm?
                              :progress progress
                              :on-change on-change
                              :on-export on-export
                              :on-cancel on-cancel
                              :on-open-tab on-open-tab
                              :on-export-here on-export-here
                              :codec-supported? dea/codec-supported?
                              :select-direction "up"}]))

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

        ;; PDF and SVG draw the 3D rotation of layers flat (see
        ;; `app.common.geom.tilt`)
        flat-3d?
        (mf/with-memo [shapes exports]
          (and (seqable? exports)
               (some #(contains? #{:pdf :svg} (:type %)) exports)
               (let [objects (deref refs/workspace-page-objects)]
                 (some (fn [shape]
                         (some #(gtl/three-d? (get objects %))
                               (cons (:id shape) (cfh/get-children-ids objects (:id shape)))))
                       shapes))))

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
          (uea/animation-export-boards shapes timelines))

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
                      :title        (if (> (count ids) 1)
                                      (tr "workspace.options.export-multiple")
                                      (tr "workspace.options.export"))
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

           (when flat-3d?
             [:p {:class (stl/css :flat-3d-hint)} (tr "workspace.options.export.flat-3d")])

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
