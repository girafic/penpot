;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.inspect.exports
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.files.helpers :as cfh]
   [app.main.data.exports.animation-formats :as daf]
   [app.main.data.exports.assets :as de]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.export-job :as vej]
   [app.main.features :as features]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.layout.tab-switcher :refer [tab-switcher*]]
   [app.main.ui.exports.animated :as uea]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr c]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private exports-cache-ref
  (l/derived :inspect-exports-cache st/state))

(def ^:private export-progress-ref
  "How far the animated export of the view mode is (see
  `vej/start-job`), nil without one."
  (l/derived :viewer-export-animation st/state))

(mf/defc animated-export*
  "The animated export of the `boards` of `page` from the view mode, with
  their `timelines` (by board id, see `dv/board-export-timeline`), as the
  workspace offers it (see `uea/animated-export*`): the settings the
  workspace keeps with the timeline of the first, changed for this export
  only, and the export in the tab the workspace exports in, which loads
  the file itself and needs nothing of the view mode (see
  `vej/start-job`). How far it is lives in the store: the panel may go
  and come back meanwhile. No list of what a format plays otherwise
  (see `dea/export-issues`, which needs the encoders)."
  {::mf/private true}
  [{:keys [boards timelines page file-id]}]
  (let [stored       (:export (get timelines (:id (first boards))))
        changes*     (mf/use-state {})
        export       (merge stored (deref changes*))
        progress     (mf/deref export-progress-ref)
        render-wasm? (features/use-feature "render-wasm/v1")
        formats      (if render-wasm? daf/formats daf/vector-formats)

        on-change
        (mf/use-fn #(swap! changes* merge %))

        ;; in the tab, opened in this click
        on-export
        (mf/use-fn
         (mf/deps file-id page boards render-wasm?)
         (fn [params]
           (let [job (vej/board-job {:file-id file-id
                                     :page-id (:id page)
                                     :board-id (:id (first boards))
                                     :wasm? render-wasm?}
                                    params)]
             (vej/open-job-tab (:router @st/state) job)
             (st/emit! (vej/start-job job)))))

        on-cancel
        (mf/use-fn #(st/emit! (vej/cancel-job)))

        ;; the tab did not say it is there: open it again, in this click
        on-open-tab
        (mf/use-fn
         (mf/deps progress)
         (fn []
           (when-let [job (:job progress)]
             (vej/open-job-tab (:router @st/state) job)
             (st/emit! (vej/job-tab-reopened (:job-id job))))))]

    [:> uea/animated-export* {:boards boards
                              :timelines timelines
                              :export export
                              :formats formats
                              :render-wasm? render-wasm?
                              :progress progress
                              :on-change on-change
                              :on-export on-export
                              :on-cancel on-cancel
                              :on-open-tab on-open-tab}]))

(mf/defc exports
  {::mf/wrap [#(mf/memo % =)]}
  [{:keys [shapes page-id file-id share-id type] :as props}]
  (let [exports     (mf/use-state [])
        xstate      (mf/deref refs/export)
        vstate      (mf/deref refs/viewer-data)
        page        (get-in vstate [:pages page-id])

        ;; a member of the team exports the animation of a board, as the
        ;; workspace does (Penpot Motion): the selected boards with a
        ;; timeline of their own or animated component copies in them,
        ;; their timelines as the export reads them
        files       (mf/deref refs/files)
        in-team?    (get-in vstate [:permissions :in-team])
        board-ids   (when (and in-team? (every? cfh/frame-shape? shapes))
                      (mapv :id shapes))
        ;; by value: a new vector each render would build them again
        boards-key  (str/join "," board-ids)
        timelines   (mf/with-memo [page files boards-key]
                      (into {}
                            (keep (fn [id]
                                    (when-let [tl (dv/board-export-timeline page files id)]
                                      [id tl])))
                            board-ids))
        ;; the kind of the selection, from the shapes: the `type` this
        ;; component gets is not it (it reads `cljs.core/type`)
        sel-type    (if (next shapes) :multiple (:type (first shapes)))
        animated    (uea/animation-export-boards sel-type shapes timelines)
        animated-key (str/join "," (map :id animated))
        tab*        (mf/use-state nil)
        tab         (or (deref tab*)
                        (if (some? (:export (get timelines (:id (first animated)))))
                          "animated"
                          "static"))
        filename    (if (= (count shapes) 1)
                      (let [sname   (-> shapes first :name)
                            suffix (-> @exports first :suffix)]
                        (cond-> sname
                          (and (= 1 (count @exports)) (some? suffix))
                          (str suffix)))
                      (:name page))

        scale-enabled?
        (mf/use-callback
         (fn [export]
           (#{:png :jpeg :webp} (:type export))))

        in-progress? (:in-progress xstate)

        on-download
        (fn [event]
          (dom/prevent-default event)
          (if (= :multiple type)
            (st/emit! (de/show-viewer-export-dialog {:shapes shapes
                                                     :exports @exports
                                                     :name filename
                                                     :page-id page-id
                                                     :file-id file-id
                                                     :share-id share-id}))

            ;; In other all cases we only allowed to have a single
            ;; shape-id because multiple shape-ids are handled
            ;; separately by the export-modal.
            (let [defaults (-> {:page-id page-id
                                :file-id file-id
                                :name filename
                                :object-id (-> shapes first :id)}
                               (cond-> share-id (assoc :share-id share-id)))
                  exports  (mapv #(merge % defaults) @exports)]
              (st/emit!
               (de/request-export {:exports exports})
               (de/export-shapes-event exports "viewer")))))

        shapes-key
        (mf/use-memo (mf/deps shapes) #(vec (sort (map :id shapes))))

        add-export
        (mf/use-callback
         (mf/deps shapes exports)
         (fn []
           (let [xspec {:type :png
                        :suffix ""
                        :scale 1}
                 new-exports (conj @exports xspec)]
             (reset! exports new-exports)
             (st/emit! (dv/update-exports-cache shapes-key new-exports)))))

        delete-export
        (mf/use-callback
         (mf/deps shapes exports)
         (fn [index]
           (let [new-exports (let [[before after] (split-at index @exports)]
                               (d/concat-vec before (rest after)))]
             (reset! exports new-exports)
             (st/emit! (dv/update-exports-cache shapes-key new-exports)))))

        on-scale-change
        (mf/use-callback
         (mf/deps shapes exports)
         (fn [index event]
           (let [scale (d/parse-double event)
                 new-exports (assoc-in @exports [index :scale] scale)]
             (reset! exports new-exports)
             (st/emit! (dv/update-exports-cache shapes-key new-exports)))))

        on-suffix-change
        (mf/use-callback
         (mf/deps shapes exports)
         (fn [event]
           (let [value (dom/get-target-val event)
                 index (-> (dom/get-current-target event)
                           (dom/get-data "value")
                           (d/parse-integer))
                 new-exports (assoc-in @exports [index :suffix] value)]
             (reset! exports new-exports)
             (st/emit! (dv/update-exports-cache shapes-key new-exports)))))

        on-type-change
        (mf/use-callback
         (mf/deps shapes exports)
         (fn [index event]
           (let [type (keyword event)
                 new-exports (assoc-in @exports [index :type] type)]
             (reset! exports new-exports)
             (st/emit! (dv/update-exports-cache shapes-key new-exports)))))

        manage-key-down
        (mf/use-callback
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
                        {:value "pdf" :label "PDF"}]]

    (mf/use-effect
     (mf/deps shapes)
     (fn []
       (let [shapes-key (vec (sort (map :id shapes)))
             cached     (get @exports-cache-ref shapes-key)]
         (if (some? cached)
           (reset! exports cached)
           (reset! exports (->> shapes
                                (mapcat #(:exports % []))
                                (distinct)
                                vec))))))
    ;; another board opens on its own tab
    (mf/with-effect [animated-key]
      (reset! tab* nil))

    [:div {:class (stl/css :element-set)}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable false
                      :title       (tr "workspace.options.export")
                      :class       (stl/css :title-spacing-export-viewer)}
       (when (or (nil? animated) (= tab "static"))
         [:> icon-button* {:variant "ghost"
                           :aria-label (tr "workspace.options.export.add-export")
                           :on-click add-export
                           :icon i/add}])]]

     (let [static
           (mf/html
            [:*
             (cond
               (= :multiple exports)
               [:div {:class (stl/css :multiple-exports)}
                [:div {:class (stl/css :label)} (tr "settings.multiple")]
                [:div {:class (stl/css :actions)}
                 [:> icon-button* {:variant "ghost"
                                   :aria-label (tr "workspace.options.export.remove-export")
                                   :icon i/remove}]]]

               (seq @exports)
               [:div {:class (stl/css :element-set-content)}
                (for [[index export] (d/enumerate @exports)]
                  [:div {:class (stl/css :element-group)
                         :key index}
                   [:div {:class (stl/css :input-wrapper)}
                    [:div  {:class (stl/css :format-select)}
                     [:& select
                      {:default-value (d/name (:type export))
                       :options format-options
                       :dropdown-class (stl/css :dropdown)
                       :on-change (partial on-type-change index)}]]
                    (when (scale-enabled? export)
                      [:div {:class (stl/css :size-select)}
                       [:& select
                        {:default-value (str (:scale export))
                         :options size-options
                         :dropdown-class (stl/css :dropdown)
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
                                     :on-click (partial delete-export index)
                                     :icon i/remove}]])])
             (when (or (= :multiple exports) (seq @exports))
               [:> button* {:variant "secondary"
                            :class (stl/css :export-btn)
                            :on-click (when-not in-progress? on-download)
                            :disabled in-progress?}
                (if in-progress?
                  (tr "workspace.options.exporting-object")
                  (tr "workspace.options.export-object" (c (count shapes))))])])]
       (if (seq animated)
         [:> tab-switcher* {:class (stl/css :export-type)
                            :tabs [{:id "export-static" :label (tr "workspace.options.export.static")}
                                   {:id "export-animated" :label (tr "workspace.options.export.animated")}]
                            :selected (str "export-" tab)
                            :on-change #(reset! tab* (if (= % "export-animated") "animated" "static"))}
          (if (= tab "animated")
            [:> animated-export* {:key animated-key
                                  :boards animated
                                  :timelines timelines
                                  :page page
                                  :file-id file-id}]
            static)]
         static))]))

