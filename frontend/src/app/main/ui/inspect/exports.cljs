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
   [app.main.data.exports.animation-job :as job]
   [app.main.data.exports.assets :as de]
   [app.main.data.notifications :as ntf]
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

(def ^:private all-formats
  "What the view mode exports an animation as with the WASM renderer;
  without it only Lottie, which the export checks draws as vectors (see
  `dea/export-formats`, which needs the encoders)."
  [:mp4 :webm :gif :avif :svg :lottie :dotlottie])

(defn animated-boards
  "The boards of `shapes` a member of the team exports as an animation from
  the view mode: those with a timeline of their own on `page`, when only
  boards are selected; nil without."
  [shapes page]
  (when (and (seq shapes) (every? cfh/frame-shape? shapes))
    (not-empty (filterv #(contains? (:timelines page) (:id %)) shapes))))

(def ^:private end-types
  "What the tab of an export says as it ends."
  #{:done :error :cancelled :closed})

(mf/defc animated-export*
  "The animated export of the `boards` of `page` from the view mode, as the
  workspace offers it (see `uea/animated-export*`): the settings the
  workspace keeps with the timeline of the first, changed for this export
  only, and the export in the tab the workspace exports in (see
  `vej/open!`), which shows how far it is too."
  {::mf/private true}
  [{:keys [boards page file-id]}]
  (let [timelines    (into {} (map (fn [{:keys [id]}] [id (get-in page [:timelines id])])) boards)
        stored       (:export (get timelines (:id (first boards))))
        changes*     (mf/use-state {})
        export       (merge stored (deref changes*))
        progress*    (mf/use-state nil)
        progress     (deref progress*)
        channel-ref  (mf/use-ref nil)
        render-wasm? (features/use-feature "render-wasm/v1")
        formats      (if render-wasm? all-formats [:lottie :dotlottie])

        close!
        (mf/use-fn
         (fn []
           (some-> (mf/ref-val channel-ref) :close (apply []))
           (mf/set-ref-val! channel-ref nil)))

        on-message
        (mf/use-fn
         (mf/deps close!)
         (fn [message]
           (swap! progress* vej/heard message)
           (when (= :error (:type message))
             (st/emit! (ntf/error (apply tr (job/error-text message)))))
           (when (contains? end-types (:type message))
             (close!))))

        on-change
        (mf/use-fn #(swap! changes* merge %))

        ;; in the tab, opened in this click
        on-export
        (mf/use-fn
         (mf/deps file-id page boards render-wasm? close! on-message)
         (fn [params]
           (close!)
           (let [job     (vej/job-of file-id (:id page) (:id (first boards)) render-wasm? params)
                 channel (vej/open! (:router @st/state) job on-message)]
             (when (some? channel)
               (mf/set-ref-val! channel-ref channel)
               (reset! progress* (vej/started job))))))

        ;; the tab stops when it is there, else it ends at once
        on-cancel
        (mf/use-fn
         (mf/deps progress close!)
         (fn []
           (if (:opened? progress)
             (do (some-> (mf/ref-val channel-ref) :post (apply [{:type :cancel}]))
                 (swap! progress* assoc :cancelling? true))
             (do (close!)
                 (reset! progress* nil)))))]

    ;; the tab goes on by itself once this is gone
    (mf/with-effect [close!]
      close!)

    [:> uea/animated-export* {:boards boards
                              :timelines timelines
                              :export export
                              :formats formats
                              :render-wasm? render-wasm?
                              :progress progress
                              :on-change on-change
                              :on-export on-export
                              :on-cancel on-cancel
                              :select-direction "auto"}]))

(mf/defc exports
  {::mf/wrap [#(mf/memo % =)]}
  [{:keys [shapes page-id file-id share-id type] :as props}]
  (let [exports     (mf/use-state [])
        xstate      (mf/deref refs/export)
        vstate      (mf/deref refs/viewer-data)
        page        (get-in vstate [:pages page-id])

        ;; a member of the team exports the animation of a board, as the
        ;; workspace does (Penpot Motion)
        animated    (when (get-in vstate [:permissions :in-team])
                      (animated-boards shapes page))
        animated-key (str/join "," (map :id animated))
        tab*        (mf/use-state nil)
        tab         (or (deref tab*)
                        (if (some? (get-in page [:timelines (:id (first animated)) :export]))
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
                                  :page page
                                  :file-id file-id}]
            static)]
         static))]))

