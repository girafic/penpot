;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.assets.animation-styles
  "The animation styles of a file in the assets panel, by their groups. A
  click applies one to the selected layers in motion mode, or opens it:
  the styles of the file are edited there, those of a library shown."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.path-names :as cpn]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.main.data.modal :as modal]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.animation-styles :as dwas]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu* dropdown-menu-item*]]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.main.ui.workspace.sidebar.assets.common :as cmm]
   [app.main.ui.workspace.sidebar.assets.groups :as grp]
   [app.main.ui.workspace.sidebar.options.menus.motion :as motion :refer [animation-item* keys-entry-item*]]
   [app.main.ui.workspace.tokens.motion :as motion-tokens]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:section-state
  (l/derived (fn [gstate]
               {:renaming-id (:rename-animation-style gstate)
                :editing-id  (:edit-animation-style gstate)})
             refs/workspace-global
             =))

(def ^:private ref:motion-mode?
  (l/derived dwa/motion-mode? st/state))

(def ^:private entry-types
  [:fade :move :scale :rotate :flip-y :flip-x :blur])

(defn- set-editing
  "Open the editor of the style `id`, or close it with nil."
  [id]
  (fn [state]
    (if (some? id)
      (assoc-in state [:workspace-global :edit-animation-style] id)
      (update state :workspace-global dissoc :edit-animation-style))))

(defn- set-renaming
  [id]
  (fn [state]
    (if (some? id)
      (assoc-in state [:workspace-global :rename-animation-style] id)
      (update state :workspace-global dissoc :rename-animation-style))))

(defn- used-style?
  "Whether a layer of the file `file-id` plays the style `style-id` of it."
  [file-id style-id]
  (let [data (dm/get-in @st/state [:files file-id :data])]
    (boolean
     (some (fn [page]
             (some (fn [timeline]
                     (some (fn [track]
                             (some #(ctas/uses-style? % file-id style-id) (:animations track)))
                           (vals (:tracks timeline))))
                   (vals (:timelines page))))
           (vals (:pages-index data))))))

(defn- delete-style!
  "Delete the style `id` of the file `file-id`, asking first when layers
  play it: they keep its animations as presets."
  [file-id id]
  (if (used-style? file-id id)
    (st/emit! (modal/show
               {:type :confirm
                :title (tr "workspace.assets.animation-styles.delete-used-title")
                :message (tr "workspace.assets.animation-styles.delete-used-message")
                :accept-label (tr "labels.delete")
                :on-accept #(st/emit! (dwas/delete-animation-style id))}))
    (st/emit! (dwas/delete-animation-style id))))

;; EDITOR

(mf/defc style-entry*
  {::mf/private true}
  [{:keys [entry on-change on-split on-remove on-change-start on-change-end read-only tokens]}]
  (let [id (:id entry)

        on-entry-change
        (mf/use-fn
         (mf/deps id on-change)
         (fn [attrs]
           (on-change id attrs)))

        on-entry-split
        (mf/use-fn
         (mf/deps id on-split)
         (fn [kind]
           (on-split id kind)))

        on-entry-remove
        (mf/use-fn
         (mf/deps id on-remove)
         #(on-remove id))]

    ;; A preset entry can play the units of the texts the style goes on.
    (if (cta/keys-entry? entry)
      [:> keys-entry-item* {:entry entry
                            :read-only read-only
                            :on-change on-entry-change
                            :on-remove (when (some? on-remove) on-entry-remove)
                            :on-change-start on-change-start
                            :on-change-end on-change-end}]
      [:> animation-item* {:animation entry
                           :read-only read-only
                           :tokens tokens
                           :split-allowed true
                           :on-change on-entry-change
                           :on-split on-entry-split
                           :on-remove (when (some? on-remove) on-entry-remove)
                           :on-change-start on-change-start
                           :on-change-end on-change-end}])))

(mf/defc style-editor*
  "The entries and description of the animation style `style` of the file
  `file-id`: edited when `is-editable`, their settings binding the design
  tokens of the file, shown with the name of its library, `library-name`,
  when not `is-local`. While a number is dragged the uses of the style
  show it as it goes; each finished change is one commit."
  {::mf/private true}
  [{:keys [style file-id is-local is-editable library-name]}]
  (let [entries   (:entries style)
        tokens    (motion-tokens/use-active-tokens-by-type)
        draft*    (mf/use-ref nil)
        dragging* (mf/use-ref false)
        menu*     (mf/use-state false)
        menu?     (deref menu*)

        commit
        (mf/use-fn
         (mf/deps file-id)
         (fn [style']
           (st/emit! (dwas/update-animation-style style' file-id))))

        on-entry-change
        (mf/use-fn
         (mf/deps style commit)
         (fn [entry-id attrs]
           (let [style' (update (or (mf/ref-val draft*) style) :entries
                                (partial mapv #(if (= entry-id (:id %))
                                                 (cta/make-style-entry (merge % attrs))
                                                 %)))]
             (if (mf/ref-val dragging*)
               (do (mf/set-ref-val! draft* style')
                   (st/emit! (dwa/preview-animation-style style')))
               (commit style')))))

        ;; a text plays one kind of units: the other split entries follow
        on-entry-split
        (mf/use-fn
         (mf/deps style commit)
         (fn [entry-id kind]
           (commit (update style :entries cta/with-split-kind entry-id kind cta/make-style-entry))))

        on-change-start
        (mf/use-fn #(mf/set-ref-val! dragging* true))

        on-change-end
        (mf/use-fn
         (mf/deps commit)
         (fn []
           (mf/set-ref-val! dragging* false)
           (when-let [style' (mf/ref-val draft*)]
             (mf/set-ref-val! draft* nil)
             (commit style'))))

        on-entry-remove
        (mf/use-fn
         (mf/deps style commit)
         (fn [entry-id]
           (commit (update style :entries (partial filterv #(not= entry-id (:id %)))))))

        on-toggle-menu
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (swap! menu* not)))

        on-close-menu
        (mf/use-fn #(reset! menu* false))

        on-add
        (mf/use-fn
         (mf/deps style commit)
         (fn [event]
           (let [type (-> (dom/get-current-target event) (dom/get-data "type") keyword)]
             (reset! menu* false)
             (commit (update style :entries conj (cta/make-style-entry {:type type :start 0}))))))

        on-description
        (mf/use-fn
         (mf/deps style commit)
         (fn [event]
           (let [value (str/trim (dom/get-value (dom/get-target event)))]
             (when (not= value (or (:description style) ""))
               (commit (if (str/blank? value)
                         (dissoc style :description)
                         (assoc style :description value)))))))

        on-description-key-down
        (mf/use-fn
         (fn [event]
           (when (kbd/enter? event)
             (dom/blur! (dom/get-target event)))))]

    [:div {:class (stl/css :style-editor)
           :on-click dom/stop-propagation}
     (when-not is-local
       [:div {:class (stl/css :style-library)}
        (tr "workspace.assets.animation-styles.from-library" library-name)])

     [:input {:class (stl/css :style-description)
              :type "text"
              :default-value (or (:description style) "")
              :key (dm/str (:id style) "-" (:description style))
              :placeholder (tr "workspace.assets.animation-styles.description")
              :aria-label (tr "workspace.assets.animation-styles.description")
              :read-only (not is-editable)
              :on-blur on-description
              :on-key-down on-description-key-down}]

     [:div {:class (stl/css :style-entries)}
      (for [entry entries]
        [:> style-entry* {:key (dm/str (:id entry))
                          :entry entry
                          :read-only (not is-editable)
                          :tokens (when is-editable tokens)
                          :on-change on-entry-change
                          :on-split on-entry-split
                          :on-remove (when (and is-editable (> (count entries) 1)) on-entry-remove)
                          :on-change-start on-change-start
                          :on-change-end on-change-end}])]

     (when is-editable
       [:div {:class (stl/css :style-add)}
        [:> icon-button* {:variant "ghost"
                          :icon i/add
                          :aria-label (tr "workspace.animation.add-animation")
                          :aria-pressed menu?
                          :disabled (>= (count entries) cta/max-style-entries)
                          :on-click on-toggle-menu}]
        [:> dropdown-menu* {:show menu?
                            :on-close on-close-menu
                            :class (stl/css :style-add-menu)}
         (for [type entry-types]
           [:> dropdown-menu-item* {:key (name type)
                                    :class (stl/css :style-add-menu-item)
                                    :data-type (name type)
                                    :on-click on-add}
            (motion/animation-label {:type type :direction :in})])]])]))

;; ITEMS

(mf/defc style-item*
  {::mf/private true}
  [{:keys [style file-id is-local library-name is-selected is-editing is-renaming
           on-asset-click on-context-menu]}]
  (let [id          (:id style)
        read-only?  (mf/use-ctx ctx/workspace-read-only?)
        motion?     (mf/deref ref:motion-mode?)
        selected    (mf/deref refs/selected-shapes)

        on-click
        (mf/use-fn
         (mf/deps id file-id on-asset-click motion? selected is-editing read-only?)
         (fn [event]
           (when-not (on-asset-click event id)
             (if (and motion? (seq selected) (not read-only?))
               (st/emit! (dwa/apply-animation-style selected file-id id))
               (st/emit! (set-editing (when-not is-editing id)))))))

        on-item-context-menu
        (mf/use-fn
         (mf/deps id on-context-menu)
         (partial on-context-menu id))

        on-rename
        (mf/use-fn
         (mf/deps id file-id)
         (fn [event]
           (let [value (str/trim (dom/get-value (dom/get-target event)))]
             (st/emit! (set-renaming nil))
             (when-not (or (str/blank? value)
                           (= value (cpn/merge-path-item (:path style) (:name style))))
               (st/emit! (dwas/rename-animation-style file-id id value))))))

        on-rename-key-down
        (mf/use-fn
         (fn [event]
           (cond
             (kbd/enter? event) (dom/blur! (dom/get-target event))
             (kbd/esc? event)   (do (dom/stop-propagation event)
                                    (st/emit! (set-renaming nil))))))]

    [:div {:class (stl/css :style-item)}
     [:div {:class (stl/css-case :style-row true
                                 :selected is-selected
                                 :editing is-editing)
            :title (tr "workspace.assets.animation-styles.item-title")
            :on-click on-click
            :on-context-menu on-item-context-menu}
      [:span {:class (stl/css :style-icon)}
       [:> icon* {:icon-id i/motion :size "s"}]]
      (if is-renaming
        [:input {:class (stl/css :style-rename)
                 :type "text"
                 :auto-focus true
                 :default-value (cpn/merge-path-item (:path style) (:name style))
                 :on-focus dom/select-target
                 :on-click dom/stop-propagation
                 :on-blur on-rename
                 :on-key-down on-rename-key-down}]
        [:span {:class (stl/css :style-name)} (:name style)])
      [:span {:class (stl/css :style-meta)}
       (dm/str (count (:entries style)) " · " (cta/style-length (:entries style)) " ms")]]

     (when is-editing
       [:> style-editor* {:style style
                          :file-id file-id
                          :is-local is-local
                          :is-editable (and is-local (not read-only?))
                          :library-name library-name}])]))

(mf/defc styles-group*
  {::mf/private true}
  [{:keys [file-id prefix groups open-groups is-local library-name selected editing-id renaming-id
           on-asset-click on-context-menu on-rename-group on-ungroup on-delete-group]}]
  (let [group-open? (if (false? (get open-groups prefix))
                      false
                      (get open-groups prefix true))]
    [:div {:class (stl/css :styles-group)}
     [:> grp/asset-group-title* {:file-id file-id
                                 :section :animation-styles
                                 :path prefix
                                 :is-group-open group-open?
                                 :on-rename on-rename-group
                                 :on-ungroup on-ungroup
                                 :on-delete-group on-delete-group}]
     (when group-open?
       [:*
        [:div {:class (stl/css :styles-list)}
         (for [{:keys [id] :as style} (get groups "" [])]
           [:> style-item* {:key (dm/str "animation-style-" id)
                            :style style
                            :file-id file-id
                            :is-local is-local
                            :library-name library-name
                            :is-selected (contains? selected id)
                            :is-editing (= editing-id id)
                            :is-renaming (= renaming-id id)
                            :on-asset-click on-asset-click
                            :on-context-menu on-context-menu}])]
        (for [[path-item content] groups]
          (when-not (empty? path-item)
            [:> styles-group* {:key (dm/str "group-" path-item)
                               :file-id file-id
                               :prefix (cpn/merge-path-item prefix path-item)
                               :groups content
                               :open-groups open-groups
                               :is-local is-local
                               :library-name library-name
                               :selected selected
                               :editing-id editing-id
                               :renaming-id renaming-id
                               :on-asset-click on-asset-click
                               :on-context-menu on-context-menu
                               :on-rename-group on-rename-group
                               :on-ungroup on-ungroup
                               :on-delete-group on-delete-group}]))])]))

(mf/defc animation-styles-section*
  "The animation styles of the file `file-id` (named `file-name`), the
  current one when `is-local`."
  [{:keys [file-id file-name styles open-status-ref selected is-local is-open is-reverse-sort
           on-asset-click on-assets-delete on-clear-selection]}]
  (let [{:keys [editing-id renaming-id]} (mf/deref ref:section-state)
        read-only?   (mf/use-ctx ctx/workspace-read-only?)
        menu-state   (mf/use-state cmm/initial-context-menu-state)
        menu-id*     (mf/use-state nil)
        menu-id      (deref menu-id*)

        groups       (mf/with-memo [styles is-reverse-sort]
                       (grp/group-assets styles is-reverse-sort))

        selected     (:animation-styles selected)
        multi?       (> (count selected) 1)
        multi-assets? (or (seq (:components selected))
                          (seq (:graphics selected))
                          (seq (:colors selected))
                          (seq (:typographies selected)))

        open-groups-ref (mf/with-memo [open-status-ref]
                          (-> (l/in [:groups :animation-styles])
                              (l/derived open-status-ref)))
        open-groups  (mf/deref open-groups-ref)

        rename-all
        (mf/use-fn
         (mf/deps file-id)
         (fn [names]
           (on-clear-selection)
           (st/emit! (dwas/rename-animation-styles file-id names))))

        grouped
        (mf/use-fn
         (mf/deps styles selected menu-id multi? rename-all)
         (fn [group-name]
           (rename-all (into {}
                             (comp (filter #(if multi? (contains? selected (:id %)) (= menu-id (:id %))))
                                   (map (fn [style] [(:id style) (cmm/add-group style group-name)])))
                             styles))))

        rename-group
        (mf/use-fn
         (mf/deps styles rename-all)
         (fn [path last-path]
           (rename-all (into {}
                             (comp (filter #(str/starts-with? (:path %) path))
                                   (map (fn [style] [(:id style) (cmm/rename-group style path last-path)])))
                             styles))))

        on-group
        (mf/use-fn
         (mf/deps grouped)
         (fn [event]
           (dom/stop-propagation event)
           (modal/show! :name-group-dialog {:accept grouped})))

        on-rename-group
        (mf/use-fn
         (mf/deps rename-group)
         (fn [event path last-path]
           (dom/stop-propagation event)
           (modal/show! :name-group-dialog {:path path
                                            :last-path last-path
                                            :accept rename-group})))

        on-ungroup
        (mf/use-fn
         (mf/deps styles rename-all)
         (fn [path]
           (rename-all (into {}
                             (comp (filter #(str/starts-with? (:path %) path))
                                   (map (fn [style] [(:id style) (cmm/ungroup style path)])))
                             styles))))

        on-delete-group
        (mf/with-memo [styles on-clear-selection]
          (cmm/make-delete-asset-group-fn
           {:assets styles
            :on-clear-selection on-clear-selection
            :delete-events #(map (fn [style] (dwas/delete-animation-style (:id style))) %)}))

        on-context-menu
        (mf/use-fn
         (mf/deps selected on-clear-selection read-only?)
         (fn [id event]
           (dom/prevent-default event)
           (when-not read-only?
             (when-not (contains? selected id)
               (on-clear-selection))
             (reset! menu-id* id)
             (swap! menu-state cmm/open-context-menu (dom/get-client-position event)))))

        on-close-menu
        (mf/use-fn #(swap! menu-state cmm/close-context-menu))

        on-rename
        (mf/use-fn (mf/deps menu-id) #(st/emit! (set-renaming menu-id)))

        on-edit
        (mf/use-fn (mf/deps menu-id) #(st/emit! (set-editing menu-id)))

        on-duplicate
        (mf/use-fn (mf/deps file-id menu-id) #(st/emit! (dwas/duplicate-animation-style file-id menu-id)))

        on-delete
        (mf/use-fn
         (mf/deps file-id menu-id multi? multi-assets? on-assets-delete)
         (fn []
           (if (or multi? multi-assets?)
             (on-assets-delete)
             (delete-style! file-id menu-id))))

        on-asset-click
        (mf/use-fn
         (mf/deps groups on-asset-click)
         (partial on-asset-click groups))

        options
        (mf/with-memo [is-local multi? multi-assets? on-rename on-edit on-duplicate on-delete on-group]
          (if is-local
            [(when-not (or multi? multi-assets?)
               {:name (tr "workspace.assets.rename") :id "assets-rename-animation-style" :handler on-rename})
             (when-not (or multi? multi-assets?)
               {:name (tr "workspace.assets.edit") :id "assets-edit-animation-style" :handler on-edit})
             (when-not (or multi? multi-assets?)
               {:name (tr "workspace.assets.duplicate") :id "assets-duplicate-animation-style" :handler on-duplicate})
             {:name (tr "workspace.assets.delete") :id "assets-delete-animation-style" :handler on-delete}
             (when-not multi-assets?
               {:name (tr "workspace.assets.group") :id "assets-group-animation-style" :handler on-group})]
            [{:name (tr "workspace.assets.edit") :id "assets-edit-animation-style" :handler on-edit}]))]

    [:> cmm/asset-section* {:file-id file-id
                            :title (tr "workspace.assets.animation-styles")
                            :section :animation-styles
                            :icon i/motion
                            :assets-count (count styles)
                            :is-open is-open}
     [:> cmm/asset-section-block* {:role :content}
      [:> styles-group* {:file-id file-id
                         :prefix ""
                         :groups groups
                         :open-groups open-groups
                         :is-local is-local
                         :library-name file-name
                         :selected selected
                         :editing-id editing-id
                         :renaming-id renaming-id
                         :on-asset-click on-asset-click
                         :on-context-menu on-context-menu
                         :on-rename-group on-rename-group
                         :on-ungroup on-ungroup
                         :on-delete-group on-delete-group}]
      [:> cmm/assets-context-menu* {:on-close on-close-menu
                                    :state @menu-state
                                    :options options}]]]))
