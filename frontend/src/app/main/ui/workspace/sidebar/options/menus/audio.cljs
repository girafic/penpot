;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.options.menus.audio
  "The audio of a board in the design tab, in motion mode, as Figma shows
  it: a row by audio lane of its timeline, named after its file, which
  selects its clips in the timeline dock, plays or mutes them and removes
  them; the add button picks audio files to add at the playhead."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.audio :as dwaa]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:timeline
  (l/derived dwa/current-timeline st/state))

(def ^:private ref:selected-clips
  (l/derived (comp dwaa/selected-clips :workspace-animation) st/state))

(def ^:private ref:active-board-id
  (l/derived dwa/active-board-id st/state))

(mf/defc audio-lane*
  "The row of the audio lane `row` (see `dwaa/audio-rows`)."
  {::mf/private true}
  [{:keys [row selected]}]
  (let [{:keys [lane name clips]} row
        clip-ids (mapv :id clips)
        muted?   (every? :muted clips)
        picked?  (every? #(contains? selected %) clip-ids)

        on-select
        (mf/use-fn
         (mf/deps clip-ids)
         #(st/emit! (dwaa/select-clips clip-ids)))

        on-toggle-muted
        (mf/use-fn
         (mf/deps lane muted?)
         #(st/emit! (dwaa/set-lane-muted lane (not muted?))))

        on-remove
        (mf/use-fn
         (mf/deps lane)
         #(st/emit! (dwaa/remove-lane-clips lane)))]

    [:div {:class (stl/css :audio-lane)}
     [:button {:type "button"
               :class (stl/css-case :audio-name true
                                    :selected picked?
                                    :muted muted?)
               :title name
               :aria-pressed picked?
               :on-click on-select}
      [:> i/icon* {:icon-id (if muted? i/volume-off i/volume) :size "s"}]
      [:span {:class (stl/css :audio-label)} name]]
     [:div {:class (stl/css :audio-actions)}
      [:> icon-button* {:variant "ghost"
                        :aria-label (if muted?
                                      (tr "workspace.animation.unmute")
                                      (tr "workspace.animation.mute"))
                        :icon (if muted? i/hide i/shown)
                        :on-click on-toggle-muted}]
      [:> icon-button* {:variant "ghost"
                        :aria-label (tr "workspace.animation.remove-audio")
                        :icon i/remove
                        :on-click on-remove}]]]))

(mf/defc audio-menu*
  "The audio of the board selected, when it is the one of the timeline
  dock: its lanes, and a button to add audio files."
  [{:keys [ids]}]
  (let [timeline  (mf/deref ref:timeline)
        selected  (mf/deref ref:selected-clips)
        board-id  (mf/deref ref:active-board-id)
        input-ref (mf/use-ref nil)
        rows      (mf/with-memo [timeline]
                    (filterv #(= :audio (:type %)) (dwaa/audio-rows timeline)))

        on-add
        (mf/use-fn
         #(some-> (mf/ref-val input-ref) (dom/click)))

        on-files
        (mf/use-fn
         (mf/deps board-id)
         (fn [event]
           (let [target (dom/get-target event)
                 files  (filterv dwaa/audio-file? (array-seq (.-files ^js target)))]
             (when (seq files)
               (st/emit! (dwaa/add-audio-files board-id files (dwa/board-playhead @st/state board-id) nil)))
             ;; the same files again add again
             (set! (.-value ^js target) ""))))]

    (when (and (= 1 (count ids)) (= (first ids) board-id))
      [:div {:class (stl/css :audio-menu)}
       [:div {:class (stl/css :audio-title)}
        [:> title-bar* {:collapsable false
                        :title (tr "workspace.animation.audio")}
         [:> icon-button* {:variant "ghost"
                           :aria-label (tr "workspace.animation.add-audio")
                           :icon i/add
                           :on-click on-add}]]
        [:input {:type "file"
                 :ref input-ref
                 :class (stl/css :audio-input)
                 :accept "audio/*"
                 :multiple true
                 :tab-index -1
                 :aria-hidden true
                 :on-change on-files}]]
       (when (seq rows)
         [:div {:class (stl/css :audio-list)}
          (for [row rows]
            [:> audio-lane* {:key (dm/str (:lane row))
                             :row row
                             :selected selected}])])])))
