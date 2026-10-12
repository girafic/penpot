;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline.recording-options
  "The options of recording sound into the timeline, in a popover over
  the button that opens them, next to the microphone of its toolbar: a
  pre-roll, the microphone, and whether the other clips keep quiet (see
  `app.main.data.workspace.audio-recording`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.workspace.audio-recording :as dwar]
   [app.main.store :as st]
   [app.main.ui.ds.controls.radio-buttons :refer [radio-buttons*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.main.ui.hooks :as hooks]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:options
  (l/derived dwar/recording-options st/state))

(def ^:private system-microphone
  "The id of the option of the system's microphone."
  "system")

(defn microphone-options
  "The options of the microphones `devices` (see
  `dwar/list-microphones!`), the system's first, each without a label
  numbered."
  [devices]
  (into [{:id system-microphone :label (tr "workspace.animation.recording-system-microphone")}]
        (map-indexed (fn [i {:keys [id label]}]
                       {:id id
                        :label (or label (tr "workspace.animation.recording-microphone" (inc i)))}))
        devices))

(mf/defc recording-options*
  "The options of recording, above `anchor` (the client rect of the button
  that opened them), over the sidebars (in a portal); `on-close` closes
  them."
  [{:keys [anchor on-close]}]
  (let [{:keys [pre-roll device-id quiet?]} (mf/deref ref:options)
        container (hooks/use-portal-container :popup)
        devices* (mf/use-state nil)
        devices  (deref devices*)

        on-pre-roll
        (mf/use-fn #(st/emit! (dwar/set-recording-options {:pre-roll (parse-long %)})))

        on-microphone
        (mf/use-fn #(st/emit! (dwar/set-recording-options
                               {:device-id (when (not= % system-microphone) %)})))

        on-quiet
        (mf/use-fn #(st/emit! (dwar/set-recording-options {:quiet? %})))

        ;; the keys of the timeline wait meanwhile (its arrows nudge clips)
        on-key-down
        (mf/use-fn
         (mf/deps on-close)
         (fn [event]
           (dom/stop-propagation event)
           (when (kbd/esc? event)
             (on-close))))]

    (mf/with-effect []
      (let [live? (volatile! true)]
        (.then (dwar/list-microphones!) #(when @live? (reset! devices* %)))
        #(vreset! live? false)))

    (mf/portal
     (mf/html
      [:*
       [:div {:class (stl/css :backdrop) :on-pointer-down on-close}]
       [:div {:class (stl/css :popover)
              :role "dialog"
              :aria-label (tr "workspace.animation.recording-options")
              :tab-index -1
              :style #js {"top" (dm/str (:top anchor) "px")
                          "left" (dm/str (+ (:left anchor) (/ (:width anchor) 2)) "px")}
              :on-key-down on-key-down}
        [:span {:class (stl/css :title)} (tr "workspace.animation.recording-options")]

        [:div {:class (stl/css :row)}
         [:span {:class (stl/css :label)} (tr "workspace.animation.recording-pre-roll")]
         [:> radio-buttons* {:name "recording-pre-roll"
                             :selected (str pre-roll)
                             :extended true
                             :options (mapv (fn [ms]
                                              {:id (dm/str "recording-pre-roll-" ms)
                                               :value (str ms)
                                               :label (if (zero? ms)
                                                        (tr "workspace.animation.recording-pre-roll-off")
                                                        (dm/str (/ ms 1000) " s"))})
                                            dwar/pre-rolls)
                             :on-change on-pre-roll}]]

        [:div {:class (stl/css :row)}
         [:span {:class (stl/css :label)} (tr "workspace.animation.recording-microphone-label")]
         ;; listed once the browser gives them; a choice gone keeps the system's
         (when (some? devices)
           (let [options (microphone-options devices)]
             [:> select* {:options options
                          :default-selected (if (some #(= device-id (:id %)) options)
                                              device-id
                                              system-microphone)
                          :on-change on-microphone}]))]

        [:div {:class (stl/css :row)}
         [:> switch* {:label (tr "workspace.animation.recording-quiet")
                      :default-checked (true? quiet?)
                      :on-change on-quiet}]]]])
     container)))
