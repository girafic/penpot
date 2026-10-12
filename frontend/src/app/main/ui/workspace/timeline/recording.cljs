;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline.recording
  "The controls of recording sound into the timeline, in its toolbar: the
  microphone, which starts and stops it and shows how long it has
  recorded, and the arrow beside it that opens its options (see
  `app.main.ui.workspace.timeline.recording-options`,
  `app.main.data.workspace.audio-recording`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.main.data.workspace.audio-recording :as dwar]
   [app.main.store :as st]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.workspace.timeline.recording-options :refer [recording-options*]]
   [app.util.audio :as audio]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(defn elapsed-label
  "`ms` recorded where the clip goes as minutes and seconds; before it,
  in the pre-roll, the seconds left, counting down (negative `ms`)."
  [ms]
  (let [secs (if (neg? ms) (js/Math.ceil (/ (- ms) 1000)) (quot ms 1000))]
    (dm/str (when (neg? ms) "−") (quot secs 60) ":" (.padStart (str (mod secs 60)) 2 "0"))))

(defn recording-row-index
  "The index among the rows of the timeline dock `rows` of the one that
  shows the recording in progress (see `dwar/with-recording-clip`): its
  lane, or the group of the audio lanes while it is folded; nil without."
  [rows]
  (let [recording? (fn [row] (some #(= dwar/recording-clip-id (:id %)) (:clips row)))]
    (or (d/index-of-pred rows #(and (= :audio (:type %)) (recording? %)))
        (d/index-of-pred rows #(and (= :audio-group (:type %)) (:collapsed? %) (recording? %))))))

(mf/defc recording-time*
  "How long the recording that started at `since` (`Date.now`) has lasted
  where its clip goes, past its pre-roll of `offset` ms (see
  `elapsed-label`)."
  {::mf/private true}
  [{:keys [since offset]}]
  (let [now* (mf/use-state #(js/Date.now))]
    (mf/with-effect [since]
      (let [timer (js/setInterval #(reset! now* (js/Date.now)) 250)]
        #(js/clearInterval timer)))
    [:span (elapsed-label (- @now* since (or offset 0)))]))

(mf/defc recording-controls*
  "The microphone and the arrow of its options; `recording` is the
  recording in progress (see `dwar/recording`): pending while the browser
  asks for the microphone, red with its time while it records."
  [{:keys [recording]}]
  (let [anchor* (mf/use-state nil)
        anchor  (deref anchor*)

        ;; the animation plays along, with its sound: a page starts sound
        ;; only from a user gesture
        on-toggle
        (mf/use-fn #(do (audio/unlock!)
                        (st/emit! (dwar/toggle-audio-recording))))

        ;; the options, above the button that opens them
        on-options
        (mf/use-fn
         (fn [event]
           (let [rect (dom/get-bounding-rect (dom/get-current-target event))]
             (reset! anchor* {:top (:top rect) :left (:left rect) :width (:width rect)}))))

        on-close-options
        (mf/use-fn #(reset! anchor* nil))]

    [:div {:class (stl/css :controls)}
     [:button {:type "button"
               :class (stl/css-case :mic-btn true
                                    :pending (true? (:pending? recording))
                                    :recording (some? (:since recording)))
               :title (if (some? recording)
                        (tr "workspace.animation.stop-audio-recording")
                        (tr "workspace.animation.record-audio"))
               :aria-pressed (some? recording)
               :on-click on-toggle}
      [:> i/icon* {:icon-id i/microphone}]
      (when-let [since (:since recording)]
        [:> recording-time* {:since since :offset (:offset recording)}])]
     [:button {:type "button"
               :class (stl/css-case :options-btn true :active (some? anchor))
               :title (tr "workspace.animation.recording-options")
               :aria-haspopup "dialog"
               :aria-expanded (some? anchor)
               :disabled (some? recording)
               :on-click on-options}
      [:> i/icon* {:icon-id i/arrow-down :size "s"}]]
     (when (some? anchor)
       [:> recording-options* {:anchor anchor
                               :on-close on-close-options}])]))
