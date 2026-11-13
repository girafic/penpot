;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.inspect-exports-test
  "Which boards the view mode exports as an animation, and the timeline
  the export of each reads (Penpot Motion): its own, or the one of the
  component copies in it, their sound and their clock."
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.components :as cthc]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.data.viewer :as dv]
   [app.main.ui.exports.animated :as uea]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(t/use-fixtures :each {:before cthi/reset-idmap!})

(defn- scene
  "An animated component (its main board's rect moves in a second, over
  and over, with a sound when `sound?`), board B holding a copy of it and
  no timeline of its own, and a still board C."
  [& {:keys [sound?]}]
  (let [file  (-> (cthf/sample-file :file1)
                  (ctho/add-simple-component :anim :main :main-rect
                                             :root-params {:x 0 :y 0 :width 200 :height 200}
                                             :child-params {:x 10 :y 20 :width 50 :height 50})
                  (ctho/add-frame :board-b :x 1000 :y 100 :width 400 :height 300)
                  (cthc/instantiate-component :anim :copy :parent-label :board-b)
                  (ctho/add-frame :board-c :x 2000 :y 100 :width 400 :height 300))
        main  (cthi/id :main)
        clip  {:id (uuid/next) :audio-id (uuid/next) :name "ding.wav" :source-duration 1000
               :start 0 :offset 0 :duration 1000 :lane 0}
        mtl   (cond-> (-> (cta/make-timeline {:board-id main :duration 1000 :playback :loop})
                          (cta/add-keyframe (cthi/id :main-rect) {:time 0 :property :x :value 10})
                          (cta/add-keyframe (cthi/id :main-rect) {:time 1000 :property :x :value 60}))
                sound? (assoc :audio-clips {(:id clip) clip}))
        file  (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                           (pcb/with-page (cthf/current-page file))
                                           (pcb/set-timeline main mtl)))
        page  (cthf/current-page file)]
    {:page page
     :files {(:id file) file}
     :board-b (get-in page [:objects (cthi/id :board-b)])
     :board-c (get-in page [:objects (cthi/id :board-c)])}))

(defn- timelines-of
  [page files boards]
  (into {}
        (keep (fn [{:keys [id]}]
                (when-let [tl (dv/board-export-timeline page files id)]
                  [id tl])))
        boards))

(t/deftest a-board-animated-by-its-copies-exports
  (let [{:keys [page files board-b board-c]} (scene)
        timelines (timelines-of page files [board-b board-c])
        timeline  (get timelines (:id board-b))]
    (t/is (nil? (get-in page [:timelines (:id board-b)])) "no timeline of its own")
    (t/is (some? timeline) "the one of its copies")
    (t/is (= [1000 :loop] [(:duration timeline) (:playback timeline)]) "on their clock")
    (t/is (nil? (get timelines (:id board-c))) "a still board has none")
    (t/is (= [board-b] (uea/animation-export-boards [board-b board-c] timelines))
          "the Animated tab for it alone")
    (t/is (nil? (uea/animation-export-boards [board-c] timelines)))))

(t/deftest the-sound-of-the-copies-counts
  (let [{:keys [page files board-b]} (scene :sound? true)
        timeline (dv/board-export-timeline page files (:id board-b))]
    (t/is (seq (:audio-clips timeline)) "the sound of the main of its copy")))

(t/deftest a-board-with-its-own-timeline-keeps-it
  (let [{:keys [page files board-c]} (scene)
        page     (assoc-in page [:timelines (:id board-c)]
                           (-> (cta/make-timeline {:board-id (:id board-c) :duration 2500})
                               (assoc :export {:format :gif})))
        timeline (dv/board-export-timeline page files (:id board-c))]
    (t/is (= 2500 (:duration timeline)))
    (t/is (= {:format :gif} (:export timeline)) "with its export settings")))

(defn- switches-shown
  "A promise of how many switches the Animated export of `board` shows
  for an MP4, with `timelines`, once what it asks the browser (whether
  it encodes the format) answered: an answer after the DOM is gone
  would reach React in a later test."
  [board timelines]
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element uea/animated-export*
                                    (js-obj "boards" [board]
                                            "timelines" timelines
                                            "export" {:format :mp4}
                                            "formats" [:mp4 :lottie]
                                            "renderWasm?" true
                                            "onChange" identity
                                            "onExport" identity
                                            "onCancel" identity
                                            "codecSupported?" (fn [& _] (js/Promise.resolve true)))))
      (.then (thd/settle)
             (fn [_] (.-length (.querySelectorAll document "[role=switch]")))))))

(t/deftest ^:async the-panel-offers-the-sound-of-the-copies
  (let [shown (fn [{:keys [page files board-b]}]
                (switches-shown board-b {(:id board-b) (dv/board-export-timeline page files (:id board-b))}))
        quiet (await (shown (scene)))
        loud  (await (shown (scene :sound? true)))]
    (t/is (= (inc quiet) loud)
          "the switch to include the sound, of the main of its copy")))
