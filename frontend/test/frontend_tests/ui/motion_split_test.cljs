;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.motion-split-test
  (:require
   [app.common.data :as d]
   [app.common.files.changes :as cfc]
   [app.common.types.animation :as cta]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.animation :as dwa]
   [app.main.ui.workspace.sidebar.options.menus.motion :as motion]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(def ^:private fade
  (cta/make-animation {:type :fade :start 0 :duration 1000}))

(def ^:private fade-by-word
  (cta/make-animation {:type :fade :start 0 :duration 1000 :split :word}))

(t/deftest split-controls-show-where-a-text-can-split
  (t/is (nil? (motion/split-controls fade-by-word false nil)) "on a layer that is no text, none")
  (t/is (= {:kind :layer} (motion/split-controls fade true nil)) "a whole one has no spread")
  (t/is (= {:kind :word :spread 0.5 :order :forward :stagger 167}
           (motion/split-controls fade-by-word true 3))
        "the ms between two of 3 units")
  (t/is (= {:kind :word :spread 0.5 :order :forward}
           (motion/split-controls fade-by-word true nil))
        "no stagger without a text: the style editor")
  (t/is (true? (:shuffle (motion/split-controls (cta/make-animation (assoc fade-by-word :order :random)) true nil)))
        "a random order shuffles")
  (t/is (true? (:capped (motion/split-controls fade-by-word true 1001))) "past the live cap"))

(t/deftest the-dock-names-the-units
  (t/is (= (tr "workspace.animation.label.split" (tr "workspace.animation.fade-in") (tr "workspace.animation.split.word"))
           (motion/animation-label fade-by-word)))
  (t/is (= (tr "workspace.animation.fade-in") (motion/animation-label fade))))

(defn- commit-of
  "The changes the commit `event` emits for `state` holds, or nil."
  [event state]
  (let [events (atom [])
        _      (->> (ptk/watch event state (rx/empty))
                    (rx/subs! #(swap! events conj %)))
        commit (d/seek #(= :app.main.data.changes/commit-changes (ptk/type %)) @events)
        result (atom nil)]
    (when (some? commit)
      ;; the commit the event schedules holds the changes
      (->> (ptk/watch commit (assoc state :permissions {:can-edit true}) (rx/empty))
           (rx/subs! #(reset! result %)))
      (deref @result))))

(defn- scene
  "A state whose current page holds a board with the shapes `shapes`,
  the top-most last, and `timeline` for it, if any."
  [board shapes timeline]
  (let [file-id (uuid/next)
        page-id (uuid/next)
        bid     (:id board)
        data    {:pages-index {page-id {:id page-id
                                        :objects (into {bid (assoc board :shapes (mapv :id shapes))}
                                                       (map (fn [shape] [(:id shape) (assoc shape :parent-id bid :frame-id bid)]))
                                                       shapes)
                                        :timelines (if (some? timeline) {bid timeline} {})}}}]
    {:data    data
     :page-id page-id
     :state   {:current-file-id file-id
               :current-page-id page-id
               :workspace-animation {:board-id bid :playhead 0}
               :files {file-id {:id file-id :data data}}}}))

(t/deftest the-kind-changes-on-all-splits-of-a-text-in-one-step
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 300 :height 100})
        text    (cts/setup-shape {:type :text :x 10 :y 10 :width 200 :height 20})
        bid     (:id board)
        tid     (:id text)
        tl      (-> (cta/make-timeline {:board-id bid})
                    (cta/add-animation tid {:type :fade :split :word})
                    (cta/add-animation tid {:type :move :split :word})
                    (cta/add-animation tid {:type :scale}))
        {:keys [data page-id state]} (scene board [text] tl)
        fade-id (:id (first (get-in tl [:tracks tid :animations])))
        changes (commit-of (dwa/set-animation-split tid fade-id :char) state)
        kinds   #(mapv :split (get-in % [:pages-index page-id :timelines bid :tracks tid :animations]))
        done    (cfc/process-changes data (:redo-changes changes))]
    (t/is (= [:char :char nil] (kinds done)))
    (t/is (= [:word :word nil] (kinds (cfc/process-changes done (:undo-changes changes))))
          "undone in one step")))

(t/deftest text-presets-go-to-the-texts-only
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 300 :height 200})
        text-a  (cts/setup-shape {:type :text :x 10 :y 10 :width 200 :height 20})
        text-b  (cts/setup-shape {:type :text :x 10 :y 50 :width 200 :height 20})
        rect    (cts/setup-shape {:type :rect :x 10 :y 90 :width 50 :height 50})
        bid     (:id board)
        {:keys [data page-id state]} (scene board [text-b rect text-a] nil)
        changes (commit-of (dwa/add-animation-preset (mapv :id [text-a rect text-b]) :rise-words) state)
        done    (cfc/process-changes data (:redo-changes changes))
        played  #(mapv (juxt :split :start) (get-in done [:pages-index page-id :timelines bid :tracks (:id %) :animations]))]
    (t/is (= [[:word 0] [:word 0]] (played text-a)) "the top-most text first")
    (t/is (= [[:word 100] [:word 100]] (played text-b)) "the next one by the stagger")
    (t/is (= [] (played rect)))))
