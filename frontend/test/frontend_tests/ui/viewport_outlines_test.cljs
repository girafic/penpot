;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.viewport-outlines-test
  "The shapes the outlines of the WASM viewport draw (see
  `viewport.utils/outline-objects`)."
  (:require
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.logic.timelines :as cltl]
   [app.common.types.animation :as cta]
   [app.common.types.animation-outline :as ctao]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.main.ui.workspace.viewport.utils :as vpu]
   [cljs.test :as t :include-macros true]))

(defn- polygon
  [points]
  (path/from-plain
   (-> [{:command :move-to :params (zipmap [:x :y] (first points))}]
       (into (map (fn [p] {:command :line-to :params (zipmap [:x :y] p)})) (rest points))
       (conj {:command :close-path :params {}}))))

(defn- morphing
  "A board with a triangle that morphs into a flat box over 1 s:
  `[objects timelines id]`."
  []
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 400 :height 300})
        shape (cts/setup-shape {:type :path :content (polygon [[20 30] [60 30] [20 70]])})
        bid   (:id board)
        id    (:id shape)
        shape (assoc shape :parent-id bid :frame-id bid)
        tl    (-> (cta/make-timeline {:board-id bid :duration 1000})
                  (cta/set-outline-at shape 0 (ctao/outline-of shape))
                  (cta/set-outline-at shape 1000 (polygon [[0 0] [40 0] [40 20] [0 20]])))]
    [{bid (assoc board :shapes [id]) id shape} {bid tl} id]))

(t/deftest a-morphing-path-is-outlined-as-it-is-shown
  (let [[objects timelines id] (morphing)
        shown    (delay (cltl/shown-shapes timelines objects 1000))
        outlined (vpu/outline-objects objects {} (vpu/outline-ids objects #{id}) shown)]
    (t/is (= (str (:content (get @shown id))) (str (get-in outlined [id :content])))
          "the outline it has at the playhead")
    (t/is (not= (str (get-in objects [id :content])) (str (get-in outlined [id :content])))
          "not the one at rest")))

(t/deftest outlines-take-only-what-they-need
  (let [[objects _ id] (morphing)
        forced (atom false)
        lazy   (delay (reset! forced true) {})]
    (t/is (= objects (vpu/outline-objects objects {} #{} lazy)))
    (t/is (false? @forced) "nothing worked out without shapes to outline")
    (t/is (= objects (vpu/outline-objects objects {} (vpu/outline-ids objects #{id}) nil)) "outside motion mode, as they are")
    (let [moved (vpu/outline-objects objects {id (gmt/translate-matrix (gpt/point 10 0))} (vpu/outline-ids objects #{id}) nil)]
      (t/is (= (+ 10 (get-in objects [id :selrect :x])) (get-in moved [id :selrect :x]))
            "moved as a drag in WASM moves them"))))

(t/deftest the-outlines-take-the-boards-their-shapes-are-in
  (let [[objects _ id] (morphing)
        bid            (get-in objects [id :parent-id])]
    (t/is (contains? (vpu/outline-ids objects #{id}) bid) "whose tilt tilts them")
    (t/is (contains? (vpu/outline-ids objects #{id}) id))
    (t/is (= #{} (vpu/outline-ids objects #{})))))
