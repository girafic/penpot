;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.prototype-preview-test
  "The preview of the prototype in the workspace: the board it starts
  at, the history of its screens in the view mode, the boards the
  workspace sends it to and its zoom."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.viewer :as dv]
   [app.main.data.workspace.prototype-preview :as dwpp]
   [app.main.router :as rt]
   [beicon.v2.core :as rx]
   [cljs.test :as t]
   [potok.v2.core :as ptk]))

(def ^:private board-a (uuid/custom 2 1))
(def ^:private board-b (uuid/custom 2 2))
(def ^:private inner (uuid/custom 2 3))
(def ^:private rect (uuid/custom 2 4))
(def ^:private loose (uuid/custom 2 5))

(def ^:private objects
  {uuid/zero {:id uuid/zero :type :frame :frame-id uuid/zero :parent-id uuid/zero
              :shapes [board-a board-b loose]}
   board-a   {:id board-a :type :frame :frame-id uuid/zero :parent-id uuid/zero :shapes [inner]}
   inner     {:id inner :type :frame :frame-id board-a :parent-id board-a :shapes [rect]}
   rect      {:id rect :type :rect :frame-id inner :parent-id inner}
   board-b   {:id board-b :type :frame :frame-id uuid/zero :parent-id uuid/zero :shapes []}
   loose     {:id loose :type :rect :frame-id uuid/zero :parent-id uuid/zero}})

(t/deftest the-preview-starts-at-the-board-of-the-selection
  (t/is (= board-b (dwpp/start-frame-id objects [board-b])))
  ;; a shape deep in a board: the board of the page it is in
  (t/is (= board-a (dwpp/start-frame-id objects [rect])))
  (t/is (= board-a (dwpp/start-frame-id objects [inner])))
  ;; the first selected shape in a board
  (t/is (= board-b (dwpp/start-frame-id objects [loose board-b])))
  ;; nothing in a board: where the view mode starts
  (t/is (nil? (dwpp/start-frame-id objects [loose])))
  (t/is (nil? (dwpp/start-frame-id objects []))))

(defn- screens
  "The state after the preview showed the screens `indexes`, one after
  the other."
  [state indexes]
  (reduce (fn [state index] (ptk/update (dv/preview-screen index) state)) state indexes))

(t/deftest the-preview-keeps-its-screens-in-a-history-of-its-own
  (let [started (ptk/update (dv/start-preview) {})]
    (t/testing "its screens replace each other in the history of the browser"
      (t/is (true? (::rt/replace-only started)))
      (t/is (dv/preview? started))
      (t/is (not (dv/preview? {}))))

    (t/testing "the screen it leaves goes to its history"
      (let [state (screens started [0 2 2 5])]
        (t/is (= {:back [0 2] :forward [] :index 5}
                 (:viewer-preview state)))))

    (t/testing "back goes to the screen before, which then shows without
                adding to the history, and forth again"
      (let [state (screens started [0 2 5])
            back  (ptk/update (dv/preview-step :back) state)
            _     (t/is (= 2 (get-in back [:viewer-preview :going])))
            back  (screens back [2])
            _     (t/is (= {:back [0] :forward [5] :index 2} (:viewer-preview back)))
            fwd   (screens (ptk/update (dv/preview-step :forward) back) [5])]
        (t/is (= {:back [0 2] :forward [] :index 5} (:viewer-preview fwd)))))

    (t/testing "a new screen after going back drops the screens ahead"
      (let [state (-> (screens started [0 2 5])
                      (as-> s (ptk/update (dv/preview-step :back) s))
                      (screens [2 7]))]
        (t/is (= {:back [0 2] :forward [] :index 7} (:viewer-preview state)))))

    (t/testing "nothing before the first screen"
      (let [state (screens started [0])]
        (t/is (= state (ptk/update (dv/preview-step :back) state)))))

    (t/testing "the view mode outside a preview keeps no history"
      (t/is (= {} (screens {} [0 2]))))))

(def ^:private page-id (uuid/custom 3 1))

(defn- viewer-state
  "The view mode as a preview showing the board at `index` of a page with
  boards A and B (the boards it lists: saved, not hidden), waiting for
  the board `pending` when given (see `dv/preview-go`)."
  [index pending]
  (cond-> {:route {:query-params {:page-id (str page-id) :index (str index)}}
           :viewer {:pages {page-id {:frames [{:id board-a} {:id board-b}]}}}
           :viewer-preview {:back [] :forward [] :index index}}
    (some? pending) (assoc-in [:viewer-preview :pending] pending)))

(defn- emits
  "The events `event` emits on `state`."
  [event state]
  (let [out (atom [])]
    (rx/subs! #(swap! out conj %) (ptk/watch event state (rx/empty)))
    @out))

(t/deftest the-preview-goes-only-to-a-board-the-view-mode-lists
  (let [state (viewer-state 0 nil)]
    (t/testing "a board it lists, there"
      (let [go (dv/preview-go board-b)]
        (t/is (= [::dv/go-to-frame] (map ptk/type (emits go (ptk/update go state)))))
        (t/is (nil? (get-in (ptk/update go (viewer-state 0 inner)) [:viewer-preview :pending]))
              "and it waits for none")))

    (t/testing "the board it shows, nowhere"
      (t/is (empty? (emits (dv/preview-go board-a) state))))

    (t/testing "a board not saved yet, or hidden in view mode: it stays, and waits for it"
      (let [go    (dv/preview-go inner)
            after (ptk/update go state)]
        (t/is (empty? (emits go after)) "not to the first board")
        (t/is (= inner (get-in after [:viewer-preview :pending])))
        (t/is (= loose (get-in (ptk/update (dv/preview-go loose) after) [:viewer-preview :pending]))
              "another takes its place")))

    (t/testing "once a refresh brings it, it goes there"
      (let [refreshed (#'app.main.data.viewer/bundle-refreshed {})]
        (t/is (= [::dv/preview-go] (map ptk/type (emits refreshed (viewer-state 0 board-b)))))
        (t/is (empty? (emits refreshed state)) "waiting for none, nowhere")))))

(t/deftest the-preview-zooms-as-the-workspace-says
  (t/is (identical? dv/zoom-to-fit (dv/preview-zoom "fit")))
  (t/is (identical? dv/zoom-to-fill (dv/preview-zoom "fill")))
  (t/is (identical? dv/reset-zoom (dv/preview-zoom "100")))
  (t/is (nil? (dv/preview-zoom "200")))
  (t/testing "fitted or filled again as it resizes; at its own size it stays"
    (let [at (fn [zoom-type] (assoc-in (viewer-state 0 nil) [:viewer-local :zoom-type] zoom-type))]
      (t/is (= [dv/zoom-to-fit] (emits (dv/refit-preview) (at :fit))))
      (t/is (= [dv/zoom-to-fill] (emits (dv/refit-preview) (at :fill))))
      (t/is (empty? (emits (dv/refit-preview) (ptk/update dv/reset-zoom (at :fit))))))))
