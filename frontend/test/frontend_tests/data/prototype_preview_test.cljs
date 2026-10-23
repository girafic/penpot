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
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.async :as async]
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

(defn- ^:async emits
  "The events `event` emits on `state`, once its watch ends."
  [event state]
  (let [out (atom [])]
    (await (async/observe (ptk/watch event state (rx/empty)) :on-next #(swap! out conj %)))
    @out))

(t/deftest ^:async the-preview-goes-only-to-a-board-the-view-mode-lists
  (let [state   (viewer-state 0 nil)
        go-b    (dv/preview-go board-b)
        go-a    (dv/preview-go board-a)
        go-in   (dv/preview-go inner)
        waiting (ptk/update go-in state)
        refresh (#'app.main.data.viewer/bundle-refreshed {})
        to-b    (await (emits go-b (ptk/update go-b state)))
        to-a    (await (emits go-a state))
        to-in   (await (emits go-in waiting))
        brought (await (emits refresh (viewer-state 0 board-b)))
        none    (await (emits refresh state))]
    (t/testing "a board it lists, there"
      (t/is (= [::dv/go-to-frame] (map ptk/type to-b)))
      (t/is (nil? (get-in (ptk/update go-b (viewer-state 0 inner)) [:viewer-preview :pending]))
            "and it waits for none"))

    (t/testing "the board it shows, nowhere"
      (t/is (empty? to-a)))

    (t/testing "a board not saved yet, or hidden in view mode: it stays, and waits for it"
      (t/is (empty? to-in) "not to the first board")
      (t/is (= inner (get-in waiting [:viewer-preview :pending])))
      (t/is (= loose (get-in (ptk/update (dv/preview-go loose) waiting) [:viewer-preview :pending]))
            "another takes its place"))

    (t/testing "once a refresh brings it, it goes there"
      (t/is (= [::dv/preview-go] (map ptk/type brought)))
      (t/is (empty? none) "waiting for none, nowhere"))))

(t/deftest ^:async the-preview-zooms-as-the-workspace-says
  (let [at     (fn [zoom-type] (assoc-in (viewer-state 0 nil) [:viewer-local :zoom-type] zoom-type))
        fitted (await (emits (dv/refit-preview) (at :fit)))
        filled (await (emits (dv/refit-preview) (at :fill)))
        as-is  (await (emits (dv/refit-preview) (ptk/update dv/reset-zoom (at :fit))))]
    (t/is (identical? dv/zoom-to-fit (dv/preview-zoom "fit")))
    (t/is (identical? dv/zoom-to-fill (dv/preview-zoom "fill")))
    (t/is (identical? dv/reset-zoom (dv/preview-zoom "100")))
    (t/is (nil? (dv/preview-zoom "200")))
    (t/testing "fitted or filled again as it resizes; at its own size it stays"
      (t/is (= [dv/zoom-to-fit] fitted))
      (t/is (= [dv/zoom-to-fill] filled))
      (t/is (empty? as-is)))))

(t/deftest the-pin-sends-each-board-once
  (let [page   (uuid/custom 3 2)
        other  (uuid/custom 3 3)
        step   #(dwpp/pin-step (merge {:board board-b :requested board-a :current-page page :page page
                                       :ready? true :listening? true}
                                      %))]
    (t/is (= :go (step {})) "the view mode listens: there")
    (t/is (nil? (step {:requested board-b})) "the board it asked for already: nothing")
    (t/is (nil? (step {:board nil})) "nothing selected in a board: nothing")
    (t/is (= :open (step {:current-page other})) "another page: it starts again there")
    (t/is (= :start (step {:ready? false :listening? false})) "no view mode yet: it starts there")
    (t/is (= :hold (step {:listening? false})) "the view mode loads: once it listens")))

(t/deftest a-view-mode-that-listens-gets-what-was-held
  (t/is (= [["penpot:preview-zoom" {:mode "fit"}]]
           (dwpp/ready-messages {:zoom "fit" :unsent nil :follow? true}))
        "its zoom, always")
  (t/is (= [["penpot:preview-zoom" {:mode "100"}]
            ["penpot:preview-go" {:frameId (str board-b)}]]
           (dwpp/ready-messages {:zoom "100" :unsent board-b :follow? true}))
        "and the board held for it")
  (t/is (= [["penpot:preview-zoom" {:mode "fill"}]]
           (dwpp/ready-messages {:zoom "fill" :unsent board-b :follow? false}))
        "not unpinned: a restart starts where it is"))

(t/deftest ^:async the-preview-starts-where-the-view-mode-starts-without-its-board
  (let [route   (fn [state frame-id] (assoc-in state [:route :query-params :frame-id] (str frame-id)))
        fetched (dv/bundle-fetched {})
        types   #(into #{} (comp (filter some?) (map ptk/type)) %)
        hidden  (await (emits fetched (route (viewer-state 0 nil) inner)))
        listed  (await (emits fetched (route (viewer-state 0 nil) board-b)))
        viewer  (await (emits fetched (route (dissoc (viewer-state 0 nil) :viewer-preview) inner)))]
    (t/testing "a board it does not list: kept for a refresh, and the start of the view mode"
      (t/is (contains? (types hidden) ::dv/preview-go))
      (t/is (contains? (types hidden) ::dv/go-to-frame-auto))
      (t/is (not (contains? (types hidden) ::dv/go-to-frame)) "not the first board by fallback"))
    (t/testing "a board it lists: there"
      (t/is (contains? (types listed) ::dv/go-to-frame))
      (t/is (not (contains? (types listed) ::dv/preview-go))))
    (t/testing "the view mode outside a preview goes as it did"
      (t/is (contains? (types viewer) ::dv/go-to-frame)))))

(t/deftest the-play-button-keeps-what-it-opens
  (let [before (get @storage/user ::dwpp/play-mode)]
    (try
      (swap! storage/user dissoc ::dwpp/play-mode)
      (t/is (= :view-mode (dwpp/play-mode)) "the view mode until a pick")
      (dwpp/set-play-mode! :preview)
      (t/is (= :preview (dwpp/play-mode)) "the preview once picked")
      (dwpp/set-play-mode! :present)
      (t/is (= :preview (dwpp/play-mode)) "an unknown mode is not kept")
      (swap! storage/user assoc ::dwpp/play-mode "preview")
      (t/is (= :view-mode (dwpp/play-mode)) "nor read back")
      (finally
        (if (some? before)
          (swap! storage/user assoc ::dwpp/play-mode before)
          (swap! storage/user dissoc ::dwpp/play-mode))))))
