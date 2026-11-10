;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-canvas-test
  "The canvas of a phone: when the view mode starts on it, when it shows,
  and the routes in and out of it."
  (:require
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.canvas :as dvc]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(def ^:private file-params
  {:file-id "f" :page-id "p"})

(t/deftest a-link-to-no-board-starts-on-the-canvas
  (t/is (dvc/starts-on-canvas? {:file-id "f"}) "a tap in the dashboard: no page yet")
  (t/is (dvc/starts-on-canvas? file-params))
  (t/is (dvc/starts-on-canvas? (assoc file-params :section "interactions")))
  (t/is (dvc/starts-on-canvas? (assoc file-params :share-id "s" :index "0"))
        "a share link: its dialog always writes the first board")
  (t/is (dvc/starts-on-canvas? (assoc file-params :share-id "s" :index ["2" "0"]))
        "a repeated key counts by its last value"))

(t/deftest a-link-to-a-board-starts-on-the-prototype
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :frame-id "b"))))
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :index "0")))
        "an index without a share link was chosen")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :share-id "s" :index "3"))))
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :follow "true"))) "live")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :preview "true")))
        "the preview of the workspace")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :section "comments"))))
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :section :inspect)))))

(t/deftest a-choice-made-on-the-phone-stays
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :canvas "false" :share-id "s" :index "0")))
        "the prototype chosen: a reload stays on its board")
  (t/is (not (dvc/starts-on-canvas? (assoc file-params :canvas "true")))
        "the canvas already in the route"))

(t/deftest the-canvas-shows-where-it-can-with-no-board
  (let [params (assoc file-params :canvas "true")]
    (t/is (dvc/canvas-shown? true params))
    (t/is (dvc/canvas-shown? true (assoc params :section "interactions")))
    (t/is (not (dvc/canvas-shown? false params)) "not where it cannot: a computer, no WASM")
    (t/is (not (dvc/canvas-shown? nil params)))
    (t/is (not (dvc/canvas-shown? true (assoc params :index "2"))) "a board in the route")
    (t/is (not (dvc/canvas-shown? true (assoc params :section "inspect"))))
    (t/is (not (dvc/canvas-shown? true (assoc file-params :canvas "false"))))
    (t/is (not (dvc/canvas-shown? true file-params)))))

(t/deftest from-the-canvas-to-a-board-and-back
  (let [canvas (dvc/canvas-params (assoc file-params :share-id "s" :index "0" :frame-id "b"
                                         :section "interactions" :zoom "fit"))
        board  (dvc/board-params canvas 3)]
    (t/is (= {:file-id "f" :page-id "p" :share-id "s" :zoom "fit" :canvas "true"} canvas))
    (t/is (dvc/canvas-shown? true canvas))
    (t/is (= (assoc canvas :index 3 :canvas "false") board) "one step, the canvas left")
    (t/is (not (dvc/canvas-shown? true board)))
    (t/is (not (dvc/starts-on-canvas? (update board :index str))) "a reload stays on the board")
    (t/is (= canvas (dvc/canvas-params board)) "back to the canvas")))

(t/deftest a-board-off-the-canvas-keeps-the-route
  (t/is (= (assoc file-params :index 2) (dvc/board-params file-params 2))
        "no canvas in the route: none added")
  (t/is (= (assoc file-params :index 2 :canvas "false")
           (dvc/board-params (assoc file-params :canvas "false") 2))))

;; --- Into the canvas and out of it

(def ^:private page-id (uuid/custom 1 1))
(def ^:private other-page-id (uuid/custom 1 2))
(def ^:private board-ids (mapv #(uuid/custom 2 %) (range 3)))
(def ^:private flow-id (uuid/custom 3 1))
(def ^:private file-id (uuid/custom 4 1))

(defn- viewer-state
  "A view mode at the route `query`, on a `phone` drawing with `wasm` (both
  by default), its page listing three boards and a flow from the third."
  [query & {:keys [phone wasm] :or {phone true wasm true}}]
  {:route {:params {:query query} :query-params query}
   :features (if wasm #{"render-wasm/v1"} #{})
   :viewer-local {:phone? phone}
   :viewer {:pages {page-id {:frames (mapv #(hash-map :id %) board-ids)
                             :flows {flow-id {:id flow-id :name "Flow 1"
                                              :starting-frame (peek board-ids)}}}}}})

(defn- watch-events
  "The events the watch of `event` emits at once (but the nil a computer
  emits for the zoom of a phone)."
  [event state]
  (let [out (atom [])]
    (some-> (ptk/watch event state nil)
            (rx/subscribe #(when (some? %) (swap! out conj %))))
    @out))

(defn- navigation
  "The navigation `event` ends in, through the events its watch emits:
  `{:params :options}`, or nil."
  [event state]
  (if (= :app.main.router/navigate (ptk/type event))
    (deref event)
    (some #(navigation % state) (watch-events event state))))

(def ^:private bundle
  {:file {:id file-id :data {:pages [page-id other-page-id]}}})

(def ^:private page-query
  {:file-id (str file-id) :page-id (str page-id)})

(t/deftest a-tap-in-the-dashboard-of-a-phone-opens-the-canvas
  (let [query {:file-id (str file-id) :section "interactions"}
        {:keys [params options]} (navigation (dv/bundle-fetched bundle) (viewer-state query))]
    (t/is (= {:file-id (str file-id) :page-id page-id :canvas "true"} params)
          "the first page, as a whole")
    (t/is (true? (:app.main.router/replace options)) "no step in the history"))
  (t/testing "on a computer the first page only"
    (let [query {:file-id (str file-id)}]
      (t/is (= (assoc query :page-id page-id)
               (:params (navigation (dv/bundle-fetched bundle) (viewer-state query :phone false))))))))

(t/deftest a-share-link-opens-the-canvas-of-a-phone
  (let [query  (assoc page-query :share-id "s" :index "0")
        events (watch-events (dv/bundle-fetched bundle) (viewer-state query))
        navs   (filter #(= :app.main.router/navigate (ptk/type %)) events)]
    (t/is (= 1 (count navs)))
    (t/is (= {:file-id (str file-id) :page-id (str page-id) :share-id "s" :canvas "true"}
             (:params @(first navs))))
    (t/is (true? (:app.main.router/replace (:options @(first navs)))))
    (t/is (not-any? #{::dv/go-to-frame-by-index ::dv/go-to-frame ::dv/go-to-frame-auto}
                    (map ptk/type events))
          "no board meanwhile"))
  (t/testing "where it cannot, the first board as now"
    (let [query (assoc page-query :share-id "s" :index "0")]
      (doseq [state [(viewer-state query :phone false)
                     (viewer-state query :wasm false)]]
        (t/is (some #{::dv/go-to-frame-by-index}
                    (map ptk/type (watch-events (dv/bundle-fetched bundle) state)))))
      (with-redefs [cf/flags (disj cf/flags :phone-canvas)]
        (t/is (some #{::dv/go-to-frame-by-index}
                    (map ptk/type (watch-events (dv/bundle-fetched bundle) (viewer-state query)))))))))

(t/deftest a-reload-on-the-canvas-stays-on-it
  (let [events (watch-events (dv/bundle-fetched bundle) (viewer-state (assoc page-query :canvas "true")))]
    (t/is (not-any? #{:app.main.router/navigate ::dv/go-to-frame-by-index ::dv/go-to-frame
                      ::dv/go-to-frame-auto}
                    (map ptk/type events)))))

(t/deftest every-way-to-a-board-leaves-the-canvas-in-one-step
  (let [state (viewer-state (assoc page-query :canvas "true"))]
    (t/is (dv/canvas-shown? state))
    (t/is (= (assoc page-query :canvas "false" :index 1)
             (:params (navigation (dv/go-to-frame-by-index 1) state))))
    (t/is (= (assoc page-query :canvas "false" :index 1)
             (:params (navigation (dv/go-to-frame (second board-ids)) state))))
    (t/is (= (assoc page-query :canvas "false" :index 2)
             (:params (navigation (dv/go-to-frame-auto) state)))
          "the start of the first flow")
    (t/is (nil? (:app.main.router/replace (:options (navigation (dv/go-to-frame-by-index 1) state))))
          "a step in the history: the back gesture returns to the canvas")
    (t/is (not (dv/canvas-shown? (viewer-state (assoc page-query :canvas "false" :index "1")))))))

(t/deftest the-canvas-comes-back-from-a-board
  (let [state (viewer-state (assoc page-query :canvas "false" :index "1" :frame-id "x"))
        {:keys [params options]} (navigation dv/show-canvas state)]
    (t/is (= (assoc page-query :canvas "true") params))
    (t/is (nil? (:app.main.router/replace options)))))

(t/deftest another-page-keeps-the-mode
  (t/is (= {:file-id (str file-id) :page-id other-page-id :canvas "true"}
           (:params (navigation (dv/go-to-page other-page-id) (viewer-state (assoc page-query :canvas "true")))))
        "the canvas of the other page")
  (t/is (= {:file-id (str file-id) :page-id other-page-id :canvas "false" :index 0}
           (:params (navigation (dv/go-to-page other-page-id)
                                (viewer-state (assoc page-query :canvas "false" :index "2")))))
        "its first board"))

(t/deftest comments-and-inspect-go-by-board
  (t/is (= (assoc page-query :canvas "false" :index 2 :section :comments)
           (:params (navigation (dv/go-to-section :comments) (viewer-state (assoc page-query :canvas "true")))))
        "from the canvas, the board the page starts at")
  (t/is (= (assoc page-query :index "1" :section :inspect)
           (:params (navigation (dv/go-to-section :inspect) (viewer-state (assoc page-query :index "1")))))
        "from a board, that board"))
