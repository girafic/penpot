;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-canvas-test
  "The canvas of a phone: when the view mode starts on it, when it shows,
  and the routes in and out of it."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.math :as mth]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.canvas :as dvc]
   [app.main.repo :as rp]
   [app.main.ui.viewer.canvas :as uvc]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.mock :as mock]
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

(t/deftest another-page-shows-without-fetching-the-file-again
  (let [loaded (fn [query] (assoc-in (viewer-state query) [:viewer :file :id] file-id))
        types  (fn [state] (map ptk/type (watch-events (dv/show-page) state)))]
    (t/testing "on the canvas"
      (let [shown (types (loaded (assoc page-query :canvas "true")))]
        (t/is (some #{::dv/update-page-position-data} shown) "its texts placed")
        (t/is (not-any? #{::dv/fetch-bundle ::dv/bundle-fetched :app.main.router/navigate} shown)
              "nothing fetched, nowhere else")))
    (t/is (some #{::dv/go-to-frame-by-index} (types (loaded (assoc page-query :index "0"))))
          "in the prototype, its first board")
    (t/is (empty? (types (viewer-state page-query)))
          "nothing while another file loads, which its bundle starts")))

(t/deftest the-comments-of-the-page-shown
  (let [profile {:id (uuid/custom 9 1)}
        loaded  (fn [query] (-> (viewer-state query)
                                (assoc-in [:viewer :file :id] file-id)
                                (assoc :profile profile)))
        fetches (fn [event state]
                  (filterv #(= ::dv/fetch-comment-threads (ptk/type %)) (watch-events event state)))
        threads [{:id (uuid/custom 8 1) :page-id page-id}
                 {:id (uuid/custom 8 2) :page-id other-page-id}]]
    (t/testing "another page fetches its threads, and keeps them only"
      (let [[fetch :as all] (fetches (dv/show-page) (loaded (assoc page-query :canvas "true")))
            kept            (atom nil)]
        (t/is (= 1 (count all)))
        (with-redefs [rp/cmd! (mock/stub (fn [& _] (rx/of threads)))]
          (some-> (ptk/watch fetch {} nil)
                  (rx/subscribe #(reset! kept (% {})))))
        (t/is (= #{(uuid/custom 8 1)} (set (keys (:comment-threads @kept))))
              "those of the page shown")))
    (t/is (empty? (fetches (dv/show-page) (dissoc (loaded page-query) :profile)))
          "logged out, none")
    (t/is (empty? (fetches (dv/initialize {:file-id file-id}) (loaded {:file-id (str file-id)})))
          "a link without a page: once it has one, by `show-page`")
    (t/is (= 1 (count (fetches (dv/initialize {:file-id file-id :page-id page-id}) (loaded page-query))))
          "a link to a page, as it starts")))

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

;; --- What the canvas draws, and its view

(defn- shape
  [id type x y w h & {:as attrs}]
  (let [rect (grc/make-rect x y w h)]
    (merge {:id id :type type :x x :y y :width w :height h
            :selrect rect :points (grc/rect->points rect)
            :parent-id uuid/zero :frame-id uuid/zero}
           attrs)))

(def ^:private ids
  (zipmap [:strip :card :inner-hidden :screen :row :tall :hidden :dot]
          (map #(uuid/custom 5 %) (range))))

(def ^:private page-objects
  "A page as the Playground and an app prototype: a wide lesson strip
  with a card in it, a screen with a row in it, a tall board below, and
  a top-level board hidden in view mode."
  (let [{:keys [strip card inner-hidden screen row tall hidden dot]} ids]
    {uuid/zero    {:id uuid/zero :type :frame :shapes [strip screen tall hidden]}
     strip        (shape strip :frame 0 0 4520 1000 :shapes [card inner-hidden])
     card         (shape card :frame 100 100 400 300 :parent-id strip :frame-id strip)
     inner-hidden (shape inner-hidden :frame 1000 100 400 300 :parent-id strip :frame-id strip
                         :hide-in-viewer true)
     screen       (shape screen :frame 0 1200 400 800 :shapes [row])
     row          (shape row :frame 20 1300 360 60 :parent-id screen :frame-id screen)
     tall         (shape tall :frame 0 3000 4520 18760)
     hidden       (shape hidden :frame 5000 0 400 400 :hide-in-viewer true :shapes [dot])
     dot          (shape dot :rect 5010 10 10 10 :parent-id hidden :frame-id hidden)}))

(def ^:private phone-screen {:width 375 :height 812})

(defn- close-point?
  [a b]
  (and (mth/close? (:x a) (:x b) 1e-6) (mth/close? (:y a) (:y b) 1e-6)))

(defn- screen-point
  "The point of the screen over the point of the page `x y` at `view`."
  [{:keys [zoom] :as view} x y]
  (gpt/point (* (- x (:x view)) zoom) (* (- y (:y view)) zoom)))

(t/deftest the-canvas-leaves-out-only-top-level-boards-hidden-in-view-mode
  (let [objects (dvc/canvas-objects page-objects)]
    (t/is (not-any? #(contains? objects (get ids %)) [:hidden :dot]) "with what they hold")
    (t/is (= (mapv ids [:strip :screen :tall]) (get-in objects [uuid/zero :shapes])))
    (t/is (contains? objects (:inner-hidden ids)) "a hidden board inside another is part of it")
    (t/is (identical? objects (dvc/canvas-objects objects)) "nothing to leave out: as it is")))

(t/deftest the-content-of-a-page
  (t/is (= (grc/make-rect 0 0 4520 21760) (dvc/content-bounds (dvc/canvas-objects page-objects))))
  (t/is (nil? (dvc/content-bounds {uuid/zero {:id uuid/zero :type :frame :shapes []}}))
        "an empty page")
  (let [id (uuid/custom 6 1)]
    (t/is (= (grc/make-rect 10 20 30 40)
             (dvc/content-bounds {uuid/zero {:id uuid/zero :type :frame :shapes [id]}
                                  id (shape id :rect 10 20 30 40)}))
          "shapes and no board")))

(t/deftest the-canvas-starts-as-wide-as-the-screen
  (let [bounds (dvc/content-bounds (dvc/canvas-objects page-objects))
        {:keys [zoom x y]} (dvc/initial-view bounds phone-screen)]
    (t/is (mth/close? (/ 375 4520) zoom 1e-9))
    (t/is (mth/close? 0.083 zoom 1e-3))
    (t/is (= 0 x) "from its left edge")
    (t/is (mth/close? (- (/ 24 zoom)) y 1e-6) "the names of the top boards above it")))

(t/deftest a-small-content-starts-as-wide-as-the-screen
  (let [bounds (grc/make-rect 0 0 50 50)
        [lo hi] (dvc/zoom-range bounds phone-screen)
        {:keys [zoom x y]} (dvc/initial-view bounds phone-screen)]
    (t/is (< lo hi) "the zooms are never empty")
    (t/is (= [3.75 7.5] [lo hi]))
    (t/is (= 7.5 zoom))
    (t/is (= 0 x) "as wide as the screen")
    (t/is (mth/close? (- 50 (/ 812 7.5 2)) y 1e-9) "its bottom at the middle of the screen, as far as it goes")
    (t/is (= 3.75 (:zoom (dvc/clamp-view {:zoom 1 :x 0 :y 0} bounds phone-screen))))))

(t/deftest the-view-moves-past-the-content-half-a-screen
  (let [bounds (grc/make-rect 0 0 4520 21760)
        start  (/ 375 4520)]
    (t/is (= dvc/max-zoom (:zoom (dvc/clamp-view {:zoom 1000 :x 0 :y 0} bounds phone-screen))))
    (t/is (mth/close? (/ start 2) (:zoom (dvc/clamp-view {:zoom 1e-6 :x 0 :y 0} bounds phone-screen)) 1e-9))
    (let [{:keys [x y]} (dvc/clamp-view {:zoom 1 :x 1e9 :y -1e9} bounds phone-screen)]
      (t/is (= (- 4520 (/ 375 2)) x) "its right edge at the middle of the screen")
      (t/is (= (- (/ 812 2)) y) "its top at the middle"))
    (t/testing "at the start view, sideways too"
      (let [view (dvc/initial-view bounds phone-screen)]
        (t/is (mth/close? (- (/ 4520 2)) (:x (dvc/clamp-view (assoc view :x -1e9) bounds phone-screen)) 1e-6))
        (t/is (mth/close? (/ 4520 2) (:x (dvc/clamp-view (assoc view :x 1e9) bounds phone-screen)) 1e-6))))))

(t/deftest a-zoom-keeps-the-point-under-the-fingers
  (let [view  {:zoom 0.1 :x 300 :y 2000}
        point (gpt/point 100 200)]
    (t/is (close-point? (dvc/page-point view point)
                        (dvc/page-point (dvc/zoom-about view point 0.35) point)))))

(t/deftest a-turn-keeps-the-middle-of-the-screen
  (let [bounds (grc/make-rect 0 0 4520 21760)
        wide   {:width 812 :height 375}
        view   {:zoom 1 :x 2000 :y 10000}
        turned (dvc/resize-view view phone-screen wide bounds)]
    (t/is (= 1 (:zoom turned)))
    (t/is (close-point? (dvc/page-point view (gpt/point 187.5 406))
                        (dvc/page-point turned (gpt/point 406 187.5))))))

(t/deftest a-double-tap-zooms-to-a-board-and-back
  (let [objects (dvc/canvas-objects page-objects)
        bounds  (dvc/content-bounds objects)
        start   (dvc/initial-view bounds phone-screen)
        tap     #(dvc/double-tap-view %1 objects bounds phone-screen (screen-point %1 %2 %3))]
    (t/is (= {:zoom (/ 375 400) :x 100 :y 100} (tap start 300 250))
          "the card in the strip, as wide as the screen")
    (t/is (= {:zoom (/ 375 400) :x 0 :y 1200} (tap start 200 1330))
          "the screen, not the row in it")
    (t/is (= start (tap (tap start 300 250) 300 250)) "and back to the start")
    (let [{:keys [zoom] :as view} (tap start 2000 10000)]
      (t/is (mth/close? (* 2 (:zoom start)) zoom 1e-9) "no board to zoom to: twice as close")
      (t/is (close-point? (gpt/point 2000 10000)
                          (dvc/page-point view (screen-point start 2000 10000)))))))

;; --- Its boards and its comments

(t/deftest the-directory-lists-the-top-level-boards
  (let [{:keys [strip card screen tall hidden]} ids
        frames (mapv #(hash-map :id %) [tall screen card strip hidden])]
    (t/is (= [tall screen strip]
             (mapv :id (dvc/top-boards (dvc/canvas-objects page-objects) frames)))
          "in the order of the view mode, neither nested nor hidden")))

(t/deftest a-board-of-the-directory-shows-as-wide-as-the-screen
  (let [objects (dvc/canvas-objects page-objects)
        bounds  (dvc/content-bounds objects)
        {:keys [zoom x y]} (dvc/board-view (get objects (:screen ids)) bounds phone-screen)]
    (t/is (mth/close? (/ 375 400) zoom 1e-9))
    (t/is (= 0 x))
    (t/is (mth/close? (- 1200 (/ 24 zoom)) y 1e-9) "its name above it")))

(t/deftest a-comment-is-about-the-top-level-board-under-it
  (let [{:keys [strip screen]} ids
        objects (dvc/canvas-objects page-objects)]
    (t/is (= strip (:id (dvc/board-at objects (gpt/point 300 250)))) "the strip, not its card")
    (t/is (= screen (:id (dvc/board-at objects (gpt/point 200 1330)))))
    (t/is (nil? (dvc/board-at objects (gpt/point 4600 100))) "beside the boards, none")
    (t/is (nil? (dvc/board-at objects (gpt/point 5010 10))) "a board hidden in view mode, none")))

(t/deftest the-canvas-shows-the-threads-of-its-boards
  (let [{:keys [strip screen hidden]} ids
        [a b c d] (map #(uuid/custom 7 %) (range 4))
        threads   [{:id a :frame-id strip :position (gpt/point 10 10)}
                   {:id b :frame-id hidden :position (gpt/point 5010 10)}
                   {:id c :frame-id uuid/zero :position (gpt/point 4600 10)}
                   {:id d :frame-id strip :position (gpt/point 20 20)}]
        positions {d {:position (gpt/point 30 1300) :frame-id screen}}
        shown     (dvc/canvas-threads threads positions (dvc/canvas-objects page-objects))]
    (t/is (= [a c d] (mapv :id shown)) "but on a board hidden in view mode")
    (t/is (= {:position (gpt/point 30 1300) :frame-id screen}
             (select-keys (last shown) [:position :frame-id]))
          "where it was moved to")))

(t/deftest a-bubble-shows-above-its-open-thread
  (let [view {:zoom 1 :x 0 :y 0}]
    (t/is (dvc/bubble-shown? view phone-screen (gpt/point 100 100)))
    (t/is (not (dvc/bubble-shown? view phone-screen (gpt/point 100 400)))
          "under the thread docked at the bottom")
    (t/is (not (dvc/bubble-shown? view phone-screen (gpt/point -5 10))) "beside the screen")))

(t/deftest the-canvas-goes-to-a-bubble
  (let [objects (dvc/canvas-objects page-objects)
        bounds  (dvc/content-bounds objects)
        start   (dvc/initial-view bounds phone-screen)
        board   (get objects (:screen ids))
        point   (gpt/point 200 1500)
        view    (dvc/bubble-view start bounds phone-screen point board)]
    (t/is (mth/close? (/ 375 400) (:zoom view) 1e-9) "as close as its board as wide as the screen")
    (t/is (close-point? (gpt/point 187.5 203) (screen-point view 200 1500))
          "in the middle, a quarter down")
    (let [close {:zoom 3 :x 0 :y 1300}]
      (t/is (= 3 (:zoom (dvc/bubble-view close bounds phone-screen point board)))
            "closer already: as close"))
    (t/is (= (:zoom start) (:zoom (dvc/bubble-view start bounds phone-screen point nil)))
          "about no board: the zoom it has")))

(t/deftest who-comments
  (t/is (true? (dv/can-comment? {:can-edit true})))
  (t/is (true? (dv/can-comment? {:is-logged true :who-comment "all"})))
  (t/is (false? (dv/can-comment? {:is-logged true :who-comment "team"})))
  (t/is (false? (dv/can-comment? {:is-logged false :who-comment "all"}))))

;; --- Gestures on the canvas

(t/deftest fingers-move-the-page-with-them
  (let [view {:zoom 0.5 :x 100 :y 200}]
    (t/is (= {:zoom 0.5 :x 80 :y 220} (dvc/pan-view view 10 -10)))
    (t/is (close-point? (dvc/page-point view (gpt/point 50 50))
                        (dvc/page-point (dvc/pan-view view 10 -10) (gpt/point 60 40))))))

(t/deftest how-far-a-view-is-past-the-edges
  (let [bounds (grc/make-rect 0 0 4520 21760)]
    (t/is (= [0 0] (dvc/overshoot {:zoom 1 :x 100 :y 100} bounds phone-screen)))
    (t/is (= [0 0] (dvc/overshoot {:zoom 0.5 :x -100 :y 0} bounds phone-screen)) "within half a screen")
    (t/is (= [-50 0] (dvc/overshoot {:zoom 0.5 :x -475 :y 0} bounds phone-screen)) "past the left, in px")
    (t/is (= [0 30] (dvc/overshoot {:zoom 1 :x 0 :y (+ (- 21760 406) 30)} bounds phone-screen))
          "past the bottom")))

(t/deftest the-speed-fingers-let-go-at
  (t/is (= [0.5 -1] (dvc/finger-speed [[0 0 0] [50 25 -50] [100 50 -100]] 100)))
  (t/is (= [0.5 -1] (dvc/finger-speed [[-500 900 900] [20 10 -20] [100 50 -100]] 100))
        "only the last 100 ms")
  (t/is (= [0 0] (dvc/finger-speed [[0 0 0] [100 50 -100]] 300)) "held still before letting go")
  (t/is (= [0 0] (dvc/finger-speed [[0 0 0]] 0))))

(t/deftest a-double-tap-zooms-smoothly
  (let [from {:zoom 0.1 :x 0 :y 0}
        to   {:zoom 0.9 :x 100 :y 100}]
    (let [{:keys [zoom x y]} (dvc/tween-view from to 0)]
      (t/is (mth/close? 0.1 zoom 1e-9))
      (t/is (close-point? (gpt/point 0 0) (gpt/point x y))))
    (let [{:keys [zoom x y]} (dvc/tween-view from to 1)]
      (t/is (mth/close? 0.9 zoom 1e-9))
      (t/is (mth/close? 100 x 1e-6))
      (t/is (mth/close? 100 y 1e-6)))
    (t/testing "one point of the screen stays over the same point of the page"
      (let [k     (- (/ 1 0.1) (/ 1 0.9))
            point (gpt/point (/ 100 k) (/ 100 k))]
        (t/is (close-point? (dvc/page-point from point)
                            (dvc/page-point (dvc/tween-view from to 0.4) point)))))
    (t/is (= {:zoom 1 :x 50 :y 25} (dvc/tween-view {:zoom 1 :x 0 :y 0} {:zoom 1 :x 100 :y 50} 0.5))
          "at one zoom, straight")))

(t/deftest two-taps-soon-and-near-are-a-double-tap
  (let [tap {:t 1000 :x 100 :y 100}]
    (t/is (dvc/double-tap? tap {:t 1200 :x 110 :y 105}))
    (t/is (not (dvc/double-tap? tap {:t 1350 :x 100 :y 100})) "too late")
    (t/is (not (dvc/double-tap? tap {:t 1100 :x 150 :y 100})) "too far")
    (t/is (not (dvc/double-tap? nil tap)))))

(t/deftest past-an-edge-the-page-gives-as-rubber
  (let [bounds (grc/make-rect 0 0 4520 21760)
        inside {:zoom 1 :x 100 :y 100}]
    (t/is (= inside (uvc/shown-view inside bounds phone-screen)) "within the edges, as it is")
    (let [{:keys [y]} (uvc/shown-view {:zoom 1 :x 100 :y -606} bounds phone-screen)]
      (t/is (< -606 y -406) "dragged 200 px past the top, it gives less")
      (t/is (< y (:y (uvc/shown-view {:zoom 1 :x 100 :y -506} bounds phone-screen)))
            "and more the further"))))

(t/deftest a-finger-held-still-begins-a-comment
  (let [down {:pointers {1 (gpt/point 10 10)} :moved false :down-t 1000}]
    (t/is (true? (dvc/holding? down (+ 1000 dvc/long-press-ms))) "alone, still, long enough")
    (t/is (false? (dvc/holding? down (+ 999 dvc/long-press-ms))) "lifted or not yet")
    (t/is (false? (dvc/holding? (assoc down :moved true) 2000)) "it moved the canvas")
    (t/is (false? (dvc/holding? (assoc-in down [:pointers 2] (gpt/point 50 50)) 2000)) "two fingers pinch")
    (t/is (false? (dvc/holding? (assoc down :pointers {}) 2000)) "none down")))

;; --- The color behind it

(t/deftest the-canvas-has-the-color-of-the-canvas-of-the-workspace
  (t/is (= "#1A1C24" (dvc/background {:background "#1A1C24"})) "the one its canvas is given")
  (t/is (= "#E8E9EA" (dvc/background {})) "else the default one there"))

