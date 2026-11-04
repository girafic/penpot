;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.data.viewer-test
  (:require
   [app.common.geom.point :as gpt]
   [app.common.uuid :as uuid]
   [app.main.data.viewer :as dv]
   [app.main.router :as rt]
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [cljs.test :as t]
   [potok.v2.core :as ptk]))

(def ^:private page-id
  (uuid/custom 1 1))

(defn- base-state
  "Build a minimal viewer state with the given frames and query-params."
  [{:keys [frames index]}]
  {:route {:params {:query {:page-id (str page-id)
                            :index   (str index)}}}
   :viewer {:pages {page-id {:frames frames}}}
   :viewer-local {:viewport-size {:width 1000 :height 800}}})

(t/deftest zoom-to-fit-clamps-out-of-bounds-index
  (t/testing "index exceeds frame count"
    (let [state (base-state {:frames [{:selrect {:width 100 :height 100}}]
                             :index  1})
          result (ptk/update dv/zoom-to-fit state)]
      (t/is (= (get-in result [:viewer-local :zoom-type]) :fit))
      (t/is (number? (get-in result [:viewer-local :zoom])))))

  (t/testing "index is zero with single frame (normal case)"
    (let [state (base-state {:frames [{:selrect {:width 100 :height 100}}]
                             :index  0})
          result (ptk/update dv/zoom-to-fit state)]
      (t/is (= (get-in result [:viewer-local :zoom-type]) :fit))
      (t/is (number? (get-in result [:viewer-local :zoom])))))

  (t/testing "index within valid range with multiple frames"
    (let [state (base-state {:frames [{:selrect {:width 100 :height 100}}
                                      {:selrect {:width 200 :height 200}}]
                             :index  1})
          result (ptk/update dv/zoom-to-fit state)]
      (t/is (= (get-in result [:viewer-local :zoom-type]) :fit))
      (t/is (number? (get-in result [:viewer-local :zoom]))))))

(t/deftest zoom-to-fill-clamps-out-of-bounds-index
  (t/testing "index exceeds frame count"
    (let [state (base-state {:frames [{:selrect {:width 100 :height 100}}]
                             :index  1})
          result (ptk/update dv/zoom-to-fill state)]
      (t/is (= (get-in result [:viewer-local :zoom-type]) :fill))
      (t/is (number? (get-in result [:viewer-local :zoom])))))

  (t/testing "index is zero with single frame (normal case)"
    (let [state (base-state {:frames [{:selrect {:width 100 :height 100}}]
                             :index  0})
          result (ptk/update dv/zoom-to-fill state)]
      (t/is (= (get-in result [:viewer-local :zoom-type]) :fill))
      (t/is (number? (get-in result [:viewer-local :zoom])))))

  (t/testing "index within valid range with multiple frames"
    (let [state (base-state {:frames [{:selrect {:width 100 :height 100}}
                                      {:selrect {:width 200 :height 200}}]
                             :index  1})
          result (ptk/update dv/zoom-to-fill state)]
      (t/is (= (get-in result [:viewer-local :zoom-type]) :fill))
      (t/is (number? (get-in result [:viewer-local :zoom]))))))

(defn- watch-events
  "Collect the events an event's watch emits synchronously."
  [event state]
  (let [out (atom [])]
    (some-> (ptk/watch event state nil)
            (rx/subscribe #(swap! out conj %)))
    @out))

(defn- zoom-state
  "Build a viewer state with the given `:zoom` query param and zoom type."
  [zoom-param zoom-type]
  {:route {:params {:query (cond-> {:page-id (str page-id) :index "0"}
                             (some? zoom-param)
                             (assoc :zoom zoom-param))}}
   :viewer-local (cond-> {}
                   (some? zoom-type)
                   (assoc :zoom-type zoom-type))})

(t/deftest update-zoom-querystring-does-not-navigate-when-url-already-matches
  (t/testing "zoom type already described by the query string"
    (t/is (empty? (watch-events dv/update-zoom-querystring
                                (zoom-state "fit" :fit)))))

  (t/testing "no zoom type and no zoom query param"
    (t/is (empty? (watch-events dv/update-zoom-querystring
                                (zoom-state nil nil))))))

(t/deftest update-zoom-querystring-navigates-when-zoom-changes
  (t/testing "zoom type differs from the query string"
    (let [events (watch-events dv/update-zoom-querystring
                               (zoom-state "fit" :fill))
          {:keys [id params options]} (some-> (first events) deref)]
      (t/is (= 1 (count events)))
      (t/is (= :viewer id))
      (t/is (= :fill (:zoom params)))
      (t/is (true? (::rt/replace options)))))

  (t/testing "zoom query param absent, other params preserved"
    (let [events (watch-events dv/update-zoom-querystring
                               (zoom-state nil :fit))
          {:keys [params]} (some-> (first events) deref)]
      (t/is (= 1 (count events)))
      (t/is (= :fit (:zoom params)))
      (t/is (= (str page-id) (:page-id params)))
      (t/is (= "0" (:index params)))))

  (t/testing "zoom type cleared drops the query param"
    (let [events (watch-events dv/update-zoom-querystring
                               (zoom-state "fit" nil))
          {:keys [params]} (some-> (first events) deref)]
      (t/is (= 1 (count events)))
      (t/is (not (contains? params :zoom))))))

(t/deftest bundle-fetched-with-zoom-fill-url-does-not-navigate
  ;; Regression test for the React "maximum update depth exceeded"
  ;; error (2.18.0-RC5): loading the viewer with a URL that already
  ;; contains `zoom=fill` re-entered the cycle zoom-to-fill →
  ;; update-zoom-querystring → nav → navigated → zoom-to-fill…,
  ;; because update-zoom-querystring navigated unconditionally.
  ;; At HEAD the guard breaks the cycle, so a bundle fetch must not
  ;; emit any navigation.
  (let [state (-> (base-state {:frames [{:selrect {:width 100 :height 100}}]
                               :index  0})
                  (assoc-in [:route :query-params :zoom] "fill"))]
    (let [events (watch-events (dv/bundle-fetched
                                {:file {:id page-id
                                        :data {:pages [page-id]
                                               :pages-index {page-id {:objects {}}}}}
                                 :project {}
                                 :team {:features []}
                                 :share-links []
                                 :libraries []
                                 :users []
                                 :permissions {}
                                 :thumbnails {}})
                               state)
          ;; `update-page-position-data` and `go-to-frame-auto` are part
          ;; of the normal init sequence; the navigation events are the
          ;; ones that can restart the zoom cycle.
          navigations (filter #(= :app.main.router/navigate (ptk/type %)) events)]
      (t/is (empty? navigations)))))


;; --- Devices

(defn- device-state
  "A view mode showing a board `bw` × `bh` in a viewport `vw` wide, the
  device `choice` picked, with the query params `query`."
  [choice bw bh vw query]
  {:route {:params {:query (merge {:page-id (str page-id) :index "0"} query)}}
   :viewer {:pages {page-id {:frames [{:selrect {:width bw :height bh}}]}}}
   :viewer-local {:viewport-size {:width vw :height 800}
                  :device {:choice choice :finish "dark"}}})

(t/deftest a-device-shows-the-board-as-wide-as-its-screen
  (let [state (device-state "iphone-island" 402 874 201 {})]
    (t/is (= :iphone-island (:kind (dv/device-shown state))))
    (t/is (= 0.5 (get-in (ptk/update dv/zoom-to-device state) [:viewer-local :zoom])))

    (t/testing "the zoom of the menus rests, their zoom type kept"
      (let [fitted (ptk/update dv/zoom-to-fit state)
            filled (ptk/update dv/zoom-to-fill state)]
        (t/is (= [0.5 :fit] (mapv #(get-in fitted [:viewer-local %]) [:zoom :zoom-type])))
        (t/is (= [0.5 :fill] (mapv #(get-in filled [:viewer-local %]) [:zoom :zoom-type])))
        (t/is (= 0.5 (get-in (ptk/update dv/reset-zoom state) [:viewer-local :zoom])))
        (t/is (= state (ptk/update dv/increase-zoom state)))
        (t/is (= state (ptk/update dv/decrease-zoom state)))))

    (t/testing "it refits as the viewport resizes"
      (t/is (= [dv/zoom-to-device] (watch-events dv/refit-zoom state))))))

(t/deftest without-a-device-the-zoom-is-as-it-was
  (let [state (device-state "none" 402 874 201 {})]
    (t/is (nil? (dv/device-shown state)))
    (t/is (= (/ 200 874) (get-in (ptk/update dv/zoom-to-fit
                                             (assoc-in state [:viewer-local :viewport-size :height] 200))
                                 [:viewer-local :zoom]))
          "fitted in a low viewport")
    (t/is (= 1.3 (get-in (ptk/update dv/increase-zoom (assoc-in state [:viewer-local :zoom] 1))
                         [:viewer-local :zoom])))
    (t/is (= [dv/zoom-to-fit]
             (watch-events dv/refit-zoom (assoc-in state [:viewer-local :zoom-type] :fit)))
          "fitted again once the device goes")
    (t/is (empty? (watch-events dv/refit-zoom state)) "at a zoom of its own it stays"))
  (t/is (nil? (dv/device-shown (device-state "auto" 1920 1080 800 {})))
        "automatic leaves a slide alone")
  (t/is (nil? (dv/device-shown (device-state "iphone-island" 402 874 201 {:section "comments"})))
        "only in the Interactions section"))

(t/deftest a-phone-turns-on-its-side-and-back
  (let [state  (device-state "iphone-island" 402 874 402 {})
        turned (ptk/update dv/turn-device state)]
    (t/is (= :landscape (:hold (dv/device turned))))
    (t/is (= [true 874 402] ((juxt :landscape? :screen-w :screen-h) (dv/device-shown turned))))
    (t/is (= (/ 402 874) (get-in (ptk/update dv/zoom-to-device turned) [:viewer-local :zoom]))
          "the board as wide as the screen on its side")
    (t/is (nil? (:hold (dv/device (ptk/update dv/turn-device turned))))
          "turned back, as it is drawn")
    (t/is (nil? (:hold (dv/device (ptk/update (dv/set-device "ipad" "dark") turned))))
          "another device stands as the boards do"))
  (let [state (device-state "watch" 416 496 402 {})]
    (t/is (= state (ptk/update dv/turn-device state)) "a watch does not turn"))
  (let [state (device-state "none" 402 874 402 {})]
    (t/is (= state (ptk/update dv/turn-device state)) "nor does no device")))

(t/deftest picking-a-device-keeps-it-in-the-url
  (let [state  (device-state "none" 402 874 201 {:zoom "fit"})
        ;; the state it leaves and the navigation of the URL it asks
        set-to (fn [choice finish]
                 (let [event (dv/set-device choice finish)
                       state (ptk/update event state)]
                   [state (some-> (watch-events (first (watch-events event state)) state) first deref)]))
        [picked {:keys [id params options]}] (set-to "macbook" "light")]
    (t/is (= {:choice "macbook" :finish "light"} (dv/device picked)))
    (t/is (= :viewer id))
    (t/is (= ["macbook" "light" "fit"] ((juxt :device :device-color :zoom) params)) "other params kept")
    (t/is (true? (::rt/replace options)))
    (t/is (not (contains? (:params (second (set-to "ipad" "dark"))) :device-color))
          "dark is the default")
    (t/is (= {:choice "none" :finish "dark"} (dv/device (first (set-to "toaster" "pink"))))
          "a choice it does not know is none")
    (let [framed (assoc-in state [:route :params :query :device] "ipad")
          event  (dv/set-device "none" "dark")
          after  (ptk/update event framed)
          {:keys [params]} (some-> (watch-events (first (watch-events event after)) after) first deref)]
      (t/is (not (contains? params :device)) "none drops it"))
    (let [same  (assoc-in state [:route :params :query :device] "ipad")
          after (ptk/update (dv/set-device "ipad" "dark") same)]
      (t/is (empty? (watch-events (first (watch-events (dv/set-device "ipad" "dark") after)) after))
            "the URL says it already"))))

(t/deftest the-view-mode-keeps-the-device-for-the-next-time
  (let [before [(get storage/user ::dv/device) (get storage/user ::dv/device-color)]
        state  (device-state "none" 402 874 201 {})]
    (ptk/effect (dv/set-device "watch" "light") state nil)
    (t/is (= ["watch" "light"] [(get storage/user ::dv/device) (get storage/user ::dv/device-color)]))
    (ptk/effect (dv/set-device "ipad" "dark") (assoc state :viewer-preview {}) nil)
    (t/is (= "watch" (get storage/user ::dv/device)) "not the device of the preview")

    (t/testing "it starts with the device of its URL, else the one kept"
      (let [init (fn [query] (dv/device (ptk/update (dv/initialize {:file-id page-id})
                                                    {:route {:params {:query query}}})))]
        (t/is (= {:choice "watch" :finish "light"} (init {})))
        (t/is (= {:choice "ipad" :finish "dark"} (init {:device "ipad"})))
        (t/is (= {:choice "none" :finish "dark"} (init {:preview "true"}))
              "the preview says its own")))
    (swap! storage/user (fn [kept]
                          (reduce (fn [kept [k v]] (if (some? v) (assoc kept k v) (dissoc kept k)))
                                  kept
                                  [[::dv/device (first before)] [::dv/device-color (second before)]])))))

;; --- Scroll of containers (Penpot Motion, see `app.main.data.viewer.scroll`)

(t/deftest the-containers-scroll-for-the-board-shown
  (let [board (uuid/custom 1 2)
        other (uuid/custom 1 3)
        list  (uuid/custom 1 4)
        chips (uuid/custom 1 5)
        s1    (ptk/update (dv/scroll-container board list [0 100] [0 0]) {:viewer-local {:zoom 1}})
        s2    (ptk/update (dv/scroll-container board chips [20 0] [0 -8]) s1)
        s3    (ptk/update (dv/scroll-container other list [0 10] [0 0]) s2)]
    (t/is (= {:board-id board :containers {list {:offset [0 100] :past [0 0]}}}
             (get-in s1 [:viewer-local :scroll])))
    (t/is (= {:offset [20 0] :past [0 -8]} (get-in s2 [:viewer-local :scroll :containers chips])))
    (t/is (= {list {:offset [0 10] :past [0 0]}} (get-in s3 [:viewer-local :scroll :containers]))
          "another board forgets them")
    (t/is (nil? (get-in (ptk/update (dv/reset-container-scroll) s2) [:viewer-local :scroll])))
    (t/is (= {:zoom 1} (dv/steady-local (assoc (:viewer-local s2) :timeline {:time 3})))
          "what changes at each frame is left out of what renders the view mode")))

(t/deftest an-overlay-opens-with-its-containers-at-rest
  (let [board   (uuid/custom 1 2)
        overlay (uuid/custom 1 6)
        inner   (uuid/custom 1 7)
        list    (uuid/custom 1 4)
        frame   {:id overlay :type :frame :parent-id uuid/zero :frame-id uuid/zero :shapes [inner]}
        objects {overlay frame
                 inner   {:id inner :type :frame :parent-id overlay :frame-id overlay :shapes []}}
        state   {:route {:params {:query {:page-id (str page-id)}}
                         :query-params {:page-id (str page-id)}}
                 :viewer {:pages {page-id {:objects objects :all-frames [frame]}}}
                 :viewer-local {:scroll {:board-id board
                                         :containers {inner {:offset [0 50] :past [0 0]}
                                                      list  {:offset [0 100] :past [0 0]}}}}}
        opened  (ptk/update (dv/open-overlay overlay (gpt/point 0 0) nil false false nil false) state)]
    (t/is (= 1 (count (:viewer-overlays opened))))
    (t/is (= {list {:offset [0 100] :past [0 0]}} (get-in opened [:viewer-local :scroll :containers]))
          "the screen's list stays scrolled")))
