;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.canvas-comments-test
  "The comments on the canvas of a phone: the bubbles of the page at
  their points, which move with the canvas by its CSS variables; a tap
  opens a thread, one's own bubble drops within its board (Penpot
  Motion)."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.data.workspace.comments :as dwcm]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.comments :as cmt]
   [app.main.ui.viewer.canvas-comments :as cvc]
   [app.util.i18n :refer [tr]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [frontend-tests.helpers.mock :as mock]
   [okulary.core :as l]
   [potok.v2.core :as ptk]
   [rumext.v2 :as mf]))

(def ^:private page-id (uuid/custom 1 1))
(def ^:private board-id (uuid/custom 2 1))
(def ^:private me (uuid/custom 3 1))

(def ^:private hidden-id (uuid/custom 2 2))

(defn- board
  [id x & {:as attrs}]
  (let [rect (grc/make-rect x 0 100 100)]
    (merge {:id id :type :frame :parent-id uuid/zero :frame-id uuid/zero
            :x x :y 0 :width 100 :height 100
            :selrect rect :points (grc/rect->points rect)}
           attrs)))

(def ^:private objects
  "A board at 0 0, and one beside it hidden in view mode, both 100 px."
  {uuid/zero {:id uuid/zero :type :frame :shapes [board-id hidden-id]}
   board-id  (board board-id 0)
   hidden-id (board hidden-id 300 :hide-in-viewer true)})

(defn- thread
  [n x y & {:as attrs}]
  (merge {:id (uuid/custom 4 n) :seqn n :page-id page-id :frame-id board-id
          :file-id (uuid/custom 5 1) :project-id (uuid/custom 5 2) :file-name "File"
          :owner-id me :participants #{me} :position (gpt/point x y)
          :content "" :created-at (ct/now) :modified-at (ct/now)
          :count-unread-comments 0 :is-resolved false}
         attrs))

(defn- emit-into
  "A stand-in for `st/emit!` that collects what it emits in the atom
  `out`, at each arity the code calls it with (`mock/stub` forwards no
  variadic call)."
  [out]
  (fn
    ([] nil)
    ([event] (swap! out conj event) nil)
    ([event & events] (swap! out into (cons event events)) nil)))

(defn- rendered
  "What the comments on the canvas show of `threads` at `zoom`, the
  comments state `local`, and what `act` (given the document, the window
  and the atom of that state) then emits: `{:anchors [[x y]...] :emitted
  [type...]}`. The refs are mocked, not the state of the store, which
  other tests replace."
  [threads local zoom & [act]]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [emitted (atom [])
            local*  (l/atom local)]
        (with-redefs [st/emit!             (emit-into emitted)
                      refs/profile         (l/atom {:id me})
                      refs/comment-threads (l/atom (into {} (map (juxt :id identity)) threads))
                      refs/comments-local  local*
                      refs/viewer-data     (l/atom {:pages {page-id {:comment-thread-positions {}}}})
                      refs/viewer-can-edit? (l/atom false)]
          (thd/render! root (mf/element cvc/canvas-comments*
                                        #js {:page {:id page-id :frames [{:id board-id}]}
                                             :objects objects
                                             :zoom zoom
                                             :size {:width 375 :height 812}
                                             :viewOf (constantly {:view {:zoom 1 :x 0 :y 0}
                                                                  :screen {:width 375 :height 812}})}))
          (reset! emitted [])
          (when (fn? act) (act document window local*))
          {:anchors (mapv (fn [^js node]
                            [(js/parseFloat (.getPropertyValue (.-style node) "--anchor-x"))
                             (js/parseFloat (.getPropertyValue (.-style node) "--anchor-y"))])
                          (.querySelectorAll document "[class*=anchor]"))
           :emitted @emitted})))))

(t/deftest the-bubbles-sit-at-their-points
  (let [far-apart [(thread 1 10 20) (thread 2 90 80)]]
    (t/is (= #{[10 20] [90 80]} (set (:anchors (rendered far-apart {} 1))))
          "each in an anchor at its point of the page")
    (t/is (= [[50 50]] (:anchors (rendered far-apart {} 0.05)))
          "far out, close on the screen: one group at their middle")
    (t/is (= [[10 20]] (:anchors (rendered [(thread 1 10 20) (thread 2 90 80 :participants #{})]
                                           {:mode :yours} 1)))
          "as the filters say")))

(t/deftest a-tap-opens-a-thread
  (let [tap! (fn [_ window _]
               (thd/click! window (.querySelector (.-document window) "[class*=floating-preview-avatar]")))]
    (t/is (= [::dcm/open-comment-thread]
             (map ptk/type (:emitted (rendered [(thread 1 10 20)] {} 1 tap!)))))
    (t/is (= [::dcm/close-comment-thread]
             (map ptk/type (:emitted (rendered [(thread 1 10 20)] {:open (uuid/custom 4 1)} 1 tap!))))
          "open, it closes")))

(t/deftest ^:async a-keystroke-in-a-draft-gathers-no-bubbles
  (let [group   dwcm/group-bubbles
        calls   (atom 0)
        local*  (l/atom {:draft {:position (gpt/point 50 50) :page-id page-id :content ""}})
        counted (atom nil)]
    (await
     (mock/with-mocks*
       {dwcm/group-bubbles     (fn [zoom threads]
                                 (swap! calls inc)
                                 (group zoom threads))
        st/emit!               (emit-into (atom []))
        refs/profile           (l/atom {:id me})
        refs/team              (l/atom {:members []})
        refs/comment-threads   (l/atom {(uuid/custom 4 1) (thread 1 10 20)})
        refs/comments-local    local*
        refs/viewer-data       (l/atom {:pages {page-id {:comment-thread-positions {}}}})
        refs/viewer-can-edit?  (l/atom false)}
       (await
        (thd/with-dom
          (fn [{:keys [root]}]
            (thd/render! root (mf/element cvc/canvas-comments*
                                          #js {:page {:id page-id}
                                               :objects objects
                                               :zoom 1
                                               :size {:width 375 :height 812}
                                               :viewOf (constantly {})}))
            (let [before @calls]
              (thd/act #(swap! local* assoc-in [:draft :content] "a"))
              (-> (thd/settle)
                  (.then (fn []
                           (reset! counted {:draft (- @calls before)})
                           (thd/act #(swap! local* assoc :mode :yours))
                           (thd/settle)))
                  (.then (fn [] (swap! counted assoc :filter (- @calls before)))))))))))
    (t/is (pos? @calls) "the bubbles were gathered as the canvas showed")
    (t/is (= 0 (:draft @counted)) "not again as the draft changed")
    (t/is (pos? (:filter @counted)) "again as a filter changed")))

(defn- focused-on-open?
  "Whether the canvas moves as the thread at `x y` of the page opens, the
  canvas at its corner, at zoom 1."
  [x y]
  (let [open! (fn [_ _ local*]
                (thd/act #(swap! local* assoc :open (uuid/custom 4 1))))
        types (map ptk/type (:emitted (rendered [(thread 1 x y)] {} 1 open!)))]
    (boolean (some #{::dv/focus-canvas} types))))

(t/deftest a-thread-opened-out-of-sight-comes-into-view
  (t/is (true? (focused-on-open? 10 600)) "below where a thread docked at the bottom leaves it room")
  (t/is (false? (focused-on-open? 10 20)) "where it shows, it stays"))

(defn- dropped
  "Where one's own bubble at 10 20 (of `attrs`) dropped after a drag of
  `dx dy` px of the screen at zoom 2, as stored."
  [dx dy & {:as attrs}]
  (let [drag! (fn [_ window _]
                (let [node  (.querySelector (.-document window) "[class*=floating-preview-avatar]")
                      fire! (fn [type x y]
                              (thd/act #(.dispatchEvent node (new (.-MouseEvent window) type
                                                                  #js {:bubbles true :clientX x :clientY y}))))]
                  ;; jsdom captures no pointer
                  (set! (.-setPointerCapture node) (fn [_]))
                  (fire! "pointerdown" 100 100)
                  (fire! "pointermove" (+ 100 dx) (+ 100 dy))
                  (fire! "pointerup" (+ 100 dx) (+ 100 dy))))
        moved (->> (:emitted (rendered [(thread 1 10 20 attrs)] {} 2 drag!))
                   (filter #(= ::dcm/update-comment-thread-position-on-viewer (ptk/type %)))
                   (first))]
    (some-> moved (ptk/update {:comment-threads {(uuid/custom 4 1) {}}})
            (get-in [:comment-threads (uuid/custom 4 1) :position]))))

(t/deftest ones-own-bubble-drops-within-its-board
  (t/is (= (gpt/point 25 40) (dropped 30 40)) "where it went, 30 40 px at zoom 2")
  (t/is (= (gpt/point 100 40) (dropped 400 40)) "out of its board, at its edge")
  (t/is (= (gpt/point 210 40) (dropped 400 40 :frame-id uuid/zero))
        "about no board, where it went"))

(defn- posted
  "What a tap on Post of a comment begun at `x y` of the page `draft-page`
  emits on the canvas of the page `page-id`: `{:types [...] :created
  params}`, the params of the thread it creates."
  [draft-page x y]
  (let [post! (fn [document window _]
                (some->> (js/Array.from (.querySelectorAll document "button"))
                         (some #(when (= (tr "labels.post") (.-textContent %)) %))
                         (thd/click! window)))
        draft {:position (gpt/point x y) :page-id draft-page :file-id (uuid/custom 5 1)
               :content "Hi"}]
    (with-redefs [dcm/create-thread-on-viewer
                  (fn [params]
                    (ptk/reify ::dcm/create-thread-on-viewer
                      IDeref
                      (-deref [_] params)))]
      (let [emitted (:emitted (rendered [] {:draft draft} 1 post!))]
        {:types   (mapv ptk/type emitted)
         :created (some #(when (= ::dcm/create-thread-on-viewer (ptk/type %)) @%) emitted)}))))

(t/deftest a-comment-begun-goes-to-the-board-under-it
  (let [{:keys [types created]} (posted page-id 50 50)]
    (t/is (= [::dcm/create-thread-on-viewer ::dcm/close-comment-thread] types))
    (t/is (= {:page-id page-id :file-id (uuid/custom 5 1) :position (gpt/point 50 50)
              :content "Hi" :frame-id board-id}
             created)
          "about the board under it, on the page it was begun on"))
  (t/is (= [::dcm/close-comment-thread] (:types (posted page-id 150 50)))
        "beside the boards: it goes, posted nowhere")
  (t/is (= [::dcm/close-comment-thread] (:types (posted (uuid/custom 1 2) 50 50)))
        "begun on another page: it goes, posted nowhere"))

(defn- held
  "What a finger held at `x y` of the canvas emits."
  [x y]
  (let [out (atom [])]
    (with-redefs [st/emit! (emit-into out)]
      (cvc/start-draft! objects page-id (uuid/custom 5 1) (gpt/point x y)))
    @out))

(t/deftest a-finger-held-on-a-board-begins-a-comment
  (let [[shown closed draft :as events] (held 50 50)]
    (t/is (= [::dcm/update-options ::dcm/close-comment-thread ::dcm/create-draft] (map ptk/type events)))
    (t/is (= {:canvas-shown? true} (:comments-local (ptk/update shown {}))) "the comments shown")
    (t/is (= ::dcm/close-comment-thread (ptk/type closed)) "the thread open closed")
    (t/is (= (gpt/point 50 50) (get-in (ptk/update draft {}) [:comments-local :draft :position]))
          "where the finger held"))
  (t/is (empty? (held 150 50)) "beside the boards, none"))

(defn- listed
  "The threads of `threads` the list of the canvas shows (`#seqn`), and
  what a tap on the first one emits."
  [threads]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [emitted (atom [])]
        (with-redefs [st/emit!             (emit-into emitted)
                      refs/profile         (l/atom {:id me})
                      refs/profiles        (l/atom {})
                      refs/comment-threads (l/atom (into {} (map (juxt :id identity)) threads))
                      refs/comments-local  (l/atom {:canvas-shown? true :show-sidebar? true})
                      refs/viewer-data     (l/atom {:pages {page-id {:comment-thread-positions {}}}})]
          (thd/render! root (mf/element cvc/canvas-comments-list*
                                        #js {:users {} :page {:id page-id :objects objects}}))
          (let [items (js/Array.from (.querySelectorAll document "[class*=location-text]"))
                shown (mapv #(re-find #"#\d+" (.-textContent %)) items)]
            (some->> (first items) (thd/click! window))
            {:shown shown :emitted (mapv ptk/type @emitted)}))))))

(t/deftest the-list-shows-what-the-canvas-shows
  (let [{:keys [shown emitted]} (listed [(thread 1 10 20) (thread 2 310 20 :frame-id hidden-id)])]
    (t/is (= ["#1"] shown) "none on a board hidden in view mode")
    (t/is (= [::dcm/open-comment-thread] emitted) "a tap opens the thread")))

(t/deftest a-thread-opened-from-the-list-closes-it
  (let [open!  (fn [_ _ local*]
                 (thd/act #(swap! local* assoc :open (uuid/custom 4 1))))
        closes (->> (:emitted (rendered [(thread 1 10 20)] {:canvas-shown? true :show-sidebar? true} 1 open!))
                    (filter #(= ::dcm/update-options (ptk/type %)))
                    (map #(:comments-local (ptk/update % {}))))]
    (t/is (= [{:show-sidebar? false}] closes))))

(defn- tapped-time
  "What opens as the timestamp of 2.5 s in the text of the open thread at
  10 20 about `frame-id` is tapped (the deref of `dv/open-thread-moment`),
  or nil: no timestamp to tap."
  [frame-id]
  (let [cid     (uuid/custom 6 1)
        comment {:id cid :thread-id (uuid/custom 4 1) :owner-id me
                 :content (str "See " (dcm/timestamp-token 2500 "0:02.5"))
                 :created-at (ct/now) :modified-at (ct/now)}
        tap!    (fn [document window _]
                  (some->> (.querySelector document "[class*=comment-timestamp-button]")
                           (thd/click! window)))]
    (with-redefs [cmt/make-comments-ref (fn [_] (l/atom {cid comment}))]
      (some->> (:emitted (rendered [(thread 1 10 20 :frame-id frame-id)] {:open (uuid/custom 4 1)} 1 tap!))
               (filter #(= ::dv/open-thread-moment (ptk/type %)))
               (first)
               (deref)))))

(t/deftest a-timestamp-in-a-comment-opens-the-prototype-at-it
  (t/is (= {:thread-id (uuid/custom 4 1) :time 2500} (tapped-time board-id)))
  (t/is (nil? (tapped-time uuid/zero)) "about no board: a timestamp to read, not to tap"))
