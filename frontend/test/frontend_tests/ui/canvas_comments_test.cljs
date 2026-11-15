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

(def ^:private objects
  (let [rect (grc/make-rect 0 0 100 100)]
    {uuid/zero {:id uuid/zero :type :frame :shapes [board-id]}
     board-id  {:id board-id :type :frame :parent-id uuid/zero :frame-id uuid/zero
                :x 0 :y 0 :width 100 :height 100
                :selrect rect :points (grc/rect->points rect)}}))

(defn- thread
  [n x y & {:as attrs}]
  (merge {:id (uuid/custom 4 n) :seqn n :page-id page-id :frame-id board-id
          :file-id (uuid/custom 5 1) :project-id (uuid/custom 5 2) :file-name "File"
          :owner-id me :participants #{me} :position (gpt/point x y)
          :content "" :created-at (ct/now) :modified-at (ct/now)
          :count-unread-comments 0}
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
                                        #js {:page {:id page-id}
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
  "What a tap on Post of a comment begun at 50 50, on the board, of the
  page `draft-page` emits on the canvas of the page `page-id`."
  [draft-page]
  (let [post! (fn [document window _]
                (some->> (js/Array.from (.querySelectorAll document "button"))
                         (some #(when (= (tr "labels.post") (.-textContent %)) %))
                         (thd/click! window)))
        draft {:position (gpt/point 50 50) :page-id draft-page :file-id (uuid/custom 5 1)
               :content "Hi"}]
    (mapv ptk/type (:emitted (rendered [] {:draft draft} 1 post!)))))

(t/deftest a-comment-begun-on-another-page-goes
  (t/is (= [::dcm/create-thread-on-viewer ::dcm/close-comment-thread] (posted page-id))
        "about the board under it")
  (t/is (= [::dcm/close-comment-thread] (posted (uuid/custom 1 2)))
        "begun on another page: it goes, posted nowhere"))
