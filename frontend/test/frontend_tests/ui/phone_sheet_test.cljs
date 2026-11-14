;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.phone-sheet-test
  "The view mode on a phone, its board alone: two fingers held on it open
  the sheet with what its bars hold elsewhere (Penpot Motion)."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.viewer.phone-sheet :as phone-sheet]
   [app.util.i18n :refer [tr]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [frontend-tests.helpers.mock :as mock]
   [okulary.core :as l]
   [potok.v2.core :as ptk]
   [rumext.v2 :as mf]))

(t/deftest two-fingers-hold-still
  (let [start {1 [100 100] 2 [200 300]}]
    (t/is (true? (phone-sheet/held-still? start {1 [104 103] 2 [200 300]})) "a finger trembles")
    (t/is (false? (phone-sheet/held-still? start {1 [100 130] 2 [200 300]})) "it scrolls or pinches")
    (t/is (false? (phone-sheet/held-still? start {1 [100 100]})) "one lifted")
    (t/is (false? (phone-sheet/held-still? start {1 [100 100] 3 [200 300]})) "another came down")))

(mf/defc holder*
  {::mf/private true}
  [{:keys [on-hold]}]
  (phone-sheet/use-two-finger-hold! true on-hold)
  [:div])

(defn- touch!
  "The fingers `points` (`[[x y]...]`) `type` on `target`, the window of
  jsdom or a node of it."
  [^js target type points]
  (let [window  (or (some-> target .-ownerDocument .-defaultView) target)
        event   (new (.-Event window) type #js {:bubbles true})
        touches (into-array (map-indexed (fn [i [x y]] #js {:identifier i :clientX x :clientY y}) points))]
    ;; a TouchList
    (set! (.-item touches) #(aget touches %))
    (set! (.-touches event) touches)
    (set! (.-changedTouches event) touches)
    (.dispatchEvent target event)))

(defn- held
  "Whether the sheet opened as two fingers come down and do `then`."
  [then]
  (thd/with-dom
    (fn [{:keys [root window]}]
      (let [held? (atom false)]
        (thd/render! root (mf/element holder* #js {:onHold #(reset! held? true)}))
        (touch! window "touchstart" [[100 100] [200 300]])
        (then window)
        (js/Promise. (fn [resolve]
                       (js/setTimeout #(resolve @held?) (+ phone-sheet/hold-ms 100))))))))

(t/deftest two-fingers-held-open-the-sheet
  (t/async
    done
    ;; one after the other: each takes the DOM globals while it runs
    (-> (held (fn [_]))
        (.then (fn [still]
                 (t/is (true? still) "held still")
                 (held #(touch! % "touchmove" [[100 140] [200 300]]))))
        (.then (fn [moved]
                 (t/is (false? moved) "a finger moved: it scrolls or pinches")
                 (held #(touch! % "touchend" [[100 100]]))))
        (.then (fn [lifted]
                 (t/is (false? lifted) "one lifted before")
                 ;; a page too busy to take the moves of a pinch in time
                 (held (fn [window]
                         (js/setTimeout #(touch! window "touchmove" [[100 140] [200 300]])
                                        (+ phone-sheet/hold-ms 2))))))
        (.then (fn [late]
                 (t/is (false? late) "a move that came late, after the hold was due")))
        (.catch (fn [cause] (t/is false (ex-message cause))))
        (.finally (fn [] (done) nil)))))

(t/deftest the-hint-goes-at-a-tap
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [hidden (atom 0)]
        (thd/render! root (mf/element phone-sheet/hold-hint* #js {:onHide #(swap! hidden inc)}))
        (let [hint (.querySelector document "[role=status]")]
          (t/is (some? hint) "each time the view mode shows it")
          (thd/click! window hint)
          (t/is (= 1 @hidden) "gone at a tap"))))))

(defn- sheet
  "How a sheet `props` renders: `[dialog? backdrop?]`, and whether a tap
  on its backdrop closed it."
  [props]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [closed? (atom false)]
        (thd/render! root (mf/element phone-sheet/sheet*
                                      (js/Object.assign #js {:label "Options"
                                                             :onClose #(reset! closed? true)
                                                             :children (mf/html [:p "content"])}
                                                        props)))
        (let [dialog   (.querySelector document "[role=dialog][aria-label=Options]")
              backdrop (.querySelector document "[class*=backdrop]")]
          (some->> backdrop (thd/click! window))
          [(some? dialog) (some? backdrop) @closed?])))))

(t/deftest a-sheet-dims-what-it-covers-unless-open-beside
  (t/is (= [true true true] (sheet #js {})) "a tap beside it closes it")
  (t/is (= [true false false] (sheet #js {:openBeside true})) "what shows above it takes taps"))

(t/deftest a-sheet-follows-the-finger-down-its-grip
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element phone-sheet/sheet* #js {:label "Options" :onClose (fn [])}))
      (let [grip (.querySelector document "[class*=grip-zone]")]
        (thd/act #(do (touch! grip "touchstart" [[100 400]])
                      (touch! grip "touchmove" [[100 440]])))
        (t/is (= "translateY(40px)"
                 (.. document (querySelector "[role=dialog]") -style -transform)))
        (thd/act #(touch! grip "touchend" []))
        (t/is (= "" (.. document (querySelector "[role=dialog]") -style -transform)) "back")))))


(t/deftest a-sheet-rests-at-a-little-of-the-screen-half-of-it-or-most-of-it
  (t/is (= [[:small 80] [:low 400] [:high 720]] (phone-sheet/sheet-heights :half 800 100))
        "as high as `:half`")
  (t/is (= [[:low 60]] (phone-sheet/sheet-heights nil 800 60)) "what it holds, short")
  (t/is (= [[:small 80] [:start 240] [:low 400] [:high 600]] (phone-sheet/sheet-heights nil 800 600 :docked true))
        "docked, where it starts too")
  (t/is (= [[:small 80] [:low 200]] (phone-sheet/sheet-heights nil 800 200 :docked true))
        "docked, holding less than that")
  (t/is (= [[:small 80] [:low 300]] (phone-sheet/sheet-heights nil 800 300)))
  (t/is (= [[:small 80] [:low 400] [:high 600]] (phone-sheet/sheet-heights nil 800 600))
        "longer than half the screen")
  (t/is (= [[:small 80] [:low 400] [:high 720]] (phone-sheet/sheet-heights nil 800 2000)))
  (t/is (= [[:small 150] [:start 240] [:low 400] [:high 600]]
           (phone-sheet/sheet-heights nil 800 600 :docked true :small 150))
        "as high as its head, a little of it"))

(t/deftest a-sheet-let-go-rests-at-the-nearest-height-or-where-flicked
  (let [heights (phone-sheet/sheet-heights :half 800 100)]
    (t/is (= :high (phone-sheet/sheet-rest 600 0 heights)) "nearest most of the screen")
    (t/is (= :low (phone-sheet/sheet-rest 500 0 heights)))
    (t/is (= :small (phone-sheet/sheet-rest 200 0 heights)) "well below half")
    (t/is (= :high (phone-sheet/sheet-rest 450 -1 heights)) "flicked up")
    (t/is (= :low (phone-sheet/sheet-rest 300 -1 heights)) "to the next height up")
    (t/is (= :low (phone-sheet/sheet-rest 700 1 heights)) "flicked down")
    (t/is (= :small (phone-sheet/sheet-rest 380 1 heights)) "to the next height down")
    (t/is (= :small (phone-sheet/sheet-rest 40 0 heights)) "a little below the lowest: back")
    (t/is (= :closed (phone-sheet/sheet-rest -10 0 heights)) "dragged far down")
    (t/is (= :closed (phone-sheet/sheet-rest 70 1 heights)) "flicked down below the lowest")))

(t/deftest a-sheet-grows-and-shrinks-by-its-grip
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [closed? (atom false)]
        (thd/render! root (mf/element phone-sheet/sheet* #js {:label "Inspect"
                                                              :height :half
                                                              :onClose #(reset! closed? true)}))
        (let [grip   (.querySelector document "[class*=grip-zone]")
              dialog #(.querySelector document "[role=dialog]")
              half   (* 0.5 (.-innerHeight window))
              drag!  (fn [y0 y1]
                       (thd/act #(do (touch! grip "touchstart" [[100 y0]])
                                     (touch! grip "touchmove" [[100 y1]])))
                       (thd/act #(touch! grip "touchend" [])))]
          (thd/act #(do (touch! grip "touchstart" [[100 400]])
                        (touch! grip "touchmove" [[100 150]])))
          (t/is (= (str (+ half 250) "px") (.. (dialog) -style -blockSize)) "it follows the finger up")
          (thd/act #(touch! grip "touchend" []))
          (t/is (re-find #"full" (.-className (dialog))) "most of the screen")
          (t/is (= "" (.. (dialog) -style -blockSize)))
          (drag! 100 350)
          (t/is (re-find #"half" (.-className (dialog))) "back to half of it")
          (drag! 100 380)
          (t/is (re-find #"small" (.-className (dialog))) "down to a little of it")
          (t/is (not @closed?))
          (drag! 100 260)
          (t/is (true? @closed?) "and down to close"))))))

(defn- phone-sheet
  "What the options of a phone over the canvas (`canvas?`) or over a
  board of the prototype show to a member of the team: how many sections,
  whether share and whether edit, whether the logo back to the dashboard
  and the names of the project and the file come before the switch
  between the canvas and the prototype, and whether it lists the lone
  page of the file."
  [canvas?]
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [page-id (uuid/next)
            frame   {:id (uuid/next) :name "Board"}
            page    {:id page-id :frames [frame]}
            file    {:name "File" :data {:pages [page-id] :pages-index {page-id {:name "Page"}}}}
            labels  (fn [] (map #(.-textContent %) (.querySelectorAll document "button")))]
        (thd/render! root (mf/element phone-sheet/phone-sheet*
                                      #js {:page page :file file :project {:name "Project"}
                                           :frame frame :index 0
                                           :permissions {:in-team true :is-logged true}
                                           :section :interactions
                                           :onClose identity
                                           :isCanvas canvas? :hasCanvas true :isDocked canvas?}))
        {:sections (count (filter #(.querySelector document (str "[aria-label=\"" % "\"]"))
                                  [(tr "viewer.header.interactions-section" (sc/get-tooltip :open-interactions))
                                   (tr "viewer.header.comments-section" (sc/get-tooltip :open-comments))
                                   (tr "viewer.header.inspect-section" (sc/get-tooltip :open-inspect))]))
         :share?   (boolean (some #{(tr "labels.share")} (labels)))
         :edit?    (some? (.querySelector document (str "[aria-label=\"" (tr "labels.edit") "\"]")))
         :page?    (boolean (some #{"Page"} (labels)))
         :modes?   (boolean (some #{(tr "viewer.header.interactions")} (labels)))
         :names-first?
         (let [switch    (.querySelector document "input[name=phone-view]")
               dashboard (.querySelector document (str "button[aria-label=\"" (tr "labels.dashboard") "\"]"))
               names     (some-> dashboard .-nextSibling .-textContent)]
           (and (some? switch) (= "ProjectFile" names)
                ;; 2, DOCUMENT_POSITION_PRECEDING: the dashboard before it
                (pos? (bit-and (.compareDocumentPosition switch dashboard) 2))))}))))

(t/deftest the-canvas-shows-no-sections-and-no-phone-edits
  (t/is (= {:sections 0 :share? false :edit? false :names-first? true :page? true :modes? false} (phone-sheet true))
        "the canvas shows the page as it is")
  (t/is (= {:sections 3 :share? true :edit? false :names-first? true :page? true :modes? true} (phone-sheet false))
        "the prototype its sections and share, but no edit, the logo and the names first too"))

(mf/defc keyer*
  {::mf/private true}
  [{:keys [is-enabled on-key]}]
  (phone-sheet/use-sheet-key! is-enabled on-key)
  [:div])

(t/deftest alt-o-opens-and-closes-the-sheet-on-a-desktop
  (let [key (fn [props] (phone-sheet/sheet-key? (js/Object.assign #js {:code "KeyO" :altKey true
                                                                       :target #js {:tagName "DIV"}}
                                                                  props)))]
    (t/is (true? (key #js {})) "Alt+O")
    (t/is (false? (key #js {:altKey false})) "O alone")
    (t/is (false? (key #js {:metaKey true})) "with Cmd")
    (t/is (false? (key #js {:ctrlKey true})) "with Ctrl")
    (t/is (false? (key #js {:code "KeyP"})) "another key")
    (t/is (false? (key #js {:target #js {:tagName "TEXTAREA"}})) "in a field it types")
    (t/is (false? (key #js {:target #js {:tagName "DIV" :isContentEditable true}})) "in a text it types"))
  (doseq [enabled? [true false]]
    (thd/with-dom
      (fn [{:keys [root window]}]
        (let [keys (atom 0)]
          (thd/render! root (mf/element keyer* #js {:isEnabled enabled? :onKey #(swap! keys inc)}))
          (thd/act #(.dispatchEvent window (new (.-KeyboardEvent window) "keydown"
                                                #js {:code "KeyO" :altKey true :bubbles true})))
          (t/is (= (if enabled? 1 0) @keys) (if enabled? "while enabled" "not while disabled")))))))

(t/deftest a-sheet-keeps-a-height-until-its-grip-drags-it
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [dragged? (atom false)]
        (thd/render! root (mf/element phone-sheet/sheet* #js {:label "Options"
                                                              :keptHeight 200
                                                              :onDrag #(reset! dragged? true)
                                                              :onClose identity}))
        (t/is (= "200px" (.. (.querySelector document "[role=dialog]") -style -blockSize))
              "as high as it was, whatever it holds")
        (thd/act #(touch! (.querySelector document "[class*=grip-zone]") "touchstart" [[100 400]]))
        (t/is (true? @dragged?) "until its grip drags it")))))

(t/deftest another-page-keeps-the-options-open
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [[page-id other-id] [(uuid/next) (uuid/next)]
            frame   {:id (uuid/next) :name "Board"}
            closed? (atom false)
            emitted (atom [])]
        (with-redefs [st/emit! (mock/stub (fn [& events] (swap! emitted into events)))]
          (thd/render! root (mf/element phone-sheet/phone-sheet*
                                        #js {:page {:id page-id :frames [frame]}
                                             :file {:name "File"
                                                    :data {:pages [page-id other-id]
                                                           :pages-index {page-id {:name "Page"}
                                                                         other-id {:name "Other"}}}}
                                             :frame frame :index 0
                                             :permissions {:in-team true :is-logged true}
                                             :section :interactions
                                             :onClose #(reset! closed? true)}))
          (thd/click! window (->> (.querySelectorAll document "button")
                                  (filter #(= "Other" (.-textContent %)))
                                  (first)))
          (t/is (= [::dv/go-to-page] (map ptk/type @emitted)) "to the other page")
          (t/is (false? @closed?) "the options stay open"))))))

(defn- sheet-taps
  "What a tap on the button of the options of a phone that `pick` finds
  among its buttons emits, and whether it closed them: on the second of
  two boards, which show their interactions on click."
  [pick]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [page-id (uuid/next)
            frames  [{:id (uuid/next) :name "Board"} {:id (uuid/next) :name "Other"}]
            closed? (atom false)
            emitted (atom [])
            button  (fn [f] (->> (.querySelectorAll document "button") (filter f) (first)))]
        (with-redefs [st/emit! (mock/stub (fn [& events] (swap! emitted into events)))]
          (thd/render! root (mf/element phone-sheet/phone-sheet*
                                        #js {:page {:id page-id :frames frames}
                                             :file {:name "File" :data {:pages [page-id]
                                                                        :pages-index {page-id {:name "Page"}}}}
                                             :frame (second frames) :index 1
                                             :permissions {:in-team true :is-logged true}
                                             :section :interactions
                                             :interactionsMode :show-on-click
                                             :onClose #(reset! closed? true)}))
          (thd/click! window (button pick))
          [(map ptk/type @emitted) @closed?])))))

(t/deftest its-buttons-keep-the-options-open
  (let [;; by the tooltip that names it (see `icon-button*`)
        labelled (fn [label]
                   (fn [^js node]
                     (= label (some-> (.getAttribute node "aria-labelledby")
                                      (as-> id (.getElementById (.-ownerDocument node) id))
                                      (.getAttribute "aria-label")))))]
    (t/is (= [[::dv/select-prev-frame] false] (sheet-taps (labelled (tr "labels.previous"))))
          "the board before")
    (t/is (= [[::dv/select-first-frame] false] (sheet-taps (labelled (tr "viewer.go-to-first-board"))))
          "the first board")))

(t/deftest the-menu-of-the-comments
  (let [events #(map ptk/type (@#'phone-sheet/comments-events %1 %2))]
    (t/is (= [::dcm/update-filters] (events "yours" false)) "which comments show")
    (t/is (= [::dcm/update-filters] (events "resolved" true)) "the resolved ones or not")
    (t/is (= [::dcm/update-options] (events "list" false)) "their list, above the options")))

(t/deftest the-canvas-lists-its-boards
  (let [[page-id a b c] (repeatedly 4 uuid/next)
        board   (fn [id name parent] {:id id :name name :type :frame :parent-id parent :frame-id parent})
        objects {uuid/zero {:id uuid/zero :type :frame :shapes [a b]}
                 a (assoc (board a "Start" uuid/zero) :shapes [c])
                 b (board b "Lesson" uuid/zero)
                 c (board c "Card" a)}
        page    {:id page-id :objects objects :frames [(get objects b) (get objects c) (get objects a)]}
        file    {:name "File" :data {:pages [page-id] :pages-index {page-id {:name "Page"}}}}
        shown   (fn [canvas? pick]
                  (thd/with-dom
                    (fn [{:keys [root document window]}]
                      (let [emitted (atom [])
                            labels  #(mapv (fn [n] (.-textContent n))
                                           (.querySelectorAll document "ul[aria-label] button"))]
                        (with-redefs [st/emit! (mock/stub (fn [& events] (swap! emitted into events)))]
                          (thd/render! root (mf/element phone-sheet/phone-sheet*
                                                        #js {:page page :file file :frame (get objects a) :index 0
                                                             :permissions {:in-team true :is-logged true}
                                                             :section :interactions
                                                             :onClose identity
                                                             :isCanvas canvas? :hasCanvas true :isDocked canvas?}))
                          (when-let [button (->> (.querySelectorAll document "ul[aria-label] button")
                                                 (filter #(= pick (.-textContent %)))
                                                 (first))]
                            (thd/click! window button))
                          {:labels (labels)
                           :focus  (some->> @emitted
                                            (filter #(= ::dv/focus-canvas (ptk/type %)))
                                            (first)
                                            (deref))})))))]
    (t/is (= {:labels [(tr "viewer.phone.whole-page") "Lesson" "Start"] :focus {:board b}}
             (shown true "Lesson"))
          "the whole page and the top-level boards, in the order of the view mode: one goes there")
    (t/is (= :page (:focus (shown true (tr "viewer.phone.whole-page")))) "the whole page too")
    (t/is (= [] (:labels (shown false nil))) "the prototype goes board by board")))

(t/deftest a-sheet-lowered-rests-where-it-starts
  (thd/with-dom
    (fn [{:keys [root document]}]
      (let [render! #(thd/render! root (mf/element phone-sheet/sheet* #js {:label "Options"
                                                                           :docked true
                                                                           :lowered %
                                                                           :onClose identity}))
            dialog  #(.querySelector document "[role=dialog]")
            grip    #(.querySelector document "[class*=grip-zone]")]
        (render! 0)
        (t/is (re-find #"start" (.-className (dialog))) "docked, where it starts")
        (thd/act #(do (touch! (grip) "touchstart" [[100 400]])
                      (touch! (grip) "touchmove" [[100 200]])))
        (thd/act #(touch! (grip) "touchend" []))
        (t/is (not (re-find #"start" (.-className (dialog)))) "dragged elsewhere")
        (render! 1)
        (t/is (re-find #"start" (.-className (dialog))) "lowered, there again")))))

(defn- canvas-comments-row
  "What the sheet of the canvas shows of its comments to a viewer with
  `permissions`, the comments `local`, two threads on its board, and what
  a tap on its switch emits. The refs are mocked, not the state of the
  store, which other tests replace."
  [permissions local]
  (let [[page-id board-id] [(uuid/next) (uuid/next)]
        threads (into {} (map (fn [n] [n {:id n :frame-id board-id}])) [(uuid/next) (uuid/next)])
        objects {uuid/zero {:id uuid/zero :type :frame :shapes [board-id]}
                 board-id  {:id board-id :name "Start" :type :frame :parent-id uuid/zero :frame-id uuid/zero}}]
    (thd/with-dom
      (fn [{:keys [root document window]}]
        (let [emitted (atom [])]
          ;; in the shape of its arities: several events go to the variadic one
          (with-redefs [st/emit!             (fn
                                               ([] nil)
                                               ([event] (swap! emitted conj event))
                                               ([event & events] (swap! emitted into (cons event events))))
                        refs/comment-threads (l/atom threads)
                        refs/comments-local  (l/atom local)]
            (thd/render! root (mf/element phone-sheet/phone-sheet*
                                          #js {:page {:id page-id :objects objects :frames [(get objects board-id)]}
                                               :file {:name "File" :data {:pages [page-id]
                                                                          :pages-index {page-id {:name "Page"}}}}
                                               :permissions permissions
                                               :section :interactions
                                               :onClose identity
                                               :isCanvas true :hasCanvas true :isDocked true}))
            (let [switch (.querySelector document "[role=switch]")
                  shown  {:switch (some-> switch .-textContent)
                          :menu?  (boolean (some #(= (tr "labels.comments") (.-textContent %))
                                                 (.querySelectorAll document "button")))
                          :hint?  (some? (.querySelector document "[class*=comments-hint]"))}]
              (some->> switch (thd/click! window))
              (assoc shown :emitted (mapv (fn [e] [(ptk/type e) (when (= ::dcm/update-options (ptk/type e))
                                                                  (get-in (ptk/update e {}) [:comments-local :canvas-shown?]))])
                                          @emitted)))))))))

(t/deftest the-canvas-sheet-switches-its-comments
  (let [editor {:can-edit true :is-logged true :in-team true}]
    (t/is (= {:switch (tr "viewer.phone.show-comments" 2) :menu? false :hint? false
              :emitted [[::dcm/update-options true]]}
             (canvas-comments-row editor {}))
          "off as the view mode starts, with how many there are: a tap shows them")
    (t/is (= {:switch (tr "viewer.phone.show-comments" 2) :menu? true :hint? true
              :emitted [[::dcm/close-comment-thread nil] [::dcm/update-options false]]}
             (canvas-comments-row editor {:canvas-shown? true}))
          "on, which show, their list and how to add one: a tap hides them, an open thread closes")
    (t/is (nil? (:switch (canvas-comments-row {:in-team true :is-logged true :who-comment "team"} {})))
          "none for one who may not comment")))

(defn- button-setting
  "What the options of a phone over the prototype (or the canvas,
  `canvas?`) offer of a button on the screen, shown or not (`shown?`):
  whether its checkbox, ticked, and what a tap on it asks."
  [canvas? shown?]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [page-id (uuid/next)
            frame   {:id (uuid/next) :name "Board"}
            asked   (atom [])]
        (thd/render! root (mf/element phone-sheet/phone-sheet*
                                      #js {:page {:id page-id :frames [frame]}
                                           :file {:name "File" :data {:pages [page-id]
                                                                      :pages-index {page-id {:name "Page"}}}}
                                           :frame frame :index 0
                                           :permissions {:in-team true :is-logged true}
                                           :section :interactions
                                           :onClose identity
                                           :isCanvas canvas? :hasCanvas true :isDocked canvas?
                                           :showButton shown?
                                           :onShowButton #(swap! asked conj %)}))
        (let [box (.querySelector document "#phone-sheet-button")]
          (some->> box (thd/click! window))
          {:offered (some? box) :ticked (some-> box .-checked) :asked @asked})))))

(t/deftest the-options-offer-a-button-on-the-screen
  (t/is (= {:offered true :ticked false :asked [true]} (button-setting false false))
        "over the prototype, off: a tap asks for it")
  (t/is (= {:offered true :ticked true :asked [false]} (button-setting false true))
        "on: a tap asks for none")
  (t/is (= {:offered false :ticked nil :asked []} (button-setting true false))
        "the canvas has its sheet docked"))

(t/deftest the-button-on-the-screen-opens-the-options
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [opened (atom 0)]
        (thd/render! root (mf/element phone-sheet/sheet-button* #js {:onClick #(swap! opened inc)}))
        (thd/click! window (.querySelector document (str "button[aria-label=\"" (tr "viewer.phone.options") "\"]")))
        (t/is (= 1 @opened))))))
