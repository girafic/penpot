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
   [app.util.storage :as storage]
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

(mf/defc hint-holder*
  "What `use-hold-hint` gives at `canvas?`, kept in `seen` at each render
  with the sheet open or not."
  {::mf/private true}
  [{:keys [canvas seen]}]
  (let [sheet* (mf/use-state false)
        [hint? hide! open-sheet! toggle-sheet!] (phone-sheet/use-hold-hint canvas sheet*)]
    (swap! seen conj {:hint? hint? :sheet? @sheet* :fns [hide! open-sheet! toggle-sheet!]})
    [:div]))

(t/deftest the-note-of-the-sheet-shows-each-time-the-prototype-does
  (thd/with-dom
    (fn [{:keys [root]}]
      (let [seen    (atom [])
            render! #(thd/render! root (mf/element hint-holder* #js {:canvas % :seen seen}))
            now     #(select-keys (peek @seen) [:hint? :sheet?])
            call!   (fn [i] (thd/act #((get-in (peek @seen) [:fns i]))))]
        (render! false)
        (t/is (= {:hint? true :sheet? false} (now)) "as the prototype shows")
        (call! 0)
        (t/is (= {:hint? false :sheet? false} (now)) "gone at a tap or after a while")
        (render! true)
        (render! false)
        (t/is (= {:hint? true :sheet? false} (now)) "the prototype again, after the canvas: again")
        (call! 1)
        (t/is (= {:hint? false :sheet? true} (now)) "two fingers open the sheet: it goes")
        (call! 2)
        (t/is (= {:hint? false :sheet? false} (now)) "its key closes it: it does not come back")
        (call! 2)
        (t/is (= {:hint? false :sheet? true} (now)) "and opens it, as two fingers do")
        (render! true)
        (render! false)
        (call! 2)
        (t/is (= {:hint? false :sheet? false} (now)) "the note shown, its key hides it too")
        (t/is (apply = (map :fns @seen)) "the same fns at every render")))))

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

(defn- controls-setting
  "What the options of a phone over the prototype (or the canvas,
  `canvas?`) offer of the controls on the screen, shown or not
  (`shown?`): whether its checkbox, ticked, and what a tap on it asks."
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
                                           :showControls shown?
                                           :onShowControls #(swap! asked conj %)}))
        (let [box (.querySelector document "#phone-screen-controls")]
          (some->> box (thd/click! window))
          {:offered (some? box) :ticked (some-> box .-checked) :asked @asked})))))

(t/deftest the-options-offer-their-controls-on-the-screen
  (t/is (= {:offered true :ticked false :asked [true]} (controls-setting false false))
        "over the prototype, off: a tap asks for them")
  (t/is (= {:offered true :ticked true :asked [false]} (controls-setting false true))
        "on: a tap asks for none")
  (t/is (= {:offered false :ticked nil :asked []} (controls-setting true false))
        "the canvas has its sheet docked"))

(defn- screen-controls
  "What the controls on the screen show at the `index`th of `total`
  boards, with the controls of an animation or not (`animated?`), and
  what a tap on the button labelled `pick` emits."
  [index total animated? pick]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [emitted (atom [])
            ;; by the tooltip that names it (see `icon-button*`)
            labelled (fn [^js node]
                       (some-> (.getAttribute node "aria-labelledby")
                               (as-> id (.getElementById document id))
                               (.getAttribute "aria-label")))]
        (with-redefs [st/emit! (mock/stub (fn [& events] (swap! emitted into events)))]
          (thd/render! root (mf/element phone-sheet/screen-controls*
                                        #js {:index index :total total
                                             :timeline (when animated? (mf/html [:p "play"]))}))
          (let [buttons (vec (.querySelectorAll document "[role=group] button"))]
            (some->> buttons (filter #(= pick (labelled %))) (first) (thd/click! window))
            {:counter  (some-> (.querySelector document "[role=group] [class*=counter]") .-textContent)
             :disabled (into #{} (comp (filter #(.-disabled %)) (map labelled)) buttons)
             :timeline (some? (.querySelector document "[role=group] p"))
             :emitted  (mapv ptk/type @emitted)}))))))

(t/deftest the-controls-on-the-screen-go-from-board-to-board
  (t/is (= {:counter "1 / 3" :disabled #{(tr "viewer.go-to-first-board") (tr "labels.previous")}
            :timeline false :emitted [::dv/select-next-frame]}
           (screen-controls 0 3 false (tr "labels.next")))
        "the first board: the next one")
  (t/is (= {:counter "3 / 3" :disabled #{(tr "labels.next")} :timeline true
            :emitted [::dv/select-prev-frame]}
           (screen-controls 2 3 true (tr "labels.previous")))
        "the last, animated: the one before, its animation too"))

(t/deftest the-controls-on-the-screen-stay-whole-where-dragged
  (let [screen {:width 375 :height 812}
        rect   {:left 70 :top 700 :width 240 :height 40}]
    (t/is (= {:x (/ 190 375) :y (/ 620 812)} (phone-sheet/controls-place rect 0 -100 screen))
          "the middle of their box where the finger took it, as parts of the screen")
    (t/is (= {:x (/ 128 375) :y (/ 28 812)} (phone-sheet/controls-place rect -500 -2000 screen))
          "past the top left: whole, 8px in from the edges")
    (t/is (= {:x (/ 247 375) :y (/ 784 812)} (phone-sheet/controls-place rect 500 500 screen))
          "past the bottom right too")))

(t/deftest the-controls-on-the-screen-keep-where-they-were-dropped
  (let [portrait  {:width 375 :height 812}
        landscape {:width 812 :height 375}
        right     {:x (/ 287 375) :y 0.5}
        bottom    {:x 0.5 :y (/ 784 812)}]
    (t/is (= right (phone-sheet/kept-place right 160 40 portrait {}))
          "as dropped while they fit")
    (t/is (= {:x (/ 247 375) :y 0.5} (phone-sheet/kept-place right 240 40 portrait {}))
          "wider, as an animation adds its buttons: whole, 8px in from the right edge")
    (t/is (= {:x 0.5 :y (/ 347 375)} (phone-sheet/kept-place bottom 240 40 landscape {}))
          "turned: whole above the bottom edge")
    (t/is (= bottom (phone-sheet/kept-place bottom 240 40 portrait {}))
          "turned back: where they were dropped")
    (t/is (= {:x (/ 172 812) :y (/ 347 375)}
             (phone-sheet/kept-place {:x 0 :y 1} 240 40 landscape {:left 44 :bottom 0}))
          "clear of the sensor housing too")
    (t/is (= {:x (/ 128 375) :y (/ 62 812)}
             (phone-sheet/controls-place {:left 0 :top 0 :width 240 :height 40} -100 -100 portrait
                                         {:top 34}))
          "a drag keeps clear of the insets")))

(t/deftest the-controls-on-the-screen-go-back-as-they-narrow
  (swap! storage/user assoc ::phone-sheet/controls-place {:x 0.9 :y 0.5})
  (let [result
        (thd/with-dom
          (fn [{:keys [root document window]}]
            (let [observed (atom [])
                  width    (atom 160)]
              ;; jsdom has no ResizeObserver: one that calls back when told
              (set! (.-ResizeObserver window)
                    (fn [callback]
                      (swap! observed conj callback)
                      #js {:observe (fn [_]) :disconnect (fn [])}))
              (with-redefs [st/emit! (mock/stub (fn [& _]))]
                (thd/render! root (mf/element phone-sheet/screen-controls* #js {:index 0 :total 3}))
                (let [bar     (.querySelector document "[role=group]")
                      x       #(js/parseFloat (.getPropertyValue (.-style bar) "--controls-x"))
                      resized (fn [w]
                                (reset! width w)
                                (thd/act #(doseq [callback @observed] (callback)))
                                (x))]
                  (set! (.-getBoundingClientRect bar)
                        (fn [] #js {:left 0 :top 0 :width @width :height 40}))
                  ;; jsdom: a screen of 1024 by 768
                  {:fits     (resized 160)
                   :wider    (resized 240)
                   :narrower (resized 160)
                   :stored   (get @storage/user ::phone-sheet/controls-place)})))))]
    (swap! storage/user dissoc ::phone-sheet/controls-place)
    (t/is (= 0.9 (:fits result)) "where they were dropped")
    (t/is (= (/ (- 1024 120 8) 1024) (:wider result)) "wider: left, whole on the screen")
    (t/is (= 0.9 (:narrower result)) "narrower again: back where they were dropped")
    (t/is (= {:x 0.9 :y 0.5} (:stored result)) "the place dropped stays as it was")))

(defn- pressed
  "What the controls on the screen emit as a finger presses their button
  to the next board, moves `dy` px and lets go, and where they stay."
  [dy]
  (swap! storage/user dissoc ::phone-sheet/controls-place)
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (let [emitted (atom [])
            fire!   (fn [node type y]
                      (thd/act #(.dispatchEvent node (new (.-MouseEvent window) type
                                                          #js {:bubbles true :clientX 100 :clientY y}))))]
        (with-redefs [st/emit! (mock/stub (fn [& events] (swap! emitted into events)))]
          (thd/render! root (mf/element phone-sheet/screen-controls* #js {:index 0 :total 3}))
          (let [bar  (.querySelector document "[role=group]")
                next (last (.querySelectorAll document "[role=group] button"))]
            ;; jsdom captures no pointer
            (set! (.-setPointerCapture bar) (fn [_]))
            (fire! next "pointerdown" 700)
            (fire! next "pointermove" (+ 700 dy))
            (fire! next "pointerup" (+ 700 dy))
            (thd/click! window next)
            (let [place (get @storage/user ::phone-sheet/controls-place)]
              (swap! storage/user dissoc ::phone-sheet/controls-place)
              {:emitted (mapv ptk/type @emitted)
               :placed  (.getAttribute bar "data-placed")
               :stays   (some? place)})))))))

(t/deftest a-finger-drags-the-controls-on-the-screen
  (t/is (= {:emitted [::dv/select-next-frame] :placed "false" :stays false} (pressed 3))
        "a tap trembling a little: the next board")
  (t/is (= {:emitted [] :placed "true" :stays true} (pressed -100))
        "dragged: elsewhere, where they stay, and no board"))

(defn- emit-into
  "A stand-in for `st/emit!` that collects what it emits in the atom
  `out`, at each arity the code calls it with."
  [out]
  (fn
    ([] nil)
    ([event] (swap! out conj event) nil)
    ([event & events] (swap! out into (cons event events)) nil)))

(t/deftest ^:async a-click-after-a-drag-cancelled-taps
  (swap! storage/user dissoc ::phone-sheet/controls-place)
  (let [emitted (atom [])]
    (await
     (mock/with-mocks*
       {st/emit! (emit-into emitted)}
       (await
        (thd/with-dom
          (fn [{:keys [root document window]}]
            (let [fire! (fn [node type y]
                          (thd/act #(.dispatchEvent node (new (.-MouseEvent window) type
                                                              #js {:bubbles true :clientX 100 :clientY y}))))]
              (thd/render! root (mf/element phone-sheet/screen-controls* #js {:index 0 :total 3}))
              (let [bar  (.querySelector document "[role=group]")
                    next (last (.querySelectorAll document "[role=group] button"))]
                ;; jsdom captures no pointer
                (set! (.-setPointerCapture bar) (fn [_]))
                ;; a drag the browser takes over: no click follows
                (fire! next "pointerdown" 700)
                (fire! next "pointermove" 600)
                (fire! next "pointercancel" 600)
                ;; later, a click with no finger down, as a key gives it
                (-> (thd/settle)
                    (.then #(thd/click! window next))))))))))
    (swap! storage/user dissoc ::phone-sheet/controls-place)
    (t/is (= [::dv/select-next-frame] (mapv ptk/type @emitted))
          "the next board: only the click of the drag itself taps nothing")))
