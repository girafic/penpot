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
   [app.main.store :as st]
   [app.main.ui.viewer.phone-sheet :as phone-sheet]
   [app.util.i18n :refer [tr]]
   [app.util.storage :as storage]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [frontend-tests.helpers.mock :as mock]
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

(t/deftest the-hint-shows-once
  (swap! storage/user dissoc ::phone-sheet/hold-hint)
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (thd/render! root (mf/element phone-sheet/hold-hint* #js {}))
      (let [hint (.querySelector document "[role=status]")]
        (t/is (some? hint) "the first time")
        (thd/click! window hint)
        (t/is (nil? (.querySelector document "[role=status]")) "gone at a tap"))))
  (thd/with-dom
    (fn [{:keys [root document]}]
      (thd/render! root (mf/element phone-sheet/hold-hint* #js {}))
      (t/is (nil? (.querySelector document "[role=status]")) "not again"))))

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
