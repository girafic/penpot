;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.phone-sheet-test
  "The view mode on a phone, its board alone: two fingers held on it open
  the sheet with what its bars hold elsewhere (Penpot Motion)."
  (:require
   [app.main.ui.viewer.phone-sheet :as phone-sheet]
   [app.util.storage :as storage]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
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
                 (t/is (false? lifted) "one lifted before")))
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


(t/deftest a-sheet-rests-at-half-the-screen-or-most-of-it
  (t/is (= [400 720] (phone-sheet/sheet-heights :half 800 100)) "as high as `:half`")
  (t/is (= [150 150] (phone-sheet/sheet-heights nil 800 150)) "what it holds, short")
  (t/is (= [400 600] (phone-sheet/sheet-heights nil 800 600)) "longer than half the screen")
  (t/is (= [400 720] (phone-sheet/sheet-heights nil 800 2000))))

(t/deftest a-sheet-let-go-rests-at-the-nearer-height-or-where-flicked
  (t/is (= :high (phone-sheet/sheet-rest 600 0 400 720)) "nearer most of the screen")
  (t/is (= :low (phone-sheet/sheet-rest 500 0 400 720)))
  (t/is (= :high (phone-sheet/sheet-rest 450 -1 400 720)) "flicked up")
  (t/is (= :low (phone-sheet/sheet-rest 700 1 400 720)) "flicked down")
  (t/is (= :low (phone-sheet/sheet-rest 350 0 400 720)) "a little below half: back")
  (t/is (= :closed (phone-sheet/sheet-rest 300 0 400 720)) "dragged far down")
  (t/is (= :closed (phone-sheet/sheet-rest 390 1 400 720)) "flicked down below half"))

(t/deftest a-sheet-grows-and-shrinks-by-its-grip
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (thd/render! root (mf/element phone-sheet/sheet* #js {:label "Inspect" :height :half :onClose (fn [])}))
      (let [grip   (.querySelector document "[class*=grip-zone]")
            dialog #(.querySelector document "[role=dialog]")
            half   (* 0.5 (.-innerHeight window))]
        (thd/act #(do (touch! grip "touchstart" [[100 400]])
                      (touch! grip "touchmove" [[100 150]])))
        (t/is (= (str (+ half 250) "px") (.. (dialog) -style -blockSize)) "it follows the finger up")
        (thd/act #(touch! grip "touchend" []))
        (t/is (re-find #"full" (.-className (dialog))) "most of the screen")
        (t/is (= "" (.. (dialog) -style -blockSize)))
        (thd/act #(do (touch! grip "touchstart" [[100 100]])
                      (touch! grip "touchmove" [[100 350]])))
        (thd/act #(touch! grip "touchend" []))
        (t/is (re-find #"half" (.-className (dialog))) "and back to half of it")))))
