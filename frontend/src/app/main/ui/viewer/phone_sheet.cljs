;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.phone-sheet
  "On a phone the view mode shows its board alone, no bars over it: two
  fingers held on it open a sheet from the bottom with what the bars hold
  elsewhere, the boards, the animation, the flows, how the interactions
  show, the sections, edit and share."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.types.page :as ctp]
   [app.main.data.comments :as dcm]
   [app.main.data.modal :as modal]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.main.ui.viewer.drag-scroll :as dds]
   [app.main.ui.viewer.header :as header]
   [app.util.globals :as g]
   [app.util.i18n :refer [tr]]
   [app.util.storage :as storage]
   [rumext.v2 :as mf]))

(def hold-ms
  "How long two fingers rest on the board to open the sheet."
  500)

(def ^:private hold-slop-px
  "How far a finger of a hold may move: further, it pinches or scrolls."
  10)

(def ^:private close-drag-px
  "How far down its lower height the sheet is dragged to close."
  80)

(def ^:private flick-speed
  "How fast, px/ms, a drag of the grip let go goes on to the next height
  of the sheet the way it went."
  0.5)

(def ^:private most-of-screen
  "How high a sheet grows, of the screen: as `.full` and `.grown`."
  0.9)

(defn sheet-heights
  "The heights, px, a sheet rests at on a screen `vh` px high, `[low
  high]`: half the screen and most of it for a sheet as high as `:half`;
  else as high as what it holds, `natural` px, up to them."
  [height vh natural]
  (let [half (* 0.5 vh)
        most (* most-of-screen vh)]
    (if (= height :half)
      [half most]
      (let [low (min half natural)]
        [low (max low (min most natural))]))))

(defn sheet-rest
  "Where a sheet let go rests, dragged by its grip to `h` px high going
  `v` px/ms (down more than 0), between its heights `low` and `high`:
  `:low`, `:high`, or `:closed` once dragged further than
  `close-drag-px` below `low`, or flicked down below it."
  [h v low high]
  (cond
    (or (< h (- low close-drag-px))
        (and (< h low) (> v flick-speed)))
    :closed

    (< v (- flick-speed)) :high
    (> v flick-speed)     :low
    (< (- high h) (- h low)) :high
    :else :low))

(defn- drag-style
  "How a sheet dragged to `h` px high shows between its heights `low` and
  `high`: as high, or, below `low`, as `low` and moved down."
  [h low high]
  {:block-size (dm/str (mth/clamp h low high) "px")
   :transform  (when (< h low) (dm/str "translateY(" (- low h) "px)"))})

(def hint-ms
  "How long the note of how the sheet opens shows."
  5000)

(defn- touch-points
  "`{identifier [x y]}` of the touches of `touches`."
  [^js touches]
  (into {} (map (fn [^js t] [(.-identifier t) [(.-clientX t) (.-clientY t)]])) (array-seq touches)))

(defn held-still?
  "Whether the touches `now` are the two fingers of `start`, each no
  further from where it came down than `hold-slop-px`."
  [start now]
  (and (= 2 (count now))
       (= (set (keys start)) (set (keys now)))
       (every? (fn [[id [x0 y0]]]
                 (let [[x y] (get now id)]
                   (<= (js/Math.hypot (- x x0) (- y y0)) hold-slop-px)))
               start)))

(defn use-two-finger-hold!
  "Calls `on-hold` once two fingers rested `hold-ms` on the page, while
  `enabled?`: in any section of the view mode, which on a phone has no
  bars to go to another by."
  [enabled? on-hold]
  (let [on-hold-ref (mf/use-ref on-hold)]
    (mf/set-ref-val! on-hold-ref on-hold)
    (mf/with-effect [enabled?]
      (when enabled?
        (let [target g/window
              hold*  (volatile! nil)

              cancel!
              (fn []
                (some-> @hold* :timer js/clearTimeout)
                (vreset! hold* nil))

              on-start
              (fn [^js event]
                (cancel!)
                (when (= 2 (.. event -touches -length))
                  (vreset! hold* {:start (touch-points (.-touches event))
                                  :timer (js/setTimeout
                                          (fn []
                                            (vreset! hold* nil)
                                            ((mf/ref-val on-hold-ref)))
                                          hold-ms)})))

              on-move
              (fn [^js event]
                (when-let [{:keys [start]} @hold*]
                  (when-not (held-still? start (touch-points (.-touches event)))
                    (cancel!))))]

          (.addEventListener target "touchstart" on-start #js {:passive true})
          (.addEventListener target "touchmove" on-move #js {:passive true})
          (.addEventListener target "touchend" cancel! #js {:passive true})
          (.addEventListener target "touchcancel" cancel! #js {:passive true})
          (fn []
            (.removeEventListener target "touchstart" on-start #js {:passive true})
            (.removeEventListener target "touchmove" on-move #js {:passive true})
            (.removeEventListener target "touchend" cancel! #js {:passive true})
            (.removeEventListener target "touchcancel" cancel! #js {:passive true})
            (cancel!)))))))

(defn use-keyboard-inset!
  "Keeps on the page, while `enabled?`, how high the keyboard of a phone
  covers its bottom, as `--keyboard-inset`: what docks at the bottom of
  the screen stays above it. The page keeps its height as the keyboard
  comes, the part of it that shows shrinks (`visualViewport`)."
  [enabled?]
  (mf/with-effect [enabled?]
    (when-let [^js view (when enabled? (unchecked-get g/window "visualViewport"))]
      (let [root (.-documentElement g/document)

            update!
            (fn []
              (let [inset (max 0 (- (.-clientHeight root) (.-height view) (.-offsetTop view)))]
                (.setProperty (.-style root) "--keyboard-inset" (dm/str inset "px"))))]

        (update!)
        (.addEventListener view "resize" update!)
        (.addEventListener view "scroll" update!)
        (fn []
          (.removeEventListener view "resize" update!)
          (.removeEventListener view "scroll" update!)
          (.removeProperty (.-style root) "--keyboard-inset"))))))

(mf/defc sheet*
  "A sheet from the bottom of a phone named `label`: `children` scroll
  under its grip. It reaches at most half the screen, or always as high
  as `height` says, `:half`. Its grip drags it higher, up to most of the
  screen, and lower: let go, it rests at the nearer of the two heights,
  or the one it was flicked to, or it calls `on-close` (see
  `sheet-rest`). A tap beside it closes it too, which dims what it
  covers, unless `open-beside`: then what shows above it takes taps."
  [{:keys [label height open-beside on-close children]}]
  (let [expanded*  (mf/use-state false)
        expanded?  (deref expanded*)
        drag*      (mf/use-state nil)
        drag       (deref drag*)
        sheet-ref  (mf/use-ref nil)
        ;; the drag going on: where it started, its heights, its speed
        gesture-ref (mf/use-ref nil)

        on-drag-start
        (mf/use-fn
         (mf/deps height expanded?)
         (fn [^js event]
           (let [y       (.. event -touches (item 0) -clientY)
                 sheet   (mf/ref-val sheet-ref)
                 natural (+ (some-> sheet .-firstChild .-offsetHeight)
                            (some-> sheet .-lastChild .-scrollHeight))
                 [low high] (sheet-heights height (.-innerHeight g/window) natural)
                 shown   (some-> sheet .-offsetHeight)]
             (mf/set-ref-val! gesture-ref {:y0 y
                                           :h0 (if (pos? shown) shown (if expanded? high low))
                                           :low low
                                           :high high
                                           :speed (dds/start y 0 (.-timeStamp event))}))))

        on-drag-move
        (mf/use-fn
         (fn [^js event]
           (when-let [{:keys [y0 h0] :as gesture} (mf/ref-val gesture-ref)]
             (let [y (.. event -touches (item 0) -clientY)]
               (mf/set-ref-val! gesture-ref (update gesture :speed dds/move y (.-timeStamp event)))
               (reset! drag* (- h0 (- y y0)))))))

        on-drag-end
        (mf/use-fn
         (mf/deps drag on-close)
         (fn [^js event]
           (let [{:keys [low high speed]} (mf/ref-val gesture-ref)]
             (mf/set-ref-val! gesture-ref nil)
             (reset! drag* nil)
             (when (some? drag)
               ;; the screen's speed up: the finger's down
               (case (sheet-rest drag (- (dds/speed speed (.-timeStamp event))) low high)
                 :closed (on-close)
                 :high   (reset! expanded* true)
                 :low    (reset! expanded* false))))))

        {:keys [block-size transform]}
        (when (some? drag)
          (let [{:keys [low high]} (mf/ref-val gesture-ref)]
            (drag-style drag low high)))]

    [:*
     (when-not open-beside
       [:div {:class (stl/css :backdrop) :on-click on-close}])
     [:div {:class (stl/css-case :sheet true
                                 :floating (not expanded?)
                                 :half (and (= height :half) (not expanded?))
                                 :full (and (= height :half) expanded?)
                                 :grown (and (nil? height) expanded?))
            :ref sheet-ref
            :role "dialog"
            :aria-label label
            ;; a map literal: rumext passes only these to React as styles
            :style {:block-size block-size
                    :max-block-size (when (some? drag) "none")
                    :transform transform
                    :transition (when (some? drag) "none")}}
      [:div {:class (stl/css :grip-zone)
             :on-touch-start on-drag-start
             :on-touch-move on-drag-move
             :on-touch-end on-drag-end
             :on-touch-cancel on-drag-end}
       [:span {:class (stl/css :grip)}]]
      [:div {:class (stl/css :body)} children]]]))

(mf/defc hold-hint*
  "At the bottom of a phone, where the sheet comes from: how it opens,
  the first time the view mode shows there. It goes after `hint-ms`, at
  a tap and as the sheet opens."
  []
  (let [shown* (mf/use-state #(not (get storage/user ::hold-hint)))
        shown? (deref shown*)
        hide!  (mf/use-fn #(reset! shown* false))]

    (mf/with-effect []
      (when shown?
        (swap! storage/user assoc ::hold-hint true)
        (let [timer (js/setTimeout hide! hint-ms)]
          #(js/clearTimeout timer))))

    (when shown?
      [:div {:class (stl/css :hint)
             :role "status"
             :on-click hide!}
       (tr "viewer.phone.hold-hint")])))

(mf/defc phone-sheet*
  "The options of the view mode of a phone over the board `frame`, the
  `index`th of `page` of `file`: the sections `permissions` allow, edit
  and share, its boards, its animation (`timeline`, the controls of the
  view mode), then what the header holds in the `section` shown: its
  flows and how its interactions show (`interactions-mode`), or which
  comments show and their list. What changes the view closes it, but
  going to another section."
  [{:keys [page file frame index permissions section interactions-mode timeline on-close]}]
  (let [frames     (:frames page)
        total      (count frames)
        flows      (not-empty (:flows page))
        flow       (ctp/get-frame-flow flows (:id frame))
        comments   (mf/deref refs/comments-local)
        cmode      (or (:mode comments) :all)
        resolved?  (not= :pending (:show comments))

        act!
        (fn [& events]
          (on-close)
          (apply st/emit! events))

        ;; the sheet stays, with what the section holds
        go-to-section
        (fn [section]
          (if (or (= section :interactions) (:is-logged permissions))
            (st/emit! dv/close-thumbnails-panel (dv/go-to-section section))
            (do (on-close) (header/open-login-dialog))))

        open-share
        (fn []
          (on-close)
          (modal/show! :share-link {:page page :file file})
          (modal/disallow-click-outside!))]

    [:> sheet* {:label (tr "viewer.phone.options") :on-close on-close}
     ;; the sections, edit and share
     [:div {:class (stl/css :row)}
      [:> icon-button* {:variant "ghost"
                        :aria-pressed (= section :interactions)
                        :aria-label (tr "viewer.header.interactions-section" (sc/get-tooltip :open-interactions))
                        :icon i/play
                        :on-click #(go-to-section :interactions)}]
      (when (or (:in-team permissions) (= (:who-comment permissions) "all"))
        [:> icon-button* {:variant "ghost"
                          :aria-pressed (= section :comments)
                          :aria-label (tr "viewer.header.comments-section" (sc/get-tooltip :open-comments))
                          :icon i/comments
                          :on-click #(go-to-section :comments)}])
      (when (or (:in-team permissions)
                (and (= (:type permissions) :share-link)
                     (= (:who-inspect permissions) "all")))
        [:> icon-button* {:variant "ghost"
                          :aria-pressed (= section :inspect)
                          :aria-label (tr "viewer.header.inspect-section" (sc/get-tooltip :open-inspect))
                          :icon i/code
                          :on-click #(go-to-section :inspect)}])
      [:span {:class (stl/css :gap)}]
      (when (:in-team permissions)
        [:> icon-button* {:variant "ghost"
                          :aria-label (tr "labels.edit")
                          :icon i/curve
                          :on-click #(act! (dv/go-to-workspace (:id page)))}])
      (when (:in-team permissions)
        [:> button* {:variant "primary" :on-click open-share}
         (tr "labels.share")])
      (when-not (:is-logged permissions)
        [:> button* {:variant "ghost"
                     :on-click #(do (on-close) (header/open-login-dialog))}
         (tr "labels.log-or-sign")])]

     ;; the boards
     [:div {:class (stl/css :row)}
      [:span {:class (stl/css :board-name)} (:name frame)]
      [:span {:class (stl/css :counter)} (dm/str (inc index) " / " total)]
      [:> icon-button* {:variant "ghost"
                        :aria-label (tr "viewer.go-to-first-board")
                        :icon i/reload
                        :disabled (zero? index)
                        :on-click #(act! dv/select-first-frame)}]
      [:> icon-button* {:variant "ghost"
                        :aria-label (tr "labels.previous")
                        :icon i/arrow-left
                        :disabled (zero? index)
                        :on-click #(act! dv/select-prev-frame)}]
      [:> icon-button* {:variant "ghost"
                        :aria-label (tr "labels.next")
                        :icon i/arrow-right
                        :disabled (>= (inc index) total)
                        :on-click #(act! dv/select-next-frame)}]]

     ;; its animation
     (when (some? timeline)
       [:div {:class (stl/css :row)} timeline])

     ;; the flows
     (when (and flows (= section :interactions))
       [:ul {:class (stl/css :list)}
        (for [[flow-id item] flows]
          [:li {:key (dm/str flow-id)}
           [:button {:type "button"
                     :class (stl/css :item)
                     :aria-pressed (= flow-id (:id flow))
                     :on-click #(act! (dv/go-to-frame (:starting-frame item)))}
            [:> icon* {:icon-id i/play :size "s"}]
            [:span {:class (stl/css :label)} (:name item)]
            (when (= flow-id (:id flow))
              [:> icon* {:icon-id i/tick :size "s"}])]])])

     ;; how the interactions show
     (when (= section :interactions)
       [:ul {:class (stl/css :list)}
        (for [[mode label] [[:hide (tr "viewer.header.dont-show-interactions")]
                            [:show (tr "viewer.header.show-interactions")]
                            [:show-on-click (tr "viewer.header.show-interactions-on-click")]]]
          [:li {:key (name mode)}
           [:button {:type "button"
                     :class (stl/css :item)
                     :aria-pressed (= mode interactions-mode)
                     :on-click #(act! (dv/set-interactions-mode mode))}
            [:span {:class (stl/css :label)} label]
            (when (= mode interactions-mode)
              [:> icon* {:icon-id i/tick :size "s"}])]])])

     ;; which comments show, and their list
     (when (= section :comments)
       [:ul {:class (stl/css :list)}
        (for [[mode label] [[:all (tr "labels.show-all-comments")]
                            [:yours (tr "labels.show-your-comments")]
                            [:mentions (tr "labels.show-mentions")]]]
          [:li {:key (name mode)}
           [:button {:type "button"
                     :class (stl/css :item)
                     :aria-pressed (= mode cmode)
                     :on-click #(act! (dcm/update-filters {:mode mode}))}
            [:span {:class (stl/css :label)} label]
            (when (= mode cmode)
              [:> icon* {:icon-id i/tick :size "s"}])]])
        [:li
         [:button {:type "button"
                   :class (stl/css :item)
                   :aria-pressed (not resolved?)
                   :on-click #(act! (dcm/update-filters {:show (if resolved? :pending :all)}))}
          [:span {:class (stl/css :label)} (tr "labels.hide-resolved-comments")]
          (when-not resolved?
            [:> icon* {:icon-id i/tick :size "s"}])]]
        [:li
         [:button {:type "button"
                   :class (stl/css :item)
                   :on-click #(act! (dcm/update-options {:show-sidebar? true}))}
          [:> icon* {:icon-id i/comments :size "s"}]
          [:span {:class (stl/css :label)} (tr "labels.show-comments-list")]]]])]))
