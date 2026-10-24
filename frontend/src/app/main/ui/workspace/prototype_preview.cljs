;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.prototype-preview
  "The preview of the prototype in a window over the workspace (Shift +
  Space, see `dwpp/toggle-preview`): the view mode of the page in a
  frame, once what is edited is saved, showing what is edited after as
  it is made (see `dv/apply-preview-changes`), and what it missed (while
  it loaded, or could not show) once that is saved. Closed, it stays
  loaded, hidden, to show again at once, for a while (see
  `unload-delay`); another page of the workspace starts without it. A
  bar on top moves the window, with the boards before
  and after, start again, the play buttons of an animated board, the
  board it shows, a settings button, open in the view mode and close;
  the corners at its bottom resize it. The menu of the settings button
  holds the zoom (fit, fill or 100 %), how the interactions show, Follow
  the selection (it shows the board selected in the workspace) and two
  sizes of the window. Where it is, as `{:x :y :w :h}` px of the browser
  window, Follow, the zoom and the interactions are kept for the next
  time; open in the view mode closes it. The view mode tells it once it listens
  (`penpot:preview-ready`), what it asks before is sent then, which
  board it shows and where among the boards (`penpot:preview-shown`),
  and how its animation plays (`penpot:preview-playback`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.transit :as t]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.common :as dcm]
   [app.main.data.persistence :as dps]
   [app.main.data.workspace.prototype-preview :as dwpp]
   [app.main.refs :as refs]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.components.check-menu-item :refer [check-menu-item*]]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.main.ui.workspace.canvas-window :as cw]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:preview
  (l/derived dwpp/preview st/state))

(def bar-height
  "The bar on top of the window, which moves it, with its buttons."
  40)

(def ^:private border 1)

(def ^:private refresh-delay
  "Ms after the workspace saved before the preview fetches what it
  missed, so the saves of an edit come in at once."
  250)

(defn start-frame
  "Where the window starts in a browser window `vw` × `vh` px for a board
  `bw` × `bh`: most of the browser window high, as wide as the board then
  is, in the middle."
  [vw vh bw bh]
  (let [h       (* 0.8 vh)
        inner-h (- h bar-height (* 2 border))
        w       (min (* 0.8 vw)
                     (+ (* inner-h (/ (max 1 bw) (max 1 bh))) (* 2 border)))]
    {:x (/ (- vw w) 2) :y (/ (- vh h) 2) :w w :h h}))

(defn actual-frame
  "`frame` resized about its middle so that the board `bw` × `bh` shows
  at its own size under the bar, kept in a browser window `vw` × `vh` px
  (see `cw/clamp-frame`)."
  [{:keys [x y w h]} vw vh bw bh]
  (let [w' (+ bw (* 2 border))
        h' (+ bh bar-height (* 2 border))]
    (cw/clamp-frame {:x (- (+ x (/ w 2)) (/ w' 2))
                     :y (- (+ y (/ h 2)) (/ h' 2))
                     :w w'
                     :h h'}
                    vw vh)))

(defn fitted-frame
  "`frame` as high as the board `bw` × `bh` then is for its width, its top
  left kept: no black above or below the board fitted in it. Narrower
  when that is higher than a browser window `vw` × `vh` px takes."
  [{:keys [x y w]} vw vh bw bh]
  (let [ratio  (/ (max 1 bw) (max 1 bh))
        high   (fn [w] (+ (/ (- w (* 2 border)) ratio) bar-height (* 2 border)))
        wide   (fn [h] (+ (* (- h bar-height (* 2 border)) ratio) (* 2 border)))
        frame  (cw/clamp-frame {:x x :y y :w w :h (high w)} vw vh)]
    (if (< (:h frame) (high (:w frame)))
      (cw/clamp-frame (assoc frame :w (wide (:h frame))) vw vh)
      frame)))

(def zoom-modes
  "The zooms of the preview, as its menu picks them: the board fitted in
  the window, filling it, or at its own size."
  #{"fit" "fill" "100"})

(defn- preview-uri
  "The view mode of the page `page-id` of the file `file-id` as a
  preview, from the board `frame-id` (or where it starts), at `zoom`
  (see `zoom-modes`)."
  [file-id page-id frame-id zoom]
  (rt/resolve-uri (:router @st/state) :viewer
                  (d/without-nils {:file-id (dm/str file-id)
                                   :page-id (dm/str page-id)
                                   :frame-id (some-> frame-id dm/str)
                                   :section "interactions"
                                   :zoom (when (contains? #{"fit" "fill"} zoom) zoom)
                                   :preview "true"})))

(defn- post!
  "Tells the view mode in `iframe` (see `viewer-content*`) `type`, with
  the fields of `data`."
  ([iframe type]
   (post! iframe type nil))
  ([iframe type data]
   (when-let [target (some-> iframe .-contentWindow)]
     (.postMessage target (clj->js (assoc data :type type)) (.. js/window -location -origin)))))

(def ^:private corners
  "The corners that resize the window: those at its bottom, the bar and
  its buttons at the top."
  [["bottom-left" (stl/css :corner :bottom-left)]
   ["bottom-right" (stl/css :corner :bottom-right)]])

(mf/defc preview-window*
  {::mf/private true}
  [{:keys [file-id preview hidden]}]
  (let [{:keys [page-id frame-id]} preview
        objects    (mf/deref refs/workspace-page-objects)
        current    (mf/deref refs/current-page-id)
        selected   (mf/deref refs/selected-shapes)
        ;; the board on screen, as the view mode tells it
        shown*     (mf/use-state frame-id)
        shown      (deref shown*)
        ;; where it is among the boards the view mode lists,
        ;; `{:index :count}`, for the arrows
        place*     (mf/use-state nil)
        {:keys [index] boards :count} (deref place*)
        board      (get objects shown)
        ;; the board the view mode in the frame starts at
        start*     (mf/use-state frame-id)
        start      (deref start*)
        follow*    (mf/use-state #(get storage/user ::follow true))
        follow?    (deref follow*)
        zoom*      (mf/use-state #(get storage/user ::zoom "fit"))
        zoom       (deref zoom*)
        ;; how the view mode shows the interactions, as it does by default
        interactions* (mf/use-state #(get storage/user ::interactions "show-on-click"))
        interactions  (deref interactions*)
        menu*      (mf/use-state false)
        menu?      (deref menu*)
        ;; whether the board shown animates, plays, sounds and is muted,
        ;; as the view mode tells (`penpot:preview-playback`)
        playback*  (mf/use-state nil)
        {:keys [animated playing sound muted]} (deref playback*)
        ;; the board of the selection the pin last asked for
        requested-ref (mf/use-ref frame-id)
        ;; the board it asked for while the view mode could not hear it
        unsent-ref (mf/use-ref nil)
        ;; whether the view mode in the frame listens (see `post!`)
        listening-ref (mf/use-ref false)
        hidden-ref (mf/use-ref hidden)
        ;; what is edited while it is hidden, for it to show once shown
        held-ref   (mf/use-ref nil)
        ;; whether it misses something edited, and whether it fetches
        ;; what was saved (see `resync!`)
        missed-ref (mf/use-ref false)
        refreshing-ref (mf/use-ref false)
        [vw vh]    (cw/use-window-size)
        stored*    (mf/use-state #(get storage/user ::frame))
        drag-frame* (mf/use-state nil)
        frame      (cw/clamp-frame (or (deref drag-frame*)
                                       (deref stored*)
                                       (start-frame vw vh (:width board 16) (:height board 9)))
                                   vw vh)
        iframe-ref (mf/use-ref nil)
        ;; the view mode loads once what is edited is saved
        ready*     (mf/use-state false)
        ready?     (deref ready*)
        ;; a new frame each time it starts again
        run*       (mf/use-state 0)
        ;; the zoom it starts at, as it starts again
        uri        (mf/with-memo [file-id page-id start (deref run*)]
                     (preview-uri file-id page-id start zoom))

        ;; it shows where it is dragged, and stays where it is let go
        on-drag
        (mf/use-fn #(reset! drag-frame* %))

        on-drop
        (mf/use-fn
         (mf/deps vw vh)
         (fn [frame _]
           (let [frame (cw/clamp-frame frame vw vh)]
             (reset! stored* frame)
             (swap! storage/user assoc ::frame frame)
             (reset! drag-frame* nil))))

        {:keys [on-down on-move on-up]} (cw/use-frame-drag frame on-drag on-drop)

        ;; a double click on the bar puts it back where it starts, as
        ;; large as the board then fits
        on-reset
        (mf/use-fn
         (fn [_]
           (reset! stored* nil)
           (swap! storage/user dissoc ::frame)))

        ;; what it missed, fetched once it is saved (see `dwpp/resync?`):
        ;; what it fetches holds what was held for it too
        resync!
        (mf/use-fn
         (fn []
           (when (dwpp/resync? {:missed? (mf/ref-val missed-ref)
                                :listening? (mf/ref-val listening-ref)
                                :refreshing? (mf/ref-val refreshing-ref)
                                :saved? (dps/terminal-status? @refs/persistence)})
             (mf/set-ref-val! missed-ref false)
             (mf/set-ref-val! refreshing-ref true)
             (mf/set-ref-val! held-ref nil)
             (post! (mf/ref-val iframe-ref) "penpot:preview-refresh"))))

        ;; what is edited, shown by the view mode as it is made, held
        ;; while it is hidden or missed (see `dwpp/edit-step`)
        on-edit
        (mf/use-fn
         (fn [{:keys [changes remote?]}]
           (let [held (mf/ref-val held-ref)
                 step (dwpp/edit-step {:listening? (mf/ref-val listening-ref)
                                       :hidden? (mf/ref-val hidden-ref)
                                       :refreshing? (mf/ref-val refreshing-ref)
                                       :held (count held)
                                       :n (count changes)
                                       :remote? remote?
                                       :saved? (dps/terminal-status? @refs/persistence)})]
             (case step
               :miss          (mf/set-ref-val! missed-ref true)
               :hold          (mf/set-ref-val! held-ref (into (or held []) changes))
               :drop-and-miss (do (mf/set-ref-val! held-ref nil)
                                  (mf/set-ref-val! missed-ref true))
               (do (when (= :post-and-miss step)
                     (mf/set-ref-val! missed-ref true))
                   (post! (mf/ref-val iframe-ref) "penpot:preview-changes"
                          {:changes (t/encode-str changes)}))))))

        on-prev    (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-prev"))
        on-next    (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-next"))
        on-play    (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-play"))
        on-stop    (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-stop"))
        on-sound   (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-sound"))
        ;; pinned, at the board shown; else where it started
        on-restart (mf/use-fn
                    (mf/deps follow? shown frame-id)
                    (fn []
                      (let [start (if follow? shown frame-id)]
                        (mf/set-ref-val! unsent-ref nil)
                        (reset! start* start)
                        (reset! shown* start)
                        (swap! run* inc))))

        ;; Enter opens the menu, as on any button: the workspace takes it
        ;; for its own shortcut else
        keep-enter
        (mf/use-fn
         (fn [event]
           (when (kbd/enter? event)
             (dom/stop-propagation event))))

        toggle-menu
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (swap! menu* not)))

        close-menu
        (mf/use-fn #(reset! menu* false))

        on-follow
        (mf/use-fn
         (mf/deps follow?)
         (fn []
           (reset! menu* false)
           (reset! follow* (not follow?))
           (swap! storage/user assoc ::follow (not follow?))))

        on-interactions
        (mf/use-fn
         (fn [event]
           (let [mode (dom/get-data (dom/get-current-target event) "value")]
             (when (contains? dwpp/interactions-modes mode)
               (reset! menu* false)
               (reset! interactions* mode)
               (swap! storage/user assoc ::interactions mode)
               (post! (mf/ref-val iframe-ref) "penpot:preview-interactions" {:mode mode})))))

        on-zoom
        (mf/use-fn
         (fn [event]
           (let [mode (dom/get-data (dom/get-current-target event) "value")]
             (when (contains? zoom-modes mode)
               (reset! menu* false)
               (reset! zoom* mode)
               (swap! storage/user assoc ::zoom mode)
               (post! (mf/ref-val iframe-ref) "penpot:preview-zoom" {:mode mode})))))

        ;; the window as large as the board is, or as high as it then
        ;; needs, kept for the next time
        resize-to
        (mf/use-fn
         (mf/deps frame vw vh board)
         (fn [size]
           (let [bw    (:width board 16)
                 bh    (:height board 9)
                 frame (if (= size :actual)
                         (actual-frame frame vw vh bw bh)
                         (fitted-frame frame vw vh bw bh))]
             (reset! menu* false)
             (reset! stored* frame)
             (swap! storage/user assoc ::frame frame))))

        on-actual-size (mf/use-fn (mf/deps resize-to) #(resize-to :actual))
        on-fit-window  (mf/use-fn (mf/deps resize-to) #(resize-to :fitted))
        on-close   (mf/use-fn #(st/emit! (dwpp/close-preview)))

        on-open
        (mf/use-fn
         (mf/deps page-id shown)
         ;; there in a tab of its own, in place of this window
         #(st/emit! (dcm/go-to-viewer :page-id page-id :frame-id shown)
                    (dwpp/close-preview)))]

    ;; what is edited saved first, so it shows
    (mf/with-effect []
      (st/emit! ::dps/force-persist)
      (let [done (fn [] (reset! ready* true))
            sub  (rx/subs! (fn [_] nil) (fn [_] (done)) done (dps/wait-persisted 15000))]
        #(rx/dispose! sub)))

    ;; then what is edited shows as it is made, and what the view mode
    ;; missed, shortly after the workspace saves it
    (mf/with-effect [file-id]
      (let [sub (->> st/stream
                     (rx/filter dch/commit?)
                     (rx/map #(dwpp/edited-changes (deref %) file-id))
                     (rx/filter some?)
                     (rx/subs! on-edit))]
        #(rx/dispose! sub)))

    (mf/with-effect [ready?]
      (when ready?
        (let [timer (volatile! nil)
              key   (str (random-uuid))]
          (add-watch refs/persistence key
                     (fn [_ _ old new]
                       (when (and (not (dps/terminal-status? old))
                                  (dps/terminal-status? new))
                         (some-> @timer js/clearTimeout)
                         (vreset! timer (js/setTimeout resync! refresh-delay)))))
          (fn []
            (remove-watch refs/persistence key)
            (some-> @timer js/clearTimeout)))))

    ;; closed, it stays loaded, hidden, for Shift + Space to show it again
    ;; at once: the view mode stops what plays, and shown again, it plays
    ;; the board from its start and shows what was edited meanwhile
    (mf/with-effect [hidden]
      (mf/set-ref-val! hidden-ref hidden)
      (let [iframe (mf/ref-val iframe-ref)
            held   (mf/ref-val held-ref)]
        (when ^boolean hidden
          (reset! menu* false)
          ;; the keys go to the workspace again
          (when (and (some? iframe) (identical? iframe (.-activeElement js/document)))
            (.blur iframe)))
        (when (mf/ref-val listening-ref)
          (when (and (not hidden) (seq held))
            (mf/set-ref-val! held-ref nil)
            (on-edit {:changes held}))
          (post! iframe (if ^boolean hidden "penpot:preview-hide" "penpot:preview-show"))
          ;; what it dropped while hidden
          (when-not hidden
            (resync!)))))

    ;; a new board to start at, a new page, or as it shows again: the
    ;; view mode goes there once it listens, else starts there
    (mf/with-effect [frame-id hidden]
      (when-not ^boolean hidden
        (mf/set-ref-val! requested-ref frame-id)
        (mf/set-ref-val! unsent-ref nil)
        (if (mf/ref-val listening-ref)
          (when (some? frame-id)
            (post! (mf/ref-val iframe-ref) "penpot:preview-go" {:frameId (dm/str frame-id)}))
          (do (reset! shown* frame-id)
              (reset! start* frame-id)))))

    ;; a new view mode in the frame listens once it says so: a new src,
    ;; or the same one started again
    (mf/with-effect [uri (deref run*)]
      (mf/set-ref-val! listening-ref false))

    ;; pinned, it shows the board of the selection (see `dwpp/pin-step`)
    (mf/with-effect [follow? ready? selected current page-id objects hidden]
      (when (and follow? (not hidden))
        (let [board-id (dwpp/start-frame-id objects selected)
              step     (dwpp/pin-step {:board board-id
                                       :requested (mf/ref-val requested-ref)
                                       :current-page current
                                       :page page-id
                                       :ready? ready?
                                       :listening? (mf/ref-val listening-ref)})]
          (when (some? step)
            (mf/set-ref-val! requested-ref board-id)
            (case step
              :open  (st/emit! (dwpp/open-preview))
              :start (do (reset! start* board-id)
                         (reset! shown* board-id))
              :go    (do (mf/set-ref-val! unsent-ref nil)
                         (post! (mf/ref-val iframe-ref) "penpot:preview-go" {:frameId (dm/str board-id)}))
              :hold  (mf/set-ref-val! unsent-ref board-id))))))

    ;; what the view mode tells: it listens (it gets what was held for
    ;; it, see `dwpp/ready-messages`), the board it shows; Shift + Space
    ;; in it closes it too
    (mf/with-effect [follow? zoom interactions]
      (let [on-message
            (fn [^js event]
              (let [iframe (mf/ref-val iframe-ref)
                    data   (.-data event)]
                (when (and (some? iframe)
                           (identical? (.-source event) (.-contentWindow iframe))
                           (= (.-origin event) (.. js/window -location -origin)))
                  (case (some-> data (unchecked-get "type"))
                    "penpot:preview-ready"
                    (let [unsent (mf/ref-val unsent-ref)]
                      (mf/set-ref-val! listening-ref true)
                      (mf/set-ref-val! refreshing-ref false)
                      (mf/set-ref-val! unsent-ref nil)
                      (doseq [[type data] (dwpp/ready-messages {:zoom zoom :interactions interactions
                                                                :unsent unsent :follow? follow?})]
                        (post! iframe type data))
                      ;; closed while it loaded
                      (when (mf/ref-val hidden-ref)
                        (post! iframe "penpot:preview-hide"))
                      (resync!))

                    ;; it could not show what was edited
                    "penpot:preview-stale"
                    (do (mf/set-ref-val! missed-ref true)
                        (resync!))

                    ;; a fetch that failed leaves what it missed for the
                    ;; next save
                    "penpot:preview-refreshed"
                    (do (mf/set-ref-val! refreshing-ref false)
                        (if (true? (unchecked-get data "failed"))
                          (mf/set-ref-val! missed-ref true)
                          (resync!)))

                    "penpot:preview-shown"
                    (do (some->> (unchecked-get data "frameId") uuid/parse* (reset! shown*))
                        (reset! place* {:index (unchecked-get data "index")
                                        :count (unchecked-get data "count")}))

                    "penpot:preview-playback"
                    (reset! playback* {:animated (true? (unchecked-get data "animated"))
                                       :playing (true? (unchecked-get data "playing"))
                                       :sound (true? (unchecked-get data "sound"))
                                       :muted (true? (unchecked-get data "muted"))})

                    "penpot:preview-close"
                    (st/emit! (dwpp/close-preview))

                    nil))))]
        (.addEventListener js/window "message" on-message)
        #(.removeEventListener js/window "message" on-message)))

    [:div {:class (stl/css-case :window true
                                :hidden hidden
                                :dragging (some? (deref drag-frame*)))
           :aria-hidden hidden
           :role "dialog"
           :aria-label (tr "workspace.preview.title")
           :style #js {"left" (dm/str (:x frame) "px")
                       "top" (dm/str (:y frame) "px")
                       "width" (dm/str (:w frame) "px")
                       "height" (dm/str (:h frame) "px")
                       "--preview-bar" (dm/str bar-height "px")
                       "--preview-border" (dm/str border "px")}}
     [:div {:class (stl/css :bar)
            :data-mode "move"
            :title (tr "workspace.preview.move")
            :on-pointer-down on-down
            :on-pointer-move on-move
            :on-lost-pointer-capture on-up
            :on-double-click on-reset}
      ;; a click on its buttons moves nothing
      [:div {:class (stl/css :buttons)
             :on-pointer-down dom/stop-propagation
             :on-double-click dom/stop-propagation}
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.previous-board")
                         :icon "arrow-left"
                         :disabled (not (and (number? index) (pos? index)))
                         :on-click on-prev}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.next-board")
                         :icon "arrow-right"
                         :disabled (not (and (number? index) (number? boards) (< index (dec boards))))
                         :on-click on-next}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.restart")
                         :icon "reload"
                         :on-click on-restart}]
       ;; the animation of the board shown, in the bar: over the view
       ;; mode they hid what it shows
       (when ^boolean animated
         [:*
          [:button {:type "button"
                    :class (stl/css :playback-button)
                    :title (tr "workspace.animation.play")
                    :aria-label (tr "workspace.animation.play")
                    :aria-pressed playing
                    :on-click on-play}
           (if ^boolean playing
             [:span {:class (stl/css :pause-icon)}]
             [:> icon* {:icon-id i/play :size "s"}])]
          [:button {:type "button"
                    :class (stl/css :playback-button)
                    :title (tr "workspace.animation.rewind")
                    :aria-label (tr "workspace.animation.rewind")
                    :on-click on-stop}
           [:span {:class (stl/css :stop-icon)}]]
          (when ^boolean sound
            [:> icon-button* {:variant "ghost"
                              :aria-label (if ^boolean muted
                                            (tr "workspace.animation.unmute")
                                            (tr "workspace.animation.mute"))
                              :aria-pressed (not muted)
                              :icon (if ^boolean muted "volume-off" "volume")
                              :on-click on-sound}])])]
      [:span {:class (stl/css :title)}
       (or (:name board) (tr "workspace.preview.title"))]
      [:div {:class (stl/css :buttons)
             :on-pointer-down dom/stop-propagation
             :on-double-click dom/stop-propagation}
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.options")
                         :aria-haspopup "menu"
                         :aria-expanded menu?
                         :class (stl/css-case :menu-open menu?)
                         :icon "settings"
                         :on-key-down keep-enter
                         :on-click toggle-menu}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.open")
                         :icon "open-link"
                         :on-click on-open}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.close")
                         :icon "close"
                         :on-click on-close}]]]
     [:> dropdown-menu* {:show menu?
                         :on-close close-menu
                         :class (stl/css :menu)
                         :style #js {"top" (dm/str (+ (:y frame) bar-height) "px")
                                     "right" (dm/str (- vw (:x frame) (:w frame) -8) "px")}}
      [:> check-menu-item* {:label (tr "workspace.preview.zoom-fit")
                            :icon i/fit-content
                            :value "fit"
                            :radio true
                            :checked (= zoom "fit")
                            :on-click on-zoom}]
      [:> check-menu-item* {:label (tr "workspace.preview.zoom-fill")
                            :icon i/fill-content
                            :value "fill"
                            :radio true
                            :checked (= zoom "fill")
                            :on-click on-zoom}]
      [:> check-menu-item* {:label (tr "workspace.preview.zoom-100")
                            :icon i/percentage
                            :value "100"
                            :radio true
                            :checked (= zoom "100")
                            :on-click on-zoom}]
      [:li {:class (stl/css :menu-separator)
            :role "separator"}]
      [:> check-menu-item* {:label (tr "viewer.header.dont-show-interactions")
                            :value "hide"
                            :radio true
                            :checked (= interactions "hide")
                            :on-click on-interactions}]
      [:> check-menu-item* {:label (tr "viewer.header.show-interactions")
                            :value "show"
                            :radio true
                            :checked (= interactions "show")
                            :on-click on-interactions}]
      [:> check-menu-item* {:label (tr "viewer.header.show-interactions-on-click")
                            :value "show-on-click"
                            :radio true
                            :checked (= interactions "show-on-click")
                            :on-click on-interactions}]
      [:li {:class (stl/css :menu-separator)
            :role "separator"}]
      [:> check-menu-item* {:label (tr "workspace.preview.follow")
                            :checked follow?
                            :on-click on-follow}]
      [:> check-menu-item* {:label (tr "workspace.preview.window-actual")
                            :on-click on-actual-size}]
      [:> check-menu-item* {:label (tr "workspace.preview.window-fit")
                            :on-click on-fit-window}]]
     [:div {:class (stl/css :content)}
      (if ready?
        [:iframe {:key (deref run*)
                  :ref iframe-ref
                  :class (stl/css :frame)
                  :src uri
                  :title (tr "workspace.preview.title")
                  :allow "autoplay"}]
        [:span {:class (stl/css :saving)} (tr "workspace.preview.saving")])]
     (for [[corner class] corners]
       [:div {:key corner
              :class class
              :data-mode corner
              :title (tr "workspace.preview.resize")
              :on-pointer-down on-down
              :on-pointer-move on-move
              :on-lost-pointer-capture on-up}])]))

(def unload-delay
  "Ms a closed preview stays loaded, hidden (see `prototype-preview*`):
  opened again meanwhile, it shows at once; after, it goes, with the
  memory its view mode takes, and loads again as it opens."
  (* 10 60 1000))

(mf/defc prototype-preview*
  "The preview of the prototype of the file `file-id`, while it is open;
  closed, hidden, for `unload-delay`."
  [{:keys [file-id]}]
  (let [preview (mf/deref ref:preview)
        kept*   (mf/use-state preview)
        kept    (deref kept*)
        shown   (d/nilv preview kept)]
    (mf/with-effect [preview]
      (when (some? preview)
        (reset! kept* preview)))
    ;; closed a while, it goes
    (mf/with-effect [preview kept]
      (when (and (nil? preview) (some? kept))
        (let [timer (js/setTimeout #(reset! kept* nil) unload-delay)]
          #(js/clearTimeout timer))))
    (when (some? shown)
      [:> preview-window* {:key (dm/str (:page-id shown))
                           :file-id file-id
                           :preview shown
                           :hidden (nil? preview)}])))
