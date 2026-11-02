;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.prototype-preview
  "The preview of the prototype in a window over the workspace (Shift +
  Space, see `dwpp/toggle-preview`): the view mode of the page in a
  frame, once what is edited is saved, showing what is edited after as
  it is made (see `dvp/apply-preview-changes`), and what it missed (while
  it loaded, or could not show) once that is saved. Closed, it stays
  loaded, hidden, to show again at once, for a while (see
  `unload-delay`); another page of the workspace starts without it. A
  bar on top moves the window, with the boards before and after, start
  again, the play buttons of an animated board, the board it shows, a
  device button (the device it shows the board in, see
  `app.main.data.viewer.devices`: the device alone then, on the
  workspace, under the bar, and a button that turns a phone or a
  tablet), a settings button, open in the view mode and close; the
  corners at its bottom resize it, the frame of a device alone scales
  it. The menu of the
  settings button holds the zoom (fit, fill or 100 %; none while a device
  shows the board), how the interactions show, Follow the selection (it
  shows the board selected in the workspace) and two sizes of the
  window. Where it is, as `{:x :y :w :h}` px of the browser window,
  Follow, the zoom, the interactions and the device are kept for the
  next time; open in the view mode closes it. The view mode tells it
  once it listens (`penpot:preview-ready`), what it asks before is sent
  then, which board it shows and where among the boards
  (`penpot:preview-shown`), once it draws its first board
  (`penpot:preview-drawn`: the window shows a loader, on the screen of
  the device, until then) and how its animation plays
  (`penpot:preview-playback`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.transit :as t]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.common :as dcm]
   [app.main.data.persistence :as dps]
   [app.main.data.viewer.devices :as dvd]
   [app.main.data.workspace.prototype-preview :as dwpp]
   [app.main.refs :as refs]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.components.check-menu-item :refer [check-menu-item*]]
   [app.main.ui.components.dropdown-menu :refer [dropdown-menu*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.main.ui.ds.product.loader :refer [loader*]]
   [app.main.ui.viewer.device-frame :as dfr :refer [device-drawing*]]
   [app.main.ui.viewer.device-menu :refer [device-items*]]
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

(defn shown-device
  "The device `choice` (one of `dvd/choices`) shows a board `bw` × `bh`
  in (see `dvd/resolve-device`), nil for none: a phone or a tablet on
  its side or standing up as the view mode says it shows it
  (`landscape`, nil until it says), whoever looks at it having turned
  it there (see `dv/turn-device`)."
  [choice bw bh landscape]
  (let [device (dvd/resolve-device choice bw bh)]
    (if (and (some? device)
             (boolean? landscape)
             (dvd/turns? (:kind device))
             (not= landscape (boolean (:landscape? device))))
      (dvd/turn device)
      device)))

(defn shown-size
  "The size `[w h]` the window shows for a board `bw` × `bh` in `device`
  (see `shown-device`): the device at its own size and the room round it
  in the view mode, little above it, or the board without one."
  [device bw bh]
  (if (some? device)
    (let [[w h] (dvd/outer-size device)]
      [(+ w (* 2 dvd/stage-padding)) (+ h dvd/stage-padding dvd/preview-top)])
    [bw bh]))

(def ^:private room-w
  "Px the window takes across round a device alone (see `hugged-frame`):
  the room either side of it in the view mode and the border."
  (+ (* 2 dvd/stage-padding) (* 2 border)))

(def ^:private room-h
  "Px the window takes up and down round a device alone: the room above
  and below it in the view mode, the bar and the border."
  (+ dvd/stage-padding dvd/preview-top bar-height (* 2 border)))

(defn- device-scale
  "The scale of the device `dw` × `dh` px at its own size in a window `w`
  × `h` px, as large as it fits (see `dvd/layout`)."
  [w h dw dh]
  (min (/ (max 1 (- w room-w)) dw)
       (/ (max 1 (- h room-h)) dh)))

(defn- device-room
  "The size `[w h]` of the window round the device `dw` × `dh` px at its
  own size, shown at `scale`: whole px, so it fits its height in it, not
  a px less."
  [dw dh scale]
  [(mth/ceil (+ (* dw scale) room-w))
   (mth/ceil (+ (* dh scale) room-h))])

(defn hugged-frame
  "`frame` round the device `dw` × `dh` px at its own size (see
  `shown-device`), which shows alone on the workspace: as high as
  `frame`, as wide as the device then is, about its middle, so that it
  covers no more of the workspace than the device, its bar as wide.
  Lower when a browser window `vw` × `vh` px is not that wide."
  [{:keys [x y w h]} vw vh [dw dh]]
  (let [scale   (device-scale (- vw (* 2 cw/margin)) h dw dh)
        [w' h'] (device-room dw dh scale)
        w'      (max cw/min-width w')
        h'      (max cw/min-height h')]
    (cw/clamp-frame {:x (- (+ x (/ w 2)) (/ w' 2)) :y y :w w' :h h'} vw vh)))

(defn turned-frame
  "The window round a device turned a quarter, at `frame` round it as it
  was, `dw` × `dh` px at its own size (see `hugged-frame`): round it
  turned at the same size, the top and the middle of the window kept."
  [{:keys [x y w h]} vw vh [dw dh]]
  (let [[w' h'] (device-room dh dw (device-scale w h dw dh))]
    (hugged-frame {:x (- (+ x (/ w 2)) (/ w' 2)) :y y :w w' :h h'} vw vh [dh dw])))

(defn scaled-frame
  "`frame`, which a drag of a corner or a side `mode` of the device
  resized from `start` (see `cw/resized` and `grips`), round the device
  `dw` × `dh` px at its own size (see `hugged-frame`): the device scaled
  as the drag changed the width or the height of the window, the one it
  changed more from a corner; the corner or the side across kept, the
  top of the window from its sides, its middle from the top or the
  bottom. Moved, it is as it is."
  [{:keys [w h] :as frame} mode start [dw dh]]
  (if (= mode :move)
    frame
    (let [{sx :x sy :y sw :w sh :h} start
          before  (device-scale sw sh dw dh)
          across  (/ (- w room-w) dw)
          down    (/ (- h room-h) dh)
          scale   (max 0 (case mode
                           (:left :right) across
                           (:top :bottom) down
                           (if (> (mth/abs (- across before)) (mth/abs (- down before))) across down)))
          [w' h'] (device-room dw dh scale)]
      {:x (case mode
            (:top-left :bottom-left :left) (- (+ sx sw) w')
            (:top :bottom) (- (+ sx (/ sw 2)) (/ w' 2))
            sx)
       :y (if (contains? #{:top-left :top-right :top} mode) (- (+ sy sh) h') sy)
       :w w'
       :h h'})))

(def ^:private grip-out
  "Px out of a device alone its frame still scales it from (see `grips`)."
  8)

(def ^:private grip-corner
  "Px into a device alone its corners scale it from (see `grips`), past
  the round of a phone."
  12)

(defn- device-layout
  "Where the device `device` (see `shown-device`) shows alone in the
  window at `frame`, as the view mode in it lays it out (see
  `dvd/layout`): px of the view mode in the window."
  [device {:keys [w h]}]
  (dvd/layout device (- w (* 2 border)) (- h bar-height (* 2 border))
              dvd/stage-padding dvd/preview-top))

(defn grips
  "Where the frame of the device `device` (see `shown-device`) scales it,
  alone in the window at `frame` (see `scaled-frame`): `[mode {:x :y :w
  :h}]`, px of the view mode in the window, for each side round its
  screen, the top and the bottom as wide as the device, then each
  corner; `grip-out` out of it."
  [device frame]
  (let [{:keys [x y screen] dw :w dh :h} (device-layout device frame)
        {sx :x sy :y sw :w sh :h} screen
        left   (- x grip-out)
        top    (- y grip-out)
        right  (+ x dw grip-out)
        bottom (+ y dh grip-out)
        side   (+ grip-out grip-corner)]
    [[:top {:x left :y top :w (- right left) :h (- sy top)}]
     [:bottom {:x left :y (+ sy sh) :w (- right left) :h (- bottom sy sh)}]
     [:left {:x left :y sy :w (- sx left) :h sh}]
     [:right {:x (+ sx sw) :y sy :w (- right sx sw) :h sh}]
     [:top-left {:x left :y top :w side :h side}]
     [:top-right {:x (- right side) :y top :w side :h side}]
     [:bottom-left {:x left :y (- bottom side) :w side :h side}]
     [:bottom-right {:x (- right side) :y (- bottom side) :w side :h side}]]))

(def ^:private ref:prototype-background
  "The background of the prototype of the page, round a board shown
  without a device (see `app.main.ui.viewer/background`)."
  (l/derived (l/key :prototype-background) refs/workspace-page))

(def ^:private drawn-wait-ms
  "How long, at most, the window shows its loader once what is edited is
  saved, ms: the view mode in it that never tells it drew its first
  board, as one that could not open the file, shows then. The view mode
  waits for its first board no more than 8 s once it shows it."
  60000)

(mf/defc loading*
  "What the window shows while the view mode in it loads, until it draws
  its first board (see `prototype-preview*`): the device alone at
  `layout` (see `device-layout`) of the color `finish`, a loader on its
  black screen; without one, the loader alone."
  {::mf/private true}
  [{:keys [device layout finish label title]}]
  [:div {:class (stl/css :loading)}
   (if (some? layout)
     [:*
      [:> device-drawing* {:layout layout
                           :kind (:kind device)
                           :finish finish
                           :label label}]
      [:div {:class (stl/css :loading-screen)
             :style (dfr/screen-style layout)}
       [:> loader* {:title title
                    :overlay true
                    :width (mth/clamp (mth/round (* 0.4 (get-in layout [:screen :w]))) 32 100)}]]
      [:> device-drawing* {:layout layout
                           :kind (:kind device)
                           :finish finish
                           :cutouts true}]]
     [:> loader* {:title title
                  :overlay true}])])

(def zoom-modes
  "The zooms of the preview, as its menu picks them: the board fitted in
  the window, filling it, or at its own size."
  #{"fit" "fill" "100"})

(defn- preview-uri
  "The view mode of the page `page-id` of the file `file-id` as a
  preview, from the board `frame-id` (or where it starts), at `zoom`
  (see `zoom-modes`), in the device `device` of the color `color`."
  [file-id page-id frame-id zoom device color]
  (rt/resolve-uri (:router @st/state) :viewer
                  (d/without-nils {:file-id (dm/str file-id)
                                   :page-id (dm/str page-id)
                                   :frame-id (some-> frame-id dm/str)
                                   :section "interactions"
                                   :zoom (when (contains? #{"fit" "fill"} zoom) zoom)
                                   :device device
                                   :device-color color
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
  its buttons at the top; round a device alone, its frame does (see
  `grips`)."
  [["bottom-left" (stl/css :corner :bottom-left)]
   ["bottom-right" (stl/css :corner :bottom-right)]])

(mf/defc preview-window*
  {::mf/private true}
  [{:keys [file-id preview hidden]}]
  (let [{:keys [page-id frame-id]} preview
        objects    (mf/deref refs/workspace-page-objects)
        background (mf/deref ref:prototype-background)
        current    (mf/deref refs/current-page-id)
        selected   (mf/deref refs/selected-shapes)
        ;; the board on screen, as the view mode tells it
        shown*     (mf/use-state frame-id)
        shown      (deref shown*)
        ;; where it is among the boards the view mode lists,
        ;; `{:index :count}`, for the arrows
        place*     (mf/use-state nil)
        {:keys [index] boards :count} (deref place*)
        ;; whether its device lies on its side, as the view mode tells:
        ;; turned there by whoever looks at it
        landscape* (mf/use-state nil)
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
        ;; the device it shows the board in, and its color (see
        ;; `app.main.data.viewer.devices`)
        device*    (mf/use-state #(dvd/parse-choice (get storage/user ::device)))
        device     (deref device*)
        color*     (mf/use-state #(dvd/parse-finish (get storage/user ::device-color)))
        color      (deref color*)
        device-menu* (mf/use-state false)
        device-menu? (deref device-menu*)
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
        ;; what the window shows of the board: in its device, if any
        device-shown (shown-device device (:width board) (:height board) (deref landscape*))
        [shown-w shown-h] (shown-size device-shown (:width board 16) (:height board 9))
        ;; the device at its own size, when it shows the board in one
        outer      (some-> device-shown dvd/outer-size)
        framed?    (some? outer)
        turns?     (and framed? (dvd/turns? (:kind device-shown)))
        frame      (cond-> (cw/clamp-frame (or (deref drag-frame*)
                                               (deref stored*)
                                               (start-frame vw vh shown-w shown-h))
                                           vw vh)
                     framed? (hugged-frame vw vh outer))
        ;; the device at its own size and the window round it, as they
        ;; last showed (see the turn below)
        last-ref   (mf/use-ref nil)
        iframe-ref (mf/use-ref nil)
        ;; the view mode loads once what is edited is saved, and shows
        ;; once it draws its first board
        ready*     (mf/use-state false)
        ready?     (deref ready*)
        drawn*     (mf/use-state false)
        drawn?     (deref drawn*)
        ;; a new frame each time it starts again
        run*       (mf/use-state 0)
        ;; the zoom it starts at, as it starts again
        uri        (mf/with-memo [file-id page-id start (deref run*)]
                     (preview-uri file-id page-id start zoom device color))

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

        ;; its frame scales the device alone, as large across as high
        constrain
        (mf/use-fn
         (mf/deps outer)
         (fn [frame mode start]
           (if (some? outer)
             (scaled-frame frame mode start outer)
             frame)))

        {:keys [on-down on-move on-up]} (cw/use-frame-drag frame on-drag on-drop constrain)

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
        on-turn    (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-turn"))
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
           (reset! device-menu* false)
           (swap! menu* not)))

        close-menu
        (mf/use-fn #(reset! menu* false))

        toggle-device-menu
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (reset! menu* false)
           (swap! device-menu* not)))

        close-device-menu
        (mf/use-fn #(reset! device-menu* false))

        ;; the view mode shows the board in it at once, kept for the
        ;; next time
        on-device
        (mf/use-fn
         (mf/deps color)
         (fn [choice]
           (reset! device-menu* false)
           (reset! device* choice)
           (swap! storage/user assoc ::device choice)
           (post! (mf/ref-val iframe-ref) "penpot:preview-device" {:device choice :color color})))

        on-device-color
        (mf/use-fn
         (mf/deps device)
         (fn [color]
           (reset! device-menu* false)
           (reset! color* color)
           (swap! storage/user assoc ::device-color color)
           (post! (mf/ref-val iframe-ref) "penpot:preview-device" {:device device :color color})))

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
         (mf/deps frame vw vh shown-w shown-h)
         (fn [size]
           (let [frame (if (= size :actual)
                         (actual-frame frame vw vh shown-w shown-h)
                         (fitted-frame frame vw vh shown-w shown-h))]
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

    ;; a device turned keeps its size: the window turns round it, its top
    ;; and middle kept, as the view mode tells it turned
    (mf/with-layout-effect [outer frame vw vh]
      (let [{before :outer before-frame :frame} (mf/ref-val last-ref)]
        (mf/set-ref-val! last-ref {:outer outer :frame frame})
        (when (and (some? before)
                   (some? outer)
                   (not= before outer)
                   (= before (vec (rseq outer))))
          (let [frame (turned-frame before-frame vw vh before)]
            (reset! stored* frame)
            (swap! storage/user assoc ::frame frame)))))

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
          (reset! device-menu* false)
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
      (mf/set-ref-val! listening-ref false)
      (reset! drawn* false))

    (mf/with-effect [ready? drawn?]
      (when (and ready? (not drawn?))
        (let [timer (js/setTimeout #(reset! drawn* true) drawn-wait-ms)]
          #(js/clearTimeout timer))))

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
    (mf/with-effect [follow? zoom interactions device color]
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
                                                                :device device :color color
                                                                :unsent unsent :follow? follow?})]
                        (post! iframe type data))
                      ;; closed while it loaded
                      (when (mf/ref-val hidden-ref)
                        (post! iframe "penpot:preview-hide"))
                      (resync!))

                    "penpot:preview-drawn"
                    (reset! drawn* true)

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
                    (let [landscape (unchecked-get data "landscape")]
                      (some->> (unchecked-get data "frameId") uuid/parse* (reset! shown*))
                      (reset! place* {:index (unchecked-get data "index")
                                      :count (unchecked-get data "count")})
                      (reset! landscape* (when (boolean? landscape) landscape)))

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
                                :framed framed?
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
       (when ^boolean turns?
         [:> icon-button* {:variant "ghost"
                           :aria-label (tr "workspace.preview.turn")
                           :icon i/turn-device
                           :on-click on-turn}])
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "viewer.header.device")
                         :aria-haspopup "menu"
                         :aria-expanded device-menu?
                         :class (stl/css-case :menu-open device-menu?)
                         :icon "device"
                         :on-key-down keep-enter
                         :on-click toggle-device-menu}]
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
     [:> dropdown-menu* {:show device-menu?
                         :on-close close-device-menu
                         :class (stl/css :menu)
                         :style #js {"top" (dm/str (+ (:y frame) bar-height) "px")
                                     "right" (dm/str (- vw (:x frame) (:w frame) -8) "px")}}
      [:> device-items* {:choice device
                         :finish color
                         :on-choice on-device
                         :on-finish on-device-color}]]
     [:> dropdown-menu* {:show menu?
                         :on-close close-menu
                         :class (stl/css :menu)
                         :style #js {"top" (dm/str (+ (:y frame) bar-height) "px")
                                     "right" (dm/str (- vw (:x frame) (:w frame) -8) "px")}}
      ;; a device shows the board as wide as its screen: no zoom
      (when-not framed?
        [:*
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
               :role "separator"}]])
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
     ;; the background of the prototype round a board without a device,
     ;; as the view mode in it shows it, while it loads
     [:div {:class (stl/css :content)
            :style (when (and (not framed?) (some? background))
                     #js {:backgroundColor background})}
      (when ready?
        [:iframe {:key (deref run*)
                  :ref iframe-ref
                  :class (stl/css-case :frame true
                                       :frame-loading (not drawn?))
                  :src uri
                  :title (tr "workspace.preview.title")
                  :allow "autoplay"}])
      (when-not ^boolean drawn?
        [:> loading* {:device device-shown
                      :layout (when framed? (device-layout device-shown frame))
                      :finish color
                      :label (:name board)
                      :title (if ready? (tr "labels.loading") (tr "workspace.preview.saving"))}])
      ;; a device alone scales from anywhere on its frame
      (when (and ^boolean framed? ^boolean ready?)
        (for [[mode {:keys [x y w h]}] (grips device-shown frame)]
          [:div {:key (name mode)
                 :class (stl/css-case :grip true
                                      :grip-across (contains? #{:left :right} mode)
                                      :grip-down (contains? #{:top :bottom} mode)
                                      :grip-falling (contains? #{:top-left :bottom-right} mode)
                                      :grip-rising (contains? #{:top-right :bottom-left} mode))
                 :style #js {"left" (dm/str x "px")
                             "top" (dm/str y "px")
                             "width" (dm/str (max 0 w) "px")
                             "height" (dm/str (max 0 h) "px")}
                 :data-mode (name mode)
                 :title (tr "workspace.preview.resize")
                 :on-pointer-down on-down
                 :on-pointer-move on-move
                 :on-lost-pointer-capture on-up}]))]
     (when-not ^boolean framed?
       (for [[corner class] corners]
         [:div {:key corner
                :class class
                :data-mode corner
                :title (tr "workspace.preview.resize")
                :on-pointer-down on-down
                :on-pointer-move on-move
                :on-lost-pointer-capture on-up}]))]))

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
