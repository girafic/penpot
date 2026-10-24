;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes.bounds :as gsb]
   [app.common.logic.timelines :as cltl]
   [app.common.transit :as t]
   [app.common.types.animation :as cta]
   [app.common.types.shape.interactions :as ctsi]
   [app.common.types.text :as txt]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.features :as features]
   [app.main.fonts :as fonts]
   [app.main.refs :as refs]
   [app.main.render-viewer-wasm :as rwv]
   [app.main.store :as st]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.foundations.assets.icon :refer [icon*] :as i]
   [app.main.ui.ds.product.loader :refer [loader*]]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.icons :as deprecated-icon]
   [app.main.ui.modal :refer [modal-container*]]
   [app.main.ui.viewer.comments :refer [comments-layer comments-sidebar*]]
   [app.main.ui.viewer.header :as header]
   [app.main.ui.viewer.inspect :as inspect]
   [app.main.ui.viewer.interactions :as interactions]
   [app.main.ui.viewer.login]
   [app.main.ui.viewer.share-link]
   [app.main.ui.viewer.thumbnails :refer [thumbnails-panel*]]
   [app.util.audio :as audio]
   [app.util.dom :as dom]
   [app.util.dom.normalize-wheel :as nw]
   [app.util.globals :as globals]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.object :as obj]
   [app.util.webapi :as wapi]
   [cuerdas.core :as str]
   [goog.events :as events]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private wasm-transition-wait-ms
  "How long, at most, the change of board waits for WASM to draw the
  board that comes in before it plays."
  600)

(def ^:private wasm-first-draw-wait-ms
  "How long, at most, a board that shows without a change of board (the
  first one, once WASM and the fonts and images of the page load) waits
  to be drawn before it plays."
  8000)

(def current-animations-ref
  (l/derived :viewer-animations st/state))

(def current-overlays-ref
  (l/derived :viewer-overlays st/state))

(def ^:private viewer-timeline-ref
  (l/derived #(dm/get-in % [:viewer-local :timeline]) st/state))

(def ^:private viewer-local-ref
  "The local state of the view mode but the playback of the animation
  (see `viewer-timeline-ref`), which changes at each of its frames."
  (l/derived #(dissoc % :timeline) refs/viewer-local =))

(defn- animation-modifiers
  "When a timeline is active, the modif-tree of the shapes of `page` at
  the current playback time: the timeline of the board at its time and
  the ones of the component copies in it, which play the animation of
  their mains, each on its own clock (see `cltl/view-animation-tree`;
  `files` holds the files of the components). It covers transforms and
  the other animated attributes (as change-property modifiers).
  Otherwise nil."
  [page files {:keys [timeline-id time elapsed] :as _viewer-timeline}]
  (when (some? timeline-id)
    (not-empty (cltl/view-animation-tree page files timeline-id (or time 0) (or elapsed time 0)))))

(defn- calculate-size
  "Calculate the total size we must reserve for the frame, including possible paddings
   added because shadows, blur, or strokes."
  [objects frame zoom]
  (let [{:keys [x y width height]} (gsb/get-object-bounds objects frame {:ignore-margin? false})]
    {:base-width  width
     :base-height height
     :x           x
     :y           y
     :width       (* width zoom)
     :height      (* height zoom)
     :vbox        (dm/fmt "% % % %" 0 0 width height)}))

(defn calculate-delta
  "Calculate the displacement we need to apply so that the original selrect appears in the
   same position as if it had no extra paddings, depending on the side the frame will
   be snapped to."
  [size selrect [snap-v snap-h] zoom]
  (let [delta-x (case snap-h
                  :left   (- (:x1 selrect) (:x size))
                  :right  (- (:x2 selrect) (+ (:x size) (/ (:width size) zoom)))
                  :center (- (/ (- (:width selrect) (/ (:width size) zoom)) 2)
                             (- (:x size) (:x1 selrect))))
        delta-y (case snap-v
                  :top    (- (:y1 selrect) (:y size))
                  :bottom (- (:y2 selrect) (+ (:y size) (/ (:height size) zoom)))
                  :center (- (/ (- (:height selrect) (/ (:height size) zoom)) 2)
                             (- (:y size) (:y1 selrect))))]
    (gpt/point (* delta-x zoom) (* delta-y zoom))))

(defn- calculate-wrapper
  [size1 size2 zoom]
  (cond
    (nil? size1) size2
    (nil? size2) size1
    :else (let [width  (max (:base-width size1) (:base-width size2))
                height (max (:base-height size1) (:base-height size2))]
            {:width  (* width zoom)
             :height (* height zoom)
             :vbox   (str "0 0 " width " " height)})))

(def ^:private ref:timeline-playing?
  (l/derived #(boolean (dm/get-in % [:viewer-local :timeline :playing?])) st/state))

(def ^:private ref:audio-muted?
  (l/derived dv/audio-muted? st/state))

(mf/defc timeline-controls*
  "Play, pause and stop the animation of the board `board-id` (Penpot
  Motion): its timeline, or the animations of the component copies in
  it (see `dv/board-clock`)."
  {::mf/private true}
  [{:keys [board-id sound?]}]
  (let [playing? (mf/deref ref:timeline-playing?)
        muted?   (mf/deref ref:audio-muted?)

        ;; a page may start sound only from a user gesture: this one
        on-toggle
        (mf/use-fn
         (mf/deps board-id)
         #(do (audio/unlock!)
              (st/emit! (dv/toggle-play-timeline board-id))))

        on-toggle-sound
        (mf/use-fn #(do (audio/unlock!)
                        (st/emit! (dv/toggle-audio-muted))))

        on-stop
        (mf/use-fn #(st/emit! (dv/stop-timeline)))]

    ;; A click here is not one on the view (see `on-click` of the viewer).
    [:div {:class (stl/css :timeline-controls)
           :on-click dom/stop-propagation}
     [:button {:type "button"
               :class (stl/css :timeline-button)
               :title (tr "workspace.animation.play")
               :aria-pressed playing?
               :on-click on-toggle}
      (if playing?
        [:span {:class (stl/css :pause-icon)}]
        [:> icon* {:icon-id i/play :size "s"}])]
     [:button {:type "button"
               :class (stl/css :timeline-button)
               :title (tr "workspace.animation.rewind")
               :on-click on-stop}
      [:span {:class (stl/css :stop-icon)}]]
     (when sound?
       [:button {:type "button"
                 :class (stl/css :timeline-button)
                 :title (if muted?
                          (tr "workspace.animation.unmute")
                          (tr "workspace.animation.mute"))
                 :aria-pressed (not muted?)
                 :on-click on-toggle-sound}
        [:> icon* {:icon-id (if muted? i/volume-off i/volume) :size "s"}]])]))

(mf/defc viewer-pagination
  [{:keys [index num-frames left-bar right-bar comment-sidebar animated sound board-id] :as props}]
  (let [go-prev-frame  (mf/use-fn #(st/emit! dv/select-prev-frame))
        go-next-frame  (mf/use-fn #(st/emit! dv/select-next-frame))
        go-first-frame (mf/use-fn #(st/emit! dv/select-first-frame))]
    [:*
     (when (pos? index)
       [:button {:class (stl/css-case :viewer-go-prev true
                                      :left-bar left-bar)
                 :on-click go-prev-frame
                 :aria-label (tr "labels.previous")}
        deprecated-icon/arrow])
     (when (< (+ index 1) num-frames)
       [:button {:class (stl/css-case :viewer-go-next  true
                                      :comment-sidebar comment-sidebar
                                      :right-bar right-bar)
                 :on-click go-next-frame
                 :aria-label (tr "labels.next")}
        deprecated-icon/arrow])
     [:div {:class (stl/css-case :viewer-bottom true
                                 :left-bar left-bar)}
      [:button {:on-click go-first-frame
                :class (stl/css :reset-button)}
       deprecated-icon/reload]
      [:div {:class (stl/css :bottom-center)}
       [:span {:class (stl/css :counter)}
        (str/join " / " [(+ index 1) num-frames])]
       (when animated
         [:> timeline-controls* {:board-id board-id :sound? sound}])]
      [:span]]]))

(mf/defc viewer-pagination-and-sidebar
  {::mf/wrap [mf/memo]}
  [{:keys [section index users frame page animated sound]}]
  (let [comments-local  (mf/deref refs/comments-local)
        show-sidebar?   (and (= section :comments) (:show-sidebar? comments-local))]
    [:*
     [:& viewer-pagination
      {:index index
       :num-frames (count (:frames page))
       :comment-sidebar show-sidebar?
       :animated animated
       :sound sound
       :board-id (:id frame)}]

     (when show-sidebar?
       [:> comments-sidebar*
        {:profiles users
         :frame frame
         :page page}])]))

(mf/defc viewer-overlay
  [{:keys [overlay page frame zoom wrapper-size interactions-mode]}]
  (let [close-click-outside? (:close-click-outside overlay)
        background-overlay?  (:background-overlay overlay)
        overlay-frame        (:frame overlay)
        overlay-position     (:position overlay)
        fixed-base?          (:fixed-source? overlay)

        size
        (mf/with-memo [page overlay zoom]
          (calculate-size (:objects page) (:frame overlay) zoom))

        delta
        (mf/with-memo [size overlay-frame overlay zoom]
          (calculate-delta size (:selrect overlay-frame) (:snap-to overlay) zoom))

        on-click
        (mf/use-fn
         (mf/deps overlay close-click-outside?)
         (fn [_]
           (when close-click-outside?
             (if-let [animation (:animation overlay)]
               (st/emit! (dv/close-overlay (:id overlay) (ctsi/invert-direction animation)))
               (st/emit! (dv/close-overlay (:id overlay)))))))]

    [:*
     (when (or close-click-outside? background-overlay?)
       [:div {:class (stl/css-case  :viewer-overlay-background true
                                    :visible background-overlay?)
              :style {:width (:width wrapper-size)
                      :height (:height wrapper-size)
                      :position "absolute"
                      :left 0
                      :top 0}
              :on-click on-click}])

     (if fixed-base?
       [:div {:class (stl/css :viewport-container-wrapper)
              :style {:position "absolute"
                      :left (* (:x overlay-position) zoom)
                      :top (* (:y overlay-position) zoom)
                      :width (:width size)
                      :height (:height size)
                      :z-index 2}}
        [:div {:class (stl/css :viewer-overlay :viewport-container)
               :id (dm/str "overlay-" (:id overlay-frame))
               :style {:width (:width size)
                       :height (:height size)
                       :position "fixed"}}
         [:> interactions/viewport*
          {:frame overlay-frame
           :base-frame frame
           :frame-offset overlay-position
           :size size
           :delta delta
           :page page
           :interactions-mode interactions-mode}]]]

       [:div {:class (stl/css :viewer-overlay :viewport-container)
              :id (dm/str "overlay-" (:id overlay-frame))
              :style {:width (:width size)
                      :height (:height size)
                      :left (* (:x overlay-position) zoom)
                      :top (* (:y overlay-position) zoom)}}
        [:> interactions/viewport*
         {:frame overlay-frame
          :base-frame frame
          :frame-offset overlay-position
          :size size
          :delta delta
          :page page
          :interactions-mode interactions-mode}]])]))

(mf/defc animated-viewport*
  "The viewport of the board `frame` of `page`, which plays the animation
  of the view mode (see `animation-modifiers`) on its own: it takes the
  modif-tree at the playback time from a ref, so neither this nor the
  rest of the view mode renders again at each frame."
  {::mf/private true}
  [{:keys [page frame size interactions-mode]}]
  (let [files         (mf/deref refs/files)
        ;; A delay: a frame the viewport does not get to is not worked out.
        modifiers-ref (mf/with-memo [page files]
                        (l/derived #(delay (animation-modifiers page files %))
                                   viewer-timeline-ref))]
    [:> interactions/viewport*
     {:frame frame
      :base-frame frame
      :frame-offset (gpt/point 0 0)
      :size size
      :page page
      :modifiers-ref modifiers-ref
      :interactions-mode interactions-mode}]))

(mf/defc viewer-wrapper
  {::mf/wrap-props false}
  [{:keys [wrapper-size orig-frame orig-viewport-ref orig-size page file users current-viewport-ref
           size frame interactions-mode overlays zoom section index animated sound]}]

  [:*
   [:& viewer-pagination-and-sidebar
    {:section section
     :index index
     :page page
     :users users
     :frame frame
     :animated animated
     :sound sound
     :interactions-mode interactions-mode}]

   [:div {:class (stl/css :viewer-wrapper)
          :style {:width (:width wrapper-size)
                  :height (:height wrapper-size)}}
    [:div {:class (stl/css :viewer-clipper)}

     (when orig-frame
       [:div {:class (stl/css :viewport-container)
              :ref orig-viewport-ref
              :style {:width (:width orig-size)
                      :height (:height orig-size)
                      :position "relative"}}

        [:> interactions/viewport*
         {:frame orig-frame
          :base-frame orig-frame
          :frame-offset (gpt/point 0 0)
          :size orig-size
          :page page
          :users users
          :interactions-mode interactions-mode
          ;; WASM keeps the picture of the board as it left
          :is-snapshot true}]])

     [:div {:class (stl/css :viewport-container)
            :ref current-viewport-ref
            :style {:width (:width size)
                    :height (:height size)
                    :position "relative"}}

      [:> animated-viewport*
       {:frame frame
        :size size
        :page page
        :interactions-mode interactions-mode}]

      (for [overlay overlays]
        [:& viewer-overlay
         {:overlay overlay
          :key (dm/str (:id overlay))
          :page page
          :frame frame
          :zoom zoom
          :wrapper-size wrapper-size
          :interactions-mode interactions-mode}])]]


    (when (= section :comments)
      [:& comments-layer {:file file
                          :users users
                          :frame frame
                          :page page
                          :zoom zoom}])]])

(mf/defc viewer-content*
  [{:keys [data page-id share-id section index interactions-mode share preview]}]
  (let [{:keys [file users project permissions]} data
        allowed (or
                 (= section :interactions)
                 (and (= section :comments)
                      (or (:can-edit permissions)
                          (and (true? (:is-logged permissions))
                               (= (:who-comment permissions) "all"))))
                 (and (= section :inspect)
                      (or (:can-edit permissions)
                          (and (true? (:is-logged permissions))
                               (= (:who-inspect permissions) "all")))))

        local (mf/deref viewer-local-ref)

        nav-scroll (:nav-scroll local)
        orig-viewport-ref    (mf/use-ref nil)
        current-viewport-ref (mf/use-ref nil)
        wasm?                (and (features/use-feature "render-wasm/v1")
                                  (contains? cf/flags :available-viewer-wasm))
        ;; the change of board hidden and started last, and when the
        ;; board that comes in shows (see below)
        hidden-nav-ref       (mf/use-ref nil)
        started-nav-ref      (mf/use-ref nil)
        reveal-ref           (mf/use-ref nil)
        revealing            (fn [frame-id]
                               (let [{:keys [promise] :as reveal} (mf/ref-val reveal-ref)]
                                 (when (= frame-id (:frame-id reveal))
                                   promise)))
        viewer-section-ref   (mf/use-ref nil)

        current-animations (mf/deref current-animations-ref)

        page-id (or page-id (-> file :data :pages first))

        base-page (mf/use-memo
                   (mf/deps data page-id)
                   (fn []
                     (get-in data [:pages page-id])))

        ;; The page as it is: only the viewport of the board plays its
        ;; animation (see `animated-viewport*`), so the header, the
        ;; thumbnails and the rest do not render again at each frame.
        files (mf/deref refs/files)
        page  base-page

        text-shapes
        (hooks/use-equal-memo
         (->> (:objects page)
              (vals)
              (filter cfh/text-shape?)))

        zoom      (:zoom local)
        zoom-type (:zoom-type local)

        frames    (:frames page)
        frame     (get frames index)

        ;; What the board on screen plays on, its timeline or the
        ;; animations of the component copies in it (see `dv/board-clock`).
        clock     (mf/with-memo [base-page files (:id frame)]
                    (when (some? frame)
                      (dv/board-clock base-page files (:id frame))))
        ;; whether it plays from its start as it shows (see below)
        autoplay? (and (some? clock) (cta/autoplay? clock) (= section :interactions))

        ;; whether the board on screen, and a board of the page, plays
        ;; sound, its own or the one of the mains of its component copies
        board-sound? (mf/with-memo [base-page files (:id frame)]
                       (and (some? frame) (dv/board-sound? base-page files (:id frame))))
        sound?    (mf/with-memo [base-page files frames]
                    (or (some (comp seq :audio-clips) (vals (:timelines base-page)))
                        (some #(dv/board-sound? base-page files (:id %)) frames)))

        fullscreen? (mf/deref header/fullscreen-ref)
        overlays    (mf/deref current-overlays-ref)
        ;; the preview shows in its bar whether it plays (see below)
        playing?    (mf/deref ref:timeline-playing?)
        muted?      (mf/deref ref:audio-muted?)

        orig-frame
        (mf/with-memo [current-animations]
          ;; We assume there can only be one animation with origin (this is used only in
          ;; navigation animations, and we cannot navigate to two different destinations
          ;; at the same time).
          (let [animation-with-origin (d/seek :orig-frame-id (vals current-animations))]
            (when animation-with-origin
              (d/seek #(= (:id %) (:orig-frame-id animation-with-origin)) frames))))

        size
        (mf/with-memo [frame zoom]
          (when frame
            (calculate-size (:objects page) frame zoom)))

        orig-size
        (mf/with-memo [orig-frame zoom]
          (when orig-frame
            (calculate-size (:objects page) orig-frame zoom)))

        wrapper-size
        (mf/with-memo [size orig-size zoom]
          (calculate-wrapper size orig-size zoom))

        click-on-screen
        (mf/use-fn
         (fn [event]
           (let [origin (dom/get-target event)
                 over-section? (dom/get-data origin "viewer-section")
                 layout (dom/get-element "viewer-layout")
                 has-force? (dom/get-data layout "force-visible")]

             (when over-section?
               (if (= has-force? "true")
                 (dom/set-data! layout "force-visible" false)
                 (dom/set-data! layout "force-visible" true))))))

        on-click
        (mf/use-fn
         (mf/deps section)
         (fn [_]
           (when (= section :comments)
             (st/emit! (dcm/close-thread)))))

        set-up-new-size
        (mf/use-fn
         (fn [_]
           (let [viewer-section (dom/get-element "viewer-section")
                 size (dom/get-client-size viewer-section)]
             (st/emit! (dv/set-viewport-size {:size size})))))

        on-wheel
        (mf/use-fn
         (fn [event]
           (let [event  (.getBrowserEvent ^js event)
                 wrapper (dom/get-element "inspect-svg-wrapper")
                 section (dom/get-element "inspect-svg-container")
                 target (.-target event)]
             (when (or (dom/child? target wrapper) (dom/id? target "inspect-svg-container"))
               (let [norm-event ^js (nw/normalize-wheel event)
                     mod? (kbd/mod? event)
                     shift? (kbd/shift? event)
                     delta (.-pixelY norm-event)
                     scroll-pos (if shift?
                                  (dom/get-h-scroll-pos section)
                                  (dom/get-scroll-pos section))
                     new-scroll-pos (+ scroll-pos delta)]
                 (when-not mod?
                   (do
                     (dom/prevent-default event)
                     (dom/stop-propagation event)
                     (if shift?
                       (dom/set-h-scroll-pos! section new-scroll-pos)
                       (dom/set-scroll-pos! section new-scroll-pos)))))))))
        on-thumbnails-close
        (mf/use-fn
         #(st/emit! dv/close-thumbnails-panel))


        on-exit-fullscreen
        (mf/use-fn
         (fn []
           (when (not (dom/fullscreen?))
             (st/emit! (dv/exit-fullscreen)))))]

    (hooks/use-shortcuts ::viewer sc/shortcuts :viewer)
    (when (nil? page)
      (ex/raise :type :not-found))

    (mf/with-effect []
      (when (not allowed)
        (st/emit! (dv/go-to-section :interactions))))

    ;; The board on screen changing, or the view mode left, stops what
    ;; plays and its sound. Shown in the prototype, a board that animates
    ;; plays from its start, as a slide of a presentation does, unless its
    ;; timeline says not to (see `cta/autoplay?`).
    (mf/with-effect [(:id frame) (some? clock) section]
      (let [alive? (volatile! true)]
        (when (and (some? clock) (cta/autoplay? clock) (= section :interactions))
          (let [board-id (:id frame)
                ;; not while the preview is hidden: it plays as it shows
                play     #(when (and @alive? (not (dv/preview-hidden? @st/state)))
                            (st/emit! (dv/seek-timeline 0) (dv/play-timeline board-id)))
                ;; with sound, once the sound can play (see below); the
                ;; board the view mode opens with comes without a
                ;; gesture, but the browser may let it sound (its
                ;; autoplay policy), else it sounds from the first one
                start    #(if board-sound?
                            (do (audio/unlock!)
                                (audio/when-running play 200))
                            (play))]
            ;; a board that comes in hidden, once it shows (see above):
            ;; its sound and animation come with it
            (if-let [reveal (revealing board-id)]
              (.then reveal start)
              (if wasm?
                ;; else once WASM draws it, as the first board, which
                ;; shows meanwhile at the start of its animation
                (do (st/emit! (dv/cue-timeline board-id))
                    (.then (rwv/when-frame-rendered (mf/ref-val current-viewport-ref) board-id
                                                    wasm-first-draw-wait-ms)
                           start))
                (start)))))
        #(do (vreset! alive? false)
             (st/emit! (dv/pause-timeline)))))

    ;; A page may start sound only from a user gesture: a click or a key
    ;; that shows a board (a prototype link, the arrows) lets its sound
    ;; play with it. Captured, before the link handles the gesture.
    (mf/with-effect [sound?]
      (when sound?
        (let [unlock (fn [_] (audio/unlock!))
              keys   [(events/listen globals/window "pointerdown" unlock true)
                      (events/listen globals/window "keydown" unlock true)]]
          #(doseq [key keys]
             (events/unlistenByKey key)))))

    ;; As the preview of the prototype in the workspace (see
    ;; `app.main.ui.workspace.prototype-preview`): the board fitted in its
    ;; window as it resizes, the screens in a history of their own, and
    ;; what the workspace asks of it (to show what is edited there as it
    ;; is made, or what it saved, to go to the board before or after, to
    ;; the board selected, a zoom, how the interactions show); it tells
    ;; the workspace once it listens, which board it shows, and when it
    ;; could not show what was edited. Shift + Space in it closes it, as
    ;; in the workspace.
    (mf/with-effect [preview index (:id frame) (count frames)]
      (when (and preview (some? index))
        (st/emit! (dv/preview-screen index))
        (when (some? frame)
          ;; where it is among the boards, for the arrows of the bar
          (.postMessage (.-parent js/window)
                        #js {:type "penpot:preview-shown"
                             :frameId (dm/str (:id frame))
                             :index index
                             :count (count frames)}
                        (.. js/window -location -origin)))))

    ;; the bar of the preview plays, pauses and stops the animation of the
    ;; board it shows, and turns its sound on and off: it is told whether
    ;; the board animates and sounds, and whether it plays, and asks for
    ;; it here. Closed, the preview stays loaded, hidden: it stops what
    ;; plays and its sound, and shown again, the board plays from its
    ;; start, as when it opened (see above)
    (mf/with-effect [preview (:id frame) (some? clock) playing? board-sound? muted?]
      (when (and preview (some? frame))
        (.postMessage (.-parent js/window)
                      #js {:type "penpot:preview-playback"
                           :animated (some? clock)
                           :playing (boolean playing?)
                           :sound (boolean board-sound?)
                           :muted (boolean muted?)}
                      (.. js/window -location -origin))))

    (mf/with-effect [preview (:id frame) autoplay? board-sound?]
      (when (and preview (some? frame))
        (let [board-id (:id frame)
              parent   (.-parent js/window)
              origin   (.. js/window -location -origin)

              ;; as it shows, unless it hid again meanwhile (the sound
              ;; may make it wait)
              replay
              (fn []
                (let [play #(when-not (dv/preview-hidden? @st/state)
                              (st/emit! (dv/seek-timeline 0) (dv/play-timeline board-id)))]
                  (if board-sound?
                    (do (audio/unlock!)
                        (audio/when-running play 200))
                    (play))))

              on-message
              (fn [^js event]
                (when (and (identical? (.-source event) parent)
                           (= (.-origin event) origin))
                  (case (some-> (.-data event) (unchecked-get "type"))
                    "penpot:preview-play"  (do (audio/unlock!)
                                               (st/emit! (dv/toggle-play-timeline board-id)))
                    "penpot:preview-stop"  (st/emit! (dv/stop-timeline))
                    "penpot:preview-sound" (do (audio/unlock!)
                                               (st/emit! (dv/toggle-audio-muted)))
                    "penpot:preview-hide"  (st/emit! (dv/preview-hidden true))
                    "penpot:preview-show"  (do (st/emit! (dv/preview-hidden false))
                                               (when autoplay? (replay)))
                    nil)))]
          (.addEventListener js/window "message" on-message)
          #(.removeEventListener js/window "message" on-message))))

    ;; fitted or filled as it resizes; at 100 % as it is
    (mf/with-effect [preview (:viewport-size local) (:id frame)]
      (when (and preview (some? frame))
        (st/emit! (dv/refit-preview))))

    (mf/with-effect [preview]
      (when preview
        (let [parent (.-parent js/window)
              origin (.. js/window -location -origin)

              on-message
              (fn [^js event]
                (when (and (identical? (.-source event) parent)
                           (= (.-origin event) origin))
                  (let [data (.-data event)]
                    (case (some-> data (unchecked-get "type"))
                      "penpot:preview-refresh"
                      (st/emit! (dv/refresh-bundle #(.postMessage parent #js {:type "penpot:preview-refreshed"} origin)))
                      ;; what is edited in the workspace, as it is made
                      "penpot:preview-changes"
                      (let [on-stale #(.postMessage parent #js {:type "penpot:preview-stale"} origin)
                            changes  (try
                                       (t/decode-str (unchecked-get data "changes"))
                                       (catch :default _ nil))]
                        (if (vector? changes)
                          (st/emit! (dv/apply-preview-changes changes on-stale))
                          (on-stale)))
                      "penpot:preview-prev"    (st/emit! (dv/preview-board :prev))
                      "penpot:preview-next"    (st/emit! (dv/preview-board :next))
                      ;; the board selected in the workspace
                      "penpot:preview-go"      (some->> (unchecked-get data "frameId")
                                                        (uuid/parse*)
                                                        (dv/preview-go)
                                                        (st/emit!))
                      "penpot:preview-zoom"    (some-> (dv/preview-zoom (unchecked-get data "mode"))
                                                       (st/emit!))
                      "penpot:preview-interactions"
                      (let [mode (keyword (unchecked-get data "mode"))]
                        (when (contains? dv/valid-interaction-modes mode)
                          (st/emit! (dv/set-interactions-mode mode))))
                      nil))))

              on-key-down
              (fn [^js event]
                (when (and (kbd/space? event) (kbd/shift? event))
                  (dom/prevent-default event)
                  (.postMessage parent #js {:type "penpot:preview-close"} origin)))]
          (.addEventListener js/window "message" on-message)
          (.addEventListener js/window "keydown" on-key-down)
          ;; what the workspace asked before is sent again
          (.postMessage parent #js {:type "penpot:preview-ready"} origin)
          (fn []
            (.removeEventListener js/window "message" on-message)
            (.removeEventListener js/window "keydown" on-key-down)))))

    ;; Set the page title
    (mf/use-effect
     (mf/deps (:name file))
     (fn []
       (let [name (:name file)]
         (dom/set-html-title (str "\u25b6 " (tr "title.viewer" name))))))

    (mf/with-effect []
      (let [events
            [(events/listen globals/window "click" on-click)
             (events/listen (mf/ref-val viewer-section-ref) "wheel" on-wheel #js {"passive" false})]]

        (doseq [event dom/fullscreen-events]
          (.addEventListener globals/document event on-exit-fullscreen false))

        (fn []
          (doseq [key events]
            (events/unlistenByKey key))

          (doseq [event dom/fullscreen-events]
            (.removeEventListener globals/document event on-exit-fullscreen)))))

    (mf/use-effect
     (fn []
       (set-up-new-size)
       (.addEventListener js/window "resize" set-up-new-size)
       (fn []
         (.removeEventListener js/window "resize" set-up-new-size))))

    (mf/use-effect
     (mf/deps nav-scroll)
     (fn []
       ;; Set scroll position after navigate
       (when (number? nav-scroll)
         (let [viewer-section (dom/get-element "viewer-section")]
           (st/emit! (dv/reset-nav-scroll))
           (dom/set-scroll-pos! viewer-section nav-scroll)))))

    (mf/use-layout-effect
     (mf/deps fullscreen?)
     (fn []
       ;; Trigger dom fullscreen depending on our state
       (let [wrapper         (dom/get-element "viewer-layout")
             fullscreen-dom? (dom/fullscreen?)]
         (when (not= fullscreen? fullscreen-dom?)
           (if fullscreen?
             (let [layout (dom/get-element "viewer-layout")]
               (dom/set-data! layout "force-visible" false)
               (wapi/request-fullscreen wrapper))
             (wapi/exit-fullscreen))))))

    (mf/use-effect
     (mf/deps zoom-type)
     (fn []
       (case zoom-type
         :fit (st/emit! dv/zoom-to-fit)
         :fill (st/emit! dv/zoom-to-fill)
         nil)))

    ;; WASM draws a board after it shows (see
    ;; `rwv/use-viewer-wasm-viewport!`): when the board changes with an
    ;; animation, the one that leaves keeps the picture it had, taken
    ;; before any drawing ends, and the one that comes in waits hidden.
    (mf/with-layout-effect [index current-animations]
      (let [nav (d/seek #(= (:kind %) :go-to-frame) (vals current-animations))]
        ;; once for each change: the overlays that close meanwhile change
        ;; the animations again
        (when (and wasm? nav (not (identical? nav (mf/ref-val hidden-nav-ref))))
          (mf/set-ref-val! hidden-nav-ref nav)
          (when-let [current-viewport (mf/ref-val current-viewport-ref)]
            (rwv/copy-canvases! current-viewport (mf/ref-val orig-viewport-ref))
            (dom/set-style! current-viewport "visibility" "hidden")
            ;; A board that plays as it shows starts once it comes in (see
            ;; below), drawn meanwhile at the start of its animation.
            (when (and (some? clock) (cta/autoplay? clock) (= section :interactions))
              ;; the board that leaves stops now: a frame of it would set
              ;; its time on this one
              (st/emit! (dv/pause-timeline) (dv/cue-timeline (:id frame))))
            (mf/set-ref-val! reveal-ref
                             {:frame-id (:id frame)
                              :promise (rwv/when-frame-rendered current-viewport (:id frame)
                                                                wasm-transition-wait-ms)})))))

    (mf/use-effect
     (mf/deps index current-animations zoom-type)
     (fn []
       (case zoom-type
         :fit  (st/emit! dv/zoom-to-fit)
         :fill (st/emit! dv/zoom-to-fill)
         nil)
       ;; Navigate animation needs to be started after navigation
       ;; is complete, and we have the next page index.
       (let [nav-animation (d/seek #(= (:kind %) :go-to-frame) (vals current-animations))]
         (when (and nav-animation
                    (not (identical? nav-animation (mf/ref-val started-nav-ref))))
           (mf/set-ref-val! started-nav-ref nav-animation)
           (let [orig-viewport    (mf/ref-val orig-viewport-ref)
                 current-viewport (mf/ref-val current-viewport-ref)
                 start!           #(interactions/animate-go-to-frame
                                    (:animation nav-animation)
                                    current-viewport
                                    orig-viewport
                                    size
                                    orig-size
                                    wrapper-size)]
             (if-let [reveal (revealing (:id frame))]
               ;; once the board that comes in is drawn
               (.then reveal
                      (fn [_]
                        (dom/set-style! current-viewport "visibility" "")
                        (start!)))
               (start!)))))))

    (mf/use-effect
     (mf/deps current-animations)
     (fn []
       ;; Overlay animations may be started when needed.
       (when current-animations
         (doseq [[overlay-frame-id animation-vals] current-animations]
           (let [overlay-viewport (dom/get-element (str "overlay-" (str (:overlay-id animation-vals))))
                 overlay          (d/seek #(= (:id (:frame %)) overlay-frame-id)
                                          overlays)
                 overlay-size     (calculate-size (:objects page) (:frame overlay) zoom)
                 overlay-position {:x (* (:x (:position overlay)) zoom)
                                   :y (* (:y (:position overlay)) zoom)}
                 orig-frame       (when (:orig-frame-id animation-vals)
                                    (d/seek #(= (:id %) (:orig-frame-id animation-vals)) frames))
                 size             (calculate-size (:objects page) frame zoom)
                 orig-size        (when orig-frame
                                    (calculate-size (:objects page) orig-frame zoom))
                 wrapper-size     (calculate-wrapper size orig-size zoom)]
             (case (:kind animation-vals)
               :open-overlay
               (interactions/animate-open-overlay
                (:animation animation-vals)
                overlay-viewport
                wrapper-size
                overlay-size
                overlay-position)

               :close-overlay
               (interactions/animate-close-overlay
                (:animation animation-vals)
                overlay-viewport
                wrapper-size
                overlay-size
                overlay-position
                (:id (:frame overlay)))

               nil))))))

    (mf/use-effect
     (mf/deps text-shapes)
     (fn []
       (let [text-nodes (->> text-shapes (mapcat #(txt/node-seq txt/is-text-node? (:content %))))
             fonts (into #{} (keep :font-id) text-nodes)]
         (run! fonts/ensure-loaded! fonts))))

    [:div#viewer-layout
     {:class (stl/css-case
              :force-visible (:show-thumbnails local)
              :viewer-layout (not= section :inspect)
              :inspect-layout (= section :inspect))
      :data-fullscreen fullscreen?
      :data-force-visible (:show-thumbnails local)
      :data-preview (true? preview)}


     [:div {:class (stl/css :viewer-content)}


      [:button {:on-click on-thumbnails-close
                :class (stl/css-case :thumbnails-close true
                                     :invisible (not (:show-thumbnails local false)))}]

      [:> thumbnails-panel* {:frames frames
                             :show (:show-thumbnails local false)
                             :page page
                             :index index
                             :thumbnail-data (:thumbnails file)}]

      [:section#viewer-section {:ref viewer-section-ref
                                :data-viewer-section true
                                :class (stl/css-case :viewer-section true
                                                     :fullscreen fullscreen?)
                                :on-click click-on-screen}

       (cond
         (empty? frames)
         [:section {:class (stl/css :empty-state)}
          [:span (tr "viewer.empty-state")]]

         (nil? frame)
         [:section {:class (stl/css :empty-state)}
          (when (some? index)
            [:span (tr "viewer.frame-not-found")])]

         (some? frame)
         (if (= :inspect section)
           [:& inspect/viewport
            {:frame frame
             :page page
             :file file
             :section section
             :local local
             :size size
             :index index
             :viewer-pagination viewer-pagination
             :interactions-mode interactions-mode
             :share-id share-id}]

           [:& (mf/provider ctx/current-zoom) {:value zoom}
            [:& viewer-wrapper
             {:wrapper-size wrapper-size
              :orig-frame orig-frame
              :orig-viewport-ref orig-viewport-ref
              :orig-size orig-size
              :page page
              :file file
              :users users
              :current-viewport-ref current-viewport-ref
              :size size
              :frame frame
              :interactions-mode interactions-mode
              :overlays overlays
              :zoom zoom
              :section section
              :index index
              :animated (some? clock)
              :sound board-sound?}]]))]]

     (when-not preview
       [:& header/header {:project project
                          :index index
                          :file file
                          :page page
                          :frame frame
                          :permissions permissions
                          :zoom zoom
                          :section section
                          :shown-thumbnails (:show-thumbnails local)
                          :interactions-mode interactions-mode
                          :share share}])]))

;; --- Component: Viewer

(mf/defc viewer*
  [{:keys [file-id share-id page-id preview] :as props}]
  (mf/with-effect [file-id page-id share-id]
    (let [params {:file-id file-id
                  :page-id page-id
                  :share-id share-id}]
      ;; before it shows a screen, which it then shows in its own history
      (when preview
        (st/emit! (dv/start-preview)))
      (st/emit! (dv/initialize params))
      (fn []
        (st/emit! (dv/finalize params)))))

  (if-let [data (mf/deref refs/viewer-data)]
    (let [props (obj/merge props #js {:data data :key (dm/str file-id)})]
      [:*
       [:> modal-container*]
       [:> viewer-content* props]])

    [:> loader*  {:title (tr "labels.loading")
                  :overlay true}]))


(mf/defc viewer-page*
  {::mf/lazy-load true}
  [{:keys [file-id] :as props}]

  (mf/with-effect [file-id]
    (when-not (uuid? file-id)
      (ex/raise :type :not-found
                :code :missing-file-id
                :hint "provided file-id is invalid uuid or nil")))

  (when (uuid? file-id)
    [:> viewer* props]))
