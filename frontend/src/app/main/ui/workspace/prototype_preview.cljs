;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.prototype-preview
  "The preview of the prototype in a window over the workspace (Shift +
  Space, see `dwpp/toggle-preview`): the view mode of the page in a
  frame, once what is edited is saved, showing it again each time the
  workspace saves; a bar on top that moves the window, with back,
  forth, start again, a pin that makes it show the board selected in the
  workspace, the zoom (fit, fill or 100 %), open in the view mode and
  close, and the corners at its bottom that resize it. Where it is, as
  `{:x :y :w :h}` px of the browser window, the pin and the zoom are
  kept for the next time. The view mode tells it once it listens
  (`penpot:preview-ready`), what it asks before is sent then, and which
  board it shows (`penpot:preview-shown`), which the bar names."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.uuid :as uuid]
   [app.main.data.common :as dcm]
   [app.main.data.persistence :as dps]
   [app.main.data.workspace.prototype-preview :as dwpp]
   [app.main.refs :as refs]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.workspace.canvas-window :as cw]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:preview
  (l/derived dwpp/preview st/state))

(def bar-height
  "The bar on top of the window, which moves it, with its buttons."
  32)

(def ^:private border 1)

(def ^:private refresh-delay
  "Ms after the workspace saved before the preview shows it, so the saves
  of an edit come in at once."
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

(def zoom-modes
  "The zooms of the preview, in the order its button goes through them:
  the board fitted in the window, filling it, or at its own size."
  ["fit" "fill" "100"])

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
  [{:keys [file-id preview]}]
  (let [{:keys [page-id frame-id]} preview
        objects    (mf/deref refs/workspace-page-objects)
        current    (mf/deref refs/current-page-id)
        selected   (mf/deref refs/selected-shapes)
        ;; the board on screen, as the view mode tells it
        shown*     (mf/use-state frame-id)
        shown      (deref shown*)
        board      (get objects shown)
        ;; the board the view mode in the frame starts at
        start*     (mf/use-state frame-id)
        start      (deref start*)
        follow*    (mf/use-state #(get storage/user ::follow true))
        follow?    (deref follow*)
        zoom*      (mf/use-state #(get storage/user ::zoom "fit"))
        zoom       (deref zoom*)
        ;; the board of the selection the pin last asked for
        requested-ref (mf/use-ref frame-id)
        ;; whether the view mode in the frame listens (see `post!`)
        listening-ref (mf/use-ref false)
        [vw vh]    (cw/use-window-size)
        stored*    (mf/use-state #(get storage/user ::frame))
        drag-frame* (mf/use-state nil)
        frame      (cw/clamp-frame (or (deref drag-frame*)
                                       (deref stored*)
                                       (start-frame vw vh (:width board 16) (:height board 9)))
                                   vw vh)
        drag-ref   (mf/use-ref nil)
        iframe-ref (mf/use-ref nil)
        ;; the view mode loads once what is edited is saved
        ready*     (mf/use-state false)
        ready?     (deref ready*)
        ;; a new frame each time it starts again
        run*       (mf/use-state 0)
        ;; the zoom it starts at, as it starts again
        uri        (mf/with-memo [file-id page-id start (deref run*)]
                     (preview-uri file-id page-id start zoom))

        on-down
        (mf/use-fn
         (mf/deps frame)
         (fn [event]
           (let [target (dom/get-current-target event)]
             (dom/stop-propagation event)
             (dom/prevent-default event)
             (.setPointerCapture target (.-pointerId event))
             (mf/set-ref-val! drag-ref {:mode (keyword (dom/get-data target "mode"))
                                        :frame frame
                                        :x (.-clientX event)
                                        :y (.-clientY event)}))))

        on-move
        (mf/use-fn
         (fn [event]
           (when-let [{:keys [mode frame x y]} (mf/ref-val drag-ref)]
             (let [dx (- (.-clientX event) x)
                   dy (- (.-clientY event) y)]
               (reset! drag-frame* (if (= mode :move)
                                     (cw/moved frame dx dy)
                                     (cw/resized frame mode dx dy)))))))

        on-up
        (mf/use-fn
         (mf/deps vw vh)
         (fn [_]
           (when (some? (mf/ref-val drag-ref))
             (mf/set-ref-val! drag-ref nil)
             (when-let [frame (some-> (deref drag-frame*) (cw/clamp-frame vw vh))]
               (reset! stored* frame)
               (swap! storage/user assoc ::frame frame))
             (reset! drag-frame* nil))))

        ;; a double click on the bar puts it back where it starts, as
        ;; large as the board then fits
        on-reset
        (mf/use-fn
         (fn [_]
           (reset! stored* nil)
           (swap! storage/user dissoc ::frame)))

        on-back    (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-back"))
        on-forward (mf/use-fn #(post! (mf/ref-val iframe-ref) "penpot:preview-forward"))
        ;; pinned, at the board shown; else where it started
        on-restart (mf/use-fn
                    (mf/deps follow? shown frame-id)
                    (fn []
                      (let [start (if follow? shown frame-id)]
                        (reset! start* start)
                        (reset! shown* start)
                        (swap! run* inc))))

        on-follow
        (mf/use-fn
         (mf/deps follow?)
         (fn []
           (reset! follow* (not follow?))
           (swap! storage/user assoc ::follow (not follow?))))

        on-zoom
        (mf/use-fn
         (mf/deps zoom)
         (fn []
           (let [next (get zoom-modes (mod (inc (d/index-of zoom-modes zoom)) (count zoom-modes)))]
             (reset! zoom* next)
             (swap! storage/user assoc ::zoom next)
             (post! (mf/ref-val iframe-ref) "penpot:preview-zoom" {:mode next}))))
        on-close   (mf/use-fn #(st/emit! (dwpp/close-preview)))

        on-open
        (mf/use-fn
         (mf/deps page-id shown)
         #(st/emit! (dcm/go-to-viewer :page-id page-id :frame-id shown)))]

    ;; what is edited saved first, so it shows
    (mf/with-effect []
      (st/emit! ::dps/force-persist)
      (let [done (fn [] (reset! ready* true))
            sub  (rx/subs! (fn [_] nil) (fn [_] (done)) done (dps/wait-persisted 15000))]
        #(rx/dispose! sub)))

    ;; then each save of the workspace shows, shortly after it
    (mf/with-effect [ready?]
      (when ready?
        (let [timer (volatile! nil)
              key   (str (random-uuid))]
          (add-watch refs/persistence key
                     (fn [_ _ old new]
                       (when (and (not (dps/terminal-status? old))
                                  (dps/terminal-status? new))
                         (some-> @timer js/clearTimeout)
                         (vreset! timer (js/setTimeout #(post! (mf/ref-val iframe-ref) "penpot:preview-refresh")
                                                       refresh-delay)))))
          (fn []
            (remove-watch refs/persistence key)
            (some-> @timer js/clearTimeout)))))

    ;; a new board to start at, a new page
    (mf/with-effect [frame-id]
      (reset! shown* frame-id)
      (reset! start* frame-id)
      (mf/set-ref-val! requested-ref frame-id))

    ;; a new view mode in the frame listens once it says so
    (mf/with-effect [uri]
      (mf/set-ref-val! listening-ref false))

    ;; pinned, it shows the board of the selection: on another page it
    ;; starts again there; before the frame is there it starts there;
    ;; while the view mode listens it goes there; while it loads, it goes
    ;; there once it listens
    (mf/with-effect [follow? ready? selected current page-id objects]
      (when follow?
        (when-let [board-id (dwpp/start-frame-id objects selected)]
          (when (not= board-id (mf/ref-val requested-ref))
            (mf/set-ref-val! requested-ref board-id)
            (cond
              (not= current page-id)
              (st/emit! (dwpp/open-preview))

              (not ready?)
              (do (reset! start* board-id)
                  (reset! shown* board-id))

              (mf/ref-val listening-ref)
              (post! (mf/ref-val iframe-ref) "penpot:preview-go" {:frameId (dm/str board-id)}))))))

    ;; what the view mode tells: it listens, the board it shows; Shift +
    ;; Space in it closes it too
    (mf/with-effect [follow? zoom start]
      (let [on-message
            (fn [^js event]
              (let [iframe (mf/ref-val iframe-ref)
                    data   (.-data event)]
                (when (and (some? iframe)
                           (identical? (.-source event) (.-contentWindow iframe)))
                  (case (some-> data (unchecked-get "type"))
                    "penpot:preview-ready"
                    (let [requested (mf/ref-val requested-ref)]
                      (mf/set-ref-val! listening-ref true)
                      (post! iframe "penpot:preview-zoom" {:mode zoom})
                      (when (and follow? (some? requested) (not= requested start))
                        (post! iframe "penpot:preview-go" {:frameId (dm/str requested)})))

                    "penpot:preview-shown"
                    (some->> (unchecked-get data "frameId") uuid/parse* (reset! shown*))

                    "penpot:preview-close"
                    (st/emit! (dwpp/close-preview))

                    nil))))]
        (.addEventListener js/window "message" on-message)
        #(.removeEventListener js/window "message" on-message)))

    [:div {:class (stl/css-case :window true
                                :dragging (some? (deref drag-frame*)))
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
                         :aria-label (tr "workspace.preview.back")
                         :icon "arrow-left"
                         :on-click on-back}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.forward")
                         :icon "arrow-right"
                         :on-click on-forward}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.restart")
                         :icon "reload"
                         :on-click on-restart}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.follow")
                         :aria-pressed follow?
                         :class (stl/css-case :pinned follow?)
                         :icon "pin"
                         :on-click on-follow}]]
      [:span {:class (stl/css :title)}
       (or (:name board) (tr "workspace.preview.title"))]
      [:div {:class (stl/css :buttons)
             :on-pointer-down dom/stop-propagation
             :on-double-click dom/stop-propagation}
       [:button {:type "button"
                 :class (stl/css :zoom)
                 :title (tr "workspace.preview.zoom")
                 :on-click on-zoom}
        (case zoom
          "fill" (tr "workspace.preview.zoom-fill")
          "100"  "100%"
          (tr "workspace.preview.zoom-fit"))]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.open")
                         :icon "open-link"
                         :on-click on-open}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.preview.close")
                         :icon "close"
                         :on-click on-close}]]]
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

(mf/defc prototype-preview*
  "The preview of the prototype of the file `file-id`, while it is open."
  [{:keys [file-id]}]
  (when-let [preview (mf/deref ref:preview)]
    [:> preview-window* {:file-id file-id
                         :preview preview}]))
