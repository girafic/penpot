;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.render-viewer-wasm
  "WASM offscreen rendering for the shared viewer (snapshot + fixed layers)."
  (:require
   [app.common.exceptions :as ex]
   [app.common.files.helpers :as cfh]
   [app.common.render-wasm.api.select :as wselect]
   [app.common.render-wasm.wasm :as wasm]
   [app.common.types.shape.layout :as ctl]
   [app.main.data.viewer.scroll :as dvs]
   [app.main.ui.viewer.viewport-common :as vpc]
   [app.render-wasm.api :as wasm.api]
   [app.render-wasm.modifiers :as wasm.modifiers]
   [app.render-wasm.shape :as wasm.shape]
   [app.util.dom :as dom]
   [app.util.timers :as ts]
   [app.util.webapi :as webapi]
   [promesa.core :as p]
   [rumext.v2 :as mf]))

;; The WASM module is a single global instance; serialize offscreen work.
(defonce ^:private wasm-render-queue (atom (p/resolved nil)))

(defn- enqueue-wasm-render!
  [task]
  (let [next-p (-> @wasm-render-queue
                   (p/handle (fn [_ _] (task))))]
    (reset! wasm-render-queue (p/handle next-p (fn [_ _] nil)))
    next-p))

(defonce ^:private viewer-snapshot
  (atom {:os-canvas nil
         :page-key  nil
         :objects   nil
         :props     []
         :canvas-w  0
         :canvas-h  0
         :dpr       1}))

(defn- reset-viewer-snapshot! []
  (reset! viewer-snapshot
          {:os-canvas nil
           :page-key nil
           :objects nil
           :props []
           :canvas-w 0
           :canvas-h 0
           :dpr 1}))

(defn- show-animation!
  "Show over the shapes loaded in WASM from `objects` the animation
  `modif-tree` at a time (see `cltl/animation-tree`), as the workspace
  does (see `dwm/set-wasm-modifiers`): its transforms as modifiers, the
  other attributes it animates (opacity, colours...) as shape
  properties, with the ones the frame before set (`prev`) back first. A
  nil `modif-tree` shows the shapes as they are. With `shown?` WASM
  shows `prev` still, and the shapes it leaves as they are need no
  update. The containers scroll as `scroll` says (`:shifts` and
  `:sheet-root`, see `dvs/scroll-entries`), after WASM laid the shapes
  out. The attribute changes it made."
  [objects prev shown? modif-tree scroll]
  (let [changes (if (some? modif-tree)
                  (vec (wasm.modifiers/extract-property-changes modif-tree))
                  [])]
    (when (or (seq prev) (seq changes))
      (let [[changed shape-changes] (wasm.modifiers/property-changes objects prev changes shown?)]
        (doseq [[id properties] shape-changes]
          (wasm.shape/set-shape-properties! (get changed id) properties))))
    (wasm.api/clean-modifiers)
    (let [structure (when (seq modif-tree) (wasm.modifiers/parse-structure-modifiers modif-tree))
          geometry  (when (seq modif-tree) (wasm.modifiers/parse-geometry-modifiers modif-tree))
          _         (when (seq structure)
                      (wasm.api/set-structure-modifiers structure))
          entries   (if (seq geometry)
                      (let [propagated (wasm.api/propagate-modifiers geometry false nil)]
                        (if (seq propagated)
                          propagated
                          (into [] (map (fn [[id data]] [id (:transform data)])) geometry)))
                      [])
          entries   (dvs/scroll-entries objects entries (:shifts scroll)
                                        :sheet-root (:sheet-root scroll))]
      (when (seq entries)
        (wasm.api/set-modifiers entries :request-render? false)))
    changes))

(defn- draw-bitmap!
  "Blit the rendered OffscreenCanvas onto the visible 2D `canvas`. Firefox+NVIDIA
  can't `drawImage` a managed OffscreenCanvas directly (yields transparent
  pixels), but `drawImage` of an `ImageBitmap` works — so go through
  `createImageBitmap` (GPU-side, correct orientation, no CPU readback)."
  [canvas os-canvas object-id vis-w vis-h finish]
  (-> (js/createImageBitmap os-canvas)
      (p/then
       (fn [bitmap]
         (ts/raf
          (fn []
            (let [ctx2d (.getContext canvas "2d")]
              (.clearRect ctx2d 0 0 vis-w vis-h)
              (.drawImage ctx2d bitmap 0 0 vis-w vis-h)
              (.close bitmap)
              (dom/set-attribute! canvas "id" (str "screenshot-" object-id))
              (finish))))))
      (p/catch
       (fn [e]
         (finish)
         (ts/schedule
          (fn []
            (ex/raise :type :wasm-error
                      :code :viewer-canvas-failed
                      :hint "Viewer canvas failed"
                      :cause e)))))))

(defn- present-bitmap!
  "Show what was rendered into the OffscreenCanvas `os-canvas` on the
  visible `canvas` at once and without a copy: the frame moves over as an
  ImageBitmap (`transferToImageBitmap`) into the `bitmaprenderer` context
  of `canvas`, so an animation plays without waiting for a copy and a
  frame. A canvas that already draws in 2D goes on with `draw-bitmap!`."
  [canvas os-canvas object-id vis-w vis-h finish]
  (if-let [ctx (.getContext canvas "bitmaprenderer")]
    (do
      (.transferFromImageBitmap ^js ctx (.transferToImageBitmap ^js os-canvas))
      (dom/set-attribute! canvas "id" (str "screenshot-" object-id))
      (finish))
    (draw-bitmap! canvas os-canvas object-id vis-w vis-h finish)))

(defn- viewer-disable-wasm-ui-overlay!
  "Workspace WASM UI (rulers + rounded viewport frame) is composited in
  `present_frame`; the viewer must not show that chrome."
  []
  (wasm.api/set-rulers-frame-visible! false)
  (wasm.api/set-rulers-visible! false))

(defn- viewer-apply-layer-mask!
  [include-ids clear-fills-ids]
  (wasm.api/clear-render-include-filter!)
  (when (seq include-ids)
    (wasm.api/set-render-include-filter! include-ids))
  ;; the board under the layers fixed when scrolling or sticky: the pass
  ;; of what scrolls draws its fills, shadows and strokes, here they would
  ;; cover it (and move with a sticky layer)
  (doseq [id clear-fills-ids]
    (wselect/use-shape id)
    (wasm.api/clear-shape-fills!)
    (wasm.api/set-shape-shadows [])
    (wasm.api/set-shape-strokes id [] false)))

(defn- viewer-restore-layer-mask!
  [page-objects clear-fills-ids]
  (wasm.api/clear-render-include-filter!)
  (doseq [id clear-fills-ids]
    (wselect/use-shape id)
    (wasm.api/set-shape-fills id (get-in page-objects [id :fills] []) false)
    (wasm.api/set-shape-shadows (get-in page-objects [id :shadow] []))
    (wasm.api/set-shape-strokes id (get-in page-objects [id :strokes] []) false)))

(defn- viewer-do-render!
  "Render the board `object-id` into `os-canvas` and blit it onto
  `canvas`. `again?` when it rendered last with the same view and shapes,
  so only what changed since (an animation) draws again."
  [page-objects canvas os-canvas object-id vis-w vis-h scale size
   include-ids clear-fills-ids again? finish]
  (viewer-disable-wasm-ui-overlay!)
  (viewer-apply-layer-mask! include-ids clear-fills-ids)
  (if again?
    (wasm.api/render-sync-shape-again object-id)
    (do
      (wasm.api/set-viewer-viewport! scale size)
      (wasm.api/render-sync-shape object-id)))
  (viewer-restore-layer-mask! page-objects clear-fills-ids)
  (present-bitmap! canvas os-canvas object-id vis-w vis-h finish))

(defn- render-to-canvas*
  [objects canvas bounds scale object-id on-render]
  (p/create
   (fn [resolve _reject]
     (let [width        (.-width canvas)
           height       (.-height canvas)
           prev-disable @wasm/disable-request-render?
           finish       (fn []
                          (reset! wasm/disable-request-render? prev-disable)
                          (when (fn? on-render) (on-render))
                          (resolve nil))]
       (try
         (reset! wasm/disable-request-render? true)
         (let [os-canvas (js/OffscreenCanvas. width height)]
           (if (wasm.api/init-canvas-context os-canvas)
             (wasm.api/initialize-viewport
              objects scale bounds
              :background-opacity 0
              :force-sync true
              :on-render
              (fn []
                (viewer-disable-wasm-ui-overlay!)
                (wasm.api/render-sync-shape object-id)
                (draw-bitmap! canvas os-canvas object-id width height
                              (fn []
                                (wasm.api/clear-canvas {:lose-browser-context? false})
                                (reset-viewer-snapshot!)
                                (finish)))))
             (finish)))
         (catch :default e
           (js/console.error "Error initializing canvas context:" e)
           (finish)))))))

(defn render-to-canvas
  "One-shot WASM render into `canvas` (exports, thumbnails). Serialized globally."
  [objects canvas bounds scale object-id on-render]
  (enqueue-wasm-render!
   (fn []
     (render-to-canvas* objects canvas bounds scale object-id on-render))))

(defn- render-viewer-frame*
  "Render the board `object-id` of `page-objects` into `canvas`. The
  shapes load into WASM once per page; `modifiers`, an animation at a
  time (a delay of its modif-tree), shows over them (see
  `show-animation!`), so playing it does not load them again at each
  frame. `scroll`, how its containers scroll (see `show-animation!`)."
  [page-key page-objects canvas size scale object-id on-render
   {:keys [include-ids clear-fills-ids modifiers scroll] :or {clear-fills-ids #{}}}]
  (p/create
   (fn [resolve _reject]
     (let [prev-disable @wasm/disable-request-render?
           finish       (fn []
                          (reset! wasm/disable-request-render? prev-disable)
                          (when (fn? on-render) (on-render))
                          (resolve nil))
           vis-w        (.-width canvas)
           vis-h        (.-height canvas)
           dpr          (wasm.api/get-dpr)
           snap         @viewer-snapshot
           same-page?   (and (some? page-key) (identical? page-key (:page-key snap)))
           same-size?   (and (= vis-w (:canvas-w snap))
                             (= vis-h (:canvas-h snap))
                             (= dpr (:dpr snap)))
           os           (:os-canvas snap)
           ;; What the tile cache holds after a render: the next one with
           ;; the same can draw only what changed (see `viewer-do-render!`).
           ;; The passes of a board with fixed layers render other shapes
           ;; each: one draws only what changed when the same pass drew last,
           ;; as the one of what scrolls does as its containers scroll (see
           ;; `use-viewer-wasm-layers!`). The one that clears fills renders
           ;; all.
           render-key   (when (empty? clear-fills-ids)
                          [object-id scale size vis-w vis-h dpr include-ids])
           do-render!   (fn [os-canvas prev-props]
                          (let [;; no other writer used a shape since
                                ;; the last frame (see `wasm/shape-uses`)
                                shown? (= (:props-uses @viewer-snapshot) wasm/shape-uses)
                                props  (show-animation! page-objects prev-props shown? (force modifiers) scroll)
                                again? (and (some? render-key)
                                            (= render-key (:render-key @viewer-snapshot)))]
                            (swap! viewer-snapshot assoc :props props :props-uses wasm/shape-uses
                                   :render-key render-key)
                            (viewer-do-render! page-objects canvas os-canvas object-id
                                               vis-w vis-h scale size include-ids
                                               clear-fills-ids again? finish)))]

       (reset! wasm/disable-request-render? true)

       (try
         (if (and same-page? (wasm.api/initialized?) os)
           (do
             (when-not same-size?
               (wasm.api/resize-offscreen-canvas! os vis-w vis-h)
               (swap! viewer-snapshot assoc :canvas-w vis-w :canvas-h vis-h :dpr dpr))
             ;; While an animation plays the shapes stay loaded: only its
             ;; modifiers change (see `show-animation!`).
             (if (identical? page-objects (:objects snap))
               (do-render! os (:props snap))
               (do
                 (swap! viewer-snapshot assoc :objects page-objects :props [] :render-key nil)
                 (wasm.api/set-objects page-objects #(do-render! os []) nil true))))
           (let [os-canvas (js/OffscreenCanvas. vis-w vis-h)]
             (when (wasm.api/initialized?)
               (wasm.api/clear-canvas {:lose-browser-context? false}))
             (if (wasm.api/init-canvas-context os-canvas)
               (do
                 (reset! viewer-snapshot
                         {:os-canvas os-canvas
                          :page-key  page-key
                          :objects   page-objects
                          :props     []
                          :render-key nil
                          :canvas-w  vis-w
                          :canvas-h  vis-h
                          :dpr       dpr})
                 (wasm.api/initialize-viewport
                  page-objects scale size
                  :background-opacity 0
                  :force-sync true
                  :on-render #(do-render! os-canvas [])))
               (finish))))
         (catch :default e
           (js/console.error "viewer-snapshot: render error" e)
           (finish)))))))

(defn- use-viewer-dpr-key
  "Bump a counter when browser zoom changes devicePixelRatio so WASM canvases
  are resized like the workspace viewport."
  []
  (let [dpr-key (mf/use-state 0)]
    (mf/use-effect
     (mf/deps [])
     (fn []
       (webapi/on-dpr-change (fn [_] (swap! dpr-key inc)))))
    @dpr-key))

(def ^:private rendered-event
  "What a canvas of a viewer viewport hears once a board shows on it."
  "viewer-frame-rendered")

(defn- mark-rendered!
  "The board `frame-id` shows on `canvases` now (see `when-frame-rendered`)."
  [canvases frame-id]
  (doseq [^js canvas canvases]
    (set! (.. canvas -dataset -frame) (str frame-id))
    (.dispatchEvent canvas (js/CustomEvent. rendered-event #js {:bubbles true}))))

(defn when-frame-rendered
  "A promise resolved once the first canvas in `container`, the one of
  its viewport, shows the board `frame-id` (WASM draws a board after it
  is shown), or after `timeout-ms`."
  [^js container frame-id timeout-ms]
  (js/Promise.
   (fn [resolve]
     (let [shown? #(= (str frame-id) (some-> (dom/query container "canvas") .-dataset .-frame))]
       (if (or (nil? container) (shown?))
         (resolve nil)
         (let [timer (atom nil)]
           (letfn [(done [& _]
                     (.removeEventListener container rendered-event on-render)
                     (js/clearTimeout @timer)
                     (resolve nil))
                   (on-render [_]
                     (when (shown?) (done)))]
             (.addEventListener container rendered-event on-render)
             (reset! timer (js/setTimeout done timeout-ms)))))))))

(defn when-rendered
  "A promise resolved once a canvas in `container` shows a board again
  (see `when-frame-rendered`), or after `timeout-ms`."
  [^js container timeout-ms]
  (js/Promise.
   (fn [resolve]
     (if (nil? container)
       (resolve nil)
       (let [timer (atom nil)]
         (letfn [(done [& _]
                   (.removeEventListener container rendered-event done)
                   (js/clearTimeout @timer)
                   (resolve nil))]
           (.addEventListener container rendered-event done)
           (reset! timer (js/setTimeout done timeout-ms))))))))

(defn copy-canvases!
  "Draw what the canvases in `from` show onto the ones in `to`, each onto
  the one of the same pass (`data-pass`, see `use-viewer-wasm-viewport!`):
  a viewport that shows a board WASM has not drawn on it yet shows the
  picture of another, as it was. One without a counterpart is cleared."
  [^js from ^js to]
  (when (and from to)
    (let [sources (into {} (map (fn [^js c] [(.. c -dataset -pass) c])) (dom/query-all from "canvas"))]
      (doseq [^js dst (dom/query-all to "canvas")]
        (let [^js src (get sources (.. dst -dataset -pass))
              ctx     (.getContext dst "2d")]
          (.clearRect ctx 0 0 (.-width dst) (.-height dst))
          (when (and (some? src) (pos? (.-width src)) (pos? (.-height src)))
            (.drawImage ctx src 0 0 (.-width dst) (.-height dst))))))))

(defn- scroll-of
  "How the containers in the board `frame-id` of `objects` scroll for
  `scroll` (`{id {:offset :past}}`, see `dvs/view-offsets`): the shifts
  of their layers (see `dvs/view-shifts`) and `sheet-root`, the root of
  an overlay drawn back at its box (see `dvs/scroll-entries`).
  `cache-ref` keeps the last: a frame of an animation that does not
  scroll works out nothing."
  [cache-ref objects scroll frame-id sheet-root]
  (let [[c-objects c-scroll c-frame c-result] (mf/ref-val cache-ref)]
    (if (and (identical? c-objects objects) (identical? c-scroll scroll) (= c-frame frame-id))
      c-result
      (let [result {:shifts     (dvs/view-shifts objects scroll :root-id frame-id :overlay-root sheet-root)
                    :sheet-root sheet-root}]
        (mf/set-ref-val! cache-ref [objects scroll frame-id result])
        result))))

(defn moving-passes
  "The tags of the passes of `passes` with a container of `objects` in
  them (see `dvs/container-axes`): a scroll of it moves what they draw."
  [objects passes]
  (into #{}
        (comp (filter (fn [{:keys [include]}]
                        (some #(dvs/container-axes objects (get objects %)) include)))
              (map :tag))
        passes))

(defn- flow-root
  "The outermost board whose layout places the layer `id` of `objects`,
  through boards the layouts around them place in turn: a change of any
  layer in it may move that one. Nil when no layout places it."
  [objects id]
  (loop [id id root nil]
    (let [shape  (get objects id)
          parent (get objects (:parent-id shape))]
      (if (and (some? parent)
               (ctl/any-layout? parent)
               (not (ctl/position-absolute? shape)))
        (recur (:id parent) (:id parent))
        root))))

(defn pass-reach
  "The layers of `objects` whose animation changes what each still pass
  of `passes` draws, by tag: its layers (`:include`), those around them,
  and all in the outermost board whose layout places one of them (see
  `flow-root`), which may move it."
  [objects passes]
  (into {}
        (comp (filter :still)
              (map (fn [{:keys [tag include]}]
                     (let [roots (into #{} (keep #(flow-root objects %)) include)]
                       [tag (-> (set include)
                                (into (mapcat #(cfh/get-parent-ids objects %)) include)
                                (into (mapcat #(cfh/get-children-ids-with-self objects %)) roots))]))))
        passes))

(defn touched-passes
  "The tags of the still passes the animation `modif-tree` (a frame of
  it) changes a layer in the reach of (`reach`, see `pass-reach`)."
  [reach modif-tree]
  (if (empty? modif-tree)
    #{}
    (let [ids (keys modif-tree)]
      (into #{}
            (keep (fn [[tag layers]]
                    (when (some #(contains? layers %) ids)
                      tag)))
            reach))))

(defn due-passes
  "The passes of `passes` a render draws: what scrolls, and a still one
  (see `app.main.ui.viewer.viewport-wasm/render-passes`) that does not
  show what it would draw: not drawn yet, or owed (not in `drawn`, their
  tags); when only the containers scrolled (`scroll-only?`), one a
  container scrolls in (in `moving`, see `moving-passes`); else one that
  shows a pose of the animation (in `posed`), or that it changes (in
  `touched`, see `touched-passes`; nil: any)."
  [passes {:keys [drawn moving posed touched scroll-only?]}]
  (filterv (fn [{:keys [tag still]}]
             (or (not still)
                 (not (contains? drawn tag))
                 (if scroll-only?
                   (contains? moving tag)
                   (or (nil? touched)
                       (contains? posed tag)
                       (contains? touched tag)))))
           passes))

(defn- use-viewer-wasm-layers!
  [page-id page-objects size scale frame-id passes delta dpr-key
   modifiers-ref scroll-ref sheet-root frozen?]
  ;; The hot-areas SVG shifts every object by `-(size + delta)` so the frame
  ;; `selrect` lands flush against the overlay snap side, ignoring the extra
  ;; padding reserved for shadows/blur/strokes. Bake the same `delta` into the
  ;; WASM view origin so the rendered canvas aligns with that SVG (otherwise it
  ;; appears offset by the shadow margin).
  (let [render-size (mf/with-memo [size delta]
                      (-> size
                          (update :x + (:x delta 0))
                          (update :y + (:y delta 0))))
        ;; The render asked last: while an animation plays, frames the
        ;; renderer did not get to are skipped, not queued.
        generation-ref (mf/use-ref 0)
        scroll-cache   (mf/use-ref nil)
        ;; the still passes that show what they drew last (fixed and
        ;; sticky layers, see `due-passes`): a scroll of the containers
        ;; leaves those as they are, unless one scrolls in them (a fixed
        ;; bar of chips), and so does a frame of the animation that
        ;; changes nothing of theirs (`reach`), unless they show a pose
        ;; of it (`posed-ref`)
        drawn-ref      (mf/use-ref #{})
        posed-ref      (mf/use-ref #{})
        moving         (mf/with-memo [page-objects passes]
                         (moving-passes page-objects passes))
        reach          (mf/with-memo [page-objects passes]
                         (pass-reach page-objects passes))

        ;; Render the layers with `modifiers`, the animation at a time (a
        ;; delay of its modif-tree), or nil; `scroll-only?` when only the
        ;; containers scrolled since.
        render!
        (mf/use-fn
         (mf/deps page-id page-objects render-size scale frame-id passes
                  scroll-ref sheet-root frozen? moving reach)
         (fn [modifiers scroll-only?]
           (when (and (not frozen?) (get page-objects frame-id))
             (let [generation (inc (mf/ref-val generation-ref))]
               (mf/set-ref-val! generation-ref generation)
               ;; the ones a container scrolls in owe a render, should a
               ;; later one come first
               (when scroll-only?
                 (mf/set-ref-val! drawn-ref (reduce disj (mf/ref-val drawn-ref) moving)))
               (->> @wasm.api/module
                    (p/fmap
                     (fn [ready?]
                       (when ready?
                         (enqueue-wasm-render!
                          (fn []
                            (if (not= generation (mf/ref-val generation-ref))
                              ;; a later frame is on its way, and draws
                              (p/resolved nil)
                              (let [touched (touched-passes reach (some-> modifiers force))
                                    due     (due-passes passes {:drawn (mf/ref-val drawn-ref)
                                                                :moving moving
                                                                :posed (mf/ref-val posed-ref)
                                                                :touched touched
                                                                :scroll-only? scroll-only?})
                                    scroll  (scroll-of scroll-cache page-objects
                                                       (some-> scroll-ref deref) frame-id sheet-root)
                                    draws
                                    (into []
                                          (keep (fn [{:keys [tag still canvas include clear pins]}]
                                                  (when-let [canvas (canvas)]
                                                    {:tag    tag
                                                     :still  still
                                                     :canvas canvas
                                                     :opts   (cond-> {;; held at the bottom of a
                                                                      ;; screen lower than the board
                                                                      :modifiers (if (seq pins)
                                                                                   (delay (vpc/pin-modifiers (force modifiers) pins))
                                                                                   modifiers)
                                                                      :scroll scroll}
                                                               (seq include) (assoc :include-ids include)
                                                               (seq clear)   (assoc :clear-fills-ids clear))})))
                                          due)
                                    still   (into #{} (comp (filter :still) (map :tag)) draws)]
                                ;; what they show once drawn, as a later
                                ;; render runs after these
                                (mf/set-ref-val! drawn-ref (into (mf/ref-val drawn-ref) still))
                                (mf/set-ref-val! posed-ref (into (reduce disj (mf/ref-val posed-ref) still)
                                                                 (filter touched)
                                                                 still))
                                (if (empty? draws)
                                  (p/resolved nil)
                                  (-> (reduce (fn [chain {:keys [canvas opts]}]
                                                (p/then chain
                                                        #(render-viewer-frame* page-id page-objects
                                                                               canvas render-size scale frame-id
                                                                               nil opts)))
                                              (p/resolved nil)
                                              draws)
                                      (p/then (fn [_]
                                                (mark-rendered! (map :canvas draws) frame-id)))))))))))))))))]

    ;; anything a render depends on changed: all passes draw again
    (mf/with-layout-effect [render! dpr-key modifiers-ref]
      (mf/set-ref-val! drawn-ref #{})
      (mf/set-ref-val! posed-ref #{})
      (render! (some-> modifiers-ref deref) false))

    ;; Each frame of the animation renders the layers from here, as the
    ;; workspace does, without the components rendering again.
    (mf/with-effect [render! modifiers-ref]
      (when (some? modifiers-ref)
        (let [key (js/Symbol "viewer-wasm-layers")]
          (add-watch modifiers-ref key (fn [_ _ _ modifiers] (render! modifiers false)))
          #(remove-watch modifiers-ref key))))

    ;; and each step of a scroll of its containers (see `scroll-of`)
    (mf/with-effect [render! modifiers-ref scroll-ref]
      (when (some? scroll-ref)
        (let [key (js/Symbol "viewer-wasm-scroll")]
          (add-watch scroll-ref key (fn [_ _ _ _] (render! (some-> modifiers-ref deref) true)))
          #(remove-watch scroll-ref key))))))

(defn use-viewer-wasm-viewport!
  "WASM render passes for the viewer viewport.
  `modifiers-ref`, when given, is a ref of the animation it plays at a
  time (a delay of its modif-tree, see `show-animation!`): it renders
  again at each of its frames. `scroll-ref`, when given, is a ref of the
  scroll of its containers (`{id {:offset :past}}`): it renders again at
  each step; `sheet-root`, the root of an overlay that scrolls as one
  (see `dvs/scroll-entries`). `frozen?` renders nothing: the canvases
  keep the picture drawn on them (see `copy-canvases!`).

  `passes`, what it draws on its canvases, in order: `{:tag :canvas
  :include :clear :pins :still}`, its name (the `data-pass` of its
  canvas), a fn of its canvas, the ids it draws (all without), those of
  them whose fills, shadows and strokes another pass draws (the board
  under a masked pass), the layers of `pins` that far up (see
  `vpc/fixed-pins`), and whether a scroll of the containers leaves it as
  it is (but for containers in it)."
  [page-id page-objects size scale frame-id passes
   delta modifiers-ref scroll-ref sheet-root frozen?]
  (let [dpr-key (use-viewer-dpr-key)]
    (use-viewer-wasm-layers! page-id page-objects size scale frame-id passes
                             delta dpr-key modifiers-ref scroll-ref sheet-root frozen?)))
