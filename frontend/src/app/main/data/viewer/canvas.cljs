;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.canvas
  "The canvas of a phone: the page of a file in the view mode as a whole,
  to move about and zoom, where a tap opens a board as the prototype.
  The route says whether it shows (`canvas`): absent, the view mode
  decides as it starts (`starts-on-canvas?`); \"true\", the canvas;
  \"false\", the prototype, chosen on this phone, so that a reload stays
  on its board. Pure: the events that go there live in
  `app.main.data.viewer`."
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.router :as rt]))

(defn- param
  [params k]
  (rt/get-query-param params k))

(defn- interactions?
  "Whether the route `params` show the Interactions section, where the
  view mode starts."
  [params]
  (contains? #{nil "interactions"} (some-> (param params :section) name)))

(defn canvas-shown?
  "Whether a view mode that can show the canvas (`able?`: on a phone,
  drawing with WASM, see `app.main.data.viewer`) shows it at the route
  `params`: in Interactions, `canvas` \"true\" and no board."
  [able? params]
  (and (true? able?)
       (interactions? params)
       (= "true" (param params :canvas))
       (nil? (param params :index))))

(defn starts-on-canvas?
  "Whether a view mode that can show the canvas starts on it at the
  route `params`: in Interactions, neither live (`follow`) nor the
  preview of the workspace, not chosen yet on this phone (no `canvas`),
  and its link points at no board: no `frame-id`, and no `index` but a
  share link's \"0\", which the share dialog writes into each link."
  [params]
  (let [index (param params :index)]
    (and (interactions? params)
         (not= "true" (param params :follow))
         (not= "true" (param params :preview))
         (nil? (param params :canvas))
         (nil? (param params :frame-id))
         (or (nil? index)
             (and (some? (param params :share-id))
                  (= "0" (str index)))))))

(defn canvas-params
  "The route `params` showing the canvas: no board, in Interactions."
  [params]
  (-> params
      (assoc :canvas "true")
      (dissoc :index :frame-id :section)))

(defn board-params
  "The route `params` showing the board at `index`: off the canvas when
  it showed, the prototype then chosen on this phone."
  [params index]
  (cond-> (assoc params :index index)
    (= "true" (param params :canvas))
    (assoc :canvas "false")))

;; --- What the canvas draws, and its view
;;
;; A view is `{:zoom :x :y}`: the point of the page at the top left
;; corner of the `screen` (`{:width :height}`, CSS px), the page `zoom`
;; times as big.

(def max-zoom
  "How far the canvas zooms in at most, unless its content is so small
  that it starts closer."
  4)

(defn canvas-objects
  "The `objects` of a page the canvas draws: all but its top-level boards
  hidden in view mode, with what they hold. A board hidden in view mode
  inside another one stays, as part of it."
  [objects]
  (let [hidden (into #{}
                     (comp (keep #(get objects %))
                           (filter #(and (cfh/frame-shape? %) (true? (:hide-in-viewer %))))
                           (map :id))
                     (dm/get-in objects [uuid/zero :shapes]))]
    (if (empty? hidden)
      objects
      (let [gone (into hidden (mapcat #(cfh/get-children-ids objects %)) hidden)]
        (-> (apply dissoc objects gone)
            (update-in [uuid/zero :shapes] #(into [] (remove gone) %)))))))

(defn content-bounds
  "The rect round the top-level shapes of `objects`, or nil for a page
  with none."
  [objects]
  (->> (dm/get-in objects [uuid/zero :shapes])
       (keep #(get objects %))
       (gsh/shapes->rect)))

(defn- start-zoom
  "The zoom that shows `bounds` as wide as the `screen`."
  [bounds screen]
  (/ (:width screen) (max 1 (:width bounds))))

(defn zoom-range
  "The zooms the canvas of `bounds` goes between on a `screen`: from half
  the zoom it starts at, up to `max-zoom`, or to the zoom it starts at
  when that is closer."
  [bounds screen]
  (let [start (start-zoom bounds screen)]
    [(min (/ start 2) max-zoom) (max max-zoom start)]))

(defn- clamp-axis
  "Where the screen starts along an axis, `at`, over `content` (`[from
  size]`) it shows `visible` of: the content as far as an edge of the
  screen, or in its middle when smaller than it."
  [at [from size] visible]
  (if (<= size visible)
    (- from (/ (- visible size) 2))
    (mth/clamp at from (- (+ from size) visible))))

(defn clamp-view
  "The `view` within the zooms of the canvas of `bounds` on a `screen`,
  the content as far as its edges, or in the middle along an axis where
  it is smaller than the screen."
  [{:keys [zoom x y]} bounds screen]
  (let [[lo hi] (zoom-range bounds screen)
        zoom    (mth/clamp zoom lo hi)]
    {:zoom zoom
     :x    (clamp-axis x [(:x bounds) (:width bounds)] (/ (:width screen) zoom))
     :y    (clamp-axis y [(:y bounds) (:height bounds)] (/ (:height screen) zoom))}))

(defn initial-view
  "The view the canvas of `bounds` starts at on a `screen`: as wide as
  it, from the top."
  [bounds screen]
  (clamp-view {:zoom (start-zoom bounds screen) :x (:x bounds) :y (:y bounds)} bounds screen))

(defn view-box
  "The rect of the page that `view` shows on a `screen`."
  [{:keys [zoom x y]} screen]
  {:x x :y y :width (/ (:width screen) zoom) :height (/ (:height screen) zoom)})

(defn page-point
  "The point of the page under the `point` of the screen (px from its top
  left corner) at `view`."
  [{:keys [zoom x y]} point]
  (gpt/point (+ x (/ (:x point) zoom)) (+ y (/ (:y point) zoom))))

(defn zoom-about
  "The `view` at `zoom`, the point of the page under the `point` of the
  screen staying under it."
  [view point zoom]
  (let [{px :x py :y} (page-point view point)]
    {:zoom zoom
     :x    (- px (/ (:x point) zoom))
     :y    (- py (/ (:y point) zoom))}))

(defn resize-view
  "The `view` of a screen turned or resized from `before` to `screen`
  (both `{:width :height}`): its zoom kept, the point of the page at the
  middle of the screen staying there."
  [view before screen bounds]
  (let [middle (page-point view (gpt/point (/ (:width before) 2) (/ (:height before) 2)))
        zoom   (:zoom view)]
    (clamp-view {:zoom zoom
                 :x    (- (:x middle) (/ (:width screen) zoom 2))
                 :y    (- (:y middle) (/ (:height screen) zoom 2))}
                bounds screen)))

(defn board-at
  "The top-level board of `objects` under the `point` of the page, the
  upper one where they overlap, or nil."
  [objects point]
  (->> (rseq (dm/get-in objects [uuid/zero :shapes] []))
       (keep #(get objects %))
       (filter cfh/frame-shape?)
       (some #(when (gsh/has-point? % point) %))))

(defn zoom-target
  "The board of `objects`, at any depth, a double tap at the `point` of
  the page zooms to at `view` on a `screen`: the largest one under it at
  most half as wide as what the screen shows, so that the tap zooms in;
  or nil."
  [objects point view screen]
  (let [widest (/ (:width screen) (:zoom view) 2)]
    (->> (vals objects)
         (filter #(and (cfh/frame-shape? %)
                       (not= uuid/zero (:id %))
                       (<= (dm/get-in % [:selrect :width]) widest)
                       (gsh/has-point? % point)))
         (reduce (fn [best shape]
                   (if (or (nil? best)
                           (> (dm/get-in shape [:selrect :width]) (dm/get-in best [:selrect :width])))
                     shape
                     best))
                 nil))))

(defn double-tap-view
  "Where a double tap at the `point` of the screen takes the `view` of
  the canvas of `objects` (`bounds` round them) on a `screen`: zoomed in
  past the start, back to it; else to a board under it, as wide as the
  screen, its top at the top (see `zoom-target`), or twice as close
  about the point when there is none."
  [view objects bounds screen point]
  (let [start (initial-view bounds screen)]
    (if (> (:zoom view) (* 1.01 (:zoom start)))
      start
      (let [target (zoom-target objects (page-point view point) view screen)]
        (clamp-view (if (some? target)
                      (let [{:keys [x y width]} (:selrect target)]
                        {:zoom (/ (:width screen) (max 1 width)) :x x :y y})
                      (zoom-about view point (* 2 (:zoom view))))
                    bounds screen)))))

;; --- Gestures on the canvas

(defn pan-view
  "The `view` fingers moved `dx dy` px over the screen: the page follows
  them."
  [{:keys [zoom] :as view} dx dy]
  (-> view
      (update :x - (/ dx zoom))
      (update :y - (/ dy zoom))))

(defn overshoot
  "How far, px of the screen, the `view` is past where the canvas of
  `bounds` on a `screen` keeps it (see `clamp-view`): `[x y]`, more than
  0 past the right or the bottom."
  [view bounds screen]
  (let [{:keys [zoom x y]} (clamp-view view bounds screen)]
    [(* (- (:x view) x) zoom) (* (- (:y view) y) zoom)]))

(def ^:private speed-window-ms
  "How far back, ms, the speed fingers let go at is measured (as
  `app.main.ui.viewer.drag-scroll` does)."
  100)

(defn finger-speed
  "The speed, px/ms of the screen, of a finger at `samples` (`[t x y]`,
  the oldest first) let go at `t`: `[vx vy]` over the last
  `speed-window-ms` it moved, `[0 0]` once it stood still that long."
  [samples t]
  (let [[last-t] (peek samples)
        samples  (filterv (fn [[st]] (> st (- last-t speed-window-ms))) samples)]
    (if (or (< (count samples) 2) (> (- t last-t) speed-window-ms))
      [0 0]
      (let [[t0 x0 y0] (first samples)
            [t1 x1 y1] (peek samples)]
        (if (> t1 t0)
          [(/ (- x1 x0) (- t1 t0)) (/ (- y1 y0) (- t1 t0))]
          [0 0])))))

(defn tween-view
  "The view `e` of the way (0 to 1) from `from` to `to`: zooming about the
  point of the screen that stays where it is between them, or moving
  straight between two views at one zoom."
  [from to e]
  (let [z0 (:zoom from)
        z1 (:zoom to)]
    (if (mth/close? z0 z1 1e-9)
      {:zoom (+ z0 (* e (- z1 z0)))
       :x    (+ (:x from) (* e (- (:x to) (:x from))))
       :y    (+ (:y from) (* e (- (:y to) (:y from))))}
      (let [k     (- (/ 1 z0) (/ 1 z1))
            point (gpt/point (/ (- (:x to) (:x from)) k) (/ (- (:y to) (:y from)) k))]
        (zoom-about from point (* z0 (mth/pow (/ z1 z0) e)))))))

(def double-tap-ms
  "How soon, ms, a second tap makes a double tap."
  300)

(def ^:private double-tap-px
  "How near, px of the screen, a second tap makes a double tap."
  24)

(defn double-tap?
  "Whether the `tap` (`{:t :x :y}`, ms and px of the screen) is the
  second of a double tap after the tap `before`: soon and near enough."
  [before tap]
  (and (some? before)
       (< (- (:t tap) (:t before)) double-tap-ms)
       (< (mth/hypot (- (:x tap) (:x before)) (- (:y tap) (:y before))) double-tap-px)))

;; --- The animations of the canvas: each board that comes into view
;; plays from its start, as the prototype plays it

(defn boards-in-view
  "The ids among `ids` of the boards of `objects` that show in `vbox` or
  `margin` (units of the page) round it."
  [objects ids {:keys [x y width height]} margin]
  (let [x0 (- x margin) x1 (+ x width margin)
        y0 (- y margin) y1 (+ y height margin)]
    (into #{}
          (filter (fn [id]
                    (when-let [{bx :x by :y bw :width bh :height} (dm/get-in objects [id :selrect])]
                      (and (< bx x1) (> (+ bx bw) x0) (< by y1) (> (+ by bh) y0)))))
          ids)))

(defn entered
  "When each board `in-view` came into view, `{id ms}`, at `now`: kept for
  those still in view since `stamps`, `now` for those that came in, none
  for those out of it, which play from their start again once back."
  [stamps in-view now]
  (into {} (map (fn [id] [id (get stamps id now)])) in-view))

(defn animation-frame
  "What the canvas shows at `now` of the boards in view since `stamps`
  (`{id ms}`), as the prototype plays them by `plays` (`{id {:clock
  :copies}}`, see `dv/board-clock`) of `page`, `files` the files of the
  components: `{:tree :ended}`, their modif-trees merged (their layers
  are apart), and the trees of those played to their end, `{id tree}`,
  which `ended` keeps from the frame before so as not to work them out
  again."
  [page files plays stamps ended now]
  (reduce-kv (fn [acc id entry]
               (if-let [done (get ended id)]
                 (-> acc
                     (update :tree merge done)
                     (assoc-in [:ended id] done))
                 (let [{:keys [clock copies]} (get plays id)
                       elapsed (- now entry)
                       tree    (cltl/view-animation-tree page files id (cta/playback-time clock elapsed) elapsed)]
                   (cond-> (update acc :tree merge tree)
                     (cltl/view-playback-ended? clock copies elapsed)
                     (assoc-in [:ended id] tree)))))
             {:tree {} :ended {}}
             stamps))
