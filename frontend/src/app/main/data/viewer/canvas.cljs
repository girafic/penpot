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
   [app.common.math :as mth]
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
