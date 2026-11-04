;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.viewer.scroll
  "How the prototype scrolls boards whose content overflows them
  (Penpot Motion).

  A top board with `:scroll-overflow :vertical` keeps its box in the
  design, its first screen, and the view mode shows it as high as its
  content goes, so that it scrolls as a board that high does (see
  `app.main.ui.viewer`): in a device, with its layers fixed when
  scrolling and its hotspots.

  A board inside it with `:scroll-overflow` is a container: it stays
  where it is and its content moves inside it by its scroll offset
  (see `container-axes`), a move WASM gets after it has laid the shapes
  out (see `scroll-entries`), so no layout puts them back."
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.tilt :as gtl]
   [app.common.math :as mth]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape.layout :as ctl]
   [app.common.uuid :as uuid]))

(defn content-rect
  "The rect of the content of the board `frame` of `objects`, in page
  coordinates: its layers, but for those fixed when scrolling and the
  hidden ones, and beyond them the right and bottom padding of its
  layout. Nil without such layers."
  [objects frame]
  (let [layers (into []
                     (comp (keep #(get objects %))
                           (remove :hidden)
                           (remove cfh/fixed-scroll?))
                     (:shapes frame))]
    (when (seq layers)
      (let [{:keys [x y width height]} (gsh/shapes->rect layers)
            layout? (ctl/any-layout? frame)
            right   (if layout? (dm/get-in frame [:layout-padding :p2] 0) 0)
            bottom  (if layout? (dm/get-in frame [:layout-padding :p3] 0) 0)]
        (grc/make-rect x y (+ width right) (+ height bottom))))))

(defn content-bottom
  "Where the content of the board `frame` of `objects` ends, the y of
  the page (see `content-rect`). Nil without content."
  [objects frame]
  (some-> (content-rect objects frame) :y2))

(defn scroll-height
  "How high the view mode shows the board `frame` of `objects`: as far
  as its content goes (see `content-bottom`) when it is a top board
  that scrolls vertically (`:vertical`, or `:both`: a screen scrolls
  vertically only), not turned; nil when it is not, or when its
  content fits in it."
  [objects frame]
  (when (and (contains? #{:vertical :both} (:scroll-overflow frame))
             (cfh/root-frame? frame)
             (let [transform (:transform frame)]
               (or (nil? transform) (gmt/unit? transform))))
    (let [{:keys [y height]} (:selrect frame)
          bottom (content-bottom objects frame)]
      (when (and (some? bottom) (> (- bottom y) height))
        (- bottom y)))))

(defn- extend-board
  "The board `frame` `height` high, from its top: its layers stay where
  they are; `::screen-h` keeps the height of its box, its first screen."
  [frame height]
  (-> (gsh/transform-shape frame (ctm/change-size frame (dm/get-in frame [:selrect :width]) height))
      (assoc ::screen-h (dm/get-in frame [:selrect :height]))))

(declare container-axes)

(defn scroll-objects
  "`objects` of a page with each top board that scrolls vertically as
  high as its content (see `scroll-height`), and every board that
  scrolls its content clipping it (see `container-axes`; a top board
  too, for when it shows as the root of an overlay)."
  [objects]
  (let [grown (reduce (fn [objects id]
                        (let [frame (get objects id)]
                          (if-let [height (and (cfh/frame-shape? frame) (scroll-height objects frame))]
                            (assoc objects id (extend-board frame height))
                            objects)))
                      objects
                      (dm/get-in objects [uuid/zero :shapes]))]
    (reduce (fn [objects shape]
              (if (and (true? (:show-content shape))
                       (container-axes objects shape :overlay-root? true))
                (assoc-in objects [(:id shape) :show-content] false)
                objects))
            grown
            (filter :scroll-overflow (vals grown)))))

(defn scroll-page
  "The page `page` of the view mode (see `app.main.data.viewer`) with its
  top boards that scroll vertically as high as their content (see
  `scroll-objects`), in its objects and in its lists of boards."
  [page]
  (let [objects (scroll-objects (:objects page))]
    (if (identical? objects (:objects page))
      page
      (let [lookup #(get objects (:id %))]
        (-> page
            (assoc :objects objects)
            (update :frames #(mapv lookup %))
            (update :all-frames #(mapv lookup %)))))))

(defn screen-h
  "The height of the first screen the board `frame` of the view mode
  scrolls from, when it scrolls through its content (see
  `scroll-page`); nil for any other board."
  [frame]
  (::screen-h frame))

;; --- Containers

(def ^:private axes-of
  {:vertical   #{:y}
   :horizontal #{:x}
   :both       #{:x :y}})

(defn container-axes
  "The axes, `#{:x :y}` or part of it, the board `shape` of `objects`
  scrolls its content along inside its box in the view mode: an inner
  board as its `:scroll-overflow` says; a top board only as the root of
  an overlay (`overlay-root?`), vertically, for `:vertical` and
  `:both` (as a screen it scrolls as a page does, see `scroll-page`).
  Nil when it does not scroll so, and for a board that is turned or
  drawn in 3D."
  [objects shape & {:keys [overlay-root?]}]
  (let [value (:scroll-overflow shape)]
    (when (and (some? value)
               (cfh/frame-shape? shape)
               (let [transform (:transform shape)]
                 (or (nil? transform) (gmt/unit? transform)))
               (nil? (gtl/projection objects shape)))
      (if (cfh/root-frame? shape)
        (when (and overlay-root? (contains? #{:vertical :both} value))
          #{:y})
        (get axes-of value)))))

(defn scroll-range
  "How far the container `frame` of `objects` scrolls along `axes` (see
  `container-axes`), `[x y]` px: from where its content is in the
  design to where it ends past its box (see `content-rect`) — for a top
  board the view mode grew, its box in the design (see `screen-h`); 0
  along the other axis, and when the content fits."
  [objects frame axes]
  (let [{:keys [x y width height]} (:selrect frame)
        height  (or (screen-h frame) height)
        content (content-rect objects frame)]
    (if (nil? content)
      [0 0]
      [(if (contains? axes :x) (max 0 (- (:x2 content) (+ x width))) 0)
       (if (contains? axes :y) (max 0 (- (:y2 content) (+ y height))) 0)])))

(defn clamp-offset
  "The scroll offset `[x y]` within the range `[max-x max-y]` (see
  `scroll-range`)."
  [[max-x max-y] [x y]]
  [(mth/clamp x 0 max-x) (mth/clamp y 0 max-y)])

(defn container-ids
  "The ids of the containers in the board `root-id` of `objects`, it
  too when it is the root of an overlay (`overlay-root?`, see
  `container-axes`)."
  [objects root-id & {:keys [overlay-root?]}]
  (into #{}
        (filter (fn [id]
                  (some? (container-axes objects (get objects id)
                                         :overlay-root? (and overlay-root? (= id root-id))))))
        (cfh/get-children-ids-with-self objects root-id)))

(defn view-offsets
  "How far the containers of `scroll` (`{id {:offset :past}}`, see
  `app.main.data.viewer/scroll-container`) show scrolled over the shown
  `objects`: their offset clamped to their range there (a turned device
  or a live change may have changed it), and past it the rubber band.
  `overlay-root`, the id of the root of an overlay, scrolls as one (see
  `container-axes`). `{id [x y]}`."
  [objects scroll & {:keys [overlay-root]}]
  (reduce-kv (fn [offsets id {:keys [offset past]}]
               (let [frame (get objects id)
                     axes  (container-axes objects frame :overlay-root? (= id overlay-root))]
                 (if (and (some? axes) (some? offset))
                   (let [[x y]   (clamp-offset (scroll-range objects frame axes) offset)
                         [px py] (or past [0 0])]
                     (assoc offsets id [(+ x px) (+ y py)]))
                   offsets)))
             {}
             scroll))

(defn gesture-axis
  "The axis of a wheel gesture whose first step is `[dx dy]`: the one it
  moves more along (a trackpad swiped down moves sideways a little too)."
  [[dx dy]]
  (if (> (mth/abs dx) (mth/abs dy)) :x :y))

(defn takes-gesture?
  "Whether a container that scrolls along `axes` takes a gesture along
  `axis` (see `gesture-axis`): one of its axis; one that scrolls both
  ways, any."
  [axes axis]
  (or (= axes #{:x :y}) (contains? axes axis)))

(defn can-scroll?
  "Whether a container scrolled `offset` (`[x y]`) within `range` (see
  `scroll-range`) along `axes` can still move as `[dx dy]` asks, along one
  of them at least."
  [[max-x max-y] [x y] axes [dx dy]]
  (letfn [(moves? [v max d]
            (or (and (pos? d) (< v max)) (and (neg? d) (> v 0))))]
    (or (and (contains? axes :x) (moves? x max-x dx))
        (and (contains? axes :y) (moves? y max-y dy)))))

(declare scroll-shifts)

(defn container-at
  "The containers of `ids` under the point `point` of the page of the
  shown `objects`, the innermost first: their boxes where the scroll
  `offsets` of the ones around them (`{id [x y]}`, see `view-offsets`)
  and `extra` (`{id point}`, how far else they show moved) have them, and
  within those boxes too (a container clips what it holds)."
  [objects ids offsets point & {:keys [extra]}]
  (let [ids     (set ids)
        shifts  (scroll-shifts objects offsets)
        zero    (gpt/point 0 0)
        inside? (fn [id]
                  (let [{:keys [x y width height]} (dm/get-in objects [id :selrect])
                        shift (gpt/add (get shifts id zero) (get extra id zero))]
                    (grc/contains-point? (grc/make-rect (+ x (:x shift)) (+ y (:y shift)) width height)
                                         point)))
        around  (fn [id] (filter ids (cfh/get-parent-ids-seq objects id)))
        hit     (into #{} (filter inside?) ids)]
    (->> hit
         (filter #(every? hit (around %)))
         (sort-by #(count (around %)) >)
         (vec))))

(defn scroll-shifts
  "How far each layer moves for the scroll `offsets` of containers
  (`{id [x y]}`): the layers of a container, but for those fixed when
  scrolling, and all within them, back by its offset, summed over the
  containers they are in. `{id point}` of the layers that move."
  [objects offsets]
  (reduce-kv
   (fn [shifts id [ox oy]]
     (if (and (zero? ox) (zero? oy))
       shifts
       (let [move (gpt/point (- ox) (- oy))]
         (reduce (fn [shifts child-id]
                   (if (cfh/fixed-scroll? (get objects child-id))
                     shifts
                     (reduce (fn [shifts id]
                               (update shifts id #(if (some? %) (gpt/add % move) move)))
                             shifts
                             (cons child-id (cfh/get-children-ids objects child-id)))))
                 shifts
                 (dm/get-in objects [id :shapes])))))
   {}
   offsets))

(defn scroll-entries
  "The modifier entries `entries` of WASM (`[id matrix]`, the whole
  transform of each shape, as `propagate-modifiers` gives them) with the
  scroll `shifts` (see `scroll-shifts`) after them. An entry of a layer
  that moves gets its shift on top. A layer that moves unlike its parent
  and has no entry gets one: its shift on top of what it would take from
  its nearest ancestor with an entry (WASM hands that to a shape without
  one), so all within it moves with it.

  `sheet-root`, the id of a top board the view mode grew to its content
  (see `scroll-page`) shown as the root of an overlay, draws back at its
  box: its entry shrinks it there, and each of its layers gets an entry
  of its own, so none takes that."
  [objects entries shifts & {:keys [sheet-root]}]
  (let [root (when (some? sheet-root)
               (let [root (get objects sheet-root)]
                 (when (some? (screen-h root)) root)))]
    (if (and (empty? shifts) (nil? root))
      entries
      (let [by-id     (into {} entries)
            inherited (fn [id]
                        (or (some by-id (cfh/get-parent-ids-seq objects id))
                            (gmt/matrix)))
            shifted   (fn [id matrix]
                        (if-let [shift (get shifts id)]
                          (gmt/multiply (gmt/translate-matrix shift) matrix)
                          matrix))
            needy     (into #{}
                            (filter (fn [id]
                                      (and (not (contains? by-id id))
                                           (not= (get shifts id)
                                                 (get shifts (dm/get-in objects [id :parent-id]))))))
                            (keys shifts))
            needy     (cond-> needy
                        (some? root)
                        (into (remove #(contains? by-id %)) (:shapes root)))
            entries   (-> (into [] (map (fn [[id matrix]] [id (shifted id matrix)])) entries)
                          (into (map (fn [id] [id (shifted id (inherited id))])) needy))]
        (if (nil? root)
          entries
          (let [{:keys [x y height]} (:selrect root)
                shrink (gmt/scale-matrix (gpt/point 1 (/ (screen-h root) height)) (gpt/point x y))]
            (if (contains? by-id sheet-root)
              (mapv (fn [[id _ :as entry]]
                      (if (= id sheet-root)
                        [id (gmt/multiply (get by-id sheet-root) shrink)]
                        entry))
                    entries)
              (conj entries [sheet-root shrink]))))))))
