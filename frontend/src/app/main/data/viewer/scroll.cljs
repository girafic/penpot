;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.viewer.scroll
  "How the prototype scrolls a top board whose content overflows it
  (Penpot Motion): a board with `:scroll-overflow :vertical` keeps its
  box in the design, its first screen, and the view mode shows it as
  high as its content goes, so that it scrolls as a board that high
  does (see `app.main.ui.viewer`): in a device, with its layers fixed
  when scrolling and its hotspots."
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.shapes :as gsh]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape.layout :as ctl]
   [app.common.uuid :as uuid]))

(defn content-bottom
  "Where the content of the board `frame` of `objects` ends, the y of
  the page: the lowest edge of its layers, but for those fixed when
  scrolling and the hidden ones, and below it the bottom padding of its
  layout. Nil without such layers."
  [objects frame]
  (let [layers (into []
                     (comp (keep #(get objects %))
                           (remove :hidden)
                           (remove cfh/fixed-scroll?))
                     (:shapes frame))]
    (when (seq layers)
      (+ (:y2 (gsh/shapes->rect layers))
         (if (ctl/any-layout? frame)
           (dm/get-in frame [:layout-padding :p3] 0)
           0)))))

(defn scroll-height
  "How high the view mode shows the board `frame` of `objects`: as far
  as its content goes (see `content-bottom`) when it is a top board
  that scrolls vertically, not turned; nil when it is not, or when its
  content fits in it."
  [objects frame]
  (when (and (= :vertical (:scroll-overflow frame))
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

(defn scroll-objects
  "`objects` of a page with each top board that scrolls vertically as
  high as its content (see `scroll-height`)."
  [objects]
  (reduce (fn [objects id]
            (let [frame (get objects id)]
              (if-let [height (and (cfh/frame-shape? frame) (scroll-height objects frame))]
                (assoc objects id (extend-board frame height))
                objects)))
          objects
          (dm/get-in objects [uuid/zero :shapes])))

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
