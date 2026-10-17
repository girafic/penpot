;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.animation-outline
  "The outlines of a path as keyframes (path morphing, the `:content`
  property of `app.common.types.animation`): how a keyframe holds an
  outline in the path's own frame and places it back, how a row of them
  is made for a box and scaled to another, how much one holds, and the
  rows matched to one structure (see `app.common.types.path.morph`),
  kept so that every frame matches them once."
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.math :as mth]
   [app.common.types.path :as path]
   [app.common.types.path.morph :as morph]))

;; OUTLINES
;;
;; The keyframes of the outline of a path (`:content`) hold it in the
;; path's own frame: its content taken back by its rest transform (which
;; turns and flips it) around its middle, from the top-left of its
;; selrect, for the size of the box its row was made for (`:outline-box`
;; of its track). A move, a turn or a flip of the path takes them along, a
;; resize scales them (see `outline->content`).

(def max-outline-segments
  "The most segments an outline keyframe holds."
  4096)

(def max-outline-row-segments
  "The most segments the outline keyframes of one row hold together: a
  track goes whole with every edit of its layer."
  32768)

(defn outline-fits?
  "Whether `value` is an outline a keyframe can hold: path content that
  draws something (more than moves), within `max-outline-segments`."
  [value]
  (and (path/content? value)
       (<= (count value) max-outline-segments)
       (boolean (some #(not= :move-to (:command %)) value))))

(defn outline-row-fits?
  "Whether the outline keyframes among `keyframes` hold no more than
  `max-outline-row-segments` segments together."
  [keyframes]
  (<= (transduce (comp (filter #(= :content (:property %)))
                       (map (comp count :value)))
                 + 0 keyframes)
      max-outline-row-segments))

(defn- rest-frame
  "The matrix taking an outline in the own frame of the path `shape` (see
  `outline-of`) where the path rests: its transform (which turns and
  flips it) around the middle of its selrect."
  [shape]
  (let [selrect (:selrect shape)]
    (gmt/multiply (gmt/transform-in (grc/rect->center selrect) (:transform shape (gmt/matrix)))
                  (gmt/translate-matrix (:x selrect) (:y selrect)))))

(defn outline-box
  "The size of the box of the path `shape` at rest, which the outlines of
  a row are made for (see `outline->content`)."
  [shape]
  {:width (dm/get-in shape [:selrect :width])
   :height (dm/get-in shape [:selrect :height])})

(defn- outline-scale
  "How much the outlines of the row `track` grow to fit the path `shape`:
  its box over the box they were made for, each axis 1 when either is
  about 0 or the row has no box."
  [shape track]
  (let [box   (:outline-box track)
        rest  (outline-box shape)
        ratio (fn [k]
                (let [made (get box k) now (get rest k)]
                  (if (and (number? made) (number? now) (> made 0.01) (> now 0.01))
                    (/ now made)
                    1)))]
    (gpt/point (ratio :width) (ratio :height))))

(defn outline-of
  "The outline of the path `shape` in its own frame: its content taken
  back by its rest transform (which turns and flips it) around its
  middle, from the top-left of its selrect."
  [shape]
  (path/transform-content (path/content (:content shape)) (gmt/inverse (rest-frame shape))))

(defn outline-value
  "The outline of the path `shape` (see `outline-of`) as a keyframe of the
  row `track` holds it: for the box its outlines were made for."
  [shape track]
  (let [{:keys [x y]} (outline-scale shape track)]
    (cond-> (outline-of shape)
      (or (not= 1 x) (not= 1 y))
      (path/transform-content (gmt/scale-matrix (gpt/point (/ 1 x) (/ 1 y)))))))

(defn ensure-outline-box
  "`timeline` with the box of the path `shape` (see `outline-box`) as the
  one its row of outlines is made for, when it has outline keyframes and
  no box yet: a new row is made for the path as it is."
  [timeline shape]
  (let [shape-id (:id shape)
        track    (get-in timeline [:tracks shape-id])]
    (if (and (some? track)
             (nil? (:outline-box track))
             (some #(= :content (:property %)) (:keyframes track)))
      (assoc-in timeline [:tracks shape-id :outline-box] (outline-box shape))
      timeline)))

(defn rescale-outlines
  "`keyframes` with their outlines, made for the box `from`, scaled to be
  made for the box `to` (see `outline->content`)."
  [keyframes from to]
  (let [ratio (fn [k]
                (let [a (get from k) b (get to k)]
                  (if (and (number? a) (number? b) (> a 0.01) (> b 0.01)) (/ b a) 1)))
        scale (gpt/point (ratio :width) (ratio :height))]
    (if (and (= 1 (:x scale)) (= 1 (:y scale)))
      keyframes
      (mapv #(cond-> %
               (= :content (:property %))
               (update :value path/transform-content (gmt/scale-matrix scale)))
            keyframes))))

(defn- close-outlines?
  "Whether the outlines `a` and `b` have one structure and their points
  are within `tolerance` of each other."
  [a b tolerance]
  (and (= (count a) (count b))
       (every? true? (map (fn [sa sb]
                            (and (= (:command sa) (:command sb))
                                 (every? #(mth/close? (dm/get-in sa [:params %] 0) (dm/get-in sb [:params %] 0) tolerance)
                                         [:x :y :c1x :c1y :c2x :c2y])))
                          a b))))

(defn outline-change
  "How an edit changed the outline of the path from `before` to `after`
  (see `outline-of`): `:none` when it only moved, turned or flipped it,
  `:resize` when it scaled it with its box, `:reshape` otherwise; within
  0.01 px, the noise of the floats of path content."
  [before after]
  (let [old (outline-of before)
        new (outline-of after)]
    (cond
      (close-outlines? old new 0.01)
      :none

      (close-outlines? (outline-value before {:outline-box (outline-box after)}) new 0.01)
      :resize

      :else
      :reshape)))

(defn outline->content
  "The content of the path `shape` drawing the outline `value`, a
  keyframe of the row `track` (see `outline-value`), where the path
  rests: scaled to its box, turned and flipped as it is."
  [shape track value]
  (let [scale (outline-scale shape track)]
    (path/transform-content value
                            (cond-> (rest-frame shape)
                              (or (not= 1 (:x scale)) (not= 1 (:y scale)))
                              (gmt/multiply (gmt/scale-matrix scale))))))

(defonce ^:private outline-rows-cache
  (atom {}))

(def ^:private outline-rows-cache-size
  "The most rows of outlines whose matched outlines are kept (see
  `matched-outlines`)."
  256)

(defn matched-outlines
  "The outlines `values` of a row of keyframes, in time order, matched to
  one structure (see `morph/match`). Kept by the outlines, and kept as
  they are for the matched ones, so that the rows every frame builds
  anew match once."
  [values]
  (or (get @outline-rows-cache values)
      (let [matched (morph/match values)]
        (swap! outline-rows-cache
               (fn [cache]
                 (-> (if (< (count cache) outline-rows-cache-size) cache {})
                     (assoc values matched matched matched))))
        matched)))
