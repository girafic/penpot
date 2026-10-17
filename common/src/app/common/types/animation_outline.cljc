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

(defn row-takes?
  "Whether the outline keyframes among `keyframes`, but those `replaced?`
  (a predicate of a keyframe) leaves out, take `value` too: an outline a
  keyframe holds (see `outline-fits?`), the row within
  `max-outline-row-segments` with it."
  [keyframes replaced? value]
  (and (outline-fits? value)
       (outline-row-fits? (conj (into [] (remove replaced?) keyframes)
                                {:property :content :value value}))))

(def schema:outline
  "The outline an outline keyframe holds: path content of at most
  `max-outline-segments` segments."
  [:and path/schema:content
   [:fn {:error/message (str "An outline keyframe holds at most " max-outline-segments " segments")}
    #(<= (count %) max-outline-segments)]])

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

(defn box-scale
  "How much outlines made for the box `from` grow to fit the box `to`
  (each `{:width :height}`), as a point: each axis 1 when either is about
  0 or not given."
  [from to]
  (let [ratio (fn [k]
                (let [a (get from k) b (get to k)]
                  (if (and (number? a) (number? b) (> a 0.01) (> b 0.01)) (/ b a) 1)))]
    (gpt/point (ratio :width) (ratio :height))))

(defn- outline-scale
  "How much the outlines of the row `track` grow to fit the path `shape`
  (see `box-scale`): its box over the box they were made for."
  [shape track]
  (box-scale (:outline-box track) (outline-box shape)))

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
  made for the box `to` (see `box-scale`, `outline->content`)."
  [keyframes from to]
  (let [scale (box-scale from to)]
    (if (and (= 1 (:x scale)) (= 1 (:y scale)))
      keyframes
      (mapv #(cond-> %
               (= :content (:property %))
               (update :value path/transform-content (gmt/scale-matrix scale)))
            keyframes))))

(defn outline-change
  "How an edit changed the outline of the path from `before` to `after`
  (see `outline-of`): `:none` when it only moved, turned or flipped it,
  `:resize` when it scaled it with its box, `:reshape` otherwise; within
  0.01 px, the noise of the floats of path content."
  [before after]
  (let [old (outline-of before)
        new (outline-of after)]
    (cond
      (morph/close? old new 0.01)
      :none

      (morph/close? (outline-value before {:outline-box (outline-box after)}) new 0.01)
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
  1024)

(defn- keep-row
  "`cache` with `entry` for the row `row-key`: in place of the one it
  had, or else, a new row when `cache` holds `size` already, in place of
  another one, so the rows kept stay kept."
  [cache row-key entry size]
  (assoc (if (or (contains? cache row-key) (< (count cache) size))
           cache
           (dissoc cache (first (keys cache))))
         row-key entry))

(defn matched-outlines
  "The outlines `values` of a row of keyframes, in time order, matched to
  one structure (see `morph/match`), those `fixed` (a boolean each: a
  first point and way round set by hand) not turned to fit the one
  before. Kept for the row `row-key` (its layer and index) until its
  outlines change, and given back as they are for the matched ones, so
  that the rows every frame builds anew match once; past
  `outline-rows-cache-size` rows, a new one takes the place of another
  (see `keep-row`)."
  ([row-key values]
   (matched-outlines row-key values nil))
  ([row-key values fixed]
   (let [entry (get @outline-rows-cache row-key)]
     (cond
       (and (some? entry) (= values (:values entry)) (= fixed (:fixed entry)))
       (:matched entry)

       (and (some? entry) (= values (:matched entry)))
       values

       :else
       (let [matched (morph/match values fixed)]
         (swap! outline-rows-cache keep-row row-key {:values values :fixed fixed :matched matched}
                outline-rows-cache-size)
         matched)))))
