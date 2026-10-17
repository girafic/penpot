;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.types.path.subpath
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.point :as gpt]
   [app.common.types.path.helpers :as helpers]))

(defn pt=
  "Check if two points are close"
  [p1 p2]
  (< (gpt/distance p1 p2) 0.1))

(defn make-subpath
  "Creates a subpath either from a single command or with all the data"
  ([command]
   (let [p (helpers/segment->point command)]
     (make-subpath p p [command])))
  ([from to data]
   {:from from
    :to to
    :data data}))

(defn add-subpath-command
  "Adds a command to the subpath"
  [subpath command]
  (let [close? (= :close-path (:command command))]
    (if (and close? (pt= (:from subpath) (:to subpath)))
      ;; Avoid adding a duplicate node at an already closed seam.
      subpath
      (let [command (if close?
                      (helpers/make-line-to (:from subpath))
                      command)
            p       (helpers/segment->point command)]
        (-> subpath
            (assoc :to p)
            (update :data conj command))))))

(defn reverse-command
  "Reverses a single command"
  [command prev]

  (let [{:keys [x y]} (:params prev)
        {:keys [c1x c1y c2x c2y]} (:params command)]

    (-> command
        (update :params assoc :x x :y y)

        (cond-> (= :curve-to (:command command))
          (update :params assoc
                  :c1x c2x :c1y c2y
                  :c2x c1x :c2y c1y)))))

(defn reverse-subpath
  "Reverses a subpath starting with move-to"
  [subpath]

  (let [reverse-commands
        (fn [result [command prev]]
          (if (some? prev)
            (conj result (reverse-command command prev))
            result))

        new-data (->> subpath :data d/with-prev reverse
                      (reduce reverse-commands [(helpers/make-move-to (:to subpath))]))]

    (make-subpath (:to subpath) (:from subpath) new-data)))

(defn get-subpaths
  "Retrieves every subpath inside the current content"
  [content]
  (let [reduce-subpath
        (fn [subpaths current]
          (let [is-move? (= :move-to (:command current))
                last-idx (dec (count subpaths))]
            (cond
              is-move?
              (conj subpaths (make-subpath current))

              (>= last-idx 0)
              (update subpaths last-idx add-subpath-command current)

              :else
              subpaths)))]
    (->> content
         (reduce reduce-subpath []))))

(defn subpaths-join
  "Join two subpaths together when the first finish where the second starts"
  [subpath other]
  (assert (pt= (:to subpath) (:from other)))
  (-> subpath
      (update :data d/concat-vec (rest (:data other)))
      (assoc :to (:to other))))

(defn- merge-paths
  "Tries to merge into candidate the subpaths. Will return the candidate with the subpaths merged
  and removed from subpaths the subpaths merged. Only meeting points accepted
  by `meet?` are joined"
  [candidate subpaths meet?]
  (let [joins?
        (fn [point other]
          (and (pt= point other) (meet? point)))

        merge-with-candidate
        (fn [[candidate result] current]
          (cond
            (pt= (:to current) (:from current))
            ;; Subpath is already a closed path
            [candidate (conj result current)]

            (joins? (:to candidate) (:from current))
            [(subpaths-join candidate current) result]

            (joins? (:from candidate) (:to current))
            [(subpaths-join current candidate) result]

            (joins? (:to candidate) (:to current))
            [(subpaths-join candidate (reverse-subpath current)) result]

            (joins? (:from candidate) (:from current))
            [(subpaths-join (reverse-subpath current) candidate) result]

            :else
            [candidate (conj result current)]))]

    (->> subpaths
         (reduce merge-with-candidate [candidate []]))))

(defn is-closed? [subpath]
  (pt= (:from subpath) (:to subpath)))

(def ^:private xf-mapcat-data
  (mapcat :data))

(defn- join-adjacent
  "Fold neighbouring subpaths into the accumulator only when the
  current accumulator's end-point matches the next subpath's start-point.
  Unlike `merge-paths` this does not reverse subpaths nor reorder them;
  the original draw order is preserved so stroke-dasharray and animation
  semantics stay intact."
  [acc subpath]
  (if-let [prev (peek acc)]
    (if (and (not (is-closed? prev))
             (not (is-closed? subpath))
             (pt= (:to prev) (:from subpath)))
      (conj (pop acc) (subpaths-join prev subpath))
      (conj acc subpath))
    (conj acc subpath)))

(defn merge-touching-subpaths
  "Merge consecutive subpaths whose endpoints coincide into a single
  continuous subpath, preserving the original drawing order.

  This is a conservative variant of `close-subpaths`: it never reverses
  a subpath and only merges immediate neighbours, so closed regions and
  fill semantics are left untouched. The intent is to recover the
  `stroke-linejoin` rendering for SVG paths whose authoring tools split
  a continuous polyline into adjacent `M..L M..L` subpaths (e.g. the
  `m0 0` markers Figma emits when exporting Heroicons-like icons)."
  [content]
  (let [subpaths (get-subpaths content)
        merged   (reduce join-adjacent [] subpaths)]
    (into [] xf-mapcat-data merged)))

(defn close-subpaths
  "Searches a path for possible subpaths that can create closed loops and merge them.
  When `meet?` is given only subpaths that touch at an accepted point are merged"
  ([content]
   (close-subpaths content (constantly true)))
  ([content meet?]
   (let [subpaths (get-subpaths content)
         closed-subpaths
         (loop [result []
                current (first subpaths)
                subpaths (rest subpaths)]

           (if (some? current)
             (let [[new-current new-subpaths]
                   (if (is-closed? current)
                     [current subpaths]
                     (merge-paths current subpaths meet?))]

               (if (= current new-current)
                 ;; If equal we haven't found any matching subpaths we advance
                 (recur (conj result new-current)
                        (first new-subpaths)
                        (rest new-subpaths))

                 ;; If different we need to pass again the merge to check for additional
                 ;; subpaths to join
                 (recur result
                        new-current
                        new-subpaths)))
             result))]

     (into [] xf-mapcat-data closed-subpaths))))

(defn- close-loop
  "Adds an explicit close command when a subpath's endpoints meet."
  [{:keys [from to data] :as subpath}]
  (let [last-seg (peek data)]
    (if (or (< (count data) 2)
            (= :close-path (:command last-seg))
            (not (pt= from to)))
      subpath
      (let [data (cond-> data
                   (= :line-to (:command last-seg)) (pop))]
        (assoc subpath
               :to from
               :data (conj data {:command :close-path :params {}}))))))

(defn close-loops
  "Adds close commands to subpaths whose endpoints meet."
  [content]
  (->> (get-subpaths content)
       (mapv close-loop)
       (into [] xf-mapcat-data)))

;; THE START AND THE WAY ROUND (of a morph keyframe, see
;; `app.common.types.path.morph`)

(defn- slices
  "The subpaths of the plain `segments`, each the vector of its segments
  from its move-to; a segment after a close starts another one from the
  start of the subpath it closed, as SVG draws it."
  [segments]
  (:out
   (reduce (fn [{:keys [out start closed?]} segment]
             (let [command (:command segment)]
               (cond
                 (= :move-to command)
                 {:out (conj out [segment]) :start (helpers/segment->point segment) :closed? false}

                 (or (empty? out) closed?)
                 {:out (conj out [(helpers/make-move-to (or start (gpt/point 0 0))) segment])
                  :start start
                  :closed? (= :close-path command)}

                 :else
                 {:out (update out (dec (count out)) conj segment)
                  :start start
                  :closed? (= :close-path command)})))
           {:out [] :start nil :closed? false}
           segments)))

(defn- closed?
  "Whether the subpath `slice` is closed: by a close, or ending where it
  starts, as the path editor holds it (see `close-subpaths`)."
  [slice]
  (let [end (some-> (peek slice) helpers/segment->point)]
    (or (= :close-path (:command (peek slice)))
        (and (> (count slice) 2) (some? end) (pt= (helpers/segment->point (first slice)) end)))))

(defn- body
  "The segments of the subpath `slice` but its move-to and close; a closed
  one with a line back to its start where it ends elsewhere, which its
  close draws."
  [slice]
  (let [start (helpers/segment->point (first slice))
        segs  (filterv #(not (contains? #{:move-to :close-path} (:command %))) (rest slice))
        end   (some-> (peek segs) helpers/segment->point)]
    (cond-> segs
      (and (closed? slice) (some? end) (not (pt= start end)))
      (conj (helpers/make-line-to start)))))

(defn- closing
  "The segments `segs` of a closed subpath from `start`, closed: a last
  line back to `start` left to the close, which draws it."
  [segs start]
  (let [last-seg (peek segs)]
    (-> (if (and (= :line-to (:command last-seg)) (pt= start (helpers/segment->point last-seg)))
          (pop segs)
          segs)
        (conj {:command :close-path :params {}}))))

(defn closed-starts
  "The first point of each closed subpath of `content` (see `closed?`)."
  [content]
  (into []
        (comp (filter closed?) (map #(helpers/segment->point (first %))))
        (slices (vec content))))

(defn start-at
  "The plain segments of `content` with its closed subpath that has a node
  at `point` (see `pt=`) starting at it, drawing the same; the others as
  they are."
  [content point]
  (into []
        (mapcat (fn [slice]
                  (let [segs (when (closed? slice) (body slice))
                        k    (when (> (count segs) 1)
                               (d/index-of-pred (pop segs) #(pt= point (helpers/segment->point %))))]
                    (if (some? k)
                      (let [p (helpers/segment->point (nth segs k))]
                        (into [(helpers/make-move-to p)]
                              (closing (-> (subvec segs (inc k)) (into (subvec segs 0 (inc k)))) p)))
                      slice))))
        (slices (vec content))))

(defn- reverse-slice
  "The subpath `slice` going the other way round: a closed one from the
  same start, an open one from its end."
  [slice]
  (let [start (helpers/segment->point (first slice))
        segs  (body slice)]
    (if (empty? segs)
      slice
      (let [prevs (into [(first slice)] (pop segs))
            back  (mapv (fn [i] (reverse-command (nth segs i) (nth prevs i)))
                        (range (dec (count segs)) -1 -1))]
        (into [(helpers/make-move-to (helpers/segment->point (peek segs)))]
              (cond-> back (closed? slice) (closing start)))))))

(defn- slice-box
  "The box `[x0 y0 x1 y1]` of the points and handles of the subpath
  `slice`."
  [slice]
  (let [ps (mapcat (fn [{:keys [params]}]
                     (cond-> [[(:x params) (:y params)]]
                       (some? (:c1x params)) (conj [(:c1x params) (:c1y params)] [(:c2x params) (:c2y params)])))
                   (filter #(some? (dm/get-in % [:params :x])) slice))]
    (when (seq ps)
      [(reduce min (map first ps)) (reduce min (map second ps))
       (reduce max (map first ps)) (reduce max (map second ps))])))

(defn- boxes-meet?
  [[ax0 ay0 ax1 ay1] [bx0 by0 bx1 by1]]
  (and (<= ax0 bx1) (<= bx0 ax1) (<= ay0 by1) (<= by0 ay1)))

(defn reverse-at
  "The plain segments of `content` with its subpaths that have a node at
  one of `points` (all of them without `points`) going the other way
  round (see `reverse-slice`), each closed one with the closed ones its
  box meets, one after another: a hole goes round with the outline
  around it, so that it stays a hole (nonzero)."
  [content points]
  (let [subs    (slices (vec content))
        boxes   (mapv slice-box subs)
        closed  (into #{} (filter #(and (closed? (nth subs %)) (some? (nth boxes %)))) (range (count subs)))
        hit?    (fn [slice]
                  (let [nodes (cons (helpers/segment->point (first slice)) (map helpers/segment->point (body slice)))]
                    (or (empty? points) (some (fn [p] (some #(pt= p %) nodes)) points))))
        hit     (into #{} (filter #(hit? (nth subs %))) (range (count subs)))
        grow    (fn [turned]
                  (into turned
                        (filter (fn [i] (some #(and (contains? closed %) (boxes-meet? (nth boxes i) (nth boxes %))) turned)))
                        closed))
        turned  (loop [turned hit]
                  (let [more (grow turned)]
                    (if (= more turned) turned (recur more))))]
    (into [] cat (map (fn [i slice] (if (contains? turned i) (reverse-slice slice) slice)) (range) subs))))

;; FIXME: revisit this fn impl for perfromance
(defn reverse-content
  "Given a content reverse the order of the commands"
  [content]
  (->> (get-subpaths content)
       (mapv reverse-subpath)
       (reverse)
       (into [] xf-mapcat-data)))

;; https://mathworld.wolfram.com/PolygonArea.html
(defn clockwise?
  "Check whether the first subpath is clockwise or counter-clock wise"
  [content]
  (let [subpath (->> content get-subpaths first :data)]
    (loop [current (first subpath)
           subpath (rest subpath)
           first-point nil
           signed-area 0]

      (if (nil? current)
        (> signed-area 0)

        (let [{x1 :x y1 :y :as p} (helpers/segment->point current)
              last? (nil? (first subpath))
              first-point (if (nil? first-point) p first-point)
              {x2 :x y2 :y} (if last? first-point (helpers/segment->point (first subpath)))
              signed-area (+ signed-area (- (* x1 y2) (* x2 y1)))]

          (recur (first subpath)
                 (rest subpath)
                 first-point
                 signed-area))))))
