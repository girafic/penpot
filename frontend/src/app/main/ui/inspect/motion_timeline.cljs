;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.inspect.motion-timeline
  "The animation of the selection as a read-only timeline, in the Motion
  tab of the Inspect (Penpot Motion): for each board its duration,
  playback and markers, a row by animated layer (diamonds at its
  keyframes, blocks for its presets, its clock when it plays a copy's)
  and, the board selected, its audio clips. Plain DOM from the code model
  (see `ctac/code-layers`), so the view mode loads no workspace code;
  nothing in it changes the file."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.types.animation :as cta]
   [app.common.types.animation-code :as ctac]
   [app.common.types.audio-clip :as ctau]
   [app.main.ui.ds.foundations.assets.icon :refer [icon*]]
   [app.util.i18n :refer [tr]]
   [app.util.motion-labels :as labels]
   [app.util.shape-icon :as usi]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn- share
  "Where `time` is on an axis `axis` ms long, in %."
  [time axis]
  (mth/precision (* 100.0 (/ time (max 1 axis))) 3))

(defn playback-label
  [playback]
  (case playback
    :loop      (tr "workspace.animation.loop")
    :ping-pong (tr "workspace.animation.ping-pong")
    (tr "workspace.animation.play-once")))

(defn- layer-row
  "The row of `layer` (see `ctac/code-layers`) on an axis `axis` ms long."
  [{:keys [id shape source clock copy split]} axis]
  (cond-> {:kind     :layer
           :id       id
           :name     (or (:name shape) "")
           :icon     (usi/get-shape-icon shape)
           :diamonds (mapv (fn [time] {:time time :at (share time axis)})
                           (into (sorted-set) (map :time) (:keyframes source)))
           :blocks   (mapv (fn [{:keys [start] :as animation}]
                             (let [end (cta/animation-end animation)]
                               {:label (labels/animation-label animation)
                                :start start
                                :end   end
                                :at    (share start axis)
                                :width (share (- end start) axis)}))
                           (:animations source))}
    (some? copy)
    (assoc :copy {:name (:name copy)
                  :duration (:duration clock)
                  :playback (cta/playback-mode clock)})

    (some? split)
    (assoc :split split)))

(defn- clip-row
  [{:keys [id name start] :as clip} axis]
  (let [end (ctau/clip-end clip)]
    {:kind  :audio
     :id    id
     :name  (or name "")
     :start start
     :end   end
     :at    (share start axis)
     :width (share (- end start) axis)}))

(defn motion-rows
  "The read-only timeline of the shapes of `ids` of `objects`: for each
  board of `codes` (see `cltl/code-timelines`) that animates some of them,
  its name, duration, playback and markers, where its duration `:end`s on
  an axis `:axis` ms long (as long as the latest of its keyframes and
  copy clocks, when later), and its `:rows`, one by animated layer of
  `ids` in layer order and, the board itself among `ids`, one by audio
  clip; positions in % of the axis."
  [codes objects ids]
  (into []
        (keep
         (fn [{:keys [board-id timeline] :as code}]
           (let [layers   (ctac/code-layers code objects :ids ids)
                 clips    (when (contains? ids board-id) (ctau/audio-clips timeline))
                 duration (:duration timeline)
                 axis     (reduce max duration
                                  (concat (keep #(when (:copy %) (:duration (:clock %))) layers)
                                          (mapcat #(map :time (:keyframes (:source %))) layers)
                                          (map ctau/clip-end clips)))]
             (when (or (seq layers) (seq clips))
               {:board-id board-id
                :name     (or (dm/get-in objects [board-id :name]) "")
                :duration duration
                :playback (cta/playback-mode timeline)
                :axis     axis
                :end      (share duration axis)
                :markers  (mapv (fn [{:keys [id name time]}]
                                  {:id id :name name :time time :at (share time axis)})
                                (sort-by :time (:markers timeline)))
                :rows     (into (mapv #(layer-row % axis) layers)
                                (map #(clip-row % axis))
                                clips)}))))
        codes))

(defn- row-label
  "The accessible name of a row: the layer and the times of its
  keyframes, or the clip."
  [{:keys [name diamonds]}]
  (if (seq diamonds)
    (tr "inspect.motion.row-label" name (str/join ", " (map :time diamonds)))
    name))

(mf/defc layer-row*
  {::mf/private true}
  [{:keys [row end]}]
  (let [{:keys [kind name icon diamonds blocks copy split at width]} row]
    [:li {:class (stl/css-case :row true :audio-row (= :audio kind))
          :aria-label (row-label row)}
     [:span {:class (stl/css :row-name)}
      (when (some? icon)
        [:> icon* {:icon-id icon :size "s" :class (stl/css :row-icon)}])
      [:span {:class (stl/css :row-text) :title name} name]
      (when (some? split)
        [:span {:class (stl/css :tag)} (labels/split-label split)])]
     [:span {:class (stl/css :lane)}
      (when (< end 100)
        [:span {:class (stl/css :past) :style {:left (dm/str end "%")}}])
      (when (some? copy)
        [:span {:class (stl/css :copy-bar)
                :title (tr "inspect.motion.copy-clock" (:duration copy) (playback-label (:playback copy)))}])
      (when (= :audio kind)
        [:span {:class (stl/css :clip)
                :style {:left (dm/str at "%") :width (dm/str width "%")}
                :title name}
         name])
      (for [{:keys [label start end at width]} blocks]
        [:span {:key (dm/str label start)
                :class (stl/css :block)
                :style {:left (dm/str at "%") :width (dm/str width "%")}
                :title (dm/str label " · " start "–" end " ms")}
         label])
      (for [{:keys [time at]} diamonds]
        [:span {:key time
                :class (stl/css :diamond)
                :style {:left (dm/str at "%")}
                :title (dm/str time " ms")}])]]))

(mf/defc motion-timeline*
  "The read-only timeline of `boards` (see `motion-rows`)."
  [{:keys [boards]}]
  [:div {:class (stl/css :timeline)}
   (for [{:keys [board-id name duration playback end markers rows]} boards]
     (let [clips (filter #(= :audio (:kind %)) rows)]
       [:details {:key (dm/str board-id) :class (stl/css :board) :open true}
        [:summary {:class (stl/css :board-head)}
         [:span {:class (stl/css :board-name)} name]
         [:span {:class (stl/css :board-clock)}
          (dm/str duration " ms · " (playback-label playback))]]
        (when (seq markers)
          [:div {:class (stl/css :markers) :aria-label (tr "workspace.animation.markers")}
           (for [{:keys [id name time at]} markers]
             [:span {:key (dm/str id)
                     :class (stl/css :marker)
                     :style {:left (dm/str at "%")}
                     :title (dm/str name " · " time " ms")}])])
        [:ul {:class (stl/css :rows)}
         (for [row (remove #(= :audio (:kind %)) rows)]
           [:> layer-row* {:key (dm/str (:id row)) :row row :end end}])
         (when (seq clips)
           [:li {:class (stl/css :group)} (tr "workspace.animation.audio-group" (count clips))])
         (for [row clips]
           [:> layer-row* {:key (dm/str (:id row)) :row row :end end}])]]))])
