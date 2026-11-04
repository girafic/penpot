;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.turn
  "A phone or a tablet of the view mode turned on its side, or stood up
  again, by whoever looks at the prototype (see
  `app.main.data.viewer.devices`). Each board stands as it is drawn, on
  its side when wider than high. Held the other way (a `hold`), the
  device shows the board of the same name that stands that way, its
  counterpart (\"Menu\" and \"Menu (landscape)\"), or the one picked
  for it in the prototype tab (`:counterpart`), else the board itself
  turned: as wide as the turned screen, its layers moved and stretched
  by their constraints, as a resize in the workspace does."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.modifiers :as gm]
   [app.common.geom.shapes :as gsh]
   [app.common.types.modifiers :as ctm]
   [app.main.data.viewer.devices :as dvd]
   [cuerdas.core :as str]))

(defn way
  "How the screen of `device` (see `dvd/resolve-device`) stands:
  `:landscape` when wider than high, else `:portrait`. The open iPhone
  Duo stands wide: on its side, its screen stands up."
  [{:keys [screen-w screen-h]}]
  (if (> screen-w screen-h) :landscape :portrait))

(defn board-way
  "How the board `frame` stands: `:landscape` when wider than high."
  [frame]
  (if (> (dm/get-in frame [:selrect :width]) (dm/get-in frame [:selrect :height]))
    :landscape
    :portrait))

(def ^:private way-words
  "A word that says how a board stands, with what joins it to the rest
  of its name."
  #"(?i)[\s\-–—_/:.,()\[\]]*(?<![a-z0-9])(?:landscape|portrait|horizontal|vertical|querformat|hochformat)(?![a-z0-9])[\s\-–—_/:.,()\[\]]*")

(defn base-name
  "The name of a board without the words that say how it stands, in
  lower case: \"menu\" of \"Menu (landscape)\"."
  [name]
  (-> (str/replace (or name "") way-words " ")
      (str/collapse-whitespace)
      (str/trim)
      (str/lower)))

(defn stands-way?
  "Whether the board `other` stands `way` next to the board `frame`:
  wider than it for `:landscape`, narrower for `:portrait`. A board on
  its side that scrolls is higher than wide, yet wider than the board
  standing up."
  [frame other way]
  (let [fw (dm/get-in frame [:selrect :width])
        ow (dm/get-in other [:selrect :width])]
    (if (= way :landscape) (> ow fw) (< ow fw))))

(defn counterpart
  "The board of `frames` that stands `way` (`:portrait` or `:landscape`)
  next to `frame` (see `stands-way?`) that `frame` shows as held that
  way: the one it names (`:counterpart`), none when it names itself (it
  turns), else one that names it, else one named as it but for the
  words that say how it stands (see `base-name`) and that names no
  other; nil for none."
  [frames frame way]
  (let [id      (:id frame)
        named   (:counterpart frame)
        stands? #(and (not= (:id %) id)
                      (cfh/root-frame? %)
                      (stands-way? frame % way))
        picked  (when (some? named)
                  (d/seek #(and (= named (:id %)) (stands? %)) frames))]
    (cond
      (= named id) nil
      (some? picked) picked
      :else
      (or (d/seek #(and (= id (:counterpart %)) (stands? %)) frames)
          (let [base (base-name (:name frame))]
            (d/seek #(and (stands? %)
                          (nil? (:counterpart %))
                          (= base (base-name (:name %))))
                    frames))))))

(defn link-changes
  "What picking `choice` as the counterpart of the board `id` (see
  `counterpart`) sets on the boards of `objects`: `{board-id
  counterpart}`, nil to remove it. The board `id` names `choice` (nil
  for the board named as it, `id` for itself turned); a board picked
  names it back; the boards that named either of them name none."
  [objects id choice]
  (let [other (when (and (some? choice) (not= choice id)) choice)
        pair  (cond-> #{id} (some? other) (conj other))
        stale (into {}
                    (comp (filter #(contains? pair (:counterpart %)))
                          (remove #(contains? pair (:id %)))
                          (map #(vector (:id %) nil)))
                    (vals objects))]
    (cond-> (assoc stale id choice)
      (some? other) (assoc other id))))

(defn- device-held
  "The device `choice` shows a board `w` × `h` in (see
  `dvd/resolve-device`), on its side or standing up as `held` says (nil:
  as the board stands): a board on its side that scrolls is higher than
  wide."
  [choice w h held]
  (let [device (dvd/resolve-device choice w h)]
    (if (or (nil? held) (nil? device) (not (dvd/turns? (:kind device))) (= held (way device)))
      device
      (dvd/resolve-device choice w (if (= held :landscape) (min h (dec w)) (max h (inc w)))))))

(defn- pair-way
  "How the board `frame` stands next to its counterpart among `frames`:
  `:landscape` when one narrower stands for it, `:portrait` when one
  wider does, nil without one."
  [frames frame]
  (cond
    (some? (counterpart frames frame :portrait)) :landscape
    (some? (counterpart frames frame :landscape)) :portrait))

(defn shown
  "How the board `frame` shows in the device `choice` (see
  `dvd/resolve-device`) held `hold` (nil, `:portrait` or `:landscape`):
  `{:frame-id :device :turned?}`, the board shown, it or its counterpart
  among `frames` (the boards of its page), the device it shows in (nil
  for none) and whether the board shows turned (see `turn-objects`).
  Held as the board stands, or without a hold or a device that turns, it
  shows as it is. A board stands as next to its counterpart, if it has
  one (see `pair-way`)."
  [frames frame choice hold]
  (let [{:keys [width height]} (:selrect frame)
        device (device-held choice width height (when (some? hold) (pair-way frames frame)))]
    (if (or (nil? hold)
            (nil? device)
            (not (dvd/turns? (:kind device)))
            (= hold (way device)))
      {:frame-id (:id frame) :device device}
      (if-let [other (counterpart frames frame hold)]
        (let [{:keys [width height]} (:selrect other)]
          {:frame-id (:id other)
           :device (device-held (name (:kind device)) width height hold)})
        {:frame-id (:id frame) :device (dvd/turn device) :turned? true}))))

(defn next-hold
  "The hold after a turn of `device`, the one the board `frame` shows in
  (see `shown`): the other way than it stands, or nil when that is the
  way the board stands in it untouched (see `dvd/resolve-device`), so
  that two turns give the prototype back as it is drawn."
  [frame choice device]
  (let [{:keys [width height]} (:selrect frame)
        drawn (some-> (dvd/resolve-device choice width height) way)
        next  (if (= :landscape (way device)) :portrait :landscape)]
    (when (not= next drawn) next)))

(defn- turned-size
  "The size `[w h]` of the board `frame` turned in `device` (see `shown`):
  as wide as its screen, as high as it was or as the screen, the higher.
  Higher, it scrolls in the screen."
  [frame device]
  [(:screen-w device) (max (dm/get-in frame [:selrect :height]) (:screen-h device))])

(defn- resize
  "`objects` with the board `id` resized to `w` × `h` from its top left,
  its layers moved and stretched by their constraints."
  [objects id w h]
  (let [modif-tree {id {:modifiers (ctm/change-size (get objects id) w h)}}]
    (gsh/apply-objects-modifiers objects (gm/set-objects-modifiers modif-tree objects))))

(defn- turn-board
  "`objects` with the board `frame` turned in `device` (see `shown`): its
  layers fixed when scrolling stay where they were on its first screen
  (see `dvd/board-first-screen-h`), by their constraints as it turns into
  the screen; the others, by them as the board turns."
  [objects frame device]
  (let [id     (:id frame)
        fw     (dm/get-in frame [:selrect :width])
        [w h]  (turned-size frame device)
        turned (resize objects id w h)
        fixed  (into #{}
                     (comp (filter #(cfh/fixed-scroll? (get objects %)))
                           (mapcat #(cfh/get-children-ids-with-self objects %)))
                     (cfh/get-children-ids objects id))]
    (if (empty? fixed)
      turned
      ;; the board as its first screen, its layers where they are,
      ;; turned as the screen
      (let [screen (update objects id gsh/transform-shape
                           (ctm/change-size (get objects id) fw (dvd/board-first-screen-h frame)))
            screen (resize screen id (:screen-w device) (:screen-h device))]
        (reduce #(assoc %1 %2 (get screen %2)) turned fixed)))))

(defn turn-objects
  "`objects` of a page with each of its boards `frames` that shows turned
  in the device `choice` held `hold` (see `shown`) turned: as wide as the
  turned screen, its layers moved and stretched by their constraints."
  [objects frames choice hold]
  (if (nil? hold)
    objects
    (reduce (fn [objects frame]
              (let [{:keys [device turned?]} (shown frames frame choice hold)]
                (cond-> objects
                  (and turned? (cfh/root-frame? frame))
                  (turn-board frame device))))
            objects
            frames)))
