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
  counterpart (\"Menu\" and \"Menu (landscape)\"), else the board itself
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
  "How `device` (see `dvd/resolve-device`) stands: `:landscape` on its
  side, else `:portrait`."
  [device]
  (if (:landscape? device) :landscape :portrait))

(defn- board-way
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

(defn counterpart
  "The board of `frames` that stands `way` (`:portrait` or `:landscape`),
  named as `frame` but for the words that say how it stands (see
  `base-name`); nil for none."
  [frames frame way]
  (let [base (base-name (:name frame))]
    (d/seek #(and (not= (:id %) (:id frame))
                  (cfh/root-frame? %)
                  (= way (board-way %))
                  (= base (base-name (:name %))))
            frames)))

(defn shown
  "How the board `frame` shows in the device `choice` (see
  `dvd/resolve-device`) held `hold` (nil, `:portrait` or `:landscape`):
  `{:frame-id :device :turned?}`, the board shown, it or its counterpart
  among `frames` (the boards of its page), the device it shows in (nil
  for none) and whether the board shows turned (see `turn-objects`).
  Held as the board stands, or without a hold or a device that turns, it
  shows as it is."
  [frames frame choice hold]
  (let [{:keys [width height]} (:selrect frame)
        device (dvd/resolve-device choice width height)]
    (if (or (nil? hold)
            (nil? device)
            (not (dvd/turns? (:kind device)))
            (= hold (way device)))
      {:frame-id (:id frame) :device device}
      (if-let [other (counterpart frames frame hold)]
        (let [{:keys [width height]} (:selrect other)]
          {:frame-id (:id other)
           :device (dvd/resolve-device (name (:kind device)) width height)})
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
  (see `dvd/first-screen-h`), by their constraints as it turns into the
  screen; the others, by them as the board turns."
  [objects frame device]
  (let [id     (:id frame)
        {fw :width fh :height} (:selrect frame)
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
                           (ctm/change-size (get objects id) fw (dvd/first-screen-h fw fh)))
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
