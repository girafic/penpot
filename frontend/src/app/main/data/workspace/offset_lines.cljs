;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.offset-lines
  "Offset lines (Penpot Motion, see `app.common.geom.offset-lines`): a
  group of paths parallel to the outline of a shape, its source, which it
  holds hidden; made from a shape and made again as its settings change
  or its source does, but in a copy of a component, which takes them from
  its main."
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.geom.offset-lines :as gol]
   [app.common.schema :as sm]
   [app.common.types.component :as ctk]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.groups :as dwg]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn- line-shapes
  "The paths of the lines `spec` makes from `source`, in the group
  `group-id` of the board `frame-id`."
  [source spec group-id frame-id]
  (into []
        (map
         (fn [{:keys [number content color opacity width]}]
           (cts/setup-shape {:id (uuid/next)
                             :type :path
                             ;; by its number, which accents name
                             :name (str "Line " number)
                             :content content
                             :parent-id group-id
                             :frame-id frame-id
                             :offset-line :line
                             :fills []
                             :strokes [{:stroke-color color
                                        :stroke-opacity opacity
                                        :stroke-width width
                                        :stroke-style :solid
                                        :stroke-alignment :center}]})))
        (gol/lines source spec)))

(defn editable?
  "Whether the offset lines of `group` of `objects` can be made again: it
  holds their source (see `gol/layers-of`) and is no copy of a component,
  which takes them from its main."
  [objects group]
  (boolean (and (some? (gol/layers-of objects group))
                (not (ctk/in-component-copy? group)))))

(defn set-offset-lines
  "Make the lines of the group of offset lines `group-id` again, its
  settings with `changes` taken into them (see `gol/defaults`): the lines
  it made go, its source stays hidden, what else it holds stays. The
  first time, `source-id` is the layer they are made from. Nothing when
  the group cannot make them (see `editable?`)."
  ([group-id] (set-offset-lines group-id nil nil))
  ([group-id changes] (set-offset-lines group-id changes nil))
  ([group-id changes source-id]
   (ptk/reify ::set-offset-lines
     ptk/WatchEvent
     (watch [it state _]
       (let [page-id (:current-page-id state)
             objects (dsh/lookup-page-objects state page-id)
             group   (get objects group-id)
             layers  (if (some? source-id)
                       {:source source-id :lines []}
                       (gol/layers-of objects group))
             source  (get objects (:source layers))
             ;; a change to nil takes the setting away (no stripes)
             spec    (-> (merge gol/defaults (:offset-lines group) changes)
                         (dissoc :source)
                         (d/without-nils))]
         (if (and (some? group) (some? source)
                  (or (some? source-id) (editable? objects group)))
           (let [lines   (line-shapes source spec group-id (:frame-id group))
                 changes (-> (pcb/empty-changes it page-id)
                             (pcb/with-objects objects)
                             ;; named so the first time
                             (pcb/update-shapes [group-id] #(cond-> (assoc % :offset-lines spec)
                                                              (nil? (:offset-lines %)) (assoc :name "Offset lines")))
                             (pcb/update-shapes [(:id source)] #(assoc % :hidden true :offset-line :source))
                             (pcb/remove-objects (:lines layers))
                             (pcb/add-objects lines)
                             (pcb/resize-parents [group-id]))]
             (rx/of (dch/commit-changes changes)))
           (rx/empty)))))))

(defn make-offset-lines
  "Make offset lines of the shape `source-id` (see `gol/lines`): a group
  `group-id` (a new one without) holding it hidden and its lines, white
  outside, the colour of its first stroke inside, `spec` taken into
  them; in one undo step, the group selected when `select?`."
  ([source-id] (make-offset-lines source-id (uuid/next) nil true))
  ([source-id group-id spec select?]
   (ptk/reify ::make-offset-lines
     ptk/WatchEvent
     (watch [_ state _]
       (let [objects (dsh/lookup-page-objects state)
             source  (get objects source-id)
             stroke  (first (:strokes source))
             spec    (merge gol/defaults
                            (when-let [color (:stroke-color stroke)]
                              (when (sm/validate cts/schema:six-digit-hex color)
                                {:inner-color color}))
                            spec)
             undo-id (js/Symbol)]
         (if (some? source)
           (rx/of (dwu/start-undo-transaction undo-id)
                  (dwg/group-shapes group-id [source-id])
                  (set-offset-lines group-id spec source-id)
                  (when select?
                    (dws/select-shapes (into (d/ordered-set) [group-id])))
                  (dwu/commit-undo-transaction undo-id))
           (rx/empty)))))))
