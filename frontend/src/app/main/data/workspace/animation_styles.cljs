;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.animation-styles
  "The animation styles of the current file (Penpot Motion): adding,
  editing, renaming, duplicating and deleting them, with the sync of
  their uses in one change set, and the repair of uses that drifted."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.logic.libraries :as cll]
   [app.common.path-names :as cpn]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.event :as ev]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.animation :as dwa]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(defn add-animation-style
  "Add the animation style `style` to the current file."
  [style]
  (let [style (-> (update style :id #(or % (uuid/next)))
                  (ctas/check-animation-style))]
    (ptk/reify ::add-animation-style
      ev/Event
      (-data [_] {:id (:id style)})

      ptk/WatchEvent
      (watch [it _ _]
        (rx/of (dch/commit-changes (-> (pcb/empty-changes it)
                                       (pcb/add-animation-style style))))))))

(defn- commit-animation-styles
  "Commit `changes` to the animation styles of the current file together
  with the sync of their uses in it (see `cll/generate-sync-timelines`),
  `styles` being its styles after them: the uses of the style
  `asset-id`, or of every style when nil. One change set, so one undo
  step, and the canvas shows it."
  [changes state styles asset-id]
  (let [file-id   (:current-file-id state)
        libraries (assoc-in (dsh/lookup-libraries state) [file-id :data :animation-styles] styles)
        changes   (cll/generate-sync-timelines changes file-id asset-id file-id libraries)]
    (rx/of (dch/commit-changes changes)
           (dwa/apply-preview))))

(defn- end-style-preview
  "`state` without the preview of a style being edited (see
  `dwa/preview-animation-style`): what follows commits it."
  [state]
  (d/update-when state :workspace-animation dissoc :style-preview))

(defn- local-style
  "The animation style `id` of the current file, the file `file-id`; nil
  for a style of another file."
  [state file-id id]
  (when (= file-id (:current-file-id state))
    (ctas/get-animation-style (dsh/lookup-file-data state) id)))

(defn update-animation-style
  "Change the animation style `style` of the current file, the file
  `file-id`, and sync its uses, in one undo step. A style that is not one
  of the current file changes nothing."
  [style file-id]
  (assert (uuid? file-id) "expected valid uuid for `file-id`")
  (let [style (ctas/check-animation-style style)]
    (ptk/reify ::update-animation-style
      ev/Event
      (-data [_] {:id (:id style)})

      ptk/UpdateEvent
      (update [_ state]
        (end-style-preview state))

      ptk/WatchEvent
      (watch [it state _]
        (when (some? (local-style state file-id (:id style)))
          (let [data (dsh/lookup-file-data state)]
            (-> (pcb/empty-changes it)
                (pcb/with-library-data data)
                (pcb/update-animation-style style)
                (commit-animation-styles state (assoc (:animation-styles data) (:id style) style) (:id style)))))))))

(defn update-style-from-layer
  "Save what the shape `shape-id` plays now into the animation style of
  the current file its use `use-id` plays (see `ctas/update-from-track`):
  the style takes its presets, the entries of its uses and its rows of
  keyframes, a use of it takes their place, and every use of the style in
  the file follows, in one undo step. Refused, with a message, when they
  hold more entries or keyframes than a style does; a use of a style of a
  library changes nothing."
  [shape-id use-id]
  (ptk/reify ::update-style-from-layer
    ptk/WatchEvent
    (watch [it state _]
      (let [file-id  (:current-file-id state)
            data     (dsh/lookup-file-data state)
            page     (dsh/lookup-page state)
            tl       (when (dwa/motion? state) (dwa/shape-timeline state shape-id))
            use      (cta/get-animation tl shape-id use-id)
            style    (when (and (cta/style-use? use) (= file-id (:style-ref-file use)))
                       (dm/get-in data [:animation-styles (:style-ref-id use)]))
            result   (when (some? style)
                       (ctas/update-from-track (cta/get-track tl shape-id) use-id
                                               (cta/rest-pose tl (:objects page) shape-id)
                                               style file-id))]
        (cond
          (nil? result)
          (rx/empty)

          (= :too-many-entries (:error result))
          (rx/of (ntf/warn (tr "workspace.animation.style-too-many-entries"
                               cta/max-style-entries (:count result))))

          (= :too-many-keyframes (:error result))
          (rx/of (ntf/warn (tr "workspace.animation.style-too-many-keyframes"
                               cta/max-style-keyframes (:count result))))

          (some? (:error result))
          (rx/empty)

          :else
          (let [style     (ctas/check-animation-style (:style result))
                board-id  (:board-id tl)
                tl'       (assoc-in tl [:tracks shape-id] (:track result))
                styles    (assoc (:animation-styles data) (:id style) style)
                ;; the sync starts from the new track, which already plays
                ;; the new style, so it writes only the other uses
                libraries (-> (dsh/lookup-libraries state)
                              (assoc-in [file-id :data :animation-styles] styles)
                              (assoc-in [file-id :data :pages-index (:id page) :timelines board-id] tl'))
                changes   (-> (pcb/empty-changes it)
                              (pcb/with-library-data data)
                              (pcb/update-animation-style style)
                              (pcb/with-page page)
                              (pcb/set-timeline-track board-id shape-id (:track result))
                              (cll/generate-sync-timelines file-id (:id style) file-id libraries))]
            (rx/of (dch/commit-changes changes)
                   (dwa/apply-preview))))))))

(defn update-animation-styles
  "Change the animation styles `styles` of the current file, the file
  `file-id`, and sync the uses of all its styles, in one change set: the
  styles its design tokens set (see `ctas/token-resolved-styles` and
  `ctas/token-renamed-styles`). Styles of other files are left out."
  [styles file-id]
  (assert (uuid? file-id) "expected valid uuid for `file-id`")
  (let [styles (mapv ctas/check-animation-style styles)]
    (ptk/reify ::update-animation-styles
      ptk/WatchEvent
      (watch [it state _]
        (let [local (filterv #(some? (local-style state file-id (:id %))) styles)]
          (when (seq local)
            (let [data    (dsh/lookup-file-data state)
                  changes (reduce pcb/update-animation-style
                                  (-> (pcb/empty-changes it)
                                      (pcb/with-library-data data))
                                  local)
                  styles' (reduce #(assoc %1 (:id %2) %2) (:animation-styles data) local)]
              (commit-animation-styles changes state styles' nil))))))))

(defn- renamed
  "`style` named `new-name`, which puts it in groups when it has slashes."
  [style new-name]
  (let [[path name] (cpn/split-group-name new-name)]
    (assoc style :path path :name name)))

(defn rename-animation-style
  "Rename the animation style `id` of the current file, the file
  `file-id`, and its uses."
  [file-id id new-name]
  (assert (uuid? file-id) "expected valid uuid for `file-id`")
  (assert (uuid? id) "expected valid uuid for `id`")
  (ptk/reify ::rename-animation-style
    ev/Event
    (-data [_] {:id id :name new-name})

    ptk/WatchEvent
    (watch [_ state _]
      (when-let [style (local-style state file-id id)]
        (when-not (str/blank? new-name)
          (rx/of (update-animation-style (renamed style new-name) file-id)))))))

(defn rename-animation-styles
  "Rename several animation styles of the current file, the file
  `file-id`, at once: `names` holds the new name, with its groups, of
  each, by id. As grouping, renaming a group or ungrouping does: one
  change set, one sync, one undo step."
  [file-id names]
  (assert (uuid? file-id) "expected valid uuid for `file-id`")
  (ptk/reify ::rename-animation-styles
    ptk/WatchEvent
    (watch [it state _]
      (let [data   (dsh/lookup-file-data state)
            styles (into []
                         (keep (fn [[id new-name]]
                                 (when-let [style (local-style state file-id id)]
                                   (when-not (str/blank? new-name)
                                     (renamed style new-name)))))
                         names)]
        (when (seq styles)
          (-> (reduce pcb/update-animation-style
                      (-> (pcb/empty-changes it)
                          (pcb/with-library-data data))
                      styles)
              (commit-animation-styles state
                                       (into (:animation-styles data) (map (juxt :id identity)) styles)
                                       nil)))))))

(defn duplicate-animation-style
  "Add a copy of the animation style `style-id` of the current file, the
  file `file-id`, with entries of its own."
  [file-id style-id]
  (assert (uuid? file-id) "expected valid uuid for `file-id`")
  (assert (uuid? style-id) "expected valid uuid for `style-id`")
  (ptk/reify ::duplicate-animation-style
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [style (local-style state file-id style-id)]
        (rx/of (add-animation-style
                (-> style
                    (assoc :id (uuid/next))
                    (dissoc :modified-at)
                    (update :entries (partial mapv #(assoc % :id (uuid/next)))))))))))

(defn delete-animation-style
  "Delete the animation style `id` of the current file; its uses turn into
  their presets in the same undo step."
  [id]
  (assert (uuid? id) "expected valid uuid for `id`")
  (ptk/reify ::delete-animation-style
    ev/Event
    (-data [_] {:id id})

    ptk/UpdateEvent
    (update [_ state]
      (end-style-preview state))

    ptk/WatchEvent
    (watch [it state _]
      (let [data (dsh/lookup-file-data state)]
        (when (some? (ctas/get-animation-style data id))
          (-> (pcb/empty-changes it)
              (pcb/with-library-data data)
              (pcb/delete-animation-style id)
              (commit-animation-styles state (dissoc (:animation-styles data) id) id)))))))

(defn- drifted-uses?
  "Whether a use of a style of the current file differs from it on some
  page (see `ctas/drifted?`)."
  [state]
  (let [file-id (:current-file-id state)
        data    (dsh/lookup-file-data state)
        styles  (:animation-styles data)]
    (some (fn [page]
            (some #(ctas/drifted? % file-id styles) (vals (:timelines page))))
          (vals (:pages-index data)))))

(defn repair-local-style-uses
  "Sync the uses of the animation styles of the current file that differ
  from their style, as when a whole track was written with old entries
  after the style changed (see `ctas/drifted?`). It brings the file back
  to what it should be, so no undo step. Nothing when no use drifted or
  the file cannot be edited."
  []
  (ptk/reify ::repair-local-style-uses
    ptk/WatchEvent
    (watch [it state _]
      (when (and (dm/get-in state [:permissions :can-edit])
                 (drifted-uses? state))
        (let [file-id (:current-file-id state)
              changes (cll/generate-sync-timelines (pcb/empty-changes it) file-id nil file-id
                                                   (dsh/lookup-libraries state))]
          (when (seq (:redo-changes changes))
            (rx/of (dch/commit-changes (assoc changes :save-undo? false))
                   (dwa/apply-preview))))))))

(defn touches-local-styles?
  "Whether `changes` (of the file `file-id`) edit an animation style, or
  write a track, or its animations, holding a use of a style of that
  file: after them a use may differ from its style (see
  `repair-local-style-uses`)."
  [file-id changes]
  (letfn [(local-use? [animation]
            (and (cta/style-use? animation)
                 (= file-id (:style-ref-file animation))))]
    (some (fn [{:keys [type params animations]}]
            (case type
              :mod-animation-style           true
              :set-timeline-track            (some local-use? (:animations params))
              :set-timeline-track-animations (some local-use? animations)
              :set-timeline                  (some local-use? (mapcat :animations (vals (:tracks params))))
              false))
          changes)))
