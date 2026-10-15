;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.svg-animation
  "Import of animated SVGs, with SMIL or CSS animations: a board with the
  layers of the SVG and its timeline (see
  `app.common.files.svg-animation`), as one undo step."
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.shapes-builder :as sb]
   [app.common.files.svg-animation :as svga]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

;; The features `svga/svg->shapes` counts:
;; (tr "workspace.svg-animation.feature.attribute")
;; (tr "workspace.svg-animation.feature.dash")
;; (tr "workspace.svg-animation.feature.filter")
;; (tr "workspace.svg-animation.feature.gradient")
;; (tr "workspace.svg-animation.feature.interactive")
;; (tr "workspace.svg-animation.feature.loop-length")
;; (tr "workspace.svg-animation.feature.paint")
;; (tr "workspace.svg-animation.feature.path-morph")
;; (tr "workspace.svg-animation.feature.skew")
;; (tr "workspace.svg-animation.feature.text-style")

(defn- skipped-message
  [skipped]
  (tr "workspace.svg-animation.imported-without"
      (->> (keys skipped)
           (map #_:clj-kondo/ignore #(tr (str "workspace.svg-animation.feature." (name %))))
           (sort)
           (str/join ", "))))

(defn svg-animation-imported
  "Add the board of the animated SVG `svg-data` (parsed, its images
  uploaded into `:image-data`) centred on `position`, with its timeline,
  and select it."
  [svg-data position]
  (ptk/reify ::svg-animation-imported
    ptk/WatchEvent
    (watch [it state _]
      (let [page     (dsh/lookup-page state)
            [_ _ w h] (sb/svg-dimensions svg-data)
            {:keys [shapes timeline skipped]}
            (svga/svg->shapes svg-data {:x (- (:x position) (/ w 2))
                                        :y (- (:y position) (/ h 2))})
            board-id (:id (first shapes))
            changes  (as-> (-> (pcb/empty-changes it (:id page))
                               (pcb/with-page page)
                               (pcb/with-objects (:objects page))) changes
                       (reduce pcb/add-object changes shapes)
                       (pcb/change-timeline changes board-id timeline))
            undo-id  (js/Symbol)]
        (rx/concat
         (rx/of (dwu/start-undo-transaction undo-id)
                (dch/commit-changes changes)
                (dws/select-shapes (d/ordered-set board-id))
                (dwu/commit-undo-transaction undo-id))
         (if (seq skipped)
           (rx/of (ntf/warn (skipped-message skipped)))
           (rx/empty)))))))
