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
   [app.common.exceptions :as ex]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.shapes-builder :as sb]
   [app.common.files.svg-animation :as svga]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.svg-upload :as svg]
   [app.main.data.workspace.undo :as dwu]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(defn- feature-name
  "What the import of an animated SVG left out, a feature
  `svga/svg->shapes` counts, as the warning names it."
  [feature]
  (case feature
    :attribute   (tr "workspace.svg-animation.feature.attribute")
    :dash        (tr "workspace.svg-animation.feature.dash")
    :filter      (tr "workspace.svg-animation.feature.filter")
    :gradient    (tr "workspace.svg-animation.feature.gradient")
    :interactive (tr "workspace.svg-animation.feature.interactive")
    :loop-length (tr "workspace.svg-animation.feature.loop-length")
    :paint       (tr "workspace.svg-animation.feature.paint")
    :path-morph  (tr "workspace.svg-animation.feature.path-morph")
    :skew        (tr "workspace.svg-animation.feature.skew")
    :text-style  (tr "workspace.svg-animation.feature.text-style")
    :timing      (tr "workspace.svg-animation.feature.timing")
    (name feature)))

(defn- skipped-message
  [skipped]
  (tr "workspace.svg-animation.imported-without"
      (->> (keys skipped)
           (map feature-name)
           (sort)
           (str/join ", "))))

(defn svg-animation-imported
  "Add the board of the animated SVG `svg-data` (parsed, its images
  uploaded into `:image-data`) centred on `position`, with its timeline,
  and select it. An SVG the import fails on is an invalid one, as for
  the import of a static SVG (see `svg/add-svg-shapes`)."
  [svg-data position]
  (ptk/reify ::svg-animation-imported
    ptk/WatchEvent
    (watch [it state _]
      (try
        (let [page     (dsh/lookup-page state)
              [_ _ w h] (sb/svg-dimensions svg-data)
              {:keys [shapes timeline skipped]}
              (svga/svg->shapes svg-data {:x (- (:x position) (/ w 2))
                                          :y (- (:y position) (/ h 2))})
              shapes   (mapv svg/resolve-text-fonts shapes)
              board-id (:id (first shapes))
              changes  (as-> (-> (pcb/empty-changes it (:id page))
                                 (pcb/with-page page)
                                 (pcb/with-objects (:objects page))) changes
                         (reduce pcb/add-object changes shapes)
                         (pcb/change-timeline changes board-id timeline))
              undo-id  (js/Symbol)
              layout   (svg/text-layout state (:id page)
                                        (into [] (comp (filter #(= :text (:type %))) (map :id)) shapes)
                                        undo-id)]
          (rx/concat
           (rx/of (dwu/start-undo-transaction undo-id))
           (rx/from (:before layout))
           (rx/of (dch/commit-changes changes)
                  (dws/select-shapes (d/ordered-set board-id)))
           (rx/from (:after layout))
           (if (seq skipped)
             (rx/of (ntf/warn (skipped-message skipped)))
             (rx/empty))))

        (catch :default cause
          (js/console.error cause)
          (rx/throw (ex/error :type :svg-parser
                              :hint (ex-message cause))))))))
