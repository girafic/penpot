;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.canvas
  "The canvas of a phone in the view mode: the page as a whole, drawn by
  WASM on the OffscreenCanvas the boards use (see
  `rwv/render-page-view`), at a view of its own (see
  `app.main.data.viewer.canvas`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.math :as mth]
   [app.main.data.viewer.canvas :as dvc]
   [app.main.render-viewer-wasm :as rwv]
   [app.render-wasm.api :as wasm.api]
   [app.util.dom :as dom]
   [rumext.v2 :as mf]))

(mf/defc canvas-viewport*
  "The `page` as a whole on a screen of `size` (`{:width :height}`, CSS
  px: the section of the view mode), from its start view, about its
  middle as the phone turns, at its start view again on another page.
  `on-drawn` once it shows (and `data-canvas-drawn` on the canvas)."
  [{:keys [page size on-drawn]}]
  (let [canvas-ref (mf/use-ref nil)
        page-id    (:id page)
        objects    (mf/with-memo [page] (dvc/canvas-objects (:objects page)))
        bounds     (mf/with-memo [objects] (dvc/content-bounds objects))
        width      (:width size 0)
        height     (:height size 0)
        dpr        (wasm.api/get-dpr)
        ;; the view, with the page and the screen it is for: it changes
        ;; at each step of a gesture, so it is no state that renders
        view-ref   (mf/use-ref nil)]

    (mf/with-effect [page-id objects bounds width height dpr]
      (let [canvas (mf/ref-val canvas-ref)]
        (when (and (some? canvas) (some? bounds) (pos? width) (pos? height))
          (let [screen {:width width :height height}
                prev   (mf/ref-val view-ref)
                view   (if (= page-id (:page-id prev))
                         (dvc/resize-view (:view prev) (:screen prev) screen bounds)
                         (dvc/initial-view bounds screen))]
            (mf/set-ref-val! view-ref {:page-id page-id :screen screen :view view})
            (rwv/render-page-view page-id objects canvas (:zoom view) (dvc/view-box view screen)
                                  (fn []
                                    (dom/set-attribute! canvas "data-canvas-drawn" "true")
                                    (when (fn? on-drawn)
                                      (on-drawn))))))))

    [:canvas {:ref canvas-ref
              :class (stl/css :canvas)
              :data-dpr dpr
              :width (mth/round (* width dpr))
              :height (mth/round (* height dpr))}]))
