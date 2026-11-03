;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.background
  "The background of the prototype of a page (Penpot Motion): a color or
  a gradient round its boards in the view mode, round a device too, and
  in the preview of the workspace round a board without one while it
  loads. It is set in the prototype tab (see
  `prototype-background-menu*`)."
  (:require
   [app.common.animation.css :as acss]))

(defn background
  "The background round the boards of `page` in the view mode: the color
  or the gradient of its prototype, round a device too; nil for the own
  color of the view mode, and round a device alone in the preview of the
  workspace (`device-only?`), round which the workspace shows."
  [page device-only?]
  (when-not device-only?
    (:prototype-background page)))

(defn background-style
  "The style of a box `width` by `height` (px) drawn with `background`
  (see `background`): its color, or its gradient as the canvas draws it
  on a shape of that size, over the own color of the box; nil without
  one."
  [background width height]
  (cond
    (string? background)
    #js {:backgroundColor background}

    (map? background)
    #js {:backgroundImage (-> (acss/css-gradient background
                                                 (max 1 (or width 0))
                                                 (max 1 (or height 0)))
                              (acss/css-gradient-image))}))
