;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.timeline.keys
  "The keys that play animations: Space in the timeline, and a tap on it
  for an animated board on the canvas (see `viewport.widgets`)."
  (:require
   [app.util.dom :as dom]
   [app.util.keyboard :as kbd]
   [app.util.text.ui :as txu]))

(defn editing-text?
  "Whether a key event comes from a text field, which keeps its keys."
  [event]
  (let [target (dom/get-target event)]
    (or (contains? #{"INPUT" "TEXTAREA"} (dom/get-tag-name target))
        (some-> target .-isContentEditable)
        (txu/some-text-editor-content? target))))

(defn space?
  "Whether a key event, of the browser or of goog, is Space."
  [^js event]
  (let [native (if (fn? (.-getBrowserEvent event))
                 (.getBrowserEvent event)
                 event)]
    (or (kbd/space? event) (kbd/space? native))))

(defn plain-space?
  "Unmodified Space, outside a text field. Key repeat does not count, so
  holding Space does not toggle on every repeat. Ctrl + Space, on macOS
  too, and Shift + Space maximize the timeline (see
  `dwa/toggle-timeline-maximized`)."
  [^js event]
  (let [native (if (fn? (.-getBrowserEvent event))
                 (.getBrowserEvent event)
                 event)]
    (and (space? event)
         (not (.-repeat native))
         (not (editing-text? event))
         (not (kbd/shift? event))
         (not (kbd/alt? event))
         (not (kbd/ctrl? event))
         (not (kbd/mod? event)))))
