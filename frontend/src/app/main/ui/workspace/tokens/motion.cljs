;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.motion
  "The design tokens the settings of animations bind (Penpot Motion): the
  presets of layers, the entries of animation styles and the easings of
  keyframes."
  (:require
   [app.common.types.tokens-lib :as ctob]
   [app.main.data.style-dictionary :as sd]
   [app.main.refs :as refs]
   [rumext.v2 :as mf]))

(defn use-active-tokens-by-type
  "The design tokens of the active sets of the current file, resolved and
  by type."
  []
  (let [tokens-lib (mf/deref refs/tokens-lib)
        active     (mf/with-memo [tokens-lib]
                     (if (some? tokens-lib)
                       (ctob/get-tokens-in-active-sets tokens-lib)
                       {}))
        resolved   (sd/use-resolved-tokens active)]
    (mf/with-memo [resolved]
      (ctob/group-by-type resolved))))
