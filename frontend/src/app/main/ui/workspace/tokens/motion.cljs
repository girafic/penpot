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
   [app.common.files.tokens :as cfo]
   [app.common.types.tokens-lib :as ctob]
   [app.config :as cf]
   [app.main.data.style-dictionary :as sd]
   [app.main.data.tokenscript :as ts]
   [app.main.refs :as refs]
   [rumext.v2 :as mf]))

(defn use-active-tokens-by-type
  "The design tokens of the active sets of the current file, resolved and
  by type: for what is not in the right sidebar, which gets them from
  its context (`app.main.ui.context/active-tokens-by-type`). Tokenscript
  resolves once per change of the tokens."
  []
  (let [tokens-lib    (mf/deref refs/tokens-lib)
        tokens-status (mf/deref refs/tokens-status)
        tokenscript?  (contains? cf/flags :tokenscript)
        active        (mf/with-memo [tokens-lib tokens-status]
                        (if (and (some? tokens-lib) (some? tokens-status))
                          (cfo/get-tokens-in-active-sets tokens-status tokens-lib)
                          {}))
        by-script     (mf/with-memo [active tokenscript?]
                        (when tokenscript?
                          (ts/resolve-tokens active)))
        resolved      (sd/use-resolved-tokens (if tokenscript? {} active))]
    (mf/with-memo [resolved by-script]
      (ctob/group-by-type (or by-script resolved)))))
