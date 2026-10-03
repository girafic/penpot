;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.management.forms.cubic-bezier
  "The form of a cubic bezier token (Penpot Motion): its four numbers,
  written like `0.42, 0, 0.58, 1`, or a reference, and the curve they
  draw (see `app.main.ui.workspace.tokens.management.forms.numbers`)."
  (:require
   [app.common.files.tokens :as cfo]
   [app.main.ui.forms :as fc]
   [app.main.ui.workspace.timeline.easing :refer [curve-preview*]]
   [app.main.ui.workspace.tokens.management.forms.controls :as token.controls]
   [app.main.ui.workspace.tokens.management.forms.numbers :as numbers]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn- value->text
  [value]
  (if (sequential? value)
    (str/join ", " value)
    (or value "")))

(mf/defc input*
  "The field of the value and, under it, the curve it draws: that of the
  token it refers to, for a reference to one of numbers."
  {::mf/private true}
  [{:keys [tokens] :as props}]
  (let [form  (mf/use-ctx fc/context)
        text  (get-in @form [:data :value] "")
        curve (mf/with-memo [text tokens]
                (numbers/referred-value cfo/parse-cubic-bezier-value text tokens))]
    [:*
     [:> token.controls/input* props]
     (when (some? curve)
       [:> curve-preview* {:curve curve}])]))

(mf/defc form*
  [props]
  [:> numbers/form* (mf/spread-props props {:parse cfo/parse-cubic-bezier-value
                                            :format value->text
                                            :error-code :error.style-dictionary/invalid-token-value-cubic-bezier
                                            :placeholder (tr "workspace.tokens.cubic-bezier-value-enter")
                                            :input-component input*})])
