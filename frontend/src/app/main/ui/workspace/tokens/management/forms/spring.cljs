;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.management.forms.spring
  "The form of a spring token (Penpot Motion): its stiffness, damping and
  mass, written like `170, 26, 1`, or a reference, and the motion of the
  spring (see `app.main.ui.workspace.tokens.management.forms.numbers`)."
  (:require
   [app.common.files.tokens :as cfo]
   [app.main.ui.forms :as fc]
   [app.main.ui.workspace.timeline.easing :refer [spring-preview*]]
   [app.main.ui.workspace.tokens.management.forms.controls :as token.controls]
   [app.main.ui.workspace.tokens.management.forms.numbers :as numbers]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn text->value
  "The value of a spring token from the text of its field: its stiffness,
  damping and mass in this order, or the text as it is (a reference, or a
  text the validation refuses)."
  [text]
  (numbers/text->value cfo/parse-spring-value text))

(defn value->text
  [value]
  (if (map? value)
    (str/join ", " (keep value cfo/spring-keys))
    (or value "")))

(mf/defc input*
  "The field of the value and, under it, the motion of the spring: that of
  the token it refers to, for a reference to one of numbers."
  {::mf/private true}
  [{:keys [tokens] :as props}]
  (let [form   (mf/use-ctx fc/context)
        text   (get-in @form [:data :value] "")
        spring (mf/with-memo [text tokens]
                 (some-> (numbers/referred-value cfo/parse-spring-value text tokens)
                         (assoc :type :spring)))]
    [:*
     [:> token.controls/input* props]
     (when (some? spring)
       [:> spring-preview* {:spring spring}])]))

(mf/defc form*
  [props]
  [:> numbers/form* (mf/spread-props props {:parse cfo/parse-spring-value
                                            :format value->text
                                            :error-code :error.style-dictionary/invalid-token-value-spring
                                            :placeholder (tr "workspace.tokens.spring-value-enter")
                                            :input-component input*})])
