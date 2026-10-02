;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.management.forms.cubic-bezier
  "The form of a cubic bezier token (Penpot Motion): its four numbers,
  written like `0.42, 0, 0.58, 1`, or a reference, and the curve they
  draw."
  (:require
   [app.common.files.tokens :as cfo]
   [app.common.schema :as sm]
   [app.common.types.token :as cto]
   [app.main.data.workspace.tokens.errors :as wte]
   [app.main.ui.forms :as fc]
   [app.main.ui.workspace.timeline.easing :refer [curve-preview*]]
   [app.main.ui.workspace.tokens.management.forms.controls :as token.controls]
   [app.main.ui.workspace.tokens.management.forms.generic-form :as generic]
   [app.main.ui.workspace.tokens.management.forms.validators :refer [check-self-reference default-validate-token]]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn- reference?
  [text]
  (str/includes? text "{"))

(defn- text->value
  "The value of a cubic bezier token from the text of its field: its four
  numbers, or the text as it is (a reference, or a text the validation
  refuses)."
  [text]
  (let [text (str/trim (or text ""))]
    (if (reference? text)
      text
      (or (cfo/parse-cubic-bezier-value text) text))))

(defn- value->text
  [value]
  (if (sequential? value)
    (str/join ", " value)
    (or value "")))

(defn- check-cubic-bezier
  [{:keys [name value]}]
  (cond
    (and (string? value) (str/blank? value))
    (wte/get-error-code :error.token/empty-input)

    (and (string? value) (not (reference? value)))
    (wte/error-with-value :error.style-dictionary/invalid-token-value-cubic-bezier value)

    (string? value)
    (check-self-reference name value)))

(defn- validate-cubic-bezier-token
  [props]
  (-> props
      (update :token-value text->value)
      (assoc :validators [check-cubic-bezier])
      (default-validate-token)))

(mf/defc input*
  "The field of the value and, under it, the curve it draws: that of the
  token it refers to, for a reference to one of numbers."
  {::mf/private true}
  [{:keys [tokens] :as props}]
  (let [form  (mf/use-ctx fc/context)
        text  (get-in @form [:data :value] "")
        curve (mf/with-memo [text tokens]
                (let [value (text->value text)]
                  (if (string? value)
                    (some-> (first (cto/find-token-value-references value))
                            (as-> token-name (get tokens token-name))
                            (:value)
                            (cfo/parse-cubic-bezier-value))
                    value)))]
    [:*
     [:> token.controls/input* props]
     (when (some? curve)
       [:> curve-preview* {:curve curve}])]))

(mf/defc form*
  [{:keys [token token-type current-token-path] :rest props}]
  (let [token
        (mf/with-memo [token]
          (if token
            (update token :value value->text)
            {:type token-type}))
        props (mf/spread-props props {:token token
                                      :token-type token-type
                                      :make-schema #(-> (cfo/make-token-schema %1 token-type current-token-path)
                                                        (sm/dissoc-key :id)
                                                        ;; The value as edited in the form is a text,
                                                        ;; which the validator turns into numbers.
                                                        (sm/assoc-key :value cfo/schema:token-value-generic))
                                      :input-value-placeholder (tr "workspace.tokens.cubic-bezier-value-enter")
                                      :validator validate-cubic-bezier-token
                                      :input-component input*})]
    [:> generic/form* props]))
