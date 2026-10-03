;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.tokens.management.forms.numbers
  "The form of a token whose value is a few numbers written in a line, or
  a reference (Penpot Motion: a cubic bezier, a spring): the parts the
  forms of those types share, each with its own field and preview."
  (:require
   [app.common.files.tokens :as cfo]
   [app.common.schema :as sm]
   [app.common.types.token :as cto]
   [app.main.data.workspace.tokens.errors :as wte]
   [app.main.ui.workspace.tokens.management.forms.generic-form :as generic]
   [app.main.ui.workspace.tokens.management.forms.validators :refer [check-self-reference default-validate-token]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn- reference?
  [text]
  (str/includes? text "{"))

(defn text->value
  "The value of a token from the text of its field: the numbers `parse`
  reads in it, or the text as it is (a reference, or a text the
  validation refuses)."
  [parse text]
  (let [text (str/trim (or text ""))]
    (if (reference? text)
      text
      (or (parse text) text))))

(defn referred-value
  "The numbers the text `text` of a field stands for (see `text->value`):
  its own, or those of the token of `tokens` it refers to; nil without
  any."
  [parse text tokens]
  (let [value (text->value parse text)]
    (if (string? value)
      (some-> (first (cto/find-token-value-references value))
              (as-> token-name (get tokens token-name))
              (:value)
              (parse))
      value)))

(defn- check-value
  "The check of a value: empty, numbers `parse` refused (`error-code`), or
  a reference to the token itself."
  [error-code]
  (fn [{:keys [name value]}]
    (cond
      (and (string? value) (str/blank? value))
      (wte/get-error-code :error.token/empty-input)

      (and (string? value) (not (reference? value)))
      (wte/error-with-value error-code value)

      (string? value)
      (check-self-reference name value))))

(mf/defc form*
  "The form of a token of numbers: `parse` reads the text of its value,
  `format` writes it, `error-code` names a text that holds none,
  `placeholder` shows in the empty field, and `input-component` is the
  field (with its preview)."
  [{:keys [token token-type current-token-path parse format error-code placeholder input-component] :rest props}]
  (let [token
        (mf/with-memo [token format]
          (if token
            (update token :value format)
            {:type token-type}))

        validator
        (mf/with-memo [parse error-code]
          (fn [props]
            (-> props
                (update :token-value #(text->value parse %))
                (assoc :validators [(check-value error-code)])
                (default-validate-token))))

        props
        (mf/spread-props props {:token token
                                :token-type token-type
                                :make-schema #(-> (cfo/make-token-schema %1 token-type current-token-path)
                                                  (sm/dissoc-key :id)
                                                  ;; The value as edited in the form is a text,
                                                  ;; which the validator turns into numbers.
                                                  (sm/assoc-key :value cfo/schema:token-value-generic))
                                :input-value-placeholder placeholder
                                :validator validator
                                :input-component input-component})]
    [:> generic/form* props]))
