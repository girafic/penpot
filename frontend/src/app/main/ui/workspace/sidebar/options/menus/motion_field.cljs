;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.motion-field
  "A number of the Penpot Motion menus (the light and the depth cues of a
  board, offset lines), a word before it and its unit after it."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.math :as mth]
   [app.main.ui.components.numeric-input :as deprecated-input]
   [app.main.ui.workspace.sidebar.options.menus.motion :refer [keyframe-diamond*]]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(mf/defc labelled-number*
  "A number shown with its `label` and `unit`: `value` (`:multiple`
  mixed), `scale` times what it keeps (percent of a share); `on-change`
  gets what it keeps, a number. With `keyframes`, the diamond of that
  property, in motion mode. It keeps the input of the measures: the one of
  the design system shows no unit after its value."
  [{:keys [label hint unit value scale min max disabled on-change keyframes]}]
  (let [scale     (or scale 1)
        on-change (mf/use-fn
                   (mf/deps on-change scale)
                   (fn [value]
                     (when (number? value)
                       (on-change (/ value scale)))))]
    [:div {:class (stl/css :field) :title hint}
     [:span {:class (stl/css :label)} label]
     [:> deprecated-input/numeric-input*
      {:no-validate true
       :disabled disabled
       :min min
       :max max
       :aria-label hint
       :placeholder (if (= :multiple value) (tr "settings.multiple") "0")
       :on-change on-change
       :class (stl/css :input)
       :value (when (number? value) (mth/precision (* scale value) 2))}]
     [:span {:class (stl/css :unit)} unit]
     (when (some? keyframes)
       [:> keyframe-diamond* {:properties [keyframes]}])]))
