;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.inspect.motion
  "The animation of the selection as the Inspect shows it (Penpot
  Motion), in the workspace and in the view mode alike: a read-only
  timeline and its code (see `app.util.code-gen.motion`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.main.data.event :as ev]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.code-block :refer [code-block*]]
   [app.main.ui.components.copy-button :refer [copy-button*]]
   [app.main.ui.components.radio-buttons :refer [radio-button radio-buttons]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.product.empty-state :refer [empty-state*]]
   [app.main.ui.hooks.resize :refer [use-resize-hook]]
   [app.main.ui.icons :as deprecated-icon]
   [app.main.ui.inspect.motion-timeline :refer [motion-timeline* motion-rows]]
   [app.util.code-gen.motion :as cgm]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(defn use-motion-page
  "The page the Inspect shows, `{:objects :timelines}`: the one of the
  workspace or the page `page-id` of the view mode (`from`), with its
  `objects`."
  [from page-id objects]
  (let [timelines-ref
        (mf/with-memo [from page-id]
          (if (= from :workspace)
            (l/derived :timelines refs/workspace-page)
            (l/derived #(dm/get-in % [:viewer :pages page-id :timelines]) st/state =)))
        timelines (mf/deref timelines-ref)]
    (mf/with-memo [objects timelines]
      {:objects objects :timelines timelines})))

(defn- notes
  "What the code in `format` leaves out of the animation of `codes` (the
  code models of the boards): split text played whole and audio, which
  only the JSON gives."
  [codes format]
  (when (not= "json" format)
    (cond-> []
      (some (comp seq :splits) codes)
      (conj (tr "workspace.options.export.split-text-whole"))

      (some #(seq (get-in % [:timeline :audio-clips])) codes)
      (conj (tr "workspace.options.export.no-audio")))))

(mf/defc motion-tab*
  "The Motion tab of the Inspect: the animation of the selection, `shapes`
  of `objects`, as a read-only timeline and as code in a format to choose
  (CSS, JSON, React for motion.dev), with what the
  format leaves out, or a note that nothing in it animates. `libraries`
  holds the files of the components, by id; `page-id` is the page shown."
  [{:keys [shapes objects page-id libraries from on-expand]}]
  (let [format*    (mf/use-state "css")
        format     (deref format*)
        collapsed* (mf/use-state false)
        collapsed? (deref collapsed*)

        page       (use-motion-page from page-id objects)

        ids
        (mf/with-memo [shapes objects]
          (into #{} (cfh/selected-with-children objects (map :id shapes))))

        codes
        (mf/with-memo [page libraries ids]
          (cgm/code-of-boards page libraries ids))

        code
        (mf/with-memo [page libraries ids format]
          (case format
            "json"  (some-> (cgm/animation-json page libraries ids) (cgm/json-text))
            "react" (cgm/animation-react page libraries ids)
            (cgm/animation-css page libraries ids)))

        notes
        (mf/with-memo [codes format]
          (notes codes format))

        boards
        (mf/with-memo [codes objects ids]
          (motion-rows codes objects ids))

        {on-pointer-down :on-pointer-down
         on-lost-pointer-capture :on-lost-pointer-capture
         on-pointer-move :on-pointer-move
         size :size}
        (use-resize-hook :motion-code 400 100 800 :y false :bottom)

        on-format
        (mf/use-fn #(reset! format* %))

        on-collapse
        (mf/use-fn #(swap! collapsed* not))

        copy-code
        (mf/use-fn (mf/deps code) (fn [] code))

        on-copied
        (mf/use-fn
         (mf/deps format from)
         (fn []
           (st/emit! (ev/event {::ev/name "copy-inspect-code"
                                ::ev/origin (if (= :workspace from) "workspace" "viewer")
                                :type (dm/str "animation-" format)}))))]

    [:div {:class (stl/css-case :motion-tab true
                                :viewer (= :viewer from))}
     (when (seq boards)
       [:> motion-timeline* {:boards boards}])

     (if (str/blank? code)
       (when (empty? boards)
         [:div {:class (stl/css :empty)}
          [:> empty-state* {:icon i/motion
                            :text (tr "inspect.motion.empty")}]])
       [:div {:class (stl/css-case :code-block true
                                   :collapsed collapsed?)}
        [:div {:class (stl/css :code-row-lang)}
         [:button {:class (stl/css :toggle-btn)
                   :aria-expanded (not collapsed?)
                   :on-click on-collapse}
          [:span {:class (stl/css-case :collapsabled-icon true
                                       :rotated collapsed?)}
           deprecated-icon/arrow]]

         [:& radio-buttons {:selected format
                            :on-change on-format
                            :class (stl/css :code-lang-options)
                            :wide true
                            :name "motion-code-format"}
          [:& radio-button {:value "css" :id :motion-css}]
          [:& radio-button {:value "json" :id :motion-json}]
          [:& radio-button {:value "react" :id :motion-react}]]

         [:div {:class (stl/css :action-btns)}
          [:button {:class (stl/css :expand-button)
                    :on-click on-expand}
           deprecated-icon/code]
          [:> copy-button* {:data copy-code
                            :class (stl/css :copy-btn)
                            :on-copied on-copied}]]]

        (when-not collapsed?
          [:div {:class (stl/css :code-row-display)
                 :style {:--code-height (dm/str (or size 400) "px")}}
           [:> code-block* {:type (if (= "react" format) "jsx" format)
                            :code code}]])

        [:div {:class (stl/css :resize-area)
               :on-pointer-down on-pointer-down
               :on-lost-pointer-capture on-lost-pointer-capture
               :on-pointer-move on-pointer-move}]

        (for [note notes]
          [:p {:key note :class (stl/css :note)} note])])]))
