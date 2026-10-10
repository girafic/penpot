;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.options.menus.export-issues
  "The issues of an Animated export, as Figma lists them: what the format
  chosen plays otherwise than the canvas (see `dea/export-issues`),
  counted in a pill that opens their list beside the sidebar."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [c tr]]
   [app.util.keyboard :as kbd]
   [rumext.v2 :as mf]))

(def ^:private issue-icons
  {:lottie/images   i/img
   :svg/appearance  i/drop-shadow
   :svg/size        i/size-horizontal
   :lottie/size     i/size-horizontal
   :svg/blur        i/effects
   :lottie/shadow   i/drop-shadow
   :lottie/blur     i/effects
   :lottie/stroke   i/stroke-size
   :background-blur i/effects
   :blend-mode      i/layers
   :split-text      i/text
   :no-audio        i/volume-off})

(defn issue-texts
  "What the list says of an issue `kind` (see `dea/export-issues`), in the
  format `format-label`: `{:title :text}`."
  [kind format-label]
  (let [key (dm/str "workspace.options.export.issue."
                    (if-let [ns (namespace kind)] (dm/str ns "-" (name kind)) (name kind)))]
    {:title (tr key format-label)
     :text  (tr (dm/str key ".text") format-label)}))

(defn- popover-place
  "Where the list opens for the pill at the client rect `rect`, in a row
  of the sidebar from `row-left`: left of the sidebar (its padding is 12
  px), by the top of the pill when it is in the upper half of the window,
  else by its bottom (the export panel is low)."
  [rect row-left]
  (let [height (.-innerHeight js/window)
        right  (+ (- (.-innerWidth js/window) row-left) 20)]
    (if (< (:top rect) (/ height 2))
      {:right right :top (- (:top rect) 12)}
      {:right right :bottom (- height (:bottom rect) 12)})))

(mf/defc export-issues*
  "The row of the issues of the export in the format `format-label`, the
  issue `kinds` (see `dea/export-issues`), when there are any."
  [{:keys [kinds format-label]}]
  (let [place* (mf/use-state nil)
        place  (deref place*)
        close  (mf/use-fn #(reset! place* nil))

        on-open
        (mf/use-fn
         (fn [event]
           (let [target (dom/get-current-target event)
                 rect   (dom/get-bounding-rect target)
                 row    (dom/get-bounding-rect (dom/get-parent target))]
             (swap! place* #(when (nil? %) (popover-place rect (:left row)))))))

        on-key-down
        (mf/use-fn
         (mf/deps close)
         (fn [event]
           (when (kbd/esc? event)
             (dom/stop-propagation event)
             (close))))]

    (when (seq kinds)
      [:div {:class (stl/css :issues-row)}
       [:span {:class (stl/css :issues-label)} (tr "workspace.options.export.issues")]
       [:button {:type "button"
                 :class (stl/css :issues-pill)
                 :aria-haspopup "dialog"
                 :aria-expanded (some? place)
                 :on-click on-open}
        (tr "workspace.options.export.issues-count" (c (count kinds)))]

       (when (some? place)
         [:*
          [:div {:class (stl/css :issues-backdrop)
                 :on-pointer-down close}]
          [:div {:class (stl/css :issues-popover)
                 :role "dialog"
                 :aria-label (tr "workspace.options.export.issues")
                 :tab-index -1
                 :ref #(some-> % (dom/focus!))
                 :style (clj->js (update-vals place #(dm/str % "px")))
                 :on-key-down on-key-down}
           [:div {:class (stl/css :issues-header)}
            [:span {:class (stl/css :issues-title)} (tr "workspace.options.export.issues")]
            [:> icon-button* {:variant "ghost"
                              :icon i/close
                              :aria-label (tr "labels.close")
                              :on-click close}]]
           [:ul {:class (stl/css :issues-list)}
            (for [kind kinds
                  :let [{:keys [title text]} (issue-texts kind format-label)]]
              [:li {:key (str kind) :class (stl/css :issue)}
               [:span {:class (stl/css :issue-icon)}
                [:> i/icon* {:icon-id (get issue-icons kind i/msg-warning) :size "m"}]]
               [:div {:class (stl/css :issue-body)}
                [:p {:class (stl/css :issue-title)} title]
                [:p {:class (stl/css :issue-text)} text]]])]]])])))
