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
   :svg/appearance  i/stroke-size
   :svg/size        i/size-horizontal
   :lottie/size     i/size-horizontal
   :lottie/shadow   i/drop-shadow
   :lottie/blur     i/effects
   :lottie/stroke   i/stroke-size
   :lottie/matte    i/mask
   :background-blur i/effects
   :blend-mode      i/layers
   :split-text      i/text
   :no-audio        i/volume-off
   :copy-clock      i/component-copy
   :gif/alpha       i/rgba
   :mp4/alpha       i/rgba})

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

(defn next-focus
  "The button of `buttons` Tab moves the focus to from the `active`
  element of a dialog: round them, back with `back?` (Shift + Tab); from
  the dialog itself, the first, or the last going back."
  [buttons active back?]
  (let [total (count buttons)
        index (.indexOf buttons active)]
    (when (pos? total)
      (nth buttons (cond
                     (neg? index) (if back? (dec total) 0)
                     back?        (mod (dec index) total)
                     :else        (mod (inc index) total))))))

(mf/defc export-issues*
  "The row of the issues of the export in the format `format-label`, the
  issue `kinds` (see `dea/export-issues`), when there are any."
  [{:keys [kinds format-label]}]
  (let [place*      (mf/use-state nil)
        place       (deref place*)
        open?       (some? place)
        pill-ref    (mf/use-ref nil)
        popover-ref (mf/use-ref nil)

        ;; the focus back on the pill, as a dialog closing gives it back
        close
        (mf/use-fn
         (fn []
           (reset! place* nil)
           (some-> (mf/ref-val pill-ref) (dom/focus!))))

        on-open
        (mf/use-fn
         (fn [event]
           (let [target (dom/get-current-target event)
                 rect   (dom/get-bounding-rect target)
                 row    (dom/get-bounding-rect (dom/get-parent target))]
             (swap! place* #(when (nil? %) (popover-place rect (:left row)))))))

        ;; Tab goes round the buttons of the list: the workspace would
        ;; take it for the next layer and keep the focus where it is
        on-key-down
        (mf/use-fn
         (mf/deps close)
         (fn [event]
           (cond
             (kbd/esc? event)
             (do (dom/stop-propagation event)
                 (close))

             (kbd/tab? event)
             (let [buttons (vec (dom/query-all (mf/ref-val popover-ref) "button"))]
               (dom/stop-propagation event)
               (dom/prevent-default event)
               (some-> (next-focus buttons (dom/get-active) (kbd/shift? event))
                       (dom/focus!))))))]

    ;; the focus in the dialog once it opens, not again as it renders
    (mf/with-effect [open?]
      (when open?
        (some-> (mf/ref-val popover-ref) (dom/focus!))))

    (when (seq kinds)
      [:div {:class (stl/css :issues-row)}
       [:span {:class (stl/css :issues-label)} (tr "workspace.options.export.issues")]
       [:button {:type "button"
                 :ref pill-ref
                 :class (stl/css :issues-pill)
                 :aria-haspopup "dialog"
                 :aria-expanded open?
                 :on-click on-open}
        (tr "workspace.options.export.issues-count" (c (count kinds)))]

       (when open?
         [:*
          [:div {:class (stl/css :issues-backdrop)
                 :on-pointer-down close}]
          [:div {:class (stl/css :issues-popover)
                 :role "dialog"
                 :aria-label (tr "workspace.options.export.issues")
                 :tab-index -1
                 :ref popover-ref
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
