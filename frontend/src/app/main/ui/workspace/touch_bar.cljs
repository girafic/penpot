;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.touch-bar
  "On a device used with a finger or a pen (see `touch/touch-ui?`), the
  keys it has no keyboard for, over the canvas: Shift, Alt and the
  command key held until let go (see `kbd/latch!`), undo and redo, the
  panels shown or hidden, and, once a pen was used, whether a finger
  alone moves the canvas (see `touch/fingers-pan?`). And the text edited
  shown above the keyboard of the screen as it opens."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.config :as cf]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.util.globals :as g]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.touch :as touch]
   [rumext.v2 :as mf]))

(defn modifier-keys
  "The keys the bar holds, on a Mac (and an iPad) or not: what it
  latches, the `:key` of its events, its `:label` and its `:name`."
  [mac?]
  [{:latch :shift :key "Shift" :label "⇧" :name "Shift"}
   (if mac?
     {:latch :alt :key "Alt" :label "⌥" :name "Option"}
     {:latch :alt :key "Alt" :label "Alt" :name "Alt"})
   (if mac?
     {:latch :meta :key "Meta" :label "⌘" :name "Command"}
     {:latch :ctrl :key "Control" :label "Ctrl" :name "Ctrl"})])

(defn- hold!
  "The modifier `latch` (its `key`) held, or let go: for what reads the
  events and for what follows the keys (`app.main.streams`)."
  [latch key on?]
  (kbd/latch! latch on?)
  (st/emit! (kbd/->KeyboardEvent (if on? :down :up) key
                                 (kbd/latched? :shift) (kbd/latched? :ctrl)
                                 (kbd/latched? :alt) (kbd/latched? :meta)
                                 (kbd/latched? (if (cf/check-platform? :macos) :meta :ctrl))
                                 false nil)))

(defn- edited-bottom
  "Where what is edited ends on screen: the caret, else the editor."
  [^js active]
  (let [selection (.getSelection g/global)
        caret     (when (and (some? selection) (pos? (.-rangeCount selection)))
                    (.getBoundingClientRect (.getRangeAt selection 0)))]
    (if (and (some? caret) (pos? (.-height caret)))
      (.-bottom caret)
      (.-bottom (.getBoundingClientRect active)))))

(defn- keep-edited-visible!
  "The keyboard of the screen opened or closed (`viewport` is the part of
  the page it leaves visible): the page stays where it is, and the text
  edited moves above the keyboard."
  [^js viewport]
  (when (pos? (.-scrollY g/global))
    (.scrollTo g/global 0 0))
  (let [active (.-activeElement g/document)]
    (when (and (some? active) (true? (.-isContentEditable active)))
      (let [lift (touch/keyboard-lift (edited-bottom active)
                                      (+ (.-offsetTop viewport) (.-height viewport))
                                      24)]
        (when (pos? lift)
          (let [zoom (dm/get-in @st/state [:workspace-local :zoom] 1)]
            (st/emit! (dw/update-viewport-position {:y #(+ % (/ lift zoom))}))))))))

(mf/defc touch-bar*
  "The bar, above the palette of colors or texts when it shows,
  `palette-size` high."
  [{:keys [palette-size]}]
  (let [touch-ui?  (mf/deref touch/touch-ui)
        pen-seen?  (mf/deref touch/pen-seen)
        pan?       (mf/deref touch/fingers-pan)
        held*      (mf/use-state #{})
        held       (deref held*)
        mac?       (cf/check-platform? :macos)

        on-key
        (mf/use-fn
         (fn [event]
           (let [node  (.-currentTarget ^js event)
                 latch (keyword (.. node -dataset -latch))
                 key   (.. node -dataset -key)
                 on?   (not (kbd/latched? latch))]
             (hold! latch key on?)
             (swap! held* (if on? conj disj) latch))))]

    ;; the keyboard of the screen does not hide the text edited
    (mf/with-effect []
      (when-let [viewport (unchecked-get g/global "visualViewport")]
        (let [on-resize #(keep-edited-visible! viewport)]
          (.addEventListener viewport "resize" on-resize)
          #(.removeEventListener viewport "resize" on-resize))))

    ;; nothing stays held once the bar goes
    (mf/with-effect []
      (fn []
        (doseq [{:keys [latch key]} (modifier-keys (cf/check-platform? :macos))]
          (when (kbd/latched? latch)
            (hold! latch key false)))))

    (when touch-ui?
      [:div {:class (stl/css :touch-anchor)
             :style {:padding-block-end (when (number? palette-size) (dm/str (+ palette-size 12) "px"))}}
       [:div {:class (stl/css :touch-bar)
              :role "toolbar"
              :aria-label (tr "workspace.touch.bar")}
        (for [{:keys [latch key label] :as modifier} (modifier-keys mac?)]
          [:button {:key (name latch)
                    :class (stl/css-case :touch-key true
                                         :held (contains? held latch))
                    :data-latch (name latch)
                    :data-key key
                    :aria-pressed (contains? held latch)
                    :aria-label (tr "workspace.touch.hold" (:name modifier))
                    :on-click on-key}
           label])
        [:span {:class (stl/css :touch-separator)}]
        [:button {:class (stl/css :touch-key)
                  :aria-label (tr "workspace.touch.undo")
                  :on-click #(st/emit! dwu/undo)}
         "↶"]
        [:button {:class (stl/css :touch-key)
                  :aria-label (tr "workspace.touch.redo")
                  :on-click #(st/emit! dwu/redo)}
         "↷"]
        [:span {:class (stl/css :touch-separator)}]
        [:button {:class (stl/css :touch-key)
                  :aria-label (tr "workspace.touch.panels")
                  :on-click #(st/emit! (dw/toggle-layout-flag :hide-ui))}
         [:> icon* {:icon-id i/layers :size "m"}]]
        (when pen-seen?
          [:button {:class (stl/css-case :touch-key true
                                         :held pan?)
                    :aria-pressed pan?
                    :aria-label (tr "workspace.touch.pen-only")
                    :on-click #(touch/set-fingers-pan! (not pan?))}
           [:> icon* {:icon-id i/pentool :size "m"}]])]])))
