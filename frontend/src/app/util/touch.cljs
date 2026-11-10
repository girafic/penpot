;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.util.touch
  "Phones, tablets and pens: what kind of screen Penpot runs on, its
  viewport tag and its web app tags, and whether a finger or a pen was
  used yet (the `touch-ui` class on the document then, and `touch-ui?`).
  Never the user agent: Safari on an iPad says it is a Mac."
  (:require
   [app.util.dom :as dom]
   [app.util.globals :as g]
   [app.util.storage :as storage]
   [cuerdas.core :as str]
   [okulary.core :as l]))

(def ^:private phone-side
  "The short side of a screen, in CSS px, under which it is a phone's:
  an iPad mini has 744."
  600)

(defn phone-screen?
  "Whether a screen `width` × `height` CSS px, `coarse?` (touched), is a
  phone's."
  [width height coarse?]
  (and (boolean coarse?)
       (number? width)
       (number? height)
       (< (min width height) phone-side)))

(defn- matches?
  [query]
  (boolean (some-> (unchecked-get g/global "matchMedia") (.call g/global query) (.-matches))))

(defn coarse?
  "Whether any pointer of the device is a finger: a phone, a tablet, a
  touch screen (also with a trackpad or a mouse next to it)."
  []
  (matches? "(any-pointer: coarse)"))

(defn phone?
  "Whether Penpot runs on a phone."
  []
  (let [screen (unchecked-get g/global "screen")]
    (phone-screen? (some-> screen .-width) (some-> screen .-height) (coarse?))))

(defn- dashboard-screen?
  "Whether `screen`, the name of a route, is a page of the dashboard."
  [screen]
  (and (keyword? screen) (str/starts-with? (name screen) "dashboard")))

(defn viewport-content
  "The content of the viewport tag for the `screen` (the name of the
  route) on a `phone?` or another `coarse?` device; nil for none. The
  view mode is as wide as the device and may reach under its notch, and
  so is the dashboard on a phone, which lays itself out for it;
  elsewhere a tablet lays Penpot out at its own width, and a phone or a
  desktop keeps the width of a desktop page (no tag)."
  [screen phone? coarse?]
  (cond
    (= screen :viewer)                    "width=device-width, initial-scale=1, viewport-fit=cover"
    (and phone? (dashboard-screen? screen)) "width=device-width, initial-scale=1, viewport-fit=cover"
    phone?                                nil
    coarse?                               "width=device-width, initial-scale=1"
    :else                                 nil))

(defn- head-tag!
  "The tag in the head of the document matching `selector`, made with
  `attrs` when missing."
  [selector tag attrs]
  (or (dom/query selector)
      (let [node (dom/create-element tag)]
        (doseq [[k v] attrs]
          (dom/set-attribute! node k v))
        (dom/append-child! (dom/query "head") node)
        node)))

(defn set-viewport!
  "The viewport tag of the document with `content`; none for nil."
  [content]
  (if (some? content)
    (-> (head-tag! "meta[name=viewport]" "meta" {"name" "viewport"})
        (dom/set-attribute! "content" content))
    (some-> (dom/query "meta[name=viewport]") (dom/remove!))))

(defn link-web-app!
  "On a device that is touched, the tags that let it keep Penpot on its
  home screen as an app of its own: without a start address, it opens
  the page it was kept from (the prototype on a phone)."
  []
  (when (coarse?)
    (head-tag! "link[rel=manifest]" "link" {"rel" "manifest" "href" "app-manifest.json"})
    (head-tag! "link[rel=apple-touch-icon]" "link" {"rel" "apple-touch-icon" "href" "images/app-icon-180.png"})
    (head-tag! "meta[name=apple-mobile-web-app-capable]" "meta"
               {"name" "apple-mobile-web-app-capable" "content" "yes"})
    (head-tag! "meta[name=mobile-web-app-capable]" "meta"
               {"name" "mobile-web-app-capable" "content" "yes"})
    (head-tag! "meta[name=apple-mobile-web-app-status-bar-style]" "meta"
               {"name" "apple-mobile-web-app-status-bar-style" "content" "black-translucent"})))

;; --- Fingers and pens

(defonce touch-ui
  (l/atom (boolean (get storage/global ::touch-ui))))

(defonce pen-seen
  (l/atom (boolean (get storage/global ::pen-seen))))

(defonce fingers-pan
  (l/atom (not (false? (get storage/global ::fingers-pan)))))

(defn fingers-pan?
  "Whether a finger alone moves the canvas: once a pen was used, and
  until it is turned off (see `set-fingers-pan!`), only the pen draws
  and selects."
  []
  (and @pen-seen @fingers-pan))

(defn set-fingers-pan!
  [on?]
  (reset! fingers-pan (boolean on?))
  (swap! storage/global assoc ::fingers-pan (boolean on?)))

(defn touch-ui?
  "Whether a finger or a pen was used on this device: Penpot shows what
  helps them then (bigger targets, the touch bar)."
  []
  @touch-ui)

(defn keyboard-lift
  "How far (px) the canvas moves up for what is edited, its bottom at
  `bottom`, to show above a keyboard that leaves the page visible down
  to `visible-bottom`, with `margin` below it; 0 when it shows."
  [bottom visible-bottom margin]
  (max 0 (- (+ bottom margin) visible-bottom)))

(defn grab-scale
  "How much larger the handles of the canvas are to grab: a finger needs
  more room than a mouse."
  []
  (if @touch-ui 2.5 1))

(defn- mark-touch-ui!
  []
  (dom/add-class! (.-documentElement ^js g/document) "touch-ui")
  (when-not @touch-ui
    (reset! touch-ui true)
    (swap! storage/global assoc ::touch-ui true)))

(defn- on-pointer-down
  [^js event]
  (case (.-pointerType event)
    "touch" (mark-touch-ui!)
    "pen"   (do (mark-touch-ui!)
                (when-not @pen-seen
                  (reset! pen-seen true)
                  (swap! storage/global assoc ::pen-seen true)))
    nil))

;; --- A finger held: the context menu

(def long-press-ms
  "How long a finger stays still for the context menu."
  500)

(def ^:private press-slop-px 8)

(def ^:private lift-ms
  "How long after the finger held lifts what the browser sends for it,
  a click and its mouse, is no click."
  700)

(defonce ^:private press
  (volatile! nil))

(defonce ^:private swallow
  (volatile! {:armed false :until 0}))

(def ^:private act-ms
  "How long after a press acted the click the browser sends for it may
  come: a finger's comes once the page did what the press does, up to a
  second and a half later on a busy phone."
  2000)

(defonce ^:private acted
  (volatile! 0))

(defn press-acted!
  "A press did what it does itself (an interaction of the view mode):
  the click the browser sends as it ends is none. A finger's comes late,
  on what the press brought up, where it showed the hotspots of the
  next board as for a click on none of them (see `dv/flash-interactions`)."
  []
  (vreset! acted (+ (js/Date.now) act-ms)))

(defn- acted-click?
  "Whether `event` is the click of a press that acted (see
  `press-acted!`): the first one after it, in time."
  [^js event]
  (when (and (= "click" (.-type event)) (< (js/Date.now) @acted))
    (vreset! acted 0)
    true))

(defn menu-opened!
  "A finger held opened a context menu: as it lifts, the browser clicks
  where it was, on the menu now; that click is none."
  []
  (vswap! swallow assoc :armed true))

(defn- cancel-press!
  []
  (when-let [timer (:timer @press)]
    (js/clearTimeout timer))
  (vreset! press nil))

(defn- open-menu!
  "The context menu of what the finger holds, as a right click opens it:
  Safari opens none for a finger."
  []
  (when-let [{:keys [^js target x y]} @press]
    (vswap! press assoc :timer nil :opened true)
    (menu-opened!)
    (.dispatchEvent target (js/MouseEvent. "contextmenu"
                                           #js {:bubbles true :cancelable true
                                                :clientX x :clientY y
                                                :button 2 :buttons 2
                                                :view g/global}))))

(defn- on-press-down
  [^js event]
  (cancel-press!)
  (vreset! swallow {:armed false :until 0})
  (vreset! acted 0)
  (when (= "touch" (.-pointerType event))
    (let [target (.-target event)]
      ;; the canvas holds its fingers itself (see
      ;; `app.main.ui.workspace.viewport.touch`)
      (when-not (some-> target (.closest ".viewport-controls"))
        (vreset! press {:id (.-pointerId event)
                        :target target
                        :x (.-clientX event)
                        :y (.-clientY event)
                        :timer (js/setTimeout open-menu! long-press-ms)})))))

(defn- on-press-move
  [^js event]
  (when-let [{:keys [id x y opened]} @press]
    (when (and (= id (.-pointerId event)) (not opened)
               (> (js/Math.hypot (- (.-clientX event) x) (- (.-clientY event) y)) press-slop-px))
      (cancel-press!))))

(defn- on-press-up
  [^js event]
  (when (= "touch" (.-pointerType event))
    (cancel-press!)
    (when (:armed @swallow)
      (vreset! swallow {:armed false :until (+ (js/Date.now) lift-ms)}))))

(defn- on-lift-click
  "A click, or its mouse, the browser sends for the finger that held a
  menu open, or the click of a press that acted: swallowed."
  [^js event]
  (when (or (< (js/Date.now) (:until @swallow)) (acted-click? event))
    (.preventDefault event)
    (.stopPropagation event)))

(defn- on-press-menu
  "A menu the browser opens for the finger itself: the press opens none
  then, or, once it opened one, the browser's comes twice."
  [^js event]
  (when (.-isTrusted event)
    (cond
      (:opened @press)   (do (.preventDefault event)
                             (.stopPropagation event))
      (some? @press)     (cancel-press!))))

(defn init!
  "Notes the first finger or pen, links the web app tags, and opens the
  context menu for a finger held still."
  []
  (when @touch-ui
    (dom/add-class! (.-documentElement ^js g/document) "touch-ui"))
  (link-web-app!)
  (let [opts #js {:capture true :passive true}]
    (.addEventListener g/global "pointerdown" on-pointer-down opts)
    (.addEventListener g/global "pointerdown" on-press-down opts)
    (.addEventListener g/global "pointermove" on-press-move opts)
    (.addEventListener g/global "pointerup" on-press-up opts)
    (.addEventListener g/global "pointercancel" cancel-press! opts))
  (doseq [type ["mousedown" "mouseup" "click"]]
    (.addEventListener g/global type on-lift-click #js {:capture true}))
  (.addEventListener g/global "contextmenu" on-press-menu #js {:capture true}))
