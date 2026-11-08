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

(defn viewport-content
  "The content of the viewport tag for the `screen` (the name of the
  route) on a `phone?` or another `coarse?` device; nil for none. The
  view mode is as wide as the device and may reach under its notch;
  elsewhere a tablet lays Penpot out at its own width, and a phone or a
  desktop keeps the width of a desktop page (no tag)."
  [screen phone? coarse?]
  (cond
    (= screen :viewer)  "width=device-width, initial-scale=1, viewport-fit=cover"
    phone?              nil
    coarse?             "width=device-width, initial-scale=1"
    :else               nil))

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

(defn init!
  "Notes the first finger or pen, and links the web app tags."
  []
  (when @touch-ui
    (dom/add-class! (.-documentElement ^js g/document) "touch-ui"))
  (link-web-app!)
  (.addEventListener g/global "pointerdown" on-pointer-down #js {:capture true :passive true}))
