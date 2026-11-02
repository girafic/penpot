;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.device-frame
  "A device round the screen of the view mode (see
  `app.main.data.viewer.devices`). The view mode places three layers in
  one stage: the drawing of the device below, its section of the board
  as the screen (`screen-style`), and the cutouts over it, which let the
  pointer through. The section stays where it is in the view mode, only
  its box changes: what listens to it keeps listening."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.main.data.viewer.devices :as dvd]
   [app.util.dom :as dom]
   [rumext.v2 :as mf]))

(defn- role-class
  [role]
  (case role
    :band      (stl/css :band)
    :glass     (stl/css :glass)
    :face      (stl/css :face)
    :button    (stl/css :button)
    :base      (stl/css :base)
    :base-dark (stl/css :base-dark)
    :chin      (stl/css :chin)
    :front     (stl/css :front)
    :stand     (stl/css :stand)
    :ring      (stl/css :ring)
    :lens      (stl/css :lens)
    :cutout    (stl/css :cutout)
    :toolbar   (stl/css :toolbar)
    :rule      (stl/css :rule)
    :close     (stl/css :close)
    :minimize  (stl/css :minimize)
    :zoom      (stl/css :zoom)
    :field     (stl/css :field)
    :title     (stl/css :title)))

(defn- fit-text
  "`text` cut with an ellipsis to about `max-w` px at `size` px."
  [text size max-w]
  (let [n (max 1 (mth/floor (/ max-w (* 0.55 size))))]
    (if (> (count text) n)
      (dm/str (subs text 0 (dec n)) "…")
      text)))

(defn- rounded-path
  "The outline of a box `x` `y` `w` × `h` with its corners round by
  `[top-left top-right bottom-right bottom-left]`, as an SVG path."
  [x y w h [tl tr br bl]]
  (let [right (+ x w) bottom (+ y h)]
    (dm/str "M" (+ x tl) "," y
            "H" (- right tr) "A" tr "," tr " 0 0 1 " right "," (+ y tr)
            "V" (- bottom br) "A" br "," br " 0 0 1 " (- right br) "," bottom
            "H" (+ x bl) "A" bl "," bl " 0 0 1 " x "," (- bottom bl)
            "V" (+ y tl) "A" tl "," tl " 0 0 1 " (+ x tl) "," y "Z")))

(defn- shape-element
  [label index {:keys [type role] :as shape}]
  (case type
    :circle
    (mf/html [:circle {:key index :class (role-class role)
                       :cx (:cx shape) :cy (:cy shape) :r (:r shape)}])
    :label
    (mf/html [:text {:key index :class (role-class role)
                     :x (:x shape) :y (:y shape) :font-size (:size shape)
                     :text-anchor "middle" :dominant-baseline "central"}
              (fit-text (or label "") (:size shape) (:max-w shape))])
    ;; a rect with corners each its own way, as a path
    (if (vector? (:r shape))
      (mf/html [:path {:key index :class (role-class role)
                       :d (rounded-path (:x shape) (:y shape) (:w shape) (:h shape) (:r shape))}])
      (mf/html [:rect {:key index :class (role-class role)
                       :x (:x shape) :y (:y shape) :width (:w shape) :height (:h shape)
                       :rx (:r shape)}]))))

(mf/defc device-drawing*
  "The device `kind` of `layout` (see `dvd/layout`) in its `finish`: its
  body below the screen, or its `cutouts` over it, in the box of the
  device; turned a quarter to the left when it lies on its side. A
  browser shows `label` in its address field."
  [{:keys [layout kind finish cutouts label]}]
  (let [{:keys [x y w h view-w view-h landscape? geometry]} layout]
    [:svg {:class (stl/css :drawing)
           :aria-hidden true
           :data-finish (dvd/parse-finish finish)
           :data-kind (some-> kind name)
           :view-box (dm/str "0 0 " view-w " " view-h)
           :style {:left x :top y :width w :height h}}
     [:g {:transform (when landscape?
                       (dm/str "translate(0 " (:w geometry) ") rotate(-90)"))}
      (map-indexed (partial shape-element label)
                   (if (true? cutouts) (:cutouts geometry) (:parts geometry)))]]))

(defn screen-style
  "The box of the section of the view mode as the screen of the device
  of `layout`, in the stage it shares with the drawing: a style object,
  as React takes it."
  [layout]
  (let [{:keys [x y w h radii]} (:screen layout)
        [tl tr br bl] radii]
    #js {:marginLeft (dm/str x "px")
         :marginTop (dm/str y "px")
         :width (dm/str w "px")
         :height (dm/str h "px")
         :borderRadius (dm/str tl "px " tr "px " br "px " bl "px")}))

(def ^:private turn-ms
  "How long a device takes to lie on its side or stand up, ms."
  450)

(def ^:private turn-wait-ms
  "How long, at most, a device that turns waits for its screen to be
  drawn the new way, ms: it stands as it stood meanwhile."
  400)

(def ^:private fade-ms
  "How long a device that turned in the preview of the workspace takes to
  show again, ms (see `use-turn!`)."
  200)

(def ^:private settle-ms
  "How long, at least, a device that turned in the preview of the
  workspace stays hidden, ms: its window turns with it meanwhile."
  100)

(defn- turn-steps
  "How a device of `layout` (see `dvd/layout`) that turned shows on its
  way from how it stood, `[[angle scale] ...]` by tenths: turned back a
  quarter (`angle` degrees) and scaled by `scale` to the size it had,
  then less and less, never larger than its stage at any angle (a box
  `w` × `h` turned by a is w·cos a + h·sin a wide)."
  [{:keys [x y w h]} angle scale]
  (let [stage-w (+ w x x)
        stage-h (+ h y y)]
    (mapv (fn [step]
            (let [t (/ step 10)
                  a (* angle (- 1 t))
                  r (/ (* (mth/abs a) js/Math.PI) 180)
                  c (js/Math.cos r)
                  s (js/Math.sin r)]
              [a (min (+ scale (* (- 1 scale) t))
                      (/ stage-w (+ (* w c) (* h s)))
                      (/ stage-h (+ (* w s) (* h c))))]))
          (range 11))))

(defn- turn-node!
  "The element `node` of the stage of a device that turned, from how the
  device stood, by `steps` (see `turn-steps`) about the middle of the
  device, at `ox` `oy` px from its top left. A paused animation."
  [^js node steps ox oy]
  (let [origin (dm/str ox "px " oy "px")
        ^js animation
        (.animate node
                  (into-array
                   (map (fn [[angle scale]]
                          #js {:transform (dm/str "rotate(" angle "deg) scale(" scale ")")
                               :transformOrigin origin})
                        steps))
                  #js {:duration turn-ms :easing "cubic-bezier(0.4, 0, 0.2, 1)"})]
    (.pause animation)
    animation))

(defn- fade-node!
  "The element `node` of the stage of a device that turned, hidden, then
  showing again. A paused animation."
  [^js node]
  (let [^js animation (.animate node
                                #js [#js {:opacity 0} #js {:opacity 1}]
                                #js {:duration fade-ms :easing "ease-out"})]
    (.pause animation)
    animation))

(defn use-turn!
  "The device `kind` of `layout` (see `dvd/layout`) turning in its stage
  as it comes to lie on its side or stand up: its drawing (in
  `stage-ref`), its screen (`screen-ref`) and its cutouts (`cutouts-ref`)
  from how it stood, once `ready` (a fn that gives a promise) says the
  screen shows the board the new way, or a while. The screen is
  `data-turning` meanwhile: what it holds fixed in the window (the bar
  at the bottom, the arrows) would turn with it. With `fade?` (in the
  preview of the workspace, its window turning round it), it hides and
  shows again once its window turned, as it then is."
  ([stage-ref screen-ref cutouts-ref layout kind ready]
   (use-turn! stage-ref screen-ref cutouts-ref layout kind ready false))
  ([stage-ref screen-ref cutouts-ref layout kind ready fade?]
   (let [before-ref     (mf/use-ref nil)
         animations-ref (mf/use-ref nil)]
     (mf/with-layout-effect [layout kind]
       (let [before (mf/ref-val before-ref)]
         (mf/set-ref-val! before-ref (when (some? layout) {:layout layout :kind kind}))
         (when (and (some? before)
                    (some? layout)
                    (= kind (:kind before))
                    (not= (:landscape? layout) (get-in before [:layout :landscape?])))
           (let [{:keys [x y w h screen landscape?]} layout
                 cx     (+ x (/ w 2))
                 cy     (+ y (/ h 2))
                 ;; on its side, it turned a quarter to the left
                 steps  (turn-steps layout (if landscape? 90 -90) (/ (get-in before [:layout :w]) h))
                 screen-node (mf/ref-val screen-ref)
                 animations (into []
                                  (keep (fn [[^js node ox oy]]
                                          (when (fn? (some-> node .-animate))
                                            (if fade?
                                              (fade-node! node)
                                              (turn-node! node steps ox oy)))))
                                  [[(mf/ref-val stage-ref) cx cy]
                                   [(mf/ref-val cutouts-ref) cx cy]
                                   [screen-node (- cx (:x screen)) (- cy (:y screen))]])
                 wait   (js/Promise. #(js/setTimeout % turn-wait-ms))
                 shown  (js/Promise.race #js [(ready) wait])
                 shown  (if fade?
                          (js/Promise.all #js [shown (js/Promise. #(js/setTimeout % settle-ms))])
                          shown)]
             (run! #(.cancel ^js %) (mf/ref-val animations-ref))
             (mf/set-ref-val! animations-ref animations)
             (dom/set-data! screen-node "turning" true)
             (-> shown
                 (.then (fn [_]
                          ;; unless it turned again meanwhile
                          (when (identical? animations (mf/ref-val animations-ref))
                            (run! #(.play ^js %) animations)
                            (-> (js/Promise.all (into-array (map #(.-finished ^js %) animations)))
                                (.then #(dom/remove-attribute! screen-node "data-turning"))
                                (.catch (constantly nil))))))))))))))

(defn use-size
  "The size `[w h]` of the element of `ref` as it renders, nil before;
  `[w h]` again only when it changes. It measures again as `present`
  changes: whether the element is rendered."
  [ref present]
  (let [size* (mf/use-state nil)]
    (mf/with-effect [ref present]
      (when-let [node (mf/ref-val ref)]
        (let [measure! (fn []
                         (let [size [(.-clientWidth node) (.-clientHeight node)]]
                           ;; the same size renders nothing again
                           (swap! size* #(if (= % size) % size))))
              observer (js/ResizeObserver. measure!)]
          (measure!)
          (.observe observer node)
          #(.disconnect observer))))
    (deref size*)))
