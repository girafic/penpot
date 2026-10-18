;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.viewport.outline
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.tilt :as gtl]
   [app.common.types.component :as ctk]
   [app.common.types.container :as ctn]
   [app.main.refs :as refs]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.shapes.attrs :as attrs]
   [app.util.object :as obj]
   [clojure.set :as set]
   [rumext.v2 :as mf]))

(defn- polylines->d
  [lines]
  (apply str
         (for [{:keys [points closed?]} lines
               [i {:keys [x y]}] (map-indexed vector points)]
           (str (if (zero? i) "M" "L") x " " y
                (when (and closed? (= i (dec (count points)))) "Z")))))

(mf/defc outline*
  "The outline of `shape`; a tilted one (or one in a tilted group or
  board, see `app.common.geom.tilt`) as it shows, from `objects` as they
  show."
  [{:keys [shape modifier zoom objects]}]
  (let [zoom      (d/nilv zoom 1)
        shape     (gsh/transform-shape shape (:modifiers modifier))
        transform (gsh/transform-str shape)
        tilted    (mf/with-memo [objects shape]
                    (when (some? objects)
                      (gtl/projected-outline objects shape)))

        ;; NOTE: that we don't use mf/deref to avoid a repaint dependency here
        objects   (deref refs/workspace-page-objects)
        color     (if (or
                       (ctn/in-any-component? objects shape)
                       (ctk/is-variant-container? shape))
                    "var(--assets-component-hightlight)"
                    "var(--color-accent-tertiary)")

        x         (dm/get-prop shape :x)
        y         (dm/get-prop shape :y)
        width     (dm/get-prop shape :width)
        height    (dm/get-prop shape :height)
        selrect   (dm/get-prop shape :selrect)
        type      (dm/get-prop shape :type)
        content   (get shape :content)
        path?     (cfh/path-shape? shape)

        path-data
        (mf/with-memo [path? content]
          (when (and ^boolean path? (some? content))
            (.toString content)))

        border-attrs
        (attrs/get-border-props shape)

        outline-type
        (case type
          :circle "ellipse"
          :path "path"
          (if (some? (obj/get border-attrs "d"))
            "path"
            "rect"))

        props
        (obj/merge!
         #js {:fill "none"
              :stroke color
              :strokeWidth (/ 1 zoom)
              :pointerEvents "none"
              :transform transform}

         (case type
           :circle
           #js {:cx (+ x (/ width 2))
                :cy (+ y (/ height 2))
                :rx (/ width 2)
                :ry (/ height 2)}

           :path
           #js {:d path-data
                :transform nil}

           (let [x (dm/get-prop selrect :x)
                 y (dm/get-prop selrect :y)
                 w (dm/get-prop selrect :width)
                 h (dm/get-prop selrect :height)]
             #js {:x x
                  :y y
                  :width w
                  :height h
                  :rx (obj/get border-attrs "rx")
                  :ry (obj/get border-attrs "ry")
                  :d  (obj/get border-attrs "d")})))]

    (if (some? tilted)
      [:path {:d (polylines->d tilted)
              :fill "none"
              :stroke color
              :stroke-width (/ 1 zoom)
              :pointer-events "none"}]
      [:> outline-type props])))

(mf/defc shape-outlines-render*
  {::mf/wrap [#(mf/memo' % (mf/check-props ["shapes" "zoom" "modifiers" "objects"]))]}
  [{:keys [shapes zoom modifiers objects]}]
  (for [shape shapes]
    (let [shape-id (dm/get-prop shape :id)
          modifier (get modifiers shape-id)]
      [:> outline* {:key (dm/str "outline-" shape-id)
                    :shape shape
                    :modifier modifier
                    :zoom zoom
                    :objects objects}])))

(mf/defc shape-outlines*
  [{:keys [selected hover highlighted objects edition zoom modifiers]}]
  (let [selected    (or selected #{})
        hover       (or hover #{})
        highlighted (or highlighted #{})

        lookup      (d/getf objects)
        edition?    (fn [o] (= edition o))

        shapes      (-> #{}
                        (into (comp (remove edition?)
                                    (keep lookup))
                              (set/union selected hover))
                        (into (comp (remove edition?)
                                    (keep lookup))
                              highlighted))

        modifiers (select-keys modifiers (map :id shapes))
        modifiers (hooks/use-equal-memo modifiers)
        shapes    (hooks/use-equal-memo shapes)]

    [:g.outlines.blurrable
     [:> shape-outlines-render* {:shapes shapes
                                 :zoom zoom
                                 :modifiers modifiers
                                 :objects objects}]]))
