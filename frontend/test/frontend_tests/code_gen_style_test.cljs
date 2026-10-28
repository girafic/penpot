;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.code-gen-style-test
  "Regression tests for the inspect code-generation (HTML/CSS export).

  Each test guards against a concrete bug found in the CSS/HTML generation
  of layout children and text shapes."
  (:require
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.uuid :as uuid]
   [app.util.code-gen.markup-html :as html]
   [app.util.code-gen.style-css :as css]
   [cljs.test :refer [deftest is testing] :include-macros true]
   [cuerdas.core :as str]))

;; --- Builders ------------------------------------------------------------

(defn- pts
  "Rectangular point ring matching a x/y/w/h box."
  [x y w h]
  [(gpt/point x y)
   (gpt/point (+ x w) y)
   (gpt/point (+ x w) (+ y h))
   (gpt/point x (+ y h))])

(defn- frame
  [id & {:as extra}]
  (merge {:id id :name "Board" :type :frame
          :parent-id uuid/zero :frame-id uuid/zero
          :selrect (grc/make-rect 0 0 200 200)
          :points (pts 0 0 200 200)}
         extra))

(defn- grid-frame
  "A grid board with a single auto cell that holds `child-id`."
  [id child-id & {:as extra}]
  (let [cell-id (uuid/next)]
    (merge (frame id)
           {:name "Grid"
            :layout :grid
            :layout-grid-rows [{:type :flex :value 1}]
            :layout-grid-columns [{:type :flex :value 1}]
            :layout-grid-cells {cell-id {:id cell-id :row 1 :column 1
                                         :row-span 1 :column-span 1
                                         :position :auto
                                         :align-self :auto :justify-self :auto
                                         :shapes [child-id]}}}
           extra)))

(defn- child
  [id parent-id & {:as extra}]
  (merge {:id id :name "Child" :type :rect :parent-id parent-id
          :selrect (grc/make-rect 0 0 50 50)
          :points (pts 0 0 50 50)
          :transform (gmt/matrix)}
         extra))

(defn- objects [& shapes]
  (into {} (map (juxt :id identity)) shapes))

(def ^:private sample-margin
  {:m1 10 :m2 20 :m3 30 :m4 40})

;; --- Margins on layout children -----------------------------------------

(deftest grid-child-margins-use-logical-longhand
  (testing "margins of grid children map to logical longhand properties"
    (let [pid (uuid/next)
          cid (uuid/next)
          c   (child cid pid
                     :layout-item-margin sample-margin
                     :layout-item-margin-type :multiple)
          objs (objects (grid-frame pid cid) c)]
      (is (= "10px" (css/get-css-value objs c :margin-block-start)))
      (is (= "20px" (css/get-css-value objs c :margin-inline-end)))
      (is (= "30px" (css/get-css-value objs c :margin-block-end)))
      (is (= "40px" (css/get-css-value objs c :margin-inline-start))))))

(deftest grid-child-css-has-no-redundant-margin-shorthand
  (testing "the generated rule emits logical longhand margins, not a shorthand"
    (let [pid (uuid/next)
          cid (uuid/next)
          c   (child cid pid
                     :layout-item-margin sample-margin
                     :layout-item-margin-type :multiple)
          out (css/get-shape-css-selector (objects (grid-frame pid cid) c) c)]
      (is (str/includes? out "margin-block-start: 10px;"))
      (is (not (re-find #"margin:" out))
          "must not emit the `margin` shorthand alongside the longhand props"))))

(deftest wrapped-layout-child-margin-only-on-wrapper
  (testing "a rotated (wrapped) child keeps margins on the wrapper, not the inner element"
    (let [pid (uuid/next)
          cid (uuid/next)
          c   (child cid pid
                     :layout-item-margin {:m1 10 :m2 0 :m3 0 :m4 0}
                     :layout-item-margin-type :multiple
                     :transform (gmt/rotate-matrix 30))
          out (css/get-shape-css-selector (objects (grid-frame pid cid) c) c)
          ;; the inner element is the only rule carrying the transform matrix
          inner (->> (str/split out "}")
                     (filter #(str/includes? % "transform:"))
                     (first))]
      (is (str/includes? out "-wrapper {"))
      (is (str/includes? out "margin-block-start: 10px;")
          "margin is emitted (on the wrapper)")
      (is (some? inner))
      (is (not (str/includes? inner "margin"))
          "the inner transformed element must not double the margin"))))

(deftest absolute-positioned-child-has-no-margin
  (testing "absolutely positioned layout children drop their margin"
    (let [pid (uuid/next)
          cid (uuid/next)
          c   (child cid pid
                     :layout-item-margin sample-margin
                     :layout-item-margin-type :multiple
                     :layout-item-absolute true)
          objs (objects (grid-frame pid cid) c)]
      (is (nil? (css/get-css-value objs c :margin)))
      (is (nil? (css/get-css-value objs c :margin-block-start))))))

;; --- Grid fill sizing ----------------------------------------------------

(deftest grid-fill-child-stretches-instead-of-fixed-size
  (testing "fill-sized grid children rely on stretch instead of width/height: 100%"
    (let [pid (uuid/next)
          cid (uuid/next)
          c   (child cid pid
                     :layout-item-h-sizing :fill
                     :layout-item-v-sizing :fill)
          objs (objects (grid-frame pid cid) c)]
      (is (nil? (css/get-css-value objs c :width))
          "no explicit width: 100% that would overflow the cell with a margin")
      (is (nil? (css/get-css-value objs c :height)))
      (is (= "stretch" (css/get-css-value objs c :justify-self)))
      (is (= "stretch" (css/get-css-value objs c :align-self))))))

;; --- Absolute positioning vs parent border -------------------------------

(deftest absolute-position-discounts-parent-border
  (testing "absolute coords are measured from the padding box, so the parent border is discounted"
    (let [pid    (uuid/next)
          cid    (uuid/next)
          parent (frame pid :strokes [{:stroke-width 80
                                       :stroke-style :solid
                                       :stroke-color "#000000"
                                       :stroke-alignment :inner}])
          c      (child cid pid
                        :selrect (grc/make-rect 100 100 50 50)
                        :points (pts 100 100 50 50))
          objs   (objects parent c)]
      ;; left/top = 100 (shape) - 0 (parent) - 80 (border) = 20
      (is (= "20px" (css/get-css-value objs c :left)))
      (is (= "20px" (css/get-css-value objs c :top))))))

(deftest absolute-position-without-border-is-unchanged
  (testing "without a parent border the absolute coords are the raw offset"
    (let [pid  (uuid/next)
          cid  (uuid/next)
          c    (child cid pid
                      :selrect (grc/make-rect 100 100 50 50)
                      :points (pts 100 100 50 50))
          objs (objects (frame pid) c)]
      (is (= "100px" (css/get-css-value objs c :left)))
      (is (= "100px" (css/get-css-value objs c :top))))))

;; --- Text node markup ----------------------------------------------------

(def ^:private text-content
  {:type "root"
   :children [{:type "paragraph-set"
               :children [{:type "paragraph"
                           :children [{:text "Hello"
                                       :fills [{:fill-color "#000000" :fill-opacity 1}]}]}]}]})

(deftest text-markup-emits-node-id-classes
  (testing "generated text markup carries the $id classes the CSS rules target"
    (let [tid  (uuid/next)
          text {:id tid :name "Text" :type :text
                :parent-id uuid/zero :frame-id uuid/zero
                :selrect (grc/make-rect 0 0 100 20)
                :points (pts 0 0 100 20)
                :grow-type :fixed
                :content text-content}
          markup (html/generate-markup (objects text) [text])]
      (is (string? markup))
      (is (str/includes? markup "root-0")
          "the text nodes must expose their $id as a class for the CSS to apply")
      (is (str/includes? markup "root-0-paragraph-set-0-paragraph-0")))))

;; --- 3D rotation (Penpot Motion) -------------------------------------------

(deftest a-tilted-layer-tilts-in-its-own-axes
  (testing "rotateX and rotateY after its turn in the plane, its parent the perspective"
    (let [pid  (uuid/next)
          cid  (uuid/next)
          c    (child cid pid
                      :frame-id pid
                      :selrect (grc/make-rect 50 50 100 100)
                      :points (pts 50 50 100 100)
                      :rotate-x 20
                      :rotate-y 45
                      :backface-hidden true)
          b    (frame pid :shapes [cid] :perspective 800)
          objs (objects b c)]
      (is (= "rotateX(20deg) rotateY(45deg)" (css/get-css-value objs c :transform))
          "a layer flat in the plane writes only its tilt")
      (is (= "hidden" (css/get-css-value objs c :backface-visibility)))
      (is (= "800px" (css/get-css-value objs b :perspective)))
      (is (= "100px 100px" (css/get-css-value objs b :perspective-origin))
          "the middle of the board")
      (is (nil? (css/get-css-value objs c :perspective)) "a layer holding no tilt has none")
      (is (= "50px 300px" (css/get-css-value (assoc objs pid (assoc b :perspective-x 0.25 :perspective-y 1.5))
                                             (assoc b :perspective-x 0.25 :perspective-y 1.5)
                                             :perspective-origin))
          "its vanishing point"))))

(deftest an-orthographic-board-writes-no-perspective
  (testing "its tilted layers seen without perspective, as CSS without one"
    (let [pid  (uuid/next)
          cid  (uuid/next)
          c    (child cid pid
                      :frame-id pid
                      :selrect (grc/make-rect 50 50 100 100)
                      :points (pts 50 50 100 100)
                      :rotate-y 45)
          b    (frame pid :shapes [cid] :perspective 800 :orthographic true)
          objs (objects b c)]
      (is (nil? (css/get-css-value objs b :perspective)))
      (is (nil? (css/get-css-value objs b :perspective-origin)))
      (is (= "rotateX(0deg) rotateY(45deg)" (css/get-css-value objs c :transform)) "the tilt stays"))))

(deftest the-light-and-depth-cues-of-a-board-show-in-the-css
  (testing "a layer turned to the light shows brighter, one set back fades and blurs"
    (let [pid  (uuid/next)
          cid  (uuid/next)
          did  (uuid/next)
          c    (child cid pid
                      :frame-id pid
                      :selrect (grc/make-rect 50 50 100 100)
                      :points (pts 50 50 100 100)
                      :rotate-y 60)
          d    (child did pid
                      :frame-id pid
                      :selrect (grc/make-rect 50 50 100 100)
                      :points (pts 50 50 100 100)
                      :opacity 0.8
                      :blur {:id (uuid/next) :type :layer-blur :value 3 :hidden false}
                      :translate-z -1000)
          b    (frame pid :shapes [cid did] :perspective 800
                      :light {:angle 90 :height 30 :ambient 0.2}
                      :depth-fade 0.5 :depth-blur 0.4)
          objs (objects b c d)]
      (is (= "brightness(1.667)" (css/get-css-value objs c :filter)))
      (is (= "blur(5px)" (css/get-css-value objs d :filter)) "its blur and theirs, 3 and 4")
      (is (= "0.4" (str (css/get-css-value objs d :opacity))) "its opacity half gone")
      (is (nil? (css/get-css-value (objects (dissoc b :light :depth-fade :depth-blur) c) c :filter))
          "no light, no filter"))))

(deftest a-3d-space-keeps-the-depth-of-its-layers
  (testing "a group that is a 3D space preserves 3D, its board sees it"
    (let [pid   (uuid/next)
          gid   (uuid/next)
          cid   (uuid/next)
          c     (child cid gid
                       :frame-id pid
                       :selrect (grc/make-rect 50 50 100 100)
                       :points (pts 50 50 100 100)
                       :translate-z 40
                       :rotate-y 90)
          group (merge (child gid pid :type :group :frame-id pid :shapes [cid])
                       {:preserve-depth true :rotate-x -20})
          b     (frame pid :shapes [gid] :perspective 800)
          objs  (objects b group c)]
      (is (= "translateZ(40px) rotateX(0deg) rotateY(90deg)" (css/get-css-value objs c :transform))
          "towards the viewer, then in its own axes")
      (is (= "preserve-3d" (css/get-css-value objs group :transform-style)))
      (is (= "800px" (css/get-css-value objs b :perspective)) "on the parent of the 3D space")
      (is (nil? (css/get-css-value objs group :perspective)) "not on the space itself: seen once")
      (is (nil? (css/get-css-value (assoc-in objs [gid :opacity] 0.5) (assoc group :opacity 0.5) :transform-style))
          "an opacity flattens it"))))

(deftest a-flip-card-keeps-its-faces-in-3d
  (testing "a tilted group whose faces hide their backs preserves 3D, unless it fades"
    (let [gid   (uuid/next)
          fid   (uuid/next)
          bid   (uuid/next)
          face  #(child %1 gid :backface-hidden true :rotate-y %2)
          group (merge (child gid uuid/zero :type :group :shapes [fid bid]) {:rotate-y 180})
          objs  (objects group (face fid nil) (face bid 180))]
      (is (= "preserve-3d" (css/get-css-value objs group :transform-style)))
      (is (nil? (css/get-css-value (assoc-in objs [gid :opacity] 0.5) (assoc group :opacity 0.5) :transform-style))
          "an opacity would flatten it"))))

;; --- Corners ---------------------------------------------------------------

(deftest an-ellipse-shows-round
  (let [pid     (uuid/next)
        ellipse (child (uuid/next) pid :frame-id pid :type :circle)
        rect    (child (uuid/next) pid :frame-id pid :r1 8 :r2 0 :r3 8 :r4 0)
        objs    (objects (frame pid) ellipse rect)]
    (is (= ["50%" "50%" "50%" "50%"]
           (map #(css/get-css-value objs ellipse %)
                [:border-start-start-radius :border-start-end-radius
                 :border-end-start-radius :border-end-end-radius])))
    (testing "a box keeps the corners it rounds"
      (is (= ["8px" nil "8px" nil]
             (map #(css/get-css-value objs rect %)
                  [:border-start-start-radius :border-start-end-radius
                   :border-end-start-radius :border-end-end-radius]))))))

;; --- Blur ------------------------------------------------------------------

(deftest a-hidden-blur-blurs-nothing
  (let [pid    (uuid/next)
        blur   #(hash-map :id (uuid/next) :type :layer-blur :value 3 :hidden %)
        shown  (child (uuid/next) pid :frame-id pid :blur (blur false))
        hidden (child (uuid/next) pid :frame-id pid :blur (blur true))
        objs   (objects (frame pid) shown hidden)]
    (is (= "blur(3px)" (css/get-css-value objs shown :filter)))
    (is (nil? (css/get-css-value objs hidden :filter))))
  (testing "nor beside the blur of the depth cues of its board"
    (let [pid    (uuid/next)
          in-3d  {:frame-id pid :rotate-y 30 :translate-z -500}
          plain  (apply child (uuid/next) pid (mapcat identity in-3d))
          hidden (apply child (uuid/next) pid (mapcat identity (assoc in-3d :blur {:id (uuid/next) :type :layer-blur
                                                                                   :value 30 :hidden true})))
          objs   (objects (frame pid :depth-blur 4) plain hidden)]
      (is (some? (css/get-css-value objs plain :filter)) "the depth blur")
      (is (= (css/get-css-value objs plain :filter) (css/get-css-value objs hidden :filter))))))
