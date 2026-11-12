;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-render-passes-test
  "The render passes of a board in the view mode: what scrolls, each
  layer that sticks at the top of the screen and the fixed layers, each
  on a canvas of its own, and which of them a render draws: as the
  containers scroll, or a frame of the animation that changes nothing
  of theirs, the still ones show what they drew last (Penpot Motion)."
  (:require
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.render-viewer-wasm :as rwv]
   [app.main.ui.viewer.viewport-common :as vpc]
   [app.main.ui.viewer.viewport-wasm :as vpw]
   [app.render-wasm.api :as wasm.api]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.mock :as mock]))

(def ^:private board-id (uuid/custom 12 1))
(def ^:private tabs (uuid/custom 12 2))
(def ^:private heading (uuid/custom 12 3))
(def ^:private label (uuid/custom 12 4))
(def ^:private row (uuid/custom 12 5))
(def ^:private chips (uuid/custom 12 6))
(def ^:private chip (uuid/custom 12 7))
(def ^:private section (uuid/custom 12 8))
(def ^:private item (uuid/custom 12 9))
(def ^:private badge (uuid/custom 12 10))

(defn- shape
  [id type parent-id x y w h & {:as attrs}]
  (-> (cts/setup-shape {:type type :x x :y y :width w :height h})
      (merge {:id id :parent-id parent-id :frame-id parent-id} attrs)))

(def ^:private objects
  (->> [(shape board-id :frame uuid/zero 0 0 402 1814 :frame-id uuid/zero
               :shapes [heading row chips tabs])
        (shape heading :frame board-id 0 100 402 40 :sticky-scroll true :shapes [label])
        (shape label :rect heading 24 110 100 20)
        (shape row :rect board-id 0 200 402 600)
        (shape chips :frame board-id 0 820 402 40 :scroll-overflow :horizontal :shapes [chip])
        (shape chip :rect chips 0 820 800 40)
        (shape tabs :rect board-id 0 1730 402 84 :fixed-scroll true)]
       (into {} (map (juxt :id identity)))))

(defn- passes
  [objects has-fixed?]
  (vpw/render-passes (vpw/layer-masks objects board-id has-fixed? (vpc/screen-sticky-ids objects board-id))
                     board-id nil))

(t/deftest a-board-renders-what-scrolls-its-sticky-and-its-fixed-layers-apart
  (let [plain (dissoc objects heading label tabs)]
    (t/is (= [{:tag "scroll" :include nil}]
             (passes (assoc-in plain [board-id :shapes] [row chips]) false))
          "without fixed or sticky layers: one pass, of all of it"))
  (let [no-tabs (-> objects
                    (dissoc tabs)
                    (update-in [board-id :shapes] #(vec (remove #{tabs} %))))
        [scroll sticky :as all] (passes no-tabs false)]
    (t/is (= ["scroll" (str "sticky:" heading)] (map :tag all)) "with a sticky one")
    (t/is (= #{board-id row chips chip} (set (:include scroll))) "what scrolls leaves it to its own")
    (t/is (= {:layer heading :include [board-id] :clear #{board-id} :still true}
             (-> sticky
                 (dissoc :tag)
                 (update :include #(vec (take 1 %)))))
          "which draws it over its board, whose fills it clears, and stays as the containers scroll")
    (t/is (= #{board-id heading label} (set (:include sticky))) "with what is in it"))
  (let [all (passes objects true)]
    (t/is (= ["scroll" (str "sticky:" heading) "fixed"] (map :tag all))
          "the fixed layers last, over the others")
    (t/is (= #{board-id tabs} (set (:include (last all)))))
    (t/is (every? :still (rest all)))))

(t/deftest a-render-draws-the-passes-a-scroll-moves
  (let [all    (passes objects true)
        sticky (str "sticky:" heading)
        tags   #(set (map :tag %))]
    (t/is (= #{"scroll"} (rwv/moving-passes objects all)) "the container is in what scrolls")
    (t/is (= #{"scroll" sticky "fixed"} (tags (rwv/due-passes all {:drawn #{sticky "fixed"} :moving #{}})))
          "a render of the animation or of a change draws all")
    (t/is (= #{"scroll"} (tags (rwv/due-passes all {:drawn #{sticky "fixed"} :moving #{} :scroll-only? true})))
          "a scroll of the containers only what scrolls")
    (t/is (= #{"scroll" "fixed"} (tags (rwv/due-passes all {:drawn #{sticky} :moving #{} :scroll-only? true})))
          "and one not drawn yet, or whose render a later one skipped")
    (t/is (= #{"scroll" sticky} (tags (rwv/due-passes all {:drawn #{sticky "fixed"} :moving #{sticky} :scroll-only? true})))
          "and one a container scrolls in")))

(def ^:private listed
  "The board with its heading in a section whose layout places it, an
  item under it and a badge placed absolutely."
  (-> objects
      (assoc-in [board-id :shapes] [section row chips tabs])
      (assoc section (shape section :frame board-id 0 100 402 400 :layout :flex :layout-flex-dir :column
                            :shapes [heading item badge]))
      (assoc-in [heading :parent-id] section)
      (assoc-in [heading :frame-id] section)
      (assoc item (shape item :rect section 0 140 402 300))
      (assoc badge (shape badge :rect section 350 110 20 20 :layout-item-absolute true))))

(t/deftest a-frame-of-the-animation-touches-the-passes-it-changes-a-layer-of
  (let [sticky  (str "sticky:" heading)
        reach   (rwv/pass-reach objects (passes objects true))
        touched #(rwv/touched-passes reach (zipmap % (repeat {})))]
    (t/is (= #{} (touched [row chip])) "not by what scrolls")
    (t/is (= #{sticky} (touched [label])) "by a layer in a sticky one")
    (t/is (= #{"fixed"} (touched [tabs])) "by a fixed one")
    (t/is (= #{sticky "fixed"} (touched [board-id])) "by the board, which all draw")
    (t/is (= #{} (rwv/touched-passes reach nil)) "nothing at rest"))
  (let [sticky  (str "sticky:" heading)
        reach   (rwv/pass-reach listed (passes listed true))
        touched #(rwv/touched-passes reach (zipmap % (repeat {})))]
    (t/is (= #{sticky} (touched [item])) "by a layer in the layout that places it, which may move it")
    (t/is (= #{sticky} (touched [section])) "by the board around it")
    (t/is (= #{} (touched [row])) "not by a layer out of that layout")))

(t/deftest a-frame-of-the-animation-leaves-the-still-passes-it-does-not-touch
  (let [all    (passes objects true)
        sticky (str "sticky:" heading)
        drawn  #{sticky "fixed"}
        tags   #(set (map :tag %))]
    (t/is (= #{"scroll"} (tags (rwv/due-passes all {:drawn drawn :posed #{} :touched #{}})))
          "it changes none of their layers: they show it as they are")
    (t/is (= #{"scroll" sticky} (tags (rwv/due-passes all {:drawn drawn :posed #{} :touched #{sticky}})))
          "the one it changes draws")
    (t/is (= #{"scroll" "fixed"} (tags (rwv/due-passes all {:drawn drawn :posed #{"fixed"} :touched #{}})))
          "and one that shows a pose of the frame before, which goes back to rest")
    (t/is (= #{"scroll" sticky} (tags (rwv/due-passes all {:drawn #{"fixed"} :posed #{} :touched #{}})))
          "and one not drawn yet, or owed")
    (t/is (= #{"scroll" sticky "fixed"} (tags (rwv/due-passes all {:drawn drawn})))
          "all without knowing what it touches")))

(t/deftest a-lost-context-shows-nothing-and-ends
  (let [present  @#'rwv/present-bitmap!
        shown    (atom nil)
        finished (atom 0)
        canvas   #js {:getContext (fn [_] #js {:transferFromImageBitmap #(reset! shown %)})
                      :setAttribute (fn [_ _])}
        lost     #js {:transferToImageBitmap #(throw (js/Error. "Cannot transfer to ImageBitmap because WebGL context is lost."))}]
    ;; no exception: the next render sets up another context
    (present canvas lost uuid/zero 10 10 #(swap! finished inc))
    (t/is (nil? @shown) "nothing shown")
    (t/is (= 1 @finished) "the render ends")
    (present canvas #js {:transferToImageBitmap (constantly :bitmap)} uuid/zero 10 10 #(swap! finished inc))
    (t/is (= :bitmap @shown) "a context that is there shows its frame")
    (t/is (= 2 @finished))))

(t/deftest another-page-loads-into-the-same-offscreen-canvas
  (let [on-snapshot @#'rwv/on-snapshot!
        snapshot    @#'rwv/viewer-snapshot
        os          #js {}
        canvas      #js {:width 300 :height 600 :dataset #js {:dpr "2"}}
        objects-a   {:page :a}
        objects-b   {:page :b}
        objects-c   {:page :c}
        contexts    (atom 0)
        loads       (atom [])
        drawn       (atom [])
        draw!       (fn [os' _] (swap! drawn conj (if (identical? os os') :drawn :another-canvas)))
        finish      #(swap! drawn conj :finished)]
    (reset! snapshot {:os-canvas os :page-key :a :objects objects-a :props []
                      :canvas-w 300 :canvas-h 600 :dpr 2})
    (with-redefs [wasm.api/initialized?        (constantly true)
                  wasm.api/init-canvas-context (mock/stub (fn [& _] (swap! contexts inc) true))
                  wasm.api/clear-canvas        (mock/stub (fn [& _] (swap! contexts inc)))
                  wasm.api/initialize-viewport (fn [objects _zoom _vbox & {:keys [on-render]}]
                                                 (swap! loads conj [objects on-render]))]
      (on-snapshot :b objects-b canvas 1 {} draw! finish)
      (t/is (= 0 @contexts) "no new WebGL context: Safari keeps each one of them")
      (t/is (= [objects-b] (map first @loads)) "the shapes of the other page load into it")
      (t/is (identical? os (:os-canvas @snapshot)))
      (t/is (= :b (:page-key @snapshot)))

      ;; the shapes of page b load late: page c took the canvas meanwhile
      (on-snapshot :c objects-c canvas 1 {} draw! finish)
      ((second (nth @loads 1)))
      ((second (nth @loads 0)))
      (t/is (= [:drawn :finished] @drawn) "page c draws, what page b loaded late draws nothing"))
    (@#'rwv/reset-viewer-snapshot!)))
