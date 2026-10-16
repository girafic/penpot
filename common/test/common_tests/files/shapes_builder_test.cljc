;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.files.shapes-builder-test
  (:require
   [app.common.files.shapes-builder :as sb]
   [app.common.types.shape.text :as sht]
   [clojure.test :as t]))

;; Regression for https://github.com/penpot/penpot/issues/7869.
;; ``parse-svg-element`` used to derive the shape name from
;; ``(or (:id attrs) (tag->name tag))`` which dropped Inkscape-authored
;; labels. ``tubax/xml->clj`` (the SVG parser the rest of the import
;; pipeline already feeds these maps to) keeps namespaced attributes as
;; ``:prefix:name`` keywords — same shape the codebase already reads
;; ``:xlink:href`` from in this file (line 134) and in
;; ``app.common.svg``.

(t/deftest resolve-element-name-prefers-inkscape-label
  (t/is (= "Layer 1"
           (sb/resolve-element-name :g {:inkscape:label "Layer 1"
                                        :id "g1234"}))))

(t/deftest resolve-element-name-prefers-sodipodi-label-when-no-inkscape-label
  (t/is (= "phone-icon"
           (sb/resolve-element-name :path {:sodipodi:label "phone-icon"
                                           :id "path5678"}))))

(t/deftest resolve-element-name-falls-back-to-id-when-no-label-namespace
  (t/is (= "manual-id"
           (sb/resolve-element-name :rect {:id "manual-id"}))))

(t/deftest resolve-element-name-falls-back-to-tag-name-when-no-id-and-no-label
  ;; The tag->name mapping returns generic names for known SVG element
  ;; tags. Asserting on the call result here (rather than a hardcoded
  ;; string) keeps the test stable if the tag->name mapping is updated.
  (t/is (some? (sb/resolve-element-name :rect {})))
  (t/is (string? (sb/resolve-element-name :rect {}))))

(t/deftest resolve-element-name-inkscape-label-wins-over-sodipodi-and-id
  ;; Both label conventions and an id present together; the priority is
  ;; inkscape > sodipodi > id > tag, matching the order operators expect
  ;; (Inkscape's own UI shows ``inkscape:label`` as the canonical name).
  (t/is (= "user-name"
           (sb/resolve-element-name :g {:inkscape:label "user-name"
                                        :sodipodi:label "stale-label"
                                        :id "g1"}))))

(t/deftest resolve-element-name-empty-attrs-uses-tag-fallback
  (t/is (some? (sb/resolve-element-name :path {}))))

;; Regression for https://tree.taiga.io/project/penpot/issue/8277
;; stroke-linecap and stroke-linejoin on the SVG root must be inherited
;; by child path shapes, and stroke-cap-start/end must be stored inside
;; the stroke entry (not at the shape top level).
(t/deftest svg-root-stroke-linecap-inherited-to-path-shapes
  (let [svg-data {:name "icon"
                  :tag :svg
                  :attrs {:xmlns "http://www.w3.org/2000/svg"
                          :width "24" :height "24"
                          :viewBox "0 0 24 24"
                          :fill "none"
                          :stroke "currentColor"
                          :stroke-width "2"
                          :stroke-linecap "round"
                          :stroke-linejoin "round"}
                  :content [{:tag :line
                             :attrs {:x1 "12" :y1 "8" :x2 "12" :y2 "12"}
                             :content []}]}
        [_root children] (sb/create-svg-shapes svg-data {:x 0 :y 0} {} nil nil #{} false)
        path-shapes      (filter #(= :path (:type %)) children)]

    ;; At least one path shape was created from the <line> element
    (t/is (seq path-shapes))

    (doseq [shape path-shapes]
      (let [stroke (first (:strokes shape))]
        ;; svg-attrs must carry stroke-linecap and stroke-linejoin for the renderers
        (t/is (= "round" (get-in shape [:svg-attrs :strokeLinecap])))
        (t/is (= "round" (get-in shape [:svg-attrs :strokeLinejoin])))

        ;; stroke-cap-start/end must be inside the stroke entry, not at shape level
        (t/is (= :round (:stroke-cap-start stroke))
              "stroke-cap-start should be in the stroke entry")
        (t/is (= :round (:stroke-cap-end stroke))
              "stroke-cap-end should be in the stroke entry")
        (t/is (nil? (:stroke-cap-start shape))
              "stroke-cap-start must NOT be at the shape top level")

        ;; stroke-style is not set at import time (nil means default solid rendering)
        (t/is (nil? (:stroke-style stroke)))))))

;; --- Masks

(defn- svg-with
  [& content]
  {:name "masked" :tag :svg
   :attrs {:viewBox "0 0 100 100" :width "100" :height "100"}
   :content (vec content)})

(defn- masked-shapes
  "The masked group of the SVG `svg-data` and its children, by name."
  [svg-data]
  (let [[_ children] (sb/create-svg-shapes svg-data {:x 0 :y 0} {} nil nil #{} false)
        group (some #(when (:masked-group %) %) children)]
    [group (filterv #(= (:id group) (:parent-id %)) children)]))

(def ^:private spot
  {:tag :circle :attrs {:id "spot" :cx "50" :cy "50" :r "20" :fill "white"} :content []})

(def ^:private square
  {:tag :rect :attrs {:id "square" :x "10" :y "10" :width "80" :height "80" :fill "red"
                      :mask "url(#m)" :transform "translate(5 5)"}
   :content []})

(t/deftest a-mask-makes-a-masked-group
  (let [[group [mask content]]
        (masked-shapes (svg-with {:tag :defs :attrs {} :content [{:tag :mask :attrs {:id "m"} :content [spot]}]}
                                 square))]
    (t/is (some? group))
    (t/is (= :luma (:mask-mode group)) "an SVG mask keeps by luminance by default")
    (t/is (= "square" (:name group)) "named after the element it masks")
    (t/is (= ["spot" "square"] (mapv :name [mask content])) "its drawing at the bottom")
    (t/is (= 15.0 (+ (:x content) 0.0)) "the element where its transform puts it")
    (t/is (= 35.0 (+ (:x mask) 0.0)) "the mask in the space of the element")))

(t/deftest an-alpha-mask-keeps-by-opacity
  (let [[group] (masked-shapes (svg-with {:tag :defs :attrs {}
                                          :content [{:tag :mask :attrs {:id "m" :mask-type "alpha"} :content [spot]}]}
                                         square))]
    (t/is (true? (:masked-group group)))
    (t/is (not (contains? group :mask-mode)))))

(t/deftest the-masks-of-penpot-exports-come-back-with-their-mode
  (doseq [[flood mode] [[{:tag :feFlood :attrs {:flood-color "black"} :content []} :alpha-inverted]
                        [{:tag :feColorMatrix :attrs {:type "matrix" :values "-1 0 0 0 1  0 -1 0 0 1  0 0 -1 0 1  0 0 0 1 0"} :content []}
                         :luma-inverted]]]
    (let [filter  {:tag :filter :attrs {:id "f"}
                   :content (if (= :feFlood (:tag flood))
                              [flood {:tag :feComposite :attrs {:in "flood" :in2 "SourceGraphic" :operator "in"} :content []}]
                              [flood])}
          mask    {:tag :mask :attrs {:id "m" :mask-type "luminance"}
                   :content [{:tag :rect :attrs {:x "-100" :y "-100" :width "300" :height "300" :fill "white"} :content []}
                             {:tag :g :attrs {:filter "url(#f)"} :content [spot]}]}
          [group [mask-shape]] (masked-shapes (svg-with {:tag :defs :attrs {} :content [filter mask]} square))]
      (t/testing (name mode)
        (t/is (= mode (:mask-mode group)))
        (t/is (= "spot" (:name mask-shape)) "the drawing, not the white region"))))
  (let [filter {:tag :filter :attrs {:id "f"}
                :content [{:tag :feFlood :attrs {:flood-color "white"} :content []}
                          {:tag :feComposite :attrs {:in "FloodResult" :in2 "SourceGraphic" :operator "in"} :content []}]}
        mask   {:tag :mask :attrs {:id "m"} :content [{:tag :g :attrs {:filter "url(#f)"} :content [spot]}]}
        [group] (masked-shapes (svg-with {:tag :defs :attrs {} :content [filter mask]} square))]
    (t/is (true? (:masked-group group)))
    (t/is (not (contains? group :mask-mode)) "a drawing turned white masks by its opacity")))

;; --- Gradient and pattern fills

(defn- filled
  "The fills of the rect `rect` in an SVG with `defs`."
  [defs rect]
  (let [[_ children] (sb/create-svg-shapes (svg-with {:tag :defs :attrs {} :content defs} rect)
                                           {:x 0 :y 0} {} nil nil #{} false)]
    (:fills (some #(when (= "box" (:name %)) %) children))))

(def ^:private ramp
  {:tag :linearGradient :attrs {:id "g" :x1 "0" :y1 "0.5" :x2 "1" :y2 "0.5"}
   :content [{:tag :stop :attrs {:offset "0" :stop-color "#ff0000"} :content []}
             {:tag :stop :attrs {:offset "100%" :stop-color "#0000ff" :stop-opacity "0.5"} :content []}]})

(t/deftest a-gradient-fill-becomes-a-penpot-gradient
  (let [[fill] (filled [ramp] {:tag :rect :attrs {:id "box" :x "10" :y "10" :width "80" :height "40" :fill "url(#g)"} :content []})]
    (t/is (= {:type :linear :start-x 0.0 :start-y 0.5 :end-x 1.0 :end-y 0.5 :width 1
              :stops [{:offset 0.0 :color "#ff0000" :opacity 1.0} {:offset 1.0 :color "#0000ff" :opacity 0.5}]}
             (update (:fill-color-gradient fill) :start-x double)))
    (t/is (= 1.0 (double (:fill-opacity fill))))))

(t/deftest a-gradient-in-user-space-is-measured-on-the-shape
  (let [g      (-> ramp
                   (assoc-in [:attrs :gradientUnits] "userSpaceOnUse")
                   (assoc-in [:attrs :x1] "10") (assoc-in [:attrs :x2] "90")
                   (assoc-in [:attrs :y1] "30") (assoc-in [:attrs :y2] "30"))
        [fill] (filled [g] {:tag :rect :attrs {:id "box" :x "10" :y "10" :width "80" :height "40" :fill "url(#g)"} :content []})
        {:keys [start-x start-y end-x]} (:fill-color-gradient fill)]
    (t/is (= [0.0 0.5 1.0] (mapv double [start-x start-y end-x])))))

(t/deftest the-pattern-fills-of-penpot-exports-come-back
  (let [pattern (fn [style]
                  {:tag :pattern :attrs {:id "p" :patternUnits "userSpaceOnUse" :x "10" :y "10" :width "80" :height "40"}
                   :content [{:tag :g :attrs {}
                              :content [{:tag :rect :attrs {:width "80" :height "40" :style style} :content []}]}]})
        rect    {:tag :rect :attrs {:id "box" :x "10" :y "10" :width "80" :height "40" :fill "url(#p)"} :content []}]
    (t/is (= [{:fill-color "#336699" :fill-opacity 0.5}]
             (mapv #(update % :fill-opacity double) (filled [(pattern "fill:#336699;fill-opacity:0.5")] rect)))
          "a colour")
    (t/is (= :linear (get-in (first (filled [ramp (pattern "fill:url(#g);fill-opacity:1")] rect))
                             [:fill-color-gradient :type]))
          "a gradient")))

;; --- Hidden layers

(t/deftest a-layer-without-display-is-hidden-and-only-hidden
  ;; once hidden it keeps no `display` that would hide it again when
  ;; it is shown
  (doseq [attrs [{:display "none"} {:style "display: none"}]]
    (let [[_ children] (sb/create-svg-shapes
                        (svg-with {:tag :rect :attrs (merge {:id "box" :width "10" :height "10"} attrs)
                                   :content []})
                        {:x 0 :y 0} {} nil nil #{} false)
          box          (some #(when (= "box" (:name %)) %) children)]
      (t/is (true? (:hidden box)) (str attrs))
      (t/is (nil? (get-in box [:svg-attrs :display])) (str attrs))
      (t/is (nil? (get-in box [:svg-attrs :style :display])) (str attrs)))))

;; --- Dashes

(t/deftest dash-arrays-become-stroke-styles
  (let [stroke (fn [dasharray]
                 (let [[_ children] (sb/create-svg-shapes
                                     (svg-with {:tag :path :attrs {:id "line" :d "M0 50 L100 50" :fill "none" :stroke "#000000"
                                                                   :stroke-width "2" :stroke-dasharray dasharray}
                                                :content []})
                                     {:x 0 :y 0} {} nil nil #{} false)]
                   (-> (some #(when (= "line" (:name %)) %) children)
                       (get-in [:strokes 0])
                       (select-keys [:stroke-style :stroke-dash :stroke-gap]))))]
    (t/is (= {:stroke-style :dashed :stroke-dash 6.0 :stroke-gap 4.0} (update-vals (stroke "6 4") #(if (number? %) (double %) %))))
    (t/is (= {:stroke-style :dashed :stroke-dash 5.0 :stroke-gap 5.0} (update-vals (stroke "5") #(if (number? %) (double %) %)))
          "one length repeats")
    (t/is (= {:stroke-style :dotted} (stroke "0,7")))
    (t/is (= {:stroke-style :mixed} (stroke "7,7,3,7")))
    (t/is (not= :dashed (:stroke-style (stroke "none"))))))

;; --- Clip paths

(t/deftest a-clip-path-makes-an-alpha-masked-group
  (let [clip  {:tag :clipPath :attrs {:id "c"}
               :content [{:tag :rect :attrs {:x "20" :y "20" :width "60" :height "60" :rx "10" :fill "red" :stroke "blue"} :content []}]}
        [group [shape content]]
        (masked-shapes (svg-with {:tag :defs :attrs {} :content [clip]}
                                 (-> square (update :attrs dissoc :mask) (assoc-in [:attrs :clip-path] "url(#c)"))))]
    (t/is (true? (:masked-group group)))
    (t/is (not (contains? group :mask-mode)) "by its opacity")
    (t/is (= ["#ffffff"] (mapv :fill-color (:fills shape))) "the clip opaque, whatever its paint")
    (t/is (empty? (:strokes shape)))
    (t/is (= "square" (:name content)))))

(t/deftest a-clip-goes-around-a-mask
  (let [clip  {:tag :clipPath :attrs {:id "c"} :content [{:tag :rect :attrs {:width "50" :height "50"} :content []}]}
        mask  {:tag :mask :attrs {:id "m" :mask-type "alpha"} :content [spot]}
        [_ children] (sb/create-svg-shapes (svg-with {:tag :defs :attrs {} :content [clip mask]}
                                                     (assoc-in square [:attrs :clip-path] "url(#c)"))
                                           {:x 0 :y 0} {} nil nil #{} false)
        groups (filterv :masked-group children)
        outer  (first groups)
        inner  (second groups)]
    (t/is (= 2 (count groups)))
    (t/is (= (:id outer) (:parent-id inner)) "the clip outside")
    (t/is (= (:id inner) (:parent-id (some #(when (= :rect (:type %)) (when (= "square" (:name %)) %)) children))))))

;; --- Texts

(defn- text-of
  [& content]
  (let [[_ children] (sb/create-svg-shapes (apply svg-with content) {:x 0 :y 0} {} nil nil #{} false)]
    [(some #(when (= :text (:type %)) %) children) children]))

(defn- paragraphs
  [shape]
  (get-in shape [:content :children 0 :children]))

(t/deftest a-text-becomes-a-penpot-text
  (let [[text children]
        (text-of {:tag :text :attrs {:id "title" :x "10" :y "30" :font-family "'Work Sans', sans-serif" :font-size "20"
                                     :font-weight "bold" :fill "#ff0000"}
                  :content ["Hello " {:tag :tspan :attrs {:font-style "italic" :fill "#0000ff"} :content ["world"]}]})
        [p]   (paragraphs text)
        [a b] (:children p)]
    (t/is (some? text))
    (t/is (sht/valid-content? (:content text)))
    (t/is (= 1 (count (filter #(= :text (:type %)) children))) "its spans in it, not beside it")
    (t/is (= ["Hello " "world"] (mapv :text [a b])))
    (t/is (= ["Work Sans" "gfont-work-sans" "20" "700" "normal"] ((juxt :font-family :font-id :font-size :font-weight :font-style) a)))
    (t/is (= ["italic" "#0000ff"] [(:font-style b) (get-in b [:fills 0 :fill-color])]))
    (t/is (= "#ff0000" (get-in a [:fills 0 :fill-color])))
    (t/is (= :auto-width (:grow-type text)))
    (t/is (= [10.0 12.0] [(double (:x text)) (double (:y text))]) "its first baseline where the SVG has it")))

(t/deftest lines-become-paragraphs
  (let [[text] (text-of {:tag :text :attrs {:x "50" :y "20" :font-size "10" :text-anchor "middle"}
                         :content [{:tag :tspan :attrs {:x "50" :dy "0"} :content ["one"]}
                                   {:tag :tspan :attrs {:x "50" :dy "12"} :content ["two"]}]})]
    (t/is (= [["one"] ["two"]] (mapv #(mapv :text (:children %)) (paragraphs text))))
    (t/is (= ["center" "center"] (mapv :text-align (paragraphs text))))
    (t/is (= :fixed (:grow-type text)) "centred where the SVG has it")))

(t/deftest the-texts-of-penpot-exports-come-back-whole
  (let [line   (fn [y t] {:tag :text :attrs {:x "61" :y y :dominant-baseline "ideographic"
                                             :style "font-family:Work Sans;font-weight:600;font-size:13px;letter-spacing:3px;fill:#5b6375;fill-opacity:1"}
                          :content [t]})
        [text children]
        (text-of {:tag :g :attrs {:class "text-container" :x "60" :y "36" :width "148" :height "34"}
                  :content [{:tag :defs :attrs {} :content []}
                            {:tag :g :attrs {:class "fills"} :content [(line "52" "PENPOT") (line "69" "MOTION")]}]})]
    (t/is (= 1 (count (filter #(= :text (:type %)) children))) "one text, not one a line")
    (t/is (= [60.0 36.0 148.0 34.0] (mapv double ((juxt :x :y :width :height) text))) "in its box")
    (t/is (= [["PENPOT"] ["MOTION"]] (mapv #(mapv :text (:children %)) (paragraphs text))))
    (t/is (= ["600" "13" "3" "#5b6375"]
             ((juxt :font-weight :font-size :letter-spacing #(get-in % [:fills 0 :fill-color]))
              (first (:children (first (paragraphs text)))))))
    (t/is (sht/valid-content? (:content text)))))
