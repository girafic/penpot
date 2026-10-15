;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.files.shapes-builder-test
  (:require
   [app.common.files.shapes-builder :as sb]
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
