;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.svg-vectors
  "The layers of an animated SVG export as vectors, as Figma writes an
  SVG: each layer as Penpot's SVG renderer draws it at rest, without the
  layers in it, which are layers of their own (see `cta/timeline->svg`),
  its images and the fonts of its texts in the file as data URIs."
  (:require
   ["react-dom/server" :as rds]
   [app.common.data :as d]
   [app.common.files.helpers :as cfh]
   [app.config :as cfg]
   [app.main.fonts :as fonts]
   [app.main.render :as render]
   [app.main.ui.shapes.embed :as embed]
   [app.main.ui.shapes.export :as export]
   [app.main.ui.shapes.text.fontfaces :as ff]
   [app.util.http :as http]
   [beicon.v2.core :as rx]
   [clojure.set :as set]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn without-layers
  "`objects` with the layers `layer-ids` out of the shapes they sit in, so
  a layer, or the board, draws without the layers in it, which draw on
  their own."
  [objects layer-ids]
  (d/update-vals objects
                 (fn [shape]
                   (cond-> shape
                     (seq (:shapes shape))
                     (update :shapes #(into [] (remove layer-ids) %))))))

(def ^:private shadow-properties
  #{:shadow-offset-x :shadow-offset-y :shadow-blur :shadow-spread :shadow-color :shadow-opacity})

(def ^:private trim-properties
  #{:trim-start :trim-end :trim-offset})

(defn- rest-shape
  "`shape` as its layer draws it: without its opacity, which the group of
  the layer has, and without what the animation `animated` (properties)
  plays over it, which would else show twice: a blur or drop shadows
  played by CSS on its group (see `cta/svg-appearance-css`). A board
  that hides its fill in exports, without it."
  [shape animated]
  (cond-> (dissoc shape :opacity)
    (contains? animated :blur)
    (dissoc :blur)

    (some shadow-properties animated)
    (update :shadow (fn [shadows] (filterv #(= :inner-shadow (:style %)) shadows)))

    (:hide-fill-on-export shape)
    (assoc :fills [])))

(defn with-path-length
  "The `markup` of the shape `id` with the outlines its strokes are drawn
  along of length 1 (`pathLength`), so its trim plays as their dash (see
  `cta/svg-appearance-css`): those after the start of its strokes."
  [markup id]
  (let [start (str/index-of markup (str "-" id "\" class=\"strokes\""))]
    (if (nil? start)
      markup
      (str (subs markup 0 start)
           (str/replace (subs markup start)
                        #"<(path|rect|ellipse|circle|polygon|polyline|line) "
                        "<$1 pathLength=\"1\" ")))))

(mf/defc ^:private layer-svg*
  [{:keys [objects shape]}]
  (let [wrapper (mf/with-memo [objects]
                  (render/shape-wrapper-factory objects))]
    [:& (mf/provider export/include-metadata-ctx) {:value false}
     [:& (mf/provider embed/context) {:value true}
      ;; in an `svg`, so its elements are SVG ones; only what is in it
      ;; is kept (see `layer-markup`)
      [:svg
       [:& wrapper {:shape shape}]]]]))

(defn layer-markup
  "The SVG of the shape `id` of `objects` (see `without-layers`) at rest,
  as Penpot's SVG renderer draws it, its images embedded when their data
  URIs are cached (see `load-media`), without metadata. `animated` is
  what its animation plays (see `rest-shape`)."
  [objects id animated]
  (let [markup (rds/renderToStaticMarkup
                (mf/element layer-svg* #js {:objects objects
                                            :shape (rest-shape (get objects id) animated)}))]
    (cond-> (-> markup
                (str/replace #"^<svg[^>]*>" "")
                (str/replace #"</svg>$" ""))
      (some trim-properties animated)
      (with-path-length id))))

(defn- image-urls
  "The URLs of the images the shapes of `objects` draw."
  [objects]
  (->> (vals objects)
       (mapcat (fn [shape]
                 (cond-> []
                   (= :image (:type shape)) (conj (:metadata shape))
                   (some? (:fill-image shape)) (conj (:fill-image shape)))))
       (concat (->> (vals objects)
                    (mapcat :fills)
                    (keep :fill-image)))
       (map cfg/resolve-file-media)
       (distinct)))

(defn load-media
  "A promise resolved once the images the shapes of `objects` draw are
  cached as data URIs, which the SVG renderer then embeds (see
  `layer-markup`); one that fails stays a link."
  [objects]
  (js/Promise.
   (fn [resolve _]
     (->> (rx/from (image-urls objects))
          (rx/merge-map #(rx/catch (http/fetch-data-uri %) (fn [_] (rx/empty))))
          (rx/reduce (constantly nil) nil)
          (rx/subs! resolve (fn [_] (resolve nil)))))))

(defn- embed-urls
  "`css` with each `url(…)` it loads replaced by the data URI of what it
  loads, a promise; one that fails stays a link."
  [css]
  (let [urls (distinct (fonts/extract-fontface-urls css))]
    (js/Promise.
     (fn [resolve _]
       (->> (rx/from urls)
            (rx/merge-map (fn [url]
                            (let [href (str/trim url "\"' ")]
                              (->> (http/fetch-data-uri href)
                                   (rx/map (fn [data] [url (get data href)]))
                                   (rx/catch (fn [_] (rx/empty)))))))
            (rx/reduce conj [])
            (rx/subs! (fn [pairs]
                        (resolve (reduce (fn [css [url data]]
                                           (cond-> css
                                             (some? data)
                                             (str/replace (str "url(" url ")") (str "url(\"" data "\")"))))
                                         css
                                         pairs)))
                      (fn [_] (resolve css))))))))

(defn fonts-css
  "A promise of the `@font-face` CSS of the fonts the texts among the
  shapes `ids` of `objects` (and in them) use, their files in it as data
  URIs, or nil without texts."
  [objects ids]
  (let [refs (reduce (fn [refs id]
                       (set/union refs (ff/shape->fonts (get objects id) objects)))
                     #{}
                     ids)]
    (if (empty? refs)
      (js/Promise.resolve nil)
      (js/Promise.
       (fn [resolve _]
         (->> (fonts/render-font-styles refs)
              (rx/subs! (fn [css] (-> (embed-urls css) (.then resolve)))
                        (fn [_] (resolve nil)))))))))

(defn text-ids
  "The texts among the shapes `ids` of `objects` and in them."
  [objects ids]
  (into #{}
        (comp (mapcat #(cfh/get-children-ids-with-self objects %))
              (filter #(= :text (get-in objects [% :type]))))
        ids))
