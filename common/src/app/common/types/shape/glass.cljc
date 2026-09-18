;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.types.shape.glass
  (:require
   [app.common.data :as d]
   [app.common.schema :as sm]
   [app.common.types.color :as ctc]))

;; Glass is a backdrop effect: it refracts, disperses and frosts the
;; content behind the shape and adds a light highlight on its edges.
;;
;; - light-angle:     direction of the light, in degrees
;; - light-intensity: strength of the edge highlight, 0..100
;; - light-color:     color of the edge highlight (white when absent)
;; - highlight-width: width of the edge highlight, in px
;; - refraction:      how much the edge bends the backdrop, 0..100
;; - depth:           width of the refracting edge, in px
;; - dispersion:      chromatic split of the refraction, 0..100
;; - frost:           blur applied to the backdrop, in px
;; - splay:           how far the bending spreads to the center, 0..100
;; - saturation:      backdrop saturation, 0..200 (100 = unchanged)
;; - brightness:      backdrop brightness, 0..200 (100 = unchanged)
;; - texture:         surface texture, see `textures`
;; - texture-amount:  strength of the texture, 0..100
;; - texture-scale:   width of one strip, or size of one dent, in px
;; - texture-angle:   direction of the texture, in degrees (0 = vertical)
;;
;; The keys added after the first version are optional. A missing key
;; means its value in `optional-defaults`, so older files render as before.

;; :reeded lens strips, :wavy seamless waves, :prismatic flat facets,
;; :cross-reeded lens strips on both axes, :hammered round dents
(def textures #{:none :reeded :wavy :prismatic :cross-reeded :hammered})

(def schema:light-color
  [:merge {:title "GlassLightColor"}
   ctc/schema:color-attrs
   ctc/schema:plain-color])

(def light-color-attrs
  (sm/keys schema:light-color))

(def default-light-color
  {:color ctc/white :opacity 1})

(def schema:glass
  [:map {:title "Glass"}
   [:id ::sm/uuid]
   [:type [:enum :glass]]
   [:light-angle ::sm/safe-number]
   [:light-intensity ::sm/safe-number]
   [:light-color {:optional true} schema:light-color]
   [:highlight-width {:optional true} ::sm/safe-number]
   [:refraction ::sm/safe-number]
   [:depth ::sm/safe-number]
   [:dispersion ::sm/safe-number]
   [:frost ::sm/safe-number]
   [:splay ::sm/safe-number]
   [:saturation {:optional true} ::sm/safe-number]
   [:brightness {:optional true} ::sm/safe-number]
   [:texture {:optional true} [::sm/one-of textures]]
   [:texture-amount {:optional true} ::sm/safe-number]
   [:texture-scale {:optional true} ::sm/safe-number]
   [:texture-angle {:optional true} ::sm/safe-number]
   [:hidden :boolean]])

(def optional-defaults
  {:highlight-width 2
   :saturation 100
   :brightness 100
   :texture :none
   :texture-amount 30
   :texture-scale 8
   :texture-angle 0})

(def default-attrs
  (merge
   {:type :glass
    :light-angle -45
    :light-intensity 80
    :refraction 80
    :depth 20
    :dispersion 50
    :frost 4
    :splay 0
    :hidden false}
   optional-defaults))

(def valid-glass?
  (sm/lazy-validator schema:glass))

(defn get-value
  "Value of `attr` in `glass`, falling back to the optional default."
  [glass attr]
  (d/nilv (get glass attr) (get optional-defaults attr)))

(defn color->light-color
  "Light color from a color picker value, keeping only the allowed keys.
  A gradient uses its first stop; an image gives nil."
  [{:keys [gradient] :as color}]
  (let [color (if-let [stop (get-in gradient [:stops 0])]
                (merge (select-keys color [:ref-id :ref-file])
                       (select-keys stop [:color :opacity]))
                color)]
    (when (string? (:color color))
      (-> (select-keys color light-color-attrs)
          (d/without-nils)))))
