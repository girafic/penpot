;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.common.types.shape.glass
  (:require
   [app.common.schema :as sm]))

;; Glass is a backdrop effect: it refracts, disperses and frosts the
;; content behind the shape and adds a light highlight on its edges.
;;
;; - light-angle:     direction of the light, in degrees
;; - light-intensity: strength of the edge highlight, 0..100
;; - refraction:      how much the edge bends the backdrop, 0..100
;; - depth:           width of the refracting edge, in px
;; - dispersion:      chromatic split of the refraction, 0..100
;; - frost:           blur radius applied to the backdrop, in px
;; - splay:           how far the bending spreads to the center, 0..100

(def schema:glass
  [:map {:title "Glass"}
   [:id ::sm/uuid]
   [:type [:enum :glass]]
   [:light-angle ::sm/safe-number]
   [:light-intensity ::sm/safe-number]
   [:refraction ::sm/safe-number]
   [:depth ::sm/safe-number]
   [:dispersion ::sm/safe-number]
   [:frost ::sm/safe-number]
   [:splay ::sm/safe-number]
   [:hidden :boolean]])

(def default-attrs
  {:type :glass
   :light-angle -45
   :light-intensity 80
   :refraction 80
   :depth 20
   :dispersion 50
   :frost 4
   :splay 0
   :hidden false})

(def valid-glass?
  (sm/lazy-validator schema:glass))
