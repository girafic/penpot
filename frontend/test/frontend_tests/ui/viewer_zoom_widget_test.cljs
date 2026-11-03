;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.ui.viewer-zoom-widget-test
  "The zoom of the header of the view mode under a device (Penpot
  Motion): a device shows the board as wide as its screen, so the zoom
  is locked and says so."
  (:require
   ["react-dom/server" :as rds]
   [app.main.ui.viewer.header :as header]
   [cljs.test :as t :include-macros true]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn- render
  [framed]
  (rds/renderToStaticMarkup
   (mf/element header/zoom-widget #js {:zoom 1 :framed framed})))

(t/deftest the-zoom-under-a-device-is-disabled
  (t/is (str/includes? (render true) "aria-disabled=\"true\"")))

(t/deftest the-zoom-without-a-device-is-not
  (let [markup (render false)]
    (t/is (str/includes? markup "aria-disabled=\"false\""))
    (t/is (not (str/includes? markup "aria-disabled=\"true\"")))))
