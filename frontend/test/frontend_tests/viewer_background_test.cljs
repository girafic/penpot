;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.viewer-background-test
  "The background of the prototype of a page, round its boards shown
  without a device in the view mode (Penpot Motion)."
  (:require
   [app.main.data.viewer.devices :as dvd]
   [app.main.ui.viewer :as viewer]
   [cljs.test :as t :include-macros true]))

(t/deftest the-background-of-the-prototype-shows-without-a-device
  (let [page {:prototype-background "#ffffff"}]
    (t/is (= "#ffffff" (viewer/background page nil)))
    (t/is (nil? (viewer/background page (dvd/resolve-device "iphone-island" 402 874)))
          "round a device, the view mode keeps its own")
    (t/is (nil? (viewer/background {} nil)) "unset, its own")))
