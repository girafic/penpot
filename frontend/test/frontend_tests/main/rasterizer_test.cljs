;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.main.rasterizer-test
  (:require
   [app.main.rasterizer :as thr]
   [cljs.test :as t :include-macros true]))

(t/deftest rasterizer-starts-on-the-first-render
  (let [inits   (atom 0)
        started thr/started?
        ready   thr/ready?]
    (set! thr/started? false)
    (set! thr/ready? false)
    (try
      (with-redefs [thr/init! (fn [] (swap! inits inc))]
        (t/is (= 0 @inits) "nothing creates the iframe before a render")
        (thr/render {:data "<svg/>" :width 10})
        (thr/render {:data "<svg/>" :width 20}))
      (t/is (= 1 @inits) "one iframe for all renders")
      (t/is (= 2 (.-length thr/queue)) "both renders wait for the iframe")
      (finally
        (.splice thr/queue 0)
        (set! thr/started? started)
        (set! thr/ready? ready)))))
