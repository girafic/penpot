;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.viewer-playback-test
  (:require
   [app.main.data.viewer :as dv]
   [app.util.timers :as ts]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]))

(t/deftest the-view-mode-plays-in-real-time
  (let [frames (atom [])
        times  (atom [])]
    (with-redefs [ts/raf        (fn [f] (swap! frames conj f) (count @frames))
                  ts/cancel-af! (fn [_] nil)]
      (let [sub (rx/subs! #(swap! times conj %) (dv/frame-times 1000))]
        ;; animation frames at these times (ms), each scheduling the next
        (doseq [now [0 16 116 5116 5132]]
          ((peek @frames) now))
        (rx/dispose! sub)))
    (t/is (= [1016 1116 1616 1632] @times)
          "as far as the time that passed, a long pause (a hidden tab) 500 ms at most")))
