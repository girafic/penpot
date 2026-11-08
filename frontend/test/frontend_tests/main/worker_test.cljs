;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.main.worker-test
  (:require
   [app.main.worker :as mw]
   [app.util.worker :as uw]
   [cljs.test :as t :include-macros true]))

(defn- with-worker-api
  "Runs `f` with `mw/instance` cleared and the global `Worker`
  constructor present (`present?`) or absent, restoring both after."
  [present? f]
  (let [instance mw/instance
        ctor     (unchecked-get js/globalThis "Worker")]
    (set! mw/instance nil)
    (if present?
      (unchecked-set js/globalThis "Worker" (or ctor (fn [])))
      (js-delete js/globalThis "Worker"))
    (try
      (f)
      (finally
        (set! mw/instance instance)
        (if (some? ctor)
          (unchecked-set js/globalThis "Worker" ctor)
          (js-delete js/globalThis "Worker"))))))

(t/deftest worker-starts-on-its-first-use
  (let [inits (atom 0)
        sent  (atom [])
        ;; multi-arity like `uw/ask!`/`uw/emit!`: callers invoke an arity directly
        send  (fn
                ([worker message] (swap! sent conj [worker (:cmd message)]) nil)
                ([worker message _transfer] (swap! sent conj [worker (:cmd message)]) nil))]
    (with-worker-api true
      (fn []
        (with-redefs [uw/init (fn [_ _] (swap! inits inc) :worker)
                      uw/ask! send
                      uw/emit! send]
          (t/is (= 0 @inits) "nothing starts the worker before it is asked")
          (mw/ask! {:cmd :first})
          (mw/emit! {:cmd :second})
          (mw/ask! {:cmd :third}))))
    (t/is (= 1 @inits) "one worker for all messages")
    (t/is (= [[:worker :configure] [:worker :first] [:worker :second] [:worker :third]] @sent)
          "configured before the first message")))

(t/deftest no-worker-where-the-api-is-missing
  (let [inits (atom 0)]
    (with-worker-api false
      (fn []
        (with-redefs [uw/init (fn [_ _] (swap! inits inc) :worker)]
          (mw/ask! {:cmd :first})
          (mw/ask-many! {:cmd :second}))))
    (t/is (= 0 @inits))))
