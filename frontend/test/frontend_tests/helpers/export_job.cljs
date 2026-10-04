;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.helpers.export-job
  "In-memory stand-ins of `BroadcastChannel` and `navigator.locks` for
  the tests of animated export jobs.")

(defn broadcast-channels
  "A constructor like `BroadcastChannel`: the channels it makes with the
  same name hear, a task later, what the others post (not what they post
  themselves)."
  []
  (let [registry (atom {})]
    (fn [name]
      (this-as this
               (swap! registry update name (fnil conj []) this)
               (set! (.-postMessage this)
                     (fn [data]
                       (doseq [^js other (get @registry name)
                               :when (not (identical? other this))]
                         (js/setTimeout #(when-let [f (.-onmessage other)] (f #js {:data data})) 0))))
               (set! (.-close this)
                     (fn []
                       (swap! registry update name (fn [xs] (filterv #(not (identical? % this)) xs)))))
               this))))

(defn locks
  "An object like `navigator.locks`: exclusive locks by name, granted in
  the order they are asked for, held until the promise of the callback
  settles."
  []
  (let [state (atom {})]
    (letfn [(grant! [name]
              (let [{:keys [held? queue]} (get @state name)]
                (when (and (not held?) (seq queue))
                  (swap! state assoc name {:held? true :queue (vec (rest queue))})
                  ((first queue)))))
            (release! [name]
              (swap! state assoc-in [name :held?] false)
              (grant! name))]
      #js {:request
           (fn [name callback]
             (js/Promise.
              (fn [resolve reject]
                (swap! state update-in [name :queue] (fnil conj [])
                       (fn []
                         (-> (js/Promise.resolve (callback #js {:name name}))
                             (.then (fn [value] (release! name) (resolve value))
                                    (fn [cause] (release! name) (reject cause))))))
                (grant! name))))})))
