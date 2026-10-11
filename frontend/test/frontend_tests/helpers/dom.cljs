;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.helpers.dom
  "A DOM of jsdom for the tests that render components with React and
  work them as a user does: clicks, keys and where the focus goes."
  (:require
   ["jsdom" :refer [JSDOM]]
   ["react" :as react]
   ["react-dom/client" :as rdc]
   [app.util.globals :as globals]))

(def ^:private dom-globals
  "What the page and React reach the DOM by."
  ["window" "document" "Node" "HTMLElement" "navigator"])

(defn act
  "Call `f` and let React render what it changed before going on."
  [f]
  (react/act f))

(defn with-dom
  "Call `f` with `{:root :document :window}`, a React root to render in
  and the DOM of jsdom that holds it, the global DOM meanwhile: the root
  unmounts and the globals are as they were after, once the promise `f`
  returns, if any, has settled (it is returned then)."
  [f]
  (let [window  (.-window (JSDOM. "<!doctype html><body><div id=\"root\"></div></body>"
                                  #js {:pretendToBeVisual true}))
        saved   (mapv (fn [k] [k (js/Object.getOwnPropertyDescriptor js/globalThis k)]) dom-globals)
        act-env (unchecked-get js/globalThis "IS_REACT_ACT_ENVIRONMENT")
        ;; what `app.util.globals` took for them as it loaded
        app-doc globals/document
        app-win globals/window
        _       (set! globals/document (.-document window))
        _       (set! globals/window window)
        _       (doseq [k dom-globals]
                  (js/Object.defineProperty js/globalThis k
                                            #js {:value (if (= k "window") window (unchecked-get window k))
                                                 :configurable true
                                                 :writable true}))
        _       (unchecked-set js/globalThis "IS_REACT_ACT_ENVIRONMENT" true)
        root    (rdc/createRoot (.. window -document (getElementById "root")))
        restore (fn []
                  (act #(.unmount root))
                  (doseq [[k descriptor] saved]
                    (if (some? descriptor)
                      (js/Object.defineProperty js/globalThis k descriptor)
                      (js-delete js/globalThis k)))
                  (unchecked-set js/globalThis "IS_REACT_ACT_ENVIRONMENT" act-env)
                  (set! globals/document app-doc)
                  (set! globals/window app-win))
        result  (try
                  (f {:root root :document (.-document window) :window window})
                  (catch :default cause
                    (restore)
                    (throw cause)))]
    (if (instance? js/Promise result)
      (.finally result restore)
      (do (restore) result))))

(defn settle
  "A promise resolved once the promises of what was rendered had a while
  to resolve, and React rendered what they changed."
  []
  ;; a promise of its own: what `act` gives is a thenable that chains not
  (js/Promise.resolve (act (fn [] (js/Promise. (fn [resolve] (js/setTimeout resolve 20)))))))

(defn render!
  "Render the React element `element` in `root`."
  [root element]
  (act #(.render ^js root element)))

(defn click!
  "Click the element `node` of the DOM of `window`."
  [window node]
  (act #(.dispatchEvent node (new (.-MouseEvent window) "click" #js {:bubbles true}))))

(defn key-down!
  "Press the key `key` (as `KeyboardEvent.key`) on the element `node`."
  [window node key]
  (act #(.dispatchEvent node (new (.-KeyboardEvent window) "keydown" #js {:key key :bubbles true}))))
