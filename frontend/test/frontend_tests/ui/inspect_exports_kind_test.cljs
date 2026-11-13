;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.inspect-exports-kind-test
  "The kind of the selection both tabs of inspect give its exports: a
  keyword, `:frame` for one board, `:multiple` for several shapes. Once
  the styles tab gave a `type` it never bound, `cljs.core/type`: several
  shapes exported the first alone, and no board got the Animated tab."
  (:require
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.uuid :as uuid]
   [app.main.ui.inspect.attributes :refer [attributes*]]
   [app.main.ui.inspect.exports :as ie]
   [app.main.ui.inspect.styles :refer [styles-tab*]]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(defn- board
  [x]
  {:id (uuid/next)
   :type :frame
   :name "Board"
   :parent-id uuid/zero
   :frame-id uuid/zero
   :selrect (grc/make-rect x 0 120 80)
   :points [(gpt/point x 0) (gpt/point (+ x 120) 0)
            (gpt/point (+ x 120) 80) (gpt/point x 80)]
   :transform (gmt/matrix)
   :fills []
   :strokes []
   :shapes []})

(defn- kinds-given
  "The kinds of the selection `shapes` that `component` gives the exports
  of inspect, rendered with them: their component stands in for itself
  and keeps what it gets."
  [component shapes]
  (let [given   (atom [])
        objects (into {} (map (juxt :id identity)) shapes)]
    (with-redefs [ie/exports (fn [^js props]
                               (swap! given conj (unchecked-get props "type"))
                               nil)]
      (thd/with-dom
        (fn [{:keys [root]}]
          (thd/render! root (mf/element component
                                        #js {:shapes shapes
                                             :objects objects
                                             :libraries {}
                                             :fileId (uuid/next)
                                             :pageId (uuid/next)
                                             :from :viewer
                                             :colorSpace "hex"})))))
    @given))

(t/deftest the-styles-tab-gives-the-kind-of-the-selection
  (let [one (kinds-given styles-tab* [(board 0)])
        two (kinds-given styles-tab* [(board 0) (board 200)])]
    (t/is (seq one) "it shows the exports")
    (t/is (every? #{:frame} one) "one board, never a function")
    (t/is (every? #{:multiple} two) "several shapes")))

(t/deftest the-attributes-tab-gives-the-kind-of-the-selection
  (let [one (kinds-given attributes* [(board 0)])
        two (kinds-given attributes* [(board 0) (board 200)])]
    (t/is (seq one) "it shows the exports")
    (t/is (every? #{:frame} one))
    (t/is (every? #{:multiple} two))))
