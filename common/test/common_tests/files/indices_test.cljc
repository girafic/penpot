;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.files.indices-test
  (:require
   [app.common.files.indices :as cfi]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- masked
  "The objects of a masked group in `mode`: a circle masking a rect."
  [mode]
  (let [group  (uuid/next)
        mask   (uuid/next)
        rect   (uuid/next)
        shape  (fn [id type attrs]
                 (-> (cts/setup-shape (merge {:id id :type type :name (name type)
                                              :x 0 :y 0 :width 100 :height 100}
                                             attrs))
                     (assoc :parent-id group :frame-id uuid/zero)))]
    {:ids     {:group group :mask mask :rect rect}
     :objects {uuid/zero (-> (cts/setup-shape {:id uuid/zero :type :frame :name "root"})
                             (assoc :shapes [group]))
               group     (-> (shape group :group (cond-> {:masked-group true}
                                                   (some? mode) (assoc :mask-mode mode)))
                             (assoc :parent-id uuid/zero :shapes [mask rect]))
               mask      (shape mask :circle {:x 25 :y 25 :width 50 :height 50})
               rect      (shape rect :rect {})}}))

(defn- clip-parents
  [{:keys [objects]} id]
  (let [index (cfi/create-clip-index objects (cfi/generate-child-all-parents-index objects))]
    (into #{} (map :id) (get index id))))

(t/deftest a-mask-clips-what-it-keeps
  (doseq [mode [nil :alpha :luma]]
    (let [file (masked mode)]
      (t/is (= #{(get-in file [:ids :mask])} (clip-parents file (get-in file [:ids :rect])))
            (str "mode " mode)))))

(t/deftest an-inverted-mask-clips-nothing
  ;; an inverted group keeps all of its layers but the mask: picking them
  ;; on the canvas must not be limited to it
  (doseq [mode [:alpha-inverted :luma-inverted]]
    (let [file (masked mode)]
      (t/is (empty? (clip-parents file (get-in file [:ids :rect])))
            (str "mode " mode)))))
