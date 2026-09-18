;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.types.shape-glass-test
  (:require
   [app.common.types.color :as clr]
   [app.common.types.library :as ctl]
   [app.common.types.shape :as cts]
   [app.common.types.shape.glass :as ctsg]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- glass
  [& {:as attrs}]
  (merge ctsg/default-attrs {:id (uuid/next)} attrs))

(t/deftest glass-schema-test
  (t/testing "The default glass is valid"
    (t/is (ctsg/valid-glass? (glass))))

  (t/testing "A glass needs every parameter"
    (t/is (not (ctsg/valid-glass? (dissoc (glass) :refraction)))))

  (t/testing "Only the :glass type is accepted"
    (t/is (not (ctsg/valid-glass? (glass :type :background-blur)))))

  (t/testing "A glass from before the optional keys is still valid"
    (t/is (ctsg/valid-glass? (apply dissoc (glass) (keys ctsg/optional-defaults)))))

  (t/testing "The light color is a color or a gradient, optionally linked"
    (t/is (ctsg/valid-glass? (glass :light-color {:color "#FFCC00"})))
    (t/is (ctsg/valid-glass?
           (glass :light-color {:gradient {:type :linear
                                           :start-x 0 :start-y 0.5
                                           :end-x 1 :end-y 0.5
                                           :width 0
                                           :stops [{:color "#FFCC00" :offset 0}
                                                   {:color "#0000FF" :offset 1}]}})))
    (t/is (not (ctsg/valid-glass? (glass :light-color {:image {:id (uuid/next)
                                                               :width 1 :height 1}}))))
    (t/is (ctsg/valid-glass? (glass :light-color {:color "#FFCC00" :opacity 1
                                                  :ref-id (uuid/next) :ref-file (uuid/next)})))
    (t/is (not (ctsg/valid-glass? (glass :light-color {:color "red"}))))
    (t/is (not (ctsg/valid-glass? (glass :light-color {:color "#FFCC00" :id (uuid/next)})))))

  (t/testing "Only known textures and numeric values are accepted"
    (doseq [texture ctsg/textures]
      (t/is (ctsg/valid-glass? (glass :texture texture))))
    (t/is (not (ctsg/valid-glass? (glass :texture :etched))))
    (t/is (not (ctsg/valid-glass? (glass :saturation "x"))))))

(t/deftest get-value-test
  (t/testing "Missing optional keys read as their defaults"
    (let [old-glass (apply dissoc (glass) (keys ctsg/optional-defaults))]
      (t/is (= 100 (ctsg/get-value old-glass :saturation)))
      (t/is (= 2 (ctsg/get-value old-glass :highlight-width)))
      (t/is (= :none (ctsg/get-value old-glass :texture)))))

  (t/testing "Stored values win"
    (t/is (= 40 (ctsg/get-value (glass :saturation 40) :saturation)))))

(t/deftest color->light-color-test
  (t/testing "A plain color keeps color, opacity and library reference"
    (let [ref-id (uuid/next)
          ref-file (uuid/next)]
      (t/is (= {:color "#112233" :opacity 0.5 :ref-id ref-id :ref-file ref-file}
               (ctsg/color->light-color {:color "#112233" :opacity 0.5
                                         :ref-id ref-id :ref-file ref-file
                                         :id (uuid/next) :name "extra"})))))

  (t/testing "A gradient passes through"
    (let [gradient {:type :linear
                    :start-x 0 :start-y 0.5 :end-x 1 :end-y 0.5 :width 0
                    :stops [{:color "#AA0000" :opacity 1 :offset 0}
                            {:color "#0000AA" :opacity 1 :offset 1}]}]
      (t/is (= {:gradient gradient}
               (ctsg/color->light-color {:gradient gradient :id (uuid/next)})))))

  (t/testing "An image gives no light color"
    (t/is (nil? (ctsg/color->light-color {:image {:id (uuid/next) :width 1 :height 1}})))))

(defn- lit-shape
  [light-color]
  (-> (cts/setup-shape {:type :rect :x 0 :y 0 :width 10 :height 10})
      (assoc :glass (glass :light-color light-color))))

(t/deftest glass-light-color-in-shape-colors-test
  (let [library-id (uuid/next)
        color-id   (uuid/next)
        linked     (lit-shape {:color "#FF0000" :opacity 1
                               :ref-id color-id :ref-file library-id})]

    (t/testing "The light color is listed with the shape colors"
      (t/is (some #(= "#FF0000" (:color %)) (cts/get-all-colors linked)))
      (t/is (not-any? #(= clr/white (:color %))
                      (cts/get-all-colors (assoc linked :glass (glass))))))

    (t/testing "A linked light color counts as a library use"
      (t/is (cts/uses-library-color? linked library-id color-id)))

    (t/testing "Syncing copies the library color into the light"
      (let [synced (ctl/sync-colors linked library-id
                                    {color-id {:id color-id :color "#00FF00" :opacity 0.5}})]
        (t/is (= {:color "#00FF00" :opacity 0.5 :ref-id color-id :ref-file library-id}
                 (get-in synced [:glass :light-color])))))

    (t/testing "Syncing a deleted library color detaches the light"
      (let [synced (ctl/sync-colors linked library-id {})]
        (t/is (= {:color "#FF0000" :opacity 1}
                 (get-in synced [:glass :light-color])))))

    (t/testing "Remapping points the light to the new library"
      (let [other-library (uuid/next)
            remapped (cts/remap-colors linked other-library {:id color-id})]
        (t/is (= other-library (get-in remapped [:glass :light-color :ref-file])))))))

(t/deftest shape-with-glass-test
  (let [shape (cts/setup-shape {:type :rect :x 0 :y 0 :width 68 :height 64})]
    (t/testing "A shape accepts a glass effect"
      (t/is (cts/valid-shape? (assoc shape :glass (glass)))))

    (t/testing "A shape rejects a malformed glass effect"
      (t/is (not (cts/valid-shape? (assoc shape :glass (glass :frost "4"))))))))
