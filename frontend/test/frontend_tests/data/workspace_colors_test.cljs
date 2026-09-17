;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.data.workspace-colors-test
  (:require
   [app.common.types.shape.glass :as ctsg]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.colors :as dwc]
   [clojure.test :as t]
   [potok.v2.core :as ptk]))

(t/deftest build-change-fill-event
  (let [color1 {:color "#fabada"
                :opacity 1}
        color2 {:color "#fabada"
                :opacity 1
                :ref-id uuid/zero
                :ref-file uuid/zero}
        color3 {:color "#fabada"
                :opacity -1}
        color4 {:opacity 1
                :color "ffffff"}]
    (t/is (ptk/event? (dwc/change-fill #{uuid/zero} color1 1)))
    (t/is (ptk/event? (dwc/change-fill #{uuid/zero} color2 1)))
    (t/is (thrown? js/Error
                   (ptk/event? (dwc/change-fill #{uuid/zero} color3 1))))
    (t/is (thrown? js/Error
                   (ptk/event? (dwc/change-fill #{uuid/zero} color4 1))))))

(t/deftest build-add-fill-event
  (let [color1 {:color "#fabada"
                :opacity 1}
        color2 {:color "#fabada"
                :opacity 1
                :ref-id uuid/zero
                :ref-file uuid/zero}
        color3 {:color "#fabada"
                :opacity -1}
        color4 {:opacity 1
                :color "ffffff"}]
    (t/is (ptk/event? (dwc/add-fill #{uuid/zero} color1 1)))
    (t/is (ptk/event? (dwc/add-fill #{uuid/zero} color2 1)))
    (t/is (thrown? js/Error
                   (ptk/event? (dwc/add-fill #{uuid/zero} color3 1))))
    (t/is (thrown? js/Error
                   (ptk/event? (dwc/add-fill #{uuid/zero} color4 1))))))


(t/deftest update-colorpicker-color-skips-add-recent-on-incomplete-image-state
  ;; Regression for https://github.com/penpot/penpot/issues/8443.
  ;;
  ;; Closing the fill dialog while the image upload is still in flight leaves
  ;; the colorpicker's current-color with only :opacity (no :image, :gradient,
  ;; or :color). Before the guard, ptk/watch eagerly built (add-recent-color
  ;; partial), which calls (clr/check-color partial) and threw an "expected
  ;; valid color" assertion that surfaced as an Internal Assertion Error toast.
  (let [partial-image-state {:colorpicker {:type :image
                                           :current-color {:opacity 1}}}
        event (dwc/update-colorpicker-color {} true)]
    (t/is (nil? (ptk/watch event partial-image-state nil)))))


(t/deftest update-colorpicker-color-skips-add-recent-when-only-opacity-on-color-type
  ;; Same incomplete-state shape, but the colorpicker is on the plain-color tab
  ;; (e.g. the user clicked elsewhere before the picker had a chance to commit
  ;; a hex). The existing :type-and-:color-nil guard sits on the colorpicker
  ;; map's :type — but get-color-from-colorpicker-state strips :type from its
  ;; output, so that guard never fires. The schema-based guard catches it.
  (let [colorless-state {:colorpicker {:type :color
                                       :current-color {:opacity 1}}}
        event (dwc/update-colorpicker-color {} true)]
    (t/is (nil? (ptk/watch event colorless-state nil)))))


(t/deftest update-colorpicker-color-still-emits-recent-for-valid-plain-color
  ;; Sanity check: a fully-populated plain color still produces a watch
  ;; observable (the rx/of branch is reached) so we know the guard isn't
  ;; over-eager and silently dropping legitimate colors.
  (let [valid-state {:colorpicker {:type :color
                                   :current-color {:color "#ff0000" :opacity 1}}}
        event (dwc/update-colorpicker-color {} true)]
    (t/is (some? (ptk/watch event valid-state nil)))))

(t/deftest extract-all-colors-lists-the-glass-light
  (let [shape-id  (uuid/next)
        file-id   (uuid/next)
        other-lib (uuid/next)
        shape     (fn [light-color]
                    {:id shape-id
                     :type :rect
                     :glass (cond-> (assoc ctsg/default-attrs :id (uuid/next))
                              (some? light-color)
                              (assoc :light-color light-color))})]

    (t/testing "the default white light is not listed"
      (t/is (empty? (dwc/extract-all-colors [(shape nil)] file-id {}))))

    (t/testing "a light color is listed as a glass color"
      (t/is (= [{:attrs {:color "#FF0000" :opacity 1}
                 :prop :glass
                 :shape-id shape-id
                 :index 0}]
               (dwc/extract-all-colors [(shape {:color "#FF0000" :opacity 1})] file-id {}))))

    (t/testing "a reference to a library that is not available is dropped"
      (let [[{:keys [attrs]}]
            (dwc/extract-all-colors
             [(shape {:color "#FF0000" :opacity 1 :ref-id (uuid/next) :ref-file other-lib})]
             file-id {})]
        (t/is (= {:color "#FF0000" :opacity 1} attrs))))

    (t/testing "a reference to the current file is kept"
      (let [ref-id (uuid/next)
            [{:keys [attrs]}]
            (dwc/extract-all-colors
             [(shape {:color "#FF0000" :opacity 1 :ref-id ref-id :ref-file file-id})]
             file-id {})]
        (t/is (= ref-id (:ref-id attrs)))))))

(t/deftest change-color-in-selected-accepts-glass
  (t/is (ptk/event? (dwc/change-color-in-selected
                     [{:prop :glass :shape-id uuid/zero :index 0}]
                     {:color "#00ff00" :opacity 1}
                     {:color "#ff0000" :opacity 1}))))
