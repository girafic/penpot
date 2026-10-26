;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.data.workspace-colors-test
  (:require
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

(t/deftest a-new-stroke-colour-keeps-the-dash-pattern
  (let [shape   {:strokes [{:stroke-color "#000000" :stroke-opacity 1 :stroke-width 2
                            :stroke-style :dashed :stroke-alignment :center
                            :stroke-dash 6 :stroke-gap 4 :stroke-dash-offset 5}]}
        colored (dwc/update-shape-stroke-color shape 0 {:color "#ff0000" :opacity 1})
        linked  (dwc/update-shape-stroke-color shape 0 {:color "#00ff00" :opacity 1
                                                        :ref-id uuid/zero :ref-file uuid/zero})]
    (t/is (= "#ff0000" (get-in colored [:strokes 0 :stroke-color])))
    (t/is (= [6 4 5] ((juxt :stroke-dash :stroke-gap :stroke-dash-offset) (get-in colored [:strokes 0]))))
    (t/is (= 5 (get-in linked [:strokes 0 :stroke-dash-offset])) "a library colour too")))
