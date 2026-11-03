;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.util.code-gen-motion-svg-test
  "Which SVG the code of the Inspect writes (Penpot Motion): the export of
  WASM for what plays nothing, the code generation, at rest, for the
  layers of an animated board."
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cthsh]
   [app.common.types.animation :as cta]
   [app.util.code-gen.motion :as cgm]
   [cljs.test :as t :include-macros true]))

(t/use-fixtures :each {:before cthi/reset-idmap!})

(defn- scene
  "Board :card holding the rect :box, which moves right in 800 ms when
  `animated?`: `{:page :files}`."
  [animated?]
  (let [file (-> (cthf/sample-file :file1)
                 (cthsh/add-sample-shape :card {:type :frame :name "Card" :x 0 :y 0 :width 400 :height 300})
                 (cthsh/add-sample-shape :box {:type :rect :parent-label :card :x 20 :y 20 :width 100 :height 60}))
        file (cond-> file
               animated?
               (cthf/apply-changes (-> (pcb/empty-changes nil)
                                       (pcb/with-page (cthf/current-page file))
                                       (pcb/set-timeline (cthi/id :card)
                                                         (-> (cta/make-timeline {:board-id (cthi/id :card) :duration 1000})
                                                             (cta/add-keyframe (cthi/id :box) {:time 0 :property :x :value 20})
                                                             (cta/add-keyframe (cthi/id :box) {:time 800 :property :x :value 220}))))))]
    {:page (cthf/current-page file) :files {(:id file) file}}))

(defn- codes
  [{:keys [page files]} & labels]
  (cgm/code-of-boards page files (map cthi/id labels)))

(t/deftest what-plays-nothing-takes-the-svg-of-wasm
  (let [still (scene false)]
    (t/is (true? (cgm/svg-by-wasm? "svg" true (codes still :box))))
    (t/is (true? (cgm/svg-by-wasm? "svg" true (codes still :card))))))

(t/deftest an-animated-board-takes-the-code-generation
  (let [moving (scene true)]
    (t/is (false? (cgm/svg-by-wasm? "svg" true (codes moving :card))))
    (t/is (false? (cgm/svg-by-wasm? "svg" true (codes moving :box)))
          "a layer of it too: WASM draws it where the preview moved it")))

(t/deftest only-svg-with-wasm-takes-the-svg-of-wasm
  (let [still (scene false)]
    (t/is (false? (cgm/svg-by-wasm? "html" true (codes still :box))))
    (t/is (false? (cgm/svg-by-wasm? "svg" false (codes still :box))))
    (t/is (false? (cgm/svg-by-wasm? "svg" nil (codes still :box))))))
