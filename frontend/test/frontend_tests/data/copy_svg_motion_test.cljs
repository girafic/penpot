;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.data.copy-svg-motion-test
  "Which SVG Copy as SVG copies (Penpot Motion): the export of WASM for
  what plays nothing, the code generation, at rest, for the layers of an
  animated board, as the Inspect writes them."
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.test-helpers.shapes :as cthsh]
   [app.common.types.animation :as cta]
   [app.main.data.workspace.clipboard :as dwcp]
   [app.util.clipboard :as clipboard]
   [app.util.code-gen.markup-svg :as svg]
   [app.util.code-gen.markup-svg-wasm :as svg.wasm]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(t/use-fixtures :each {:before cthi/reset-idmap!})

(def ^:private wasm-markup "<svg data-from=\"wasm\"/>")
(def ^:private codegen-markup "<svg data-from=\"codegen\"/>")

(defn- scene
  "Board :card, whose rect :box moves right in 800 ms, and board :still
  with the rect :rock, which plays nothing."
  []
  (let [file (-> (cthf/sample-file :file1)
                 (cthsh/add-sample-shape :card {:type :frame :name "Card" :x 0 :y 0 :width 400 :height 300})
                 (cthsh/add-sample-shape :box {:type :rect :parent-label :card :x 20 :y 20 :width 100 :height 60})
                 (cthsh/add-sample-shape :still {:type :frame :name "Still" :x 600 :y 0 :width 400 :height 300})
                 (cthsh/add-sample-shape :rock {:type :rect :parent-label :still :x 620 :y 20 :width 100 :height 60}))]
    (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                 (pcb/with-page (cthf/current-page file))
                                 (pcb/set-timeline (cthi/id :card)
                                                   (-> (cta/make-timeline {:board-id (cthi/id :card) :duration 1000})
                                                       (cta/add-keyframe (cthi/id :box) {:time 0 :property :x :value 20})
                                                       (cta/add-keyframe (cthi/id :box) {:time 800 :property :x :value 220})))))))

(defn- state-of
  "The workspace state of `file` with the layer `label` selected and the
  `features` on."
  [file label features]
  {:current-file-id (:id file)
   :current-page-id (cthf/current-page-id file)
   :files {(:id file) file}
   :features features
   :workspace-local {:selected (d/ordered-set (cthi/id label))}})

(defn- copied
  "The SVG Copy as SVG puts on the clipboard for the layer `label` of
  `file` with the `features` on: `wasm-markup` from the export of WASM,
  `codegen-markup` from the code generation."
  [file label features]
  (let [copied* (atom nil)]
    (with-redefs [clipboard/to-clipboard-multi
                  (fn [data]
                    (reset! copied* data)
                    (p/resolved nil))
                  svg.wasm/get-formatted-svg-markup
                  (constantly wasm-markup)
                  svg/generate-formatted-markup
                  (constantly codegen-markup)]
      (ptk/effect (dwcp/copy-selected-svg) (state-of file label features) nil))
    (let [data @copied*]
      (t/is (= (get data "image/svg+xml") (get data "text/plain")))
      (get data "image/svg+xml"))))

(t/deftest an-animated-layer-is-copied-by-the-code-generation
  (t/is (= codegen-markup (copied (scene) :box #{"render-wasm/v1" "animation/v1"}))
        "WASM draws it where the preview moved it"))

(t/deftest a-layer-of-a-still-board-is-copied-by-wasm
  (t/is (= wasm-markup (copied (scene) :rock #{"render-wasm/v1" "animation/v1"}))))

(t/deftest without-wasm-the-code-generation-copies
  (t/is (= codegen-markup (copied (scene) :rock #{"animation/v1"}))))

(t/deftest without-motion-wasm-copies-as-upstream-does
  (t/is (= wasm-markup (copied (scene) :box #{"render-wasm/v1"}))))
