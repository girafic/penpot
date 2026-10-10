;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.render-wasm.unit-outlines-test
  (:require
   [app.common.types.path :as path]
   [app.render-wasm.api :as api]
   [cljs.test :as t :include-macros true]))

(t/deftest the-outlines-of-the-units-of-a-text-read-back
  ;; as `_get_text_unit_outlines` writes them: a piece of the unit 1 with
  ;; one run of the span 2, then the glyphs of no unit, with none
  (let [content (path/from-plain [{:command :move-to :params {:x 1 :y 2}}
                                  {:command :line-to :params {:x 3 :y 4}}
                                  {:command :close-path :params {}}])
        words   (/ (path/get-byte-size content) 4)
        offset  2
        buffer  (js/ArrayBuffer. (* 4 (+ offset 1 4 3 words 4)))
        u32     (js/Uint32Array. buffer)
        f32     (js/Float32Array. buffer)
        rest-at (+ offset 8 words)]
    (aset u32 offset 2)
    (aset u32 (+ offset 1) 1)
    (aset f32 (+ offset 2) 5.5)
    (aset f32 (+ offset 3) 7.25)
    (aset u32 (+ offset 4) 1)
    (aset u32 (+ offset 5) 0)
    (aset u32 (+ offset 6) 2)
    (aset u32 (+ offset 7) 3)
    (path/write-to content buffer (* 4 (+ offset 8)))
    (aset u32 rest-at 0xFFFFFFFF)
    (aset u32 (+ rest-at 3) 0)
    (let [[unit rest] (api/read-unit-outlines u32 f32 offset)
          run         (first (:runs unit))]
      (t/is (= 1 (:unit unit)))
      (t/is (= {:x 5.5 :y 7.25} (:center unit)))
      (t/is (= [0 2] [(:paragraph run) (:span run)]))
      (t/is (= (str content) (str (:content run))))
      (t/is (nil? (:unit rest)) "the glyphs of no unit")
      (t/is (= [] (:runs rest))))))
