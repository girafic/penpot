;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.ds-menu-test
  "The widest a menu of the design system may be: the one it is given,
  else on a phone the screen less a margin, else the default of the
  component."
  (:require
   [app.main.ui.ds.layout.menu :as menu]
   [app.util.touch :as touch]
   [cljs.test :as t :include-macros true]))

(def ^:private max-width-of @#'menu/max-width-of)

(t/deftest max-width-of-a-menu
  (t/testing "a given width stays, on a phone too"
    (with-redefs [touch/phone? (constantly true)]
      (t/is (= 320 (max-width-of 320)))
      (t/is (= "20rem" (max-width-of "20rem")))))
  (t/testing "on a phone, without one, the screen less a margin"
    (with-redefs [touch/phone? (constantly true)]
      (t/is (= "calc(100vw - 24px)" (max-width-of nil)))))
  (t/testing "elsewhere, without one, none: the component keeps its own"
    (with-redefs [touch/phone? (constantly false)]
      (t/is (nil? (max-width-of nil))))))
