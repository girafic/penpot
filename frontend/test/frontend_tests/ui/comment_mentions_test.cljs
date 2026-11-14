;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.comment-mentions-test
  "A person picked among those to mention goes into the comment where
  its @ was typed, also once the caret left the input: a tap on a phone
  takes it out before the click on the entry (Penpot Motion)."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.uuid :as uuid]
   [app.main.refs :as refs]
   [app.main.ui.comments :as cmt]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ann
  {:id (uuid/custom 1 1) :fullname "Ann" :email "ann@example.com"})

(defn- picked
  "What the input of a new comment holds once `@` was typed in it, the
  caret moved out (`caret-out?`) and the entry of Ann tapped: the names
  mentioned, and whether the list is still shown."
  [caret-out?]
  (thd/with-dom
    (fn [{:keys [root document window]}]
      (with-redefs [refs/team    (l/atom {:members [ann]})
                    refs/profile (l/atom {:id (uuid/custom 1 2)})]
        (thd/render! root (mf/element cmt/comment-floating-thread-draft*
                                      #js {:draft {:position (gpt/point 0 0) :content ""}
                                           :zoom 1
                                           :docked true
                                           :onCancel (fn [])
                                           :onSubmit (fn [_])}))
        (let [input     (.querySelector document "[contenteditable=true]")
              span      (.-firstChild input)
              text      (.createTextNode document "@")
              selection (.getSelection window)
              range     (.createRange document)]
          ;; `@` typed: the caret after it, the list shown
          (set! (.-textContent span) "")
          (.appendChild span text)
          (.setStart range text 1)
          (.collapse range true)
          (.removeAllRanges selection)
          (.addRange selection range)
          (thd/act #(.dispatchEvent document (new (.-Event window) "selectionchange")))
          (when caret-out?
            (.removeAllRanges selection)
            (thd/act #(.dispatchEvent document (new (.-Event window) "selectionchange"))))
          (some->> (.querySelector document "[class*=comments-mentions-entry]")
                   (thd/click! window))
          (-> (thd/settle)
              (.then (fn []
                       {:mentioned (mapv #(.-textContent %) (.querySelectorAll input "[data-type=mention]"))
                        :list?     (some? (.querySelector document "[class*=comments-mentions-choice]"))}))))))))

(t/deftest ^:async a-mention-picked-goes-where-its-at-was-typed
  (t/is (= {:mentioned ["Ann"] :list? false} (await (picked false)))
        "the caret in the input, as with a mouse")
  (t/is (= {:mentioned ["Ann"] :list? false} (await (picked true)))
        "the caret gone from the input, as a tap on a phone takes it"))
