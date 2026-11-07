;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.touch-devices-test
  "Penpot on phones: what kind of screen it is, the view mode as wide as
  a phone, its link from the workspace, and the view mode that follows
  the board selected in the workspace."
  (:require
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.follow :as dvf]
   [app.main.data.workspace.phone :as dwph]
   [app.main.ui.workspace.phone-link :as phone-link]
   [app.util.touch :as touch]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(def ^:private page-id (uuid/next))
(def ^:private board-a (uuid/next))
(def ^:private board-b (uuid/next))
(def ^:private other-page (uuid/next))
(def ^:private board-c (uuid/next))

(defn- viewer-state
  "The view mode of a page of two boards, B listed first, at `index`; a
  page of its own lists C."
  [index]
  {:route {:params {:query (cond-> {:page-id (str page-id)}
                             (some? index) (assoc :index (str index)))}}
   :viewer {:pages {page-id {:frames [{:id board-b :selrect {:width 780}}
                                      {:id board-a :selrect {:width 390}}]}
                    other-page {:frames [{:id board-c :selrect {:width 390}}]}}}})

(t/deftest phone-screens
  (t/is (true? (touch/phone-screen? 393 852 true)) "an iPhone")
  (t/is (true? (touch/phone-screen? 852 393 true)) "an iPhone held across")
  (t/is (false? (touch/phone-screen? 744 1133 true)) "an iPad mini is a tablet")
  (t/is (false? (touch/phone-screen? 393 852 false)) "not touched: no phone")
  (t/is (false? (touch/phone-screen? nil nil true)) "no screen"))

(t/deftest viewport-tags
  (t/is (= "width=device-width, initial-scale=1, viewport-fit=cover"
           (touch/viewport-content :viewer true true)
           (touch/viewport-content :viewer false false))
        "the view mode is as wide as the device, under its notch")
  (t/is (nil? (touch/viewport-content :workspace true true))
        "a phone keeps the workspace as wide as a desktop page")
  (t/is (= "width=device-width, initial-scale=1"
           (touch/viewport-content :workspace false true)
           (touch/viewport-content :dashboard-recent false true))
        "a tablet lays Penpot out at its own width")
  (t/is (nil? (touch/viewport-content :workspace false false)) "a desktop has no tag"))

(t/deftest phone-links
  (let [file-id (uuid/next)
        url     (phone-link/link "https://pc.example:3449/" file-id page-id board-a)]
    (t/is (= (str "https://pc.example:3449/?screen=viewer&file-id=" file-id
                  "&page-id=" page-id "&section=interactions&frame-id=" board-a
                  "&follow=true")
             url))
    (t/is (not (re-find #"frame-id" (phone-link/link "/" file-id page-id nil)))
          "without a board selected, the page")
    (t/is (phone-link/local-host? "localhost"))
    (t/is (phone-link/local-host? "127.0.0.1"))
    (t/is (not (phone-link/local-host? "studio-mac.tail1234.ts.net")))))

(t/deftest qr-modules
  (t/is (= "M0 0h1v1h-1zM1 1h1v1h-1z"
           (phone-link/qr-path [[true false] [false true]]))))

(t/deftest width-zoom
  (let [state (-> (viewer-state 1)
                  (assoc :viewer-local {:viewport-size {:width 393 :height 852}}))
        zoomed (ptk/update dv/zoom-to-width state)]
    (t/is (= (/ 393 390) (get-in zoomed [:viewer-local :zoom])) "board A as wide as the screen")
    (t/is (= :width (get-in zoomed [:viewer-local :zoom-type]))))
  (let [zoomed (ptk/update dv/zoom-to-width (assoc (viewer-state 0) :viewer-local {:zoom 1}))]
    (t/is (= 1 (get-in zoomed [:viewer-local :zoom])) "no size yet: the zoom it had")
    (t/is (= :width (get-in zoomed [:viewer-local :zoom-type])) "to fit once it has its size")))

(t/deftest follow-steps
  (t/is (dvf/follow? {:follow "true"}))
  (t/is (not (dvf/follow? {})))
  (t/is (= [:frame board-a] (dvf/board-step (viewer-state 0) page-id board-a))
        "another board of the page shown")
  (t/is (= [:frame board-b] (dvf/board-step (viewer-state nil) page-id board-b))
        "no index in the route yet: it goes there")
  (t/is (nil? (dvf/board-step (viewer-state 1) page-id board-a)) "shown already")
  (t/is (= [:page other-page 0] (dvf/board-step (viewer-state 0) other-page board-c))
        "a board of another page")
  (t/is (nil? (dvf/board-step (viewer-state 0) page-id (uuid/next))) "not listed"))

(t/deftest follow-pending-board
  (let [unknown (uuid/next)
        waiting (ptk/update (dvf/go-to-board page-id unknown) (viewer-state 0))]
    (t/is (= [page-id unknown] (get-in waiting [:viewer-local ::dvf/pending]))
          "a board not listed yet is kept")
    (t/is (nil? (get-in (ptk/update (dvf/go-to-board page-id board-a) waiting)
                        [:viewer-local ::dvf/pending]))
          "a board listed takes its place")))

(t/deftest follow-messages
  (let [file-id (uuid/next)]
    (t/is (= {:type :broadcast :event :viewer-follow :file-id file-id}
             (dvf/hello-message file-id)))
    (t/is (= {:type :broadcast :event :workspace-board :file-id file-id
              :page-id page-id :frame-id board-a}
             (dvf/board-message file-id page-id board-a)))))

(t/deftest workspace-followed
  (let [file-id (uuid/next)
        now     (inst-ms (ct/now))
        state   {:current-file-id file-id
                 :workspace-local {::dwph/told [page-id board-a]}}
        heard   (ptk/update (dwph/view-mode-heard {:file-id file-id}) state)]
    (t/is (dwph/followed? heard now) "followed once it heard the view mode")
    (t/is (not (dwph/followed? heard (+ now dvf/followed-ms 1000))) "for a while")
    (t/is (nil? (get-in heard [:workspace-local ::dwph/told]))
          "a view mode that starts gets the board selected again")
    (t/is (= state (ptk/update (dwph/view-mode-heard {:file-id (uuid/next)}) state))
          "the view mode of another file")
    (let [told (ptk/update (dwph/board-selected page-id board-b) heard)]
      (t/is (= [page-id board-b] (get-in told [:workspace-local ::dwph/told])))
      (t/is (= told (ptk/update (dwph/board-selected page-id board-b) told))
            "told once"))
    (t/is (= state (ptk/update (dwph/board-selected page-id board-b) state))
          "nothing told while no view mode follows")))
