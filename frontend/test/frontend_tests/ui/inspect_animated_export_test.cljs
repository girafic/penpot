;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.inspect-animated-export-test
  "The animated export panel of inspect in the view mode (Penpot Motion):
  what it offers when the tab said nothing, the tab opened in the click
  before the job starts (a tab opened later is blocked on a phone), how
  far the export is kept in the store while the panel goes and comes
  back, and the tab opened again."
  (:require
   [app.common.geom.rect :as grc]
   [app.common.types.animation :as cta]
   [app.common.uuid :as uuid]
   [app.main.data.viewer.export-job :as vej]
   [app.main.store :as st]
   [app.main.ui.exports.animated :as uea]
   [app.main.ui.inspect.exports]
   [app.util.dom :as dom]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.dom :as thd]
   [rumext.v2 :as mf]))

(def ^:private board-id (uuid/custom 31 1))

(def ^:private board
  {:id board-id :type :frame :name "Board" :selrect (grc/make-rect 0 0 400 300)})

(def ^:private timelines
  {board-id (cta/make-timeline {:board-id board-id :duration 1000})})

(defn- panel-props
  "The props of `uea/animated-export*` for `board`, as `more` changes
  them."
  [& {:as more}]
  (let [{:keys [progress on-open-tab on-export-here]} more]
    (js-obj "boards" [board]
            "timelines" timelines
            "export" {:format :lottie}
            "formats" [:lottie :dotlottie]
            "renderWasm?" true
            "progress" progress
            "onChange" identity
            "onExport" identity
            "onCancel" identity
            "onOpenTab" on-open-tab
            "onExportHere" on-export-here
            "codecSupported?" (fn [& _] (js/Promise.resolve true)))))

(defn- offer-buttons
  [document]
  (.-length (.querySelectorAll document "[class*=animated-offer] button")))

(t/deftest ^:async the-offer-has-no-export-here-without-it
  (await
   (thd/with-dom
     (fn [{:keys [root document]}]
       (let [offer {:in-progress true :in-tab? true :opened? false :offer? true}]
         (thd/render! root (mf/element uea/animated-export* (panel-props :progress offer :on-open-tab identity)))
         (t/is (= 1 (offer-buttons document)) "open the tab again, alone: the view mode exports in no place")
         (thd/render! root (mf/element uea/animated-export* (panel-props :progress offer
                                                                         :on-open-tab identity
                                                                         :on-export-here identity)))
         (t/is (= 2 (offer-buttons document)) "and export here, where it can (the workspace)")
         ;; what the panel asked the browser answers before the DOM goes
         (thd/settle))))))

(def ^:private inspect-panel
  @#'app.main.ui.inspect.exports/animated-export*)

(defn- inspect-props
  []
  #js {:boards [board]
       :timelines timelines
       :page {:id (uuid/custom 31 2)}
       :fileId (uuid/custom 31 3)})

(defn- export-button
  [document]
  (last (.querySelectorAll document "[class*=animated-export-btn]")))

(t/deftest ^:async the-tab-opens-in-the-click-and-the-store-keeps-the-export
  (let [log    (atom [])
        router (:router @st/state)]
    (swap! st/state assoc :router #{:export-animation})
    (add-watch st/state ::started
               (fn [_ _ old new]
                 (when (and (nil? (:viewer-export-animation old)) (some? (:viewer-export-animation new)))
                   (swap! log conj :started))))
    (try
      (await
       (with-redefs [dom/open-new-window (fn [& _] (swap! log conj :open) nil)]
         (thd/with-dom
           (fn [{:keys [root document window]}]
             (thd/render! root (mf/element inspect-panel (inspect-props)))
             (let [idle (.-textContent (export-button document))]
               (thd/click! window (export-button document))
               (t/is (= [:open :started] @log) "the tab opened in the click, then the job started")
               (t/is (some? (get-in @st/state [:viewer-export-animation :job :job-id])))

               ;; the panel goes (another tab of inspect) and comes back
               (thd/render! root nil)
               (thd/render! root (mf/element inspect-panel (inspect-props)))
               (t/is (not= idle (.-textContent (export-button document)))
                     "it shows the export going on, from the store")

               ;; the tab said nothing: it is opened again, in the click
               (let [job-id (get-in @st/state [:viewer-export-animation :job :job-id])]
                 (thd/act #(swap! st/state assoc-in [:viewer-export-animation :offer?] true))
                 (t/is (= 1 (offer-buttons document)))
                 (thd/click! window (.querySelector document "[class*=animated-offer] button"))
                 (t/is (= [:open :started :open] @log) "opened again")
                 (t/is (false? (get-in @st/state [:viewer-export-animation :offer?])) "the offer gone")
                 (t/is (= job-id (get-in @st/state [:viewer-export-animation :job :job-id])) "the same job"))
               ;; what the panel asked the browser answers before the DOM goes
               (thd/settle))))))
      (finally
        (remove-watch st/state ::started)
        (st/emit! (vej/cancel-job))
        (swap! st/state #(-> % (dissoc :viewer-export-animation) (assoc :router router)))))))
