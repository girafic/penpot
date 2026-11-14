;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.ui.dashboard-templates-test
  "A copy of a template added from the dashboard: on the page of a
  project its files are fetched again once the import ended, as the page
  counts the copy among them (it showed \"loading your files\" for good);
  from the other pages the dashboard goes to the recent files."
  (:require
   [app.common.uuid :as uuid]
   [app.main.data.common :as dcm]
   [app.main.data.modal :as modal]
   [app.main.data.project :as dpj]
   [app.main.store :as st]
   [app.main.ui.dashboard.templates]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(def ^:private import-template!
  @#'app.main.ui.dashboard.templates/import-template!)

(defn- types-at-the-end
  "The types of the events the dashboard emits once the import of a
  template into `project-id` (nil: from the recent files) ended."
  [project-id]
  (let [emitted (atom [])]
    ;; of the arities of `st/emit!`, which its callers call directly
    (with-redefs [st/emit! (fn
                             ([] nil)
                             ([event] (swap! emitted conj event))
                             ([event & events] (swap! emitted into (cons event events))))]
      (import-template! {:id "wireframes" :name "Wireframes"} (uuid/next) project-id (uuid/next) "dashboard-project")
      (let [shown     (first (filter #(= ::modal/show-modal (ptk/type %)) @emitted))
            ;; the callback as the modal holds it, what the import calls
            on-finish (get-in (ptk/update shown {}) [::modal/modal :props :on-finish-import])]
        (reset! emitted [])
        (on-finish)
        (set (map ptk/type @emitted))))))

(t/deftest a-project-fetches-its-files-again
  (let [types (types-at-the-end (uuid/next))]
    (t/is (contains? types ::dpj/fetch-files) "the copy shows among its files")
    (t/is (not (contains? types ::dcm/go-to-dashboard-recent)) "the page stays")))

(t/deftest the-recent-files-show-the-copy
  (let [types (types-at-the-end nil)]
    (t/is (contains? types ::dcm/go-to-dashboard-recent))
    (t/is (not (contains? types ::dpj/fetch-files)))))
