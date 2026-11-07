;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.phone-link
  "\"On your phone\": the view mode of the board selected, live on a
  phone (see `app.main.data.viewer.follow`), as a QR code and a link."
  (:require-macros [app.main.style :as stl])
  (:require
   ["uqr" :as uqr]
   [app.common.data.macros :as dm]
   [app.common.uri :as u]
   [app.main.data.modal :as modal]
   [app.main.data.notifications :as ntf]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.clipboard :as clipboard]
   [app.util.globals :as g]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn link
  "The address of the view mode of the board `frame-id` (else of the
  page `page-id`) of the file `file-id`, following the workspace, at
  `base` (where Penpot shows now: a phone reaches it as this computer
  does, not as the public address of the instance says)."
  [base file-id page-id frame-id]
  (dm/str base "?" (u/map->query-string
                    (cond-> {:screen "viewer"
                             :file-id file-id
                             :page-id page-id
                             :section "interactions"}
                      (some? frame-id) (assoc :frame-id frame-id)
                      :always          (assoc :follow true)))))

(defn local-host?
  "Whether `host` names this computer only: a phone does not reach it."
  [host]
  (contains? #{"localhost" "127.0.0.1" "[::1]" "::1"} host))

(defn qr-path
  "The dark modules of the QR code `modules` (rows of booleans) as the
  `d` of a path, one unit each."
  [modules]
  (str/join
   (for [[y row] (map-indexed vector modules)
         [x dark?] (map-indexed vector row)
         :when dark?]
     (dm/str "M" x " " y "h1v1h-1z"))))

(mf/defc qr-code*
  {::mf/private true}
  [{:keys [text]}]
  (let [qr      (mf/with-memo [text]
                  ;; tagged: a release build renames the members of what
                  ;; comes from outside it
                  (let [^js result (uqr/encode text #js {:ecc "L" :border 2})]
                    {:size (.-size result)
                     :path (qr-path (map array-seq (array-seq (.-data result))))}))
        size    (:size qr)]
    [:svg {:class (stl/css :qr-code)
           :view-box (dm/str "0 0 " size " " size)
           :shape-rendering "crispEdges"
           :role "img"
           :aria-label (tr "workspace.phone.qr")}
     [:rect {:width size :height size :fill "#ffffff"}]
     [:path {:d (:path qr) :fill "#000000"}]]))

(mf/defc phone-link-dialog*
  {::mf/register modal/components
   ::mf/register-as :phone-link}
  [all-props]
  ;; shown as a modal: its props come as they were given (see
  ;; `modal/show!`)
  (let [{:keys [file-id page-id frame-id]} (js->clj all-props :keywordize-keys true)
        location (unchecked-get g/global "location")
        host     (.-hostname ^js location)
        base     (dm/str (.-origin ^js location) (.-pathname ^js location))
        url      (link base file-id page-id frame-id)

        on-copy
        (mf/use-fn
         (mf/deps url)
         (fn []
           (-> (clipboard/to-clipboard url)
               (.then #(st/emit! (ntf/show {:level :info
                                            :type :toast
                                            :content (tr "workspace.phone.copied")
                                            :timeout 3000}))))))]

    [:div {:class (stl/css :modal-overlay)}
     [:div {:class (stl/css :modal-container)}
      [:div {:class (stl/css :modal-header)}
       [:h2 {:class (stl/css :modal-title)} (tr "workspace.phone.title")]
       [:> icon-button* {:variant "ghost"
                         :class (stl/css :modal-close-btn)
                         :aria-label (tr "labels.close")
                         :icon i/close
                         :on-click modal/hide!}]]
      [:div {:class (stl/css :modal-content)}
       [:> qr-code* {:text url}]
       [:p {:class (stl/css :modal-msg)} (tr "workspace.phone.scan")]
       (when (local-host? host)
         [:p {:class (stl/css :modal-warning)} (tr "workspace.phone.local-host")])
       [:div {:class (stl/css :link-row)}
        [:input {:class (stl/css :link)
                 :read-only true
                 :value url
                 :on-focus #(.select ^js (.-target %))}]
        [:> button* {:variant "secondary"
                     :on-click on-copy}
         (tr "workspace.phone.copy")]]
       [:p {:class (stl/css :modal-hint)} (tr "workspace.phone.home-screen")]]]]))
