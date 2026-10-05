;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.export-animation
  "The page of the export tab (see `app.main.data.exports.animation-tab`)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.math :as mth]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-job :as job]
   [app.main.data.exports.animation-tab :as dat]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:export-tab
  (l/derived :export-tab st/state))

(defn size-label
  "The size of a file, `bytes`, as people read it."
  [bytes]
  (cond
    (nil? bytes)                  ""
    (< bytes 1024)                (str bytes " B")
    (< bytes (* 1024 1024))       (str (mth/round (/ bytes 1024.0)) " KB")
    :else                         (str (mth/precision (/ bytes (* 1024.0 1024.0)) 1) " MB")))

(defn preview-kind
  "How the Done page plays a file of the extension `ext`: as a video, as
  an image (GIF, AVIF and SVG animate there; an SVG runs no script in
  an image), or not (`:none`: Lottie, for want of a player)."
  [ext]
  (case ext
    (".mp4" ".webm")        :video
    (".gif" ".avif" ".svg") :image
    :none))

(defn page-state
  "What the export tab shows of its state `tab`: `{:text :percent :notes
  :preview :actions}`, the text as `[key & args]` for `tr`, the percent
  while it exports, the notes of the file (see `dea/export-notes`), how
  to play it (`{:kind :url :name}`, see `preview-kind`) and its buttons
  (`:cancel` `:download` `:close`)."
  [{:keys [status params page percent cancelling? result error frame-url]}]
  (let [board  (get-in page [:objects (:board-id params) :name])
        format (some-> (:format params) name str/upper)]
    (case status
      ;; before the tab starts its job
      nil        {:text ["export-animation.waiting"] :actions []}
      :waiting   {:text ["export-animation.waiting"] :actions []}
      :loading   {:text ["export-animation.loading"] :actions []}
      :exporting (cond-> {:text ["export-animation.exporting" (or board "") (or format "")]
                          :percent (or percent 0)
                          :actions (if cancelling? [] [:cancel])}
                   ;; the picture it took last
                   (some? frame-url) (assoc :frame frame-url))
      :done      {:text ["export-animation.done"
                         (str (:name result) (:ext result))
                         (size-label (:size result))]
                  :notes (:notes result)
                  :preview {:kind (preview-kind (:ext result))
                            :url (:url result)
                            :name (str (:name result) (:ext result))}
                  :actions [:download :close]}
      :cancelled {:text ["export-animation.cancelled"] :actions [:close]}
      :stale     {:text ["export-animation.stale"] :actions [:close]}
      :duplicate {:text ["export-animation.duplicate"] :actions [:close]}
      :error     {:text (job/error-text error) :actions [:close]}
      {:text ["export-animation.invalid"] :actions [:close]})))

(mf/defc export-page*
  "The export tab, for the job of the query params `query`."
  {::mf/lazy-load true}
  [{:keys [query]}]
  (let [tab  (mf/deref ref:export-tab)
        view (page-state tab)

        on-cancel   (mf/use-fn #(st/emit! (dat/cancel)))
        on-download (mf/use-fn #(st/emit! (dat/download-again)))
        on-close    (mf/use-fn #(.close js/window))]

    (mf/with-effect []
      (st/emit! (dat/initialize query))
      #(st/emit! (dat/finalize)))

    [:main {:class (stl/css :export-page)}
     [:section {:class (stl/css-case :export-card true
                                     :export-card-wide (or (some? (:frame view))
                                                           (contains? #{:video :image}
                                                                      (get-in view [:preview :kind]))))}
      [:h1 {:class (stl/css :title)} (tr "export-animation.title")]
      [:p {:class (stl/css :status) :role "status" :aria-live "polite"}
       (apply tr (:text view))]

      (when-let [frame (:frame view)]
        [:div {:class (stl/css :preview-frame)}
         [:img {:class (stl/css :preview)
                :src frame
                :alt (tr "export-animation.frame-alt")}]])

      (when-let [percent (:percent view)]
        [:div {:class (stl/css :progress)}
         [:div {:class (stl/css :bar)
                :role "progressbar"
                :aria-valuenow percent
                :aria-valuemin 0
                :aria-valuemax 100}
          [:div {:class (stl/css :bar-fill)
                 :style {:width (str percent "%")}}]]
         [:span {:class (stl/css :percent)} (str percent "%")]])

      (when-let [{:keys [kind url name]} (:preview view)]
        (case kind
          :video (when url
                   [:div {:class (stl/css :preview-frame)}
                    [:video {:class (stl/css :preview)
                             :src url
                             :controls true
                             :auto-play true
                             :muted true
                             :loop true
                             :plays-inline true}]])
          :image (when url
                   [:div {:class (stl/css :preview-frame)}
                    [:img {:class (stl/css :preview)
                           :src url
                           :alt (tr "export-animation.preview-alt" name)}]])
          [:p {:class (stl/css :note)} (tr "export-animation.no-preview")]))

      (for [note (:notes view)]
        [:p {:key (name (first note)) :class (stl/css :note)} (dea/note-text note)])

      [:div {:class (stl/css :actions)}
       (for [action (:actions view)]
         (case action
           :cancel   [:> button* {:key "cancel" :variant "secondary" :on-click on-cancel}
                      (tr "export-animation.cancel")]
           :download [:> button* {:key "download" :variant "secondary" :on-click on-download}
                      (tr "export-animation.download-again")]
           :close    [:> button* {:key "close" :variant "primary" :on-click on-close}
                      (tr "export-animation.close")]))]]]))
