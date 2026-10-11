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
   [app.config :as cf]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-job :as job]
   [app.main.data.exports.animation-tab :as dat]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.modules :as mod]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [promesa.core :as p]
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
  an image), in lottie-web (a Lottie or dotLottie, see
  `lottie-preview*`), the files of a ZIP, or not (`:none`)."
  [ext]
  (case ext
    (".mp4" ".webm")        :video
    (".gif" ".avif" ".svg") :image
    (".json" ".lottie")     :lottie
    ".zip"                  :zip
    :none))

(defn frame-view
  "How the export tab shows the pictures the export takes of the board,
  its `selrect`, at `scale`: `{:width :height :selrect}`, on a canvas of
  the board at that scale (see `frame-canvas*`); nil without them."
  [selrect scale]
  (let [{:keys [width height]} selrect]
    (when (and (number? scale) (number? width) (number? height) (pos? width) (pos? height))
      {:width (mth/round (* scale width))
       :height (mth/round (* scale height))
       :selrect selrect})))

(defn page-state
  "What the export tab shows of its state `tab`: `{:text :percent :notes
  :preview :actions}`, the text as `[key & args]` for `tr`, the percent
  while it exports, the notes of the file (see `dea/export-notes`), how
  to play it (`{:kind :url :name}`, see `preview-kind`) and its buttons
  (`:cancel` `:download` `:close`)."
  [{:keys [status params page percent cancelling? result error frame-scale progress]}]
  (let [boards (dat/job-boards params)
        index  (min (or (:board progress) 0) (dec (count boards)))
        ;; the board it exports: of several, the one at `index`
        shape  (get-in page [:objects (:board-id (nth boards index))])
        board  (:name shape)
        frame  (frame-view (:selrect shape) frame-scale)
        format (some->> (:format params) name keyword (get dea/format-labels))]
    (case status
      ;; before the tab starts its job
      nil        {:text ["export-animation.waiting"] :actions []}
      :waiting   {:text ["export-animation.waiting"] :actions []}
      :loading   {:text ["export-animation.loading"] :actions []}
      :exporting (cond-> {:text (if (> (count boards) 1)
                                  ["export-animation.exporting-boards"
                                   (or board "") (inc index) (count boards) (or format "")]
                                  ["export-animation.exporting" (or board "") (or format "")])
                          :percent (or percent 0)
                          :actions (if cancelling? [] [:cancel])}
                   ;; the pictures it takes, as it takes them
                   (some? frame) (assoc :frame frame))
      :done      {:text ["export-animation.done"
                         (str (:name result) (:ext result))
                         (size-label (:size result))]
                  :notes (:notes result)
                  :preview (cond-> {:kind (preview-kind (:ext result))
                                    :url (:url result)
                                    :name (str (:name result) (:ext result))}
                             ;; the files in a ZIP of several boards
                             (seq (:files result)) (assoc :files (:files result)))
                  :actions [:download :close]}
      :cancelled {:text ["export-animation.cancelled"] :actions [:close]}
      :stale     {:text ["export-animation.stale"] :actions [:close]}
      :duplicate {:text ["export-animation.duplicate"] :actions [:close]}
      :error     {:text (job/error-text error) :actions [:close]}
      {:text ["export-animation.invalid"] :actions [:close]})))

(defn page-title
  "What the title of the export tab says of its state `tab`: `{:board
  :percent}`, the board it exports and the percent while it exports, the
  file it made once done; nil before it knows the board."
  [{:keys [status params page percent progress result]}]
  (when-let [board (not-empty
                    (if (= :done status)
                      ;; the file it made: the board, or the ZIP of several
                      (:name result)
                      (get-in page [:objects
                                    (:board-id (get (dat/job-boards params) (or (:board progress) 0)))
                                    :name])))]
    (cond-> {:board board}
      (= :exporting status) (assoc :percent (or percent 0)))))

(defn- image-data
  [image]
  (if (instance? js/ImageData image)
    image
    (js/ImageData. (:data image) (:width image) (:height image))))

(mf/defc frame-canvas*
  "The pictures the export takes, as it takes them (see
  `dat/watch-frames!`), cut to the board, its `selrect`, on a canvas of
  `width` × `height` pixels: one drawing for each refresh of the screen."
  [{:keys [width height selrect]}]
  (let [canvas-ref (mf/use-ref nil)
        bx         (:x selrect)
        by         (:y selrect)]
    (mf/with-effect [width height bx by]
      (let [pending (atom nil)
            raf     (atom nil)
            draw    (fn []
                      (reset! raf nil)
                      (when-let [[image rect scale] @pending]
                        (when-let [^js canvas (mf/ref-val canvas-ref)]
                          (let [ctx     (.getContext canvas "2d")
                                [ox oy] (if rect
                                          (dea/board-offset {:x bx :y by} rect scale)
                                          [0 0])]
                            (.clearRect ctx 0 0 width height)
                            (.putImageData ctx (image-data image) (- ox) (- oy))))))
            stop    (dat/watch-frames!
                     (fn [image rect scale]
                       (reset! pending [image rect scale])
                       (when-not @raf
                         (reset! raf (js/requestAnimationFrame draw)))))]
        (fn []
          (stop)
          (some-> @raf js/cancelAnimationFrame))))

    [:canvas {:ref canvas-ref
              :class (stl/css :preview)
              :width width
              :height height
              :role "img"
              :aria-label (tr "export-animation.frame-alt")}]))

(mf/defc lottie-preview*
  "The Lottie at `url` (of a dotLottie, its Lottie with its images in it,
  see `dea/dotlottie-preview`) played by lottie-web, its SVG renderer,
  which the export draws its effects for, loaded here alone
  (`js/lottie.js`, see `scripts/build-libs.js`); a note when it can not."
  [{:keys [url name]}]
  (let [node-ref (mf/use-ref nil)
        size*    (mf/use-state nil)
        failed*  (mf/use-state false)]
    (mf/with-effect [url]
      (let [anim  (atom nil)
            live? (atom true)]
        (-> (p/let [module (mod/import (cf/resolve-href "js/lottie.js"))
                    data   (p/-> (js/fetch url) (.json))]
              (when-let [node (and @live? (mf/ref-val node-ref))]
                (reset! size* [(.-w ^js data) (.-h ^js data)])
                (reset! anim (.loadAnimation (unchecked-get module "default")
                                             #js {:container node
                                                  :renderer "svg"
                                                  :loop true
                                                  :autoplay true
                                                  :animationData data}))))
            (p/catch (fn [_]
                       (when @live?
                         (reset! failed* true)))))
        (fn []
          (reset! live? false)
          (some-> ^js @anim (.destroy)))))

    (if @failed*
      [:p {:class (stl/css :note)} (tr "export-animation.no-preview")]
      [:div {:class (stl/css :preview-frame)}
       [:div {:ref node-ref
              :class (stl/css :lottie-preview)
              :role "img"
              :aria-label (tr "export-animation.preview-alt" name)
              ;; at most its own size, as the other previews
              :style (when-let [[w h] @size*]
                       #js {:aspectRatio (str w " / " h)
                            :maxWidth (str w "px")})}]])))

(mf/defc export-page*
  "The export tab, for the job of the query params `query`."
  {::mf/lazy-load true}
  [{:keys [query]}]
  (let [tab   (mf/deref ref:export-tab)
        view  (page-state tab)
        title (if-let [{:keys [board percent]} (page-title tab)]
                (cond->> (tr "title.export-animation" board)
                  ;; how far it is, also from another tab
                  (some? percent) (str percent "% · "))
                (tr "title.default"))

        on-cancel   (mf/use-fn #(st/emit! (dat/cancel)))
        on-download (mf/use-fn #(st/emit! (dat/download-again)))
        on-close    (mf/use-fn #(.close js/window))]

    (mf/with-effect []
      (st/emit! (dat/initialize query))
      #(st/emit! (dat/finalize)))

    (mf/with-effect [title]
      (dom/set-html-title title))

    [:main {:class (stl/css :export-page)}
     [:section {:class (stl/css-case :export-card true
                                     :export-card-wide (or (some? (:frame view))
                                                           (contains? #{:video :image :lottie}
                                                                      (get-in view [:preview :kind]))))}
      [:h1 {:class (stl/css :title)} (tr "export-animation.title")]
      [:p {:class (stl/css :status) :role "status" :aria-live "polite"}
       (apply tr (:text view))]

      (when-let [{:keys [width height selrect]} (:frame view)]
        [:div {:class (stl/css :preview-frame)}
         [:> frame-canvas* {:width width :height height :selrect selrect}]])

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

      (when-let [{:keys [kind url name files]} (:preview view)]
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
          :lottie (when url
                    [:> lottie-preview* {:url url :name name}])
          :zip [:p {:class (stl/css :note)}
                (tr "export-animation.zip-files" (count files) (str/join ", " files))]
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
