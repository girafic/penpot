;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.preview
  "The view mode as the preview of the prototype in a window of the
  workspace (`?preview=true`, see `app.main.ui.workspace.prototype-preview`):
  its screens in a history of their own, the boards the workspace sends
  it to, its zoom, hidden while closed, and what is edited in the
  workspace, shown as it is made or fetched once saved. The view mode
  itself keeps what it needs as it loads (`dv/preview?`, `dv/preview-go`)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.logging :as log]
   [app.common.uuid :as uuid]
   [app.main.data.fonts :as df]
   [app.main.data.viewer :as dv]
   [app.main.router :as rt]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn start-preview
  "The viewer as the preview of the prototype: its screens replace each
  other in the history of the browser, which it shares with the
  workspace, and go back and forth in a history of their own (see
  `preview-screen`, `preview-step`)."
  []
  (ptk/reify ::start-preview
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc ::rt/replace-only true)
          (assoc :viewer-preview {:back [] :forward []})))))

(defn preview-hidden?
  "Whether the preview of the prototype is closed, loaded, hidden (see
  `preview-hidden`)."
  [state]
  (true? (dm/get-in state [:viewer-preview :hidden?])))

(defn preview-hidden
  "The preview of the prototype closed (`hidden?`), loaded, hidden, to
  show again at once: nothing plays in it then, or shown again."
  [hidden?]
  (ptk/reify ::preview-hidden
    ptk/UpdateEvent
    (update [_ state]
      (d/update-when state :viewer-preview assoc :hidden? hidden?))

    ptk/WatchEvent
    (watch [_ _ _]
      (if hidden?
        (rx/of (dv/pause-timeline))
        (rx/empty)))))

(defn preview-screen
  "The screen at `index` on show in the preview: the one it leaves goes
  to its history, unless it got there going back or forth in it."
  [index]
  (ptk/reify ::preview-screen
    ptk/UpdateEvent
    (update [_ state]
      (d/update-when state :viewer-preview
                     (fn [{:keys [going] :as history}]
                       (let [current (:index history)]
                         (cond
                           (or (nil? current) (= index current))
                           (assoc history :index index)

                           (= index going)
                           (-> history (assoc :index index) (dissoc :going))

                           :else
                           (-> history
                               (update :back conj current)
                               (assoc :forward [] :index index)))))))))

(defn preview-step
  "The preview back to the screen before (`:back`) or forth to the one
  after (`:forward`), in its history."
  [direction]
  (ptk/reify ::preview-step
    ptk/UpdateEvent
    (update [_ state]
      (d/update-when state :viewer-preview
                     (fn [{:keys [back forward index] :as history}]
                       (let [[from to] (if (= :back direction) [:back :forward] [:forward :back])
                             target    (peek (if (= :back direction) back forward))]
                         (if (and (some? target) (some? index))
                           (-> history
                               (update from pop)
                               (update to conj index)
                               (assoc :going target))
                           history)))))

    ptk/WatchEvent
    (watch [_ state _]
      (when-let [target (dm/get-in state [:viewer-preview :going])]
        (rx/of (dv/go-to-frame-by-index target))))))

(defn preview-board
  "The preview to the board before (`:prev`) or after (`:next`) the one
  it shows, in the order the view mode lists them (see `dv/preview-frames`),
  as the arrows of its bar go: none before the first or after the last.
  The screen it leaves goes to its history (see `preview-screen`)."
  [direction]
  (ptk/reify ::preview-board
    ptk/WatchEvent
    (watch [_ state _]
      (let [current (some-> (rt/get-query-param (dm/get-in state [:route :query-params]) :index) parse-long)
            target  (when (some? current) (if (= :prev direction) (dec current) (inc current)))]
        (if (and (some? target) (< -1 target (count (dv/preview-frames state))))
          (rx/of (dv/go-to-frame-by-index target))
          (rx/empty))))))

(defn preview-zoom
  "The zoom of the preview as the workspace sets it: the board fitted in
  it (\"fit\"), filling it (\"fill\") or at its own size (\"100\"); nil
  for another `mode`."
  [mode]
  (case mode
    "fit"  dv/zoom-to-fit
    "fill" dv/zoom-to-fill
    "100"  dv/reset-zoom
    nil))

(defn refit-preview
  "The board of the preview fitted or filled again as it resizes, as its
  zoom says (see `preview-zoom`); at its own size it stays."
  []
  (ptk/reify ::refit-preview
    ptk/WatchEvent
    (watch [_ state _]
      (case (dm/get-in state [:viewer-local :zoom-type])
        :fit  (rx/of dv/zoom-to-fit)
        :fill (rx/of dv/zoom-to-fill)
        (rx/empty)))))

(defn- bundle-refreshed
  "The view mode showing `bundle`, and going to the board the preview
  waits for (see `dv/preview-go`), now that it may list it."
  [bundle]
  (ptk/reify ::bundle-refreshed
    ptk/UpdateEvent
    (update [_ state]
      (dv/assoc-bundle state bundle))

    ptk/WatchEvent
    (watch [_ state _]
      (if-let [pending (dm/get-in state [:viewer-preview :pending])]
        (rx/of (dv/preview-go pending))
        (rx/empty)))))

(defn refresh-bundle
  "The file shown fetched again, as it was saved last, the screen and the
  overlays on it staying: the preview of the prototype, when it missed
  what the workspace edited (see `apply-preview-changes`), once that is
  saved. `on-refreshed` is called once it is done; `on-failed` in its
  place when the fetch fails, which then shows nothing but a warning in
  the log: the preview tries again once the workspace saves."
  [on-refreshed on-failed]
  (ptk/reify ::refresh-bundle
    ptk/WatchEvent
    (watch [_ state _]
      (let [params   (rt/get-params state)
            file-id  (some-> (rt/get-query-param params :file-id) uuid/parse*)
            page-id  (some-> (rt/get-query-param params :page-id) uuid/parse*)
            share-id (some-> (rt/get-query-param params :share-id) uuid/parse*)]
        (->> (if (some? file-id)
               (->> (dv/load-bundle {:file-id file-id :share-id share-id})
                    (rx/mapcat
                     (fn [{:keys [fonts] :as bundle}]
                       (rx/of (df/fonts-fetched fonts)
                              (bundle-refreshed bundle)
                              (dv/update-page-position-data file-id page-id)))))
               (rx/empty))
             (rx/tap (fn [_] nil) (fn [_] nil) on-refreshed)
             (rx/catch (fn [cause]
                         (log/warn :hint "unable to refresh the preview" :cause cause)
                         (on-failed)
                         (rx/empty))))))))

(defn apply-preview-changes
  "The preview of the prototype showing `changes`, made in the workspace
  and not saved yet, as they are made; `on-stale` is called when they
  do not apply, for the workspace to refresh it once they are saved
  (see `refresh-bundle`)."
  [changes on-stale]
  (let [applied? (volatile! true)]
    (ptk/reify ::apply-preview-changes
      ptk/UpdateEvent
      (update [_ state]
        (try
          (dv/apply-changes state changes)
          (catch :default cause
            (log/warn :hint "unable to apply the changes of the workspace" :cause cause)
            (vreset! applied? false)
            state)))

      ptk/WatchEvent
      (watch [_ state _]
        (let [params  (rt/get-params state)
              file-id (some-> (rt/get-query-param params :file-id) uuid/parse*)
              page-id (some-> (rt/get-query-param params :page-id) uuid/parse*)]
          (if (and @applied? (some? file-id))
            (rx/of (dv/update-page-position-data file-id page-id))
            (rx/empty))))

      ptk/EffectEvent
      (effect [_ _ _]
        (when-not @applied?
          (on-stale))))))
