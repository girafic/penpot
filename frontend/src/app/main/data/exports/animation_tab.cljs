;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.animation-tab
  "The export tab: it runs the animated export the workspace starts (see
  `deaw/start-export-job`) in a tab of its own, with the file loaded as
  the view mode loads it and a WASM renderer of its own, so the
  workspace stays fluid. It waits for the workspace to save, exports,
  downloads, and tells the workspace on the channel of the job (see
  `app.main.data.exports.animation-job`) how far it is and how it ended;
  it holds the lock of the job while it lives."
  (:require
   [app.common.data :as d]
   [app.common.render-wasm.wasm :as wasm]
   [app.main.data.event :as ev]
   [app.main.data.exports.animation :as dea]
   [app.main.data.exports.animation-job :as job]
   [app.main.data.fonts :as df]
   [app.main.data.viewer :as dv]
   [app.main.features :as features]
   [app.main.store :as st]
   [app.render-wasm.api :as wasm.api]
   [app.render-wasm.shape :as wasm.shape]
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(def hello-interval-ms
  "How often the tab says it is there until the workspace has saved."
  2000)

(def save-wait-ms
  "How long the tab waits for the workspace to save before it gives up:
  a tab opened by hand, or a workspace closed meanwhile."
  30000)

(def progress-interval-ms
  "How often, at most, the tab tells the workspace its progress."
  100)

(defonce ^:private live
  ;; {:channel :release :cancel :cancel-requested? :result :preview-url
  ;;  :draw-frame :last-frame :listeners :last-post :start-wasm :save-wait-ms}
  (atom nil))

(defn- post!
  [message]
  (when-let [post (get-in @live [:channel :post])]
    (post message)))

(declare received)
(declare lock-held)

(defn- tab-state
  [state]
  (:export-tab state))

;; THE STEPS

(defn- failed
  "The export cannot go on: `reason` (`:save-failed` `:load` `:wasm`
  `:unavailable` `:nothing` `:export`), told to the workspace."
  [reason & [message]]
  (ptk/reify ::failed
    ptk/UpdateEvent
    (update [_ state]
      (update state :export-tab assoc :status :error :error {:reason reason :message message}))

    ptk/EffectEvent
    (effect [_ _ _]
      (post! {:type :error :reason reason :message message}))))

(defn- stale
  []
  (ptk/reify ::stale
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= :waiting (:status (tab-state state)))
        (assoc-in [:export-tab :status] :stale)))))

(defn- progress
  [progress]
  (ptk/reify ::progress
    ptk/UpdateEvent
    (update [_ state]
      (update state :export-tab assoc
              :progress progress
              :percent (dea/progress-percent progress)))

    ptk/EffectEvent
    (effect [_ _ _]
      ;; at most every `progress-interval-ms`, and the end
      (let [now     (js/Date.now)
            percent (dea/progress-percent progress)]
        (when (or (= 100 percent)
                  (> (- now (or (:last-post @live) 0)) progress-interval-ms))
          (swap! live assoc :last-post now)
          (post! (assoc progress :type :progress :percent percent)))))))

(defn- start-wasm!
  "A promise resolved once WASM draws the shapes of `objects`: its module
  loaded, a context on an `OffscreenCanvas`, the shapes in it with their
  fonts and images (`:on-render`). WASM asks for no frames of its own
  meanwhile (`wasm/disable-request-render?`), for the whole export."
  [objects board-id]
  (p/let [ready? @wasm.api/module]
    (if-not ready?
      (p/rejected (ex-info "WASM unavailable" {:type :wasm-unavailable}))
      (p/create
       (fn [resolve reject]
         (reset! wasm/disable-request-render? true)
         (let [rect   (or (get-in objects [board-id :selrect]) {:x 0 :y 0 :width 1 :height 1})
               canvas (js/OffscreenCanvas. 256 256)]
           (if (wasm.api/init-canvas-context canvas)
             (wasm.api/initialize-viewport objects 1 rect
                                           :background-opacity 0
                                           :force-sync true
                                           :on-render #(resolve nil))
             (reject (ex-info "no WebGL context" {:type :wasm-unavailable})))))))))

(defn tab-bridge
  "The bridge to WASM of the export tab (see `dea/default-bridge`): it
  sets the properties of the shapes it loaded itself, whatever page the
  workspace would show (as the view mode does, see
  `wasm.shape/set-shape-properties!`)."
  []
  (assoc (dea/default-bridge) :apply-shape-properties wasm.shape/set-shape-properties!))

(defn- preview-ready
  "The file of the export can be played at `url` (see `finished`)."
  [url]
  (ptk/reify ::preview-ready
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= :done (:status (tab-state state)))
        (assoc-in [:export-tab :result :url] url)))))

(defn- finished
  "The export ended with `result` (see `dea/run-export`): downloaded,
  kept for Download again and for its preview (an object URL of its
  blob, revoked when the tab closes), told to the workspace."
  [result]
  (ptk/reify ::finished
    ptk/UpdateEvent
    (update [_ state]
      (case (:status result)
        :ok (update state :export-tab assoc
                    :status :done
                    :result {:name (:name result)
                             :ext (:ext result)
                             :size (some-> (:blob result) .-size)
                             :notes (:notes (dea/export-notes result))
                             :files (:files result)})
        state))

    ptk/WatchEvent
    (watch [_ _ _]
      (case (:status result)
        :ok          (rx/empty)
        :unavailable (rx/of (failed :unavailable))
        (rx/of (failed :nothing))))

    ptk/EffectEvent
    (effect [_ _ _]
      (when (= :ok (:status result))
        (let [url (wapi/create-uri (:blob result))]
          (swap! live assoc :result result :preview-url url)
          (st/emit! (preview-ready url)))
        (dea/download-result result)
        (post! {:type :done
                :name (:name result)
                :ext (:ext result)
                :size (some-> (:blob result) .-size)
                :notes (:notes (dea/export-notes result))})))))

(defn- cancelled
  []
  (ptk/reify ::cancelled
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:export-tab :status] :cancelled))

    ptk/EffectEvent
    (effect [_ _ _]
      (post! {:type :cancelled}))))

(defn- frame-shown
  "The export took a picture of the board at `scale`: the page shows it
  (see `watch-frames!`)."
  [scale]
  (ptk/reify ::frame-shown
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (= :exporting (:status (tab-state state)))
        (assoc-in [:export-tab :frame-scale] scale)))))

(defn- show-frame!
  "Hand the picture `image` the export took, covering `rect` at `scale`
  (see `dea/run-export`), to the page that draws it (see
  `watch-frames!`), each of them: the page plays them as they come."
  [image rect scale]
  (let [frame [image rect scale]
        [old] (swap-vals! live assoc :last-frame frame)]
    (when-let [draw (:draw-frame old)]
      (apply draw frame))
    ;; the first one, and the first of a board at another scale
    (when (not= scale (get (:last-frame old) 2))
      (st/emit! (frame-shown scale)))))

(defn watch-frames!
  "`draw` (`[image rect scale]`) hears the pictures the export takes,
  first the one it took last; returns the function that stops it."
  [draw]
  (let [[_ {:keys [last-frame]}] (swap-vals! live assoc :draw-frame draw)]
    (when last-frame
      (apply draw last-frame))
    (fn []
      (swap! live #(cond-> % (identical? draw (:draw-frame %)) (dissoc :draw-frame))))))

(defn job-boards
  "The boards of the job `params`, `[{:board-id :scale}]`: its `:boards`
  (several, into a ZIP), else its board."
  [{:keys [board-id scale boards]}]
  (or (not-empty boards) [{:board-id board-id :scale scale}]))

(defn- run-job
  "A promise of the export of the job `params` on `page` (see
  `dea/run-exports`), its boards into a ZIP named after the file,
  unless it was cancelled meanwhile (while WASM started)."
  [params page files]
  (if (:cancel-requested? @live)
    (p/rejected (ex-info "cancelled" {:type :export-cancelled}))
    (let [{:keys [promise cancel]}
          (dea/run-exports (cond-> {:page page
                                    :files files
                                    :bridge (tab-bridge)
                                    :on-progress #(st/emit! (progress %))}
                             ;; the pictures come from WASM
                             (:wasm? params) (assoc :on-frame show-frame!))
                           (select-keys params (into [:format :quality :fps :loop? :audio?] (keys job/svg-options)))
                           (job-boards params)
                           (get-in files [(:file-id params) :name]))]
      (swap! live assoc :cancel cancel)
      promise)))

(defn- start-export
  "Run the export of the job on the file loaded (see `run-job`)."
  []
  (ptk/reify ::start-export
    ptk/UpdateEvent
    (update [_ state]
      (update state :export-tab assoc :status :exporting :percent 0))

    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [params page files]}     (tab-state state)
            {:keys [format board-id wasm?]} params]
        (rx/concat
         ;; as the export in the workspace tells it
         (rx/of (ev/event {::ev/name "export-animation"
                           ::ev/origin "export-tab"
                           :format (name format)
                           :in-tab true}))
         ;; WASM when the workspace renders with it: a Lottie may have
         ;; layers that are no vectors, taken as pictures
         (->> (rx/from
               (if wasm?
                 (-> ((:start-wasm @live) (:objects page) board-id)
                     (p/then #(run-job params page files)))
                 (run-job params page files)))
              (rx/map finished)
              (rx/catch
               (fn [cause]
                 (case (:type (ex-data cause))
                   :export-cancelled  (rx/of (cancelled))
                   :wasm-unavailable  (rx/of (failed :wasm (ex-message cause)))
                   (rx/of (failed :export (ex-message cause))))))))))))

(defn- bundle-loaded
  [{:keys [file libraries]}]
  (ptk/reify ::bundle-loaded
    ptk/UpdateEvent
    (update [_ state]
      (let [{:keys [page-id]} (:params (tab-state state))]
        (-> state
            (assoc :current-file-id (:id file))
            (update :export-tab assoc
                    :page (get-in file [:data :pages-index page-id])
                    :files (-> (d/index-by :id libraries)
                               (assoc (:id file) file))))))

    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [status page]} (tab-state state)]
        (cond
          ;; cancelled meanwhile
          (not= :loading status) (rx/empty)
          (some? page)           (rx/of (start-export))
          :else                  (rx/of (failed :load "page not found")))))))

(defn- saved
  "The workspace has saved: the file loads (as the view mode loads it,
  without what the view mode does with it)."
  []
  (ptk/reify ::saved
    ptk/UpdateEvent
    (update [_ state]
      (cond-> state
        (#{:waiting :stale} (:status (tab-state state)))
        (assoc-in [:export-tab :status] :loading)))

    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [status params]} (tab-state state)]
        (if (= :loading status)
          (->> (dv/load-bundle {:file-id (:file-id params)})
               (rx/mapcat (fn [{:keys [fonts team] :as bundle}]
                            (rx/of (df/fonts-fetched fonts)
                                   (features/initialize (:features team))
                                   (bundle-loaded bundle))))
               (rx/catch (fn [cause] (rx/of (failed :load (ex-message cause))))))
          (rx/empty))))))

(def ^:private before-export?
  #{:waiting :loading :stale})

(defn cancel
  "Cancel the export of the tab, from its Cancel or from the workspace:
  at its next frame, or at once when it has not started yet."
  []
  (ptk/reify ::cancel
    ptk/UpdateEvent
    (update [_ state]
      (case (:status (tab-state state))
        :exporting (assoc-in state [:export-tab :cancelling?] true)
        state))

    ptk/WatchEvent
    (watch [_ state _]
      (if (before-export? (:status (tab-state state)))
        (rx/of (cancelled))
        (rx/empty)))

    ptk/EffectEvent
    (effect [_ _ _]
      ;; kept for an export not started yet (WASM starting)
      (swap! live assoc :cancel-requested? true)
      (when-let [cancel (:cancel @live)]
        (cancel)))))

(defn- received
  "A message of the workspace. A `:saved` that comes again (the workspace
  answers every `:hello`) loads nothing more."
  [{:keys [type]}]
  (ptk/reify ::received
    ptk/WatchEvent
    (watch [_ state _]
      (case type
        :saved       (if (#{:waiting :stale} (:status (tab-state state)))
                       (rx/of (saved))
                       (rx/empty))
        :save-failed (rx/of (failed :save-failed))
        :cancel      (rx/of (cancel))
        (rx/empty)))))

(defn- lock-held
  "The tab holds the lock of its job: it says it is there until the
  workspace has saved (or it is cancelled), and gives up after
  `save-wait-ms`."
  []
  (ptk/reify ::lock-held
    ptk/WatchEvent
    (watch [_ _ stream]
      (let [answered (rx/filter #(contains? #{::saved ::failed ::cancelled} (ptk/type %)) stream)]
        (rx/merge
         (->> (rx/timer 0 hello-interval-ms)
              (rx/take-until answered)
              (rx/tap (fn [_] (post! {:type :hello})))
              (rx/ignore))
         (->> (rx/timer (:save-wait-ms @live))
              (rx/take-until answered)
              (rx/map (fn [_] (stale)))))))))

(defn- duplicate
  "Another tab holds the lock of the job: it exports, this one does not."
  []
  (ptk/reify ::duplicate
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:export-tab :status] :duplicate))))

(defn- listen!
  "Tell the workspace when the tab goes (`pagehide`) and when the browser
  freezes it while hidden, and resumes it."
  []
  (if (and (exists? js/window) (exists? js/document))
    (let [on-hide   #(post! {:type :closed})
          on-freeze #(post! {:type :paused})
          on-resume #(post! {:type :resumed})]
      (.addEventListener js/window "pagehide" on-hide)
      (.addEventListener js/document "freeze" on-freeze)
      (.addEventListener js/document "resume" on-resume)
      (fn []
        (.removeEventListener js/window "pagehide" on-hide)
        (.removeEventListener js/document "freeze" on-freeze)
        (.removeEventListener js/document "resume" on-resume)))
    (constantly nil)))

(defn- open-channel!
  "Open the channel of the job `job-id` and tell the workspace when the
  tab goes or freezes (see `listen!`)."
  [job-id channel-ctor]
  (let [on-message #(st/emit! (received %))
        channel    (job/open-channel job-id on-message (or channel-ctor (job/default-channel-ctor)))]
    (swap! live assoc :channel channel :listeners (listen!))))

(defn initialize
  "Start the export tab of the job of the query params `query` (see
  `job/decode-params`): it takes the lock of the job, then opens the
  channel of the job and says it is there until the workspace has saved.
  When another tab holds the lock, that tab exports and this one ends
  (`:duplicate`); params that are not valid end it at once.
  `channel-ctor`, `locks`, `start-wasm` and `save-wait-ms` replace
  `BroadcastChannel`, `navigator.locks`, `start-wasm!` and
  `save-wait-ms` (in tests)."
  [query & {:keys [channel-ctor locks start-wasm] :as opts}]
  (ptk/reify ::initialize
    ptk/UpdateEvent
    (update [_ state]
      (let [params (job/decode-params query)]
        (assoc state :export-tab (if (some? params)
                                   {:status :waiting :params params}
                                   {:status :invalid}))))

    ptk/EffectEvent
    (effect [_ state _]
      (let [{:keys [status params]} (tab-state state)]
        (when (= :waiting status)
          (let [job-id  (:job-id params)
                release (p/deferred)]
            (reset! live {:release release
                          :start-wasm (or start-wasm start-wasm!)
                          :save-wait-ms (get opts :save-wait-ms save-wait-ms)})
            (-> (job/hold-lock job-id release (or locks (job/default-locks)))
                (p/then (fn [held?]
                          ;; unless the tab closed meanwhile
                          (when (identical? release (:release @live))
                            (if held?
                              (do (open-channel! job-id channel-ctor)
                                  (st/emit! (lock-held)))
                              (st/emit! (duplicate)))))))))))))

(defn download-again
  "Download the file of the export once more."
  []
  (ptk/reify ::download-again
    ptk/EffectEvent
    (effect [_ _ _]
      (when-let [result (:result @live)]
        (dea/download-result result)))))

(defn finalize
  "The tab closes: the channel closes, the lock goes, and the preview of
  the file with it."
  []
  (ptk/reify ::finalize
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state :export-tab))

    ptk/EffectEvent
    (effect [_ _ _]
      (when-let [{:keys [channel release listeners preview-url]} @live]
        (when listeners (listeners))
        (when channel ((:close channel)))
        (when release (p/resolve release nil))
        (when preview-url (wapi/revoke-uri preview-url)))
      (reset! live nil))))
