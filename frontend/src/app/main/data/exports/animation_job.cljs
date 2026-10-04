;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.exports.animation-job
  "The job of an animated export run in a tab of its own (see
  `app.main.ui.export-animation`): the settings the tab gets in its URL,
  the messages the workspace and the tab exchange on the channel of the
  job, and the lock the tab holds while it lives, by which the workspace
  knows when it is gone."
  (:require
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.schema :as sm]
   [app.common.transit :as t]
   [app.common.uuid :as uuid]))

(def formats
  #{:mp4 :webm :gif :avif :svg :lottie :dotlottie})

(def ^:private qualities
  #{:low :medium :high})

(def schema:params
  "The settings of a job, as the tab reads them from its URL."
  [:map {:title "ExportAnimationJob"}
   [:file-id ::sm/uuid]
   [:page-id ::sm/uuid]
   [:board-id ::sm/uuid]
   [:job-id ::sm/uuid]
   [:format (into [:enum] formats)]
   [:quality (into [:enum] qualities)]
   [:fps [::sm/int {:min 1 :max 120}]]
   ;; a preset that fits a height or a width may scale a small board up a lot
   [:scale [::sm/number {:min 0.001}]]
   [:loop? :boolean]
   [:audio? :boolean]
   [:wasm? :boolean]])

(def ^:private valid-params?
  (sm/lazy-validator schema:params))

(defn needs-wasm?
  "Whether `format` renders pictures through WASM: all but Lottie, which
  draws vector layers as they are (see `dea/export-formats`)."
  [format]
  (not= :lottie format))

(defn new-job-id
  []
  (uuid/next))

(defn encode-params
  "The query params of the tab of the job `params` (see `schema:params`),
  the renderer the workspace uses as `wasm`, which the app honours (see
  `app.main.features/setup-wasm-features`)."
  [{:keys [file-id page-id board-id job-id format quality fps scale loop? audio? wasm?]}]
  {:file-id (str file-id)
   :page-id (str page-id)
   :board-id (str board-id)
   :job-id (str job-id)
   :format (name format)
   :quality (name quality)
   :fps (str fps)
   :scale (str scale)
   :loop (str (boolean loop?))
   :audio (str (boolean audio?))
   :wasm (str (boolean wasm?))})

(defn decode-params
  "The settings of a job from the query params of its tab (strings), or
  nil when they are not valid: a format the renderer cannot draw (a
  picture without WASM) is not."
  [{:keys [file-id page-id board-id job-id format quality fps scale loop audio wasm]}]
  (let [params {:file-id (uuid/parse* file-id)
                :page-id (uuid/parse* page-id)
                :board-id (uuid/parse* board-id)
                :job-id (uuid/parse* job-id)
                :format (some-> format keyword)
                :quality (some-> quality keyword)
                :fps (d/parse-integer fps)
                :scale (d/parse-double scale)
                :loop? (= "true" loop)
                :audio? (= "true" audio)
                :wasm? (= "true" wasm)}]
    (when (and (valid-params? params)
               (or (:wasm? params) (not (needs-wasm? (:format params)))))
      params)))

;; MESSAGES

(def ^:private message-types
  #{;; tab -> workspace
    :hello :progress :paused :resumed :done :error :cancelled :closed
    ;; workspace -> tab
    :saved :save-failed :cancel})

(defn encode-message
  [message]
  (t/encode-str message))

(defn decode-message
  "The message of `data`, nil when it is none (another sender, a later
  version)."
  [data]
  (let [message (when (string? data) (ex/ignoring (t/decode-str data)))]
    (when (and (map? message) (contains? message-types (:type message)))
      message)))

(defn error-text
  "The text of the `:error` message of a tab (`:reason` `:save-failed`
  `:load` `:wasm` `:unavailable` `:nothing` `:export`, `:message`), as
  `[key & args]` for `tr`."
  [{:keys [reason message]}]
  (case reason
    :save-failed ["export-animation.error.save-failed"]
    :load        ["export-animation.error.load"]
    :wasm        ["export-animation.error.wasm"]
    :unavailable ["workspace.options.export.codec-unavailable"]
    :nothing     ["export-animation.error.nothing"]
    ["export-animation.error.export" (or message "")]))

(defn channel-name
  [job-id]
  (str "penpot-export-" job-id))

(defn open-channel
  "The channel of the job `job-id`: `{:post :close}`, `on-message`
  hearing each message the other side posts (see `decode-message`).
  `ctor` replaces `BroadcastChannel` (in tests); nil without one."
  ([job-id on-message]
   (open-channel job-id on-message (unchecked-get js/globalThis "BroadcastChannel")))
  ([job-id on-message ctor]
   (when (some? ctor)
     (let [^js channel (new ctor (channel-name job-id))]
       (set! (.-onmessage channel)
             (fn [^js event]
               (when-let [message (decode-message (.-data event))]
                 (on-message message))))
       {:post  (fn [message] (.postMessage channel (encode-message message)))
        :close (fn [] (.close channel))}))))

;; THE LOCK OF THE TAB

(defn lock-name
  [job-id]
  (str "penpot-export-" job-id))

(defn- default-locks
  []
  (some-> (unchecked-get js/globalThis "navigator") (unchecked-get "locks")))

(defn hold-lock
  "Hold the lock of the job `job-id` until the promise `release`
  resolves, as the tab does while it lives (the browser lets it go when
  the tab closes or crashes): a promise resolved once it is held. `locks`
  replaces `navigator.locks`; without one, resolved at once."
  ([job-id release]
   (hold-lock job-id release (default-locks)))
  ([job-id release ^js locks]
   (if (some? locks)
     (js/Promise.
      (fn [resolve _]
        (.request locks (lock-name job-id)
                  (fn [_]
                    (resolve nil)
                    release))))
     (js/Promise.resolve nil))))

(defn when-released
  "A promise resolved once nobody holds the lock of the job `job-id`: the
  tab that held it is gone. `locks` replaces `navigator.locks`; without
  one, never."
  ([job-id]
   (when-released job-id (default-locks)))
  ([job-id ^js locks]
   (if (some? locks)
     (.request locks (lock-name job-id) (fn [_] nil))
     (js/Promise. (fn [_ _])))))
