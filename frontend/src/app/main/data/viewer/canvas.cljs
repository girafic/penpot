;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.viewer.canvas
  "The canvas of a phone: the page of a file in the view mode as a whole,
  to move about and zoom, where a tap opens a board as the prototype.
  The route says whether it shows (`canvas`): absent, the view mode
  decides as it starts (`starts-on-canvas?`); \"true\", the canvas;
  \"false\", the prototype, chosen on this phone, so that a reload stays
  on its board. Pure: the events that go there live in
  `app.main.data.viewer`."
  (:require
   [app.main.router :as rt]))

(defn- param
  [params k]
  (rt/get-query-param params k))

(defn- interactions?
  "Whether the route `params` show the Interactions section, where the
  view mode starts."
  [params]
  (contains? #{nil "interactions"} (some-> (param params :section) name)))

(defn canvas-shown?
  "Whether a view mode that can show the canvas (`able?`: on a phone,
  drawing with WASM, see `app.main.data.viewer`) shows it at the route
  `params`: in Interactions, `canvas` \"true\" and no board."
  [able? params]
  (and (true? able?)
       (interactions? params)
       (= "true" (param params :canvas))
       (nil? (param params :index))))

(defn starts-on-canvas?
  "Whether a view mode that can show the canvas starts on it at the
  route `params`: in Interactions, neither live (`follow`) nor the
  preview of the workspace, not chosen yet on this phone (no `canvas`),
  and its link points at no board: no `frame-id`, and no `index` but a
  share link's \"0\", which the share dialog writes into each link."
  [params]
  (let [index (param params :index)]
    (and (interactions? params)
         (not= "true" (param params :follow))
         (not= "true" (param params :preview))
         (nil? (param params :canvas))
         (nil? (param params :frame-id))
         (or (nil? index)
             (and (some? (param params :share-id))
                  (= "0" (str index)))))))

(defn canvas-params
  "The route `params` showing the canvas: no board, in Interactions."
  [params]
  (-> params
      (assoc :canvas "true")
      (dissoc :index :frame-id :section)))

(defn board-params
  "The route `params` showing the board at `index`: off the canvas when
  it showed, the prototype then chosen on this phone."
  [params index]
  (cond-> (assoc params :index index)
    (= "true" (param params :canvas))
    (assoc :canvas "false")))
