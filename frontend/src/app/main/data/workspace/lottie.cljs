;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.lottie
  "Import of Lottie animations, `.json` files and dotLottie `.lottie`
  archives: a board with the layers of the animation and its timeline
  (see `app.common.files.lottie`), as one undo step."
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.lottie :as lottie]
   [app.common.json :as json]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.main.repo :as rp]
   [app.util.i18n :refer [tr]]
   [app.util.webapi :as wapi]
   [app.util.zip :as uz]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(def accept
  "The files the Lottie import takes, for a file picker."
  ".json,.lottie,application/json")

(defn lottie-file?
  "Whether `blob`, a dropped or picked file, is a Lottie animation: a
  `.json` or a dotLottie `.lottie` file."
  [^js blob]
  (let [name (str/lower (or (.-name blob) ""))]
    (or (str/ends-with? name ".lottie")
        (str/ends-with? name ".json"))))

(defn- file-name
  [^js blob]
  (-> (or (.-name blob) "Lottie")
      (str/replace #"(?i)\.(json|lottie)$" "")))

(def ^:private image-types
  {"png" "image/png" "jpg" "image/jpeg" "jpeg" "image/jpeg"
   "webp" "image/webp" "gif" "image/gif" "svg" "image/svg+xml"})

(defn- read-entry-blob
  "A promise of the file `entry` of an archive as a blob, of the image
  type its extension says (the media upload checks it)."
  [^js entry]
  (let [ext (str/lower (or (last (str/split (.-filename entry) #"\.")) ""))]
    (.getData entry (uz/blob-writer :mtype (get image-types ext "application/octet-stream")))))

(defn- read-dotlottie
  "The animation of the dotLottie archive `blob` (the first its manifest
  lists) and its files by path, as a promise of `{:doc :files}`."
  [blob]
  (p/let [reader   (uz/reader blob)
          entries  (uz/get-entries reader)
          entries  (vec entries)
          by-path  (into {} (map (fn [^js e] [(.-filename e) e])) entries)
          manifest (when-let [entry (get by-path "manifest.json")]
                     (p/-> (uz/read-as-text entry) (json/decode :key-fn keyword)))
          anim-id  (-> manifest :animations first :id)
          entry    (or (get by-path (str "animations/" anim-id ".json"))
                       (get by-path (str "a/" anim-id ".json"))
                       (d/seek #(re-find #"^(animations|a)/[^/]+\.json$" (.-filename ^js %)) entries))
          text     (uz/read-as-text entry)]
    ;; left open: the images are read from it afterwards
    {:doc   (json/decode text :key-fn keyword)
     :files by-path}))

(defn- read-lottie
  "A promise of the animation of `blob`: `{:doc :files}`, `:files` the
  entries of a dotLottie archive by path."
  [^js blob]
  (if (str/ends-with? (str/lower (or (.-name blob) "")) ".lottie")
    (read-dotlottie blob)
    (p/let [text (.text blob)]
      {:doc (json/decode text :key-fn keyword) :files {}})))

(defn- image-blob
  "A promise of the blob of the image asset `image` (see `lottie/images`):
  its data URI or its file in the archive `files`."
  [{:keys [data-uri path]} files]
  (if (some? data-uri)
    (p/resolved (wapi/data-uri->blob data-uri))
    (let [path  (str/replace (or path "") #"^/+" "")
          base  (last (str/split path #"/"))
          entry (or (get files path)
                    (get files (str "images/" base))
                    (get files (str "i/" base)))]
      (if (some? entry)
        (read-entry-blob entry)
        (p/rejected (ex-info "missing image" {:path path}))))))

(defn- upload-images
  "Upload the images `doc` needs to the file `file-id`: a stream of the
  map of their asset ids to the media objects, with their Lottie size."
  [file-id doc files]
  (let [images (lottie/images doc)]
    (if (empty? images)
      (rx/of {})
      (->> (rx/from images)
           (rx/mapcat
            (fn [{:keys [id w h] :as image}]
              (->> (rx/from (image-blob image files))
                   (rx/mapcat #(rp/cmd! :upload-file-media-object
                                        {:file-id file-id :name id :is-local true :content %}))
                   (rx/map #(vector id (assoc % :w w :h h)))
                   ;; an image that cannot be read is left out
                   (rx/catch (fn [cause]
                               (js/console.warn "Lottie image not imported" id cause)
                               (rx/empty))))))
           (rx/reduce conj {})))))

(defn- skipped-message
  [skipped]
  (tr "workspace.lottie.imported-without"
      (->> (keys skipped)
           (map #(tr (str "workspace.lottie.feature." (name %))))
           (sort)
           (str/join ", "))))

(defn- lottie-imported
  "Add the board of the Lottie document `doc` centred on `position`, with
  its timeline, and select it."
  [doc images name position]
  (ptk/reify ::lottie-imported
    ptk/WatchEvent
    (watch [it state _]
      (let [page    (dsh/lookup-page state)
            size    #(let [v (get doc %)] (if (number? v) v 0))
            {:keys [shapes timeline skipped]}
            (lottie/lottie->shapes doc {:name name
                                        :x (- (:x position) (/ (size :w) 2))
                                        :y (- (:y position) (/ (size :h) 2))
                                        :images images})
            board-id (:id (first shapes))
            changes  (as-> (-> (pcb/empty-changes it (:id page))
                               (pcb/with-page page)
                               (pcb/with-objects (:objects page))) changes
                       (reduce pcb/add-object changes shapes)
                       (pcb/change-timeline changes board-id timeline))
            undo-id  (js/Symbol)]
        (rx/concat
         (rx/of (dwu/start-undo-transaction undo-id)
                (dch/commit-changes changes)
                (dws/select-shapes (d/ordered-set board-id))
                (dwu/commit-undo-transaction undo-id))
         (if (seq skipped)
           (rx/of (ntf/warn (skipped-message skipped)))
           (rx/empty)))))))

(defn import-lottie
  "Import the Lottie animation of the file `blob` (see `lottie-file?`) as
  a board centred on `position`, with its timeline."
  [blob position]
  (ptk/reify ::import-lottie
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)]
        (->> (rx/from (read-lottie blob))
             (rx/mapcat (fn [{:keys [doc files]}]
                          (when-not (and (map? doc) (sequential? (:layers doc)))
                            (throw (ex-info "not a Lottie animation" {})))
                          (->> (upload-images file-id doc files)
                               (rx/map #(lottie-imported doc % (file-name blob) position)))))
             (rx/catch (fn [cause]
                         (js/console.error cause)
                         (rx/of (ntf/error (tr "workspace.lottie.import-error"))))))))))
