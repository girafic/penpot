;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.audio
  "Audio files added to the timelines of Penpot Motion: decoded in the
  browser (which checks them and gives their duration and waveform),
  uploaded as file media objects and played by audio clips of a board
  (see `app.common.types.audio-clip`)."
  (:require
   [app.common.files.helpers :as cfh]
   [app.common.logic.timelines :as cltl]
   [app.common.math :as mth]
   [app.common.media :as cm]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.media :as dwm]
   [app.main.data.workspace.modifiers :as dwmo]
   [app.util.audio :as audio]
   [app.util.clipboard :as clipboard]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

(def ^:private extension-types
  "The canonical media types of audio files by their extension, for the
  files a browser gives no type or the type of a video."
  {"mp3"  "audio/mpeg"
   "wav"  "audio/wav"
   "wave" "audio/wav"
   "ogg"  "audio/ogg"
   "oga"  "audio/ogg"
   "opus" "audio/ogg"
   "m4a"  "audio/mp4"
   "mp4"  "audio/mp4"
   "webm" "audio/webm"
   "weba" "audio/webm"
   "flac" "audio/flac"})

(defn audio-type
  "The canonical media type of the audio `file` (a `js/File`), by its type
  or else its extension; nil when it is no audio file a clip plays."
  [^js file]
  (or (cm/canonical-audio-type (.-type file))
      (when-let [name (not-empty (.-name file))]
        (when (str/includes? name ".")
          (get extension-types (str/lower (last (str/split name "."))))))))

(defn audio-file?
  [file]
  (some? (audio-type file)))

(def ^:private max-new-duration
  "The longest a timeline a drop of audio creates lasts, in ms: as long
  as an export at 30 fps can be (see `cta/max-export-frames`)."
  (* 1000 (/ cta/max-export-frames 30)))

(defn prepare
  "A promise of what adding the audio `file` takes, `{:file :name :buffer
  :duration}` (the file typed as it is stored, its decoded `AudioBuffer`
  and duration in ms), or `{:error :name}` when it is no audio file a
  clip plays (`:unsupported`), the browser cannot decode it
  (`:undecodable`) or it lasts longer than `ctau/max-audio-duration`
  (`:too-long`). The plugin API adds audio through it too."
  [^js file decode]
  (let [name  (.-name file)
        mtype (audio-type file)]
    (if (nil? mtype)
      (p/resolved {:error :unsupported :name name})
      (-> (.arrayBuffer file)
          (p/then decode)
          (p/then (fn [^js buffer]
                    (let [duration (js/Math.round (* 1000 (.-duration buffer)))]
                      (cond
                        (> duration ctau/max-audio-duration)
                        {:error :too-long :name name}

                        (< duration ctau/min-clip-duration)
                        {:error :undecodable :name name}

                        :else
                        {:file (js/File. #js [file] name #js {:type mtype})
                         :name name
                         :buffer buffer
                         :duration duration}))))
          (p/catch (fn [_] {:error :undecodable :name name}))))))

(defn- error-notification
  [{:keys [error name]}]
  (case error
    :unsupported (ntf/error (tr "workspace.audio.unsupported" name))
    :undecodable (ntf/error (tr "workspace.audio.undecodable" name))
    :too-long    (ntf/error (tr "workspace.audio.too-long" name (/ ctau/max-audio-duration 60000)))
    nil))

(defn- new-clips
  "The clips of the `added` files, each with its `:audio-id`, one after
  the other from `time`, on `lane` when given (else on a free one, see
  `ctau/add-audio-clip`), each without its first `trim` ms (from a
  recording, the pre-roll before where it goes, see
  `app.main.data.workspace.audio-recording`)."
  [added time lane trim]
  (first
   (reduce (fn [[clips start] {:keys [audio-id name duration]}]
             (let [offset (min (or trim 0) (- duration ctau/min-clip-duration))]
               [(conj clips (cond-> {:id (uuid/next)
                                     :audio-id audio-id
                                     :name name
                                     :source-duration duration
                                     :start start
                                     :offset offset
                                     :duration (- duration offset)}
                              (some? lane) (assoc :lane lane)))
                (+ start (- duration offset))]))
           [[] (js/Math.round time)]
           added)))

(defn- with-clips
  "`timeline` with `clips`. Its duration stays, but for a timeline the
  board did not have (not `stored?`): that one lasts until they end, at
  most `max-new-duration`."
  [timeline clips stored?]
  (let [timeline (reduce ctau/add-audio-clip timeline clips)]
    (if stored?
      timeline
      (assoc timeline :duration (min max-new-duration (reduce max 1 (map ctau/clip-end clips)))))))

(defn- commit-clips
  "Add the clips of the `added` files to the timeline of the board
  `board-id` of the page `page-id`, as `add-audio-files` says, in one
  change, whatever page shows meanwhile: nothing but a notice when the
  board, or its page, is no longer there."
  [page-id board-id added time lane trim]
  (ptk/reify ::commit-clips
    ptk/WatchEvent
    (watch [it state _]
      (let [page    (when (some? page-id) (dsh/lookup-page state page-id))
            objects (:objects page)]
        (if (or (nil? page)
                (not (contains? objects board-id))
                (not (cfh/root-frame? objects board-id)))
          (rx/of (ntf/error (tr "workspace.audio.board-gone")))
          (let [timelines (:timelines page)
                timeline  (-> (dwa/board-timeline timelines (get objects board-id))
                              (with-clips (new-clips added time lane trim) (contains? timelines board-id)))]
            (rx/of (dwa/commit-page-timeline it state page-id board-id timeline))))))))

(defn- upload-error
  [cause]
  (if (= :media-type-not-allowed (:code (ex-data cause)))
    ;; the server takes audio only with Motion (`enable-feature-animation`)
    (rx/of (ntf/error (tr "workspace.audio.upload-not-enabled")))
    (dwm/handle-media-error cause)))

(defn upload
  "Upload the audio `file` of what `prepare` gives as a file media object
  of `file-id`: an observable of it."
  [file-id {:keys [file name]}]
  (dwm/process-blobs {:file-id file-id
                      :local? true
                      :name name
                      :blobs [file]
                      :force-media true
                      :allowed-types cm/audio-canonical-types
                      :on-image (constantly nil)
                      :on-svg (constantly nil)}))

(defn add-audio-files
  "Add the audio `files` (`js/File`) to the timeline of the board
  `board-id` as clips, one after the other from `time` ms, on `lane` (on
  a free one when nil). Each file is decoded first: a notice for one
  that is no audio, does not decode or lasts longer than
  `ctau/max-audio-duration`, which is not uploaded. Once every upload is
  done, all the clips go in one change, one undo. The duration of the
  timeline stays (see `with-clips`), on its page, the current one or
  `:page-id`, whatever page shows once they are uploaded. `:trim` leaves
  out the first ms of each (the pre-roll of a recording). `:decode`
  replaces `audio/decode` in tests."
  [board-id files time lane & {:keys [decode trim page-id]}]
  (ptk/reify ::add-audio-files
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)
            page-id (or page-id (:current-page-id state))
            decode  (or decode audio/decode)]
        (->> (rx/from (p/all (map #(prepare % decode) files)))
             (rx/mapcat
              (fn [prepared]
                (let [ready (remove :error prepared)]
                  (rx/concat
                   (rx/from (keep error-notification prepared))
                   (if (empty? ready)
                     (rx/empty)
                     (rx/concat
                      (rx/of (ntf/show {:content (tr "media.loading")
                                        :type :toast
                                        :level :info
                                        :timeout nil
                                        :tag ::uploading}))
                      (->> (rx/from ready)
                           (rx/mapcat (fn [{:keys [buffer] :as item}]
                                        (->> (upload file-id item)
                                             (rx/map (fn [{:keys [id]}]
                                                       (audio/remember! id buffer)
                                                       (assoc item :audio-id id))))))
                           (rx/reduce conj [])
                           (rx/map #(commit-clips page-id board-id % time lane trim))
                           (rx/catch upload-error))
                      ;; the notice of an error has taken its place
                      (rx/of (ntf/hide :tag ::uploading)))))))))))))

(defn drop-audio-files
  "Add the audio `files` dropped on the canvas at `position`: to the
  top-level board under it, else to the active board, at the playhead of
  that board; only a notice without one. `opts` go to `add-audio-files`."
  [files position & {:as opts}]
  (ptk/reify ::drop-audio-files
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects (dsh/lookup-page-objects state)
            frame   (ctst/top-nested-frame objects position)
            board   (or (when (and (some? frame) (not= uuid/zero frame))
                          (cfh/get-shape-id-root-frame objects frame))
                        (dwa/active-board-id state))]
        (if (nil? board)
          (rx/of (ntf/error (tr "workspace.audio.drop-on-a-board")))
          (rx/of (add-audio-files board files (dwa/board-playhead state board) nil opts)))))))

;; CLIPS IN THE DOCK

(defn audio-rows
  "The rows of the audio clips of `timeline` in the timeline dock, after
  the rows of the layers, none without clips: a group row, `{:type
  :audio-group :collapsed? :count :clips :muted? :locked?}` (`:count`
  its lanes, `:clips` all, by start, `:muted?` and `:locked?` when all
  are), then, unless `collapsed?`, one by lane holding clips, in order,
  as `{:type :audio :lane :number :name :clips}`, `:number` counting them
  from 1 (whatever lanes are empty between them), `:name` the one of the
  file the lane plays (see `ctau/lane-takes?`; the ones of all, in a
  file from before a lane played one), its clips by start (the later
  ones draw on top)."
  [timeline & {:keys [collapsed?]}]
  (let [clips (vals (:audio-clips timeline))
        lanes (->> clips
                   (group-by :lane)
                   (sort-by key)
                   (map-indexed (fn [i [lane clips]]
                                  (let [clips (vec (sort-by :start clips))]
                                    {:type :audio
                                     :lane lane
                                     :number (inc i)
                                     :name (str/join ", " (distinct (keep :name clips)))
                                     :clips clips})))
                   (vec))]
    (if (empty? lanes)
      []
      (into [{:type :audio-group
              :collapsed? (boolean collapsed?)
              :count (count lanes)
              :clips (vec (sort-by :start clips))
              :muted? (every? :muted clips)
              :locked? (every? :locked clips)}]
            (when-not collapsed? lanes)))))

(defn toggle-audio-collapsed
  "Show the rows of the audio lanes in the dock, or only their group."
  []
  (ptk/reify ::toggle-audio-collapsed
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:workspace-animation :audio-collapsed?] not))))

(defn lane-after
  "The lane after the ones of the clips of `timeline` but `clip-id`: where
  that clip goes, on its own, dragged under the audio rows (so lanes put
  together come apart again)."
  [timeline clip-id]
  (inc (reduce max -1 (keep #(when (not= clip-id (:id %)) (:lane %))
                            (vals (:audio-clips timeline))))))

(defn selected-clips
  "The ids of the clips selected in the dock, a set, from the
  `:workspace-animation` state `anim` (see `select-clip`)."
  [anim]
  (or (:selected-clips anim)
      (some-> (:selected-clip anim) hash-set)
      #{}))

(defn- with-clip-selection
  "`anim` with the clips `ids` selected and `primary`, when one of them,
  the one the clip bar shows (else any); with `alone?`, the selected
  keyframes and preset animations let go of."
  [anim ids primary alone?]
  (let [ids  (set ids)
        anim (cond-> anim
               alone? (dissoc :selected-kfs :selected-kf :selected-anims))]
    (if (empty? ids)
      (dissoc anim :selected-clip :selected-clips)
      (assoc anim
             :selected-clips ids
             :selected-clip (if (contains? ids primary) primary (first ids))))))

(defn select-clip
  "Select the audio clip `clip-id` of the timeline in the dock alone (nil
  for none), letting go of the selected keyframes: `:selected-clips`
  holds the selected clips, `:selected-clip` the one clicked last."
  [clip-id]
  (ptk/reify ::select-clip
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation with-clip-selection (when (some? clip-id) [clip-id]) clip-id true))))

(defn toggle-clip
  "Add the clip `clip-id` to the selected clips, or take it out of them, as
  Shift or Cmd/Ctrl + click does."
  [clip-id]
  (ptk/reify ::toggle-clip
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation
              (fn [anim]
                (let [ids (selected-clips anim)]
                  (with-clip-selection anim
                    (if (contains? ids clip-id) (disj ids clip-id) (conj ids clip-id))
                    clip-id
                    false)))))))

(defn select-clips
  "Select the clips `clip-ids`, as a box drawn over them in the dock does."
  [clip-ids]
  (ptk/reify ::select-clips
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation with-clip-selection clip-ids nil true))))

(defn- selection
  [state]
  (selected-clips (:workspace-animation state)))

(declare drag-clips)

;; ALL THAT IS SELECTED IN THE DOCK
;;
;; Keyframes (`:selected-kfs`, see `dwa/select-keyframe`), preset
;; animations (`:selected-anims`, see `dwa/select-animation-block`) and
;; audio clips (`:selected-clips`) are selected together: a box or Shift
;; + click picks any of them, and they move, nudge and go together.

(defn selection-of
  "What is selected in the dock, from the `:workspace-animation` state
  `anim`: `{:kfs :anims :clips}`, sets of `{:shape-id :keyframe-id}`,
  of `{:shape-id :animation-id}` and of clip ids."
  [anim]
  {:kfs   (or (:selected-kfs anim) #{})
   :anims (or (:selected-anims anim) #{})
   :clips (selected-clips anim)})

(defn selection-count
  [{:keys [kfs anims clips]}]
  (+ (count kfs) (count anims) (count clips)))

(defn select-items
  "Select the keyframes `kfs`, the preset animations `anims` and the clips
  `clips` (see `selection-of`), all that was selected let go of, as a box
  in the dock does."
  [{:keys [kfs anims clips]}]
  (ptk/reify ::select-items
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-animation
              (fn [anim]
                (let [kfs   (set kfs)
                      anims (set anims)]
                  (cond-> (-> anim
                              (assoc :selected-kfs kfs
                                     :selected-kf (when (= 1 (count kfs)) (first kfs)))
                              (with-clip-selection clips nil false))
                    (seq anims)  (assoc :selected-anims anims)
                    (empty? anims) (dissoc :selected-anims))))))))

(defn copy-selection
  "Copy the keyframes (hidden or not) and the audio clips selected in the
  dock to the clipboard as one animation clip (see
  `dwa/write-animation-clip`), which pastes them together, at the same
  distance."
  []
  (ptk/reify ::copy-selection
    ptk/WatchEvent
    (watch [_ state _]
      (let [ids      (selected-clips (:workspace-animation state))
            timeline (dwa/current-timeline state)
            audio    (when (and (some? timeline) (seq ids))
                       (cltl/audio-copy timeline ids))
            clip     (cond-> (dwa/selected-keyframes-clip state)
                       (seq audio) (assoc :board-id (:board-id timeline) :audio audio))]
        (when (or (seq (:layers clip)) (seq (:audio clip)))
          (rx/of (dwa/write-animation-clip clip)))))))

(defn cut-selection
  "Copy the keyframes and the audio clips selected in the dock (see
  `copy-selection`) and delete them but the locked clips, which stay
  selected, in one undo step. The selected preset animations stay."
  []
  (ptk/reify ::cut-selection
    ptk/WatchEvent
    (watch [it state _]
      (let [{:keys [kfs clips]} (selection-of (:workspace-animation state))
            timeline (dwa/current-timeline state)]
        (if (nil? timeline)
          (rx/empty)
          (let [locked    (into #{} (filter #(:locked (ctau/get-audio-clip timeline %))) clips)
                timeline' (as-> timeline tl
                            (reduce (fn [tl {:keys [shape-id keyframe-id]}]
                                      (cta/remove-keyframe tl shape-id keyframe-id))
                                    tl kfs)
                            (reduce ctau/remove-audio-clip tl (remove locked clips)))]
            (rx/concat
             (rx/of (copy-selection)
                    (select-items {:clips locked}))
             (if (= timeline timeline')
               (rx/empty)
               (rx/of (dwa/commit-timeline it state (:board-id timeline') timeline')
                      (dwmo/clear-local-transform)
                      (dwa/apply-preview))))))))))

(defn paste-clipboard-at-playhead
  "Paste the animation on the clipboard at the playhead, as Cmd/Ctrl+V in
  the dock does (see `dwa/paste-clip-at-playhead`)."
  []
  (ptk/reify ::paste-clipboard-at-playhead
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rx/from (clipboard/read-text))
           (rx/map (fn [text]
                     (if-let [clip (dwa/payload-clip text)]
                       (dwa/paste-clip-at-playhead clip)
                       (ntf/warn (tr "workspace.animation.paste-invalid")))))
           (rx/catch (fn [cause]
                       (rx/of (ntf/warn (or (clipboard/error-message cause)
                                            (tr "workspace.animation.paste-invalid"))))))))))

(defn move-selection
  "`base` with the keyframes, preset animations and clips of `selection`
  (see `selection-of`) moved `dt` ms together, before 0 too, so they keep
  their spacing; the locked ones stay."
  [base {:keys [kfs anims clips]} dt]
  (as-> base timeline
    (if (seq kfs) (cta/shift-keyframes timeline kfs dt) timeline)
    (reduce (fn [timeline {:keys [shape-id animation-id]}]
              (cta/update-animation timeline shape-id animation-id #(update % :start + dt)))
            timeline
            anims)
    (drag-clips timeline clips dt)))

(defn commit-selection-move
  "Commit `base` with `selection` moved by `dt` (see `move-selection`): each
  step of a drag from the same `base`, inside one undo transaction."
  [base selection dt]
  (ptk/reify ::commit-selection-move
    ptk/WatchEvent
    (watch [it state _]
      (let [timeline (move-selection base selection dt)]
        (rx/of (dwa/commit-timeline it state (:board-id timeline) timeline)
               (dwa/apply-preview))))))

(defn nudge-selection
  "Move all that is selected in the dock by `dt` ms together (see
  `move-selection`), as the arrow keys do: one undo step."
  [dt]
  (ptk/reify ::nudge-selection
    ptk/WatchEvent
    (watch [it state _]
      (if-let [timeline (dwa/current-timeline state)]
        (let [timeline' (move-selection timeline (selection-of (:workspace-animation state)) dt)]
          (rx/of (dwa/commit-timeline it state (:board-id timeline') timeline')
                 (dwa/apply-preview)))
        (rx/empty)))))

(defn delete-selection
  "Delete all that is selected in the dock, keyframes, preset animations
  and clips, but the locked ones (locked clips stay selected): one undo
  step."
  []
  (ptk/reify ::delete-selection
    ptk/WatchEvent
    (watch [it state _]
      (if-let [timeline (dwa/current-timeline state)]
        (let [{:keys [kfs anims clips]} (selection-of (:workspace-animation state))
              locked    (into #{} (filter #(:locked (ctau/get-audio-clip timeline %))) clips)
              timeline' (as-> timeline tl
                          (reduce (fn [tl {:keys [shape-id keyframe-id]}]
                                    (cta/remove-keyframe tl shape-id keyframe-id))
                                  tl kfs)
                          (reduce (fn [tl {:keys [shape-id animation-id]}]
                                    (cta/remove-animation tl shape-id animation-id))
                                  tl anims)
                          (reduce ctau/remove-audio-clip tl clips))]
          (rx/concat
           (rx/of (select-items {:clips locked}))
           (if (= timeline timeline')
             (rx/empty)
             (rx/of (dwa/commit-timeline it state (:board-id timeline') timeline')
                    (dwmo/clear-local-transform)
                    (dwa/apply-preview)))))
        (rx/empty)))))

(defn drag-clip
  "`base`, the timeline as a drag of its clip `clip-id` started, with the
  clip dragged by its `mode` part to `time` ms: its start when it moves
  (`:move`, onto `lane` when given), the edge it trims (`:start`, `:end`,
  see `ctau/trim-audio-clip-start`), or where a fade ends (`:fade-in`,
  `:fade-out`; each at most half of the clip, none past its edge); or,
  by its volume line (`:volume`), to the volume `time` (from 0 to
  `ctau/max-volume`). A locked clip stays."
  [base clip-id mode time lane]
  (let [clip (ctau/get-audio-clip base clip-id)]
    (case mode
      :move     (ctau/move-audio-clip base clip-id time :lane lane)
      :start    (ctau/trim-audio-clip-start base clip-id time)
      :end      (ctau/trim-audio-clip-end base clip-id time)
      :fade-in  (ctau/set-audio-clip-fades base clip-id (max 0 (- time (:start clip))) (:fade-out clip))
      :fade-out (ctau/set-audio-clip-fades base clip-id (:fade-in clip) (max 0 (- (ctau/clip-end clip) time)))
      :volume   (ctau/set-audio-clip-volume base clip-id (mth/clamp time 0 ctau/max-volume))
      base)))

(defn drag-clips
  "`base`, the timeline as a drag of its clips `clip-ids` started, with
  them moved by `dt` ms together, before 0 too, each on its lane (a
  locked one stays)."
  [base clip-ids dt]
  (reduce (fn [timeline clip-id]
            (if-let [clip (ctau/get-audio-clip base clip-id)]
              (ctau/move-audio-clip timeline clip-id (+ (:start clip) dt))
              timeline))
          base
          clip-ids))

(defn commit-clips-drag
  "Commit `base` with its clips `clip-ids` dragged by `dt` (see
  `drag-clips`): each step of a drag from the same `base`, inside one undo
  transaction."
  [base clip-ids dt]
  (ptk/reify ::commit-clips-drag
    ptk/WatchEvent
    (watch [it state _]
      (let [timeline (drag-clips base clip-ids dt)]
        (rx/of (dwa/commit-timeline it state (:board-id timeline) timeline))))))

(defn commit-clip-drag
  "Commit `base` with its clip `clip-id` dragged (see `drag-clip`): each
  step of a drag from the same `base`, inside one undo transaction."
  [base clip-id mode time lane]
  (ptk/reify ::commit-clip-drag
    ptk/WatchEvent
    (watch [it state _]
      (let [timeline (drag-clip base clip-id mode time lane)]
        (rx/of (dwa/commit-timeline it state (:board-id timeline) timeline))))))

(defn- edit-clips
  "Commit the timeline of the board in the dock as `f` changes it."
  [f]
  (ptk/reify ::edit-clips
    ptk/WatchEvent
    (watch [it state _]
      (if-let [timeline (dwa/current-timeline state)]
        (rx/of (dwa/commit-timeline it state (:board-id timeline) (f timeline)))
        (rx/empty)))))

(defn set-clip-volume
  "Set the volume of the clip `clip-id`, 0..1."
  [clip-id volume]
  (edit-clips #(ctau/set-audio-clip-volume % clip-id volume)))

(defn toggle-clip-muted
  [clip-id]
  (edit-clips #(ctau/set-audio-clip-muted % clip-id (not (:muted (ctau/get-audio-clip % clip-id))))))

(defn split-clip
  "Cut the clip `clip-id` in two at the playhead (see
  `ctau/split-audio-clip`): nothing when it is not inside it."
  [clip-id]
  (ptk/reify ::split-clip
    ptk/WatchEvent
    (watch [_ state _]
      (let [time (dwa/playhead state)]
        (rx/of (edit-clips #(ctau/split-audio-clip % clip-id time (uuid/next))))))))

(defn split-all-clips
  "Cut in two every clip of the timeline in the dock the playhead is
  inside (see `ctau/split-audio-clip`), in one undo step, as Final Cut's
  Blade All."
  []
  (ptk/reify ::split-all-clips
    ptk/WatchEvent
    (watch [_ state _]
      (let [time (dwa/playhead state)]
        (rx/of (edit-clips (fn [timeline]
                             (reduce #(ctau/split-audio-clip %1 %2 time (uuid/next))
                                     timeline
                                     (keys (:audio-clips timeline))))))))))

(defn delete-clip
  "Delete the clip `clip-id`, and let go of it: nothing when it is locked
  (it stays selected, so the keys still go to it)."
  [clip-id]
  (ptk/reify ::delete-clip
    ptk/WatchEvent
    (watch [_ state _]
      (if (:locked (some-> (dwa/current-timeline state) (ctau/get-audio-clip clip-id)))
        (rx/empty)
        (rx/of (select-clip nil)
               (edit-clips #(ctau/remove-audio-clip % clip-id)))))))

(defn split-selected-clips
  "Cut the selected clips in two at the playhead (see
  `ctau/split-audio-clip`), in one undo step."
  []
  (ptk/reify ::split-selected-clips
    ptk/WatchEvent
    (watch [_ state _]
      (let [time (dwa/playhead state)
            ids  (selection state)]
        (rx/of (edit-clips (fn [timeline]
                             (reduce #(ctau/split-audio-clip %1 %2 time (uuid/next)) timeline ids))))))))

(defn delete-selected-clips
  "Delete the selected clips but the locked ones, which stay selected, in
  one undo step."
  []
  (ptk/reify ::delete-selected-clips
    ptk/WatchEvent
    (watch [_ state _]
      (let [timeline (dwa/current-timeline state)
            ids      (selection state)
            locked   (into #{} (filter #(:locked (ctau/get-audio-clip timeline %))) ids)]
        (if (= locked ids)
          (rx/empty)
          (rx/of (select-clips locked)
                 (edit-clips (fn [timeline]
                               (reduce ctau/remove-audio-clip timeline (remove locked ids))))))))))

(defn nudge-selected-clips
  "Move the selected clips by `dt` ms together, none before 0 (see
  `drag-clips`), in one undo step."
  [dt]
  (ptk/reify ::nudge-selected-clips
    ptk/WatchEvent
    (watch [_ state _]
      (let [ids (selection state)]
        (rx/of (edit-clips #(drag-clips % ids dt)))))))

(defn fit-timeline-to-audio
  "Make the timeline of the board in the dock as long as its clips play
  (see `ctau/fit-duration-to-audio`): what a drop never does to it."
  []
  (edit-clips ctau/fit-duration-to-audio))

(defn set-clip-speed
  "Play the file of the clip `clip-id` at `speed` (see
  `ctau/set-audio-clip-speed`): the same part of it, in less time and
  higher, or in more time and lower."
  [clip-id speed]
  (edit-clips #(ctau/set-audio-clip-speed % clip-id speed)))

(defn set-clip-fades
  "Set the fades of the clip `clip-id`, in ms (nil or 0 for none): each
  at most half the clip (see `ctau/make-audio-clip`)."
  [clip-id fade-in fade-out]
  (edit-clips #(ctau/set-audio-clip-fades % clip-id fade-in fade-out)))

(defn toggle-clip-locked
  "Lock the clip `clip-id` or unlock it: the one change a locked clip
  takes."
  [clip-id]
  (edit-clips #(ctau/set-audio-clip-locked % clip-id (not (:locked (ctau/get-audio-clip % clip-id))))))

(defn- lane-clip-ids
  [timeline lane]
  (into [] (comp (filter #(= lane (:lane %))) (map :id)) (vals (:audio-clips timeline))))

(defn set-lane-muted
  "Mute the clips on the audio `lane` of the timeline in the dock, or
  play them, in one undo step: what the sound button of its row does. A
  locked clip stays as it is."
  [lane muted?]
  (edit-clips (fn [timeline]
                (reduce #(ctau/set-audio-clip-muted %1 %2 muted?) timeline (lane-clip-ids timeline lane)))))

(defn rename-lane
  "Name the files the audio `lane` of the timeline in the dock plays
  `name`, on all their clips (see `ctau/rename-audio-file`), in one undo
  step: what editing the name of its row does."
  [lane name]
  (edit-clips (fn [timeline]
                (reduce #(ctau/rename-audio-file %1 %2 name)
                        timeline
                        (into #{}
                              (comp (filter #(= lane (:lane %))) (map :audio-id))
                              (vals (:audio-clips timeline)))))))

(defn remove-lane-clips
  "Remove the clips on the audio `lane` of the timeline in the dock, in
  one undo step: what the remove button of its row in the design tab
  does. A locked clip stays."
  [lane]
  (edit-clips (fn [timeline]
                (reduce ctau/remove-audio-clip timeline (lane-clip-ids timeline lane)))))

(defn set-all-muted
  "Mute all the clips of the timeline in the dock, or play them, in one
  undo step: what the sound button of their group row does. A locked
  clip stays as it is."
  [muted?]
  (edit-clips (fn [timeline]
                (reduce #(ctau/set-audio-clip-muted %1 %2 muted?) timeline (keys (:audio-clips timeline))))))

(defn set-all-locked
  "Lock all the clips of the timeline in the dock, or unlock them, in one
  undo step: what the lock button of their group row does."
  [locked?]
  (edit-clips (fn [timeline]
                (reduce #(ctau/set-audio-clip-locked %1 %2 locked?) timeline (keys (:audio-clips timeline))))))

(defn move-lane
  "Put the audio `lane` of the timeline in the dock at the `to`th place of
  its lanes (see `ctau/move-lane`), as dragging its row does: one undo
  step."
  [lane to]
  (edit-clips #(ctau/move-lane % lane to)))

(defn set-lane-locked
  "Lock the clips on the audio `lane` of the timeline in the dock, or
  unlock them, in one undo step: what the lock button of its row does."
  [lane locked?]
  (edit-clips (fn [timeline]
                (reduce #(ctau/set-audio-clip-locked %1 %2 locked?) timeline (lane-clip-ids timeline lane)))))

(defn nudge-clip
  "Move the clip `clip-id` by `dt` ms, before 0 too."
  [clip-id dt]
  (edit-clips (fn [timeline]
                (if-let [clip (ctau/get-audio-clip timeline clip-id)]
                  (ctau/move-audio-clip timeline clip-id (+ (:start clip) dt))
                  timeline))))
