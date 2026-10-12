;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.workspace-audio-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.geom.point :as gpt]
   [app.common.media :as cm]
   [app.common.test-helpers.compositions :as ctho]
   [app.common.test-helpers.files :as cthf]
   [app.common.test-helpers.ids-map :as cthi]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.audio :as dwaa]
   [app.main.data.workspace.clipboard :as dwcp]
   [app.main.data.workspace.media :as dwm]
   [app.main.data.workspace.undo :as dwu]
   [app.main.repo :as rp]
   [app.util.audio :as audio]
   [app.util.clipboard :as clipboard]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [frontend-tests.helpers.mock :as mock]
   [frontend-tests.helpers.pages :as thp]
   [frontend-tests.helpers.state :as ths]
   [potok.v2.core :as ptk]))

(t/use-fixtures :each
  {:before (fn []
             (thp/reset-idmap!)
             (cthi/reset-idmap!))})

(defn- setup-file
  "Board A at 0,0, whose timeline plays a second, and board B at 1000,0,
  without one."
  []
  (let [file (-> (cthf/sample-file :file1 :page-label :page-1)
                 (ctho/add-frame :board-a :x 0 :y 0 :width 400 :height 300)
                 (ctho/add-frame :board-b :x 1000 :y 0 :width 400 :height 300))]
    (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                 (pcb/with-page (cthf/current-page file))
                                 (pcb/set-timeline (cthi/id :board-a)
                                                   (cta/make-timeline {:board-id (cthi/id :board-a)
                                                                       :duration 1000}))))))

(defn- store-of
  [file]
  (let [store (ths/setup-store file)]
    (ptk/emit! store #(update % :features (fnil conj #{}) "animation/v1"))
    store))

(defn- audio-file
  "A file `name` that decodes (see `decode`) to `ms` of audio, or not at
  all without them."
  [name & {:keys [type ms] :or {type "audio/mpeg"}}]
  (js/File. #js [(if ms (str "ms:" ms) "noise")] name #js {:type type}))

(defn- fake-buffer
  "What decoding a file of `ms` gives."
  [ms]
  (let [rate 48000
        n    (js/Math.round (* rate (/ ms 1000)))]
    #js {:duration (/ ms 1000)
         :length n
         :sampleRate rate
         :numberOfChannels 1
         :getChannelData (fn [_] (js/Float32Array. n))}))

(defn- decode
  "Decode the `bytes` of an `audio-file`."
  [bytes]
  (let [text (.decode (js/TextDecoder.) bytes)]
    (if (.startsWith text "ms:")
      (js/Promise.resolve (fake-buffer (js/parseInt (subs text 3))))
      (js/Promise.reject (js/Error. "undecodable")))))

(defn- uploads
  "An `rp/cmd!` that uploads a file media object of each file, with the
  ids of `ids` in turn, and records the uploads in `calls`; other
  commands (as the thumbnails of the workspace send) answer nothing."
  [calls ids]
  (mock/stub
   (fn [cmd params]
     (if (= :upload-file-media-object cmd)
       (do (swap! calls conj [cmd (:name params) (.-type (:content params))])
           (rx/of {:id (nth ids (dec (count @calls))) :mtype "audio/mpeg"}))
       (rx/empty)))))

(defn- watch-undo-stack
  "Put the commits on the undo stack, as the workspace does, until
  `::done`."
  []
  (ptk/reify ::watch-undo-stack
    ptk/WatchEvent
    (watch [_ _ stream]
      (->> stream
           (rx/filter dch/commit?)
           (rx/map deref)
           (rx/filter #(and (= :local (:source %)) (:save-undo? %) (seq (:undo-changes %))))
           (rx/map #(dwu/append-undo (select-keys % [:undo-changes :redo-changes :undo-group :tags]) false))
           (rx/take-until (rx/filter #(= ::done %) stream))))))

(defn- after
  "A stopper of `ths/run-store`: a moment after the first event `pred`
  holds for."
  [pred]
  (fn [stream]
    (->> stream (rx/filter pred) (rx/take 1) (rx/delay 20))))

(defn- after-error
  "A stopper of `ths/run-store`: once `store` shows an error."
  [store]
  (fn [stream]
    (->> stream
         (rx/filter #(= :app.main.data.notifications/show (ptk/type %)))
         (rx/delay 10)
         (rx/filter (fn [_] (= :error (get-in @store [:notification :level]))))
         (rx/take 1))))

(defn- commit-of-clips?
  "Whether `event` commits audio clips: one by one, or with a new
  timeline."
  [event]
  (and (dch/commit? event)
       (some #(or (= :set-timeline-audio-clip (:type %))
                  (seq (get-in % [:params :audio-clips])))
             (:redo-changes (deref event)))))

(defn- timeline-of
  [state label]
  (get (:timelines (thp/current-page state)) (cthi/id label)))

(defn- clips-of
  [state label]
  (ctau/audio-clips (timeline-of state label)))

(defn- toast
  [state]
  (get-in state [:notification :content]))

(t/deftest an-audio-file-becomes-a-clip
  (t/async
    done
    (let [store (store-of (setup-file))
          calls (atom [])
          id    (uuid/next)]
      (mock/with-mocks
        {rp/cmd! (uploads calls [id])}
        (fn [done']
          (ths/run-store
           store done'
           [(watch-undo-stack)
            (dwaa/add-audio-files (cthi/id :board-a) [(audio-file "beat.mp3" :ms 2500)] 1200 nil
                                  :decode decode)]
           (fn [state]
             (let [[clip :as clips] (clips-of state :board-a)]
               (t/is (= [[:upload-file-media-object "beat.mp3" "audio/mpeg"]] @calls))
               (t/is (= 1 (count clips)))
               (t/is (= [id "beat.mp3" 1200 0 2500 2500 0]
                        ((juxt :audio-id :name :start :offset :duration :source-duration :lane) clip)))
               (t/is (= 1000 (:duration (timeline-of state :board-a))) "the timeline keeps its length")
               (t/is (some? (audio/waveform id)) "its waveform, without loading it again")
               (ptk/emit! store dwu/undo)
               (t/is (empty? (clips-of @store :board-a)) "one undo")
               (ptk/emit! store ::done)))
           (after commit-of-clips?)))
        done))))

(t/deftest a-recording-leaves-out-its-pre-roll
  ;; Its first ms play before where its clip goes (see
  ;; `dwar/pre-roll-from`): the clip starts past them in its file.
  (t/async
    done
    (let [store (store-of (setup-file))
          id    (uuid/next)]
      (mock/with-mocks
        {rp/cmd! (uploads (atom []) [id])}
        (fn [done']
          (ths/run-store
           store done'
           [(dwaa/add-audio-files (cthi/id :board-a) [(audio-file "take.webm" :type "audio/webm" :ms 2500)] 1200 nil
                                  :decode decode :trim 1000)]
           (fn [state]
             (let [[clip] (clips-of state :board-a)]
               (t/is (= [1200 1000 1500 2500] ((juxt :start :offset :duration :source-duration) clip)))))
           (after commit-of-clips?)))
        done))))

(t/deftest files-dropped-together-make-one-change
  (t/async
    done
    (let [store   (store-of (setup-file))
          calls   (atom [])
          commits (atom 0)
          [a b]   [(uuid/next) (uuid/next)]]
      (mock/with-mocks
        {rp/cmd! (uploads calls [a b])}
        (fn [done']
          (ths/run-store
           store done'
           [(ptk/reify ::count-commits
              ptk/WatchEvent
              (watch [_ _ stream]
                (->> stream
                     (rx/filter dch/commit?)
                     (rx/tap #(swap! commits inc))
                     (rx/ignore)
                     (rx/take-until (rx/filter #(= ::done %) stream)))))
            (dwaa/add-audio-files (cthi/id :board-b)
                                  [(audio-file "one.mp3" :ms 1000)
                                   (audio-file "two.wav" :type "audio/x-wav" :ms 1500)]
                                  0 nil
                                  :decode decode)]
           (fn [state]
             (t/is (= 1 @commits) "one change, one undo")
             (t/is (= [[a 0 1000] [b 1000 1500]] (mapv (juxt :audio-id :start :duration) (clips-of state :board-b)))
                   "one after the other")
             (t/is (= "audio/wav" (nth (second @calls) 2)) "uploaded as its canonical type")
             (t/is (= 2500 (:duration (timeline-of state :board-b)))
                   "a new timeline lasts until its clips end")
             (ptk/emit! store ::done))
           (after commit-of-clips?)))
        done))))

(t/deftest nothing-is-added-to-a-board-that-is-gone
  (t/async
    done
    (let [store  (store-of (setup-file))
          board  (cthi/id :board-a)
          delete (ptk/reify ::delete-board
                   ptk/UpdateEvent
                   (update [_ state]
                     (update-in state [:files (:current-file-id state) :data :pages-index
                                       (:current-page-id state) :objects]
                                dissoc board)))]
      (mock/with-mocks
        {rp/cmd! (mock/stub (fn [_ _]
                              ;; the board is deleted while the file uploads
                              (ptk/emit! store delete)
                              (rx/of {:id (uuid/next)})))}
        (fn [done']
          (ths/run-store
           store done'
           [(dwaa/add-audio-files board [(audio-file "beat.mp3" :ms 1000)] 0 nil :decode decode)]
           (fn [state]
             (t/is (= (tr "workspace.audio.board-gone") (toast state)))
             (t/is (empty? (clips-of state :board-a))))
           (after-error store)))
        done))))

(t/deftest what-does-not-decode-is-not-uploaded
  (t/async
    done
    (let [store (store-of (setup-file))
          calls (atom [])]
      (mock/with-mocks
        {rp/cmd! (uploads calls [(uuid/next)])}
        (fn [done']
          (ths/run-store
           store done'
           [(dwaa/add-audio-files (cthi/id :board-a) [(audio-file "noise.mp3")] 0 nil :decode decode)]
           (fn [state]
             (t/is (= (tr "workspace.audio.undecodable" "noise.mp3") (toast state)))
             (t/is (empty? @calls)))
           (after-error store)))
        done))))

(t/deftest what-lasts-too-long-is-not-uploaded
  (t/async
    done
    (let [store (store-of (setup-file))
          calls (atom [])]
      (mock/with-mocks
        {rp/cmd! (uploads calls [(uuid/next)])}
        (fn [done']
          (ths/run-store
           store done'
           [(dwaa/add-audio-files (cthi/id :board-a)
                                  [(audio-file "long.mp3" :ms (inc ctau/max-audio-duration))]
                                  0 nil :decode decode)]
           (fn [state]
             (t/is (= (tr "workspace.audio.too-long" "long.mp3" 5) (toast state)))
             (t/is (empty? @calls)))
           (after-error store)))
        done))))

(t/deftest a-server-without-motion-says-so
  (t/async
    done
    (let [store (store-of (setup-file))]
      (mock/with-mocks
        {rp/cmd! (mock/stub (fn [_ _]
                              (rx/throw (ex-info "not allowed" {:type :validation
                                                                :code :media-type-not-allowed}))))}
        (fn [done']
          (ths/run-store
           store done'
           [(dwaa/add-audio-files (cthi/id :board-a) [(audio-file "beat.mp3" :ms 1000)] 0 nil :decode decode)]
           (fn [state]
             (t/is (= (tr "workspace.audio.upload-not-enabled") (toast state)))
             (t/is (empty? (clips-of state :board-a))))
           (after-error store)))
        done))))

(t/deftest audio-files-are-known-by-type-or-extension
  (t/is (= "audio/mpeg" (dwaa/audio-type (audio-file "a.bin" :type "audio/mp3"))))
  (t/is (= "audio/webm" (dwaa/audio-type (audio-file "a.webm" :type "video/webm"))))
  (t/is (= "audio/mp4" (dwaa/audio-type (audio-file "a.M4A" :type ""))))
  (t/is (nil? (dwaa/audio-type (audio-file "a.png" :type "image/png"))))
  (t/is (nil? (dwaa/audio-type (audio-file "noextension" :type "")))))

(t/deftest blobs-upload-the-types-they-allow
  (t/async
    done
    (let [calls (atom [])
          blobs (fn [blob & {:as opts}]
                  (dwm/process-blobs (merge {:file-id (uuid/next) :local? true :blobs [blob]
                                             :on-image (constantly nil) :on-svg (constantly nil)}
                                            opts)))]
      (mock/with-mocks
        {rp/cmd! (mock/stub (fn [cmd params]
                              (if (= :upload-file-media-object cmd)
                                (do (swap! calls conj [cmd (.-type (:content params))])
                                    (rx/of {:id (uuid/next)}))
                                (rx/empty))))}
        (fn [done']
          (->> (rx/concat
                (blobs (js/File. #js ["png"] "a.png" #js {:type "image/png"}))
                (->> (blobs (audio-file "a.mp3"))
                     (rx/catch (fn [cause] (rx/of (:code (ex-data cause))))))
                (blobs (audio-file "a.mp3") :force-media true :allowed-types cm/audio-canonical-types))
               (rx/reduce conj [])
               (rx/subs! (fn [results]
                           (t/is (= :media-type-not-allowed (second results)) "images only, by default")
                           (t/is (= [[:upload-file-media-object "image/png"]
                                     [:upload-file-media-object "audio/mpeg"]]
                                    @calls)))
                         (fn [cause]
                           (t/is (nil? cause))
                           (done'))
                         done')))
        done))))

(t/deftest audio-dropped-on-a-board-plays-from-its-playhead
  (t/async
    done
    (let [store (store-of (setup-file))
          id    (uuid/next)]
      (mock/with-mocks
        {rp/cmd! (uploads (atom []) [id])}
        (fn [done']
          (ths/run-store
           store done'
           [(dwa/set-active-board (cthi/id :board-b))
            (dwa/set-playhead 700)
            ;; dropped over board B, at 1000,0
            (dwaa/drop-audio-files [(audio-file "beat.mp3" :ms 1000)] (gpt/point 1100 50) :decode decode)]
           (fn [state]
             (t/is (= [[id 700]] (mapv (juxt :audio-id :start) (clips-of state :board-b)))))
           (after commit-of-clips?)))
        done))))

(t/deftest audio-dropped-away-from-the-boards-needs-a-board
  (t/async
    done
    (let [store (store-of (setup-file))
          calls (atom [])]
      (mock/with-mocks
        {rp/cmd! (uploads calls [(uuid/next)])}
        (fn [done']
          (ths/run-store
           store done'
           [(dwaa/drop-audio-files [(audio-file "beat.mp3" :ms 1000)] (gpt/point 5000 5000) :decode decode)]
           (fn [state]
             (t/is (= (tr "workspace.audio.drop-on-a-board") (toast state)))
             (t/is (empty? @calls)))
           (after-error store)))
        done))))

(t/deftest pasted-clips-of-another-file-play-copies-of-their-audio
  (t/async
    done
    (let [calls  (atom [])
          [a b]  [(uuid/next) (uuid/next)]
          copies {a (uuid/next) b (uuid/next)}
          clip   #(ctau/make-audio-clip {:audio-id % :name "beat.mp3" :source-duration 1000 :duration 1000})
          board  (uuid/next)
          clips  (into {} (map (juxt :id identity)) [(clip a) (clip a) (clip b)])
          pdata  {:type :copied-shapes :motion {:tracks {} :settings {} :audio {board clips}}}]
      (mock/with-mocks
        {rp/cmd! (mock/stub (fn [cmd params]
                              (swap! calls conj [cmd (:id params)])
                              (rx/of {:id (get copies (:id params))})))}
        (fn [done']
          (->> (rx/concat
                (dwcp/with-own-audio (uuid/next) pdata)
                (dwcp/with-own-audio (uuid/next) {:type :copied-shapes :motion {:tracks {}}}))
               (rx/reduce conj [])
               (rx/subs! (fn [[pasted plain]]
                           (t/is (= #{[:clone-file-media-object a] [:clone-file-media-object b]} (set @calls))
                                 "one clone of each file")
                           (t/is (= 2 (count @calls)))
                           (t/is (= (frequencies (map (comp copies :audio-id) (vals clips)))
                                    (frequencies (map :audio-id (vals (get-in pasted [:motion :audio board])))))
                                 "the clips play the copies")
                           (t/is (= {:type :copied-shapes :motion {:tracks {}}} plain) "nothing to clone"))
                         (fn [cause]
                           (t/is (nil? cause))
                           (done'))
                         done')))
        done))))

;; CLIPS IN THE DOCK

(defn- lane-rows
  "The rows of the lanes of `timeline`, after the one of their group."
  [timeline]
  (filterv #(= :audio (:type %)) (dwaa/audio-rows timeline)))

(t/deftest each-lane-with-clips-is-a-row
  (let [song (uuid/next)
        clip #(ctau/make-audio-clip (merge {:audio-id (uuid/next) :name "x" :source-duration 1000
                                            :duration 500}
                                           %))
        tl   (reduce ctau/add-audio-clip
                     (cta/make-timeline {:board-id (uuid/next)})
                     [(clip {:start 600 :lane 2 :audio-id song :name "song.mp3"})
                      (clip {:start 0 :lane 0})
                      (clip {:start 100 :lane 2 :audio-id song :name "song.mp3"})])]
    (t/is (= ["x" "song.mp3"] (mapv :name (lane-rows tl))) "named after the file of their lane")
    (t/is (= [[:audio 0 1 [0]] [:audio 2 2 [100 600]]]
             (mapv (fn [{:keys [type lane number clips]}] [type lane number (mapv :start clips)])
                   (lane-rows tl)))
          "numbered in order, whatever lanes are empty")
    (t/is (= [] (dwaa/audio-rows (cta/make-timeline {:board-id (uuid/next)}))) "none without clips")
    (t/testing "their group first, a folder of them"
      (let [[group & lanes] (dwaa/audio-rows tl)]
        (t/is (= {:type :audio-group :collapsed? false :count 2 :muted? false :locked? false}
                 (dissoc group :clips)))
        (t/is (= [0 100 600] (mapv :start (:clips group))) "all the clips")
        (t/is (= 2 (count lanes))))
      (t/is (= [:audio-group] (mapv :type (dwaa/audio-rows tl :collapsed? true))) "collapsed, alone"))))

(t/deftest a-clip-dragged-under-the-rows-gets-a-lane-of-its-own
  (let [song (uuid/next)
        clip #(ctau/make-audio-clip (merge {:audio-id song :name "song.mp3" :source-duration 1000
                                            :duration 500}
                                           %))
        a    (clip {:start 0 :lane 0})
        b    (clip {:start 600 :lane 0})
        c    (clip {:start 0 :lane 3})
        tl   (reduce ctau/add-audio-clip (cta/make-timeline {:board-id (uuid/next)}) [a b c])
        rows #(mapv (juxt :lane (fn [row] (mapv :start (:clips row)))) (lane-rows %))]
    (t/is (= 4 (dwaa/lane-after tl (:id a))) "after the others")
    (t/is (= 1 (dwaa/lane-after tl (:id c))) "alone on the last one, it closes the gap")
    (t/testing "two lanes of a file put together come apart again"
      (let [merged (dwaa/drag-clip tl (:id c) :move 0 0)
            apart  (dwaa/drag-clip merged (:id b) :move 600 (dwaa/lane-after merged (:id b)))]
        (t/is (= [[0 [0 0 600]]] (rows merged)))
        (t/is (= [[0 [0 0]] [1 [600]]] (rows apart)))))
    (t/testing "a lane plays a single file"
      (let [other (ctau/make-audio-clip {:audio-id (uuid/next) :name "other.wav" :source-duration 1000
                                         :start 2000 :duration 500 :lane 5})
            tl    (ctau/add-audio-clip tl other)]
        (t/is (= 5 (:lane (ctau/get-audio-clip (dwaa/drag-clip tl (:id other) :move 2000 0) (:id other))))
              "dragged onto a lane of another file, it stays on its own")
        (t/is (= ["song.mp3" "song.mp3" "other.wav"] (mapv :name (lane-rows tl)))
              "and the rows are named after their files")))))

(t/deftest a-dragged-clip-moves-or-trims
  (let [clip   (ctau/make-audio-clip {:audio-id (uuid/next) :name "x" :source-duration 3000
                                      :start 1000 :offset 500 :duration 1000})
        base   (ctau/add-audio-clip (cta/make-timeline {:board-id (uuid/next) :duration 5000}) clip)
        id     (:id clip)
        at     #(select-keys (ctau/get-audio-clip % id) [:start :offset :duration :lane])]
    (t/is (= {:start 1300 :offset 500 :duration 1000 :lane 0} (at (dwaa/drag-clip base id :move 1300 nil)))
          "the body moves it")
    (t/is (= 1 (:lane (at (dwaa/drag-clip base id :move 1300 1)))) "onto another lane")
    (t/is (= {:start 1200 :offset 700 :duration 800 :lane 0} (at (dwaa/drag-clip base id :start 1200 nil)))
          "the left grip trims its start and the part of its file together")
    (t/is (= {:start 1000 :offset 500 :duration 1300 :lane 0} (at (dwaa/drag-clip base id :end 2300 nil))))
    (let [locked (ctau/set-audio-clip-locked base id true)]
      (t/is (= locked (dwaa/drag-clip locked id :move 0 nil) (dwaa/drag-clip locked id :start 1200 nil))
            "a locked clip stays"))))

(t/deftest a-whole-drag-is-one-undo
  (t/async
    done
    (let [clip  (ctau/make-audio-clip {:audio-id (uuid/next) :name "x" :source-duration 3000
                                       :start 100 :duration 1000})
          file  (let [file (setup-file)]
                  (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                               (pcb/with-page (cthf/current-page file))
                                               (pcb/set-timeline-audio-clip (cthi/id :board-a) (:id clip) clip))))
          store (store-of file)
          base  (timeline-of @store :board-a)
          undo  (js/Symbol)]
      (ths/run-store
       store done
       [(watch-undo-stack)
        (dwu/start-undo-transaction undo)
        (dwaa/commit-clip-drag base (:id clip) :move 200 nil)
        (dwaa/commit-clip-drag base (:id clip) :move 400 nil)
        (dwu/commit-undo-transaction undo)]
       (fn [state]
         (t/is (= 400 (:start (ctau/get-audio-clip (timeline-of state :board-a) (:id clip)))))
         (ptk/emit! store dwu/undo)
         (t/is (= 100 (:start (ctau/get-audio-clip (timeline-of @store :board-a) (:id clip)))) "one undo")
         (ptk/emit! store ::done))))))

(t/deftest a-dragged-grip-changes-a-fade
  (let [clip  (ctau/make-audio-clip {:audio-id (uuid/next) :name "x" :source-duration 3000
                                     :start 1000 :offset 500 :duration 1000 :fade-out 200})
        base  (ctau/add-audio-clip (cta/make-timeline {:board-id (uuid/next) :duration 5000}) clip)
        id    (:id clip)
        fades #((juxt :fade-in :fade-out) (ctau/get-audio-clip % id))]
    (t/is (= [300 200] (fades (dwaa/drag-clip base id :fade-in 1300 nil))) "the fade in ends where it is dropped")
    (t/is (= [nil 400] (fades (dwaa/drag-clip base id :fade-out 1600 nil))) "the fade out from there to the end")
    (t/is (= [500 200] (fades (dwaa/drag-clip base id :fade-in 1900 nil))) "at most half of the clip")
    (t/is (= [nil 200] (fades (dwaa/drag-clip base id :fade-in 900 nil))) "none before its start")
    (t/is (= [nil nil] (fades (dwaa/drag-clip base id :fade-out 2500 nil))) "none past its end")
    (t/testing "its volume line"
      (t/is (= 0.4 (:volume (ctau/get-audio-clip (dwaa/drag-clip base id :volume 0.4 nil) id))))
      (t/is (= 0 (:volume (ctau/get-audio-clip (dwaa/drag-clip base id :volume -0.3 nil) id))) "not below silence")
      (t/is (= 1.5 (:volume (ctau/get-audio-clip (dwaa/drag-clip base id :volume 1.5 nil) id))) "louder than its file")
      (t/is (= 4 (:volume (ctau/get-audio-clip (dwaa/drag-clip base id :volume 5 nil) id))) "four times at most"))
    (let [locked (ctau/set-audio-clip-locked base id true)]
      (t/is (= locked (dwaa/drag-clip locked id :fade-in 1300 nil)) "a locked clip stays"))))

(t/deftest selecting-a-clip-lets-go-of-the-keyframes
  (let [state (-> {:workspace-animation {:selected-kfs #{{:shape-id (uuid/next) :keyframe-id (uuid/next)}}}}
                  (as-> s (ptk/update (dwaa/select-clip :clip) s)))]
    (t/is (= :clip (get-in state [:workspace-animation :selected-clip])))
    (t/is (nil? (get-in state [:workspace-animation :selected-kfs])))
    (t/is (nil? (get-in (ptk/update (dwa/select-keyframes [{:shape-id 1 :keyframe-id 2}]) state)
                        [:workspace-animation :selected-clip]))
          "and the other way round")))

(t/deftest several-clips-are-selected
  (let [anim  #(:workspace-animation %)
        sel   #(dwaa/selected-clips (anim %))
        state (ptk/update (dwaa/select-clip :a) {:workspace-animation {:selected-kfs #{:k}}})
        two   (ptk/update (dwaa/toggle-clip :b) state)]
    (t/is (= #{:a} (sel state)))
    (t/is (= [#{:a :b} :b] [(sel two) (:selected-clip (anim two))]) "Shift + click adds one")
    (t/is (= #{:a} (sel (ptk/update (dwaa/toggle-clip :b) two))) "and takes it out again")
    (t/is (= #{} (sel (ptk/update (dwaa/toggle-clip :a) state))) "the last one too")
    (t/is (= #{:c :d} (sel (ptk/update (dwaa/select-clips [:c :d]) two))) "a box picks them")
    (t/is (= #{} (sel (ptk/update (dwa/select-keyframes [{:shape-id 1 :keyframe-id 2}]) two)))
          "keyframes take their place")
    (t/is (nil? (:selected-kfs (anim two))) "as they take the place of keyframes")))

(t/deftest clips-dragged-together-keep-their-spacing
  (let [clip #(ctau/make-audio-clip (merge {:audio-id (uuid/next) :name "x" :source-duration 3000
                                            :duration 500}
                                           %))
        a    (clip {:start 200 :lane 0})
        b    (clip {:start 1000 :lane 1})
        c    (clip {:start 50 :lane 2 :locked true})
        tl   (reduce ctau/add-audio-clip (cta/make-timeline {:board-id (uuid/next) :duration 5000}) [a b c])
        at   #(mapv (fn [clip] ((juxt :start :lane) (ctau/get-audio-clip % (:id clip)))) [a b c])]
    (t/is (= [[500 0] [1300 1] [50 2]] (at (dwaa/drag-clips tl (map :id [a b]) 300))) "each on its lane")
    (t/is (= [[-700 0] [100 1] [50 2]] (at (dwaa/drag-clips tl (map :id [a b]) -900)))
          "before 0 too, as in Figma")
    (t/is (= [[200 0] [1000 1] [50 2]] (at (dwaa/drag-clips tl (map :id [c]) 300))) "a locked one stays")))

(t/deftest keyframes-blocks-and-clips-are-selected-together
  (let [anim  #(:workspace-animation %)
        sel   #(dwaa/selection-of (anim %))
        kf    {:shape-id 1 :keyframe-id 2}
        block {:shape-id 1 :animation-id 3}
        clip  (ptk/update (dwaa/select-clip :c) {})
        more  (->> clip
                   (ptk/update (dwa/select-keyframe 1 2 true))
                   (ptk/update (dwa/select-animation-block 1 3 true)))]
    (t/is (= {:kfs #{kf} :anims #{block} :clips #{:c}} (sel more)) "Shift + click adds any kind")
    (t/is (= {:kfs #{kf} :anims #{} :clips #{}} (sel (ptk/update (dwa/select-keyframe 1 2) more)))
          "a click picks one alone")
    (t/is (= {:kfs #{} :anims #{block} :clips #{}} (sel (ptk/update (dwa/select-animation-block 1 3 false) more))))
    (t/is (= {:kfs #{kf} :anims #{} :clips #{:d}}
             (sel (ptk/update (dwaa/select-items {:kfs [kf] :clips [:d]}) more)))
          "a box picks them all")
    (t/is (= 3 (dwaa/selection-count (sel more))))))

(t/deftest what-is-selected-moves-together
  (let [shape (uuid/next)
        kf    (uuid/next)
        block (uuid/next)
        tl    (-> (cta/make-timeline {:board-id (uuid/next) :duration 5000})
                  (cta/add-keyframe shape {:id kf :time 300 :property :x :value 0})
                  (cta/add-animation shape {:id block :type :fade :start 600 :duration 200}))
        clip  (ctau/make-audio-clip {:audio-id (uuid/next) :name "x" :source-duration 3000
                                     :start 1000 :duration 500})
        tl    (ctau/add-audio-clip tl clip)
        sel   {:kfs #{{:shape-id shape :keyframe-id kf}}
               :anims #{{:shape-id shape :animation-id block}}
               :clips #{(:id clip)}}
        at    (fn [tl]
                [(:time (first (get-in tl [:tracks shape :keyframes])))
                 (:start (cta/get-animation tl shape block))
                 (:start (ctau/get-audio-clip tl (:id clip)))])]
    (t/is (= [500 800 1200] (at (dwaa/move-selection tl sel 200))) "all by as much")
    (t/is (= [-700 -400 0] (at (dwaa/move-selection tl sel -1000))) "before 0 too, their spacing kept")
    (t/is (= [300 600 1000] (at (dwaa/move-selection tl sel 0))) "nothing moves by 0")
    (t/is (= [500 800 1000] (at (dwaa/move-selection (ctau/set-audio-clip-locked tl (:id clip) true) sel 200)))
          "a locked clip stays")))

(defn- with-clip
  "`setup-file` with a clip from 100 to 1100 ms in the timeline of board A."
  [& {:as params}]
  (let [clip (ctau/make-audio-clip (merge {:audio-id (uuid/next) :name "x" :source-duration 3000
                                           :start 100 :duration 1000}
                                          params))
        file (setup-file)]
    [clip (cthf/apply-changes file (-> (pcb/empty-changes nil)
                                       (pcb/with-page (cthf/current-page file))
                                       (pcb/set-timeline-audio-clip (cthi/id :board-a) (:id clip) clip)))]))

(defn- run-clip-edits
  "Run `events` on board A of `file` (the active board), then `check` the
  state, and after one undo `undone`."
  [done file events check undone]
  (let [store (store-of file)]
    (ths/run-store
     store done
     (into [(watch-undo-stack) (dwa/set-active-board (cthi/id :board-a))] events)
     (fn [state]
       (check state)
       (ptk/emit! store dwu/undo)
       (undone @store)
       (ptk/emit! store ::done)))))

(defn- with-clips
  "`setup-file` with clips of `params` (see `ctau/make-audio-clip`) in the
  timeline of board A: `[clips file]`."
  [& params]
  (let [clips (mapv #(ctau/make-audio-clip (merge {:audio-id (uuid/next) :name "x" :source-duration 3000
                                                   :start 100 :duration 1000}
                                                  %))
                    params)
        file  (setup-file)]
    [clips (cthf/apply-changes file (reduce (fn [changes clip]
                                              (pcb/set-timeline-audio-clip changes (cthi/id :board-a) (:id clip) clip))
                                            (-> (pcb/empty-changes nil)
                                                (pcb/with-page (cthf/current-page file)))
                                            clips))]))

(t/deftest audio-dropped-on-a-lane-of-another-file-gets-its-own
  (t/async
    done
    (let [[[clip] file] (with-clips {:lane 0})
          store         (store-of file)
          id            (uuid/next)]
      (mock/with-mocks
        {rp/cmd! (uploads (atom []) [id])}
        (fn [done']
          (ths/run-store
           store done'
           [(dwaa/add-audio-files (cthi/id :board-a) [(audio-file "voice.mp3" :ms 500)] 0 0 :decode decode)]
           (fn [state]
             (t/is (= [[(:audio-id clip) 0] [id 1]] (mapv (juxt :audio-id :lane) (clips-of state :board-a)))))
           (after commit-of-clips?)))
        done))))

(t/deftest the-selected-clips-are-cut-but-the-locked-ones
  (t/async
    done
    (let [[[free locked] file] (with-clips {:lane 0} {:lane 1 :start 1200 :locked true})
          written              (atom nil)]
      (mock/with-mocks
        {clipboard/to-clipboard (mock/stub (fn [text] (reset! written text) (js/Promise.resolve nil)))}
        (fn [done']
          (run-clip-edits
           done' file
           [(dwaa/select-items {:clips [(:id free) (:id locked)]})
            (dwaa/cut-selection)]
           (fn [state]
             (let [clip (dwa/payload-clip @written)]
               (t/is (= [(:id free) (:id locked)] (mapv :id (:audio clip))) "both copied, by start")
               (t/is (= (cthi/id :board-a) (:board-id clip)))
               (t/is (not-any? :locked (:audio clip)) "never locked"))
             (t/is (= [(:id locked)] (mapv :id (clips-of state :board-a))) "the locked one stays")
             (t/is (= #{(:id locked)} (dwaa/selected-clips (:workspace-animation state))) "selected"))
           (fn [state]
             (t/is (= 2 (count (clips-of state :board-a))) "one undo"))))
        done))))

(t/deftest clips-pasted-in-the-dock-from-another-file-play-copies-of-their-audio
  (t/async
    done
    (let [store  (store-of (setup-file))
          [a c]  [(uuid/next) (uuid/next)]
          calls  (atom [])
          clip   {:file-id (uuid/next) :page-id (uuid/next) :board-id (uuid/next) :layers []
                  :audio [(ctau/make-audio-clip {:audio-id a :name "ping.wav" :source-duration 1000
                                                 :start 300 :duration 500})]}]
      (mock/with-mocks
        {rp/cmd! (mock/stub (fn [cmd params]
                              (swap! calls conj [cmd (:id params)])
                              (rx/of {:id c})))}
        (fn [done']
          (ths/run-store
           store done'
           [(dwa/set-active-board (cthi/id :board-a))
            (fn [state] (assoc-in state [:workspace-animation :playhead] 600))
            (dwa/paste-clip-at-playhead clip)]
           (fn [state]
             (t/is (= [[:clone-file-media-object a]] @calls))
             (t/is (= [[c 600 0]] (mapv (juxt :audio-id :start :lane) (clips-of state :board-a)))
                   "at the playhead, the copy of its audio")
             (t/is (= (into #{} (map :id) (clips-of state :board-a))
                      (dwaa/selected-clips (:workspace-animation state)))
                   "selected"))
           (after commit-of-clips?)))
        done))))

(t/deftest the-row-of-a-lane-mutes-and-locks-its-clips
  (t/async
    done
    (let [[clips file] (with-clips {:lane 0} {:lane 0 :start 1200 :locked true} {:lane 1})
          states       (fn [state k]
                         (let [timeline (timeline-of state :board-a)]
                           (mapv #(get (ctau/get-audio-clip timeline (:id %)) k) clips)))]
      (run-clip-edits
       done file
       [(dwaa/set-lane-muted 0 true) (dwaa/set-lane-locked 1 true)]
       (fn [state]
         (t/is (= [true nil nil] (states state :muted)) "the clips of its lane, but a locked one")
         (t/is (= [nil true true] (states state :locked))))
       (fn [state]
         (t/is (= [nil true nil] (states state :locked)) "one undo a step")
         (t/is (= [true nil nil] (states state :muted))))))))

(t/deftest the-row-of-a-lane-renames-its-file
  (t/async
    done
    (let [voice        (uuid/next)
          [clips file] (with-clips {:lane 0 :audio-id voice} {:lane 0 :audio-id voice :start 1200 :locked true}
                         {:lane 1})
          names        (fn [state]
                         (let [timeline (timeline-of state :board-a)]
                           (mapv #(:name (ctau/get-audio-clip timeline (:id %))) clips)))]
      (run-clip-edits
       done file
       [(dwaa/rename-lane 0 "Voice-over")]
       (fn [state]
         (t/is (= ["Voice-over" "Voice-over" "x"] (names state)) "all the clips of its file, a locked one too"))
       (fn [state]
         (t/is (= ["x" "x" "x"] (names state)) "one undo"))))))

(t/deftest the-group-of-the-lanes-mutes-and-locks-all
  (t/async
    done
    (let [[clips file] (with-clips {:lane 0} {:lane 1 :locked true} {:lane 2})
          states       (fn [state k]
                         (let [timeline (timeline-of state :board-a)]
                           (mapv #(get (ctau/get-audio-clip timeline (:id %)) k) clips)))]
      (run-clip-edits
       done file
       [(dwaa/set-all-muted true) (dwaa/set-all-locked true)]
       (fn [state]
         (t/is (= [true nil true] (states state :muted)) "all, but a locked one")
         (t/is (= [true true true] (states state :locked))))
       (fn [state]
         (t/is (= [nil true nil] (states state :locked)) "one undo a step"))))))

(t/deftest a-lane-moves-to-another-place
  (t/async
    done
    (let [[clips file] (with-clips {:lane 0} {:lane 1} {:lane 2})
          lanes        (fn [state]
                         (let [timeline (timeline-of state :board-a)]
                           (mapv #(:lane (ctau/get-audio-clip timeline (:id %))) clips)))]
      (run-clip-edits
       done file
       [(dwaa/move-lane 2 0)]
       (fn [state]
         (t/is (= [1 2 0] (lanes state)) "the last one first"))
       (fn [state]
         (t/is (= [0 1 2] (lanes state)) "one undo"))))))

(t/deftest the-lanes-fold-into-their-group
  (let [state (ptk/update (dwaa/toggle-audio-collapsed) {})]
    (t/is (true? (get-in state [:workspace-animation :audio-collapsed?])))
    (t/is (false? (get-in (ptk/update (dwaa/toggle-audio-collapsed) state)
                          [:workspace-animation :audio-collapsed?]))
          "and unfold")))

(t/deftest a-clip-splits-at-the-playhead
  (t/async
    done
    (let [[clip file] (with-clip)]
      (run-clip-edits
       done file
       [(dwa/set-playhead 400) (dwaa/split-clip (:id clip))]
       (fn [state]
         (t/is (= [[100 0 300] [400 300 700]]
                  (mapv (juxt :start :offset :duration) (ctau/audio-clips (timeline-of state :board-a))))
               "two clips of the same file, lined up"))
       (fn [state]
         (t/is (= [clip] (ctau/audio-clips (timeline-of state :board-a))) "one undo"))))))

(t/deftest every-clip-under-the-playhead-splits-in-one-step
  (t/async
    done
    (let [[clips file] (with-clips {:lane 0 :start 100 :duration 1000}
                         {:lane 1 :start 300 :duration 1000}
                         {:lane 2 :start 600 :duration 200})
          spans        #(mapv (juxt :start :duration) (ctau/audio-clips (timeline-of % :board-a)))]
      (run-clip-edits
       done file
       [(dwa/set-playhead 400) (dwaa/split-all-clips)]
       (fn [state]
         (t/is (= [[100 300] [400 700] [300 100] [400 900] [600 200]] (spans state))
               "the two under it, not the one after it"))
       (fn [state]
         (t/is (= (mapv (juxt :start :duration) clips) (spans state)) "one undo"))))))

(t/deftest the-selected-clips-are-edited-together
  (t/async
    done
    (let [[[a b c] file] (with-clips {:lane 0 :start 100 :duration 1000}
                           {:lane 1 :start 300 :duration 1000}
                           {:lane 2 :start 600 :duration 1000 :locked true})
          spans          #(mapv (juxt :start :duration) (ctau/audio-clips (timeline-of % :board-a)))]
      (run-clip-edits
       done file
       [(dwaa/select-clips (map :id [a b c]))
        (dwaa/nudge-selected-clips 100)
        (dwa/set-playhead 800)
        (dwaa/split-selected-clips)
        (dwaa/select-clips (map :id [a c]))
        (dwaa/delete-selected-clips)]
       (fn [state]
         (t/is (= [[800 400] [400 400] [800 600] [600 1000]] (spans state))
               "moved and cut together, the first part of the first deleted; the locked one stays")
         (t/is (= #{(:id c)} (dwaa/selected-clips (:workspace-animation state))) "and stays selected"))
       (fn [state]
         (t/is (= 5 (count (ctau/audio-clips (timeline-of state :board-a)))) "one undo a step"))))))

(t/deftest the-volume-and-mute-of-a-clip
  (t/async
    done
    (let [[clip file] (with-clip)
          id          (:id clip)]
      (run-clip-edits
       done file
       [(dwaa/set-clip-volume id 0.5) (dwaa/toggle-clip-muted id)]
       (fn [state]
         (let [stored (ctau/get-audio-clip (timeline-of state :board-a) id)]
           (t/is (= [0.5 true] ((juxt :volume :muted) stored)))))
       (fn [state]
         (t/is (= 0.5 (:volume (ctau/get-audio-clip (timeline-of state :board-a) id)))
               "one undo a step")
         (t/is (nil? (:volume (ctau/get-audio-clip (ctau/set-audio-clip-volume (timeline-of state :board-a) id 1) id)))
               "at full volume, no key"))))))

(t/deftest the-speed-of-a-clip
  (t/async
    done
    (let [[clip file] (with-clip)
          id          (:id clip)
          stored      #((juxt :speed :duration) (ctau/get-audio-clip (timeline-of % :board-a) id))]
      (run-clip-edits
       done file
       [(dwaa/set-clip-speed id 2)]
       (fn [state]
         (t/is (= [2 500] (stored state)) "the same part of its file in half the time"))
       (fn [state]
         (t/is (= [nil 1000] (stored state)) "one undo"))))))

(t/deftest a-clip-is-deleted
  (t/async
    done
    (let [[clip file] (with-clip)]
      (run-clip-edits
       done file
       [(dwaa/select-clip (:id clip)) (dwaa/delete-clip (:id clip))]
       (fn [state]
         (t/is (empty? (ctau/audio-clips (timeline-of state :board-a))))
         (t/is (nil? (get-in state [:workspace-animation :selected-clip]))))
       (fn [state]
         (t/is (= [clip] (ctau/audio-clips (timeline-of state :board-a)))))))))

(t/deftest the-timeline-fits-its-audio-in-one-step
  (t/async
    done
    (let [[_ file] (with-clip :start 500 :duration 2000)]
      (run-clip-edits
       done file
       [(dwaa/fit-timeline-to-audio)]
       (fn [state]
         (t/is (= 2500 (:duration (timeline-of state :board-a)))))
       (fn [state]
         (t/is (= 1000 (:duration (timeline-of state :board-a)))))))))

(t/deftest the-fades-of-a-clip-are-at-most-half-of-it
  (t/async
    done
    (let [[clip file] (with-clip)
          id          (:id clip)]
      (run-clip-edits
       done file
       [(dwaa/set-clip-fades id 200 900)]
       (fn [state]
         (t/is (= [200 500] ((juxt :fade-in :fade-out) (ctau/get-audio-clip (timeline-of state :board-a) id)))))
       (fn [state]
         (t/is (= [clip] (clips-of state :board-a)) "one undo"))))))

(t/deftest a-clip-nudges-before-zero-too
  (t/async
    done
    (let [[clip file] (with-clip)
          id          (:id clip)]
      (run-clip-edits
       done file
       [(dwaa/nudge-clip id 10) (dwaa/nudge-clip id 100) (dwaa/nudge-clip id -1000)]
       (fn [state]
         (t/is (= -790 (:start (ctau/get-audio-clip (timeline-of state :board-a) id)))))
       (fn [state]
         (t/is (= 210 (:start (ctau/get-audio-clip (timeline-of state :board-a) id)))
               "one undo a step"))))))

(t/deftest the-design-tab-removes-a-lane-of-clips
  (t/async
    done
    (let [[clip file] (with-clip :lane 2)
          id          (:id clip)]
      (run-clip-edits
       done file
       [(dwaa/remove-lane-clips 2)]
       (fn [state]
         (t/is (nil? (ctau/get-audio-clip (timeline-of state :board-a) id))))
       (fn [state]
         (t/is (some? (ctau/get-audio-clip (timeline-of state :board-a) id)) "one undo"))))))

(t/deftest a-locked-clip-stays-when-its-lane-is-removed
  (t/async
    done
    (let [[clip file] (with-clip :locked true)
          id          (:id clip)]
      (run-clip-edits
       done file
       [(dwaa/remove-lane-clips 0)]
       (fn [state]
         (t/is (some? (ctau/get-audio-clip (timeline-of state :board-a) id))))
       (fn [_] nil)))))

(t/deftest a-locked-clip-takes-no-edit-but-its-unlocking
  (t/async
    done
    (let [[clip file] (with-clip)
          id          (:id clip)]
      (run-clip-edits
       done file
       [(dwaa/select-clip id)
        (dwaa/toggle-clip-locked id)
        (dwaa/set-clip-volume id 0.5)
        (dwaa/toggle-clip-muted id)
        (dwaa/set-clip-fades id 100 100)
        (dwaa/nudge-clip id 10)
        (dwa/set-playhead 400)
        (dwaa/split-clip id)
        (dwaa/delete-clip id)]
       (fn [state]
         (t/is (= [(assoc clip :locked true)] (clips-of state :board-a)))
         (t/is (= id (get-in state [:workspace-animation :selected-clip]))
               "still selected, so the arrows do not move the board"))
       (fn [state]
         (t/is (= [clip] (clips-of state :board-a)) "the lock was the one step"))))))

(t/deftest the-export-keeps-whether-it-has-sound
  (t/async
    done
    (let [[_ file] (with-clip)
          store    (store-of file)
          board    (cthi/id :board-a)]
      (ths/run-store
       store done
       [(dwa/set-export board {:format :mp4 :audio false})]
       (fn [state]
         (t/is (= {:format :mp4 :audio false} (:export (timeline-of state :board-a))))
         (ptk/emit! store (dwa/set-export board {:audio true}))
         (t/is (true? (get-in (timeline-of @store :board-a) [:export :audio])) "and back"))))))
