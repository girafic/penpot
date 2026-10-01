;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.animation-style-test
  (:require
   [app.common.files.changes :as ch]
   [app.common.files.changes-builder :as pcb]
   [app.common.schema :as sm]
   [app.common.schema.generators :as sg]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.time :as ct]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.types.file :as ctf]
   [app.common.types.shape :as cts]
   [app.common.types.text-units :as tu]
   [app.common.uuid :as uuid]
   [clojure.test :as t]
   [common-tests.types.shape-decode-encode-test :refer [json-roundtrip]]))

(t/use-fixtures :each thi/test-fixture)

(defn- fades
  "`n` fades in, one after the other."
  [n]
  (mapv #(hash-map :type :fade :start (* 100 %) :duration 100) (range n)))

(defn- pop-style
  [& {:as params}]
  (ctas/make-animation-style
   (merge {:name "Pop"
           :path "Enter"
           :entries [{:type :fade :start 0 :duration 300}
                     {:type :scale :start 100 :duration 400 :easing cta/overshoot-easing}]}
          params)))

(defn- styles
  [file]
  (get-in file [:data :animation-styles]))

(defn- changes-of
  "Empty changes over the data of `file`, its library data too."
  [file]
  (-> (pcb/empty-changes nil)
      (pcb/with-library-data (:data file))))

(defn- without-time
  [style]
  (dissoc style :modified-at))

(t/deftest make-animation-style-makes-the-entries-valid
  (let [style (pop-style :entries [{:type :fade :start 0 :duration 300 :hidden true :color "red"}])
        entry (first (:entries style))]
    (t/is (ctas/valid-animation-style? style))
    (t/is (= ["Pop" "Enter"] [(:name style) (:path style)]))
    (t/is (= [:fade :in 0 300] [(:type entry) (:direction entry) (:start entry) (:duration entry)]))
    (t/is (not-any? #(contains? entry %) [:hidden :color]))
    (t/is (= (:id entry) (-> style ctas/make-animation-style :entries first :id))
          "making it again keeps the ids of its entries")))

(t/deftest a-style-holds-1-to-32-entries
  (t/is (ctas/valid-animation-style? (pop-style :entries (fades 1))))
  (t/is (ctas/valid-animation-style? (pop-style :entries (fades 32))))
  (t/is (thrown? #?(:cljs js/Error :clj Exception) (ctas/check-animation-style (pop-style :entries []))))
  (t/is (thrown? #?(:cljs js/Error :clj Exception) (ctas/check-animation-style (pop-style :entries (fades 33)))))
  (t/is (not (ctas/valid-animation-style? (assoc (pop-style) :entries [(cta/make-animation {:type :style})])))
        "an entry is a preset, never a use"))

(t/deftest add-modify-and-delete-a-style
  (let [file    (thf/sample-file :file1)
        style   (pop-style)
        sid     (:id style)

        added   (changes-of file)
        added   (pcb/add-animation-style added style)
        file1   (thf/apply-changes file added)

        renamed (pcb/update-animation-style (changes-of file1) (assoc style :name "Pop in" :path nil))
        file2   (thf/apply-changes file1 renamed)

        deleted (pcb/delete-animation-style (changes-of file2) sid)
        file3   (thf/apply-changes file2 deleted)]

    (t/testing "add"
      (t/is (= style (without-time (ctas/get-animation-style (:data file1) sid))))
      (t/is (= [sid] (map :id (ctas/animation-styles-seq (:data file1)))))
      (t/is (empty? (styles (thf/apply-undo-changes file1 added)))))

    (t/testing "modify, the whole style"
      (t/is (= (assoc style :name "Pop in" :path nil)
               (without-time (ctas/get-animation-style (:data file2) sid))))
      (t/is (= (without-time (ctas/get-animation-style (:data file1) sid))
               (without-time (ctas/get-animation-style (:data (thf/apply-undo-changes file2 renamed)) sid)))))

    (t/testing "delete"
      (t/is (= {} (styles file3)))
      (t/is (= (without-time (ctas/get-animation-style (:data file2) sid))
               (without-time (ctas/get-animation-style (:data (thf/apply-undo-changes file3 deleted)) sid)))))

    (t/testing "a change for a style that is not there does nothing"
      (t/is (= (:data file)
               (ch/process-changes (:data file)
                                   [{:type :mod-animation-style :animation-style style}
                                    {:type :del-animation-style :id sid}]))))))

(t/deftest modified-at-changes-on-add-and-update
  (let [file  (thf/sample-file :file1)
        style (assoc (pop-style) :modified-at (ct/inst 0))
        file1 (thf/apply-changes file (pcb/add-animation-style (changes-of file) style))
        at1   (:modified-at (ctas/get-animation-style (:data file1) (:id style)))
        file2 (thf/apply-changes file1 (pcb/update-animation-style (changes-of file1)
                                                                   (assoc style :name "Pop in")))
        at2   (:modified-at (ctas/get-animation-style (:data file2) (:id style)))]
    (t/is (not= (ct/inst 0) at1))
    (t/is (not= (ct/inst 0) at2))
    (t/is (some? at2))))

(t/deftest plugin-data-of-a-style
  (let [file  (thf/sample-file :file1)
        style (pop-style)
        file1 (thf/apply-changes file (pcb/add-animation-style (changes-of file) style))
        data  (ch/process-changes (:data file1)
                                  [{:type :set-plugin-data :object-type :animation-style
                                    :object-id (:id style) :namespace :plugin :key "k" :value "v"}])]
    (t/is (= {:plugin {"k" "v"}} (:plugin-data (ctas/get-animation-style data (:id style)))))))

(t/deftest file-data-holds-styles
  (let [file  (thf/sample-file :file1)
        style (pop-style)
        data  (ctas/add-animation-style (:data file) style)]
    (t/is (= data (ctf/check-file-data data)))
    (t/is (thrown? #?(:cljs js/Error :clj Exception)
                   (ctf/check-file-data (assoc-in data [:animation-styles (:id style) :entries] []))))))

(t/deftest style-changes-json-encode-decode
  (let [style   (assoc (pop-style :description "Pops in") :modified-at (ct/inst 1000))
        changes [{:type :add-animation-style :animation-style style}
                 {:type :mod-animation-style :animation-style (assoc style :name "Pop in")}
                 {:type :set-plugin-data :object-type :animation-style :object-id (:id style)
                  :namespace :plugin :key "k" :value "v"}
                 {:type :del-animation-style :id (:id style)}]
        encode  (sm/encoder ch/schema:changes (sm/json-transformer))
        decode  (sm/decoder ch/schema:changes (sm/json-transformer))]
    (t/is (ch/check-changes changes))
    (t/is (= changes (-> changes encode json-roundtrip decode))))
  (let [encode (sm/encoder ctas/schema:animation-style (sm/json-transformer))
        decode (sm/decoder ctas/schema:animation-style (sm/json-transformer))]
    ;; in JS an instant goes to JSON without its milliseconds, so the
    ;; change set above has whole seconds
    (t/is (every? #(= % (-> % encode json-roundtrip decode))
                  (map #(dissoc % :modified-at) (sg/sample ctas/schema:animation-style {:size 50}))))))

;; USES OF STYLES

(defn- board-and-rect
  "A board at 100,100 with a rect at 50,100 inside it."
  []
  (let [board (cts/setup-shape {:type :frame :x 100 :y 100 :width 800 :height 600})
        rect  (cts/setup-shape {:type :rect :x 150 :y 200 :width 100 :height 50})
        bid   (:id board)]
    {:board board
     :rect rect
     :objects {bid board
               (:id rect) (assoc rect :frame-id bid :parent-id bid)}}))

(defn- timeline-of
  "A timeline of `board`, a second long, with `animations` on `rect`."
  [board rect animations]
  (reduce #(cta/add-animation %1 (:id rect) %2)
          (cta/make-timeline {:board-id (:id board) :duration 1000})
          animations))

(defn- plays
  "What `timeline` shows of `shape-id` every 10 ms, to its end."
  [timeline objects shape-id]
  (let [resolved (cta/resolve-animations timeline objects)]
    (mapv #(get (cta/values-at resolved %) shape-id) (range 0 (inc (:duration timeline)) 10))))

(defn- three-style
  "A style of a scale in, a turn and a fade out, 300 ms long."
  [& {:as params}]
  (pop-style (merge {:name "Three"
                     :entries [{:type :scale :start 0 :duration 200}
                               {:type :rotate :start 50 :duration 200}
                               {:type :fade :direction :out :start 100 :duration 200}]}
                    params)))

(defn- animations
  [timeline shape-id]
  (get-in timeline [:tracks shape-id :animations]))

(t/deftest combining-presets-and-uses-into-a-style
  (let [{:keys [board rect objects]} (board-and-rect)
        library  (uuid/next)
        slide-up (mapv #(assoc % :start 100) (:slide-up cta/builtin-styles))
        use      (ctas/make-use (three-style) library 300)
        before   (timeline-of board rect (conj slide-up use))

        {:keys [entries start]} (ctas/animations->entries (animations before (:id rect)))
        style    (pop-style :name "Card enter" :entries entries)
        after    (timeline-of board rect [(ctas/make-use style (uuid/next) start)])]
    (t/is (= 5 (count entries)))
    (t/is (= 100 start))
    (t/is (= [0 0 200 250 300] (mapv :start entries)))
    (t/is (not-any? (set (map :id (:entries use))) (map :id entries)) "new ids")
    (t/is (ctas/valid-animation-style? style))
    (t/is (= (plays before objects (:id rect)) (plays after objects (:id rect)))
          "a use of it plays as the layer did")))

(t/deftest combining-at-most-32-entries
  (let [library (uuid/next)
        twenty  (pop-style :entries (fades 20))
        uses    [(ctas/make-use twenty library 0) (ctas/make-use twenty library 0)]]
    (t/is (= {:error :too-many-entries :count 40} (ctas/animations->entries uses)))
    (t/is (= {:error :no-entries :count 0} (ctas/animations->entries [])))))

(t/deftest combining-leaves-out-hidden-and-locked-rows
  (let [library (uuid/next)
        hidden  #(assoc % :hidden true)
        locked  #(assoc % :locked true)
        result  (ctas/animations->entries
                 [(cta/make-animation {:type :fade :start 100})
                  (hidden (cta/make-animation {:type :move :start 0}))
                  (hidden (ctas/make-use (three-style) library 0))
                  (locked (cta/make-animation {:type :rotate :start 0}))
                  (locked (ctas/make-use (three-style) library 0))])]
    (t/is (= [[:fade 0]] (mapv (juxt :type :start) (:entries result))))
    (t/is (= 100 (:start result)))))

(t/deftest a-new-use-takes-the-place-of-what-it-stands-for
  (let [{:keys [board rect objects]} (board-and-rect)
        sid    (:id rect)
        fade   (cta/make-animation {:type :fade :start 100 :duration 300})
        locked (cta/make-animation {:type :scale :direction :out :start 250 :duration 300
                                    :amount 2 :locked true})
        scale  (cta/make-animation {:type :scale :start 200 :duration 400})
        hidden (cta/make-animation {:type :rotate :start 0 :hidden true})
        before (timeline-of board rect [fade locked scale hidden])

        {:keys [entries start]} (ctas/animations->entries (animations before sid))
        use    (ctas/make-use (pop-style :entries entries) (uuid/next) start)
        after  (update-in before [:tracks sid] ctas/replace-with-use use)]
    (t/is (= [(:id use) (:id locked) (:id hidden)] (mapv :id (animations after sid)))
          "at the row of the first one, the locked and hidden rows in place")
    (t/is (= (plays before objects sid) (plays after objects sid)))
    (t/is (cta/valid-timeline? after))))

(t/deftest sync-follows-the-style
  (let [library (uuid/next)
        style   (three-style)
        longer  (assoc style :name "Three slow" :entries (mapv #(update % :duration * 4) (:entries style)))
        use     (ctas/make-use style library 100)]
    (t/testing "a use not stretched takes the new length"
      (let [synced (ctas/sync-use use longer)]
        (t/is (= [(:id use) "Three slow" 900 100]
                 ((juxt :id :name :duration :start) synced)))
        (t/is (= (:entries longer) (:entries synced)))))
    (t/testing "a stretched one keeps its length"
      (let [synced (ctas/sync-use (assoc use :duration 1000) longer)]
        (t/is (= 1000 (:duration synced)))
        (t/is (= (:entries longer) (:entries synced)))))
    (t/testing "nothing new, nothing changes"
      (t/is (= use (ctas/sync-use use style))))))

(t/deftest a-use-of-a-style-that-is-gone-turns-into-its-presets
  (let [{:keys [board rect objects]} (board-and-rect)
        sid     (:id rect)
        library (uuid/next)
        use     (assoc (ctas/make-use (three-style) library 100) :duration 600 :locked true)
        before  (timeline-of board rect [(cta/make-animation {:type :move :start 0}) use])
        presets (ctas/sync-use use nil)
        after   (ctas/sync-timeline before library {} nil)]
    (t/is (= [:scale :rotate :fade] (mapv :type presets)))
    (t/is (every? :locked presets) "with the flags of the use")
    (t/is (not-any? (set (map :id (:entries use))) (map :id presets)) "new ids")
    (t/is (= [:move :scale :rotate :fade] (mapv :type (animations after sid))) "in its place")
    (t/is (= (plays before objects sid) (plays after objects sid)))
    (t/is (identical? before (ctas/sync-timeline before library nil nil))
          "nothing changes while the styles are not known")))

(t/deftest sync-timeline-grows-the-duration-to-fit
  (let [sid     (uuid/next)
        library (uuid/next)
        style   (three-style)
        tl      (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                    (cta/add-animation sid (ctas/make-use style library 400))
                    (assoc :audio-clips [{:id (uuid/next) :start 0 :duration 5000}]))
        resized #(assoc style :entries (mapv (fn [entry] (update entry :duration * %)) (:entries style)))
        twice   (ctas/sync-timeline tl library {(:id style) (resized 2)} nil)
        shorter (ctas/sync-timeline tl library {(:id style) (update (resized 1) :entries pop)} nil)]
    (t/is (= 900 (cta/animation-end (first (animations twice sid)))))
    (t/is (= 1000 (:duration twice)) "still within the duration")
    (let [tl    (assoc tl :duration 600)
          twice (ctas/sync-timeline tl library {(:id style) (resized 2)} nil)]
      (t/is (= 900 (:duration twice)) "to the end of the last use, never to the audio")
      (t/is (= (:audio-clips tl) (:audio-clips twice))))
    (t/is (= 1000 (:duration shorter)) "never shorter")
    (t/is (= 650 (cta/animation-end (first (animations shorter sid)))))))

(t/deftest sync-timeline-leaves-other-styles-alone
  (let [sid     (uuid/next)
        lib-a   (uuid/next)
        lib-b   (uuid/next)
        one     (three-style :name "One")
        two     (three-style :name "Two")
        renamed {(:id one) (assoc one :name "One!") (:id two) (assoc two :name "Two!")}
        tl      (-> (cta/make-timeline {:board-id (uuid/next)})
                    (cta/add-animation sid (ctas/make-use one lib-a 0))
                    (cta/add-animation sid (ctas/make-use two lib-a 0))
                    (cta/add-animation sid (ctas/make-use one lib-b 0)))
        names   #(mapv :name (animations % sid))]
    (t/is (= ["One!" "Two" "One"] (names (ctas/sync-timeline tl lib-a renamed (:id one)))))
    (t/is (= ["One!" "Two!" "One"] (names (ctas/sync-timeline tl lib-a renamed nil))))))

(t/deftest drifted-finds-uses-a-sync-missed
  (let [file    (uuid/next)
        sid     (uuid/next)
        text    (uuid/next)
        style   (three-style)
        styles  {(:id style) style}
        use     (ctas/make-use style file 0)
        tl      (-> (cta/make-timeline {:board-id (uuid/next)})
                    (cta/add-animation sid use)
                    (cta/add-animation text (ctas/make-use style file 300)))
        edited  {(:id style) (update-in style [:entries 0 :duration] + 100)}]
    (t/is (= (:entries style) (:entries use) (:entries (first (animations tl text))))
          "a use keeps every entry, on a rect as on a text")
    (t/is (not (ctas/drifted? tl file styles)))
    (t/is (ctas/drifted? tl file edited) "the style changed, its uses did not")
    (t/is (ctas/drifted? tl file {(:id style) (assoc style :name "Renamed")}))
    (t/is (ctas/drifted? tl file {}) "its style is gone")
    (t/is (not (ctas/drifted? tl (uuid/next) {})) "uses of a library are not checked")
    (t/is (not (ctas/drifted? (ctas/sync-timeline tl file edited nil) file edited)))))

(t/deftest detach-foreign-uses-keeps-the-local-ones
  (let [{:keys [board rect objects]} (board-and-rect)
        sid    (:id rect)
        file   (uuid/next)
        local  (ctas/make-use (three-style) file 0)
        before (timeline-of board rect [local (ctas/make-use (pop-style) (uuid/next) 400)])
        after  (ctas/detach-foreign-uses before file)]
    (t/is (= [:style :fade :scale] (mapv :type (animations after sid))))
    (t/is (= local (first (animations after sid))))
    (t/is (= (plays before objects sid) (plays after objects sid)))))

(t/deftest used-styles-changed-since-a-date
  (let [library (uuid/next)
        since   (ct/inst 10000)
        changed (assoc (three-style :name "Changed") :modified-at (ct/inst 20000))
        same    (assoc (three-style :name "Same") :modified-at (ct/inst 5000))
        gone    (three-style :name "Gone")
        board   (uuid/next)
        sid     (uuid/next)
        page    {:id (uuid/next)
                 :timelines {board (-> (cta/make-timeline {:board-id board})
                                       (cta/add-animation sid (ctas/make-use changed library 0))
                                       (cta/add-animation sid (ctas/make-use same library 0))
                                       (cta/add-animation sid (ctas/make-use gone library 0))
                                       (cta/add-animation sid (ctas/make-use changed (uuid/next) 0)))}}
        lib     {:id library :data {:animation-styles {(:id changed) changed (:id same) same}}}
        found   #(set (map (juxt :asset-id :asset-type :shape-id :container-id) %))]
    (t/is (= #{[(:id changed) :animation-style sid (:id page)]
               [(:id gone) :animation-style sid (:id page)]}
             (found (ctas/used-styles-changed-since page lib since))))
    (t/is (= #{} (found (ctas/used-styles-changed-since page (dissoc lib :data) since)))
          "without its data, a library tells nothing")))

(t/deftest remap-style-refs-links-uses-to-the-file
  (let [library (uuid/next)
        file    (uuid/next)
        board   (uuid/next)
        sid     (uuid/next)
        one     (three-style)
        two     (three-style)
        tls     {board (-> (cta/make-timeline {:board-id board})
                           (cta/add-animation sid (ctas/make-use one library 0))
                           (cta/add-animation sid (ctas/make-use two library 0)))}
        tls'    (ctas/remap-style-refs tls library file #{(:id one)})]
    (t/is (= [file library] (mapv :style-ref-file (get-in tls' [board :tracks sid :animations]))))))

(t/deftest a-file-without-its-libraries-plays-their-styles-as-presets
  (let [{:keys [board rect objects]} (board-and-rect)
        file-id (uuid/next)
        page-id (uuid/next)
        sid     (:id rect)
        local   (ctas/make-use (three-style) file-id 0)
        before  (timeline-of board rect [local (ctas/make-use (pop-style) (uuid/next) 400)])
        file    {:id file-id
                 :data {:pages-index {page-id {:id page-id
                                               :objects objects
                                               :timelines {(:id board) before}}}}}
        after   (get-in (ctf/detach-external-references file file-id)
                        [:data :pages-index page-id :timelines (:id board)])]
    (t/is (= [:style :fade :scale] (mapv :type (animations after sid))))
    (t/is (= local (first (animations after sid))) "its own styles stay linked")
    (t/is (= (plays before objects sid) (plays after objects sid)))))

;; TEXT SPLITS

(defn- split-settings
  [animation]
  (mapv animation [:split :spread :order :seed]))

(t/deftest a-split-entry-keeps-its-settings
  (let [entry  (cta/make-style-entry {:type :fade :split :word :spread 0.8 :order :random :seed 42
                                      :start 0 :duration 600})
        style  (pop-style :entries [entry])
        preset (cta/make-animation (assoc entry :start 200))]
    (t/is (= [:word 0.8 :random 42] (split-settings entry)))
    (t/is (ctas/valid-animation-style? style))
    (t/testing "saved as a style, with a new id"
      (let [{:keys [entries]} (ctas/animations->entries [preset])]
        (t/is (= [[:word 0.8 :random 42]] (mapv split-settings entries)))
        (t/is (not= (:id preset) (:id (first entries))))))
    (t/testing "synced into a use"
      (let [synced (ctas/sync-use (ctas/make-use (pop-style) (uuid/next) 0) style)]
        (t/is (= [[:word 0.8 :random 42]] (mapv split-settings (:entries synced))))))))

(t/deftest a-random-entry-gets-a-seed-and-keeps-it
  (let [entry (cta/make-style-entry {:type :fade :split :char :order :random :start 0 :duration 300})
        use   (ctas/make-use (pop-style :entries [entry]) (uuid/next) 100)
        order #(mapv :start (tu/unit-animations (first (cta/expand-style use)) 8))]
    (t/is (int? (:seed entry)))
    (t/is (= (:seed entry) (:seed (cta/make-style-entry entry))) "kept")
    (t/is (= (:seed entry) (:seed (first (:entries use)))) "and by the uses")
    (t/is (= (order) (order)) "which play the units in the same order")))
