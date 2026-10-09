;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.animation-style-test
  (:require
   [app.common.files.changes :as ch]
   [app.common.files.changes-builder :as pcb]
   [app.common.math :as mth]
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

(def ^:private pose
  "Where a layer rests, for what does not depend on it."
  {:x 0 :y 0 :width 100 :height 100 :rotation 0})

(defn- combine
  "What a new style takes from `animations` (see `ctas/animations->entries`)."
  [animations]
  (ctas/animations->entries {:keyframes [] :animations animations} pose))

(t/deftest combining-presets-and-uses-into-a-style
  (let [{:keys [board rect objects]} (board-and-rect)
        library  (uuid/next)
        slide-up (mapv #(assoc % :start 100) (:slide-up cta/builtin-styles))
        use      (ctas/make-use (three-style) library 300)
        before   (timeline-of board rect (conj slide-up use))

        {:keys [entries start]} (combine (animations before (:id rect)))
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
    (t/is (= {:error :too-many-entries :count 40} (combine uses)))
    (t/is (= {:error :no-entries :count 0} (combine [])))))

(t/deftest combining-leaves-out-hidden-and-locked-rows
  (let [library (uuid/next)
        hidden  #(assoc % :hidden true)
        locked  #(assoc % :locked true)
        result  (combine
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

        {:keys [entries start]} (combine (animations before sid))
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
        after   (ctas/sync-timeline before objects library {} nil)]
    (t/is (= [:scale :rotate :fade] (mapv :type presets)))
    (t/is (every? :locked presets) "with the flags of the use")
    (t/is (not-any? (set (map :id (:entries use))) (map :id presets)) "new ids")
    (t/is (= [:move :scale :rotate :fade] (mapv :type (animations after sid))) "in its place")
    (t/is (= (plays before objects sid) (plays after objects sid)))
    (t/is (identical? before (ctas/sync-timeline before objects library nil nil))
          "nothing changes while the styles are not known")))

(t/deftest sync-timeline-grows-the-duration-to-fit
  (let [sid     (uuid/next)
        library (uuid/next)
        style   (three-style)
        tl      (-> (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                    (cta/add-animation sid (ctas/make-use style library 400))
                    (assoc :audio-clips [{:id (uuid/next) :start 0 :duration 5000}]))
        resized #(assoc style :entries (mapv (fn [entry] (update entry :duration * %)) (:entries style)))
        twice   (ctas/sync-timeline tl {} library {(:id style) (resized 2)} nil)
        shorter (ctas/sync-timeline tl {} library {(:id style) (update (resized 1) :entries pop)} nil)]
    (t/is (= 900 (cta/animation-end (first (animations twice sid)))))
    (t/is (= 1000 (:duration twice)) "still within the duration")
    (let [tl    (assoc tl :duration 600)
          twice (ctas/sync-timeline tl {} library {(:id style) (resized 2)} nil)]
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
    (t/is (= ["One!" "Two" "One"] (names (ctas/sync-timeline tl {} lib-a renamed (:id one)))))
    (t/is (= ["One!" "Two!" "One"] (names (ctas/sync-timeline tl {} lib-a renamed nil))))))

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
    (t/is (not (ctas/drifted? (ctas/sync-timeline tl {} file edited nil) file edited)))))

(t/deftest detach-foreign-uses-keeps-the-local-ones
  (let [{:keys [board rect objects]} (board-and-rect)
        sid    (:id rect)
        file   (uuid/next)
        local  (ctas/make-use (three-style) file 0)
        before (timeline-of board rect [local (ctas/make-use (pop-style) (uuid/next) 400)])
        after  (ctas/detach-foreign-uses before {} file)]
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
      (let [{:keys [entries]} (combine [preset])]
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

;; DESIGN TOKENS

(t/deftest an-entry-keeps-the-tokens-of-its-settings
  (let [entry (cta/make-style-entry {:type :fade :start 0 :duration 300
                                     :applied-tokens {:duration "motion.fast"
                                                      :amount "opacity.half"
                                                      :offset-x "space.s"
                                                      :start 3}})]
    (t/is (= {:duration "motion.fast" :amount "opacity.half"} (:applied-tokens entry))
          "an offset belongs to a move only, and a name is a string")
    (t/is (= {:offset-x "space.s" :easing "ease.out"}
             (:applied-tokens (cta/make-style-entry {:type :move
                                                     :applied-tokens {:offset-x "space.s"
                                                                      :easing "ease.out"}}))))
    (t/is (not (contains? (cta/make-style-entry {:type :fade :applied-tokens {:offset-x "space.s"}})
                          :applied-tokens))
          "none left, none kept")
    (t/is (ctas/valid-animation-style? (pop-style :entries [entry])))
    (t/is (not (ctas/valid-animation-style?
                (pop-style :entries [(assoc entry :applied-tokens {:duration "not a name"})]))))))

(t/deftest the-tokens-of-an-entry-set-its-settings
  (let [entry  (cta/make-style-entry {:type :move :start 0 :duration 300
                                      :applied-tokens {:start "motion.delay"
                                                       :duration "motion.fast"
                                                       :easing "ease.out"
                                                       :offset-y "space.l"}})
        values {"motion.delay" 50.4 "motion.fast" 150 "ease.out" [0 0 0.58 1] "space.l" 24}]
    (t/is (= [50 150 {:type :bezier :curve [0 0 0.58 1]} 24]
             ((juxt :start :duration :easing :offset-y) (cta/resolve-animation-tokens entry values))))
    (t/is (= (:applied-tokens entry) (:applied-tokens (cta/resolve-animation-tokens entry values)))
          "the names stay")
    (t/testing "a token missing, or of a value that does not fit, leaves its setting"
      (t/is (= entry (cta/resolve-animation-tokens entry {})))
      (t/is (= 300 (:duration (cta/resolve-animation-tokens entry {"motion.fast" "fast"}))))
      (t/is (= (:easing entry) (:easing (cta/resolve-animation-tokens entry {"ease.out" [0 1 2]})))))
    (t/testing "the times of a curve stay within 0..1, a duration lasts 1 ms at least"
      (let [resolved (cta/resolve-animation-tokens entry {"ease.out" [-1 0 2 1] "motion.fast" 0})]
        (t/is (= [0 0 1 1] (get-in resolved [:easing :curve])))
        (t/is (= 1 (:duration resolved)))))))

(t/deftest an-easing-takes-a-bezier-or-a-spring-token
  (t/is (= [:cubic-bezier :spring] (cta/animation-token-types :fade :easing)))
  (let [entry (cta/make-style-entry {:type :scale :start 0 :duration 400
                                     :applied-tokens {:easing "spring.soft"}})]
    (t/is (= {:type :spring :stiffness 120 :damping 14 :mass 1}
             (:easing (cta/resolve-animation-tokens entry {"spring.soft" {:stiffness 120 :damping 14 :mass 1}}))))
    (t/is (= (:easing entry)
             (:easing (cta/resolve-animation-tokens entry {"spring.soft" {:stiffness 0 :damping 14 :mass 1}})))
          "a spring that does not fit leaves the easing")))

(t/deftest token-resolved-styles-are-the-ones-that-change
  (let [fast   (pop-style :name "Fast" :entries [{:type :fade :start 0 :duration 300
                                                  :applied-tokens {:duration "motion.fast"}}])
        plain  (pop-style :name "Plain")
        same   (pop-style :name "Same" :entries [{:type :fade :start 0 :duration 150
                                                  :applied-tokens {:duration "motion.fast"}}])
        styles {(:id fast) fast (:id plain) plain (:id same) same}
        [changed :as all] (ctas/token-resolved-styles styles {"motion.fast" 150})]
    (t/is (= 1 (count all)) "the style without tokens and the one already there stay")
    (t/is (= [(:id fast) 150] [(:id changed) (-> changed :entries first :duration)]))))

(t/deftest token-renamed-styles-take-the-new-names
  (let [style  (pop-style :entries [{:type :fade :start 0 :duration 300
                                     :applied-tokens {:duration "motion.fast"
                                                      :amount "opacity.half"}}])
        other  (pop-style :name "Other")
        rename #(if (= % "motion.fast") "motion.quick" %)
        [renamed :as all] (ctas/token-renamed-styles {(:id style) style (:id other) other} rename)]
    (t/is (= 1 (count all)))
    (t/is (= {:duration "motion.quick" :amount "opacity.half"}
             (-> renamed :entries first :applied-tokens)))))

(t/deftest uses-and-their-detached-presets-keep-the-token-names
  (let [style (pop-style :entries [{:type :fade :start 0 :duration 150
                                    :applied-tokens {:duration "motion.fast"}}])
        use   (ctas/make-use style (uuid/next) 0)]
    (t/is (= {:duration "motion.fast"} (-> use :entries first :applied-tokens)))
    (t/is (sm/validate cta/schema:animation use))
    (t/is (= [{:duration "motion.fast"}] (map :applied-tokens (ctas/detach-use use)))
          "a preset binds its settings too")))

;; KEYFRAME ENTRIES

(defn- x-style
  "A style of a move 40 px to the right over 600 ms, by keyframes, and of
  `entries`."
  [& entries]
  (pop-style :entries (into [{:type :keys :property :x :start 0 :duration 600
                              :keyframes [{:time 0 :value 0 :easing :linear} {:time 600 :value 40}]}]
                            entries)))

(defn- close-all?
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map mth/close? expected actual))))

(t/deftest a-keys-entry-is-made-valid
  (let [entry (cta/make-style-entry {:type :keys :property :x :start -5 :duration 600
                                     :keyframes [{:time 600 :value 40}
                                                 {:time 0 :value 0 :easing :ease-in}
                                                 {:time 300 :value "#ff0000"}
                                                 {:time 600.4 :value 50}
                                                 {:time 900 :value 1}]})]
    (t/is (= [:keys :x 0 600] ((juxt :type :property :start :duration) entry)))
    (t/is (= [[0 0] [600 50]] (mapv (juxt :time :value) (:keyframes entry)))
          "by time, one at each ms (the last one given), within its span, values that fit")
    (t/is (= :ease-in (get-in entry [:keyframes 0 :easing])))
    (t/is (not (contains? entry :index)))
    (t/is (= 0 (:index (cta/make-style-entry {:type :keys :property :fill-color
                                              :keyframes [{:time 0 :value "#ff0000"}]})))
          "a fill gets an index")
    (t/is (= 300 (:duration (cta/make-style-entry {:type :keys :property :x
                                                   :keyframes [{:time 0 :value 0} {:time 300 :value 4}]})))
          "as long as its keyframes")
    (t/is (ctas/valid-animation-style? (x-style {:type :fade :start 0 :duration 100})))))

(t/deftest a-keys-entry-holds-values-relative-to-its-layer
  (let [from  {:x 100 :y 20 :width 100 :height 50 :rotation 10}
        to    {:x 300 :y 0 :width 50 :height 50 :rotation 0}
        row   #(vector (cta/make-keyframe {:time 200 :property %1 :value %2})
                       (cta/make-keyframe {:time 800 :property %1 :value %3}))
        entry (cta/keyframes->keys-entry (row :x 100 140) from)]
    (t/is (= [200 600 [0 600]] [(:start entry) (:duration entry) (mapv :time (:keyframes entry))]))
    (t/is (close-all? [0 40] (mapv :value (:keyframes entry))) "positions as offsets")
    (t/is (close-all? [300 340] (mapv :value (cta/keys-entry-keyframes entry to))) "from where it rests")
    (t/is (= [200 800] (mapv :time (cta/keys-entry-keyframes entry to))))
    (let [entry (cta/keyframes->keys-entry (row :width 100 150) from)]
      (t/is (close-all? [1 1.5] (mapv :value (:keyframes entry))) "sizes as factors")
      (t/is (close-all? [50 75] (mapv :value (cta/keys-entry-keyframes entry to)))))
    (let [entry (cta/keyframes->keys-entry (row :rotation 10 100) from)]
      (t/is (close-all? [0 90] (mapv :value (:keyframes entry)))))
    (let [entry (cta/keyframes->keys-entry (row :opacity 1 0.5) from)]
      (t/is (= [1 0.5] (mapv :value (cta/keys-entry-keyframes entry to))) "the others as they are"))))

(t/deftest a-fill-entry-keeps-its-index
  (let [entry (cta/make-style-entry {:type :keys :property :fill-color :index 1
                                     :keyframes [{:time 0 :value "#ff0000"} {:time 400 :value "#0000ff"}]})]
    (t/is (= [[:fill-color 1 "#ff0000"] [:fill-color 1 "#0000ff"]]
             (mapv (juxt :property :index :value)
                   (cta/keys-entry-keyframes entry {:x 0 :y 0 :width 10 :height 10 :rotation 0}))))))

(t/deftest a-use-plays-its-keys-entries-on-its-layer
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (-> (timeline-of board rect [(ctas/make-use (x-style) (uuid/next) 100)])
                (assoc :duration 2000))
        xs  (fn [tl times]
              (let [resolved (cta/resolve-animations tl objects)]
                (mapv #(get-in (cta/values-at resolved %) [sid :x]) times)))]
    (t/is (cta/valid-timeline? tl))
    (t/is (close-all? [50 70 90] (xs tl [100 400 700])) "from where the rect rests, 50 px into the board")
    (t/is (close-all? [50 70 90] (xs (assoc-in tl [:tracks sid :animations 0 :duration] 1200) [100 700 1300]))
          "stretched to twice its length, twice as slow")))

(defn- x-times
  "The times of the x keyframes `sid` plays in `tl`."
  [tl objects sid]
  (->> (get-in (cta/resolve-animations tl objects) [:tracks sid :keyframes])
       (filter #(= :x (:property %)))
       (mapv :time)))

(t/deftest own-keyframes-win-in-their-span-and-presets-over-both
  (let [{:keys [board rect objects]} (board-and-rect)
        sid (:id rect)
        tl  (timeline-of board rect [(ctas/make-use (x-style) (uuid/next) 100)])]
    (t/testing "the entry plays outside the span of the own keyframes"
      (let [tl (-> tl
                   (cta/add-keyframe sid {:time 300 :property :x :value 500})
                   (cta/add-keyframe sid {:time 400 :property :x :value 520}))]
        (t/is (= [100 300 400 700] (x-times tl objects sid)))
        (t/is (= [100 300 500 700]
                 (x-times (cta/add-animation tl sid {:type :move :start 300 :duration 200 :offset-x 100 :offset-y 0})
                          objects sid))
              "a move over the same row wins in its span")))
    (t/testing "an edit on the layer shows at once"
      (let [tl (cta/add-keyframe tl sid {:time 400 :property :x :value 200})]
        (t/is (mth/close? 200 (get-in (cta/values-at (cta/resolve-animations tl objects) 400) [sid :x])))))
    (t/testing "own keyframes before the entry leave it whole"
      (let [tl (-> tl
                   (cta/add-keyframe sid {:time 0 :property :x :value 10})
                   (cta/add-keyframe sid {:time 50 :property :x :value 20}))]
        (t/is (= [0 50 100 700] (x-times tl objects sid)))))))

(t/deftest the-keyframes-of-a-keys-entry-bind-easing-tokens
  (let [style    (pop-style :entries [{:type :keys :property :rotation :start 0 :duration 400
                                       :keyframes [{:time 0 :value 0 :applied-tokens {:easing "ease.out"}}
                                                   {:time 400 :value 90}]}])
        resolved (ctas/resolve-style-tokens style {"ease.out" [0 0 0.58 1]})
        renamed  (first (ctas/token-renamed-styles {(:id style) style} #(if (= % "ease.out") "ease.exit" %)))]
    (t/is (= {:easing "ease.out"} (get-in style [:entries 0 :keyframes 0 :applied-tokens])))
    (t/is (= {:type :bezier :curve [0 0 0.58 1]} (get-in resolved [:entries 0 :keyframes 0 :easing])))
    (t/is (= {:easing "ease.exit"} (get-in renamed [:entries 0 :keyframes 0 :applied-tokens])))))

(t/deftest a-detached-use-gives-its-layer-the-keyframes-of-its-keys-entries
  (let [{:keys [board rect objects]} (board-and-rect)
        sid    (:id rect)
        use    (ctas/make-use (x-style {:type :fade :start 0 :duration 300}) (uuid/next) 100)
        before (timeline-of board rect [use])
        after  (update-in before [:tracks sid] ctas/detach-track-use (:id use) before objects)
        gone   (ctas/sync-timeline before objects (:style-ref-file use) {} nil)
        xs     #(get-in % [:tracks sid :keyframes])]
    (t/is (= [:fade] (mapv :type (animations after sid))) "its presets in its place")
    (t/is (= [100 700] (mapv :time (xs after))))
    (t/is (close-all? [50 90] (mapv :value (xs after))) "keyframes of the rect where it rests")
    (t/is (= (plays before objects sid) (plays after objects sid)))
    (t/is (cta/valid-timeline? after))
    (t/is (= (mapv (juxt :time :value) (xs after)) (mapv (juxt :time :value) (xs gone)))
          "and so when its style is gone")
    (t/is (= [:fade] (mapv :type (animations gone sid))))))

(t/deftest a-keys-entry-of-a-turn-takes-the-pivot
  (let [use   (ctas/make-use (pop-style :entries [{:type :keys :property :rotation :start 0 :duration 600
                                                   :keyframes [{:time 0 :value 0} {:time 600 :value 90}]}])
                             (uuid/next) 0)
        sid   (uuid/next)
        track {:shape-id (uuid/next) :keyframes [] :animations [use] :origin {:x 0 :y 0}}
        empty (cta/make-timeline {:board-id (uuid/next) :duration 1000})
        turns (-> empty
                  (cta/add-keyframe sid {:time 0 :property :rotation :value 0})
                  (cta/add-keyframe sid {:time 500 :property :rotation :value 45}))]
    (t/is (= {:x 0.0 :y 0.0} (get-in (:timeline (cta/paste-track empty sid track :merge)) [:tracks sid :origin])))
    (t/is (:pivot-kept? (cta/paste-track turns sid track :merge)) "a layer that turns keeps its own")))

;; SAVING AND UPDATING STYLES FROM A LAYER

(defn- with-rows
  "`timeline` with a rotation row on `sid` (0 at 100 ms, 90 at 500) and
  `f` of it."
  [timeline sid & {:keys [f] :or {f identity}}]
  (-> timeline
      (cta/add-keyframe sid {:time 100 :property :rotation :value 0 :easing :linear})
      (cta/add-keyframe sid {:time 500 :property :rotation :value 90})
      (f)))

(t/deftest saving-a-layer-takes-its-rows-of-keyframes
  (let [{:keys [board rect objects]} (board-and-rect)
        sid    (:id rect)
        before (with-rows (timeline-of board rect [(cta/make-animation {:type :fade :start 200 :duration 300})]) sid)
        pose   (cta/rest-pose before objects sid)
        track  (get-in before [:tracks sid])

        {:keys [entries start]} (ctas/animations->entries track pose)
        style  (pop-style :name "Turn in" :entries entries)
        use    (ctas/make-use style (uuid/next) start)
        after  (update-in before [:tracks sid] ctas/replace-with-use use)]
    (t/is (= [[:keys 0] [:fade 100]] (mapv (juxt :type :start) entries)) "a keyframe entry and a preset")
    (t/is (= 100 start))
    (t/is (ctas/valid-animation-style? style))
    (t/is (= [(:id use)] (mapv :id (animations after sid))) "one use")
    (t/is (empty? (get-in after [:tracks sid :keyframes])) "and no rotation keyframes")
    (t/is (= (plays before objects sid) (plays after objects sid)) "it plays as before")
    (t/is (cta/valid-timeline? after))))

(t/deftest saving-a-layer-that-starts-before-the-start
  (let [{:keys [board rect objects]} (board-and-rect)
        sid    (:id rect)
        before (-> (timeline-of board rect [(cta/make-animation {:type :fade :start -100 :duration 300})])
                   (cta/add-keyframe sid {:time -300 :property :rotation :value 0 :easing :linear})
                   (cta/add-keyframe sid {:time 200 :property :rotation :value 90}))
        pose   (cta/rest-pose before objects sid)

        {:keys [entries start]} (ctas/animations->entries (get-in before [:tracks sid]) pose)
        style  (pop-style :name "Early" :entries entries)
        use    (ctas/make-use style (uuid/next) start)
        after  (update-in before [:tracks sid] ctas/replace-with-use use)]
    (t/is (= -300 start) "the use starts where the layer did")
    (t/is (= [[:keys 0] [:fade 200]] (mapv (juxt :type :start) entries)) "in the time of the style, from 0")
    (t/is (ctas/valid-animation-style? style))
    (t/is (= (plays before objects sid) (plays after objects sid)) "it plays as before")))

(t/deftest looping-hidden-and-locked-rows-stay-on-the-layer
  (let [{:keys [board rect objects]} (board-and-rect)
        sid   (:id rect)
        row   (fn [tl property]
                (-> tl
                    (cta/add-keyframe sid {:time 0 :property property :value 0})
                    (cta/add-keyframe sid {:time 400 :property property :value 10})))
        tl    (-> (cta/make-timeline {:board-id (:id board) :duration 1000})
                  (row :x) (row :y) (row :rotation) (row :opacity)
                  (cta/toggle-loop sid :y nil)
                  (cta/toggle-slot-flag sid :hidden :rotation nil)
                  (cta/toggle-slot-flag sid :locked :opacity nil))
        track (get-in tl [:tracks sid])
        {:keys [entries start]} (ctas/animations->entries track (cta/rest-pose tl objects sid))
        after (ctas/replace-with-use track (ctas/make-use (pop-style :entries entries) (uuid/next) start))]
    (t/is (= [:x] (mapv :property entries)))
    (t/is (= #{:y :rotation :opacity} (into #{} (map :property) (:keyframes after))))))

(t/deftest saving-counts-the-keyframes-of-a-style
  (let [sid   (uuid/next)
        many  (reduce #(cta/add-keyframe %1 sid {:time %2 :property :x :value %2})
                      (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                      (range (inc cta/max-style-keyframes)))
        track (get-in many [:tracks sid])]
    (t/is (= {:error :too-many-keyframes :count (inc cta/max-style-keyframes)}
             (ctas/animations->entries track pose)))
    (t/is (= :too-many-keyframes (:error (ctas/entries-error track))))
    (t/is (nil? (ctas/entries-error (update track :keyframes (partial take 3)))))))

(t/deftest updating-a-style-from-a-stretched-use
  (let [{:keys [board rect objects]} (board-and-rect)
        sid     (:id rect)
        library (uuid/next)
        style   (x-style)
        use     (assoc (ctas/make-use style library 100) :duration 1200)
        tl      (with-rows (assoc (timeline-of board rect [use]) :duration 2000) sid)
        result  (ctas/update-from-track (get-in tl [:tracks sid]) (:id use) (cta/rest-pose tl objects sid)
                                        style library)
        style'  (:style result)
        after   (assoc-in tl [:tracks sid] (:track result))]
    (t/is (= [[:keys :x 0 600] [:keys :rotation 0 200]]
             (mapv (juxt :type :property :start :duration) (:entries style')))
          "in the time of the style: the use plays it twice as slow")
    (t/is (= [(:id use)] (mapv :id (animations after sid))) "the use keeps its id")
    (t/is (= [100 1200] ((juxt :start :duration) (first (animations after sid)))) "and its stretch")
    (t/is (empty? (get-in after [:tracks sid :keyframes])))
    (t/is (= (plays tl objects sid) (plays after objects sid)) "the layer plays as it did")
    (t/testing "the other uses follow by sync"
      (let [other  (ctas/make-use style library 0)
            synced (ctas/sync-use other style')]
        (t/is (= (:entries style') (:entries synced)))
        (t/is (= 600 (:duration synced)) "not stretched, as long as the style")))))

(t/deftest sync-and-drift-keep-keys-entries
  (let [library (uuid/next)
        style   (x-style {:type :fade :start 0 :duration 300})
        use     (ctas/make-use (assoc (pop-style) :id (:id style)) library 0)
        synced  (ctas/sync-use use style)
        tl      (cta/add-animation (cta/make-timeline {:board-id (uuid/next) :duration 1000}) (uuid/next) synced)]
    (t/is (= (:entries style) (:entries synced)))
    (t/is (not (ctas/drifted? tl library {(:id style) style})))
    (t/is (ctas/drifted? tl library {(:id style) (update style :entries pop)}))))

;; OWN KEYFRAMES UNDER KEYFRAME ENTRIES

(defn- with-own-x
  "`tl` with keyframes of x of `sid`, `[time value]` each, easing linear."
  [tl sid own]
  (reduce (fn [tl [time value]]
            (cta/add-keyframe tl sid {:time time :property :x :value value :easing :linear}))
          tl
          own))

(defn- x-keyframe-times
  [tl sid]
  (->> (get-in tl [:tracks sid :keyframes])
       (filter #(= :x (:property %)))
       (mapv :time)))

(t/deftest saving-a-row-under-an-entry-saves-what-it-plays
  (let [{:keys [board rect objects]} (board-and-rect)
        sid   (:id rect)
        lib   (uuid/next)
        style (x-style)
        use   (ctas/make-use style lib 100)
        base  (timeline-of board rect [use])
        pose  (cta/rest-pose base objects sid)]
    (doseq [[label own] [["before the entry" [[0 10] [50 20]]]
                         ["across its start" [[50 0] [200 80]]]
                         ["inside it" [[300 500] [400 520]]]
                         ["after it" [[800 30] [900 60]]]]]
      (t/testing label
        (let [before (with-own-x base sid own)
              track  (get-in before [:tracks sid])

              {:keys [entries start]} (ctas/animations->entries track pose)
              saved  (assoc-in before [:tracks sid]
                               (ctas/replace-with-use track (ctas/make-use (pop-style :entries entries) (uuid/next) start)))
              result (ctas/update-from-track track (:id use) pose style lib)
              again  (ctas/update-from-track (:track result) (:id use) pose (:style result) lib)]
          (t/is (= 1 (count entries)) "one entry for the row")
          (t/is (= (plays before objects sid) (plays saved objects sid)) "saved as a style, it plays as before")
          (t/is (= (plays before objects sid) (plays (assoc-in before [:tracks sid] (:track result)) objects sid))
                "and so when the style is updated from it")
          (t/is (= 1 (count (:entries (:style again)))) "a second update keeps one entry"))))))

(t/deftest detaching-keeps-the-own-keyframes-and-the-hidden-rows
  (let [{:keys [board rect objects]} (board-and-rect)
        sid   (:id rect)
        style (x-style {:type :keys :property :y :start 0 :duration 300
                        :keyframes [{:time 0 :value 0} {:time 300 :value 20}]})
        own   [[300 500] [400 520]]]
    (t/testing "a hidden use played nothing: it only gives the rows the layer has none of, hidden"
      (let [use    (assoc (ctas/make-use style (uuid/next) 100) :hidden true)
            before (with-own-x (timeline-of board rect [use]) sid own)
            after  (update-in before [:tracks sid] ctas/detach-track-use (:id use) before objects)]
        (t/is (= [300 400] (x-keyframe-times after sid)) "the own row keeps its keyframes")
        (t/is (= #{:y} (get-in after [:tracks sid :hidden])) "and stays visible")
        (t/is (= (plays before objects sid) (plays after objects sid)))
        (t/testing "and so when its style is gone"
          (let [gone (ctas/sync-timeline before objects (:style-ref-file use) {} nil)]
            (t/is (= [300 400] (x-keyframe-times gone sid)))
            (t/is (= (plays before objects sid) (plays gone objects sid)))))))
    (t/testing "a visible use over a row the user hid: the row keeps its keyframes and stays hidden"
      (let [use    (ctas/make-use style (uuid/next) 100)
            before (-> (with-own-x (timeline-of board rect [use]) sid own)
                       (cta/toggle-slot-flag sid :hidden :x nil))
            after  (update-in before [:tracks sid] ctas/detach-track-use (:id use) before objects)]
        (t/is (= [100 300 400 700] (x-keyframe-times after sid)))
        (t/is (contains? (get-in after [:tracks sid :hidden]) :x))
        (t/is (cta/valid-timeline? after))))
    (t/testing "a visible use over a visible row plays as before"
      (let [use    (ctas/make-use style (uuid/next) 100)
            before (with-own-x (timeline-of board rect [use]) sid own)
            after  (update-in before [:tracks sid] ctas/detach-track-use (:id use) before objects)]
        (t/is (= [100 300 400 700] (x-keyframe-times after sid)))
        (t/is (= (plays before objects sid) (plays after objects sid)))))))
