;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns frontend-tests.data.animation-playback-test
  (:require
   [app.common.geom.point :as gpt]
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.animation :as dwa]
   [app.main.data.workspace.edition :as dwe]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.pages :as dwp]
   [app.render-wasm.modifiers :as wasm.modifiers]
   [app.util.audio :as audio]
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [cljs.test :as t :include-macros true]
   [potok.v2.core :as ptk]))

(defn- steps
  "How far playback moves on in frames `dts` ms apart."
  [dts]
  (first (reduce (fn [[steps carry] dt]
                   (let [[step carry] (dwa/frame-step carry dt)]
                     [(conj steps step) carry]))
                 [[] 0]
                 dts)))

(t/deftest playback-keeps-to-the-time-that-passes
  (t/testing "a slow frame moves on by all the time it took"
    (t/is (= [16 210 16] (steps [16 210 16]))))
  (t/testing "the fractions of a ms carry over"
    (let [steps (steps (repeat 60 16.5))]
      (t/is (every? #{16 17} steps))
      (t/is (= 990 (reduce + steps)))))
  (t/testing "back in a hidden tab, it goes on where it was"
    (t/is (= [500] (steps [60000]))))
  (t/is (= [0] (steps [-5])) "a clock going back does not")
  (t/testing "keeping time with its sound, it catches up"
    (t/is (= [2000 0] (dwa/frame-step 0 2000 false)))
    (t/is (= [500 0] (dwa/frame-step 0 2000 true)))))

;; SOUND

(defn- sounding
  "A fake running `AudioContext` that writes down in `log` the sources it
  starts and stops, with a clip playing in it from `follow!`."
  [log]
  (let [ctx #js {:state "running"
                 :currentTime 10
                 :destination #js {}
                 :createBufferSource (fn []
                                       #js {:connect (fn [_]) :disconnect (fn [])
                                            :playbackRate #js {:value 1}
                                            :start (fn [& _] (swap! log conj :start))
                                            :stop (fn [] (swap! log conj :stop))})
                 :createGain (fn []
                               #js {:gain #js {:setValueAtTime (fn [_ _]) :linearRampToValueAtTime (fn [_ _])}
                                    :connect (fn [_]) :disconnect (fn [])})}
        id  (uuid/next)
        n   4800]
    (audio/use-context! ctx)
    (audio/remember! id #js {:duration 0.1 :length n :sampleRate 48000 :numberOfChannels 1
                             :getChannelData (fn [_] (js/Float32Array. n))})
    (audio/follow! (ctau/add-audio-clip (cta/make-timeline {:board-id (uuid/next) :duration 1000})
                                        {:audio-id id :name "x" :source-duration 100 :duration 100})
                   0 1 true)))

(t/deftest pausing-and-leaving-the-page-stop-the-sound
  (doseq [[label event] [["pause" (dwa/pause)]
                         ["stop and clear-preview, through pause" (dwa/pause)]
                         ["page change" (dwp/finalize-page (uuid/next) (uuid/next))]]]
    (let [log (atom [])]
      (sounding log)
      (t/is (= [:start] @log) label)
      (ptk/effect event {} (rx/empty))
      (t/is (= [:start :stop] @log) label)))
  (audio/use-context! nil))

(t/deftest the-sound-stays-muted-once-muted
  (let [before (get @storage/user ::dwa/audio-muted)
        state  (ptk/update (dwa/toggle-audio-muted) {})]
    (ptk/effect (dwa/toggle-audio-muted) state (rx/empty))
    (t/is (true? (dwa/audio-muted? (:workspace-animation state))))
    (t/is (true? (dwa/audio-muted? {})) "the next time too")
    (swap! storage/user assoc ::dwa/audio-muted before)))

(t/deftest a-board-with-only-clips-plays
  (let [board (cts/setup-shape {:type :frame :x 0 :y 0 :width 100 :height 100})
        id    (:id board)
        page  {:objects {uuid/zero {:id uuid/zero :type :frame :shapes [id]}
                         id (assoc board :parent-id uuid/zero :frame-id uuid/zero)}
               :timelines {id (ctau/add-audio-clip (cta/make-timeline {:board-id id})
                                                   {:audio-id (uuid/next) :name "x"
                                                    :source-duration 1000 :duration 1000})}}]
    (t/is (= #{id} (dwa/animated-board-ids page {})) "its title shows the play button")
    (t/is (= #{} (dwa/animated-board-ids (assoc-in page [:timelines id :audio-clips] {}) {})))))

(t/deftest the-dock-rests-while-playing
  (let [anim {:playing? true :playhead 1234 :play-start 500 :direction -1
              :preview {:time 1234} :selected-kfs #{:a}}]
    (t/is (= {:playing? true :playhead 500 :play-start 500 :selected-kfs #{:a}}
             (dwa/dock-state anim))
          "at the playhead it started from")
    (t/is (= (dwa/dock-state anim)
             (dwa/dock-state (assoc anim :playhead 2000 :preview {:time 2000})))
          "the same as it plays on")
    (t/is (= {:playing? false :playhead 1234}
             (dwa/dock-state (-> anim
                                 (assoc :playing? false)
                                 (dissoc :play-start :selected-kfs))))
          "and where it is once it stops")))

(t/deftest the-panels-show-where-playback-started
  (let [state {:workspace-animation {:playing? true :playhead 1234 :play-start 500}}]
    (t/is (= 500 (dwa/shown-playhead state)))
    (t/is (= 1234 (dwa/shown-playhead (assoc-in state [:workspace-animation :playing?] false))))
    (t/is (= 0 (dwa/shown-playhead {})))))

(t/deftest only-the-board-that-plays-shows-its-animation
  (let [file-id (uuid/next)
        page-id (uuid/next)
        a       (uuid/next)
        b       (uuid/next)
        state   {:current-file-id file-id
                 :current-page-id page-id
                 :workspace-layout #{:animation-timeline}
                 :workspace-animation {:preview {:board-id a :time 500}}
                 :files {file-id {:data {:pages-index {page-id {:timelines {a {:board-id a}
                                                                            b {:board-id b}}}}}}}}]
    (t/is (= {:timelines {a {:board-id a}} :board-id a :time 500}
             (dsh/lookup-animation-preview state))
          "the other boards stay as they are")
    (t/is (nil? (dsh/lookup-animation-preview (update state :workspace-layout disj :animation-timeline)))
          "none outside motion mode")))

(t/deftest each-board-keeps-its-playhead
  (let [a      (uuid/next)
        b      (uuid/next)
        switch (fn [state board-id] (ptk/update (dwa/set-active-board board-id) state))
        at     #(get-in % [:workspace-animation :playhead])
        start  {:workspace-animation {:board-id a :playhead 700}}
        on-b   (switch start b)]
    (t/is (= 0 (at on-b)) "a board shows from the start the first time")
    (let [back (-> on-b
                   (assoc-in [:workspace-animation :playhead] 250)
                   (switch a))]
      (t/is (= 700 (at back)) "back where it was left")
      (t/is (= 250 (at (switch back b))) "and so is the other one"))
    (t/is (= 700 (at (switch start a))) "the same board stays where it is")))

(t/deftest another-board-stops-what-plays
  (let [a      (uuid/next)
        b      (uuid/next)
        state  {:workspace-animation {:board-id a :playhead 700 :playing? true
                                      :preview {:board-id a :time 700}}}
        types  (fn [event state]
                 (let [types (atom [])]
                   (->> (ptk/watch event state (rx/empty))
                        (rx/subs! #(swap! types conj (ptk/type %))))
                   @types))]
    (t/is (= [::dwa/pause] (types (dwa/set-active-board b) state)))
    (t/is (= [] (types (dwa/set-active-board a) state)) "not the board that plays")))

(t/deftest out-of-motion-mode-the-boards-show-as-they-are
  ;; an edit of a timeline there (the plugin API, an export) poses
  ;; nothing, which no dock would settle in full quality, but a board
  ;; that plays from its title does
  (let [file-id (uuid/next)
        page-id (uuid/next)
        board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 300 :height 200})
        bid     (:id board)
        tl      (-> (cta/make-timeline {:board-id bid :duration 1000})
                    (cta/add-keyframe bid {:time 0 :property :opacity :value 0})
                    (cta/add-keyframe bid {:time 1000 :property :opacity :value 1}))
        state   {:current-file-id file-id
                 :current-page-id page-id
                 :workspace-animation {:board-id bid :playhead 500 :preview {:board-id bid :time 500}}
                 :files {file-id {:data {:pages-index {page-id {:id page-id
                                                                :objects {bid board}
                                                                :timelines {bid tl}}}}}}}
        types   (fn [state]
                  (let [types (atom [])]
                    (->> (ptk/watch (dwa/apply-preview) state (rx/empty))
                         (rx/subs! #(swap! types conj (ptk/type %))))
                    @types))
        posed?  #(some? (get-in (ptk/update (dwa/apply-preview) %) [:workspace-animation :preview]))
        motion  (assoc state :workspace-layout #{:animation-timeline})
        playing (assoc-in state [:workspace-animation :playing?] true)]
    (t/is (= [::dwm/clear-local-transform] (types state)) "out of motion mode")
    (t/is (not (posed? state)))
    (t/is (not= [::dwm/clear-local-transform] (types motion)) "in motion mode")
    (t/is (posed? motion))
    (t/is (not= [::dwm/clear-local-transform] (types playing)) "while a board plays from its title")
    (t/is (posed? playing))))

(t/deftest clearing-the-preview-sets-back-what-it-changed
  ;; a board that played out of motion mode shows as it is again: the
  ;; opacity it faded to set back, though the update drops what the
  ;; effect reads
  (let [id    (uuid/next)
        props [[id {:type :change-property :property :opacity :value 0.8}]]
        seen  (atom nil)
        event (dwm/clear-local-transform)
        state {:features #{"render-wasm/v1"} :wasm-props props}]
    (with-redefs [dwm/set-wasm-props! (fn [_ prev next] (reset! seen [prev next]))]
      (ptk/effect event (ptk/update event state) nil))
    (t/is (= [props []] @seen))))

;; TEXT SPLITS

(defn- split-texts
  "A board with two texts, each fading in word by word in its first
  500 ms."
  []
  (let [board   (cts/setup-shape {:type :frame :x 0 :y 0 :width 300 :height 200})
        content (fn [text]
                  {:type "root"
                   :children [{:type "paragraph-set"
                               :children [{:type "paragraph" :children [{:text text}]}]}]})
        text-a  (assoc (cts/setup-shape {:type :text :x 10 :y 10 :width 200 :height 20})
                       :content (content "Hi you"))
        text-b  (assoc (cts/setup-shape {:type :text :x 10 :y 50 :width 200 :height 20})
                       :content (content "and you"))
        bid     (:id board)
        [a b]   (map :id [text-a text-b])]
    {:a a
     :b b
     :objects  {bid (assoc board :shapes [a b])
                a   (assoc text-a :parent-id bid :frame-id bid)
                b   (assoc text-b :parent-id bid :frame-id bid)}
     :timeline (reduce #(cta/add-animation %1 %2 {:type :fade :split :word :start 0 :duration 500})
                       (cta/make-timeline {:board-id bid :duration 1000})
                       [a b])}))

(defn- units-of
  "The `:text-units` `modif-tree` gives the shape `id`, `::none` without."
  [modif-tree id]
  (get (cta/appearance-changes (get-in modif-tree [id :modifiers])) :text-units ::none))

(t/deftest the-text-in-edition-shows-at-rest
  (let [{:keys [a b objects timeline]} (split-texts)
        tree (cta/timeline->modif-tree timeline objects 100)]
    (t/is (seq (units-of tree a)))
    (let [shown (dwa/texts-at-rest tree {:workspace-local {:edition a}})]
      (t/is (= [] (units-of shown a)))
      (t/is (= (units-of tree b) (units-of shown b)) "the other texts play on"))
    (t/is (= tree (dwa/texts-at-rest tree {})) "out of edition, as it plays")))

(t/deftest a-text-no-more-posed-is-set-at-rest
  (let [{:keys [a b objects timeline]} (split-texts)
        posed   (cta/timeline->modif-tree timeline objects 100)
        ;; what the canvas was given for that frame
        props   (vec (wasm.modifiers/extract-property-changes posed))
        ;; the split animations of `a` removed
        without (cta/timeline->modif-tree (update-in timeline [:tracks a] dissoc :animations) objects 100)
        shown   (dwa/texts-at-rest without {:wasm-props props})]
    (t/is (= ::none (units-of without a)))
    (t/is (= [] (units-of shown a)) "its pose clears")
    (t/is (= (units-of without b) (units-of shown b)))
    (t/is (= without (dwa/texts-at-rest without {:wasm-props (vec (wasm.modifiers/extract-property-changes shown))}))
          "once at rest, nothing more to clear")))

(t/deftest the-pose-of-a-text-clears-once-its-split-ends
  (let [{:keys [a objects timeline]} (split-texts)
        frame   #(vec (wasm.modifiers/extract-property-changes (cta/timeline->modif-tree timeline objects %)))
        [changed changes] (wasm.modifiers/property-changes objects (frame 100) (frame 600))]
    (t/is (contains? (get changes a) :text-units))
    (t/is (= [] (get-in changed [a :text-units])) "the text is restored")))

(t/deftest entering-and-leaving-edition-shows-the-animation-again
  (let [stream (rx/subject)
        types  (atom [])
        sub    (->> (ptk/watch (dwa/start-canvas-keyframes) {} stream)
                    (rx/subs! #(swap! types conj (ptk/type %))))]
    (rx/push! stream (dwe/start-edition-mode (uuid/next)))
    (rx/push! stream (dwe/clear-edition-mode))
    (rx/push! stream (dwa/stop-canvas-keyframes))
    (rx/dispose! sub)
    (t/is (= [::dwa/restore-preview ::dwa/restore-preview] @types))))

(t/deftest an-edit-of-the-text-in-edition-shows-it-at-rest
  ;; as typing grows the text: an edit over the animation (see
  ;; `cltl/edit-preview`)
  (let [{:keys [a b objects timeline]} (split-texts)
        file-id (uuid/next)
        page-id (uuid/next)
        bid     (:board-id timeline)
        state   {:current-file-id file-id
                 :current-page-id page-id
                 :workspace-layout #{:animation-timeline}
                 :workspace-animation {:preview {:board-id bid :time 100}}
                 :workspace-local {:edition a}
                 :files {file-id {:data {:pages-index {page-id {:id page-id
                                                                :objects objects
                                                                :timelines {bid timeline}}}}}}}
        edit    {a {:modifiers (ctm/move (ctm/empty) (gpt/point 5 0))}}
        units   (fn [state id]
                  (some (fn [[shape-id {:keys [property value]}]]
                          (when (and (= id shape-id) (= :text-units property)) value))
                        (:wasm-props (ptk/update (dwm/set-wasm-modifiers edit) state))))]
    (t/is (= [] (units state a)))
    (t/is (seq (units state b)) "the other texts play on")
    (t/is (seq (units (update state :workspace-local dissoc :edition) a)) "out of edition, posed")))
