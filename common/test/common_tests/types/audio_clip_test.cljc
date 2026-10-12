;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.audio-clip-test
  (:require
   [app.common.types.animation :as cta]
   [app.common.types.audio-clip :as ctau]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- clip
  "The params of a clip of a 4 s file, from `start`, playing `duration` ms
  of it from `offset`; no lane unless given."
  [& {:as params}]
  (merge {:id (uuid/next) :audio-id (uuid/next) :name "beat.mp3" :source-duration 4000
          :start 0 :offset 0 :duration 4000}
         params))

(defn- timeline-with
  "A timeline of `duration` ms with `clips` added."
  [duration & clips]
  (reduce ctau/add-audio-clip
          (cta/make-timeline {:board-id (uuid/next) :duration duration})
          clips))

(defn- the-clip
  [timeline clip]
  (ctau/get-audio-clip timeline (:id clip)))

(t/deftest the-schema-bounds-a-clip
  (let [valid (ctau/make-audio-clip (clip :lane 2 :volume 0.5 :fade-in 100 :fade-out 200 :muted true :locked true))]
    (t/is (ctau/valid-audio-clip? valid))
    (t/is (not (ctau/valid-audio-clip? (dissoc valid :audio-id))))
    (t/is (ctau/valid-audio-clip? (assoc valid :start -1)) "it may start before the timeline")
    (doseq [[k v] [[:offset -1] [:fade-in -1] [:duration 9] [:volume 4.5]
                   [:volume -0.1] [:lane -1] [:source-duration (inc ctau/max-audio-duration)]]]
      (t/is (not (ctau/valid-audio-clip? (assoc valid k v))) (str k " " v)))
    (t/is (ctau/valid-audio-clip? (assoc valid :volume 4)) "up to four times as loud")))

(t/deftest a-clip-is-made-valid
  (let [made (ctau/make-audio-clip {:audio-id (uuid/next) :source-duration 2000.4 :start -50
                                    :offset 1999 :duration 500 :volume 1 :fade-in 9000 :color "red"})]
    (t/is (ctau/valid-audio-clip? made))
    (t/is (= [-50 1990 10] ((juxt :start :offset :duration) made))
          "at least 10 ms of it, within its file, before the timeline too")
    (t/is (nil? (:volume made)) "a full volume is left out")
    (t/is (= 1.5 (:volume (ctau/make-audio-clip (assoc made :volume 1.5)))) "a louder one kept")
    (t/is (= 4 (:volume (ctau/make-audio-clip (assoc made :volume 9)))) "at most four times")
    (t/is (= 5 (:fade-in made)) "a fade within half the clip")
    (t/is (not (contains? made :color)) "only the keys of a clip")))

(t/deftest adding-a-clip-keeps-the-length-and-picks-a-lane
  (let [song (clip :start 0 :duration 4000)
        tl   (timeline-with 2000 song)]
    (t/is (= 2000 (:duration tl)) "even for a clip longer than the timeline")
    (t/is (= 0 (:lane (the-clip tl song))))
    (let [over  (clip :start 1000 :duration 500)
          after (clip :start 4000 :duration 500)
          part  (clip :audio-id (:audio-id song) :start 4000 :duration 500)
          fixed (clip :start 1000 :duration 500 :lane 3)
          tl    (timeline-with 2000 song over after part fixed)]
      (t/is (= 1 (:lane (the-clip tl over))) "the first lane free over its span")
      (t/is (= 2 (:lane (the-clip tl after))) "past the first one, another file has a lane of its own")
      (t/is (= 0 (:lane (the-clip tl part))) "a part of the same file goes after it")
      (t/is (= 3 (:lane (the-clip tl fixed))) "its own lane")
      (t/is (= [0 0 1 2 3] (map :lane (ctau/audio-clips tl)))))))

(t/deftest the-timeline-fits-its-audio-when-asked
  (let [tl (timeline-with 2000 (clip :start 500 :duration 3000) (clip :start 0 :duration 1000))]
    (t/is (= 3500 (:duration (ctau/fit-duration-to-audio tl))))
    (t/is (= 2000 (:duration (ctau/fit-duration-to-audio (timeline-with 2000)))) "no clips, no change")))

(t/deftest trimming-the-start-keeps-the-end
  (let [c  (clip :start 1000 :offset 500 :duration 2000)
        tl (timeline-with 5000 c)
        at #(select-keys (the-clip % c) [:start :offset :duration])]
    (t/is (= {:start 1200 :offset 700 :duration 1800} (at (ctau/trim-audio-clip-start tl (:id c) 1200))))
    (t/is (= {:start 800 :offset 300 :duration 2200} (at (ctau/trim-audio-clip-start tl (:id c) 800))))
    (t/is (= {:start 500 :offset 0 :duration 2500} (at (ctau/trim-audio-clip-start tl (:id c) 0)))
          "not before the file starts")
    (t/is (= {:start 2990 :offset 2490 :duration 10} (at (ctau/trim-audio-clip-start tl (:id c) 2999)))
          "not shorter than 10 ms")))

(t/deftest trimming-the-end-stays-within-the-file
  (let [c  (clip :start 1000 :offset 500 :duration 2000)
        tl (timeline-with 5000 c)
        at #(select-keys (the-clip % c) [:start :offset :duration])]
    (t/is (= {:start 1000 :offset 500 :duration 1500} (at (ctau/trim-audio-clip-end tl (:id c) 2500))))
    (t/is (= {:start 1000 :offset 500 :duration 3500} (at (ctau/trim-audio-clip-end tl (:id c) 9000)))
          "not past the end of the file")
    (t/is (= 10 (:duration (the-clip (ctau/trim-audio-clip-end tl (:id c) 0) c))) "not shorter than 10 ms")))

(t/deftest a-split-makes-two-clips-of-the-same-file
  (let [c      (clip :start 1000 :offset 500 :duration 2000 :lane 2 :fade-in 100 :fade-out 200)
        new-id (uuid/next)
        tl     (timeline-with 5000 c)
        split  (ctau/split-audio-clip tl (:id c) 1600 new-id)
        left   (ctau/get-audio-clip split (:id c))
        right  (ctau/get-audio-clip split new-id)]
    (t/is (= [1000 500 600 2 100 nil] ((juxt :start :offset :duration :lane :fade-in :fade-out) left)))
    (t/is (= [1600 1100 1400 2 nil 200] ((juxt :start :offset :duration :lane :fade-in :fade-out) right)))
    (t/is (= (:audio-id c) (:audio-id left) (:audio-id right)))
    (t/is (= (:duration c) (+ (:duration left) (:duration right))))
    (t/testing "at or outside its edges, nothing changes"
      (doseq [time [1000 3000 900 3500 1005]]
        (t/is (= tl (ctau/split-audio-clip tl (:id c) time new-id)) (str time))))))

(t/deftest a-locked-clip-takes-no-edit
  (let [c      (clip :start 1000 :duration 2000)
        tl     (ctau/set-audio-clip-locked (timeline-with 5000 c) (:id c) true)
        id     (:id c)]
    (doseq [[label edited] [["move" (ctau/move-audio-clip tl id 0)]
                            ["trim start" (ctau/trim-audio-clip-start tl id 1500)]
                            ["trim end" (ctau/trim-audio-clip-end tl id 1500)]
                            ["split" (ctau/split-audio-clip tl id 1500 (uuid/next))]
                            ["volume" (ctau/set-audio-clip-volume tl id 0.2)]
                            ["mute" (ctau/set-audio-clip-muted tl id true)]
                            ["fades" (ctau/set-audio-clip-fades tl id 100 100)]
                            ["offset" (ctau/set-audio-clip-offset tl id 500)]
                            ["speed" (ctau/set-audio-clip-speed tl id 2)]
                            ["remove" (ctau/remove-audio-clip tl id)]]]
      (t/is (= tl edited) label))
    (t/testing "but its unlocking"
      (let [unlocked (ctau/set-audio-clip-locked tl id false)]
        (t/is (not (:locked (ctau/get-audio-clip unlocked id))))
        (t/is (= 0 (:start (ctau/get-audio-clip (ctau/move-audio-clip unlocked id 0) id))))))))

(t/deftest a-file-takes-the-name-it-is-given
  ;; All its clips, a locked one too, as a locked layer takes a new name;
  ;; those of other files stay.
  (let [a     (clip :start 0 :duration 1000)
        b     (clip :audio-id (:audio-id a) :start 2000 :duration 1000 :locked true)
        other (clip :name "drums.wav" :start 0 :duration 1000)
        tl    (timeline-with 5000 a b other)
        names #(mapv (comp :name (partial the-clip %)) [a b other])]
    (t/is (= ["Voice" "Voice" "drums.wav"] (names (ctau/rename-audio-file tl (:audio-id a) "  Voice "))))
    (t/is (= tl (ctau/rename-audio-file tl (:audio-id a) " ")) "not blank")))

(t/deftest edits-of-a-clip
  (let [c  (clip :start 1000 :duration 2000)
        tl (timeline-with 5000 c)
        id (:id c)]
    (t/is (= [300 1] ((juxt :start :lane) (ctau/get-audio-clip (ctau/move-audio-clip tl id 300 :lane 1) id))))
    (t/is (= -300 (:start (ctau/get-audio-clip (ctau/move-audio-clip tl id -300) id))) "before the timeline too")
    (t/is (= 0.25 (:volume (ctau/get-audio-clip (ctau/set-audio-clip-volume tl id 0.25) id))))
    (t/is (true? (:muted (ctau/get-audio-clip (ctau/set-audio-clip-muted tl id true) id))))
    (t/is (= [100 200] ((juxt :fade-in :fade-out) (ctau/get-audio-clip (ctau/set-audio-clip-fades tl id 100 200) id))))
    (t/is (= [1000 500 2000] ((juxt :start :offset :duration) (ctau/get-audio-clip (ctau/set-audio-clip-offset tl id 500) id))))
    (t/is (= [1000 3000 1000] ((juxt :start :offset :duration) (ctau/get-audio-clip (ctau/set-audio-clip-offset tl id 3000) id)))
          "shorter where the file ends")
    (t/is (not (contains? (ctau/remove-audio-clip tl id) :audio-clips)) "the last one gone, no clips left")))

(t/deftest a-clip-plays-its-file-at-a-speed
  (let [at #(select-keys % [:start :offset :duration :speed])]
    (t/testing "made valid"
      (t/is (= {:start 0 :offset 1000 :duration 1500 :speed 2}
               (at (ctau/make-audio-clip (clip :offset 1000 :duration nil :speed 2))))
            "as long as what is left of its file lasts at that speed")
      (t/is (= 4 (:speed (ctau/make-audio-clip (clip :speed 10)))))
      (t/is (= 0.25 (:speed (ctau/make-audio-clip (clip :speed 0.1)))))
      (t/is (not (contains? (ctau/make-audio-clip (clip :speed 1)) :speed)) "a normal one left out"))

    (let [c  (clip :start 1000 :offset 1000 :duration 1000 :speed 2)
          tl (timeline-with 5000 c)
          id (:id c)]
      (t/testing "trimmed and split in its file at its speed"
        (t/is (= {:start 1200 :offset 1400 :duration 800 :speed 2}
                 (at (the-clip (ctau/trim-audio-clip-start tl id 1200) c))))
        (t/is (= {:start 500 :offset 0 :duration 1500 :speed 2}
                 (at (the-clip (ctau/trim-audio-clip-start tl id 0) c)))
              "not before its file starts")
        (t/is (= 1500 (:duration (the-clip (ctau/trim-audio-clip-end tl id 9000) c)))
              "nor past its end")
        (let [new-id (uuid/next)
              split  (ctau/split-audio-clip tl id 1400 new-id)]
          (t/is (= {:start 1000 :offset 1000 :duration 400 :speed 2} (at (ctau/get-audio-clip split id))))
          (t/is (= {:start 1400 :offset 1800 :duration 600 :speed 2} (at (ctau/get-audio-clip split new-id))))))

      (t/testing "its speed changes, the part of its file stays"
        (t/is (= {:start 1000 :offset 1000 :duration 2000} (at (the-clip (ctau/set-audio-clip-speed tl id 1) c))))
        (t/is (= {:start 1000 :offset 1000 :duration 500 :speed 4} (at (the-clip (ctau/set-audio-clip-speed tl id 8) c)))
              "at most four times as fast")))))

(t/deftest a-lane-plays-one-file
  (let [a  (clip :start 0 :duration 1000 :lane 0)
        b  (clip :start 2000 :duration 1000 :lane 0)
        tl (timeline-with 5000 a b)]
    (t/is (= [0 1] (mapv #(:lane (the-clip tl %)) [a b])) "another file goes onto a lane of its own")
    (t/is (= 1 (:lane (the-clip (ctau/move-audio-clip tl (:id b) 2000 :lane 0) b)))
          "nor does it move onto one of another file")
    (t/is (= 2000 (:start (the-clip (ctau/move-audio-clip tl (:id b) 2000 :lane 0) b))))
    (t/is (= 3 (:lane (the-clip (ctau/move-audio-clip tl (:id b) 2000 :lane 3) b))) "onto an empty one it does")
    (let [part (clip :audio-id (:audio-id a) :start 500 :duration 1000 :lane 0)]
      (t/is (= 0 (:lane (the-clip (ctau/add-audio-clip tl part) part)))
            "a part of the same file shares it, over the others too"))
    (t/is (ctau/lane-takes? tl 0 (:audio-id a)))
    (t/is (not (ctau/lane-takes? tl 0 (:audio-id b))))
    (t/is (ctau/lane-takes? tl 0 (:audio-id b) :except-ids #{(:id a)}))
    (t/is (= 2 (ctau/free-lane tl 0 100 :audio-id (uuid/next))) "a new file, past the lanes of the others")))

(t/deftest lanes-change-places
  (let [a     (clip :start 0 :lane 0)
        b     (clip :start 0 :lane 2)
        c     (clip :start 0 :lane 5 :locked true)
        tl    (timeline-with 5000 a b c)
        lanes #(mapv (fn [c] (:lane (the-clip % c))) [a b c])]
    (t/is (= [1 2 0] (lanes (ctau/move-lane tl 5 0))) "first, the others after it, numbered from 0")
    (t/is (= [2 0 1] (lanes (ctau/move-lane tl 0 9))) "last")
    (t/is (= [0 1 2] (lanes (ctau/move-lane tl 2 1))) "in place, the gaps closed")
    (t/is (= tl (ctau/move-lane tl 7 0)) "a lane without clips")
    (t/is (:locked (the-clip (ctau/move-lane tl 5 0) c)) "a locked clip goes with its lane")))

(t/deftest gain-points-ramp-the-fades
  (let [c (clip :duration 1000 :volume 0.5 :fade-in 200 :fade-out 400)]
    (t/is (= [[0 0.0] [200 0.5] [600 0.5] [1000 0.0]] (ctau/gain-points c 0 1000)))
    (t/is (= [[0 0.25] [100 0.5] [300 0.5]] (ctau/gain-points c 100 300)) "from the middle of the fade in")
    (t/is (= [[0 0.375] [100 0.25]] (ctau/gain-points c 700 100)) "within the fade out")
    (t/is (= [[0 1.0] [500 1.0]] (ctau/gain-points (clip :duration 1000) 0 500)) "no fades, full volume")))

(t/deftest drags-snap-to-other-clips
  (let [a  (clip :start 100 :duration 400)
        b  (clip :start 1000 :duration 300)
        tl (timeline-with 5000 a b)]
    (t/is (= #{100 500 1000 1300} (set (ctau/clip-times tl))))
    (t/is (= #{1000 1300} (set (ctau/clip-times tl :clip-ids #{(:id a)}))))))
