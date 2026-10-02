;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.files.helpers-test
  (:require
   [app.common.files.helpers :as cfh]
   [app.common.types.animation :as cta]
   [app.common.types.animation-style :as ctas]
   [app.common.types.audio-clip :as ctau]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(t/deftest test-generate-unique-name
  (t/testing "Test unique name generation"
    (let [suffix-fn #(str "-copy-" %)]
      (t/is (cfh/generate-unique-name "base-name"
                                      #{"base-name" "base-name-copy-1"}
                                      :suffix-fn suffix-fn)
            "base-name-copy-2")
      (t/is (cfh/generate-unique-name "base-name"
                                      #{"base-name-copy-2"}
                                      :suffix-fn suffix-fn)
            "base-name-copy-1")
      (t/is (cfh/generate-unique-name "base-name"
                                      #{"base-namec-copy"}
                                      :suffix-fn suffix-fn)
            "base-name-copy-1")
      (t/is (cfh/generate-unique-name "base-name"
                                      #{"base-name"}
                                      :suffix-fn suffix-fn)
            "base-name-copy-1")))

  (t/testing "Test unique name generation with immidate suffix and default suffix-fn"
    (t/is (cfh/generate-unique-name "base-name" #{} :immediate-suffix? true)
          "base-name 1")
    (t/is (cfh/generate-unique-name "base-name"
                                    #{"base-name 1" "base-name 2"}
                                    :immediate-suffix? true)
          "base-name 3")))

(t/deftest test-get-prev-sibling
  (let [parent-id (uuid/custom 1 1)
        child-a   (uuid/custom 1 2)
        child-b   (uuid/custom 1 3)
        orphan-id (uuid/custom 1 4)
        objects   {parent-id {:id parent-id :shapes [child-a child-b]}
                   child-a   {:id child-a :parent-id parent-id}
                   child-b   {:id child-b :parent-id parent-id}
                   orphan-id {:id orphan-id :parent-id parent-id}}]
    (t/testing "Returns previous sibling when present in parent ordering"
      (t/is (= child-a
               (cfh/get-prev-sibling objects child-b))))

    (t/testing "Returns nil when the shape is missing from parent ordering"
      (t/is (nil? (cfh/get-prev-sibling objects orphan-id))))))

(t/deftest relink-refs-relinks-the-uses-of-animation-styles
  (let [old-file (uuid/next)
        new-file (uuid/next)
        library  (uuid/next)
        page-id  (uuid/next)
        board    (uuid/next)
        shape    (uuid/next)
        style    (ctas/make-animation-style {:name "Pop" :entries [{:type :fade :duration 300}]})
        timeline (-> (cta/make-timeline {:board-id board})
                     (cta/add-animation shape (ctas/make-use style old-file 0))
                     (cta/add-animation shape (ctas/make-use style library 0)))
        data     {:pages-index {page-id {:id page-id :timelines {board timeline}}}}
        relinked (cfh/relink-refs data #(get {old-file new-file} % %))]
    (t/is (= [new-file library]
             (mapv :style-ref-file
                   (get-in relinked [:pages-index page-id :timelines board :tracks shape :animations]))))))

(defn- clips-data
  "File data whose page plays the audio objects `audio-ids` on the
  timeline of a board, and a legacy media entry."
  [page-id board & audio-ids]
  (let [timeline (reduce #(ctau/add-audio-clip %1 {:audio-id %2 :name "a.mp3" :source-duration 1000
                                                   :start 0 :duration 1000})
                         (cta/make-timeline {:board-id board})
                         audio-ids)
        legacy   (uuid/next)]
    {:pages-index {page-id {:id page-id :objects {} :timelines {board timeline}}}
     :media {legacy {:id legacy :name "old.png" :mtype "image/png" :width 1 :height 1}}}))

(t/deftest the-media-of-the-audio-clips-are-used
  (let [a    (uuid/next)
        b    (uuid/next)
        data (clips-data (uuid/next) (uuid/next) a b)]
    (t/is (= #{a b} (cfh/collect-timeline-media-refs (first (vals (:pages-index data))))))
    (t/is (= (into #{a b} (keys (:media data))) (cfh/collect-used-media data)))))

(t/deftest relink-refs-relinks-the-media-of-the-audio-clips
  (let [page-id  (uuid/next)
        board    (uuid/next)
        a        (uuid/next)
        b        (uuid/next)
        a'       (uuid/next)
        data     (clips-data page-id board a b)
        ;; it also maps the ids of the legacy media entries: they stay
        relinked (cfh/relink-refs data #(get (into {a a'} (map (fn [id] [id (uuid/next)])) (keys (:media data))) % %))]
    (t/is (= #{a' b}
             (set (map :audio-id (vals (get-in relinked [:pages-index page-id :timelines board :audio-clips]))))))
    (t/is (= (:media data) (:media relinked)))))
