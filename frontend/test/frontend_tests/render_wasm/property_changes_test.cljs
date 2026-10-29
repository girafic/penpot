;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns frontend-tests.render-wasm.property-changes-test
  "The attribute changes a frame of an animation pushes to WASM
  (`wasm.modifiers/property-changes`): with what the frame before pushed
  shown still, only the shapes that change."
  (:require
   [app.common.types.modifiers :as ctm]
   [app.common.uuid :as uuid]
   [app.render-wasm.modifiers :as wasm.modifiers]
   [cljs.test :as t :include-macros true]))

(def ^:private a (uuid/next))
(def ^:private b (uuid/next))
(def ^:private c (uuid/next))

(def ^:private objects
  {a {:id a :type :rect :opacity 1}
   b {:id b :type :path :opacity 1 :trim-start 0 :trim-end 1 :trim-offset 0}
   c {:id c :type :rect :opacity 0.5}})

(defn- frame
  "The attribute changes of a frame that sets `values` (`{id {property
  value}}`)."
  [values]
  (vec (wasm.modifiers/extract-property-changes
        (update-vals values
                     (fn [props]
                       {:modifiers (reduce-kv ctm/change-property (ctm/empty) props)})))))

(def ^:private before
  (frame {a {:opacity 0.2}
          b {:trim-start 0 :trim-end 0.4 :trim-offset 0}
          c {:opacity 0.1}}))

(def ^:private after
  (frame {a {:opacity 0.2}
          b {:trim-start 0 :trim-end 0.5 :trim-offset 0}}))

(t/deftest a-frame-pushes-every-shape-unless-the-one-before-shows
  (let [[changed changes] (wasm.modifiers/property-changes objects before after)]
    (t/is (= #{a b c} (set (keys changes))))
    (t/is (= #{:opacity} (get changes a)))
    (t/is (= 0.2 (get-in changed [a :opacity])))))

(t/deftest a-frame-shown-before-pushes-what-changes
  (let [[changed changes] (wasm.modifiers/property-changes objects before after true)]
    (t/is (not (contains? changes a)) "a keeps its opacity")
    (t/is (= #{:trim-start :trim-end :trim-offset} (get changes b))
          "a shape that changes pushes all it changes: its trim goes whole")
    (t/is (= [0 0.5 0] (mapv #(get-in changed [b %]) [:trim-start :trim-end :trim-offset])))
    (t/is (= #{:opacity} (get changes c)) "c no longer changes")
    (t/is (= 0.5 (get-in changed [c :opacity])) "c is set back")))

(t/deftest a-property-no-more-changed-goes-back
  (let [[changed changes] (wasm.modifiers/property-changes
                           objects
                           (frame {a {:opacity 0.2 :blur 4}})
                           (frame {a {:opacity 0.2}})
                           true)]
    (t/is (= #{:opacity :blur} (get changes a)))
    (t/is (= 0.2 (get-in changed [a :opacity])))
    (t/is (nil? (get-in changed [a :blur])))))

(t/deftest the-last-change-of-a-property-wins
  (let [changes (into (frame {a {:opacity 0.2}}) (frame {a {:opacity 0.7}}))
        [changed] (wasm.modifiers/property-changes objects [] changes)]
    (t/is (= 0.7 (get-in changed [a :opacity])))))
