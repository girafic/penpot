;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.viewer.canvas-comments
  "The comments on the canvas of a phone (see `app.main.ui.viewer.canvas`),
  once its sheet shows them: the bubbles of the threads of the page at
  their points, which move with the canvas by its CSS variables (each in
  an anchor at its point of the page, see `canvas.scss`), and the thread
  open docked at the bottom, as in the prototype."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.math :as mth]
   [app.main.data.comments :as dcm]
   [app.main.data.event :as ev]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.canvas :as dvc]
   [app.main.data.workspace.comments :as dwcm]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.comments :as cmt]
   [app.main.ui.viewer.comments :as vcm]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

;; who may move the bubbles of the others too
(def ^:private ref:can-edit?
  (l/derived #(true? (dm/get-in % [:viewer :permissions :can-edit])) st/state))

(defn close-on-tap!
  "What a tap on the canvas does to the comments: it collapses a group
  spread out, or closes the thread open or begun."
  []
  (let [local (:comments-local @st/state)]
    (cond
      (some? (:expanded local))
      (st/emit! (dcm/collapse-comment-group))

      (or (some? (:open local)) (some? (:draft local)))
      (st/emit! (dcm/close-thread)))))

(defn- anchored
  "The modifier that draws a bubble at `point` of the page in its anchor
  there: at the corner of the anchor, which moves with the canvas."
  [point]
  (gmt/translate-matrix (gpt/negate point)))

(defn- anchor-style
  [point]
  #js {"--anchor-x" (:x point) "--anchor-y" (:y point)})

(defn- within-board
  "`position` of the page within the board of `objects` the `thread` is
  about, as the prototype keeps a bubble dropped: out of it, the
  prototype would show it no more."
  [objects {:keys [frame-id]} position]
  (if-let [board (get objects frame-id)]
    (let [{:keys [x1 y1 x2 y2]} (-> board :points grc/points->rect)]
      (gpt/point (mth/clamp (:x position) x1 x2)
                 (mth/clamp (:y position) y1 y2)))
    position))

(mf/defc canvas-comments*
  "The comments of `page` of `file` on its canvas drawn of `objects` (see
  `dvc/canvas-objects`), at the `zoom` it rests at (see
  `dwcm/group-bubbles`), on a screen of `size`. `view-of` gives the view
  and the screen of the canvas now (see `canvas-viewport*`), to bring a
  thread opened out of sight into view."
  [{:keys [page objects zoom size view-of]}]
  (let [profile     (mf/deref refs/profile)
        local       (mf/deref refs/comments-local)
        threads-map (mf/deref refs/comment-threads)
        can-edit?   (mf/deref ref:can-edit?)
        page-id     (:id page)

        positions-ref (mf/with-memo [page-id]
                        (-> (l/in [:pages page-id :comment-thread-positions])
                            (l/derived refs/viewer-data)))
        positions     (mf/deref positions-ref)

        all         (mf/with-memo [threads-map positions objects]
                      (dvc/canvas-threads (vals threads-map) positions objects))
        threads     (mf/with-memo [all local profile]
                      (dcm/apply-filters local profile all))
        groups      (mf/with-memo [threads zoom]
                      (dwcm/group-bubbles zoom threads))

        open-id     (:open local)
        expanded    (:expanded local)
        ;; open, it stays so as the filters change
        open-thread (some #(when (= open-id (:id %)) %) all)

        on-bubble-click
        (mf/use-fn
         (mf/deps open-id)
         (fn [{:keys [id] :as thread}]
           (st/emit! (if (= open-id id)
                       (dcm/close-thread)
                       (-> (dcm/open-thread thread)
                           (with-meta {::ev/origin "viewer"}))))))

        ;; one's own threads, and all those of a file one edits
        movable?    (fn [thread]
                      (or can-edit? (= (:owner-id thread) (:id profile))))

        ;; a bubble drops where it is in its anchor at `anchor`
        on-move-from
        (fn [anchor]
          (fn [thread [x y]]
            (let [position (within-board objects thread (gpt/add anchor (gpt/point x y)))]
              (st/emit! (dcm/update-comment-thread-position-on-viewer thread position)))))

        bubble
        (fn [thread & {:keys [anchor offset ring]}]
          (let [anchor (or anchor (:position thread))]
            (mf/html
             [:> cmt/comment-floating-bubble*
              {:thread thread
               :position-modifier (anchored anchor)
               :zoom zoom
               :offset offset
               :ring ring
               :on-click on-bubble-click
               :on-move (when (movable? thread) (on-move-from anchor))
               :is-open (and (nil? offset) (= (:id thread) open-id))
               :origin :viewer
               :key (:seqn thread)}])))]

    ;; another zoom gathers the bubbles again, as the prototype does
    (mf/with-effect [zoom]
      (st/emit! (dcm/collapse-comment-group)))

    ;; a thread opened, from the list too, which closes: where its bubble
    ;; does not show above it, there
    (mf/with-effect [open-id]
      (when-let [{:keys [position frame-id]} open-thread]
        (when (:show-sidebar? local)
          (st/emit! (dcm/update-options {:show-sidebar? false})))
        (let [{:keys [view screen]} (view-of)]
          (when (and (some? view) (not (dvc/bubble-shown? view screen position)))
            (st/emit! (dv/focus-canvas {:point position :board frame-id}))))))

    [:div {:class (stl/css :comments)}
     (for [group groups]
       (let [thread    (first group)
             center    (gpt/center-points (mapv :position group))
             expanded? (and (> (count group) 1)
                            (= expanded (into #{} (map :id) group)))]
         (cond
           expanded?
           [:div {:key (:seqn thread)
                  :class (stl/css :anchor)
                  :style (anchor-style center)}
            [:> cmt/comment-floating-ghost* {:thread-group group
                                             :zoom zoom
                                             :position-modifier (anchored center)}]
            (for [[thread offset ring] (cmt/expanded-group-offsets zoom group)]
              (bubble thread :anchor center :offset offset :ring ring))]

           (> (count group) 1)
           [:div {:key (:seqn thread)
                  :class (stl/css :anchor)
                  :style (anchor-style center)}
            [:> cmt/comment-floating-group* {:thread-group group
                                             :zoom zoom
                                             :position-modifier (anchored center)}]]

           :else
           [:div {:key (:seqn thread)
                  :class (stl/css :anchor)
                  :style (anchor-style (:position thread))}
            (bubble thread)])))

     (when (some? open-thread)
       [:> cmt/comment-floating-thread*
        {:thread open-thread
         :origin :viewer
         :viewport {:offset-x 0 :offset-y 0 :width (:width size) :height (:height size)}
         :zoom zoom
         :docked true}])]))

(mf/defc canvas-comments-list*
  "The list of the comment threads of `page` on its canvas, once its sheet
  opens it (see `app.main.ui.viewer.phone-sheet`), as a sheet: a tap on
  one closes it and opens the thread (see `canvas-comments*`)."
  [{:keys [users page]}]
  (let [local (mf/deref refs/comments-local)]
    (when (and (true? (:canvas-shown? local)) (true? (:show-sidebar? local)))
      [:> vcm/comments-sidebar* {:profiles users :page page}])))
