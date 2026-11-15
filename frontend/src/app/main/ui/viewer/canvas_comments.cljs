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
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.canvas :as dvc]
   [app.main.data.workspace.comments :as dwcm]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.comments :as cmt]
   [app.main.ui.viewer.comments :as vcm]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

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

(defn start-draft!
  "Begin a comment at the `point` of the page `page-id` of the file
  `file-id`, held on its canvas of `objects`, about the top-level board
  there: the comments shown, the thread open closed. Nothing beside the
  boards: the prototype shows a thread on its board only."
  [objects page-id file-id point]
  (when (some? (dvc/board-at objects point))
    (st/emit! (dcm/update-options {:canvas-shown? true})
              (dcm/close-thread)
              (dcm/create-draft {:position point :page-id page-id :file-id file-id}))))

(def ^:private draft-field
  "[class*=floating-thread-draft-inner-wrapper] [contenteditable=true]")

(defn focus-draft!
  "Give the field of a comment begun the focus, in the gesture of the
  finger that lifts: only then does iOS open its keyboard."
  []
  (some-> (.querySelector js/document draft-field) (.focus)))

(defn- anchored
  "The modifier that draws a bubble at `point` of the page in its anchor
  there: at the corner of the anchor, which moves with the canvas."
  [point]
  (gmt/translate-matrix (gpt/negate point)))

(defn- anchor-style
  [point]
  #js {"--anchor-x" (:x point) "--anchor-y" (:y point)})

(mf/defc canvas-bubble*
  "The bubble of `thread` on the canvas of `objects` at `zoom`, in its
  anchor at `anchor` (the middle of its group spread out, else its
  point). One may move it (`movable`): it drops within its board."
  {::mf/wrap [mf/memo]}
  [{:keys [thread anchor objects zoom offset ring is-open movable on-click]}]
  (let [anchor   (or anchor (:position thread))
        modifier (mf/with-memo [anchor]
                   (anchored anchor))

        ;; it drops where it is in its anchor
        on-move
        (mf/use-fn
         (mf/deps anchor objects)
         (fn [thread [x y]]
           (let [position (-> (dvc/thread-board objects (:frame-id thread))
                              (dvc/clamp-to-board (gpt/add anchor (gpt/point x y))))]
             (st/emit! (dcm/update-comment-thread-position-on-viewer thread position)))))]
    [:> cmt/comment-floating-bubble*
     {:thread thread
      :position-modifier modifier
      :zoom zoom
      :offset offset
      :ring ring
      :on-click on-click
      :on-move (when movable on-move)
      :is-open is-open
      :origin :viewer}]))

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
        can-edit?   (mf/deref refs/viewer-can-edit?)
        page-id     (:id page)

        positions-ref (mf/with-memo [page-id]
                        (-> (l/in [:pages page-id :comment-thread-positions])
                            (l/derived refs/viewer-data)))
        positions     (mf/deref positions-ref)

        ;; only the filters of the comments, not a keystroke in a draft,
        ;; gather the bubbles again
        mode        (:mode local)
        show        (:show local)
        all         (mf/with-memo [threads-map positions objects]
                      (dvc/canvas-threads (vals threads-map) positions objects))
        threads     (mf/with-memo [all mode show profile]
                      (dcm/apply-filters {:mode mode :show show} profile all))
        groups      (mf/with-memo [threads zoom]
                      (dwcm/group-bubbles zoom threads))

        open-id     (:open local)
        expanded    (:expanded local)
        draft       (:draft local)


        on-draft-cancel
        (mf/use-fn #(st/emit! (dcm/close-thread)))

        ;; about the board under it, which the prototype shows it on;
        ;; begun on another page, it goes
        on-draft-submit
        (mf/use-fn
         (mf/deps objects page-id)
         (fn [{:keys [position] :as draft}]
           (if-let [board (when (= page-id (:page-id draft))
                            (dvc/board-at objects position))]
             (st/emit! (dcm/create-thread-on-viewer
                        (-> draft
                            (select-keys [:page-id :file-id :position :content])
                            (assoc :frame-id (:id board))))
                       (dcm/close-thread))
             (st/emit! (dcm/close-thread)))))
        ;; open, it stays so as the filters change
        open-thread (some #(when (= open-id (:id %)) %) all)

        ;; the canvas plays no animation: the moment a thread is about,
        ;; or a timestamp in its text, shows in the prototype, on a board
        ;; it lists
        shows-time? (some? (some #(when (= (:frame-id open-thread) (:id %)) %) (:frames page)))
        on-show-time
        (mf/use-fn
         (mf/deps open-thread)
         (fn [time]
           (st/emit! (dv/open-thread-moment open-thread time))))

        on-bubble-click
        (mf/use-fn
         (mf/deps open-id)
         (fn [thread]
           (st/emit! (dcm/toggle-thread-on-viewer open-id thread))))

        movable?
        (partial dcm/thread-movable? can-edit? profile)]

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
              [:> canvas-bubble* {:key (:seqn thread)
                                  :thread thread
                                  :anchor center
                                  :objects objects
                                  :zoom zoom
                                  :offset offset
                                  :ring ring
                                  :on-click on-bubble-click
                                  :movable (movable? thread)
                                  :is-open false}])]

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
            [:> canvas-bubble* {:thread thread
                                :objects objects
                                :zoom zoom
                                :on-click on-bubble-click
                                :movable (movable? thread)
                                :is-open (= (:id thread) open-id)}]])))

     ;; a comment begun, its bubble where the finger held, its field
     ;; docked at the bottom
     (when (some? draft)
       [:div {:class (stl/css :anchor)
              :style (anchor-style (:position draft))}
        [:> cmt/comment-floating-thread-draft*
         {:draft draft
          :position-modifier (anchored (:position draft))
          :on-cancel on-draft-cancel
          :on-submit on-draft-submit
          :zoom zoom
          :docked true}]])

     (when (some? open-thread)
       [:> cmt/comment-floating-thread*
        {:thread open-thread
         :origin :viewer
         :on-show-time (when shows-time? on-show-time)
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
