# Plan: Animated export in the view mode — review fixes

Status: done
Review Log:
- 2026-11-12T18:25:58Z — done

## Context

Parent: `2026-11-12-viewer-animated-export.md`
Source: review-code over `42ea6f3b08^..663cd54034` (branch `girafic-penpot-timeline-animation`)
Created: 2026-11-12

The view mode exports the animation of a board from Inspect (Static /
Animated tabs) in the export tab of the workspace (`:export-animation`
route, `app.main.data.exports.animation-tab`). The review asked for
changes:

- **R1** — the panel can hang: it ends a job only when the tab says
  `:done`/`:error`/`:cancelled`/`:closed`. A tab that dies silently
  (out of memory on a phone, discarded) leaves "Exporting… N%" for
  ever, and Cancel disables the button with nothing to free it. The
  workspace has three guards the view mode lacks: the tab's Web Lock
  released, a cancel timeout (`cancel-wait-ms`), a hello timeout that
  offers to open the tab again (`hello-wait-ms`).
- **R2** — the tab waits for `:saved` (it says `:hello` every 2 s, gives
  up after 30 s with "The workspace did not answer"), and only the
  mounted `animated-export*` answers it. Switching to Static, selecting
  another shape or closing the view mode before the tab's first hello,
  or iOS suspending the view-mode tab in the background, strands it.
- **R3** — the view-mode flow has no tests.
- **M1** — boards animated only through component copies get no
  Animated tab, and the copies' sound and clock are ignored (the
  `cltl` helpers exist; the parent plan's Decision 4 asked for them).
- **M2** — `vej/started`/`vej/heard` and `end-types` duplicate the
  workspace's job state (`deaw/start-export-job`, `job-heard`).
- **M3** — `(.finally done)` in `exports_animation_formats_test`.
- **M4–M8, S1, S2** (user decision: all included) — codec check
  stricter than Mediabunny's, the shared panel requiring a workspace UI
  namespace, `!` names, fixed `setTimeout` waits in a test, small
  leftovers, a third copy of the format list, an indirect
  `select-direction` default.

Fixed constraints (from the parent plan and the user):
- team members only; the settings of the workspace, for this export
  only (the view mode saves nothing); phones too;
- the view mode must not require `app.main.data.exports.animation`
  (`dea`: Mediabunny, gifenc, the AVIF encoder) nor workspace
  namespaces (`dwa`, `deaw`, the workspace export menu), which would
  move them into `:shared`;
- the per-format issue hints stay out of the view mode
  (`dea/export-issues` needs the Lottie/SVG converters); documented,
  not built.

## Affected Modules

frontend; common (`app.common.logic.timelines`, one composition helper).

## Architecture Decisions

1. **Nothing to save is part of the job (R2).** `job/schema:params`
   gets an optional `:saved?` (URL `saved=true`). A tab whose job has
   it posts one `:hello` once it holds the lock and loads at once
   (`saved`), never `lock-held`/`stale`. The view mode builds its jobs
   with it and no longer answers hellos. The workspace is unchanged.
2. **One progress state machine (M2).** `animation-job` (light, no
   potok) gets `hello-wait-ms`, `cancel-wait-ms`, `started`, `heard`
   (what a message does to the progress map; unchanged for end types)
   and `ended?` (`:done :error :cancelled :closed`). `deaw` uses them.
3. **The view-mode job lives in the store (R1).** `vej` becomes a few
   potok events mirroring `deaw` without the save step, under
   `:viewer-export-animation`: `start-job`, `job-heard`, `job-ended`,
   the hello wait (offer to open the tab again), `job-tab-reopened`,
   `cancel-job` (cancel timer), and after the first `:hello` the lock
   watch (`job/when-released`). Without Web Locks (plain http, as the
   phone snapshot) the lock watch never fires and the cancel timer is
   the way out. The panel derefs a ref; unmounting it ends nothing, so
   switching tabs or the selection keeps the export and its progress.
   Plumbing is mirrored rather than shared with `deaw`, which is tied
   to persistence and `dea` (notes, in-place export) and fully tested;
   the state logic itself is shared (Decision 2).
4. **One rule for the boards and their timelines (M1).** A light
   `cltl/with-copies` composes `with-copies-clock`,
   `with-instance-tracks` and `with-instance-clips`;
   `dwa/board-export-timeline` uses it after its own bake steps, and a
   new light `dv/board-export-timeline` (beside `dv/board-clock`) uses
   it without them. `animation-export-boards` moves from the workspace
   menu into `uea`; both entry points use it.
5. **Light panel dependencies (M5).** The issue list component moves to
   `app.main.ui.exports.issues`; the shared panel hides "Export here"
   when no `on-export-here` is given (the view mode cannot export in
   place).

## Risks and Considerations

- `deaw` refactor (Task 1) touches a working, tested flow: the
  workspace tests must stay green unchanged in intent.
- Module placement: `dv/board-export-timeline` and `cltl/with-copies`
  must stay light (`cltl` is already in the viewer graph through
  `dv/board-clock`). Re-check the release bundles.
- A view-mode job that outlives its board selection: the panel of
  another board shows the running job's progress (as the workspace
  does); one job at a time.
- iOS Safari opens the tab only within the tap: building the job and
  `window.open` stay synchronous in the click; the event is emitted
  after.
- Messages missed while the view-mode tab is suspended: on return the
  progress resumes; a missed `:done` is covered by the lock watch
  (https) or the user's Cancel (timer).

## Approach

Foundation first (shared state, the tab), then the view-mode flow on
it, then the copies, then the small findings.

## Task List

### Task 1: Shared progress of a job

**Description:** Move `hello-wait-ms` and `cancel-wait-ms` from `deaw`
into `app.main.data.exports.animation-job`; add `started`, `heard`,
`ended?` there. `deaw/start-export-job` and `job-heard` use them. Fix
M3 on the way.

**Rationale:** The view mode needs the same state logic without `deaw`
(heavy); the review's root cause of R1 (M2).

**Code sketch:**
```clojure
(defn started [job] {:in-progress true :in-tab? true :job job :format (:format job)
                     :current 0 :total 0 :percent 0 :cancelling? false
                     :paused? false :opened? false :offer? false})
(defn heard [progress {:keys [type] :as message}]
  (case type
    :hello    (assoc progress :opened? true :offer? false)
    :progress (merge progress (select-keys message [:phase :current :total :percent]))
    :paused   (assoc progress :paused? true)
    :resumed  (assoc progress :paused? false)
    progress))
(defn ended? [{:keys [type]}] (contains? #{:done :error :cancelled :closed} type))
```

**Acceptance criteria:**
- [ ] `export_animation_job_test` covers `started`, `heard` (each type,
      end types unchanged) and `ended?`.
- [ ] `deaw` has no own copy of the initial map or the message update.
- [ ] `exports_animation_formats_test` ends with
      `(.catch …)` and `(.finally (fn [] (done) nil))` (M3).

**Verification:**
- [ ] `/home/penpot/fe-test-focus.sh frontend-tests.data.export-animation-job-test frontend-tests.data.export-job-workspace-test frontend-tests.data.exports-animation-formats-test` (the script runs the last `--focus` only: run each, or `node target/tests/test.js --focus <ns>` after the first build).
- [ ] cljfmt + clj-kondo on the touched files (in the devenv).

**Dependencies:** None

**Files likely touched:**
- `frontend/src/app/main/data/exports/animation_job.cljs`
- `frontend/src/app/main/data/exports/animation_workspace.cljs`
- `frontend/test/frontend_tests/data/export_animation_job_test.cljs`
- `frontend/test/frontend_tests/data/exports_animation_formats_test.cljs`

**Estimated scope:** S

### Task 2: A job with nothing to save

**Description:** Optional `:saved?` in `job/schema:params`,
`encode-params` (`saved=true`) and `decode-params`. In
`animation-tab/initialize`, once the lock is held: with `:saved?` the
tab opens its channel, posts one `:hello` and emits `saved`; without,
`lock-held` as now.

**Rationale:** R2; the view mode's answer is a constant, so it belongs
in the job, not in a round trip that needs the view mode alive.

**Acceptance criteria:**
- [ ] `encode-params`/`decode-params` round-trip `:saved?`; absent stays
      absent.
- [ ] A tab with `:saved?` reaches `:loading` with nobody on the channel,
      posts exactly one `:hello`, never shows `:waiting`/`:stale`.
- [ ] A tab without it behaves as before (existing tests green).

**Verification:**
- [ ] `fe-test-focus.sh` for `frontend-tests.data.export-animation-job-test` and `frontend-tests.ui.export-animation-test`.
- [ ] cljfmt + clj-kondo.

**Dependencies:** None

**Files likely touched:**
- `frontend/src/app/main/data/exports/animation_job.cljs`
- `frontend/src/app/main/data/exports/animation_tab.cljs`
- `frontend/test/frontend_tests/data/export_animation_job_test.cljs`
- `frontend/test/frontend_tests/ui/export_animation_test.cljs`

**Estimated scope:** S

### Task 3: The view-mode job as events

**Description:** Rewrite `app.main.data.viewer.export-job` as potok
events under `:viewer-export-animation` (Decision 3): `start-job`
(state `job/started`, channel open while the job lasts, messages →
`job-heard`, hello wait → offer), `job-heard` (state `job/heard`;
`:error` → one `ntf/error` with `job/error-text`; `ended?` →
`job-ended`; first `:hello` → `job/when-released` → `job-ended`),
`job-ended` (dissoc, close the channel, post a message if any),
`job-tab-reopened`, `cancel-job` (not opened → end at once; opened →
post `:cancel`, end after `job/cancel-wait-ms` unless ended). Jobs are
built with `:saved? true`; `open-job-tab` (no `!`) opens the tab in the
click. A ref in `refs` (or local `l/derived`) reads the state key.
`inspect/exports` `animated-export*` derefs it, emits the events, keeps
no channel refs and ends nothing on unmount; passes `on-open-tab` (open
the tab again + `job-tab-reopened`) and no `on-export-here`. `uea`
hides "Export here" without `on-export-here`. Drops `job-of`,
`end-types`, the `(some-> f (apply [x]))` calls (M8), the `!` names
(M6).

**Rationale:** R1, R3, M2 consumer; the store keeps the job through
remounts (R2's remount half).

**Acceptance criteria:**
- [ ] `viewer_export_job_test` (rewritten, `thj/broadcast-channels`,
      `thj/locks`, `thj/wait-until`, no fixed timeouts — M7):
      - `start-job` → "Opening…" state; `:hello` → opened, 0 %;
        `:progress 40` → 40 %;
      - `:done` frees the state and closes the channel;
      - `:error` emits one error notification, frees the state;
      - Cancel before `:hello` frees at once; after, posts `:cancel` and
        frees on `:cancelled`;
      - Cancel after `:hello` with no answer frees after
        `cancel-wait-ms` (short in the test);
      - the tab's lock released without `:closed` frees the state;
      - no `:hello` within `hello-wait-ms` sets the offer;
        `job-tab-reopened` clears it.
- [ ] `inspect_exports_test` still green; the panel shows the progress
      from the store.
- [ ] Without `on-export-here` the shared panel shows only "Open the
      tab again" in the offer.

**Verification:**
- [ ] `fe-test-focus.sh frontend-tests.data.viewer-export-job-test`, then `node target/tests/test.js --focus frontend-tests.ui.inspect-exports-test` and `frontend-tests.ui.exports-menu-test`.
- [ ] cljfmt + clj-kondo; stylelint/prettier if scss changes.

**Dependencies:** Task 1, Task 2

**Files likely touched:**
- `frontend/src/app/main/data/viewer/export_job.cljs`
- `frontend/src/app/main/ui/inspect/exports.cljs`
- `frontend/src/app/main/ui/exports/animated.cljs`
- `frontend/src/app/main/refs.cljs` (if the ref goes there)
- `frontend/test/frontend_tests/data/viewer_export_job_test.cljs`

**Estimated scope:** M

### Checkpoint 1: After Tasks 1–3

- [ ] Focused tests of Tasks 1–3 green.
- [ ] Browser (dev build, `https://localhost:3449`, harness account):
      desktop view mode exports a Lottie and an MP4 (release WASM:
      rebuild it after the test runs); switching to Static and back
      during the export keeps the progress; closing the export tab
      frees the panel; Cancel with the tab frozen frees it after 10 s.
- [ ] Emulated iPhone: Lottie from the view mode, done.
- [ ] Workspace export panel unchanged (Lottie + MP4 from the editor).

### Task 4: Boards animated by their component copies

**Description:** Add `cltl/with-copies` (Decision 4) and use it in
`dwa/board-export-timeline`. Add a light `dv/board-export-timeline
[page files board-id]` (own timeline or a new one, with the copies;
nil when neither). Move `animation-export-boards` into `uea` (same
signature), used by the workspace menu and by `inspect/exports`, which
builds `timelines` with `dv/board-export-timeline` over `refs/files`
and replaces `animated-boards`.

**Rationale:** M1 against the parent plan's Decision 4; one selection
rule for both entry points.

**Acceptance criteria:**
- [ ] `timelines_test` (common) covers `with-copies` (clock, tracks,
      clips of a copy).
- [ ] The workspace's `board-export-timeline` tests stay green.
- [ ] A view-mode test: a board without a timeline but with an animated
      copy gets an Animated tab; one whose copy's main has audio shows
      the audio switch (`clips?`); its duration is the copies' clock.
- [ ] `exports_menu_test` uses `uea/animation-export-boards`.

**Verification:**
- [ ] common: `clojure -M:dev:test --focus common-tests.logic.timelines-test` (in the devenv, `common/`).
- [ ] frontend: `fe-test-focus.sh frontend-tests.ui.inspect-exports-test`, then `node target/tests/test.js --focus` for `frontend-tests.ui.exports-menu-test` and the dwa animation tests.
- [ ] cljfmt + clj-kondo (common and frontend).

**Dependencies:** Task 3 (same component)

**Files likely touched:**
- `common/src/app/common/logic/timelines.cljc`
- `common/test/common_tests/logic/timelines_test.cljc`
- `frontend/src/app/main/data/workspace/animation.cljs`
- `frontend/src/app/main/data/viewer.cljs`
- `frontend/src/app/main/ui/exports/animated.cljs`
- `frontend/src/app/main/ui/workspace/sidebar/options/menus/exports.cljs`
- `frontend/src/app/main/ui/inspect/exports.cljs`
- tests: `inspect_exports_test`, `exports_menu_test`

**Estimated scope:** M

### Task 5: The small findings

**Description:**
- M4: align `daf` codec strings with Mediabunny's (AVC High
  `avc1.6400xx` as Mediabunny builds it, `hev1.…`, `vp8` in MP4);
  docstring: optimistic on Firefox.
- M5: move `app.main.ui.workspace.sidebar.options.menus.export-issues`
  to `app.main.ui.exports.issues` (placement not tied to the workspace
  sidebar); note in `uea/animated-export*` and the inspect docstring
  that the view mode shows no issues.
- M8: remove the unused `@use "ds/_sizes.scss"` of the workspace
  `exports.scss` (the rest of M8, `job-of` and the `some->`/`apply`
  calls, goes in Task 3). The parent plan stays as executed: its
  Decision 3 (the channel closed at the first `:hello`) is superseded by
  Decision 3 of this plan, which records the design as built.
- S1: one ordered `daf/formats`; `dea/export-formats` order and
  `job/formats` and the inspect list derive from it.
- S2: `uea` `select-direction` defaults to auto; the workspace passes
  `"up"`.

**Rationale:** user decision: the review fully addressed.

**Acceptance criteria:**
- [ ] `exports_animation_formats_test`: a fake encoder accepting only
      the strings Mediabunny asks for says MP4 yes; one accepting only
      the old Baseline string says no (or as Mediabunny decides).
- [ ] No namespace under `app.main.ui.workspace` required by `uea`.
- [ ] One literal list of formats.
- [ ] Workspace panel selects open upward as before; the view mode's
      as they have room.

**Verification:**
- [ ] Focused tests: formats, exports-menu, inspect-exports, export-animation-job.
- [ ] Lint: cljfmt, clj-kondo, stylelint, prettier (devenv).

**Dependencies:** Task 4

**Files likely touched:**
- `frontend/src/app/main/data/exports/animation_formats.cljs`
- `frontend/src/app/main/data/exports/animation.cljs`
- `frontend/src/app/main/data/exports/animation_job.cljs`
- `frontend/src/app/main/ui/exports/animated.cljs`, new `issues.cljs` (+ scss)
- `frontend/src/app/main/ui/workspace/sidebar/options/menus/exports.cljs` + `.scss`, `export_issues.cljs` (moved)
- `frontend/src/app/main/ui/inspect/exports.cljs`
- tests: formats, exports-menu

**Estimated scope:** M

### Checkpoint 2: After Tasks 4–5

- [ ] Full frontend run green: `/home/penpot/full-tests.sh`; common
      focused test green.
- [ ] Release bundle (`release-serve.sh`): `shared.js` and
      `main-viewer.js` without Mediabunny/gifenc; `shared.js` size about
      as before (6.99 MB).
- [ ] Browser: a board animated only by copies exports from the view
      mode; the audio switch shows for a copy's sound.
- [ ] Release WASM rebuilt after the test runs; snapshot rebuilt.

## Verification and Testing

Per task: `/home/penpot/fe-test-focus.sh <ns>` in the devenv (one
`--focus` per run; further namespaces with
`node target/tests/test.js --focus <ns>`), stop Storybook first and
restart it after. Lint in the devenv: `cljfmt check`, `clj-kondo
--config-dir ../.clj-kondo --lint`, `npx stylelint`, `npx prettier
--check`. Never pnpm on the Mac. Browser checks with Playwright against
the dev build (harness account) and the release snapshot.

## Parallelization

Tasks 1 and 2 are independent; Task 3 needs both. Task 4's common part
(`with-copies`) could start in parallel with Tasks 1–3, its frontend
part after Task 3. Task 5 last.

## Open Questions

None (the small findings: all included, user 2026-11-12).
