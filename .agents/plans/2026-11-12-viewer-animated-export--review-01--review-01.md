# Plan: Animated export in the view mode — second review fixes

Status: done
Review Log:
- 2026-11-13T18:29:59Z — done

## Context

Parent: `2026-11-12-viewer-animated-export--review-01.md`
Source: review-code over `3a369072ae^..c695ac9bcb` (branch `girafic-penpot-timeline-animation`)
Created: 2026-11-13

The parent plan made the view mode follow its animated export jobs as
the workspace does, offered boards animated by component copies, and
fixed a wrong `type` passed by inspect's styles tab. The review of its
nine commits asked for changes:

- **R1 (High)** — no regression test for the `type` bug: `inspect/styles.cljs`
  passed an unbound `type` (so `cljs.core/type`) to `inspect.exports/exports`;
  several selected shapes exported the first alone, no board got the
  Animated tab. Lint and two reviews missed it.
- **M1** — since that fix, a selected board that is a copy of a variant
  component reaches `uea/animation-export-boards` as `:variant` and gets no
  Animated tab in the view mode, though the workspace offers it (it passes
  `:frame`).
- **M2** — the job plumbing (channel life, hello wait, lock watch, cancel
  timer, end) is copied line for line between `vej` and `deaw`; the last
  review's R1 was drift between two such copies.
- **M3** — the panel wiring has no tests (offer without "Export here",
  progress from the store over a remount, the tab opened in the click before
  `start-job`, reopening, the audio switch).
- **S1** — only the single `:hello` of a view-mode tab (`:saved`) marks the
  tab as there; if it is lost while the view mode sleeps, the panel says
  "did not open" while progress arrives, and the lock watch never starts.
- **M4** `:saved?` is a new `?` map key (the project rule forbids new ones);
  **M5** blank lines around `cltl/with-copies`; **M6** the new tab test uses
  the legacy `mock/with-mocks`; **S2** "own timeline or a new one" is written
  in `dv` and `dwa` both.

Fixed constraints: the view mode must not require
`app.main.data.exports.animation` (`dea`) nor workspace namespaces (they
would move into `:shared`); no new `!` names; no new `?` map keys; UI
strings only in `en.po`; workspace behaviour unchanged.

## Affected Modules

frontend; common (`app.common.logic.timelines`, S2).

## Architecture Decisions

1. **One follow namespace (M2).** New light
   `app.main.data.exports.animation-follow` (requires only `job`, `ntf`,
   `st`, `rx`, `ptk`): the jobs registry, `job-ended`, `job-heard`, the
   hello wait, `start-job`, `job-tab-reopened`, `cancel-job`, all taking the
   state key (`:export-animation`, `:viewer-export-animation`) and hooks:
   `:extra` (a fn of job-id and stream giving more events to merge at start:
   the workspace's force-persist and save outcome), `:on-hello` (an effect
   at each `:hello`: the workspace posts its saved outcome), `:done-events`
   (events on `:done`: the workspace's `dea/notes-shown`). `deaw` and `vej`
   keep their public names as thin wrappers (`start-export-job`,
   `job-tab-reopened`, `cancel-export-animation`, `start-job`, `cancel-job`
   …), so callers and most tests stay; `deaw` keeps the in-place parts
   (`export-in-tab?`, `export-job-here`, `cancel-in-place!`).
2. **Any message proves the tab (S1).** `job/heard`: every message that
   does not end the job sets `:opened? true :offer? false`; the lock watch
   starts at the first such message; the hello wait stops at any message of
   the job. The workspace still answers its save only on `:hello`.
3. **Boards by shape, not by kind (M1).** `uea/animation-export-boards`
   drops its `type` argument: `(every? cfh/frame-shape? shapes)` decides
   "only boards"; both entry points call it with shapes and timelines.
4. **Regression test at the call sites (R1).** A test renders the two
   callers of `exports` (`styles-tab*`, `attributes*`) with
   `inspect.exports/exports` replaced by a stub that records its props, and
   asserts the kind passed (`:frame` for one board, `:multiple` for two).
   The upstream component is not converted to a schema'd `exports*` (fewer
   conflicts in upstream merges); the test is the guard.
5. **`:saved` (M4).** The job key loses its `?` (`:saved`), as the project
   rule asks; the URL param stays `saved`.
6. **`cltl/board-timeline` (S2).** The pure "own timeline or a new one
   named after the board" moves from `dwa/board-timeline` to common;
   `dwa` and `dv/board-export-timeline` use it.

## Risks and Considerations

- Task 1 refactors the workspace export flow, which is fully tested
  (`export_job_workspace_test`, 12 tests): they must stay green with no
  change of intent; only private helpers move.
- The follow namespace must stay light: check the release bundles
  (`shared.js`, `main-viewer.js` without Mediabunny) after Task 1.
- S1 changes when the panel counts the tab as opened; the workspace's
  "Open the tab again / Export here" offer then disappears at a progress
  message too (it is there).
- A DOM test of `styles-tab*` may need some store state (tokens refs);
  seed what it reads, as the inspect DOM tests do.

## Approach

Refactor first, behaviour-preserving (Task 1), then the one behaviour
change on the shared code (Task 2), then the independent fixes and tests
(Tasks 3–5), then the small findings (Task 6).

## Task List

### Task 1: One follow namespace for both sides

**Description:** Create `app.main.data.exports.animation-follow`
(Decision 1) from the private plumbing of `vej` and `deaw`; make both thin
wrappers over it.

**Rationale:** M2; the drift the last review found lived exactly there.

**Code sketch:**
```clojure
(ns app.main.data.exports.animation-follow
  "How the side that started an animated export job follows its tab:
  how far it is under `state-key`, the hello wait, the lock watch, the
  cancel timer. Light: no encoders, no persistence."
  (:require [app.main.data.exports.animation-job :as job]
            [app.main.data.notifications :as ntf]
            [app.main.store :as st]
            [beicon.v2.core :as rx]
            [potok.v2.core :as ptk]))

(defn post [job-id message] ...)
(defn job-ended ([state-key job-id] ...) ([state-key job-id message] ...))
(defn start-job [state-key job & {:keys [extra on-hello done-events channel-ctor
                                         locks hello-wait-ms cancel-wait-ms]}] ...)
(defn job-tab-reopened [state-key job-id & {:as opts}] ...)
(defn cancel-job [state-key] ...)
```

**Acceptance criteria:**
- [x] `vej` holds only `board-job`, `open-job-tab` and wrappers; `deaw`
      only the workspace parts (job, save, notes, in place) and wrappers.
- [x] `export_job_workspace_test` and `viewer_export_job_test` green,
      unchanged in intent.
- [x] No namespace of the view mode requires `dea`/`deaw` (require walk).

**Verification:**
- [x] `/home/penpot/fe-test-focus.sh frontend-tests.data.viewer-export-job-test`, then `node target/tests/test.js --focus` for `frontend-tests.data.export-job-workspace-test`, `frontend-tests.ui.export-animation-test`, `frontend-tests.data.export-animation-job-test`.
- [x] cljfmt + clj-kondo (devenv).

**Dependencies:** None

**Files likely touched:**
- new `frontend/src/app/main/data/exports/animation_follow.cljs`
- `frontend/src/app/main/data/exports/animation_workspace.cljs`
- `frontend/src/app/main/data/viewer/export_job.cljs`

**Estimated scope:** M

### Task 2: Any message of the tab proves it is there

**Description:** Decision 2 in `job/heard` and the follow namespace (lock
watch at the first non-ending message, hello wait stopped by any message
of the job).

**Rationale:** S1; makes the iPhone case (view mode asleep at the one
`:hello`) safe, once, for both sides.

**Acceptance criteria:**
- [x] `export_animation_job_test`: a `:progress` with no `:hello` before it
      sets `:opened?` and clears `:offer?`; end messages leave it as it is.
- [x] `viewer_export_job_test`: a `:progress` first (no `:hello`) opens the
      panel, stops the offer, and the tab's lock gone afterwards frees it.
- [x] A message of another job changes nothing.
- [x] The workspace still posts its save at `:hello` only (its tests).

**Verification:**
- [x] Focused tests as in Task 1.

**Dependencies:** Task 1

**Files likely touched:**
- `frontend/src/app/main/data/exports/animation_job.cljs`
- `frontend/src/app/main/data/exports/animation_follow.cljs`
- tests: `export_animation_job_test`, `viewer_export_job_test`

**Estimated scope:** S

### Checkpoint 1: After Tasks 1–2

- [x] Focused tests green.
- [x] Browser (dev build, release WASM rebuilt after the tests): view mode
      desktop Lottie + MP4, switch Static/Animated during the export, close
      the export tab mid-export (panel free); emulated iPhone Lottie;
      workspace export from the editor (Lottie).
- [x] Release bundles: `shared.js`/`main-viewer.js` without Mediabunny.

### Task 3: Boards by shape (M1)

**Description:** Decision 3: `uea/animation-export-boards [shapes
timelines]`; update the workspace menu and inspect call sites and
`exports_menu_test` (the `:rect`-type case goes; a rect shape still
hides the tab).

**Acceptance criteria:**
- [x] A frame that is a copy of a variant component (`:component-id`,
      `:variant-id` set) with a timeline is returned.
- [x] Mixed selections (a rect) and empty ones return nil.

**Verification:**
- [x] `fe-test-focus.sh frontend-tests.ui.exports-menu-test`, then `inspect-exports-test`.

**Dependencies:** None

**Files likely touched:**
- `frontend/src/app/main/ui/exports/animated.cljs`
- `frontend/src/app/main/ui/inspect/exports.cljs`
- `frontend/src/app/main/ui/workspace/sidebar/options/menus/exports.cljs`
- `frontend/test/frontend_tests/ui/exports_menu_test.cljs`

**Estimated scope:** S

### Task 4: The kind of the selection reaches the exports (R1)

**Description:** Decision 4: a test renders `styles-tab*` and
`attributes*` with `app.main.ui.inspect.exports/exports` stubbed (records
its props) for one board and for two shapes.

**Acceptance criteria:**
- [x] Both callers pass `:type` `:frame` for one board and `:multiple` for
      two shapes — a keyword, never a function.
- [x] Reverting `7cfa20e221` (`:type type` in styles) makes the test fail.

**Verification:**
- [x] `fe-test-focus.sh <the new test ns>`; registered in the runner
      (both lists, cljfmt sorted).

**Dependencies:** None (Task 3 if it touches the same test file)

**Files likely touched:**
- new `frontend/test/frontend_tests/ui/inspect_exports_kind_test.cljs`
- `frontend/test/frontend_tests/runner.cljs`

**Estimated scope:** S

### Task 5: Tests of the panel wiring (M3)

**Description:** DOM tests (`thd/with-dom`, `mock/with-mocks*`):
- `uea/animated-export*` with an offer: without `on-export-here` one
  offer button ("Open the export tab"), with it two;
- the inspect `animated-export*` (private, `@#'`): a click on Export calls
  the mocked `dom/open-new-window` before `::vej/start-job`… (record the
  order), sets `:viewer-export-animation`; unmounted and mounted again it
  shows the store's progress; in an offer, "Open the export tab" emits the
  reopen of the same job;
- the audio switch shows for MP4 with the copies' sound (the
  `inspect_exports_test` scene with `:sound? true`).

**Acceptance criteria:**
- [x] The three groups above pass; none relies on fixed timeouts.

**Verification:**
- [x] `fe-test-focus.sh <test ns>`; then the other export tests.

**Dependencies:** Task 1 (event names), Task 3

**Files likely touched:**
- `frontend/test/frontend_tests/ui/inspect_exports_test.cljs` (or a new
  `inspect_animated_export_test.cljs` + runner)
- maybe `frontend/test/frontend_tests/ui/exports_menu_test.cljs`

**Estimated scope:** M

### Task 6: The small findings

**Description:**
- M4: `:saved?` → `:saved` in `job/schema:params`, `job-params`,
  `encode-params`/`decode-params`, the tab, `vej/board-job`, tests.
- M5: one blank line before `cltl/with-copies` and before `layer-ids`.
- M6: the new tab test (`a-tab-with-nothing-to-save-loads-at-once`) on
  `mock/with-mocks*`.
- S2: `cltl/board-timeline` (Decision 6), used by `dwa` and `dv`.

**Acceptance criteria:**
- [x] No `:saved?` left; round trip and tab tests green.
- [x] `dwa`/`dv` tests green; a common test for `cltl/board-timeline`.

**Verification:**
- [x] Focused frontend tests; `clojure -M:dev:test --focus common-tests.logic.timelines-test` (devenv, `common/`).
- [x] Lint: cljfmt, clj-kondo.

**Dependencies:** Tasks 1–5

**Files likely touched:**
- `frontend/src/app/main/data/exports/animation_job.cljs`,
  `animation_tab.cljs`, `viewer/export_job.cljs`, `data/viewer.cljs`,
  `data/workspace/animation.cljs`
- `common/src/app/common/logic/timelines.cljc` + test
- tests: `export_animation_job_test`, `export_animation_test`,
  `viewer_export_job_test`

**Estimated scope:** M

### Checkpoint 2: After Tasks 3–6

- [x] Full run `/home/penpot/full-tests.sh` green.
- [x] Browser: view mode multi-selection static export (dialog with all),
      single board Animated tab desktop + phone, a variant-copy board if one
      is at hand (none was: covered by `exports_menu_test`); release WASM and snapshot rebuilt; bundles checked.
- [x] Acceptance boxes of this plan ticked as they pass.

## Verification and Testing

Per task `/home/penpot/fe-test-focus.sh <ns>` (one `--focus` per run;
more with `node target/tests/test.js --focus <ns>`), Storybook stopped
first and restarted at the end; lint in the devenv (cljfmt, clj-kondo
`--config-dir ../.clj-kondo`, stylelint/prettier if scss). Never pnpm on
the Mac. Release WASM rebuilt after test runs, before browser checks.

## Parallelization

Tasks 1→2 in order; Tasks 3 and 4 independent of them; Task 5 after 1
and 3; Task 6 last.

## Open Questions

None (the small findings included as last time; M4 settled by the
project rule).
