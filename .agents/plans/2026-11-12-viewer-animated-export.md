# Plan: Animated export in the view mode

Status: done
Review Log:
- 2026-11-12T13:30:00Z — done

## Context

Inspect in the view mode exports a board as PNG/JPG/SVG/PDF only. The
workspace also exports the animation of a board (MP4, WebM, GIF, AVIF,
SVG, Lottie, dotLottie) from its Export panel (`animated-exports*` in
`app.main.ui.workspace.sidebar.options.menus.exports`), in a tab of its
own (`:export-animation` route, `app.main.data.exports.animation-tab`)
that loads the file by `dv/load-bundle` and renders it there.

User decisions (2026-11-12):
- team members only (the tab loads the file as only they may);
- all the settings of the workspace (format, size, quality, fps, loop,
  audio, SVG options), for this export only: the view mode saves nothing;
- on the phone too.

Constraint: the view mode must not require
`app.main.data.exports.animation` (`dea`: Mediabunny, gifenc, the AVIF
encoder) nor workspace namespaces (`dwa`, `deaw`, the workspace export
menu): shadow-cljs would move them into the `:shared` module, which every
page loads, undoing the memory diet of the phone (see the
`touch-devices` notes: view mode 829 → 617 MB).

## Affected Modules

frontend only.

## Architecture Decisions

1. **Light helpers.** Move the pure, light parts the panel needs out of
   `dea` into a new `app.main.data.exports.animation-formats`:
   `format-labels`, `audio-formats`, `estimated-size`,
   `approx-size-label`, `codec-supported?` (WebCodecs/`isConfigSupported`
   only; check it needs no Mediabunny, else keep a light variant). `dea`
   re-exports or requires them, so its callers stay as they are.
2. **One settings component.** Move the fields and the button of
   `animated-exports*` into `app.main.ui.exports.animated` as
   `animated-export*`: it gets the `export` settings, `on-change` (a map
   merged into them), the boards and what it needs of their timelines
   (duration, audio clips), the button `view` and its handlers. The
   workspace wraps it as now (`dwa/set-export` in one undo transaction,
   `deaw` for the job); the view mode with its own state.
3. **The job from the view mode.** A light
   `app.main.data.viewer.export-job`: builds the job params (file, page,
   board, settings, `wasm?`), opens the tab (`job/encode-params`, route
   `:export-animation`, `noopener`, in the click) and answers the tab on
   its channel (`job/open-channel`): `:saved` at its first `:hello` (the
   view mode has nothing to save), closing the channel once it heard it.
   The tab shows the progress and the download itself; the panel shows
   "Opening the export tab…" until the tab answered, then is free again.
   No in-place export in the view mode (it needs `dea`).
4. **Timelines in the view mode.** The view mode has the page data
   (`:timelines`); the board's own timeline is
   `(get-in page [:timelines board-id])`. Component copies with their own
   animations inside a board without a timeline: shown only if a light
   pure function for them exists in common (`cltl`); else left out at
   first (the tab still exports what the workspace exports).

## Risks and Considerations

- Module placement: verify with the release build (release-serve) that
  `mediabunny`/`gifenc` stay out of `shared.js` and `main-viewer.js`
  (grep the bundles), and compare the view mode memory on the phone
  probe (`checks/touch/buildup.mjs`).
- iOS Safari opens the tab only within the tap (no `await` before
  `window.open`).
- A long animation may exhaust the memory of a phone in the tab: the tab
  reports a failure as in the workspace.

## Task List

### Task 1: Light export helpers

Move the helpers of Decision 1 into
`app.main.data.exports.animation-formats`; `dea` and its callers use
them. Tests: the existing export tests stay green; a test for
`estimated-size` / `approx-size-label` moves along.

Files: `src/app/main/data/exports/animation.cljs`, new
`animation_formats.cljs`, `src/app/main/ui/workspace/sidebar/options/menus/exports.cljs`,
`src/app/main/ui/export_animation.cljs`, tests.
Scope: M.

### Task 2: Shared settings component

`app.main.ui.exports.animated/animated-export*` (Decision 2), the
workspace panel a thin wrapper over it; same look and behaviour in the
workspace. Tests: the workspace export panel tests stay green.

Files: new `src/app/main/ui/exports/animated.cljs` + `.scss`, the
workspace exports menu, tests.
Scope: M.

### Checkpoint 1

Workspace export panel unchanged (shots), frontend tests of the
exports green.

### Task 3: The job from the view mode

`app.main.data.viewer.export-job` (Decision 3), with tests of the params
and of the answer on a fake channel.

Files: new `src/app/main/data/viewer/export_job.cljs`, test.
Scope: S.

### Task 4: Inspect exports the animation

In `app.main.ui.inspect.exports`, for a member of the team and a board
with a timeline: the animated export below the image exports, local
settings from the board's `:export` (Decision 4), the button opening the
tab. Phone and desktop.

Files: `src/app/main/ui/inspect/exports.cljs` (+ scss), test.
Scope: M.

### Checkpoint 2

Browser: desktop and phone (emulated) export an MP4 and a Lottie from
the view mode; release build: no Mediabunny in `shared.js`/`main-viewer.js`.

## Verification and Testing

`/home/penpot/fe-test-focus.sh <ns>` per task; lint (cljfmt, clj-kondo,
stylelint, prettier) in the devenv; release snapshot for the bundle
check.

## Open Questions

None (decisions above).
