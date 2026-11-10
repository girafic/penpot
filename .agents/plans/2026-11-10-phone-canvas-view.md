# Plan: The canvas of a file in the view mode of a phone

Status: done
Review Log:
- 2026-11-10T18:00:03Z — applied review-plan findings 1–20 and the user's answers: share links (`index "0"`) start on the canvas on phones; `canvas` param is true/false/absent, every way to a board leaves the canvas in one push; `dvc` pure (no require cycle), events in `dv`; live view in a component ref; per-board clock from when a board comes into view (`cta/playback-time`, no `:loop-count`, no `board-time-at`); a tap opens the top-level board, a double tap zooms to the largest board up to half the visible width; fallback ladder below 30 fps, then back to the user; WASM-only start, `:phone-canvas` flag; render-queue seam; objects as designed with hidden top-level boards pruned; sheet on the canvas specified; spike split from the component, memory ceiling absolute; geometry edge cases; gesture math moved to `app.util.touch`; copy-pasteable devenv commands.
- 2026-11-10T19:13:58Z — Task 4 spike (Mac WebKit, iPhone 13 mini profile, dev build + release WASM, Playground now 4520 × 50425, 2822 shapes, canvas objects identical to the board view's): (B) set-viewer-viewport! + render-sync per frame 36 ms (p90 52) at the start view, 85 ms close up; (A) own canvas pans at 1 ms/frame but loads the page again on every switch (1.4 s on the Mac); (C, chosen) the shared OffscreenCanvas resized to the screen, `_set_view` + `_render_from_cache` per frame 1 ms (60 fps), render-sync to settle 16–61 ms, first full render 250 ms, no reload on a switch. Animation at the start view: 13 autoplay boards in view (not 20–45), trees 3 ms + show 2–3 ms + render-sync 45 ms ≈ 20 fps on the Mac; render-sync-shape-again of the root 340 ms (no use). iPhone memory and fps not measured yet (user away): taken at the checkpoint after Task 5 on the release snapshot.
- 2026-11-10T20:47:32Z — done (no issue). Mac WebKit: animations at the start view of the Playground ≈ 32 fps (a frame renders once the one before did), no sound; tap → board 0.4 s, back and reload as planned. Still open with the user: memory and fps on the real iPhone 13 mini, the margin of the start view.

## Context

Created: 2026-11-10. The user: "wie können wir den auf mobilen telefonen die
canvas anzeigen als view mode only und nicht wie es im prototyp", then "mach
mal den plan für die Leinwand". Example file: "Penpot Motion Playground"
(one page 4520 × 21760, 88 top-level boards, 140 boards listed by the view
mode with the nested ones shown in it, many screenshots, 47 timelines, one
flow "Flow 1").

Today a phone (`touch/phone?`) shows a file only as the prototype: one
board at a time, as wide as the screen (`dv/zoom-to-width`), the next by
the options sheet (two fingers held, `ui/viewer/phone_sheet.cljs`). A file
that is a collection of boards (a tutorial, a style guide, a moodboard)
cannot be seen as a whole: the Playground opens on "Invite", one of 140
boards.

Findings in the code:
- The view mode already loads **all shapes of the page** into WASM
  (`render-viewer-wasm/render-viewer-frame*` → `wasm.api/set-objects
  page-objects`), then draws one board with `wasm.api/set-viewer-viewport!`
  + `render-sync-shape` into a shared `OffscreenCanvas` (`viewer-snapshot`)
  and blits it onto the visible canvases (`present-bitmap!`). Every viewer
  render runs on the private promise queue `enqueue-wasm-render!`
  (`render_viewer_wasm.cljs:29-34`); `render-to-canvas` (exports,
  thumbnails) swaps and clears the global context too.
- The renderer draws a free view with a tile cache: the workspace calls
  `wasm.api/initialize-viewport objects zoom vbox`, then `set-view-box`
  (`_set_view` + `_render_from_cache`, the cached tiles moved and scaled at
  once) and `request-render` (progressive tiles) as the view changes
  (`ui/workspace/viewport_wasm.cljs`, on-screen canvas via
  `init-canvas-context`, `clear-canvas` on unmount). There is no full-view
  "draw only what changed" call: `render-sync` runs `rebuild_tiles()` and a
  full synchronous render every time (`render-wasm/src/main.rs:147-155`);
  `render-sync-shape-again` is incremental per shape.
- Gesture math exists as pure functions in
  `ui/workspace/viewport/touch.cljs` (`middle`, `pinch-step`, `view-step`:
  two fingers pan and zoom about their middle), but that namespace requires
  `app.main.data.workspace` and the workspace `viewport-ref`;
  `ui/viewer/drag_scroll.cljs` has `speed`/`coast`/`rubber` (1-D coast and
  rubber band of a finger drag); the sheet's `use-two-finger-hold!` only
  fires on two fingers held still within `hold-slop-px`.
- Animation of a board in the view mode: `dv/board-clock page files
  board-id` (`data/viewer.cljs:1456`) gives the timeline a board plays on,
  its component copies' clocks included (nil when it plays nothing);
  `cta/playback-time clock elapsed` (`animation.cljc:1010`) its time for
  `:once` (held at the end), `:loop` and `:ping-pong`; `cta/autoplay?`;
  `cltl/view-playback-ended?`; `cltl/view-animation-tree page files
  board-id time elapsed` its modif-tree; `render-viewer-wasm/show-animation!`
  (private) puts a tree on the loaded shapes; `dv/frame-times` is the rAF
  clock. `cltl/merge-modif-trees` is private; `:loop-count` exists only in
  the schema (`animation.cljc:787, 807, 929-935`), nothing sets or reads it.
- Board navigation builds on `rt/get-params` and keeps the params it does
  not know: `go-to-frame-by-index` (`data/viewer.cljs:1042`, reached by
  `go-to-frame`, `go-to-frame-auto`, the sheet's flow list, `select-*-frame`),
  `go-to-page` (`:1331`, sets `:index 0`), `go-to-section` (`:1107`).
- Share links always carry `index "0"`: `ui/viewer/share_link.cljs:75-78`
  builds them from `(:query-params route)` with `(assoc :index "0")`, so
  they take the page-present branch of `bundle-fetched` (`:405-422`).
- `viewer-content*` derefs `(l/derived dv/steady-local refs/viewer-local =)`;
  `steady-local` drops only `:timeline` and `:scroll` (`:966-970`): any
  other write to `[:viewer-local …]` re-renders the view mode.
- `viewer-content*` grows top boards that scroll vertically
  (`dvs/scroll-page`, `ui/viewer.cljs:600-606`) and turns the page on a
  phone held sideways (`dvt/turn-objects`, `:630`); it shows `empty-state`
  for `(empty? frames)` before any other branch (`:1311`); the two-finger
  hint shows only once `drawn?` (`:1384`).
- `#viewer-section` carries `use-touch-rubber!` (phone + Interactions,
  `:1067`), `use-touch-taps!` (`:1072`) and `use-container-scroll!`
  (`:1075`), each with an `enabled?` argument.
- The viewer draws with WASM only with `render-wasm/v1` and
  `:available-viewer-wasm` (`ui/viewer.cljs:580-581`); otherwise SVG
  (deprecated; test accounts get SVG without `&wasm=true`).
- On a phone `update-zoom-querystring` keeps `:width` out of the URL, and
  `refit-zoom` runs whenever the shown board changes (`ui/viewer.cljs:978`).
- Since be01a0d012 a link without a page (a dashboard tap) gets the first
  page in its URL (`bundle-fetched`, replace) and reuses the bundle.
- On the real iPhone 13 mini the view mode of the small coffee prototype
  already peaks near 1024 MiB footprint, where iOS kills the tab
  (2026-11-09, `touch-devices` memory).

## Decisions (the user's, 2026-11-10)

1. **Start**: a file opened on a phone starts on the canvas, unless its
   link points at a board: a URL with `frame-id` or an `index` other than
   a share link's `"0"`, a live link (`follow=true`) and the prototype
   preview start the prototype as now. A dashboard tap and **every share
   link** (old and new, all carry `index "0"`) start on the canvas. On
   computers and tablets nothing changes.
2. **A tap on a board opens it as the prototype**: the **top-level board**
   under the finger (the screen, the whole lesson strip), never a board
   nested in it. One step in the history: the back gesture returns to the
   canvas, as does a "Canvas" entry in the sheet. A double tap zooms to a
   board and from there back to the whole width; a single tap therefore
   waits ~300 ms for a second one.
3. **Silent canvas**: no sound plays on the canvas; it plays once a board
   opens as the prototype.
4. **All visible boards play**, each **from its start when it comes into
   view** (again from its start after it was out of view entirely), by its
   own settings as the prototype plays it (autoplay, playback mode).
5. **If it stutters** (under 30 fps at the start view on the iPhone): dampen
   without asking, in this order — draw every 2nd frame, a lower pixel
   ratio while playing, pause the clocks while a finger moves; if it is
   still under 30 fps, stop and come back to the user with the numbers
   before Task 9.

Decided here (no product impact, or following from the above):
- **Phones only**, and only with the WASM viewer (`render-wasm/v1` +
  `:available-viewer-wasm`); else the prototype starts as now (no SVG
  canvas: the SVG renderer is deprecated).
- **Kill switch**: a new flag `:phone-canvas`, on by default
  (`:enable-phone-canvas` in `common/src/app/common/flags.cljc` `default`);
  `disable-phone-canvas` in `penpotFlags` turns the canvas start off
  without a revert.
- **The canvas belongs to the Interactions section**; Comments and Inspect
  stay per board: chosen on the canvas, they open the flow's start board
  (else the first) in that section, one push.
- **The canvas is in the URL**: query param `canvas` — absent: the start
  rule decides at load; `"true"`: the canvas; `"false"`: the prototype,
  chosen on this phone (a reload stays on the board, also at a share
  link's board 0). Its view (zoom, position) is not in the URL; it starts
  at the width of the page's content each time.
- **The loop count is ignored**, as the prototype does (nothing sets it).
- **What the canvas draws**: the page as designed
  (`[:viewer :pages page-id :objects]`, not grown by `dvs/scroll-page`,
  not turned by `dvt/turn-objects`), without its top-level boards hidden
  in view mode (pruned from the map before `set-objects`); nested boards
  hidden in view mode stay drawn as part of their parent.
- **Background**: the prototype background (`vbg/background` of the page),
  seen through the transparent canvas (`:background-opacity 0`), as behind
  a board today.
- **Zoom range**: from the width of the content on the screen width
  (`initial`) down to half of that, up to 4: `[min(½ × initial, 4),
  max(4, initial)]`, so the range is never empty.
- **No board names drawn over the canvas** (the WASM UI overlay stays off
  as in the viewer); possible later.

## Affected Modules

- `frontend/` (ClojureScript + SCSS); `common/` only for the flag
  (`common/src/app/common/flags.cljc`). The renderer API is used as it is
  unless the spike (Task 4) names a missing WASM entry point; then it goes
  back to the user before Task 5.
- Commands — prefix every container command with
  `docker exec -u penpot -w /home/penpot/penpot/frontend penpot-devenv-ws0-main bash -lc '…'`
  (never pnpm on the Mac):
  - focused test, ONE namespace per call (the script runs only its last
    argument): `/home/penpot/fe-test-focus.sh frontend-tests.data.viewer-canvas-test`
  - full suite (memory watchdog, log in `/home/penpot/fe-test-focus.log`):
    `/home/penpot/fe-test-focus.sh`
  - before a test build stop Storybook (`tmux send-keys -t penpot:1 C-c`),
    after it start it again (`tmux send-keys -t penpot:1 './scripts/watch storybook' Enter`)
  - lint: `npx clj-kondo --fail-level error --config-dir ../.clj-kondo --lint <files>`;
    format from the repo root (`-w /home/penpot/penpot`): `cljfmt check <files>`;
    styles: `npx stylelint "src/**/*.scss" "resources/styles/**/*.scss"`
  - common (the flag): on the host, `cd common && clojure -M:dev:test --focus <ns>`
  - every test build leaves a debug render-wasm in `resources/public/js`:
    before probes or perf numbers run
    `docker exec -u penpot penpot-devenv-ws0-main bash -lc 'cd ~/penpot/render-wasm && CARGO_HOME=/home/penpot/.cargo BUILD_MODE=release ./build frontend'`,
    then `bash ~/DEV/penpot-demo-tools/checks/touch/release-serve.sh`
    (snapshot at http://sbook.local:14179)
- Probes in `~/DEV/penpot-demo-tools/checks/touch/` (Playwright WebKit
  iPhone 13 mini for layout; Chromium + CDP `Input.dispatchTouchEvent` for
  two fingers, as `sheet-dashboard.mjs`; the real iPhone over WebDriver,
  `iphone-buildup.mjs`, on the release snapshot). Files: the Playground
  (one page) and "Penpot Motion – iPhone prototype" (several pages, for
  the page list).

## Architecture Decisions

- New namespace **`app.main.data.viewer.canvas`** (`dvc`), **pure only**:
  it never requires `app.main.data.viewer` (as `devices` and `turn`, which
  `dv` requires; `preview` and `follow` go the other way). It holds the
  route rules (`canvas-shown?`, `starts-on-canvas?`, the param helpers
  `canvas-params`, `board-params`), the geometry (`content-bounds`,
  `initial-view`, `clamp-view`, `zoom-about`, `resize-view`, `board-at`,
  `zoom-target`, `double-tap-view`), the objects it draws (`canvas-objects`)
  and the clock helpers (`entered`, `board-times`). Params are read with
  `rt/get-query-param` (repeated keys).
- **Events live in `dv`** (`app.main.data.viewer`), which requires `dvc`:
  `show-canvas` (a `defn` with a `replace?` option), the start in
  `bundle-fetched`, and the canvas-aware `go-to-frame-by-index`,
  `go-to-page`, `go-to-section`.
- **The URL is the only state of the mode.** The canvas shows when
  `(dvc/canvas-shown? phone? params)`: phone, section Interactions,
  `canvas "true"`, no `index`. Every way to a board goes through
  `go-to-frame-by-index`, which turns `canvas "true"` into `"false"` in its
  one push; `show-canvas` drops `index` and `frame-id`. `app.main.ui`
  parses `canvas` next to `preview` and `follow` and passes it as a prop.
- **The live view stays in the component**: `{:zoom :vbox}` and the
  clocks live in refs of `canvas-viewport*`, never in `[:viewer-local …]`
  (which would re-render `viewer-content*` at 60 Hz, see `steady-local`).
- New component **`app.main.ui.viewer.canvas`** (`canvas-viewport*`): one
  visible `<canvas>` filling `#viewer-section`; the gestures; the drawing
  (Task 4 decides how); the animation clocks (Task 8). `viewer-content*`
  renders it in the `#viewer-section` `cond` before `(nil? frame)` when the
  canvas shows, with `drawn` nil meanwhile (no autoplay, no sound, no
  `refit-zoom` for a hidden board), and turns `use-touch-rubber!`,
  `use-touch-taps!` and `use-container-scroll!` off.
- `render-viewer-wasm` gets a public seam instead of the canvas reaching
  into its private atoms: `enqueue!` (the render queue), `release-snapshot!`
  (run on the queue: the board view sets its context up again next time),
  `disable-ui-overlay!` (the rulers frame `initialize-viewport` would
  draw), and `show-animation!` public (or a thin wrapper). The canvas runs
  its init, renders and clear on that queue and clears only while the WASM
  context is still its own canvas.
- **Gesture math moves** from `workspace.viewport.touch` (`middle`,
  `pinch-step`, `view-step`) into `app.util.touch`; the workspace calls it
  there. Requiring the workspace namespace from the viewer would pull
  `app.main.data.workspace` and `viewport-ref` into the code every page
  loads, against the memory diet.

## Risks and Considerations

- **iPhone memory** (13 mini, 4 GB): the coffee prototype already peaks
  near 1024 MiB, where iOS kills the tab. The canvas must not add to it: the
  ceiling is the prototype's own peak for the same file in the same run,
  and never above 950 MiB footprint; GPU memory recorded too. If the
  prototype of the Playground alone is above 950 MiB, record it and come
  back to the user. Levers: a lower canvas pixel ratio while it moves or
  zooms out (as `canvas-dpr` for tall boards), a capped tile atlas.
- **Animation load at the start view**: at zoom ≈ 0.083 the 812 CSS px of
  the screen cover ~9,800 design px, ~45 % of the Playground; with the
  margin of half a screen above and below, roughly half to nearly all of
  the 47 timelines are in view at once (not "3–8"). Each costs a
  `view-animation-tree` (which runs `copy-timelines` on every call) and its
  share of the render; at that zoom one tile spans thousands of design px,
  so the touched tiles are most of the screen. Measured in the spike
  (Task 4); fallback ladder in decision 5.
- **One WASM module for all canvases**: canvas → board, a board render
  still queued can run after the canvas set its context up; board →
  canvas, the canvas's unmount clear can destroy the board's new context.
  Hence the queue seam. Tests can't render WASM: covered by probes.
- **Single tap delay** (decision 2): 300 ms; a tap that follows a drag, a
  pinch or a coast never opens a board (Safari's pointerup after a scroll,
  see 52c0c1c6bc).
- **Clock on entry** (decision 4): a board that leaves the view (with its
  margin) entirely and comes back starts again from its start; boards
  partly in view keep running.
- **Share links**: their bundle may hold only some pages; the canvas shows
  the page of the URL. A share dialog opened on the canvas copies the
  route: it drops `canvas` (links stay as today, `index "0"`).
- **Breaking change**: share links opened on a phone start on the canvas
  instead of the first board (the user's choice); computers unchanged.
- Sound: the canvas never starts audio; a board opened from it starts with
  its sound as now (a tap is a user gesture).

## Approach

Three stages, each usable on its own: (1) a static canvas with pan, zoom,
the switch and pages (Tasks 1–7); (2) the animations (Task 8); (3) a tap
opens the prototype at a board (Task 9). Tasks 1–3 are pure and tested
first; the spike (Task 4) then decides the drawing on the real iPhone,
measuring animation frames too, before the component is built on it.

## Task List

### Task 1: The rules of the canvas route and the flag

**Description:** Add the flag `:phone-canvas` (varia set and
`:enable-phone-canvas` in `default`, `common/src/app/common/flags.cljc`).
New namespace `app.main.data.viewer.canvas` with the pure route rules:
`canvas-shown?` (phone, section interactions, `canvas "true"`, no
`index`); `starts-on-canvas?` over `{:phone? :wasm? :flag? :params}`
(phone, WASM viewer, flag, section absent or interactions, no `follow`/
`preview`, `canvas` absent, no `frame-id`, and no `index` or a share
link's `index "0"`); `canvas-params` (assoc `canvas "true"`, dissoc
`index`, `frame-id`, section interactions) and `board-params` (assoc
`index`, `canvas "false"` when it was `"true"`).

**Rationale:** Pure functions carry the whole URL model, so every event
in Task 2 is a thin wrapper and every case is a unit test; `dvc` stays
free of `dv` (no require cycle).

**Code sketch:**
```clojure
(defn canvas-shown? [phone? params] ...)
(defn starts-on-canvas? [{:keys [phone? wasm? flag? params]}] ...)
(defn canvas-params [params] ...)
(defn board-params [params index] ...)
```

**Acceptance criteria:**
- [ ] `starts-on-canvas?` true for: no page/no index (dashboard tap); page
      and no index; `share-id` + `index "0"`. False for: `frame-id`; an
      `index` without `share-id`; `share-id` + `index "3"`; `follow` or
      `preview` `"true"`; `canvas "false"` or `"true"`; section comments;
      not a phone; no WASM; flag off.
- [ ] `canvas-shown?` false with an `index`, on another section, off a
      phone.
- [ ] `canvas-params` / `board-params` round-trip: canvas → board has
      `index` and `canvas "false"`, board → canvas has no `index`/`frame-id`.

**Verification:**
- [ ] `/home/penpot/fe-test-focus.sh frontend-tests.data.viewer-canvas-test`
      green (new test namespace, registered in BOTH lists of
      `frontend/test/frontend_tests/runner.cljs`: the `:require` vector
      and the quoted symbols).
- [ ] cljfmt + clj-kondo clean.

**Dependencies:** None

**Files likely touched:**
- `common/src/app/common/flags.cljc`
- `frontend/src/app/main/data/viewer/canvas.cljs` (new)
- `frontend/test/frontend_tests/data/viewer_canvas_test.cljs` (new)
- `frontend/test/frontend_tests/runner.cljs`

**Estimated scope:** S

### Task 2: Navigation in and out of the canvas

**Description:** In `data/viewer.cljs`:
- `bundle-fetched`, page-less branch: when the start rule holds, its
  replace navigation also carries `canvas-params`.
- `bundle-fetched`, page-present branch: when the start rule holds, one
  `rt/nav … {::rt/replace true}` with `canvas-params` and no board
  navigation (the page is the same: `viewer*` does not re-initialize); when
  the canvas already shows (reload), no board navigation either. The zoom
  event and `update-page-position-data` stay as now (on a phone
  `zoom-to-width` writes nothing into the URL; `refit-zoom` fits the board
  once one opens).
- `go-to-frame-by-index`: builds on `board-params`, so every way to a
  board leaves the canvas in one push.
- `go-to-page`: while the canvas shows, the new page with `canvas "true"`
  and no `index`; else as now.
- `go-to-section` to comments/inspect while the canvas shows: one push with
  the section, `canvas "false"` and the index of the flow's start board
  (else 0) — a helper `start-index` shared with `go-to-frame-auto`.
- `show-canvas` (`defn [& {:keys [replace?]}]`): navigation with
  `canvas-params`.
- `wasm?` and `flag?` for the start rule: `(features/active-feature? state
  "render-wasm/v1")`, `(contains? cf/flags :available-viewer-wasm)`,
  `(contains? cf/flags :phone-canvas)`.

In `share_link.cljs`, the params of a link drop `:canvas`. In `app.main.ui`
the `:viewer` route parses `canvas` (`(= "true" (rt/get-query-param params
:canvas))`) and passes it to `viewer-page*`.

**Rationale:** One rule in `dvc`, applied at the few places that navigate;
no second history step, no refetch; `drawn` stays nil on the canvas, so no
hidden board plays.

**Acceptance criteria:**
- [ ] Dashboard tap on a phone: one replace navigation with `page-id` and
      `canvas=true`, no `index`.
- [ ] Share link (`index "0"`) on a phone: one replace navigation to
      `canvas=true` without `index`; no `go-to-frame*` emitted. On a
      computer: as today.
- [ ] Canvas → board (`go-to-frame`, `go-to-frame-auto`, by index): the URL
      has `index` and `canvas "false"`, one push. Board → canvas
      (`show-canvas`): no `index`, no `frame-id`.
- [ ] A page change on the canvas keeps `canvas "true"` and has no `index`;
      in the prototype it sets `index 0` as now.
- [ ] Comments from the canvas: section comments, `canvas "false"`, the
      flow start's index.
- [ ] A share link copied on the canvas has no `canvas` param.
- [ ] Existing viewer tests pass unchanged (the page-less test from
      be01a0d012 included).

**Verification:**
- [ ] Tests in `viewer_canvas_test.cljs` (events against a test store, as
      `viewer_test.cljs` does): `/home/penpot/fe-test-focus.sh
      frontend-tests.data.viewer-canvas-test`, then
      `/home/penpot/fe-test-focus.sh frontend-tests.data.viewer-test`
      (two calls).
- [ ] cljfmt + clj-kondo clean.

**Dependencies:** Task 1

**Files likely touched:**
- `frontend/src/app/main/data/viewer.cljs`
- `frontend/src/app/main/ui/viewer/share_link.cljs`
- `frontend/src/app/main/ui.cljs`
- `frontend/test/frontend_tests/data/viewer_canvas_test.cljs`

**Estimated scope:** M

### Task 3: The geometry of the canvas and the gesture math

**Description:** Move `middle`, `pinch-step` and `view-step` from
`ui/workspace/viewport/touch.cljs` into `app.util.touch` (the workspace
calls them there, its tests follow). In `dvc`:
- `canvas-objects`: the page objects without top-level boards hidden in
  view mode (and their children); nested hidden boards stay.
- `content-bounds`: rect of the top-level shapes of `canvas-objects`
  (`gsh/shapes->rect`); nil when there are none.
- `initial-view`: zoom = screen width / content width, vbox at the
  content's top-left.
- `clamp-view`: zoom within `[min(½ × initial, 4), max(4, initial)]`; the
  vbox keeps at least a quarter of the screen over the content, and centres
  the content on an axis where it is smaller than the screen.
- `zoom-about`: new zoom keeping a screen point over the same design point.
- `resize-view`: a turn or resize keeps the zoom and the design point at
  the screen centre, then clamps.
- `board-at`: the top-level board under a design point (`gsh/has-point?`,
  rotation-proof; the last of the root's `:shapes` is on top), nil when
  none.
- `zoom-target`: the largest board (any depth) under the point whose width
  is at most half the visible design width; `double-tap-view`: zoomed in
  past the start view → `initial-view`; else the target's width on the
  screen width, its top at the top of the screen (clamped), or 2× about
  the point when there is no target.

**Rationale:** All the math is pure and testable; the component only
feeds touches in and draws what comes out. The size rule makes the double
tap zoom to the card in the Playground and to the screen in an app
prototype.

**Acceptance criteria:**
- [ ] `initial-view` of a Playground-like fixture (4520 wide, 375 screen)
      gives zoom ≈ 0.083 and the left edge at x 0.
- [ ] Content narrower than a quarter of the screen: the zoom range is not
      empty, the content is centred.
- [ ] Empty page and a page of shapes without boards: `content-bounds`
      nil / the shapes' rect; `board-at` nil.
- [ ] `clamp-view` keeps zoom and position in range for extreme inputs;
      `zoom-about` and `resize-view` keep their design point within 1e-6.
- [ ] `board-at` returns the top-level board for a point over a nested
      listed board; the upper one of two overlapping; a rotated board by
      its shape; never a pruned hidden one.
- [ ] `double-tap-view`: a card inside a wide strip → the card; a screen in
      an app layout → the screen, not a row in it; zoomed in → back.
- [ ] Workspace touch tests still pass after the move.

**Verification:**
- [ ] `/home/penpot/fe-test-focus.sh frontend-tests.data.viewer-canvas-test`,
      then the workspace touch test namespace (grep `viewport.touch` under
      `frontend/test`), one call each.
- [ ] cljfmt + clj-kondo clean.

**Dependencies:** Task 1 (same namespace)

**Files likely touched:**
- `frontend/src/app/main/data/viewer/canvas.cljs`
- `frontend/src/app/util/touch.cljs`
- `frontend/src/app/main/ui/workspace/viewport/touch.cljs`
- `frontend/test/frontend_tests/data/viewer_canvas_test.cljs`

**Estimated scope:** M

### Checkpoint: After Tasks 1–3
- [ ] Full frontend suite green (`/home/penpot/fe-test-focus.sh`, Storybook
      stopped); lint clean.

### Task 4: Spike — how the canvas draws (throwaway)

**Description:** On the real iPhone 13 mini (release snapshot; the
Playground in the harness account, opened by a share link added to
`checks/touch/mem/links.json`), compare two ways to draw a free view of
`canvas-objects`:
- (A) the canvas takes the WASM context on its own visible `<canvas>`
  like the workspace (`init-canvas-context`, `initialize-viewport`,
  `set-view-box` per move, `request-render` once it settles,
  `clear-canvas` on leave);
- (B) it keeps the viewer's shared `OffscreenCanvas` (`viewer-snapshot`)
  and draws with `set-viewer-viewport!` + `render-sync`, blitting after
  each render.

Measure for each: memory (WebContent footprint by `xcrun xctrace` with
Activity Monitor, started before the WebDriver session; GPU memory in Mac
WebKit by `checks/touch/mem/regions.mjs` and `gl-contexts.mjs` — the iOS
simulator shows no WebGL memory); ms per pan frame; time to switch canvas
↔ board and back (counting the reload `canvas-objects` causes when it
differs from the board view's page); **and one animation frame at the
start view**: all boards in view with `show-animation!` + (A)
`request-render` / (B) `render-sync-shape-again` on the root `uuid/zero`,
or name the WASM entry point that is missing. Also the Playground's
prototype as the memory baseline. Write the numbers and the choice (A or
B) into this plan's Review Log; nothing of the spike is committed but the
probes in `~/DEV/penpot-demo-tools`.

**Rationale:** Everything after stands on this; (A) has the tile cache
made for panning, (B) avoids loading the shapes again on every switch.
Only the iPhone can tell which costs less, animations included.

**Acceptance criteria:**
- [ ] Numbers for A and B (memory, pan frame, switch, animation frame) and
      the prototype baseline in the Review Log; one approach chosen.
- [ ] If neither stays under the memory ceiling (Risks) or a WASM entry
      point is missing: back to the user before Task 5.

**Verification:**
- [ ] Probe `checks/touch/canvas-spike.mjs` and the `iphone-buildup.mjs`
      measurement on the release snapshot.

**Dependencies:** Tasks 1–3

**Files likely touched:**
- `~/DEV/penpot-demo-tools/checks/touch/canvas-spike.mjs` (new),
  `checks/touch/mem/links.json`
- throwaway changes in `frontend/src/app/main/ui/viewer/canvas.cljs` and
  `render_viewer_wasm.cljs`, reverted after

**Estimated scope:** M

### Task 5: The canvas component

**Description:** Build `canvas-viewport*` with the approach of Task 4 and
mount it in `viewer-content*` (the `#viewer-section` `cond`, before
`(nil? frame)` and after `(empty? frames)`, which keeps the empty state for
a page without boards): it draws `canvas-objects` at `initial-view`, keeps
its view in a ref, follows `resize-view` on a turn or resize, and reports
its first draw (an `on-drawn` callback that sets `drawn*`, so the
two-finger hint shows; and `data-canvas-drawn` on the element for probes).
`render-viewer-wasm` gets the seam (`enqueue!`, `release-snapshot!`,
`disable-ui-overlay!`); the canvas runs init, renders and clear on the
queue and clears only while the context is its own. `use-touch-rubber!`,
`use-touch-taps!` and `use-container-scroll!` get `false` while the canvas
shows. The prototype background shows behind it.

**Rationale:** The seam keeps the one WASM module's context handed over in
order; the hooks off keep work off every touchmove on the iPhone.

**Acceptance criteria:**
- [ ] The Playground opens from the dashboard and by its share link on the
      canvas: all boards at their places, as wide as the screen; the hint
      shows the first time.
- [ ] Canvas → board → canvas by URL (an `index` added, then the back
      gesture) shows each correctly, no blank screen, no stale board, no
      sound and no animation on the canvas.
- [ ] A turn keeps the design point at the centre.
- [ ] On the real iPhone the peak stays under the ceiling (Risks), no tab
      kill in a scripted 2-minute session (pans, zooms, 5 switches);
      numbers in the Review Log.

**Verification:**
- [ ] Probe `checks/touch/canvas-open.mjs` (WebKit iPhone emulation): URL,
      canvas size, `data-canvas-drawn`, screenshot; a page with a scrolling
      board ("Full menu" of the iPhone prototype) drawn as designed.
- [ ] `iphone-buildup.mjs`-style run on the real iPhone (release snapshot).
- [ ] cljfmt, clj-kondo, stylelint clean; full frontend suite green.

**Dependencies:** Task 4

**Files likely touched:**
- `frontend/src/app/main/ui/viewer/canvas.cljs` (new)
- `frontend/src/app/main/ui/viewer/canvas.scss` (new; touch
  `resources/styles/main-default.scss` in the container for the watch)
- `frontend/src/app/main/ui/viewer.cljs`
- `frontend/src/app/main/render_viewer_wasm.cljs`

**Estimated scope:** M

### Checkpoint: After Tasks 4–5
- [ ] Full frontend suite green; lint clean; release snapshot rebuilt.
- [ ] Playground and "Penpot Motion – iPhone prototype" open on the canvas
      on the real iPhone; memory numbers recorded.
- [ ] User looks at it on the phone before gestures are built on it.

### Task 6: Gestures on the canvas

**Description:** In `canvas-viewport*` (pointer events, `touch-action:
none` on the canvas): one finger pans with coast and a rubber band at the
clamp (per axis with `drag-scroll/speed`, `coast`, `rubber`); two fingers
pan and zoom about their middle (`touch/pinch-step`, `touch/view-step`,
`dvc/zoom-about`), one rAF flush per frame into the view ref; a double tap
(two taps < 300 ms, < 24 px apart) runs `double-tap-view` animated over
~250 ms. Two fingers held still still open the sheet (existing hook). A
wheel/trackpad zooms with ctrl and pans otherwise (desktop Chrome device
mode).

**Rationale:** The same feel as the rest of the phone view mode and the
workspace's pinch math, so nothing new to tune.

**Acceptance criteria:**
- [ ] Drag, coast, rubber band, pinch about the fingers' middle, double
      tap in and out, all within the clamp.
- [ ] A pinch never opens the sheet; two fingers held still do.
- [ ] No page scroll or Safari zoom while gesturing; no store write per
      frame (the view lives in the ref).

**Verification:**
- [ ] Unit tests for any new pure helper (e.g. the 2-D coast step) in
      `viewer_canvas_test.cljs`.
- [ ] Probe `checks/touch/canvas-gestures.mjs` (Chromium + CDP touches):
      view after drag, pinch, double tap; sheet after a still hold.
- [ ] Real iPhone by hand; lint clean.

**Dependencies:** Task 5

**Files likely touched:**
- `frontend/src/app/main/ui/viewer/canvas.cljs`
- `frontend/src/app/main/data/viewer/canvas.cljs`
- `frontend/test/frontend_tests/data/viewer_canvas_test.cljs`

**Estimated scope:** M

### Task 7: The sheet on the canvas: switch, pages, sections

**Description:** `viewer-content*` passes `canvas?` to the phone sheet. On
the canvas the sheet shows a row "Canvas | Prototype" (segmented, the
current one pressed) instead of the board row and the animation controls,
and hides the interactions-mode list; "Prototype" runs
`dv/go-to-frame-auto` (flow start, else the first board), a flow of the
flow list starts the prototype at its board (`go-to-frame`, which leaves
the canvas by Task 2); Comments and Inspect leave the canvas by Task 2's
`go-to-section`. In the prototype the same row brings the canvas back
(`dv/show-canvas`, a push). A list of the pages when the file has more
than one, reusing the page list of `ui/viewer/header.cljs:252-279`
(`[:data :pages]`, `dv/go-to-page`, which keeps the mode). New strings only
in `en.po` (Weblate does the rest).

**Rationale:** The sheet is the phone's only menu; the switch must work
both ways besides the back gesture, and a canvas of one page is not the
whole file.

**Acceptance criteria:**
- [ ] Canvas → Prototype starts at the flow start (or the first board);
      Prototype → Canvas returns to the canvas at its start view.
- [ ] Pages listed only for files with several pages; the current one
      marked; choosing one keeps the mode.
- [ ] On the canvas no interactions-mode list; Comments opens the flow
      start in Comments.

**Verification:**
- [ ] Probe extending `sheet-dashboard.mjs` (two-finger hold, switch, page
      change on "Penpot Motion – iPhone prototype").
- [ ] cljfmt, clj-kondo, stylelint clean.

**Dependencies:** Tasks 2, 5

**Files likely touched:**
- `frontend/src/app/main/ui/viewer/phone_sheet.cljs`
- `frontend/src/app/main/ui/viewer/phone_sheet.scss`
- `frontend/src/app/main/ui/viewer.cljs`
- `frontend/translations/en.po`

**Estimated scope:** M

### Checkpoint: After Tasks 6–7 (stage 1 done)
- [ ] Full frontend suite green; lint clean; release snapshot rebuilt.
- [ ] User tries the static canvas on the iPhone; commit stage 1.

### Task 8: Animations on the canvas

**Description:** Per page (memo on page and files) the clocks
`{board-id (dv/board-clock page files board-id)}` of the top-level boards
of `canvas-objects` that play (`cta/autoplay?`). Each rAF, in the
component: the boards in view (vbox + a margin of half a screen) get an
entry stamp when they come in after being out entirely (`dvc/entered`);
their time is `(cta/playback-time clock (- now entry))`; a `:once` board
that `cltl/view-playback-ended?` keeps its last tree and is not worked out
again; the trees of `cltl/view-animation-tree` merge with plain `merge`
(the layers of top-level boards are disjoint) and show by the seam's
`show-animation!`; the render follows Task 4's choice. Autoplay off: no
tree (the board as designed). No audio. Clocks and stamps stay in refs.
If the iPhone measures under 30 fps at the start view, apply decision 5's
ladder in order and record each step's numbers; still under 30 fps → stop
and go back to the user.

**Rationale:** Decision 4, with the helpers the prototype plays by, so the
canvas plays a board as the prototype does; boards off screen cost
nothing.

**Code sketch:**
```clojure
(defn entered
  "The entry stamps of the boards `in-view` at `now`: kept for those still
  in view, `now` for those that came in, none for those out."
  [stamps in-view now] ...)
(defn board-times [clocks stamps now] ...)   ; {board-id time}
```

**Acceptance criteria:**
- [ ] `entered`: kept while in view, reset after leaving entirely, set on
      entry (tests).
- [ ] `board-times` uses `cta/playback-time` per board's clock (once,
      loop, ping-pong, a copy's clock); no entry for autoplay off (tests).
- [ ] Only boards in view are worked out; an ended `:once` board is not
      recomputed (tests on the inputs).
- [ ] On the iPhone the Playground at the start view plays at ≥ 30 fps
      (frame stamps of the probe; numbers and any ladder steps in the
      Review Log); no sound.

**Verification:**
- [ ] Tests in `viewer_canvas_test.cljs`; focused run green.
- [ ] Probe `canvas-play.mjs` (frame times) on the release snapshot; real
      iPhone by hand.

**Dependencies:** Task 5 (Task 6 for the pause while a finger moves)

**Files likely touched:**
- `frontend/src/app/main/data/viewer/canvas.cljs`
- `frontend/src/app/main/ui/viewer/canvas.cljs`
- `frontend/src/app/main/render_viewer_wasm.cljs`
- `frontend/test/frontend_tests/data/viewer_canvas_test.cljs`

**Estimated scope:** M

### Task 9: A tap opens the board as the prototype

**Description:** A single tap (no second tap within 300 ms, no drag or
pinch before it, not the end of a coast) on a board (`dvc/board-at`, the
top-level board) runs `dv/go-to-frame` with its id: one push with its
`index` and `canvas "false"` (Task 2), the board as wide as the screen
with its sound and animation as the prototype starts them. The back
gesture returns to the canvas (its start view). A tap on empty canvas does
nothing.

**Rationale:** Decision 2; a push keeps the canvas one back gesture away,
the bundle stays (page and file do not change); `go-to-frame` maps the id
to the `index` `viewer-content*` reads.

**Acceptance criteria:**
- [ ] Tap on a board → prototype at that board (also on a card in a
      strip: the strip); back → canvas.
- [ ] Tap after a drag, a pinch or a coast never opens a board; a double
      tap only zooms.
- [ ] The sound of the opened board plays (the tap is the user gesture).
- [ ] A reload on the opened board of a share link stays on it.

**Verification:**
- [ ] Tests for the tap classification helper (pure: times, distances,
      drag flag).
- [ ] Probe `canvas-tap.mjs` (Chromium + CDP): URL after tap, back and
      reload.
- [ ] Full frontend suite green; lint clean; release snapshot; real iPhone
      by hand.

**Dependencies:** Tasks 6, 7

**Files likely touched:**
- `frontend/src/app/main/ui/viewer/canvas.cljs`
- `frontend/src/app/main/data/viewer/canvas.cljs`
- `frontend/test/frontend_tests/data/viewer_canvas_test.cljs`

**Estimated scope:** S

### Checkpoint: After Tasks 8–9 (stages 2 and 3 done)
- [ ] Full frontend suite green; lint clean; release snapshot rebuilt.
- [ ] User tries the Playground on the iPhone: canvas, animations, tap
      into a board and back.

## Verification and Testing

- Pure logic (route rules, navigation events, geometry, clocks, tap
  classification) under
  `frontend/test/frontend_tests/data/viewer_canvas_test.cljs`, written
  before the code of each task; one namespace per `fe-test-focus.sh` call.
- WASM drawing and gestures by probes (WebKit iPhone emulation for layout,
  Chromium + CDP for real multi-touch) and on the real iPhone 13 mini via
  the release snapshot (the dev build crashes iOS Safari), always after
  `BUILD_MODE=release ./build frontend` in render-wasm (a test build leaves
  a debug WASM behind).
- After each stage: full frontend suite (Storybook stopped, started again
  after), stylelint over all styles, cljfmt, clj-kondo.

## Parallelization

- Tasks 1 → 2 sequential; Task 3 can run beside Task 2 (different
  functions of the same new namespace, plus the move). Task 4 needs 1–3;
  Task 5 needs 4. Tasks 6 and 7 can run in parallel after Task 5. Task 8
  needs 5 (and 6 for the gesture pause); Task 9 needs 6 and 7. One person
  works it in order 1 → 9.

## Out of Scope

- The canvas on tablets and computers (the desktop view mode stays one
  board at a time).
- Comments and Inspect on the canvas itself.
- Board names or a minimap over the canvas.
- Remembering the canvas view (zoom, position) between visits.
- Honouring a timeline's loop count (the prototype ignores it too).

## Open Questions

None for the user now. Task 4's spike decides how the canvas draws (A or
B); its numbers go into the Review Log before Task 5. Two points go back to
the user if they come up: the memory ceiling or a missing WASM entry point
(Task 4), and under 30 fps after the fallback ladder (Task 8).
