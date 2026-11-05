# Plan: Sticky layers in the prototype

Status: done
Review Log:
- 2026-11-05T13:20:00Z — review findings 1–20 applied: two existing bugs first (sheet hotspots ignore the sheet's scroll; a scroll-only render skips the fixed pass holding a scrolled container), CSS semantics with the parent board as containing block and the innermost scrolling ancestor as the area (sticky in a section board works), one shared pure range rule (next sticky sibling past one's end, no sticking for layers above the area edge, only along the area's axes), no screen boxes in overlays, box height and prepared coordinates for the push, hit testing and backdrops in sticky boxes, passes as tagged pass specs (copy-canvases by tag), overlays opened from a stuck layer, design tab attrs/memo/mixed value, CSS for layout and absolute children, allowed-shape-attrs and SVG export, root strokes cleared in masked passes, rubber band and stacking stated, Task 5 split, memory and render cost measured, test files named.
- 2026-11-05T15:03:08Z — done

## Context

Stages 1 and 2 of prototype scrolling (branch `girafic-penpot-timeline-animation`,
up to `31192a9deb`) let a screen scroll through its content (`:scroll-overflow
:vertical` on a top board, the browser scrolls `#viewer-section` natively),
inner boards scroll as containers (`:horizontal`/`:vertical`/`:both`, moved by
WASM scroll entries after the layout, `dvs/scroll-shifts`), and a sheet board
scrolls as the root of an overlay. A layer can be fixed when scrolling
(`:fixed-scroll`, the design-tab checkbox "Fix when scrolling", offered to
direct children of a board, `first-level?`): on a screen it draws on its own
canvas in a CSS sticky box at the top left of the section; in a container or a
sheet it stays with it (`scroll-shifts` skips it).

This plan adds a third scroll behaviour, **Sticky**: the layer scrolls with what
scrolls until it reaches an edge of the area that scrolls, then stays there —
section headers of a list, the label of a carousel row.

User decisions (2026-11-05):
- It behaves like CSS `position: sticky; top: 0`: the layer stops at the top edge
  of the area that scrolls (the screen, a container, a sheet). Layers fixed when
  scrolling are not taken into account; they draw over it.
- Several sticky layers in a row: the next one pushes the previous one out (as
  section headers do on iOS).
- In a container that scrolls sideways it stops at the left edge, in a Both one at
  both. A screen scrolls vertically only: top edge.
- The design tab shows a select of three options, as in the user's screenshot:
  "Scroll with parent", "Fixed (stay in place)", "Sticky (stop at top edge)" (no
  other tool is named anywhere).
- Following "like CSS" (taken by Claude after the review, finding 3): the parent
  board of a sticky layer is its containing block (it leaves with its parent's
  end), and the area it sticks in is its innermost scrolling ancestor. A list made
  of section boards, each with a sticky header, works.

Out of scope: an offset for where it sticks (`top: 24px`), sticking at the
bottom or right edge, sticky layers in groups (the setting stays where the fixed
one is: direct children of a board), the deprecated SVG renderer (a sticky layer
scrolls as any other there).

## Affected Modules

- `common`: the attribute, its helper, allowed attrs, component sync group.
- `frontend`: design tab constraints menu; viewer scroll geometry
  (`data/viewer/scroll.cljs`), WASM viewport and passes (`viewport_wasm.cljs`,
  `viewport_common.cljs`, `render_viewer_wasm.cljs`), container input
  (`container_scroll.cljs`), overlays (`viewer/shapes.cljs`), SVG export, plugin
  API, CSS code generation, `en.po`.
- `plugins/libs/plugin-types`, `plugin-api-test-suite`, `mcp/.../api_types.yml`.
- No Rust change. The backend restarts for the schema key (user consent).

## Architecture Decisions

1. **Model.** `:sticky-scroll` (optional boolean) beside `:fixed-scroll`: schema,
   every attrs list with `:fixed-scroll`, `allowed-shape-attrs`
   (shape.cljc:619-626, kept on variant switch), `:constraints-group`, SVG export
   (`export.cljs:101`). `cfh/sticky-scroll?` mirrors `cfh/fixed-scroll?`
   (helpers.cljc:863-867: parent = frame, frame ≠ zero) and is false when
   `:fixed-scroll` is true. Fixed wins when both are set.
2. **One pure range rule** (`dvs/sticky-span`) for containers and screens, per
   axis of the area:
   - `start` — the area's edge (container box, or design box of a grown top
     board / sheet root, `dvs/design-box`); on screens the section's top.
   - The layer sticks only when its design edge is at or past `start`.
   - `end` — the earliest of: its parent board's end (when the parent is not the
     area), and the next sticky sibling's design start, where "next" is the
     nearest sticky sibling (same parent) whose start along the axis is at or
     past this layer's end (`edge + size`); ties by z-order.
   - Shown edge at offset `o`: `min(max(edge − o, start), end − o − size)`.
   - Only along the axes the area scrolls.
3. **Containers and sheets: `dvs/scroll-shifts`.** A sticky layer whose innermost
   scrolling ancestor is a container (or the sheet root of an overlay) gets the
   shift `shown − edge` for itself and all within it, in place of what its
   containers give it. `scroll-shifts` learns the axes: `view-offsets` hands back
   `{id {:offset [x y] :axes #{…}}}` (or `scroll-shifts` takes `objects` and
   `:overlay-root`). Rubber band: the rule uses the clamped offset, `past` is
   added as a plain shift, so a stuck header moves with the content past an end,
   as on screens.
4. **Screens: native CSS sticky boxes**, only when the viewport is not an overlay
   (finding 1). A screen sticky layer (`vpc/screen-sticky-layer?`: sticky, not in
   a container, not in a screen-fixed layer) leaves the scrolling pass and gets a
   pass of its own (mask: its subtree only, no parents; fixed layers inside it go
   to the fixed pass) on a board-sized canvas in:
   - a wrapper `position: absolute` over its parent's span, cut at `end`,
     `pointer-events: none`;
   - a spacer of its edge, then a box `position: sticky; top: 0`, as high as the
     layer × scale, the canvas hung from it by `-edge`, its hotspots
     (`pointer-events: all` on them) and its backdrops (`wasm-backdrops*`) in it.
   Coordinates are the prepared ones (canvas origin, margins, `delta`). Boxes in
   the layers' z-order, between the scrolling canvas and the fixed box.
   Containers inside a sticky box take the wheel and drags through it
   (`data-sticky-layer=<id>`, in `containers-under` after the fixed ones).
5. **Passes.** The viewport gets a vector of pass specs `{:id :canvas-ref :include
   :clear :pins :tag}` instead of more positional arguments; canvases carry
   `data-pass` (`"scroll"`, `"fixed"`, `"sticky:<id>"`) and `copy-canvases!`
   pairs by tag. A pass is skipped on a scroll-only render only when no scrolled
   container lies in its mask (this also fixes the fixed pass today). Masked
   passes clear and restore the root's fills, shadows and strokes.
6. **Overlays from a stuck layer.** At open time the placing layer's stuck shift
   comes from the section's `scrollTop` (screens) or the shifts (containers),
   the same rule; a stuck screen layer anchors `:layer` (stays on the screen),
   an unstuck one `:page`.
7. **Plugin API** `stickyWhenScrolling: boolean` beside `fixedWhenScrolling`;
   setting one true clears the other. **CSS:** a layout child gets `position:
   sticky; top: 0` (`left: 0` in a horizontal container, both for Both) in place of
   its `:position`/`:top`/`:left`; an absolutely placed child keeps `position:
   absolute` and gets a comment that it sticks in the prototype.
8. **Stacking** (stated, finding 18): on a screen a sticky layer draws over the
   layers that scroll and under the fixed ones; in a container and a sheet the
   layers keep their design order.

## Risks and Considerations

- Cost: every screen sticky layer is a masked full pass when the board renders in
  full (load, animation frames, a resize) and a board-sized canvas: a 402×3000
  board at DPR 3 is ~43 MB per canvas. A board with a sticky layer also masks its
  scrolling pass, which then renders without the incremental tile path. Measure
  with `~/DEV/penpot-demo-tools/scroll-demo/probe/perf-probe.mjs`; if needed, an
  extrect-sized canvas or a cap on sticky canvases in a later step.
- Hotspot svgs pose all objects every 100 ms each: share the posed objects
  between the svgs of one viewport.
- CSS sticky needs no clipping ancestor between the box and `#viewer-section`:
  `.viewer-clipper` is `overflow: clip` and `.viewport-container` has a
  `clip-path`, neither breaks it (the fixed box relies on the same).
- Turned devices and counterpart boards: spans come from the shown objects.

## Approach

Fix the two existing bugs (Tasks 0a, 0b). Model and design tab (1, 2). The pure
rule (3) and containers/sheets (4). Screens in three steps (5a passes and boxes,
5b input/hotspots/backdrops/overlays, 5c transitions and cost). API, code, docs
(6). One commit per task.

## Task List

### Task 0a: Hotspots of a sheet follow its scroll
**Description:** `wasm-hotspots-svg*` computes the shifts with
`(dvs/view-offsets objects scroll)` (viewport_wasm.cljs:85) and the clip
containers with `container-ids` without the overlay root, so the hotspots of a
scrolled sheet stay at their design places. Pass the sheet root (as the render
does, `is-overlay` + `dvs/screen-h`) into the svg, to `view-offsets
:overlay-root` and `container-ids :overlay-root?`.
**Acceptance criteria:**
- [ ] A button in a sheet scrolled by 300 px clicks where it shows; a hotspot
      scrolled out of the sheet's box takes no click.
**Verification:** a browser check (`scroll-overflow/sheet-hotspots-check.mjs`,
headless on the GPU), frontend suite.
**Dependencies:** None. **Files:** `viewport_wasm.cljs`. **Scope:** XS

### Task 0b: A scroll-only render keeps the passes it moves
**Description:** since `66b01e3cda` a scroll-only render skips the fixed pass;
a container inside a fixed layer then does not redraw while it scrolls. Skip a
pass only when none of the containers of the board's scroll lies in its mask.
**Acceptance criteria:**
- [ ] A horizontal container in a fixed tab bar redraws as it scrolls; a board
      whose fixed layers hold no container keeps skipping.
**Verification:** browser check `fixed-container-check.mjs`; a unit test of the
rule if it is a pure helper.
**Dependencies:** None. **Files:** `render_viewer_wasm.cljs`. **Scope:** XS

### Task 1: The sticky attribute
**Description:** Architecture Decision 1; `variants.cljs:708` (combine as
variants) clears `:sticky-scroll` like `:fixed-scroll`.
**Acceptance criteria:**
- [ ] Validates; `cfh/sticky-scroll?` true only for a direct child of a board,
      false when fixed too.
- [ ] Kept on variant switch for an overridden copy; component sync follows the
      main.
- [ ] Exported in SVG attrs.
**Verification:** common tests (JVM and JS), a frontend test for the variant
switch.
**Dependencies:** None. **Files:** `shape.cljc`, `shape/attrs.cljc`,
`component.cljc`, `files/helpers.cljc`, `variants.cljs`, `shapes/export.cljs`,
`common/test/.../types/shape_*_test.cljc` or the helpers test.
**Scope:** S

### Task 2: The scroll behaviour select in the design tab
**Description:** in `menus/constraints.cljs` the checkbox becomes a select of the
three options; `:sticky-scroll` joins `constraint-attrs` (constraints.cljs:25-29)
and the memo keys (31-54); "Scroll with parent" dissocs both keys (or nil is
normalised to false, as `multiple.cljs:364-366` does for `:hidden`); a mixed
selection shows the `settings.multiple` option (constraints.cljs:162-165).
Strings in `en.po` only.
**Acceptance criteria:**
- [ ] The select shows and sets the value, re-renders on undo, shows mixed for a
      mixed selection and nothing mixed for untouched layers.
**Verification:** `scroll-overflow/sticky-ui-check.mjs`; stylelint if scss.
**Dependencies:** Task 1. **Files:** `menus/constraints.cljs`,
`menus/constraints.scss`, `multiple.cljs` (if normalising), `en.po`.
**Scope:** S

### Checkpoint: After Tasks 0a–2
- [ ] Suites pass; backend restarted for the schema key (consent).

### Task 3: The sticky range rule
**Description:** `dvs/sticky-span` and `dvs/sticky-edge` (Architecture Decision
2) with the area, parent and sibling lookup (`dvs/sticky-area`: innermost
scrolling ancestor and its axes; screens as area when none).
**Code sketch:**
```clojure
(defn sticky-edge
  "Where a sticky layer at `edge`, `size` long, shows at `offset`, in an area
  from `start`, until `end` (nil: none)."
  [edge size offset start end]
  (let [shown (max (- edge offset) start)]
    (cond-> shown (some? end) (min (- end offset size)))))
```
**Acceptance criteria (tests in `viewer_scroll_test.cljs`):**
- [ ] Scrolls, sticks at the start, is pushed by the next sticky sibling (the one
      past its end, ties by z-order), leaves with its parent's end.
- [ ] Does not stick when its design edge is above the start.
- [ ] Only along the area's axes; side by side layers do not push each other.
**Verification:** `fe-test-focus.sh frontend-tests.data.viewer-scroll-test`.
**Dependencies:** Task 1. **Files:** `data/viewer/scroll.cljs`, the test.
**Scope:** S

### Task 4: Sticky layers in containers and sheets
**Description:** Architecture Decision 3: `scroll-shifts` with axes, sticky
shifts for layers whose area is a container or the sheet root, rubber band as a
plain shift. Hotspots and overlay positions use the shifted objects already.
**Acceptance criteria:** in the harness file "Scroll overflow check", page
"Sticky" (a list container with three sticky headers, a container of section
boards each with a sticky header, a horizontal container with a sticky label, a
sheet with a sticky title and a button below):
- [ ] the headers stick and push, the section headers leave with their section,
      the label sticks at the left, the title in the sheet;
- [ ] their hotspots follow and click.
**Verification:** `scroll-overflow/sticky-container-check.mjs` (headless, GPU),
reading hotspot rects at set offsets; frontend suite.
**Dependencies:** Tasks 0a, 3. **Files:** `data/viewer/scroll.cljs`,
`viewport_wasm.cljs`, `render_viewer_wasm.cljs` (scroll-of), tests.
**Scope:** M

### Task 5a: Screen sticky layers: passes and boxes
**Description:** Architecture Decisions 4 and 5 without input: pass specs,
`data-pass` tags, `vpc/screen-sticky-layer?` and the masks, the wrappers, spacers
and boxes in prepared coordinates and z-order, root strokes cleared in masked
passes, `data-scroll-layer="true"` on the boxes (rubber band moves them).
**Acceptance criteria:**
- [ ] On a screen with Overflow Vertical, in iPhone and without a device, a
      sticky header scrolls, sticks at the top, is pushed by the next, leaves with
      its section board; the fixed status bar draws over it.
- [ ] A board without sticky layers renders with the passes it had.
**Verification:** `scroll-overflow/sticky-screen-check.mjs` (positions at several
`scrollTop`s, screenshots); a pure test of the span helper; frontend suite.
**Dependencies:** Tasks 0b, 1, 3. **Files:** `viewport_wasm.cljs`,
`viewport_common.cljs`, `render_viewer_wasm.cljs`, `viewer.scss` or
`interactions.scss`, tests. **Scope:** M

### Checkpoint: After Task 5a
- [ ] Suites pass; the scroll demo records as before.

### Task 5b: Screen sticky layers: hotspots, input, backdrops, overlays
**Description:** hotspots in the boxes (posed objects shared), containers inside a
sticky box take the wheel and drags (`data-sticky-layer`, `containers-under`),
backdrops in the boxes, overlays from a stuck layer (Architecture Decision 6).
**Acceptance criteria:**
- [ ] A stuck header clicks; a horizontal chip row in a sticky header scrolls
      with the wheel and a drag; a frosted sticky header blurs what scrolls
      under it; a dropdown from a stuck header opens under it.
**Verification:** `sticky-screen-check.mjs` extended; `viewer_overlay_test.cljs`
for the anchor; frontend suite.
**Dependencies:** Task 5a. **Files:** `viewport_wasm.cljs`,
`container_scroll.cljs`, `viewer/shapes.cljs`, `viewer.cljs`, tests.
**Scope:** M

### Task 5c: Transitions and cost
**Description:** `copy-canvases!` by tag; a board change with an animation between
boards with different numbers of sticky layers; perf and memory with 0 and 3
sticky layers.
**Acceptance criteria:**
- [ ] The leaving board keeps its picture, the coming one draws in place.
- [ ] Numbers recorded in the memory notes.
**Verification:** a transition check; `perf-probe.mjs`.
**Dependencies:** Task 5a. **Files:** `render_viewer_wasm.cljs`, `viewer.cljs`.
**Scope:** S

### Task 6: Plugin API, code and docs
**Description:** Architecture Decision 7; docs in `plugin-types/index.d.ts`,
`api_types.yml` regenerated (recipe in the memory notes), MCP restart with
consent.
**Acceptance criteria:**
- [ ] The API reads/sets it, the two exclude each other.
- [ ] Inspect: `position: sticky; top: 0;` for a layout child, the absolute case
      as decided.
**Verification:** `frontend_tests/plugins/interactions_test.cljs`,
`plugin-api-test-suite/src/tests/shapes-geometry.test.ts`,
`code_gen_style_test.cljs`.
**Dependencies:** Task 1. **Files:** `plugins/shape.cljs`,
`code_gen/style_css_values.cljs`, `index.d.ts`, `api_types.yml`, tests.
**Scope:** S

### Checkpoint: After Task 6
- [ ] All suites (`/home/penpot/full-tests.sh`) and the checks of the tasks.

## Verification and Testing

- Unit: common (attribute, helper), `viewer_scroll_test` (rule, shifts),
  `viewport_common` spans, `viewer_overlay_test` (anchor), plugins, code gen.
  A new test namespace is registered in `frontend/test/frontend_tests/runner.cljs`.
- Browser checks in `~/DEV/penpot-demo-tools/scroll-overflow/` against "Scroll
  overflow check", page "Sticky", one at a time, `HEADLESS=1` when the screen is
  locked.
- Full runs: `docker exec -u penpot penpot-devenv-ws0-main bash -lc
  /home/penpot/full-tests.sh`.

## Parallelization

0a and 0b first (independent). Then 1; 2, 3 and 6 after 1; 4 after 0a and 3; 5a
after 0b, 1, 3; 5b and 5c after 5a. Sequential in practice: one commit per task.

## Open Questions

None.
