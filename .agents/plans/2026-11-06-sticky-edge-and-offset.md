# Plan: Sticky layers at any edge, with an offset

Status: done
Review Log:
- 2026-11-06T06:46:49Z — done

## Context

Follows `2026-11-05-sticky-layers.md` (done, not pushed: origin is before it),
whose out-of-scope list named an offset (`top: 24px`) and the bottom and right
edges. User request (2026-11-06, "mach mal 3"): a sticky layer stops at the
top, bottom, left or right edge of what scrolls it, a number of px from it.
Uses: a CTA at the end of a form or a sum row that sticks at the bottom of the
screen until its place comes, headers that stick under a fixed nav bar.

Decisions (Claude, the user asked for edge top/bottom/left/right + px):
- One edge per layer, as one CSS inset: `:sticky-edge` `:top` | `:bottom` |
  `:left` | `:right`, and `:sticky-offset` (px of the board, default 0).
- Without an edge a layer sticks at the top, or at the left in a board that
  scrolls only sideways. In a board that scrolls both ways it now sticks at the
  top only (before: top and left), as CSS `top: 0` does. Not pushed, so no file
  outside the devenv has the old behaviour.
- An edge along an axis the area does not scroll (left in a list, bottom on a
  carousel) does nothing: the layer scrolls as any other.
- The bottom and right edges mirror the top and left ones: the layer stops with
  its end at the line, its parent board's start holds it (it leaves with its
  board), and the previous layer of its board sticky at the same edge pushes it
  on. It sticks at rest too: that is the CTA below the first screen.
- The top and left edges: a layer sticks only when its design place is at or
  past its line (the area's edge plus the offset); one above it scrolls as any
  other (no jump at rest). Only layers sticky at the same edge push each other.
- Screens: the bottom edge is the bottom of the section, the screen of the
  device (or the window without one), as CSS `bottom` does natively.
- Edge and offset stay on the layer when its scroll behaviour changes (as
  constraints do); they only count while it is sticky.
- Design tab, Prototype tab, Scrolling: the select's sticky option reads
  "Sticky (stop at an edge)"; while sticky, a row "Edge" with four icon radios
  (top, bottom, left, right; the default edge shown selected) and a row "Offset"
  with a number field.

Out of scope: an edge per axis (a corner cell sticking top and left), negative
insets in the UI (the API and schema take any number), the deprecated SVG
renderer.

## Affected Modules

- `common`: schema keys, attrs lists, allowed attrs, component sync group, the
  pure `cfh/sticky-side`.
- `frontend`: `data/viewer/scroll.cljs` (rule, containers, sheets, rest shifts),
  `ui/viewer/viewport_common.cljs`, `viewport_wasm.cljs` (screen boxes),
  `ui/viewer/shapes.cljs` (overlays from a stuck layer), the Prototype tab menu,
  SVG export, CSS code gen, plugin API, `en.po`.
- `plugins/libs/plugin-types/index.d.ts`, `mcp/packages/server/data/api_types.yml`,
  the user guide.
- Backend restart for the schema keys (user consent); MCP restart for the docs
  (consent).

## Approach

### Task 1: Model
`:sticky-edge` (`[::sm/one-of #{:top :bottom :left :right}]`) and
`:sticky-offset` (`::sm/safe-number`) beside `:sticky-scroll` in the schema,
every attrs list, `allowed-shape-attrs`, `:constraints-group`, SVG export.
`cfh/sticky-side [shape axes]`: the edge it sticks at in an area scrolling along
`axes`, nil when none. Tests in `common_tests/files/helpers_test.cljc`.

### Task 2: The rule, containers and sheets
`dvs/sticky-edge` takes the side: start (`line` = area edge + offset, sticks only
when its edge is at or past the line, held by `bound` from the end) or end
(`line` = area end − offset, held by `bound` from the start). `sticky-end`
becomes `sticky-bound` (same-edge siblings only; the end side looks back).
`sticky-move` works by side, and moves an end-side layer at offset 0 too;
`scroll-shifts` (with `:overlay-root`) adds the areas that hold an end-side
sticky layer at offset 0 (a scan of `objects` cached by identity). Tests in
`data/viewer_scroll_test.cljs`.

### Task 3: Screens
`vpc/screen-sticky-layer?` takes the top (at or past root top + offset) and the
bottom edge; `sticky-span` gives `:side` and `:inset` and, for the bottom, the
wrapper from the parent's start (or the previous bottom-sticky sibling's end) to
the layer's end; `wasm-sticky-box*` sets `top` or `bottom`. `screen-stuck-move`
takes the visible height and moves both ways; `stuck-place-now` reads the
section's client height. Tests in `viewer_sticky_boxes_test.cljs` and
`viewer_overlay_test.cljs`; a browser check
`scroll-overflow/sticky-edge-check.mjs`.

### Task 4: Prototype tab
The edge radios and the offset field, `dwi/set-sticky-edge` and
`dwi/set-sticky-offset`, mixed values for a mixed selection. Strings in `en.po`
only. Test in `data/workspace_interactions_test.cljs`.

### Task 5: Code, API, docs
CSS: `top`/`bottom`/`left`/`right` of the offset for the side, a comment when
the edge does nothing. Plugin API `stickyEdge` and `stickyOffset`, `index.d.ts`,
`api_types.yml`, the user guide. Tests in `code_gen_style_test.cljs`,
`plugins/context_shapes_test.cljs`.

## Verification and Testing

- `docker exec -u penpot penpot-devenv-ws0-main bash -lc /home/penpot/fe-test-focus.sh`
  (whole frontend suite), common tests, cljfmt and clj-kondo in the container.
- Browser checks against a harness file "Sticky edge check", WASM, iPhone and
  no device.
- Full run (`/home/penpot/full-tests.sh`) before the last commit.

## Open Questions

None.
