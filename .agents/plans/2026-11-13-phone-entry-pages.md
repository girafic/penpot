# Plan: Phone layout only where the page is a phone's, and the error pages of a phone

Status: done
Review Log:
- 2026-11-14T07:28:26Z — done

## Context

Source: review-code over `5b2e7a7a3b^..d2ebbca6bf` (branch `girafic-penpot-timeline-animation`, not pushed) plus the user's item "the remaining error pages of `static.cljs` on a phone".
Created: 2026-11-13

The commits under review gave the pages before the dashboard a phone layout: login, sign-up and recovery (`75cf3b7fde`), the login shown over a view mode link opened without a session (`de4c1b3529`, `d2ebbca6bf`), and two onboarding steps (`0cb76aebc3`, `cc2fe088ef`). The review asked for changes:

- **R1 (High)**: `login-modal*` (`frontend/src/app/main/ui/static.cljs`) turns its phone layout on from `touch/phone?` alone. A phone keeps some routes at desktop width, the workspace above all (no viewport tag: a ~980 px page). A logged-out workspace link on a phone now shows a full-width form at a scale of ~0.38: 16 px text shows at ~6 px, iOS no longer zooms in on focus, and the close button is gone.
- **M1 (Medium)**: the `"auth"` prefix of `touch/phone-page?` also gives `:auth-verify-token` the device-width tag. That route renders `verify-token*` → `static/invalid-token` → `error-container*`, which has no phone layout. On a phone, "Go to Personal Projects" in its header runs through the decorative pencil at the top. The same happens on the not-found page of dashboard and view mode routes.
- **M2 (Medium)**: the new phone layouts (`auth.scss`, `static.scss`, `onboarding/team_choice.scss`, and `onboarding/questions.scss`, which is older) end their inner scroll at `env(safe-area-inset-bottom) + 24px`. On iOS 26 the bar of Safari floats over the bottom of the page, so the last links stay partly under it: the sign-up terms, the register section of the static login, and the team step's "continue without a team". The dashboard solved this in `91ecd93c17` with `env(safe-area-inset-bottom) + px2rem(96)`.
- **M3 (Medium)**: no tests render `login-modal*` (`data-phone`, the close button on a phone, on a desktop, and on a desktop-width route of a phone).
- **M4 (Low)**: `frontend/test/frontend_tests/ui/dashboard_templates_test.cljs` takes `on-finish-import` from the analytics data of the modal event (`ev/-data`) instead of the modal state.
- **M5 (Low)**: the comment over the 16 px field rule in `frontend/resources/styles/common/base.scss` still names only the dashboard, the settings and the view mode.
- **S1**: the four-side safe-area padding is written out by hand in five files.

Research for the user's item (`~/DEV/penpot-demo-tools/checks/touch/login/errors.mjs`, screenshots in `out-errors/`, emulated iPhone 13 mini against the dev build):

- **Request access** (`request-dialog*`, also used by `sso-error-section*`), in the view mode: the 556 px dialog shrinks to the full 375 px, so its rounded corners touch the screen edges. Behind it, `context-wrapper*` draws the desktop header of the view mode, cut off at the right ("Device No d…"). Its two buttons squeeze into one row ("GO TO PERSONAL PROJECTS" wraps).
- **Request access in the dashboard context**: the fake desktop dashboard (sidebar) shows behind it.
- **Request access in the workspace context**: the page stays a desktop page. That is by design: a phone keeps the workspace desktop.
- **Not found and invalid invitation**: they fit (no sideways scrolling, no fields), apart from the pencil overlap (M1).

Done outside this plan, in the same session: the team dropdown's minimum height on a phone (`54ce6a25d6`), and the share sheet flush with the bottom edge (`cf3aa2c817`).

## Affected Modules

- **frontend**:
  - `app.util.touch`: the rule that decides when a phone lays a route out at its own width;
  - `app.main.ui.static`: the login, request-access, SSO-error and error pages, with `static.scss`;
  - SCSS of the auth page, the onboarding and the DS mixins;
  - tests under `frontend/test/frontend_tests/`.
- No backend, common or render-wasm change. No new UI string.

## Architecture Decisions

1. **One rule for "a phone lays this route out at its own width"**: `touch/phone-layout? [screen phone?]`, true for the view mode and the routes of `phone-page?` (dashboard, settings, auth). `viewport-content` uses it, and so does every component that switches to a phone layout. The viewport tag and the layout can then no longer disagree, which is the cause of R1. A phone keeps the workspace at desktop width, as before.
2. **The route comes from the context, not from props**: a private hook in `static.cljs`, `use-phone-layout`, reads `ctx/current-route`, which `app.main.ui/main` provides around both the exception page and every page. It returns `(touch/phone-layout? route-name (touch/phone?))`.
   - Users: `login-modal*`, `request-dialog*`, `context-wrapper*` and `error-container*`.
   - Why not props: threading `:is-phone` through `exception-page*`, `request-access*`, `sso-error-section*`, `exception-section*` and `verify-token*`'s `invalid-token` would touch 6+ call sites for the same value. The review's prop variant is equivalent.
3. **`:auth-verify-token` keeps the device-width tag** (M1, the second fix the review offers): `error-container*` gets a phone layout instead. This also fixes not-found on dashboard and view mode routes, which already have the tag.
4. **On a phone, request access (and the SSO error) is a sheet from the bottom**, like the other sheets of a phone:
   - it uses `--phone-sheet-gap` and `--phone-sheet-radius` from `viewer/phone_sheet.scss`, which sets them on `:root`;
   - its buttons are stacked full width, the primary one on top;
   - the fake desktop context (view mode header, dashboard sidebar) is not rendered behind it, since a phone's view mode has no bars.

   `context-wrapper*` skips the fake context on a phone layout; its children (the dialog) stay.
5. **The error page on a phone**:
   - the header (logo, "Go to …" link, or the "Login" button when logged out) stays clear of the notch;
   - the decorative pencil at the top is hidden, because the header link would always cross it on a narrow screen;
   - the pencil at the bottom, with the copyright, stays.
6. **The end of the scroll, M2**: on a phone, every scrolling phone layout ends with `env(safe-area-inset-bottom) + px2rem(96)` of room, the clearance of the dashboard grid (`91ecd93c17`).
   - Why not scroll the auth page as a page: the auth pages are short (only sign-up scrolls), so the clearance is enough.
   - Why the clearance also in standalone mode: there it only adds empty space at the end, as on the dashboard.
7. **S1, one mixin**: `phone-safe-padding($block-start, $inline, $block-end: px2rem(96))` in `frontend/src/app/main/ui/ds/mixins.scss` writes the four `calc(env(safe-area-inset-*) + …)` sides. The files of this plan use it. Older call sites elsewhere (`releases/base.scss`, dashboard files) stay as they are.

## Risks and Considerations

- **Rendering `login-modal*` in tests** pulls in `login-dialog*` and the register components. They read `cf/flags`, refs and the store, but render without a backend.
  - If one needs more state, render with the minimal store state (`frontend-tests.helpers.state`) or assert on the overlay and close button only.
  - Mock `touch/phone?` through `frontend-tests.helpers.mock` (`mock/with-mocks*`), not `with-redefs`: these tests are async (`thd/settle`). See `mem:frontend/testing`.
- **`context-wrapper*` without the fake context on a phone**: the overlay background (`rgb(0 0 0 / 0.65)`) then sits over the plain page background. That is intended. The fake header was only a hint of the view mode, and a phone's view mode shows no bars.
- **The pencil decoration hidden on a phone** is a visible design change. It is the smallest way to stop the overlap: any header link crosses a centered pencil on a 375 px screen.
- **The emulated checks cannot show Safari's floating bar.** They can only measure the clearance at the end of the scroll; the real iPhone (iOS 26.6) has to confirm it. After the implementation, the release snapshot must be rebuilt (`~/DEV/penpot-demo-tools/checks/touch/release-serve.sh`), or the user tests the old code (it happened in this session).
- **The specificity of the phone rules** has to keep beating the desktop rules without `!important`. Use the pattern `.x[data-phone="true"]`, as `.auth-section` and `.overlay` already do.

## Approach

1. Add `touch/phone-layout?` and route `viewport-content` through it, with tests: the workspace on a phone stays desktop.
2. Add the safe-area mixin and use it in the four existing phone layouts with the 96 px end clearance (M2, S1).
3. Give `static.cljs` the `use-phone-layout` hook. `login-modal*` uses it (R1), with render tests (M3).
4. Lay out request access and the SSO error for a phone: a bottom sheet without the fake context (the user's item).
5. Lay out `error-container*` for a phone: not found, invalid invitation, and the other exceptions (M1, the user's item).
6. The small ones: M4 (test reads the modal state) and M5 (the comment).

## Task List

### Task 1: One rule for the routes a phone lays out at its own width

**Description:** Add `phone-layout? [screen phone?]` to `app.util.touch`: `(boolean (and phone? (or (= screen :viewer) (phone-page? screen))))`.
- Make `viewport-content` use it for the phone branch. Keep `(= screen :viewer)` first, so the view mode gets the tag on every device.
- Update the namespace and function docstrings so they say that `viewport-content` and the phone layouts share this rule.

**Rationale:** R1 came from a component deciding "phone layout" differently from the viewport tag. One rule in one tested place prevents that drift.

**Code sketch:**
```clojure
(defn phone-layout?
  "Whether a `phone?` lays the page of the route `screen` out at its own
  width: the view mode and the pages of `phone-page?`, as its viewport
  tag says (see `viewport-content`). Other routes, the workspace above
  all, it keeps at the width of a desktop page."
  [screen phone?]
  (boolean (and phone? (or (= screen :viewer) (phone-page? screen)))))
```

**Acceptance criteria:**
- [ ] `phone-layout?` is true for `:viewer`, `:dashboard-recent`, `:settings-profile`, `:auth-login` and `:auth-verify-token` on a phone.
- [ ] `phone-layout?` is false for `:workspace` on a phone, and false for every route when `phone?` is false.
- [ ] `viewport-content` returns the same strings as before for every case in `viewport-tags`.

**Verification:**
- [ ] Tests first in `frontend/test/frontend_tests/data/touch_devices_test.cljs`: a new `phone-layouts` deftest, plus `:workspace`/phone → nil in `viewport-tags`. Then in the devenv: `/home/penpot/fe-test-focus.sh frontend-tests.data.touch-devices-test`, which is `pnpm run test:quiet -- --focus frontend-tests.data.touch-devices-test` with the memory watchdog. Never run pnpm on the Mac. Stop Storybook first (tmux `penpot:1` C-c).
- [ ] Lint in the devenv: `cljfmt check` and `clj-kondo --config-dir ../.clj-kondo --lint` on both files.

**Dependencies:** None

**Files likely touched:**
- `frontend/src/app/util/touch.cljs`
- `frontend/test/frontend_tests/data/touch_devices_test.cljs`

**Estimated scope:** XS

### Task 2: The end of the scroll clear of Safari's bar, from one mixin

**Description:** Add `phone-safe-padding($block-start, $inline, $block-end: px2rem(96))` to `frontend/src/app/main/ui/ds/mixins.scss`. It writes `padding` with `calc(env(safe-area-inset-top) + $block-start)` and the matching inline and bottom sides.

Use it in:
- `.auth-section[data-phone="true"]` (`auth.scss`);
- `.overlay[data-phone="true"] .dialog-login` (`static.scss`);
- the phone `.modal-container` of `onboarding/team_choice.scss` and of `onboarding/questions.scss`.

Each keeps its current top and side values; the bottom gets the 96 px clearance.

**Rationale:** M2 needs the same fix in four places, and S1 points out that they already copy each other. A mixin with the dashboard's clearance as its default fixes both, and keeps the next phone layout from forgetting it.

**Code sketch:**
```scss
// the padding of a page or sheet of a phone that scrolls: clear of the notch,
// the home indicator and, at its end, the bar Safari floats over the bottom
// on iOS 26 (see `dashboard/grid.scss`)
@mixin phone-safe-padding($block-start, $inline, $block-end: px2rem(96)) {
  padding: calc(env(safe-area-inset-top) + #{$block-start}) calc(env(safe-area-inset-right) + #{$inline})
    calc(env(safe-area-inset-bottom) + #{$block-end}) calc(env(safe-area-inset-left) + #{$inline});
}
```

**Acceptance criteria:**
- [ ] On an emulated phone with the safe areas of an iPhone (top 50, bottom 34; `safe-area.mjs` sets them through CDP `Emulation.setSafeAreaInsetsOverride`), each of these pages ends at least 34 + 96 px below its last element when scrolled to the end:
  - sign-up;
  - the static login's register section;
  - the team step;
  - every question step.
- [ ] The tops and sides are unchanged: the logo is still at 70 px with a 50 px top inset.
- [ ] Desktop rendering is unchanged (`DESK=1 node check.mjs`).

**Verification:**
- [ ] Extend `~/DEV/penpot-demo-tools/checks/touch/login/safe-area.mjs`: for each of those pages, scroll its box to the end and log the room below the last link. Run it against the dev build (`http://localhost:3450/`).
- [ ] `node check.mjs` (all states still pass) and `DESK=1 node check.mjs`.
- [ ] Lint in the devenv: `npx stylelint` and `npx prettier --check` on the five SCSS files.

**Dependencies:** None

**Files likely touched:**
- `frontend/src/app/main/ui/ds/mixins.scss`
- `frontend/src/app/main/ui/auth.scss`
- `frontend/src/app/main/ui/static.scss`
- `frontend/src/app/main/ui/onboarding/team_choice.scss`
- `frontend/src/app/main/ui/onboarding/questions.scss`

**Estimated scope:** M

### Task 3: The login of a link takes the phone layout only where the page is a phone's

**Description:** In `static.cljs`:
- add the private hook `use-phone-layout`, which reads the route from `ctx/current-route` and calls `touch/phone-layout?`;
- replace `login-modal*`'s `(mf/with-memo [] (touch/phone?))` with it.

So a logged-out workspace link on a phone shows the desktop card again: close button, 14 px fields, iOS zoom on focus. View mode and dashboard links keep the full-screen phone login without a close button.

**Rationale:** This fixes R1 at its root with the rule of Task 1. The hook serves Tasks 4 and 5 too.

**Code sketch:**
```clojure
(defn- use-phone-layout
  "Whether the page shown lays itself out for a phone: a phone, on a
  route it keeps at its own width (`touch/phone-layout?`)."
  []
  (let [screen (dm/get-in (mf/use-ctx ctx/current-route) [:data :name])]
    (mf/with-memo [screen]
      (touch/phone-layout? screen (touch/phone?)))))
```

**Acceptance criteria:**
- [ ] Phone, `:viewer` route: the overlay has `data-phone="true"` and there is no `[aria-label=Close]` button.
- [ ] Phone, `:workspace` route: `data-phone="false"` and the close button is there.
- [ ] Desktop, `:viewer` route: `data-phone="false"` and the close button is there.
- [ ] `check.mjs` `viewer-login` is unchanged. A logged-out workspace link on the emulated phone shows the 556 px card, as before `de4c1b3529`.

**Verification:**
- [ ] Render tests first, in `frontend/test/frontend_tests/ui/static_test.cljs`:
  - render `@#'app.main.ui.static/login-modal*` inside `(mf/provider ctx/current-route)` with `thd/with-dom`/`thd/render!`;
  - mock `touch/phone?` through `mock/with-mocks*`;
  - make the tests `^:async` and await `thd/settle`.

  Then run `/home/penpot/fe-test-focus.sh frontend-tests.ui.static-test`.
- [ ] Add a workspace case to `~/DEV/penpot-demo-tools/checks/touch/login/check.mjs` (logged out, `?screen=workspace&…`, expect no viewport tag and `data-phone="false"`). Run it.
- [ ] Lint on `static.cljs` and the test.

**Dependencies:** Task 1

**Files likely touched:**
- `frontend/src/app/main/ui/static.cljs`
- `frontend/test/frontend_tests/ui/static_test.cljs`

**Estimated scope:** S

### Checkpoint: After Tasks 1-3
- [ ] `/home/penpot/fe-test-focus.sh frontend-tests.data.touch-devices-test` and `frontend-tests.ui.static-test` pass. For both at once: `node target/tests/test.js --focus …` after one build.
- [ ] `node check.mjs` and `DESK=1 node check.mjs` show every state as before, plus the new workspace case.
- [ ] The dev build compiles without warnings (tmux `penpot:0`).

### Task 4: Request access and the SSO error as a sheet of a phone

**Description:**
- `request-dialog*` gets `data-phone` from `use-phone-layout` on its overlay.
- `context-wrapper*` renders no fake context (view mode header, dashboard sidebar, workspace columns) when `use-phone-layout` is true; its children stay.
- `static.scss`: on a phone, the overlay lines its dialog up at the bottom (`align-items: flex-end`). The dialog becomes a sheet:
  - `--phone-sheet-gap` from the edges, plus the safe area at the bottom;
  - `--phone-sheet-radius`, padding from Task 2's mixin;
  - `.sign-info` buttons stacked full width (`flex-direction: column-reverse`, the primary one on top), `cancel-button` without its inline margin.

**Rationale:** This is the user's item: on a phone the dialog ran edge to edge over a cut-off desktop header, with squeezed buttons. Every other dialog of a phone's view mode is a bottom sheet (`phone_sheet.scss`, `share_link.scss`, `comments.scss`), so request access follows them.

**Acceptance criteria:**
- [ ] `errors.mjs` `request-viewer` on the phone:
  - the dialog is a sheet `--phone-sheet-gap` (8 px) from the left, right and bottom edges, rounded all round;
  - no viewer header is rendered behind it;
  - both buttons are full width, each on one line.
- [ ] The same in the dashboard context (a dashboard URL of a team the harness is not a member of): no fake sidebar behind it.
- [ ] `request-workspace` on the phone is unchanged (desktop page, desktop dialog).
- [ ] `DESK=1 node errors.mjs`: the request dialog and its fake contexts look as before.
- [ ] Render test: on a phone with the `:viewer` route, `request-dialog*`'s overlay has `data-phone="true"`, and `context-wrapper*` with `:is-viewer` renders no header. On a desktop it does, if the viewer header renders in the test DOM; if not, assert only the phone case.

**Verification:**
- [ ] Tests first in `frontend/test/frontend_tests/ui/static_test.cljs`, then `/home/penpot/fe-test-focus.sh frontend-tests.ui.static-test`.
- [ ] Add a dashboard-context case to `~/DEV/penpot-demo-tools/checks/touch/login/errors.mjs` (`#/dashboard/recent?team-id=<foreign team>`). Run `node errors.mjs` and `DESK=1 node errors.mjs`, and look at the screenshots.
- [ ] Lint on the touched files.

**Dependencies:** Tasks 2, 3

**Files likely touched:**
- `frontend/src/app/main/ui/static.cljs`
- `frontend/src/app/main/ui/static.scss`
- `frontend/test/frontend_tests/ui/static_test.cljs`

**Estimated scope:** M

### Task 5: The error page of a phone

**Description:**
- `error-container*` gets `data-phone` from `use-phone-layout` on its `.exception-layout`.
- `static.scss`, on a phone:
  - `.exception-header` keeps clear of the notch: top and side padding from the safe areas, a 20 px side margin;
  - the logged-out `.login-header` keeps clear of the notch too;
  - `.deco-before` (the top pencil) is hidden; `.deco-after2` stays;
  - `.exception-content .container` is as wide as the screen less the side margins.

This covers not found and invalid invitation (`verify-token*` → `invalid-token`), and every other exception shown through `exception-section*` on a phone-layout route.

**Rationale:** M1 and the user's item. Laying out `error-container*` is better than taking `:auth-verify-token` out of the phone routes: it also fixes not found on dashboard and view mode routes, which already have the device-width tag.

**Acceptance criteria:**
- [ ] `errors.mjs` on the phone, for `not-found-viewer`, `not-found-dashboard` and `invalid-token`:
  - the header link does not overlap any decoration;
  - the header is ≥ 50 px from the top with a 50 px top inset (`safe-area.mjs`);
  - the messages are centered and wrap;
  - nothing scrolls sideways.
- [ ] Logged out, the invalid-invitation page shows its "Login" button clear of the notch and of the logo.
- [ ] `DESK=1 node errors.mjs`: the error pages are unchanged (both pencils).
- [ ] Render test: on a phone with the `:auth-verify-token` route, `error-container*`'s layout has `data-phone="true"`; on a `:workspace` route it has `"false"`.

**Verification:**
- [ ] Tests first, then `/home/penpot/fe-test-focus.sh frontend-tests.ui.static-test`.
- [ ] Add the logged-out invalid-invitation case to `errors.mjs`, and an error-page case (header top) to `safe-area.mjs`. Run `node errors.mjs`, `DESK=1 node errors.mjs` and `node safe-area.mjs`.
- [ ] Lint on the touched files.

**Dependencies:** Tasks 2, 3

**Files likely touched:**
- `frontend/src/app/main/ui/static.cljs`
- `frontend/src/app/main/ui/static.scss`
- `frontend/test/frontend_tests/ui/static_test.cljs`

**Estimated scope:** S

### Task 6: The template test reads the modal, the 16 px rule names who uses it

**Description:**
- M4: in `dashboard_templates_test.cljs`, read the callback from the modal state the show event writes: `(get-in (ptk/update shown {}) [::modal/modal :props :on-finish-import])`.
- M5: update the comment over the field rule in `base.scss`. The pages a phone lays out for itself are now the dashboard, the settings, the view mode, login and sign-up, the onboarding, and the login, request-access and error pages of a link.

**Rationale:** M4 keeps the test from depending on analytics data. M5 tells the next reader who relies on the rule.

**Acceptance criteria:**
- [ ] `frontend-tests.ui.dashboard-templates-test` passes and no longer uses `ev/-data`; the `app.main.data.event` require goes if unused.
- [ ] The `base.scss` comment lists the pages above.

**Verification:**
- [ ] `/home/penpot/fe-test-focus.sh frontend-tests.ui.dashboard-templates-test`.
- [ ] `npx stylelint` and `npx prettier --check` on `base.scss`; `cljfmt check` and `clj-kondo` on the test.

**Dependencies:** None

**Files likely touched:**
- `frontend/test/frontend_tests/ui/dashboard_templates_test.cljs`
- `frontend/resources/styles/common/base.scss`

**Estimated scope:** XS

### Checkpoint: After Tasks 4-6
- [ ] Full frontend run in the devenv: `/home/penpot/full-tests.sh` (≈ 6 min, all suites).
- [ ] `node check.mjs`, `DESK=1 node check.mjs`, `node errors.mjs`, `DESK=1 node errors.mjs`, `node safe-area.mjs` and `node sheets.mjs`, all against the dev build: every state passes, and the screenshots look right.
- [ ] Release WASM rebuilt after the test runs (`cd ~/penpot/render-wasm && CARGO_HOME=/home/penpot/.cargo BUILD_MODE=release ./build frontend`), Storybook restarted, and the release snapshot rebuilt for the real iPhone (`release-serve.sh`).

## Verification and Testing

- **Unit and render tests:**
  - `frontend-tests.data.touch-devices-test`: the rule;
  - `frontend-tests.ui.static-test`: `login-modal*`, `request-dialog*`, `context-wrapper*` and `error-container*` on phone, desktop and the workspace route;
  - `frontend-tests.ui.dashboard-templates-test`.

  Write them before the code where it applies (Tasks 1, 3, 4, 5).
- **Visual checks** (`~/DEV/penpot-demo-tools/checks/touch/login/`, emulated iPhone 13 mini, dev build): `check.mjs`, `errors.mjs`, `safe-area.mjs` and `sheets.mjs`, each also with `DESK=1` where it has that mode.
- **Real iPhone (iOS 26.6)**, by the user, after the snapshot rebuild:
  - scroll sign-up, the static login's register section and the team step to their end, and tap the last link;
  - open a view mode link of another team: the request sheet;
  - open an invalid invitation link.
- **Lint** in the devenv only: `cljfmt check`, `clj-kondo --config-dir ../.clj-kondo --lint`, `npx stylelint` and `npx prettier --check`.
- **Commits** go through `~/DEV/penpot-demo-tools/commit-split/commit-files.sh`, one per task, without AI trailers and without `manage.sh`.

## Parallelization

- Tasks 1, 2 and 6 are independent and could run in parallel.
- Task 3 needs Task 1. Tasks 4 and 5 need Tasks 2 and 3.
- Tasks 3, 4 and 5 all edit `static.cljs`, `static.scss` and `static_test.cljs`, so they run one after the other in that order.
- In one session, the order is simply 1 → 2 → 3 → 4 → 5 → 6.

## Open Questions

None. Every choice above follows from the review, the existing phone layouts (sheets, safe areas, the dashboard's clearance) or the rule that a phone keeps the workspace desktop.
