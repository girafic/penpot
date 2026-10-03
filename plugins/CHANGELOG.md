## 1.6.0 (Unreleased)

### 🚀 Features

- **plugin-types:** Added `paddingType` (`'simple' | 'multiple'`) to flex and grid layouts and `marginType` (`'simple' | 'multiple'`) to layout children, exposing whether the four padding/margin sides are mirrored or honoured independently.
- **plugin-types**: Added `waitForLayoutUpdate` to wait until pending layout updates have finished. It rejects when the optional timeout elapses, defaulting to 30 seconds so a wait never hangs.
- **plugin-types**: Added `waitForLayoutUpdate` to the `Shape` interface to wait until the pending layout updates of a shape and its children have finished
- **plugin-types**: Added the **motion API** for the keyframe animation of boards (Penpot Motion, behind the `animation/v1` feature): `Board.timeline`, `Board.addTimeline()` and `Page.timelines`; the `Timeline` interface with its settings (`name`, `duration`, `playback`), `keyframes`, `animations` and `markers`, `addKeyframe()`, `addAnimation()`, `addAnimationStyle()`, `addMarker()`, `record()` (auto-keyframe: records the changes a callback makes as keyframes), `valueAt()`, `clear()`, `toCSS()` and `remove()`; and the `Keyframe`, `KeyframeProps`, `PresetAnimation`, `PresetAnimationProps`, `Marker`, `MarkerProps`, `TimelineOptions`, `AnimatableProperty`, `MotionEasing`, `TimelinePlayback`, `PresetAnimationType` and `AnimationStyle` types.
- **plugin-types**: Added `Timeline.exportSettings` (the format, quality, frame rate, size and loop an animation is exported with, as the export panel keeps them; setting it changes the ones given) with the `TimelineExportSettings`, `TimelineExportFormat` and `TimelineExportSize` types, and `Board.animatedCopies` (the component copies in a board that play the animation of their mains) with the `AnimatedCopy` type. `Timeline.toCSS()` now includes the animated component copies of the board.
- **plugin-types**: Added `Timeline.copyAnimation()` to copy the animation of a layer onto other layers of the page, in any board, as pasting a copied animation on the canvas does: each target moves, turns and resizes from where it rests, optionally only some properties, with exact values, at another start or staggered. Added the `CopyAnimationOptions` and `CopyAnimationResult` types.
- **plugin-types**: `TimelineExportSettings.format` takes `dotlottie`: a dotLottie archive (`.lottie`) holding the Lottie animation and its images as files.
- **plugin-types**: Added split animations of text layers: `PresetAnimationProps.split` (`'char' | 'word' | 'line'`) plays a preset animation on each character, word or line of a text in turn, with `spread`, `order` and `seed`, readable and settable on `PresetAnimation`; added the `TextSplit` and `TextSplitOrder` types. Only text layers split, and a text plays one kind of units. `AnimationStyle` adds the text styles `typewriter`, `rise-words`, `cascade-lines` and `pop-letters`, for text layers only. `Timeline.toCSS()` plays split animations on the whole text, as the SVG and Lottie exports do.
- **plugin-types**: `PresetAnimationType` takes `blur`: a layer blur from a radius of `amount` pixels (8 by default) to the layer's own, on any layer (one without a blur gets one while it plays) and on each unit of a split text.
- **plugin-types**: Added animation styles of libraries: `Library.animationStyles` and `Library.createAnimationStyle(entries)` (in the library of the file itself) with the `LibraryAnimationStyle` type (`description`, `entries`, `duration`, `remove()`, `clone()`), whose edits every use follows, and the `AnimationStyleEntry` type. `Timeline.applyAnimationStyle(shape, style, start?)` adds a use of a style of the file or of a connected library, and `Timeline.appliedStyles` lists the uses (`AppliedAnimationStyle`: `style`, `name`, `start`, `duration`, `detach()`, `remove()`). `Timeline.animations` no longer lists uses of styles. A style of a library of another file can only be read.
- **plugin-types**: Added audio clips of timelines: `Timeline.audioClips` and `Timeline.addAudioClip(name, data, mimeType, props?)`, which decodes the bytes of an audio file in the browser (MP3, WAV, Ogg, M4A, WebM or FLAC, at most 5 minutes), uploads it and resolves to the new clip, with the `AudioClip` type (`start`, `offset`, `duration`, `speed`, `lane`, `volume`, `muted`, `fadeIn`, `fadeOut`, `locked`, `split()`, `remove()`) and the `AudioClipProps` type. `TimelineExportSettings.audio` says whether an MP4 or WebM export has their sound (`true` by default).
- **plugin-types**: Added the motion token types `duration` (a number of ms or s, resolved to ms) and `cubicBezier` (four numbers `[x1, y1, x2, y2]`) to `TokenType`, with the `TokenDuration` and `TokenCubicBezier` types; no shape takes them, so `applyToken`, `applyToShapes` and `applyToSelected` refuse them. `AnimationStyleEntry.appliedTokens` (`AnimationStyleEntryTokens`) names the tokens that set the start, duration, easing, amount or offsets of an entry; the file of the style resolves them, and setting `LibraryAnimationStyle.entries` with them binds them.
- **plugin-types**: Added `Timeline.toJSON()` (the animation of a board as data, version 1: the board, its duration, playback and markers, the animated layers with their rest pose, pivot, keyframes by property with the easing of each segment and preset animations, the layers of animated component copies with the clock of their main, and the audio clips; `JSON.stringify(timeline)` gives it as text) with the `TimelineJson`, `TimelineJsonLayer`, `TimelineJsonProperty`, `TimelineJsonKeyframe`, `TimelineJsonEasing`, `TimelineJsonPreset`, `TimelineJsonComponent`, `TimelineJsonMarker` and `TimelineJsonAudioClip` types, and `Timeline.toReact()` (React code for motion.dev). They give what the new Motion tab of Inspect gives with the board selected. `Timeline.toCSS()` now gives the CSS of that tab: the classes of the code of Inspect instead of `.penpot-shape-<id>`, keeping the transforms the shapes have at rest.
- **plugin-types**: Added `Timeline.autoplay` and `TimelineOptions.autoplay`: whether the prototype (view mode) plays the animation of a board from its start when it shows the board (`true` by default; `false` keeps the board at rest until it is played). In view mode the animated component copies in a board go on looping on their own clock once its animation has ended.
- **plugin-types**: Added the motion token type `spring` (`{ stiffness, damping, mass }`, DTCG `$type` `spring`) to `TokenType`, with the `TokenSpring` and `TokenSpringValue` types; like the other motion tokens it applies to no shape. Design tokens now bind the settings of preset animations and keyframes too: `PresetAnimation.appliedTokens` and `PresetAnimationProps.appliedTokens` (`AnimationStyleEntryTokens`) name the tokens that set the start, duration, easing, amount or offsets of an animation, and `Keyframe.appliedTokens` and `KeyframeProps.appliedTokens` (`KeyframeTokens`) the token that sets the easing of a keyframe. The file resolves them in its active sets on every page, and setting a bound setting by hand unbinds its token. An easing takes a `cubicBezier` or a `spring` token. `TimelineJsonKeyframe.tokens` names the token of a keyframe in `Timeline.toJSON()`.
- **plugin-types**: Added keyframe entries to animation styles: `AnimationStyleEntry` is now `PresetStyleEntry | KeyframesStyleEntry`. A `KeyframesStyleEntry` (`type: 'keyframes'`, with `property`, `index`, `start`, `duration` and `keyframes`, each a `KeyframesStyleEntryKeyframe`) holds the keyframes of one property, their values relative to the layer that plays them (offsets for the position and rotation, factors for the size), and plays in that span of the row of the property on every layer that uses the style, but where the layer has keyframes of its own in that row (from the first to the last of them), which play instead; `createAnimationStyle` and `LibraryAnimationStyle.entries` take them, a style holding at most 256 keyframes. Added `LibraryAnimationStyle.updateFromShape(shape)`, which saves what a layer that uses the style plays now, its rows of keyframes too, into the style, and every use follows. Detaching a use, or removing its style, gives its layer the keyframes of its keyframe entries. In `Timeline.toJSON()` the keyframe entries of a use are not listed as presets: their keyframes are in the properties.
- **plugin-types**: Split text plays by its units in the code and the markup exports: `Timeline.toCSS()` gives a rule for each character or word (the class `penpot-unit-<n>`, which `generateMarkup` puts on a span around it), `toReact()` a `motion.span` for each, and `toJSON()` their keyframes in the new `TimelineJsonLayer.units` (`TimelineJsonUnits`); a split by lines plays on the whole text there. The SVG, Lottie and dotLottie exports play every kind of units, each as its own picture (an underlined or struck text plays whole). Turned layers, and layers with shadows or blur, sit where they are in these exports.
- **plugin-types**: The animated component copies in a board play the audio clips of the board of their main, on their own clock, once for all the copies of one main (see `Board.animatedCopies`), in the workspace, in view mode and in MP4 and WebM exports; `Timeline.audioClips` still lists only the board's own clips.

### 🩹 Fixes

- **plugins-runtime**: An interaction obtained from `Shape.interactions` now keeps addressing that interaction instead of the position it held when the array was read. Removing every interaction of a shape from a single read removes all of them rather than leaving some behind, and writing through a held interaction after an earlier one is removed no longer lands on a different interaction.
- **plugins-runtime**: `Shape.removeInteraction()` now rejects an interaction belonging to a different shape with a validation error, instead of removing whichever interaction sat at the same position on the target shape.
- **plugins-runtime**: `Library.createComponent()` now rejects invalid input (an empty shape list, or a shape inside a component copy) with a validation error instead of returning a component proxy pointing at nothing.
- **plugins-runtime**: Setting an individual padding/margin side (`leftPadding`, `topMargin`, …) now re-derives the padding/margin type, switching to `multiple` when the four sides stop being symmetric (so the value is actually painted) and back to `simple` once top/bottom and left/right are mirrored again.
- **plugins-runtime**: Removed the premature deep-hardening of the host plugin context, which froze shared host functions (including `Function.prototype`) before SES override taming, causing `TypeError: Cannot assign to read only property 'toString'` on later host-side function extension. Related to #11001.
- **plugins-runtime**: Fixed the `fontFamilies` token property mapping so `Shape.applyToken(token, ["fontFamilies"])` resolves to the canonical `:font-family` attribute and applied-token readback exposes the documented `fontFamilies` key instead of the undocumented singular `fontFamily`. Closes #11405.

## 1.5.0 (2026-07-08)

### 💣 Breaking changes & Deprecations

- **plugins-runtime**: changes outside the current page now raise a validation error when the target belongs to a page that is not currently active, instead of silently operating on the active page.
- **plugin-types**: Change return type of `combineAsVariants`
- **plugin-types**: Removed the `type` (`'layer-blur'`) property from the `Blur` interface; a blur is now described only by `id`, `value` and `hidden`.
- **plugin-types:** Deprecate the legacy `Image` shape interface — image shapes exist only for backward compatibility with old files; new images are embedded in a `Fill` via its `fillImage` (an `ImageData`).
- We've solved several inconsistencies accross the API, if you relied on an undocumented property or method be aware that might have changed.

### 🚀 Features

- **plugin-types**: Added the design **tokens API**: `Library.tokens` (a `TokenCatalog` exposing `themes`/`sets`, `addTheme`, `addSet`, `getThemeById`, `getSetById`), the `TokenSet` and `TokenTheme` interfaces, the full `Token` union (`TokenColor`, `TokenDimension`, `TokenBorderRadius`, `TokenShadow`, `TokenTypography`, `TokenFontFamilies`, etc.) with `value`/`resolvedValue`/`resolvedValueString`, `Shape.tokens` and `Shape.applyToken()`, per-token `applyToShapes()`/`applyToSelected()`, and the `TokenType` and `TokenProperty` types. Notable behaviors:
  - `TokenCatalog.addSet` accepts an optional `active` flag to create an already-active set (sets are inactive by default).
  - `TokenTheme.addSet` and `TokenTheme.removeSet` accept a token set id (`string`) in addition to a `TokenSet`.
  - `TokenSet.addToken` resolves references against all token sets, allowing references to tokens in inactive sets.
  - A `fontFamilies` token's `resolvedValue` returns the resolved family list as `string[]`.
- **plugin-types**: Added `backgroundBlur` property for shapes (a `Blur` applied to the content behind the shape)
- **plugin-types**: Added a `hidden` flag to `penpot.ui.open()` options to open the plugin without showing the modal
- **plugins-runtime**: Added `version` field that returns the current version
- **plugins-runtime**: Added optional parameter `throwOnError` to `penpot.ui.sendMessage` (default false, backwards-compatible)
- **plugin-types**: Added a `penpot.flags` subcontext with the flag `naturalChildOrdering`
- **plugin-types**: Added flag `throwValidationErrors` to enable exceptions on validation
- **plugin-types**: `penpot.openPage()` now returns `Promise<void>` and should be awaited before performing operations on the new page
- **plugin-types:** Change `LibraryComponent.isVariant()` return type to type guard `this is LibraryVariantComponent`
- **plugin-types**: Added `createVariantFromComponents`
- **plugin-types**: Added `textBounds` property for text shapes
- **plugin-types**: Fix missing `webp` export format in `Export.type`
- **plugin-types**: Added `fixedWhenScrolling` property for shapes
- **plugin-types**: Added `Page.remove()` to remove a page from the file (the last remaining page cannot be removed; removing the active page activates another one)
- **plugin-types**: Added `RulerGuide.remove()` to remove a ruler guide
- **plugin-types**: Added `File.validate()` to run the file's referential-integrity validation and return the list of errors found (empty when the file is valid) — the same errors the backend rejects on save
- **plugin-types**: Added `Shape.resetOverrides()` to restore a component copy's attributes (and its children's) to the linked main component, like the "reset overrides" action on the Penpot interface

### 🩹 Fixes

- **plugins-runtime**: The validation error raised when setting a `fontWeight` the current font has no variant for now lists the weights the font supports.
- **plugins-runtime**: Fix inverted validation that rejected valid values (and accepted invalid ones) on text range `align`, `direction`, `textDecoration`, `letterSpacing` and on layout child `zIndex`.
- **plugins-runtime**: Array-typed properties (e.g. `page.flows`, `shape.exports`, `shape.shadows`, layout `rows`/`columns`, ruler guides, path `commands`) now always return an array, returning an empty array instead of `null` when there are no items
- **plugin-types**: Fix `CommonLayout.horizontalSizing`/`verticalSizing` values, which were typed as `'fit-content'` but the runtime uses `'fix'` (now `'fix' | 'fill' | 'auto'`)
- **plugin-types**: Fix `Shape.layoutCell` type, which pointed to `LayoutChildProperties` instead of `LayoutCellProperties`
- **plugin-types**: Mark the interaction `animation` as optional (`animation?: Animation`) to match interactions that have no animation
- **plugin-types**: Fix penpot.openPage() to navigate in same tab by default
- **plugin-types**: Rename `LibraryTypography.fontFamilies` to `fontFamily` to match the runtime (it holds a single font family, not an array)
- **plugin-runtime:** Setting a `LibraryColor`'s `gradient` or `image` now clears the other color representations (solid/gradient/image are mutually exclusive), so the result is a valid color instead of being rejected with "expected valid color"
- **plugin-types:** Mark members that have no runtime setter as `readonly`, fixing a mismatch where they were typed as writable: font metadata (`Font.*`, `FontVariant.*`, `FontsContext.all`), the `Ellipse`/`Image`/`SvgRaw` `type` discriminants (now consistent with the other shapes), `File.name`/`pages`/`revn`, `Page.root`, `TokenTheme.activeSets`, `Variants.properties`, `ImageData.*`, the board guide value objects (`GuideColumn`/`GuideRow`/`GuideSquare` and their params — `board.guides` returns a formatted snapshot, so reconfiguring means reassigning the whole array), the `Point` and `Bounds` value objects, the `Penpot.ui`/`Penpot.utils` subcontexts, the derived `Boolean` path data (`d`/`content`/`commands` are computed from the operands; `Boolean` is not editable like a `Path`), and the `EventsMap` event entries (a type-only event→callback map, never assigned). Members that do expose a setter stay writable: `Board.children`, `Path.d`/`content`/`commands` and `FileVersion.label`.
- **plugins-runtime**: `Shape.rotation` (and `Shape.rotate()`) now reject `NaN`/non-finite values instead of letting them reach the geometry layer as an invalid move vector (which surfaced as an error toast).
- **plugins-runtime**: The positional variant-property operations (`Variants.removeProperty`, `Variants.renameProperty`, `LibraryVariantComponent.setVariantProperty`) now reject an out-of-range `pos` instead of letting it reach the data layer as an index-out-of-bounds error (which surfaced as an error toast).
- **plugins-runtime**: `createShapeFromSvg`/`createShapeFromSvgWithImages` now reject malformed SVG markup up front instead of failing asynchronously inside the import pipeline (which surfaced as an "SVG is invalid or malformed" error toast).
- **plugins-runtime**: Setting `FileVersion.label` no longer raises an internal error toast after renaming the version (the internal rename event was built with a misplaced argument).
- **plugins-runtime**: `Text.getRange(start, end)` now clamps an `end` past the text length to the character count instead of throwing an internal `TypeError` when reading the range's `characters`.
- **plugins-runtime**: `penpot.openPage()` (and `Page.openPage()`) now resolves immediately when the target page is already active, instead of waiting forever for a page-initialization event that never fires.
- **plugins-runtime**: `Shape.shadows`, `Shape.exports` and grid `rows`/`columns` now return live proxies, so writing a member on a returned shadow/export/track (e.g. `shape.shadows[0].blur = 7`) persists to the shape instead of mutating a detached snapshot that was silently discarded. The shadow `color` remains a plain snapshot (reconfigure it by assigning `shadow.color`).
- **plugins-runtime**: Setting a variant component's `path` now renames the whole variant (its container and every main instance), like the `name` setter already did, instead of renaming only the component and leaving the file referentially inconsistent (which the backend rejected on save with a `variant-component-bad-name` error).
- **plugins-runtime**: `Page.getSharedPluginDataKeys(namespace)` now works instead of always raising a namespace validation error: the implementation expected a spurious leading argument, so the caller's `namespace` was read as a missing second argument.
- **plugins-runtime**: Storing plugin data on a connected (non-local) shared library is now consistently rejected with a `setPluginData-non-local-library` error on the `Library` object as well as its assets (colors, typographies, components). Previously the `Library` object accepted the write and applied it optimistically, but plugin data is not part of library synchronization and the change only persists when the caller can edit the library file — on a read-only shared library it failed silently and was lost on reload. Plugin data can only be stored on the file currently being edited.

## 1.4.2 (2026-01-21)

- **plugin-runtime:** fix atob/btoa functions

## 1.4.0 (2026-01-21)

### 🚀 Features

- switch component ([7d68450](https://github.com/penpot/penpot-plugins/commit/7d68450))
- Add variants to plugins API ([04f3c26](https://github.com/penpot/penpot-plugins/commit/04f3c26))
- format ci job ([17b5834](https://github.com/penpot/penpot-plugins/commit/17b5834))
- fix problem with ci ([4b3c50f](https://github.com/penpot/penpot-plugins/commit/4b3c50f))
- change in workflow ([3a69f51](https://github.com/penpot/penpot-plugins/commit/3a69f51))
- **plugin-types:** add methods to modify the index for shapes ([4ad50af](https://github.com/penpot/penpot-plugins/commit/4ad50af))
- **plugin-types:** change content type and added new attributes ([dbb68a5](https://github.com/penpot/penpot-plugins/commit/dbb68a5))
- **plugins-runtime:** add data method to image data ([f077481](https://github.com/penpot/penpot-plugins/commit/f077481))
- **plugins-runtime:** fix problem with linter ([30f4984](https://github.com/penpot/penpot-plugins/commit/30f4984))
- **plugins-runtime:** allow openPage() to toggle opening on a new window or not ([da8288b](https://github.com/penpot/penpot-plugins/commit/da8288b))

### 🩹 Fixes

- package-lock.json ([d1d940a](https://github.com/penpot/penpot-plugins/commit/d1d940a))
- fonts gdpr & switch provider ([d63231e](https://github.com/penpot/penpot-plugins/commit/d63231e))
- missing changes ([b8fc936](https://github.com/penpot/penpot-plugins/commit/b8fc936))
- format ci ([e0fab2e](https://github.com/penpot/penpot-plugins/commit/e0fab2e))
- fetch main only in pr ([e48c5d4](https://github.com/penpot/penpot-plugins/commit/e48c5d4))

### ❤️ Thank You

- alonso.torres
- Juanfran @juanfran
- Michał Korczak
- Miguel de Benito Delgado
- Pablo Alba

## 1.3.2 (2025-07-04)

### 🩹 Fixes

- plugins-runtime public package.json ([70fd69f](https://github.com/penpot/penpot-plugins/commit/70fd69f))

### ❤️ Thank You

- Juanfran @juanfran

## 1.3.1 (2025-07-04)

### 🚀 Features

- plugins-runtime as npm library ([41c56b1](https://github.com/penpot/penpot-plugins/commit/41c56b1))

### 🩹 Fixes

- package-lock.json ([16b29f8](https://github.com/penpot/penpot-plugins/commit/16b29f8))

### ❤️ Thank You

- Juanfran @juanfran

## 1.3.0 (2025-06-25)

### 🚀 Features

- **plugin-types:** add skipChildren to exports ([b3373ba](https://github.com/penpot/penpot-plugins/commit/b3373ba))
- **plugins-runtime:** change plugins modal z-index ([c6a4a7d](https://github.com/penpot/penpot-plugins/commit/c6a4a7d))
- **plugins-runtime:** adds max resize to the screen size ([f2fe501](https://github.com/penpot/penpot-plugins/commit/f2fe501))
- **plugins-runtime:** adds localstorage wrapper API for plugins ([0006ca9](https://github.com/penpot/penpot-plugins/commit/0006ca9))
- **plugins-runtime:** add generateFontFaces method ([30e1d02](https://github.com/penpot/penpot-plugins/commit/30e1d02))
- **poc-state-plugins:** add some methods to the example ([b95961a](https://github.com/penpot/penpot-plugins/commit/b95961a))
- **poc-state-plugins:** example using the localstorage api ([b101523](https://github.com/penpot/penpot-plugins/commit/b101523))

### 🩹 Fixes

- **plugin-colors-to-tokens:** adapt to Penpot tokens metadata format ([3a1ff00](https://github.com/penpot/penpot-plugins/commit/3a1ff00))
- **plugin-colors-to-tokens:** avoid unvalid character in names ([dd0fd1a](https://github.com/penpot/penpot-plugins/commit/dd0fd1a))
- **plugin-types:** add missing board properties ([de4a2a0](https://github.com/penpot/penpot-plugins/commit/de4a2a0))
- **plugin-types:** fix problem with type ([9759964](https://github.com/penpot/penpot-plugins/commit/9759964))
- **plugins-runtime:** add allow-same-origin to iframe ([65d5351](https://github.com/penpot/penpot-plugins/commit/65d5351))
- **plugins-runtime:** fixes null checking issue ([6b5b562](https://github.com/penpot/penpot-plugins/commit/6b5b562))
- **plugins-runtime:** fix problem with resize modal position ([45dc41d](https://github.com/penpot/penpot-plugins/commit/45dc41d))
- **plugins-styles:** migrate to fonts css api v2 ([45a9ee9](https://github.com/penpot/penpot-plugins/commit/45a9ee9))

### ❤️ Thank You

- alonso.torres
- Martynas Barzda
- Xavier Julian

## 1.2.0 (2025-02-27)

### 🚀 Features

- upgrade nx & angular & prettier ([32de075](https://github.com/penpot/penpot-plugins/commit/32de075))
- add ui.resize & ui.size api ([815181d](https://github.com/penpot/penpot-plugins/commit/815181d))
- colors to tokens export plugin ([7f8a011](https://github.com/penpot/penpot-plugins/commit/7f8a011))
- transform color & opacity to rgba ([9a3e6e0](https://github.com/penpot/penpot-plugins/commit/9a3e6e0))
- **plugin-colors-to-tokens:** only rgba when the opacity is not 1 ([e922cf9](https://github.com/penpot/penpot-plugins/commit/e922cf9))
- **plugin-types:** deprecated fields in colors ([6adcc4c](https://github.com/penpot/penpot-plugins/commit/6adcc4c))
- **plugins-runtime:** add upload svg with images ([df925b5](https://github.com/penpot/penpot-plugins/commit/df925b5))

### 🩹 Fixes

- duplicated css ([19ca648](https://github.com/penpot/penpot-plugins/commit/19ca648))
- add error styles on invalid input ([1c29c34](https://github.com/penpot/penpot-plugins/commit/1c29c34))
- remove nonexistent api ([3837f1c](https://github.com/penpot/penpot-plugins/commit/3837f1c))

### ❤️ Thank You

- alonso.torres
- Juanfran @juanfran
- Michał Korczak

## 1.1.0 (2024-12-12)

### 🚀 Features

- updated doc links ([cb49dfb](https://github.com/penpot/penpot-plugins/commit/cb49dfb))
- **plugin-types:** add support for file history versions ([eab57d7](https://github.com/penpot/penpot-plugins/commit/eab57d7))

### 🩹 Fixes

- styles rename layers ([40e08f8](https://github.com/penpot/penpot-plugins/commit/40e08f8))
- **rename-layers:** i#8951 disable buttons when empty ([#8951](https://github.com/penpot/penpot-plugins/issues/8951))

### ❤️ Thank You

- alonso.torres
- María Valderrama @mavalroot
- Marina López @cocotime

# 1.0.0 (2024-10-25)

### 🚀 Features

- **plugins-runtime:** add close callback to load api ([aeddab7](https://github.com/penpot/penpot-plugins/commit/aeddab7))
- **runtime:** unload plugin ([b4d0463](https://github.com/penpot/penpot-plugins/commit/b4d0463))

### 🩹 Fixes

- search in icons plugin ([b4664a2](https://github.com/penpot/penpot-plugins/commit/b4664a2))
- **table-plugin:** i#8965 empty cell values when importing csv files ([#8965](https://github.com/penpot/penpot-plugins/issues/8965))

### ❤️ Thank You

- alonso.torres
- Juanfran @juanfran
- María Valderrama @mavalroot
- Marina López @cocotime

## 0.12.0 (2024-10-04)

### 🚀 Features

- e2e tests ([1371af9](https://github.com/penpot/penpot-plugins/commit/1371af9))
- add build to CI ([a434209](https://github.com/penpot/penpot-plugins/commit/a434209))
- **api-doc:** update readme ([99ff81d](https://github.com/penpot/penpot-plugins/commit/99ff81d))
- **docs:** add examples for new permissions ([2f0f7a6](https://github.com/penpot/penpot-plugins/commit/2f0f7a6))
- **e2e:** add screenshots ENV variable ([9292bf2](https://github.com/penpot/penpot-plugins/commit/9292bf2))
- **plugin-types:** add ruler guides and new zoom methods ([c8066be](https://github.com/penpot/penpot-plugins/commit/c8066be))
- **plugin-types:** add apis for comments ([e34e56c](https://github.com/penpot/penpot-plugins/commit/e34e56c))
- **plugin-types:** update comment related methods ([50bc7ba](https://github.com/penpot/penpot-plugins/commit/50bc7ba))
- **plugin-types:** removed old method and replaced with attributes ([1866299](https://github.com/penpot/penpot-plugins/commit/1866299))
- **plugins-runtime:** plugin live reload ([bbc77e4](https://github.com/penpot/penpot-plugins/commit/bbc77e4))
- **plugins-runtime:** adds new permissions `comment:read`, `comment:write` and `allow:downloads` ([5adbee2](https://github.com/penpot/penpot-plugins/commit/5adbee2))
- **plugins-runtime:** expose some public JS APIs to the plugins code ([22dfa92](https://github.com/penpot/penpot-plugins/commit/22dfa92))
- **poc-state-plugin:** add new functions to the plugin to test comments and rulers ([6adee11](https://github.com/penpot/penpot-plugins/commit/6adee11))
- **rename-layers:** final review - undo group ([2909bcc](https://github.com/penpot/penpot-plugins/commit/2909bcc))
- **runtime:** refactor plugin state ([16595c2](https://github.com/penpot/penpot-plugins/commit/16595c2))
- **runtime:** remove deprecated method ([ccc5f78](https://github.com/penpot/penpot-plugins/commit/ccc5f78))
- **table-plugin:** enhancement save config ([07af57d](https://github.com/penpot/penpot-plugins/commit/07af57d))

### 🩹 Fixes

- **e2e:** update dump params to shape model ([ade39ee](https://github.com/penpot/penpot-plugins/commit/ade39ee))
- **plugin-types:** optional path curves ([0ea57f1](https://github.com/penpot/penpot-plugins/commit/0ea57f1))
- **plugins-runtime:** clean pending timeouts ([8870dda](https://github.com/penpot/penpot-plugins/commit/8870dda))
- **plugins-runtime:** prevent plugin execution after close ([b65492a](https://github.com/penpot/penpot-plugins/commit/b65492a))
- **plugins-styles:** import svg inline ([567b0b5](https://github.com/penpot/penpot-plugins/commit/567b0b5))
- **runtime:** ses errorTrapping interferes with penpot error handler ([8c0e36d](https://github.com/penpot/penpot-plugins/commit/8c0e36d))
- **runtime:** prevent override Penpot objects ([120e9e5](https://github.com/penpot/penpot-plugins/commit/120e9e5))

### ❤️ Thank You

- alonso.torres
- Juanfran @juanfran
- María Valderrama @mavalroot

## 0.10.0 (2024-07-31)

### 🚀 Features

- change permissions names ([99126f8](https://github.com/penpot/penpot-plugins/commit/99126f8))
- stop offering icons in the style library ([5a219e9](https://github.com/penpot/penpot-plugins/commit/5a219e9))
- new publish script ([5114e78](https://github.com/penpot/penpot-plugins/commit/5114e78))
- init e2e test ([b0af705](https://github.com/penpot/penpot-plugins/commit/b0af705))
- **docs:** how api docs are generated ([e047977](https://github.com/penpot/penpot-plugins/commit/e047977))
- **docs:** basic css theme for typedoc ([0eac44d](https://github.com/penpot/penpot-plugins/commit/0eac44d))
- **plugin-types:** update API types ([bffa467](https://github.com/penpot/penpot-plugins/commit/bffa467))
- **plugin-types:** add pages info to the file ([b54edb3](https://github.com/penpot/penpot-plugins/commit/b54edb3))
- **plugin-types:** add parent reference to the shape ([2588778](https://github.com/penpot/penpot-plugins/commit/2588778))
- **plugin-types:** add root shape reference to the pages ([c712759](https://github.com/penpot/penpot-plugins/commit/c712759))
- **plugin-types:** add undo block operations to api ([1d3ad89](https://github.com/penpot/penpot-plugins/commit/1d3ad89))
- **plugins-runtime:** update selection ([f36fa23](https://github.com/penpot/penpot-plugins/commit/f36fa23))
- **plugins-runtime:** add new events 'contentsave' and 'shapechange', changed on/off signatures ([2b8a76b](https://github.com/penpot/penpot-plugins/commit/2b8a76b))
- **plugins-runtime:** add detach shape from component method ([ff488d4](https://github.com/penpot/penpot-plugins/commit/ff488d4))
- **plugins-runtime:** add API to access to prototypes ([a554775](https://github.com/penpot/penpot-plugins/commit/a554775))
- **plugins-runtime:** add method for pages ([9a9b33a](https://github.com/penpot/penpot-plugins/commit/9a9b33a))
- **plugins-types:** expose new attributes ([9ce45a2](https://github.com/penpot/penpot-plugins/commit/9ce45a2))

### 🩹 Fixes

- typo checkox > checkbox ([877a3f2](https://github.com/penpot/penpot-plugins/commit/877a3f2))
- avoid plugin location question ([b4c6165](https://github.com/penpot/penpot-plugins/commit/b4c6165))
- add files so no unexpected when creating new plugin ([ef5629a](https://github.com/penpot/penpot-plugins/commit/ef5629a))
- eslint migration to ESM docs ([249ea62](https://github.com/penpot/penpot-plugins/commit/249ea62))
- fix runtime version ([95afbf3](https://github.com/penpot/penpot-plugins/commit/95afbf3))
- horizontal scroll height on plugins modal ([08f989a](https://github.com/penpot/penpot-plugins/commit/08f989a))
- **contrast-plugin:** update colors when shape change ([8ce04d3](https://github.com/penpot/penpot-plugins/commit/8ce04d3))
- **docs:** add missing variant on destructive button ([9fa96e9](https://github.com/penpot/penpot-plugins/commit/9fa96e9))
- **plugin-types:** readonly PenpotShapeBase width & height ([415284f](https://github.com/penpot/penpot-plugins/commit/415284f))
- **plugins-runtime:** remove plugin event listener on close ([2138985](https://github.com/penpot/penpot-plugins/commit/2138985))
- **plugins-runtime:** fix problem with types in test ([17db173](https://github.com/penpot/penpot-plugins/commit/17db173))
- **styles:** input, button & select worksans font family ([1b9d3b2](https://github.com/penpot/penpot-plugins/commit/1b9d3b2))

### ❤️ Thank You

- alonso.torres
- Juanfran @juanfran
- María Valderrama @mavalroot
- Marina López @cocotime
- Xaviju

## 0.9.0 (2024-07-10)

### 🚀 Features

- change permissions names ([99126f8](https://github.com/penpot/penpot-plugins/commit/99126f8))
- stop offering icons in the style library ([5a219e9](https://github.com/penpot/penpot-plugins/commit/5a219e9))
- new publish script ([5114e78](https://github.com/penpot/penpot-plugins/commit/5114e78))
- **plugin-types:** update API types ([bffa467](https://github.com/penpot/penpot-plugins/commit/bffa467))
- **plugins-runtime:** update selection ([f36fa23](https://github.com/penpot/penpot-plugins/commit/f36fa23))
- **plugins-types:** expose new attributes ([9ce45a2](https://github.com/penpot/penpot-plugins/commit/9ce45a2))

### 🩹 Fixes

- typo checkox > checkbox ([877a3f2](https://github.com/penpot/penpot-plugins/commit/877a3f2))
- avoid plugin location question ([b4c6165](https://github.com/penpot/penpot-plugins/commit/b4c6165))
- fix runtime version ([2401a77](https://github.com/penpot/penpot-plugins/commit/2401a77))
- **styles:** input, button & select worksans font family ([1b9d3b2](https://github.com/penpot/penpot-plugins/commit/1b9d3b2))

### ❤️ Thank You

- alonso.torres
- Juanfran @juanfran
- Marina López @cocotime
- Xaviju @xaviju
