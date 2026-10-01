//! Painting a text with its units posed one by one, as a split animation of
//! Penpot Motion moves them (see `shapes::text_units`). The text is laid out
//! as it paints at rest; its glyph runs are then drawn piece by piece, the
//! glyphs of each unit moved, turned, scaled and faded around the centre of
//! the unit's box. Shaping, kerning, fallback fonts and emoji so stay as Skia
//! laid them out, and the glyphs of no unit draw at rest. The text content
//! is never changed, so it stays editable.
//!
//! Each glyph run paints with the paint its style run has in the laid out
//! paragraph, so every pass of the text renderer poses alike: fills,
//! strokes, the masks of inner and outer strokes and the shadow masks
//! (`text::render` and the stroke renderers end in `paint_layouts` when the
//! shape has units). A glyph goes with the unit of the first character of
//! its cluster, so a ligature moves with its first character.

use std::ops::Range;

use skia_safe::{
    textlayout::{self, RectHeightStyle, RectWidthStyle, TextStyle},
    Canvas, Matrix, Paint, Point, Rect,
};

use crate::globals::current_browser;
use crate::performance;
use crate::shapes::text_units::{TextUnits, UnitPose};
use crate::shapes::{Paragraph, ParagraphLayout, TextContent, TextDecorationSegment};

/// A unit piece where it paints: its range in the laid out text of its
/// paragraph, its box at rest and the matrix that poses it.
struct Placed {
    range: Range<usize>,
    bounds: Rect,
    matrix: Matrix,
    opacity: f32,
}

/// Paint `layouts`, the paragraphs of the text `content` laid out from
/// builder groups of `group_sizes` builders each (a group per paragraph, see
/// `calculate_text_layout_data`), with `units` posed. A layout the groups do
/// not account for paints at rest.
pub fn paint_layouts(
    canvas: &Canvas,
    content: &TextContent,
    layouts: &mut [ParagraphLayout],
    group_sizes: &[usize],
    units: &TextUnits,
) {
    performance::begin_measure!("render_text_units");

    let paragraphs = content.paragraphs();
    let paragraph_of: Vec<usize> = group_sizes
        .iter()
        .enumerate()
        .flat_map(|(index, &size)| std::iter::repeat_n(index, size))
        .collect();

    if paragraph_of.len() != layouts.len()
        || group_sizes.len() != paragraphs.len()
        || group_sizes.contains(&0)
    {
        for layout in layouts.iter() {
            layout.paragraph.paint(canvas, (layout.x, layout.y));
            for decoration in &layout.decorations {
                paint_decoration(canvas, decoration, &[]);
            }
        }
        performance::end_measure!("render_text_units");
        return;
    }

    // the units are found once per paragraph, in the first of its layouts
    let pieces = {
        let mut firsts: Vec<&textlayout::Paragraph> = Vec::with_capacity(paragraphs.len());
        for (layout, &paragraph) in layouts.iter().zip(&paragraph_of) {
            if paragraph == firsts.len() {
                firsts.push(&layout.paragraph);
            }
        }
        units.pieces(paragraphs, &firsts, current_browser())
    };

    for (layout, &index) in layouts.iter_mut().zip(&paragraph_of) {
        let placed: Vec<Placed> = pieces
            .iter()
            .filter(|piece| piece.paragraph == index)
            .map(|piece| place(layout, piece.range.clone(), &piece.pose))
            .collect();
        paint_paragraph(canvas, &paragraphs[index], layout, &placed);
    }

    performance::end_measure!("render_text_units");
}

/// Where the piece `range` of the laid out paragraph is, and the matrix
/// that poses it around the centre of its box.
fn place(layout: &ParagraphLayout, range: Range<usize>, pose: &UnitPose) -> Placed {
    let bounds = layout
        .paragraph
        .get_rects_for_range(range.clone(), RectHeightStyle::Tight, RectWidthStyle::Tight)
        .iter()
        .fold(Rect::new_empty(), |mut bounds, text_box| {
            bounds.join(text_box.rect);
            bounds
        })
        .with_offset((layout.x, layout.y));
    let center = bounds.center();

    let mut matrix = Matrix::translate((center.x + pose.dx, center.y + pose.dy));
    matrix.pre_rotate(pose.rotation, None);
    matrix.pre_scale((pose.scale_x, pose.scale_y), None);
    matrix.pre_translate((-center.x, -center.y));

    Placed {
        range,
        bounds,
        matrix,
        opacity: pose.opacity,
    }
}

/// Paint the laid out `paragraph`: its glyph runs split where a unit starts
/// or ends, then its decorations, cut where they run under a unit.
fn paint_paragraph(
    canvas: &Canvas,
    paragraph: &Paragraph,
    layout: &mut ParagraphLayout,
    placed: &[Placed],
) {
    let text = laid_out_text(paragraph);
    let utf16_at = utf8_to_utf16(&text);
    let utf16_len = utf16_at[text.len()];

    // the piece each UTF-16 unit of the laid out text goes with
    let mut piece_at: Vec<Option<usize>> = vec![None; utf16_len];
    for (index, piece) in placed.iter().enumerate() {
        let end = piece.range.end.min(utf16_len);
        for slot in &mut piece_at[piece.range.start.min(end)..end] {
            *slot = Some(index);
        }
    }

    // the paint of each style run of each line, by the UTF-8 offset where it
    // starts (the keys of the style metrics)
    let line_paints: Vec<Vec<(usize, Paint)>> = layout
        .paragraph
        .get_line_metrics()
        .iter()
        .map(|line| {
            line.get_style_metrics(0..usize::MAX)
                .into_iter()
                .map(|(start, metrics)| (start, paint_of(metrics.text_style)))
                .collect()
        })
        .collect();
    let paint_at = |line: usize, cluster: usize| {
        let runs = line_paints.get(line)?;
        runs.iter()
            .rev()
            .find(|(start, _)| *start <= cluster)
            .or(runs.first())
            .map(|(_, paint)| paint)
    };

    let origin = Point::new(layout.x, layout.y);
    layout.paragraph.visit(|line, info| {
        let Some(info) = info else {
            return;
        };
        let glyphs = info.glyphs();
        let positions = info.positions();
        let clusters = info.utf8_starts();
        let utf16_of = |glyph: usize| {
            clusters
                .get(glyph)
                .and_then(|&cluster| utf16_at.get(cluster as usize))
                .copied()
                .unwrap_or(0)
        };
        let piece_of = |glyph: usize| piece_at.get(utf16_of(glyph)).copied().flatten();
        let Some(paint) = paint_at(line, clusters.first().copied().unwrap_or(0) as usize) else {
            return;
        };
        let run_origin = origin + info.origin();

        let mut start = 0;
        while start < glyphs.len() {
            let piece = piece_of(start);
            let mut end = start + 1;
            while end < glyphs.len() && piece_of(end) == piece {
                end += 1;
            }
            let draw = |canvas: &Canvas, paint: &Paint| {
                canvas.draw_glyphs_at(
                    &glyphs[start..end],
                    &positions[start..end],
                    run_origin,
                    info.font(),
                    paint,
                );
            };
            match piece.map(|index| &placed[index]) {
                None => draw(canvas, paint),
                Some(piece) => posed(canvas, piece, paint, draw),
            }
            start = end;
        }
    });

    for decoration in &layout.decorations {
        paint_decoration(canvas, decoration, placed);
    }
}

/// Draw with `draw` and `paint` as the unit of `piece` stands: moved,
/// turned, scaled and faded. A unit faded out draws nothing.
fn posed(canvas: &Canvas, piece: &Placed, paint: &Paint, draw: impl Fn(&Canvas, &Paint)) {
    if piece.opacity <= 0.0 {
        return;
    }
    canvas.save();
    canvas.concat(&piece.matrix);
    if piece.opacity < 1.0 {
        let mut faded = paint.clone();
        faded.set_alpha_f(paint.alpha_f() * piece.opacity);
        draw(canvas, &faded);
    } else {
        draw(canvas, paint);
    }
    canvas.restore();
}

/// Paint `decoration` (an underline or line through a span on one line):
/// the parts under a unit go with it, the rest stays.
fn paint_decoration(canvas: &Canvas, decoration: &TextDecorationSegment, placed: &[Placed]) {
    let mut paint = decoration.text_style.foreground();
    paint.set_anti_alias(true);
    let rect = decoration.rect();

    // the units on the line of the decoration, left to right
    let mut cuts: Vec<&Placed> = placed
        .iter()
        .filter(|piece| {
            piece.bounds.left < rect.right
                && piece.bounds.right > rect.left
                && (piece.bounds.center_y() - rect.center_y()).abs() <= piece.bounds.height()
        })
        .collect();
    cuts.sort_by(|a, b| a.bounds.left.total_cmp(&b.bounds.left));

    let mut left = rect.left;
    for piece in cuts {
        let cut_left = piece.bounds.left.max(left);
        let cut_right = piece.bounds.right.min(rect.right);
        if cut_left > left {
            canvas.draw_rect(Rect::new(left, rect.top, cut_left, rect.bottom), &paint);
        }
        if cut_right > cut_left {
            let part = Rect::new(cut_left, rect.top, cut_right, rect.bottom);
            posed(canvas, piece, &paint, |canvas, paint| {
                canvas.draw_rect(part, paint);
            });
        }
        left = left.max(cut_right);
    }
    if rect.right > left {
        canvas.draw_rect(Rect::new(left, rect.top, rect.right, rect.bottom), &paint);
    }
}

/// The text Skia lays out for `paragraph`: the spans with their text
/// transforms, each tab a placeholder (U+FFFC), a space when it is empty
/// (see `TextContent::paragraph_builders`).
fn laid_out_text(paragraph: &Paragraph) -> String {
    let text: String = paragraph
        .children()
        .iter()
        .map(|span| span.apply_text_transform().replace('\t', "\u{FFFC}"))
        .collect();
    if text.is_empty() {
        " ".to_string()
    } else {
        text
    }
}

/// The UTF-16 offset of each UTF-8 offset of `text`, one more for the end.
fn utf8_to_utf16(text: &str) -> Vec<usize> {
    let mut offsets = Vec::with_capacity(text.len() + 1);
    let mut utf16 = 0;
    for c in text.chars() {
        offsets.extend(std::iter::repeat_n(utf16, c.len_utf8()));
        utf16 += c.len_utf16();
    }
    offsets.push(utf16);
    offsets
}

/// The paint `style` paints its glyphs with.
fn paint_of(style: &TextStyle) -> Paint {
    if style.has_foreground() {
        style.foreground()
    } else {
        let mut paint = Paint::default();
        paint.set_color(style.color());
        paint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globals::TestRenderResourcesGuard;
    use crate::render::{text, RenderResources};
    use crate::shapes::text_units::{TextUnit, UnitKind};
    use crate::shapes::{
        calculate_text_layout_data, Fill, FontFamily, FontStyle, GrowType, Shadow, ShadowStyle,
        Shape, SolidColor, Stroke, StrokeStyle, TextAlign, TextDirection, TextSpan, Type,
    };
    use crate::uuid::Uuid;
    use skia_safe as skia;

    const WIDTH: i32 = 400;
    const HEIGHT: i32 = 120;

    fn text_shape(text: &str, decoration: Option<skia::textlayout::TextDecoration>) -> Shape {
        let bounds = Rect::from_xywh(0.0, 0.0, WIDTH as f32, HEIGHT as f32);
        let mut content = TextContent::new(bounds, GrowType::Fixed);
        let span = TextSpan::new(
            text.to_string(),
            FontFamily::new(Uuid::nil(), 400, FontStyle::Normal),
            40.0,
            1.2,
            0.0,
            decoration,
            None,
            TextDirection::LTR,
            400,
            Uuid::nil(),
            vec![Fill::Solid(SolidColor(skia::Color::BLACK))],
        );
        content.add_paragraph(Paragraph::new(
            TextAlign::Left,
            TextDirection::LTR,
            None,
            None,
            1.2,
            0.0,
            vec![span],
        ));
        let mut shape = Shape::new(Uuid::nil());
        shape.set_selrect(0.0, 0.0, WIDTH as f32, HEIGHT as f32);
        shape.set_shape_type(Type::Text(content));
        shape
    }

    fn unit(kind: UnitKind, start: u32, end: u32, pose: UnitPose) -> TextUnit {
        TextUnit {
            kind,
            index: 0,
            start,
            end,
            pose,
        }
    }

    /// A pass of the text renderer, as `render_shape` runs it.
    type Pass = fn(&Canvas, &Shape);

    fn fills(canvas: &Canvas, shape: &Shape) {
        let content = shape.get_text_content();
        let mut builders = content.paragraph_builder_group_from_text(None);
        text::render(
            None,
            Some(canvas),
            shape,
            &mut builders,
            None,
            None,
            None,
            None,
            None,
        )
        .expect("text fills");
    }

    /// A drop shadow (offset 8, blur 4) under the fills.
    fn shadow_and_fills(canvas: &Canvas, shape: &Shape) {
        let shadow = Shadow::new(
            skia::Color::BLACK,
            4.0,
            0.0,
            (8.0, 8.0),
            ShadowStyle::Drop,
            false,
        );
        let mut paint = Paint::default();
        paint.set_image_filter(shadow.get_drop_shadow_filter());
        let content = shape.get_text_content();
        let mut builders = content.paragraph_builder_group_from_text(Some(true));
        text::render(
            None,
            Some(canvas),
            shape,
            &mut builders,
            None,
            Some(&paint),
            None,
            None,
            None,
        )
        .expect("text shadow");
        fills(canvas, shape);
    }

    /// An outer stroke 4 wide, red, around the fills.
    fn outer_stroke_and_fills(canvas: &Canvas, shape: &Shape) {
        let mut stroke = Stroke::new_outer_stroke(4.0, StrokeStyle::Solid, None, None, None, None);
        stroke.fill = Fill::Solid(SolidColor(skia::Color::RED));
        let content = shape.get_text_content();
        let (mut builders, opacity) = text::stroke_paragraph_builder_group_from_text(
            content,
            &stroke,
            &shape.selrect(),
            None,
        );
        fills(canvas, shape);
        text::render_outer_stroke(
            None,
            Some(canvas),
            shape,
            &mut builders,
            None,
            None,
            4.0,
            opacity,
        )
        .expect("text outer stroke");
    }

    /// The pixels `pass` paints for the text of `shape` with `units` posed
    /// (none: at rest) on white, RGBA row by row.
    fn painted(shape: &Shape, units: Option<&TextUnits>, pass: Pass) -> Vec<u8> {
        let mut shape = shape.clone();
        shape.set_text_units(units.cloned());
        let mut surface = skia::surfaces::raster_n32_premul((WIDTH, HEIGHT)).expect("surface");
        let canvas = surface.canvas();
        canvas.clear(skia::Color::WHITE);
        pass(canvas, &shape);
        let info = surface.image_info();
        let row_bytes = info.min_row_bytes();
        let mut pixels = vec![0; row_bytes * HEIGHT as usize];
        assert!(surface.read_pixels(&info, &mut pixels, row_bytes, (0, 0)));
        pixels
    }

    fn pixel(pixels: &[u8], x: i32, y: i32) -> &[u8] {
        let at = ((y * WIDTH + x) * 4) as usize;
        &pixels[at..at + 4]
    }

    fn inked(pixels: &[u8], x: i32, y: i32) -> bool {
        pixel(pixels, x, y) != [255, 255, 255, 255]
    }

    /// The box of the pixels with ink within the columns `columns`.
    fn ink_box(pixels: &[u8], columns: Range<i32>) -> Option<(i32, i32, i32, i32)> {
        let mut found: Option<(i32, i32, i32, i32)> = None;
        for y in 0..HEIGHT {
            for x in columns.clone() {
                if inked(pixels, x, y) {
                    found = Some(match found {
                        None => (x, y, x, y),
                        Some((l, t, r, b)) => (l.min(x), t.min(y), r.max(x), b.max(y)),
                    });
                }
            }
        }
        found
    }

    /// Where the UTF-16 range `range` of the text of `shape` paints at rest.
    fn range_box(shape: &Shape, range: Range<usize>) -> Rect {
        let content = shape.get_text_content();
        let mut builders = content.paragraph_builder_group_from_text(None);
        let layout = calculate_text_layout_data(shape, content, &mut builders, true);
        place(&layout.paragraphs[0], range, &UnitPose::REST).bounds
    }

    #[test]
    fn units_at_rest_paint_as_the_text_does() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        let shape = text_shape("Hello world", None);
        let units = TextUnits {
            line_count: 0,
            units: vec![
                unit(UnitKind::Word, 6, 11, UnitPose::REST),
                unit(UnitKind::Char, 1, 2, UnitPose::REST),
            ],
        };

        let at_rest = painted(&shape, None, fills);
        assert!(ink_box(&at_rest, 0..WIDTH).is_some(), "the text paints");
        assert!(
            at_rest == painted(&shape, Some(&units), fills),
            "same pixels"
        );
    }

    #[test]
    fn a_moved_word_moves_its_glyphs_only() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        let shape = text_shape("Hello world", None);
        let world = range_box(&shape, 6..11);
        let units = TextUnits {
            line_count: 0,
            units: vec![unit(
                UnitKind::Word,
                6,
                11,
                UnitPose {
                    dy: 20.0,
                    ..UnitPose::REST
                },
            )],
        };

        let at_rest = painted(&shape, None, fills);
        let moved = painted(&shape, Some(&units), fills);
        let split = world.left.floor() as i32 - 1;

        // "Hello" stays
        assert_eq!(ink_box(&moved, 0..split), ink_box(&at_rest, 0..split));
        // "world" is 20 px lower, pixel for pixel
        let (l, t, r, b) = ink_box(&at_rest, split..WIDTH).expect("world paints");
        assert_eq!(ink_box(&moved, split..WIDTH), Some((l, t + 20, r, b + 20)));
        for y in t..=b {
            for x in l..=r {
                assert_eq!(pixel(&moved, x, y + 20), pixel(&at_rest, x, y));
            }
        }
    }

    #[test]
    fn faded_units_leave_only_the_others() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        let shape = text_shape("Hello world", None);
        let hidden = UnitPose {
            opacity: 0.0,
            ..UnitPose::REST
        };
        // every character but the `w`
        let units = TextUnits {
            line_count: 0,
            units: (0..11)
                .filter(|&i| i != 5 && i != 6)
                .map(|i| unit(UnitKind::Char, i, i + 1, hidden))
                .collect(),
        };

        let w = range_box(&shape, 6..7);
        let (l, t, r, b) =
            ink_box(&painted(&shape, Some(&units), fills), 0..WIDTH).expect("the w paints");
        assert!(
            l as f32 >= w.left.floor() && r as f32 <= w.right.ceil(),
            "only the w: {l}..{r} in {w:?}"
        );
        assert!(t as f32 >= w.top.floor() && b as f32 <= w.bottom.ceil());
    }

    #[test]
    fn half_faded_units_paint_lighter() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        let shape = text_shape("Hello world", None);
        let units = TextUnits {
            line_count: 0,
            units: vec![unit(
                UnitKind::Word,
                0,
                11,
                UnitPose {
                    opacity: 0.5,
                    ..UnitPose::REST
                },
            )],
        };

        let at_rest = painted(&shape, None, fills);
        let faded = painted(&shape, Some(&units), fills);
        let darkest = |pixels: &[u8]| pixels.chunks(4).map(|p| p[0]).min().unwrap();
        assert_eq!(darkest(&at_rest), 0);
        let half = darkest(&faded);
        assert!((120..=135).contains(&half), "half faded black: {half}");
    }

    #[test]
    fn decorations_go_with_their_unit() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        let shape = text_shape(
            "Hello world",
            Some(skia::textlayout::TextDecoration::UNDERLINE),
        );
        let world = range_box(&shape, 6..11);
        let units = TextUnits {
            line_count: 0,
            units: vec![unit(
                UnitKind::Word,
                6,
                11,
                UnitPose {
                    dy: 30.0,
                    ..UnitPose::REST
                },
            )],
        };

        let at_rest = painted(&shape, None, fills);
        let moved = painted(&shape, Some(&units), fills);
        // the underline under "world" is in the moved word's columns, 30 px lower
        let column = ((world.left + world.right) / 2.0) as i32;
        let lowest = |pixels: &[u8]| (0..HEIGHT).rev().find(|&y| inked(pixels, column, y));
        assert_eq!(lowest(&moved), lowest(&at_rest).map(|y| y + 30));
    }

    #[test]
    fn the_shadow_of_a_moved_word_moves_with_it() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        // a wide gap, so the shadow and stroke of "Hello" stay clear of "world"
        let shape = text_shape("Hello    world", None);
        let world = range_box(&shape, 9..14);
        let units = TextUnits {
            line_count: 0,
            units: vec![unit(
                UnitKind::Word,
                9,
                14,
                UnitPose {
                    dy: 30.0,
                    ..UnitPose::REST
                },
            )],
        };

        let at_rest = painted(&shape, None, shadow_and_fills);
        let moved = painted(&shape, Some(&units), shadow_and_fills);
        let split = world.left.floor() as i32 - 2;

        // "Hello" and its shadow stay as they are, not cut where "world" starts
        for y in 0..HEIGHT {
            for x in 0..split {
                assert_eq!(pixel(&moved, x, y), pixel(&at_rest, x, y), "at {x},{y}");
            }
        }
        // "world" and its shadow are 30 px lower: none is left where it was
        let (l, t, r, b) = ink_box(&at_rest, split..WIDTH).expect("world paints");
        assert_eq!(
            ink_box(&moved, split..WIDTH),
            Some((l, t + 30, r, (b + 30).min(HEIGHT - 1)))
        );
        for y in t..t + 30 {
            for x in split..WIDTH {
                assert!(!inked(&moved, x, y), "nothing left at {x},{y}");
            }
        }
    }

    #[test]
    fn the_outer_stroke_of_a_moved_word_is_whole() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        // a wide gap, so the shadow and stroke of "Hello" stay clear of "world"
        let shape = text_shape("Hello    world", None);
        let world = range_box(&shape, 9..14);
        let units = TextUnits {
            line_count: 0,
            units: vec![unit(
                UnitKind::Word,
                9,
                14,
                UnitPose {
                    dy: 20.0,
                    ..UnitPose::REST
                },
            )],
        };

        let at_rest = painted(&shape, None, outer_stroke_and_fills);
        let moved = painted(&shape, Some(&units), outer_stroke_and_fills);
        let split = world.left.floor() as i32 - 6;

        // the stroked "world" moved pixel for pixel: its stroke goes all round
        let (l, t, r, b) = ink_box(&at_rest, split..WIDTH).expect("world paints");
        assert_eq!(ink_box(&moved, split..WIDTH), Some((l, t + 20, r, b + 20)));
        for y in t..=b {
            for x in l..=r {
                assert_eq!(
                    pixel(&moved, x, y + 20),
                    pixel(&at_rest, x, y),
                    "at {x},{y}"
                );
            }
        }
    }
}
