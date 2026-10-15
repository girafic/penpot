//! The units of a text (characters, words or lines) that a split animation
//! of Penpot Motion moves, scales, turns and fades one by one. The frontend
//! works out the pose of each unit at a time (easing, order, spread) and
//! sends the units that are off their rest pose with
//! `set_shape_text_units`; this module holds them, finds them in the laid
//! out text and tells how far they can reach out of it. The text content is
//! never changed: units are found by their offsets, so the text stays
//! editable.
//!
//! Byte contract (little endian), as `app.common.render-wasm.serializers.text-units`
//! writes it:
//!
//! ```text
//! u32 count, u32 line_count, then `count` entries of 44 bytes:
//!   u8 kind (0 char, 1 word, 2 line), u8[3] padding,
//!   u32 index (the paragraph of a char or word unit; the line of a line unit),
//!   u32 start, u32 end (UTF-16 offsets in the source text of the paragraph; 0 for a line),
//!   f32 dx, f32 dy, f32 rotation (degrees), f32 scale_x, f32 scale_y, f32 opacity,
//!   f32 blur (the radius of a layer blur, px)
//! ```

use std::collections::HashMap;
use std::ops::Range;

use skia_safe::textlayout;

use super::{radius_to_sigma, Paragraph};
use crate::error::{Error, Result};
use crate::math::Rect;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnitKind {
    Char,
    Word,
    Line,
}

impl TryFrom<u8> for UnitKind {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self> {
        match value {
            0 => Ok(Self::Char),
            1 => Ok(Self::Word),
            2 => Ok(Self::Line),
            _ => Err(Error::RecoverableError(format!(
                "unknown text unit kind {value}"
            ))),
        }
    }
}

/// How a unit stands off its rest pose: moved by `dx`, `dy`, turned by
/// `rotation` degrees and scaled around the centre of its box, faded to
/// `opacity` and blurred by `blur`, the radius of a layer blur.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnitPose {
    pub dx: f32,
    pub dy: f32,
    pub rotation: f32,
    pub scale_x: f32,
    pub scale_y: f32,
    pub opacity: f32,
    pub blur: f32,
}

impl UnitPose {
    pub const REST: Self = Self {
        dx: 0.0,
        dy: 0.0,
        rotation: 0.0,
        scale_x: 1.0,
        scale_y: 1.0,
        opacity: 1.0,
        blur: 0.0,
    };

    /// The pose of the given values, a value that is not a number taking
    /// the rest one, the opacity kept within 0 and 1 and the blur from 0.
    fn sanitized(
        dx: f32,
        dy: f32,
        rotation: f32,
        scale_x: f32,
        scale_y: f32,
        opacity: f32,
        blur: f32,
    ) -> Self {
        let or = |value: f32, rest: f32| if value.is_finite() { value } else { rest };
        let rest = Self::REST;
        Self {
            dx: or(dx, rest.dx),
            dy: or(dy, rest.dy),
            rotation: or(rotation, rest.rotation),
            scale_x: or(scale_x, rest.scale_x),
            scale_y: or(scale_y, rest.scale_y),
            opacity: or(opacity, rest.opacity).clamp(0.0, 1.0),
            blur: or(blur, rest.blur).max(0.0),
        }
    }

    /// Whether a point of the unit can end up further from the centre of
    /// its box than it is at rest: when it turns or grows.
    fn reaches_out(&self) -> bool {
        self.rotation != 0.0 || self.scale_x.abs() > 1.0 || self.scale_y.abs() > 1.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextUnit {
    pub kind: UnitKind,
    /// The paragraph of a character or word unit; for a line unit, its index
    /// among the lines of the text that are not blank, top to bottom.
    pub index: u32,
    /// The UTF-16 range of a character or word unit in the source text of
    /// its paragraph, before the text transforms (see `utf16_offset_map`).
    pub start: u32,
    pub end: u32,
    pub pose: UnitPose,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextUnits {
    /// How many lines that are not blank the frontend saw: a line at or past
    /// it takes the pose of the last one, so a line typed before the
    /// frontend knows of it moves with the one above instead of at rest.
    pub line_count: u32,
    pub units: Vec<TextUnit>,
}

/// A piece of the laid out text that moves as one: a character or word
/// unit, or a line that takes the pose of a line unit, by its UTF-16 range
/// in the laid out text of its paragraph; `unit` is the index of its unit
/// among the units.
#[derive(Debug, Clone, PartialEq)]
pub struct UnitPiece {
    pub unit: usize,
    pub paragraph: usize,
    pub range: Range<usize>,
    pub pose: UnitPose,
}

impl TextUnits {
    pub const HEADER_SIZE: usize = 8;
    pub const ENTRY_SIZE: usize = 44;

    /// The units `bytes` holds (see the byte contract above); `None` for no
    /// units, which clears them.
    pub fn from_bytes(bytes: &[u8]) -> Result<Option<Self>> {
        let truncated = || Error::RecoverableError("truncated text units".to_string());
        let u32_at = |at: usize| -> Result<u32> {
            let word = bytes.get(at..at + 4).ok_or_else(truncated)?;
            Ok(u32::from_le_bytes([word[0], word[1], word[2], word[3]]))
        };

        let count = u32_at(0)? as usize;
        let line_count = u32_at(4)?;
        if count == 0 {
            return Ok(None);
        }

        let size = count
            .checked_mul(Self::ENTRY_SIZE)
            .and_then(|entries| entries.checked_add(Self::HEADER_SIZE))
            .ok_or_else(truncated)?;
        if bytes.len() < size {
            return Err(truncated());
        }

        let units = bytes[Self::HEADER_SIZE..size]
            .as_chunks::<{ Self::ENTRY_SIZE }>()
            .0
            .iter()
            .map(|entry| parse_unit(entry))
            .collect::<Result<Vec<_>>>()?;

        Ok(Some(Self { line_count, units }))
    }

    /// The pieces of the laid out text that move with the units: the range
    /// of each character or word unit in the laid out text of its paragraph,
    /// and each line that takes the pose of a line unit. `paragraphs` are
    /// the paragraphs of the text and `layouts` them laid out; `browser`
    /// decides which control characters the laid out text drops. Units out
    /// of the text are left out.
    pub fn pieces(
        &self,
        paragraphs: &[Paragraph],
        layouts: &[&textlayout::Paragraph],
        browser: u8,
    ) -> Vec<UnitPiece> {
        let mut maps: HashMap<usize, Vec<usize>> = HashMap::new();
        let mut line_poses: HashMap<u32, (usize, UnitPose)> = HashMap::new();
        let mut pieces = Vec::new();

        for (position, unit) in self.units.iter().enumerate() {
            match unit.kind {
                UnitKind::Char | UnitKind::Word => {
                    let index = unit.index as usize;
                    let Some(paragraph) = paragraphs.get(index) else {
                        continue;
                    };
                    let map = maps
                        .entry(index)
                        .or_insert_with(|| utf16_offset_map(paragraph, browser));
                    let range = map_range(map, unit.start, unit.end);
                    if !range.is_empty() {
                        pieces.push(UnitPiece {
                            unit: position,
                            paragraph: index,
                            range,
                            pose: unit.pose,
                        });
                    }
                }
                UnitKind::Line => {
                    line_poses
                        .entry(unit.index)
                        .or_insert((position, unit.pose));
                }
            }
        }

        if !line_poses.is_empty() {
            let last = self.line_count.max(1) - 1;
            for (line, (paragraph, range)) in non_blank_lines(layouts).into_iter().enumerate() {
                let unit = (line as u32).min(last);
                if let Some((position, pose)) = line_poses.get(&unit) {
                    pieces.push(UnitPiece {
                        unit: *position,
                        paragraph,
                        range,
                        pose: *pose,
                    });
                }
            }
        }

        pieces
    }

    /// How far the units can reach out of `text`, the box of the text at
    /// rest: as far as they move and blur and, for a unit that turns or
    /// grows, as far as a box as big as the text could turn or grow around
    /// its centre.
    pub fn outset(&self, text: &Rect) -> f32 {
        let half_diagonal = text.width().hypot(text.height()) / 2.0;
        self.units
            .iter()
            .map(|unit| {
                let pose = &unit.pose;
                let blurred = if pose.blur > 0.0 {
                    3.0 * radius_to_sigma(pose.blur)
                } else {
                    0.0
                };
                let moved = pose.dx.abs().max(pose.dy.abs()) + blurred;
                if pose.reaches_out() {
                    let scale = pose.scale_x.abs().max(pose.scale_y.abs()).max(1.0);
                    moved + scale * half_diagonal
                } else {
                    moved
                }
            })
            .fold(0.0, f32::max)
    }
}

fn parse_unit(bytes: &[u8]) -> Result<TextUnit> {
    let word = |at: usize| [bytes[at], bytes[at + 1], bytes[at + 2], bytes[at + 3]];
    let u32_at = |at: usize| u32::from_le_bytes(word(at));
    let f32_at = |at: usize| f32::from_le_bytes(word(at));

    Ok(TextUnit {
        kind: UnitKind::try_from(bytes[0])?,
        index: u32_at(4),
        start: u32_at(8),
        end: u32_at(12),
        pose: UnitPose::sanitized(
            f32_at(16),
            f32_at(20),
            f32_at(24),
            f32_at(28),
            f32_at(32),
            f32_at(36),
            f32_at(40),
        ),
    })
}

/// For each UTF-16 offset of the source text of `paragraph`, the offset in
/// its laid out text, where the ignored control characters of `browser` are
/// dropped or spaces and each span has its text transform (see
/// `TextSpan::for_each_laid_out_char`); one more entry for the end. An
/// offset inside a character maps to the start of it.
pub fn utf16_offset_map(paragraph: &Paragraph, browser: u8) -> Vec<usize> {
    let mut map = Vec::new();
    let mut offset = 0;
    for span in paragraph.children() {
        span.for_each_laid_out_char(browser, |c, len| {
            map.extend(std::iter::repeat_n(offset, c.len_utf16()));
            offset += len;
        });
    }
    map.push(offset);
    map
}

/// The source range `start..end` in the laid out text by `map` (see
/// `utf16_offset_map`), clamped to the text.
fn map_range(map: &[usize], start: u32, end: u32) -> Range<usize> {
    let last = map.len() - 1;
    let at = |offset: u32| map[(offset as usize).min(last)];
    at(start)..at(end.max(start))
}

/// The lines of a laid out text that hold more than whitespace, top to
/// bottom: the paragraph and the UTF-16 range in its laid out text,
/// trailing whitespace left out. `layouts` are the paragraphs laid out. A
/// blank paragraph has no line, so it never shifts the index of the lines
/// below it.
pub fn non_blank_lines(layouts: &[&textlayout::Paragraph]) -> Vec<(usize, Range<usize>)> {
    let mut lines = Vec::new();
    for (index, layout) in layouts.iter().enumerate() {
        for line in layout.get_line_metrics() {
            if line.end_excluding_whitespaces > line.start_index {
                lines.push((index, line.start_index..line.end_excluding_whitespaces));
            }
        }
    }
    lines
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globals::TestRenderResourcesGuard;
    use crate::render::RenderResources;
    use crate::shapes::{
        FontFamily, FontStyle, GrowType, TextAlign, TextContent, TextDirection, TextSpan,
        TextTransform,
    };
    use crate::state::ShapesPool;
    use crate::utils::Browser;
    use crate::uuid::Uuid;

    fn bytes(line_count: u32, entries: &[(u8, u32, u32, u32, [f32; 7])]) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend((entries.len() as u32).to_le_bytes());
        out.extend(line_count.to_le_bytes());
        for (kind, index, start, end, floats) in entries {
            out.extend([*kind, 0, 0, 0]);
            out.extend(index.to_le_bytes());
            out.extend(start.to_le_bytes());
            out.extend(end.to_le_bytes());
            for value in floats {
                out.extend(value.to_le_bytes());
            }
        }
        out
    }

    fn span(text: &str, transform: Option<TextTransform>) -> TextSpan {
        TextSpan::new(
            text.to_string(),
            FontFamily::new(Uuid::nil(), 400, FontStyle::Normal),
            14.0,
            1.2,
            0.0,
            None,
            transform,
            TextDirection::LTR,
            400,
            Uuid::nil(),
            vec![],
        )
    }

    fn paragraph(spans: Vec<TextSpan>) -> Paragraph {
        Paragraph::new(
            TextAlign::Left,
            TextDirection::LTR,
            None,
            None,
            1.2,
            0.0,
            spans,
        )
    }

    const REST: [f32; 7] = [0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0];

    #[test]
    fn no_units_clear_them() {
        assert_eq!(TextUnits::from_bytes(&bytes(0, &[])).unwrap(), None);
    }

    #[test]
    fn units_read_their_kind_place_and_pose() {
        let units = TextUnits::from_bytes(&bytes(
            3,
            &[
                (1, 0, 6, 11, [0.0, 12.5, 0.0, 1.0, 1.0, 0.4, 0.0]),
                (2, 1, 0, 0, [3.0, 0.0, 45.0, 2.0, 0.5, 1.0, 2.5]),
            ],
        ))
        .unwrap()
        .unwrap();

        assert_eq!(units.line_count, 3);
        assert_eq!(
            units.units,
            vec![
                TextUnit {
                    kind: UnitKind::Word,
                    index: 0,
                    start: 6,
                    end: 11,
                    pose: UnitPose {
                        dy: 12.5,
                        opacity: 0.4,
                        ..UnitPose::REST
                    },
                },
                TextUnit {
                    kind: UnitKind::Line,
                    index: 1,
                    start: 0,
                    end: 0,
                    pose: UnitPose {
                        dx: 3.0,
                        rotation: 45.0,
                        scale_x: 2.0,
                        scale_y: 0.5,
                        blur: 2.5,
                        ..UnitPose::REST
                    },
                },
            ]
        );
    }

    #[test]
    fn broken_units_are_an_error_not_a_panic() {
        let full = bytes(0, &[(0, 0, 0, 1, REST), (0, 0, 1, 2, REST)]);
        assert!(TextUnits::from_bytes(&full[..full.len() - 1]).is_err());
        assert!(TextUnits::from_bytes(&full[..6]).is_err());
        assert!(TextUnits::from_bytes(&[]).is_err());
        assert!(TextUnits::from_bytes(&bytes(0, &[(7, 0, 0, 1, REST)])).is_err());

        let mut huge = bytes(0, &[]);
        huge[0..4].copy_from_slice(&u32::MAX.to_le_bytes());
        assert!(TextUnits::from_bytes(&huge).is_err());
    }

    #[test]
    fn values_that_are_no_number_rest() {
        let units = TextUnits::from_bytes(&bytes(
            0,
            &[(
                0,
                0,
                0,
                1,
                [f32::NAN, f32::INFINITY, f32::NAN, f32::NAN, 2.0, 7.0, -3.0],
            )],
        ))
        .unwrap()
        .unwrap();
        assert_eq!(
            units.units[0].pose,
            UnitPose {
                scale_y: 2.0,
                ..UnitPose::REST
            }
        );
    }

    #[test]
    fn offsets_follow_text_transforms() {
        let firefox = Browser::Firefox as u8;
        let chrome = Browser::Chrome as u8;

        // `ß` takes two UTF-16 units in uppercase, so the `e` after it moves on
        let map = utf16_offset_map(
            &paragraph(vec![span("straße", Some(TextTransform::Uppercase))]),
            chrome,
        );
        assert_eq!(map_range(&map, 4, 5), 4..6);
        assert_eq!(map_range(&map, 5, 6), 6..7);
        assert_eq!(map.last(), Some(&7));

        // capitalize keeps the offsets
        let map = utf16_offset_map(
            &paragraph(vec![span("hello world", Some(TextTransform::Capitalize))]),
            chrome,
        );
        assert_eq!(map_range(&map, 6, 11), 6..11);

        // a control character is dropped in Firefox, a space elsewhere
        let text = paragraph(vec![span("a\u{0007}b", None)]);
        assert_eq!(map_range(&utf16_offset_map(&text, firefox), 2, 3), 1..2);
        assert_eq!(map_range(&utf16_offset_map(&text, chrome), 2, 3), 2..3);

        // an emoji keeps its two UTF-16 units, across spans
        let text = paragraph(vec![
            span("🎉", None),
            span("ab", Some(TextTransform::Uppercase)),
        ]);
        assert_eq!(map_range(&utf16_offset_map(&text, chrome), 0, 2), 0..2);
        assert_eq!(map_range(&utf16_offset_map(&text, chrome), 3, 4), 3..4);

        // offsets past the end are clamped
        assert_eq!(map_range(&utf16_offset_map(&text, chrome), 3, 99), 3..4);

        // a family joined by ZWJ keeps its eight UTF-16 units
        let text = paragraph(vec![span("👨\u{200d}👩\u{200d}👧 a", None)]);
        assert_eq!(map_range(&utf16_offset_map(&text, chrome), 0, 8), 0..8);
        assert_eq!(map_range(&utf16_offset_map(&text, chrome), 9, 10), 9..10);
    }

    #[test]
    fn offsets_agree_with_the_laid_out_text() {
        let text = paragraph(vec![
            span("Größe ", Some(TextTransform::Uppercase)),
            span("🎉 ok\u{0001}", None),
            span("wort und satz", Some(TextTransform::Capitalize)),
            span("İSTANBUL", Some(TextTransform::Lowercase)),
        ]);
        // the browser `char_offset_to_utf16` reads
        let browser = crate::globals::current_browser();
        let map = utf16_offset_map(&text, browser);

        let mut source = 0;
        let mut chars = 0;
        for span in text.children() {
            for c in span.text.chars() {
                chars += 1;
                source += c.len_utf16();
                // `char_offset_to_utf16` transforms the text before the offset
                assert_eq!(map[source], text.char_offset_to_utf16(chars), "after {c:?}");
            }
        }
        let laid_out: usize = text
            .children()
            .iter()
            .map(|span| span.transform_text(&span.text).encode_utf16().count())
            .sum();
        assert_eq!(map.last(), Some(&laid_out));
    }

    fn laid_out(paragraphs: Vec<Paragraph>) -> TextContent {
        let bounds = skia_safe::Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
        let mut content = TextContent::new(bounds, GrowType::Fixed);
        for paragraph in paragraphs {
            content.add_paragraph(paragraph);
        }
        content.update_layout(bounds);
        content
    }

    #[test]
    fn line_units_skip_blank_lines_and_follow_the_last_one() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        let content = laid_out(vec![
            paragraph(vec![span("a", None)]),
            paragraph(vec![span("", None)]),
            paragraph(vec![span("b", None)]),
        ]);
        let layouts: Vec<_> = content
            .layout
            .paragraphs
            .iter()
            .map(|group| &group[0])
            .collect();

        let lines = non_blank_lines(&layouts);
        assert_eq!(lines, vec![(0, 0..1), (2, 0..1)]);

        let moved = UnitPose {
            dy: 10.0,
            ..UnitPose::REST
        };
        let unit = |index| TextUnit {
            kind: UnitKind::Line,
            index,
            start: 0,
            end: 0,
            pose: moved,
        };
        let pieces = |line_count, units: Vec<TextUnit>| {
            TextUnits { line_count, units }.pieces(
                content.paragraphs(),
                &layouts,
                Browser::Chrome as u8,
            )
        };

        // line 1 is "b", in the third paragraph
        assert_eq!(
            pieces(2, vec![unit(1)]),
            vec![UnitPiece {
                unit: 0,
                paragraph: 2,
                range: 0..1,
                pose: moved
            }]
        );
        // seen as one line, "b" moves with the last line the frontend knew
        assert_eq!(
            pieces(1, vec![unit(0)]),
            vec![
                UnitPiece {
                    unit: 0,
                    paragraph: 0,
                    range: 0..1,
                    pose: moved
                },
                UnitPiece {
                    unit: 0,
                    paragraph: 2,
                    range: 0..1,
                    pose: moved
                },
            ]
        );
    }

    #[test]
    fn char_units_find_their_range_in_the_laid_out_text() {
        let text = vec![paragraph(vec![span(
            "Größe",
            Some(TextTransform::Uppercase),
        )])];
        let unit = TextUnit {
            kind: UnitKind::Char,
            index: 0,
            start: 3,
            end: 4,
            pose: UnitPose::REST,
        };
        let out_of_text = TextUnit { index: 5, ..unit };
        let units = TextUnits {
            line_count: 0,
            units: vec![unit, out_of_text],
        };
        // `ß` is `SS` once laid out
        assert_eq!(
            units.pieces(&text, &[], Browser::Chrome as u8),
            vec![UnitPiece {
                unit: 0,
                paragraph: 0,
                range: 3..5,
                pose: UnitPose::REST
            }]
        );
    }

    #[test]
    fn the_extrect_grows_while_units_are_set() {
        let mut resources = RenderResources::try_new_headless().expect("headless resources");
        let _guard = TestRenderResourcesGuard::install(&mut resources);
        let mut pool = ShapesPool::new();
        let id = Uuid::from_u64_pair(0, 1);
        let mut content = TextContent::new(Rect::from_xywh(0.0, 0.0, 200.0, 40.0), GrowType::Fixed);
        content.add_paragraph(paragraph(vec![span("Hello world", None)]));
        let shape = pool.add_shape(id);
        shape.set_parent(Uuid::nil());
        shape.set_selrect(0.0, 0.0, 200.0, 40.0);
        shape.set_shape_type(crate::shapes::Type::Text(content));

        let at_rest = pool.get(&id).unwrap().extrect(&pool, 1.0);
        let units = TextUnits {
            line_count: 0,
            units: vec![TextUnit {
                kind: UnitKind::Word,
                index: 0,
                start: 6,
                end: 11,
                pose: UnitPose {
                    dy: 20.0,
                    ..UnitPose::REST
                },
            }],
        };
        pool.get_mut(&id).unwrap().set_text_units(Some(units));
        let posed = pool.get(&id).unwrap().extrect(&pool, 1.0);
        assert_eq!(posed, at_rest.with_outset((20.0, 20.0)));

        pool.get_mut(&id).unwrap().set_text_units(None);
        assert_eq!(pool.get(&id).unwrap().extrect(&pool, 1.0), at_rest);
    }

    #[test]
    fn units_reach_as_far_as_they_move_turn_or_grow() {
        let text = Rect::from_xywh(0.0, 0.0, 30.0, 40.0);
        let units = |poses: Vec<UnitPose>| TextUnits {
            line_count: 0,
            units: poses
                .into_iter()
                .map(|pose| TextUnit {
                    kind: UnitKind::Word,
                    index: 0,
                    start: 0,
                    end: 1,
                    pose,
                })
                .collect(),
        };

        assert_eq!(units(vec![]).outset(&text), 0.0);
        let moved = UnitPose {
            dx: -5.0,
            dy: 12.0,
            ..UnitPose::REST
        };
        let shrunk = UnitPose {
            scale_x: 0.5,
            scale_y: 0.2,
            ..moved
        };
        assert_eq!(units(vec![moved, UnitPose::REST]).outset(&text), 12.0);
        assert_eq!(units(vec![shrunk]).outset(&text), 12.0);
        // a box as big as the text (half diagonal 25) twice as big
        let grown = UnitPose {
            scale_x: 2.0,
            ..moved
        };
        assert_eq!(units(vec![grown]).outset(&text), 62.0);
        let turned = UnitPose {
            rotation: 30.0,
            ..UnitPose::REST
        };
        assert_eq!(units(vec![turned]).outset(&text), 25.0);
        // a blur reaches three sigmas out
        let blurred = UnitPose {
            blur: 6.0,
            ..UnitPose::REST
        };
        let reach = units(vec![blurred]).outset(&text);
        assert!((reach - 3.0 * radius_to_sigma(6.0)).abs() < 1e-4, "{reach}");
    }
}
