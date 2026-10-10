#![allow(unused_mut, unused_variables)]
use macros::{wasm_error, ToJs};
use mem::SerializableResult;
use std::mem::size_of;
use std::sync::{Mutex, OnceLock};

use crate::error::{Error, Result};
use crate::render::text_units::outline_layouts;
use crate::shapes::{
    calculate_text_layout_data, page_matrix, stroke_to_path, transform_segments, Path, Segment,
    ToPath, Type,
};
use crate::{mem, with_current_shape, with_current_shape_mut};

const RAW_SEGMENT_DATA_SIZE: usize = size_of::<RawSegmentData>();

pub mod bools;

#[repr(C, u16, align(4))]
#[derive(Debug, PartialEq, Clone, Copy, ToJs)]
#[allow(dead_code)]
enum RawSegmentData {
    MoveTo(RawMoveCommand) = 0x01,
    LineTo(RawLineCommand) = 0x02,
    CurveTo(RawCurveCommand) = 0x03,
    Close = 0x04,
}

impl RawSegmentData {
    pub fn from_segment(segment: Segment) -> Self {
        match segment {
            Segment::MoveTo(to) => RawSegmentData::MoveTo(RawMoveCommand::new(to)),
            Segment::LineTo(to) => RawSegmentData::LineTo(RawLineCommand::new(to)),
            Segment::CurveTo((c1, c2, to)) => {
                RawSegmentData::CurveTo(RawCurveCommand::new(c1, c2, to))
            }
            Segment::Close => RawSegmentData::Close,
        }
    }
}

impl From<[u8; size_of::<RawSegmentData>()]> for RawSegmentData {
    fn from(bytes: [u8; size_of::<RawSegmentData>()]) -> Self {
        unsafe { std::mem::transmute(bytes) }
    }
}

impl TryFrom<&[u8]> for RawSegmentData {
    type Error = Error;
    fn try_from(bytes: &[u8]) -> Result<Self> {
        let data: [u8; RAW_SEGMENT_DATA_SIZE] = bytes
            .get(0..RAW_SEGMENT_DATA_SIZE)
            .and_then(|slice| slice.try_into().ok())
            .ok_or(Error::CriticalError("Invalid path data".to_string()))?;
        Ok(RawSegmentData::from(data))
    }
}

impl From<RawSegmentData> for [u8; RAW_SEGMENT_DATA_SIZE] {
    fn from(value: RawSegmentData) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

impl SerializableResult for RawSegmentData {
    type BytesType = [u8; RAW_SEGMENT_DATA_SIZE];

    // The generic trait doesn't know the size of the array. This is why the
    // clone needs to be here even if it could be generic.
    fn clone_to_slice(&self, slice: &mut [u8]) {
        let bytes = Self::BytesType::from(*self);
        slice.clone_from_slice(&bytes);
    }
}

#[repr(C, align(4))]
#[derive(Debug, PartialEq, Clone, Copy)]
struct RawMoveCommand {
    _padding: [u32; 4],
    x: f32,
    y: f32,
}
impl RawMoveCommand {
    pub fn new((x, y): (f32, f32)) -> Self {
        Self {
            _padding: [0u32; 4],
            x,
            y,
        }
    }
}

#[repr(C, align(4))]
#[derive(Debug, PartialEq, Clone, Copy)]
struct RawLineCommand {
    _padding: [u32; 4],
    x: f32,
    y: f32,
}

impl RawLineCommand {
    pub fn new((x, y): (f32, f32)) -> Self {
        Self {
            _padding: [0u32; 4],
            x,
            y,
        }
    }
}

#[repr(C, align(4))]
#[derive(Debug, PartialEq, Clone, Copy)]
struct RawCurveCommand {
    c1_x: f32,
    c1_y: f32,
    c2_x: f32,
    c2_y: f32,
    x: f32,
    y: f32,
}

impl RawCurveCommand {
    pub fn new((c1_x, c1_y): (f32, f32), (c2_x, c2_y): (f32, f32), (x, y): (f32, f32)) -> Self {
        Self {
            c1_x,
            c1_y,
            c2_x,
            c2_y,
            x,
            y,
        }
    }
}

impl From<RawSegmentData> for Segment {
    fn from(value: RawSegmentData) -> Self {
        match value {
            RawSegmentData::MoveTo(cmd) => Segment::MoveTo((cmd.x, cmd.y)),
            RawSegmentData::LineTo(cmd) => Segment::LineTo((cmd.x, cmd.y)),
            RawSegmentData::CurveTo(cmd) => {
                Segment::CurveTo(((cmd.c1_x, cmd.c1_y), (cmd.c2_x, cmd.c2_y), (cmd.x, cmd.y)))
            }
            RawSegmentData::Close => Segment::Close,
        }
    }
}

impl From<Vec<RawSegmentData>> for Path {
    fn from(value: Vec<RawSegmentData>) -> Self {
        let segments = value.into_iter().map(Segment::from).collect();
        Path::new(segments)
    }
}

static PATH_UPLOAD_BUFFER: OnceLock<Mutex<Vec<u8>>> = OnceLock::new();

fn get_path_upload_buffer() -> &'static Mutex<Vec<u8>> {
    PATH_UPLOAD_BUFFER.get_or_init(|| Mutex::new(Vec::new()))
}

#[no_mangle]
#[wasm_error]
pub extern "C" fn start_shape_path_buffer() -> Result<()> {
    let buffer = get_path_upload_buffer();
    let mut buffer = buffer
        .lock()
        .map_err(|_| Error::CriticalError("Failed to lock path buffer".to_string()))?;
    buffer.clear();
    Ok(())
}

#[no_mangle]
#[wasm_error]
pub extern "C" fn set_shape_path_chunk_buffer() -> Result<()> {
    let bytes = mem::bytes();
    let buffer = get_path_upload_buffer();
    let mut buffer = buffer
        .lock()
        .map_err(|_| Error::CriticalError("Failed to lock path buffer".to_string()))?;
    buffer.extend_from_slice(&bytes);
    mem::free_bytes()?;
    Ok(())
}

#[no_mangle]
#[wasm_error]
pub extern "C" fn set_shape_path_buffer() -> Result<()> {
    let buffer = get_path_upload_buffer();
    let mut buffer = buffer
        .lock()
        .map_err(|_| Error::CriticalError("Failed to lock path buffer".to_string()))?;
    let chunk_size = size_of::<RawSegmentData>();
    if !buffer.len().is_multiple_of(chunk_size) {
        // FIXME
        println!("Warning: buffer length is not a multiple of chunk size!");
    }
    let mut segments = Vec::new();
    for (i, chunk) in buffer.chunks(chunk_size).enumerate() {
        match RawSegmentData::try_from(chunk) {
            Ok(seg) => segments.push(Segment::from(seg)),
            Err(e) => println!("Error at segment {}: {}", i, e),
        }
    }

    with_current_shape_mut!(state, |shape: &mut Shape| {
        shape.set_path_segments(segments);
    });
    buffer.clear();

    Ok(())
}

#[no_mangle]
pub extern "C" fn set_shape_path_content() {
    with_current_shape_mut!(state, |shape: &mut Shape| {
        let bytes = mem::bytes();
        let segments = bytes
            .chunks(size_of::<RawSegmentData>())
            .map(|chunk| RawSegmentData::try_from(chunk).expect("Invalid path data"))
            .map(Segment::from)
            .collect();
        shape.set_path_segments(segments);
    });
}

#[no_mangle]
pub extern "C" fn current_to_path() -> *mut u8 {
    let mut result = Vec::<RawSegmentData>::default();
    with_current_shape!(state, |shape: &Shape| {
        let path = shape.to_path(&state.shapes);
        result = path
            .segments()
            .iter()
            .copied()
            .map(RawSegmentData::from_segment)
            .collect();
    });

    mem::write_vec(result)
}

fn push_word(bytes: &mut Vec<u8>, word: u32) {
    bytes.extend_from_slice(&word.to_le_bytes());
}

/// The outlines of the glyphs of the current text with its units, as it
/// paints at rest (see `render::text_units::outline_layouts`), on the
/// page: what an SVG draws of each unit of a split animation. Nothing for
/// a shape that is no text or has no units. Little endian u32 words, the
/// numbers f32: the piece count, then for each piece its unit (`u32::MAX`
/// for the glyphs of no unit), the x and y of the centre of its box, its
/// run count, then for each run the paragraph and the span that paint it,
/// its segment count and its segments (as `current_to_path` writes them).
#[no_mangle]
pub extern "C" fn get_text_unit_outlines() -> *mut u8 {
    let mut bytes: Vec<u8> = Vec::new();
    with_current_shape!(state, |shape: &Shape| {
        if let (Type::Text(stored), Some(units)) = (&shape.shape_type, &shape.text_units) {
            let content = stored.paint_content_for_selrect(shape.selrect());
            let mut groups = content.paragraph_builder_group_from_text(None);
            let sizes: Vec<usize> = groups.iter().map(Vec::len).collect();
            let mut layout = calculate_text_layout_data(shape, stored, &mut groups, true);
            let outlines = outline_layouts(stored, &mut layout.paragraphs, &sizes, units);
            let matrix = page_matrix(shape);

            push_word(&mut bytes, outlines.len() as u32);
            for outline in &outlines {
                let center = matrix.map_point(outline.center);
                push_word(
                    &mut bytes,
                    outline.unit.map_or(u32::MAX, |unit| unit as u32),
                );
                push_word(&mut bytes, center.x.to_bits());
                push_word(&mut bytes, center.y.to_bits());
                push_word(&mut bytes, outline.runs.len() as u32);
                for (paragraph, span, path) in &outline.runs {
                    let segments = transform_segments(
                        Path::from_skia_path(path.clone()).segments().clone(),
                        shape,
                    );
                    push_word(&mut bytes, *paragraph);
                    push_word(&mut bytes, *span);
                    push_word(&mut bytes, segments.len() as u32);
                    for segment in segments {
                        let raw: [u8; RAW_SEGMENT_DATA_SIZE] =
                            RawSegmentData::from_segment(segment).into();
                        bytes.extend_from_slice(&raw);
                    }
                }
            }
        }
    });
    if bytes.is_empty() {
        push_word(&mut bytes, 0);
    }
    mem::write_bytes(bytes)
}

/// Converts a shape's stroke (at the given index) into a filled path.
///
/// This uses Skia's `fill_path_with_paint` to convert the stroke outline
/// into a filled path, properly handling inner/outer/center alignment
/// via boolean path operations.
#[no_mangle]
pub extern "C" fn convert_stroke_to_path(stroke_index: i32) -> *mut u8 {
    let mut result = Vec::<RawSegmentData>::default();
    let mut even_odd = false;
    with_current_shape!(state, |shape: &Shape| {
        let idx = stroke_index as usize;
        if let Some(stroke) = shape.strokes.get(idx) {
            let shape_path = shape.to_path(&state.shapes);
            let path_transform = shape.to_path_transform();

            if let Some(path) = stroke_to_path(
                stroke,
                &shape_path,
                path_transform.as_ref(),
                &shape.selrect,
                shape.svg_attrs.as_ref(),
                false,
            ) {
                even_odd = path.is_even_odd();
                result = path
                    .segments()
                    .iter()
                    .copied()
                    .map(RawSegmentData::from_segment)
                    .collect();
            }
        }
    });

    mem::write_vec_with_header(even_odd as u32, result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_command_deserialization() {
        let mut bytes = [0x00; size_of::<RawSegmentData>()];
        bytes[0..2].copy_from_slice(&0x01_u16.to_le_bytes());
        bytes[20..24].copy_from_slice(&1.0_f32.to_le_bytes());
        bytes[24..28].copy_from_slice(&2.0_f32.to_le_bytes());

        let raw_segment = RawSegmentData::try_from(&bytes[..]).unwrap();
        let segment = Segment::from(raw_segment);

        assert_eq!(segment, Segment::MoveTo((1.0, 2.0)));
    }

    #[test]
    fn test_line_command_deserialization() {
        let mut bytes = [0x00; size_of::<RawSegmentData>()];
        bytes[0..2].copy_from_slice(&0x02_u16.to_le_bytes());
        bytes[20..24].copy_from_slice(&3.0_f32.to_le_bytes());
        bytes[24..28].copy_from_slice(&4.0_f32.to_le_bytes());

        let raw_segment = RawSegmentData::try_from(&bytes[..]).unwrap();
        let segment = Segment::from(raw_segment);

        assert_eq!(segment, Segment::LineTo((3.0, 4.0)));
    }

    #[test]
    fn test_curve_command_deserialization() {
        let mut bytes = [0x00; size_of::<RawSegmentData>()];
        bytes[0..2].copy_from_slice(&0x03_u16.to_le_bytes());
        bytes[4..8].copy_from_slice(&1.0_f32.to_le_bytes());
        bytes[8..12].copy_from_slice(&2.0_f32.to_le_bytes());
        bytes[12..16].copy_from_slice(&3.0_f32.to_le_bytes());
        bytes[16..20].copy_from_slice(&4.0_f32.to_le_bytes());
        bytes[20..24].copy_from_slice(&5.0_f32.to_le_bytes());
        bytes[24..28].copy_from_slice(&6.0_f32.to_le_bytes());

        let raw_segment = RawSegmentData::try_from(&bytes[..]).unwrap();
        let segment = Segment::from(raw_segment);

        assert_eq!(
            segment,
            Segment::CurveTo(((1.0, 2.0), (3.0, 4.0), (5.0, 6.0)))
        );
    }

    #[test]
    fn test_close_command_deserialization() {
        let mut bytes = [0x00; size_of::<RawSegmentData>()];
        bytes[0..2].copy_from_slice(&0x04_u16.to_le_bytes());

        let raw_segment = RawSegmentData::try_from(&bytes[..]).unwrap();
        let segment = Segment::from(raw_segment);

        assert_eq!(segment, Segment::Close);
    }
}
