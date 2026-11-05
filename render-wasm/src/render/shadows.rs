use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use super::filters;
use super::{RenderState, SurfaceId};
use crate::error::Result;
use crate::get_resources;
use crate::render::strokes;
use crate::render::text;
use crate::shapes::radius_to_sigma;
use crate::shapes::{ParagraphBuilderGroup, Shadow, Shape, Stroke, StrokeKind, TextContent, Type};
use crate::uuid::Uuid;
use skia_safe::{self as skia, canvas::SaveLayerRec, Paint, Path, Rect};

// ---------------------------------------------------------------------------
// Direct frame drop shadows (fast inline blur + filter-surface cache fallback)
// ---------------------------------------------------------------------------

pub(crate) struct DropShadowFilterCache {
    entries: HashMap<DropShadowFilterCacheKey, CachedDropShadowFilter>,
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub(crate) struct DropShadowFilterCacheKey {
    shape_id: Uuid,
    blur_bits: u32,
    spread_bits: u32,
    offset_x_bits: u32,
    offset_y_bits: u32,
    scale_bits: u32,
    transform_a_bits: u32,
    transform_b_bits: u32,
    transform_c_bits: u32,
    transform_d_bits: u32,
    transform_e_bits: u32,
    transform_f_bits: u32,
    layer_blur_bits: u32,
}

pub(crate) struct CachedDropShadowFilter {
    bounds: Rect,
    filter_scale: f32,
    image: skia::Image,
}

impl CachedDropShadowFilter {
    pub(crate) fn new(bounds: Rect, filter_scale: f32, surface: &mut skia::Surface) -> Self {
        // The reusable filter surface can be viewport-sized. Retaining all of it
        // for every shadow keeps large, mostly transparent textures alive.
        // Filter drawing translates bounds to the origin before applying scale.
        let width = (bounds.width() * filter_scale)
            .ceil()
            .clamp(1.0, surface.width() as f32) as i32;
        let height = (bounds.height() * filter_scale)
            .ceil()
            .clamp(1.0, surface.height() as f32) as i32;
        let image = surface
            .image_snapshot_with_bounds(skia::IRect::from_wh(width, height))
            .expect("shadow filter bounds must intersect its surface");
        Self {
            bounds,
            filter_scale,
            image,
        }
    }
}

impl DropShadowFilterCacheKey {
    pub(crate) fn for_shape(
        shape_id: Uuid,
        shadow: &Shadow,
        scale: f32,
        transform: &skia::Matrix,
        layer_blur: f32,
    ) -> Self {
        Self::new(shape_id, shadow, scale, transform, layer_blur)
    }

    fn new(
        shape_id: Uuid,
        shadow: &Shadow,
        scale: f32,
        transform: &skia::Matrix,
        layer_blur: f32,
    ) -> Self {
        Self {
            shape_id,
            blur_bits: shadow.blur.to_bits(),
            spread_bits: shadow.spread.to_bits(),
            offset_x_bits: shadow.offset.0.to_bits(),
            offset_y_bits: shadow.offset.1.to_bits(),
            scale_bits: scale.to_bits(),
            transform_a_bits: transform[0].to_bits(),
            transform_b_bits: transform[1].to_bits(),
            transform_c_bits: transform[2].to_bits(),
            transform_d_bits: transform[3].to_bits(),
            transform_e_bits: transform[4].to_bits(),
            transform_f_bits: transform[5].to_bits(),
            layer_blur_bits: layer_blur.to_bits(),
        }
    }
}

impl DropShadowFilterCache {
    pub fn new() -> Self {
        Self {
            entries: HashMap::default(),
        }
    }

    pub fn clear(&mut self) {
        self.entries.clear();
    }

    pub(crate) fn lookup(&self, key: &DropShadowFilterCacheKey) -> Option<&CachedDropShadowFilter> {
        self.entries.get(key)
    }

    pub(crate) fn store(&mut self, key: DropShadowFilterCacheKey, value: CachedDropShadowFilter) {
        self.entries.insert(key, value);
    }
}

// ---------------------------------------------------------------------------
// Drop shadows kept across renders, for shapes that only move
// ---------------------------------------------------------------------------

/// How many drop shadows `MovingShadowCache` keeps, and how many pixels of
/// them, before it starts again.
const MOVING_SHADOWS_MAX: usize = 512;
const MOVING_SHADOWS_MAX_PIXELS: i64 = 16 * 1024 * 1024;

/// What the drop shadow `shadow` of a shape blurs the same for at a
/// `scale` and under a layer blur: all that shapes its silhouette but where
/// the shape is (see `shadow_moves_with`), so a shape that only moved, a
/// container scrolling or a move keyframe, finds it again.
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct MovingShadowKey {
    shape_id: Uuid,
    /// its blur, spread and offset
    shadow: u64,
    /// the size, turn and corners of the shape and whether it has fills
    silhouette: u64,
    scale_bits: u32,
    layer_blur_bits: u32,
}

fn hash_bits(values: &[f32]) -> u64 {
    let mut hasher = DefaultHasher::new();
    values.iter().for_each(|v| v.to_bits().hash(&mut hasher));
    hasher.finish()
}

impl MovingShadowKey {
    pub(crate) fn for_shape(shape: &Shape, shadow: &Shadow, scale: f32, layer_blur: f32) -> Self {
        let selrect = shape.selrect();
        let transform = shape.transform;
        // the turn, scale and skew of the shape, not its move
        let mut geometry = vec![
            selrect.width(),
            selrect.height(),
            transform[0],
            transform[1],
            transform[3],
            transform[4],
            if shape.has_fills() { 1.0 } else { 0.0 },
        ];
        if let Some(corners) = shape.shape_type.corners() {
            corners
                .iter()
                .for_each(|corner| geometry.extend([corner.x, corner.y]));
        }
        let mut hasher = DefaultHasher::new();
        std::mem::discriminant(&shape.shape_type).hash(&mut hasher);
        hash_bits(&geometry).hash(&mut hasher);
        Self {
            shape_id: shape.id,
            shadow: hash_bits(&[shadow.blur, shadow.spread, shadow.offset.0, shadow.offset.1]),
            silhouette: hasher.finish(),
            scale_bits: scale.to_bits(),
            layer_blur_bits: layer_blur.to_bits(),
        }
    }

    /// The place of the shadow in `MovingShadowCache`: one for each shape
    /// and shadow, whatever its shape and scale come to.
    fn slot(&self) -> (Uuid, u64) {
        (self.shape_id, self.shadow)
    }
}

/// Whether the silhouette the drop shadows of `shape` blur goes with it as
/// it moves, all of it in `MovingShadowKey`: a rectangle or an ellipse
/// without strokes (the rect of a board draws through
/// `render_direct_frame_drop_shadow`).
pub(crate) fn shadow_moves_with(shape: &Shape) -> bool {
    matches!(shape.shape_type, Type::Rect(_) | Type::Circle)
        && shape.visible_strokes().next().is_none()
}

/// Where `shape` is, for how far it moved since its shadow was kept.
pub(crate) fn shadow_anchor(shape: &Shape) -> (f32, f32) {
    let selrect = shape.selrect();
    (selrect.left, selrect.top)
}

/// A drop shadow kept across renders: its blur, and where its shape was.
pub(crate) struct MovingShadow {
    key: MovingShadowKey,
    cached: CachedDropShadowFilter,
    anchor: (f32, f32),
}

impl MovingShadow {
    fn pixels(&self) -> i64 {
        self.cached.image.width() as i64 * self.cached.image.height() as i64
    }
}

/// The drop shadows of the shapes that only move, kept across renders
/// (unlike `DropShadowFilterCache`, which a render starts empty): one for
/// each shape and shadow, the last it blurred, so a shape that changes
/// takes the place it had. It starts again once it holds too many.
pub(crate) struct MovingShadowCache {
    entries: HashMap<(Uuid, u64), MovingShadow>,
    pixels: i64,
}

impl MovingShadowCache {
    pub fn new() -> Self {
        Self {
            entries: HashMap::default(),
            pixels: 0,
        }
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.pixels = 0;
    }

    pub(crate) fn lookup(&self, key: &MovingShadowKey) -> Option<&MovingShadow> {
        self.entries
            .get(&key.slot())
            .filter(|moved| moved.key == *key)
    }

    /// Keeps `cached`, blurred for `key` with its shape at `anchor`.
    pub(crate) fn store(
        &mut self,
        key: MovingShadowKey,
        cached: CachedDropShadowFilter,
        anchor: (f32, f32),
    ) -> &MovingShadow {
        let moved = MovingShadow {
            key,
            cached,
            anchor,
        };
        let pixels = moved.pixels();
        if !self.entries.contains_key(&key.slot())
            && (self.entries.len() >= MOVING_SHADOWS_MAX
                || self.pixels + pixels > MOVING_SHADOWS_MAX_PIXELS)
        {
            self.clear();
        }
        self.pixels += pixels;
        if let Some(old) = self.entries.insert(key.slot(), moved) {
            self.pixels -= old.pixels();
        }
        &self.entries[&key.slot()]
    }
}

/// `moved` blitted where its shape is now, `anchor` (see `shadow_anchor`).
pub(crate) fn blit_moved_drop_shadow(
    surfaces: &mut super::Surfaces,
    moved: &MovingShadow,
    layer_blur: Option<skia::ImageFilter>,
    anchor: (f32, f32),
) {
    let canvas = surfaces.canvas(SurfaceId::DropShadows);
    canvas.save();
    canvas.translate((anchor.0 - moved.anchor.0, anchor.1 - moved.anchor.1));
    blit_cached_drop_shadow_filter(surfaces, &moved.cached, layer_blur);
    surfaces.canvas(SurfaceId::DropShadows).restore();
}

/// Renders a direct frame drop shadow: inline blur on the tile when the kernel
/// fits the margin, otherwise a cached filter-surface pass shared across tiles.
///
/// Does not apply the caller's clip stack; clip is applied when compositing
/// `DropShadows` onto the target surface.
pub(crate) fn render_direct_frame_drop_shadow(
    state: &mut RenderState,
    frame: &Shape,
    shape_bounds: &Rect,
    shadow: &Shadow,
    scale: f32,
) -> Result<()> {
    if render_moving_frame_shadow(state, frame, shape_bounds, shadow, scale)? {
        return Ok(());
    }
    let margin = state.surfaces.margins().width as f32;
    let sigma_device = radius_to_sigma(shadow.blur) * scale;
    if sigma_device <= margin / 3.0 {
        render_inline_frame_shadow(state, frame, shadow, scale)
    } else {
        render_cached_filter_frame_shadow(state, frame, shape_bounds, shadow, scale)
    }
}

fn frame_shadow_antialias(state: &RenderState, frame: &Shape, scale: f32) -> bool {
    !state.options.is_fast_mode()
        && frame.should_use_antialias(scale, state.options.antialias_threshold)
}

fn spread_outset(spread: f32) -> Option<f32> {
    Some(spread).filter(|&s| s > 0.0)
}

fn spread_inset(spread: f32) -> Option<f32> {
    Some(-spread).filter(|&s| s > 0.0)
}

fn blur_layer_paint(blur: f32, sigma_scale: f32) -> skia::Paint {
    let mut paint = skia::Paint::default();
    if blur > 0.0 {
        let sigma = radius_to_sigma(blur) * sigma_scale;
        paint.set_image_filter(skia::image_filters::blur((sigma, sigma), None, None, None));
    }
    paint.set_blend_mode(skia::BlendMode::SrcOver);
    paint
}

fn draw_frame_shadow_rect(
    surfaces: &mut super::Surfaces,
    surface_id: SurfaceId,
    frame: &Shape,
    shadow: &Shadow,
    antialias: bool,
) {
    let mut fill_paint = skia::Paint::default();
    fill_paint.set_color(skia::Color::BLACK);
    fill_paint.set_anti_alias(antialias);
    surfaces.draw_rect_to(
        surface_id,
        frame,
        &fill_paint,
        spread_outset(shadow.spread),
        spread_inset(shadow.spread),
    );
}

fn shadow_filter_bounds(
    shadow: &Shadow,
    shape_bounds: &Rect,
    world_offset: (f32, f32),
) -> Option<Rect> {
    let mut shadow_cull = *shadow;
    shadow_cull.color = skia::Color::BLACK;
    shadow_cull.offset = (0.0, 0.0);
    let drop_filter = shadow_cull.get_drop_shadow_filter()?;
    let mut bounds = drop_filter.compute_fast_bounds(*shape_bounds);
    bounds.offset(world_offset);
    Some(bounds)
}

/// Local draw matrix for frame shadow geometry: centered shape transform plus
/// shadow offset in local space (matches `render_shape` with `Some(offset)`).
fn frame_shadow_draw_matrix(frame: &Shape, shadow: &Shadow) -> skia::Matrix {
    let mut matrix = frame.centered_transform();
    matrix.pre_translate((shadow.offset.0, shadow.offset.1));
    matrix
}

/// Shadow offset mapped to world space (for bounds culling and cache blit).
fn shadow_world_offset(frame: &Shape, shadow: &Shadow) -> (f32, f32) {
    let mapped = frame
        .centered_transform()
        .map_vector((shadow.offset.0, shadow.offset.1));
    (mapped.x, mapped.y)
}

/// When bounds fit in the filter surface, skip blur downscale to avoid banding
/// at high zoom. The tile cache makes a single full-res pass affordable.
fn blur_downscale_for_frame_shadow(
    blur: f32,
    bounds: Rect,
    filter_width: i32,
    filter_height: i32,
    threshold: f32,
) -> f32 {
    let bounds_w = bounds.width().ceil().max(1.0) as i32;
    let bounds_h = bounds.height().ceil().max(1.0) as i32;
    if bounds_w <= filter_width && bounds_h <= filter_height {
        return 1.0;
    }
    if blur > threshold {
        (threshold / blur).max(1.0 / threshold)
    } else {
        1.0
    }
}

pub(crate) fn blit_cached_drop_shadow_filter(
    surfaces: &mut super::Surfaces,
    cached: &CachedDropShadowFilter,
    layer_blur: Option<skia::ImageFilter>,
) {
    let sampling = get_resources().sampling_options;
    let mut paint = skia::Paint::default();
    if let Some(filter) = layer_blur {
        paint.set_image_filter(filter);
    }
    let drop_canvas = surfaces.canvas(SurfaceId::DropShadows);
    let dst = skia::Rect::from_wh(cached.image.width() as f32, cached.image.height() as f32);

    drop_canvas.save();
    drop_canvas.save();
    if cached.filter_scale != 1.0 {
        drop_canvas.scale((1.0 / cached.filter_scale, 1.0 / cached.filter_scale));
        drop_canvas.translate((
            cached.bounds.left * cached.filter_scale,
            cached.bounds.top * cached.filter_scale,
        ));
    } else {
        drop_canvas.translate((cached.bounds.left, cached.bounds.top));
    }
    drop_canvas.draw_image_rect_with_sampling_options(&cached.image, None, dst, sampling, &paint);
    drop_canvas.restore();
    drop_canvas.restore();
}

fn render_inline_frame_shadow(
    state: &mut RenderState,
    frame: &Shape,
    shadow: &Shadow,
    scale: f32,
) -> Result<()> {
    let antialias = frame_shadow_antialias(state, frame, scale);
    let layer_paint = blur_layer_paint(shadow.blur, 1.0);
    let draw_matrix = frame_shadow_draw_matrix(frame, shadow);

    {
        let drop_canvas = state.surfaces.canvas(SurfaceId::DropShadows);
        drop_canvas.save();
        drop_canvas.concat(&draw_matrix);
        let layer_rec = skia::canvas::SaveLayerRec::default().paint(&layer_paint);
        drop_canvas.save_layer(&layer_rec);
    }

    draw_frame_shadow_rect(
        &mut state.surfaces,
        SurfaceId::DropShadows,
        frame,
        shadow,
        antialias,
    );

    {
        let drop_canvas = state.surfaces.canvas(SurfaceId::DropShadows);
        drop_canvas.restore();
        drop_canvas.restore();
    }

    Ok(())
}

/// The drop shadow of the rect of `frame` from the shadows kept across
/// renders (see `MovingShadowCache`), blurred once at the scale of the
/// render and moved with the frame since. False when it does not fit the
/// filter surface so: the other ways draw it.
fn render_moving_frame_shadow(
    state: &mut RenderState,
    frame: &Shape,
    shape_bounds: &Rect,
    shadow: &Shadow,
    scale: f32,
) -> Result<bool> {
    let key = MovingShadowKey::for_shape(frame, shadow, scale, 0.0);
    let anchor = shadow_anchor(frame);
    if let Some(moved) = state.moving_shadow_cache.lookup(&key) {
        blit_moved_drop_shadow(&mut state.surfaces, moved, None, anchor);
        return Ok(true);
    }

    let world_offset = shadow_world_offset(frame, shadow);
    let Some(bounds) = shadow_filter_bounds(shadow, shape_bounds, world_offset) else {
        return Ok(true);
    };
    let antialias = frame_shadow_antialias(state, frame, scale);
    let layer_paint = blur_layer_paint(shadow.blur, 1.0);
    let layer_rec = skia::canvas::SaveLayerRec::default().paint(&layer_paint);
    let shadow_draw_matrix = frame_shadow_draw_matrix(frame, shadow);
    let filter_result =
        filters::render_into_filter_surface_at(state, bounds, scale, |state, temp_surface| {
            {
                let canvas = state.surfaces.canvas(temp_surface);
                canvas.save();
                canvas.concat(&shadow_draw_matrix);
                canvas.save_layer(&layer_rec);
            }
            draw_frame_shadow_rect(&mut state.surfaces, temp_surface, frame, shadow, antialias);
            {
                let canvas = state.surfaces.canvas(temp_surface);
                canvas.restore();
                canvas.restore();
            }
            Ok(())
        })?;

    let Some((mut surface, filter_scale)) = filter_result else {
        return Ok(false);
    };
    let cached = CachedDropShadowFilter::new(bounds, filter_scale, &mut surface);
    let moved = state.moving_shadow_cache.store(key, cached, anchor);
    blit_moved_drop_shadow(&mut state.surfaces, moved, None, anchor);
    Ok(true)
}

fn render_cached_filter_frame_shadow(
    state: &mut RenderState,
    frame: &Shape,
    shape_bounds: &Rect,
    shadow: &Shadow,
    scale: f32,
) -> Result<()> {
    let draw_matrix = frame.centered_transform();
    let key = DropShadowFilterCacheKey::for_shape(frame.id, shadow, scale, &draw_matrix, 0.0);
    if let Some(cached) = state.drop_shadow_filter_cache.lookup(&key) {
        blit_cached_drop_shadow_filter(&mut state.surfaces, cached, None);
        return Ok(());
    }

    let world_offset = shadow_world_offset(frame, shadow);
    let Some(bounds) = shadow_filter_bounds(shadow, shape_bounds, world_offset) else {
        return Ok(());
    };

    let antialias = frame_shadow_antialias(state, frame, scale);
    let (filter_w, filter_h) = state.surfaces.filter_size();
    let blur_downscale = blur_downscale_for_frame_shadow(
        shadow.blur,
        bounds,
        filter_w,
        filter_h,
        state.options.blur_downscale_threshold,
    );
    let layer_paint = blur_layer_paint(shadow.blur, blur_downscale);
    let layer_rec = skia::canvas::SaveLayerRec::default().paint(&layer_paint);

    let shadow_draw_matrix = frame_shadow_draw_matrix(frame, shadow);
    let filter_result = filters::render_into_filter_surface(
        state,
        bounds,
        blur_downscale,
        |state, temp_surface| {
            {
                let canvas = state.surfaces.canvas(temp_surface);
                canvas.save();
                canvas.concat(&shadow_draw_matrix);
                canvas.save_layer(&layer_rec);
            }
            draw_frame_shadow_rect(&mut state.surfaces, temp_surface, frame, shadow, antialias);
            {
                let canvas = state.surfaces.canvas(temp_surface);
                canvas.restore();
                canvas.restore();
            }
            Ok(())
        },
    )?;

    if let Some((mut surface, filter_scale)) = filter_result {
        let cached = CachedDropShadowFilter::new(bounds, filter_scale, &mut surface);
        blit_cached_drop_shadow_filter(&mut state.surfaces, &cached, None);
        state.drop_shadow_filter_cache.store(key, cached);
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Inner / text shadows
// ---------------------------------------------------------------------------

pub fn render_fill_inner_shadows(
    render_state: &mut RenderState,
    shape: &Shape,
    antialias: bool,
    surface_id: SurfaceId,
) {
    // A masked group's inner shadows are applied to the masked result by its
    // layer filter (see `Shape::masked_group_layer_filter`).
    if !shape.has_fills() || shape.is_masked_group() || render_state.should_skip_drop_shadows() {
        return;
    }
    let scale = render_state.get_scale();
    let recursive = shape.is_recursive();
    for shadow in shape.inner_shadows_visible() {
        if !shadow.is_perceptible_at_scale_for(scale, recursive) {
            continue;
        }
        render_fill_inner_shadow(render_state, shape, shadow, antialias, surface_id);
    }
}

fn render_fill_inner_shadow(
    render_state: &mut RenderState,
    shape: &Shape,
    shadow: &Shadow,
    antialias: bool,
    surface_id: SurfaceId,
) {
    let paint = &shadow.get_inner_shadow_paint(antialias, shape.image_filter(1.).as_ref());
    render_shadow_paint(render_state, shape, paint, surface_id);
}

pub fn render_stroke_inner_shadows(
    render_state: &mut RenderState,
    shape: &Shape,
    stroke: &Stroke,
    antialias: bool,
    surface_id: SurfaceId,
) -> Result<()> {
    if shape.has_fills() || render_state.should_skip_drop_shadows() {
        return Ok(());
    }
    let scale = render_state.get_scale();
    let recursive = shape.is_recursive();
    for shadow in shape.inner_shadows_visible() {
        if !shadow.is_perceptible_at_scale_for(scale, recursive) {
            continue;
        }
        let filter = shadow.get_inner_shadow_filter();
        strokes::render_single(
            render_state,
            shape,
            stroke,
            Some(surface_id),
            filter.as_ref(),
            antialias,
            None, // Inner shadows don't use spread
        )?;
    }
    Ok(())
}

// Render text paths (unused)
#[allow(dead_code)]
pub fn render_text_path_stroke_drop_shadows(
    render_state: &mut RenderState,
    shape: &Shape,
    paths: &Vec<(Path, Paint)>,
    stroke: &Stroke,
    antialias: bool,
) {
    for shadow in shape.drop_shadows_visible() {
        let stroke_shadow = shadow.get_drop_shadow_filter();
        strokes::render_text_paths(
            render_state,
            shape,
            stroke,
            paths,
            Some(SurfaceId::DropShadows),
            stroke_shadow.as_ref(),
            antialias,
        );
    }
}

// Render text paths (unused)
#[allow(dead_code)]
pub fn render_text_path_stroke_inner_shadows(
    render_state: &mut RenderState,
    shape: &Shape,
    paths: &Vec<(Path, Paint)>,
    stroke: &Stroke,
    antialias: bool,
) {
    for shadow in shape.inner_shadows_visible() {
        let stroke_shadow = shadow.get_inner_shadow_filter();
        strokes::render_text_paths(
            render_state,
            shape,
            stroke,
            paths,
            Some(SurfaceId::InnerShadows),
            stroke_shadow.as_ref(),
            antialias,
        );
    }
}

fn render_shadow_paint(
    render_state: &mut RenderState,
    shape: &Shape,
    paint: &Paint,
    surface_id: SurfaceId,
) {
    match &shape.shape_type {
        Type::Rect(_) | Type::Frame(_) => {
            render_state
                .surfaces
                .draw_rect_to(surface_id, shape, paint, None, None);
        }
        Type::Circle => {
            render_state
                .surfaces
                .draw_circle_to(surface_id, shape, paint, None, None);
        }
        Type::Path(_) | Type::Bool(_) => {
            render_state
                .surfaces
                .draw_path_to(surface_id, shape, paint, None, None);
        }
        _ => {}
    }
}

#[allow(clippy::too_many_arguments)]
pub fn render_text_shadows(
    render_state: &mut RenderState,
    shape: &Shape,
    paragraphs: &mut [ParagraphBuilderGroup],
    stroke_paragraphs_group: &mut [Vec<ParagraphBuilderGroup>],
    surface_id: Option<SurfaceId>,
    shadows: &[Paint],
    blur_filter: &Option<skia_safe::ImageFilter>,
    stroke_kinds: &[StrokeKind],
    text_content: &TextContent,
) -> Result<()> {
    if stroke_paragraphs_group.is_empty() {
        return Ok(());
    }

    let canvas = render_state
        .surfaces
        .canvas_and_mark_dirty(surface_id.unwrap_or(SurfaceId::TextDropShadows));

    for shadow in shadows {
        let shadow_layer = SaveLayerRec::default().paint(shadow);
        canvas.save_layer(&shadow_layer);

        text::render(
            None,
            Some(canvas),
            shape,
            paragraphs,
            surface_id,
            None,
            blur_filter.as_ref(),
            None,
            None,
        )?;

        for (i, stroke_paragraphs) in stroke_paragraphs_group.iter_mut().enumerate() {
            if i < stroke_kinds.len() && stroke_kinds[i] == StrokeKind::Inner {
                let mut fill_builders = text_content.paragraph_builder_group_from_text(Some(true));
                text::render_inner_stroke(
                    None,
                    Some(canvas),
                    shape,
                    stroke_paragraphs,
                    &mut fill_builders,
                    surface_id,
                    blur_filter.as_ref(),
                    0.0,
                    None,
                )?;
            } else if i < stroke_kinds.len() && stroke_kinds[i] == StrokeKind::Outer {
                text::render_outer_stroke(
                    None,
                    Some(canvas),
                    shape,
                    stroke_paragraphs,
                    surface_id,
                    blur_filter.as_ref(),
                    0.0,
                    None,
                )?;
            } else {
                text::render(
                    None,
                    Some(canvas),
                    shape,
                    stroke_paragraphs,
                    surface_id,
                    None,
                    blur_filter.as_ref(),
                    None,
                    None,
                )?;
            }
        }

        canvas.restore();
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cached_shadow_retains_only_its_scaled_bounds() {
        for (bounds, scale, expected) in [
            (Rect::from_xywh(100.0, 200.0, 50.5, 40.5), 1.0, (51, 41)),
            (Rect::from_xywh(100.0, 200.0, 50.5, 40.5), 0.5, (26, 21)),
            (Rect::from_xywh(0.0, 0.0, 2000.0, 1500.0), 1.0, (1024, 768)),
        ] {
            let mut surface = skia::surfaces::raster_n32_premul((1024, 768)).unwrap();
            let cached = CachedDropShadowFilter::new(bounds, scale, &mut surface);

            assert_eq!(cached.image.dimensions(), skia::ISize::from(expected));
        }
    }

    #[test]
    fn cached_shadow_survives_filter_surface_reuse() {
        let mut surface = skia::surfaces::raster_n32_premul((1024, 768)).unwrap();
        surface.canvas().clear(skia::Color::TRANSPARENT);
        let mut paint = Paint::default();
        paint.set_color(skia::Color::RED);
        surface
            .canvas()
            .draw_rect(Rect::from_xywh(4.0, 5.0, 10.0, 10.0), &paint);
        let cached = CachedDropShadowFilter::new(
            Rect::from_xywh(100.0, 200.0, 50.5, 40.5),
            1.0,
            &mut surface,
        );

        surface.canvas().clear(skia::Color::BLUE);

        let pixels = cached.image.peek_pixels().unwrap();
        assert_eq!(pixels.get_color((5, 6)), skia::Color::RED);
        assert_eq!(pixels.get_color((0, 0)), skia::Color::TRANSPARENT);
    }

    use crate::shapes::{Rect as RectType, ShadowStyle, StrokeStyle};

    fn card(x: f32, y: f32) -> Shape {
        let mut shape = Shape::new(Uuid::nil());
        shape.set_shape_type(Type::Rect(RectType::default()));
        shape.set_selrect(x, y, x + 300.0, y + 212.0);
        shape.set_corners((28.0, 28.0, 28.0, 28.0));
        shape
    }

    fn drop(blur: f32) -> Shadow {
        Shadow::new(
            skia::Color::BLACK,
            blur,
            0.0,
            (0.0, 10.0),
            ShadowStyle::Drop,
            false,
        )
    }

    fn kept(width: i32, height: i32) -> CachedDropShadowFilter {
        let mut surface = skia::surfaces::raster_n32_premul((width, height)).unwrap();
        CachedDropShadowFilter::new(
            Rect::from_xywh(0.0, 0.0, width as f32, height as f32),
            1.0,
            &mut surface,
        )
    }

    #[test]
    fn a_shadow_keeps_its_key_as_its_shape_moves() {
        let key = MovingShadowKey::for_shape(&card(24.0, 254.0), &drop(22.0), 0.96, 0.0);
        assert_eq!(
            key,
            MovingShadowKey::for_shape(&card(-209.7, 254.0), &drop(22.0), 0.96, 0.0),
            "moved"
        );

        let mut wider = card(24.0, 254.0);
        wider.set_selrect(24.0, 254.0, 340.0, 466.0);
        let mut round = card(24.0, 254.0);
        round.set_corners((12.0, 12.0, 12.0, 12.0));
        let mut turned = card(24.0, 254.0);
        turned.apply_transform(&skia::Matrix::rotate_deg(30.0));
        for (other, what) in [
            (
                MovingShadowKey::for_shape(&wider, &drop(22.0), 0.96, 0.0),
                "resized",
            ),
            (
                MovingShadowKey::for_shape(&round, &drop(22.0), 0.96, 0.0),
                "rounder",
            ),
            (
                MovingShadowKey::for_shape(&turned, &drop(22.0), 0.96, 0.0),
                "turned",
            ),
            (
                MovingShadowKey::for_shape(&card(24.0, 254.0), &drop(30.0), 0.96, 0.0),
                "more blur",
            ),
            (
                MovingShadowKey::for_shape(&card(24.0, 254.0), &drop(22.0), 2.0, 0.0),
                "zoomed",
            ),
            (
                MovingShadowKey::for_shape(&card(24.0, 254.0), &drop(22.0), 0.96, 4.0),
                "under a layer blur",
            ),
        ] {
            assert_ne!(key, other, "{what}");
        }
        assert_eq!(
            key.slot(),
            MovingShadowKey::for_shape(&wider, &drop(22.0), 2.0, 0.0).slot(),
            "one place for the shape and its shadow"
        );
    }

    #[test]
    fn a_rect_or_an_ellipse_without_strokes_moves_its_shadow() {
        let mut ellipse = card(0.0, 0.0);
        ellipse.set_shape_type(Type::Circle);
        assert!(shadow_moves_with(&card(0.0, 0.0)));
        assert!(shadow_moves_with(&ellipse));

        let mut stroked = card(0.0, 0.0);
        stroked.add_stroke(Stroke::new_center_stroke(
            2.0,
            StrokeStyle::Solid,
            None,
            None,
            None,
            None,
        ));
        assert!(!shadow_moves_with(&stroked), "its strokes shape the shadow");
        let mut group = card(0.0, 0.0);
        group.set_shape_type(Type::Group(Default::default()));
        assert!(
            !shadow_moves_with(&group),
            "what it holds shapes the shadow"
        );
    }

    #[test]
    fn the_moving_shadows_keep_one_place_a_shape() {
        let mut cache = MovingShadowCache::new();
        let key = MovingShadowKey::for_shape(&card(24.0, 254.0), &drop(22.0), 0.96, 0.0);
        cache.store(key, kept(40, 30), (24.0, 254.0));
        assert!(cache.lookup(&key).is_some());
        assert_eq!(cache.lookup(&key).unwrap().anchor, (24.0, 254.0));

        let mut wider = card(24.0, 254.0);
        wider.set_selrect(24.0, 254.0, 340.0, 466.0);
        let resized = MovingShadowKey::for_shape(&wider, &drop(22.0), 0.96, 0.0);
        assert!(
            cache.lookup(&resized).is_none(),
            "a resized shape blurs again"
        );
        cache.store(resized, kept(50, 30), (24.0, 254.0));
        assert_eq!(cache.entries.len(), 1, "in the place it had");
        assert_eq!(cache.pixels, 50 * 30);
        assert!(cache.lookup(&key).is_none());
    }

    #[test]
    fn the_moving_shadows_start_again_when_full() {
        let mut cache = MovingShadowCache::new();
        for i in 0..MOVING_SHADOWS_MAX {
            let mut shape = card(0.0, 0.0);
            shape.id = Uuid::from_u64_pair(0, i as u64 + 1);
            cache.store(
                MovingShadowKey::for_shape(&shape, &drop(22.0), 1.0, 0.0),
                kept(4, 4),
                (0.0, 0.0),
            );
        }
        assert_eq!(cache.entries.len(), MOVING_SHADOWS_MAX);
        let mut one_more = card(0.0, 0.0);
        one_more.id = Uuid::from_u64_pair(1, 0);
        cache.store(
            MovingShadowKey::for_shape(&one_more, &drop(22.0), 1.0, 0.0),
            kept(4, 4),
            (0.0, 0.0),
        );
        assert_eq!(cache.entries.len(), 1);
        assert_eq!(cache.pixels, 16);
    }
}
