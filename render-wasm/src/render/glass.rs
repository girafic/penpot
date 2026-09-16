//! Glass backdrop effect.
//!
//! The effect is a Skia image-filter graph used as a *backdrop* filter, so it
//! reads the pixels already drawn behind the shape:
//!
//! ```text
//! map      = shader(glass map)             R,G: unit offset (0.5 = none), B: light
//! frosted  = blur(frost)                   (the backdrop when frost = 0)
//! refract  = displacement_map(map, frosted) × 3 scales, merged by channel
//!            (one pass when there is no dispersion)
//! result   = screen(refract, white × map.B)
//! ```
//!
//! The glass map is a runtime shader. Rects, frames, groups and circles use an
//! analytic signed distance field; paths, bools and text use a blurred alpha
//! mask of their silhouette. Skia's displacement filter reads the backdrop
//! outside the shape bounds when the offset points outward, which a runtime
//! image filter (sample radius 0 in our bindings) cannot do.

use std::cell::OnceCell;

use skia_safe::{
    self as skia, color_filters, image_filters, runtime_effect::ChildPtr, Blender, Canvas,
    ColorChannel, ImageFilter, Matrix, Paint, RuntimeEffect, Shader,
};

use super::text;
use super::RenderState;
use crate::shapes::{Glass, Shape, Stroke, Type};

/// Largest share of the refraction added to red and taken from blue.
const MAX_DISPERSION: f32 = 0.6;
/// Peak strength of the light highlight at 100% intensity.
const MAX_LIGHT: f32 = 0.85;
/// Strength of the highlight on the edge that faces away from the light.
const BACK_LIGHT: f32 = 0.45;

const GLASS_MAP_SKSL: &str = r#"
uniform shader mask;
uniform float3x3 toLocal;
uniform float4 rect;
uniform float4 radii;
uniform float kind;
uniform float scale;
uniform float depth;
uniform float stepSize;
uniform float profile;
uniform float3 light;
uniform float rimWidth;

float sdRRect(float2 p) {
    float2 c = (rect.xy + rect.zw) * 0.5;
    float2 b = (rect.zw - rect.xy) * 0.5;
    float2 q = p - c;
    float r = q.x < 0.0 ? (q.y < 0.0 ? radii.x : radii.w)
                        : (q.y < 0.0 ? radii.y : radii.z);
    r = min(r, min(b.x, b.y));
    float2 d = abs(q) - b + r;
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0)) - r;
}

float sdEllipse(float2 p) {
    float2 c = (rect.xy + rect.zw) * 0.5;
    float2 r = max((rect.zw - rect.xy) * 0.5, float2(0.001));
    float2 q = p - c;
    float k0 = length(q / r);
    float k1 = length(q / (r * r));
    return k1 < 0.00001 ? -min(r.x, r.y) : k0 * (k0 - 1.0) / k1;
}

// 0 on the edge, 1 at `depth` px inside. Not clamped, so the gradient
// still points inward outside the shape and in thin parts of a mask.
float bevel(float2 coord) {
    if (kind > 1.5) {
        return (mask.eval(coord).a - 0.5) * 2.0;
    }
    float3 l = toLocal * float3(coord, 1.0);
    float2 p = l.xy / l.z;
    float d = kind > 0.5 ? sdEllipse(p) : sdRRect(p);
    return -d * scale / depth;
}

half4 main(float2 coord) {
    float t = saturate(bevel(coord));
    float2 g = float2(bevel(coord + float2(stepSize, 0.0)) - bevel(coord - float2(stepSize, 0.0)),
                      bevel(coord + float2(0.0, stepSize)) - bevel(coord - float2(0.0, stepSize)));
    float len = length(g);
    float2 inward = len > 0.000001 ? g / len : float2(0.0);

    // Sample outward, strongest at the edge: the rim shows the backdrop
    // around the shape, compressed.
    float bend = pow(1.0 - t, profile);
    float2 offset = 0.5 - 0.5 * inward * bend;

    float rim = saturate(1.0 - t * depth / rimWidth);
    rim *= rim;
    float front = max(dot(-inward, light.xy), 0.0);
    float back = max(dot(inward, light.xy), 0.0);
    float highlight = light.z * rim * (front * front + BACK * back * back);

    return half4(half2(offset), half(saturate(highlight)), 1.0);
}
"#;

// Keeps the green/alpha of the backdrop pass and takes one channel from src.
const TAKE_RED_SKSL: &str = r#"
half4 main(half4 src, half4 dst) {
    return half4(src.r, dst.g, dst.b, dst.a);
}
"#;

const TAKE_BLUE_SKSL: &str = r#"
half4 main(half4 src, half4 dst) {
    return half4(dst.r, dst.g, src.b, dst.a);
}
"#;

struct Effects {
    map: RuntimeEffect,
    take_red: Blender,
    take_blue: Blender,
}

thread_local! {
    static EFFECTS: OnceCell<Option<Effects>> = const { OnceCell::new() };
}

fn compile_effects() -> Option<Effects> {
    let map_source = GLASS_MAP_SKSL.replace("BACK", &format!("{BACK_LIGHT:.3}"));
    let map = RuntimeEffect::make_for_shader(map_source, None)
        .map_err(|e| eprintln!("Glass map shader error: {e}"))
        .ok()?;
    let blender = |source: &str| {
        RuntimeEffect::make_for_blender(source, None)
            .map_err(|e| eprintln!("Glass blender error: {e}"))
            .ok()?
            .make_blender(skia::Data::new_empty(), None)
    };
    Some(Effects {
        map,
        take_red: blender(TAKE_RED_SKSL)?,
        take_blue: blender(TAKE_BLUE_SKSL)?,
    })
}

fn with_effects<R>(f: impl FnOnce(Option<&Effects>) -> R) -> R {
    EFFECTS.with(|cell| f(cell.get_or_init(compile_effects).as_ref()))
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum MapKind {
    RRect = 0,
    Ellipse = 1,
    Mask = 2,
}

fn map_kind(shape: &Shape) -> MapKind {
    match shape.shape_type {
        Type::Rect(_) | Type::Frame(_) | Type::Group(_) => MapKind::RRect,
        Type::Circle => MapKind::Ellipse,
        Type::Path(_) | Type::Bool(_) | Type::Text(_) | Type::SVGRaw(_) => MapKind::Mask,
    }
}

/// Values derived from the glass parameters, in device px.
#[derive(Debug, Clone, Copy, PartialEq)]
struct Params {
    depth: f32,
    /// Largest refraction offset (before dispersion).
    offset: f32,
    /// Share added to red and taken from blue.
    dispersion: f32,
    frost_sigma: f32,
    /// Exponent of the bend falloff from the edge to the center.
    profile: f32,
    /// Direction the light comes from (y down).
    light_dir: (f32, f32),
    light: f32,
    rim_width: f32,
}

impl Params {
    /// `max_reach` limits how far (device px) the filter may sample outside
    /// a pixel, so tiled rendering never reads past the tile margin.
    fn new(glass: &Glass, scale: f32, max_reach: Option<f32>) -> Self {
        let depth = glass.depth * scale;
        let dispersion = glass.dispersion / 100.0 * MAX_DISPERSION;
        let mut offset = glass.refraction / 100.0 * depth;
        let mut frost_sigma = glass.frost_sigma(scale);

        if let Some(reach) = max_reach {
            // Half of the reach for the offset, half for the blur (≈3σ).
            offset = offset.min(reach * 0.5 / (1.0 + dispersion));
            frost_sigma = frost_sigma.min(reach * 0.5 / 3.0);
        }

        let angle = glass.light_angle.to_radians();
        let splay = glass.splay / 100.0;

        Params {
            depth,
            offset,
            dispersion,
            frost_sigma,
            profile: 3.0 + (0.75 - 3.0) * splay,
            light_dir: (angle.sin(), -angle.cos()),
            light: glass.light_intensity / 100.0 * MAX_LIGHT,
            rim_width: (depth * 0.35).max(1.5),
        }
    }

    fn refracts(&self) -> bool {
        self.offset > 0.01 && self.depth > 0.01
    }

    fn lights(&self) -> bool {
        self.light > 0.001 && self.depth > 0.01
    }

    fn needs_map(&self) -> bool {
        self.refracts() || self.lights()
    }

    /// Blur sigma for the silhouette mask so it rises from 0.5 on the edge to
    /// ~1 at `depth` inside.
    fn mask_sigma(&self) -> f32 {
        (self.depth * 0.5).max(0.5)
    }
}

struct Uniforms {
    data: Vec<u8>,
}

impl Uniforms {
    fn new(effect: &RuntimeEffect) -> Self {
        Uniforms {
            data: vec![0; effect.uniform_size()],
        }
    }

    fn set(&mut self, effect: &RuntimeEffect, name: &str, values: &[f32]) {
        let Some(uniform) = effect.find_uniform(name) else {
            return;
        };
        let offset = uniform.offset();
        for (i, value) in values.iter().enumerate() {
            let start = offset + i * 4;
            if let Some(slot) = self.data.get_mut(start..start + 4) {
                slot.copy_from_slice(&value.to_le_bytes());
            }
        }
    }
}

/// SkSL matrices are column-major.
fn column_major(m: &Matrix) -> [f32; 9] {
    [
        m.scale_x(),
        m.skew_y(),
        m.persp_x(),
        m.skew_x(),
        m.scale_y(),
        m.persp_y(),
        m.translate_x(),
        m.translate_y(),
        m[8],
    ]
}

/// Area covered by the glass in shape-local coordinates.
fn glass_rect(shape: &Shape, stroke_outset: f32) -> skia::Rect {
    let mut rect = shape.selrect;
    if stroke_outset > 0.0 {
        rect.outset((stroke_outset, stroke_outset));
    }
    rect
}

/// Corner radii (UL, UR, LR, LL) of the glass area, grown by the stroke reach.
fn bevel_radii(shape: &Shape, stroke_outset: f32) -> [f32; 4] {
    let corners = match &shape.shape_type {
        Type::Rect(data) => data.corners,
        Type::Frame(data) => data.corners,
        _ => None,
    };
    let mut radii = [0.0; 4];
    if let Some(corners) = corners {
        for (radius, corner) in radii.iter_mut().zip(corners.iter()) {
            *radius = corner.x.min(corner.y).max(0.0) + stroke_outset;
        }
    }
    radii
}

fn paint_silhouette(canvas: &Canvas, shape: &Shape, stroke_outset: f32) {
    if matches!(shape.shape_type, Type::Text(_)) {
        text::paint_text_mask(canvas, shape);
        return;
    }
    let mut paint = Paint::default();
    paint.set_anti_alias(true);
    paint.set_color(skia::Color::BLACK);
    let path = if stroke_outset > 0.0 {
        Some(RenderState::background_blur_clip_path(shape, stroke_outset))
    } else {
        shape.get_skia_path()
    };
    match path {
        Some(path) => {
            canvas.draw_path(&path, &paint);
        }
        None => {
            canvas.draw_rect(shape.selrect, &paint);
        }
    }
}

/// Blurred silhouette of the shape in device space, as a shader.
fn silhouette_shader(
    shape: &Shape,
    local_to_device: &Matrix,
    stroke_outset: f32,
    sigma: f32,
) -> Option<Shader> {
    let device_rect = local_to_device.map_rect(glass_rect(shape, stroke_outset)).0;
    let bounds = device_rect.with_outset((sigma * 3.0 + 2.0, sigma * 3.0 + 2.0));

    let blur = image_filters::blur((sigma, sigma), skia::TileMode::Decal, None, None)?;
    let mut layer_paint = Paint::default();
    layer_paint.set_image_filter(blur);

    let mut recorder = skia::PictureRecorder::new();
    let canvas = recorder.begin_recording(bounds, false);
    canvas.save_layer(&skia::canvas::SaveLayerRec::default().paint(&layer_paint));
    canvas.set_matrix(&skia::M44::from(local_to_device));
    paint_silhouette(canvas, shape, stroke_outset);
    canvas.restore();
    let picture = recorder.finish_recording_as_picture(Some(&bounds))?;

    // The picture shader puts the tile's top-left corner at the origin; move
    // it back so the mask lines up with the device pixels.
    let offset = Matrix::translate((bounds.left, bounds.top));
    Some(picture.to_shader(
        (skia::TileMode::Decal, skia::TileMode::Decal),
        skia::FilterMode::Linear,
        Some(&offset),
        Some(&bounds),
    ))
}

fn glass_map_shader(
    effect: &RuntimeEffect,
    shape: &Shape,
    params: &Params,
    local_to_device: &Matrix,
    scale: f32,
    stroke_outset: f32,
) -> Option<Shader> {
    let to_local = local_to_device.invert()?;
    let kind = map_kind(shape);
    let rect = glass_rect(shape, stroke_outset);

    let (mask, step) = if kind == MapKind::Mask {
        let sigma = params.mask_sigma();
        (
            silhouette_shader(shape, local_to_device, stroke_outset, sigma)?,
            (sigma * 0.5).max(1.0),
        )
    } else {
        (skia::shaders::color(skia::Color::TRANSPARENT), 1.0)
    };

    let mut uniforms = Uniforms::new(effect);
    uniforms.set(effect, "toLocal", &column_major(&to_local));
    uniforms.set(
        effect,
        "rect",
        &[rect.left, rect.top, rect.right, rect.bottom],
    );
    uniforms.set(effect, "radii", &bevel_radii(shape, stroke_outset));
    uniforms.set(effect, "kind", &[kind as i32 as f32]);
    uniforms.set(effect, "scale", &[scale]);
    uniforms.set(effect, "depth", &[params.depth.max(0.01)]);
    uniforms.set(effect, "stepSize", &[step]);
    uniforms.set(effect, "profile", &[params.profile]);
    uniforms.set(
        effect,
        "light",
        &[params.light_dir.0, params.light_dir.1, params.light],
    );
    uniforms.set(effect, "rimWidth", &[params.rim_width]);

    effect.make_shader(
        skia::Data::new_copy(&uniforms.data),
        &[ChildPtr::Shader(mask)],
        None,
    )
}

/// White with the map's blue channel as alpha, for the highlight.
fn light_filter(map: &ImageFilter) -> Option<ImageFilter> {
    #[rustfmt::skip]
    let matrix = [
        0.0, 0.0, 0.0, 0.0, 1.0,
        0.0, 0.0, 0.0, 0.0, 1.0,
        0.0, 0.0, 0.0, 0.0, 1.0,
        0.0, 0.0, 1.0, 0.0, 0.0,
    ];
    image_filters::color_filter(
        color_filters::matrix_row_major(&matrix, None),
        map.clone(),
        None,
    )
}

/// Builds the backdrop filter for `glass`. `local_to_device` maps the shape's
/// local coordinates to the device pixels the filter runs on.
pub fn build_filter(
    shape: &Shape,
    glass: &Glass,
    local_to_device: &Matrix,
    scale: f32,
    max_reach: Option<f32>,
) -> Option<ImageFilter> {
    if glass.is_noop() {
        return None;
    }

    let params = Params::new(glass, scale, max_reach);
    let stroke_outset = stroke_outset(shape);

    let frosted = if params.frost_sigma > 0.0 {
        image_filters::blur(
            (params.frost_sigma, params.frost_sigma),
            skia::TileMode::Clamp,
            None,
            None,
        )
    } else {
        None
    };

    let map = if params.needs_map() {
        with_effects(|effects| {
            let effects = effects?;
            let shader = glass_map_shader(
                &effects.map,
                shape,
                &params,
                local_to_device,
                scale,
                stroke_outset,
            )?;
            let map = image_filters::shader(shader, None)?;
            Some((map, effects.take_red.clone(), effects.take_blue.clone()))
        })
    } else {
        None
    };

    let Some((map, take_red, take_blue)) = map else {
        // Frost only (or the shader is unavailable).
        return frosted;
    };

    let mut result = frosted.clone();
    let mut applied = frosted.is_some();

    if params.refracts() {
        let scale = params.offset * 2.0;
        let displace = |s: f32| {
            image_filters::displacement_map(
                (ColorChannel::R, ColorChannel::G),
                s,
                map.clone(),
                frosted.clone(),
                None,
            )
        };

        result = if params.dispersion > 0.001 {
            let red = displace(scale * (1.0 + params.dispersion))?;
            let green = displace(scale)?;
            let blue = displace(scale * (1.0 - params.dispersion))?;
            let red_green = image_filters::blend(take_red, green, red, None)?;
            image_filters::blend(take_blue, red_green, blue, None)
        } else {
            displace(scale)
        };
        applied = result.is_some();
    }

    if params.lights() {
        if let Some(light) = light_filter(&map) {
            result = image_filters::blend(skia::BlendMode::Screen, result, light, None);
            applied = result.is_some();
        }
    }

    if applied {
        result
    } else {
        None
    }
}

fn stroke_outset(shape: &Shape) -> f32 {
    let is_open = !matches!(shape.shape_type, Type::Text(_)) && shape.is_open();
    Stroke::max_bounds_width(shape.visible_strokes(), is_open)
}

/// Clips `canvas` to the area covered by the glass (fill plus the outward
/// reach of the strokes). The canvas matrix must be the shape's
/// local-to-device matrix.
pub fn clip_to_glass(canvas: &Canvas, shape: &Shape) {
    let outset = stroke_outset(shape);
    if outset > 0.0 && !matches!(shape.shape_type, Type::Text(_)) {
        let clip_path = RenderState::background_blur_clip_path(shape, outset);
        canvas.clip_path(&clip_path, skia::ClipOp::Intersect, true);
        return;
    }

    match &shape.shape_type {
        Type::Rect(data) if data.corners.is_some() => {
            let rrect = skia::RRect::new_rect_radii(shape.selrect, data.corners.as_ref().unwrap());
            canvas.clip_rrect(rrect, skia::ClipOp::Intersect, true);
        }
        Type::Frame(data) if data.corners.is_some() => {
            let rrect = skia::RRect::new_rect_radii(shape.selrect, data.corners.as_ref().unwrap());
            canvas.clip_rrect(rrect, skia::ClipOp::Intersect, true);
        }
        Type::Circle => {
            let mut pb = skia::PathBuilder::new();
            pb.add_oval(shape.selrect, None, None);
            canvas.clip_path(&pb.detach(), skia::ClipOp::Intersect, true);
        }
        Type::Path(_) | Type::Bool(_) => match shape.get_skia_path() {
            Some(path) => {
                canvas.clip_path(&path, skia::ClipOp::Intersect, true);
            }
            None => {
                canvas.clip_rect(shape.selrect, skia::ClipOp::Intersect, true);
            }
        },
        Type::Text(_) => {
            canvas.clip_rect(glass_rect(shape, outset), skia::ClipOp::Intersect, true);
        }
        Type::Rect(_) | Type::Frame(_) | Type::Group(_) | Type::SVGRaw(_) => {
            canvas.clip_rect(shape.selrect, skia::ClipOp::Intersect, true);
        }
    }
}

/// Draws the glass effect of `shape` over what `canvas` already holds.
///
/// The canvas matrix must be `local_to_device` when called; it is restored on
/// return. `max_reach` is the tile margin budget (None when exporting).
pub fn render_glass_backdrop(
    canvas: &Canvas,
    shape: &Shape,
    glass: &Glass,
    local_to_device: &Matrix,
    scale: f32,
    max_reach: Option<f32>,
) {
    if matches!(shape.shape_type, Type::SVGRaw(_)) {
        return;
    }
    let Some(filter) = build_filter(shape, glass, local_to_device, scale, max_reach) else {
        return;
    };

    canvas.save();
    clip_to_glass(canvas, shape);
    // The filter works in device space; the clip survives reset_matrix.
    canvas.reset_matrix();

    if matches!(shape.shape_type, Type::Text(_)) {
        // SrcOver + DstIn mask: keep the glass only under the glyphs and
        // leave the rest of the clip rect untouched.
        let layer_rec = skia::canvas::SaveLayerRec::default()
            .backdrop(&filter)
            .backdrop_tile_mode(skia::TileMode::Clamp);
        canvas.save_layer(&layer_rec);
        canvas.set_matrix(&skia::M44::from(local_to_device));

        let mut mask_paint = Paint::default();
        mask_paint.set_blend_mode(skia::BlendMode::DstIn);
        canvas.save_layer(&skia::canvas::SaveLayerRec::default().paint(&mask_paint));
        text::paint_text_mask(canvas, shape);
        canvas.restore(); // mask layer
        canvas.restore(); // glass layer
        canvas.restore(); // clip
        return;
    }

    // Src: replace the clipped backdrop with the glass result instead of
    // drawing it on top (which would show the sharp backdrop through).
    let mut paint = Paint::default();
    paint.set_blend_mode(skia::BlendMode::Src);
    let layer_rec = skia::canvas::SaveLayerRec::default()
        .backdrop(&filter)
        .backdrop_tile_mode(skia::TileMode::Clamp)
        .paint(&paint);
    canvas.save_layer(&layer_rec);
    canvas.restore(); // glass layer
    canvas.restore(); // clip
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shapes::Type;
    use crate::uuid::Uuid;

    fn glass() -> Glass {
        Glass::new(false, -45.0, 80.0, 80.0, 20.0, 50.0, 4.0, 0.0)
    }

    fn rect_shape() -> Shape {
        let mut shape = Shape::new(Uuid::new_v4());
        shape.set_shape_type(Type::Rect(Default::default()));
        shape.set_selrect(0.0, 0.0, 68.0, 64.0);
        shape
    }

    #[test]
    fn params_scale_with_zoom() {
        let params = Params::new(&glass(), 2.0, None);
        assert_eq!(params.depth, 40.0);
        assert_eq!(params.offset, 32.0);
        assert!((params.dispersion - 0.3).abs() < 1e-6);
        assert_eq!(params.frost_sigma, glass().frost_sigma(2.0));
    }

    #[test]
    fn params_respect_the_reach_budget() {
        let params = Params::new(&glass(), 20.0, Some(64.0));
        assert!(params.offset * (1.0 + params.dispersion) <= 32.0 + 1e-4);
        assert!(params.frost_sigma * 3.0 <= 32.0 + 1e-4);
    }

    #[test]
    fn light_comes_from_the_top_left_at_minus_45() {
        let params = Params::new(&glass(), 1.0, None);
        let (x, y) = params.light_dir;
        assert!(x < 0.0 && y < 0.0);
        assert!((x - y).abs() < 1e-5);
    }

    #[test]
    fn splay_softens_the_profile() {
        let sharp = Params::new(&glass(), 1.0, None);
        let soft = Params::new(
            &Glass::new(false, -45.0, 80.0, 80.0, 20.0, 50.0, 4.0, 100.0),
            1.0,
            None,
        );
        assert!(soft.profile < sharp.profile);
    }

    #[test]
    fn bevel_radii_follow_corners_and_stroke_reach() {
        let mut shape = rect_shape();
        assert_eq!(bevel_radii(&shape, 0.0), [0.0; 4]);
        shape.set_corners((4.0, 8.0, 12.0, 16.0));
        assert_eq!(bevel_radii(&shape, 2.0), [6.0, 10.0, 14.0, 18.0]);
    }

    #[test]
    fn column_major_matches_sksl_layout() {
        let m = Matrix::new_all(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 0.0, 0.0, 1.0);
        assert_eq!(
            column_major(&m),
            [1.0, 4.0, 0.0, 2.0, 5.0, 0.0, 3.0, 6.0, 1.0]
        );
    }

    #[test]
    fn noop_glass_has_no_filter() {
        let shape = rect_shape();
        let glass = Glass::new(false, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
        assert!(build_filter(&shape, &glass, &Matrix::new_identity(), 1.0, None).is_none());
    }

    #[test]
    fn shaders_compile() {
        with_effects(|effects| assert!(effects.is_some()));
    }

    #[test]
    fn filter_builds_for_every_kind() {
        let shape = rect_shape();
        assert!(build_filter(&shape, &glass(), &Matrix::new_identity(), 1.0, None).is_some());

        let mut circle = rect_shape();
        circle.set_shape_type(Type::Circle);
        assert!(build_filter(&circle, &glass(), &Matrix::new_identity(), 1.0, None).is_some());
    }

    #[test]
    fn glass_refracts_and_frosts_the_backdrop() {
        let mut surface = skia::surfaces::raster_n32_premul((96, 96)).unwrap();
        let canvas = surface.canvas();
        // Stripes behind the glass so both frost and refraction show.
        canvas.clear(skia::Color::WHITE);
        let mut paint = Paint::default();
        paint.set_color(skia::Color::BLACK);
        for i in (0..96).step_by(8) {
            canvas.draw_rect(skia::Rect::from_xywh(i as f32, 0.0, 4.0, 96.0), &paint);
        }
        let before = surface.image_snapshot();

        let shape = rect_shape();
        let canvas = surface.canvas();
        let matrix = Matrix::new_identity();
        canvas.set_matrix(&skia::M44::from(&matrix));
        render_glass_backdrop(canvas, &shape, &glass(), &matrix, 1.0, None);
        let after = surface.image_snapshot();

        let read = |image: &skia::Image, x: i32, y: i32| -> skia::Color {
            image.peek_pixels().unwrap().get_color((x, y))
        };

        // Outside the glass nothing changes.
        assert_eq!(read(&before, 80, 80), read(&after, 80, 80));
        // Inside, the hard stripes are softened.
        let changed = (0..64).any(|x| read(&before, x, 32) != read(&after, x, 32));
        assert!(changed);
    }

    #[test]
    fn silhouette_mask_lines_up_with_the_shape() {
        use crate::shapes::Path;

        let mut triangle = skia::PathBuilder::new();
        triangle.move_to((0.0, 0.0));
        triangle.line_to((100.0, 0.0));
        triangle.line_to((50.0, 100.0));
        triangle.close();
        let mut shape = Shape::new(Uuid::new_v4());
        shape.set_shape_type(Type::Path(Path::from_skia_path(triangle.detach())));
        shape.set_selrect(0.0, 0.0, 100.0, 100.0);

        let mask = silhouette_shader(&shape, &Matrix::new_identity(), 0.0, 10.0).unwrap();
        let mut surface = skia::surfaces::raster_n32_premul((100, 100)).unwrap();
        let mut paint = Paint::default();
        paint.set_shader(mask);
        surface.canvas().draw_paint(&paint);
        let image = surface.image_snapshot();
        let pixels = image.peek_pixels().unwrap();

        // Deep inside the mask is opaque, on the edge it is about half.
        assert!(pixels.get_color((50, 30)).a() > 240);
        let edge = pixels.get_color((50, 0)).a();
        assert!((100..160).contains(&edge), "edge alpha {edge}");
        assert_eq!(pixels.get_color((5, 90)).a(), 0);
    }
}
