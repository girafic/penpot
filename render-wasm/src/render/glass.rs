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
//! The glass map is a runtime shader. Rects, frames and groups get their
//! bevel from eased edge ramps, circles from an ellipse distance field, and
//! paths, bools and text from a blurred silhouette mask. The rim of the glass
//! samples further inside, so it magnifies the backdrop like a lens edge.
//! Skia's displacement filter does the sampling: it may read around a pixel,
//! which a runtime image filter (sample radius 0 in our bindings) cannot.

use std::cell::OnceCell;

use skia_safe::{
    self as skia, color_filters, image_filters, runtime_effect::ChildPtr, Blender, Canvas,
    ColorChannel, ImageFilter, Matrix, Paint, RuntimeEffect, Shader,
};

use super::text;
use super::RenderState;
use crate::shapes::{Glass, Shape, Stroke, Type};

/// Largest share of the refraction added to red and taken from blue.
const MAX_DISPERSION: f32 = 0.15;
/// Peak strength of the light highlight at 100% intensity.
const MAX_LIGHT: f32 = 0.85;
/// Strength of the highlight on the edge that faces away from the light.
const BACK_LIGHT: f32 = 0.45;
/// Share of the highlight every edge gets, whatever the light angle.
const AMBIENT_LIGHT: f32 = 0.4;
/// Width of the refracting band, relative to the depth.
const BAND_PER_DEPTH: f32 = 3.0;
/// Largest inward sample offset, relative to the band width. At 100%
/// refraction the edge magnifies as much as it can without mirroring.
const MAX_OFFSET_PER_BAND: f32 = 0.33;
/// Width of the highlight line along the outline, in document px.
const RIM_WIDTH: f32 = 2.0;

// The glass map. R,G hold the sample offset (0.5 = none) and B the light.
//
// `glassField` returns the bevel height (0 on the edge, 1 at `band` px
// inside) and the distance to the outline in device px. Rects use the
// product of two eased ramps, so the bevel has no ridges from the corners.
// Masks come from the silhouette blurred twice: a wide blur in green for the
// bevel and a narrow one in red, whose value is ~linear in the distance to
// the edge, for the highlight.
const GLASS_MAP_SKSL: &str = r#"
uniform shader mask;
uniform float3x3 toLocal;
uniform float4 rect;
uniform float4 radii;
uniform float kind;
uniform float scale;
uniform float band;
uniform float rimSigma;
uniform float stepSize;
uniform float profile;
uniform float3 light;
uniform float rimWidth;

float easeOut(float u) {
    float v = 1.0 - min(u, 1.0);
    return 1.0 - v * v;
}

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

float2 glassField(float2 coord) {
    if (kind > 1.5) {
        half4 m = mask.eval(coord);
        return float2((m.g - 0.5) * 2.0, (m.r - 0.5) * rimSigma * 2.5066);
    }
    float3 l = toLocal * float3(coord, 1.0);
    float2 p = l.xy / l.z;
    float2 extent = (rect.zw - rect.xy) * 0.5;
    if (kind > 0.5) {
        float d = -sdEllipse(p) * scale;
        float w = max(min(band, min(extent.x, extent.y) * scale), 0.001);
        return float2(easeOut(d / w), d);
    }
    float2 edge = (extent - abs(p - (rect.xy + rect.zw) * 0.5)) * scale;
    float2 w = max(min(float2(band), extent * scale), float2(0.001));
    float ex = easeOut(edge.x / w.x);
    float ey = easeOut(edge.y / w.y);
    float field = (edge.x < 0.0 || edge.y < 0.0) ? min(ex, ey) : ex * ey;
    return float2(field, -sdRRect(p) * scale);
}

half4 main(float2 coord) {
    float2 f = glassField(coord);
    float t = saturate(f.x);
    float2 g = float2(
        glassField(coord + float2(stepSize, 0.0)).x - glassField(coord - float2(stepSize, 0.0)).x,
        glassField(coord + float2(0.0, stepSize)).x - glassField(coord - float2(0.0, stepSize)).x);
    float len = length(g);
    float2 inward = len > 0.000001 ? g / len : float2(0.0);

    // Sample further inside, strongest on the edge: the rim magnifies the
    // backdrop like the curved edge of a lens.
    float bend = pow(1.0 - t, profile);
    float2 offset = 0.5 + 0.5 * inward * bend;

    // Thin highlight along the outline.
    float rim = 1.0 - smoothstep(0.0, rimWidth, f.y);
    float front = max(dot(-inward, light.xy), 0.0);
    float back = max(dot(inward, light.xy), 0.0);
    float shade = AMBIENT + (1.0 - AMBIENT) * (front + BACK * back);
    float highlight = light.z * rim * shade;

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
    let map_source = GLASS_MAP_SKSL
        .replace("AMBIENT", &format!("{AMBIENT_LIGHT:.3}"))
        .replace("BACK", &format!("{BACK_LIGHT:.3}"));
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
    /// Width of the refracting band along the outline.
    band: f32,
    /// Largest inward sample offset (before dispersion).
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
    /// `max_reach` limits how far (device px) the filter may sample away from
    /// a pixel, so tiled rendering never reads past the tile margin.
    fn new(glass: &Glass, scale: f32, max_reach: Option<f32>) -> Self {
        let band = glass.depth * BAND_PER_DEPTH * scale;
        let dispersion = glass.dispersion / 100.0 * MAX_DISPERSION;
        let mut offset = glass.refraction / 100.0 * MAX_OFFSET_PER_BAND * band;
        let mut frost_sigma = glass.frost_sigma(scale);

        if let Some(reach) = max_reach {
            // Half of the reach for the offset, half for the blur (≈3σ).
            offset = offset.min(reach * 0.5 / (1.0 + dispersion));
            frost_sigma = frost_sigma.min(reach * 0.5 / 3.0);
        }

        let angle = glass.light_angle.to_radians();
        let splay = glass.splay / 100.0;

        Params {
            band,
            offset,
            dispersion,
            frost_sigma,
            // Splay spreads the bend from the edge toward the center.
            profile: 1.5 + (0.75 - 1.5) * splay,
            light_dir: (angle.sin(), -angle.cos()),
            light: glass.light_intensity / 100.0 * MAX_LIGHT,
            rim_width: (RIM_WIDTH * scale).max(1.0),
        }
    }

    fn refracts(&self) -> bool {
        self.offset > 0.01 && self.band > 0.01
    }

    fn lights(&self) -> bool {
        self.light > 0.001
    }

    fn needs_map(&self) -> bool {
        self.refracts() || self.lights()
    }

    /// Blur sigma for the silhouette mask. Its slope on the edge matches the
    /// rect bevel, so both kinds bend the same amount.
    fn mask_sigma(&self) -> f32 {
        (self.band * 0.4).max(0.5)
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

fn paint_silhouette(canvas: &Canvas, shape: &Shape, stroke_outset: f32, color: skia::Color) {
    let mut paint = Paint::default();
    paint.set_anti_alias(true);
    paint.set_color(color);

    if matches!(shape.shape_type, Type::Text(_)) {
        // The text mask paints opaque black; tint it with the channel color.
        let mut tint = Paint::default();
        tint.set_color_filter(color_filters::blend(color, skia::BlendMode::SrcIn));
        canvas.save_layer(&skia::canvas::SaveLayerRec::default().paint(&tint));
        text::paint_text_mask(canvas, shape);
        canvas.restore();
        return;
    }

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

/// Adds the silhouette, blurred by `sigma`, into one channel of the mask.
fn add_blurred_silhouette(
    canvas: &Canvas,
    shape: &Shape,
    local_to_device: &Matrix,
    stroke_outset: f32,
    sigma: f32,
    color: skia::Color,
) {
    let Some(blur) = image_filters::blur((sigma, sigma), skia::TileMode::Decal, None, None) else {
        return;
    };
    let mut layer_paint = Paint::default();
    layer_paint.set_image_filter(blur);
    layer_paint.set_blend_mode(skia::BlendMode::Plus);

    canvas.save_layer(&skia::canvas::SaveLayerRec::default().paint(&layer_paint));
    canvas.set_matrix(&skia::M44::from(local_to_device));
    paint_silhouette(canvas, shape, stroke_outset, color);
    canvas.restore();
}

/// The silhouette in device space as a shader: green holds it blurred by
/// `sigma` (the bevel), red blurred by `rim_sigma` (the edge distance).
/// Alpha is opaque inside the recorded bounds so both channels survive
/// premultiplication.
fn silhouette_shader(
    shape: &Shape,
    local_to_device: &Matrix,
    stroke_outset: f32,
    sigma: f32,
    rim_sigma: f32,
) -> Option<Shader> {
    let device_rect = local_to_device.map_rect(glass_rect(shape, stroke_outset)).0;
    let reach = sigma.max(rim_sigma) * 3.0 + 2.0;
    let bounds = device_rect.with_outset((reach, reach));

    let mut recorder = skia::PictureRecorder::new();
    let canvas = recorder.begin_recording(bounds, false);
    canvas.clear(skia::Color::BLACK);
    add_blurred_silhouette(
        canvas,
        shape,
        local_to_device,
        stroke_outset,
        sigma,
        skia::Color::from_rgb(0, 255, 0),
    );
    add_blurred_silhouette(
        canvas,
        shape,
        local_to_device,
        stroke_outset,
        rim_sigma,
        skia::Color::from_rgb(255, 0, 0),
    );
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
            silhouette_shader(
                shape,
                local_to_device,
                stroke_outset,
                sigma,
                params.rim_width,
            )?,
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
    uniforms.set(effect, "band", &[params.band.max(0.01)]);
    uniforms.set(effect, "rimSigma", &[params.rim_width]);
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
        assert_eq!(params.band, 120.0);
        assert!((params.offset - 31.68).abs() < 1e-3);
        assert!((params.dispersion - 0.075).abs() < 1e-6);
        assert_eq!(params.rim_width, 4.0);
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
    fn rect_bevel_has_no_diagonal_seam() {
        let mut shape = rect_shape();
        shape.set_selrect(0.0, 0.0, 200.0, 200.0);
        let glass = Glass::new(false, -45.0, 80.0, 80.0, 30.0, 0.0, 0.0, 0.0);
        let params = Params::new(&glass, 1.0, None);
        let matrix = Matrix::new_identity();

        let map = with_effects(|effects| {
            glass_map_shader(&effects.unwrap().map, &shape, &params, &matrix, 1.0, 0.0)
        })
        .unwrap();
        let mut surface = skia::surfaces::raster_n32_premul((200, 200)).unwrap();
        let mut paint = Paint::default();
        paint.set_shader(map);
        surface.canvas().draw_paint(&paint);
        let image = surface.image_snapshot();
        let pixels = image.peek_pixels().unwrap();

        // Both points sit next to the corner diagonal, on either side of it.
        // A mitered bevel bends one sideways and the other up; a smooth one
        // bends both the same way.
        let a = pixels.get_color((20, 22));
        let b = pixels.get_color((22, 20));
        assert!(a.r() > 140 && a.g() > 140, "inward offset {a:?}");
        assert!((a.r() as i32 - b.g() as i32).abs() <= 2);
        assert!((a.r() as i32 - b.r() as i32).abs() <= 12, "{a:?} vs {b:?}");
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

        let mask = silhouette_shader(&shape, &Matrix::new_identity(), 0.0, 10.0, 1.0).unwrap();
        let mut surface = skia::surfaces::raster_n32_premul((100, 100)).unwrap();
        let mut paint = Paint::default();
        paint.set_shader(mask);
        surface.canvas().draw_paint(&paint);
        let image = surface.image_snapshot();
        let pixels = image.peek_pixels().unwrap();

        // Deep inside both channels are full, on the edge about half, and
        // outside the shape empty. Row 0 is half a pixel inside the edge,
        // which shows more in the narrow red blur.
        let inside = pixels.get_color((50, 30));
        assert!(inside.g() > 240 && inside.r() > 240, "{inside:?}");
        let edge = pixels.get_color((50, 0));
        assert!((100..160).contains(&edge.g()), "edge {edge:?}");
        assert!((150..200).contains(&edge.r()), "edge {edge:?}");
        let near = pixels.get_color((50, 4));
        assert!(near.r() > 250 && near.g() < 200, "near {near:?}");
        let outside = pixels.get_color((5, 90));
        assert_eq!((outside.r(), outside.g()), (0, 0));
    }

    /// Board with black stripes and a glass rect on top.
    fn glass_scene(with_glass: bool) -> (crate::state::ShapesPool, Uuid) {
        use crate::shapes::{Fill, Frame, Rect as RectType, SolidColor};

        let board = Uuid::new_v4();
        let mut pool = crate::state::ShapesPool::new();
        let mut children = vec![];

        for i in 0..10 {
            let id = Uuid::new_v4();
            let stripe = pool.add_shape(id);
            stripe.set_parent(board);
            stripe.set_shape_type(Type::Rect(RectType::default()));
            let x = (i * 10) as f32;
            stripe.set_selrect(x, 0.0, x + 5.0, 100.0);
            stripe.set_fills(vec![Fill::Solid(SolidColor(skia::Color::BLACK))]);
            children.push(id);
        }

        let panel_id = Uuid::new_v4();
        let panel = pool.add_shape(panel_id);
        panel.set_parent(board);
        panel.set_shape_type(Type::Rect(RectType::default()));
        panel.set_selrect(20.0, 20.0, 80.0, 80.0);
        panel.set_fills(vec![Fill::Solid(SolidColor(skia::Color::from_argb(
            51, 217, 217, 217,
        )))]);
        if with_glass {
            panel.set_glass(Some(glass()));
        }
        children.push(panel_id);

        let frame = pool.add_shape(board);
        frame.set_parent(Uuid::nil());
        frame.set_shape_type(Type::Frame(Frame::default()));
        frame.set_selrect(0.0, 0.0, 100.0, 100.0);
        frame.set_fills(vec![Fill::Solid(SolidColor(skia::Color::WHITE))]);
        frame.set_clip(true);
        for child in children {
            frame.add_child(child);
        }

        (pool, board)
    }

    fn export_pixels(with_glass: bool) -> skia::Image {
        let (pool, board) = glass_scene(with_glass);
        let mut resources = super::super::RenderResources::try_new_headless().unwrap();
        let _guard = crate::globals::TestRenderResourcesGuard::install(&mut resources);
        let mut surface = skia::surfaces::raster_n32_premul((100, 100)).unwrap();
        let page = skia::Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
        super::super::vector::render_tree(
            &mut resources,
            surface.canvas(),
            &board,
            &pool,
            1.0,
            page,
        )
        .unwrap();
        surface.image_snapshot()
    }

    #[test]
    fn raster_export_renders_glass() {
        let plain = export_pixels(false);
        let glassy = export_pixels(true);
        let plain_px = plain.peek_pixels().unwrap();
        let glass_px = glassy.peek_pixels().unwrap();

        // Outside the panel the export is unchanged.
        assert_eq!(plain_px.get_color((5, 5)), glass_px.get_color((5, 5)));
        // Inside, the stripes are frosted and bent.
        let changed = (20..80)
            .filter(|x| plain_px.get_color((*x, 50)) != glass_px.get_color((*x, 50)))
            .count();
        assert!(changed > 30, "only {changed} pixels changed");
    }

    #[test]
    fn pdf_export_embeds_the_glass_backdrop() {
        let render = |with_glass: bool| {
            let (pool, board) = glass_scene(with_glass);
            let mut resources = super::super::RenderResources::try_new_headless().unwrap();
            let _guard = crate::globals::TestRenderResourcesGuard::install(&mut resources);
            super::super::pdf::render_to_pdf(&mut resources, &board, &pool, 1.0).unwrap()
        };
        let count_images = |pdf: &[u8]| {
            let text = String::from_utf8_lossy(pdf);
            text.matches("/Subtype /Image").count()
        };

        // PDF has no backdrop filters, so glass is embedded as an image.
        assert_eq!(count_images(&render(false)), 0);
        assert!(count_images(&render(true)) > 0);
    }
}
