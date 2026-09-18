//! Glass backdrop effect.
//!
//! The effect has two parts:
//!
//! 1. A Skia image-filter graph used as a *backdrop* filter, drawn before the
//!    shape. It reads the pixels already drawn behind the shape:
//!
//!    ```text
//!    map      = shader(glass map)             R,G: sample offset (0.5 = none)
//!    frosted  = blur(frost)                   (the backdrop when frost = 0)
//!    adjusted = color matrix(saturation, brightness) on frosted
//!    refract  = displacement_map(map, adjusted) × 3 scales, merged by channel
//!               (one pass when there is no dispersion)
//!    ```
//!
//! 2. The edge light, drawn after the shape's own fills (so a tinted fill
//!    does not dim it): the glass map's blue channel, tinted with the light
//!    color, screen-blended onto the canvas.
//!
//! The glass map is a runtime shader. Rects, frames and groups get their
//! bevel from eased edge ramps, circles from an ellipse distance field, and
//! paths, bools and text from a blurred silhouette mask. The rim of the glass
//! samples further inside, so it magnifies the backdrop like a lens edge.
//! A surface texture adds strips or dents over the whole surface.
//! Skia's displacement filter does the sampling: it may read around a pixel,
//! which a runtime image filter (sample radius 0 in our bindings) cannot.

use std::cell::OnceCell;

use skia_safe::{
    self as skia, color_filters, image_filters, runtime_effect::ChildPtr, Blender, Canvas,
    ColorChannel, ColorFilter, ImageFilter, Matrix, Paint, RuntimeEffect, Shader,
};

use super::text;
use super::RenderState;
use crate::shapes::{Glass, GlassTexture, Shape, Stroke, Type};

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
/// At 100% texture amount, each strip (or dent) shows a backdrop slice this
/// many strip widths wider than itself.
const MAX_FLUTE_COMPRESSION: f32 = 2.0;
/// Strips narrower than this (device px) are not drawn; they would only
/// flicker. Between this and `FULL_FLUTE_PX` the texture fades in.
const MIN_FLUTE_PX: f32 = 2.0;
const FULL_FLUTE_PX: f32 = 4.0;
/// Width of the smoothed seam between two strips, in device px.
const FLUTE_SEAM_PX: f32 = 1.0;

// The glass map. R,G hold the sample offset (0.5 = none) and B the light.
//
// `glassField` returns the bevel height (0 on the edge, 1 at `band` px
// inside) and the distance to the outline in device px. Rects use the
// product of two eased ramps, so the bevel has no ridges from the corners.
// Masks come from the silhouette blurred twice: a wide blur in green for the
// bevel and a narrow one in red, whose value is ~linear in the distance to
// the edge, for the highlight.
//
// `flute` gives the surface texture offset. `u` counts document px across
// the strips from the shape's corner and `v` along them, so the texture
// moves with the shape. The strip textures offset along `texDir` — a lens
// profile with smoothed seams (reeded), a seamless sine (wavy) or a flat
// facet (prismatic). Cross-reeded adds the lens on `texDir2` as well, and
// hammered puts one round dent per grid cell, jittered inside its cell so
// neighbouring cells never seam.
//
// The placeholders AMBIENT and BACK are replaced with constants before
// compiling; no other identifier may contain them.
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
uniform float3 texRow;
uniform float2 texDir;
uniform float3 texRow2;
uniform float2 texDir2;
uniform float texKind;
uniform float texPeriod;
uniform float texEdge;
uniform float texShare;
uniform float bevelShare;

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

float strip(float s) {
    float x = 2.0 * s - 1.0;
    if (texKind > 2.5 && texKind < 3.5) {
        return x;
    }
    if (texKind > 1.5 && texKind < 2.5) {
        return sin(3.14159274 * x);
    }
    return x * (0.65 + 0.35 * x * x);
}

float seamAt(float s) {
    if (texKind > 1.5 && texKind < 2.5) {
        return 1.0;
    }
    return smoothstep(0.0, texEdge, s) * smoothstep(0.0, texEdge, 1.0 - s);
}

float2 hash2(float2 p) {
    float3 q = fract(p.xyx * float3(0.1031, 0.1030, 0.0973));
    q += dot(q, q.yzx + 33.33);
    return fract((q.xx + q.yz) * q.zy);
}

float2 dimple(float2 uv) {
    float2 cell = floor(uv);
    float2 h = hash2(cell);
    float2 g = hash2(cell + float2(11.7, 3.1));
    float radius = 0.3 + 0.18 * g.x;
    float2 center = radius + h * (1.0 - 2.0 * radius);
    float2 q = uv - cell - center;
    float len = length(q);
    float r = len / radius;
    if (r >= 1.0 || len < 0.0001) {
        return float2(0.0);
    }
    float lens = 2.6 * r * (1.0 - r * r);
    float2 dir = q / len;
    return (texDir * dir.x + texDir2 * dir.y) * lens;
}

float2 flute(float2 coord) {
    float u = (dot(texRow.xy, coord) + texRow.z) / texPeriod;
    if (texKind > 4.5) {
        float v = (dot(texRow2.xy, coord) + texRow2.z) / texPeriod;
        return dimple(float2(u, v));
    }
    float s = fract(u);
    float2 d = texDir * (strip(s) * seamAt(s));
    if (texKind > 3.5) {
        float t = fract((dot(texRow2.xy, coord) + texRow2.z) / texPeriod);
        // Both axes bend at once, so scale them to keep the diagonal within
        // the offset the other textures use.
        d = 0.70710678 * (d + texDir2 * (strip(t) * seamAt(t)));
    }
    return d;
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
    float2 d = bevelShare * inward * bend;
    if (texShare > 0.0) {
        d += texShare * flute(coord);
    }
    float2 offset = 0.5 + 0.5 * d;

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

#[inline]
fn smoothstep(edge0: f32, edge1: f32, x: f32) -> f32 {
    let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/// Values derived from the glass parameters, in device px.
#[derive(Debug, Clone, Copy, PartialEq)]
struct Params {
    /// Width of the refracting band along the outline.
    band: f32,
    /// Largest inward sample offset of the bevel (before dispersion).
    offset: f32,
    /// Largest sample offset of the texture (before dispersion).
    texture: f32,
    /// Width of one strip, or the size of one cell of the hammered grid.
    flute: f32,
    /// Share added to red and taken from blue.
    dispersion: f32,
    frost_sigma: f32,
    /// Whether saturation or brightness change the backdrop.
    adjusts_color: bool,
    /// Exponent of the bend falloff from the edge to the center.
    profile: f32,
    /// Direction the light comes from (y down).
    light_dir: (f32, f32),
    light: f32,
    /// Light color, 0..1 per channel.
    light_rgb: [f32; 3],
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

        let flute = if glass.has_texture() {
            glass.texture_scale * scale
        } else {
            0.0
        };
        let fade = smoothstep(MIN_FLUTE_PX, FULL_FLUTE_PX, flute);
        let mut texture = glass.texture_amount / 100.0 * MAX_FLUTE_COMPRESSION * flute * 0.5 * fade;

        if let Some(reach) = max_reach {
            // Half of the reach for the offsets, half for the blur (≈3σ).
            let cap = reach * 0.5 / (1.0 + dispersion);
            let total = offset + texture;
            if total > cap {
                let factor = cap / total;
                offset *= factor;
                texture *= factor;
            }
            frost_sigma = frost_sigma.min(reach * 0.5 / 3.0);
        }

        let angle = glass.light_angle.to_radians();
        let splay = glass.splay / 100.0;
        let color = glass.light_color;

        Params {
            band,
            offset,
            texture,
            flute,
            dispersion,
            frost_sigma,
            adjusts_color: glass.adjusts_color(),
            // Splay spreads the bend from the edge toward the center.
            profile: 1.5 + (0.75 - 1.5) * splay,
            light_dir: (angle.sin(), -angle.cos()),
            light: if glass.is_dark() {
                0.0
            } else {
                glass.light_intensity / 100.0 * MAX_LIGHT
            },
            light_rgb: [
                color.r() as f32 / 255.0,
                color.g() as f32 / 255.0,
                color.b() as f32 / 255.0,
            ],
            // Never wider than the band: outside it the bevel has no
            // direction to light.
            rim_width: (glass.highlight_width * scale)
                .min(band.max(2.0 * scale))
                .max(1.0),
        }
    }

    /// The same values without the texture, for passes that only read the
    /// light from the map.
    fn without_texture(self) -> Self {
        Params {
            texture: 0.0,
            flute: 0.0,
            ..self
        }
    }

    fn bends(&self) -> bool {
        self.offset > 0.01 && self.band > 0.01
    }

    fn textured(&self) -> bool {
        self.texture > 0.01
    }

    fn refracts(&self) -> bool {
        self.bends() || self.textured()
    }

    /// Largest sample offset of bevel and texture together.
    fn displacement(&self) -> f32 {
        let bevel = if self.bends() { self.offset } else { 0.0 };
        let texture = if self.textured() { self.texture } else { 0.0 };
        bevel + texture
    }

    fn lights(&self) -> bool {
        self.light > 0.001
    }

    /// True when the backdrop filter has anything to do.
    fn changes_backdrop(&self) -> bool {
        self.refracts() || self.frost_sigma > 0.0 || self.adjusts_color
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

/// Coefficients of `u(coord) = x·coord.x + y·coord.y + z`: the position
/// across the flutes in document px, measured from `origin` in the shape.
/// `to_local` maps device to shape coordinates; 0° gives vertical flutes.
fn flute_row(to_local: &Matrix, origin: skia::Point, angle_deg: f32) -> [f32; 3] {
    let (s, c) = angle_deg.to_radians().sin_cos();
    let m = to_local;
    [
        c * m.scale_x() + s * m.skew_y(),
        c * m.skew_x() + s * m.scale_y(),
        c * (m.translate_x() - origin.x) + s * (m.translate_y() - origin.y),
    ]
}

/// Code the map shader uses to pick the texture profile, see `flute` in the
/// SkSL source. It follows the order of the serialized texture values.
fn texture_kind(texture: GlassTexture) -> f32 {
    match texture {
        GlassTexture::None => 0.0,
        GlassTexture::Reeded => 1.0,
        GlassTexture::Wavy => 2.0,
        GlassTexture::Prismatic => 3.0,
        GlassTexture::CrossReeded => 4.0,
        GlassTexture::Hammered => 5.0,
    }
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

#[allow(clippy::too_many_arguments)]
fn glass_map_shader(
    effect: &RuntimeEffect,
    shape: &Shape,
    glass: &Glass,
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

    // Split the displacement scale between the bevel and the texture.
    let total = params.displacement();
    let share = |part: f32, active: bool| {
        if active && total > 0.0 {
            part / total
        } else {
            0.0
        }
    };
    uniforms.set(
        effect,
        "bevelShare",
        &[share(params.offset, params.bends())],
    );

    let origin = skia::Point::new(shape.selrect.left, shape.selrect.top);
    let row = flute_row(&to_local, origin, glass.texture_angle);
    let row2 = flute_row(&to_local, origin, glass.texture_angle + 90.0);
    let row_len = (row[0] * row[0] + row[1] * row[1]).sqrt();
    let row2_len = (row2[0] * row2[0] + row2[1] * row2[1]).sqrt();
    let textured = params.textured() && row_len > 1e-6 && row2_len > 1e-6;
    uniforms.set(effect, "texShare", &[share(params.texture, textured)]);
    uniforms.set(effect, "texKind", &[texture_kind(glass.texture)]);
    uniforms.set(effect, "texRow", &row);
    uniforms.set(
        effect,
        "texDir",
        &if textured {
            [row[0] / row_len, row[1] / row_len]
        } else {
            [0.0, 0.0]
        },
    );
    uniforms.set(effect, "texRow2", &row2);
    uniforms.set(
        effect,
        "texDir2",
        &if textured {
            [row2[0] / row2_len, row2[1] / row2_len]
        } else {
            [0.0, 0.0]
        },
    );
    uniforms.set(effect, "texPeriod", &[glass.texture_scale.max(0.01)]);
    uniforms.set(
        effect,
        "texEdge",
        &[(FLUTE_SEAM_PX / params.flute.max(0.01)).clamp(0.001, 0.25)],
    );

    effect.make_shader(
        skia::Data::new_copy(&uniforms.data),
        &[ChildPtr::Shader(mask)],
        None,
    )
}

/// Saturation and brightness as a color matrix on `input` (the frosted
/// backdrop, or the backdrop itself when `None`). The saturation weights are
/// the ones of CSS `saturate()`, so the CSS export matches.
fn color_adjust(glass: &Glass, input: Option<ImageFilter>) -> Option<ImageFilter> {
    let s = glass.saturation / 100.0;
    let b = glass.brightness / 100.0;
    #[rustfmt::skip]
    let matrix = [
        b * (0.213 + 0.787 * s), b * (0.715 - 0.715 * s), b * (0.072 - 0.072 * s), 0.0, 0.0,
        b * (0.213 - 0.213 * s), b * (0.715 + 0.285 * s), b * (0.072 - 0.072 * s), 0.0, 0.0,
        b * (0.213 - 0.213 * s), b * (0.715 - 0.715 * s), b * (0.072 + 0.928 * s), 0.0, 0.0,
        0.0, 0.0, 0.0, 1.0, 0.0,
    ];
    image_filters::color_filter(color_filters::matrix_row_major(&matrix, None), input, None)
}

/// Builds the backdrop filter for `glass`. `local_to_device` maps the shape's
/// local coordinates to the device pixels the filter runs on. The edge light
/// is not part of it; see [`render_glass_light`].
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
    if !params.changes_backdrop() {
        return None;
    }
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

    let base = if params.adjusts_color {
        color_adjust(glass, frosted)
    } else {
        frosted
    };

    if !params.refracts() {
        return base;
    }

    let map = with_effects(|effects| {
        let effects = effects?;
        let shader = glass_map_shader(
            &effects.map,
            shape,
            glass,
            &params,
            local_to_device,
            scale,
            stroke_outset,
        )?;
        let map = image_filters::shader(shader, None)?;
        Some((map, effects.take_red.clone(), effects.take_blue.clone()))
    });

    let Some((map, take_red, take_blue)) = map else {
        // The shader is unavailable: keep frost and color.
        return base;
    };

    let scale = params.displacement() * 2.0;
    let displace = |s: f32| {
        image_filters::displacement_map(
            (ColorChannel::R, ColorChannel::G),
            s,
            map.clone(),
            base.clone(),
            None,
        )
    };

    if params.dispersion > 0.001 {
        let red = displace(scale * (1.0 + params.dispersion))?;
        let green = displace(scale)?;
        let blue = displace(scale * (1.0 - params.dispersion))?;
        let red_green = image_filters::blend(take_red, green, red, None)?;
        image_filters::blend(take_blue, red_green, blue, None)
    } else {
        displace(scale)
    }
}

fn stroke_outset(shape: &Shape) -> f32 {
    let is_open = !matches!(shape.shape_type, Type::Text(_)) && shape.is_open();
    Stroke::max_bounds_width(shape.visible_strokes(), is_open)
}

/// Area of the glass in shape-local coordinates, including the outward
/// reach of the strokes.
pub fn glass_bounds(shape: &Shape) -> skia::Rect {
    glass_rect(shape, stroke_outset(shape))
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

/// Draws the glass backdrop of `shape` over what `canvas` already holds.
/// Call it before the shape's own fills.
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

/// Light color with the map's blue channel as alpha.
fn light_color_filter([r, g, b]: [f32; 3]) -> ColorFilter {
    #[rustfmt::skip]
    let matrix = [
        0.0, 0.0, 0.0, 0.0, r,
        0.0, 0.0, 0.0, 0.0, g,
        0.0, 0.0, 0.0, 0.0, b,
        0.0, 0.0, 1.0, 0.0, 0.0,
    ];
    color_filters::matrix_row_major(&matrix, None)
}

/// Shader of the edge light in device space: the light color, with the
/// highlight strength as alpha.
fn light_shader(
    shape: &Shape,
    glass: &Glass,
    local_to_device: &Matrix,
    scale: f32,
) -> Option<Shader> {
    let params = Params::new(glass, scale, None).without_texture();
    if !params.lights() {
        return None;
    }
    let map = with_effects(|effects| {
        glass_map_shader(
            &effects?.map,
            shape,
            glass,
            &params,
            local_to_device,
            scale,
            stroke_outset(shape),
        )
    })?;
    Some(map.with_color_filter(light_color_filter(params.light_rgb)))
}

/// Draws the edge light of `shape`. Call it after the shape's own fills and
/// strokes, before its children, so a tinted fill does not dim the light.
///
/// The canvas matrix must be `local_to_device` when called; it is restored on
/// return.
pub fn render_glass_light(
    canvas: &Canvas,
    shape: &Shape,
    glass: &Glass,
    local_to_device: &Matrix,
    scale: f32,
) {
    if matches!(shape.shape_type, Type::SVGRaw(_)) {
        return;
    }
    let Some(shader) = light_shader(shape, glass, local_to_device, scale) else {
        return;
    };
    let mut paint = Paint::default();
    paint.set_shader(shader);

    canvas.save();
    clip_to_glass(canvas, shape);
    // The map works in device space; the clip survives reset_matrix.
    canvas.reset_matrix();

    if matches!(shape.shape_type, Type::Text(_)) {
        // Keep the light only on the glyphs, then screen the result.
        let mut screen = Paint::default();
        screen.set_blend_mode(skia::BlendMode::Screen);
        canvas.save_layer(&skia::canvas::SaveLayerRec::default().paint(&screen));
        canvas.draw_paint(&paint);
        canvas.set_matrix(&skia::M44::from(local_to_device));

        let mut mask_paint = Paint::default();
        mask_paint.set_blend_mode(skia::BlendMode::DstIn);
        canvas.save_layer(&skia::canvas::SaveLayerRec::default().paint(&mask_paint));
        text::paint_text_mask(canvas, shape);
        canvas.restore(); // mask layer
        canvas.restore(); // light layer
    } else {
        paint.set_blend_mode(skia::BlendMode::Screen);
        canvas.draw_paint(&paint);
    }
    canvas.restore(); // clip
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shapes::{GlassTexture, Type};
    use crate::uuid::Uuid;

    fn glass() -> Glass {
        Glass::default()
    }

    /// Glass that changes nothing; tests switch single parts on.
    fn inert() -> Glass {
        Glass {
            light_intensity: 0.0,
            refraction: 0.0,
            depth: 0.0,
            dispersion: 0.0,
            frost: 0.0,
            ..Glass::default()
        }
    }

    fn rect_shape() -> Shape {
        let mut shape = Shape::new(Uuid::new_v4());
        shape.set_shape_type(Type::Rect(Default::default()));
        shape.set_selrect(0.0, 0.0, 68.0, 64.0);
        shape
    }

    fn surface(width: i32, height: i32, color: skia::Color) -> skia::Surface {
        let mut surface = skia::surfaces::raster_n32_premul((width, height)).unwrap();
        surface.canvas().clear(color);
        surface
    }

    fn pixel(surface: &mut skia::Surface, x: i32, y: i32) -> skia::Color {
        surface
            .image_snapshot()
            .peek_pixels()
            .unwrap()
            .get_color((x, y))
    }

    /// Draws the glass map of `shape` at identity scale.
    fn render_map(shape: &Shape, glass: &Glass, width: i32, height: i32) -> skia::Surface {
        let params = Params::new(glass, 1.0, None);
        let matrix = Matrix::new_identity();
        let map = with_effects(|effects| {
            glass_map_shader(
                &effects.unwrap().map,
                shape,
                glass,
                &params,
                &matrix,
                1.0,
                0.0,
            )
        })
        .unwrap();
        let mut surface = surface(width, height, skia::Color::TRANSPARENT);
        let mut paint = Paint::default();
        paint.set_shader(map);
        surface.canvas().draw_paint(&paint);
        surface
    }

    #[test]
    fn params_scale_with_zoom() {
        let params = Params::new(&glass(), 2.0, None);
        assert_eq!(params.band, 120.0);
        assert!((params.offset - 31.68).abs() < 1e-3);
        assert!((params.dispersion - 0.075).abs() < 1e-6);
        assert_eq!(params.rim_width, 4.0);
        assert_eq!(params.texture, 0.0);
        assert_eq!(params.frost_sigma, glass().frost_sigma(2.0));
    }

    #[test]
    fn params_limit_the_highlight_width() {
        let wide = Glass {
            highlight_width: 24.0,
            depth: 1.0,
            ..glass()
        };
        // Never wider than the band (3 × depth).
        assert_eq!(Params::new(&wide, 1.0, None).rim_width, 3.0);

        let thin = Glass {
            highlight_width: 0.5,
            ..glass()
        };
        // At least one device pixel.
        assert_eq!(Params::new(&thin, 1.0, None).rim_width, 1.0);

        let flat = Glass {
            highlight_width: 2.0,
            depth: 0.0,
            ..glass()
        };
        assert_eq!(Params::new(&flat, 2.0, None).rim_width, 4.0);
    }

    #[test]
    fn params_texture_scales_and_fades() {
        let reeded = Glass {
            texture: GlassTexture::Reeded,
            texture_scale: 8.0,
            texture_amount: 50.0,
            ..glass()
        };
        assert_eq!(Params::new(&reeded, 1.0, None).texture, 4.0);
        // 1.6 px flutes are not drawn.
        assert_eq!(Params::new(&reeded, 0.2, None).texture, 0.0);
        // 3.2 px flutes fade in.
        let fading = Params::new(&reeded, 0.4, None).texture;
        assert!(fading > 0.0 && fading < 0.5 * 2.0 * 3.2 * 0.5);
    }

    #[test]
    fn params_respect_the_reach_budget() {
        let params = Params::new(&glass(), 20.0, Some(64.0));
        assert!(params.offset * (1.0 + params.dispersion) <= 32.0 + 1e-4);
        assert!(params.frost_sigma * 3.0 <= 32.0 + 1e-4);

        let reeded = Glass {
            texture: GlassTexture::Reeded,
            texture_scale: 64.0,
            texture_amount: 100.0,
            ..glass()
        };
        let params = Params::new(&reeded, 20.0, Some(64.0));
        assert!(params.textured() && params.bends());
        assert!((params.offset + params.texture) * (1.0 + params.dispersion) <= 32.0 + 1e-3);
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
            &Glass {
                splay: 100.0,
                ..glass()
            },
            1.0,
            None,
        );
        assert!(soft.profile < sharp.profile);
    }

    #[test]
    fn params_take_the_light_color_and_skip_black() {
        let red = Glass {
            light_color: skia::Color::from_rgb(255, 0, 0),
            ..glass()
        };
        assert_eq!(Params::new(&red, 1.0, None).light_rgb, [1.0, 0.0, 0.0]);

        let black = Glass {
            light_color: skia::Color::BLACK,
            ..glass()
        };
        assert!(!Params::new(&black, 1.0, None).lights());
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
    fn flute_row_measures_across_the_flutes_from_the_origin() {
        let identity = Matrix::new_identity();
        let origin = skia::Point::new(10.0, 20.0);
        // 0°: flutes are vertical, u grows along x.
        let row = flute_row(&identity, origin, 0.0);
        assert!((row[0] - 1.0).abs() < 1e-6 && row[1].abs() < 1e-6);
        assert!((row[2] + 10.0).abs() < 1e-5);
        // 90°: u grows along y.
        let row = flute_row(&identity, origin, 90.0);
        assert!(row[0].abs() < 1e-6 && (row[1] - 1.0).abs() < 1e-6);
        assert!((row[2] + 20.0).abs() < 1e-5);
    }

    #[test]
    fn shaders_compile() {
        with_effects(|effects| assert!(effects.is_some()));
    }

    #[test]
    fn map_declares_every_uniform() {
        with_effects(|effects| {
            let map = &effects.unwrap().map;
            for name in [
                "toLocal",
                "rect",
                "radii",
                "kind",
                "scale",
                "band",
                "rimSigma",
                "stepSize",
                "profile",
                "light",
                "rimWidth",
                "texRow",
                "texDir",
                "texRow2",
                "texDir2",
                "texKind",
                "texPeriod",
                "texEdge",
                "texShare",
                "bevelShare",
            ] {
                assert!(map.find_uniform(name).is_some(), "missing uniform {name}");
            }
        });
    }

    #[test]
    fn noop_glass_has_no_filter() {
        let shape = rect_shape();
        assert!(build_filter(&shape, &inert(), &Matrix::new_identity(), 1.0, None).is_none());
    }

    #[test]
    fn light_only_glass_has_no_backdrop_filter() {
        let shape = rect_shape();
        let lit = Glass {
            light_intensity: 80.0,
            ..inert()
        };
        assert!(!lit.is_noop());
        assert!(build_filter(&shape, &lit, &Matrix::new_identity(), 1.0, None).is_none());
    }

    #[test]
    fn color_or_texture_alone_builds_a_filter() {
        let shape = rect_shape();
        let identity = Matrix::new_identity();

        let gray = Glass {
            saturation: 0.0,
            ..inert()
        };
        assert!(build_filter(&shape, &gray, &identity, 1.0, None).is_some());

        let reeded = Glass {
            texture: GlassTexture::Reeded,
            ..inert()
        };
        assert!(build_filter(&shape, &reeded, &identity, 1.0, None).is_some());

        let flat = Glass {
            texture_amount: 0.0,
            ..reeded
        };
        assert!(build_filter(&shape, &flat, &identity, 1.0, None).is_none());
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
        let mut surface = surface(96, 96, skia::Color::WHITE);
        // Stripes behind the glass so both frost and refraction show.
        let mut paint = Paint::default();
        paint.set_color(skia::Color::BLACK);
        for i in (0..96).step_by(8) {
            let stripe = skia::Rect::from_xywh(i as f32, 0.0, 4.0, 96.0);
            surface.canvas().draw_rect(stripe, &paint);
        }
        let before = surface.image_snapshot();

        let shape = rect_shape();
        let matrix = Matrix::new_identity();
        let canvas = surface.canvas();
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

    fn render_backdrop_over(color: skia::Color, glass: &Glass) -> skia::Color {
        let mut surface = surface(96, 96, color);
        let shape = rect_shape();
        let matrix = Matrix::new_identity();
        render_glass_backdrop(surface.canvas(), &shape, glass, &matrix, 1.0, None);
        pixel(&mut surface, 34, 32)
    }

    #[test]
    fn zero_saturation_turns_the_backdrop_gray() {
        let gray = Glass {
            saturation: 0.0,
            ..inert()
        };
        let result = render_backdrop_over(skia::Color::from_rgb(255, 0, 0), &gray);
        assert_eq!(result.r(), result.g());
        assert_eq!(result.g(), result.b());
        // CSS weight of red.
        assert!((result.r() as i32 - 54).abs() <= 2, "{result:?}");
    }

    #[test]
    fn half_brightness_halves_the_channels() {
        let dim = Glass {
            brightness: 50.0,
            ..inert()
        };
        let result = render_backdrop_over(skia::Color::from_rgb(200, 100, 40), &dim);
        assert!((result.r() as i32 - 100).abs() <= 2, "{result:?}");
        assert!((result.g() as i32 - 50).abs() <= 2, "{result:?}");
        assert!((result.b() as i32 - 20).abs() <= 2, "{result:?}");
    }

    fn render_light_over_fill(glass: &Glass) -> skia::Surface {
        let mut surface = surface(96, 96, skia::Color::WHITE);
        let shape = rect_shape();
        let mut fill = Paint::default();
        fill.set_color(skia::Color::from_rgb(32, 32, 32));
        surface.canvas().draw_rect(shape.selrect, &fill);

        let matrix = Matrix::new_identity();
        render_glass_light(surface.canvas(), &shape, glass, &matrix, 1.0);
        surface
    }

    #[test]
    fn light_brightens_the_rim_over_an_opaque_fill() {
        let lit = Glass {
            light_intensity: 100.0,
            ..inert()
        };
        let mut surface = render_light_over_fill(&lit);

        let rim = pixel(&mut surface, 0, 32);
        assert!(rim.r() > 90, "rim {rim:?}");
        // The center and the outside are untouched.
        assert_eq!(
            pixel(&mut surface, 34, 32),
            skia::Color::from_rgb(32, 32, 32)
        );
        assert_eq!(pixel(&mut surface, 80, 80), skia::Color::WHITE);
    }

    #[test]
    fn light_color_tints_the_rim() {
        let red = Glass {
            light_intensity: 100.0,
            light_color: skia::Color::from_rgb(255, 0, 0),
            ..inert()
        };
        let mut surface = render_light_over_fill(&red);
        let rim = pixel(&mut surface, 0, 32);
        assert!(rim.r() > 90, "rim {rim:?}");
        assert!(rim.g() < 40 && rim.b() < 40, "rim {rim:?}");
    }

    #[test]
    fn black_light_draws_nothing() {
        let black = Glass {
            light_intensity: 100.0,
            light_color: skia::Color::BLACK,
            ..inert()
        };
        let mut surface = render_light_over_fill(&black);
        assert_eq!(
            pixel(&mut surface, 0, 32),
            skia::Color::from_rgb(32, 32, 32)
        );
    }

    #[test]
    fn rect_bevel_has_no_diagonal_seam() {
        let mut shape = rect_shape();
        shape.set_selrect(0.0, 0.0, 200.0, 200.0);
        let glass = Glass {
            depth: 30.0,
            dispersion: 0.0,
            frost: 0.0,
            ..glass()
        };
        let mut map = render_map(&shape, &glass, 200, 200);

        // Both points sit next to the corner diagonal, on either side of it.
        // A mitered bevel bends one sideways and the other up; a smooth one
        // bends both the same way.
        let a = pixel(&mut map, 20, 22);
        let b = pixel(&mut map, 22, 20);
        assert!(a.r() > 140 && a.g() > 140, "inward offset {a:?}");
        assert!((a.r() as i32 - b.g() as i32).abs() <= 2);
        assert!((a.r() as i32 - b.r() as i32).abs() <= 12, "{a:?} vs {b:?}");
    }

    fn reeded(angle: f32) -> Glass {
        Glass {
            texture: GlassTexture::Reeded,
            texture_amount: 100.0,
            texture_scale: 10.0,
            texture_angle: angle,
            ..inert()
        }
    }

    fn wide_rect() -> Shape {
        let mut shape = rect_shape();
        shape.set_selrect(0.0, 0.0, 200.0, 40.0);
        shape
    }

    #[test]
    fn reeded_map_repeats_per_flute() {
        let mut map = render_map(&wide_rect(), &reeded(0.0), 200, 40);
        let row: Vec<skia::Color> = (0..200).map(|x| pixel(&mut map, x, 20)).collect();

        for x in 20..170 {
            assert!(
                (row[x].r() as i32 - row[x + 10].r() as i32).abs() <= 1,
                "not periodic at {x}"
            );
            assert!(
                (row[x].g() as i32 - 128).abs() <= 1,
                "vertical offset at {x}"
            );
        }
        let min = row.iter().map(|c| c.r()).min().unwrap();
        let max = row.iter().map(|c| c.r()).max().unwrap();
        assert!(max - min > 100, "flutes too flat: {min}..{max}");
    }

    #[test]
    fn texture_angle_turns_the_flutes() {
        let mut shape = rect_shape();
        shape.set_selrect(0.0, 0.0, 40.0, 200.0);
        let mut map = render_map(&shape, &reeded(90.0), 40, 200);
        let column: Vec<skia::Color> = (0..200).map(|y| pixel(&mut map, 20, y)).collect();

        assert!(column.iter().all(|c| (c.r() as i32 - 128).abs() <= 1));
        let min = column.iter().map(|c| c.g()).min().unwrap();
        let max = column.iter().map(|c| c.g()).max().unwrap();
        assert!(max - min > 100, "flutes too flat: {min}..{max}");
    }

    #[test]
    fn texture_moves_with_the_shape() {
        let mut moved = wide_rect();
        moved.set_selrect(37.0, 0.0, 237.0, 40.0);

        let mut a = render_map(&wide_rect(), &reeded(0.0), 300, 40);
        let mut b = render_map(&moved, &reeded(0.0), 300, 40);
        for x in 20..150 {
            let expected = pixel(&mut a, x, 20).r() as i32;
            let actual = pixel(&mut b, x + 37, 20).r() as i32;
            assert!((expected - actual).abs() <= 1, "flute moved at {x}");
        }
    }

    #[test]
    fn reeded_glass_repeats_the_backdrop() {
        let mut surface = surface(200, 60, skia::Color::WHITE);
        let mut bar = Paint::default();
        bar.set_color(skia::Color::BLACK);
        surface
            .canvas()
            .draw_rect(skia::Rect::from_xywh(100.0, 0.0, 4.0, 60.0), &bar);

        let mut shape = rect_shape();
        shape.set_selrect(0.0, 0.0, 200.0, 60.0);
        let glass = Glass {
            texture_scale: 20.0,
            ..reeded(0.0)
        };
        let matrix = Matrix::new_identity();
        render_glass_backdrop(surface.canvas(), &shape, &glass, &matrix, 1.0, None);

        let dark_flutes: std::collections::HashSet<i32> = (0..200)
            .filter(|x| pixel(&mut surface, *x, 30).r() < 128)
            .map(|x| x / 20)
            .collect();
        assert!(dark_flutes.len() >= 2, "bar seen in {dark_flutes:?}");
    }

    fn strips(kind: GlassTexture) -> Glass {
        Glass {
            texture: kind,
            texture_amount: 100.0,
            texture_scale: 20.0,
            ..inert()
        }
    }

    /// Sideways offsets over one 20 px strip, sampled in the middle of a wide
    /// rect. 0 means the texture bends nothing there.
    fn strip_period(kind: GlassTexture) -> Vec<i32> {
        let mut map = render_map(&wide_rect(), &strips(kind), 200, 40);
        (100..120)
            .map(|x| pixel(&mut map, x, 20).r() as i32 - 128)
            .collect()
    }

    /// Offset a flat facet would have at `index`, the strip being 20 px wide.
    fn facet_at(index: usize) -> i32 {
        (127.5 * ((index as f32 + 0.5) / 10.0 - 1.0)) as i32
    }

    fn peak_of(row: &[i32]) -> usize {
        (0..row.len()).max_by_key(|i| row[*i].abs()).unwrap()
    }

    #[test]
    fn prismatic_strips_are_flat_facets() {
        let row = strip_period(GlassTexture::Prismatic);
        // Away from the seams the offset grows linearly across the facet.
        for i in 2..18 {
            assert!(
                (row[i] - facet_at(i)).abs() <= 4,
                "not a facet at {i}: {row:?}"
            );
        }
    }

    #[test]
    fn reeded_strips_bend_like_a_lens() {
        let row = strip_period(GlassTexture::Reeded);
        // A lens is flatter than a facet in the middle and steepest next to
        // the seam, where the smoothing takes over.
        assert!(row[5].abs() < (facet_at(5).abs() * 4) / 5, "{row:?}");
        let peak = peak_of(&row);
        assert!(peak <= 4 || peak >= 15, "peak at {peak}: {row:?}");
    }

    #[test]
    fn wavy_strips_bend_most_in_their_middle() {
        let row = strip_period(GlassTexture::Wavy);
        let peak = peak_of(&row);
        assert!(
            (4..=6).contains(&peak) || (14..=16).contains(&peak),
            "{row:?}"
        );
        // A sine runs through zero at the seam, so it needs no smoothing, and
        // it bends to both sides inside one strip.
        assert!(row[0].abs() * 3 < row[peak].abs(), "{row:?}");
        assert!(row[5] * row[15] < 0, "{row:?}");
    }

    fn square_shape() -> Shape {
        let mut shape = rect_shape();
        shape.set_selrect(0.0, 0.0, 200.0, 200.0);
        shape
    }

    #[test]
    fn cross_reeded_bends_both_axes() {
        let mut map = render_map(
            &square_shape(),
            &strips(GlassTexture::CrossReeded),
            200,
            200,
        );
        let across: Vec<i32> = (100..120)
            .map(|x| pixel(&mut map, x, 110).r() as i32 - 128)
            .collect();
        let along: Vec<i32> = (100..120)
            .map(|y| pixel(&mut map, 110, y).g() as i32 - 128)
            .collect();

        for row in [&across, &along] {
            let span = row.iter().max().unwrap() - row.iter().min().unwrap();
            assert!(span > 60, "axis too flat: {row:?}");
        }
    }

    #[test]
    fn hammered_dents_bend_in_every_direction_and_leave_gaps() {
        let mut map = render_map(&square_shape(), &strips(GlassTexture::Hammered), 200, 200);
        let image = map.image_snapshot();
        let pixels = image.peek_pixels().unwrap();
        let samples: Vec<(i32, i32)> = (40..160)
            .step_by(2)
            .flat_map(|y| (40..160).step_by(2).map(move |x| (x, y)))
            .map(|(x, y)| {
                let c = pixels.get_color((x, y));
                (c.r() as i32 - 128, c.g() as i32 - 128)
            })
            .collect();

        let span = |f: fn(&(i32, i32)) -> i32| {
            samples.iter().map(f).max().unwrap() - samples.iter().map(f).min().unwrap()
        };
        assert!(span(|s| s.0) > 60, "no sideways bend");
        assert!(span(|s| s.1) > 60, "no vertical bend");

        // The dents stay inside their grid cell, so the glass between them
        // shows the backdrop untouched.
        let flat = samples
            .iter()
            .filter(|(dx, dy)| dx.abs() <= 2 && dy.abs() <= 2)
            .count();
        assert!(flat * 5 > samples.len(), "dents leave no gaps: {flat}");
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
        let mut surface = surface(100, 100, skia::Color::TRANSPARENT);
        let mut paint = Paint::default();
        paint.set_shader(mask);
        surface.canvas().draw_paint(&paint);

        // Deep inside both channels are full, on the edge about half, and
        // outside the shape empty. Row 0 is half a pixel inside the edge,
        // which shows more in the narrow red blur.
        let inside = pixel(&mut surface, 50, 30);
        assert!(inside.g() > 240 && inside.r() > 240, "{inside:?}");
        let edge = pixel(&mut surface, 50, 0);
        assert!((100..160).contains(&edge.g()), "edge {edge:?}");
        assert!((150..200).contains(&edge.r()), "edge {edge:?}");
        let near = pixel(&mut surface, 50, 4);
        assert!(near.r() > 250 && near.g() < 200, "near {near:?}");
        let outside = pixel(&mut surface, 5, 90);
        assert_eq!((outside.r(), outside.g()), (0, 0));
    }

    /// Board with black stripes and a panel on top.
    fn glass_scene(glass: Option<Glass>, panel: skia::Color) -> (crate::state::ShapesPool, Uuid) {
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
        let shape = pool.add_shape(panel_id);
        shape.set_parent(board);
        shape.set_shape_type(Type::Rect(RectType::default()));
        shape.set_selrect(20.0, 20.0, 80.0, 80.0);
        shape.set_fills(vec![Fill::Solid(SolidColor(panel))]);
        shape.set_glass(glass);
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

    fn tint() -> skia::Color {
        skia::Color::from_argb(51, 217, 217, 217)
    }

    fn export_pixels(glass: Option<Glass>, panel: skia::Color) -> skia::Image {
        let (pool, board) = glass_scene(glass, panel);
        let mut resources = super::super::RenderResources::try_new_headless().unwrap();
        let _guard = crate::globals::TestRenderResourcesGuard::install(&mut resources);
        let mut surface = surface(100, 100, skia::Color::TRANSPARENT);
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
        let plain = export_pixels(None, tint());
        let glassy = export_pixels(Some(glass()), tint());
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
    fn export_light_is_drawn_over_the_fill() {
        let panel = skia::Color::from_rgb(32, 32, 32);
        let lit = Glass {
            light_intensity: 100.0,
            ..inert()
        };
        let plain = export_pixels(None, panel);
        let glassy = export_pixels(Some(lit), panel);

        let rim = glassy.peek_pixels().unwrap().get_color((20, 50));
        let fill = plain.peek_pixels().unwrap().get_color((20, 50));
        assert_eq!(fill, panel);
        assert!(rim.r() > fill.r() + 40, "rim {rim:?}");
    }

    #[test]
    fn pdf_export_embeds_the_glass() {
        let render = |glass: Option<Glass>| {
            let (pool, board) = glass_scene(glass, tint());
            let mut resources = super::super::RenderResources::try_new_headless().unwrap();
            let _guard = crate::globals::TestRenderResourcesGuard::install(&mut resources);
            super::super::pdf::render_to_pdf(&mut resources, &board, &pool, 1.0).unwrap()
        };
        let count = |pdf: &[u8], needle: &str| String::from_utf8_lossy(pdf).matches(needle).count();

        // PDF has no backdrop filters, so the glass backdrop is embedded as
        // an image, and the light as an image drawn with a Screen blend.
        let unlit = Glass {
            light_intensity: 0.0,
            ..glass()
        };
        let none = render(None);
        let backdrop = render(Some(unlit));
        let with_light = render(Some(glass()));

        assert_eq!(count(&none, "/Subtype /Image"), 0);
        assert!(count(&backdrop, "/Subtype /Image") > 0);
        assert_eq!(count(&backdrop, "/Screen"), 0);
        assert!(count(&with_light, "/Screen") > 0);
    }
}
