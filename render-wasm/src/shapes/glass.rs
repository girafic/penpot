use skia_safe as skia;

use super::fills::{Fill, SolidColor};

/// Blur sigma per frost unit. Frost is stronger than a blur of the same
/// value, so low values already give a clear frosted look.
const FROST_SIGMA_PER_UNIT: f32 = 1.5;

/// Surface texture of the glass.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum GlassTexture {
    #[default]
    None,
    /// Parallel lens strips (fluted glass).
    Reeded,
    /// Parallel waves; the profile is smooth, so the strips have no seams.
    Wavy,
    /// Parallel prisms: flat facets with a hard seam between them.
    Prismatic,
    /// Lens strips on two perpendicular axes.
    CrossReeded,
    /// Round dents on a jittered grid (hammered glass).
    Hammered,
}

/// Glass backdrop effect. It refracts, disperses and frosts the content
/// behind the shape and adds a light highlight along its edges.
///
/// Units follow the frontend data model:
/// - `light_angle`, `texture_angle`: degrees
/// - `light_intensity`, `refraction`, `dispersion`, `splay`,
///   `texture_amount`: 0..100
/// - `saturation`, `brightness`: 0..200, 100 leaves the backdrop unchanged
/// - `depth`, `frost`, `highlight_width`, `texture_scale`: document px
///   (scaled by zoom when rendering)
///
/// Build values as a struct literal (`..Glass::default()`) and pass them
/// through [`Glass::sanitized`].
#[derive(Debug, Clone, PartialEq)]
pub struct Glass {
    pub hidden: bool,
    pub light_angle: f32,
    pub light_intensity: f32,
    /// Paint of the edge highlight: a solid color or a gradient.
    pub light: Fill,
    pub highlight_width: f32,
    pub refraction: f32,
    pub depth: f32,
    pub dispersion: f32,
    pub frost: f32,
    pub splay: f32,
    pub saturation: f32,
    pub brightness: f32,
    pub texture: GlassTexture,
    pub texture_amount: f32,
    pub texture_scale: f32,
    pub texture_angle: f32,
}

impl Default for Glass {
    /// The frontend defaults for a new glass effect.
    fn default() -> Self {
        Glass {
            hidden: false,
            light_angle: -45.0,
            light_intensity: 80.0,
            light: Fill::Solid(SolidColor(skia::Color::WHITE)),
            highlight_width: 2.0,
            refraction: 80.0,
            depth: 20.0,
            dispersion: 50.0,
            frost: 4.0,
            splay: 0.0,
            saturation: 100.0,
            brightness: 100.0,
            texture: GlassTexture::None,
            texture_amount: 30.0,
            texture_scale: 8.0,
            texture_angle: 0.0,
        }
    }
}

impl Glass {
    /// Clamps every value to its valid range. Non-finite values fall back
    /// to a neutral value.
    pub fn sanitized(self) -> Self {
        let light = match self.light {
            // A solid light is always opaque; its strength is the light
            // intensity. An image cannot light anything, so it reads as white.
            Fill::Solid(SolidColor(c)) => {
                Fill::Solid(SolidColor(skia::Color::from_argb(255, c.r(), c.g(), c.b())))
            }
            Fill::Image(_) => Fill::Solid(SolidColor(skia::Color::WHITE)),
            gradient => gradient,
        };
        Glass {
            hidden: self.hidden,
            light_angle: finite_or(self.light_angle, 0.0),
            light_intensity: percent(self.light_intensity),
            light,
            highlight_width: finite_or(self.highlight_width, 2.0).max(0.0),
            refraction: percent(self.refraction),
            depth: finite_or(self.depth, 0.0).max(0.0),
            dispersion: percent(self.dispersion),
            frost: finite_or(self.frost, 0.0).max(0.0),
            splay: percent(self.splay),
            saturation: finite_or(self.saturation, 100.0).clamp(0.0, 200.0),
            brightness: finite_or(self.brightness, 100.0).clamp(0.0, 200.0),
            texture: self.texture,
            texture_amount: percent(self.texture_amount),
            texture_scale: finite_or(self.texture_scale, 8.0).max(0.0),
            texture_angle: finite_or(self.texture_angle, 0.0),
        }
    }

    pub fn scale_content(&mut self, value: f32) {
        self.depth *= value;
        self.frost *= value;
        self.highlight_width *= value;
        self.texture_scale *= value;
    }

    /// Blur sigma for the frost at the given zoom scale.
    #[inline]
    pub fn frost_sigma(&self, scale: f32) -> f32 {
        self.frost * FROST_SIGMA_PER_UNIT * scale
    }

    /// True when saturation or brightness change the backdrop.
    pub fn adjusts_color(&self) -> bool {
        (self.saturation - 100.0).abs() > 0.01 || (self.brightness - 100.0).abs() > 0.01
    }

    /// True when the surface texture is visible.
    pub fn has_texture(&self) -> bool {
        self.texture != GlassTexture::None && self.texture_amount > 0.0 && self.texture_scale > 0.0
    }

    /// True when the light draws nothing: no intensity, or a paint that
    /// adds nothing in the screen blend.
    pub fn is_dark(&self) -> bool {
        self.light_intensity <= 0.0 || fill_is_dark(&self.light)
    }

    /// True when the effect changes nothing.
    pub fn is_noop(&self) -> bool {
        (self.refraction <= 0.0 || self.depth <= 0.0)
            && self.frost <= 0.0
            && self.is_dark()
            && !self.adjusts_color()
            && !self.has_texture()
    }
}

/// True when a light paint adds nothing in a screen blend: every color it
/// paints is black or fully transparent.
fn fill_is_dark(fill: &Fill) -> bool {
    let dark = |color: &skia::Color| {
        color.a() == 0 || (color.r() == 0 && color.g() == 0 && color.b() == 0)
    };
    match fill {
        Fill::Solid(SolidColor(color)) => dark(color),
        Fill::LinearGradient(gradient) | Fill::RadialGradient(gradient) => {
            gradient.opacity() == 0 || gradient.stops().all(|(color, _)| dark(&color))
        }
        Fill::Image(_) => true,
    }
}

#[inline]
fn finite_or(value: f32, fallback: f32) -> f32 {
    if value.is_finite() {
        value
    } else {
        fallback
    }
}

#[inline]
fn percent(value: f32) -> f32 {
    finite_or(value, 0.0).clamp(0.0, 100.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shapes::fills::Gradient;

    fn glass(f: impl FnOnce(&mut Glass)) -> Glass {
        let mut glass = Glass::default();
        f(&mut glass);
        glass.sanitized()
    }

    #[test]
    fn sanitized_clamps_percent_values() {
        let glass = glass(|g| {
            g.light_intensity = 180.0;
            g.refraction = -3.0;
            g.splay = 101.0;
            g.texture_amount = 140.0;
        });
        assert_eq!(glass.light_intensity, 100.0);
        assert_eq!(glass.refraction, 0.0);
        assert_eq!(glass.dispersion, 50.0);
        assert_eq!(glass.splay, 100.0);
        assert_eq!(glass.texture_amount, 100.0);
        assert_eq!(glass.light_angle, -45.0);
    }

    #[test]
    fn sanitized_rejects_negative_and_non_finite_lengths() {
        let glass = glass(|g| {
            g.light_angle = f32::NAN;
            g.depth = -5.0;
            g.frost = f32::INFINITY;
            g.highlight_width = -1.0;
            g.texture_scale = f32::NAN;
        });
        assert_eq!(glass.light_angle, 0.0);
        assert_eq!(glass.depth, 0.0);
        assert_eq!(glass.frost, 0.0);
        assert_eq!(glass.highlight_width, 0.0);
        assert_eq!(glass.texture_scale, 8.0);
    }

    #[test]
    fn sanitized_limits_color_adjustments() {
        let adjusted = glass(|g| {
            g.saturation = 250.0;
            g.brightness = f32::NAN;
        });
        assert_eq!(adjusted.saturation, 200.0);
        assert_eq!(adjusted.brightness, 100.0);
        assert_eq!(glass(|g| g.saturation = -1.0).saturation, 0.0);
    }

    #[test]
    fn sanitized_makes_the_light_color_opaque() {
        let glass =
            glass(|g| g.light = Fill::Solid(SolidColor(skia::Color::from_argb(10, 255, 0, 0))));
        assert_eq!(
            glass.light,
            Fill::Solid(SolidColor(skia::Color::from_argb(255, 255, 0, 0)))
        );
    }

    #[test]
    fn scale_content_scales_lengths_only() {
        let mut glass = Glass::default();
        glass.scale_content(2.0);
        assert_eq!(glass.depth, 40.0);
        assert_eq!(glass.frost, 8.0);
        assert_eq!(glass.highlight_width, 4.0);
        assert_eq!(glass.texture_scale, 16.0);
        assert_eq!(glass.refraction, 80.0);
        assert_eq!(glass.light_intensity, 80.0);
        assert_eq!(glass.saturation, 100.0);
        assert_eq!(glass.texture_amount, 30.0);
        assert_eq!(glass.texture_angle, 0.0);
    }

    #[test]
    fn frost_sigma_uses_zoom_scale() {
        let frosted = glass(|g| g.frost = 4.0);
        assert_eq!(frosted.frost_sigma(2.0), 12.0);
        let clear = glass(|g| g.frost = 0.0);
        assert_eq!(clear.frost_sigma(2.0), 0.0);
    }

    fn inert() -> Glass {
        glass(|g| {
            g.light_intensity = 0.0;
            g.refraction = 0.0;
            g.depth = 0.0;
            g.frost = 0.0;
        })
    }

    #[test]
    fn is_noop_only_when_nothing_is_visible() {
        assert!(inert().is_noop());

        let mut bent = inert();
        bent.refraction = 80.0;
        assert!(bent.is_noop(), "refraction needs depth");
        bent.depth = 20.0;
        assert!(!bent.is_noop());

        let mut frosted = inert();
        frosted.frost = 2.0;
        assert!(!frosted.is_noop());

        let mut lit = inert();
        lit.light_intensity = 10.0;
        assert!(!lit.is_noop());
        lit.light = Fill::Solid(SolidColor(skia::Color::BLACK));
        assert!(lit.is_noop(), "a black light draws nothing");

        let mut gradient_lit = inert();
        gradient_lit.light_intensity = 10.0;
        gradient_lit.light = Fill::LinearGradient(Gradient::new(
            (0.0, 0.0),
            (1.0, 0.0),
            255,
            0.0,
            &[
                (skia::Color::BLACK, 0.0),
                (skia::Color::from_rgb(255, 0, 0), 1.0),
            ],
        ));
        assert!(!gradient_lit.is_noop(), "a lit gradient draws something");

        let mut saturated = inert();
        saturated.saturation = 50.0;
        assert!(!saturated.is_noop());

        let mut brighter = inert();
        brighter.brightness = 150.0;
        assert!(!brighter.is_noop());

        let mut reeded = inert();
        reeded.texture = GlassTexture::Reeded;
        assert!(!reeded.is_noop());
        reeded.texture_amount = 0.0;
        assert!(reeded.is_noop());

        let mut none = inert();
        none.texture_amount = 30.0;
        assert!(none.is_noop(), "an amount without texture changes nothing");
    }
}
