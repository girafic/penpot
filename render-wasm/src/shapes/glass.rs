/// Blur sigma per frost unit. Frost is stronger than a blur of the same
/// value, so low values already give a clear frosted look.
const FROST_SIGMA_PER_UNIT: f32 = 1.5;

/// Glass backdrop effect. It refracts, disperses and frosts the content
/// behind the shape and adds a light highlight along its edges.
///
/// Units follow the frontend data model:
/// - `light_angle`: degrees
/// - `light_intensity`, `refraction`, `dispersion`, `splay`: 0..100
/// - `depth`, `frost`: document px (scaled by zoom when rendering)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Glass {
    pub hidden: bool,
    pub light_angle: f32,
    pub light_intensity: f32,
    pub refraction: f32,
    pub depth: f32,
    pub dispersion: f32,
    pub frost: f32,
    pub splay: f32,
}

impl Glass {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        hidden: bool,
        light_angle: f32,
        light_intensity: f32,
        refraction: f32,
        depth: f32,
        dispersion: f32,
        frost: f32,
        splay: f32,
    ) -> Self {
        Glass {
            hidden,
            light_angle: sanitize(light_angle),
            light_intensity: percent(light_intensity),
            refraction: percent(refraction),
            depth: sanitize(depth).max(0.0),
            dispersion: percent(dispersion),
            frost: sanitize(frost).max(0.0),
            splay: percent(splay),
        }
    }

    pub fn scale_content(&mut self, value: f32) {
        self.depth *= value;
        self.frost *= value;
    }

    /// Blur sigma for the frost at the given zoom scale.
    #[inline]
    pub fn frost_sigma(&self, scale: f32) -> f32 {
        self.frost * FROST_SIGMA_PER_UNIT * scale
    }

    /// True when the effect changes nothing (no bend, no frost, no light).
    pub fn is_noop(&self) -> bool {
        (self.refraction <= 0.0 || self.depth <= 0.0)
            && self.frost <= 0.0
            && self.light_intensity <= 0.0
    }
}

#[inline]
fn sanitize(value: f32) -> f32 {
    if value.is_finite() {
        value
    } else {
        0.0
    }
}

#[inline]
fn percent(value: f32) -> f32 {
    sanitize(value).clamp(0.0, 100.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_clamps_percent_values() {
        let glass = Glass::new(false, -45.0, 180.0, -3.0, 20.0, 50.0, 4.0, 101.0);
        assert_eq!(glass.light_intensity, 100.0);
        assert_eq!(glass.refraction, 0.0);
        assert_eq!(glass.dispersion, 50.0);
        assert_eq!(glass.splay, 100.0);
        assert_eq!(glass.light_angle, -45.0);
    }

    #[test]
    fn new_rejects_negative_and_non_finite_lengths() {
        let glass = Glass::new(false, f32::NAN, 80.0, 80.0, -5.0, 50.0, f32::INFINITY, 0.0);
        assert_eq!(glass.light_angle, 0.0);
        assert_eq!(glass.depth, 0.0);
        assert_eq!(glass.frost, 0.0);
    }

    #[test]
    fn scale_content_scales_lengths_only() {
        let mut glass = Glass::new(false, -45.0, 80.0, 80.0, 20.0, 50.0, 4.0, 0.0);
        glass.scale_content(2.0);
        assert_eq!(glass.depth, 40.0);
        assert_eq!(glass.frost, 8.0);
        assert_eq!(glass.refraction, 80.0);
        assert_eq!(glass.light_intensity, 80.0);
    }

    #[test]
    fn frost_sigma_uses_zoom_scale() {
        let glass = Glass::new(false, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0);
        assert_eq!(glass.frost_sigma(2.0), 12.0);
        let no_frost = Glass::new(false, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
        assert_eq!(no_frost.frost_sigma(2.0), 0.0);
    }

    #[test]
    fn is_noop_only_when_nothing_is_visible() {
        assert!(Glass::new(false, 0.0, 0.0, 80.0, 0.0, 0.0, 0.0, 0.0).is_noop());
        assert!(!Glass::new(false, 0.0, 0.0, 80.0, 20.0, 0.0, 0.0, 0.0).is_noop());
        assert!(!Glass::new(false, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0).is_noop());
        assert!(!Glass::new(false, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0).is_noop());
    }
}
