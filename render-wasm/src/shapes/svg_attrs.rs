use crate::wasm::svg_attrs::{RawFillRule, RawStrokeLineCap, RawStrokeLineJoin};
use skia_safe as skia;

#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub enum FillRule {
    #[default]
    Nonzero,
    Evenodd,
}

#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub enum StrokeLineCap {
    #[default]
    Butt,
    Round,
    Square,
}

#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub enum StrokeLineJoin {
    #[default]
    Miter,
    Round,
    Bevel,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SvgAttrs {
    pub fill_rule: FillRule,
    pub stroke_linecap: StrokeLineCap,
    pub stroke_linejoin: StrokeLineJoin,
    /// Indicates that this shape has an explicit `fill="none"` attribute.
    ///
    /// In SVG, the `fill` attribute is inheritable from container elements like `<g>`.
    /// However, when a shape explicitly sets `fill="none"`, it breaks the color
    /// inheritance chain - the shape will not inherit fill colors from parent containers.
    ///
    /// This is different from having an empty fills array, as it explicitly signals
    /// the intention to have no fill, preventing inheritance.
    pub fill_none: bool,
    /// The `stroke-dasharray` of an imported SVG shape whose dashes Penpot's
    /// stroke styles cannot draw (any pattern but a dash and a gap, or
    /// dots, or one with an offset), empty for none, and its
    /// `stroke-dashoffset`: drawn as SVG draws them (see `dash_effect`).
    pub stroke_dasharray: Vec<f32>,
    pub stroke_dashoffset: f32,
}

impl Default for SvgAttrs {
    fn default() -> Self {
        Self {
            fill_rule: FillRule::Nonzero,
            stroke_linecap: StrokeLineCap::Butt,
            stroke_linejoin: StrokeLineJoin::Miter,
            fill_none: false,
            stroke_dasharray: Vec::new(),
            stroke_dashoffset: 0.0,
        }
    }
}

impl SvgAttrs {
    pub fn from_raw(
        fill_rule: u8,
        stroke_linecap: u8,
        stroke_linejoin: u8,
        fill_none: bool,
    ) -> Self {
        Self {
            fill_rule: RawFillRule::from(fill_rule).into(),
            stroke_linecap: RawStrokeLineCap::from(stroke_linecap).into(),
            stroke_linejoin: RawStrokeLineJoin::from(stroke_linejoin).into(),
            fill_none,
            ..Self::default()
        }
    }

    /// The dash `PathEffect` of `stroke_dasharray` from `stroke_dashoffset`,
    /// as SVG draws it: a list of an odd count repeats. None for no list, or
    /// one with a negative length or that is all gaps, which SVG draws as a
    /// line.
    pub fn dash_effect(&self) -> Option<skia::PathEffect> {
        let lengths = &self.stroke_dasharray;
        if lengths.is_empty()
            || lengths.iter().any(|v| *v < 0.0 || !v.is_finite())
            || lengths.iter().sum::<f32>() <= 0.0
        {
            return None;
        }
        let intervals: Vec<f32> = if lengths.len() % 2 == 1 {
            lengths.iter().chain(lengths.iter()).copied().collect()
        } else {
            lengths.clone()
        };
        skia::PathEffect::dash(&intervals, self.stroke_dashoffset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dashed(lengths: Vec<f32>) -> SvgAttrs {
        SvgAttrs {
            stroke_dasharray: lengths,
            ..SvgAttrs::default()
        }
    }

    #[test]
    fn a_dash_array_dashes() {
        assert!(dashed(vec![12.0, 4.0, 2.0, 4.0]).dash_effect().is_some());
        // an odd count repeats, as in SVG
        assert!(dashed(vec![1.0, 2.0, 3.0]).dash_effect().is_some());
    }

    #[test]
    fn no_dash_array_draws_a_line() {
        assert!(SvgAttrs::default().dash_effect().is_none());
        assert!(dashed(vec![0.0, 0.0]).dash_effect().is_none());
        assert!(dashed(vec![4.0, -1.0]).dash_effect().is_none());
    }
}
