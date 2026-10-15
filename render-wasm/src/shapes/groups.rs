use skia_safe as skia;

/// How the mask of a masked group, its bottom-most child, keeps the
/// layers the group holds, as the track mattes of After Effects.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MaskMode {
    /// By the opacity of the mask.
    #[default]
    Alpha,
    /// Where the mask is not.
    AlphaInverted,
    /// By the brightness of the mask.
    Luma,
    /// By the darkness of the mask.
    LumaInverted,
}

impl MaskMode {
    /// The mode of the value the frontend sends (`props/mask-value`):
    /// `None` for 0, a group that is not masked.
    pub fn from_value(value: u8) -> Option<MaskMode> {
        match value {
            0 => None,
            2 => Some(MaskMode::AlphaInverted),
            3 => Some(MaskMode::Luma),
            4 => Some(MaskMode::LumaInverted),
            _ => Some(MaskMode::Alpha),
        }
    }

    /// Whether the mask keeps what is out of it.
    pub fn inverted(&self) -> bool {
        matches!(self, MaskMode::AlphaInverted | MaskMode::LumaInverted)
    }

    /// Whether the brightness of the mask keeps, not its opacity.
    pub fn luma(&self) -> bool {
        matches!(self, MaskMode::Luma | MaskMode::LumaInverted)
    }

    /// The paint of the layer of the mask, which it draws over what the
    /// group holds: keeping it where the mask is (`DstIn`) or where it
    /// is not (`DstOut`), by the brightness of the mask with `luma`.
    pub fn paint(&self) -> skia::Paint {
        let mut paint = skia::Paint::default();
        paint.set_blend_mode(if self.inverted() {
            skia::BlendMode::DstOut
        } else {
            skia::BlendMode::DstIn
        });
        if self.luma() {
            paint.set_color_filter(skia::ColorFilter::luma());
        }
        paint
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Group {
    pub masked: bool,
    pub mask_mode: MaskMode,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_mask_value_gives_the_mode() {
        assert_eq!(MaskMode::from_value(0), None);
        assert_eq!(MaskMode::from_value(1), Some(MaskMode::Alpha));
        assert_eq!(MaskMode::from_value(2), Some(MaskMode::AlphaInverted));
        assert_eq!(MaskMode::from_value(3), Some(MaskMode::Luma));
        assert_eq!(MaskMode::from_value(4), Some(MaskMode::LumaInverted));
    }

    #[test]
    fn the_paint_keeps_where_the_mask_is_or_is_not() {
        let alpha = MaskMode::Alpha.paint();
        assert_eq!(
            alpha.blend_mode_or(skia::BlendMode::Clear),
            skia::BlendMode::DstIn
        );
        assert!(alpha.color_filter().is_none());

        let inverted = MaskMode::AlphaInverted.paint();
        assert_eq!(
            inverted.blend_mode_or(skia::BlendMode::Clear),
            skia::BlendMode::DstOut
        );

        let luma = MaskMode::LumaInverted.paint();
        assert_eq!(
            luma.blend_mode_or(skia::BlendMode::Clear),
            skia::BlendMode::DstOut
        );
        assert!(
            luma.color_filter().is_some(),
            "by the brightness of the mask"
        );
    }

    /// The pixels in the middle and in a corner of a red square kept by
    /// a mask of `color` over its middle, as the renderer draws it.
    fn kept(mode: MaskMode, color: skia::Color) -> (skia::Color, skia::Color) {
        let mut surface = skia::surfaces::raster_n32_premul((100, 100)).expect("surface");
        let canvas = surface.canvas();
        canvas.save_layer(&skia::canvas::SaveLayerRec::default());
        let mut red = skia::Paint::default();
        red.set_color(skia::Color::RED);
        canvas.draw_rect(skia::Rect::from_xywh(0.0, 0.0, 100.0, 100.0), &red);
        let paint = mode.paint();
        canvas.save_layer(&skia::canvas::SaveLayerRec::default().paint(&paint));
        let mut mask = skia::Paint::default();
        mask.set_color(color);
        canvas.draw_rect(skia::Rect::from_xywh(25.0, 25.0, 50.0, 50.0), &mask);
        canvas.restore();
        canvas.restore();
        let image = surface.image_snapshot();
        let pixmap = image.peek_pixels().expect("pixels");
        (pixmap.get_color((50, 50)), pixmap.get_color((5, 5)))
    }

    #[test]
    fn each_mode_keeps_what_its_matte_keeps() {
        let red = skia::Color::RED;
        let none = skia::Color::TRANSPARENT;
        let white = skia::Color::WHITE;
        let black = skia::Color::BLACK;
        assert_eq!(
            kept(MaskMode::Alpha, white),
            (red, none),
            "where the mask is"
        );
        assert_eq!(
            kept(MaskMode::Alpha, black),
            (red, none),
            "whatever its colour"
        );
        assert_eq!(
            kept(MaskMode::AlphaInverted, white),
            (none, red),
            "where it is not"
        );
        assert_eq!(
            kept(MaskMode::Luma, white),
            (red, none),
            "where it is bright"
        );
        assert_eq!(
            kept(MaskMode::Luma, black),
            (none, none),
            "not where it is dark"
        );
        assert_eq!(
            kept(MaskMode::LumaInverted, black),
            (red, red),
            "where it is dark"
        );
        assert_eq!(kept(MaskMode::LumaInverted, white), (none, red));
    }
}
