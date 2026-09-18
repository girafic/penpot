use macros::{wasm_error, ToJs};

use crate::error::{Error, Result};
use crate::mem;
use crate::shapes::{Glass, GlassTexture};
use crate::wasm::fills::{RawFillData, RAW_FILL_DATA_SIZE};
use crate::with_current_shape_mut;

/// Offset of the light paint inside a serialized glass effect.
const LIGHT_OFFSET: usize = 56;

/// Size of a serialized glass effect:
/// `[u8 hidden][u8 texture][2 pad][13 × f32][fill: the light paint]`.
pub const RAW_GLASS_DATA_SIZE: usize = LIGHT_OFFSET + RAW_FILL_DATA_SIZE;

#[derive(Debug, Clone, Copy, PartialEq, ToJs)]
#[repr(u8)]
#[allow(dead_code)]
pub enum RawGlassTexture {
    None = 0,
    Reeded = 1,
    Wavy = 2,
    Prismatic = 3,
    CrossReeded = 4,
    Hammered = 5,
}

impl From<u8> for RawGlassTexture {
    fn from(value: u8) -> Self {
        match value {
            1 => RawGlassTexture::Reeded,
            2 => RawGlassTexture::Wavy,
            3 => RawGlassTexture::Prismatic,
            4 => RawGlassTexture::CrossReeded,
            5 => RawGlassTexture::Hammered,
            _ => RawGlassTexture::None,
        }
    }
}

impl From<RawGlassTexture> for GlassTexture {
    fn from(value: RawGlassTexture) -> Self {
        match value {
            RawGlassTexture::None => GlassTexture::None,
            RawGlassTexture::Reeded => GlassTexture::Reeded,
            RawGlassTexture::Wavy => GlassTexture::Wavy,
            RawGlassTexture::Prismatic => GlassTexture::Prismatic,
            RawGlassTexture::CrossReeded => GlassTexture::CrossReeded,
            RawGlassTexture::Hammered => GlassTexture::Hammered,
        }
    }
}

/// Reads a glass effect written by `serializers/glass.cljs`.
pub fn glass_from_bytes(bytes: &[u8]) -> Result<Glass> {
    if bytes.len() < RAW_GLASS_DATA_SIZE {
        return Err(Error::CriticalError(format!(
            "glass: expected {RAW_GLASS_DATA_SIZE} bytes, got {}",
            bytes.len()
        )));
    }

    let f32_at = |index: usize| {
        let start = 4 + index * 4;
        f32::from_le_bytes([
            bytes[start],
            bytes[start + 1],
            bytes[start + 2],
            bytes[start + 3],
        ])
    };
    let light = RawFillData::try_from(&bytes[LIGHT_OFFSET..])
        .map_err(|cause| Error::CriticalError(format!("glass light: {cause}")))?;

    Ok(Glass {
        hidden: bytes[0] != 0,
        texture: RawGlassTexture::from(bytes[1]).into(),
        light_angle: f32_at(0),
        light_intensity: f32_at(1),
        refraction: f32_at(2),
        depth: f32_at(3),
        dispersion: f32_at(4),
        frost: f32_at(5),
        splay: f32_at(6),
        saturation: f32_at(7),
        brightness: f32_at(8),
        highlight_width: f32_at(9),
        texture_amount: f32_at(10),
        texture_scale: f32_at(11),
        texture_angle: f32_at(12),
        light: light.into(),
    }
    .sanitized())
}

/// Sets the current shape's glass from the buffer written with
/// `_alloc_bytes`.
#[no_mangle]
#[wasm_error]
pub extern "C" fn set_shape_glass() -> Result<()> {
    let bytes = mem::bytes();
    let glass = glass_from_bytes(&bytes)?;
    with_current_shape_mut!(state, |shape: &mut Shape| {
        shape.set_glass(Some(glass));
    });
    mem::free_bytes()?;
    Ok(())
}

#[no_mangle]
pub extern "C" fn clear_shape_glass() {
    with_current_shape_mut!(state, |shape: &mut Shape| {
        shape.set_glass(None);
    });
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use skia_safe as skia;

    use crate::shapes::{Fill, SolidColor};

    /// A fill record holding one solid color, as `write-solid-fill` writes it.
    pub(crate) fn solid_light(argb: u32) -> Vec<u8> {
        let mut bytes = vec![0u8; RAW_FILL_DATA_SIZE];
        bytes[4..8].copy_from_slice(&argb.to_le_bytes());
        bytes
    }

    /// A fill record holding a linear gradient, as `write-gradient-fill`
    /// writes it. Stops are `(argb, offset)`.
    pub(crate) fn gradient_light(stops: &[(u32, f32)]) -> Vec<u8> {
        let mut bytes = vec![0u8; RAW_FILL_DATA_SIZE];
        bytes[0] = 0x01;
        bytes[4..8].copy_from_slice(&0.0f32.to_le_bytes()); // start x
        bytes[8..12].copy_from_slice(&0.5f32.to_le_bytes()); // start y
        bytes[12..16].copy_from_slice(&1.0f32.to_le_bytes()); // end x
        bytes[16..20].copy_from_slice(&0.5f32.to_le_bytes()); // end y
        bytes[20] = 0xFF; // opacity
        bytes[24..28].copy_from_slice(&0.0f32.to_le_bytes()); // width
        bytes[28] = stops.len() as u8;
        for (index, (color, offset)) in stops.iter().enumerate() {
            let at = 32 + index * 8;
            bytes[at..at + 4].copy_from_slice(&color.to_le_bytes());
            bytes[at + 4..at + 8].copy_from_slice(&offset.to_le_bytes());
        }
        bytes
    }

    /// Bytes as `serializers/glass.cljs` writes them, with a solid light.
    pub(crate) fn glass_bytes(hidden: bool, texture: u8, floats: [f32; 13], light: u32) -> Vec<u8> {
        glass_bytes_with(hidden, texture, floats, &solid_light(light))
    }

    /// Bytes of a glass with `light` as its light paint.
    pub(crate) fn glass_bytes_with(
        hidden: bool,
        texture: u8,
        floats: [f32; 13],
        light: &[u8],
    ) -> Vec<u8> {
        let mut bytes = vec![hidden as u8, texture, 0, 0];
        for value in floats {
            bytes.extend_from_slice(&value.to_le_bytes());
        }
        bytes.extend_from_slice(light);
        bytes
    }

    #[test]
    fn unknown_texture_bytes_read_as_none() {
        assert_eq!(RawGlassTexture::from(0), RawGlassTexture::None);
        assert_eq!(RawGlassTexture::from(1), RawGlassTexture::Reeded);
        assert_eq!(RawGlassTexture::from(2), RawGlassTexture::Wavy);
        assert_eq!(RawGlassTexture::from(3), RawGlassTexture::Prismatic);
        assert_eq!(RawGlassTexture::from(4), RawGlassTexture::CrossReeded);
        assert_eq!(RawGlassTexture::from(5), RawGlassTexture::Hammered);
        assert_eq!(RawGlassTexture::from(7), RawGlassTexture::None);
    }

    #[test]
    fn every_texture_byte_reaches_the_shape() {
        for (byte, texture) in [
            (2u8, GlassTexture::Wavy),
            (3, GlassTexture::Prismatic),
            (4, GlassTexture::CrossReeded),
            (5, GlassTexture::Hammered),
        ] {
            let bytes = glass_bytes(false, byte, [0.0; 13], 0xFFFF_FFFF);
            assert_eq!(glass_from_bytes(&bytes).unwrap().texture, texture);
        }
    }

    #[test]
    fn glass_from_bytes_reads_every_field() {
        let floats = [
            -45.0, 80.0, 70.0, 20.0, 50.0, 4.0, 10.0, 150.0, 60.0, 3.0, 40.0, 12.0, 30.0,
        ];
        let bytes = glass_bytes(true, 1, floats, 0xFF00_FF00);
        let glass = glass_from_bytes(&bytes).unwrap();

        assert_eq!(
            glass,
            Glass {
                hidden: true,
                light_angle: -45.0,
                light_intensity: 80.0,
                refraction: 70.0,
                depth: 20.0,
                dispersion: 50.0,
                frost: 4.0,
                splay: 10.0,
                saturation: 150.0,
                brightness: 60.0,
                highlight_width: 3.0,
                texture: GlassTexture::Reeded,
                texture_amount: 40.0,
                texture_scale: 12.0,
                texture_angle: 30.0,
                light: Fill::Solid(SolidColor(skia::Color::from_rgb(0, 255, 0))),
            }
        );
    }

    #[test]
    fn glass_from_bytes_sanitizes_values() {
        let mut floats = [0.0; 13];
        floats[7] = 500.0; // saturation
        floats[8] = f32::NAN; // brightness
        let bytes = glass_bytes(false, 0, floats, 0x0000_00FF);
        let glass = glass_from_bytes(&bytes).unwrap();
        assert_eq!(glass.saturation, 200.0);
        assert_eq!(glass.brightness, 100.0);
        assert_eq!(
            glass.light,
            Fill::Solid(SolidColor(skia::Color::from_rgb(0, 0, 255)))
        );
    }

    #[test]
    fn glass_from_bytes_reads_a_gradient_light() {
        let light = gradient_light(&[(0xFFFF_0000, 0.0), (0xFF00_00FF, 1.0)]);
        let bytes = glass_bytes_with(false, 0, [0.0; 13], &light);
        let glass = glass_from_bytes(&bytes).unwrap();

        let Fill::LinearGradient(gradient) = glass.light else {
            panic!("expected a linear gradient, got {:?}", glass.light);
        };
        let stops: Vec<(skia::Color, f32)> = gradient.stops().collect();
        assert_eq!(
            stops,
            vec![
                (skia::Color::from_rgb(255, 0, 0), 0.0),
                (skia::Color::from_rgb(0, 0, 255), 1.0),
            ]
        );
        assert_eq!(gradient.start(), (0.0, 0.5));
        assert_eq!(gradient.end(), (1.0, 0.5));
    }

    #[test]
    fn an_image_light_reads_as_white() {
        let mut light = solid_light(0xFF00_FF00);
        light[0] = 0x03; // image fill
        let bytes = glass_bytes_with(false, 0, [0.0; 13], &light);
        let glass = glass_from_bytes(&bytes).unwrap();
        assert_eq!(glass.light, Fill::Solid(SolidColor(skia::Color::WHITE)));
    }

    #[test]
    fn glass_from_bytes_fails_on_short_input() {
        let bytes = vec![0u8; RAW_GLASS_DATA_SIZE - 1];
        assert!(glass_from_bytes(&bytes).is_err());
    }
}
