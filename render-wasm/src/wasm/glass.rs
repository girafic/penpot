use macros::{wasm_error, ToJs};
use skia_safe as skia;

use crate::error::{Error, Result};
use crate::mem;
use crate::shapes::{Glass, GlassTexture};
use crate::with_current_shape_mut;

/// Size of a serialized glass effect:
/// `[u8 hidden][u8 texture][2 pad][13 × f32][u32 light ARGB]`.
pub const RAW_GLASS_DATA_SIZE: usize = 60;

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
    let light = u32::from_le_bytes([bytes[56], bytes[57], bytes[58], bytes[59]]);

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
        light_color: skia::Color::new(light),
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

    /// Bytes as `serializers/glass.cljs` writes them.
    pub(crate) fn glass_bytes(hidden: bool, texture: u8, floats: [f32; 13], light: u32) -> Vec<u8> {
        let mut bytes = vec![hidden as u8, texture, 0, 0];
        for value in floats {
            bytes.extend_from_slice(&value.to_le_bytes());
        }
        bytes.extend_from_slice(&light.to_le_bytes());
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
                light_color: skia::Color::from_rgb(0, 255, 0),
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
        assert_eq!(glass.light_color, skia::Color::from_rgb(0, 0, 255));
    }

    #[test]
    fn glass_from_bytes_fails_on_short_input() {
        let bytes = vec![0u8; RAW_GLASS_DATA_SIZE - 1];
        assert!(glass_from_bytes(&bytes).is_err());
    }
}
