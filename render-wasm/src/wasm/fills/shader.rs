use macros::wasm_error;

use crate::mem;
use crate::render::shaders::compile_shader;
use crate::shapes::ShaderFill;
use crate::utils::uuid_from_u32_quartet;
use crate::uuid::Uuid;
use crate::with_state_mut;
use crate::STATE;

pub const MAX_SHADER_COLORS: usize = 4;
pub const MAX_SHADER_PARAMS: usize = 4;

const SHADER_IDS_SIZE: usize = 32; // shape UUID + shader UUID

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(C)]
#[repr(align(4))]
pub struct RawShaderFillData {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    opacity: u8,
    flags: u8,
    // 16-bit padding here, reserved for future use
    _pad: u16,
    colors: [u32; MAX_SHADER_COLORS],
    params: [f32; MAX_SHADER_PARAMS],
}

impl From<RawShaderFillData> for ShaderFill {
    fn from(value: RawShaderFillData) -> Self {
        let id = uuid_from_u32_quartet(value.a, value.b, value.c, value.d);
        let colors = value.colors.map(crate::shapes::Color::new);
        Self::new(id, value.opacity, colors, value.params)
    }
}

fn parse_shader_ids(bytes: &[u8]) -> (Uuid, Uuid) {
    let shape_id = Uuid::try_from(&bytes[0..16]).unwrap();
    let shader_id = Uuid::try_from(&bytes[16..32]).unwrap();
    (shape_id, shader_id)
}

/// Stores a compiled SkSL shader in the shader store.
/// Expected memory layout:
/// - bytes 0-15: shape UUID
/// - bytes 16-31: shader UUID
/// - bytes 32..: UTF-8 SkSL source
#[no_mangle]
#[wasm_error]
pub extern "C" fn store_shader() -> crate::error::Result<()> {
    let bytes = mem::bytes();
    let (shape_id, shader_id) = parse_shader_ids(&bytes[0..SHADER_IDS_SIZE]);

    let source = std::str::from_utf8(&bytes[SHADER_IDS_SIZE..]).unwrap_or_default();

    with_state_mut!(state, {
        if let Err(msg) = state.render_state_mut().shaders.add(shader_id, source) {
            eprintln!("store_shader error: {}", msg);
        }
        state.touch_shape(shape_id);
    });

    mem::free_bytes()?;
    Ok(())
}

#[no_mangle]
#[wasm_error]
pub extern "C" fn is_shader_cached(a: u32, b: u32, c: u32, d: u32) -> crate::error::Result<bool> {
    with_state_mut!(state, {
        let id = uuid_from_u32_quartet(a, b, c, d);
        Ok(state.render_state().shaders.contains(&id))
    })
}

/// Compiles the SkSL source stored in the shared memory buffer without
/// storing it. Returns a pointer to a length-prefixed (u32 LE) UTF-8 error
/// string; a zero length means the source compiled successfully. The caller
/// must free the returned buffer with `free_bytes`.
#[no_mangle]
pub extern "C" fn validate_shader() -> *mut u8 {
    let bytes = mem::bytes();
    let source = std::str::from_utf8(&bytes).unwrap_or_default();

    let error = match compile_shader(source) {
        Ok(_) => String::new(),
        Err(msg) => msg,
    };

    let error_bytes = error.as_bytes();
    let mut result = Vec::with_capacity(4 + error_bytes.len());
    result.extend_from_slice(&(error_bytes.len() as u32).to_le_bytes());
    result.extend_from_slice(error_bytes);
    mem::write_bytes(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raw_shader_fill_data_layout() {
        assert_eq!(std::mem::size_of::<RawShaderFillData>(), 52);
        assert_eq!(std::mem::align_of::<RawShaderFillData>(), 4);
    }
}
