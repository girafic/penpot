use skia_safe::{self as skia};
use std::collections::HashMap;

use crate::uuid::Uuid;

pub const TIME_UNIFORM: &str = "u_time";
pub const RESOLUTION_UNIFORM: &str = "u_resolution";

#[derive(Clone)]
pub struct CompiledShader {
    pub effect: skia::RuntimeEffect,
    // A shader is animated when it declares a `u_time` uniform.
    pub animated: bool,
}

pub struct ShaderStore {
    shaders: HashMap<Uuid, CompiledShader>,
}

pub fn compile_shader(source: &str) -> Result<CompiledShader, String> {
    let effect = skia::RuntimeEffect::make_for_shader(source, None)?;
    let animated = effect
        .uniforms()
        .iter()
        .any(|uniform| uniform.name() == TIME_UNIFORM);
    Ok(CompiledShader { effect, animated })
}

impl ShaderStore {
    pub fn new() -> Self {
        Self {
            shaders: HashMap::new(),
        }
    }

    pub fn add(&mut self, id: Uuid, source: &str) -> Result<(), String> {
        if self.shaders.contains_key(&id) {
            return Ok(());
        }
        let shader = compile_shader(source)?;
        self.shaders.insert(id, shader);
        Ok(())
    }

    pub fn contains(&self, id: &Uuid) -> bool {
        self.shaders.contains_key(id)
    }

    pub fn get(&self, id: &Uuid) -> Option<&CompiledShader> {
        self.shaders.get(id)
    }
}
