use skia_safe::{self as skia, Paint, Rect};

pub use super::Color;
use crate::render::shaders::{CompiledShader, RESOLUTION_UNIFORM, TIME_UNIFORM};
use crate::utils::{get_image, get_shader, get_shader_time};
use crate::uuid::Uuid;

#[derive(Debug, Clone, PartialEq)]
pub struct Gradient {
    start: (f32, f32),
    end: (f32, f32),
    opacity: u8,
    width: f32,
    colors: Vec<Color>,
    offsets: Vec<f32>,
}

impl Gradient {
    pub fn new(
        start: (f32, f32),
        end: (f32, f32),
        opacity: u8,
        width: f32,
        stops: &[(Color, f32)],
    ) -> Self {
        let mut gradient = Gradient {
            start,
            end,
            opacity,
            colors: vec![],
            offsets: vec![],
            width,
        };

        gradient.add_stops(stops);
        gradient
    }

    fn add_stops(&mut self, stops: &[(Color, f32)]) {
        let colors = stops.iter().map(|(color, _)| *color);
        let offsets = stops.iter().map(|(_, offset)| *offset);
        self.colors.extend(colors);
        self.offsets.extend(offsets);
    }

    pub fn to_linear_shader(&self, rect: &Rect) -> Option<skia::Shader> {
        let start = (
            rect.left + self.start.0 * rect.width(),
            rect.top + self.start.1 * rect.height(),
        );
        let end = (
            rect.left + self.end.0 * rect.width(),
            rect.top + self.end.1 * rect.height(),
        );
        skia::gradient_shader::linear(
            (start, end),
            self.colors.as_slice(),
            Some(self.offsets.as_slice()),
            skia::TileMode::Clamp,
            None,
            None,
        )
    }

    pub fn to_radial_shader(&self, rect: &Rect) -> Option<skia::Shader> {
        let center = skia::Point::new(
            rect.left + self.start.0 * rect.width(),
            rect.top + self.start.1 * rect.height(),
        );
        let end = skia::Point::new(
            rect.left + self.end.0 * rect.width(),
            rect.top + self.end.1 * rect.height(),
        );

        let direction = end - center;
        let distance = (direction.x.powi(2) + direction.y.powi(2)).sqrt();
        let angle = direction.y.atan2(direction.x).to_degrees();

        // Based on the code from frontend/src/app/main/ui/shapes/gradients.cljs
        let mut transform = skia::Matrix::new_identity();
        transform.pre_translate((center.x, center.y));
        transform.pre_rotate(angle + 90., skia::Point::new(0., 0.));
        // We need an extra transform, because in skia radial gradients are circular and we need them to be ellipses if they must adapt to the shape
        transform.pre_scale((self.width * rect.width() / rect.height(), 1.), None);
        transform.pre_translate((-center.x, -center.y));

        skia::gradient_shader::radial(
            center,
            distance,
            self.colors.as_slice(),
            Some(self.offsets.as_slice()),
            skia::TileMode::Clamp,
            None,
            Some(&transform),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImageFill {
    id: Uuid,
    opacity: u8,
    width: i32,
    height: i32,
    keep_aspect_ratio: bool,
}

impl ImageFill {
    pub fn new(id: Uuid, opacity: u8, width: i32, height: i32, keep_aspect_ratio: bool) -> Self {
        Self {
            id,
            opacity,
            width,
            height,
            keep_aspect_ratio,
        }
    }

    pub fn id(&self) -> Uuid {
        self.id
    }

    pub fn opacity(&self) -> u8 {
        self.opacity
    }

    pub fn keep_aspect_ratio(&self) -> bool {
        self.keep_aspect_ratio
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct SolidColor(pub Color);

pub const MAX_SHADER_COLORS: usize = 4;
pub const MAX_SHADER_PARAMS: usize = 4;

#[derive(Debug, Clone, PartialEq)]
pub struct ShaderFill {
    id: Uuid,
    opacity: u8,
    colors: [Color; MAX_SHADER_COLORS],
    params: [f32; MAX_SHADER_PARAMS],
}

impl ShaderFill {
    pub fn new(
        id: Uuid,
        opacity: u8,
        colors: [Color; MAX_SHADER_COLORS],
        params: [f32; MAX_SHADER_PARAMS],
    ) -> Self {
        Self {
            id,
            opacity,
            colors,
            params,
        }
    }

    pub fn id(&self) -> Uuid {
        self.id
    }

    pub fn opacity(&self) -> u8 {
        self.opacity
    }

    /// Builds the uniform data blob for this fill by introspecting the
    /// uniforms declared by the compiled runtime effect. Every uniform of
    /// the v1 contract (`u_time`, `u_resolution`, `u_color1..4`,
    /// `u_param1..4`) is optional; unknown uniforms are left zeroed.
    fn uniform_data(&self, shader: &CompiledShader, bounding_box: &Rect) -> Vec<u8> {
        let mut data = vec![0u8; shader.effect.uniform_size()];

        let mut write_floats = |offset: usize, values: &[f32]| {
            let bytes_len = values.len() * 4;
            if offset + bytes_len > data.len() {
                return;
            }
            for (i, value) in values.iter().enumerate() {
                let base = offset + i * 4;
                data[base..base + 4].copy_from_slice(&value.to_le_bytes());
            }
        };

        for uniform in shader.effect.uniforms() {
            let offset = uniform.offset();
            match uniform.name() {
                TIME_UNIFORM => write_floats(offset, &[get_shader_time()]),
                RESOLUTION_UNIFORM => {
                    write_floats(offset, &[bounding_box.width(), bounding_box.height()])
                }
                "u_color1" | "u_color2" | "u_color3" | "u_color4" => {
                    let index = (uniform.name().as_bytes()[7] - b'1') as usize;
                    let color = skia::Color4f::from(self.colors[index]);
                    write_floats(offset, &[color.r, color.g, color.b, color.a]);
                }
                "u_param1" | "u_param2" | "u_param3" | "u_param4" => {
                    let index = (uniform.name().as_bytes()[7] - b'1') as usize;
                    write_floats(offset, &[self.params[index]]);
                }
                _ => {}
            }
        }

        data
    }

    /// Returns the runtime-effect shader for this fill, positioned so that
    /// `fragCoord` is local to the shape (origin at the bounding box
    /// top-left). Returns `None` when the shader is not cached yet,
    /// mirroring the behavior of image fills with missing images.
    pub fn to_shader(&self, bounding_box: &Rect) -> Option<skia::Shader> {
        let shader = get_shader(&self.id)?;
        let uniforms = skia::Data::new_copy(&self.uniform_data(&shader, bounding_box));
        let mut matrix = skia::Matrix::new_identity();
        matrix.pre_translate((bounding_box.left, bounding_box.top));
        shader.effect.make_shader(uniforms, &[], Some(&matrix))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Fill {
    Solid(SolidColor),
    LinearGradient(Gradient),
    RadialGradient(Gradient),
    Image(ImageFill),
    Shader(ShaderFill),
}

impl Fill {
    pub fn opacity(&self) -> f32 {
        match self {
            Fill::Solid(SolidColor(color)) => color.a() as f32 / 255.0,
            Fill::LinearGradient(g) => g.opacity as f32 / 255.0,
            Fill::RadialGradient(g) => g.opacity as f32 / 255.0,
            Fill::Image(i) => i.opacity as f32 / 255.0,
            Fill::Shader(s) => s.opacity as f32 / 255.0,
        }
    }

    pub fn with_full_opacity(&self) -> Fill {
        match self {
            Fill::Solid(SolidColor(color)) => Fill::Solid(SolidColor(skia::Color::from_argb(
                255,
                color.r(),
                color.g(),
                color.b(),
            ))),
            Fill::LinearGradient(g) => Fill::LinearGradient(Gradient {
                opacity: 255,
                ..g.clone()
            }),
            Fill::RadialGradient(g) => Fill::RadialGradient(Gradient {
                opacity: 255,
                ..g.clone()
            }),
            Fill::Image(i) => Fill::Image(ImageFill {
                opacity: 255,
                ..i.clone()
            }),
            Fill::Shader(s) => Fill::Shader(ShaderFill {
                opacity: 255,
                ..s.clone()
            }),
        }
    }

    pub fn to_paint(&self, rect: &Rect, anti_alias: bool) -> skia::Paint {
        match self {
            Self::Solid(SolidColor(color)) => {
                let mut p = skia::Paint::default();
                p.set_color(*color);
                p.set_style(skia::PaintStyle::Fill);
                p.set_anti_alias(anti_alias);
                p.set_blend_mode(skia::BlendMode::SrcOver);
                p
            }
            Self::LinearGradient(gradient) => {
                let mut p = skia::Paint::default();
                p.set_shader(gradient.to_linear_shader(rect));
                p.set_alpha(gradient.opacity);
                p.set_style(skia::PaintStyle::Fill);
                p.set_anti_alias(anti_alias);
                p.set_blend_mode(skia::BlendMode::SrcOver);
                p
            }
            Self::RadialGradient(gradient) => {
                let mut p = skia::Paint::default();
                p.set_shader(gradient.to_radial_shader(rect));
                p.set_alpha(gradient.opacity);
                p.set_style(skia::PaintStyle::Fill);
                p.set_anti_alias(anti_alias);
                p.set_blend_mode(skia::BlendMode::SrcOver);
                p
            }
            Self::Image(image_fill) => {
                let mut p = skia::Paint::default();
                p.set_style(skia::PaintStyle::Fill);
                p.set_anti_alias(anti_alias);
                p.set_blend_mode(skia::BlendMode::SrcOver);
                p.set_alpha(image_fill.opacity);
                p
            }
            Self::Shader(shader_fill) => {
                let mut p = skia::Paint::default();
                p.set_shader(shader_fill.to_shader(rect));
                p.set_alpha(shader_fill.opacity);
                p.set_style(skia::PaintStyle::Fill);
                p.set_anti_alias(anti_alias);
                p.set_blend_mode(skia::BlendMode::SrcOver);
                p
            }
        }
    }
}

pub fn get_fill_shader(fill: &Fill, bounding_box: &Rect) -> Option<skia::Shader> {
    match fill {
        Fill::Solid(SolidColor(color)) => Some(skia::shaders::color(*color)),
        Fill::LinearGradient(gradient) => gradient.to_linear_shader(bounding_box),
        Fill::RadialGradient(gradient) => gradient.to_radial_shader(bounding_box),
        Fill::Image(image_fill) => {
            let mut image_shader = None;
            let image = get_image(&image_fill.id);
            if let Some(image) = image {
                let sampling_options =
                    skia::SamplingOptions::new(skia::FilterMode::Linear, skia::MipmapMode::Nearest);

                // FIXME no image ratio applied, centered to the current rect
                let tile_modes = (skia::TileMode::Clamp, skia::TileMode::Clamp);
                let image_width = image_fill.width as f32;
                let image_height = image_fill.height as f32;
                let scale_x = bounding_box.width() / image_width;
                let scale_y = bounding_box.height() / image_height;
                let scale = scale_x.max(scale_y);
                let scaled_width = image_width * scale;
                let scaled_height = image_height * scale;
                let pos_x = bounding_box.left() - (scaled_width - bounding_box.width()) / 2.0;
                let pos_y = bounding_box.top() - (scaled_height - bounding_box.height()) / 2.0;

                let mut matrix = skia::Matrix::new_identity();
                matrix.pre_translate((pos_x, pos_y));
                matrix.pre_scale((scale, scale), None);

                let opacity = image_fill.opacity();
                let alpha_color = skia::Color4f::new(1.0, 1.0, 1.0, opacity as f32 / 255.0);
                let alpha_shader = skia::shaders::color(alpha_color.to_color());

                image_shader = image.to_shader(tile_modes, sampling_options, &matrix);
                if let Some(shader) = image_shader {
                    image_shader = Some(skia::shaders::blend(
                        skia::Blender::mode(skia::BlendMode::DstIn),
                        shader,
                        alpha_shader,
                    ));
                }
            }
            image_shader
        }
        Fill::Shader(shader_fill) => {
            shader_fill.to_shader(bounding_box).map(|runtime_shader| {
                // Apply the fill opacity in shader space so it is honored
                // when several fills get merged into a single paint.
                let opacity = shader_fill.opacity() as f32 / 255.0;
                let alpha_color = skia::Color4f::new(1.0, 1.0, 1.0, opacity);
                let alpha_shader = skia::shaders::color(alpha_color.to_color());
                skia::shaders::blend(
                    skia::Blender::mode(skia::BlendMode::DstIn),
                    runtime_shader,
                    alpha_shader,
                )
            })
        }
    }
}

pub fn merge_fills(fills: &[Fill], bounding_box: Rect) -> skia::Paint {
    let mut combined_shader: Option<skia::Shader> = None;
    let mut fills_paint = skia::Paint::default();

    if fills.is_empty() {
        combined_shader = Some(skia::shaders::color(skia::Color::TRANSPARENT));
        fills_paint.set_shader(combined_shader);
        return fills_paint;
    }

    for fill in fills {
        let shader = get_fill_shader(fill, &bounding_box);

        if let Some(shader) = shader {
            combined_shader = match combined_shader {
                // Use SrcOver and treat the newly encountered fill as the source (top),
                // overlaying it over the previously composed shader (destination/bottom).
                // This avoids edge bleed from underlying fills when anti-aliasing causes
                // fractional coverage at shape boundaries.
                Some(existing_shader) => Some(skia::shaders::blend(
                    skia::Blender::mode(skia::BlendMode::SrcOver),
                    shader,
                    existing_shader,
                )),
                None => Some(shader),
            };
        }
    }

    fills_paint.set_shader(combined_shader.clone());
    fills_paint
}

pub fn set_paint_fill(paint: &mut Paint, fill: &Fill, bounding_box: &Rect, remove_alpha: bool) {
    if remove_alpha {
        paint.set_color(skia::Color::BLACK);
        paint.set_alpha(255);
        return;
    }
    let shader = get_fill_shader(fill, bounding_box);
    if let Some(shader) = shader {
        paint.set_shader(shader);
    }
}
