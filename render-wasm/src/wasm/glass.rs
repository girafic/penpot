use crate::shapes::Glass;
use crate::with_current_shape_mut;

#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub extern "C" fn set_shape_glass(
    hidden: bool,
    light_angle: f32,
    light_intensity: f32,
    refraction: f32,
    depth: f32,
    dispersion: f32,
    frost: f32,
    splay: f32,
) {
    with_current_shape_mut!(state, |shape: &mut Shape| {
        shape.set_glass(Some(Glass::new(
            hidden,
            light_angle,
            light_intensity,
            refraction,
            depth,
            dispersion,
            frost,
            splay,
        )));
    });
}

#[no_mangle]
pub extern "C" fn clear_shape_glass() {
    with_current_shape_mut!(state, |shape: &mut Shape| {
        shape.set_glass(None);
    });
}
