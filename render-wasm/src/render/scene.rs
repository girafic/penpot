//! How the light and the depth cues of their boards show the layers in 3D
//! a pass draws (see `Shape::scene_effects`), and how far the blur of their
//! depth cues reaches, for the tiles that hold them.

use skia_safe::{self as skia, Rect};
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use crate::math::tilt;
use crate::shapes::{radius_to_sigma, Shape};
use crate::state::ShapesPoolRef;
use crate::uuid::Uuid;

/// A shape as the walker draws it: itself, or as the light and the depth
/// cues of its board show it, shared by every visit of a pass.
pub enum Scened<'a> {
    Plain(&'a Shape),
    Lit(Rc<Shape>),
}

impl Deref for Scened<'_> {
    type Target = Shape;

    fn deref(&self) -> &Shape {
        match self {
            Scened::Plain(shape) => shape,
            Scened::Lit(shape) => shape,
        }
    }
}

/// The scene of the shapes of a pass: their effects and the shapes as they
/// show them, worked out once a pass, cleared when it starts anew.
#[derive(Default)]
pub struct SceneCache {
    effects: HashMap<Uuid, Option<tilt::SceneEffects>>,
    shapes: HashMap<Uuid, Rc<Shape>>,
}

impl SceneCache {
    pub fn clear(&mut self) {
        self.effects.clear();
        self.shapes.clear();
    }

    /// `shape` as the light and the depth cues of its board show it (see
    /// `Shape::with_scene_effects`): one copy a pass, which every visit
    /// shares; itself when nothing in the page leaves the plane or nothing
    /// shows it anew.
    pub fn scened<'a>(&mut self, shape: &'a Shape, tree: ShapesPoolRef) -> Scened<'a> {
        if !tree.has_tilts() {
            return Scened::Plain(shape);
        }
        if let Some(lit) = self.shapes.get(&shape.id) {
            return Scened::Lit(Rc::clone(lit));
        }
        let effects = *self
            .effects
            .entry(shape.id)
            .or_insert_with(|| shape.scene_effects(tree));
        match effects {
            Some(effects) => {
                let lit = Rc::new(shape.with_scene_effects(&effects));
                self.shapes.insert(shape.id, Rc::clone(&lit));
                Scened::Lit(lit)
            }
            None => Scened::Plain(shape),
        }
    }
}

/// The extrect of `shape` the tiles index it by: with the halo of the blur
/// of its depth cues, which it draws in the space of the page (see
/// `Shape::scene_effects`). Of a shape as the pool holds it, before a pass
/// lights it (see `SceneCache::scened`), which would carry that blur already.
pub fn indexed_extrect(shape: &Shape, tree: ShapesPoolRef, scale: f32) -> Rect {
    let extrect = shape.extrect(tree, scale);
    if !tree.has_tilts() {
        return extrect;
    }
    match shape.scene_effects(tree) {
        Some(effects) if effects.blur > 0.0 => {
            let sigma = radius_to_sigma(effects.blur * scale);
            skia::image_filters::blur((sigma, sigma), None, None, None)
                .map(|blur| blur.compute_fast_bounds(extrect))
                .unwrap_or(extrect)
        }
        _ => extrect,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shapes::{Frame, Rect as RectType, Type};
    use crate::state::ShapesPool;
    use skia_safe::Contains;

    /// A board blurring what is far back, holding a rect 500 px back.
    fn deep_rect() -> (ShapesPool, Uuid) {
        let mut pool = ShapesPool::new();
        pool.initialize(2);
        let [board_id, rect_id] = [(); 2].map(|_| Uuid::new_v4());
        {
            let board = pool.add_shape(board_id);
            board.set_shape_type(Type::Frame(Frame::default()));
            board.set_selrect(0.0, 0.0, 400.0, 400.0);
            board.set_scene(tilt::Scene {
                blur: 2.0,
                ..tilt::Scene::default()
            });
            board.children = vec![rect_id];
        }
        {
            let rect = pool.add_shape(rect_id);
            rect.set_shape_type(Type::Rect(RectType::default()));
            rect.set_selrect(100.0, 150.0, 300.0, 250.0);
            rect.set_parent(board_id);
            rect.set_depth(-500.0, false);
        }
        pool.mark_tilts();
        (pool, rect_id)
    }

    #[test]
    fn a_pass_shows_a_layer_lit_once() {
        let (pool, rect_id) = deep_rect();
        let rect = pool.get(&rect_id).expect("rect");
        let mut cache = SceneCache::default();
        let first = cache.scened(rect, &pool);
        let again = cache.scened(rect, &pool);
        match (&first, &again) {
            (Scened::Lit(a), Scened::Lit(b)) => assert!(Rc::ptr_eq(a, b), "the same copy"),
            _ => panic!("its depth cues show it anew"),
        }
        assert!(
            first.blur.is_some_and(|b| b.value > 0.0),
            "blurred far back"
        );
        cache.clear();
        let anew = cache.scened(rect, &pool);
        match (&first, &anew) {
            (Scened::Lit(a), Scened::Lit(b)) => {
                assert!(!Rc::ptr_eq(a, b), "a new pass, a new copy")
            }
            _ => panic!("still shown anew"),
        }
    }

    #[test]
    fn the_tiles_of_a_layer_hold_the_blur_of_its_depth_cues() {
        let (pool, rect_id) = deep_rect();
        let rect = pool.get(&rect_id).expect("rect");
        let plain = rect.extrect(&pool, 1.0);
        let indexed = indexed_extrect(rect, &pool, 1.0);
        assert!(
            indexed.contains(plain) && indexed.width() > plain.width() + 1.0,
            "{indexed:?}"
        );
        let copy = rect.clone();
        assert_eq!(
            indexed_extrect(&copy, &pool, 1.0),
            indexed,
            "any shape given, as a raw one before its modifiers"
        );
    }
}
