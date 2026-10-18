//! The 3D rotation of layers (Penpot Motion): a layer tilts about its own
//! X and Y axes, seen with the perspective of its board.
//!
//! The same sums as `app.common.geom.tilt` (common), checked against the
//! same corners Chromium's `DOMMatrix` gives for the CSS
//! `perspective(d) rotate(r) rotateX(a) rotateY(b)`: the tilt turns the
//! layer in its own axes, before its turn in the plane, the top edge going
//! back for a positive X, the right edge for a positive Y; a point `z`
//! towards the viewer shows at `vp + (p - vp) * d / (d - z)`, `vp` the
//! vanishing point of the board (its middle unless it is moved, as CSS
//! `perspective-origin`).
//!
//! A layer also comes `translate_z` px towards the viewer, in the space of
//! its parent.
//!
//! The tilt of a layer is a homography of the page (a 3×3 `Matrix` with a
//! perspective row), drawn after the layer's own transform. The layers in a
//! tilted group or board are drawn through its homography too, after their
//! own: each tilted layer flattens what it holds, as CSS does without
//! `preserve-3d`. A group that is a 3D space does not: its tilt and depth go
//! into those of what it holds (`model44`), seen once (`seen`), and the
//! layers of a 3D space draw back to front.

use super::{Matrix, Point, Rect};

/// The distance of the viewer, px, of a board with no perspective of its own.
pub const DEFAULT_PERSPECTIVE: f32 = 1000.0;

/// How far the viewer of the tilted layers of an orthographic board is, px:
/// far enough to see them without perspective, and finite, so that the
/// planes of the depth sort stay numbers.
pub const ORTHOGRAPHIC_DISTANCE: f32 = 1e7;

/// The least depth factor a point shows at: a point nearer the viewer (or
/// behind) does not show.
const MIN_W: f64 = 0.01;

/// The tilt of a layer, degrees, and the pivot it turns around: the middle
/// of its box at rest; while an animation plays it, the fractions of its box
/// of its pivot and the tilt it rests at (see `lift`).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Tilt {
    pub rotate_x: f32,
    pub rotate_y: f32,
    pub pivot: (f32, f32),
    pub rest: (f32, f32),
}

impl Tilt {
    /// A tilt around the middle of the box.
    #[cfg(test)]
    pub fn new(rotate_x: f32, rotate_y: f32) -> Self {
        Self {
            rotate_x,
            rotate_y,
            pivot: (0.5, 0.5),
            rest: (rotate_x, rotate_y),
        }
    }

    /// Whether it turns the layer out of the plane at all.
    pub fn is_flat(&self) -> bool {
        self.rotate_x.abs() < 1e-6 && self.rotate_y.abs() < 1e-6
    }
}

/// A matrix of space, by rows, turning column vectors `[x y z 1]`.
pub type M44 = [f64; 16];

pub fn mul44(a: &M44, b: &M44) -> M44 {
    let mut out = [0.0; 16];
    for r in 0..4 {
        for c in 0..4 {
            out[r * 4 + c] = (0..4).map(|k| a[r * 4 + k] * b[k * 4 + c]).sum();
        }
    }
    out
}

fn translate44(x: f64, y: f64, z: f64) -> M44 {
    [
        1.0, 0.0, 0.0, x, //
        0.0, 1.0, 0.0, y, //
        0.0, 0.0, 1.0, z, //
        0.0, 0.0, 0.0, 1.0,
    ]
}

fn rotate_x44(deg: f64) -> M44 {
    let (s, c) = deg.to_radians().sin_cos();
    [
        1.0, 0.0, 0.0, 0.0, //
        0.0, c, -s, 0.0, //
        0.0, s, c, 0.0, //
        0.0, 0.0, 0.0, 1.0,
    ]
}

fn rotate_y44(deg: f64) -> M44 {
    let (s, c) = deg.to_radians().sin_cos();
    [
        c, 0.0, s, 0.0, //
        0.0, 1.0, 0.0, 0.0, //
        -s, 0.0, c, 0.0, //
        0.0, 0.0, 0.0, 1.0,
    ]
}

fn perspective44(d: f64) -> M44 {
    [
        1.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        0.0,
        0.0,
        -1.0 / d,
        1.0,
    ]
}

/// The plane matrix `m` (affine) in space: depth stays.
fn matrix44(m: &Matrix) -> M44 {
    [
        m.scale_x() as f64,
        m.skew_x() as f64,
        0.0,
        m.translate_x() as f64,
        m.skew_y() as f64,
        m.scale_y() as f64,
        0.0,
        m.translate_y() as f64,
        0.0,
        0.0,
        1.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
    ]
}

pub fn apply44(m: &M44, [x, y, z]: [f64; 3]) -> [f64; 3] {
    [
        m[0] * x + m[1] * y + m[2] * z + m[3],
        m[4] * x + m[5] * y + m[6] * z + m[7],
        m[8] * x + m[9] * y + m[10] * z + m[11],
    ]
}

/// `m` around the point `[x y z]`.
fn about44(m: &M44, [x, y, z]: [f64; 3]) -> M44 {
    mul44(&translate44(x, y, z), &mul44(m, &translate44(-x, -y, -z)))
}

/// The homography `m` makes of the plane `z = 0`: its rows and columns of
/// x, y and w.
fn restrict(m: &M44) -> Matrix {
    Matrix::new_all(
        m[0] as f32,
        m[1] as f32,
        m[3] as f32,
        m[4] as f32,
        m[5] as f32,
        m[7] as f32,
        m[12] as f32,
        m[13] as f32,
        m[15] as f32,
    )
}

fn rotation44(rx: f64, ry: f64) -> M44 {
    mul44(&rotate_x44(rx), &rotate_y44(ry))
}

fn box_point(selrect: &Rect, fx: f64, fy: f64) -> [f64; 3] {
    [
        selrect.left as f64 + selrect.width() as f64 * fx,
        selrect.top as f64 + selrect.height() as f64 * fy,
        0.0,
    ]
}

/// How far the point at the fractions `fx`, `fy` of `selrect` moves,
/// `[dx dy dz]` (dz towards the viewer), when the box tilts by `rest`
/// around its middle: the place an animation turns it from, around that
/// point, so that its values at rest show it as it rests.
pub fn lift(selrect: &Rect, fx: f32, fy: f32, rest: (f32, f32)) -> [f64; 3] {
    let o = box_point(selrect, fx as f64, fy as f64);
    let center = box_point(selrect, 0.5, 0.5);
    let moved = apply44(
        &about44(&rotation44(rest.0 as f64, rest.1 as f64), center),
        o,
    );
    [moved[0] - o[0], moved[1] - o[1], moved[2] - o[2]]
}

/// The tilt in the space of the box `selrect` (see `Tilt`).
fn tilt44(selrect: &Rect, tilt: &Tilt) -> M44 {
    let rotation = rotation44(tilt.rotate_x as f64, tilt.rotate_y as f64);
    let (fx, fy) = tilt.pivot;
    let o = box_point(selrect, fx as f64, fy as f64);
    let [dx, dy, dz] = lift(selrect, fx, fy, tilt.rest);
    mul44(
        &translate44(o[0] + dx, o[1] + dy, dz),
        &mul44(&rotation, &translate44(-o[0], -o[1], 0.0)),
    )
}

/// Where a layer with the box `selrect`, drawn through `transform` (its
/// transform around its middle), takes the points of the page in space:
/// its `tilt` (in its own axes) and then its depth `z` (towards the
/// viewer). `None` when it stays in the plane.
pub fn model44(selrect: &Rect, transform: &Matrix, tilt: Option<&Tilt>, z: f32) -> Option<M44> {
    let tilt = tilt.filter(|t| !t.is_flat());
    if tilt.is_none() && z.abs() < 1e-6 {
        return None;
    }
    let lifted = translate44(0.0, 0.0, z as f64);
    Some(match tilt {
        Some(tilt) => {
            let inverse = transform.invert().unwrap_or_default();
            [
                matrix44(transform),
                tilt44(selrect, tilt),
                matrix44(&inverse),
            ]
            .iter()
            .fold(lifted, |acc, m| mul44(&acc, m))
        }
        None => lifted,
    })
}

/// The homography of the page the points of `m` show at, seen from
/// `distance` px with the vanishing point `vp`.
pub fn seen(m: &M44, distance: f32, vp: Point) -> Matrix {
    let camera = about44(
        &perspective44(distance.max(1.0) as f64),
        [vp.x as f64, vp.y as f64, 0.0],
    );
    restrict(&mul44(&camera, m))
}

/// The homography of the page a layer with the box `selrect`, drawn
/// through `transform` (its transform around its middle), is drawn
/// through for `tilt`, seen from `distance` px with the vanishing point
/// `vp`. `None` for a flat tilt.
#[cfg(test)]
pub fn tilt_matrix(
    selrect: &Rect,
    transform: &Matrix,
    tilt: &Tilt,
    distance: f32,
    vp: Point,
) -> Option<Matrix> {
    model44(selrect, transform, Some(tilt), 0.0).map(|m| seen(&m, distance, vp))
}

/// How far, px, a corner can be off a plane and count as in it.
const PLANE_TOLERANCE: f64 = 0.01;

fn sub3(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn dot3(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn cross3(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// How far `points` are from the plane of the layer with the `corners`,
/// px, positive on the side of `eye`; `None` for a layer with no area.
fn eye_sides(corners: &[[f64; 3]; 4], points: &[[f64; 3]; 4], eye: [f64; 3]) -> Option<[f64; 4]> {
    let c0 = corners[0];
    let n = cross3(sub3(corners[1], c0), sub3(corners[3], c0));
    let len = dot3(n, n).sqrt();
    if len <= 1e-9 {
        return None;
    }
    let side = |p: [f64; 3]| dot3(n, sub3(p, c0)) / len;
    let e = side(eye);
    if e.abs() < 1e-9 {
        return None;
    }
    Some(points.map(|p| if e > 0.0 { side(p) } else { -side(p) }))
}

/// Whether the layers with the corners `a` and `b` lie in one plane.
pub fn coplanar(a: &[[f64; 3]; 4], b: &[[f64; 3]; 4]) -> bool {
    let c0 = a[0];
    let n = cross3(sub3(a[1], c0), sub3(a[3], c0));
    let len = dot3(n, n).sqrt();
    len > 1e-9
        && b.iter()
            .all(|p| (dot3(n, sub3(*p, c0)) / len).abs() < PLANE_TOLERANCE)
}

/// Whether the layer with the corners `b` shows in front of the one with
/// the corners `a`, seen from `eye`: `Some(true)` when `b` is all on the side
/// of the plane of `a` the viewer is on, or `a` all behind the plane of `b`;
/// `Some(false)` the other way round; `None` when they cross or lie in one
/// plane.
pub fn in_front(a: &[[f64; 3]; 4], b: &[[f64; 3]; 4], eye: [f64; 3]) -> Option<bool> {
    let on_eye_side = |sides: Option<[f64; 4]>| {
        sides.is_some_and(|s| {
            s.iter().all(|v| *v > -PLANE_TOLERANCE) && s.iter().any(|v| *v > PLANE_TOLERANCE)
        })
    };
    let behind = |sides: Option<[f64; 4]>| {
        sides.is_some_and(|s| {
            s.iter().all(|v| *v < PLANE_TOLERANCE) && s.iter().any(|v| *v < -PLANE_TOLERANCE)
        })
    };
    let b_to_a = eye_sides(a, b, eye);
    let a_to_b = eye_sides(b, a, eye);
    if on_eye_side(b_to_a) || behind(a_to_b) {
        Some(true)
    } else if behind(b_to_a) || on_eye_side(a_to_b) {
        Some(false)
    } else {
        None
    }
}

fn get9(h: &Matrix) -> [f64; 9] {
    let mut v = [0.0f32; 9];
    h.get_9(&mut v);
    v.map(|x| x as f64)
}

/// `point` through the homography `h`, `None` where it reaches the viewer.
pub fn project_point(h: &Matrix, point: Point) -> Option<Point> {
    let m = get9(h);
    let (x, y) = (point.x as f64, point.y as f64);
    let w = m[6] * x + m[7] * y + m[8];
    if w <= MIN_W {
        return None;
    }
    Some(Point::new(
        ((m[0] * x + m[1] * y + m[2]) / w) as f32,
        ((m[3] * x + m[4] * y + m[5]) / w) as f32,
    ))
}

/// The box around `rect` as `h` shows it, `None` where a corner reaches the
/// viewer.
pub fn project_rect(h: &Matrix, rect: &Rect) -> Option<Rect> {
    let corners = [
        Point::new(rect.left, rect.top),
        Point::new(rect.right, rect.top),
        Point::new(rect.right, rect.bottom),
        Point::new(rect.left, rect.bottom),
    ];
    let mut out = Rect::new(f32::MAX, f32::MAX, f32::MIN, f32::MIN);
    for corner in corners {
        let p = project_point(h, corner)?;
        out.left = out.left.min(p.x);
        out.top = out.top.min(p.y);
        out.right = out.right.max(p.x);
        out.bottom = out.bottom.max(p.y);
    }
    Some(out)
}

/// Whether a layer drawn through `h` shows its back: `h` turns it over. A
/// flip in the plane is no back.
pub fn back_facing(h: &Matrix) -> bool {
    let [a, b, c, d, e, f, g, hh, i] = get9(h);
    a * (e * i - f * hh) - b * (d * i - f * g) + c * (d * hh - e * g) < 0.0
}

#[cfg(test)]
mod tests {
    use super::*;

    // The corners Chromium's DOMMatrix gives for the same CSS (the same
    // literals are in `common_tests/geom_tilt_test.cljc`).

    fn corners(h: &Matrix, x: f32, y: f32, w: f32, hh: f32) -> Vec<(f32, f32)> {
        [(x, y), (x + w, y), (x + w, y + hh), (x, y + hh)]
            .iter()
            .map(|&(px, py)| {
                let p = project_point(h, Point::new(px, py)).expect("shows");
                (p.x, p.y)
            })
            .collect()
    }

    fn assert_corners(expected: &[(f64, f64)], actual: &[(f32, f32)]) {
        for ((ex, ey), (ax, ay)) in expected.iter().zip(actual) {
            assert!(
                (*ex - *ax as f64).abs() < 0.01 && (*ey - *ay as f64).abs() < 0.01,
                "expected {expected:?}, got {actual:?}"
            );
        }
    }

    fn rect_tilt(rx: f32, ry: f32) -> Matrix {
        let selrect = Rect::from_xywh(100.0, 150.0, 200.0, 100.0);
        tilt_matrix(
            &selrect,
            &Matrix::default(),
            &Tilt::new(rx, ry),
            1000.0,
            Point::new(200.0, 200.0),
        )
        .unwrap()
    }

    #[test]
    fn a_flat_tilt_draws_nothing_else() {
        let selrect = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
        assert!(tilt_matrix(
            &selrect,
            &Matrix::default(),
            &Tilt::new(0.0, 0.0),
            1000.0,
            Point::new(0.0, 0.0)
        )
        .is_none());
    }

    #[test]
    fn a_tilt_shows_as_css_draws_it() {
        assert_corners(
            &[
                (145.2593178650658, 145.25931786506578),
                (246.014985371363, 153.98501462863697),
                (246.014985371363, 246.01498537136305),
                (145.2593178650658, 254.74068213493425),
            ],
            &corners(&rect_tilt(0.0, 60.0), 100.0, 150.0, 200.0, 100.0),
        );
        assert_corners(
            &[
                (102.43902439024389, 157.75485835197856),
                (297.56097560975604, 157.75485835197856),
                (302.56410256410254, 244.41155916843272),
                (97.43589743589743, 244.41155916843272),
            ],
            &corners(&rect_tilt(30.0, 0.0), 100.0, 150.0, 200.0, 100.0),
        );
        assert_corners(
            &[
                (114.9553614615924, 130.8534770133438),
                (276.48481601970104, 174.44708427275793),
                (279.007907225437, 264.2382890559766),
                (111.82436407334525, 229.4587175766942),
            ],
            &corners(&rect_tilt(20.0, 35.0), 100.0, 150.0, 200.0, 100.0),
        );
    }

    #[test]
    fn the_tilt_turns_in_the_axes_of_a_turned_layer() {
        let selrect = Rect::from_xywh(100.0, 150.0, 200.0, 100.0);
        let mut turn = Matrix::rotate_deg(30.0);
        turn.post_translate((200.0, 200.0));
        turn.pre_translate((-200.0, -200.0));
        let h = tilt_matrix(
            &selrect,
            &turn,
            &Tilt::new(45.0, 0.0),
            1000.0,
            Point::new(200.0, 200.0),
        )
        .unwrap();
        // the corners of the turned box, through its tilt
        let mut pts = [
            Point::new(100.0, 150.0),
            Point::new(300.0, 150.0),
            Point::new(300.0, 250.0),
            Point::new(100.0, 250.0),
        ];
        turn.map_points_inplace(&mut pts);
        let actual: Vec<(f32, f32)> = pts
            .iter()
            .map(|p| {
                let q = project_point(&h, *p).unwrap();
                (q.x, q.y)
            })
            .collect();
        assert_corners(
            &[
                (133.42877730132543, 122.13434485397467),
                (300.7192467881137, 218.71954244503078),
                (271.4510468358039, 283.5733872265198),
                (91.89780016361794, 179.90827192646196),
            ],
            &actual,
        );
    }

    #[test]
    fn a_door_turns_around_its_hinge() {
        let selrect = Rect::from_xywh(100.0, 150.0, 200.0, 100.0);
        let tilt = Tilt {
            rotate_x: 0.0,
            rotate_y: 50.0,
            pivot: (0.0, 0.5),
            rest: (0.0, 0.0),
        };
        let h = tilt_matrix(
            &selrect,
            &Matrix::default(),
            &tilt,
            1000.0,
            Point::new(200.0, 200.0),
        )
        .unwrap();
        assert_corners(
            &[
                (100.0, 150.0),
                (224.7635291567927, 156.64272059187087),
                (224.7635291567927, 243.35727940812916),
                (100.0, 250.0),
            ],
            &corners(&h, 100.0, 150.0, 200.0, 100.0),
        );
    }

    #[test]
    fn at_its_rest_values_a_layer_shows_as_it_rests_whatever_its_pivot() {
        let selrect = Rect::from_xywh(100.0, 150.0, 200.0, 100.0);
        let rest = [
            (108.83943118058538, 147.3684210526316),
            (282.47860988423224, 152.38095238095238),
            (282.47860988423224, 247.61904761904762),
            (108.83943118058538, 252.63157894736844),
        ];
        for pivot in [(0.5, 0.5), (0.0, 0.5), (1.0, 0.0)] {
            let tilt = Tilt {
                rotate_x: 0.0,
                rotate_y: 30.0,
                pivot,
                rest: (0.0, 30.0),
            };
            let h = tilt_matrix(
                &selrect,
                &Matrix::default(),
                &tilt,
                1000.0,
                Point::new(200.0, 200.0),
            )
            .unwrap();
            assert_corners(&rest, &corners(&h, 100.0, 150.0, 200.0, 100.0));
        }
    }

    #[test]
    fn a_tilted_group_tilts_what_it_holds() {
        let group = tilt_matrix(
            &Rect::from_xywh(50.0, 50.0, 300.0, 300.0),
            &Matrix::default(),
            &Tilt::new(20.0, 0.0),
            1000.0,
            Point::new(200.0, 200.0),
        )
        .unwrap();
        let own = rect_tilt(0.0, 40.0);
        let h = Matrix::concat(&group, &own);
        assert_corners(
            &[
                (119.60258629812793, 150.68898608655059),
                (270.8395390492815, 156.55122055748268),
                (273.1532388209567, 244.86786590785678),
                (116.60923452951954, 251.14695867725789),
            ],
            &corners(&h, 100.0, 150.0, 200.0, 100.0),
        );
    }

    #[test]
    fn back_faces() {
        let at_vp = |ry: f32| {
            let selrect = Rect::from_xywh(150.0, 150.0, 100.0, 100.0);
            back_facing(
                &tilt_matrix(
                    &selrect,
                    &Matrix::default(),
                    &Tilt::new(0.0, ry),
                    1000.0,
                    Point::new(200.0, 200.0),
                )
                .unwrap(),
            )
        };
        assert!(!at_vp(89.0));
        assert!(at_vp(91.0));

        // 500 px right of the vanishing point it turns over before 65
        let off = |ry: f32| {
            let selrect = Rect::from_xywh(1050.0, 150.0, 100.0, 100.0);
            back_facing(
                &tilt_matrix(
                    &selrect,
                    &Matrix::default(),
                    &Tilt::new(0.0, ry),
                    1000.0,
                    Point::new(600.0, 200.0),
                )
                .unwrap(),
            )
        };
        assert!(!off(60.0));
        assert!(off(65.0));
    }

    #[test]
    fn the_vanishing_point_moves_the_projection() {
        // `perspective-origin: 25% 75%` of a board 1200 x 400
        let selrect = Rect::from_xywh(1050.0, 150.0, 100.0, 100.0);
        let h = tilt_matrix(
            &selrect,
            &Matrix::default(),
            &Tilt::new(0.0, 70.0),
            1000.0,
            Point::new(300.0, 300.0),
        )
        .unwrap();
        assert_corners(
            &[
                (1121.4967127838604, 142.60484679950116),
                (1080.4326662896508, 156.7314213093065),
                (1080.4326662896508, 252.24380710310214),
                (1121.4967127838604, 247.53494893316704),
            ],
            &corners(&h, 1050.0, 150.0, 100.0, 100.0),
        );
    }

    #[test]
    fn planes_tell_which_layer_is_in_front() {
        let at = |z: f64| {
            [
                [0.0, 0.0, z],
                [10.0, 0.0, z],
                [10.0, 10.0, z],
                [0.0, 10.0, z],
            ]
        };
        let eye = [5.0, 5.0, 1000.0];
        assert_eq!(in_front(&at(0.0), &at(10.0), eye), Some(true));
        assert_eq!(in_front(&at(10.0), &at(0.0), eye), Some(false));
        assert_eq!(in_front(&at(0.0), &at(0.0), eye), None);
        assert!(coplanar(&at(0.0), &at(0.0)));
        assert!(!coplanar(&at(0.0), &at(10.0)));
    }

    #[test]
    fn a_layer_that_reaches_the_viewer_does_not_show() {
        let selrect = Rect::from_xywh(-1800.0, 150.0, 4000.0, 100.0);
        let h = tilt_matrix(
            &selrect,
            &Matrix::default(),
            &Tilt::new(0.0, 89.0),
            1000.0,
            Point::new(200.0, 200.0),
        )
        .unwrap();
        assert!(project_rect(&h, &selrect).is_none());
    }
}
