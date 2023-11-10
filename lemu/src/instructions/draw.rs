use super::{get_num, Flow, LInstruction};
use crate::{
    executor::{Display, DisplayState, ExecutorContext},
    memory::{LAddress, LRegistry, LVar},
};
use enum_dispatch::enum_dispatch;
use fimg::Image;
use std::fmt::{self, Display as Disp, Formatter};

pub const INSTRS: &[&str] = &[
    "clear", "color", "col", "stroke", "line", "rect", "lineRect", "triangle", "poly", "linePoly",
];

#[enum_dispatch]
pub trait DrawInstruction: Disp {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    );
}

#[derive(Debug, Copy, Clone)]
#[enum_dispatch(DrawInstruction)]
pub enum DrawInstr {
    Line(Line),
    RectBordered(RectBordered),
    RectFilled(RectFilled),
    Triangle(Triangle),
    Clear(Clear),
    SetColor(SetColor),
    SetStroke(SetStroke),
    Poly(Poly),
    LinePoly(LinePoly),
}

impl Disp for DrawInstr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Line(i) => write!(f, "{i}"),
            Self::RectBordered(i) => write!(f, "{i}"),
            Self::RectFilled(i) => write!(f, "{i}"),
            Self::Triangle(i) => write!(f, "{i}"),
            Self::Clear(i) => write!(f, "{i}"),
            Self::SetColor(i) => write!(f, "{i}"),
            Self::SetStroke(i) => write!(f, "{i}"),
            Self::Poly(i) => write!(f, "{i}"),
            Self::LinePoly(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug, Copy, Clone)]

pub struct Clear {
    pub r: LAddress,
    pub g: LAddress,
    pub b: LAddress,
}

impl DrawInstruction for Clear {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        _: &mut DisplayState,
    ) {
        macro_rules! u8 {
            ($v:ident) => {
                match mem.get(self.$v) {
                    LVar::Num(n) => n.round() as u8,
                    _ => return,
                }
            };
        }
        let (r, g, b) = (u8!(r), u8!(g), u8!(b));
        for [r2, g2, b2, a2] in image.chunked_mut() {
            (*r2, *b2, *g2, *a2) = (r, g, b, 255);
        }
    }
}

impl Disp for Clear {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "draw clear {} {} {}", self.r, self.g, self.b)
    }
}

#[derive(Debug, Copy, Clone)]

pub struct SetColor {
    pub r: LAddress,
    pub g: LAddress,
    pub b: LAddress,
    pub a: LAddress,
}
impl DrawInstruction for SetColor {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        _: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        macro_rules! u8 {
            ($v:ident) => {
                match mem.get(self.$v) {
                    LVar::Num(n) => n.round() as u8,
                    _ => return,
                }
            };
        }
        state.color = (u8!(r), u8!(g), u8!(b), u8!(a));
    }
}

impl Disp for SetColor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "draw color {} {} {} {}", self.r, self.g, self.b, self.a)
    }
}

#[derive(Debug, Copy, Clone)]

pub struct SetStroke {
    pub size: LAddress,
}
impl DrawInstruction for SetStroke {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        _: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        if let &LVar::Num(n) = mem.get(self.size) {
            state.stroke = n;
        }
    }
}

impl Disp for SetStroke {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "draw stroke {}", self.size)
    }
}

pub type Point = (LAddress, LAddress);
#[rustfmt::skip]
macro_rules! point {
    ($mem:ident@$point:expr) => {{
        let &LVar::Num(a) = $mem.get($point.0) else { return };
        let &LVar::Num(b) = $mem.get($point.1) else { return };
        (a,b)
    }}
}

macro_rules! map {
    ($tup:expr, $fn:expr) => {{
        let (a, b) = $tup;
        ($fn(a), $fn(b))
    }};
}
#[derive(Debug, Copy, Clone)]

pub struct Line {
    pub point_a: Point,
    pub point_b: Point,
}

impl DrawInstruction for Line {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let a = map!(point!(mem@self.point_a), |n| n as f32);
        let b = map!(point!(mem@self.point_b), |n| n as f32);
        image.thick_line(a, b, state.stroke as f32, state.col());
    }
}

impl Disp for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw line {} {} {} {}",
            self.point_a.0, self.point_a.1, self.point_b.0, self.point_b.1
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct RectFilled {
    pub position: Point,
    pub width: LAddress,
    pub height: LAddress,
}

impl DrawInstruction for RectFilled {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(self.width)) as u32;
        let height = get_num!(mem.get(self.height)) as u32;
        image.filled_box(pos, width, height, state.col());
    }
}

impl Disp for RectFilled {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw rect {} {} {} {}",
            self.position.0, self.position.1, self.width, self.height
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct RectBordered {
    pub position: Point,
    pub width: LAddress,
    pub height: LAddress,
}

impl Disp for RectBordered {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw lineRect {} {} {} {}",
            self.position.0, self.position.1, self.width, self.height
        )
    }
}

impl DrawInstruction for RectBordered {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(self.width)) as u32;
        let height = get_num!(mem.get(self.height)) as u32;
        image.stroked_box(pos, width, height, state.stroke.round() as u32, state.col());
    }
}

#[derive(Debug, Copy, Clone)]

pub struct Triangle {
    pub points: (Point, Point, Point),
}
impl DrawInstruction for Triangle {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        i: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let to32 = |n| n as f32;
        let (a, b, c) = (
            map!(point!(mem@self.points.0), to32),
            map!(point!(mem@self.points.1), to32),
            map!(point!(mem@self.points.2), to32),
        );
        i.tri::<f32>(a, b, c, state.col());
    }
}
impl Disp for Triangle {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw triangle {} {} {} {} {} {}",
            self.points.0.0,
            self.points.0.1,
            self.points.1.0,
            self.points.1.1,
            self.points.2.0,
            self.points.2.1
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct Poly {
    pub(crate) pos: Point,
    pub(crate) sides: LAddress,
    pub(crate) radius: LAddress,
    pub(crate) rot: LAddress,
}

impl DrawInstruction for Poly {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let sides = get_num!(mem.get(self.sides)).round() as usize;
        if sides < 90 {
            image.poly(
                map!(point!(mem@self.pos), |n| n as f32),
                sides,
                get_num!(mem.get(self.radius)) as f32,
                get_num!(mem.get(self.rot)) as f32,
                state.col(),
            );
        } else {
            image.circle(
                map!(point!(mem@self.pos), |n: f64| n.round() as i32),
                get_num!(mem.get(self.radius)).round() as i32,
                state.col(),
            );
        }
    }
}

impl Disp for Poly {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw poly {} {} {} {} {}",
            self.pos.0, self.pos.1, self.sides, self.radius, self.rot
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct LinePoly {
    pub(crate) pos: Point,
    pub(crate) sides: LAddress,
    pub(crate) radius: LAddress,
    pub(crate) rot: LAddress,
}

impl DrawInstruction for LinePoly {
    fn draw<'v>(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let sides = get_num!(mem.get(self.sides)).round() as usize;
        if sides < 90 {
            image.border_poly(
                map!(point!(mem@self.pos), |n| n as f32),
                sides,
                get_num!(mem.get(self.radius)) as f32,
                get_num!(mem.get(self.rot)) as f32,
                state.stroke as f32,
                state.col(),
            );
        } else {
            image.border_circle(
                map!(point!(mem@self.pos), |n: f64| n.round() as i32),
                get_num!(mem.get(self.radius)).round() as i32,
                state.col(),
            );
        }
    }
}

impl Disp for LinePoly {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw linePoly {} {} {} {} {}",
            self.pos.0, self.pos.1, self.sides, self.radius, self.rot
        )
    }
}

#[derive(Debug, Copy, Clone, Default)]

pub struct Flush {
    pub(crate) display: Display,
}
impl LInstruction for Flush {
    fn run<W: std::io::Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.flush(self.display);
        Flow::Continue
    }
}

impl Disp for Flush {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let d = self.display;
        write!(f, "drawflush {d}")
    }
}
