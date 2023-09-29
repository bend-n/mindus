use super::{get_num, Flow, LInstruction};
use crate::{
    executor::{Display, DisplayState, ExecutorContext},
    memory::{LAddress, LRegistry, LVar},
};
use enum_dispatch::enum_dispatch;
use fimg::Image;
use std::fmt::{self, Display as Disp, Formatter};

pub const INSTRS: &[&str] = &[
    "clear", "color", "col", "stroke", "line", "rect", "lineRect", "triangle", "poly",
];

#[enum_dispatch]
pub trait DrawInstruction<'v>: Disp {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    );
}

#[derive(Debug)]
#[enum_dispatch(DrawInstruction)]
pub enum DrawInstr<'v> {
    Line(Line<'v>),
    RectBordered(RectBordered<'v>),
    RectFilled(RectFilled<'v>),
    Triangle(Triangle<'v>),
    Clear(Clear<'v>),
    SetColor(SetColor<'v>),
    SetStroke(SetStroke<'v>),
    Poly(Poly<'v>),
}

impl Disp for DrawInstr<'_> {
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
        }
    }
}

#[derive(Debug)]
pub struct Clear<'v> {
    pub r: LAddress<'v>,
    pub g: LAddress<'v>,
    pub b: LAddress<'v>,
    pub a: LAddress<'v>,
}

impl<'v> DrawInstruction<'v> for Clear<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, image: &mut Image<&mut [u8], 4>, _: &mut DisplayState) {
        macro_rules! u8 {
            ($v:ident) => {
                match mem.get(&self.$v) {
                    LVar::Num(n) => n.round() as u8,
                    _ => return,
                }
            };
        }
        let (r, g, b, a) = (u8!(r), u8!(g), u8!(b), u8!(a));
        for [r2, g2, b2, a2] in image.chunked_mut() {
            (*r2, *b2, *g2, *a2) = (r, g, b, a);
        }
    }
}

impl Disp for Clear<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "draw clear {} {} {} {}", self.r, self.g, self.b, self.a)
    }
}

#[derive(Debug)]
pub struct SetColor<'v> {
    pub r: LAddress<'v>,
    pub g: LAddress<'v>,
    pub b: LAddress<'v>,
    pub a: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for SetColor<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        macro_rules! u8 {
            ($v:ident) => {
                match mem.get(&self.$v) {
                    LVar::Num(n) => n.round() as u8,
                    _ => return,
                }
            };
        }
        state.color = (u8!(r), u8!(g), u8!(b), u8!(a));
    }
}

impl Disp for SetColor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "draw color {} {} {} {}", self.r, self.g, self.b, self.a)
    }
}

#[derive(Debug)]
pub struct SetStroke<'v> {
    pub size: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for SetStroke<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        if let &LVar::Num(n) = mem.get(&self.size) {
            state.stroke = n;
        }
    }
}

impl Disp for SetStroke<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "draw stroke {}", self.size)
    }
}

pub type Point<'v> = (LAddress<'v>, LAddress<'v>);
#[rustfmt::skip]
macro_rules! point {
    ($mem:ident@$point:expr) => {{
        let &LVar::Num(a) = $mem.get(&$point.0) else { return; };
        let &LVar::Num(b) = $mem.get(&$point.1) else { return; };
        (a,b)
    }}
}

macro_rules! map {
    ($tup:expr, $fn:expr) => {{
        let (a, b) = $tup;
        ($fn(a), $fn(b))
    }};
}
#[derive(Debug)]
pub struct Line<'v> {
    pub point_a: Point<'v>,
    pub point_b: Point<'v>,
}

impl<'v> DrawInstruction<'v> for Line<'v> {
    fn draw(
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

impl Disp for Line<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw line {} {} {} {}",
            self.point_a.0, self.point_a.1, self.point_b.0, self.point_b.1
        )
    }
}

#[derive(Debug)]
pub struct RectFilled<'v> {
    pub position: Point<'v>,
    pub width: LAddress<'v>,
    pub height: LAddress<'v>,
}

impl<'v> DrawInstruction<'v> for RectFilled<'v> {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(&self.width)) as u32;
        let height = get_num!(mem.get(&self.height)) as u32;
        image.filled_box(pos, width, height, state.col());
    }
}

impl Disp for RectFilled<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw rect {} {} {} {}",
            self.position.0, self.position.1, self.width, self.height
        )
    }
}

#[derive(Debug)]
pub struct RectBordered<'v> {
    pub position: Point<'v>,
    pub width: LAddress<'v>,
    pub height: LAddress<'v>,
}

impl Disp for RectBordered<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw lineRect {} {} {} {}",
            self.position.0, self.position.1, self.width, self.height
        )
    }
}

impl<'v> DrawInstruction<'v> for RectBordered<'v> {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(&self.width)) as u32;
        let height = get_num!(mem.get(&self.height)) as u32;
        image.stroked_box(pos, width, height, state.stroke.round() as u32, state.col());
    }
}

#[derive(Debug)]
pub struct Triangle<'v> {
    pub points: (Point<'v>, Point<'v>, Point<'v>),
}
impl<'v> DrawInstruction<'v> for Triangle<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, i: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        let to32 = |n| n as f32;
        let (a, b, c) = (
            map!(point!(mem@self.points.0), to32),
            map!(point!(mem@self.points.1), to32),
            map!(point!(mem@self.points.2), to32),
        );
        i.tri(a, b, c, state.col());
    }
}
impl Disp for Triangle<'_> {
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

#[derive(Debug)]
pub struct Poly<'v> {
    pub(crate) pos: Point<'v>,
    pub(crate) sides: LAddress<'v>,
    pub(crate) radius: LAddress<'v>,
    pub(crate) rot: LAddress<'v>,
}

impl<'v> DrawInstruction<'v> for Poly<'v> {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let pos = map!(point!(mem@self.pos), |n| n as f32);
        image.poly(
            pos,
            get_num!(mem.get(&self.sides)).round() as usize,
            get_num!(mem.get(&self.radius)) as f32,
            get_num!(mem.get(&self.rot)) as f32,
            state.col(),
        );
    }
}

impl Disp for Poly<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw poly {} {} {} {} {}",
            self.pos.0, self.pos.1, self.sides, self.radius, self.rot
        )
    }
}

#[derive(Debug, Default)]
pub struct Flush {
    pub(crate) display: Display,
}
impl LInstruction<'_> for Flush {
    fn run<W: std::io::Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.flush(self.display);
        Flow::Continue
    }
}

impl Disp for Flush {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "drawflush {}", self.display)
    }
}
