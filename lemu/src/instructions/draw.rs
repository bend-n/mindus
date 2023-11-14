use super::{get_num, Flow, LInstruction};
use crate::{
    debug::{info::DebugInfo, printable::Printable},
    executor::{Display, DisplayState, ExecutorContext},
    memory::{LAddress, LRegistry, LVar},
};
use enum_dispatch::enum_dispatch;
use fimg::Image;
use std::fmt::{self, Display as Disp};
use vecto::Vec2;

pub const INSTRS: &[&str] = &[
    "clear", "color", "col", "stroke", "line", "rect", "lineRect", "triangle", "poly", "linePoly",
];

#[enum_dispatch]
pub trait Apply: Disp {
    fn apply(self, image: Image<&mut [u8], 4>, state: &mut DisplayState);
}

#[derive(Debug)]
#[enum_dispatch(Apply)]
pub enum Drawn {
    Line(LineD),
    RectBordered(RectBorderedD),
    RectFilled(RectFilledD),
    Triangle(TriangleD),
    Clear(ClearD),
    SetColor(SetColorD),
    SetStroke(SetStrokeD),
    Poly(PolyD),
    LinePoly(LinePolyD),
}

impl std::fmt::Display for Drawn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

pub trait Frozen<A: Apply>: Printable {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<A>;
}

macro_rules! dinstr {
    [$($x:ident),+] => {
        #[derive(Debug, Copy, Clone)]
        pub enum DrawInstr {
            $($x($x),)+
        }

        $(impl From<$x> for DrawInstr {
            fn from(v: $x) -> Self { Self::$x(v) }
        })+

        impl Frozen<Drawn> for DrawInstr {
            fn freeze(&self, mem: &LRegistry<'_>) -> Option<Drawn> {
                Some(match self {
                    $(Self::$x(i) => Drawn::from(i.freeze(mem)?),)+
                })
            }
        }
        impl Printable for DrawInstr {
            fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
                match self {
                    $(Self::$x(i) => i.print(info, f)),+
                }
            }
        }
    }
}

dinstr! {
    Line,
    RectBordered,
    RectFilled,
    Triangle,
    Clear,
    SetColor,
    SetCol,
    SetStroke,
    Poly,
    LinePoly
}

#[derive(Debug, Copy, Clone)]

pub struct Clear {
    pub r: LAddress,
    pub g: LAddress,
    pub b: LAddress,
}

#[derive(Debug, Copy, Clone)]
pub struct ClearD((u8, u8, u8));

impl Apply for ClearD {
    fn apply(self, mut image: Image<&mut [u8], 4>, _: &mut DisplayState) {
        let (r, g, b) = self.0;
        for [r2, g2, b2, a2] in image.chunked_mut() {
            (*r2, *b2, *g2, *a2) = (r, g, b, 255);
        }
    }
}

macro_rules! u8 {
    ($self:ident, $mem:expr, $($v:ident)+) => {
        ($(match $mem.get($self.$v) {
            LVar::Num(n) => n.round() as u8,
            _ => return None,
        },)+)
    };
}

impl Frozen<ClearD> for Clear {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<ClearD> {
        Some(ClearD(u8!(self, mem, r g b)))
    }
}

impl Printable for Clear {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw clear {} {} {}",
            info[self.r], info[self.g], info[self.b]
        )
    }
}

impl Disp for ClearD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "draw clear {} {} {}", self.0.0, self.0.1, self.0.2)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SetColorD((u8, u8, u8, u8));

impl Apply for SetColorD {
    fn apply(self, _: Image<&mut [u8], 4>, state: &mut DisplayState) {
        state.color = self.0;
    }
}

#[derive(Debug, Copy, Clone)]

pub struct SetColor {
    pub r: LAddress,
    pub g: LAddress,
    pub b: LAddress,
    pub a: LAddress,
}

impl Frozen<SetColorD> for SetColor {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<SetColorD> {
        Some(SetColorD(u8!(self, mem, r g b a)))
    }
}

impl Printable for SetColor {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw color {} {} {} {}",
            info[self.r], info[self.g], info[self.b], info[self.a]
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SetCol {
    pub col: LAddress,
}

impl Printable for SetCol {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "draw col ")?;
        match &info[self.col] {
            crate::debug::info::VarData::Variable(v) => write!(f, "{v}"),
            crate::debug::info::VarData::Constant(c) => match c {
                LVar::Num(n) => write!(f, "0x{:0<6x}", *n as u32),
                LVar::String(s) => write!(f, r#""{s}""#),
            },
        }
    }
}

impl Frozen<SetColorD> for SetCol {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<SetColorD> {
        let [r, g, b, a] = fimg::Pack::unpack(mem.get(self.col).num()? as u32);
        Some(SetColorD((r, g, b, a)))
    }
}

impl Disp for SetColorD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw color {} {} {} {}",
            self.0.0, self.0.1, self.0.2, self.0.3
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct SetStroke {
    pub size: LAddress,
}

#[derive(Debug, Copy, Clone)]
pub struct SetStrokeD(f64);

impl Apply for SetStrokeD {
    fn apply(self, _: Image<&mut [u8], 4>, state: &mut DisplayState) {
        state.stroke = self.0;
    }
}

impl Frozen<SetStrokeD> for SetStroke {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<SetStrokeD> {
        mem.get(self.size).num().map(SetStrokeD)
    }
}

impl Disp for SetStrokeD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "draw stroke {}", self.0)
    }
}

impl Printable for SetStroke {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "draw stroke {}", info[self.size])
    }
}

pub type Point = (LAddress, LAddress);
#[rustfmt::skip]
macro_rules! point {
    ($mem:ident@$point:expr) => {
        ($mem.get($point.0).num()?, $mem.get($point.1).num()?)
    }
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

#[derive(Debug)]
pub struct LineD(Vec2, Vec2);

impl Apply for LineD {
    fn apply(self, mut image: Image<&mut [u8], 4>, state: &mut DisplayState) {
        image.thick_line(self.0, self.1, state.stroke as f32, state.col());
    }
}

impl Frozen<LineD> for Line {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<LineD> {
        Some(LineD(
            map!(point!(mem@self.point_a), |n| n as f32).into(),
            map!(point!(mem@self.point_b), |n| n as f32).into(),
        ))
    }
}

impl Disp for LineD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw line {} {} {} {}",
            self.0.x, self.0.y, self.1.x, self.1.y
        )
    }
}

impl Printable for Line {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw line {} {} {} {}",
            info[self.point_a.0], info[self.point_a.1], info[self.point_b.0], info[self.point_b.1]
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct RectFilled {
    pub position: Point,
    pub width: LAddress,
    pub height: LAddress,
}

#[derive(Debug)]
pub struct RectFilledD((u32, u32), (u32, u32));

impl Apply for RectFilledD {
    fn apply(self, mut image: Image<&mut [u8], 4>, state: &mut DisplayState) {
        image.filled_box(self.0, self.1.0, self.1.1, state.col());
    }
}

impl Frozen<RectFilledD> for RectFilled {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<RectFilledD> {
        Some(RectFilledD(
            map!(point!(mem@self.position), |n| n as u32),
            (
                get_num!(mem.get(self.width)) as u32,
                get_num!(mem.get(self.height)) as u32,
            ),
        ))
    }
}

impl Disp for RectFilledD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw rect {} {} {} {}",
            self.0.0, self.0.1, self.1.0, self.1.1
        )
    }
}

impl Printable for RectFilled {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw rect {} {} {} {}",
            info[self.position.0], info[self.position.1], info[self.width], info[self.height]
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct RectBordered {
    pub position: Point,
    pub width: LAddress,
    pub height: LAddress,
}

impl Disp for RectBorderedD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw lineRect {} {} {} {}",
            self.0.0, self.0.1, self.1.0, self.1.1
        )
    }
}

impl Printable for RectBordered {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw lineRect {} {} {} {}",
            info[self.position.0], info[self.position.1], info[self.width], info[self.height]
        )
    }
}

#[derive(Debug)]
pub struct RectBorderedD((u32, u32), (u32, u32));

impl Apply for RectBorderedD {
    fn apply(self, mut image: Image<&mut [u8], 4>, state: &mut DisplayState) {
        image.stroked_box(
            self.0,
            self.1.0,
            self.1.1,
            state.stroke.round() as u32,
            state.col(),
        );
    }
}

impl Frozen<RectBorderedD> for RectBordered {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<RectBorderedD> {
        Some(RectBorderedD(
            map!(point!(mem@self.position), |n| n as u32),
            (
                get_num!(mem.get(self.width)) as u32,
                get_num!(mem.get(self.height)) as u32,
            ),
        ))
    }
}

#[derive(Debug, Copy, Clone)]

pub struct Triangle {
    pub points: (Point, Point, Point),
}

#[derive(Debug)]
pub struct TriangleD(Vec2, Vec2, Vec2);

impl Apply for TriangleD {
    fn apply(self, mut image: Image<&mut [u8], 4>, state: &mut DisplayState) {
        image.tri::<f32>(self.0, self.1, self.2, state.col());
    }
}

impl Frozen<TriangleD> for Triangle {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<TriangleD> {
        Some(TriangleD(
            map!(point!(mem@self.points.0), |n| n as f32).into(),
            map!(point!(mem@self.points.1), |n| n as f32).into(),
            map!(point!(mem@self.points.2), |n| n as f32).into(),
        ))
    }
}

impl Disp for TriangleD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw triangle {} {} {} {} {} {}",
            self.0.x, self.0.y, self.1.x, self.1.y, self.2.x, self.2.y
        )
    }
}

impl Printable for Triangle {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw triangle {} {} {} {} {} {}",
            info[self.points.0.0],
            info[self.points.0.1],
            info[self.points.1.0],
            info[self.points.1.1],
            info[self.points.2.0],
            info[self.points.2.1]
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

#[derive(Debug)]
pub enum PolyD {
    Poly(Vec2, usize, f32, f32),
    Circle((i32, i32), i32),
}

impl Apply for PolyD {
    fn apply(self, mut image: Image<&mut [u8], 4>, state: &mut DisplayState) {
        match self {
            PolyD::Poly(pos, sides, radius, rotation) => {
                image.poly(pos, sides, radius, rotation, state.col())
            }
            PolyD::Circle(pos, radius) => image.circle(pos, radius, state.col()),
        }
    }
}

impl Frozen<PolyD> for Poly {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<PolyD> {
        let sides = get_num!(mem.get(self.sides)).round() as usize;
        Some(if sides < 90 {
            PolyD::Poly(
                map!(point!(mem@self.pos), |n| n as f32).into(),
                sides,
                get_num!(mem.get(self.radius)) as f32,
                get_num!(mem.get(self.rot)) as f32,
            )
        } else {
            PolyD::Circle(
                map!(point!(mem@self.pos), |n: f64| n.round() as i32),
                get_num!(mem.get(self.radius)).round() as i32,
            )
        })
    }
}

impl Disp for PolyD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PolyD::Poly(Vec2 { x, y }, sides, radius, rot) => {
                write!(f, "draw poly {x} {y} {sides} {radius} {rot}")
            }
            PolyD::Circle((x, y), sides) => write!(f, "draw poly {x} {y} {sides}"),
        }
    }
}

impl Printable for Poly {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw poly {} {} {} {} {}",
            info[self.pos.0], info[self.pos.1], info[self.sides], info[self.radius], info[self.rot],
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

#[derive(Debug)]
/// border_Circle doesnt let you specify a stroke
pub struct LinePolyD(Vec2, usize, f32, f32);

impl Apply for LinePolyD {
    fn apply(self, mut image: Image<&mut [u8], 4>, state: &mut DisplayState) {
        image.border_poly(
            self.0,
            self.1,
            self.2,
            self.3,
            state.stroke as f32,
            state.col(),
        )
    }
}

impl Frozen<LinePolyD> for LinePoly {
    fn freeze(&self, mem: &LRegistry<'_>) -> Option<LinePolyD> {
        Some(LinePolyD(
            map!(point!(mem@self.pos), |n| n as f32).into(),
            get_num!(mem.get(self.sides)).round() as usize,
            get_num!(mem.get(self.radius)) as f32,
            get_num!(mem.get(self.rot)) as f32,
        ))
    }
}

impl Disp for LinePolyD {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "draw linePoly {} {} {} {} {}",
            self.0.x, self.0.y, self.1, self.2, self.3
        )
    }
}

impl Printable for LinePoly {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "draw linePoly {} {} {} {} {}",
            info[self.pos.0], info[self.pos.1], info[self.sides], info[self.radius], info[self.rot],
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

impl Printable for Flush {
    fn print(&self, _: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "drawflush {}", self.display)
    }
}
