//! supported instrs
//!
//! ```text
//! jump
//! op
//! stop
//! end
//! set
//! read
//! write
//! print
//!
//! draw {color, col, flush, line, rect, lineRect, triangle, stroke, clear}
//! ```
use enum_dispatch::enum_dispatch;
use fimg::Image;
use std::f64::consts::PI;
use std::io::Write as Wr;

use crate::{executor::DisplayState, memory::LRegistry};

use super::{
    executor::{Display, ExecutorContext, Instruction, Memory},
    lexer::Token,
    memory::{LAddress, LVar},
};

#[must_use = "to change control flow"]
pub enum Flow {
    Continue,
    Stay,
    Exit,
}

#[enum_dispatch]
pub trait LInstruction<'v> {
    #[allow(unused_variables)]
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        Flow::Continue
    }
}

#[derive(Debug)]
#[enum_dispatch(LInstruction)]
pub enum Instr<'v> {
    Read(Read<'v>),
    Write(Write<'v>),
    Set(Set<'v>),
    Op1(Op1<'v>),
    Op2(Op2<'v>),
    End(End),
    DrawFlush(DrawFlush),
    Print(Print<'v>),
    Stop(Stop),
    Jump(Jump<'v>),
    AlwaysJump(AlwaysJump),
}

#[enum_dispatch]
pub trait DrawInstruction<'v> {
    #[allow(unused_variables)]
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
    }
}

#[derive(Debug)]
#[enum_dispatch(DrawInstruction)]
pub enum DrawInstr<'v> {
    DrawLine(DrawLine<'v>),
    DrawRectBordered(DrawRectBordered<'v>),
    DrawRectFilled(DrawRectFilled<'v>),
    DrawTriangle(DrawTriangle<'v>),
    Clear(Clear<'v>),
    SetColorDyn(SetColorDyn<'v>),
    SetColorConst(SetColorConst),
    SetStroke(SetStroke<'v>),
}

#[derive(Debug)]
pub struct Read<'v> {
    // index guranteed to never be out of bounds
    pub(crate) index: usize,
    pub(crate) output: LAddress<'v>,
    pub(crate) container: Memory,
}

impl<'v> LInstruction<'v> for Read<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let to = exec.mem(self.container)[self.index];
        let Some(out) = exec.get_mut(self.output) else {
            return Flow::Continue;
        };
        *out = LVar::from(to);
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct Write<'v> {
    // index guranteed to never be out of bounds
    pub(crate) index: usize,
    pub(crate) set: LAddress<'v>,
    pub(crate) container: Memory,
}

impl<'v> LInstruction<'v> for Write<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let LVar::Num(n) = exec.get(self.set) else {
            return Flow::Continue;
        };
        exec.mem(self.container)[self.index] = n;
        Flow::Continue
    }
}
#[derive(Debug)]
pub struct Set<'v> {
    pub(crate) from: LAddress<'v>,
    pub(crate) to: LAddress<'v>,
}
impl<'v> LInstruction<'v> for Set<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        exec.set(self.from, self.to);
        Flow::Continue
    }
}

macro_rules! op_enum {
    ($v:vis enum $name:ident {
        $($variant:ident),+ $(,)?
    }) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        $v enum $name {
            $($variant),+
        }

        impl TryFrom<Token<'_>> for $name {
            type Error = ();
            fn try_from(value: Token<'_>) -> Result<Self, Self::Error> {
                match value {
                    $(Token::$variant => Ok(Self::$variant),)+
                    _ => Err(())
                }
            }
        }
    }
}

op_enum! { pub(crate) enum MathOp1 {
    Floor,
    Not,
    Log,
    Abs,
    Rand,
    Ceil,
    Sqrt,
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,
    Log10,
}}

macro_rules! get_num {
    ($x:expr) => {
        match $x {
            LVar::Num(x) => x,
            _ => return LVar::Null,
        }
    };
    ($x:expr, or ret) => {
        match $x {
            LVar::Num(x) => x,
            _ => return,
        }
    };
}

impl MathOp1 {
    const fn get_fn(self) -> fn(LVar<'_>) -> LVar<'_> {
        macro_rules! num {
            ($c:expr) => {
                |v| LVar::from($c(get_num!(v)))
            };
        }
        macro_rules! flbop {
            ($f: expr, $fn: expr) => {
                $fn($f as u64) as f64
            };
        }
        match self {
            Self::Floor => num!(f64::floor),
            Self::Not => |v| match v {
                LVar::Num(n) => LVar::Num(flbop!(n, |n: u64| !n)),
                _ => LVar::Null,
            },
            Self::Log => num!(f64::ln),
            Self::Abs => num!(f64::abs),
            Self::Rand => |_| LVar::Num(4.0),
            Self::Ceil => num!(f64::ceil),
            Self::Sqrt => num!(f64::sqrt),
            Self::Sin => num!(f64::sin),
            Self::Cos => num!(f64::cos),
            Self::Tan => num!(f64::tan),
            Self::ASin => num!(f64::asin),
            Self::ACos => num!(f64::acos),
            Self::ATan => num!(f64::atan),
            Self::Log10 => num!(f64::log10),
        }
    }
}

#[derive(Debug)]
pub struct Op1<'v> {
    pub(crate) op: fn(LVar<'v>) -> LVar<'v>,
    pub(crate) x: LAddress<'v>,
    pub(crate) out: LAddress<'v>,
}
impl<'v> Op1<'v> {
    pub(crate) const fn new(op: MathOp1, x: LAddress<'v>, out: LAddress<'v>) -> Self {
        Self {
            op: op.get_fn(),
            x,
            out,
        }
    }
}

impl<'s> LInstruction<'s> for Op1<'s> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'s, W>) -> Flow {
        let x = (self.op)(exec.get(self.x));
        if let Some(y) = exec.get_mut(self.out) {
            *y = x;
        }
        Flow::Continue
    }
}

op_enum! { pub(crate) enum MathOp2 {
    Angle,
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    Equal,
    NotEqual,
    And,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
    ShiftLeft,
    ShiftRight,
    BitOr,
    BitAnd,
    ExclusiveOr,
    Max,
    Min,
    AngleDiff,
    Len,
    Noise, // unimplemented
} }

impl MathOp2 {
    const fn get_fn<'a>(self) -> fn(LVar<'a>, LVar<'a>) -> LVar<'a> {
        macro_rules! bop {
            ($op: tt) => {
                |a, b| LVar::from(((get_num!(a) as u64) $op (get_num!(b) as u64)) as f64)
            };
        }
        macro_rules! num {
            ($fn:expr) => {{
                |a, b| LVar::from($fn(get_num!(a), get_num!(b)))
            }};
        }
        macro_rules! op {
            ($op:tt) => {
                num!(|a,b| a $op b)
            }
        }
        match self {
            Self::Angle => num!(|a: f64, b: f64| {
                let mut x = a.atan2(b) * (180.0 / PI);
                if x < 0.0 {
                    x += 360.0;
                }
                x
            }),
            Self::Add => op!(+),
            Self::Sub => op!(-),
            Self::Mul => op!(*),
            Self::Div => op!(/),
            Self::IDiv => bop!(/),
            Self::Mod => op!(%),
            Self::Pow => num!(f64::powf),
            // we kind of interpret strings as numbers so yeah
            Self::Equal | Self::StrictEqual => |a, b| {
                LVar::from(match a {
                    LVar::Num(a) => match b {
                        LVar::Num(b) => a == b,
                        _ => false,
                    },
                    LVar::String(a) => match b {
                        LVar::String(b) => a == b,
                        _ => false,
                    },
                    LVar::Null => matches!(b, LVar::Null),
                })
            },
            Self::NotEqual => |a, b| {
                LVar::from(match a {
                    LVar::Num(a) => match b {
                        LVar::Num(b) => a != b,
                        _ => true,
                    },
                    LVar::String(a) => match b {
                        LVar::String(b) => a != b,
                        _ => true,
                    },
                    LVar::Null => !matches!(b, LVar::Null),
                })
            },
            Self::And => num!(|a, b| a != 0.0 && b != 0.0),
            Self::LessThan => op!(<),
            Self::LessThanEq => op!(<=),
            Self::GreaterThan => op!(>),
            Self::GreaterThanEq => op!(>=),
            Self::ShiftLeft => bop!(<<),
            Self::ShiftRight => bop!(>>),
            Self::BitOr => bop!(|),
            Self::BitAnd => bop!(&),
            Self::ExclusiveOr => bop!(^),
            Self::Max => num!(f64::max),
            Self::Min => num!(f64::min),
            Self::AngleDiff => num!(|a, b| {
                let a = a % (360.0 * PI);
                let b = b % (360.0 * PI);
                f64::min(
                    if (a - b) < 0.0 { a - b + 360.0 } else { a - b },
                    if (b - a) < 0.0 { b - a + 360.0 } else { b - a },
                )
            }),
            Self::Len => num!(|a: f64, b: f64| { (a * a + b * b).sqrt() }),
            Self::Noise => |_, _| LVar::Num(9.0),
        }
    }
}

#[derive(Debug)]
pub struct Op2<'v> {
    pub(crate) op: fn(LVar<'v>, LVar<'v>) -> LVar<'v>,
    pub(crate) a: LAddress<'v>,
    pub(crate) b: LAddress<'v>,
    pub(crate) out: LAddress<'v>,
}
impl<'v> Op2<'v> {
    pub(crate) const fn new(
        op: MathOp2,
        a: LAddress<'v>,
        b: LAddress<'v>,
        out: LAddress<'v>,
    ) -> Self {
        Self {
            op: op.get_fn(),
            a,
            b,
            out,
        }
    }
}

impl<'v> LInstruction<'v> for Op2<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let x = (self.op)(exec.get(self.a), exec.get(self.b));
        if let Some(y) = exec.get_mut(self.out) {
            *y = x;
        }
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct End {}
impl LInstruction<'_> for End {}

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
                match mem.get(self.$v) {
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

#[derive(Debug)]
pub struct SetColorDyn<'v> {
    pub r: LAddress<'v>,
    pub g: LAddress<'v>,
    pub b: LAddress<'v>,
    pub a: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for SetColorDyn<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
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

#[derive(Debug)]
pub struct SetColorConst {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}
impl DrawInstruction<'_> for SetColorConst {
    fn draw(&self, _: &mut LRegistry<'_>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        state.color = (self.r, self.g, self.b, self.a);
    }
}

#[derive(Debug)]
pub struct SetStroke<'v> {
    pub size: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for SetStroke<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, _: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        if let LVar::Num(n) = mem.get(self.size) {
            state.stroke = n;
        }
    }
}

pub type Point<'v> = (LAddress<'v>, LAddress<'v>);
#[rustfmt::skip]
macro_rules! point {
    ($mem:ident@$point:expr) => {{
        let LVar::Num(a) = $mem.get($point.0) else { return; };
        let LVar::Num(b) = $mem.get($point.1) else { return; };
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
pub struct DrawLine<'v> {
    pub point_a: Point<'v>,
    pub point_b: Point<'v>,
}
impl<'v> DrawInstruction<'v> for DrawLine<'v> {
    #[allow(unused_variables)]
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        // i will happily ignore that stroke specifys the stroke of lines
        let a = map!(point!(mem@self.point_a), |n| n as i32);
        let b = map!(point!(mem@self.point_b), |n| n as i32);
        image.line(a, b, state.col());
    }
}

macro_rules! unbounded {
    ($img:ident @ $x:expr => $y:expr) => {
        $img.width() < $x || $img.height() < $y
    };
}

#[derive(Debug)]
pub struct DrawRectFilled<'v> {
    pub position: Point<'v>,
    pub width: LAddress<'v>,
    pub height: LAddress<'v>,
}
impl<'v> DrawInstruction<'v> for DrawRectFilled<'v> {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(self.width), or ret) as u32;
        let height = get_num!(mem.get(self.height), or ret) as u32;
        if unbounded!(image @ pos.0 + width => pos.1 + height) {
            return;
        }
        // SAFETY: bounds checked above
        unsafe { image.filled_box(pos, width, height, state.col()) };
    }
}

#[derive(Debug)]
pub struct DrawRectBordered<'v> {
    pub position: Point<'v>,
    pub width: LAddress<'v>,
    pub height: LAddress<'v>,
}

impl<'v> DrawInstruction<'v> for DrawRectBordered<'v> {
    fn draw(
        &self,
        mem: &mut LRegistry<'v>,
        image: &mut Image<&mut [u8], 4>,
        state: &mut DisplayState,
    ) {
        // happily ignoring that state specifies box stroke width
        let pos = map!(point!(mem@self.position), |n| n as u32);
        let width = get_num!(mem.get(self.width), or ret) as u32;
        let height = get_num!(mem.get(self.height), or ret) as u32;
        if unbounded!(image @ pos.0 + width => pos.1 + height) {
            return;
        }
        // SAFETY: bounds checked above
        unsafe { image.r#box(pos, width, height, state.col()) };
    }
}

#[derive(Debug)]
pub struct DrawTriangle<'v> {
    pub points: (Point<'v>, Point<'v>, Point<'v>),
}
impl<'v> DrawInstruction<'v> for DrawTriangle<'v> {
    fn draw(&self, mem: &mut LRegistry<'v>, i: &mut Image<&mut [u8], 4>, state: &mut DisplayState) {
        let to32 = |n| n as f32;
        let (a, b, c) = (
            map!(point!(mem@self.points.0), to32),
            map!(point!(mem@self.points.1), to32),
            map!(point!(mem@self.points.2), to32),
        );
        if unbounded!(i @ a.0 as u32 => a.1 as u32)
            || unbounded!(i @ b.0 as u32 => b.1 as u32)
            || unbounded!(i @ c.0 as u32 => c.1 as u32)
        {
            return;
        }
        // SAFETY: bounds are checked
        unsafe { i.tri(a, b, c, state.col()) };
    }
}

#[derive(Debug)]
pub struct DrawFlush {
    pub(crate) display: Display,
}
impl LInstruction<'_> for DrawFlush {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.flush(self.display);
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct Print<'v> {
    pub(crate) val: LAddress<'v>,
}
impl LInstruction<'_> for Print<'_> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        write!(exec.output, "{}", exec.get(self.val)).unwrap();
        Flow::Continue
    }
}

op_enum! { pub enum ConditionOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
} }

impl ConditionOp {
    const fn get_fn<'v>(self) -> fn(LVar<'v>, LVar<'v>) -> bool {
        macro_rules! op {
            ($op:tt) => {
                |a, b| match a {
                    LVar::Num(a) => match b {
                        LVar::Num(b) => a $op b,
                        _ => false,
                    },
                    _ => false,
                }
            };
        }
        match self {
            Self::Equal | Self::StrictEqual => |a, b| match a {
                LVar::Num(a) => match b {
                    LVar::Num(b) => a == b,
                    _ => false,
                },
                LVar::String(a) => match b {
                    LVar::String(b) => a == b,
                    _ => false,
                },
                LVar::Null => matches!(b, LVar::Null),
            },
            Self::NotEqual => |a, b| match a {
                LVar::Num(a) => match b {
                    LVar::Num(b) => a != b,
                    _ => true,
                },
                LVar::String(a) => match b {
                    LVar::String(b) => a != b,
                    _ => true,
                },
                LVar::Null => !matches!(b, LVar::Null),
            },
            Self::LessThan => op!(<),
            Self::LessThanEq => op!(<=),
            Self::GreaterThan => op!(>),
            Self::GreaterThanEq => op!(>=),
        }
    }
}

#[derive(Debug)]
pub struct AlwaysJump {
    pub(crate) to: Instruction,
}
impl LInstruction<'_> for AlwaysJump {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.jump(self.to);
        Flow::Stay
    }
}

#[derive(Debug)]
pub struct Jump<'v> {
    pub(crate) op: fn(LVar<'v>, LVar<'v>) -> bool,
    pub(crate) to: Instruction,
    pub(crate) a: LAddress<'v>,
    pub(crate) b: LAddress<'v>,
}
impl<'v> Jump<'v> {
    pub fn new(op: ConditionOp, to: Instruction, a: LAddress<'v>, b: LAddress<'v>) -> Self {
        Self {
            op: op.get_fn(),
            to,
            a,
            b,
        }
    }
}

impl<'v> LInstruction<'v> for Jump<'v> {
    #[allow(unused_variables)]
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        if (self.op)(exec.get(self.a), exec.get(self.b)) {
            exec.jump(self.to);
            Flow::Stay
        } else {
            Flow::Continue
        }
    }
}

#[derive(Debug)]
pub struct Stop {}
impl LInstruction<'_> for Stop {
    fn run<W: Wr>(&self, _: &mut ExecutorContext<'_, W>) -> Flow {
        Flow::Exit
    }
}
