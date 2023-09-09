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
//! draw
//! packcolor
//! ```
use enum_dispatch::enum_dispatch;
use std::f64::consts::PI;

use super::{
    executor::{Cell, Display, ExecutorContext, Instruction, LAddress},
    lexer::Token,
    memory::LVar,
};

#[must_use = "to change control flow"]
pub enum Flow {
    Continue,
    Stay,
    Exit,
}

#[derive(Debug)]
#[enum_dispatch(LInstruction)]
pub(crate) enum Instr<'v> {
    Read(Read<'v>),
    Write(Write),
    Set(Set<'v>),
    Op1(Op1<'v>),
    Op2(Op2<'v>),
    End(End),
    DrawFlush(DrawFlush),
    Print(Print<'v>),
    PackColor(PackColor<'v>),
    Stop(Stop),
    Jump(Jump<'v>),
    AlwaysJump(AlwaysJump),
}

#[enum_dispatch]
pub trait LInstruction<'v> {
    #[allow(unused_variables)]
    fn run(&self, exec: &mut ExecutorContext<'v>) -> Flow {
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct Read<'v> {
    pub(crate) index: usize,
    pub(crate) output: LAddress<'v>,
    pub(crate) from: Cell,
}
impl LInstruction<'_> for Read<'_> {}

#[derive(Debug)]
pub struct Write {
    pub(crate) index: usize,
    pub(crate) set: u64,
    pub(crate) to: Cell,
}
impl LInstruction<'_> for Write {}
#[derive(Debug)]
pub struct Set<'v> {
    pub(crate) from: LAddress<'v>,
    pub(crate) to: LAddress<'v>,
}
impl<'v> LInstruction<'v> for Set<'v> {
    fn run(&self, exec: &mut ExecutorContext<'v>) -> Flow {
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
}

impl MathOp1 {
    fn run(self, v: LVar<'_>) -> LVar<'_> {
        macro_rules! num {
            ($c:expr) => {
                LVar::from($c(get_num!(v)))
            };
        }
        macro_rules! flbop {
            ($f: expr, $fn: expr) => {
                $fn($f as u64) as f64
            };
        }
        match self {
            Self::Floor => num!(f64::floor),
            Self::Not => match v {
                LVar::Num(n) => LVar::Num(flbop!(n, |n: u64| !n)),
                _ => LVar::Null,
            },
            Self::Log => num!(f64::ln),
            Self::Abs => num!(f64::abs),
            Self::Rand => LVar::Num(4.0),
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
    pub(crate) op: MathOp1,
    pub(crate) x: LAddress<'v>,
    pub(crate) out: LAddress<'v>,
}
impl<'s> LInstruction<'s> for Op1<'s> {
    fn run(&self, exec: &mut ExecutorContext<'s>) -> Flow {
        let x = self.op.run(exec.get(self.x));
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
    fn run<'v>(self, a: LVar<'v>, b: LVar<'v>) -> LVar<'v> {
        macro_rules! bop {
            ($op: tt) => {
                LVar::from(((get_num!(a) as u64) $op (get_num!(b) as u64)) as f64)
            };
        }
        macro_rules! num {
            ($fn:expr) => {{
                let c: fn(f64, f64) -> _ = $fn;
                LVar::from(c(get_num!(a), get_num!(b)))
            }};
        }
        macro_rules! op {
            ($op:tt) => {
                num!(|a,b| a $op b)
            }
        }
        match self {
            Self::Angle => num!(|a, b| {
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
            Self::Equal | Self::StrictEqual => op!(==),
            Self::NotEqual => op!(!=),
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
            Self::Len => num!(|a, b| { (a * a + b * b).sqrt() }),
            Self::Noise => LVar::Num(9.0),
        }
    }
}

#[derive(Debug)]
pub struct Op2<'v> {
    pub(crate) op: MathOp2,
    pub(crate) a: LAddress<'v>,
    pub(crate) b: LAddress<'v>,
    pub(crate) out: LAddress<'v>,
}
impl<'v> LInstruction<'v> for Op2<'v> {
    fn run(&self, exec: &mut ExecutorContext<'v>) -> Flow {
        let x = self.op.run(exec.get(self.a), exec.get(self.b));
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
pub struct Draw {
    // todo
}
// impl LInstruction for Draw {}

#[derive(Debug)]
pub struct DrawFlush {
    pub(crate) display: Display,
}
impl LInstruction<'_> for DrawFlush {}

#[derive(Debug)]
pub struct Print<'v> {
    pub(crate) val: LAddress<'v>,
}
impl LInstruction<'_> for Print<'_> {
    fn run(&self, exec: &mut ExecutorContext<'_>) -> Flow {
        use std::fmt::Write;

        write!(exec.peripherals.output, "{}", exec.get(self.val)).unwrap();
        Flow::Continue
    }
}

macro_rules! cond_enum {
    ($($v:ident),+ $(,)?) => {
        op_enum! { pub enum ConditionOp {
            $($v,)+
        } }

        impl ConditionOp {
            fn run<'v>(self, a: LVar<'v>, b: LVar<'v>) -> bool {
                let var = match self {
                    $(Self::$v => MathOp2::$v.run(a, b),)+
                };
                match var {
                    LVar::Num(n) => n != 0.0,
                    _ => false,
                }
            }
        }

    }
}

cond_enum! {
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
}

#[derive(Debug)]
pub struct AlwaysJump {
    pub(crate) to: Instruction,
}
impl LInstruction<'_> for AlwaysJump {
    fn run(&self, exec: &mut ExecutorContext<'_>) -> Flow {
        exec.jump(self.to);
        Flow::Stay
    }
}

#[derive(Debug)]
pub struct Jump<'v> {
    pub(crate) op: ConditionOp,
    pub(crate) to: Instruction,
    pub(crate) a: LAddress<'v>,
    pub(crate) b: LAddress<'v>,
}
impl<'v> LInstruction<'v> for Jump<'v> {
    #[allow(unused_variables)]
    fn run(&self, exec: &mut ExecutorContext<'v>) -> Flow {
        if self.op.run(exec.get(self.a), exec.get(self.b)) {
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
    fn run(&self, _: &mut ExecutorContext<'_>) -> Flow {
        Flow::Exit
    }
}

#[derive(Debug)]
pub struct PackColor<'v> {
    color: (u8, u8, u8, u8),
    output: LAddress<'v>,
}
impl LInstruction<'_> for PackColor<'_> {}
