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
mod cop;
pub mod draw;
pub mod io;
mod mop;
mod mop2;

pub use cop::ConditionOp;
pub use draw::{DrawInstr, DrawInstruction};
use enum_dispatch::enum_dispatch;
pub use mop::MathOp1;
pub use mop2::MathOp2;
use std::{
    fmt::{self, Display, Formatter},
    io::Write,
};

use super::{
    executor::{ExecutorContext, Instruction},
    lexer::Token,
    memory::{LAddress, LVar},
};

pub const OPS: &[&str] = &[
    "equal",
    "notEqual",
    "lessThan",
    "lessThanEq",
    "greaterThan",
    "greaterThanEq",
    "strictEqual",
    "always",
    "add",
    "sub",
    "mul",
    "div",
    "idiv",
    "mod",
    "pow",
    "land",
    "not",
    "shl",
    "shr",
    "or",
    "and",
    "xor",
    "max",
    "min",
    "angle",
    "angleDiff",
    "len",
    "noise",
    "abs",
    "log",
    "log10",
    "floor",
    "ceil",
    "sqrt",
    "rand",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
];

#[must_use = "to change control flow"]
#[derive(Default)]
pub enum Flow {
    #[default]
    Continue,
    Stay,
    Exit,
}

#[enum_dispatch]
pub trait LInstruction<'v>: Display {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow;
}

#[derive(Debug)]
#[enum_dispatch(LInstruction)]
pub enum Instr<'v> {
    Op2(Op2<'v>),
    Jump(Jump<'v>),
    AlwaysJump(AlwaysJump<'v>),
    Set(Set<'v>),
    Op1(Op1<'v>),
    Read(io::Read<'v>),
    Write(io::Write<'v>),
    DrawFlush(draw::Flush),
    DynJump(DynJump<'v>),
    Print(io::Print<'v>),
    Stop(Stop),
    End(End),
}
impl Display for Instr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Op2(i) => write!(f, "{i}"),
            Self::Jump(i) => write!(f, "{i}"),
            Self::AlwaysJump(i) => write!(f, "{i}"),
            Self::Set(i) => write!(f, "{i}"),
            Self::Op1(i) => write!(f, "{i}"),
            Self::Read(i) => write!(f, "{i}"),
            Self::Write(i) => write!(f, "{i}"),
            Self::DrawFlush(i) => write!(f, "{i}"),
            Self::DynJump(i) => write!(f, "{i}"),
            Self::Print(i) => write!(f, "{i}"),
            Self::Stop(i) => write!(f, "{i}"),
            Self::End(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug)]
pub struct Set<'v> {
    pub(crate) from: LAddress<'v>,
    pub(crate) to: LAddress<'v>,
}
impl<'v> LInstruction<'v> for Set<'v> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        exec.set(&self.from, self.to.clone());
        Flow::Continue
    }
}

impl Display for Set<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "set {} {}", self.from, self.to)
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

        impl<'a> TryFrom<Token<'a>> for $name {
            type Error = Token<'a>;
            fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
                match value {
                    $(Token::$variant => Ok(Self::$variant),)+
                    v => Err(v)
                }
            }
        }

        impl<'a> From<$name> for Token<'a> {
            fn from(value: $name) -> Self {
                match value {
                    $($name::$variant => Self::$variant,)+
                }
            }
        }
    }
}
use op_enum;

macro_rules! get_num {
    ($x:expr) => {
        match $x {
            LVar::Num(x) => *x,
            _ => return Default::default(),
        }
    };
}
use get_num;

#[derive(Debug)]
pub struct Op1<'v> {
    op_id: MathOp1,
    op: fn(&LVar<'v>) -> f64,
    x: LAddress<'v>,
    out: LAddress<'v>,
}
impl<'v> Op1<'v> {
    pub(crate) const fn new(op: MathOp1, x: LAddress<'v>, out: LAddress<'v>) -> Self {
        Self {
            op_id: op,
            op: op.get_fn(),
            x,
            out,
        }
    }
}

impl<'s> LInstruction<'s> for Op1<'s> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'s, W>) -> Flow {
        let x = (self.op)(exec.get(&self.x));
        if let Some(y) = exec.get_mut(&self.out) {
            *y = LVar::Num(x);
        }
        Flow::Continue
    }
}

impl Display for Op1<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { op_id, x, out, .. } = self;
        write!(f, "op {} {out} {x}", Token::from(*op_id))
    }
}

#[derive(Debug)]
pub struct Op2<'v> {
    op_id: MathOp2,
    op: fn(&LVar<'v>, &LVar<'v>) -> f64,
    a: LAddress<'v>,
    b: LAddress<'v>,
    out: LAddress<'v>,
}
impl<'v> Op2<'v> {
    pub(crate) const fn new(
        op: MathOp2,
        a: LAddress<'v>,
        b: LAddress<'v>,
        out: LAddress<'v>,
    ) -> Self {
        Self {
            op_id: op,
            op: op.get_fn(),
            a,
            b,
            out,
        }
    }
}

impl<'v> LInstruction<'v> for Op2<'v> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let x = (self.op)(exec.get(&self.a), exec.get(&self.b));
        if let Some(y) = exec.get_mut(&self.out) {
            *y = LVar::from(x);
        }
        Flow::Continue
    }
}

impl Display for Op2<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self {
            op_id, a, b, out, ..
        } = self;
        write!(f, "op {} {out} {a} {b}", Token::from(*op_id))
    }
}

#[derive(Debug)]
pub struct End {}

impl LInstruction<'_> for End {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.memory.clear();
        exec.iterations += 1;
        // SAFETY: if we exist, 0 exists.
        unsafe { exec.jump(Instruction::new(0)) };
        Flow::Stay
    }
}

impl Display for End {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "end")
    }
}

#[derive(Debug)]
pub struct AlwaysJump<'s> {
    pub(crate) to: Instruction,
    pub(crate) label: Option<&'s str>,
}
impl LInstruction<'_> for AlwaysJump<'_> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.jump(self.to);
        Flow::Stay
    }
}

impl Display for AlwaysJump<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.label {
            None => write!(f, "jump {} always", self.to.get()),
            Some(l) => write!(f, "jump {l} always"),
        }
    }
}

#[derive(Debug)]
pub struct Jump<'v> {
    label: Option<&'v str>,
    op_id: ConditionOp,
    op: fn(&LVar<'v>, &LVar<'v>) -> bool,
    pub(crate) to: Instruction,
    a: LAddress<'v>,
    b: LAddress<'v>,
}
impl<'v> Jump<'v> {
    pub fn new(
        op: ConditionOp,
        to: Instruction,
        a: LAddress<'v>,
        b: LAddress<'v>,
        label: Option<&'v str>,
    ) -> Self {
        Self {
            op_id: op,
            op: op.get_fn(),
            label,
            to,
            a,
            b,
        }
    }
}

impl<'v> LInstruction<'v> for Jump<'v> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        if (self.op)(exec.get(&self.a), exec.get(&self.b)) {
            exec.jump(self.to);
            Flow::Stay
        } else {
            Flow::Continue
        }
    }
}

impl Display for Jump<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self {
            op_id,
            label,
            to,
            a,
            b,
            ..
        } = self;
        write!(f, "jump ")?;
        match label {
            Some(v) => write!(f, "{v} "),
            None => write!(f, "{} ", to.get()),
        }?;
        write!(f, "{} {a} {b}", Token::from(*op_id))
    }
}

#[derive(Debug)]
pub struct DynJump<'v> {
    pub to: LAddress<'v>,
    pub proglen: usize,
}

impl<'v> LInstruction<'v> for DynJump<'v> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        if let &LVar::Num(n) = exec.get(&self.to) {
            let i = n.round() as usize;
            if i < self.proglen {
                // SAFETY: just checked bounds
                exec.jump(unsafe { Instruction::new(i) });
                return Flow::Stay;
            }
        }
        Flow::Continue
    }
}

impl Display for DynJump<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "set @counter {}", self.to)
    }
}

#[derive(Debug)]
pub struct Stop {}
impl LInstruction<'_> for Stop {
    fn run<W: Write>(&self, _: &mut ExecutorContext<'_, W>) -> Flow {
        Flow::Exit
    }
}

impl Display for Stop {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "stop")
    }
}
