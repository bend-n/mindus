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
pub trait LInstruction: Display {
    fn run<'v, W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow;
}

#[derive(Debug, Copy, Clone)]
#[enum_dispatch(LInstruction)]
pub enum Instr {
    Op2(Op2),
    Jump(Jump),
    AlwaysJump(AlwaysJump),
    Set(Set),
    Op1(Op1),
    Read(io::Read),
    Write(io::Write),
    DrawFlush(draw::Flush),
    DynJump(DynJump),
    Print(io::Print),
    Stop(Stop),
    End(End),
}
impl Display for Instr {
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

#[derive(Debug, Copy, Clone)]
pub struct Set {
    pub(crate) from: LAddress,
    pub(crate) to: LAddress,
}
impl LInstruction for Set {
    fn run<'v, W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        exec.set(self.from, self.to.clone());
        Flow::Continue
    }
}

impl Display for Set {
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

#[derive(Debug, Copy, Clone)]
pub struct Op1 {
    op: for<'v> fn(&LVar<'v>) -> f64,
    x: LAddress,
    out: LAddress,
}
impl<'v> Op1 {
    pub(crate) const fn new(op: MathOp1, x: LAddress, out: LAddress) -> Self {
        Self {
            op: op.get_fn(),
            x,
            out,
        }
    }
}

impl LInstruction for Op1 {
    fn run<'s, W: Write>(&self, exec: &mut ExecutorContext<'s, W>) -> Flow {
        let x = (self.op)(exec.get(self.x));
        *exec.get_mut(self.out) = LVar::Num(x);
        Flow::Continue
    }
}

impl Display for Op1 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { x, out, .. } = self;
        write!(f, "op .. {out} {x}")
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Op2 {
    op: for<'v> fn(&LVar<'v>, &LVar<'v>) -> f64,
    a: LAddress,
    b: LAddress,
    out: LAddress,
}
impl Op2 {
    pub(crate) const fn new(op: MathOp2, a: LAddress, b: LAddress, out: LAddress) -> Self {
        Self {
            op: op.get_fn(),
            a,
            b,
            out,
        }
    }
}

impl LInstruction for Op2 {
    fn run<'v, W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let x = (self.op)(exec.get(self.a), exec.get(self.b));
        exec.memory[self.out] = LVar::Num(x);
        Flow::Continue
    }
}

impl Display for Op2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { a, b, out, .. } = self;
        write!(f, "op .. {out} {a} {b}")
    }
}

#[derive(Debug, Copy, Clone)]
pub struct End {}

impl LInstruction for End {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
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

#[derive(Debug, Copy, Clone)]
pub struct AlwaysJump {
    pub(crate) to: Instruction,
}
impl LInstruction for AlwaysJump {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.jump(self.to);
        Flow::Stay
    }
}

impl Display for AlwaysJump {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "jump {} always", self.to.get())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Jump {
    op: for<'v> fn(&LVar<'v>, &LVar<'v>) -> bool,
    pub(crate) to: Instruction,
    a: LAddress,
    b: LAddress,
}
impl Jump {
    pub fn new(op: ConditionOp, to: Instruction, a: LAddress, b: LAddress) -> Self {
        Self {
            op: op.get_fn(),
            to,
            a,
            b,
        }
    }
}

impl LInstruction for Jump {
    fn run<'v, W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        if (self.op)(exec.get(self.a), exec.get(self.b)) {
            exec.jump(self.to);
            Flow::Stay
        } else {
            Flow::Continue
        }
    }
}

impl Display for Jump {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { to, a, b, .. } = self;
        write!(f, "jump .. {} {a} {b}", to.get())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DynJump {
    pub to: LAddress,
    pub proglen: usize,
}

impl LInstruction for DynJump {
    fn run<'v, W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        if let &LVar::Num(n) = exec.get(self.to) {
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

impl Display for DynJump {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "set @counter {}", self.to)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Stop {}
impl LInstruction for Stop {
    fn run<W: Write>(&self, _: &mut ExecutorContext<'_, W>) -> Flow {
        Flow::Exit
    }
}

impl Display for Stop {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "stop")
    }
}
