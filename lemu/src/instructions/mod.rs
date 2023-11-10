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
pub use draw::{DrawInstr, Frozen};
use enum_dispatch::enum_dispatch;
pub use mop::MathOp1;
pub use mop2::MathOp2;
use std::{fmt, io::Write};

use crate::debug::{info::DebugInfo, printable::Printable};

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
pub trait LInstruction: Printable {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow;
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

impl Printable for Instr {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        match self {
            Self::Op2(i) => i.print(info, f),
            Self::Jump(i) => i.print(info, f),
            Self::AlwaysJump(i) => i.print(info, f),
            Self::Set(i) => i.print(info, f),
            Self::Op1(i) => i.print(info, f),
            Self::Read(i) => i.print(info, f),
            Self::Write(i) => i.print(info, f),
            Self::DrawFlush(i) => i.print(info, f),
            Self::DynJump(i) => i.print(info, f),
            Self::Print(i) => i.print(info, f),
            Self::Stop(i) => i.print(info, f),
            Self::End(i) => i.print(info, f),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Set {
    pub(crate) from: LAddress,
    pub(crate) to: LAddress,
}
impl LInstruction for Set {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        exec.set(self.from, self.to);
        Flow::Continue
    }
}

impl Printable for Set {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "set {} {}", info[self.from], info[self.to])
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

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                write!(f, "{}", Token::from(*self))
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

// not part of op_enum due to rem
macro_rules! op_impl {
    ($name:ident, ptr type = $ptr:ty { $($own:ident => $fn:ident,)+ }) => {
        impl $name {
            pub const fn get_fn(self) -> $ptr {
                match self {
                    $(Self::$own => $fn,)+
                }
            }
        }

        impl TryFrom<$ptr> for $name {
            type Error = ();
            fn try_from(f:$ptr) -> Result<Self, ()> {
                match f {
                    $(f if f == $fn => Ok(Self::$own),)+
                    _ => Err(()),
                }
            }
        }
    }
}
use op_impl;

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
impl Op1 {
    pub(crate) const fn new(op: MathOp1, x: LAddress, out: LAddress) -> Self {
        Self {
            op: op.get_fn(),
            x,
            out,
        }
    }
}

impl LInstruction for Op1 {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        let x = (self.op)(exec.get(self.x));
        *exec.get_mut(self.out) = LVar::Num(x);
        Flow::Continue
    }
}

impl Printable for Op1 {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        let op = mop::MathOp1::try_from(self.op).unwrap();
        write!(f, "op {op} {} {}", info[self.out], info[self.x])
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
    #[inline]
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        let x = (self.op)(exec.get(self.a), exec.get(self.b));
        exec.memory[self.out] = LVar::Num(x);
        Flow::Continue
    }
}

impl Printable for Op2 {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        let op = mop2::MathOp2::try_from(self.op).unwrap();
        write!(
            f,
            "op {op} {} {} {}",
            info[self.out], info[self.a], info[self.b]
        )
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

impl Printable for End {
    fn print(&self, _: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
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

impl Printable for AlwaysJump {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "jump ")?;
        match info.label(self.to) {
            Some(l) => f.write_str(l)?,
            None => write!(f, "{}", self.to.get())?,
        }
        write!(f, " always")
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
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        if (self.op)(exec.get(self.a), exec.get(self.b)) {
            exec.jump(self.to);
            Flow::Stay
        } else {
            Flow::Continue
        }
    }
}

impl Printable for Jump {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        let op = ConditionOp::try_from(self.op).unwrap();
        write!(f, "jump {op} ")?;
        match info.label(self.to) {
            Some(l) => f.write_str(l)?,
            None => write!(f, "{}", self.to.get())?,
        };
        write!(f, " {} {}", info[self.a], info[self.b])
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DynJump {
    pub to: LAddress,
    pub proglen: usize,
}

impl LInstruction for DynJump {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
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

impl Printable for DynJump {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "set @counter {}", info[self.to])
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Stop {}
impl LInstruction for Stop {
    fn run<W: Write>(&self, _: &mut ExecutorContext<'_, W>) -> Flow {
        Flow::Exit
    }
}

impl Printable for Stop {
    fn print(&self, _: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "stop")
    }
}
