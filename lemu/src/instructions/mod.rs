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
use std::io::Write;

use super::{
    executor::{ExecutorContext, Instruction},
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
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        Flow::Continue
    }
}

#[derive(Debug)]
#[enum_dispatch(LInstruction)]
pub enum Instr<'v> {
    Op2(Op2<'v>),
    Jump(Jump<'v>),
    AlwaysJump(AlwaysJump),
    Set(Set<'v>),
    Op1(Op1<'v>),
    Read(io::Read<'v>),
    Write(io::Write<'v>),
    DrawFlush(draw::DrawFlush),
    DynJump(DynJump<'v>),
    Print(io::Print<'v>),
    Stop(Stop),
    End(End),
}

#[derive(Debug)]
pub struct Set<'v> {
    pub(crate) from: LAddress<'v>,
    pub(crate) to: LAddress<'v>,
}
impl<'v> LInstruction<'v> for Set<'v> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
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
use op_enum;

macro_rules! get_num {
    ($x:expr) => {
        match $x {
            LVar::Num(x) => x,
            _ => return LVar::null(),
        }
    };
    ($x:expr, or ret) => {
        match $x {
            LVar::Num(x) => x,
            _ => return,
        }
    };
}
use get_num;

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
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'s, W>) -> Flow {
        let x = (self.op)(exec.get(self.x));
        if let Some(y) = exec.get_mut(self.out) {
            *y = x;
        }
        Flow::Continue
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
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
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
pub struct AlwaysJump {
    pub(crate) to: Instruction,
}
impl LInstruction<'_> for AlwaysJump {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
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

#[derive(Debug)]
pub struct DynJump<'v> {
    pub to: LAddress<'v>,
    pub proglen: usize,
}

impl<'v> LInstruction<'v> for DynJump<'v> {
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        if let LVar::Num(n) = exec.get(self.to) {
            let i = n.round() as usize;
            if i < self.proglen {
                exec.jump(Instruction(i));
                return Flow::Stay;
            }
        }
        Flow::Continue
    }
}

impl<'v> LInstruction<'v> for Jump<'v> {
    #[allow(unused_variables)]
    fn run<W: Write>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
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
    fn run<W: Write>(&self, _: &mut ExecutorContext<'_, W>) -> Flow {
        Flow::Exit
    }
}
