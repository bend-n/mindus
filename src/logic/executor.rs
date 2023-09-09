use std::num::NonZeroUsize;

use super::{
    instructions::{Flow, Instr, LInstruction},
    memory::{LRegistry, LVar},
};

#[derive(Debug, Copy, Clone)]
pub(crate) struct Display(usize);
#[derive(Debug, Copy, Clone)]
pub(crate) struct Cell(usize);
#[derive(Copy, Clone)]
pub struct Instruction(pub usize);

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Instruction#{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum ProgramInstruction<'s> {
    Code(&'s str),
    Instr(Instr<'s>),
    UnfinishedJump,
    NoOp,
}

#[derive(Debug, Copy, Clone)]
pub enum Limit {
    /// limited to n
    Limited(NonZeroUsize),
    /// unlimited
    Unlimited,
}

impl Limit {
    /// panics if n != 0
    pub fn limited(n: usize) -> Self {
        Self::Limited(n.try_into().expect("nonzero"))
    }
}

impl Limit {
    pub(crate) fn reached(&self, n: usize) -> bool {
        match self {
            Self::Limited(v) => <NonZeroUsize as Into<usize>>::into(*v) <= n,
            Self::Unlimited => false,
        }
    }
}

#[derive(Copy, Clone)]
pub enum LAddress<'varname> {
    Const(usize),
    Name(&'varname str),
}

impl std::fmt::Debug for LAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(c) => write!(f, "LAddress {c:x}"),
            Self::Name(n) => write!(f, "LAddress {n}"),
        }
    }
}

pub struct LogicExecutor<'varnames> {
    pub(crate) inner: ExecutorContext<'varnames>,
    pub(crate) program: Vec<ProgramInstruction<'varnames>>,
    pub instructions_ran: usize,
    pub iterations: usize,
}

pub struct ExecutorContext<'varnames> {
    pub constants: Vec<LVar<'varnames>>,
    pub memory: LRegistry<'varnames>,
    pub counter: usize,
    pub cells: Vec<Vec<u8>>,
    pub displays: Vec<fimg::Image<Vec<u8>, 4>>,
    pub output: String,
}

impl<'s> ExecutorContext<'s> {
    pub fn set(&mut self, a: LAddress<'s>, b: LAddress<'s>) -> bool {
        match a {
            LAddress::Const(_) => false,
            LAddress::Name(v) => {
                match b {
                    LAddress::Const(n) => {
                        let b = self.constants[n];
                        *self.memory.get_mut(v) = b;
                    }
                    LAddress::Name(n) => {
                        let b = self.memory.get(n);
                        *self.memory.get_mut(v) = b;
                    }
                };
                true
            }
        }
    }

    pub fn get_mut(&mut self, a: LAddress<'s>) -> Option<&mut LVar<'s>> {
        match a {
            LAddress::Const(_) => None,
            LAddress::Name(n) => Some(self.memory.get_mut(n)),
        }
    }

    // please verify n is valid
    pub fn jump(&mut self, Instruction(n): Instruction) {
        self.counter = n;
    }

    pub fn get(&self, a: LAddress<'s>) -> LVar<'s> {
        match a {
            LAddress::Name(n) => self.memory.get(n),
            LAddress::Const(n) => self.constants[n],
        }
    }
}

impl<'s> LogicExecutor<'s> {
    pub fn output(&self) -> &str {
        &self.inner.output
    }

    pub(crate) fn next(&self) -> Instruction {
        Instruction(self.program.len())
    }

    pub(crate) fn last(&self) -> Instruction {
        Instruction(self.program.len() - 1)
    }

    pub(crate) fn add_const(&mut self, var: impl Into<LVar<'s>>) -> LAddress<'s> {
        self.inner.constants.push(var.into());
        LAddress::Const(self.inner.constants.len() - 1)
    }

    pub(crate) fn addr(&self, var: &'s str) -> LAddress<'s> {
        LAddress::Name(var)
    }

    pub(crate) fn add(&mut self, i: impl Into<Instr<'s>>) {
        self.program.push(ProgramInstruction::Instr(i.into()));
    }

    pub(crate) fn valid(&self, Instruction(i): Instruction) -> bool {
        self.program.len() > i
    }

    pub(crate) fn noop(&mut self) {
        self.program.push(ProgramInstruction::NoOp);
    }

    fn run_current(&mut self) -> Flow {
        match &self.program[self.inner.counter] {
            ProgramInstruction::Instr(i) => {
                // println!("run {i:?} ({:?})", self.inner.memory);
                i.run(&mut self.inner)
            }
            ProgramInstruction::UnfinishedJump => {
                panic!("all jumps should have been succesfully init")
            }
            _ => Flow::Continue,
        }
    }

    /// instructions:
    /// if limited, will run n instructions before exiting.
    /// iterations:
    /// if limtited, will loop(go from a end to the start) n times before exiting
    /// unlimited does not mean this function will never return;
    /// a `Stop` instruction will break the loop.
    pub fn run(&mut self, instructions: Limit, iterations: Limit) {
        while !instructions.reached(self.instructions_ran) && !iterations.reached(self.iterations) {
            match self.run_current() {
                Flow::Continue => {}
                Flow::Exit => break,
                Flow::Stay => {
                    self.instructions_ran += 1;
                    continue;
                }
            };
            self.instructions_ran += 1;
            self.inner.counter += 1;
            if self.inner.counter >= self.program.len() {
                self.inner.counter = 0;
                self.iterations += 1;
                self.inner.memory.clear();
            }
        }
    }
}
