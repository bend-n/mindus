use std::num::NonZeroUsize;

use super::{
    instructions::{Flow, Instr, LInstruction},
    memory::{LRegistry, LVar},
};

#[derive(Debug, Copy, Clone)]
pub struct Display(usize);
#[derive(Debug, Copy, Clone)]
// negative means bank, positive means cell
pub struct Memory(pub(crate) i8);
impl Memory {
    pub(crate) fn limit(self, i: usize) -> usize {
        if self.0 < 0 {
            i.min(BANK_SIZE)
        } else {
            i.min(CELL_SIZE)
        }
    }
}
const BANK_SIZE: usize = 512;
const CELL_SIZE: usize = 64;
#[derive(Copy, Clone)]
pub struct Instruction(pub usize);

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Instruction#{}", self.0)
    }
}

#[derive(Debug, Default)]
pub struct Peripherals {
    pub displays: Vec<fimg::Image<Vec<u8>, 4>>,
    pub output: String,
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
    /// if limited, will run n instructions before exiting.
    pub instruction_limit: Limit,
    /// if limtited, will loop(go from a end to the start) n times before exiting
    /// both unlimited does not mean this function will never return;
    /// a `Stop` instruction will break the loop.
    pub iteration_limit: Limit,
    pub(crate) inner: ExecutorContext<'varnames>,
    pub(crate) program: Vec<ProgramInstruction<'varnames>>,
    pub instructions_ran: usize,
    pub iterations: usize,
}

pub struct ExecutorContext<'varnames> {
    // maximum of 128 elements, so can use ~60KB
    pub cells: Vec<f64>, // [64] | [64] | [f64; 64] // screw world cells
    // maximum of 127 elements, so can use ~500KB
    pub banks: Vec<f64>, // [512] | [512] | [f64; 512]
    pub constants: Vec<LVar<'varnames>>,
    pub memory: LRegistry<'varnames>,
    pub counter: usize,
    pub peripherals: Peripherals,
}

impl<'s> ExecutorContext<'s> {
    pub fn mem(&mut self, Memory(m): Memory) -> &mut [f64] {
        if m < 0 {
            let m = (m + 1).unsigned_abs() as usize;
            &mut self.banks[m * BANK_SIZE..m * BANK_SIZE + BANK_SIZE]
        } else {
            let m = m as usize;
            &mut self.cells[m * CELL_SIZE..m * CELL_SIZE + CELL_SIZE]
        }
    }

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
        &self.inner.peripherals.output
    }

    pub(crate) fn bank(&mut self, n: usize) -> Memory {
        assert!(n != 0);
        if n * BANK_SIZE > self.inner.banks.len() {
            self.inner.banks.resize(n * BANK_SIZE, 0.0);
        }
        Memory(-(((self.inner.banks.len() - BANK_SIZE) / BANK_SIZE) as i8) - 1)
    }

    pub(crate) fn cell(&mut self, n: usize) -> Memory {
        assert!(n != 0);
        if n * CELL_SIZE > self.inner.cells.len() {
            self.inner.cells.resize(n * CELL_SIZE, 0.0);
        }
        Memory(((self.inner.cells.len() - CELL_SIZE) / CELL_SIZE) as i8)
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

    pub(crate) const fn addr(&self, var: &'s str) -> LAddress<'s> {
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

    pub fn run(&mut self) {
        while !self.instruction_limit.reached(self.instructions_ran)
            && !self.iteration_limit.reached(self.iterations)
        {
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
