mod builder;

use crate::{
    debug::{info::DebugInfo, printable::Printable},
    instructions::draw::Drawn,
};

use super::{
    code::{Code, PInstr},
    instructions::{DrawInstr, Flow, Frozen, Instr, LInstruction},
    lexer::Token,
    memory::{LAddress, LRegistry, LVar},
};
pub use builder::ExecutorBuilderInternal;
use fimg::Image;
use std::{collections::VecDeque, io::Write, num::NonZeroUsize};

#[derive(Debug, Copy, Clone, Default)]
pub struct Display(pub usize);

impl std::fmt::Display for Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "display{}", self.0 + 1)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Memory {
    Cell(u8),
    Bank(u8),
}

impl Memory {
    pub(crate) const fn fits(self, i: usize) -> bool {
        match self {
            Self::Bank(_) => i < BANK_SIZE,
            Self::Cell(_) => i < CELL_SIZE,
        }
    }

    pub(crate) const fn size(&self) -> usize {
        match self {
            Self::Bank(_) => BANK_SIZE,
            Self::Cell(_) => CELL_SIZE,
        }
    }
}

impl std::fmt::Display for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bank(n) => write!(f, "bank{}", n + 1),
            Self::Cell(n) => write!(f, "cell{}", n + 1),
        }
    }
}

pub const BANK_SIZE: usize = 512;
pub const CELL_SIZE: usize = 64;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Instruction(usize);

impl Instruction {
    /// # Safety
    /// verify n is valid.
    pub const unsafe fn new(n: usize) -> Self {
        Self(n)
    }

    pub const fn get(self) -> usize {
        self.0
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Instruction#{}", self.0)
    }
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
    pub(crate) const fn reached(self, n: usize) -> bool {
        match self {
            Self::Limited(v) => v.get() <= n,
            Self::Unlimited => false,
        }
    }
}

/// One time use logic executor.
pub struct Executor<'varnames, W: Write> {
    /// if limited, will run n instructions before exiting.
    pub instruction_limit: Limit,
    /// if limtited, will loop(go from a end to the start) n times before exiting
    /// both unlimited does not mean this function will never return;
    /// a `Stop` instruction will break the loop.
    pub iteration_limit: Limit,
    pub(crate) inner: ExecutorContext<'varnames, W>,
    /// gets pointed to by drawbuf (pls no move)
    pub(crate) program: Code<'varnames>,
    /// Counter for the number of instructions we have run so far.
    pub instructions_ran: usize,
    debug_info: DebugInfo<'varnames>,
}

impl<W: Write> std::fmt::Display for Executor<'_, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.program.print(&self.debug_info, f)
    }
}

#[derive(Debug)]
pub enum UPInstr<'s> {
    Instr(Instr),
    Draw(DrawInstr),
    UnfinishedJump,
    Code(Box<[Token<'s>]>),
    Comment(&'s str),
}

pub struct Drawing {
    pub displays: Box<[(fimg::Image<Vec<u8>, 4>, DisplayState)]>,
    pub buffer: VecDeque<Drawn>,
}

impl Drawing {
    fn buffer(&mut self, i: Drawn) {
        self.buffer.push_back(i);
    }
}
pub struct ExecutorContext<'strings, W: Write> {
    // maximum of 128 elements, so can use ~60KB
    pub cells: Box<[[f64; CELL_SIZE]]>, // screw world cells
    // maximum of 127 elements, so can use ~500KB
    pub banks: Box<[[f64; BANK_SIZE]]>,
    pub memory: LRegistry<'strings>,
    pub counter: usize,
    pub display: Drawing,
    pub output: Option<W>,
    /// Counter for the number of iterations we have run so far.
    pub iterations: usize,
}

/// State of a display.
pub struct DisplayState {
    /// Color to draw
    pub color: (u8, u8, u8, u8),
    /// Stroke to draw
    pub stroke: f64,
}

impl DisplayState {
    pub const fn col(&self) -> [u8; 4] {
        [self.color.0, self.color.1, self.color.2, self.color.3]
    }
}

impl Default for DisplayState {
    fn default() -> Self {
        Self {
            color: Default::default(),
            stroke: 5.0,
        }
    }
}

impl<'s, W: Write> ExecutorContext<'s, W> {
    pub fn buffer(&mut self, instr: &DrawInstr) {
        if let Some(i) = instr.freeze(&self.memory) {
            self.display.buffer(i)
        }
    }

    pub fn flush(&mut self, to: Display) {
        let (ref mut img, ref mut state) = &mut self.display.displays[to.0];
        while let Some(d) = self.display.buffer.pop_front() {
            use crate::instructions::draw::Apply;
            #[cfg(feature = "debug")]
            comat::cprintln!("{d:blue}");
            d.apply(img.as_mut(), state);
        }
    }

    pub fn mem(&mut self, m: Memory) -> &mut [f64] {
        match m {
            Memory::Bank(m) => &mut self.banks[m as usize],
            Memory::Cell(m) => &mut self.cells[m as usize],
        }
    }

    pub fn set(&mut self, a: LAddress, b: LAddress) {
        self.memory[a] = self.memory[b].clone();
    }

    pub fn get_mut(&mut self, a: LAddress) -> &mut LVar<'s> {
        &mut self.memory[a]
    }

    pub fn jump(&mut self, Instruction(n): Instruction) {
        self.counter = n;
    }

    pub fn get<'a>(&'a self, a: LAddress) -> &LVar<'s> {
        &self.memory[a]
    }
}

/// Returned by the [`output`](Executor::output).function.
pub struct Output<W: Write> {
    /// Everything created by a `print` instruction.
    pub output: Option<W>,
    /// Logic displays that were drawn with `draw` instructions.
    pub displays: Box<[(Image<Vec<u8>, 4>, DisplayState)]>,
    /// Memory banks, written to with the `write`/`read` instructions
    pub cells: Box<[[f64; CELL_SIZE]]>,
    /// Memory cells, written to with the `write`/`read` instructions
    pub banks: Box<[[f64; BANK_SIZE]]>,
}

impl<'s, W: Write> Executor<'s, W> {
    /// Consume this executor, returning all output.
    pub fn output(mut self) -> Output<W> {
        for (display, _) in &mut *self.inner.display.displays {
            // TODO make the instructions draw flipped-ly
            display.flip_v();
        }
        Output {
            output: self.inner.output,
            displays: self.inner.display.displays,
            cells: self.inner.cells,
            banks: self.inner.banks,
        }
    }

    /// # Safety
    ///
    /// `counter` *must* be in bounds.
    unsafe fn run_current(&mut self) -> Flow {
        // SAFETY: yee
        match unsafe { self.program.get_unchecked(self.inner.counter) } {
            PInstr::Instr(i) => {
                #[cfg(feature = "debug")]
                {
                    let mut instr = String::new();
                    i.print(&self.debug_info, &mut instr).unwrap();
                    let mut mem = String::new();
                    self.inner.memory.print(&self.debug_info, &mut mem).unwrap();
                    comat::cprintln!(
                        "{black}{:0<2} | {green}{instr} {black}({mem}){reset}",
                        self.inner.counter
                    );
                }

                i.run(&mut self.inner)
            }
            PInstr::Draw(i) => {
                self.inner.buffer(i);
                Flow::Continue
            }
            _ => Flow::Continue,
        }
    }

    /// Begin code execution.
    pub fn run(&mut self) {
        while !self.instruction_limit.reached(self.instructions_ran)
            && !self.iteration_limit.reached(self.inner.iterations)
        {
            // SAFETY: we have a check
            match unsafe { self.run_current() } {
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
                self.inner.iterations += 1;
            }
        }
    }
}
