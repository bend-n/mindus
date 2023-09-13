mod builder;
use super::{
    instructions::{DrawInstr, DrawInstruction, Flow, Instr, LInstruction},
    memory::{LAddress, LRegistry, LVar},
};
pub use builder::ExecutorBuilderInternal;
use fimg::Image;
use std::{collections::VecDeque, io::Write, num::NonZeroUsize, pin::Pin};

#[derive(Debug, Copy, Clone)]
pub struct Display(pub usize);
impl Default for Display {
    fn default() -> Self {
        Self(1)
    }
}
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
pub const BANK_SIZE: usize = 512;
pub const CELL_SIZE: usize = 64;

#[derive(Copy, Clone)]
pub struct Instruction(pub usize);

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Instruction#{}", self.0)
    }
}

#[derive(Debug)]
pub enum PInstr<'s> {
    Instr(Instr<'s>),
    Draw(DrawInstr<'s>),
    Code(String),
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
    /// gets pointed to by drawbuf
    pub(crate) program: Pin<Box<[PInstr<'varnames>]>>,
    /// Counter for the number of instructions we have run so far.
    pub instructions_ran: usize,
    /// Counter for the number of iterations we have run so far.
    pub iterations: usize,
}

pub enum UPInstr<'s> {
    Instr(Instr<'s>),
    Draw(DrawInstr<'s>),
    UnfinishedJump,
    Code(String),
    NoOp,
}

pub struct Drawing<'v> {
    pub displays: Box<[fimg::Image<Vec<u8>, 4>]>,
    /// points to `Executor.program`
    pub buffer: VecDeque<*const DrawInstr<'v>>,
}

// SAFETY: false
#[cfg(feature = "__send__")]
unsafe impl Send for Drawing<'_> {}

impl<'v> Drawing<'v> {
    fn buffer(&mut self, i: &DrawInstr<'v>) {
        self.buffer.push_back(i);
    }
}
pub struct ExecutorContext<'varnames, W: Write> {
    // maximum of 128 elements, so can use ~60KB
    pub cells: Box<[[f64; CELL_SIZE]]>, // screw world cells
    // maximum of 127 elements, so can use ~500KB
    pub banks: Box<[[f64; BANK_SIZE]]>,
    pub memory: LRegistry<'varnames>,
    pub counter: usize,
    pub display: Drawing<'varnames>,
    pub output: Option<W>,
}

pub struct DisplayState {
    pub color: (u8, u8, u8, u8),
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
            stroke: 1.0,
        }
    }
}

impl<'s, W: Write> ExecutorContext<'s, W> {
    pub fn flush(&mut self, to: Display) {
        let mut state = DisplayState::default();
        // SAFETY: safe as long as the instruction isnt held too long
        while let Some(d) = unsafe { self.display.buffer.pop_front().map(|v| &*v) } {
            d.draw(
                &mut self.memory,
                &mut self.display.displays[to.0].as_mut(),
                &mut state,
            );
        }
    }

    pub fn mem(&mut self, Memory(m): Memory) -> &mut [f64] {
        if m < 0 {
            let m = (m + 1).unsigned_abs() as usize;
            &mut self.banks[m]
        } else {
            let m = m as usize;
            &mut self.cells[m]
        }
    }

    pub fn set(&mut self, a: &LAddress<'s>, b: LAddress<'s>) -> bool {
        self.memory.set(a, b)
    }

    pub fn get_mut(&mut self, a: &LAddress<'s>) -> Option<&mut LVar<'s>> {
        self.memory.get_mut(a)
    }

    // please verify n is valid
    pub fn jump(&mut self, Instruction(n): Instruction) {
        self.counter = n;
    }

    pub fn get<'a>(&'a self, a: &'a LAddress<'s>) -> &LVar<'s> {
        self.memory.get(a)
    }
}

/// Returned by the [`output`](Executor::output).function.
pub struct Output<W: Write> {
    /// Everything created by a `print` instruction.
    pub output: Option<W>,
    /// Logic displays that were drawn with `draw` instructions.
    pub displays: Box<[Image<Vec<u8>, 4>]>,
    /// Memory banks, written to with the `write`/`read` instructions
    pub cells: Box<[[f64; CELL_SIZE]]>,
    /// Memory cells, written to with the `write`/`read` instructions
    pub banks: Box<[[f64; BANK_SIZE]]>,
}

impl<'s, W: Write> Executor<'s, W> {
    /// Consume this executor, returning all output.
    pub fn output(mut self) -> Output<W> {
        for display in &mut *self.inner.display.displays {
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
                // println!("run {i:?} ({:?})", self.inner.memory);
                i.run(&mut self.inner)
            }
            PInstr::Draw(i) => {
                self.inner.display.buffer(i);
                Flow::Continue
            }
            _ => Flow::Continue,
        }
    }

    /// Begin code execution.
    pub fn run(&mut self) {
        while !self.instruction_limit.reached(self.instructions_ran)
            && !self.iteration_limit.reached(self.iterations)
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
                self.iterations += 1;
                self.inner.memory.clear();
            }
        }
    }
}
