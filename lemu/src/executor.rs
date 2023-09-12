use std::{collections::VecDeque, io::Write, num::NonZeroUsize, pin::Pin};

use fimg::Image;

use crate::instructions::{DrawInstr, DrawInstruction};

use super::{
    instructions::{Flow, Instr, LInstruction},
    memory::{LAddress, LRegistry, LVar},
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

#[derive(Debug)]
pub enum PInstr<'s> {
    Code(&'s str),
    Instr(Instr<'s>),
    Draw(DrawInstr<'s>),
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

pub struct LogicExecutor<'varnames, W: Write> {
    /// if limited, will run n instructions before exiting.
    pub instruction_limit: Limit,
    /// if limtited, will loop(go from a end to the start) n times before exiting
    /// both unlimited does not mean this function will never return;
    /// a `Stop` instruction will break the loop.
    pub iteration_limit: Limit,
    pub(crate) inner: ExecutorContext<'varnames, W>,
    /// gets pointed to by drawbuf
    pub(crate) program: Pin<Box<[PInstr<'varnames>]>>,
    pub instructions_ran: usize,
    pub iterations: usize,
}

pub enum UPInstr<'s> {
    Instr(Instr<'s>),
    Draw(DrawInstr<'s>),
    UnfinishedJump,
    Code(&'s str),
    NoOp,
}

/// for use by [`parser`](crate::parser)
pub struct ExecutorBuilder<'v, W: Write> {
    displays: Vec<Image<Vec<u8>, 4>>,
    pub(crate) program: Vec<UPInstr<'v>>,
    output: W,
    banks: Vec<f64>,
    cells: Vec<f64>,
    iteration_limit: Limit,
    instruction_limit: Limit,
    mem: usize,
}

impl<'s, W: Write> ExecutorBuilder<'s, W> {
    pub(crate) fn new(w: W, d: Vec<Image<Vec<u8>, 4>>) -> Self {
        Self {
            output: w,
            displays: d,
            program: vec![],
            banks: vec![],
            cells: vec![],
            iteration_limit: Limit::limited(1),
            instruction_limit: Limit::Unlimited,
            mem: 0,
        }
    }

    pub(crate) fn inslimit(&mut self, ilimit: Limit) -> &mut Self {
        self.instruction_limit = ilimit;
        self
    }

    pub(crate) fn itrlimit(&mut self, ilimit: Limit) -> &mut Self {
        self.iteration_limit = ilimit;
        self
    }

    pub(crate) fn jmp(&mut self) {
        self.program.push(UPInstr::UnfinishedJump);
    }

    pub(crate) fn bank(&mut self, n: usize) -> Memory {
        assert!(n != 0);
        if n * BANK_SIZE > self.banks.len() {
            self.banks.resize(n * BANK_SIZE, 0.0);
        }
        Memory(-(((self.banks.len() - BANK_SIZE) / BANK_SIZE) as i8) - 1)
    }

    pub(crate) fn cell(&mut self, n: usize) -> Memory {
        assert!(n != 0);
        if n * CELL_SIZE > self.cells.len() {
            self.cells.resize(n * CELL_SIZE, 0.0);
        }
        Memory(((self.cells.len() - CELL_SIZE) / CELL_SIZE) as i8)
    }

    pub(crate) fn next(&self) -> Instruction {
        Instruction(self.program.len())
    }

    pub(crate) fn last(&self) -> Instruction {
        Instruction(self.program.len() - 1)
    }

    pub(crate) fn mem(&mut self, size: usize) {
        self.mem = size;
    }

    pub(crate) fn add(&mut self, i: impl Into<Instr<'s>>) {
        self.program.push(UPInstr::Instr(i.into()));
    }

    pub(crate) fn draw(&mut self, i: impl Into<DrawInstr<'s>>) {
        self.program.push(UPInstr::Draw(i.into()));
    }

    pub(crate) fn valid(&self, Instruction(i): Instruction) -> bool {
        self.program.len() > i
    }

    pub(crate) fn noop(&mut self) {
        self.program.push(UPInstr::NoOp);
    }

    pub(crate) fn display(&mut self, n: usize) -> Result<Display, usize> {
        self.displays
            .get(n.checked_sub(1).ok_or(n)?)
            .map(|_| Display(n - 1))
            .ok_or(n)
    }

    pub(crate) fn finish(self) -> LogicExecutor<'s, W> {
        fn cst<const N: usize>(a: Vec<f64>) -> Box<[[f64; N]]> {
            let len = a.len();
            let ptr: *mut [f64] = Box::into_raw(a.into());
            let ptr: *mut [[f64; N]] =
                core::ptr::slice_from_raw_parts_mut(ptr.cast::<[f64; N]>(), len / N);
            unsafe { Box::from_raw(ptr) }
        }
        let Self {
            instruction_limit,
            iteration_limit,
            program,
            displays,
            output,
            banks,
            cells,
            mem,
        } = self;
        LogicExecutor {
            instruction_limit,
            iteration_limit,
            inner: ExecutorContext {
                cells: cst::<CELL_SIZE>(cells),
                banks: cst::<BANK_SIZE>(banks),
                memory: LRegistry::new(mem),
                counter: 0,
                display: Drawing {
                    displays: displays.into(),
                    buffer: VecDeque::new(),
                },
                output,
            },
            program: Pin::new(
                program
                    .into_iter()
                    .map(|v| {
                        match v {
                            UPInstr::Instr(i) => PInstr::Instr(i),
                            UPInstr::Draw(i) => PInstr::Draw(i),
                            UPInstr::NoOp => PInstr::NoOp,
                            UPInstr::UnfinishedJump => panic!("all jumps should have finished"),
                            // todo
                            UPInstr::Code(c) => PInstr::Code(c),
                        }
                    })
                    .collect::<Box<[PInstr]>>(),
            ),
            instructions_ran: 0,
            iterations: 0,
        }
    }
}

pub struct Drawing<'v> {
    pub displays: Box<[fimg::Image<Vec<u8>, 4>]>,
    /// points to `Executor.program`
    pub buffer: VecDeque<*const DrawInstr<'v>>,
}

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
    pub output: W,
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

    pub fn set(&mut self, a: LAddress<'s>, b: LAddress<'s>) -> bool {
        self.memory.set(a, b)
    }

    pub fn get_mut(&mut self, a: LAddress<'s>) -> Option<&mut LVar<'s>> {
        self.memory.get_mut(a)
    }

    // please verify n is valid
    pub fn jump(&mut self, Instruction(n): Instruction) {
        self.counter = n;
    }

    pub fn get(&self, a: LAddress<'s>) -> LVar<'s> {
        self.memory.get(a)
    }
}

pub struct Output<W: Write> {
    pub output: W,
    pub displays: Box<[Image<Vec<u8>, 4>]>,
    pub cells: Box<[[f64; CELL_SIZE]]>,
    pub banks: Box<[[f64; BANK_SIZE]]>,
}

impl<'s, W: Write> LogicExecutor<'s, W> {
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

    fn run_current(&mut self) -> Flow {
        match &self.program[self.inner.counter] {
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
