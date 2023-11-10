use fimg::Image;
use std::{collections::VecDeque, io::Write as Wr};

use super::{
    Display, Drawing, Executor, ExecutorContext, Instruction, Limit, Memory, PInstr, UPInstr,
    BANK_SIZE, CELL_SIZE,
};
use crate::{
    code::Code,
    debug::info::DebugInfo,
    instructions::{DrawInstr, Instr},
    lexer::Token,
    memory::LRegistry,
};

/// for internal use by [parser](crate::parser) only
pub struct ExecutorBuilderInternal<'v, W: Wr> {
    displays: Vec<Image<Vec<u8>, 4>>,
    pub(crate) program: Vec<UPInstr<'v>>,
    output: Option<W>,
    banks: Vec<f64>,
    cells: Vec<f64>,
    iteration_limit: Limit,
    instruction_limit: Limit,
    pub(crate) mem: LRegistry<'v>,
    pub(crate) debug_info: DebugInfo<'v>,
}

impl<'s, W: Wr> ExecutorBuilderInternal<'s, W> {
    pub(crate) fn new(w: Option<W>, d: Vec<Image<Vec<u8>, 4>>) -> Self {
        Self {
            output: w,
            displays: d,
            program: vec![],
            banks: vec![],
            cells: vec![],
            iteration_limit: Limit::limited(1),
            instruction_limit: Limit::Unlimited,
            mem: LRegistry::default(),
            debug_info: DebugInfo::default(),
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

    pub(crate) fn code(&mut self, v: Box<[Token<'s>]>) {
        self.program.push(UPInstr::Code(v));
    }

    pub(crate) fn bank(&mut self, n: usize) -> Memory {
        assert!(n != 0);
        if n * BANK_SIZE > self.banks.len() {
            self.banks.resize(n * BANK_SIZE, 0.0);
            println!("resized");
        }
        Memory::Bank(((self.banks.len() - BANK_SIZE) / BANK_SIZE) as u8)
    }

    pub(crate) fn cell(&mut self, n: usize) -> Memory {
        assert!(n != 0);
        if n * CELL_SIZE > self.cells.len() {
            self.cells.resize(n * CELL_SIZE, 0.0);
        }
        Memory::Cell(((self.cells.len() - CELL_SIZE) / CELL_SIZE) as u8)
    }

    pub(crate) fn next(&self) -> Instruction {
        Instruction(self.program.len())
    }

    pub(crate) fn last(&self) -> Instruction {
        Instruction(self.program.len() - 1)
    }

    pub(crate) fn add(&mut self, i: impl Into<Instr>) {
        self.program.push(UPInstr::Instr(i.into()));
    }

    pub(crate) fn draw(&mut self, i: impl Into<DrawInstr>) {
        self.program.push(UPInstr::Draw(i.into()));
    }

    pub(crate) fn valid(&self, Instruction(i): Instruction) -> bool {
        self.program.len() > i
    }

    pub(crate) fn display(&mut self, n: usize) -> Result<Display, usize> {
        self.displays
            .get(n.checked_sub(1).ok_or(n)?)
            .map(|_| Display(n - 1))
            .ok_or(n)
    }

    pub(crate) fn finish(self) -> Executor<'s, W> {
        fn cst<const N: usize>(a: Vec<f64>) -> Box<[[f64; N]]> {
            let len = a.len();
            let ptr: *mut [f64] = Box::into_raw(a.into());
            let ptr: *mut [[f64; N]] =
                core::ptr::slice_from_raw_parts_mut(ptr.cast::<[f64; N]>(), len / N);
            unsafe { Box::from_raw(ptr) }
        }
        let program = Code::new(
            self.program
                .into_iter()
                .map(|v| match v {
                    UPInstr::Instr(i) => PInstr::Instr(i),
                    UPInstr::Draw(i) => PInstr::Draw(i),
                    UPInstr::Comment(c) => PInstr::Comment(c),
                    UPInstr::UnfinishedJump => panic!("all jumps should have finished"),
                    UPInstr::Code(c) => PInstr::Code(c),
                })
                .collect::<Box<[PInstr]>>(),
        );
        let Self {
            instruction_limit,
            iteration_limit,
            displays,
            output,
            banks,
            debug_info,
            cells,
            mem,
            ..
        } = self;
        Executor {
            instruction_limit,
            iteration_limit,
            inner: ExecutorContext {
                cells: cst::<CELL_SIZE>(cells),
                banks: cst::<BANK_SIZE>(banks),
                memory: mem,
                counter: 0,
                iterations: 0,
                display: Drawing {
                    displays: displays.into(),
                    buffer: VecDeque::new(),
                },
                output,
            },
            instructions_ran: 0,
            debug_info,
            program,
        }
    }
}
