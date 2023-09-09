mod executor;
mod field;
pub(crate) mod instructions;
pub(crate) mod lexer;
pub(crate) mod memory;
pub(crate) mod parser;

pub(crate) use field::{LogicField, TryFromU8Error};

pub use executor::{Limit, LogicExecutor};
pub use parser::ParserError;

use self::{
    executor::{ExecutorContext, Peripherals},
    memory::LRegistry,
};

impl LogicExecutor<'_> {
    pub fn build() -> ProcessorBuilder {
        ProcessorBuilder::default()
    }
}

pub struct ProcessorBuilder {
    peripherals: Peripherals,
    instruction_limit: Limit,
    iteration_limit: Limit,
}

pub enum Cell {
    /// 64 slots
    MemoryCell,
    /// 128 slots
    WorldCell,
    /// 512 (!!!) slots
    MemoryBank,
}

impl Default for ProcessorBuilder {
    fn default() -> Self {
        Self {
            peripherals: Peripherals::default(),
            instruction_limit: Limit::Unlimited,
            iteration_limit: Limit::limited(1),
        }
    }
}

impl ProcessorBuilder {
    pub fn limit_iterations(self, n: usize) -> Self {
        Self {
            iteration_limit: Limit::limited(n),
            ..self
        }
    }

    pub fn unlimit_iterations(self) -> Self {
        Self {
            iteration_limit: Limit::Unlimited,
            ..self
        }
    }

    pub fn unlimit_instructions(self) -> Self {
        Self {
            iteration_limit: Limit::Unlimited,
            ..self
        }
    }

    pub fn limit_instructions(self, n: usize) -> Self {
        Self {
            instruction_limit: Limit::limited(n),
            ..self
        }
    }

    pub fn cell(mut self, cell: Cell) -> Self {
        self.peripherals.cells.push(vec![
            0;
            match cell {
                Cell::MemoryCell => 64,
                Cell::WorldCell => 128,
                Cell::MemoryBank => 512,
            }
        ]);
        self
    }

    pub fn program(self, program: &str) -> Result<LogicExecutor<'_>, ParserError<'_>> {
        let Self {
            peripherals,
            instruction_limit,
            iteration_limit,
        } = self;
        let mut executor = LogicExecutor {
            instructions_ran: 0,
            iterations: 0,
            program: Vec::new(),
            inner: ExecutorContext {
                constants: Vec::new(),
                memory: LRegistry::default(),
                counter: 0,
                peripherals,
            },
            instruction_limit,
            iteration_limit,
        };
        parser::parse(lexer::lex(program), &mut executor)?;
        Ok(executor)
    }
}

#[test]
fn execute() -> Result<(), ParserError<'static>> {
    let mut lex = ProcessorBuilder::default()
        .unlimit_instructions()
        .limit_iterations(1)
        .program(
            r#"
    set n 50
    set previous 0
    set fib 1
    op add end n 1
    set i 2
loop:
    jump ret greaterThanEq i end
    op add tmp previous fib
    set previous fib
    set fib tmp
    op add i i 1
    jump loop always
ret:
    print fib
    stop
    "#,
        )?;
    lex.run();
    assert_eq!(lex.output(), "12586269025");
    Ok(())
}
