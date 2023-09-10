#![feature(let_chains)]
#![allow(clippy::redundant_closure_call)]
#![warn(
    clippy::multiple_unsafe_ops_per_block,
    clippy::missing_const_for_fn,
    clippy::missing_safety_doc,
    unsafe_op_in_unsafe_fn,
    clippy::dbg_macro,
    clippy::perf
)]
mod executor;
pub(crate) mod instructions;
pub(crate) mod lexer;
pub(crate) mod memory;
pub(crate) mod parser;

use std::io::Write;

pub use executor::{Limit, LogicExecutor};
pub use parser::ParserError;

use self::{
    executor::{ExecutorContext, Peripherals},
    memory::LRegistry,
};

impl<W: Write + Default> LogicExecutor<'_, W> {
    pub fn build() -> ProcessorBuilder<W> {
        ProcessorBuilder::default()
    }
}
impl<W: Write> LogicExecutor<'_, W> {
    pub fn with_output(w: W) -> ProcessorBuilder<W> {
        ProcessorBuilder {
            peripherals: Peripherals {
                displays: Vec::new(),
                output: w,
            },
            instruction_limit: Limit::Unlimited,
            iteration_limit: Limit::limited(1),
        }
    }
}

pub struct ProcessorBuilder<W: Write> {
    peripherals: Peripherals<W>,
    instruction_limit: Limit,
    iteration_limit: Limit,
}

impl<W: Write + Default> Default for ProcessorBuilder<W> {
    fn default() -> Self {
        Self {
            peripherals: Peripherals::default(),
            instruction_limit: Limit::Unlimited,
            iteration_limit: Limit::limited(1),
        }
    }
}

impl<W: Write> ProcessorBuilder<W> {
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

    pub fn program(self, program: &str) -> Result<LogicExecutor<'_, W>, ParserError<'_>> {
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
                cells: Vec::new(),
                banks: Vec::new(),
                constants: Vec::new(),
                memory: LRegistry::default(),
                counter: 0,
                peripherals,
            },
            instruction_limit,
            iteration_limit,
        };
        #[cfg(debug_assertions)]
        lexer::print_stream(lexer::lex(program));
        parser::parse(lexer::lex(program), &mut executor)?;
        Ok(executor)
    }
}

#[cfg(test)]
mod test {
    use super::executor::Memory;
    use super::*;

    macro_rules! test {
        (run $fn:ident.mlog $($times:literal times)?;
        $(output = $to_be:literal $(;)?)?
        $(cell[$cell_n:literal][$cell_index:literal] = $what:literal $(;)?)?
        ) => {
            #[test]
            fn $fn() -> Result<(), ParserError<'static>> {
                let v = vec![];
                let mut lex = LogicExecutor::with_output(v)
                    .unlimit_instructions()
                    $(.limit_iterations($times))?
                    .program(include_str!(concat!(stringify!($fn), ".mlog")))?;
                lex.run();
                $(assert_eq!(lex.output(), $to_be);)?
                $(assert_eq!(lex.inner.mem(Memory($cell_n))[$cell_index], $what);)?
                Ok(())
            }
        };
    }

    test!(run fib.mlog; output = b"12586269025");
    test!(run celliterate.mlog 500 times; cell[0][0] = 500.0);
}
