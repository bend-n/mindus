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
                let mut lex = LogicExecutor::build()
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

    test!(run fib.mlog; output = "12586269025");
    test!(run celliterate.mlog 500 times; cell[0][0] = 500.0);
}
