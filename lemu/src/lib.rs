//! crate for [MLOG](https://mindustrygame.github.io/wiki/logic/0-introduction/#what-is-mindustry-logic) emulation.
#![feature(let_chains)]
#![allow(clippy::redundant_closure_call)]
#![warn(
    clippy::multiple_unsafe_ops_per_block,
    clippy::missing_const_for_fn,
    clippy::redundant_pub_crate,
    clippy::missing_safety_doc,
    clippy::imprecise_flops,
    unsafe_op_in_unsafe_fn,
    clippy::dbg_macro,
    missing_docs
)]
mod executor;
mod instructions;
mod lexer;
mod memory;
mod parser;

use std::io::Write;

pub use executor::{Executor, Output};
use executor::{ExecutorBuilderInternal, Limit};
use fimg::Image;
pub use parser::ParserError;

impl<W: Write + Default> Executor<'_, W> {
    /// Create a new [`ExecutorBuilder`]
    pub fn build() -> ExecutorBuilder<W> {
        ExecutorBuilder::default()
    }
}
impl<W: Write> Executor<'_, W> {
    /// Create a new [`ExecutorBuilder`] with a output.
    ///
    /// Output simply must impement [`Write`], so this can be set to stdout.
    /// Or simply set it to [`Vec<u8>`].
    pub fn with_output(w: W) -> ExecutorBuilder<W> {
        ExecutorBuilder {
            displays: Vec::new(),
            output: Some(w),
            instruction_limit: Limit::Unlimited,
            iteration_limit: Limit::limited(1),
        }
    }
}

/// Builder for a [`Executor`].
pub struct ExecutorBuilder<W: Write> {
    output: Option<W>,
    displays: Vec<Image<Vec<u8>, 4>>,
    instruction_limit: Limit,
    iteration_limit: Limit,
}

impl<W: Write> Default for ExecutorBuilder<W> {
    fn default() -> Self {
        Self {
            output: None,
            displays: Vec::new(),
            instruction_limit: Limit::Unlimited,
            iteration_limit: Limit::limited(1),
        }
    }
}

impl<W: Write> ExecutorBuilder<W> {
    /// Limit the number of iterations.
    pub fn limit_iterations(self, n: usize) -> Self {
        Self {
            iteration_limit: Limit::limited(n),
            ..self
        }
    }

    /// Unlimit the number of iterations.
    pub fn unlimit_iterations(self) -> Self {
        Self {
            iteration_limit: Limit::Unlimited,
            ..self
        }
    }

    /// Unlimit the number of instructions.
    /// Make sure to limit the number of the iterations, else it will possibly run forever.
    pub fn unlimit_instructions(self) -> Self {
        Self {
            iteration_limit: Limit::Unlimited,
            ..self
        }
    }

    /// Limit the number of processed instructions.
    ///
    /// Use this if you want it to *definetly finish*.
    pub fn limit_instructions(self, n: usize) -> Self {
        Self {
            instruction_limit: Limit::limited(n),
            ..self
        }
    }

    /// Add a small (`80x80`) logic display.
    pub fn display(self) -> Self {
        let mut d = self.displays;
        d.push(Image::alloc(80, 80));
        Self {
            displays: d,
            ..self
        }
    }

    /// Add a large (`176x176`) logic display.
    pub fn large_display(self) -> Self {
        let mut d = self.displays;
        d.push(Image::alloc(176, 176));
        Self {
            displays: d,
            ..self
        }
    }

    /// Build the [`Executor`] with this code.
    ///
    /// # Errors
    ///
    /// errors if the code is malformed.
    pub fn program(self, program: &str) -> Result<Executor<'_, W>, ParserError<'_>> {
        let Self {
            output,
            displays,
            instruction_limit,
            iteration_limit,
        } = self;
        let mut executor = ExecutorBuilderInternal::new(output, displays);
        executor
            .inslimit(instruction_limit)
            .itrlimit(iteration_limit);
        // #[cfg(debug_assertions)]
        // lexer::print_stream(lexer::lex(program));
        parser::parse(lexer::lex(program), &mut executor)?;
        Ok(executor.finish())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test {
        (run $fn:ident.mlog $($times:literal times)?;
        $(output = $to_be:literal $(;)?)?
        $(cell[$cell_n:literal][$cell_index:literal] = $what:literal $(;)?)?
        ) => {
            #[test]
            fn $fn() -> Result<(), ParserError<'static>> {
                let v = vec![];
                let mut lex = Executor::with_output(v)
                    .unlimit_instructions()
                    $(.limit_iterations($times))?
                    .program(include_str!(concat!(stringify!($fn), ".mlog")))?;
                lex.run();
                let output = lex.output();
                $(assert_eq!(output.output.unwrap(), $to_be);)?
                $(assert_eq!(output.cells[$cell_n][$cell_index], $what);)?
                Ok(())
            }
        };
    }

    test!(run fib.mlog; output = b"12586269025");
    test!(run celliterate.mlog 500 times; cell[0][0] = 500.0);
}
