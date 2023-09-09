pub mod executor;
mod field;
pub(crate) mod instructions;
pub(crate) mod lexer;
pub(crate) mod memory;
pub(crate) mod parser;

pub(crate) use field::{LogicField, TryFromU8Error};

pub use executor::{Limit, LogicExecutor};
pub use parser::ParserError;

impl<'s> TryFrom<&'s str> for LogicExecutor<'s> {
    type Error = ParserError<'s>;
    fn try_from(value: &'s str) -> Result<Self, Self::Error> {
        let tokens = lexer::lex(value);
        lexer::print_stream(lexer::lex(value));
        let executor = parser::parse(tokens)?;
        Ok(executor)
    }
}

#[test]
fn execute() -> Result<(), ParserError<'static>> {
    let mut lex = LogicExecutor::try_from(
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
    lex.run(Limit::Unlimited, Limit::Unlimited);
    assert_eq!(lex.inner.output, "12586269025");
    Ok(())
}
