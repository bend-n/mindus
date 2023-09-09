use mindus::LogicExecutor;
use std::env::Args;
pub fn main(args: Args) {
    for file in args {
        let f = std::fs::read_to_string(file).unwrap();
        let mut lex = LogicExecutor::build().program(&f).unwrap();
        lex.run();
        print!("{}", lex.output());
    }
}
