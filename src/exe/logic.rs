use mindus::LogicExecutor;
use std::env::Args;
pub fn main(args: Args) {
    for file in args {
        let f = std::fs::read_to_string(file).unwrap();
        let mut lex = LogicExecutor::try_from(f.as_str()).unwrap();
        lex.run(
            mindus::logic::Limit::Unlimited,
            mindus::logic::Limit::limited(1),
        );
        print!("{}", lex.output());
    }
}
