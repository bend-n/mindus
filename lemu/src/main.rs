use std::io::{self, Stdout};

use lemu::LogicExecutor;

fn main() {
    let mut args = std::env::args();
    args.next().unwrap(); // path to executable
    for file in args {
        let f = std::fs::read_to_string(file).unwrap();
        let mut lex: LogicExecutor<Stdout> = LogicExecutor::with_output(io::stdout())
            .program(&f)
            .unwrap();
        lex.run();
    }
}
