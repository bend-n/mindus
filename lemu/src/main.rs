use lemu::LogicExecutor;

fn main() {
    let mut args = std::env::args();
    args.next().unwrap(); // path to executable
    for file in args {
        let f = std::fs::read_to_string(file).unwrap();
        let mut lex = LogicExecutor::build().program(&f).unwrap();
        lex.run();
        print!("{}", lex.output());
    }
}
