use std::io::{self, Stdout};

use lemu::{Executor, Output};

fn main() {
    let mut args = std::env::args();
    args.next().unwrap(); // path to executable
    for file in args {
        let f = std::fs::read_to_string(file).unwrap();
        let mut lex: Executor<Stdout> = Executor::with_output(io::stdout())
            .display()
            .program(&f)
            .unwrap();
        lex.run();
        let Output { displays, .. } = lex.output();
        for (d, i) in displays.iter().zip(1..=displays.len()) {
            d.save(format!("image{i}.png"));
        }
    }
}
