use std::{
    io::{self, Stdout},
    process::ExitCode,
};

use lemu::{Executor, Output};

fn main() -> ExitCode {
    let mut args = std::env::args();
    args.next().unwrap(); // path to executable
    for file in args {
        let f = std::fs::read_to_string(&file).unwrap();
        let mut lex: Executor<Stdout> = match Executor::with_output(io::stdout())
            .large_display()
            .program(&f)
        {
            Ok(x) => x,
            Err(e) => {
                eprint!("{}", e.diagnose(&f));
                return ExitCode::FAILURE;
            }
        };
        lex.run();
        let Output { displays, .. } = lex.output();
        for (d, i) in displays.iter().zip(1..=displays.len()) {
            d.save(format!("image{i}.png"));
        }
    }
    ExitCode::SUCCESS
}
