mod draw;
mod map;

macro_rules! print_err {
	($err:expr, $($msg:tt)*) => {{
		use std::error::Error;
		let err = $err;
		eprint!($($msg)*);
		eprintln!(": {err}");
		let mut err_ref = &err as &dyn Error;
		loop {
			let Some(next) = err_ref.source() else {
				break;
			};
			eprintln!("\tSource: {next}");
			err_ref = next;
			}
		}
	};
}
pub(crate) use print_err;

fn main() {
    let mut args = std::env::args();
    args.next().unwrap(); // path to executable
    match args.next() {
        None => eprintln!("Not enough arguments, valid commands are: draw, map"),
        Some(s) if s == "draw" => draw::main(args),
        Some(s) if s == "map" => map::main(args),
        Some(s) => eprintln!("Unknown argument {s}, valid commands are: draw, map"),
    }
}
