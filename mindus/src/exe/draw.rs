use mindus::Renderable;
use mindus::Schematic;
use std::env::Args;

use crate::print_err;

pub fn main(args: Args) {
    // process schematics from command line
    for curr in args {
        match Schematic::deserialize_base64(&curr) {
            Ok(s) => {
                s.render().save("x.png");
            }
            // continue processing literals & maybe interactive mode
            Err(e) => {
                print_err!(e, "Could not read schematic");
            }
        }
    }
}
