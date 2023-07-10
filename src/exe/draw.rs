use mindus::build_registry;
use mindus::Renderable;
use mindus::SchematicSerializer;
use std::env::Args;

use crate::print_err;

pub fn main(args: Args) {
    let reg = build_registry();
    let mut ss = SchematicSerializer(&reg);
    let mut first = true;
    let mut need_space = false;

    // process schematics from command line
    for curr in args {
        match ss.deserialize_base64(&curr) {
            Ok(s) => {
                if !first || need_space {
                    println!();
                }
                s.render().save("x.png").unwrap();
            }
            // continue processing literals & maybe interactive mode
            Err(e) => {
                if need_space {
                    println!();
                }
                first = false;
                need_space = false;
                print_err!(e, "Could not read schematic");
            }
        }
    }
}
