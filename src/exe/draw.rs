use mindus::build_registry;
use mindus::Renderable;
use mindus::SchematicSerializer;
use std::env::Args;

use crate::print_err;

pub fn main(args: Args) {
    let reg = build_registry();
    let mut ss = SchematicSerializer(&reg);

    // process schematics from command line
    for curr in args {
        match ss.deserialize_base64(&curr) {
            Ok(s) => {
                s.render().save("x.png").unwrap();
            }
            // continue processing literals & maybe interactive mode
            Err(e) => {
                print_err!(e, "Could not read schematic");
            }
        }
    }
}
