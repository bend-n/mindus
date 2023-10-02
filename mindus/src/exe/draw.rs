use mindus::data::DataRead;
use mindus::{Renderable, Schematic, Serializable};
use std::env::Args;

use crate::print_err;

pub fn main(args: Args) {
    // process schematics from command line
    for curr in args {
        match if curr.ends_with(".msch") {
            let Ok(v) = std::fs::read(curr) else {
                panic!("no file found");
            };
            Schematic::deserialize(&mut DataRead::new(&v))
        } else {
            match Schematic::deserialize_base64(&curr) {
                Err(mindus::data::schematic::R64Error::Base64(e)) => {
                    print_err!(e, "Could not read schematic");
                    continue;
                }
                Err(mindus::data::schematic::R64Error::Content(e)) => Err(e),
                Ok(o) => Ok(o),
            }
        } {
            Ok(s) => {
                let i = s.render();
                if let Ok(v) = std::env::var("SAVE") && v == "1" {
                    i.save("x.png");
                    continue;
                }
            }
            // continue processing literals & maybe interactive mode
            Err(e) => {
                print_err!(e, "Could not read schematic");
            }
        }
    }
}
