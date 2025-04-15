use mindus::data::DataRead;
use mindus::{Schematic, Serializable};
use std::env::Args;

use crate::print_err;

pub fn main(args: Args) {
    // process schematics from command line
    for curr in args {
        match if curr.ends_with(".msch") {
            let Ok(v) = std::fs::read(&curr) else {
                panic!("no file found");
            };
            Schematic::deserialize(&mut DataRead::new(&v))
        } else {
            match Schematic::deserialize_base64(&curr) {
                Err(e) => {
                    print_err!(e, "Could not read schematic");
                    continue;
                }
                Ok(o) => Ok(o),
            }
        } {
            Ok(s) => {
                println!("{curr} {}", s.tags.get("name").unwrap());
                let i = mindus::Renderable::render(&s);
                if let Ok(v) = std::env::var("SAVE")
                    && v == "1"
                {
                    i.save("x.png");
                    println!("drawn");
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
