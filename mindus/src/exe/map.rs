use mindus::data::DataRead;
use mindus::{Map, Renderable, Serializable};
use std::env::Args;

use super::print_err;

pub fn main(args: Args) {
    // process schematics from command line
    for curr in args {
        let Ok(s) = std::fs::read(curr) else {
            continue;
        };
        match (|| {
            let m = Map::deserialize(&mut DataRead::new(&s))?;
            println!(
                "rendering {}",
                m.tags.get("name").map_or("<unknown>", |v| v)
            );
            Ok::<_, mindus::data::map::ReadError>(m.render())
        })() {
            Err(e) => print_err!(e, "fail"),
            Ok(m) => {
                if let Ok(v) = std::env::var("SAVE")
                    && v == "1"
                {
                    m.save("x.png");
                    continue;
                }
            }
        }
    }
}
