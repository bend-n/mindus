use mindus::data::DataRead;
use mindus::Renderable;
use mindus::{Map, Serializable};
use std::env::Args;
use std::time::Instant;

use super::print_err;
pub fn main(args: Args) {
    let runs = std::env::var("RUNS").map_or(10u8, |x| x.parse().unwrap_or(10u8));

    // process schematics from command line
    println!("starting timing");
    let then = Instant::now();
    for curr in args {
        let Ok(s) = std::fs::read(curr) else {
            continue;
        };
        let starting_deser = Instant::now();
        match Map::deserialize(&mut DataRead::new(&s)) {
            Err(e) => print_err!(e, "fail"),
            Ok(m) => {
                let deser_took = starting_deser.elapsed();
                if let Ok(v) = std::env::var("SAVE")
                    && v == "1"
                {
                    m.render().save("x.png");
                    continue;
                }
                let starting_render = Instant::now();
                for _ in 0..runs {
                    drop(m.render());
                }
                let renders_took = starting_render.elapsed();
                let took = then.elapsed();
                println!(
                    "μ total: {:.2}s ({} runs) (deser: {}ms, render: {:.2}s) on map {}",
                    took.as_secs_f32() / runs as f32,
                    runs,
                    deser_took.as_millis(),
                    renders_took.as_secs_f32() / runs as f32,
                    m.tags.get("mapname").unwrap(),
                );
            }
        }
    }
}
