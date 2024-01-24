use mindus::data::map::MapReader;
use mindus::data::DataRead;
use std::env::Args;

use super::print_err;
pub fn main(args: Args) {
    // process schematics from command line
    for curr in args {
        let Ok(s) = std::fs::read(curr) else {
            continue;
        };
        match (|| {
            let mut m = MapReader::new(&mut DataRead::new(&s))?;
            m.header()?;
            m.version()?;
            let t = m.tags()?;
            println!("rendering {}", t["name"]);
            m.skip()?;
            let (mut img, sz) = mindus::data::renderer::draw_map_single(&mut m)?;
            mindus::data::renderer::draw_units(&mut m, img.as_mut(), sz)?;
            Ok::<_, mindus::data::map::ReadError>(img)
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
