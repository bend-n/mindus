#![feature(let_chains)]
use image::codecs::png::PngDecoder;
use image::DynamicImage;
use std::fs::File;
use std::io::{BufReader, Write as _};
use std::iter::Iterator;
use std::path::Path;
use walkdir::WalkDir;

macro_rules! wr {
    ($dst:expr => $($arg:tt)*) => { writeln!($dst, $($arg)*).unwrap() };
}

fn kebab2bigsnek(kebab: &str) -> String {
    let mut n = String::new();
    n.reserve(kebab.len());
    for c in kebab.chars() {
        if c == '-' {
            n.push('_');
        } else {
            n.push(c.to_ascii_uppercase());
        }
    }
    n
}

fn main() {
    let _ = std::fs::remove_dir_all("target/out");
    let walkdir = WalkDir::new("assets");
    println!("cargo:rerun-if-changed=assets/");
    println!("cargo:rerun-if-changed=build.rs");
    let o = std::env::var("OUT_DIR").unwrap();
    let o = Path::new(&o);
    let mut full = File::create(o.join("full.rs")).unwrap();
    // let mut half = File::create(o.join("half.rs")).unwrap();
    let mut quar = File::create(o.join("quar.rs")).unwrap();
    let mut eigh = File::create(o.join("eigh.rs")).unwrap();
    let mut n = 22usize;

    wr!(full => "pub mod full {{");
    wr!(full => "pub static EMPTY4: Image<&[u8], 4> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked(32) }}, unsafe {{ std::num::NonZeroU32::new_unchecked(32) }}, &[0; 32 * 32 * 4]);");
    wr!(full => "pub static EMPTY: Image<&[u8], 3> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked(32) }}, unsafe {{ std::num::NonZeroU32::new_unchecked(32) }}, &[0; 32 * 32 * 3]);");

    wr!(quar => "pub mod quar {{");
    // forced to do this because try_into isnt const
    wr!(quar => "pub static EMPTY4: Image<&[u8], 4> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked(8) }}, unsafe {{ std::num::NonZeroU32::new_unchecked(8) }}, &[0; 8 * 8 * 4]);");
    wr!(quar => "pub static EMPTY: Image<&[u8], 3> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked(8) }}, unsafe {{ std::num::NonZeroU32::new_unchecked(8) }}, &[0; 8 * 8 * 3]);");

    wr!(eigh => "pub mod eigh {{");
    wr!(eigh => "pub static EMPTY4: Image<&[u8], 4> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked(4) }}, unsafe {{ std::num::NonZeroU32::new_unchecked(4) }}, &[0; 4 * 4 * 4]);");
    wr!(eigh => "pub static EMPTY: Image<&[u8], 3> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked(4) }}, unsafe {{ std::num::NonZeroU32::new_unchecked(4) }}, &[0; 4 * 4 * 3]);");

    for mut file in [&full, &quar, &eigh] {
        wr!(file => "use crate::utils::Image;");
        wr!(file => "pub static CLIFF: Image<&[u8], 4> = EMPTY4.copy();");
        for i in 1..=16 {
            wr!(file => "pub static BUILD{}: Image<&[u8], 4> = EMPTY4.copy();", i);
        }
    }
    for e in walkdir.into_iter().filter_map(Result::ok) {
        let path = e.path();
        if path.is_file() && let Some(e) = path.extension() && e == "png" {
            let p = DynamicImage::from_decoder(PngDecoder::new(BufReader::new(File::open(path).unwrap())).unwrap()).unwrap();
            if path.file_name().unwrap().to_str().unwrap().contains("-liquid.png") {
                continue
            }
            let f = path.file_name().unwrap().to_str().unwrap();
            if f.contains("bottom") || f.contains("-team") || f.contains("-end") || f.contains("stack") {
                continue;
            }
            let rgb = path.components().any(|c| c.as_os_str() == "floors");
            let path = kebab2bigsnek(path.with_extension("").file_name().unwrap().to_str().unwrap());
            if matches!(path.as_str(), "CLIFF_CRUSHER_ROTATOR" | "NEOPLASIA_REACTOR_CENTER" | "FLUX_REACTOR_MID" | "EDGE" | "PHASE_CONVEYOR_BRIDGE" | "BRIDGE_ARROW" | "DUCT_BRIDGE_BRIDGE" | "DUCT_BRIDGE_ARROW" | "LAUNCHPOD" | "BRIDGE_CONVEYOR_BRIDGE" | "BRIDGE_CONVEYOR_ARROW" | "PHASE_CONVEYOR_ARROW" | "REINFORCED_BRIDGE_CONDUIT_ARROW" | "REINFORCED_BRIDGE_CONDUIT_BRIDGE" | "PHASE_CONDUIT_BRIDGE" | "BRIDGE_CONDUIT_ARROW" | "PHASE_CONDUIT_ARROW" | "BRIDGE_CONDUIT_BRIDGE" | "PLATED_CONDUIT_CAP") {
                continue
            }
            macro_rules! writ {
                ($ext:ident / $scale:literal) => {
                    let mut buf = File::create(o.join(n.to_string() + "-" + stringify!($ext))).unwrap();
                    // boulders
                    let (mx, my) = if p.width() + p.height() == 48+48 {
                        (32, 32)
                    // vents (dont match VENT_CONDENSER, do match (RHYOLITE_VENT)
                    } else if path.contains("_VENT")
                        // talls
                        || matches!(path.as_str(), "YELLOWCORAL" | "WHITE_TREE" | "WHITE_TREE_DEAD" | "REDWEED" | "SPORE_CLUSTER" | "CRYSTAL_BLOCKS" | "CRYSTAL_CLUSTER" | "VIBRANT_CRYSTAL_CLUSTER" | "CRYSTAL_ORBS") {
                        (32, 32)
                    } else {
                        (p.height(), p.height())
                    };
                    let new = if $scale == 1 {
                        p.clone()
                    } else {
                        DynamicImage::ImageRgba8(image::imageops::resize(
                            &p,
                            mx / $scale,
                            my / $scale,
                            image::imageops::Nearest,
                        ))
                    };
                    let x = new.width();
                    let y = new.height();
                    if rgb {
                        buf.write_all(&new.into_rgb8().into_raw()).unwrap();
                        wr!($ext =>
                            r#"pub(crate) static {path}: Image<&[u8], 3> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked( {x} ) }}, unsafe {{ std::num::NonZeroU32::new_unchecked( {y} ) }}, include_bytes!(concat!(env!("OUT_DIR"), "/{n}-{}")));"#,
                            stringify!($ext)
                        );
                    } else {
                        buf.write_all(&new.into_rgba8().into_raw()).unwrap();
                        wr!($ext =>
                            r#"pub(crate) static {path}: Image<&[u8], 4> = Image::new(unsafe {{ std::num::NonZeroU32::new_unchecked( {x} ) }}, unsafe {{ std::num::NonZeroU32::new_unchecked( {y} ) }}, include_bytes!(concat!(env!("OUT_DIR"), "/{n}-{}")));"#,
                            stringify!($ext)
                        );
                    }
                };
            }
            writ!(full / 1);
            // writ!(half + 0.5);
            writ!(quar / 4);
            writ!(eigh / 8);
            n += 1;
        }
    }
    for mut f in [full, eigh, quar] {
        f.write_all(b"}").unwrap();
    }
}
