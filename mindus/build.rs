#![feature(let_chains)]
use fimg::DynImage;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write as _;
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

    let mut to_colors = File::create(o.join("2.rs")).unwrap();
    let mut from_colors = File::create(o.join("4.rs")).unwrap();
    let mut n = 22usize;

    wr!(full => "pub mod full {{");
    wr!(full => "pub static EMPTY4: Image<&[u8], 4> = Image::make::<32, 32>();");
    wr!(full => "pub static EMPTY: Image<&[u8], 3> = Image::make::<32, 32>();");

    wr!(quar => "pub mod quar {{");
    // forced to do this because try_into isnt const
    wr!(quar => "pub static EMPTY4: Image<&[u8], 4> = Image::make::<8, 8>();");
    wr!(quar => "pub static EMPTY: Image<&[u8], 3> = Image::make::<8, 8>();");

    wr!(eigh => "pub mod eigh {{");
    wr!(eigh => "pub static EMPTY4: Image<&[u8], 4> = Image::make::<4, 4>();");
    wr!(eigh => "pub static EMPTY: Image<&[u8], 3> = Image::make::<4, 4>();");

    wr!(to_colors => "phf::phf_map! {{");
    wr!(from_colors => "phf::phf_map! {{");

    let mut mapped = HashMap::with_capacity(68);

    for mut file in [&full, &quar, &eigh] {
        wr!(file => "use crate::utils::Image;");
        wr!(file => "pub static CLIFF: Image<&[u8], 4> = EMPTY4.copy();");
        for i in 1..=16 {
            wr!(file => "pub static BUILD{}: Image<&[u8], 4> = EMPTY4.copy();", i);
        }
    }
    for e in walkdir.into_iter().filter_map(Result::ok) {
        let path = e.path();
        if path.is_file()
            && let Some(e) = path.extension()
            && e == "png"
        {
            let mut p = DynImage::open(path);
            if path
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .contains("-liquid.png")
            {
                continue;
            }
            let f = path.file_name().unwrap().to_str().unwrap();
            if f.contains("bottom")
                || f.contains("-team")
                || f.contains("-end")
                || f.contains("stack")
            {
                continue;
            }
            let rgb = path.components().any(|c| c.as_os_str() == "floors");
            let env = path.components().any(|c| c.as_os_str() == "environment");
            let path = kebab2bigsnek(
                path.with_extension("")
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap(),
            );
            if matches!(
                path.as_str(),
                "CLIFF_CRUSHER_ROTATOR"
                    | "NEOPLASIA_REACTOR_CENTER"
                    | "FLUX_REACTOR_MID"
                    | "EDGE"
                    | "PHASE_CONVEYOR_BRIDGE"
                    | "BRIDGE_ARROW"
                    | "DUCT_BRIDGE_BRIDGE"
                    | "DUCT_BRIDGE_ARROW"
                    | "LAUNCHPOD"
                    | "BRIDGE_CONVEYOR_BRIDGE"
                    | "BRIDGE_CONVEYOR_ARROW"
                    | "PHASE_CONVEYOR_ARROW"
                    | "REINFORCED_BRIDGE_CONDUIT_ARROW"
                    | "REINFORCED_BRIDGE_CONDUIT_BRIDGE"
                    | "PHASE_CONDUIT_BRIDGE"
                    | "BRIDGE_CONDUIT_ARROW"
                    | "PHASE_CONDUIT_ARROW"
                    | "BRIDGE_CONDUIT_BRIDGE"
                    | "PLATED_CONDUIT_CAP"
            ) {
                continue;
            }
            println!("do {path}");

            if rgb {
                let [r, g, b] = match &*path {
                    "HOTROCK" => [221u8, 123, 82],
                    "YELLOW_STONE" => [255, 210, 128],
                    "REGOLITH" => [146, 90, 70],
                    "CRATER_STONE" => [92, 87, 87],
                    "CHAR" => [86, 79, 79],
                    "METAL_FLOOR_DAMAGED" => [134, 138, 154],
                    "FERRIC_CRATERS" => [69, 69, 69],
                    "RHYOLITE_CRATER" => [100, 60, 53],
                    "ROUGH_RHYOLITE" => [99, 60, 53],
                    "DARK_PANEL_1" => [93, 81, 79],
                    "DARK_PANEL_2" => [91, 81, 85],
                    "DARK_PANEL_3" => [91, 79, 77],
                    "DARK_PANEL_4" => [103, 90, 88],
                    "DENSE_RED_STONE" => [206, 116, 95],
                    "SPORE_MOSS" => [124, 76, 128],
                    "METAL_FLOOR" => [126, 113, 111],
                    "METAL_FLOOR_2" => [117, 106, 104],
                    "MAGMAROCK" => [104, 74, 53],
                    "ICE_SNOW" => [130, 125, 233],
                    _ => unsafe { p.pixel::<3>(p.width() / 2, p.height() / 2) },
                };
                match mapped.entry((r, g, b)) {
                    std::collections::hash_map::Entry::Occupied(with) => panic!(
                        "slot {path} occupied by {}, shared color {r}, {g}, {b}",
                        with.get()
                    ),
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(path.clone());
                    }
                }
                wr!(from_colors => "0x{:x}_u32 => &crate::block::{path},", fimg::Pack::pack(&[r,g,b]));
                wr!(to_colors => r#""{f}" => ({r}, {g}, {b}),"#)
            }

            macro_rules! writ {
                ($ext:ident / $scale:literal) => {
                    let mut buf = File::create(o.join(n.to_string() + "-" + stringify!($ext))).unwrap();
                    let out_path = format!("{}/{n}-{}", o.display(), stringify!($ext));
                    // boulders
                    let (mx, my) = if env && p.width() + p.height() == 48+48 {
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
                        p.scale::<fimg::scale::Nearest>(mx / $scale, my / $scale)
                    };
                    let x = new.width();
                    let y = new.height();
                    if rgb {
                        buf.write_all(&new.to_rgb().bytes()).unwrap();
                        wr!($ext => r#"pub(crate) static {path}: Image<&[u8], 3> = unsafe {{ Image::new(std::num::NonZeroU32::new({x}).unwrap(), std::num::NonZeroU32::new({y}).unwrap(), include_bytes!(r"{out_path}")) }};"#);
                    } else {
                        buf.write_all(&new.to_rgba().bytes()).unwrap();
                        wr!($ext => r#"pub(crate) static {path}: Image<&[u8], 4> = unsafe {{ Image::new(std::num::NonZeroU32::new({x}).unwrap(), std::num::NonZeroU32::new({y}).unwrap(), include_bytes!(r"{out_path}")) }};"#);
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
    for mut f in [full, eigh, quar, to_colors, from_colors] {
        f.write_all(b"}").unwrap();
    }
}
