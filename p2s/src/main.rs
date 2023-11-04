use exoquant::{ditherer, Color, Remapper, SimpleColorSpace};
use fimg::{scale::Nearest, DynImage, Image};
use mindus::{
    block::{Rotation, SORTER},
    data::dynamic::DynData,
    item::Type::*,
    Schematic,
};
use std::process::ExitCode;
macro_rules! fail {
    () => {
        fail!("<in> (<width>x<height>)")
    };
    ($usage:literal) => {{
        eprintln!(concat!("usage: p2s ", comat::comat!($usage)));
        return ExitCode::FAILURE;
    }};
}
const ITEMS: [mindus::item::Type; 22] = [
    Copper,
    Lead,
    Metaglass,
    Graphite,
    Sand,
    Coal,
    Titanium,
    Thorium,
    Scrap,
    Silicon,
    Plastanium,
    PhaseFabric,
    SurgeAlloy,
    SporePod,
    BlastCompound,
    Pyratite,
    Beryllium,
    Tungsten,
    Oxide,
    Carbide,
    FissileMatter,
    DormantCyst,
];

fn main() -> ExitCode {
    let mut args = std::env::args().skip(1);
    let Some(input) = args.next() else {
        fail!("{bold_red}<input_file>{reset}");
    };
    let mut img = DynImage::open(input).to_rgb();
    let palette = ITEMS
        .map(|i| i.color())
        .map(|(r, g, b)| Color::new(r, g, b, 255));

    if let Some(size) = args.next() {
        let Some((w, h)) = size.split_once('x') else {
            fail!(".. <w>{bold_red}x{reset}<h>")
        };
        let Ok(w) = w.parse() else {
            fail!(".. <width: valid number>x<h>")
        };
        let Ok(h) = h.parse() else {
            fail!(".. <w>x<h: valid number>")
        };
        img = img.scale::<Nearest>(w, h);
    }

    let quant = Remapper::new(&palette, &SimpleColorSpace::default(), &ditherer::None).remap(
        &img.chunked()
            .map(|&[r, g, b]| Color::new(r, g, b, 255))
            .collect::<Vec<_>>(),
        img.width() as usize,
    );
    Image::<Box<[u8]>, 3>::build(img.width(), img.height())
        .buf(
            quant
                .iter()
                .map(|&i| ITEMS[i as usize].color())
                .flat_map(|(r, g, b)| [r, g, b])
                .collect(),
        )
        .scale::<Nearest>(img.width() * 4, img.height() * 4)
        .save("quant.png");
    let mut s = Schematic::new(img.width() as usize, img.height() as usize);
    for x in 0..img.width() as usize {
        for y in 0..img.height() as usize {
            s.set(
                x as usize,
                y as usize,
                &SORTER,
                DynData::Content(
                    mindus::content::Type::Item,
                    quant[(img.height() as usize - y - 1) * img.width() as usize + x] as u16,
                ),
                Rotation::Up,
            )
            .unwrap();
        }
    }
    clipp::copy(s.serialize_base64().unwrap());
    ExitCode::SUCCESS
}
