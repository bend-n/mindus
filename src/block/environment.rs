//! grass
use crate::block::make_register;
use crate::block::simple::make_simple;
use crate::data::renderer::*;
use tinyrand::{Rand, RandRange, Seeded, StdRand};
use tinyrand_std::clock_seed::ClockSeed;

macro_rules! register_env {
    ($($field:literal: $size:literal @ $variations:literal;)+) => {
        make_register!(
            $($field => EnvironmentBlock::new($size, true, &[]);)*
        );

        make_simple!(EnvironmentBlock, |_, _, name, _, _| {
            let mut rand = StdRand::seed(ClockSeed::default().next_u64());
            match name {
                $($field => {
                    #[allow(clippy::reversed_empty_ranges)]
                    if $variations == 1 { Some(ImageHolder::Borrow(load("environment", $field).unwrap())) }
                    else if $variations == 0 { return None }
                    else { Some(ImageHolder::Borrow(load("environment", &format!("{}{}", $field, rand.next_range(1usize..$variations))).unwrap())) }
                   },)*
                _ => { unreachable!() }
            }
          });
    };
}

register_env! {
    "build1": 1@0;
    "build2": 1@0;
    "build3": 1@0;
    "arkycite-floor": 1@1;
    "arkyic-stone": 1@3;
    "arkyic-vent": 3@2;
    "arkyic-wall-large": 2@1;
    "arkyic-wall": 1@3;
    "basalt": 1@3;
    "beryllic-stone-wall-large": 2@1;
    "beryllic-stone-wall": 1@2;
    "beryllic-stone": 1@4;
    "bluemat": 1@3;
    "carbon-stone": 1@4;
    "carbon-vent": 3@2;
    "carbon-wall-large": 2@1;
    "carbon-wall": 1@2;
    "char": 1@3;
    "cliff": 1@7;
    "core-zone": 1@1;
    "crater-stone": 1@6;
    "crystal-floor": 1@4;
    "crystalline-stone-wall-large": 2@1;
    "crystalline-stone-wall": 1@4;
    "crystalline-stone": 1@5;
    "crystalline-vent": 3@2;
    "dacite-wall-large": 2@1;
    "dacite-wall": 1@2;
    "dacite": 1@3;
    "dark-metal-large": 2@1;
    "dark-metal": 1@2;
    "dark-panel-1": 1@1;
    "dark-panel-2": 1@1;
    "dark-panel-3": 1@1;
    "dark-panel-4": 1@1;
    "dark-panel-5": 1@1;
    "dark-panel-6": 1@1;
    "metal-floor": 1@1;
    "metal-floor-2": 1@1;
    "metal-floor-3": 1@1;
    "metal-floor-4": 1@1;
    "metal-floor-5": 1@1;
    "metal-floor-damaged": 1@3;
    "darksand-tainted-water": 1@1;
    "darksand-water": 1@1;
    "darksand": 1@3;
    "deep-tainted-water": 1@1;
    "deep-water": 1@1;
    "dense-red-stone": 1@4;
    "dirt-wall-large": 2@1;
    "dirt-wall": 1@2;
    "dirt": 1@3;
    "dune-wall-large": 2@1;
    "dune-wall": 1@2;
    "ferric-craters": 1@3; // ferris section
    "ferric-stone-wall-large": 2@1;
    "ferric-stone-wall": 1@2;
    "ferric-stone": 1@4;
    "graphite-wall-large": 2@1;
    "graphite-wall": 1@3;
    "grass": 1@3;
    "hotrock": 1@3;
    "ice-snow": 1@3;
    "ice-wall-large": 2@1;
    "ice-wall": 1@2;
    "ice": 1@3;
    "magmarock": 1@3;
    "molten-slag": 1@1;
    "moss": 1@3;
    "mud": 1@3;
    "ore-beryllium": 1@3;
    "ore-coal": 1@3;
    "ore-copper": 1@3;
    "ore-crystal-thorium": 1@3;
    "ore-lead": 1@3;
    "ore-scrap": 1@3;
    "ore-thorium": 1@3;
    "ore-titanium": 1@3;
    "ore-tungsten": 1@3;
    "ore-wall-beryllium": 1@3;
    "ore-wall-thorium": 1@3;
    "ore-wall-tungsten": 1@3;
    "pebbles": 1@3;
    "pine": 1@1;
    "pooled-cryofluid": 1@1;
    "red-diamond-wall": 1@3;
    "red-ice-wall-large": 2@1;
    "red-ice": 1@3;
    "red-stone-vent": 3@2;
    "red-stone-wall-large": 2@1;
    "red-stone-wall": 1@3;
    "red-stone": 1@4;
    "redmat": 1@3;
    "regolith-wall-large": 2@1;
    "regolith": 1@3;
    "rhyolite-crater": 1@3;
    "rhyolite-vent": 3@2;
    "rhyolite-wall-large": 2@1;
    "rhyolite-wall": 1@2;
    "rhyolite": 1@3;
    "rough-rhyolite": 1@3;
    "salt-wall-large": 2@1;
    "salt-wall": 1@2;
    "salt": 1@1;
    "sand-floor": 1@3;
    "sand-wall-large": 2@1;
    "sand-wall": 1@2;
    "sand-water": 1@1;
    "shale-wall-large": 2@1;
    "shale-wall": 1@2;
    "shale": 1@3;
    "shallow-water": 1@1;
    "shrubs-large": 2@1;
    "shrubs": 1@2;
    "snow-pine": 1@1;
    "snow-wall-large": 2@1;
    "snow-wall": 1@2;
    "snow": 1@3;
    "space": 1@1;
    "spawn": 1@1;
    "spore-moss": 1@3;
    "spore-pine": 1@1;
    "spore-wall-large": 2@1;
    "spore-wall": 1@2;
    "stone-wall-large": 2@1;
    "stone-wall": 1@2;
    "stone": 1@3;
    "tainted-water": 1@1;
    "tar": 1@1;
    "yellow-stone-plates": 1@3;
    "yellow-stone-vent": 3@2;
    "yellow-stone-wall-large": 2@1;
    "yellow-stone-wall": 1@2;
    "yellow-stone": 1@3;
    // props
    "snow-boulder": 1@2;
    "shale-boulder": 1@2;
    "arkyic-boulder": 1@3;
    "basalt-boulder": 1@2;
    "beryllic-boulder": 1@2;
    "boulder": 1@2;
    "carbon-boulder": 1@2;
    "crystalline-boulder": 1@2;
    "dacite-boulder": 1@2;
    "ferric-boulder": 1@2;
    "red-ice-boulder": 1@3;
    "red-stone-boulder": 1@4;
    "rhyolite-boulder": 1@3;
    "sand-boulder": 1@2;
    "yellow-sand-boulder": 1@2;
    // these are tall but uh
    "crystal-blocks": 1@3;
    "crytal-cluster": 1@3;
    "crystal-orbs": 1@3;
}
