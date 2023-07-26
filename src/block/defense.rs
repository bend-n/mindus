//! defense
use crate::block::simple::{cost, make_simple};
use crate::block::*;
make_simple!(DefenseBlock);
make_simple!(HeatedBlock => |_, _, _, buff: &mut DataRead| read_heated(buff));
make_register! {
    "mender" => HeatedBlock::new(1, true, cost!(Copper: 25, Lead: 30));
    "mend-projector" => HeatedBlock::new(2, true, cost!(Copper: 50, Lead: 100, Titanium: 25, Silicon: 40));
    "overdrive-projector" => HeatedBlock::new(2, true, cost!(Lead: 100, Titanium: 75, Silicon: 75, Plastanium: 30));
    "overdrive-dome" => HeatedBlock::new(3, true, cost!(Lead: 200, Titanium: 130, Silicon: 130, Plastanium: 80, SurgeAlloy: 120));
    "force-projector" => DefenseBlock::new(3, true, cost!(Lead: 100, Titanium: 75, Silicon: 125));
    "regen-projector" => DefenseBlock::new(3, true, cost!(Silicon: 80, Tungsten: 60, Oxide: 40, Beryllium: 80));
    "shock-mine" => DefenseBlock::new(1, true, cost!(Lead: 25, Silicon: 12));
    "radar" => DefenseBlock::new(1, true, cost!(Silicon: 60, Graphite: 50, Beryllium: 10));
    "build-tower" => DefenseBlock::new(3, true, cost!(Silicon: 150, Oxide: 40, Thorium: 60));
    // barrier projector
    // editor only
    "shockwave-tower" => DefenseBlock::new(3, true, cost!(SurgeAlloy: 50, Silicon: 150, Oxide: 30, Tungsten: 100));
    "shield-projector" => DefenseBlock::new(3, true, &[]);
    "large-shield-projector" => DefenseBlock::new(4, true, &[]);
}

/// format:
/// - heat: [`f32`]
/// - phase heat: [`f32`]
fn read_heated(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(8)
}