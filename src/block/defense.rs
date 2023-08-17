//! defense
use crate::block::simple::{cost, make_simple, BasicBlock};
use crate::block::*;
make_simple!(HeatedBlock => |_, _, buff: &mut DataRead| read_heated(buff));
make_simple!(RadarBlock => |_, _, buff: &mut DataRead| buff.skip(4));
make_simple!(ShieldBlock => |_, _, buff: &mut DataRead| read_shield(buff));
make_register! {
    "mender" -> HeatedBlock::new(1, true, cost!(Copper: 25, Lead: 30));
    "mend-projector" -> HeatedBlock::new(2, true, cost!(Copper: 50, Lead: 100, Titanium: 25, Silicon: 40));
    "overdrive-projector" -> HeatedBlock::new(2, true, cost!(Lead: 100, Titanium: 75, Silicon: 75, Plastanium: 30));
    "overdrive-dome" -> HeatedBlock::new(3, true, cost!(Lead: 200, Titanium: 130, Silicon: 130, Plastanium: 80, SurgeAlloy: 120));
    "force-projector" -> BasicBlock::new(3, true, cost!(Lead: 100, Titanium: 75, Silicon: 125));
    "regen-projector" -> BasicBlock::new(3, true, cost!(Silicon: 80, Tungsten: 60, Oxide: 40, Beryllium: 80));
    "shock-mine" -> BasicBlock::new(1, true, cost!(Lead: 25, Silicon: 12));
    "radar" -> RadarBlock::new(1, true, cost!(Silicon: 60, Graphite: 50, Beryllium: 10));
    "build-tower" -> BasicBlock::new(3, true, cost!(Silicon: 150, Oxide: 40, Thorium: 60));
    "shockwave-tower" -> BasicBlock::new(3, true, cost!(SurgeAlloy: 50, Silicon: 150, Oxide: 30, Tungsten: 100));
    // barrier projector
    // editor only
    "barrier-projector" -> BasicBlock::new(3, true, &[]);
    "shield-projector" -> ShieldBlock::new(3, true, &[]);
    "large-shield-projector" -> ShieldBlock::new(4, true, &[]);
}

/// format:
/// - heat: [`f32`]
/// - phase heat: [`f32`]
fn read_heated(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(8)
}

/// format:
/// - smoothing: [`f32`]
/// - broken: [`bool`]
fn read_shield(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(5)
}
