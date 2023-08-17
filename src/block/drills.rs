//! extraction of raw resources (mine part)
use super::production::ProductionBlock;
use crate::block::simple::{cost, make_simple, BasicBlock};
use crate::block::*;

make_simple!(
    DrillBlock,
    |_, name, _, _, rot: Rotation, s| {
        let mut base =
            load!(from name which is ["large-plasma-bore" | "plasma-bore" | "cliff-crusher"], s);
        unsafe {
            base.overlay(load!(concat "top" => name which is ["large-plasma-bore" | "plasma-bore" | "cliff-crusher"], s).rotate(rot.rotated(false).count()) )
        };
        base
    },
    |_, _, buff: &mut DataRead| read_drill(buff)
);
make_simple!(WallDrillBlock, |_, _, _, _, rot: Rotation, scl| {
    let mut base = load!("cliff-crusher", scl);
    unsafe { base.overlay(load!("cliff-crusher-top", scl).rotate(rot.rotated(false).count())) };
    base
});

make_register! {
    "mechanical-drill" -> DrillBlock::new(2, true, cost!(Copper: 12));
    "pneumatic-drill" -> DrillBlock::new(2, true, cost!(Copper: 18, Graphite: 10));
    "laser-drill" -> DrillBlock::new(3, true, cost!(Copper: 35, Graphite: 30, Titanium: 20, Silicon: 30));
    "blast-drill" -> DrillBlock::new(4, true, cost!(Copper: 65, Titanium: 50, Thorium: 75, Silicon: 60));
    "water-extractor" -> BasicBlock::new(2, true, cost!(Copper: 30, Lead: 30, Metaglass: 30, Graphite: 30));
    "oil-extractor" -> BasicBlock::new(3, true, cost!(Copper: 150, Lead: 115, Graphite: 175, Thorium: 115, Silicon: 75));
    "vent-condenser" -> ProductionBlock::new(3, true, cost!(Graphite: 20, Beryllium: 60));
    "cliff-crusher" -> WallDrillBlock::new(2, false, cost!(Beryllium: 100, Graphite: 40));
    "plasma-bore" => DrillBlock::new(2, false, cost!(Beryllium: 40));
    "large-plasma-bore" => DrillBlock::new(3, false, cost!(Silicon: 100, Oxide: 25, Beryllium: 100, Tungsten: 70));
    "impact-drill" -> DrillBlock::new(4, true, cost!(Silicon: 70, Beryllium: 90, Graphite: 60));
    "eruption-drill" -> DrillBlock::new(5, true, cost!(Silicon: 200, Oxide: 20, Tungsten: 200, Thorium: 120));
}

/// format:
/// - progress: [`f32`]
/// - warmup: [`f32`]
fn read_drill(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(8)
}
