//! the industry part of mindustry
use crate::block::make_register;
use crate::block::simple::{cost, make_simple};

make_register! {
    "cultivator" => ProductionBlock::new(2, true, cost!(Copper: 25, Lead: 25, Silicon: 10));
    "graphite-press" => ProductionBlock::new(2, true, cost!(Copper: 75, Lead: 30));
    "multi-press" => ProductionBlock::new(3, true, cost!(Lead: 100, Graphite: 50, Titanium: 100, Silicon: 25));
    "silicon-smelter" => ProductionBlock::new(2, true, cost!(Copper: 30, Lead: 25));
    "silicon-crucible" => ProductionBlock::new(3, true, cost!(Metaglass: 80, Titanium: 120, Silicon: 60, Plastanium: 35));
    "kiln" => ProductionBlock::new(2, true, cost!(Copper: 60, Lead: 30, Graphite: 30));
    "plastanium-compressor" => ProductionBlock::new(2, true, cost!(Lead: 115, Graphite: 60, Titanium: 80, Silicon: 80));
    "phase-weaver" => ProductionBlock::new(2, true, cost!(Lead: 120, Thorium: 75, Silicon: 130));
    "surge-smelter" => ProductionBlock::new(3, true, cost!(Lead: 80, Thorium: 70, Silicon: 80));
    "cryofluid-mixer" => ProductionBlock::new(2, true, cost!(Lead: 65, Thorium: 60, Silicon: 40));
    "pyratite-mixer" => ProductionBlock::new(2, true, cost!(Copper: 50, Lead: 25));
    "blast-mixer" => ProductionBlock::new(2, true, cost!(Lead: 30, Thorium: 20));
    "melter" => ProductionBlock::new(1, true, cost!(Copper: 30, Lead: 35, Graphite: 45));
    "separator" => ProductionBlock::new(2, true, cost!(Copper: 30, Titanium: 25));
    "disassembler" => ProductionBlock::new(3, true, cost!(Titanium: 100, Thorium: 80, Silicon: 150, Plastanium: 40));
    "spore-press" => ProductionBlock::new(2, true, cost!(Lead: 35, Silicon: 30));
    "pulverizer" => ProductionBlock::new(1, true, cost!(Copper: 30, Lead: 25));
    "coal-centrifuge" => ProductionBlock::new(2, true, cost!(Lead: 30, Graphite: 40, Titanium: 20));
    "incinerator" => ProductionBlock::new(1, true, cost!(Lead: 15, Graphite: 5));
    "silicon-arc-furnace" => ProductionBlock::new(3, true, cost!(Beryllium: 70, Graphite: 80));
    "electrolyzer" => ProductionBlock::new(3, true, cost!(Silicon: 50, Graphite: 40, Beryllium: 130, Tungsten: 80));
    "atmospheric-concentrator" => ProductionBlock::new(3, true, cost!(Oxide: 60, Beryllium: 180, Silicon: 150));
    "oxidation-chamber" => HeatCrafter::new(3, true, cost!(Tungsten: 120, Graphite: 80, Silicon: 100, Beryllium: 120));
    "electric-heater" => HeatCrafter::new(2, false, cost!(Tungsten: 30, Oxide: 30));
    "slag-heater" => HeatCrafter::new(3, false, cost!(Tungsten: 50, Oxide: 20, Beryllium: 20));
    "phase-heater" => ProductionBlock::new(2, false, cost!(Oxide: 30, Carbide: 30, Beryllium: 30));
    "heat-redirector" => ProductionBlock::new(3, false, cost!(Tungsten: 10, Graphite: 10));
    "heat-router" => ProductionBlock::new(3, false, cost!(Tungsten: 15, Graphite: 10));
    "slag-incinerator" => ProductionBlock::new(1, true, cost!(Tungsten: 15));
    "carbide-crucible" => ProductionBlock::new(3, true, cost!(Tungsten: 110, Thorium: 150, Oxide: 60));
    // slag centrifuge
    "surge-crucible" => ProductionBlock::new(3, true, cost!(Silicon: 100, Graphite: 80, Tungsten: 80, Oxide: 80));
    "cyanogen-synthesizer" => ProductionBlock::new(3, true, cost!(Carbide: 50, Silicon: 80, Beryllium: 90));
    "phase-synthesizer" => ProductionBlock::new(3, true, cost!(Carbide: 90, Silicon: 100, Thorium: 100, Tungsten: 200));
    // heat reactor
    // sandbox only
    "heat-source" => ProductionBlock::new(1, false, &[]);
}

make_simple!(
    ProductionBlock,
    |_, _, _, _, _| None,
    |_, _, _, _, _, buff: &mut crate::data::DataRead| {
        // format:
        // - progress: `f32`
        // - warmup: `f32`
        buff.read_f32()?;
        buff.read_f32()?;
        Ok(())
    }
);

make_simple!(
    HeatCrafter,
    |_, _, _, _, _| None,
    |_, _, _, _, _, buff: &mut crate::data::DataRead| {
        // format:
        // - progress: `f32`
        // - warmup: `f32`
        // - heat: f32
        buff.read_f32()?;
        buff.read_f32()?;
        buff.read_f32()?;
        Ok(())
    }
);
