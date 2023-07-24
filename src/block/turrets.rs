//! idk why its not in the [`crate::block::defense`] module
use super::simple::make_simple;
use super::{BlockLogic, Rotation, State};
use crate::block::make_register;
use crate::block::simple::cost;
use crate::data::{renderer::*, DataRead, ReadError};
use crate::utils::ImageUtils;

make_register! {
    "duo" => ItemTurret::new(1, true, cost!(Copper: 35));
    "scatter" => ItemTurret::new(2, true, cost!(Copper: 85, Lead: 45));
    "scorch" => ItemTurret::new(1, true, cost!(Copper: 25, Graphite: 22));
    "hail" => ItemTurret::new(1, true, cost!(Copper: 40, Graphite: 17));
    "wave" => TurretBlock::new(2, true, cost!(Copper: 25, Lead: 75, Metaglass: 45));
    "lancer" => TurretBlock::new(2, true, cost!(Copper: 60, Lead: 70, Titanium: 30, Silicon: 60));
    "arc" => TurretBlock::new(1, true, cost!(Copper: 50, Lead: 50));
    "parallax" => TurretBlock::new(2, true, cost!(Graphite: 30, Titanium: 90, Silicon: 120));
    "swarmer" => ItemTurret::new(2, true, cost!(Graphite: 35, Titanium: 35, Silicon: 30, Plastanium: 45));
    "salvo" => ItemTurret::new(2, true, cost!(Copper: 100, Graphite: 80, Titanium: 50));
    "segment" => TurretBlock::new(2, true, cost!(Titanium: 40, Thorium: 80, Silicon: 130, PhaseFabric: 40));
    "tsunami" => TurretBlock::new(3, true, cost!(Lead: 400, Metaglass: 100, Titanium: 250, Thorium: 100));
    "fuse" => ItemTurret::new(3, true, cost!(Copper: 225, Graphite: 225, Thorium: 100));
    "ripple" => ItemTurret::new(3, true, cost!(Copper: 150, Graphite: 135, Titanium: 60));
    "cyclone" => ItemTurret::new(3, true, cost!(Copper: 200, Titanium: 125, Plastanium: 80));
    "foreshadow" => ItemTurret::new(4, true, cost!(Copper: 1000, Metaglass: 600, Silicon: 600, Plastanium: 200, SurgeAlloy: 300));
    "spectre" => ItemTurret::new(4, true, cost!(Copper: 900, Graphite: 300, Thorium: 250, Plastanium: 175, SurgeAlloy: 250));
    "meltdown" => TurretBlock::new(4, true, cost!(Copper: 1200, Lead: 350, Graphite: 300, Silicon: 325, SurgeAlloy: 325));
    "breach" => ItemTurret::new(3, true, cost!(Beryllium: 150, Silicon: 150, Graphite: 250));
    "diffuse" => ItemTurret::new(3, true, cost!(Beryllium: 150, Silicon: 200, Graphite: 200, Tungsten: 50));
    "sublimate" => TurretBlock::new(3, true, cost!(Tungsten: 150, Silicon: 200, Oxide: 40, Beryllium: 400));
    "titan" => ItemTurret::new(4, true, cost!(Tungsten: 250, Silicon: 300, Thorium: 400));
    "disperse" => ItemTurret::new(4, true, cost!(Thorium: 50, Oxide: 150, Silicon: 200, Beryllium: 350));
    "afflict" => TurretBlock::new(4, true, cost!(SurgeAlloy: 100, Silicon: 200, Graphite: 250, Oxide: 40));
    "lustre" => TurretBlock::new(4, true, cost!(Silicon: 250, Graphite: 200, Oxide: 50, Carbide: 90));
    "scathe" => ItemTurret::new(5, true, cost!(Oxide: 200, SurgeAlloy: 400, Silicon: 800, Carbide: 500, PhaseFabric: 300));
    "malign" => TurretBlock::new(5, true, cost!(Carbide: 400, Beryllium: 2000, Silicon: 800, Graphite: 800, PhaseFabric: 300));
    "smite" => ItemTurret::new(5, true, cost!(Oxide: 200, SurgeAlloy: 400, Silicon: 800, Carbide: 500, PhaseFabric: 300));
}

fn draw_turret(
    me: &impl BlockLogic,
    _: &str,
    name: &str,
    _: Option<&State>,
    _: Option<&RenderingContext>,
    _: Rotation,
) -> Option<ImageHolder> {
    let path = match name {
        "breach" | "diffuse" | "sublimate" | "titan" | "disperse" | "afflict" | "lustre"
        | "scathe" | "malign" | "smite" => format!("bases/reinforced-block-{}", me.get_size()),
        _ => format!("bases/block-{}", me.get_size()),
    };
    let mut base = load("turrets", &path).unwrap().value().clone();
    base.overlay(load("turrets", name).unwrap().value(), 0, 0);
    Some(ImageHolder::from(base))
}

make_simple!(TurretBlock, draw_turret);

make_simple!(ItemTurret, draw_turret, |_, _, _, buff: &mut DataRead| {
    read_item_turret(buff)
});

/// format:
/// - iterate `u8`
///     - item: `u16` as item
///     - add: `u16`
fn read_item_turret(buff: &mut DataRead) -> Result<(), ReadError> {
    read_turret(buff)?;
    for _ in 0..buff.read_u8()? {
        buff.skip(4)?;
    }
    Ok(())
}

/// format:
/// - reload: f32
/// - rotation: f32
fn read_turret(buff: &mut DataRead) -> Result<(), ReadError> {
    buff.skip(8)
}
