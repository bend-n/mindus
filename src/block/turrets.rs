//! idk why its not in the [`crate::block::defense`] module
use super::simple::make_simple;
use crate::data::{DataRead, ReadError};

make_simple!(Turret => |_, buff: &mut DataRead| read_turret(buff));
make_simple!(PointDefenseTurret => |_, buff: &mut DataRead| read_point_defense_turret(buff));
make_simple!(ContinousTurret => |_, buff: &mut DataRead| read_continous_turret(buff));
make_simple!(TractorBeamTurret => |_, buff: &mut DataRead| read_tractor_beam_turret(buff));
make_simple!(ItemTurret => |_, buff: &mut DataRead| read_item_turret(buff));

/// format:
/// - call [`read_turret`]
/// - iterate [`u8`]
///     - item: [`u16`] as [`Item`](crate::item::Type)
///     - amount: [`u16`]
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

/// format:
/// - rotation: [`f32`]
fn read_point_defense_turret(buff: &mut DataRead) -> Result<(), ReadError> {
    buff.skip(4)
}

/// format:
/// - call [`read_turret`]
/// - last length: [`f32`]
fn read_continous_turret(buff: &mut DataRead) -> Result<(), ReadError> {
    read_turret(buff)?;
    buff.skip(4)
}

/// format:
/// - rotation: [`f32`]
fn read_tractor_beam_turret(buff: &mut DataRead) -> Result<(), ReadError> {
    buff.skip(4)
}
