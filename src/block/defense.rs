//! defense
use crate::block::simple::*;
use crate::block::*;
make_simple!(HeatedBlock => |_, buff: &mut DataRead| read_heated(buff));
make_simple!(RadarBlock => |_, buff: &mut DataRead| buff.skip(4));
make_simple!(ShieldBlock => |_, buff: &mut DataRead| read_shield(buff));

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
