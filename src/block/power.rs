//! power connection and generation
use thiserror::Error;

use crate::block::simple::*;
use crate::block::*;
use crate::data::dynamic::DynType;

make_simple!(GeneratorBlock => |_, buff: &mut DataRead| read_generator(buff));
make_simple!(NuclearGeneratorBlock => |_, buff: &mut DataRead| read_nuclear(buff));
make_simple!(ImpactReactorBlock => |_, buff: &mut DataRead| read_impact(buff));
make_simple!(
    Neoplasia,
    |_, _, _, _, rot: Rotation, scl| {
        let mut base = load!("neoplasia-reactor", scl);
        unsafe {
            base.overlay(
                load!(scl -> match rot {
                    Rotation::Up | Rotation::Right => "neoplasia-reactor-top1",
                    Rotation::Down | Rotation::Left => "neoplasia-reactor-top2",
                })
                .rotate(rot.rotated(false).count()),
            )
        };
        base
    },
    |_, buff: &mut DataRead| read_heater(buff)
);
make_simple!(DiodeBlock, |_, _, _, _, rot: Rotation, s| {
    let mut base = load!("diode", s);
    if rot == Rotation::Right {
        return base;
    }
    let mut top = load!("diode-arrow", s);
    unsafe {
        top.rotate(rot.rotated(false).count());
        base.overlay(&top)
    };
    base
});

pub struct ConnectorBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
    pub max: u8,
}

impl ConnectorBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost, max: u8) -> Self {
        assert!(size != 0, "invalid size");
        assert!(
            !(max == 0 || max > i8::MAX as u8),
            "invalid maximum link count"
        );
        Self {
            size,
            symmetric,
            build_cost,
            max,
        }
    }
    state_impl!(pub Vec<(i16, i16)>);
}

impl BlockLogic for ConnectorBlock {
    impl_block!();

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Empty)
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(Vec::new()))),
            DynData::Point2Array(s) => Ok(Some(Self::create_state(s))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Boolean,
            }),
        }
    }

    fn mirror_state(&self, state: &mut State, horizontally: bool, vertically: bool) {
        for (dx, dy) in &mut *Self::get_state_mut(state) {
            if horizontally {
                *dx = -*dx;
            }
            if vertically {
                *dy = -*dy;
            }
        }
    }

    fn rotate_state(&self, state: &mut State, clockwise: bool) {
        for (dx, dy) in &mut *Self::get_state_mut(state) {
            let (cdx, cdy) = (*dx, *dy);
            *dx = if clockwise { cdy } else { -cdy };
            *dy = if clockwise { -cdx } else { cdx };
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(DynData::Point2Array(Self::get_state(state).clone()))
    }
}

#[derive(Debug, Error)]
pub enum ConnectorDeserializeError {
    #[error("too many links ({have} but only {max} allowed)")]
    LinkCount { have: usize, max: u8 },
}

impl ConnectorDeserializeError {
    pub fn forward<T, E: Into<Self>>(result: Result<T, E>) -> Result<T, DeserializeError> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(DeserializeError::Custom(Box::new(e.into()))),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Rgba(u8, u8, u8, u8);

impl From<u32> for Rgba {
    fn from(value: u32) -> Self {
        Self(
            (value >> 24) as u8,
            (value >> 16) as u8,
            (value >> 8) as u8,
            value as u8,
        )
    }
}

impl From<Rgba> for u32 {
    fn from(value: Rgba) -> Self {
        (u32::from(value.0) << 24)
            | (u32::from(value.1) << 16)
            | (u32::from(value.2) << 8)
            | u32::from(value.3)
    }
}

pub struct LampBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl LampBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub Rgba);
}

impl BlockLogic for LampBlock {
    impl_block!();

    fn data_from_i32(&self, config: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Int(config))
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Int(rgba) => Ok(Some(Self::create_state(Rgba::from(rgba as u32)))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Int,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        let state = Self::get_state(state);
        Ok(DynData::Int(u32::from(*state) as i32))
    }
}

/// format:
/// - production efficiency: [`f32`]
/// - generate time: [`f32`]
fn read_generator(buff: &mut DataRead) -> Result<(), DataReadError> {
    buff.skip(8)
}

/// format:
/// - call [`read_generator`]
/// - heat: [`f32`]
fn read_nuclear(buff: &mut DataRead) -> Result<(), DataReadError> {
    read_generator(buff)?;
    buff.skip(4)
}

/// format:
/// - call [`read_generator`]
/// - warmup: [`f32`]
fn read_impact(buff: &mut DataRead) -> Result<(), DataReadError> {
    read_generator(buff)?;
    buff.skip(4)
}

/// format:
/// - call [`read_generator`]
/// - heat: [`f32`]
fn read_heater(buff: &mut DataRead) -> Result<(), DataReadError> {
    read_generator(buff)?;
    buff.skip(4)
}
