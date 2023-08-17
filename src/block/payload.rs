//! payload related bits and bobs
use thiserror::Error;

use crate::block::content::Type as BlockEnum;
use crate::block::distribution::BridgeBlock;
use crate::block::simple::*;
use crate::block::{self, *};
use crate::content::{self, Content};
use crate::data::dynamic::DynType;
use crate::data::entity_mapping;
use crate::data::ReadError;
use crate::unit;

use super::BlockRegistry;

make_simple!(SimplePayloadBlock, |_, n, _, _, r: Rotation, scl| {
    match n {
        "deconstructor" | "small-deconstructor" | "payload-void" => {
            let mut base = load!(from n which is ["deconstructor" | "small-deconstructor" | "payload-void"], scl);
            let mut r#in = load!(scl -> match n {
                "small-deconstructor" => "factory-in-3",
                _ => "factory-in-5",
            });
            unsafe {
                base.overlay(r#in.rotate(r.rotated(false).count()))
                    .overlay(&load!(scl -> match n {
                        "small-deconstructor" => "small-deconstructor-top",
                        "deconstructor" => "deconstructor-top",
                        _ => "payload-void-top",
                    }))
            };
            base
        }
        // "payload-loader" | "payload-unloader"
        _ => {
            let mut base = load!(from n which is ["payload-loader" | "payload-unloader"], scl);
            let mut input = load!("factory-in-3-dark", scl);
            let mut output = load!("factory-out-3-dark", scl);
            unsafe {
                base.overlay(input.rotate(r.rotated(false).count()))
                .overlay(output.rotate(r.rotated(false).count()))
                .overlay(
                    &load!(concat "top" => n which is ["payload-loader" | "payload-unloader"], scl),
                )
            };
            base
        }
    }
});
make_simple!(
    PayloadConveyor,
    |_, n, _, _, r: Rotation, s| {
        let mut base =
            load!(from n which is ["payload-conveyor" | "reinforced-payload-conveyor"], s);
        unsafe { base.rotate(r.rotated(false).count()) };
        base
    },
    read_payload_conveyor
);

make_simple!(
    PayloadRouter,
    |_, n, _, _, r: Rotation, s| {
        let mut base = load!(from n which is ["payload-router" | "reinforced-payload-router"], s);
        unsafe {
            base.rotate(r.rotated(false).count());
            base.overlay(& load!(concat "over" => n which is ["payload-router" | "reinforced-payload-router"], s));
        }
        base
    } // read_payload_router
);

make_register! {
    "payload-conveyor" => PayloadConveyor::new(3, false, cost!(Copper: 10, Graphite: 10));
    "payload-router" => PayloadRouter::new(3, false, cost!(Copper: 10, Graphite: 15));
    "reinforced-payload-conveyor" => PayloadConveyor::new(3, false, cost!(Tungsten: 10));
    "reinforced-payload-router" => PayloadRouter::new(3, false, cost!(Tungsten: 15));
    "payload-mass-driver" -> BridgeBlock::new(3, true, cost!(Tungsten: 120, Silicon: 120, Graphite: 50), 700, false);
    "large-payload-mass-driver" -> BridgeBlock::new(5, true, cost!(Thorium: 200, Tungsten: 200, Silicon: 200, Graphite: 100, Oxide: 30), 1100, false);
    "small-deconstructor" => SimplePayloadBlock::new(3, true, cost!(Beryllium: 100, Silicon: 100, Oxide: 40, Graphite: 80));
    "deconstructor" => SimplePayloadBlock::new(5, true, cost!(Beryllium: 250, Oxide: 100, Silicon: 250, Carbide: 250));
    "constructor" => PayloadBlock::new(3, true, cost!(Silicon: 100, Beryllium: 150, Tungsten: 80));
    "large-constructor" => PayloadBlock::new(5, true, cost!(Silicon: 150, Oxide: 150, Tungsten: 200, PhaseFabric: 40));
    "payload-loader" => SimplePayloadBlock::new(3, false, cost!(Graphite: 50, Silicon: 50, Tungsten: 80));
    "payload-unloader" => SimplePayloadBlock::new(3, false, cost!(Graphite: 50, Silicon: 50, Tungsten: 30));
    // sandbox only
    "payload-source" => PayloadBlock::new(5, false, &[]);
    "payload-void" => SimplePayloadBlock::new(5, true, &[]);
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// payload item cfg
pub enum Payload {
    Empty,
    Block(block::content::Type),
    Unit(unit::Type),
}

/// a payload related block with [item cfg](Payload)
pub struct PayloadBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl PayloadBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub Payload);
}

impl BlockLogic for PayloadBlock {
    impl_block!();

    fn draw(
        &self,
        name: &str,
        _: Option<&State>,
        _: Option<&RenderingContext>,
        r: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        let mut base =
            load!(from name which is ["constructor" | "large-constructor" | "payload-source"], s);
        let mut out = load!(s -> match name {
            "constructor" => "factory-out-3",
            "large-constructor" => "factory-out-5-dark",
            _ => "factory-out-5",
        });
        unsafe {
            out.rotate(r.rotated(false).count());
            base.overlay(&out);
            base.overlay(&load!(concat "top" => name which is ["constructor" | "large-constructor" | "payload-source"], s))
        };
        base
    }

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Empty)
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(Payload::Empty))),
            DynData::Content(content::Type::Block, id) => {
                let block = PayloadDeserializeError::forward(block::content::Type::try_from(id))?;
                Ok(Some(Self::create_state(Payload::Block(block))))
            }
            DynData::Content(content::Type::Unit, id) => {
                let unit = PayloadDeserializeError::forward(unit::Type::try_from(id))?;
                Ok(Some(Self::create_state(Payload::Unit(unit))))
            }
            DynData::Content(have, ..) => Err(DeserializeError::Custom(Box::new(
                PayloadDeserializeError::ContentType(have),
            ))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Content,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        match Self::get_state(state) {
            Payload::Empty => Ok(DynData::Empty),
            Payload::Block(block) => Ok(DynData::Content(content::Type::Block, (*block).into())),
            Payload::Unit(unit) => Ok(DynData::Content(content::Type::Unit, (*unit).into())),
        }
    }
}

/// format:
/// - call [`read_payload_conveyor`]
/// - t: [`u8`]
/// - sort: [`u16`]
/// - recdir: [`u8`]
fn read_payload_router(
    b: &mut Build,
    reg: &BlockRegistry,
    buff: &mut DataRead,
) -> Result<(), DataReadError> {
    read_payload_conveyor(b, reg, buff)?;
    buff.skip(4)
}

/// format:
/// - [`skip(4)`](`DataRead::skip`)
/// - rot: [`f32`]
/// - become [`read_payload`]
fn read_payload_conveyor(
    _: &mut Build,
    reg: &BlockRegistry,
    buff: &mut DataRead,
) -> Result<(), DataReadError> {
    buff.skip(8)?;
    read_payload(reg, buff)
}

/// format:
/// - iterate [`i16`]..0
///     - [`u8`], [`i16`], [`i32`]
pub(crate) fn read_payload_seq(buff: &mut DataRead) -> Result<(), DataReadError> {
    let amount = (-buff.read_i16()?) as usize;
    buff.skip(amount * 7)
}

/// format:
/// - vector: ([`f32`], [`f32`])
/// - rotation: [`f32`]
/// - become [`read_payload`]
pub(crate) fn read_payload_block(
    reg: &BlockRegistry,
    buff: &mut DataRead,
) -> Result<(), DataReadError> {
    buff.skip(12)?;
    read_payload(reg, buff)
}

/// format:
/// - exists: [`bool`]
/// - if !exists: ok
/// - type: [`u8`]
/// - if type == `1` (payload block):
///     - block: [`u16`]
///     - version: [`u8`]
///     - [`BlockLogic::read`] (recursion :ferrisHmm:),
/// - if type == 2 (paylood unit):
///     - id: [`u8`]
///     - call [`UnitClass::read`](crate::data::entity_mapping::UnitClass::read)
fn read_payload(reg: &BlockRegistry, buff: &mut DataRead) -> Result<(), DataReadError> {
    if !buff.read_bool()? {
        return Ok(());
    }
    let t = buff.read_u8()?;
    const BLOCK: u8 = 1;
    const UNIT: u8 = 0;
    match t {
        BLOCK => {
            let b = buff.read_u16()?;
            let b = BlockEnum::try_from(b).unwrap_or(BlockEnum::Router);
            let block = reg.get(b.get_name()).unwrap();
            block.logic.read(&mut Build::new(block), reg, buff)?;
        }
        UNIT => {
            let u = buff.read_u8()? as usize;
            let Some(&Some(u)) = entity_mapping::ID.get(u) else {
                return Err(ReadError::Expected("map entry"));
            };
            let _ = u.read(buff)?;
        }
        _ => return Err(ReadError::Expected("0 | 1")),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::registry::Registry;

    use super::*;
    #[test]
    fn payload_conv() {
        let mut reg = Registry::default();
        register(&mut reg);
        let mut r = DataRead::new(&[0, 0, 0, 0, 0, 0, 0, 0, 0]);
        read_payload_conveyor(&mut Build::new(&PAYLOAD_CONVEYOR), &reg, &mut r).unwrap();
        assert!(r.read_bool().is_err());
        let mut r = DataRead::new(&[
            65, 198, 232, 0, 67, 51, 255, 249, 1, 1, 0, 157, 0, 67, 197, 128, 0, 128, 1, 3,
        ]);
        read_payload_conveyor(&mut Build::new(&PAYLOAD_CONVEYOR), &reg, &mut r).unwrap();
        assert!(r.read_bool().is_err());
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Error)]
pub enum PayloadDeserializeError {
    #[error("expected Unit or Block but got {0:?}")]
    ContentType(content::Type),
    #[error("payload block not found")]
    BlockNotFound(#[from] block::content::TryFromU16Error),
    #[error("payload unit not found")]
    UnitNotFound(#[from] unit::TryFromU16Error),
}

impl PayloadDeserializeError {
    pub fn forward<T, E: Into<Self>>(result: Result<T, E>) -> Result<T, DeserializeError> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(DeserializeError::Custom(Box::new(e.into()))),
        }
    }
}
