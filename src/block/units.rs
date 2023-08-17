//! unit creation related blocks
use thiserror::Error;

use super::payload::{read_payload_block, read_payload_seq};
use crate::block::simple::*;
use crate::data::command::UnitCommand;
use crate::data::dynamic::DynType;
use crate::unit;
use crate::{block::*, Serializable};

// fn is_pay(b: &str) -> bool {
//     matches!(
//         b,
//         "ground-factory"
//             | "air-factory"
//             | "naval-factory"
//             | "additive-reconstructor"
//             | "multiplicative-reconstructor"
//             | "exponential-reconstructor"
//             | "tank-fabricator"
//             | "ship-fabricator"
//             | "mech-fabricator"
//             | "tank-refabricator"
//             | "ship-refabricator"
//             | "payload-conveyor"
//             | "payload-router"
//             | "reinforced-payload-conveyor"
//             | "reinforced-payload-router"
//             | "payload-mass-driver"
//             | "large-payload-mass-driver"
//             | "constructor"
//             | "large-constructor"
//             | "payload-source"
//     )
// }

make_simple!(
    AssemblerBlock,
    |_, name, _, _, rot: Rotation, s| {
        let mut base =
            load!(from name which is ["tank-assembler" | "ship-assembler" | "mech-assembler"], s);
        unsafe {
            base.overlay(
            match rot {
                Rotation::Up | Rotation::Right => load!(concat "side1" => name which is ["tank-assembler" | "ship-assembler" | "mech-assembler"], s),
                Rotation::Down | Rotation::Left => load!(concat "side2" => name which is ["tank-assembler" | "ship-assembler" | "mech-assembler"], s)
            }
            .rotate(rot.rotated(false).count())
        );
            base.overlay(&load!(concat "top" => name which is ["tank-assembler" | "ship-assembler" | "mech-assembler"], s))
        };
        base
    },
    |_, buff| read_assembler(buff)
);

/// format:
/// - call [`read_payload_block`]
/// - progress: [`f32`]
/// - iterate [`u8`]
///     - read: [`i32`]
/// - call [`read_payload_seq`]
/// - point: ([`f32`], [`f32`]) (maybe [`NaN`](f32::NAN))
fn read_assembler(buff: &mut DataRead) -> Result<(), DataReadError> {
    read_payload_block(buff)?;
    buff.skip(4)?;
    let n = buff.read_u8()? as usize;
    buff.skip(n * 4)?;
    read_payload_seq(buff)?;
    buff.skip(8)
}

make_simple!(
    AssemblerModule,
    |_, _, _, _, rot: Rotation, scl| {
        let mut base = load!("basic-assembler-module", scl);
        unsafe {
            base.overlay(
                load!(scl -> match rot {
                    Rotation::Up | Rotation::Right => "basic-assembler-module-side1",
                    _ => "basic-assembler-module-side2",
                })
                .rotate(rot.rotated(false).count()),
            )
        };
        base
    },
    |_, buff| read_payload_block(buff)
);

make_simple!(
    RepairTurret => |_, buff: &mut DataRead| {
        buff.skip(4) // rotation: [`f32`]
    }
);

pub const GROUND_UNITS: &[unit::Type] =
    &[unit::Type::Dagger, unit::Type::Crawler, unit::Type::Nova];
pub const AIR_UNITS: &[unit::Type] = &[unit::Type::Flare, unit::Type::Mono];
pub const NAVAL_UNITS: &[unit::Type] = &[unit::Type::Risso, unit::Type::Retusa];

pub struct ConstructorBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl ConstructorBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub Option<UnitCommand>);
}

impl BlockLogic for ConstructorBlock {
    impl_block!();

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Empty)
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(None))),
            DynData::UnitCommand(u) => Ok(Some(Self::create_state(Some(u)))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::UnitCommand,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(Self::get_state(state).map_or(DynData::Empty, DynData::UnitCommand))
    }

    fn draw(
        &self,
        name: &str,
        _: Option<&State>,
        _: Option<&RenderingContext>,
        rot: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        let mut base = load!(from name which is ["additive-reconstructor" | "multiplicative-reconstructor" | "exponential-reconstructor" | "tetrative-reconstructor" | "tank-refabricator" | "mech-refabricator" | "ship-refabricator" | "prime-refabricator"], s);
        let times = rot.rotated(false).count();
        let mut out = load!(s -> match name {
            "additive-reconstructor" => "factory-out-3",
            "multiplicative-reconstructor" => "factory-out-5",
            "tank-refabricator" | "mech-refabricator" | "ship-refabricator" =>
                "factory-out-3-dark",
            "exponential-reconstructor" => "factory-out-7",
            "prime-refabricator" => "factory-out-5-dark",
            "tetrative-reconstructor" => "factory-out-9",
        });
        unsafe { base.overlay(out.rotate(times)) };

        let mut r#in = load!(s -> match name {
            "additive-reconstructor" => "factory-in-3",
            "multiplicative-reconstructor" => "factory-in-5",
            "tank-refabricator" | "mech-refabricator" | "ship-refabricator" =>
                "factory-in-3-dark",
            "exponential-reconstructor" => "factory-in-7",
            "prime-refabricator" => "factory-in-5-dark",
            "tetrative-reconstructor" => "factory-in-9",
        });
        unsafe { base.overlay(r#in.rotate(times)) };

        // TODO: the context cross is too small
        // for i in 0..4u8 {
        //     if let Some((b, rot)) = dbg!(ctx.cross[i as usize]) {
        //         if rot.mirrored(true, true) != ctx.rotation &&  match rot {
        //             Rotation::Up => i == 3,
        //             Rotation::Right => i == 4,
        //             Rotation::Down => i == 0,
        //             Rotation::Left => i == 2,
        //         } && is_pay(b.name())
        //         {
        //             let r = unsafe { std::mem::transmute::<u8, Rotation>(i) }
        //                 .mirrored(true, true)
        //                 .rotated(false);
        //             let mut input = input.clone();
        //             input.rotate(r.count());
        //             base.overlay(&input);
        //         }
        //     }
        // }

        unsafe {
            base.overlay(&load!(concat "top" => name which is ["additive-reconstructor" | "multiplicative-reconstructor" | "exponential-reconstructor" | "tetrative-reconstructor" | "tank-refabricator" | "mech-refabricator" | "ship-refabricator" | "prime-refabricator"], s))
        };
        base
    }

    /// format:
    /// - call [`read_payload_block`]
    /// - progress: [`f32`]
    /// - point: ([`f32`], [`f32`]) (maybe [`NaN`](f32::NAN))
    /// - command: [`DynData::UnitCommand`]
    fn read(&self, _: &mut Build, buff: &mut DataRead) -> Result<(), DataReadError> {
        read_payload_block(buff)?;
        buff.skip(12)?;
        // self.deserialize_state(DynData::deserialize(buff).unwrap())
        //     .unwrap();
        Ok(())
    }
}

pub struct UnitFactory {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
    valid: &'static [unit::Type],
}

impl UnitFactory {
    #[must_use]
    pub const fn new(
        size: u8,
        symmetric: bool,
        build_cost: BuildCost,
        valid: &'static [unit::Type],
    ) -> Self {
        assert!(size != 0, "invalid size");
        assert!(!valid.is_empty(), "no valid units");
        assert!(valid.len() <= i32::MAX as usize, "too many valid units");
        Self {
            size,
            symmetric,
            build_cost,
            valid,
        }
    }

    state_impl!(pub Option<unit::Type>);
}

impl BlockLogic for UnitFactory {
    impl_block!();

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Int(-1))
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(None))),
            DynData::Int(idx) => {
                if idx == -1 {
                    Ok(Some(Self::create_state(None)))
                } else if idx >= 0 && idx < self.valid.len() as i32 {
                    Ok(Some(Self::create_state(Some(self.valid[idx as usize]))))
                } else {
                    Err(DeserializeError::Custom(Box::new(
                        AssemblerDeserializeError {
                            idx,
                            count: self.valid.len() as i32,
                        },
                    )))
                }
            }
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Int,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        if let Some(state) = Self::get_state(state) {
            for (i, curr) in self.valid.iter().enumerate() {
                if curr == state {
                    return Ok(DynData::Int(i as i32));
                }
            }
            Err(SerializeError::Custom(Box::new(AssemblerSerializeError(
                *state,
            ))))
        } else {
            Ok(DynData::Int(-1))
        }
    }

    fn draw(
        &self,
        name: &str,
        _: Option<&State>,
        _: Option<&RenderingContext>,
        rot: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        let mut base = load!(from name which is ["ground-factory" | "air-factory" | "naval-factory" | "tank-fabricator" | "ship-fabricator" | "mech-fabricator"], s);
        unsafe {
            base.overlay(
                load!(s -> match name {
                    "ground-factory" | "air-factory" | "naval-factory" => "factory-out-3",
                    _ => "factory-out-3-dark",
                })
                .rotate(rot.rotated(false).count()),
            )
            .overlay(&load!(s -> match name {
                "ground-factory" | "air-factory" | "naval-factory" => "factory-top-3",
                "tank-fabricator" => "tank-fabricator-top",
                "ship-fabricator" => "ship-fabricator-top",
                "mech-fabricator" => "mech-fabricator-top",
            }))
        };
        base
    }

    /// format:
    /// - call [`read_payload_block`]
    /// - progress: [`f32`]
    /// - plan: [`u16`]
    /// - point: ([`f32`], [`f32`]) (maybe [`NaN`](f32::NAN))
    fn read(&self, _: &mut Build, buff: &mut DataRead) -> Result<(), DataReadError> {
        read_payload_block(buff)?;
        buff.skip(14)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Error)]
#[error("invalid unit index ({idx}, valid: {count})")]
pub struct AssemblerDeserializeError {
    pub idx: i32,
    pub count: i32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Error)]
#[error("invalid unit {0:?}")]
pub struct AssemblerSerializeError(unit::Type);
