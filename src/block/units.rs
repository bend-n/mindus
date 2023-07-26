//! unit creation related blocks
use thiserror::Error;

use super::payload::read_payload_block;
use crate::block::simple::*;
use crate::block::*;
use crate::data::dynamic::DynType;
use crate::unit;

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
    ConstructorBlock,
    |me: &Self, _, name, _, _, rot: Rotation| {
        let mut base = load("units", name).unwrap().to_owned();
        let times = rot.rotated(false).count();
        {
            let out = load(
                "payload",
                &match name {
                    "additive-reconstructor"
                    | "multiplicative-reconstructor"
                    | "exponential-reconstructor"
                    | "tetrative-reconstructor" => format!("factory-out-{}", me.size),
                    _ => format!("factory-out-{}-dark", me.size),
                },
            )
            .unwrap();
            if times != 0 {
                let mut out = out.clone();
                out.rotate(times);
                base.overlay(&out, 0, 0);
            } else {
                base.overlay(&out, 0, 0);
            }
        }
        {
            let input = load(
                "payload",
                &match name {
                    "additive-reconstructor"
                    | "multiplicative-reconstructor"
                    | "exponential-reconstructor"
                    | "tetrative-reconstructor" => format!("factory-in-{}", me.size),
                    _ => format!("factory-in-{}-dark", me.size),
                },
            )
            .unwrap();
            if times != 0 {
                let mut input = input.clone();
                input.rotate(times);
                base.overlay(&input, 0, 0);
            } else {
                base.overlay(&input, 0, 0);
            }
        }
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
        //             base.overlay(&input, 0, 0);
        //         }
        //     }
        // }
        {
            base.overlay(&load("units", &format!("{name}-top")).unwrap(), 0, 0);
        }
        if matches!(name, "mech-assembler" | "tank-assembler" | "ship-assembler") {
            let side = load("units", &format!("{name}-side")).unwrap();
            if times != 0 {
                let mut side = side.clone();
                side.rotate(times);
                base.overlay(&side, 0, 0);
            } else {
                base.overlay(&side, 0, 0);
            }
        }
        Some(ImageHolder::from(base))
    }
);
make_simple!(UnitBlock);
make_simple!(RepairTurret => |_, _, _, buff: &mut DataRead| {
   buff.skip(4) // rotation: [`f32`]
});

const GROUND_UNITS: &[unit::Type] = &[unit::Type::Dagger, unit::Type::Crawler, unit::Type::Nova];
const AIR_UNITS: &[unit::Type] = &[unit::Type::Flare, unit::Type::Mono];
const NAVAL_UNITS: &[unit::Type] = &[unit::Type::Risso, unit::Type::Retusa];

make_register! {
    "ground-factory" => AssemblerBlock::new(3, false, cost!(Copper: 50, Lead: 120, Silicon: 80), GROUND_UNITS);
    "air-factory" => AssemblerBlock::new(3, false, cost!(Copper: 60, Lead: 70), AIR_UNITS);
    "naval-factory" => AssemblerBlock::new(3, false, cost!(Copper: 150, Lead: 130, Metaglass: 120), NAVAL_UNITS);
    "additive-reconstructor" => ConstructorBlock::new(3, false, cost!(Copper: 200, Lead: 120, Silicon: 90));
    "multiplicative-reconstructor" => ConstructorBlock::new(5, false, cost!(Lead: 650, Titanium: 350, Thorium: 650, Silicon: 450));
    "exponential-reconstructor" => ConstructorBlock::new(7, false,
        cost!(Lead: 2000, Titanium: 2000, Thorium: 750, Silicon: 1000, Plastanium: 450, PhaseFabric: 600));
    "tetrative-reconstructor" => ConstructorBlock::new(9, false,
        cost!(Lead: 4000, Thorium: 1000, Silicon: 3000, Plastanium: 600, PhaseFabric: 600, SurgeAlloy: 800));
    "repair-point" => RepairTurret::new(1, true, cost!(Copper: 30, Lead: 30, Silicon: 20));
    "repair-turret" => RepairTurret::new(2, true, cost!(Thorium: 80, Silicon: 90, Plastanium: 60));
    "tank-fabricator" => AssemblerBlock::new(3, true, cost!(Silicon: 200, Beryllium: 150), &[unit::Type::Stell]);
    "ship-fabricator" => AssemblerBlock::new(3, true, cost!(Silicon: 250, Beryllium: 200), &[unit::Type::Elude]);
    "mech-fabricator" => AssemblerBlock::new(3, true, cost!(Silicon: 200, Graphite: 300, Tungsten: 60), &[unit::Type::Merui]);
    "tank-refabricator" => ConstructorBlock::new(3, true, cost!(Beryllium: 200, Tungsten: 80, Silicon: 100));
    "mech-refabricator" => ConstructorBlock::new(3, true, cost!(Beryllium: 250, Tungsten: 120, Silicon: 150));
    "ship-refabricator" => ConstructorBlock::new(3, true, cost!(Beryllium: 200, Tungsten: 100, Silicon: 150, Oxide: 40));
    "prime-refabricator" => ConstructorBlock::new(5, true, cost!(Thorium: 250, Oxide: 200, Tungsten: 200, Silicon: 400));
    "tank-assembler" => ConstructorBlock::new(5, true, cost!(Thorium: 500, Oxide: 150, Carbide: 80, Silicon: 500));
    "ship-assembler" => ConstructorBlock::new(5, true, cost!(Carbide: 100, Oxide: 200, Tungsten: 500, Silicon: 800, Thorium: 400));
    "mech-assembler" => ConstructorBlock::new(5, true, cost!(Carbide: 200, Thorium: 600, Oxide: 200, Tungsten: 500, Silicon: 900)); // smh collaris
    "basic-assembler-module" => UnitBlock::new(5, true, cost!(Carbide: 300, Thorium: 500, Oxide: 200, PhaseFabric: 400)); // the dummy block
    "unit-repair-tower" => UnitBlock::new(2, true, cost!(Graphite: 90, Silicon: 90, Tungsten: 80));

}

pub struct AssemblerBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
    valid: &'static [unit::Type],
}

impl AssemblerBlock {
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

impl BlockLogic for AssemblerBlock {
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

    fn clone_state(&self, state: &State) -> State {
        let state = Self::get_state(state);
        Box::new(Self::create_state(*state))
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
        _: &str,
        name: &str,
        _: Option<&State>,
        _: Option<&RenderingContext>,
        rot: Rotation,
    ) -> Option<ImageHolder> {
        let mut base = load("units", name).unwrap().to_owned();
        let out = load(
            "payload",
            match name {
                "ground-factory" | "air-factory" | "naval-factory" => "factory-out-3",
                _ => "factory-out-3-dark",
            },
        )
        .unwrap();
        let times = rot.rotated(false).count();
        if times != 0 {
            let mut out = out.clone();
            out.rotate(times);
            base.overlay(&out, 0, 0);
        } else {
            base.overlay(&out, 0, 0);
        }
        base.overlay(
            &load(
                match name {
                    "ground-factory" | "air-factory" | "naval-factory" => "payload",
                    _ => "units",
                },
                &match name {
                    "ground-factory" | "air-factory" | "naval-factory" => {
                        format!("factory-top-{}", self.size)
                    }
                    _ => format!("{name}-top"),
                },
            )
            .unwrap(),
            0,
            0,
        );
        Some(ImageHolder::from(base))
    }

    /// format:
    /// - call [`read_payload_block`]
    /// - progress: [`f32`]
    /// - plan: [`u16`]
    /// - point: ([`f32`], [`f32`]) (maybe [`NaN`](f32::NAN))
    fn read(
        &self,
        _: &mut Build,
        reg: &BlockRegistry,
        mapping: &EntityMapping,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        read_payload_block(reg, mapping, buff)?;
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
