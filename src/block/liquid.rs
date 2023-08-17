//! liquid related things
use thiserror::Error;

use crate::block::distribution::BridgeBlock;
use crate::block::simple::*;
use crate::block::*;
use crate::content;
use crate::data::dynamic::DynType;
use crate::data::renderer::load;
use crate::fluid;
use crate::utils::ImageUtils;

make_simple!(ConduitBlock, |_,
                            name,
                            _,
                            ctx: Option<&RenderingContext>,
                            rot,
                            s| {
    let ctx = ctx.unwrap();

    let mask = mask(ctx, rot, name);
    // TODO caps. stopped trying bcz too complex
    mask2tile(mask, rot, name, s)
});

make_register! {
    "reinforced-pump" -> BasicBlock::new(2, true, cost!(Beryllium: 40, Tungsten: 30, Silicon: 20));
    "mechanical-pump" -> BasicBlock::new(1, true, cost!(Copper: 15, Metaglass: 10));
    "rotary-pump" -> BasicBlock::new(2, true, cost!(Copper: 70, Metaglass: 50, Titanium: 35, Silicon: 20));
    "impulse-pump" -> BasicBlock::new(3, true, cost!(Copper: 80, Metaglass: 90, Titanium: 40, Thorium: 35, Silicon: 30));
    "conduit" => ConduitBlock::new(1, false, cost!(Metaglass: 1));
    "pulse-conduit" => ConduitBlock::new(1, false, cost!(Metaglass: 1, Titanium: 2));
    "plated-conduit" => ConduitBlock::new(1, false, cost!(Metaglass: 1, Thorium: 2, Plastanium: 1));
    "liquid-router" -> BasicBlock::new(1, true, cost!(Metaglass: 2, Graphite: 4));
    "liquid-container" -> BasicBlock::new(2, true, cost!(Metaglass: 15, Titanium: 10));
    "liquid-tank" -> BasicBlock::new(3, true, cost!(Metaglass: 40, Titanium: 30));
    "liquid-junction" -> BasicBlock::new(1, true, cost!(Metaglass: 8, Graphite: 4));
    "bridge-conduit" -> BridgeBlock::new(1, true, cost!(Metaglass: 8, Graphite: 4), 4, true);
    "phase-conduit" -> BridgeBlock::new(1, true, cost!(Metaglass: 20, Titanium: 10, Silicon: 7, PhaseFabric: 5), 12, true);
    "reinforced-conduit" => ConduitBlock::new(1, false, cost!(Beryllium: 2));
    "reinforced-liquid-junction" -> BasicBlock::new(1, true, cost!(Graphite: 4, Beryllium: 8));
    "reinforced-bridge-conduit" => BridgeBlock::new(1, true, cost!(Graphite: 8, Beryllium: 20), 4, true);
    "reinforced-liquid-router" -> BasicBlock::new(1, true, cost!(Graphite: 8, Beryllium: 4));
    "reinforced-liquid-container" -> BasicBlock::new(2, true, cost!(Tungsten: 10, Beryllium: 16));
    "reinforced-liquid-tank" -> BasicBlock::new(3, true, cost!(Tungsten: 40, Beryllium: 50));
    // sandbox only
    "liquid-source" => FluidBlock::new(1, true, &[]);
    "liquid-void" -> BasicBlock::new(1, true, &[]);
}

pub struct FluidBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl FluidBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub Option<fluid::Type>);
}

impl BlockLogic for FluidBlock {
    impl_block!();

    fn data_from_i32(&self, config: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        if config < 0 || config > i32::from(u16::MAX) {
            return Err(DataConvertError::Custom(Box::new(FluidConvertError(
                config,
            ))));
        }
        Ok(DynData::Content(content::Type::Fluid, config as u16))
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(None))),
            DynData::Content(content::Type::Fluid, id) => Ok(Some(Self::create_state(Some(
                FluidDeserializeError::forward(fluid::Type::try_from(id))?,
            )))),
            DynData::Content(have, ..) => Err(DeserializeError::Custom(Box::new(
                FluidDeserializeError::ContentType(have),
            ))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Content,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(Self::get_state(state)
            .as_ref()
            .map_or(DynData::Empty, |&fluid| {
                DynData::Content(content::Type::Fluid, fluid.into())
            }))
    }

    fn draw(
        &self,
        _: &str,
        state: Option<&State>,
        _: Option<&RenderingContext>,
        _: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        let mut p = load!("liquid-source", s);
        if let Some(state) = state && let Some(liq) = Self::get_state(state) {
            let mut top = load!("center", s);
            unsafe { p.overlay(top.tint(liq.color())) };
            return p;
        }
        p
    }

    /// format:
    /// - fluid: [`u16`] as [`Fluid`](fluid::Type)
    fn read(
        &self,
        b: &mut Build,
        _: &BlockRegistry,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        let f = buff.read_u16()?;
        b.state = Some(Self::create_state(fluid::Type::try_from(f).ok()));
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Error)]
#[error("invalid config ({0}) for fluid")]
pub struct FluidConvertError(pub i32);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Error)]
pub enum FluidDeserializeError {
    #[error("expected Fluid but got {0:?}")]
    ContentType(content::Type),
    #[error("fluid not found")]
    NotFound(#[from] fluid::TryFromU16Error),
}

impl FluidDeserializeError {
    pub fn forward<T, E: Into<Self>>(result: Result<T, E>) -> Result<T, DeserializeError> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(DeserializeError::Custom(Box::new(e.into()))),
        }
    }
}
