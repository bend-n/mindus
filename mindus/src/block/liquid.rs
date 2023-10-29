//! liquid related things
use thiserror::Error;

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
        if let Some(state) = state
            && let Some(liq) = Self::get_state(state)
        {
            let mut top = load!("center", s);
            unsafe { p.overlay(top.tint(liq.color())) };
            return p;
        }
        p
    }

    /// format:
    /// - fluid: [`u16`] as [`Fluid`](fluid::Type)
    fn read(&self, b: &mut Build, buff: &mut DataRead) -> Result<(), DataReadError> {
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
