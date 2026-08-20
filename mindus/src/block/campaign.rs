use fimg::Overlay;

use crate::{
    block::{
        BlockLogic, DataConvertError, DeserializeError, SerializeError, State,
        all::{ItemConvertError, ItemDeserializeError},
        impl_block,
        simple::{BuildCost, state_impl},
    },
    content,
    data::{
        GridPos,
        dynamic::{DynData, DynType},
    },
    item::Type as Item,
    load,
    utils::{ImageHolder, ImageUtils},
};

pub struct LandingPadBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl LandingPadBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> LandingPadBlock {
        assert!(size != 0, "invalid size");

        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub Option<Item>);
}

impl BlockLogic for LandingPadBlock {
    impl_block!();

    fn data_from_i32(&self, config: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        if config < 0 || config > i32::from(u16::MAX) {
            return Err(DataConvertError::Custom(Box::new(ItemConvertError(config))));
        }
        Ok(DynData::Content(content::Type::Item, config as u16))
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(None))),
            DynData::Content(content::Type::Item, id) => Ok(Some(Self::create_state(Some(
                ItemDeserializeError::forward(Item::try_from(id))?,
            )))),
            DynData::Content(have, ..) => Err(DeserializeError::Custom(Box::new(
                ItemDeserializeError::ContentType(have),
            ))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Content,
            }),
        }
    }
    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(Self::get_state(state).map_or(DynData::Empty, |item| {
            DynData::Content(content::Type::Item, item.into())
        }))
    }
    fn draw(
        &self,
        _: &str,
        state: Option<&State>,
        _: Option<&crate::data::autotile::RenderingContext>,
        _: super::Rotation,
        scale: crate::data::renderer::Scale,
    ) -> crate::utils::ImageHolder<4> {
        let mut i: ImageHolder<4> = load!("landing-pad", scale);

        match state.map(Self::get_state) {
            Some(Some(item)) => {
                let mut x = load!("landing-pad-bg", scale);
                unsafe { i.overlay(x.tint(item.color())) };
            }
            _ => {}
        }
        i
    }
}
