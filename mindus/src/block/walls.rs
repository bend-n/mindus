//! walls
use crate::block::simple::*;
use crate::block::*;
use crate::data::dynamic::DynType;
use crate::data::renderer::load;

make_simple!(WallBlock, |_, _, _, _, _, s| {
    let mut base = load!("thruster", s);
    unsafe { base.overlay(&load!("thruster-top", s)) };
    base
});

pub struct DoorBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl DoorBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub bool);
}

impl BlockLogic for DoorBlock {
    impl_block!();

    fn draw(
        &self,
        name: &str,
        state: Option<&State>,
        _: Option<&RenderingContext>,
        _: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        if let Some(state) = state {
            if *Self::get_state(state) {
                return load!(s -> match name {
                    "door" => "door-open",
                    "blast-door" => "blast-door-open",
                    _ => "door-large-open",
                });
            };
        }
        load!(from name which is ["door" | "blast-door" | "door-large"], s)
    }

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Boolean(false))
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Boolean(opened) => Ok(Some(Self::create_state(opened))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Boolean,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        let state = Self::get_state(state);
        Ok(DynData::Boolean(*state))
    }

    fn read(&self, build: &mut Build, buff: &mut DataRead) -> Result<(), DataReadError> {
        build.state = Some(Self::create_state(buff.read_bool()?));
        Ok(())
    }
}
