//! type used for basic blocks, eg turrets and factorys
use crate::item;

macro_rules! state_impl {
	($vis:vis $type:ty) => {
		#[must_use] $vis fn get_state(state: &$crate::block::State) -> &$type
		where Self: Sized {
			state.downcast_ref::<$type>().unwrap()
		}

		$vis fn get_state_mut(state: &mut $crate::block::State) -> &mut $type
		where Self: Sized {
			state.downcast_mut::<$type>().unwrap()
		}

		fn create_state(val: $type) -> $crate::block::State
		where Self: Sized {
			State::from(val)
		}
	};
}

pub(crate) use state_impl;

/// draw is called with self, name, state, context, rotation
/// read is called with build, reg, `entity_mapping`, buff
macro_rules! make_simple {
    ($name: ident, $draw: expr, $read: expr) => {
        pub struct $name {
            size: u8,
            symmetric: bool,
            build_cost: crate::block::simple::BuildCost,
        }
        impl $name {
            #[must_use]
            pub const fn new(
                size: u8,
                symmetric: bool,
                build_cost: crate::block::simple::BuildCost,
            ) -> Self {
                assert!(size != 0, "invalid size");
                Self {
                    size,
                    symmetric,
                    build_cost,
                }
            }
        }

        impl crate::block::BlockLogic for $name {
            crate::block::impl_block!();

            fn data_from_i32(
                &self,
                _: i32,
                _: crate::data::GridPos,
            ) -> Result<crate::DynData, crate::block::DataConvertError> {
                Ok(crate::DynData::Empty)
            }

            fn deserialize_state(
                &self,
                _: crate::DynData,
            ) -> Result<Option<crate::block::State>, crate::block::DeserializeError> {
                Ok(None)
            }

            fn mirror_state(&self, _: &mut crate::block::State, _: bool, _: bool) {
                panic!("{} has no custom state", stringify!($name));
            }

            fn rotate_state(&self, _: &mut crate::block::State, _: bool) {
                panic!("{} has no custom state", stringify!($name));
            }

            fn serialize_state(
                &self,
                _: &crate::block::State,
            ) -> Result<crate::DynData, crate::block::SerializeError> {
                Ok(crate::DynData::Empty)
            }

            fn draw(
                &self,
                name: &str,
                state: Option<&crate::block::State>,
                context: Option<&crate::data::renderer::RenderingContext>,
                rot: crate::block::Rotation,
                scale: crate::data::renderer::Scale,
            ) -> crate::data::renderer::ImageHolder<4> {
                #[allow(clippy::redundant_closure_call)]
                $draw(self, name, state, context, rot, scale)
            }

            fn read(
                &self,
                build: &mut crate::data::map::Build,
                reg: &crate::block::BlockRegistry,
                buff: &mut crate::data::DataRead,
            ) -> Result<(), crate::data::ReadError> {
                #[allow(clippy::redundant_closure_call)]
                $read(build, reg, buff)
            }
        }
    };
    ($name: ident, $draw: expr) => {
        crate::block::simple::make_simple!($name, $draw, |_, _, _| Ok(()));
    };
    ($name: ident, $draw: expr, $read: expr) => {
        crate::block::simple::make_simple!($name, $draw, $read);
    };
    ($name: ident => $read: expr) => {
        crate::block::simple::make_simple!($name, |_, n, _, _, _, _| unimplemented!("{n}"), $read);
    };
    ($name: ident => $draw: expr, $read: expr) => {
        crate::block::simple::make_simple!($name, |_, _, _, _, _, scl| $draw(scl), $read);
    };
    ($name: ident / $draw: expr) => {
        crate::block::simple::make_simple!(
            $name,
            |_, _, _, _, _, scl| $draw(scl),
            |_, _, _| Ok(())
        );
    };
    ($name: ident) => {
        crate::block::simple::make_simple!($name, |_, n, _, _, _, _| unimplemented!("{n}"));
    };
}
pub(crate) use make_simple;

pub type BuildCost = &'static [(item::Type, u32)];

macro_rules! cost {
	($($item:ident: $cnt:expr),+) => {
		&[$((crate::item::Type::$item, $cnt)),*]
	};
}
pub(crate) use cost;

make_simple!(BasicBlock);
