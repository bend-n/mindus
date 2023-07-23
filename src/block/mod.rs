//! deal with blocks.
//!
//! categorized as mindustry categorizes them in its assets folder, for easy drawing.
//!
//! with the exception of sandbox, that is.
use bobbin_bits::U4::{self, *};
use std::any::Any;
use std::borrow::Cow;
use std::error::Error;
use std::fmt;

use crate::access::BoxAccess;
use crate::data::dynamic::{DynData, DynType};
use crate::data::map::{Build, EntityMapping};
use crate::data::{self, renderer::*, CompressError};
use crate::data::{DataRead, GridPos, ReadError as DataReadError};
use crate::item::storage::ItemStorage;
use crate::registry::RegistryEntry;

macro_rules! mods {
    ($($mod:ident)*) => {
        $(pub mod $mod;)*

        pub mod all {
            $(pub use crate::block::$mod::*;)*
        }
    }
}

mods! {
    campaign content defense distribution drills environment liquid logic payload power production storage turrets walls units
}

mod simple;

pub type State = Box<dyn Any + Sync + Send>;
pub trait BlockLogic {
    /// mindustry blocks are the same width and height
    fn get_size(&self) -> u8;

    fn is_symmetric(&self) -> bool;

    fn create_build_cost(&self) -> Option<ItemStorage>;

    fn data_from_i32(&self, config: i32, pos: GridPos) -> Result<DynData, DataConvertError>;

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError>;

    fn clone_state(&self, state: &State) -> State;

    #[allow(unused_variables)]
    fn mirror_state(&self, state: &mut State, horizontally: bool, vertically: bool) {}

    #[allow(unused_variables)]
    fn rotate_state(&self, state: &mut State, clockwise: bool) {}

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError>;

    #[allow(unused_variables)]
    fn draw(
        &self,
        category: &str,
        name: &str,
        state: Option<&State>,
        context: Option<&RenderingContext>,
        rot: Rotation,
    ) -> Option<ImageHolder> {
        None
    }

    fn want_context(&self) -> bool {
        false
    }

    #[allow(unused_variables)]
    fn read(
        &self,
        build: &mut Build,
        reg: &BlockRegistry,
        mapping: &EntityMapping,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        Ok(())
    }
}

// i wish i could derive
macro_rules! impl_block {
    () => {
        fn get_size(&self) -> u8 {
            self.size
        }

        fn is_symmetric(&self) -> bool {
            self.symmetric
        }

        fn create_build_cost(&self) -> Option<$crate::item::storage::ItemStorage> {
            if self.build_cost.is_empty() {
                None
            } else {
                let mut storage = crate::item::storage::Storage::new();
                for (ty, cnt) in self.build_cost {
                    storage.add(*ty, *cnt, u32::MAX);
                }
                Some(storage)
            }
        }
    };
}
pub(crate) use impl_block;

#[derive(Debug, thiserror::Error)]
pub enum DataConvertError {
    #[error(transparent)]
    Custom(#[from] Box<dyn Error + Sync + Send>),
}

impl DataConvertError {
    pub fn forward<T, E: Error + Sync + Send + 'static>(result: Result<T, E>) -> Result<T, Self> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(Self::Custom(Box::new(e))),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DeserializeError {
    #[error(transparent)]
    DecompressError(#[from] data::DecompressError),
    #[error("expected type {expect:?} but got {have:?}")]
    InvalidType { have: DynType, expect: DynType },
    #[error(transparent)]
    Custom(#[from] Box<dyn Error + Sync + Send>),
}

impl DeserializeError {
    pub fn forward<T, E: Error + Sync + Send + 'static>(result: Result<T, E>) -> Result<T, Self> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(Self::Custom(Box::new(e))),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SerializeError {
    #[error(transparent)]
    Custom(#[from] Box<dyn Error + Sync + Send>),
    #[error(transparent)]
    Compress(#[from] CompressError),
}

impl SerializeError {
    pub fn forward<T, E: Error + Sync + Send + 'static>(result: Result<T, E>) -> Result<T, Self> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(Self::Custom(Box::new(e))),
        }
    }
}

/// a block. put it in stuff!
pub struct Block {
    category: Cow<'static, str>,
    name: Cow<'static, str>,
    pub(crate) logic: BoxAccess<'static, dyn BlockLogic + Sync>,
}

impl PartialEq for Block {
    fn eq(&self, rhs: &Block) -> bool {
        self.name == rhs.name
    }
}

impl Block {
    #[must_use]
    /// create a new block
    pub const fn new(
        category: Cow<'static, str>,
        name: Cow<'static, str>,
        logic: BoxAccess<'static, dyn BlockLogic + Sync>,
    ) -> Self {
        Self {
            category,
            name,
            logic,
        }
    }

    /// this blocks name
    /// ```
    /// assert!(mindus::block::distribution::DISTRIBUTOR.name() == "distributor")
    /// ```
    pub fn name(&self) -> &str {
        &self.name
    }

    /// should you send context to [`image`]?
    pub fn wants_context(&self) -> bool {
        self.logic.as_ref().want_context()
    }

    /// draw this block, with this state
    pub fn image(
        &self,
        state: Option<&State>,
        context: Option<&RenderingContext>,
        rot: Rotation,
    ) -> ImageHolder {
        if let Some(p) = self
            .logic
            .as_ref()
            .draw(&self.category, &self.name, state, context, rot)
        {
            return p;
        }
        ImageHolder::Own(read(&self.category, &self.name, self.get_size()))
    }

    pub fn has_building(&self) -> bool {
        &self.category != "environment"
    }

    /// size.
    pub fn get_size(&self) -> u8 {
        self.logic.get_size()
    }

    /// does it matter if its rotated
    pub fn is_symmetric(&self) -> bool {
        self.logic.is_symmetric()
    }

    /// cost
    pub fn get_build_cost(&self) -> Option<ItemStorage> {
        self.logic.as_ref().create_build_cost()
    }

    pub(crate) fn data_from_i32(
        &self,
        config: i32,
        pos: GridPos,
    ) -> Result<DynData, DataConvertError> {
        self.logic.data_from_i32(config, pos)
    }

    pub(crate) fn deserialize_state(
        &self,
        data: DynData,
    ) -> Result<Option<State>, DeserializeError> {
        self.logic.deserialize_state(data)
    }

    pub(crate) fn clone_state(&self, state: &State) -> State {
        self.logic.clone_state(state)
    }

    pub(crate) fn mirror_state(&self, state: &mut State, horizontally: bool, vertically: bool) {
        self.logic.mirror_state(state, horizontally, vertically);
    }

    pub(crate) fn rotate_state(&self, state: &mut State, clockwise: bool) {
        self.logic.rotate_state(state, clockwise);
    }

    pub(crate) fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        self.logic.serialize_state(state)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name: &str = &self.name;
        write!(f, "Block<{name:?}>")
    }
}

impl RegistryEntry for Block {
    fn get_name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// the possible rotation states of a object
#[repr(u8)]
pub enum Rotation {
    Up,
    Right,
    Down,
    Left,
}

impl Rotation {
    #[must_use]
    /// count rotations
    pub fn count(self) -> u8 {
        self as u8
    }

    #[must_use]
    /// mask
    pub fn mask(self) -> U4 {
        match self {
            Rotation::Up => B1000,
            Rotation::Right => B0100,
            Rotation::Down => B0010,
            Rotation::Left => B0001,
        }
    }

    #[must_use]
    /// character of this rot (Right => >, Up => ^, Left => <, Down => v)
    pub fn ch(self) -> char {
        match self {
            Rotation::Right => '>',
            Rotation::Up => '^',
            Rotation::Left => '<',
            Rotation::Down => 'v',
        }
    }

    #[must_use]
    /// mirror the directions.
    pub fn mirrored(self, horizontally: bool, vertically: bool) -> Self {
        match self {
            Self::Right => {
                if horizontally {
                    Self::Left
                } else {
                    Self::Right
                }
            }
            Self::Up => {
                if vertically {
                    Self::Down
                } else {
                    Self::Up
                }
            }
            Self::Left => {
                if horizontally {
                    Self::Right
                } else {
                    Self::Left
                }
            }
            Self::Down => {
                if vertically {
                    Self::Up
                } else {
                    Self::Down
                }
            }
        }
    }

    /// mirror in place
    pub fn mirror(&mut self, horizontally: bool, vertically: bool) {
        *self = self.mirrored(horizontally, vertically);
    }

    #[must_use]
    /// rotate the rotation
    pub fn rotated(self, clockwise: bool) -> Self {
        match self {
            Self::Right => {
                if clockwise {
                    Self::Down
                } else {
                    Self::Up
                }
            }
            Self::Up => {
                if clockwise {
                    Self::Right
                } else {
                    Self::Left
                }
            }
            Self::Left => {
                if clockwise {
                    Self::Up
                } else {
                    Self::Down
                }
            }
            Self::Down => {
                if clockwise {
                    Self::Left
                } else {
                    Self::Right
                }
            }
        }
    }

    /// rotate the rotation in place
    pub fn rotate(&mut self, clockwise: bool) {
        *self = self.rotated(clockwise);
    }

    #[must_use]
    /// rotate 180
    pub fn rotated_180(self) -> Self {
        match self {
            Self::Right => Self::Left,
            Self::Up => Self::Down,
            Self::Left => Self::Right,
            Self::Down => Self::Up,
        }
    }

    /// rotate 180 in place
    pub fn rotate_180(&mut self) {
        *self = self.rotated_180();
    }
}

impl From<u8> for Rotation {
    fn from(val: u8) -> Self {
        match val & 3 {
            0 => Self::Right,
            1 => Self::Up,
            2 => Self::Left,
            _ => Self::Down,
        }
    }
}

impl From<Rotation> for u8 {
    fn from(rot: Rotation) -> Self {
        match rot {
            Rotation::Right => 0,
            Rotation::Up => 1,
            Rotation::Left => 2,
            Rotation::Down => 3,
        }
    }
}

pub type BlockRegistry<'l> = crate::registry::Registry<'l, Block>;
pub type RegisterError<'l> = crate::registry::RegisterError<'l, Block>;

macro_rules! make_register {
	($($field:literal => $logic:expr;)+) => { paste::paste! {
		$(
			pub static [<$field:snake:upper>]: $crate::block::Block = $crate::block::Block::new(
                std::borrow::Cow::Borrowed(
                    const_str::split!(module_path!(), "::")[2]
                ),
				std::borrow::Cow::Borrowed($field), $crate::access::Access::Borrowed(&$logic));
		)+

		pub(crate) fn register(reg: &mut $crate::block::BlockRegistry<'_>) {
			$(assert!(reg.register(&[<$field:snake:upper>]).is_ok(), "duplicate block {:?}", $field);)+
		}
    }};
}
pub(crate) use make_register;

#[must_use]
/// create a block registry
pub fn build_registry() -> BlockRegistry<'static> {
    let mut reg = BlockRegistry::default();
    register(&mut reg);
    reg
}

fn register(reg: &mut BlockRegistry<'_>) {
    turrets::register(reg);
    drills::register(reg);
    distribution::register(reg);
    storage::register(reg);
    liquid::register(reg);
    power::register(reg);
    defense::register(reg);
    production::register(reg);
    payload::register(reg);
    campaign::register(reg);
    logic::register(reg);
    walls::register(reg);
    environment::register(reg);
    units::register(reg);
}
