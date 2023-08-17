//! deal with blocks.
//!
//! categorized as mindustry categorizes them in its assets folder, for easy drawing.
//!
//! with the exception of sandbox, that is.
use bobbin_bits::U4::{self, B0000, B0001, B0010, B0100, B1000};
use std::error::Error;
use std::fmt;

use crate::data::dynamic::{DynData, DynType};
use crate::data::map::Build;
use crate::data::{self, renderer::*, CompressError};
use crate::data::{DataRead, GridPos, ReadError as DataReadError};
use crate::item::storage::ItemStorage;
use crate::registry::RegistryEntry;

macro_rules! mods {
    ($($mod:ident)*) => {
        $(pub mod $mod;)*

        pub mod all {
            $(pub use crate::block::$mod::*;)*
            pub use super::simple::BasicBlock;
        }
    }
}

mods! {
    campaign content defense distribution drills environment liquid logic payload power production storage turrets walls units
}

mod simple;

macro_rules! disp {
    ($($k:ident,)+) => {
        use all::{$($k,)+};
        #[enum_dispatch::enum_dispatch]
        pub(crate) enum BlockLogicEnum {
            $($k,)+
        }
        #[const_trait]
        pub trait ConstFrom<T>: Sized {
            fn fro(value: T) -> Self;
        }
        $(
            impl const ConstFrom<$k> for BlockLogicEnum {
                fn fro(v: $k) -> Self {
                    BlockLogicEnum::$k(v)
                }
            }
        )+

        /*impl std::fmt::Debug for BlockLogicEnum {
            fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                match self {
                    $(BlockLogicEnum::$k { .. } => write!(w, stringify!($k)),)+
                }
            }
        }*/
    }
}

disp! {
    BasicBlock,
    WallBlock,
    DuctBlock,
    BridgeBlock,
    ItemBlock,
    ProductionBlock,
    SeparatorBlock,
    StackConveyor,
    HeatCrafter,
    ConnectorBlock,
    ItemTurret,
    ConveyorBlock,
    PayloadRouter,
    WallDrillBlock,
    DrillBlock,
    NuclearGeneratorBlock,
    GeneratorBlock,
    ConduitBlock,
    HeatedBlock,
    RadarBlock,
    ShieldBlock,
    PointDefenseTurret,
    JunctionBlock,
    Turret,
    MemoryBlock,
    MessageLogic,
    ConstructorBlock,
    AssemblerBlock,
    UnitFactory,
    SimpleDuctBlock,
    SurgeRouter,
    SimplePayloadBlock,
    PayloadConveyor,
    ImpactReactorBlock,
    Neoplasia,
    DiodeBlock,
    HeatConduit,
    ContinousTurret,
    TractorBeamTurret,
    AssemblerModule,
    RepairTurret,
    FluidBlock,
    CanvasBlock,
    SwitchLogic,
    ProcessorLogic,
    PayloadBlock,
    LampBlock,
    DoorBlock,
}

pub trait Cast {
    fn downcast_ref(state: &State) -> Option<&Self>;
    fn downcast_mut(state: &mut State) -> Option<&mut Self>;
}

macro_rules! stater {
    ($($k: ident($v: ty),)+) => {
        #[derive(Debug, Clone)]
        pub enum State {
            $($k($v),)+
        }

        $(
            impl From<$v> for State {
                fn from(v: $v) -> State { State::$k(v) }
            }

            impl Cast for $v {
                fn downcast_ref(state: &State) -> Option<&Self> {
                    match state {
                        State::$k(v) => Some(v),
                        _ => None,
                    }
                }
                fn downcast_mut(state: &mut State) -> Option<&mut Self> {
                    match state {
                        State::$k(v) => Some(v),
                        _ => None,
                    }
                }
            }
        )+
    }
}

stater! {
    // TODO deoptionize
    String(String),
    Item(Option<crate::item::Type>),
    Fluid(Option<crate::fluid::Type>),
    Image(Image<Vec<u8>, 1>),
    Point(Option<(i32, i32)>),
    Bool(bool),
    Processor(logic::ProcessorState),
    Payload(payload::Payload),
    Power(Vec<(i16, i16)>),
    Color(power::Rgba),
    Command(Option<crate::data::command::UnitCommand>),
    Unit(Option<crate::unit::Type>),
}

impl State {
    pub fn downcast_ref<T: Cast>(&self) -> Option<&T> {
        T::downcast_ref(self)
    }

    pub fn downcast_mut<T: Cast>(&mut self) -> Option<&mut T> {
        T::downcast_mut(self)
    }

    pub fn new<T: Into<Self>>(from: T) -> Self {
        from.into()
    }
}

#[enum_dispatch::enum_dispatch(BlockLogicEnum)]
pub trait BlockLogic {
    /// mindustry blocks are the same width and height
    fn get_size(&self) -> u8;

    fn is_symmetric(&self) -> bool;

    fn create_build_cost(&self) -> Option<ItemStorage>;

    fn data_from_i32(&self, config: i32, pos: GridPos) -> Result<DynData, DataConvertError>;

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError>;

    #[allow(unused_variables)]
    fn mirror_state(&self, state: &mut State, horizontally: bool, vertically: bool) {}

    #[allow(unused_variables)]
    fn rotate_state(&self, state: &mut State, clockwise: bool) {}

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError>;

    #[allow(unused_variables)]
    fn draw(
        &self,
        name: &str,
        state: Option<&State>,
        context: Option<&RenderingContext>,
        rot: Rotation,
        scale: Scale,
    ) -> ImageHolder<4> {
        unimplemented!("{name}")
    }

    #[allow(unused_variables)]
    fn read(
        &self,
        build: &mut Build,
        reg: &BlockRegistry,
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
    image: Option<[Image<&'static [u8], 4>; 3]>,
    name: &'static str,
    logic: BlockLogicEnum,
}

impl PartialEq for Block {
    fn eq(&self, rhs: &Block) -> bool {
        self.name == rhs.name
    }
}

impl Block {
    /// create a new block
    #[must_use]
    #[inline]
    pub(crate) const fn new(
        name: &'static str,
        logic: BlockLogicEnum,
        image: Option<[Image<&'static [u8], 4>; 3]>,
    ) -> Self {
        Self { image, name, logic }
    }

    /// this blocks name
    /// ```
    /// assert!(mindus::block::distribution::DISTRIBUTOR.name() == "distributor")
    /// ```
    #[must_use]
    #[inline]
    pub const fn name(&self) -> &'static str {
        self.name
    }

    /// should you send context to [`image`]?
    #[must_use]
    #[inline]
    pub fn wants_context(&self) -> bool {
        use BlockLogicEnum::*;
        matches!(
            self.logic,
            ConveyorBlock(..) | DuctBlock(..) | StackConveyor(..) | ConduitBlock(..)
        )
    }

    /// draw this block, with this state
    #[must_use]
    #[inline]
    pub fn image(
        &self,
        state: Option<&State>,
        context: Option<&RenderingContext>,
        rot: Rotation,
        scale: Scale,
    ) -> ImageHolder<4> {
        if let Some(imgs) = &self.image {
            return ImageHolder::from((imgs[scale as usize]).copy());
        }
        self.logic.draw(self.name, state, context, rot, scale)
    }

    /// size.
    #[must_use]
    #[inline]
    pub fn get_size(&self) -> u8 {
        self.logic.get_size()
    }

    /// does it matter if its rotated
    #[must_use]
    #[inline]
    pub fn is_symmetric(&self) -> bool {
        self.logic.is_symmetric()
    }

    /// cost
    #[must_use]
    #[inline]
    pub fn get_build_cost(&self) -> Option<ItemStorage> {
        self.logic.create_build_cost()
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

    pub(crate) fn mirror_state(&self, state: &mut State, horizontally: bool, vertically: bool) {
        self.logic.mirror_state(state, horizontally, vertically);
    }

    pub(crate) fn rotate_state(&self, state: &mut State, clockwise: bool) {
        self.logic.rotate_state(state, clockwise);
    }

    pub(crate) fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        self.logic.serialize_state(state)
    }

    #[inline]
    pub(crate) fn read(
        &self,
        build: &mut Build,
        reg: &BlockRegistry,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        self.logic.read(build, reg, buff)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Block<{:?}>", self.name)
    }
}

impl RegistryEntry for Block {
    fn get_name(&self) -> &str {
        self.name
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
    pub const fn count(self) -> u8 {
        self as u8
    }

    #[must_use]
    /// mask
    pub const fn mask(self) -> U4 {
        match self {
            Rotation::Up => B1000,
            Rotation::Right => B0100,
            Rotation::Down => B0010,
            Rotation::Left => B0001,
        }
    }

    #[must_use]
    /// character of this rot (Right => >, Up => ^, Left => <, Down => v)
    pub const fn ch(self) -> char {
        match self {
            Rotation::Right => '>',
            Rotation::Up => '^',
            Rotation::Left => '<',
            Rotation::Down => 'v',
        }
    }

    #[must_use]
    /// mirror the directions.
    pub const fn mirrored(self, horizontally: bool, vertically: bool) -> Self {
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
    pub const fn rotated(self, clockwise: bool) -> Self {
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
    pub const fn rotated_180(self) -> Self {
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
	($($field:literal $op:tt $logic:expr;)+) => { paste::paste! {
		$(
            $crate::block::make_register!(impl $field $op $logic);
        )+

		pub(crate) fn register(reg: &mut $crate::block::BlockRegistry<'_>) {
            // get the static we make
			$(assert!(reg.register(&[<$field:snake:upper>]).is_ok());)+
		}
    }};
    (impl $field: literal => $logic: expr) => {
        paste::paste! { pub static [<$field:snake:upper>]: $crate::block::Block = $crate::block::Block::new(
            $field, <crate::block::BlockLogicEnum as crate::block::ConstFrom<_>>::fro($logic), None
        ); }
    };
    (impl $field: literal -> $logic: expr) => {
        paste::paste! { pub static [<$field:snake:upper>]: $crate::block::Block = $crate::block::Block::new(
            $field, <crate::block::BlockLogicEnum as crate::block::ConstFrom<_>>::fro($logic), Some(crate::data::renderer::load!($field))
        ); }
    }
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
