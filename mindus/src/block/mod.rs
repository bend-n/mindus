//! deal with blocks.
//!
//! different block types are organized into modules
use bobbin_bits::U4::{self, B0000, B0001, B0010, B0100, B1000};
use std::error::Error;
use std::fmt;

use crate::data::dynamic::{DynData, DynType};
use crate::data::map::Build;
use crate::data::{self, renderer::*, CompressError};
use crate::data::{DataRead, GridPos, ReadError as DataReadError};
use crate::item::storage::ItemStorage;

macro_rules! mods {
    ($($mod:ident)*) => {
        $(pub mod $mod;)*

        mod all {
            $(#[allow(unused_imports)] pub use crate::block::$mod::*;)*
            pub use super::simple::BasicBlock;
        }
    }
}

mods! {
    content defense distribution drills liquid logic payload power production turrets walls units
}

pub mod ratios;
mod simple;
use simple::*;

macro_rules! disp {
    ($($k:ident,)+) => {
        use all::{$($k,)+};
        #[enum_dispatch::enum_dispatch]
        pub(crate) enum BlockLogicEnum {
            $($k,)+
        }

        impl ratios::Ratios for BlockLogicEnum {
            fn io(&self, state: Option<&State>, name: &str) -> ratios::Io {
                match self {
                    $(Self::$k(x) => x.io(state, name),)+
                }
            }
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
    UnitCargoLoader,
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
    Image(Image<Box<[u8]>, 1>),
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
pub trait BlockLogic: ratios::Ratios {
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
    fn read(&self, build: &mut Build, buff: &mut DataRead) -> Result<(), DataReadError> {
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
    /// assert!(mindus::block::DISTRIBUTOR.name() == "distributor")
    /// ```
    #[must_use]
    #[inline]
    pub const fn name(&self) -> &'static str {
        self.name
    }

    pub fn io(&self, state: Option<&State>) -> ratios::Io {
        <BlockLogicEnum as ratios::Ratios>::io(&self.logic, state, self.name)
    }

    /// should you send context to [`image`]?
    #[must_use]
    #[inline]
    pub const fn wants_context(&self) -> bool {
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
    pub(crate) fn read(&self, build: &mut Build, buff: &mut DataRead) -> Result<(), DataReadError> {
        self.logic.read(build, buff)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Block<{:?}>", self.name)
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

macro_rules! make_register {
	($($field:literal $op:tt $logic:expr;)+) => { paste::paste! {
		$(
            make_register!(impl $field $op $logic);
        )+
        pub static BLOCK_REGISTRY: phf::Map<&str, &Block> = phf::phf_map! {$(
            $field => &[<$field:snake:upper>],
        )+};

        impl content::Type {
            pub fn to_block(self) -> Option<&'static Block> {
                // static L: &[&Block] = &[$(&[<$field:snake:upper>],)+];
                // L.get(self as usize).copied()
                match self {
                    $(content::Type::[<$field:camel>] => Some(&[<$field:snake:upper>]),)+
                    _ => None,
                }
            }
        }
    }};
    (impl $field: literal => $logic: expr) => {
        paste::paste! { pub static [<$field:snake:upper>]: Block = Block::new(
            $field, <crate::block::BlockLogicEnum as crate::block::ConstFrom<_>>::fro($logic), None
        ); }
    };
    (impl $field: literal -> $logic: expr) => {
        paste::paste! { pub static [<$field:snake:upper>]: Block = Block::new(
            $field, <crate::block::BlockLogicEnum as crate::block::ConstFrom<_>>::fro($logic), Some(crate::data::renderer::load!($field))
        ); }
    };
    (impl $field: literal : $size: literal) => {
        paste::paste! { pub static [<$field:snake:upper>]: Block = Block::new(
            $field, BlockLogicEnum::BasicBlock(BasicBlock::new($size, true, &[])), Some(crate::data::renderer::load!($field))
        ); }
    };
    // floors
    (impl $field: literal > $size: literal) => {
        paste::paste! { pub static [<$field:snake:upper>]: Block = Block::new(
            $field, BlockLogicEnum::BasicBlock(BasicBlock::new($size, true, &[])), Some(crate::data::renderer::load!("empty4"))
        ); }
    };
}
// pub(self) use make_register;
make_register! {
    "darksand" > 1;
    "sand-floor" > 1;
    "yellow-stone" > 1;
    "arkyic-stone" > 1;
    "carbon-stone" > 1;
    "dacite" > 1;
    "dirt" > 1;
    "arkycite-floor" > 1;
    "basalt" > 1;
    "ice" > 1;
    "molten-slag" > 1;
    "moss" > 1;
    "mud" > 1;
    "magmarock" > 1;
    "grass" > 1;
    "ice-snow" > 1;
    "hotrock" > 1;
    "char" > 1;
    "snow" > 1;
    "salt" > 1;
    "shale" > 1;
    "metal-floor" > 1;
    "metal-floor-2" > 1;
    "metal-floor-3" > 1;
    "metal-floor-4" > 1;
    "metal-floor-5" > 1;
    "dark-panel-1" > 1;
    "dark-panel-2" > 1;
    "dark-panel-3" > 1;
    "dark-panel-4" > 1;
    "dark-panel-5" > 1;
    "dark-panel-6" > 1;
    "darksand-tainted-water" > 1;
    "darksand-water" > 1;
    "deep-tainted-water" > 1;
    "deep-water" > 1;
    "sand-water" > 1;
    "shallow-water" > 1;
    "space" > 1;
    "stone" > 1;
    "arkyic-vent" > 1;
    "beryllic-stone" > 1;
    "bluemat" > 1;
    "carbon-vent" > 1;
    "core-zone" > 1;
    "crater-stone" > 1;
    "crystal-floor" > 1;
    "crystalline-stone" > 1;
    "crystalline-vent" > 1;
    "metal-floor-damaged" > 1;
    "dense-red-stone" > 1;
    "ferric-craters" > 1; // ferris section
    "ferric-stone" > 1;
    "pooled-cryofluid" > 1;
    "red-ice" > 1;
    "red-stone-vent" > 1;
    "red-stone" > 1;
    "redmat" > 1;
    "regolith" > 1;
    "rhyolite-crater" > 1;
    "rhyolite" > 1;
    "rough-rhyolite" > 1;
    "tainted-water" > 1;
    "tar" > 1;
    "yellow-stone-plates" > 1;
    "yellow-stone-vent" > 1;
    "spore-moss" > 1;
    "ore-beryllium": 1;
    "ore-copper": 1;
    "ore-lead": 1;
    "ore-coal": 1;
    "ore-scrap": 1;
    "ore-thorium": 1;
    "ore-titanium": 1;
    "ore-tungsten": 1;
    "ore-crystal-thorium": 1;
    "ore-wall-beryllium": 1;
    "ore-wall-thorium": 1;
    "ore-wall-tungsten": 1;
    "graphitic-wall": 1;
    "boulder": 1;
    "arkyic-wall": 1;
    "beryllic-stone-wall": 1;
    "carbon-wall": 1;
    "cliff": 1;
    "crystalline-stone-wall": 1;
    "dacite-wall": 1;
    "dark-metal": 1;
    "dirt-wall": 1;
    "dune-wall": 1;
    "ferric-stone-wall": 1;
    "ice-wall": 1;
    "pebbles": 1;
    "pine": 1;
    "red-diamond-wall": 1;
    "red-ice-wall": 1;
    "red-stone-wall": 1;
    "regolith-wall": 1;
    "rhyolite-vent" > 1;
    "rhyolite-wall": 1;
    "salt-wall": 1;
    "sand-wall": 1;
    "shale-wall": 1;
    "shrubs": 1;
    "snow-pine": 1;
    "snow-wall": 1;
    "spawn": 1;
    "spore-pine": 1;
    "spore-wall": 1;
    "stone-wall": 1;
    "yellow-stone-wall": 1;
    // props
    "yellow-stone-boulder": 1;
    "snow-boulder": 1;
    "shale-boulder": 1;
    "arkyic-boulder": 1;
    "basalt-boulder": 1;
    "beryllic-boulder": 1;
    "carbon-boulder": 1;
    "crystalline-boulder": 1;
    "dacite-boulder": 1;
    "ferric-boulder": 1;
    "red-ice-boulder": 1;
    "red-stone-boulder": 1;
    "rhyolite-boulder": 1;
    "sand-boulder": 1;
    "pur-bush": 1;
    "tendrils": 1;
    // these are tall but uh (TODO layering)
    "white-tree-dead": 1;
    "yellowcoral": 1;
    "white-tree": 1;
    "redweed": 1;
    "spore-cluster": 1;
    "crystal-blocks": 1;
    "crystal-cluster": 1;
    "vibrant-crystal-cluster": 1;
    "crystal-orbs": 1;
    // end tall
    "build1": 1;
    "build2": 1;
    "build3": 1;
    "build4": 1;
    "build5": 1;
    "build6": 1;
    "build7": 1;
    "build8": 1;
    "build9": 1;
    "build10": 1;
    "build11": 1;
    "build12": 1;
    "build13": 1;
    "build14": 1;
    "build15": 1;
    "build16": 1;
    "conveyor" => ConveyorBlock::new(1, false, cost!(Copper: 1));
    "titanium-conveyor" => ConveyorBlock::new(1, false, cost!(Copper: 1, Lead: 1, Titanium: 1));
    "plastanium-conveyor" => StackConveyor::new(1, false, cost!(Graphite: 1, Silicon: 1, Plastanium: 1));
    "armored-conveyor" => ConveyorBlock::new(1, false, cost!(Metaglass: 1, Thorium: 1, Plastanium: 1));
    "junction" -> JunctionBlock::new(1, true, cost!(Copper: 2));
    "bridge-conveyor" -> BridgeBlock::new(1, false, cost!(Copper: 6, Lead: 6), 4, true);
    "phase-conveyor" -> BridgeBlock::new(1, false, cost!(Lead: 10, Graphite: 10, Silicon: 7, PhaseFabric: 5), 12, true);
    "sorter" => ItemBlock::new(1, true, cost!(Copper: 2, Lead: 2));
    "inverted-sorter" => ItemBlock::new(1, true, cost!(Copper: 2, Lead: 2));
    "unloader" => ItemBlock::new(1, true, cost!(Titanium: 25, Silicon: 30));
    "router" -> BasicBlock::new(1, true, cost!(Copper: 3));
    "distributor" -> BasicBlock::new(2, true, cost!(Copper: 4, Lead: 4));
    "overflow-gate" -> BasicBlock::new(1, true, cost!(Copper: 4, Lead: 2));
    "underflow-gate" -> BasicBlock::new(1, true, cost!(Copper: 4, Lead: 2));
    "mass-driver" => BridgeBlock::new(3, true, cost!(Lead: 125, Titanium: 125, Thorium: 50, Silicon: 75), 55, false);
    "duct" => DuctBlock::new(1, false, cost!(Beryllium: 1));
    "armored-duct" => DuctBlock::new(1, false, cost!(Beryllium: 2, Tungsten: 1));
    "duct-router" => ItemBlock::new(1, true, cost!(Beryllium: 10));
    "overflow-duct" => SimpleDuctBlock::new(1, true, cost!(Graphite: 8, Beryllium: 8));
    "underflow-duct" => SimpleDuctBlock::new(1, true, cost!(Graphite: 8, Beryllium: 8));
    "duct-bridge" => BridgeBlock::new(1, true, cost!(Beryllium: 20), 3, true);
    "duct-unloader" => ItemBlock::new(1, true, cost!(Graphite: 20, Silicon: 20, Tungsten: 10));
    "surge-conveyor" => StackConveyor::new(1, false, cost!(SurgeAlloy: 1, Tungsten: 1));
    "surge-router" => SurgeRouter::new(1, false, cost!(SurgeAlloy: 5, Tungsten: 1)); // not symmetric
    "unit-cargo-loader" -> UnitCargoLoader::new(3, true, cost!(Silicon: 80, SurgeAlloy: 50, Oxide: 20));
    "unit-cargo-unload-point" => ItemBlock::new(2, true, cost!(Silicon: 60, Tungsten: 60));
    "cultivator" -> ProductionBlock::new(2, true, cost!(Copper: 25, Lead: 25, Silicon: 10));
    "graphite-press" -> ProductionBlock::new(2, true, cost!(Copper: 75, Lead: 30));
    "multi-press" -> ProductionBlock::new(3, true, cost!(Lead: 100, Graphite: 50, Titanium: 100, Silicon: 25));
    "silicon-smelter" -> ProductionBlock::new(2, true, cost!(Copper: 30, Lead: 25));
    "silicon-crucible" -> ProductionBlock::new(3, true, cost!(Metaglass: 80, Titanium: 120, Silicon: 60, Plastanium: 35));
    "kiln" -> ProductionBlock::new(2, true, cost!(Copper: 60, Lead: 30, Graphite: 30));
    "plastanium-compressor" -> ProductionBlock::new(2, true, cost!(Lead: 115, Graphite: 60, Titanium: 80, Silicon: 80));
    "phase-weaver" -> ProductionBlock::new(2, true, cost!(Lead: 120, Thorium: 75, Silicon: 130));
    "surge-smelter" -> ProductionBlock::new(3, true, cost!(Lead: 80, Thorium: 70, Silicon: 80));
    "cryofluid-mixer" -> ProductionBlock::new(2, true, cost!(Lead: 65, Thorium: 60, Silicon: 40));
    "pyratite-mixer" -> ProductionBlock::new(2, true, cost!(Copper: 50, Lead: 25));
    "blast-mixer" -> ProductionBlock::new(2, true, cost!(Lead: 30, Titanium: 20));
    "melter" -> ProductionBlock::new(1, true, cost!(Copper: 30, Lead: 35, Graphite: 45));
    "separator" -> SeparatorBlock::new(2, true, cost!(Copper: 30, Titanium: 25));
    "disassembler" -> SeparatorBlock::new(3, true, cost!(Titanium: 100, Thorium: 80, Silicon: 150, Plastanium: 40));
    "spore-press" -> ProductionBlock::new(2, true, cost!(Lead: 35, Silicon: 30));
    "pulverizer" -> ProductionBlock::new(1, true, cost!(Copper: 30, Lead: 25));
    "coal-centrifuge" -> ProductionBlock::new(2, true, cost!(Lead: 30, Graphite: 40, Titanium: 20));
    "incinerator" -> BasicBlock::new(1, true, cost!(Lead: 15, Graphite: 5));
    "silicon-arc-furnace" -> ProductionBlock::new(3, true, cost!(Beryllium: 70, Graphite: 80));
    "electrolyzer" => ProductionBlock::new(3, true, cost!(Silicon: 50, Graphite: 40, Beryllium: 130, Tungsten: 80));
    "atmospheric-concentrator" -> ProductionBlock::new(3, true, cost!(Oxide: 60, Beryllium: 180, Silicon: 150));
    "oxidation-chamber" => HeatCrafter::new(3, true, cost!(Tungsten: 120, Graphite: 80, Silicon: 100, Beryllium: 120));
    "electric-heater" => HeatCrafter::new(2, false, cost!(Tungsten: 30, Oxide: 30));
    "slag-heater" => HeatCrafter::new(3, false, cost!(Tungsten: 50, Oxide: 20, Beryllium: 20));
    "phase-heater" => HeatCrafter::new(2, false, cost!(Oxide: 30, Carbide: 30, Beryllium: 30));
    "heat-redirector" => HeatConduit::new(3, false, cost!(Tungsten: 10, Graphite: 10));
    "heat-router" => HeatConduit::new(3, false, cost!(Tungsten: 15, Graphite: 10));
    "slag-incinerator" -> BasicBlock::new(1, true, cost!(Tungsten: 15));
    "carbide-crucible" -> ProductionBlock::new(3, true, cost!(Tungsten: 110, Thorium: 150, Oxide: 60));
    // slag centrifuge
    "surge-crucible" -> ProductionBlock::new(3, true, cost!(Silicon: 100, Graphite: 80, Tungsten: 80, Oxide: 80));
    "cyanogen-synthesizer" -> ProductionBlock::new(3, true, cost!(Carbide: 50, Silicon: 80, Beryllium: 90));
    "phase-synthesizer" -> ProductionBlock::new(3, true, cost!(Carbide: 90, Silicon: 100, Thorium: 100, Tungsten: 200));
    // heat reactor
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
    "copper-wall" -> WallBlock::new(1, true, cost!(Copper: 6));
    "copper-wall-large" -> WallBlock::new(2, true, cost!(Copper: 6 * 4));
    "titanium-wall" -> WallBlock::new(1, true, cost!(Titanium: 6));
    "titanium-wall-large" -> WallBlock::new(2, true, cost!(Titanium: 6 * 4));
    "plastanium-wall" -> WallBlock::new(1, true, cost!(Metaglass: 2, Plastanium: 5));
    "plastanium-wall-large" -> WallBlock::new(2, true, cost!(Metaglass: 2 * 4, Plastanium: 5 * 4));
    "thorium-wall" -> WallBlock::new(1, true, cost!(Thorium: 6));
    "thorium-wall-large" -> WallBlock::new(2, true, cost!(Thorium: 6 * 4));
    "phase-wall" -> WallBlock::new(1, true, cost!(PhaseFabric: 6));
    "phase-wall-large" -> WallBlock::new(2, true, cost!(PhaseFabric: 6 * 4));
    "surge-wall" -> WallBlock::new(1, true, cost!(SurgeAlloy: 6));
    "surge-wall-large" -> WallBlock::new(2, true, cost!(SurgeAlloy: 6 * 4));
    "door" => DoorBlock::new(1, true, cost!(Titanium: 6, Silicon: 4));
    "door-large" => DoorBlock::new(2, true, cost!(Titanium: 6 * 4, Silicon: 4 * 4));
    "tungsten-wall" -> WallBlock::new(1, true, cost!(Tungsten: 6));
    "tungsten-wall-large" -> WallBlock::new(2, true, cost!(Tungsten: 6 * 4));
    "blast-door" -> DoorBlock::new(2, true, cost!(Tungsten: 24, Silicon: 24));
    "reinforced-surge-wall" -> WallBlock::new(1, true, cost!(SurgeAlloy: 6, Tungsten: 2));
    "reinforced-surge-wall-large" -> WallBlock::new(2, true, cost!(SurgeAlloy: 6 * 4, Tungsten: 2 * 4));
    "carbide-wall" -> WallBlock::new(1, true, cost!(Thorium: 6, Carbide: 6));
    "carbide-wall-large" -> WallBlock::new(2, true, cost!(Thorium: 6 * 4, Carbide: 6 * 4));
    "shielded-wall" -> WallBlock::new(2, true, cost!(PhaseFabric: 20, SurgeAlloy: 12, Beryllium: 12));
    "beryllium-wall" -> WallBlock::new(1, true, cost!(Beryllium: 6));
    "beryllium-wall-large" -> WallBlock::new(2, true, cost!(Beryllium: 6 * 4));
    "scrap-wall" -> WallBlock::new(1, true, cost!(Scrap: 6));
    "scrap-wall-large" -> WallBlock::new(2, true, cost!(Scrap: 24));
    "scrap-wall-huge" -> WallBlock::new(3, true, cost!(Scrap: 54));
    "scrap-wall-gigantic" -> WallBlock::new(4, true, cost!(Scrap: 96));
    "thruster" => WallBlock::new(4, false, cost!(Scrap: 96));
    "mender" -> HeatedBlock::new(1, true, cost!(Copper: 25, Lead: 30));
    "mend-projector" -> HeatedBlock::new(2, true, cost!(Copper: 50, Lead: 100, Titanium: 25, Silicon: 40));
    "overdrive-projector" -> HeatedBlock::new(2, true, cost!(Lead: 100, Titanium: 75, Silicon: 75, Plastanium: 30));
    "overdrive-dome" -> HeatedBlock::new(3, true, cost!(Lead: 200, Titanium: 130, Silicon: 130, Plastanium: 80, SurgeAlloy: 120));
    "force-projector" -> BasicBlock::new(3, true, cost!(Lead: 100, Titanium: 75, Silicon: 125));
    "regen-projector" -> BasicBlock::new(3, true, cost!(Silicon: 80, Tungsten: 60, Oxide: 40, Beryllium: 80));
    "shock-mine" -> BasicBlock::new(1, true, cost!(Lead: 25, Silicon: 12));
    "radar" -> RadarBlock::new(1, true, cost!(Silicon: 60, Graphite: 50, Beryllium: 10));
    "build-tower" -> BasicBlock::new(3, true, cost!(Silicon: 150, Oxide: 40, Thorium: 60));
    "shockwave-tower" -> BasicBlock::new(3, true, cost!(SurgeAlloy: 50, Silicon: 150, Oxide: 30, Tungsten: 100));
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
    "ground-factory" => UnitFactory::new(3, false, cost!(Copper: 50, Lead: 120, Silicon: 80), units::GROUND_UNITS);
    "air-factory" => UnitFactory::new(3, false, cost!(Copper: 60, Lead: 70), units::AIR_UNITS);
    "naval-factory" => UnitFactory::new(3, false, cost!(Copper: 150, Lead: 130, Metaglass: 120), units::NAVAL_UNITS);
    "additive-reconstructor" => ConstructorBlock::new(3, false, cost!(Copper: 200, Lead: 120, Silicon: 90));
    "multiplicative-reconstructor" => ConstructorBlock::new(5, false, cost!(Lead: 650, Titanium: 350, Thorium: 650, Silicon: 450));
    "exponential-reconstructor" => ConstructorBlock::new(7, false, cost!(Lead: 2000, Titanium: 2000, Thorium: 750, Silicon: 1000, Plastanium: 450, PhaseFabric: 600));
    "tetrative-reconstructor" => ConstructorBlock::new(9, false, cost!(Lead: 4000, Thorium: 1000, Silicon: 3000, Plastanium: 600, PhaseFabric: 600, SurgeAlloy: 800));
    "repair-point" -> RepairTurret::new(1, true, cost!(Copper: 30, Lead: 30, Silicon: 20));
    "repair-turret" -> RepairTurret::new(2, true, cost!(Thorium: 80, Silicon: 90, Plastanium: 60));
    "tank-fabricator" => UnitFactory::new(3, true, cost!(Silicon: 200, Beryllium: 150), &[crate::unit::Type::Stell]);
    "ship-fabricator" => UnitFactory::new(3, true, cost!(Silicon: 250, Beryllium: 200), &[crate::unit::Type::Elude]);
    "mech-fabricator" => UnitFactory::new(3, true, cost!(Silicon: 200, Graphite: 300, Tungsten: 60), &[crate::unit::Type::Merui]);
    "tank-refabricator" => ConstructorBlock::new(3, true, cost!(Beryllium: 200, Tungsten: 80, Silicon: 100));
    "mech-refabricator" => ConstructorBlock::new(3, true, cost!(Beryllium: 250, Tungsten: 120, Silicon: 150));
    "ship-refabricator" => ConstructorBlock::new(3, true, cost!(Beryllium: 200, Tungsten: 100, Silicon: 150, Oxide: 40));
    "prime-refabricator" => ConstructorBlock::new(5, true, cost!(Thorium: 250, Oxide: 200, Tungsten: 200, Silicon: 400));
    "tank-assembler" => AssemblerBlock::new(5, true, cost!(Thorium: 500, Oxide: 150, Carbide: 80, Silicon: 500));
    "ship-assembler" => AssemblerBlock::new(5, true, cost!(Carbide: 100, Oxide: 200, Tungsten: 500, Silicon: 800, Thorium: 400));
    "mech-assembler" => AssemblerBlock::new(5, true, cost!(Carbide: 200, Thorium: 600, Oxide: 200, Tungsten: 500, Silicon: 900)); // smh collaris
    "basic-assembler-module" => AssemblerModule::new(5, true, cost!(Carbide: 300, Thorium: 500, Oxide: 200, PhaseFabric: 400)); // the dummy block
    "unit-repair-tower" -> BasicBlock::new(2, true, cost!(Graphite: 90, Silicon: 90, Tungsten: 80));
    "launch-pad" -> BasicBlock::new(3, true, cost!(Copper: 350, Lead: 200, Titanium: 150, Silicon: 140));
    "interplanetary-accelerator" -> BasicBlock::new(7, true, cost!(Copper: 16000, Silicon: 11000, Thorium: 13000, Titanium: 12000, SurgeAlloy: 6000, PhaseFabric: 5000));
    "mechanical-drill" -> DrillBlock::new(2, true, cost!(Copper: 12));
    "pneumatic-drill" -> DrillBlock::new(2, true, cost!(Copper: 18, Graphite: 10));
    "laser-drill" -> DrillBlock::new(3, true, cost!(Copper: 35, Graphite: 30, Titanium: 20, Silicon: 30));
    "blast-drill" -> DrillBlock::new(4, true, cost!(Copper: 65, Titanium: 50, Thorium: 75, Silicon: 60));
    "water-extractor" -> BasicBlock::new(2, true, cost!(Copper: 30, Lead: 30, Metaglass: 30, Graphite: 30));
    "oil-extractor" -> BasicBlock::new(3, true, cost!(Copper: 150, Lead: 115, Graphite: 175, Thorium: 115, Silicon: 75));
    "vent-condenser" -> ProductionBlock::new(3, true, cost!(Graphite: 20, Beryllium: 60));
    "cliff-crusher" => WallDrillBlock::new(2, false, cost!(Beryllium: 100, Graphite: 40));
    "plasma-bore" => DrillBlock::new(2, false, cost!(Beryllium: 40));
    "large-plasma-bore" => DrillBlock::new(3, false, cost!(Silicon: 100, Oxide: 25, Beryllium: 100, Tungsten: 70));
    "impact-drill" -> DrillBlock::new(4, true, cost!(Silicon: 70, Beryllium: 90, Graphite: 60));
    "eruption-drill" -> DrillBlock::new(5, true, cost!(Silicon: 200, Oxide: 20, Tungsten: 200, Thorium: 120));
    "reinforced-message" -> MessageLogic::new(1, true, cost!(Graphite: 10, Beryllium: 5));
    "message" -> MessageLogic::new(1, true, cost!(Copper: 5, Graphite: 5));
    "switch" => SwitchLogic::new(1, true, cost!(Copper: 5, Graphite: 5));
    "micro-processor" -> ProcessorLogic::new(1, true, cost!(Copper: 90, Lead: 50, Silicon: 50));
    "logic-processor" -> ProcessorLogic::new(2, true, cost!(Lead: 320, Graphite: 60, Thorium: 50, Silicon: 80));
    "hyper-processor" -> ProcessorLogic::new(3, true, cost!(Lead: 450, Thorium: 75, Silicon: 150, SurgeAlloy: 50));
    "memory-cell" -> MemoryBlock::new(1, true, cost!(Copper: 30, Graphite: 30, Silicon: 30));
    "memory-bank" -> MemoryBlock::new(2, true, cost!(Copper: 30, Graphite: 80, Silicon: 80, PhaseFabric: 30));
    "logic-display" -> BasicBlock::new(3, true, cost!(Lead: 100, Metaglass: 50, Silicon: 50));
    "large-logic-display" -> BasicBlock::new(6, true, cost!(Lead: 200, Metaglass: 100, Silicon: 150, PhaseFabric: 75));
    "canvas" => CanvasBlock::new(2, true, cost!(Silicon: 30, Beryllium: 10), 12);
    "illuminator" -> LampBlock::new(1, true, cost!(Lead: 8, Graphite: 12, Silicon: 8));
    "power-node" -> ConnectorBlock::new(1, true, cost!(Copper: 1, Lead: 3), 10);
    "power-node-large" -> ConnectorBlock::new(2, true, cost!(Lead: 10, Titanium: 5, Silicon: 3), 15);
    "surge-tower" -> ConnectorBlock::new(2, true, cost!(Lead: 10, Titanium: 7, Silicon: 15, SurgeAlloy: 15), 2);
    "diode" => DiodeBlock::new(1, false, cost!(Metaglass: 10, Silicon: 10, Plastanium: 5));
    "battery" -> BasicBlock::new(1, true, cost!(Copper: 5, Lead: 20));
    "battery-large" -> BasicBlock::new(3, true, cost!(Lead: 50, Titanium: 20, Silicon: 30));
    "combustion-generator" -> GeneratorBlock::new(1, true, cost!(Copper: 25, Lead: 15));
    "thermal-generator" -> GeneratorBlock::new(2, true, cost!(Copper: 40, Lead: 50, Metaglass: 40, Graphite: 35, Silicon: 35));
    "steam-generator" -> GeneratorBlock::new(2, true, cost!(Copper: 35, Lead: 40, Graphite: 25, Silicon: 30));
    "differential-generator" -> GeneratorBlock::new(3, true, cost!(Copper: 70, Lead: 100, Metaglass: 50, Titanium: 50, Silicon: 65));
    "rtg-generator" -> GeneratorBlock::new(2, true, cost!(Lead: 100, Thorium: 50, Silicon: 75, Plastanium: 75, PhaseFabric: 25));
    "solar-panel" -> GeneratorBlock::new(1, true, cost!(Lead: 10, Silicon: 15));
    "solar-panel-large" -> GeneratorBlock::new(3, true, cost!(Lead: 80, Silicon: 110, PhaseFabric: 15));
    "thorium-reactor" -> NuclearGeneratorBlock::new(3, true, cost!(Lead: 300, Metaglass: 50, Graphite: 150, Thorium: 150, Silicon: 200));
    "impact-reactor" -> ImpactReactorBlock::new(4, true, cost!(Lead: 500, Metaglass: 250, Graphite: 400, Thorium: 100, Silicon: 300, SurgeAlloy: 250));
    "beam-node" -> ConnectorBlock::new(1, true, cost!(Beryllium: 8), 4);
    "beam-tower" -> ConnectorBlock::new(3, true, cost!(Beryllium: 30, Oxide: 10, Silicon: 10), 12);
    "turbine-condenser" -> GeneratorBlock::new(3, true, cost!(Beryllium: 60));
    "chemical-combustion-chamber" -> GeneratorBlock::new(3, true, cost!(Graphite: 40, Tungsten: 40, Oxide: 40, Silicon: 30));
    "pyrolysis-generator" -> GeneratorBlock::new(3, true, cost!(Graphite: 50, Carbide: 50, Oxide: 60, Silicon: 50));
    "flux-reactor" -> GeneratorBlock::new(5, true, cost!(Graphite: 300, Carbide: 200, Oxide: 100, Silicon: 600, SurgeAlloy: 300));
    "neoplasia-reactor" => Neoplasia::new(5, true, cost!(Tungsten: 1000, Carbide: 300, Oxide: 150, Silicon: 500, PhaseFabric: 300, SurgeAlloy: 200));
    "core-shard" -> BasicBlock::new(3, true, cost!(Copper: 1000, Lead: 800));
    "core-foundation" -> BasicBlock::new(4, true, cost!(Copper: 3000, Lead: 3000, Silicon: 2000));
    "core-nucleus" -> BasicBlock::new(5, true, cost!(Copper: 8000, Lead: 8000, Thorium: 4000, Silicon: 5000));
    "core-bastion" -> BasicBlock::new(4, true, cost!(Graphite: 1000, Silicon: 1000, Beryllium: 800));
    "core-citadel" -> BasicBlock::new(5, true, cost!(Silicon: 4000, Beryllium: 4000, Tungsten: 3000, Oxide: 1000));
    "core-acropolis" -> BasicBlock::new(6, true, cost!(Beryllium: 6000, Silicon: 5000, Tungsten: 5000, Carbide: 3000, Oxide: 3000));
    "container" -> BasicBlock::new(2, true, cost!(Titanium: 100));
    "vault" -> BasicBlock::new(3, true, cost!(Titanium: 250, Thorium: 125));
    "reinforced-container" -> BasicBlock::new(2, true, cost!(Tungsten: 30, Graphite: 40));
    "reinforced-vault" -> BasicBlock::new(3, true, cost!(Tungsten: 125, Thorium: 70, Beryllium: 100));
    "duo" -> ItemTurret::new(1, true, cost!(Copper: 35));
    "scatter" -> ItemTurret::new(2, true, cost!(Copper: 85, Lead: 45));
    "scorch" -> ItemTurret::new(1, true, cost!(Copper: 25, Graphite: 22));
    "hail" -> ItemTurret::new(1, true, cost!(Copper: 40, Graphite: 17));
    "wave" -> Turret::new(2, true, cost!(Copper: 25, Lead: 75, Metaglass: 45));
    "tsunami" -> Turret::new(3, true, cost!(Lead: 400, Metaglass: 100, Titanium: 250, Thorium: 100));
    "lancer" -> Turret::new(2, true, cost!(Copper: 60, Lead: 70, Titanium: 30, Silicon: 60));
    "arc" -> Turret::new(1, true, cost!(Copper: 50, Lead: 50));
    "parallax" -> TractorBeamTurret::new(2, true, cost!(Graphite: 30, Titanium: 90, Silicon: 120));
    "swarmer" -> ItemTurret::new(2, true, cost!(Graphite: 35, Titanium: 35, Silicon: 30, Plastanium: 45));
    "salvo" -> ItemTurret::new(2, true, cost!(Copper: 100, Graphite: 80, Titanium: 50));
    "segment" -> PointDefenseTurret::new(2, true, cost!(Titanium: 40, Thorium: 80, Silicon: 130, PhaseFabric: 40));
    "fuse" -> ItemTurret::new(3, true, cost!(Copper: 225, Graphite: 225, Thorium: 100));
    "ripple" -> ItemTurret::new(3, true, cost!(Copper: 150, Graphite: 135, Titanium: 60));
    "cyclone" -> ItemTurret::new(3, true, cost!(Copper: 200, Titanium: 125, Plastanium: 80));
    "foreshadow" -> ItemTurret::new(4, true, cost!(Copper: 1000, Metaglass: 600, Silicon: 600, Plastanium: 200, SurgeAlloy: 300));
    "spectre" -> ItemTurret::new(4, true, cost!(Copper: 900, Graphite: 300, Thorium: 250, Plastanium: 175, SurgeAlloy: 250));
    "meltdown" -> Turret::new(4, true, cost!(Copper: 1200, Lead: 350, Graphite: 300, Silicon: 325, SurgeAlloy: 325));
    "breach" -> ItemTurret::new(3, true, cost!(Beryllium: 150, Silicon: 150, Graphite: 250));
    "diffuse" -> ItemTurret::new(3, true, cost!(Beryllium: 150, Silicon: 200, Graphite: 200, Tungsten: 50));
    "sublimate" -> ContinousTurret::new(3, true, cost!(Tungsten: 150, Silicon: 200, Oxide: 40, Beryllium: 400));
    "titan" -> ItemTurret::new(4, true, cost!(Tungsten: 250, Silicon: 300, Thorium: 400));
    "disperse" -> ItemTurret::new(4, true, cost!(Thorium: 50, Oxide: 150, Silicon: 200, Beryllium: 350));
    "afflict" -> Turret::new(4, true, cost!(SurgeAlloy: 100, Silicon: 200, Graphite: 250, Oxide: 40));
    "lustre" -> ContinousTurret::new(4, true, cost!(Silicon: 250, Graphite: 200, Oxide: 50, Carbide: 90));
    "scathe" -> ItemTurret::new(4, true, cost!(Oxide: 200, SurgeAlloy: 400, Silicon: 800, Carbide: 500, PhaseFabric: 300));
    "malign" -> Turret::new(5, true, cost!(Carbide: 400, Beryllium: 2000, Silicon: 800, Graphite: 800, PhaseFabric: 300));
    "smite" -> ItemTurret::new(5, true, cost!(Oxide: 200, SurgeAlloy: 400, Silicon: 800, Carbide: 500, PhaseFabric: 300));
    // sandbox only
    "beam-link" -> ConnectorBlock::new(3, true, &[], 12);
    "power-source" -> ConnectorBlock::new(1, true, &[], 100);
    "power-void" -> BasicBlock::new(1, true, &[]);
    "world-processor" -> BasicBlock::new(1, true, &[]);
    "world-message" -> MessageLogic::new(1, true, &[]);
    "world-cell" -> MemoryBlock::new(1, true, &[]);
    "liquid-source" => FluidBlock::new(1, true, &[]);
    "liquid-void" -> BasicBlock::new(1, true, &[]);
    "shield-projector" -> ShieldBlock::new(3, true, &[]);
    "large-shield-projector" -> ShieldBlock::new(4, true, &[]);
    "payload-source" => PayloadBlock::new(5, false, &[]);
    "payload-void" => SimplePayloadBlock::new(5, true, &[]);
    "item-source" => ItemBlock::new(1, true, &[]);
    "item-void" -> BasicBlock::new(1, true, &[]);
    "heat-source" => HeatCrafter::new(1, false, &[]);
}
