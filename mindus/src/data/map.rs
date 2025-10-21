//! the map module
//! ### format
//! note: utf = `len<u16>` + `utf8(read(len))`
//!
//! note: each section has a `u32` denoting its length
//!
//! key: `: T` and `x<T>` both mean read T, `iterate T` means iterate `read_T()` times
//!
//! ZLIB compressed stream contains:
//! - header: 4b = `MSCH` [`MapReader::header`]
//! - version: `u32` (should be 7+) [`MapReader::version`]
//! - tag section `<u32>` [`MapReader::tags`]
//!     - 1 byte of idk (skip)
//!     - string map (`u16` for map len, iterate each, read `utf`)
//! - content header section `<u32>`: [`MapReader::content`] (note: if map v8, will use [`BlockEnum`] and skip reading this)
//!     - iterate `i8` (should = `10`)'
//!         - the type: `i8` (0: item, block: 1, liquid: 4, status: 5, unit: 6, weather: 7, sector: 9, planet: 13
//!         - item count: `u16` (item: 22, block: 422, liquid: 11, status: 21, unit: 66, weather: 6, sector: 35, planet: 7)
//!         - these types all have their own modules: [`item`], [`content`], [`fluid`], [`modifier`], [`mod@unit`], [`weather`], [`sector`], [`planet`]
//!         - iterate `u16`
//!             - name: `utf`
//! - map section `<u32>` [`MapReader::map`]
//!     - width: `u16`, height: `u16`
//!     - floor and tiles:
//!         - for `i` in `w * h`
//!             - `x = i % w`, `y = i / w`
//!             - floor id: `u16`
//!             - overlay id: `u16`
//!             - consecutives: `u8`
//!             - iterate `(i + 1)..(i + 1 + consecutives)`
//!                 - `x = j % w`, `y = j / w`
//!             - i += consecutives
//!     - blocks
//!         - for `i` in `w * h`
//!             - block id: `u16`
//!             - packed?: `i8`
//!             - entity = `(packed & 1) not 0`
//!             - data = `(packed & 2) not 0`
//!             - newdata = `(packed & 3) not 0`
//!             - if entity: central: `bool`
//!             - if entity:
//!                 - if central:
//!                     - chunk len: `u16`
//!                     - if block == building:
//!                         - revision: `i8`
//!                         - [`Build::read`]
//!                     - else skip `chunk len`
//!                 - or data
//!                     - data: `i8`
//!                 - or new data
//!                     - data: `i8`
//!                     - floor: `i8`
//!                     - overlay: `i8`
//!                     - extra: `i32`
//!                 - else
//!                     - consecutives: `u8`
//!                     - iterate `(i + 1)..(i + 1 + consecutives)`
//!                         - same block
//!                     - i += consecutives
//! - entities section `<u32>` [`MapReader::entities`]
//!     - entity mapping
//!         - iterate `u16`
//!             - id: `i16`, name: `utf`
//!     - team build plans
//!         - for t in `teams<u32>`
//!             - team = `team#<u32>`
//!             - iterate `plans<u32>`
//!                 - x: `u16`, y: `u16`, rot: `u16`, id: `u16`
//!                 - o: `DynData` (refer to [`DynSerializer`])
//!         - world entities
//!             - iterate `u32`
//!                 - len: `u16`
//!                 - type: `u8`
//!                 - if !mapping\[type\]
//!                     - skip(len - 1)
//!                     - continue
//!                 - id: `u32`
//!                 - entity read
//! - markers section (v8)
use fimg::DynImage;
use std::collections::HashMap;
use std::ops::CoroutineState::*;
use std::ops::{Coroutine, Index, IndexMut};
use std::pin::Pin;
use std::sync::atomic::{AtomicU64, Ordering};
use thiserror::Error;

use crate::block::content::Type as BlockEnum;
use crate::block::{Block, Rotation, State};
use crate::data::DataRead;
use crate::data::dynamic::DynData;
use crate::data::renderer::*;
use crate::fluid::Type as Fluid;
use crate::item::{Type as Item, storage::Storage};
use crate::team::Team;
use crate::unit::Unit;
#[cfg(doc)]
use crate::{block::content, data::*, fluid, item, modifier, unit};

use super::{Serializable, entity_mapping};
use crate::content::Content;

/// a tile in a map
#[derive(Clone)]
pub struct Tile {
    pub floor: BlockEnum,
    pub ore: BlockEnum,
    build: Option<Build>,
    pub nd: [u8; 7],
}

pub fn table(x: BlockEnum, scale: Scale) -> DynImage<&'static [u8]> {
    FLOOR_TABLE[x as usize][mcg() as usize][scale as usize]
}

pub fn mcg() -> u64 {
    static STATE: AtomicU64 = AtomicU64::new(25);
    let s = STATE.load(Ordering::Relaxed);
    // a * s % m
    let s = (s.wrapping_mul(0xecc5)) % (1 << 32);
    STATE.store(s, Ordering::Relaxed);
    (s >> 13) & (0b111)
}

static FLOOR_TABLE: [[[DynImage<&'static [u8]>; 3]; 8]; BlockEnum::ALL.len()] = {
    let mut table = [[car::map!(load!("empty"), DynImage::from); 8]; BlockEnum::ALL.len()];
    macro_rules! image {
        (| $($x:literal $(|)?)+) => { paste::paste! {
            $(table[BlockEnum::[<$x:camel>] as usize] = load!(8x $x));+
        } };
        (| single $($x:literal $(|)?)+) => { paste::paste! {
            $(table[BlockEnum::[<$x:camel>] as usize] =
                car::map!([load!($x); 8], |x| car::map!(x, DynImage::from)));+
        } }
    }

    image! [
        | single "colored-floor" | "colored-wall" | "metal-tiles-1" | "metal-tiles-2" | "metal-tiles-3" | "metal-tiles-4" | "metal-tiles-5" | "metal-tiles-6" | "metal-tiles-7" | "metal-tiles-8" | "metal-tiles-9" | "metal-tiles-10" | "metal-tiles-11" | "metal-tiles-12"
        | "metal-floor" | "metal-floor-2" | "metal-floor-3" | "metal-floor-4" | "metal-floor-5"
        | "dark-panel-1" | "dark-panel-2" | "dark-panel-3" | "dark-panel-4" | "dark-panel-5" | "dark-panel-6"
        | "arkycite-floor"
        | "darksand-tainted-water" | "darksand-water" | "deep-tainted-water" | "deep-water" | "sand-water" | "shallow-water" | "tainted-water"
        | "tar" | "pooled-cryofluid"
        | "space"
        | "stone-vent"
        | "core-zone"
        | "spawn"
    ];
    image! {
        | "ore-copper" | "ore-beryllium" | "ore-lead" | "ore-scrap" | "ore-coal" | "ore-thorium" | "ore-titanium" | "ore-tungsten" | "pebbles" | "tendrils" | "ore-wall-tungsten" | "ore-wall-beryllium" | "ore-wall-thorium" | "ore-crystal-thorium"
        | "yellow-stone-vent" | "arkyic-vent" | "crystalline-vent"  | "red-stone-vent" | "carbon-vent"
        | "darksand"
        | "sand-floor"
        | "dacite"
        | "dirt"
        | "basalt" | "basalt-vent"
        | "moss"
        | "mud"
        | "ice-snow" | "snow" | "salt" | "ice"
        | "hotrock" | "char" | "magmarock" | "molten-slag"
        | "shale"
        | "metal-floor-damaged"
        | "stone"
        | "bluemat"
        | "ferric-craters"
        | "beryllic-stone"
        | "grass"
        | "rhyolite" | "rough-rhyolite" | "rhyolite-crater" | "rhyolite-vent"
        | "crater-stone"
        | "redmat"
        | "red-ice"
        | "spore-moss"
        | "regolith"
        | "ferric-stone"
        | "arkyic-stone"
        | "yellow-stone" | "yellow-stone-plates"
        | "red-stone" | "dense-red-stone"
        | "carbon-stone"
        | "crystal-floor" | "crystalline-stone"
    };
    table
};

impl Tile {
    #[must_use]
    pub const fn new(floor: BlockEnum, ore: BlockEnum) -> Self {
        Self {
            floor,
            ore,
            build: None,
            nd: [0; 7],
        }
    }

    fn set_block(&mut self, block: &'static Block) {
        self.build = Some(Build {
            block,
            state: None,
            items: Storage::new(),
            liquids: Storage::new(),
            rotation: Rotation::Up,
            team: Team::SHARDED,
            data: 0,
        });
    }

    #[must_use]
    pub const fn build(&self) -> Option<&Build> {
        self.build.as_ref()
    }

    /// size of this tile
    ///
    /// ._.
    ///
    /// dont think about it too much
    #[must_use]
    #[inline]
    pub fn size(&self) -> u8 {
        self.build.as_ref().map_or(1, |v| v.block.get_size())
    }

    #[must_use]
    #[inline]
    pub fn has_ore(&self) -> bool {
        self.ore != BlockEnum::Air
    }

    /// Draw this tiles build.
    #[must_use]
    #[inline]
    pub fn build_image(
        &self,
        context: Option<&RenderingContext>,
        s: Scale,
    ) -> DynImage<image::Cow> {
        // building covers floore
        let Some(b) = &self.build else {
            unreachable!();
        };
        b.image(context, s)
    }
}

impl std::fmt::Debug for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Tile@{}{}{}",
            self.floor.get_name(),
            if self.ore != BlockEnum::Air {
                format!("+{}", self.ore.get_name())
            } else {
                String::new()
            },
            if let Some(build) = &self.build {
                format!(":{}", build.block.name())
            } else {
                String::new()
            }
        )
    }
}

impl BlockState for Tile {
    fn get_block(&self) -> Option<&'static Block> {
        Some(self.build()?.block)
    }
}

impl RotationState for Tile {
    fn get_rotation(&self) -> Option<Rotation> {
        Some(self.build()?.rotation)
    }
}

impl RotationState for Option<Tile> {
    fn get_rotation(&self) -> Option<Rotation> {
        self.as_ref().unwrap().get_rotation()
    }
}

impl BlockState for Option<Tile> {
    fn get_block(&self) -> Option<&'static Block> {
        self.as_ref().unwrap().get_block()
    }
}

/// a build on a tile in a map
#[derive(Clone)]
pub struct Build {
    pub block: &'static Block,
    pub items: Storage<Item>,
    pub liquids: Storage<Fluid>,
    pub state: Option<State>,
    // pub health: f32,
    pub rotation: Rotation,
    pub team: Team,
    pub data: i8,
}

impl std::fmt::Debug for Build {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Build<{block}>", block = self.block.name(),)
    }
}

impl Build {
    #[must_use]
    pub fn new(block: &'static Block) -> Build {
        Self {
            block,
            items: Storage::default(),
            liquids: Storage::default(),
            state: None,
            rotation: Rotation::Up,
            team: Team::SHARDED,
            data: 0,
        }
    }

    fn image(&self, context: Option<&RenderingContext>, s: Scale) -> DynImage<image::Cow> {
        self.block
            .image(self.state.as_ref(), context, self.rotation, s)
    }

    #[must_use]
    pub const fn name(&self) -> &str {
        self.block.name()
    }

    pub fn read(&mut self, buff: &mut DataRead<'_>) -> Result<(), ReadError> {
        // health
        let _ = buff.read_f32()?;
        let rot = buff.read_i8()? as i16;
        // team
        let _ = buff.read_i8()?;
        self.rotation = Rotation::try_from((rot & 127) as u8).unwrap_or(Rotation::Up);
        let mut mask = 0;
        let mut version = 0;
        if rot & 128 != 0 {
            version = buff.read_u8()?;
            if version < 3 {
                return Err(ReadError::Version(version));
            }
            buff.skip(1)?;
            mask = buff.read_u8()?;
        }

        if mask & 1 != 0 {
            read_items(buff, &mut self.items)?;
        }
        if mask & 2 != 0 {
            read_power(buff)?;
        }
        if mask & 4 != 0 {
            read_liquids(buff, &mut self.liquids)?;
        }
        // "efficiency"
        _ = buff.read_u8()? as f64 / 255.;
        _ = buff.read_u8()? as f64 / 255.;

        if version == 4 {
            // visible flags for fog
            _ = buff.read_u64()?;
        }
        // "overridden by subclasses"
        self.block.read(self, buff)?;

        Ok(())
    }
}

/// format:
/// - iterate [`u16`]
///     - item: [`u16`] as [`Item`]
///     - amount: [`u32`]
///
fn read_items(from: &mut DataRead, to: &mut Storage<Item>) -> Result<(), ReadError> {
    to.clear();
    let n = from.read_u16()?;
    to.reserve(n as usize);
    for _ in 0..n {
        let item = from.read_u16()?;
        let amount = from.read_u32()?;
        if let Ok(item) = Item::try_from(item) {
            to.set(item, amount);
        }
    }
    Ok(())
}

/// format:
/// - iterate [`u16`]
///     - liquid: [`u16`] as [`Fluid`]
///     - amount: [`f32`]
fn read_liquids(from: &mut DataRead, to: &mut Storage<Fluid>) -> Result<(), ReadError> {
    to.clear();
    let n = from.read_u16()?;
    to.reserve(n as usize);
    for _ in 0..n {
        let fluid = from.read_u16()?;
        let amount = from.read_f32()?;
        if let Ok(fluid) = Fluid::try_from(fluid) {
            to.set(fluid, (amount * 100.0) as u32);
        }
    }
    Ok(())
}

/// format:
/// - iterate [`u16`]
///     - link: [`i32`]
/// - status: [`f32`]
fn read_power(from: &mut DataRead) -> Result<(), ReadError> {
    let n = from.read_u16()? as usize;
    from.skip((n + 1) * 4)?;
    Ok(())
}

#[test]
fn test_read_items() {
    let mut s = Storage::new();
    read_items(
        &mut DataRead::new(&[
            0, 6, 0, 0, 0, 0, 2, 187, 0, 1, 0, 0, 1, 154, 0, 2, 0, 0, 15, 160, 0, 3, 0, 0, 0, 235,
            0, 6, 0, 0, 1, 46, 0, 12, 0, 0, 1, 81, 255, 255,
        ]),
        &mut s,
    )
    .unwrap();
    assert!(s.get_total() == 5983);
}

#[test]
fn test_read_liquids() {
    let mut s = Storage::new();
    read_liquids(
        &mut DataRead::new(&[0, 1, 0, 0, 67, 111, 247, 126, 255, 255]),
        &mut s,
    )
    .unwrap();
    assert!(s.get(Fluid::Water) == 23996);
}

/// a map.
/// ## Does not support serialization yet!
#[derive(Debug)]
pub struct Map {
    pub width: usize,
    pub height: usize,
    pub tags: HashMap<String, String>,
    pub entities: Vec<Unit>,
    /// row major 2d array
    /// ```rs
    /// (0, 0), (1, 0), (2, 0)
    /// (0, 1), (1, 1), (2, 1)
    /// (0, 2), (1, 2), (2, 2)
    /// ```
    pub tiles: Vec<Tile>,
}

impl Map {
    #[lower::apply(wrapping)]
    pub(crate) fn cross(&self, j: usize) -> [Option<&Tile>; 4] {
        [
            self.tiles.get(j + self.width),
            self.tiles.get(j + 1),
            self.tiles.get(j - self.width),
            self.tiles.get(j - 1),
        ]
    }
    #[lower::apply(wrapping)]
    pub(crate) fn corners(&self, j: usize) -> [Option<&Tile>; 4] {
        [
            self.tiles.get(j - self.width - 1),
            self.tiles.get(j - self.width + 1),
            self.tiles.get(j + self.width - 1),
            self.tiles.get(j + self.width + 1),
        ]
    }

    #[must_use]
    pub fn new(width: usize, height: usize, tags: HashMap<String, String>) -> Self {
        Self {
            tiles: Vec::with_capacity(width * height),
            height,
            width,
            tags,
            entities: vec![],
        }
    }

    fn push(&mut self, t: Tile) {
        self.tiles.push(t);
    }
}

impl Index<usize> for Map {
    type Output = Tile;
    fn index(&self, index: usize) -> &Self::Output {
        &self.tiles[index]
    }
}

impl IndexMut<usize> for Map {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.tiles[index]
    }
}

const MAP_HEADER: [u8; 4] = [b'M', b'S', b'A', b'V'];

/// error occurring when reading a map fails
#[derive(Debug, Error)]
pub enum ReadError {
    #[error("failed to read from buffer")]
    Read(#[from] super::ReadError),
    #[error(transparent)]
    Decompress(#[from] super::DecompressError),
    #[error("incorrect header ({0:?})")]
    Header([u8; 4]),
    #[error("unsupported version ({0})")]
    Version(u8),
    #[error("unknown block at index {0:?}")]
    NoSuchBlock(u16),
    #[error("no block {0}")]
    NoBlockFound(String),
    #[error("failed to read block data")]
    ReadState(#[from] super::dynamic::ReadError),
    #[error("no block, but data")]
    NoBlockWithData,
}

/// Struct for granular map deserialization.
pub struct MapReader {
    #[allow(dead_code)]
    backing: Vec<u8>,
    // dataread references 'backing'
    buff: DataRead<'static>,
}

#[derive(Debug)]
pub enum ThinBloc {
    Build(Rotation, &'static Block, Team),
    Many(Option<&'static Block>, u8),
}

#[derive(Debug)]
pub enum ThinMapData {
    Init { width: u16, height: u16 },
    Bloc(ThinBloc),
    Tile { floor: BlockEnum, ore: BlockEnum },
}

#[derive(Debug)]
pub enum Bloc {
    Build(Build, &'static Block),
    Data(&'static Block, i8),
    Nd(Option<&'static Block>, [u8; 7]),
    Many(Option<&'static Block>, u8),
}

#[derive(Debug)]
pub enum MapData {
    Init { width: u16, height: u16 },
    Bloc(Bloc),
    Tile { floor: BlockEnum, ore: BlockEnum },
}

#[derive(Debug)]
pub enum EntityData {
    Length(u32),
    Data(Unit),
}

macro_rules! tiles {
    ($count:ident, $me:ident, $w: ident,$r:ident) => {
        let mut i = 0;
        while i < $count {
            let floor_id = $me.buff.read_u16()?;
            let overlay_id = $me.buff.read_u16()?;
            let &floor = $r.get(floor_id as usize).unwrap_or(&BlockEnum::Stone);
            let &ore = $r.get(overlay_id as usize).unwrap_or(&BlockEnum::Air);
            yield $w::Tile { floor, ore };
            let consecutives = $me.buff.read_u8()? as usize;
            for _ in 0..consecutives {
                yield $w::Tile { floor, ore };
            }
            i += consecutives;
            i += 1;
        }
    };
}

pub type Registrar = [BlockEnum; BlockEnum::ALL.len()];
impl MapReader {
    pub fn new(buff: &mut DataRead<'_>) -> Result<Self, ReadError> {
        let backing = buff.deflate()?;
        Ok(Self {
            buff: DataRead::new(unsafe {
                std::mem::transmute::<&'_ [u8], &'static [u8]>(&backing)
            }),
            backing,
        })
    }

    pub fn header(&mut self) -> Result<[u8; 4], ReadError> {
        let b = self.buff.readN::<4>()?;
        (b == MAP_HEADER).then_some(b).ok_or(ReadError::Header(b))
    }

    pub fn version(&mut self) -> Result<u32, ReadError> {
        let x = self.buff.read_u32()?;
        (7..=9)
            .contains(&x)
            .then_some(x)
            .ok_or(ReadError::Version(x.try_into().unwrap_or(0)))
    }

    pub fn tags(&mut self) -> Result<HashMap<&str, &str>, ReadError> {
        let mut tags = HashMap::new();
        self.buff.skip(5)?;
        for _ in 0..self.buff.read_u8()? {
            let key = self.buff.read_utf()?;
            let value = self.buff.read_utf()?;
            tags.insert(key, value);
        }
        Ok(tags)
    }

    pub fn content(&mut self) -> Result<Registrar, ReadError> {
        let mut registrar = BlockEnum::ALL;
        _ = self.buff.read_u32()?;
        for _ in 0..self.buff.read_i8()? {
            let ty = self.buff.read_u8()?;
            for index in 0..self.buff.read_u16()? as usize {
                if ty == 1 {
                    let name = self.buff.read_utf()?;
                    let block = BlockEnum::by_name(name)
                        .ok_or_else(|| ReadError::NoBlockFound(name.to_string()))?;
                    registrar.get_mut(index).map(|x| *x = block);
                } else {
                    let n = self.buff.read_u16()?;
                    self.buff.skip(n as usize)?;
                }
            }
        }
        Ok(registrar)
    }

    pub fn tags_alloc(&mut self) -> Result<HashMap<String, String>, ReadError> {
        let mut tags = HashMap::new();
        self.buff
            .read_chunk(true, |buff| {
                buff.skip(1)?;
                for _ in 0..buff.read_u8()? {
                    let key = buff.read_utf()?;
                    let value = buff.read_utf()?;
                    tags.insert(key.to_owned(), value.to_owned());
                }
                Ok::<(), ReadError>(())
            })
            .map(|_| tags)
    }

    pub fn skip(&mut self) -> Result<(), ReadError> {
        let len = self.buff.read_u32()? as usize;
        self.buff.skip(len)?;
        Ok(())
    }

    pub fn thin_map(
        &mut self,
        r: Registrar,
    ) -> Result<
        impl Coroutine<(), Return = Result<(), ReadError>, Yield = ThinMapData> + '_,
        ReadError,
    > {
        let len = self.buff.read_u32()? as usize;
        let rb4 = self.buff.read;
        let map = #[coroutine]
        move || {
            let w = self.buff.read_u16()?;
            let h = self.buff.read_u16()?;
            yield ThinMapData::Init {
                width: w,
                height: h,
            };
            let w = w as usize;
            let h = h as usize;
            let count = w * h;
            tiles!(count, self, ThinMapData, r);

            let mut i = 0;
            while i < count {
                let block_id = self.buff.read_u16()?;
                let packed = self.buff.read_u8()?;
                let entity = (packed & 1) != 0;
                let data = (packed & 2) != 0;
                let newdata = (packed & 4) != 0;
                if newdata {
                    self.buff.skip(7)?;
                }
                let central = if entity {
                    self.buff.read_bool()?
                } else {
                    false
                };
                let block = *r
                    .get(block_id as usize)
                    .ok_or(ReadError::NoSuchBlock(block_id))?;
                let block = block.to_block();
                yield if entity {
                    if central {
                        let len = self.buff.read_u16()? as usize;
                        let rb4 = self.buff.read;

                        #[cfg(debug_assertions)]
                        println!("reading {block:?} ");
                        let _ = self.buff.read_i8()?;
                        let _ = self.buff.read_f32()?;
                        let rot = self.buff.read_i8()?;
                        let rot = Rotation::try_from((rot & 127) as u8).unwrap_or(Rotation::Up);
                        let team = Team::of(self.buff.read_u8()?);
                        let read = self.buff.read - rb4;
                        let n = len - read;
                        self.buff.skip(n)?;

                        ThinMapData::Bloc(ThinBloc::Build(rot, block.unwrap(), team))
                    } else {
                        ThinMapData::Bloc(ThinBloc::Many(None, 0))
                    }
                } else if data {
                    _ = self.buff.read_i8()?;
                    ThinMapData::Bloc(ThinBloc::Many(block, 0))
                } else if newdata {
                    ThinMapData::Bloc(ThinBloc::Many(block, 0))
                } else {
                    let consecutives = self.buff.read_u8()?;
                    i += consecutives as usize;
                    ThinMapData::Bloc(ThinBloc::Many(block, consecutives))
                };

                i += 1;
            }
            let read = self.buff.read - rb4;
            debug_assert!(len >= read, "overread; supposed to read {len}; read {read}");
            debug_assert!((len - read) == 0, "supposed to read {len}; read {read}");
            Ok(())
        };
        Ok(map)
    }

    pub fn collect_map(
        &mut self,
        tags: HashMap<String, String>,
        r: Registrar,
    ) -> Result<Map, ReadError> {
        let mut co = self.map(r)?;
        let (w, h) = match Pin::new(&mut co).resume(()) {
            Yielded(MapData::Init { width, height }) => (width as usize, height as usize),
            Complete(Err(x)) => return Err(x),
            _ => unreachable!(),
        };
        let mut m = Map::new(w, h, tags);
        for _ in 0..w * h {
            match Pin::new(&mut co).resume(()) {
                Yielded(MapData::Tile { floor, ore }) => m.push(Tile::new(floor, ore)),
                Complete(Err(x)) => return Err(x),
                _ => unreachable!(),
            }
        }
        let mut i = 0;
        while i < w * h {
            match Pin::new(&mut co).resume(()) {
                Yielded(MapData::Bloc(Bloc::Many(None, n))) => i += n as usize,
                Yielded(MapData::Bloc(Bloc::Build(x, y))) => {
                    m[i].set_block(y);
                    m[i].build = Some(x);
                }
                Yielded(MapData::Bloc(Bloc::Data(x, y))) => {
                    m[i].set_block(x);
                    m[i].build.as_mut().unwrap().data = y;
                }
                Yielded(MapData::Bloc(Bloc::Many(Some(bloc), n))) => {
                    for i in i..=i + n as usize {
                        m[i].set_block(bloc);
                    }
                    i += n as usize;
                }
                Yielded(MapData::Bloc(Bloc::Nd(b, d))) => {
                    b.map(|b| m[i].set_block(b));
                    m[i].nd = d
                }
                Complete(Err(x)) => return Err(x),
                _ => unreachable!(),
            }
            i += 1;
        }
        match Pin::new(&mut co).resume(()) {
            Complete(Ok(())) => (),
            _ => unreachable!(),
        };
        Ok(m)
    }

    pub fn map(
        &mut self,
        r: Registrar,
    ) -> Result<impl Coroutine<(), Return = Result<(), ReadError>, Yield = MapData> + '_, ReadError>
    {
        let len = self.buff.read_u32()? as usize;
        let rb4 = self.buff.read;

        let c = #[coroutine]
        move || {
            let w = self.buff.read_u16()?;
            let h = self.buff.read_u16()?;
            yield MapData::Init {
                width: w,
                height: h,
            };
            let w = w as usize;
            let h = h as usize;
            let count = w * h;
            tiles!(count, self, MapData, r);

            let mut i = 0;
            while i < count {
                let block_id = self.buff.read_u16()?;
                let packed = self.buff.read_u8()?;
                let entity = (packed & 1) != 0;
                let data = (packed & 2) != 0;
                let newdata = (packed & 4) != 0;
                let newdata = newdata.then(|| self.buff.readN::<7>()).transpose()?;
                let central = if entity {
                    self.buff.read_bool()?
                } else {
                    false
                };
                let block = r
                    .get(block_id as usize)
                    .ok_or(ReadError::NoSuchBlock(block_id))?;
                let block = block.to_block();
                yield if entity {
                    if central {
                        let block = block.ok_or(ReadError::NoBlockWithData)?;
                        let len = self.buff.read_u16()? as usize;
                        let rb4 = self.buff.read;

                        #[cfg(debug_assertions)]
                        println!("reading {block:?} ");
                        let _ = self.buff.read_i8()?;
                        let mut b = Build::new(block);
                        b.read(&mut self.buff)?;
                        // implementation not complete, skip remaining bytes (TODO finish impl)
                        let read = self.buff.read - rb4;

                        // skip this chunk
                        assert!(len >= read, "overread; supposed to read {len}; read {read}");
                        let n = len - read;
                        if n != 0 {
                            #[cfg(debug_assertions)]
                            println!(
                                "({block:?}) supposed to read {len}; read {read} - skipping excess"
                            );
                            self.buff.skip(n)?;
                        };

                        MapData::Bloc(Bloc::Build(b, block))
                    } else {
                        MapData::Bloc(Bloc::Many(None, 0))
                    }
                } else if data {
                    MapData::Bloc(Bloc::Data(
                        block.ok_or(ReadError::NoBlockWithData)?,
                        self.buff.read_i8()?,
                    ))
                } else if let Some(nd) = newdata {
                    MapData::Bloc(Bloc::Nd(block, nd))
                } else {
                    let consecutives = self.buff.read_u8()?;
                    i += consecutives as usize;
                    MapData::Bloc(Bloc::Many(block, consecutives))
                };

                i += 1;
            }
            let read = self.buff.read - rb4;
            debug_assert!(len >= read, "overread; supposed to read {len}; read {read}");
            debug_assert!((len - read) == 0, "supposed to read {len}; read {read}");
            Ok(())
        };
        Ok(c)
    }

    pub fn entities(
        &mut self,
    ) -> Result<
        impl Coroutine<(), Yield = EntityData, Return = Result<(), ReadError>> + '_,
        ReadError,
    > {
        let len = self.buff.read_u32()? as usize;
        let rb4 = self.buff.read;

        let c = #[coroutine]
        move || {
            for _ in 0..self.buff.read_u16()? {
                self.buff.skip(2)?;
                let _ = self.buff.read_utf()?;
            }
            // read team block plans (ghosts) (SaveVersion.java#389)
            for _ in 0..self.buff.read_u32()? {
                self.buff.skip(4)?;
                for _ in 0..self.buff.read_u32()? {
                    self.buff.skip(8usize)?;
                    let _ = DynData::deserialize(&mut self.buff)?;
                }
            }
            // read world entities (#412). eg units
            let n = self.buff.read_u32()?;
            yield EntityData::Length(n);
            for _ in 0..n {
                let len = self.buff.read_u16()? as usize;
                let rb4 = self.buff.read;
                let id = self.buff.read_u8()? as usize;
                let Some(&Some(u)) = entity_mapping::ID.get(id) else {
                    self.buff.skip(len - 1)?;
                    continue;
                };
                _ = self.buff.read_u32()?;
                yield EntityData::Data(u.read(&mut self.buff)?);
                let read = self.buff.read - rb4;
                debug_assert!(len >= read, "overread; supposed to read {len}; read {read}");
                let n = len - read;
                if n != 0 && cfg!(debug_assertions) {
                    dbg!("underread: skipping {n} bytes");
                }
                self.buff.skip(n)?;
            }
            let read = self.buff.read - rb4;
            debug_assert!(len >= read, "overread; supposed to read {len}; read {read}");
            debug_assert!((len - read) == 0, "supposed to read {len}; read {read}");
            Ok(())
        };
        Ok(c)
    }

    pub fn collect_entities(&mut self) -> Result<Vec<Unit>, ReadError> {
        let mut co = self.entities()?;
        let n = match Pin::new(&mut co).resume(()) {
            Yielded(EntityData::Length(x)) => x,
            Complete(Err(e)) => return Err(e),
            _ => unreachable!(),
        };
        let mut o = Vec::with_capacity(n as usize);
        for _ in 0..n {
            match Pin::new(&mut co).resume(()) {
                Yielded(EntityData::Data(x)) => o.push(x),
                Complete(Err(e)) => return Err(e),
                Complete(Ok(())) => break,
                _ => unreachable!(),
            }
        }
        Ok(o)
    }
}

/// serde map
impl Serializable for Map {
    type ReadError = ReadError;
    type WriteError = ();
    /// deserialize a map
    ///
    /// note: does not deserialize all data
    fn deserialize(buff: &mut DataRead<'_>) -> Result<Map, Self::ReadError> {
        let mut buff = MapReader::new(buff)?;
        buff.header()?;
        let v = buff.version()?;
        let tags = buff.tags_alloc()?;
        let r = buff.content()?;

        let mut m = buff.collect_map(tags, r)?;
        m.entities = buff.collect_entities()?;
        if v == 8 {
            // skip marker region
            buff.skip()?;
        }

        // skip custom chunks
        buff.skip()?;
        Ok(m)
    }

    /// serialize a map (todo)
    /// panics: always
    fn serialize(&self, _: &mut super::DataWrite<'_>) -> Result<(), Self::WriteError> {
        unimplemented!()
    }
}
