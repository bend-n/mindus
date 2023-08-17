//! the map module
//! ### format
//! note: utf = `len<u16>` + `utf8(read(len))`
//!
//! note: each section has a `u32` denoting its length
//!
//! key: `: T` and `x<T>` both mean read T, `iterate T` means iterate `read_T()` times
//!
//! ZLIB compressed stream contains:
//! - header: 4b = `MSCH`
//! - version: `u32` (should be 7)
//! - tag section `<u32>`
//!     - 1 byte of idk (skip)
//!     - string map (`u16` for map len, iterate each, read `utf`)
//! - content header section `<u32>`:
//!     - iterate `i8` (should = `8`)'
//!         - the type: `i8` (0: item, block: 1, liquid: 4, status: 5, unit: 6, weather: 7, sector: 9, planet: 13//!         - item count: `u16` (item: 22, block: 412, liquid: 11, status: 21, unit: 66, weather: 6, sector: 35, planet: 7)
//!         - these types all have their own modules: [`item`], [`content`], [`fluid`], [`modifier`], [`mod@unit`], [`weather`], [`sector`], [`planet`]
//!         - iterate `u16`
//!             - name: `utf`
//! - map section `<u32>`
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
//!                 - else
//!                     - consecutives: `u8`
//!                     - iterate `(i + 1)..(i + 1 + consecutives)`
//!                         - same block
//!                     - i += consecutives
//! - entities section `<u32>`
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
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use thiserror::Error;

use crate::block::content::Type as BlockEnum;
use crate::block::{Block, BlockRegistry, Rotation, State};
use crate::data::dynamic::DynSerializer;
use crate::data::renderer::*;
use crate::data::DataRead;
use crate::fluid::Type as Fluid;
use crate::item::{storage::Storage, Type as Item};
use crate::team::{self, Team};
#[cfg(doc)]
use crate::{block::content, data::*, fluid, item, modifier, unit};

use super::Serializer;
use crate::content::Content;
use crate::utils::image::ImageUtils;

/// a tile in a map
#[derive(Clone)]
pub struct Tile<'l> {
    pub floor: BlockEnum,
    pub ore: BlockEnum,
    build: Option<Build<'l>>,
}

macro_rules! lo {
	($v:expr => [$(|)? $($k:literal $(|)?)+], $scale: ident) => { paste::paste! {
		match $v {
			$(BlockEnum::[<$k:camel>] => load!($k, $scale),)+
				n => unreachable!("{n:?}"),
			}
	} };
}

impl<'l> Tile<'l> {
    #[must_use]
    pub const fn new(floor: BlockEnum, ore: BlockEnum) -> Self {
        Self {
            floor,
            ore,
            build: None,
        }
    }

    fn set_block(&mut self, block: &'l Block) {
        self.build = Some(Build {
            block,
            state: None,
            items: Storage::new(),
            liquids: Storage::new(),
            rotation: Rotation::Up,
            team: crate::team::SHARDED,
            data: 0,
        });
    }

    #[must_use]
    pub const fn build(&self) -> Option<&Build<'l>> {
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

    #[inline]
    pub(crate) fn floor(&self, s: Scale) -> ImageHolder<4> {
        lo!(self.floor => [
			| "darksand"
			| "sand-floor"
			| "dacite"
			| "dirt"
			| "arkycite-floor"
			| "basalt"
			| "moss"
			| "mud"
			| "grass"
			| "ice-snow" | "snow" | "salt" | "ice"
			| "hotrock" | "char" | "magmarock"
			| "shale"
			| "metal-floor" | "metal-floor-2" | "metal-floor-3" | "metal-floor-4" | "metal-floor-5" | "metal-floor-damaged"
			| "dark-panel-1" | "dark-panel-2" | "dark-panel-3" | "dark-panel-4" | "dark-panel-5" | "dark-panel-6"
			| "darksand-tainted-water" | "darksand-water" | "deep-tainted-water" | "deep-water" | "sand-water" | "shallow-water" | "tainted-water"
			| "tar" | "pooled-cryofluid" | "molten-slag"
			| "space"
			| "stone"
			| "bluemat"
			| "ferric-craters"
			| "beryllic-stone"
			| "rhyolite" | "rough-rhyolite" | "rhyolite-crater" | "rhyolite-vent"
			| "core-zone"
			| "crater-stone"
			| "redmat"
			| "red-ice"
			| "spore-moss"
			| "regolith"
			| "ferric-stone"
			| "arkyic-stone" | "arkyic-vent"
			| "yellow-stone" | "yellow-stone-plates" | "yellow-stone-vent"
			| "red-stone" | "red-stone-vent" | "dense-red-stone"
			| "carbon-stone" | "carbon-vent"
			| "crystal-floor" | "crystalline-stone" | "crystalline-vent"
			| "empty"], s)
    }

    #[must_use]
    #[inline]
    pub(crate) fn ore(&self, s: Scale) -> ImageHolder<4> {
        lo!(self.ore => ["ore-copper" | "ore-beryllium" | "ore-lead" | "ore-scrap" | "ore-coal" | "ore-thorium" | "ore-titanium" | "ore-tungsten" | "pebbles" | "tendrils" | "ore-wall-tungsten" | "ore-wall-beryllium" | "ore-wall-thorium" | "spawn" | "ore-crystal-thorium"], s)
    }

    #[must_use]
    #[inline]
    pub fn has_ore(&self) -> bool {
        self.ore != BlockEnum::Air
    }

    /// Draw the floor of this tile
    #[must_use]
    pub fn floor_image(&self, s: Scale) -> ImageHolder<4> {
        let mut floor = self.floor(s);
        if self.has_ore() {
            unsafe { floor.overlay(&self.ore(s)) };
        }
        floor
    }

    /// Draw this tiles build.
    #[must_use]
    #[inline]
    pub fn build_image(&self, context: Option<&RenderingContext>, s: Scale) -> ImageHolder<4> {
        // building covers floore
        let Some(b) = &self.build else {
            unreachable!();
        };
        b.image(context, s)
    }
}

impl std::fmt::Debug for Tile<'_> {
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

impl<'l> BlockState<'l> for Tile<'l> {
    fn get_block(&self) -> Option<&'l Block> {
        Some(self.build()?.block)
    }
}

impl RotationState for Tile<'_> {
    fn get_rotation(&self) -> Option<Rotation> {
        Some(self.build()?.rotation)
    }
}

impl RotationState for Option<Tile<'_>> {
    fn get_rotation(&self) -> Option<Rotation> {
        self.as_ref().unwrap().get_rotation()
    }
}

impl<'l> BlockState<'l> for Option<Tile<'_>> {
    fn get_block(&'l self) -> Option<&'l Block> {
        self.as_ref().unwrap().get_block()
    }
}

/// a build on a tile in a map
#[derive(Clone)]
pub struct Build<'l> {
    pub block: &'l Block,
    pub items: Storage<Item>,
    pub liquids: Storage<Fluid>,
    pub state: Option<State>,
    // pub health: f32,
    pub rotation: Rotation,
    pub team: Team,
    pub data: i8,
}

impl std::fmt::Debug for Build<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Build<{block}>", block = self.block.name(),)
    }
}

impl<'l> Build<'l> {
    #[must_use]
    pub fn new(block: &'l Block) -> Build<'l> {
        Self {
            block,
            items: Storage::default(),
            liquids: Storage::default(),
            state: None,
            rotation: Rotation::Up,
            team: team::SHARDED,
            data: 0,
        }
    }

    fn image(&self, context: Option<&RenderingContext>, s: Scale) -> ImageHolder<4> {
        self.block
            .image(self.state.as_ref(), context, self.rotation, s)
    }

    #[must_use]
    pub const fn name(&self) -> &str {
        self.block.name()
    }

    pub fn read(&mut self, buff: &mut DataRead<'_>, reg: &BlockRegistry) -> Result<(), ReadError> {
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
        // "efficiency"?
        buff.skip(2)?;
        if version == 4 {
            // visible flags for fog
            buff.skip(4)?;
        }
        // "overridden by subclasses"
        self.block.read(self, reg, buff)?;
        // implementation not complete, simply error, causing the remaining bytes in the chunk to be skipped (TODO finish impl)
        Err(ReadError::Version(0x0))
        // Ok(())
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
pub struct Map<'l> {
    pub width: usize,
    pub height: usize,
    pub tags: HashMap<String, String>,
    /// row major 2d array
    /// ```rs
    /// (0, 0), (1, 0), (2, 0)
    /// (0, 1), (1, 1), (2, 1)
    /// (0, 2), (1, 2), (2, 2)
    /// ```
    pub tiles: Vec<Tile<'l>>,
}

macro_rules! cond {
    ($cond: expr, $do: expr) => {
        if $cond {
            None
        } else {
            $do
        }
    };
}

impl<'l> Crossable for Map<'l> {
    // N
    // cond!(pos.position.1 >= (pos.height - 1) as u16, get(j + 1)),
    // // E
    // cond!(
    //     pos.position.0 >= (pos.height - 1) as u16,
    //     get(j + pos.height)
    // ),
    // // S
    // cond!(
    //     pos.position.1 == 0 || pos.position.1 >= pos.height as u16,
    // cond!(pos.position.1 >= (pos.height - 1), get(j + 1)),
    //      // E
    //      cond!(pos.position.0 >= (pos.height - 1), get(j + pos.height)),
    //      // S
    //      cond!(
    //          pos.position.1 == 0 || pos.position.1 >= pos.height,
    //          get(j - 1)
    //      ),
    //      // W
    //      cond!(j < pos.height, get(j - pos.height)),
    fn cross(&self, j: usize, c: &PositionContext) -> Cross {
        let get = |i| {
            let b = &self[i];
            Some((b.get_block()?, b.get_rotation()?))
        };
        [
            cond![
                c.position.1 == 0 || c.position.1 >= c.height,
                get(j + self.height)
            ],
            cond![c.position.0 >= (c.height - 1), get(j + 1)],
            cond![c.position.1 >= (c.height - 1), get(j - self.width)],
            cond![j < c.height, get(j - 1)],
        ]
    }
}

impl<'l> Map<'l> {
    #[must_use]
    pub fn new(width: usize, height: usize, tags: HashMap<String, String>) -> Self {
        Self {
            tiles: vec![Tile::new(BlockEnum::Air, BlockEnum::Air); width * height],
            height,
            width,
            tags,
        }
    }
}

impl<'l> Index<usize> for Map<'l> {
    type Output = Tile<'l>;
    fn index(&self, index: usize) -> &Self::Output {
        &self.tiles[index]
    }
}

impl<'l> IndexMut<usize> for Map<'l> {
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
    #[error("unknown block {0:?}")]
    NoSuchBlock(String),
    #[error("failed to read block data")]
    ReadState(#[from] super::dynamic::ReadError),
}

/// serde map
pub struct MapSerializer<'l>(pub &'l BlockRegistry<'l>);
impl<'l> Serializer<Map<'l>> for MapSerializer<'l> {
    type ReadError = ReadError;
    type WriteError = ();
    /// deserialize a map
    ///
    /// notes:
    /// - does not deserialize data
    /// - does not deserialize entities
    fn deserialize(&mut self, buff: &mut DataRead<'_>) -> Result<Map<'l>, Self::ReadError> {
        let buff = buff.deflate()?;
        let mut buff = DataRead::new(&buff);
        {
            let mut b = [0; 4];
            buff.read_bytes(&mut b)?;
            if b != MAP_HEADER {
                return Err(ReadError::Header(b));
            }
        }
        let version = buff.read_u32()?;
        if version != 7 {
            return Err(ReadError::Version(version.try_into().unwrap_or(0)));
        }
        let mut tags = HashMap::new();
        buff.read_chunk(true, |buff| {
            buff.skip(1)?;
            for _ in 0..buff.read_u8()? {
                let key = buff.read_utf()?;
                let value = buff.read_utf()?;
                tags.insert(key.to_owned(), value.to_owned());
            }
            Ok::<(), super::ReadError>(())
        })?;
        // we skip the content header (just keep the respective modules updated)
        buff.skip_chunk()?;
        // map section
        let mut w = 0;
        let mut h = 0;
        let mut m = None;
        buff.read_chunk(true, |buff| {
            w = buff.read_u16()? as usize;
            h = buff.read_u16()? as usize;
            let mut map = Map::new(w, h, tags);
            let count = w * h;
            let mut i = 0;
            while i < count {
                let floor_id = buff.read_u16()?;
                let overlay_id = buff.read_u16()?;
                let floor = BlockEnum::try_from(floor_id).unwrap_or(BlockEnum::Stone);
                let ore = BlockEnum::try_from(overlay_id).unwrap_or(BlockEnum::Air);
                map[i] = Tile::new(floor, ore);
                let consecutives = buff.read_u8()? as usize;
                if consecutives > 0 {
                    for i in (i + 1)..(i + 1 + consecutives) {
                        map[i] = Tile::new(floor, ore);
                    }
                    i += consecutives;
                }
                i += 1;
            }
            let mut i = 0usize;
            while i < count {
                let block_id = buff.read_u16()?;
                let packed = buff.read_u8()?;
                let entity = (packed & 1) != 0;
                let data = (packed & 2) != 0;
                let central = if entity { buff.read_bool()? } else { false };
                let block = BlockEnum::try_from(block_id)
                    .map_err(|_| ReadError::NoSuchBlock(block_id.to_string()))?;
                let block = if block == BlockEnum::Air {
                    None
                } else {
                    Some(
                        self.0
                            .get(block.get_name())
                            .ok_or_else(|| ReadError::NoSuchBlock(block.to_string()))?,
                    )
                };
                if central && let Some(block) = block {
                    map[i].set_block(block);
                }
                if entity {
                    if central {
                        let mut output = [0u8; 2];
                        output.copy_from_slice(&buff.data[..2]);
                        let _ = buff.read_chunk(false, |buff| {
                            #[cfg(debug_assertions)]
                            println!("reading {:?}", map[i].build.as_ref().unwrap());
                            let _ = buff.read_i8()?;

                            map[i].build.as_mut().unwrap().read(buff, self.0)?;
                            Ok::<(), ReadError>(())
                        });
                    }
                } else if data {
                    if let Some(block) = block {
                        map[i].set_block(block);
                    }
                    map[i].build.as_mut().unwrap().data = buff.read_i8()?;
                } else {
                    let consecutives = buff.read_u8()? as usize;
                    for i in i..=i + consecutives {
                        if let Some(block) = block {
                            map.tiles[i].set_block(block);
                        }
                    }
                    i += consecutives;
                }
                i += 1;
            }
            m = Some(map);
            Ok::<(), ReadError>(())
        })?;
        buff.read_chunk(true, |buff| {
            // read entity mapping (SaveVersion.java#436)
            for _ in 0..buff.read_u16()? {
                buff.skip(2)?;
                let _ = buff.read_utf()?;
            }
            // read team block plans (ghosts) (SaveVersion.java#389)
            for _ in 0..buff.read_u32()? {
                buff.skip(4)?;
                for _ in 0..buff.read_u32()? {
                    buff.skip(8usize)?;
                    let _ = DynSerializer::deserialize(&mut DynSerializer, buff)?;
                }
            }
            // read world entities (#412). eg units
            for _ in 0..buff.read_u32()? {
                let len = buff.read_u16()? as usize;
                buff.skip(len)?;
            }
            Ok::<(), ReadError>(())
        })?;
        // skip custom chunks
        buff.skip_chunk()?;
        Ok(m.unwrap())
    }

    /// serialize a map (todo)
    /// panics: always
    fn serialize(
        &mut self,
        _: &mut super::DataWrite<'_>,
        _: &Map<'_>,
    ) -> Result<(), Self::WriteError> {
        todo!()
    }
}
