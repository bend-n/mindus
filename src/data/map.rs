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
//!         - the type: `i8` (0: item, block: 1, liquid: 4, status: 5, unit: 6, weather: 7, sector: 9, planet: 13//!         - item count: `u1\'6` (item: 22, block: 412, liquid: 11, status: 21, unit: 66, weather: 6, sector: 35, planet: 7)
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
//!                         - tile.build.readAll
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
use crate::block::{environment, Block, BlockRegistry, Rotation};
use crate::data::dynamic::DynSerializer;
use crate::data::renderer::*;
use crate::data::DataRead;
use crate::fluid::Type as Fluid;
use crate::item::{storage::Storage, Type as Item};
use crate::team::Team;
#[allow(unused_imports)]
use crate::{block::content, data::*, fluid, item, modifier, unit};

use super::Serializer;
use crate::content::Content;
use crate::utils::image::ImageUtils;

/// a tile in a map
#[derive(Clone)]
pub struct Tile<'l> {
    pub floor: &'l Block,
    pub ore: Option<&'l Block>,
    build: Option<Build<'l>>,
}

pub type EntityMapping = HashMap<u8, Box<dyn Content>>;
impl<'l> Tile<'l> {
    pub fn new(floor: &'l Block, ore: Option<&'l Block>) -> Self {
        Self {
            floor,
            ore,
            build: None,
        }
    }

    fn set_block(&mut self, block: &'l Block) {
        self.build = Some(Build {
            block,
            items: Storage::new(),
            liquids: Storage::new(),
            rotation: Rotation::Up,
            team: crate::team::SHARDED,
            data: 0,
        });
    }

    pub fn build(&self) -> Option<&Build<'l>> {
        self.build.as_ref()
    }

    /// check if this tile contains a building.
    pub fn has_building(&self) -> bool {
        if let Some(b) = &self.build {
            return b.block.has_building();
        }
        false
    }

    /// size of this tile
    ///
    /// ._.
    ///
    /// dont think about it too much
    pub fn size(&self) -> u8 {
        if let Some(b) = &self.build {
            return b.block.get_size();
        }
        1
    }

    pub fn image(&self, context: Option<&RenderingContext>) -> ImageHolder {
        // building covers floore
        let i = if let Some(b) = &self.build {
            b.image(context)
        } else {
            let mut i = self.floor.image(None, context).own();
            if let Some(ore) = self.ore {
                i.overlay(ore.image(None, context).borrow(), 0, 0);
            }
            ImageHolder::from(i)
        };
        i
    }
}

impl std::fmt::Debug for Tile<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Tile@{}{}{}",
            self.floor.name(),
            if let Some(ore) = &self.ore {
                format!("+{}", ore.name())
            } else {
                "".into()
            },
            if let Some(build) = &self.build {
                format!(":{}", build.block.name())
            } else {
                "".to_string()
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
#[derive(Debug, Clone)]
pub struct Build<'l> {
    pub block: &'l Block,
    pub items: Storage<Item>,
    pub liquids: Storage<Fluid>,
    // pub health: f32,
    pub rotation: Rotation,
    pub team: Team,
    pub data: i8,
}

impl Build<'_> {
    pub fn image(&self, context: Option<&RenderingContext>) -> ImageHolder {
        self.block.image(None, context)
    }

    pub fn read(
        &mut self,
        buff: &mut DataRead<'_>,
        _reg: &BlockRegistry,
        _map: &EntityMapping,
    ) -> Result<(), ReadError> {
        // health
        let _ = buff.read_f32()?; // 4
        let rot = buff.read_u8()?; // 5
        self.rotation = Rotation::try_from(rot & 127).unwrap_or(Rotation::Up);
        if (rot & 128) == 0 {
            return Err(ReadError::Version(rot & 128));
        }

        let _t = buff.read_u8()?; // 6
        let _v = buff.read_u8()?; // 7
        let _mask = buff.read_u8()?; // 8

        // if (mask & 1) != 0 {
        //     self.items.clear();
        //     // 10
        //     for _ in 0..dbg!(buff.read_u16()?) {
        //         let item = buff.read_u16()?;
        //         let amount = buff.read_u32()?;
        //         if let Ok(item) = Item::try_from(item) {
        //             self.items.set(item, amount);
        //         }
        //     }
        // }
        // if mask & 2 == 0 {
        //     let n = buff.read_u16()? as usize;
        //     buff.skip((n * 4) + 1)?;
        // }
        // if mask & 4 == 0 {
        //     self.liquids.clear();
        //     for _ in 0..buff.read_u16()? {
        //         let fluid = buff.read_u16()?;
        //         let amount = buff.read_f32()?;
        //         if let Ok(fluid) = Fluid::try_from(fluid) {
        //             self.liquids.set(fluid, (amount * 100.0) as u32);
        //         }
        //     }
        // }
        // "efficiency"?
        // let _ = buff.read_u8()?;
        // let _ = buff.read_u8()?;
        // visible flags
        // let _ = buff.read_i64()?;
        // implementation not complete, simply error, causing the remaining bytes in the chunk to be skipped (TODO finish impl)
        Err(ReadError::Version(0x0))
        // "overridden by subclasses"
        // self.block.read(buff, reg, map)?;
        // Ok(())
    }
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
    pub fn new(width: usize, height: usize, tags: HashMap<String, String>) -> Self {
        Self {
            tiles: vec![Tile::new(&environment::STONE, None); width * height],
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
                let floor = BlockEnum::try_from(floor_id)
                    .unwrap_or(BlockEnum::Stone)
                    .to(self.0)
                    .unwrap_or(&environment::STONE);
                let ore = BlockEnum::try_from(overlay_id)
                    .unwrap_or(BlockEnum::Air)
                    .to(self.0);
                map[i] = Tile::new(floor, ore);
                let consecutives = buff.read_u8()? as usize;
                if consecutives > 0 {
                    for i in (i + 1)..(i + 1 + consecutives) {
                        map[i] = Tile::new(floor, ore)
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
                let block = if block != BlockEnum::Air {
                    Some(
                        self.0
                            .get(block.get_name())
                            .ok_or(ReadError::NoSuchBlock(block.to_string()))?,
                    )
                } else {
                    None
                };
                if central {
                    if let Some(block) = block {
                        map[i].set_block(block);
                    }
                }
                if entity {
                    if central {
                        let _ = buff.read_chunk(false, |buff| {
                            let _ = buff.read_i8()?;
                            map[i]
                                .build
                                .as_mut()
                                .unwrap()
                                // map not initialized yet
                                .read(buff, self.0, &HashMap::new())?;
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
                    for tile in map.tiles.iter_mut().take(consecutives).skip(i + 1) {
                        if let Some(block) = block {
                            tile.set_block(block);
                        }
                    }
                    i += consecutives;
                }
                i += 1
            }
            m = Some(map);
            Ok::<(), ReadError>(())
        })?;
        let mut mapping = EntityMapping::new();
        buff.read_chunk(true, |buff| {
            for _ in 0..buff.read_u16()? {
                let id = buff.read_i16()? as u8;
                let nam = buff.read_utf()?;
                dbg!(nam);
                mapping.insert(id, Box::new(Item::Copper));
                // mapping.push(content::Type::get_name(nam));
            }
            for _ in 0..buff.read_u32()? {
                buff.skip(4)?;
                for _ in 0..buff.read_u32()? {
                    buff.skip(8usize)?;
                    let _ = DynSerializer::deserialize(&mut DynSerializer, buff)?;
                }
            }
            for _ in 0..buff.read_u32()? {
                let len = buff.read_u16()? as usize;
                let ty = buff.read_u8()?;
                if !mapping.contains_key(&ty) {
                    buff.skip(len - 1)?;
                    continue;
                }
                let _id = buff.read_u32()?;
                // TODO
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
