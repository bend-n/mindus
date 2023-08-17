//! schematic parsing
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{self, Write};
use thiserror::Error;

use crate::block::{self, Block, BlockRegistry, Rotation, State};
use crate::data::base64;
use crate::data::dynamic::{self, DynData, DynSerializer};
use crate::data::renderer::*;
use crate::data::{self, DataRead, DataWrite, GridPos, Serializer};
use crate::item::storage::ItemStorage;
use crate::registry::RegistryEntry;
use crate::utils::array::Array2D;

/// biggest schematic
pub const MAX_DIMENSION: usize = 256;
/// most possible blocks
pub const MAX_BLOCKS: u32 = 256 * 256;

/// a placement in a schematic
#[derive(Clone)]
pub struct Placement<'l> {
    pub block: &'l Block,
    pub rot: Rotation,
    state: Option<State>,
}

impl PartialEq for Placement<'_> {
    fn eq(&self, rhs: &Placement<'_>) -> bool {
        self.block == rhs.block && self.rot == rhs.rot
    }
}

impl fmt::Debug for Placement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "P<{}[*{}]>", self.block.name(), self.rot.ch())
    }
}

impl<'l> Placement<'l> {
    /// make a placement from a block
    #[must_use]
    pub const fn new(block: &'l Block) -> Self {
        Self {
            block,
            rot: Rotation::Up,
            state: None,
        }
    }

    /// gets the current state of this placement. you can cast it with `placement.block::get_state(placement.get_state()?)?`
    #[must_use]
    pub const fn get_state(&self) -> Option<&State> {
        self.state.as_ref()
    }

    /// get mutable state.
    pub fn get_state_mut(&mut self) -> Option<&mut State> {
        self.state.as_mut()
    }

    /// draws this placement in particular
    #[must_use]
    pub fn image(
        &self,
        context: Option<&RenderingContext>,
        rot: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        self.block.image(self.get_state(), context, rot, s)
    }

    /// set the state
    pub fn set_state(&mut self, data: DynData) -> Result<Option<State>, block::DeserializeError> {
        let state = self.block.deserialize_state(data)?;
        Ok(std::mem::replace(&mut self.state, state))
    }

    /// rotate this
    pub fn set_rotation(&mut self, rot: Rotation) -> Rotation {
        std::mem::replace(&mut self.rot, rot)
    }
}

impl<'l> BlockState<'l> for Placement<'l> {
    fn get_block(&self) -> Option<&'l Block> {
        Some(self.block)
    }
}

impl RotationState for Placement<'_> {
    fn get_rotation(&self) -> Option<Rotation> {
        Some(self.rot)
    }
}

impl<'l> BlockState<'l> for Option<Placement<'l>> {
    fn get_block(&self) -> Option<&'l Block> {
        let Some(p) = self else {
            return None;
        };
        Some(p.block)
    }
}

impl RotationState for Option<Placement<'_>> {
    fn get_rotation(&self) -> Option<Rotation> {
        let Some(p) = self else {
            return None;
        };
        Some(p.rot)
    }
}

#[derive(Clone, Debug)]
/// a schematic.
pub struct Schematic<'l> {
    pub width: usize,
    pub height: usize,
    pub tags: HashMap<String, String>,
    /// schems can have holes, so [Option] is used.
    pub blocks: Array2D<Option<Placement<'l>>>,
}

impl<'l> PartialEq for Schematic<'l> {
    fn eq(&self, rhs: &Schematic<'l>) -> bool {
        self.width == rhs.width
            && self.height == rhs.height
            && self.blocks == rhs.blocks
            && self.tags == rhs.tags
    }
}

impl<'l> Schematic<'l> {
    #[must_use]
    /// create a new schematic, panicking if too big
    /// ```
    /// # use mindus::Schematic;
    /// let s = Schematic::new(5, 5);
    /// ```
    pub fn new(width: usize, height: usize) -> Self {
        match Self::try_new(width, height) {
            Ok(s) => s,
            Err(NewError::Width(w)) => panic!("invalid schematic width ({w})"),
            Err(NewError::Height(h)) => panic!("invalid schematic height ({h})"),
        }
    }

    /// the area around a point
    pub(crate) fn cross(&self, c: &PositionContext) -> Cross {
        let get = |x, y| {
            let b = self.get(x?, y?).ok()??;
            Some((b.get_block()?, b.get_rotation()?))
        };
        macro_rules! s {
            ($x:expr) => {
                Some($x)
            };
            ($a:expr => $b:expr) => {
                if $a < $b {
                    None
                } else {
                    Some($a - $b)
                }
            };
        }
        [
            get(s!(c.position.0), s!(c.position.1 + 1)),
            get(s!(c.position.0 + 1), s!(c.position.1)),
            get(s!(c.position.0), s!(c.position.1 => 1)),
            get(s!(c.position.0 => 1), s!(c.position.1)),
        ]
    }

    /// create a new schematic, erroring if too big
    /// ```
    /// # use mindus::Schematic;
    /// assert!(Schematic::try_new(500, 500).is_err() == true);
    /// ```
    pub fn try_new(width: usize, height: usize) -> Result<Self, NewError> {
        if width > MAX_DIMENSION {
            return Err(NewError::Width(width));
        }
        if height > MAX_DIMENSION {
            return Err(NewError::Height(height));
        }
        let mut tags = HashMap::<String, String>::new();
        tags.insert("name".to_string(), String::new());
        tags.insert("description".to_string(), String::new());
        tags.insert("labels".to_string(), "[]".to_string());
        Ok(Self {
            width,
            height,
            tags,
            blocks: Array2D::new(None, width, height),
        })
    }

    // #[must_use]
    // /// check if a rect is empty
    // /// ```
    // /// # use mindus::Schematic;
    // /// # use mindus::block::distribution::ROUTER;
    // /// let mut s = Schematic::new(5, 5);
    // /// s.put(0, 0, &ROUTER);
    // /// assert!(s.is_region_empty(1, 1, 4, 4));
    // /// s.put(2, 2, &ROUTER);
    // /// assert!(s.is_region_empty(1, 1, 4, 4) == false);
    // /// // out of bounds is empty
    // /// assert!(s.is_region_empty(25, 25, 0, 0));
    // /// ```
    // pub fn is_region_empty(&self, x: usize, y: usize, w: usize, h: usize) -> bool {
    //     if x >= self.width || y >= self.height || w == 0 || h == 0 {
    //         return true;
    //     }
    //     if w > 1 || h > 1 {
    //         for y in y..(y + h).min(self.height) {
    //             for x in x..(x + w).min(self.width) {
    //                 if self.get(x, y).unwrap().is_some() {
    //                     return false;
    //                 }
    //             }
    //         }
    //         true
    //     } else {
    //         self.get(x, y).unwrap().is_none()
    //     }
    // }

    /// gets a block
    /// ```
    /// # use mindus::Schematic;
    /// # use mindus::block::Rotation;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// assert!(s.get(0, 0).unwrap().is_none());
    /// s.put(0, 0, &mindus::block::turrets::DUO);
    /// assert!(s.get(0, 0).unwrap().is_some());
    /// ```
    pub fn get(&self, x: usize, y: usize) -> Result<Option<&Placement<'l>>, PosError> {
        if x >= self.width || y >= self.height {
            return Err(PosError {
                x,
                y,
                w: self.width,
                h: self.height,
            });
        }
        Ok(self.blocks[x][y].as_ref())
    }

    /// gets a block, mutably
    pub fn get_mut(&mut self, x: usize, y: usize) -> Result<Option<&mut Placement<'l>>, PosError> {
        if x >= self.width || y >= self.height {
            return Err(PosError {
                x,
                y,
                w: self.width,
                h: self.height,
            });
        }
        Ok(self.blocks[x][y].as_mut())
    }

    /// put a block in (same as [`Schematic::set`], but less arguments and builder-ness). panics!!!
    /// ```
    /// # use mindus::Schematic;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// s.put(0, 0, &mindus::block::distribution::ROUTER);
    /// assert!(s.get(0, 0).unwrap().is_some() == true);
    /// ```
    pub fn put(&mut self, x: usize, y: usize, block: &'l Block) -> &mut Self {
        self.set(x, y, block, DynData::Empty, Rotation::Up).unwrap();
        self
    }

    /// set a block
    /// ```
    /// # use mindus::Schematic;
    /// # use mindus::DynData;
    /// # use mindus::block::Rotation;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// s.set(0, 0, &mindus::block::distribution::ROUTER, DynData::Empty, Rotation::Right);
    /// assert!(s.get(0, 0).unwrap().is_some() == true);
    /// ```
    pub fn set(
        &mut self,
        x: usize,
        y: usize,
        block: &'l Block,
        data: DynData,
        rot: Rotation,
    ) -> Result<(), PlaceError> {
        let sz = usize::from(block.get_size());
        let off = (sz - 1) / 2;
        if x < off || y < off {
            return Err(PlaceError::Bounds {
                x,
                y,
                sz: block.get_size(),
                w: self.width,
                h: self.height,
            });
        }
        if self.width - x < sz - off || self.height - y < sz - off {
            return Err(PlaceError::Bounds {
                x,
                y,
                sz: block.get_size(),
                w: self.width,
                h: self.height,
            });
        }
        let state = block.deserialize_state(data)?;
        let p = Placement { block, rot, state };
        self.blocks[x][y] = Some(p);
        Ok(())
    }

    /// take out a block
    /// ```
    /// # use mindus::Schematic;
    /// # use mindus::DynData;

    /// # use mindus::block::Rotation;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// s.put(0, 0, &mindus::block::turrets::DUO);
    /// assert!(s.get(0, 0).unwrap().is_some() == true);
    /// assert!(s.take(0, 0).unwrap().is_some() == true);
    /// assert!(s.get(0, 0).unwrap().is_none() == true);
    /// ```
    pub fn take(&mut self, x: usize, y: usize) -> Result<Option<Placement<'l>>, PosError> {
        if x >= self.width || y >= self.height {
            return Err(PosError {
                x,
                y,
                w: self.width,
                h: self.height,
            });
        }
        let b = self.blocks[x][y].take();
        Ok(b)
    }

    /// iterate over all the blocks
    pub fn block_iter(&self) -> impl Iterator<Item = (GridPos, &Placement<'_>)> {
        self.blocks.iter().enumerate().filter_map(|(i, p)| {
            let Some(p) = p else {
                return None;
            };
            Some((GridPos(i / self.height, i % self.height), p))
        })
    }

    #[must_use]
    /// see how much this schematic costs.
    /// returns (cost, `is_sandbox`)
    /// ```
    /// # use mindus::Schematic;
    /// # use mindus::DynData;
    /// # use mindus::block::Rotation;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// s.put(1, 1, &mindus::block::turrets::CYCLONE);
    /// assert_eq!(s.compute_total_cost().0.get_total(), 405);
    /// ```
    pub fn compute_total_cost(&self) -> (ItemStorage, bool) {
        let mut cost = ItemStorage::new();
        let mut sandbox = false;
        for &Placement { block, .. } in self.blocks.iter().filter_map(|b| b.as_ref()) {
            if let Some(curr) = block.get_build_cost() {
                cost.add_all(&curr, u32::MAX);
            } else {
                sandbox = true;
            }
        }
        (cost, sandbox)
    }
}

/// error created by creating a new schematic
#[derive(Copy, Clone, Debug, Eq, PartialEq, Error)]
pub enum NewError {
    #[error("invalid schematic width ({0})")]
    Width(usize),
    #[error("invalid schematic height ({0})")]
    Height(usize),
}
/// error created by doing stuff out of bounds
#[derive(Copy, Clone, Debug, Eq, PartialEq, Error)]
#[error("position {x} / {y} out of bounds {w} / {h}")]
pub struct PosError {
    pub x: usize,
    pub y: usize,
    pub w: usize,
    pub h: usize,
}

#[derive(Debug, Error)]
pub enum PlaceError {
    #[error("invalid block placement {x} / {y} (size {sz}) within {w} / {h}")]
    Bounds {
        x: usize,
        y: usize,
        sz: u8,
        w: usize,
        h: usize,
    },
    #[error("overlapping an existing block at {x} / {y}")]
    Overlap { x: usize, y: usize },
    #[error("block state deserialization failed")]
    Deserialize(#[from] block::DeserializeError),
}

#[derive(Debug, Error)]
pub enum ResizeError {
    #[error("invalid target width ({0})")]
    TargetWidth(u16),
    #[error("invalid target height ({0})")]
    TargetHeight(u16),
    #[error("horizontal offset {dx} not in [-{new_w}, {old_w}]")]
    XOffset { dx: i16, old_w: u16, new_w: u16 },
    #[error("vertical offset {dy} not in [-{new_h}, {old_h}]")]
    YOffset { dy: i16, old_h: u16, new_h: u16 },
    #[error(transparent)]
    Truncated(#[from] TruncatedError),
}

#[derive(Error, Debug)]
pub struct TruncatedError {
    right: u16,
    top: u16,
    left: u16,
    bottom: u16,
}

impl fmt::Display for TruncatedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! fmt_dir {
            ($f:ident, $first:ident, $name:expr, $value:expr) => {
                if $value != 0 {
                    if $first {
                        f.write_str(" (")?;
                        $first = false;
                    } else {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}: {}", $name, $value)?;
                }
            };
        }

        f.write_str("truncated blocks")?;
        let mut first = true;
        fmt_dir!(f, first, "right", self.right);
        fmt_dir!(f, first, "top", self.top);
        fmt_dir!(f, first, "left", self.left);
        fmt_dir!(f, first, "bottom", self.bottom);
        if !first {
            f.write_char(')')?;
        }
        Ok(())
    }
}

const SCHEMATIC_HEADER: u32 =
    ((b'm' as u32) << 24) | ((b's' as u32) << 16) | ((b'c' as u32) << 8) | (b'h' as u32);

/// `serde_schematic`
pub struct SchematicSerializer<'l>(pub &'l BlockRegistry<'l>);

impl<'l> Serializer<Schematic<'l>> for SchematicSerializer<'l> {
    type ReadError = ReadError;
    type WriteError = WriteError;
    fn deserialize(&mut self, buff: &mut DataRead<'_>) -> Result<Schematic<'l>, Self::ReadError> {
        let hdr = buff.read_u32()?;
        if hdr != SCHEMATIC_HEADER {
            return Err(ReadError::Header(hdr));
        }
        let version = buff.read_u8()?;
        if version > 1 {
            return Err(ReadError::Version(version));
        }
        let buff = buff.deflate()?;
        let mut buff = DataRead::new(&buff);
        let w = buff.read_i16()? as usize;
        let h = buff.read_i16()? as usize;
        if w > MAX_DIMENSION || h > MAX_DIMENSION {
            return Err(ReadError::Dimensions(w, h));
        }
        let mut schematic = Schematic::new(w, h);
        buff.read_map(&mut schematic.tags)?;
        let num_table = buff.read_i8()?;
        if num_table < 0 {
            return Err(ReadError::TableSize(num_table));
        }
        let mut block_table = Vec::new();
        block_table.reserve(num_table as usize);
        for _ in 0..num_table {
            let name = buff.read_utf()?;
            match self.0.get(name) {
                None => return Err(ReadError::NoSuchBlock(name.to_owned())),
                Some(b) => block_table.push(b),
            }
        }
        let num_blocks = buff.read_i32()?;
        if num_blocks < 0 || num_blocks as u32 > MAX_BLOCKS {
            return Err(ReadError::BlockCount(num_blocks));
        }
        for _ in 0..num_blocks {
            let idx = buff.read_i8()?;
            if idx < 0 || idx as usize >= block_table.len() {
                return Err(ReadError::BlockIndex(idx, block_table.len()));
            }
            let pos = GridPos::from(buff.read_u32()?);
            let block = block_table[idx as usize];
            let config = if version < 1 {
                block.data_from_i32(buff.read_i32()?, pos)?
            } else {
                DynSerializer.deserialize(&mut buff)?
            };
            let rot = Rotation::from(buff.read_u8()?);
            schematic.set(pos.0, pos.1, block, config, rot)?;
        }
        Ok(schematic)
    }

    fn serialize(
        &mut self,
        buff: &mut DataWrite<'_>,
        data: &Schematic,
    ) -> Result<(), Self::WriteError> {
        // write the header first just in case
        buff.write_u32(SCHEMATIC_HEADER)?;
        buff.write_u8(1)?;

        let mut rbuff = DataWrite::default();
        // don't have to check dimensions because they're already limited to MAX_DIMENSION
        rbuff.write_i16(data.width as i16)?;
        rbuff.write_i16(data.height as i16)?;
        if data.tags.len() > u8::MAX as usize {
            return Err(WriteError::TagCount(data.tags.len()));
        }
        rbuff.write_u8(data.tags.len() as u8)?;
        for (k, v) in &data.tags {
            rbuff.write_utf(k)?;
            rbuff.write_utf(v)?;
        }
        // use string keys here to avoid issues with different block refs with the same name
        let mut block_map = HashMap::new();
        let mut block_table = Vec::new();
        let mut block_count = 0i32;
        for curr in data.blocks.iter().filter_map(|b| b.as_ref()) {
            block_count += 1;
            if let Entry::Vacant(e) = block_map.entry(curr.block.get_name()) {
                e.insert(block_table.len() as u32);
                block_table.push(curr.block.get_name());
            }
        }
        if block_table.len() > i8::MAX as usize {
            return Err(WriteError::TableSize(block_table.len()));
        }
        // else: implies contents are also valid i8 (they're strictly less than the map length)
        rbuff.write_i8(block_table.len() as i8)?;
        for &name in &block_table {
            rbuff.write_utf(name)?;
        }
        // don't have to check data.blocks.len() because dimensions don't allow exceeding MAX_BLOCKS
        rbuff.write_i32(block_count)?;
        for (pos, curr) in data.block_iter() {
            rbuff.write_i8(block_map[curr.block.get_name()] as i8)?;
            rbuff.write_u32(pos.into())?;
            let data = match curr.state {
                None => DynData::Empty,
                Some(ref s) => curr.block.serialize_state(s)?,
            };
            DynSerializer.serialize(&mut rbuff, &data)?;
            rbuff.write_u8(curr.rot.into())?;
        }
        rbuff.inflate(buff)?;
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ReadError {
    #[error("failed to read from buffer")]
    Read(#[from] data::ReadError),
    #[error("incorrect header ({0:08X})")]
    Header(u32),
    #[error("unsupported version ({0})")]
    Version(u8),
    #[error("invalid schematic dimensions ({0} / {1})")]
    Dimensions(usize, usize),
    #[error("invalid block table size ({0})")]
    TableSize(i8),
    #[error("unknown block {0:?}")]
    NoSuchBlock(String),
    #[error("invalid total block count ({0})")]
    BlockCount(i32),
    #[error("invalid block index ({0} / {1})")]
    BlockIndex(i8, usize),
    #[error("block config conversion failed")]
    BlockConfig(#[from] block::DataConvertError),
    #[error("failed to read block data")]
    ReadState(#[from] dynamic::ReadError),
    #[error("deserialized block could not be placed")]
    Placement(#[from] PlaceError),
    #[error(transparent)]
    Decompress(#[from] super::DecompressError),
}

#[derive(Debug, Error)]
pub enum WriteError {
    #[error("failed to write data to buffer")]
    Write(#[from] data::WriteError),
    #[error("tag list too long ({0})")]
    TagCount(usize),
    #[error("block table too long ({0})")]
    TableSize(usize),
    #[error(transparent)]
    StateSerialize(#[from] block::SerializeError),
    #[error("failed to write block data")]
    WriteState(#[from] dynamic::WriteError),
    #[error(transparent)]
    Compress(#[from] super::CompressError),
}

impl<'l> SchematicSerializer<'l> {
    /// deserializes a schematic from base64
    /// ```
    /// # use mindus::*;
    /// let string = "bXNjaAF4nGNgZmBmZmDJS8xNZeBOyslPzlYAkwzcKanFyUWZBSWZ+XkMDAxsOYlJqTnFDEzRsYwMfAWJlTn5iSm6RfmlJalFQGlGEGJkZWSYxQAAcBkUPA==";
    /// let reg = build_registry();
    /// let mut ss = SchematicSerializer(&reg);
    /// let s = ss.deserialize_base64(string).unwrap();
    /// assert!(s.get(1, 1).unwrap().unwrap().block.name() == "payload-router");
    /// ```
    pub fn deserialize_base64(&mut self, data: &str) -> Result<Schematic<'l>, R64Error> {
        let mut buff = vec![0; data.len() / 4 * 3 + 1];
        let n_out = base64::decode(data.as_bytes(), buff.as_mut())?;
        Ok(self.deserialize(&mut DataRead::new(&buff[..n_out]))?)
    }

    /// serialize a schematic to base64
    pub fn serialize_base64(&mut self, data: &Schematic<'l>) -> Result<String, W64Error> {
        let mut buff = DataWrite::default();
        self.serialize(&mut buff, data)?;
        let buff = buff.get_written();
        // round up because of padding
        let mut text = vec![0; 4 * (buff.len() / 3 + usize::from(buff.len() % 3 != 0))];
        let n_out = base64::encode(buff, text.as_mut())?;
        // trailing zeros are valid UTF8, but not valid base64
        assert_eq!(n_out, text.len());
        // SAFETY: base64 encoding outputs pure ASCII (hopefully)
        Ok(unsafe { String::from_utf8_unchecked(text) })
    }
}

#[derive(Debug, Error)]
pub enum R64Error {
    #[error("base-64 decoding failed")]
    Base64(#[from] base64::DecodeError),
    #[error(transparent)]
    Content(#[from] ReadError),
}

#[derive(Debug, Error)]
pub enum W64Error {
    #[error("base-64 encoding failed")]
    Base64(#[from] base64::EncodeError),
    #[error(transparent)]
    Content(#[from] WriteError),
}

#[cfg(test)]
mod test {
    use super::*;
    #[track_caller]
    fn unwrap_pretty<T, E: std::fmt::Display + std::error::Error>(r: Result<T, E>) -> T {
        match r {
            Ok(t) => t,
            Err(e) => {
                use std::error::Error;
                eprintln!("{e}");
                let mut err_ref = &e as &dyn Error;
                loop {
                    let Some(next) = err_ref.source() else {
                        panic!();
                    };
                    eprintln!("\tFrom: {next}");
                    err_ref = next;
                }
            }
        }
    }

    macro_rules! test_schem {
        ($name:ident, $($val:expr);+;) => {
            #[test]
            fn $name() {
                let reg = crate::block::build_registry();
                let mut ser = SchematicSerializer(&reg);
                $(
                    let parsed = unwrap_pretty(ser.deserialize_base64($val));
                    println!("\x1b[38;5;2mdeserialized\x1b[0m {}", parsed.tags.get("name").unwrap());
                    let unparsed = unwrap_pretty(ser.serialize_base64(&parsed));
                    println!("\x1b[38;5;2mserialized\x1b[0m {}", parsed.tags.get("name").unwrap());
                    let parsed2 = unwrap_pretty(ser.deserialize_base64(&unparsed));
                    println!("\x1b[38;5;2mredeserialized\x1b[0m {}", parsed.tags.get("name").unwrap());
                    if parsed != parsed2 {
                        #[cfg(feature = "bin")]
                        parsed2.render().save("p2.png");
                        #[cfg(feature = "bin")]
                        parsed.render().save("p1.png");
                        panic!("DIFFERENT! see `p1.png` != `p2.png`")
                    }
                )*
            }
        };
    }

    test_schem! {
        ser_de,
        "bXNjaAF4nCVNy07DMBCcvC1c4MBnoNz4G8TBSSxRycSRbVr646iHlmUc2/KOZ3dmFo9QDdrVfFkMb9Gsi5mgFxvncNzS0a8Aemcm6yLq948Bz2eTbBjtTwpmTj7gafs00Y6zX0/2Qt6dzLdLeNj8mbrVLxZ6ciamcQlH59BHH5iAYTKJeOGCF6AisFSoBxF55V+hJm1Lvwca8lpVIuzlS0eGLoMqTGUG6OLRJes3Mw40E5ijc2QedkPuU3DfLX0eHriDsgMapaScu9zkT26o5Uq8EmV/zS5vi4tr/wHvJE7M";
        "bXNjaAF4nE2MzWrEMAyEJ7bjdOnPobDQvfUF8kSlhyTWFlOv3VWcQvv0lRwoawzSjL4ZHOAtXJ4uhEdi+oz8ek5bDCvuA60Lx68aSwbg0zRTWmHe3j2emWI+F14ojEvJYYsVD5RoqVzSzy8xDjNNlzGXQHi5gVO8SvnIZasCnW4uM8fwQf9tT9+Ua1OUV0GBI9ozHToY6IeDtaIACxkOnaoe1rVrg2RV1cP0CuycLA5+LxuUU+U055W0Yrb4sEcGNQ3u1NTh9iHmH6qaOTI=";
        "bXNjaAF4nE2R226kQAxEzW1oYC5kopV23/IDfMw+R3ng0klaYehsD6w2+fqtamuiDILCLtvH9MgPaTLJl/5ipR3cy4MN9s2FB//PTVaayV7H4N5X5xcR2c39YOerpI9Pe/kVrFuefRjt1A3BTS+2G/0ybW6V41+7rDGyy9UGjNnGtQt+W78C7ZCcgVSD7S/d4kH8+W3q7P5sbrr1nb85N9DeznZcg58/PlFxx6V77tqNr/1lQOr0anuQ7eQCCn2QQ6Rvy+z7Cb7Ib9xSSJpICsGDV5bxoVJKxpSRLIdUKrVkBQoSkVxYJDuWq5SaNByboUEYJ5LgmFlZRhdejit6oDO5Uw/trDTqgWfgpCqFiiG91MVL7IJfLKck3NooyBDEZM4Gw+9jtJOEXgQZ7TQAJZSaM+POFd5TSWpIoVHEVsqrlUcX8xq+U2pi94wyCHZpICn625jAGdVy4DxGpdom2gXeKu2OIw+6w5E7UABnMgKO9CgxOukiHBGjmGz1dFp+VQO57cA7pUR4+wVvFd5q9x2aQT0r/Ew4k/FfPyvunjhGaPgPoVJdLw==";
        "bXNjaAF4nD1TDUxTVxS+r6+vr30triCSVjLXiulKoAjMrJRkU8b4qSgLUAlIZ1rah7yt9L31h1LMMCNbRAQhYrKwOnEslhCcdmzJuohL1DjZT4FJtiVsoG5LFtzmGgGngHm790mam7x77ne+c945934HKIAcB2K3vYUGScXmWvM+TQ3jYhysG8idtNfhYTgfAw8ASFz2RtrlBaKG12VA6X1KMjg8fgfT6KLBJi7osfsYH21oYdpoD6A4NkB7DG7WSQOJl/X4IPYM426loeU0bABSv9vF2p3I1cI4PKyB87AO2gu9gGi1+10+kMTCiCYXGzActvtoWEY+ABhcIgzaOBCJ4EZICYDx6xAV86vCdx2IAS5QJJAEIRkQ4XAjAHSIIITBUCCGRwIuESCgheEIkwgYIpEAF4I3wSw9bWccTpvNmVkZy5raWT1p3r+vajJ2odyQb+HAW9HxvV556vfvpNy4oVUfDyq36Kyqe73xsdemprMyv52uAreYwcXzJaPU+aDp8fFM24nuzUvVqYo9yr7CjFT/aDDzUUz8S8W7g+X3VCpVnargblNubl4kI1q6J+cFPH2HS6VSF5xzZWhCyYCKO2FjqAEprB9WRsJbwNFFoLKhITRCQheBbByQCMAQQwow1I8M9oPJ2870npqvvq5RvvfFyYE3hjrLmst3TixrV0XSN08Uax/UrMSeHdmKDdj8uhh3Pef2Wa+qDljrj82pK+aM300sl0eTrC/rL3zzZKZhRWFMq+mLvvTZb0bbweGZL/85ywwnl4RLzR9MBdIGy0LJowOWHxoOY2EiaJ/7s7ZP0Tg2wjWb3y6Lm3IPRNonw/0yT/+lZsdFy/LmUEp2RojHl68B41zDx43WJ/qANkwdVOvGtxjzpgo/keUURn2XK6zerz9Km10w3Vb8Ww/t/UdmHyx7fXwEcPiP0w1Xx9f+/m/X/d13Wiees8yPnk69ePlS9Yuf9sQf1dvVB27mm68U+51Fj7emzS+mzw1jzwuvTKFXHoK30l9EXctVlozIiSPdpk5djrW965BmV1XW4qsp8kNXmtWztdklXXTa0u6lO0d1+GS3TV/Q95O+17+S23Hs5sIfP4e/uqvd9oo+p7u0cYiPb4+9f/L+Qn3PmuXDdDai/ev0ts69I9nuNTOXp9HfOmoy/a5Y9D2cYYsebq+cKgB1V9vXdYFfOz7vWiVCLNnUUVkLOGO9umVN0jl2KoIjYSINEzgUORoDBKAnJwSLTLikQOBSAoC0ABBAbMgDWYIuBBeFRE7CbBCXCAwxFBAJPRgCSAFADBlykokcZCKHFAkPbSRKRaFUUsRGUyZLTJksMWWyjSlDJKhfFALZmFAJdFPo1+gkQVKXw/EW8/zToeZ5fh0t/H+V6k8+";
        "bXNjaAF4nGNgZ2BjZmDJS8xNZRByrkzOyc9LdctJLEpVT1F4v3AaA3dKanFyUWZBSWZ+HgMDA1tOYlJqTjEDU3QsFwN/eWJJapFuakVJUWJySX4RA3tSYglQpJKBPRliEgNXQX45UElefkoqA39SUWZKeqpucn5eWWolSDmQlVKaWcIgkpyfm1RaDLJDNz01L7UoEWQaf3JRZX5aTmlmim5uZkUqUCA3M7koX7egKD85tbgYqIIlIzEzB+gqQQYwYGYEEkwgxMjAAuQCKSYOZgam//8ZWP7/+/+HgZGZBSTPDlEGVMEKVssAooAMNqAWBpA0SCdQKTMDMzvEZAZGRhCXBZXLyv7///8cIC4AKgZaCOKGAHEh0DBWBpAKIAW0hQNkAR9Q16+KOXtDbhfNNhBQneu5XNV+o/0FSYFCtbrHC+dm3v/MnK3TnKUYGCv0+X3uygksNwzr3jbr755T/O3NuiOxk+7YX7lSoNb3oW3CUq7vKxq4bn1rUKqJsqldfsLX2KkoICQ679L8bW8fCLaY3K+LfGLIe6hoXlaW3iMvrsUq7Hc9Mq1W/OrydlRf+VK9+Ov1iSmsK1deCPKVPF69dG+I5RQ3qSf2PLmlH2bkLwgT4Q3V5+3qnBDPqcG1dNuqZfETim+6G0UqT9b5bGsUznqqb7GZxoxc5eUMT/JvvC79UdlruPvxJis9XhbeTZXLN+68WFB41+DkNRv7uZEGOr/2rvPPu8ZfyXRLI+zoUnmLXRu3+nz0EnP1Omq39TLX3c23cleZ8F62FSnMVCviO2H6tWXB7z2LpA02vleTOXiJK20BT+ADsencfQd0tlqrfQuoWut5dHaX1OP/KwIY5r66Zp4T9+2p241L0XvPfu5w/Zu3bNX77V89kt42zOLf9jJ9vk+msV1vy/Knlywv7Lh4NEY7fvHay0t3Sxo+2q918+je/O23P+50/qEWe45XqGzaR1vh0h1idRwBYZter2DKPJv559VDhbXSHzgin2x8PeXIKsvmtRIVB5V5b/1zcBP+f7VpfuP1OLcJKiePP7n8paroYrul0uF88dp5619+MN8Z7WT0p7DTUqftYOt3XqN51hGf+IVDH0UwcDKwAZMFMwCWiVNe";
        "bXNjaAF4nGNgZGBkZmDJS8xNZeBMrShIzSvOLEtl4E5JLU4uyiwoyczPY2BgYMtJTErNKWZgio5lZODPzUwuytctKMpPTi0uzi8CyjMygAAfA4PQ+Yo5by9u9GxmZGB9GME502nTzKW+Aht/FJq1ez+o8nzYGn5n+wHR70VVf23t9tutu58/Xbm+qr5t/v+PAa93zIn+L1BpFbXfY17fNf1Jyxd/7X7yMuOv0qjQqNCo0KjQqNCo0KjQqNCo0KjQqNCo0KjQqNCo0KjQqNCoEJWFHp987V9uXjv/9y4GAOhu6pc=";
        "bXNjaAF4nGNgY2BjZmDJS8xNZWBLTswrSyxm4E5JLU4uyiwoyczPY2BgYMtJTErNKWZgio5lhKthYOBkAAE+IDZjIB8wUWoAC2UGMFHqBSaoF1QYGTycJjFMUFHxVPBkmpQyiYXhpAonQ4OnEAPDJBVWBhXPW0wek7bkTlRhvLXNk4khdzYLQ8M2sAEUeoGFUi+wUBoLLJR5AQDzuCAp";
        "bXNjaAF4nEWNQRLCIAxFf5O0LhxdewlP5LighQUzCIyl97chVmHx8nmZDyYIQ7J7BUgqruLsw7q8Y22xZABTcnNIK+jxZJyWkv0WGy51S2u4H/Fak2vB/zJww/8MIAVZYh2Gw+jtCx2s+O7pE6nZB0V3bD1sTqtITe8Uc2JOzIm50RpH/U9Bht19AOy5Ge4=";
        "bXNjaAF4nBXKPQ6AIAwG0I+fuLjrKTyRcUDo0ASKKdXzq8kbHwJCQJTUCLGxEOZCIytfxl0ATDWdVAf8fjgsqRQ2fmhTyl2G6Z2t69fcz62I/gVp0BSJ";
    }

    #[test]
    fn block_iter() {
        macro_rules! test_iter {
            ($it:ident, $($val:expr;)*) => {
                $(assert_eq!($it.next().map(|(pos, p)| (pos, p.block)), $val);)*
            };
        }
        macro_rules! pair {
            ($x:literal,$y:literal,$v:expr) => {
                Some((GridPos($x, $y), &$v))
            };
            () => {
                None
            };
        }
        use crate::block::all::*;
        let mut s = Schematic::new(3, 3);
        s.put(0, 0, &DISTRIBUTOR)
            .put(0, 1, &JUNCTION)
            .put(1, 1, &PHASE_CONVEYOR)
            .put(2, 0, &ROUTER)
            .put(1, 1, &CONVEYOR);
        let mut it = s.block_iter();
        test_iter![
            it,
            pair!(0, 0, DISTRIBUTOR);
            pair!(0, 1, JUNCTION);
            pair!(1, 1, CONVEYOR);
            pair!(2, 0, ROUTER);
            pair!( );
        ];
        let reg = crate::block::build_registry();
        let mut s = SchematicSerializer(&reg);

        let s = s.deserialize_base64("bXNjaAF4nDXKywqAIBQA0fFRBH1itDC7C8E01IT+vgia1VkMFmOwyR3C0N0VG/Mu1ZdwtpATMEa3SazoZdVMPqcudy7/DJovpV4peAAt0xF6").unwrap();
        let mut it = s.block_iter();
        test_iter![it,
            pair!(0, 0, CONVEYOR);
            pair!(2, 1, VAULT);
            pair!();
        ];
    }
}
