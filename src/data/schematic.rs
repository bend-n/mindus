//! schematic parsing
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::iter::FusedIterator;
use std::slice::Iter;
use thiserror::Error;

use crate::block::{self, Block, BlockRegistry, Rotation, State};
use crate::data::base64;
use crate::data::dynamic::{self, DynData, DynSerializer};
use crate::data::renderer::*;
use crate::data::{self, DataRead, DataWrite, GridPos, Serializer};
use crate::item::storage::ItemStorage;
use crate::registry::RegistryEntry;

/// biggest schematic
pub const MAX_DIMENSION: u16 = 256;
/// most possible blocks
pub const MAX_BLOCKS: u32 = 256 * 256;

/// a placement in a schematic
pub struct Placement<'l> {
    pub pos: GridPos,
    pub block: &'l Block,
    pub rot: Rotation,
    state: Option<State>,
}

impl PartialEq for Placement<'_> {
    fn eq(&self, rhs: &Placement<'_>) -> bool {
        self.pos == rhs.pos && self.block == rhs.block && self.rot == rhs.rot
    }
}

impl fmt::Debug for Placement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "Placement<{},{}>+{}>",
            self.pos.0,
            self.pos.1,
            self.block.name()
        )
    }
}

impl<'l> Placement<'l> {
    /// gets the current state of this placement. you can cast it with `placement.block::get_state(placement.get_state()?)?`
    #[must_use]
    pub fn get_state(&self) -> Option<&State> {
        self.state.as_ref()
    }

    /// get mutable state.
    pub fn get_state_mut(&mut self) -> Option<&mut State> {
        self.state.as_mut()
    }

    /// draws this placement in particular
    pub fn image(&self, context: Option<&RenderingContext>) -> ImageHolder {
        self.block.image(self.get_state(), context)
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
    fn get_rotation(&self) -> Rotation {
        self.rot
    }
}

impl PositionState for Placement<'_> {
    fn get_position(&self) -> GridPos {
        self.pos
    }
}

// manual impl because trait objects cannot be cloned
impl<'l> Clone for Placement<'l> {
    fn clone(&self) -> Self {
        Self {
            pos: self.pos,
            block: self.block,
            state: match self.state {
                None => None,
                Some(ref s) => Some(self.block.clone_state(s)),
            },
            rot: self.rot,
        }
    }
}

#[derive(Clone)]
/// a schematic.
pub struct Schematic<'l> {
    pub width: u16,
    pub height: u16,
    pub tags: HashMap<String, String>,
    pub blocks: Vec<Placement<'l>>,
    lookup: Vec<Option<usize>>,
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
    pub fn new(width: u16, height: u16) -> Self {
        match Self::try_new(width, height) {
            Ok(s) => s,
            Err(NewError::Width(w)) => panic!("invalid schematic width ({w})"),
            Err(NewError::Height(h)) => panic!("invalid schematic height ({h})"),
        }
    }

    /// create a new schematic, erroring if too big
    /// ```
    /// # use mindus::Schematic;
    /// assert!(Schematic::try_new(500, 500).is_err() == true);
    /// ```
    pub fn try_new(width: u16, height: u16) -> Result<Self, NewError> {
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
            blocks: Vec::new(),
            lookup: Vec::new(),
        })
    }

    #[must_use]
    /// have blocks?
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }

    #[must_use]
    /// count blocks
    pub fn get_block_count(&self) -> usize {
        self.blocks.len()
    }

    #[must_use]
    /// check if a rect is empty
    /// ```
    /// # use mindus::Schematic;
    /// let s = Schematic::new(5, 5);
    /// assert!(s.is_region_empty(1, 1, 4, 4) == true);
    /// ```
    pub fn is_region_empty(&self, x: u16, y: u16, w: u16, h: u16) -> bool {
        if self.blocks.is_empty() {
            return true;
        }
        if x >= self.width || y >= self.height || w == 0 || h == 0 {
            return true;
        }
        if w > 1 || h > 1 {
            let stride = self.width as usize;
            let x_end = if self.width - x > w {
                x + w
            } else {
                self.width
            } as usize;
            let y_end = if self.height - y > h {
                y + h
            } else {
                self.height
            } as usize;
            let x = x as usize;
            let y = y as usize;
            for cy in y..y_end {
                for cx in x..x_end {
                    if self.lookup[cx + cy * stride].is_some() {
                        return false;
                    }
                }
            }
            true
        } else {
            self.lookup[(x as usize) + (y as usize) * (self.width as usize)].is_none()
        }
    }

    /// gets a block
    pub fn get(&self, x: u16, y: u16) -> Result<Option<&Placement<'l>>, PosError> {
        if x >= self.width || y >= self.height {
            return Err(PosError {
                x,
                y,
                w: self.width,
                h: self.height,
            });
        }
        if self.blocks.is_empty() {
            return Ok(None);
        }
        let pos = (x as usize) + (y as usize) * (self.width as usize);
        match self.lookup[pos] {
            None => Ok(None),
            Some(idx) => Ok(Some(&self.blocks[idx])),
        }
    }

    /// gets a block, mutably
    pub fn get_mut(&mut self, x: u16, y: u16) -> Result<Option<&mut Placement<'l>>, PosError> {
        if x >= self.width || y >= self.height {
            return Err(PosError {
                x,
                y,
                w: self.width,
                h: self.height,
            });
        }
        if self.blocks.is_empty() {
            return Ok(None);
        }
        let pos = (x as usize) + (y as usize) * (self.width as usize);
        match self.lookup[pos] {
            None => Ok(None),
            Some(idx) => Ok(Some(&mut self.blocks[idx])),
        }
    }

    fn remove(&mut self, idx: usize) -> Placement<'l> {
        // swap_remove not only avoids moves in self.blocks but also reduces the lookup changes we have to do
        let prev = self.blocks.swap_remove(idx);
        self.fill_lookup(
            prev.pos.0 as usize,
            prev.pos.1 as usize,
            prev.block.get_size() as usize,
            None,
        );
        if idx < self.blocks.len() {
            // fix the swapped block's lookup entries
            let swapped = &self.blocks[idx];
            self.fill_lookup(
                swapped.pos.0 as usize,
                swapped.pos.1 as usize,
                swapped.block.get_size() as usize,
                Some(idx),
            );
        }
        prev
    }

    fn fill_lookup(&mut self, x: usize, y: usize, sz: usize, val: Option<usize>) {
        if self.lookup.is_empty() {
            self.lookup
                .resize((self.width as usize) * (self.height as usize), None);
        }
        if sz > 1 {
            let off = (sz - 1) / 2;
            let (x0, y0) = (x - off, y - off);
            for dy in 0..sz {
                for dx in 0..sz {
                    self.lookup[(x0 + dx) + (y0 + dy) * (self.width as usize)] = val;
                }
            }
        } else {
            self.lookup[x + y * (self.width as usize)] = val;
        }
    }

    /// put a block in (same as [Schematic::set], but less arguments)
    /// ```
    /// # use mindus::Schematic;
    /// # use mindus::DynData;
    /// # use mindus::block::Rotation;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// s.put(0, 0, &mindus::block::distribution::ROUTER);
    /// assert!(s.get(0, 0).unwrap().is_some() == true);
    /// ```
    pub fn put(&mut self, x: u16, y: u16, block: &'l Block) -> Result<&Placement<'l>, PlaceError> {
        self.set(x, y, block, DynData::Empty, Rotation::Up)
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
        x: u16,
        y: u16,
        block: &'l Block,
        data: DynData,
        rot: Rotation,
    ) -> Result<&Placement<'l>, PlaceError> {
        let sz = u16::from(block.get_size());
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
        if self.is_region_empty(x - off, y - off, sz, sz) {
            let idx = self.blocks.len();
            let state = block.deserialize_state(data)?;
            self.blocks.push(Placement {
                pos: GridPos(x, y),
                block,
                state,
                rot,
            });
            self.fill_lookup(x as usize, y as usize, block.get_size() as usize, Some(idx));
            Ok(&self.blocks[idx])
        } else {
            Err(PlaceError::Overlap { x, y })
        }
    }

    pub fn replace(
        &mut self,
        x: u16,
        y: u16,
        block: &'l Block,
        data: DynData,
        rot: Rotation,
        collect: bool,
    ) -> Result<Option<Vec<Placement<'l>>>, PlaceError> {
        let sz = u16::from(block.get_size());
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
        if sz > 1 {
            let mut result = if collect { Some(Vec::new()) } else { None };
            // remove all blocks in the region
            for dy in 0..(sz as usize) {
                for dx in 0..(sz as usize) {
                    if let Some(idx) =
                        self.lookup[(x as usize + dx) + (y as usize + dy) * (self.width as usize)]
                    {
                        let prev = self.remove(idx);
                        if let Some(ref mut v) = result {
                            v.push(prev);
                        }
                    }
                }
            }
            let idx = self.blocks.len();
            let state = block.deserialize_state(data)?;
            self.blocks.push(Placement {
                pos: GridPos(x, y),
                block,
                state,
                rot,
            });
            self.fill_lookup(x as usize, y as usize, sz as usize, Some(idx));
            Ok(result)
        } else {
            let pos = (x as usize) + (y as usize) * (self.width as usize);
            match self.lookup[pos] {
                None => {
                    let idx = self.blocks.len();
                    let state = block.deserialize_state(data)?;
                    self.blocks.push(Placement {
                        pos: GridPos(x, y),
                        block,
                        state,
                        rot,
                    });
                    self.lookup[pos] = Some(idx);
                    Ok(if collect { Some(Vec::new()) } else { None })
                }
                Some(idx) => {
                    let state = block.deserialize_state(data)?;
                    let prev = std::mem::replace(
                        &mut self.blocks[idx],
                        Placement {
                            pos: GridPos(x, y),
                            block,
                            state,
                            rot,
                        },
                    );
                    self.fill_lookup(
                        prev.pos.0 as usize,
                        prev.pos.1 as usize,
                        prev.block.get_size() as usize,
                        None,
                    );
                    self.fill_lookup(x as usize, y as usize, sz as usize, Some(idx));
                    Ok(if collect { Some(vec![prev]) } else { None })
                }
            }
        }
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
    pub fn take(&mut self, x: u16, y: u16) -> Result<Option<Placement<'l>>, PosError> {
        if x >= self.width || y >= self.height {
            return Err(PosError {
                x,
                y,
                w: self.width,
                h: self.height,
            });
        }
        if self.blocks.is_empty() {
            Ok(None)
        } else {
            let pos = (x as usize) + (y as usize) * (self.width as usize);
            match self.lookup[pos] {
                None => Ok(None),
                Some(idx) => Ok(Some(self.remove(idx))),
            }
        }
    }

    fn rebuild_lookup(&mut self) {
        self.lookup.clear();
        if !self.blocks.is_empty() {
            self.lookup
                .resize((self.width as usize) * (self.height as usize), None);
            for (i, curr) in self.blocks.iter().enumerate() {
                let sz = curr.block.get_size() as usize;
                let x = curr.pos.0 as usize - (sz - 1) / 2;
                let y = curr.pos.1 as usize - (sz - 1) / 2;
                if sz > 1 {
                    for dy in 0..sz {
                        for dx in 0..sz {
                            self.lookup[(x + dx) + (y + dy) * (self.width as usize)] = Some(i);
                        }
                    }
                } else {
                    self.lookup[x + y * (self.width as usize)] = Some(i);
                }
            }
        }
    }

    /// flip it
    pub fn mirror(&mut self, horizontally: bool, vertically: bool) {
        if !self.blocks.is_empty() && (horizontally || vertically) {
            for curr in &mut self.blocks {
                // because position is the bottom left of the center (which changes during mirroring)
                let shift = (u16::from(curr.block.get_size()) - 1) % 2;
                if horizontally {
                    curr.pos.0 = self.width - 1 - curr.pos.0 - shift;
                }
                if vertically {
                    curr.pos.1 = self.height - 1 - curr.pos.1 - shift;
                }
                if !curr.block.is_symmetric() {
                    curr.rot.mirror(horizontally, vertically);
                }
                if let Some(ref mut state) = curr.state {
                    curr.block.mirror_state(state, horizontally, vertically);
                }
            }
            self.rebuild_lookup();
        }
    }

    /// turn
    /// ```
    /// # use mindus::Schematic;
    /// # use mindus::DynData;
    /// # use mindus::block::Rotation;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// // 0, 0 == bottom left
    /// s.put(0, 0, &mindus::block::turrets::HAIL);
    /// s.rotate(true);
    /// assert!(s.get(0, 4).unwrap().is_some() == true);
    /// ```
    pub fn rotate(&mut self, clockwise: bool) {
        let w = self.width;
        let h = self.height;
        self.width = h;
        self.height = w;
        if !self.blocks.is_empty() {
            for curr in &mut self.blocks {
                let x = curr.pos.0;
                let y = curr.pos.1;
                // because position is the bottom left of the center (which changes during rotation)
                let shift = (u16::from(curr.block.get_size()) - 1) % 2;
                if clockwise {
                    curr.pos.0 = y;
                    curr.pos.1 = w - 1 - x - shift;
                } else {
                    curr.pos.0 = h - 1 - y - shift;
                    curr.pos.1 = x;
                }
                if !curr.block.is_symmetric() {
                    curr.rot.rotate(clockwise);
                }
                if let Some(ref mut state) = curr.state {
                    curr.block.rotate_state(state, clockwise);
                }
            }
            self.rebuild_lookup();
        }
    }

    /// resize this schematic
    pub fn resize(&mut self, dx: i16, dy: i16, w: u16, h: u16) -> Result<(), ResizeError> {
        if w > MAX_DIMENSION {
            return Err(ResizeError::TargetWidth(w));
        }
        if h > MAX_DIMENSION {
            return Err(ResizeError::TargetHeight(h));
        }
        if dx <= -(w as i16) || dx >= self.width as i16 {
            return Err(ResizeError::XOffset {
                dx,
                old_w: self.width,
                new_w: w,
            });
        }
        if dy <= -(h as i16) || dy >= self.height as i16 {
            return Err(ResizeError::YOffset {
                dy,
                old_h: self.height,
                new_h: h,
            });
        }
        // check that all blocks fit into the new bounds
        let mut right = 0u16;
        let mut top = 0u16;
        let mut left = 0u16;
        let mut bottom = 0u16;
        let right_bound = dx + w as i16 - 1;
        let top_bound = dy + h as i16 - 1;
        let left_bound = dx;
        let bottom_bound = dy;
        for Placement { pos, block, .. } in &self.blocks {
            let sz = u16::from(block.get_size());
            let (x0, y0, x1, y1) = (
                pos.0 - (sz - 1) / 2,
                pos.1 - (sz - 1) / 2,
                pos.0 + sz / 2,
                pos.1 + sz / 2,
            );
            if (x1 as i16) > right_bound && x1 - right_bound as u16 > right {
                right = x1 - right_bound as u16;
            }
            if (y1 as i16) > top_bound && y1 - top_bound as u16 > top {
                top = y1 - top_bound as u16;
            }
            if (x0 as i16) < left_bound && left_bound as u16 - x0 > left {
                left = left_bound as u16 - x0;
            }
            if (y0 as i16) < bottom_bound && bottom_bound as u16 - y0 > bottom {
                bottom = bottom_bound as u16 - y0;
            }
        }
        if left > 0 || top > 0 || right > 0 || bottom > 0 {
            return Err(TruncatedError {
                right,
                top,
                left,
                bottom,
            })?;
        }
        self.width = w;
        self.height = h;
        for Placement { pos, .. } in &mut self.blocks {
            pos.0 = (pos.0 as i16 + dx) as u16;
            pos.1 = (pos.1 as i16 + dy) as u16;
        }
        Ok(())
    }

    /// like rotate(), but 180
    pub fn rotate_180(&mut self) {
        self.mirror(true, true);
    }

    #[must_use]
    pub fn pos_iter(&self) -> PosIter {
        PosIter {
            x: 0,
            y: 0,
            w: self.width,
            h: self.height,
        }
    }

    /// iterate over all the blocks
    pub fn block_iter<'s>(&'s self) -> Iter<'s, Placement<'l>> {
        self.blocks.iter()
    }

    #[must_use]
    /// see how much this schematic costs.
    /// ```
    /// # use mindus::Schematic;
    /// # use mindus::DynData;
    /// # use mindus::block::Rotation;
    ///
    /// let mut s = Schematic::new(5, 5);
    /// s.put(0, 0, &mindus::block::turrets::CYCLONE);
    /// // assert_eq!(s.compute_total_cost().0.get_total(), 405);
    /// ```
    pub fn compute_total_cost(&self) -> (ItemStorage, bool) {
        let mut cost = ItemStorage::new();
        let mut sandbox = false;
        for &Placement { block, .. } in &self.blocks {
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
    Width(u16),
    #[error("invalid schematic height ({0})")]
    Height(u16),
}
/// error created by doing stuff out of bounds
#[derive(Copy, Clone, Debug, Eq, PartialEq, Error)]
#[error("position {x} / {y} out of bounds {w} / {h}")]
pub struct PosError {
    pub x: u16,
    pub y: u16,
    pub w: u16,
    pub h: u16,
}

#[derive(Debug, Error)]
pub enum PlaceError {
    #[error("invalid block placement {x} / {y} (size {sz}) within {w} / {h}")]
    Bounds {
        x: u16,
        y: u16,
        sz: u8,
        w: u16,
        h: u16,
    },
    #[error("overlapping an existing block at {x} / {y}")]
    Overlap { x: u16, y: u16 },
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

impl fmt::Debug for Schematic<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<'l> fmt::Display for Schematic<'l> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        /*
        Because characters are about twice as tall as they are wide, two are used to represent a single block.
        Each block has a single letter to describe what it is + an optional rotation.
        For size-1 blocks, that's "*]" for symmetric and "*>", "*^", "*<", "*v" for rotations.
        Larger blocks are formed using pipes, slashes and minuses to form a border, which is filled with spaces.
        Then, the letter is placed inside followed by the rotation (if any).
        */

        // find unique letters for each block, more common blocks pick first
        let mut name_cnt = HashMap::<&str, u16>::new();
        for p in &self.blocks {
            match name_cnt.entry(p.block.get_name()) {
                Entry::Occupied(mut e) => *e.get_mut() += 1,
                Entry::Vacant(e) => {
                    e.insert(1);
                }
            }
        }
        // only needed the map for counting
        let mut name_cnt = Vec::from_iter(name_cnt);
        name_cnt.sort_by(|l, r| r.1.cmp(&l.1));
        // set for control characters, space, b'*', DEL and b">^<v]/|\\-"
        let mut used = [
            0xFFu8, 0xFFu8, 0xFFu8, 0xFFu8, 0x01u8, 0xA4u8, 0x00u8, 0x50u8, 0x00u8, 0x00u8, 0x00u8,
            0x70u8, 0x00u8, 0x00u8, 0x40u8, 0x90u8,
        ];
        let mut types = HashMap::<&str, char>::new();
        for &(name, _) in &name_cnt {
            let mut found = false;
            for c in name.chars() {
                if c > ' ' && c <= '~' {
                    let upper = c.to_ascii_uppercase() as usize;
                    let lower = c.to_ascii_lowercase() as usize;
                    if used[upper >> 3] & (1 << (upper & 7)) == 0 {
                        found = true;
                        used[upper >> 3] |= 1 << (upper & 7);
                        types.insert(name, unsafe { char::from_u32_unchecked(upper as u32) });
                        break;
                    }
                    if lower != upper && used[lower >> 3] & (1 << (lower & 7)) == 0 {
                        found = true;
                        used[lower >> 3] |= 1 << (lower & 7);
                        types.insert(name, unsafe { char::from_u32_unchecked(lower as u32) });
                        break;
                    }
                }
            }
            if !found {
                // just take whatever symbol's still free (avoids collisions with letters)
                match used.iter().enumerate().find(|(_, &v)| v != u8::MAX) {
                    // there's no more free symbols... how? use b'*' instead for all of them (reserved)
                    None => {
                        types.insert(name, '*');
                    }
                    Some((i, v)) => {
                        let idx = i + v.trailing_ones() as usize;
                        used[idx >> 3] |= 1 << (idx & 7);
                        types.insert(name, unsafe { char::from_u32_unchecked(idx as u32) });
                    }
                }
            }
        }

        // coordinates start in the bottom left, so y starts at self.height - 1
        if self.blocks.is_empty() {
            write!(f, "<empty {} * {}>", self.width, self.height)?;
        } else {
            for y in (0..self.height as usize).rev() {
                let mut x = 0usize;
                while x < self.width as usize {
                    if let Some(idx) = self.lookup[x + y * (self.width as usize)] {
                        let Placement {
                            pos,
                            block,
                            state: _,
                            rot,
                        } = self.blocks[idx];
                        let c = *types.get(block.get_name()).unwrap();
                        match block.get_size() as usize {
                            0 => unreachable!(),
                            1 => {
                                f.write_char(c)?;
                                match rot {
                                    _ if block.is_symmetric() => f.write_char(']')?,
                                    Rotation::Right => f.write_char('>')?,
                                    Rotation::Up => f.write_char('^')?,
                                    Rotation::Left => f.write_char('<')?,
                                    Rotation::Down => f.write_char('v')?,
                                }
                            }
                            s => {
                                let y0 = pos.1 as usize - (s - 1) / 2;
                                if y == y0 + (s - 1) {
                                    // top row, which looks like /---[...]---\
                                    f.write_char('/')?;
                                    if s == 2 {
                                        // label & rotation are in this row
                                        f.write_char(c)?;
                                        match rot {
                                            _ if block.is_symmetric() => f.write_char('-')?,
                                            Rotation::Right => f.write_char('>')?,
                                            Rotation::Up => f.write_char('^')?,
                                            Rotation::Left => f.write_char('<')?,
                                            Rotation::Down => f.write_char('v')?,
                                        }
                                    } else {
                                        // label & rotation are not in this row
                                        for _ in 0..(2 * s - 2) {
                                            f.write_char('-')?;
                                        }
                                    }
                                    f.write_char('\\')?;
                                } else if y == y0 {
                                    // bottom row, which looks like \---[...]---/
                                    f.write_char('\\')?;
                                    for _ in 0..(2 * s - 2) {
                                        f.write_char('-')?;
                                    }
                                    f.write_char('/')?;
                                } else if s > 2 && y == y0 + s / 2 {
                                    // middle row with label
                                    f.write_char('|')?;
                                    for cx in 0..(2 * s - 2) {
                                        if cx == s - 2 {
                                            f.write_char(c)?;
                                        } else if cx == s - 1 {
                                            match rot {
                                                _ if block.is_symmetric() => f.write_char(' ')?,
                                                Rotation::Right => f.write_char('>')?,
                                                Rotation::Up => f.write_char('^')?,
                                                Rotation::Left => f.write_char('<')?,
                                                Rotation::Down => f.write_char('v')?,
                                            }
                                        } else {
                                            f.write_char(' ')?;
                                        }
                                    }
                                    f.write_char('|')?;
                                } else {
                                    // middle row, which looks like |   [...]   |
                                    f.write_char('|')?;
                                    for _ in 0..(2 * s - 2) {
                                        f.write_char(' ')?;
                                    }
                                    f.write_char('|')?;
                                }
                            }
                        }
                        x += block.get_size() as usize;
                    } else {
                        f.write_str("  ")?;
                        x += 1;
                    }
                }
                writeln!(f)?;
            }
            // print the letters assigned to blocks
            for (k, _) in name_cnt {
                let v = *types.get(k).unwrap();
                write!(f, "\n({v}) {k}")?;
            }
        }
        Ok(())
    }
}

const SCHEMATIC_HEADER: u32 =
    ((b'm' as u32) << 24) | ((b's' as u32) << 16) | ((b'c' as u32) << 8) | (b'h' as u32);

/// serde_schematic
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
        let w = buff.read_i16()?;
        let h = buff.read_i16()?;
        if w < 0 || h < 0 || w as u16 > MAX_DIMENSION || h as u16 > MAX_DIMENSION {
            return Err(ReadError::Dimensions(w, h));
        }
        let mut schematic = Schematic::new(w as u16, h as u16);
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
        for curr in &data.blocks {
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
        rbuff.write_i32(data.blocks.len() as i32)?;
        let mut num = 0;
        for curr in &data.blocks {
            rbuff.write_i8(block_map[curr.block.get_name()] as i8)?;
            rbuff.write_u32(u32::from(curr.pos))?;
            let data = match curr.state {
                None => DynData::Empty,
                Some(ref s) => curr.block.serialize_state(s)?,
            };
            DynSerializer.serialize(&mut rbuff, &data)?;
            rbuff.write_u8(curr.rot.into())?;
            num += 1;
        }
        assert_eq!(num, data.blocks.len());
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
    #[error("invalid schematic dimensions ({0} * {1})")]
    Dimensions(i16, i16),
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
}

impl<'l> SchematicSerializer<'l> {
    /// deserializes a schematic from base64
    /// ```
    /// # use mindus::*;
    /// let string = "bXNjaAF4nGNgZmBmZmDJS8xNZeBOyslPzlYAkwzcKanFyUWZBSWZ+XkMDAxsOYlJqTnFDEzRsYwMfAWJlTn5iSm6RfmlJalFQGlGEGJkZWSYxQAAcBkUPA==";
    /// let reg = build_registry();
    /// let mut ss = SchematicSerializer(&reg);
    /// let s = ss.deserialize_base64(string).unwrap();
    /// assert!(s.get(0, 0).unwrap().unwrap().block.name() == "payload-router");
    /// ```
    pub fn deserialize_base64(&mut self, data: &str) -> Result<Schematic<'l>, R64Error> {
        let mut buff = Vec::<u8>::new();
        buff.resize(data.len() / 4 * 3 + 1, 0);
        let n_out = base64::decode(data.as_bytes(), buff.as_mut())?;
        Ok(self.deserialize(&mut DataRead::new(&buff[..n_out]))?)
    }

    /// serialize a schematic to base64
    pub fn serialize_base64(&mut self, data: &Schematic<'l>) -> Result<String, W64Error> {
        let mut buff = DataWrite::default();
        self.serialize(&mut buff, data)?;
        let buff = buff.get_written();
        // round up because of padding
        let required = 4 * (buff.len() / 3 + usize::from(buff.len() % 3 != 0));
        let mut text = Vec::<u8>::new();
        text.resize(required, 0);
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

pub struct PosIter {
    x: u16,
    y: u16,
    w: u16,
    h: u16,
}

impl Iterator for PosIter {
    type Item = GridPos;

    fn next(&mut self) -> Option<Self::Item> {
        if self.w > 0 && self.y < self.h {
            let p = GridPos(self.x, self.y);
            self.x += 1;
            if self.x == self.w {
                self.x = 0;
                self.y += 1;
            }
            Some(p)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let pos = (self.x as usize) + (self.y as usize) * (self.w as usize);
        let end = (self.w as usize) * (self.h as usize);
        (end - pos, Some(end - pos))
    }

    fn count(self) -> usize {
        let pos = (self.x as usize) + (self.y as usize) * (self.w as usize);
        let end = (self.w as usize) * (self.h as usize);
        end - pos
    }

    fn last(self) -> Option<Self::Item> {
        // self.y < self.h implies self.h > 0
        if self.w > 0 && self.y < self.h {
            Some(GridPos(self.w - 1, self.h - 1))
        } else {
            None
        }
    }
}

impl FusedIterator for PosIter {}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_iter {
		($name:ident, $it:expr, $($val:expr),+) => {
			#[test]
			fn $name() {
				let mut it = $it;
				$(test_iter!(impl it, $val);)+
			}
		};
		(impl $it:ident, $val:literal) => {
			for _ in 0..$val {
				assert_ne!($it.next(), None, "iterator returned None too early");
			}
		};
		(impl $it:ident, $val:expr) => {
			assert_eq!($it.next(), $val);
		};
	}

    macro_rules! test_schem {
        ($name:ident, $($val:expr),+) => {
            #[test]
            fn $name() {
                let reg = crate::block::build_registry();
                let mut ser = SchematicSerializer(&reg);
                $(
                    let parsed = ser.deserialize_base64($val).unwrap();
                    println!("{}", parsed.tags.get("name").unwrap());
                    let unparsed = ser.serialize_base64(&parsed).unwrap();
                    let parsed2 = ser.deserialize_base64(&unparsed).unwrap();
                    assert_eq!(parsed, parsed2);
                )*
            }
        };
    }

    test_schem! {
        ser_de,
        "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"
    }

    test_iter!(
        block_iter,
        Schematic::new(3, 4).pos_iter(),
        Some(GridPos(0, 0)),
        Some(GridPos(1, 0)),
        Some(GridPos(2, 0)),
        Some(GridPos(0, 1)),
        7,
        Some(GridPos(2, 3)),
        None
    );
}
