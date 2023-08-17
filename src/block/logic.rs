//! logic processors and stuff
use std::borrow::Cow;
use std::string::FromUtf8Error;

use crate::block::simple::*;
use crate::data::dynamic::{DynSerializer, DynType};
use crate::{block::*, Serializer};

use crate::data::{self, CompressError, DataRead, DataWrite};

make_simple!(
    MemoryBlock =>
    |_, _, buff: &mut DataRead| {
        // format:
        // - iterate [`u32`]
        //     - memory: [`f64`]
        let n = buff.read_u32()? as usize;
        buff.skip(n * 8)
    }
);

make_register! {
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
    // editor only
    "world-processor" -> BasicBlock::new(1, true, &[]);
    "world-message" -> MessageLogic::new(1, true, &[]);
    "world-cell" -> MemoryBlock::new(1, true, &[]);
}

pub struct CanvasBlock {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
    canvas_size: u8,
}

macro_rules! h {
    ($x:literal) => {{
        let v = color_hex::color_from_hex!($x);
        (v[0], v[1], v[2])
    }};
}
const PALETTE: &[(u8, u8, u8); 8] = &[
    h!("#362944"),
    h!("#c45d9f"),
    h!("#e39aac"),
    h!("#f0dab1"),
    h!("#6461c2"),
    h!("#2ba9b4"),
    h!("#93d4b5"),
    h!("#f0f6e8"),
];

impl CanvasBlock {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost, canvas_size: u8) -> Self {
        assert!(size != 0, "invalid size");
        assert!(canvas_size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
            canvas_size,
        }
    }

    state_impl!(pub Image<Vec<u8>, 1>);
}

fn deser_canvas_image(b: &[u8], size: usize) -> Image<Vec<u8>, 1> {
    let mut p = Image::alloc(size as u32, size as u32);
    for i in 0..(size * size) {
        let offset = i * 3;
        let mut n = 0;
        for i in 0..3 {
            let word = (i + offset) >> 3;
            n |= (((b[word] & (1 << ((i + offset) & 7))) != 0) as u8) << i;
        }
        unsafe { p.set_pixel(i as u32 % size as u32, i as u32 / size as u32, [n]) };
    }
    p
}

impl BlockLogic for CanvasBlock {
    impl_block!();

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Empty)
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::ByteArray(b) => Ok(Some(Self::create_state(deser_canvas_image(
                &b,
                self.canvas_size as usize,
            )))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::String,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        let mut o = vec![0; self.canvas_size as usize * self.canvas_size as usize * 3];
        let p = Self::get_state(state);
        for i in 0..(self.canvas_size * self.canvas_size) as usize {
            let index = unsafe {
                p.pixel(
                    i as u32 % self.canvas_size as u32,
                    i as u32 / self.canvas_size as u32,
                )[0]
            };
            let offset = i * 3;
            for i in 0..3 {
                let word = (i + offset) >> 3;
                if index >> i & 1 == 0 {
                    o[word] &= !(1 << ((i + offset) & 7));
                } else {
                    o[word] |= 1 << ((i + offset) & 7);
                }
            }
        }
        Ok(DynData::ByteArray(o))
    }

    /// i thought about drawing the borders and stuff but it felt like too much work
    fn draw(
        &self,
        _: &str,
        state: Option<&State>,
        _: Option<&RenderingContext>,
        _: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        if let Some(state) = state {
            let p = Self::get_state(state);
            let offset = match s {
                Scale::Full => 7,
                // Scale::Half => 3,
                Scale::Quarter => 2,
                Scale::Eigth => 1,
            };
            let mut img = Image::alloc(p.width(), p.height());
            for ([r, g, b, a], &y) in img.chunked_mut().zip(p.buffer.iter()) {
                (*r, *g, *b) = PALETTE[y as usize];
                *a = 255;
            }
            let img = img.as_mut().scale((s * self.size as u32) - offset * 2);
            let mut borders = load!("canvas", s);
            unsafe {
                borders
                    .borrow_mut()
                    .overlay_at(&img.as_ref(), offset, offset)
            };
            return borders;
        }

        let mut def = Image::alloc(s * self.size as u32, s * self.size as u32);
        for [r, g, b, a] in def.buffer.array_chunks_mut::<4>() {
            (*r, *g, *b) = PALETTE[0];
            *a = 255;
        }
        ImageHolder::from(def)
    }

    /// format:
    /// - len: [`i32`]
    /// - read(len) -> [`deser_canvas_image`]
    fn read(
        &self,
        build: &mut Build,
        _: &BlockRegistry,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        let n = buff.read_i32()? as usize;
        let mut b = vec![0; n];
        buff.read_bytes(&mut b)?;
        build.state = Some(Self::create_state(deser_canvas_image(
            &b,
            self.canvas_size as usize,
        )));
        Ok(())
    }
}

pub struct MessageLogic {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl MessageLogic {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub String);
}

impl BlockLogic for MessageLogic {
    impl_block!();

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Empty)
    }

    fn draw(
        &self,
        _: &str,
        _: Option<&State>,
        _: Option<&RenderingContext>,
        _: Rotation,
        _: Scale,
    ) -> ImageHolder<4> {
        unreachable!()
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty | DynData::String(None) => Ok(Some(Self::create_state(String::new()))),
            DynData::String(Some(s)) => Ok(Some(Self::create_state(s))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::String,
            }),
        }
    }

    fn mirror_state(&self, _: &mut State, _: bool, _: bool) {}

    fn rotate_state(&self, _: &mut State, _: bool) {}

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(DynData::String(Some(Self::get_state(state).clone())))
    }

    fn read(
        &self,
        b: &mut Build,
        _: &BlockRegistry,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        b.state = Some(Self::create_state(buff.read_utf()?.to_string()));
        Ok(())
    }
}

pub struct SwitchLogic {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl SwitchLogic {
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

impl BlockLogic for SwitchLogic {
    impl_block!();

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Empty)
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(true))),
            DynData::Boolean(enabled) => Ok(Some(Self::create_state(enabled))),
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Boolean,
            }),
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(DynData::Boolean(*Self::get_state(state)))
    }

    fn read(
        &self,
        build: &mut Build,
        _: &BlockRegistry,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        build.state = Some(Self::create_state(buff.read_bool()?));
        Ok(())
    }

    fn draw(
        &self,
        _: &str,
        state: Option<&State>,
        _: Option<&RenderingContext>,
        _: Rotation,
        s: Scale,
    ) -> ImageHolder<4> {
        let mut base = load!("switch", s);
        if let Some(state) = state {
            if *Self::get_state(state) {
                let on = load!("switch-on", s);
                unsafe { base.overlay(&on) };
                return base;
            }
        }
        base
    }
}

pub struct ProcessorLogic {
    size: u8,
    symmetric: bool,
    build_cost: BuildCost,
}

impl ProcessorLogic {
    #[must_use]
    pub const fn new(size: u8, symmetric: bool, build_cost: BuildCost) -> Self {
        assert!(size != 0, "invalid size");
        Self {
            size,
            symmetric,
            build_cost,
        }
    }

    state_impl!(pub ProcessorState);
}

fn read_decompressed(buff: &mut DataRead) -> Result<ProcessorState, ProcessorDeserializeError> {
    let ver = buff.read_u8()?;
    if ver != 1 {
        return Err(ProcessorDeserializeError::Version(ver));
    }

    let code_len = buff.read_u32()? as usize;
    if !(0..=500 * 1024).contains(&code_len) {
        return Err(ProcessorDeserializeError::CodeLength(code_len));
    }
    let mut code = vec![0; code_len];
    buff.read_bytes(&mut code)?;
    let code = String::from_utf8(code)?;
    let link_cnt = buff.read_u32()? as usize;
    let mut links = vec![];
    links.reserve(link_cnt);
    for _ in 0..link_cnt {
        let name = buff.read_utf()?;
        let x = buff.read_i16()?;
        let y = buff.read_i16()?;
        links.push(ProcessorLink {
            name: String::from(name),
            x,
            y,
        });
    }
    Ok(ProcessorState { code, links })
}

impl BlockLogic for ProcessorLogic {
    impl_block!();

    fn data_from_i32(&self, _: i32, _: GridPos) -> Result<DynData, DataConvertError> {
        Ok(DynData::Empty)
    }

    fn deserialize_state(&self, data: DynData) -> Result<Option<State>, DeserializeError> {
        match data {
            DynData::Empty => Ok(Some(Self::create_state(ProcessorState::default()))),
            DynData::ByteArray(arr) => {
                let input = arr.as_ref();
                let buff = DataRead::new(input).deflate()?;
                Ok(Some(Self::create_state(
                    ProcessorDeserializeError::forward(read_decompressed(&mut DataRead::new(
                        &buff,
                    )))?,
                )))
            }
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Boolean,
            }),
        }
    }

    fn read(
        &self,
        b: &mut Build,
        _: &BlockRegistry,
        buff: &mut DataRead,
    ) -> Result<(), DataReadError> {
        let n = buff.read_u32()? as usize;
        let mut v = vec![0; n];
        buff.read_bytes(&mut v)?;
        v = DataRead::new(&v).deflate().unwrap();
        b.state = Some(Self::create_state(
            read_decompressed(&mut DataRead::new(&v)).unwrap(),
        ));
        for _ in 0..buff.read_u32()? {
            let _ = buff.read_utf()?;
            let _ = DynSerializer.deserialize(buff).unwrap();
        }
        let memory = buff.read_u32()? as usize;
        buff.skip(memory * 8)?;
        Ok(())
    }

    fn mirror_state(&self, state: &mut State, horizontally: bool, vertically: bool) {
        for link in &mut Self::get_state_mut(state).links {
            if horizontally {
                link.x = -link.x;
            }
            if vertically {
                link.y = -link.y;
            }
        }
    }

    fn rotate_state(&self, state: &mut State, clockwise: bool) {
        for link in &mut Self::get_state_mut(state).links {
            let (cdx, cdy) = link.get_pos();
            link.x = if clockwise { cdy } else { -cdy };
            link.y = if clockwise { -cdx } else { cdx };
        }
    }

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        let state = Self::get_state(state);
        let mut rbuff = DataWrite::default();
        ProcessorSerializeError::forward(rbuff.write_u8(1))?;
        assert!(state.code.len() < 500 * 1024);
        ProcessorSerializeError::forward(rbuff.write_i32(state.code.len() as i32))?;
        ProcessorSerializeError::forward(rbuff.write_bytes(state.code.as_bytes()))?;
        assert!(state.links.len() < i32::MAX as usize);
        ProcessorSerializeError::forward(rbuff.write_i32(state.links.len() as i32))?;
        for link in &state.links {
            ProcessorSerializeError::forward(rbuff.write_utf(&link.name))?;
            ProcessorSerializeError::forward(rbuff.write_i16(link.x))?;
            ProcessorSerializeError::forward(rbuff.write_i16(link.y))?;
        }
        let mut out = DataWrite::default();
        rbuff.inflate(&mut out)?;
        Ok(DynData::ByteArray(out.consume()))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ProcessorDeserializeError {
    #[error("failed to read state data")]
    Read(#[from] data::ReadError),
    #[error("malformed utf-8 in processor code")]
    FromUtf8(#[from] FromUtf8Error),
    #[error("unsupported version ({0})")]
    Version(u8),
    #[error("invalid code length ({0})")]
    CodeLength(usize),
}

impl ProcessorDeserializeError {
    pub fn forward<T, E: Into<Self>>(result: Result<T, E>) -> Result<T, DeserializeError> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(DeserializeError::Custom(Box::new(e.into()))),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ProcessorSerializeError {
    #[error("failed to write state data")]
    Write(#[from] data::WriteError),
    #[error(transparent)]
    Compress(#[from] CompressError),
}

impl ProcessorSerializeError {
    pub fn forward<T, E: Into<Self>>(result: Result<T, E>) -> Result<T, SerializeError> {
        match result {
            Ok(v) => Ok(v),
            Err(e) => Err(SerializeError::Custom(Box::new(e.into()))),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct ProcessorLink {
    name: String,
    x: i16,
    y: i16,
}

impl ProcessorLink {
    #[must_use]
    pub fn new(name: Cow<'_, str>, x: i16, y: i16) -> Self {
        assert!(
            u16::try_from(name.len()).is_ok(),
            "name too long ({})",
            name.len()
        );
        Self {
            name: name.into_owned(),
            x,
            y,
        }
    }

    #[must_use]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub const fn get_pos(&self) -> (i16, i16) {
        (self.x, self.y)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct ProcessorState {
    code: String,
    links: Vec<ProcessorLink>,
}

impl ProcessorState {
    #[must_use]
    pub fn get_code(&self) -> &str {
        &self.code
    }

    pub fn set_code(&mut self, code: Cow<'_, str>) -> Result<(), CodeError> {
        let as_str = &code as &str;
        if as_str.len() > 500 * 1024 {
            return Err(CodeError::TooLong(as_str.len()));
        }
        match code {
            Cow::Borrowed(s) => {
                self.code.clear();
                self.code.push_str(s);
            }
            Cow::Owned(s) => self.code = s,
        }
        Ok(())
    }

    #[must_use]
    pub fn get_links(&self) -> &[ProcessorLink] {
        &self.links
    }

    pub fn create_link(
        &mut self,
        mut name: String,
        x: i16,
        y: i16,
    ) -> Result<&ProcessorLink, CreateError> {
        if name.len() > u16::MAX as usize {
            return Err(CreateError::NameLength(name.len()));
        }
        for curr in &self.links {
            if name == curr.name {
                return Err(CreateError::DuplicateName(name));
            }
            if x == curr.x && y == curr.y {
                name.clear();
                name.push_str(&curr.name);
                return Err(CreateError::DuplicatePos { name, x, y });
            }
        }
        let idx = self.links.len();
        self.links.push(ProcessorLink { name, x, y });
        Ok(&self.links[idx])
    }

    pub fn add_link(&mut self, link: ProcessorLink) -> Result<&ProcessorLink, CreateError> {
        self.create_link(link.name, link.x, link.y)
    }

    pub fn remove_link(&mut self, idx: usize) -> Option<ProcessorLink> {
        if idx < self.links.len() {
            Some(self.links.remove(idx))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum CodeError {
    #[error("code too long ({0} bytes)")]
    TooLong(usize),
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum CreateError {
    #[error("link name too long ({0} bytes)")]
    NameLength(usize),
    #[error("there is already a link named {0}")]
    DuplicateName(String),
    #[error("link {name} already points to ({x}, {y})")]
    DuplicatePos { name: String, x: i16, y: i16 },
}
