//! logic processors and stuff
use std::borrow::Cow;
use std::string::FromUtf8Error;

use crate::block::simple::*;
use crate::block::*;
use crate::data::dynamic::DynType;
use crate::data::{self, CompressError, DataRead, DataWrite};

make_simple!(LogicBlock);

make_register! {
    "reinforced-message" => MessageLogic::new(1, true, cost!(Graphite: 10, Beryllium: 5));
    "message" => MessageLogic::new(1, true, cost!(Copper: 5, Graphite: 5));
    "switch" => SwitchLogic::new(1, true, cost!(Copper: 5, Graphite: 5));
    "micro-processor" => ProcessorLogic::new(1, true, cost!(Copper: 90, Lead: 50, Silicon: 50));
    "logic-processor" => ProcessorLogic::new(2, true, cost!(Lead: 320, Graphite: 60, Thorium: 50, Silicon: 80));
    "hyper-processor" => ProcessorLogic::new(3, true, cost!(Lead: 450, Thorium: 75, Silicon: 150, SurgeAlloy: 50));
    "memory-cell" => LogicBlock::new(1, true, cost!(Copper: 30, Graphite: 30, Silicon: 30));
    "memory-bank" => LogicBlock::new(2, true, cost!(Copper: 30, Graphite: 80, Silicon: 80, PhaseFabric: 30));
    "logic-display" => LogicBlock::new(3, true, cost!(Lead: 100, Metaglass: 50, Silicon: 50));
    "large-logic-display" => LogicBlock::new(6, true, cost!(Lead: 200, Metaglass: 100, Silicon: 150, PhaseFabric: 75));
    // todo canvas (cost!(Silicon: 30, Beryllium: 10))
    // editor only
    "world-processor" => LogicBlock::new(1, true, &[]);
    "world-message" => MessageLogic::new(1, true, &[]);
    "world-cell" => LogicBlock::new(1, true, &[]);
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

    fn clone_state(&self, state: &State) -> State {
        Box::new(Self::get_state(state).clone())
    }

    fn mirror_state(&self, _: &mut State, _: bool, _: bool) {}

    fn rotate_state(&self, _: &mut State, _: bool) {}

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(DynData::String(Some(Self::get_state(state).clone())))
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

    fn clone_state(&self, state: &State) -> State {
        Box::new(*Self::get_state(state))
    }

    fn mirror_state(&self, _: &mut State, _: bool, _: bool) {}

    fn rotate_state(&self, _: &mut State, _: bool) {}

    fn serialize_state(&self, state: &State) -> Result<DynData, SerializeError> {
        Ok(DynData::Boolean(*Self::get_state(state)))
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
                let mut buff = DataRead::new(&buff);
                let ver = ProcessorDeserializeError::forward(buff.read_u8())?;
                if ver != 1 {
                    return Err(DeserializeError::Custom(Box::new(
                        ProcessorDeserializeError::Version(ver),
                    )));
                }

                let code_len = ProcessorDeserializeError::forward(buff.read_i32())?;
                if !(0..=500 * 1024).contains(&code_len) {
                    return Err(DeserializeError::Custom(Box::new(
                        ProcessorDeserializeError::CodeLength(code_len),
                    )));
                }
                let mut code = Vec::<u8>::new();
                code.resize(code_len as usize, 0);
                ProcessorDeserializeError::forward(buff.read_bytes(&mut code))?;
                let code = ProcessorDeserializeError::forward(String::from_utf8(code))?;
                let link_cnt = ProcessorDeserializeError::forward(buff.read_i32())?;
                if link_cnt < 0 {
                    return Err(DeserializeError::Custom(Box::new(
                        ProcessorDeserializeError::LinkCount(link_cnt),
                    )));
                }
                let mut links = Vec::<ProcessorLink>::new();
                links.reserve(link_cnt as usize);
                for _ in 0..link_cnt {
                    let name = ProcessorDeserializeError::forward(buff.read_utf())?;
                    let x = ProcessorDeserializeError::forward(buff.read_i16())?;
                    let y = ProcessorDeserializeError::forward(buff.read_i16())?;
                    links.push(ProcessorLink {
                        name: String::from(name),
                        x,
                        y,
                    });
                }
                Ok(Some(Self::create_state(ProcessorState { code, links })))
            }
            _ => Err(DeserializeError::InvalidType {
                have: data.get_type(),
                expect: DynType::Boolean,
            }),
        }
    }

    fn clone_state(&self, state: &State) -> State {
        Box::new(Self::get_state(state).clone())
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
    CodeLength(i32),
    #[error("invalid link count {0}")]
    LinkCount(i32),
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
    pub fn get_pos(&self) -> (i16, i16) {
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
