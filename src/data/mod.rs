//! all the IO
use flate2::{
    Compress, CompressError as CError, Compression, Decompress, DecompressError as DError,
    FlushCompress, FlushDecompress, Status,
};
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::str::Utf8Error;
use thiserror::Error;

mod base64;
mod command;
pub mod dynamic;
pub mod map;
pub mod planet;
pub mod renderer;
pub mod schematic;
pub mod sector;
pub mod weather;

#[derive(Debug)]
pub struct DataRead<'d> {
    data: &'d [u8],
    // used with read_chunk
    read: usize,
}

impl fmt::Display for DataRead<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(self.data))
    }
}

macro_rules! make_read {
    ($name:ident, $type:ty) => {
        pub fn $name(&mut self) -> Result<$type, ReadError> {
            const LEN: usize = std::mem::size_of::<$type>();
            if self.data.len() < LEN {
                return Err(ReadError::Underflow {
                    need: LEN,
                    have: self.data.len(),
                });
            }
            let mut output = [0u8; LEN];
            output.copy_from_slice(&self.data[..LEN]);
            self.data = &self.data[LEN..];
            self.read += LEN;
            Ok(<$type>::from_be_bytes(output))
        }
    };
}

impl<'d> DataRead<'d> {
    #[must_use]
    pub fn new(data: &'d [u8]) -> Self {
        Self { data, read: 0 }
    }

    pub fn read_bool(&mut self) -> Result<bool, ReadError> {
        Ok(self.read_u8()? != 0)
    }

    make_read!(read_u8, u8);
    make_read!(read_i8, i8);
    make_read!(read_u16, u16);
    make_read!(read_i16, i16);
    make_read!(read_u32, u32);
    make_read!(read_i32, i32);
    make_read!(read_f32, f32);
    make_read!(read_u64, u64);
    make_read!(read_i64, i64);
    make_read!(read_f64, f64);

    pub fn read_utf(&mut self) -> Result<&'d str, ReadError> {
        if self.data.len() < 2 {
            return Err(ReadError::Underflow {
                need: 2,
                have: self.data.len(),
            });
        }
        let len = self.read_u16()?;
        let end = len as usize;
        if self.data.len() < end {
            return Err(ReadError::Underflow {
                need: end,
                have: self.data.len(),
            });
        }
        let result = std::str::from_utf8(&self.data[..end])?;
        self.data = &self.data[end..];
        self.read += end;
        Ok(result)
    }

    pub fn read_bytes(&mut self, dst: &mut [u8]) -> Result<(), ReadError> {
        if self.data.len() < dst.len() {
            return Err(ReadError::Underflow {
                need: dst.len(),
                have: self.data.len(),
            });
        }
        dst.copy_from_slice(&self.data[..dst.len()]);
        self.data = &self.data[dst.len()..];
        self.read += dst.len();
        Ok(())
    }

    pub fn skip(&mut self, n: usize) -> Result<(), ReadError> {
        if self.data.len() < n {
            return Err(ReadError::Underflow {
                need: n,
                have: self.data.len(),
            });
        }
        self.data = &self.data[n..];
        self.read += n;
        Ok(())
    }

    pub fn skip_chunk(&mut self) -> Result<usize, ReadError> {
        let len = self.read_u32()? as usize;
        self.skip(len)?;
        Ok(len)
    }

    pub fn read_chunk<E>(
        &mut self,
        big: bool,
        f: impl FnOnce(&mut DataRead) -> Result<(), E>,
    ) -> Result<(), E>
    where
        E: Error + From<ReadError>,
    {
        let len = if big {
            self.read_u32()? as usize
        } else {
            self.read_u16()? as usize
        };
        self.read = 0;
        let r = f(self);
        match r {
            Err(e) => {
                // skip this chunk
                if len < self.read {
                    eprintln!("overread ({e:?})");
                    return Err(e);
                }
                let n = len - self.read;
                if n != 0 {
                    self.skip(n)?;
                };
                Err(e)
            }
            Ok(_) => Ok(()),
        }
    }

    pub fn read_vec(&mut self, dst: &mut Vec<u8>, len: usize) -> Result<(), ReadError> {
        if self.data.len() < len {
            return Err(ReadError::Underflow {
                need: len,
                have: self.data.len(),
            });
        }
        dst.extend_from_slice(&self.data[..len]);
        self.data = &self.data[len..];
        self.read += len;
        Ok(())
    }

    pub fn read_map(&mut self, dst: &mut HashMap<String, String>) -> Result<(), ReadError> {
        let n = self.read_u8()?;
        for _ in 0..n {
            let key = self.read_utf()?;
            let value = self.read_utf()?;
            dst.insert(key.to_owned(), value.to_owned());
        }
        Ok(())
    }

    pub fn deflate(&mut self) -> Result<Vec<u8>, DecompressError> {
        let mut dec = Decompress::new(true);
        let mut raw = Vec::<u8>::new();
        raw.reserve(1024);
        loop {
            let t_in = dec.total_in();
            let t_out = dec.total_out();
            let res = dec.decompress_vec(self.data, &mut raw, FlushDecompress::Finish)?;
            if dec.total_in() > t_in {
                // we have to advance input every time, decompress_vec only knows the output position
                self.data = &self.data[(dec.total_in() - t_in) as usize..];
            }
            match res {
                // there's no more input (and the flush mode says so), we need to reserve additional space
                Status::Ok | Status::BufError => (),
                // input was already at the end, so this is referring to the output
                Status::StreamEnd => break,
            }
            if dec.total_in() == t_in && dec.total_out() == t_out {
                // protect against looping forever
                return Err(DecompressError::DecompressStall);
            }
            raw.reserve(1024);
        }
        assert_eq!(dec.total_out() as usize, raw.len());
        self.read = 0;
        Ok(raw)
    }
}

#[derive(Debug, Error)]
pub enum DecompressError {
    #[error("zlib decompression failed")]
    Decompress(#[from] DError),
    #[error("decompressor stalled before completion")]
    DecompressStall,
}

#[derive(Debug, Error)]
pub enum ReadError {
    #[error("buffer underflow (expected {need} but got {have})")]
    Underflow { need: usize, have: usize },
    #[error("expected {0}")]
    Expected(&'static str),
    #[error("malformed utf8 in string")]
    Utf8 {
        #[from]
        source: Utf8Error,
    },
}

impl PartialEq for ReadError {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

enum WriteBuff<'d> {
    // unlike the DataRead want to access the written region after
    Ref { raw: &'d mut [u8], pos: usize },
    Vec(Vec<u8>),
}

impl<'d> WriteBuff<'d> {
    fn check_capacity(&self, need: usize) -> Result<(), WriteError> {
        match self {
            Self::Ref { raw, pos } if raw.len() - pos < need => Err(WriteError::Overflow {
                need,
                have: raw.len() - pos,
            }),
            _ => Ok(()),
        }
    }

    fn write(&mut self, data: &[u8]) {
        match self {
            Self::Ref { raw, pos } => {
                let end = *pos + data.len();
                raw[*pos..end].copy_from_slice(data);
                *pos += data.len();
            }
            Self::Vec(v) => v.extend_from_slice(data),
        }
    }
}

pub struct DataWrite<'d> {
    data: WriteBuff<'d>,
}

macro_rules! make_write {
    ($name:ident, $type:ty) => {
        pub fn $name(&mut self, val: $type) -> Result<(), WriteError> {
            const LEN: usize = std::mem::size_of::<$type>();
            self.data.check_capacity(LEN)?;
            self.data.write(&<$type>::to_be_bytes(val));
            Ok(())
        }
    };
}

impl<'d> DataWrite<'d> {
    pub fn write_bool(&mut self, val: bool) -> Result<(), WriteError> {
        self.write_u8(u8::from(val))
    }

    make_write!(write_u8, u8);
    make_write!(write_i8, i8);
    make_write!(write_u16, u16);
    make_write!(write_i16, i16);
    make_write!(write_u32, u32);
    make_write!(write_i32, i32);
    make_write!(write_f32, f32);
    make_write!(write_u64, u64);
    make_write!(write_i64, i64);
    make_write!(write_f64, f64);

    pub fn write_utf(&mut self, val: &str) -> Result<(), WriteError> {
        if val.len() > u16::MAX as usize {
            return Err(WriteError::TooLong { len: val.len() });
        }
        self.data.check_capacity(2 + val.len())?;
        self.data.write(&u16::to_be_bytes(val.len() as u16));
        self.data.write(val.as_bytes());
        Ok(())
    }

    pub fn write_bytes(&mut self, val: &[u8]) -> Result<(), WriteError> {
        self.data.check_capacity(val.len())?;
        self.data.write(val);
        Ok(())
    }

    #[must_use]
    pub fn is_owned(&self) -> bool {
        matches!(self.data, WriteBuff::Vec(..))
    }

    #[must_use]
    pub fn get_written(&self) -> &[u8] {
        match &self.data {
            WriteBuff::Ref { raw, pos } => &raw[..*pos],
            WriteBuff::Vec(v) => v,
        }
    }

    /// eat this datawrite
    ///
    /// panics if ref write buffer
    pub fn consume(self) -> Vec<u8> {
        match self.data {
            WriteBuff::Vec(v) => v,
            WriteBuff::Ref { .. } => unreachable!(),
        }
    }

    pub fn inflate(self, to: &mut DataWrite) -> Result<(), CompressError> {
        // compress into the provided buffer
        let WriteBuff::Vec(raw) = self.data else {
            unreachable!("write buffer not owned")
        };
        let mut comp = Compress::new(Compression::default(), true);
        // compress the immediate buffer into a temp buffer to copy it to buff? no thanks
        match to.data {
            WriteBuff::Ref {
                raw: ref mut dst,
                ref mut pos,
            } => {
                match comp.compress(&raw, &mut dst[*pos..], FlushCompress::Finish)? {
                    // there's no more input (and the flush mode says so), but we can't resize the output
                    Status::Ok | Status::BufError => {
                        return Err(CompressError::CompressEof(
                            raw.len() - comp.total_in() as usize,
                        ))
                    }
                    Status::StreamEnd => (),
                }
            }
            WriteBuff::Vec(ref mut dst) => {
                let mut input = raw.as_ref();
                dst.reserve(1024);
                loop {
                    let t_in = comp.total_in();
                    let t_out = comp.total_out();
                    let res = comp.compress_vec(input, dst, FlushCompress::Finish)?;
                    if comp.total_in() > t_in {
                        // we have to advance input every time, compress_vec only knows the output position
                        input = &input[(comp.total_in() - t_in) as usize..];
                    }
                    match res {
                        // there's no more input (and the flush mode says so), we need to reserve additional space
                        Status::Ok | Status::BufError => (),
                        // input was already at the end, so this is referring to the output
                        Status::StreamEnd => break,
                    }
                    if comp.total_in() == t_in && comp.total_out() == t_out {
                        // protect against looping forever
                        return Err(CompressError::CompressStall);
                    }
                    dst.reserve(1024);
                }
            }
        }
        assert_eq!(comp.total_in() as usize, raw.len());
        Ok(())
    }
}

impl Default for DataWrite<'static> {
    fn default() -> Self {
        Self {
            data: WriteBuff::Vec(Vec::new()),
        }
    }
}

#[derive(Debug, Error)]
pub enum CompressError {
    #[error(transparent)]
    Compress(#[from] CError),
    #[error("compression overflow with {0} bytes of input remaining")]
    CompressEof(usize),
    #[error("compressor stalled before completion")]
    CompressStall,
}

#[derive(Debug, Error)]
pub enum WriteError {
    #[error("buffer overflow (expected {need} but got {have})")]
    Overflow { need: usize, have: usize },
    #[error("string too long ({len} bytes of {})", u16::MAX)]
    TooLong { len: usize },
}

impl PartialEq for WriteError {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl<'d> From<&'d mut [u8]> for DataWrite<'d> {
    fn from(value: &'d mut [u8]) -> Self {
        Self {
            data: WriteBuff::Ref { raw: value, pos: 0 },
        }
    }
}

impl From<Vec<u8>> for DataWrite<'static> {
    fn from(value: Vec<u8>) -> Self {
        Self {
            data: WriteBuff::Vec(value),
        }
    }
}

impl<'d> TryFrom<DataWrite<'d>> for Vec<u8> {
    type Error = ();

    fn try_from(value: DataWrite<'d>) -> Result<Self, Self::Error> {
        match value.data {
            WriteBuff::Vec(v) => Ok(v),
            _ => Err(()),
        }
    }
}
/// basic serialization/deserialization functions
pub trait Serializer<D> {
    type ReadError;
    type WriteError;

    fn deserialize(&mut self, buff: &mut DataRead<'_>) -> Result<D, Self::ReadError>;

    fn serialize(&mut self, buff: &mut DataWrite<'_>, data: &D) -> Result<(), Self::WriteError>;
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct GridPos(pub usize, pub usize);

impl From<u32> for GridPos {
    fn from(value: u32) -> Self {
        GridPos((value >> 16) as u16 as usize, value as u16 as usize)
    }
}

impl From<GridPos> for u32 {
    /// ```
    /// # use mindus::data::GridPos;
    /// assert_eq!(GridPos::from(u32::from(GridPos(1000, 5))), GridPos(1000, 5));
    /// ```
    fn from(value: GridPos) -> Self {
        ((value.0 << 16) | value.1) as u32
    }
}

impl fmt::Debug for GridPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({0}, {1})", self.0, self.1)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn read() {
        let mut read = DataRead::new("Thé qûick ઉrown fox 🦘 over\0\rthe lazy dog.".as_bytes());
        assert_eq!(read.read_u8(), Ok(84));
        assert_eq!(read.read_i8(), Ok(104));
        assert_eq!(read.read_i8(), Ok(-61));
        assert_eq!(read.read_u16(), Ok(43296));
        assert_eq!(read.read_i16(), Ok(29123));
        assert_eq!(read.read_i16(), Ok(-17559));
        assert_eq!(read.read_i32(), Ok(1_667_965_152));
        assert_eq!(read.read_i32(), Ok(-1_433_832_849));
        assert_eq!(read.read_i64(), Ok(8_605_851_562_280_493_296));
        assert_eq!(read.read_i64(), Ok(-6_942_694_510_468_635_278));
        assert_eq!(read.read_utf(), Ok("the lazy dog."));
    }

    #[test]
    fn write() {
        let mut write = DataWrite::default();
        assert_eq!(write.write_u8(84), Ok(()));
        assert_eq!(write.write_i8(104), Ok(()));
        assert_eq!(write.write_i8(-61), Ok(()));
        assert_eq!(write.write_u16(43296), Ok(()));
        assert_eq!(write.write_i16(29123), Ok(()));
        assert_eq!(write.write_i16(-17559), Ok(()));
        assert_eq!(write.write_i32(1_667_965_152), Ok(()));
        assert_eq!(write.write_i32(-1_433_832_849), Ok(()));
        assert_eq!(write.write_i64(8_605_851_562_280_493_296), Ok(()));
        assert_eq!(write.write_i64(-6_942_694_510_468_635_278), Ok(()));
        assert_eq!(write.write_utf("the lazy dog."), Ok(()));
        assert_eq!(
            write.get_written(),
            "Thé qûick ઉrown fox 🦘 over\0\rthe lazy dog.".as_bytes()
        );
    }
}
