//! variable type
use thiserror::Error;

use crate::content;
use crate::data::command::{self, UnitCommand};
use crate::data::{self, DataRead, DataWrite, GridPos, Serializable};
use crate::team::Team;

macro_rules! datamaker {
    (
        $($k: ident($v: ty),)+
    ) => { paste::paste! {
        #[derive(Clone, Debug, PartialEq)]
        /// holds different kinds of data
        pub enum DynData {
            Empty,
            Content(content::Type, u16),
            Point2(i32, i32),
            Vec2(f32, f32),
            TechNode(content::Type, u16),
            $($k($v),)+
        }

        $(
            impl From<$v> for DynData {
                #[doc = concat!(" to [`DynData::", stringify!($k), "`]")]
                fn from(f: $v) -> Self {
                    Self::$k(f)
                }
            }
        )+


        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum DynType {
            Content,
            Point2,
            Vec2,
            TechNode,
            Empty,
            $($k,)+
        }

        impl DynData {
            #[must_use]
            pub const fn get_type(&self) -> DynType {
                match self {
                    Self::Empty => DynType::Empty,
                    Self::Point2(..) => DynType::Point2,
                    Self::Vec2(..) => DynType::Vec2,
                    Self::TechNode(..) => DynType::TechNode,
                    Self::Content(..) => DynType::Content,
                    $(Self::$k(..) => DynType::$k,)+
                }
            }
        }
    } }
}

datamaker! {
    Int(i32),
    Long(i64),
    Float(f32),
    String(Option<String>),
    IntArray(Vec<i32>),
    Point2Array(Vec<(i16, i16)>),
    Boolean(bool),
    Double(f64),
    Building(GridPos),
    ByteArray(Vec<u8>),
    UnitCommand(UnitCommand),
    BoolArray(Vec<bool>),
    Unit(u32),
    Vec2Array(Vec<(f32, f32)>),
    Team(Team),
}

impl Serializable for DynData {
    type ReadError = ReadError;
    type WriteError = WriteError;

    fn deserialize(buff: &mut DataRead<'_>) -> Result<DynData, Self::ReadError> {
        match buff.read_u8()? {
            0 => Ok(DynData::Empty),
            1 => Ok(DynData::from(buff.read_i32()?)),
            2 => Ok(DynData::from(buff.read_i64()?)),
            3 => Ok(DynData::from(buff.read_f32()?)),
            4 => {
                if buff.read_bool()? {
                    Ok(DynData::from(Some(String::from(buff.read_utf()?))))
                } else {
                    Ok(DynData::from(None))
                }
            }
            5 => Ok(DynData::Content(
                content::Type::try_from(buff.read_u8()?)?,
                buff.read_u16()?,
            )),
            6 => {
                let len = buff.read_i16()?;
                let Ok(len) = usize::try_from(len) else {
                    return Err(ReadError::IntArrayLen(len));
                };
                let mut result = vec![0; len];
                for item in result.iter_mut() {
                    *item = buff.read_i32()?;
                }
                Ok(DynData::from(result))
            }
            7 => Ok(DynData::Point2(buff.read_i32()?, buff.read_i32()?)),
            8 => {
                let len = buff.read_i8()?;
                let Ok(len) = usize::try_from(len) else {
                    return Err(ReadError::Point2ArrayLen(len));
                };
                let mut result = vec![(0, 0); len];
                for item in result.iter_mut() {
                    let pt = buff.read_i32()?;
                    *item = ((pt >> 16) as i16, pt as i16);
                }
                Ok(DynData::from(result))
            }
            9 => Ok(DynData::TechNode(
                content::Type::try_from(buff.read_u8()?)?,
                buff.read_u16()?,
            )),
            10 => Ok(DynData::from(buff.read_bool()?)),
            11 => Ok(DynData::from(buff.read_f64()?)),
            12 => Ok(DynData::from(GridPos::from(buff.read_u32()?))),
            14 => {
                let len = buff.read_i32()?;
                let Ok(len) = usize::try_from(len) else {
                    return Err(ReadError::ByteArrayLen(len));
                };
                let mut result = vec![];
                buff.read_vec(&mut result, len)?;
                Ok(DynData::from(result))
            }
            16 => {
                let len = buff.read_i32()?;
                let Ok(len) = usize::try_from(len) else {
                    return Err(ReadError::BoolArrayLen(len));
                };
                let mut result = vec![false; len];
                for item in result.iter_mut() {
                    *item = buff.read_bool()?;
                }
                Ok(DynData::from(result))
            }
            17 => Ok(DynData::Unit(buff.read_u32()?)),
            18 => {
                let len = buff.read_i16()?;
                let Ok(len) = usize::try_from(len) else {
                    return Err(ReadError::Vec2ArrayLen(len));
                };
                let mut result = vec![(0., 0.); len];
                for item in result.iter_mut() {
                    *item = (buff.read_f32()?, buff.read_f32()?);
                }
                Ok(DynData::from(result))
            }
            19 => Ok(DynData::Vec2(buff.read_f32()?, buff.read_f32()?)),
            20 => Ok(DynData::from(Team::of(buff.read_u8()?))),
            23 => Ok(DynData::from(
                UnitCommand::try_from(buff.read_i16()? as u8)?,
            )),
            id => Err(ReadError::Type(id)),
        }
    }

    fn serialize(&self, buff: &mut DataWrite<'_>) -> Result<(), Self::WriteError> {
        match self {
            DynData::Empty => {
                buff.write_u8(0)?;
                Ok(())
            }
            DynData::Int(val) => {
                buff.write_u8(1)?;
                buff.write_i32(*val)?;
                Ok(())
            }
            DynData::Long(val) => {
                buff.write_u8(2)?;
                buff.write_i64(*val)?;
                Ok(())
            }
            DynData::Float(val) => {
                buff.write_u8(3)?;
                buff.write_f32(*val)?;
                Ok(())
            }
            DynData::String(opt) => {
                buff.write_u8(4)?;
                match opt {
                    None => buff.write_bool(false)?,
                    Some(s) => {
                        buff.write_bool(true)?;
                        buff.write_utf(s)?;
                    }
                }
                Ok(())
            }
            DynData::Content(ty, id) => {
                buff.write_u8(5)?;
                buff.write_u8((*ty).into())?;
                buff.write_u16(*id)?;
                Ok(())
            }
            DynData::IntArray(arr) => {
                if arr.len() > i16::MAX as usize {
                    return Err(WriteError::IntArrayLen(arr.len()));
                }
                buff.write_u8(6)?;
                buff.write_i16(arr.len() as i16)?;
                for &v in arr {
                    buff.write_i32(v)?;
                }
                Ok(())
            }
            DynData::Point2(x, y) => {
                buff.write_u8(7)?;
                buff.write_i32(*x)?;
                buff.write_i32(*y)?;
                Ok(())
            }
            DynData::Point2Array(arr) => {
                if arr.len() > i8::MAX as usize {
                    return Err(WriteError::Point2ArrayLen(arr.len()));
                }
                buff.write_u8(8)?;
                buff.write_i8(arr.len() as i8)?;
                for &(x, y) in arr {
                    buff.write_i32((i32::from(x) << 16) | (i32::from(y) & 0xFFFF))?;
                }
                Ok(())
            }
            DynData::TechNode(ty, id) => {
                buff.write_u8(9)?;
                buff.write_u8((*ty).into())?;
                buff.write_u16(*id)?;
                Ok(())
            }
            DynData::Boolean(val) => {
                buff.write_u8(10)?;
                buff.write_bool(*val)?;
                Ok(())
            }
            DynData::Double(val) => {
                buff.write_u8(11)?;
                buff.write_f64(*val)?;
                Ok(())
            }
            DynData::Building(pos) => {
                buff.write_u8(12)?;
                buff.write_u32(u32::from(*pos))?;
                Ok(())
            }
            DynData::ByteArray(arr) => {
                if arr.len() > i32::MAX as usize {
                    return Err(WriteError::ByteArrayLen(arr.len()));
                }
                buff.write_u8(14)?;
                buff.write_i32(arr.len() as i32)?;
                buff.write_bytes(arr)?;
                Ok(())
            }
            DynData::UnitCommand(cmd) => {
                buff.write_u8(23)?;
                buff.write_u16(u8::from(*cmd).into())?;
                Ok(())
            }
            DynData::BoolArray(arr) => {
                if arr.len() > i32::MAX as usize {
                    return Err(WriteError::BoolArrayLen(arr.len()));
                }
                buff.write_u8(16)?;
                buff.write_i32(arr.len() as i32)?;
                for &b in arr {
                    buff.write_bool(b)?;
                }
                Ok(())
            }
            DynData::Unit(id) => {
                buff.write_u8(17)?;
                buff.write_u32(*id)?;
                Ok(())
            }
            DynData::Vec2Array(arr) => {
                if arr.len() > i16::MAX as usize {
                    return Err(WriteError::Vec2ArrayLen(arr.len()));
                }
                buff.write_u8(18)?;
                buff.write_i16(arr.len() as i16)?;
                for &(x, y) in arr {
                    buff.write_f32(x)?;
                    buff.write_f32(y)?;
                }
                Ok(())
            }
            DynData::Vec2(x, y) => {
                buff.write_u8(19)?;
                buff.write_f32(*x)?;
                buff.write_f32(*y)?;
                Ok(())
            }
            DynData::Team(team) => {
                buff.write_u8(20)?;
                buff.write_u8(u8::from(*team))?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum ReadError {
    #[error("failed to read from buffer")]
    Underlying(#[from] data::ReadError),
    #[error("invalid dynamic data type ({0})")]
    Type(u8),
    #[error("content type not found")]
    ContentType(#[from] content::TryFromU8Error),
    #[error("integer array too long ({0})")]
    IntArrayLen(i16),
    #[error("point2 array too long ({0})")]
    Point2ArrayLen(i8),
    #[error("byte array too long ({0})")]
    ByteArrayLen(i32),
    #[error("unit command not found")]
    UnitCommand(#[from] command::TryFromU8Error),
    #[error("boolean array too long ({0}")]
    BoolArrayLen(i32),
    #[error("vec2 array too long ({0})")]
    Vec2ArrayLen(i16),
}

#[derive(Debug, PartialEq, Error)]
pub enum WriteError {
    #[error("failed to write to buffer")]
    Underlying(#[from] data::WriteError),
    #[error("integer array too long ({0})")]
    IntArrayLen(usize),
    #[error("point2 array too long ({0})")]
    Point2ArrayLen(usize),
    #[error("byte array too long ({0})")]
    ByteArrayLen(usize),
    #[error("boolean array too long ({0})")]
    BoolArrayLen(usize),
    #[error("vec2 array too long ({0})")]
    Vec2ArrayLen(usize),
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::team::Team;

    macro_rules! _zero {
        ($tt:tt) => {
            0usize
        };
    }

    macro_rules! make_dyn_test {
		($name:ident, $($val:expr),+) => {
			#[test]
			fn $name() {
				let input = [$($val),+];
				let mut positions = [$(_zero!($val)),+];
				let mut writer = DataWrite::default();
				for (i, d) in input.iter().enumerate()
				{
					assert_eq!(d.serialize(&mut writer), Ok(()));
					positions[i] = writer.get_written().len();
				}
				let written = writer.get_written();
				let end = written.len();
				let mut reader = DataRead::new(written);
				for (i, original) in input.iter().enumerate()
				{
					match DynData::deserialize(&mut reader)
					{
						Ok(read) => assert_eq!(*original, read, "serialization of {original:?} became {read:?}"),
						e => assert!(false, "could not re-read {original:?} (at {i}), got {e:?}"),
					}
					let expect = end - reader.data.len();
					let before = if i > 0 {positions[i - 1]} else {0};
					assert_eq!(expect, positions[i], "uneven deserialization of {original:?} ({} vs {})", expect - before, positions[i] - before);
				}
			}
		};
	}

    make_dyn_test!(
        reparse_empty,
        DynData::Empty,
        DynData::Empty,
        DynData::Empty
    );
    make_dyn_test!(
        reparse_int,
        DynData::Int(581_923),
        DynData::Int(2_147_483_647),
        DynData::Int(-1_047_563_850)
    );
    make_dyn_test!(
        reparse_long,
        DynData::Long(11_295_882_949_812),
        DynData::Long(-5_222_358_074_010_407_789)
    );
    make_dyn_test!(
        reparse_float,
        DynData::Float(std::f32::consts::PI),
        DynData::Float(f32::INFINITY),
        DynData::Float(f32::EPSILON)
    );
    make_dyn_test!(
        reparse_string,
        DynData::String(None),
        DynData::String(Some("hello \u{10FE03}".to_string())),
        DynData::String(Some(String::new()))
    );
    make_dyn_test!(
        reparse_content,
        DynData::Content(content::Type::Item, 12345),
        DynData::Content(content::Type::Planet, 25431)
    );
    make_dyn_test!(
        reparse_int_array,
        DynData::IntArray(vec![581_923, 2_147_483_647, -1_047_563_850]),
        DynData::IntArray(vec![1_902_864_703])
    );
    make_dyn_test!(
        reparse_point2,
        DynData::Point2(17, 0),
        DynData::Point2(234, -345),
        DynData::Point2(-2_147_483_648, -1)
    );
    make_dyn_test!(
        reparse_point2_array,
        DynData::Point2Array(vec![(44, 55), (-33, 66), (-22, -77)]),
        DynData::Point2Array(vec![(22, -88)])
    );
    make_dyn_test!(
        reparse_technode,
        DynData::TechNode(content::Type::Item, 12345),
        DynData::TechNode(content::Type::Planet, 25431)
    );
    make_dyn_test!(
        reparse_boolean,
        DynData::Boolean(false),
        DynData::Boolean(true),
        DynData::Boolean(false)
    );
    make_dyn_test!(
        reparse_double,
        DynData::Double(std::f64::consts::E),
        DynData::Double(-f64::INFINITY)
    );
    make_dyn_test!(
        reparse_building,
        DynData::Building(GridPos(10, 0)),
        DynData::Building(GridPos(4444, 0xFE98))
    );
    make_dyn_test!(
        reparse_byte_array,
        DynData::ByteArray(b"c\x00nstruct \xADditio\nal pylons".to_vec()),
        DynData::ByteArray(b"\x00\x01\xFE\xFF".to_vec())
    );
    make_dyn_test!(
        reparse_unit_command,
        DynData::UnitCommand(UnitCommand::Mine),
        DynData::UnitCommand(UnitCommand::Mine)
    );
    make_dyn_test!(
        reparse_bool_array,
        DynData::BoolArray(vec![true, true, true, false, true, false, true]),
        DynData::BoolArray(vec![false, true])
    );
    make_dyn_test!(reparse_unit, DynData::Unit(0), DynData::Unit(2_147_483_647));
    make_dyn_test!(
        reparse_vec2_array,
        DynData::Vec2Array(vec![(4.4, 5.5), (-3.3, 6.6), (-2.2, -7.7)]),
        DynData::Vec2Array(vec![(2.2, -8.8)])
    );
    make_dyn_test!(
        reparse_vec2,
        DynData::Vec2(1.5, 9.1234),
        DynData::Vec2(-0.0, -17.0),
        DynData::Vec2(-10.7, 3.8)
    );
    make_dyn_test!(
        reparse_team,
        DynData::Team(Team::SHARDED),
        DynData::Team(Team::CRUX),
        DynData::Team(Team::DERELICT)
    );
}
