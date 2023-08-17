use std::fmt;

use crate::content::{Content, Type};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Default)]
pub struct Team(u8);

impl Team {
    #[must_use]
    pub const fn of(id: u8) -> Self {
        Self(id)
    }

    #[must_use]
    pub const fn is_base(self) -> bool {
        self.0 < 7
    }
}

impl From<u8> for Team {
    fn from(value: u8) -> Self {
        Team::of(value)
    }
}

impl TryFrom<u16> for Team {
    type Error = TryFromU16Error;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        if u8::try_from(value).is_ok() {
            Ok(Team(value as u8))
        } else {
            Err(TryFromU16Error(value))
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, thiserror::Error)]
#[error("no content of type Team for value {0}")]
pub struct TryFromU16Error(pub u16);

impl From<Team> for u8 {
    fn from(value: Team) -> Self {
        value.0
    }
}

impl From<Team> for u16 {
    fn from(value: Team) -> Self {
        u16::from(value.0)
    }
}

impl fmt::Display for Team {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            0 => f.write_str("Derelict"),
            1 => f.write_str("Sharded"),
            2 => f.write_str("Crux"),
            3 => f.write_str("Malis"),
            4 => f.write_str("Green"),
            5 => f.write_str("Blue"),
            6 => f.write_str("Neoplastic"),
            id => write!(f, "Team #{id}"),
        }
    }
}

const TEAM_NAMES: &str = include_str!("../res/team_names.txt");

impl Content for Team {
    fn get_type(&self) -> Type {
        Type::Team
    }

    fn get_id(&self) -> u16 {
        u16::from(self.0)
    }

    fn get_name(&self) -> &'static str {
        match self.0 {
            0 => "derelict",
            1 => "sharded",
            2 => "crux",
            3 => "malis",
            4 => "green",
            5 => "blue",
            6 => "neoplastic",
            // dark magic: offsets manually computed, then rely on the format "...|team#{i}|..."
            i @ 7..=9 => {
                // length: 7 ("team#" (5) + 1 digit + "|" (1))
                let s = ((i - 6) as usize) * 7;
                &TEAM_NAMES[s..s + 6] // exclude the trailing "|"
            }
            i @ 10..=99 => {
                // length: 8 ("team#" (5) + 2 digits + "|" (1))
                let s = 28 + ((i - 10) as usize) * 8;
                &TEAM_NAMES[s..s + 7] // exclude the trailing "|"
            }
            i @ 100..=255 => {
                // length: 9 ("team#" (5) + 3 digits + "|" (1))
                let s = 748 + ((i - 100) as usize) * 9;
                &TEAM_NAMES[s..s + 8] // exclude the trailing "|"
            }
        }
    }
}

impl Team {
    pub const fn color(self) -> (u8, u8, u8) {
        macro_rules! h {
            ($x:literal) => {{
                let v = color_hex::color_from_hex!($x);
                (v[0], v[1], v[2])
            }};
        }
        match self {
            SHARDED => h!("ffd37f"),
            DERELICT => h!("4d4e58"),
            CRUX => h!("f25555"),
            MALIS => h!("a27ce5"),
            GREEN => h!("54d67d"),
            BLUE => h!("6c87fd"),
            NEOPLASTIC => h!("e05438"),
            _ => h!("a9a9a9"),
        }
    }
}

pub const DERELICT: Team = Team(0);
pub const SHARDED: Team = Team(1);
pub const CRUX: Team = Team(2);
pub const MALIS: Team = Team(3);
pub const GREEN: Team = Team(4);
pub const BLUE: Team = Team(5);
pub const NEOPLASTIC: Team = Team(6);
