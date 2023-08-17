//! units
//!
//! [source](https://github.com/Anuken/Mindustry/blob/master/core/src/mindustry/content/UnitTypes.java)
use crate::content::content_enum;
use crate::data::command::UnitCommand;
use crate::data::dynamic::DynSerializer;
use crate::data::entity_mapping::UnitClass;
use crate::data::{DataRead, ReadError};
use crate::item::Type as Item;
use crate::modifier::Type as Status;
use crate::team::Team;
use crate::Serializer;

content_enum! {
    pub enum Type / Unit for u16 | TryFromU16Error {
        "dagger",
        "mace",
        "fortress",
        "scepter",
        "reign",
        "nova",
        "pulsar",
        "quasar",
        "vela",
        "corvus",
        "crawler",
        "atrax",
        "spiroct",
        "arkyid",
        "toxopid",
        "flare",
        "horizon",
        "zenith",
        "antumbra",
        "eclipse",
        "mono",
        "poly",
        "mega",
        "quad",
        "oct",
        "risso",
        "minke",
        "bryde",
        "sei",
        "omura",
        "retusa",
        "oxynoe",
        "cyerce",
        "aegires",
        "navanax",
        "alpha",
        "beta",
        "gamma",
        "stell",
        "locus",
        "precept",
        "vanquish",
        "conquer",
        "merui",
        "cleroi",
        "anthicus",
        "anthicus-missile",
        "tecta",
        "collaris",
        "elude",
        "avert",
        "obviate",
        "quell",
        "quell-missile",
        "disrupt",
        "disrupt-missile",
        "renale",
        "latum",
        "evoke",
        "incite",
        "emanate",
        "block",
        "manifold",
        "assembly-drone",
        "scathe-missile",
    }
}

#[derive(Default, Debug)]
pub struct UnitState {
    pub ammo: f32,
    pub elevation: f32,
    pub flag: i64,
    pub health: f32,
    pub is_shooting: bool,
    pub rotation: f32,
    pub shield: f32,
    pub stack: (Option<Item>, u32),
    /// how many status can you realistically be afflicted with
    pub status: [Status; 3],
    pub team: Team,
    pub velocity: (f32, f32),
    pub position: (f32, f32),
    pub controller: Controller,
}

#[derive(Default, Debug)]
pub enum Controller {
    Player(i32),
    Logic(i32),
    Command {
        target: Option<i32>,
        pos: Option<(f32, f32)>,
        command: Option<UnitCommand>,
    },
    #[default]
    Assembler,
}

impl Controller {
    /// format:
    /// - match [`u8`]
    ///     - (0): player
    ///     - player: [`i32`]
    ///
    ///     - (3): logic ai
    ///     - position: [`i32`]
    ///
    ///     - (6): command
    ///     - has_attack: [`bool`]
    ///     - has_pos: [`bool`]
    ///     - if `has_pos`:
    ///         - pos: ([`f32`], [`f32`])
    ///     - if `has_attack`:
    ///         - ty: [`i8`]
    ///         - target_id: [`i32`]
    ///     - command: [`i8`] attempt as [`UnitCommand`]
    ///     - (_): assembler
    fn read(buff: &mut DataRead) -> Result<Controller, ReadError> {
        Ok(match buff.read_u8()? {
            0 => Controller::Player(buff.read_i32()?),
            3 => Controller::Logic(buff.read_i32()?),
            6 => {
                let has_attack = buff.read_bool()?;
                let pos = if buff.read_bool()? {
                    Some((buff.read_f32()?, buff.read_f32()?))
                } else {
                    None
                };
                let target = if has_attack {
                    buff.skip(1)?;
                    Some(buff.read_i32()?)
                } else {
                    None
                };
                let n = buff.read_i8()?;
                let command = if let Ok(n) = u8::try_from(n) && let Ok(u) = UnitCommand::try_from(n) {
                    Some(u)
                } else {
                    None
                };
                Controller::Command {
                    target,
                    pos,
                    command,
                }
            }
            _ => Controller::Assembler,
        })
    }
}

#[derive(Debug)]
pub struct Unit {
    pub state: UnitState,
    pub ty: Type,
}

impl UnitClass {
    pub fn read(self, buff: &mut DataRead) -> Result<Unit, ReadError> {
        buff.skip(2)?;
        let mut state = UnitState::default();
        read_abilities(buff)?;
        state.ammo = buff.read_f32()?;
        state.controller = Controller::read(buff)?;
        state.elevation = buff.read_f32()?;
        state.flag = buff.read_i64()?;
        state.health = buff.read_f32()?;
        state.is_shooting = buff.read_bool()?;
        dbg!(&state);
        read_tile(buff)?;
        read_mounts(buff)?;
        read_plans(buff)?;
        state.rotation = buff.read_f32()?;
        state.shield = buff.read_f32()?;
        buff.skip(1)?; // spawned_by_core
        state.stack = read_stack(buff)?;
        state.status = read_status(buff)?;
        state.team = Team::of(buff.read_u8()?);
        let ty = Type::try_from(buff.read_u16()?).unwrap();
        buff.skip(1)?; // update_building
        state.velocity = (buff.read_f32()?, buff.read_f32()?);
        state.position = (buff.read_f32()?, buff.read_f32()?);

        Ok(Unit { state, ty })
    }
}

/// format:
/// - iterate [u8]
///     - ability: [`f32`]
fn read_abilities(buff: &mut DataRead) -> Result<(), ReadError> {
    let n = buff.read_u8()? as usize;
    buff.skip(n * 4)
}

/// format:
/// - tile: [`i32`]
fn read_tile(buff: &mut DataRead) -> Result<(), ReadError> {
    buff.skip(4)
}

/// format:
/// - iterate [`u8`]
///     - state: [`u8`]
///     - x aim: [`f32`]
///     - y aim: [`f32`]
fn read_mounts(buff: &mut DataRead) -> Result<(), ReadError> {
    let n = dbg!(buff.read_u8()? as usize);
    buff.skip(n * 9)
}

/// format:
/// - plan count: [`i32`]
/// - plan_count == -1 => return
/// - iterate `plan_count`
///     - call [`read_plan`]
fn read_plans(buff: &mut DataRead) -> Result<(), ReadError> {
    let used = dbg!(buff.read_i32()?);
    if used == -1 {
        return Ok(());
    }
    for _ in 0..used {
        read_plan(buff)?;
    }
    Ok(())
}

/// format:
/// - ty: [`u8`]
/// - position: [`i32`]
/// - if ty != 1
///     - block: [`u16`]
///     - rotation: [`i8`]
///     - has_config: [`bool`]
///     - config: [`DynData`](crate::data::dynamic::DynData)
fn read_plan(buff: &mut DataRead) -> Result<(), ReadError> {
    let ty = buff.read_u8()?;
    buff.skip(4)?;
    if ty != 1 {
        buff.skip(4)?;
        let _ = DynSerializer.deserialize(buff).unwrap();
    }
    Ok(())
}

/// format:
/// - item: [`i16`] attempt into [`Item`]
/// - count: [`u32`]
fn read_stack(buff: &mut DataRead) -> Result<(Option<Item>, u32), ReadError> {
    let n = buff.read_i16()?;
    Ok((
        (n != -1).then(|| Item::try_from(n as u16).unwrap()),
        buff.read_u32()?,
    ))
}

/// read the status.
/// i take only 3
///
/// format:
/// - iterate [`i32`]
///     - status: [`u16`] attempt into [`Status`]
fn read_status(buff: &mut DataRead) -> Result<[Status; 3], ReadError> {
    let mut status = [Status::None, Status::None, Status::None];
    for i in 0..buff.read_i32()? {
        let this = Status::try_from(buff.read_u16()?).unwrap_or_default();
        if i < 3 {
            status[i as usize] = this;
        }
    }
    Ok(status)
}
