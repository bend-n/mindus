#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnitClass {
    Block,
    // same read impl as block
    Legs,
    Elevated,
    Crawl,
    Boat,
    Tank,
    Air,
    // different read impl from block
    Mech,
    Tethered,
    Payload,
    Bomb,
}

pub static ID: [Option<UnitClass>; 47] = amap::amap! {
    2 => UnitClass::Block,
    24 => UnitClass::Legs,
    45 => UnitClass::Elevated,
    46 => UnitClass::Crawl,
    36 => UnitClass::Tethered,
    5 | 23 | 26 => UnitClass::Payload,
    39 => UnitClass::Bomb,
    20 => UnitClass::Boat,
    43 => UnitClass::Tank,
    4 | 17 | 19 | 32 => UnitClass::Mech,
    21 | 29 | 33 => UnitClass::Legs,
    3 | 16 | 18 | 0 | 30 | 31 => UnitClass::Air,
};
