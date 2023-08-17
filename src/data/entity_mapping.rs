#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnitClass {
    Block,
    Legs,
    Elevated,
    Crawl,
    Mech,
    Tethered,
    Payload,
    Bomb,
    Boat,
    Tank,
}

pub static ID: [Option<UnitClass>; 47] = amap::amap! {
    2 => UnitClass::Block,
    24 => UnitClass::Legs,
    45 => UnitClass::Elevated,
    46 => UnitClass::Crawl,
    4 => UnitClass::Mech,
    36 => UnitClass::Tethered,
    5 => UnitClass::Payload,
    39 => UnitClass::Bomb,
    20 => UnitClass::Boat,
    43 => UnitClass::Tank,
};
