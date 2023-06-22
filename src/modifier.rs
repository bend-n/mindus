use crate::content::content_enum;

content_enum! {
    pub enum Type / Modifier for u16 | TryFromU16Error {
        "none",
        "burning",
        "freezing",
        "unmoving",
        "slow",
        "wet",
        "muddy",
        "melting",
        "sapped",
        "electrified",
        "spore-slowed",
        "tarred",
        "overdrive",
        "overclock",
        "shielded",
        "boss",
        "shocked",
        "blasted",
        "corroded",
        "disarmed",
        "invincible",
    }
}
