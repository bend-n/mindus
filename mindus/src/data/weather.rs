//! weathers
//!
//! [source](https://github.com/Anuken/Mindustry/blob/master/core/src/mindustry/content/Weathers.java)
use crate::content::numeric_enum;

numeric_enum! {
    pub enum Weather for u8 | TryFromU8Error {
      Snow, Rain, Sandstorm, Sporestorm, Fog, SuspendParticles
    }
}
