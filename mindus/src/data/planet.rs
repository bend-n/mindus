//! planets
//!
//! [source](https://github.com/Anuken/Mindustry/blob/master/core/src/mindustry/content/Planets.java)
use crate::content::numeric_enum;

numeric_enum! {
    pub enum Planet for u8 | TryFromU8Error {
      Sun, Erekir, Gier, Notva, Tantros, Serpulo, Verlius
    }
}
