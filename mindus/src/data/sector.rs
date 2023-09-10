//! sectors
//!
//! [source](https://github.com/Anuken/Mindustry/blob/master/core/src/mindustry/content/SectorPresets.java)
use crate::content::numeric_enum;

numeric_enum! {
  pub enum Sector for u8 | TryFromU8Error {
    GroundZero,
    SaltFlats,
    FrozenForest,
    BiomassFacility,
    Craters,
    RuinousShores,
    WindsweptIslands,
    StainedMountains,
    ExtractionOutpost,
    Coastline,
    NavalFortress,
    FungalPass,
    Overgrowth,
    TarFields,
    Impact0078,
    DesolateRift,
    NuclearComplex,
    PlanetaryTerminal,
    Onset,
    Aegis,
    Lake,
    Intersect,
    Atlas,
    Split,
    Basin,
    Marsh,
    Peaks,
    Ravine,
    CalderaErekir,
    Stronghold,
    Crevice,
    Siege,
    Crossroads,
    Karst,
    Origin,
  }
}
