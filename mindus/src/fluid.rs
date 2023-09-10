//! fluids
//!
//! [source](https://github.com/Anuken/Mindustry/blob/master/core/src/mindustry/content/Liquids.java)
use crate::content::color_content_enum;

color_content_enum! {
    pub enum Type / Fluid for u16 | TryFromU16Error
    {
        "water": "596ab8",
        "slag": "ffa166",
        "oil": "313131",
        "cryofluid": "6ecdec",
        "neoplasm": "c33e2b",
        "arkycite": "84a94b",
        "gallium": "9a9dbf",
        "ozone": "fc81dd",
        "hydrogen": "9eabf7",
        "nitrogen": "efe3ff",
        "cyanogen": "89e8b6",
    }
}
