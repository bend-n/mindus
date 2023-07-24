//! crate for dealing with mindustry
mod access;
pub mod block;
mod content;
pub mod data;
pub mod fluid;
pub mod item;
mod logic;
pub mod modifier;
mod registry;
mod team;
pub mod unit;
mod utils;
#[doc(inline)]
pub use {
    block::build_registry,
    data::{
        dynamic::DynData,
        map::{Map, MapSerializer},
        renderer::Renderable,
        schematic::{Schematic, SchematicSerializer},
        Serializer,
    },
};
