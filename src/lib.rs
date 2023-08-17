//! crate for dealing with mindustry
#![feature(
    array_chunks,
    const_trait_impl,
    unchecked_math,
    slice_as_chunks,
    slice_swap_unchecked,
    let_chains
)]
#![allow(clippy::missing_safety_doc, clippy::missing_const_for_fn, clippy::perf)]
pub mod block;
mod content;
pub mod data;
pub mod fluid;
pub mod item;
mod logic;
pub mod modifier;
mod team;
pub mod unit;
mod utils;
#[doc(inline)]
pub use data::{map::Map, renderer::Renderable, schematic::Schematic, Serializable};
