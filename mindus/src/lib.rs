//! crate for dealing with mindustry
#![feature(const_trait_impl, let_chains, effects, try_blocks, const_option)]
#![allow(clippy::redundant_closure_call)]
#![warn(
    clippy::multiple_unsafe_ops_per_block,
    clippy::missing_const_for_fn,
    clippy::missing_safety_doc,
    unsafe_op_in_unsafe_fn,
    clippy::dbg_macro,
    clippy::perf
)]
pub mod block;
mod content;
pub mod data;
pub mod fluid;
pub mod item;
pub mod modifier;
mod team;
pub mod unit;
mod utils;
#[doc(inline)]
pub use data::{map::Map, renderer::Renderable, schematic::Schematic, Serializable};
