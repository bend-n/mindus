//! crate for dealing with mindustry
#![feature(
    stmt_expr_attributes,
    iter_from_coroutine,
    generic_arg_infer,
    const_trait_impl,
    coroutine_trait,
    const_option,
    derive_const,
    coroutines,
    slice_take,
    let_chains,
    effects
)]
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
pub mod color_mapping;
pub mod content;
pub mod data;
pub mod fluid;
pub mod item;
pub mod modifier;
mod team;
pub mod unit;
mod utils;
#[doc(inline)]
pub use data::{map::Map, renderer::Renderable, schematic::Schematic, Serializable};
