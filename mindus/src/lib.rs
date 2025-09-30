//! crate for dealing with mindustry
#![feature(
    error_generic_member_access,
    maybe_uninit_write_slice,
    try_trait_v2_residual,
    stmt_expr_attributes,
    generic_const_exprs,
    iter_from_coroutine,
    const_trait_impl,
    coroutine_trait,
    likely_unlikely,
    portable_simd,
    derive_const,
    try_trait_v2,
    try_blocks,
    const_convert,
    coroutines
)]
#![allow(clippy::redundant_closure_call, incomplete_features)]
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
pub use data::{Serializable, map::Map, renderer::Renderable, schematic::Schematic};
