/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Pagable serialization framework.
//!
//! The main traits for types to implement are:
//! - [`PagableSerialize`] - implemented by types that can be serialized
//! - [`PagableDeserialize`] - implemented by types that can be deserialized
//! - [`Pagable`] - a convenience trait combining both PagableSerialize and PagableDeserialize with `Send + Sync + Debug`
//!
//! Implementations can use the derive macros `#[derive(Pagable)]` (which will derive both serialize and deserialize), `#[derive(PagableSerialize)]`,
//! and `#[derive(PagableDeserialize)]` for automatic implementations.

pub use pagable_arc::PagableArc;
pub use pagable_arc::PinnedPagableArc;
pub use pagable_arc::PinnedPagableArcBorrow;
pub use pagable_derive::Pagable;
pub use pagable_derive::PagableDeserialize;
pub use pagable_derive::PagableSerialize;

pub mod arc_erase;
pub mod context;
mod impls;
mod pagable_arc;
pub mod storage;
mod test;
pub mod testing;
pub mod traits;

pub use traits::Pagable;
pub use traits::PagableDeserialize;
pub use traits::PagableDeserializer;
pub use traits::PagableEagerDeserialize;
pub use traits::PagableEagerSerialize;
pub use traits::PagableSerialize;
pub use traits::PagableSerializer;

pub type Result<O> = anyhow::Result<O>;
pub type Error = anyhow::Error;

pub mod __internal {
    pub use anyhow;
    pub use serde;
}
