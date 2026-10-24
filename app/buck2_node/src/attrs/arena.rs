/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Arena storage for coerced attributes.
//!
//! One [`CoercedAttrArena`] holds every attribute value coerced for one
//! package: a byte buffer of tagged records plus typed side tables (shares
//! of globally interned labels and of the package listing's paths, and
//! tables of the whole-value leaves, one entry per distinct value). An
//! [`AttrWord`] names a value in it: either an inline leaf, held in the
//! word itself, or the offset of a record. The byte format is in `storage`;
//! what each kind of record holds is the kinds table in `schema`, and what
//! each kind means to traversal, configuration and rendering is its
//! `Semantics` impl beside its attribute type.
//!
//! An arena is *sealed* when its package finishes evaluating: writes end,
//! reads begin (distinct from starlark *freezing*, which happens earlier,
//! to the values attrs are coerced from). Three properties carry the
//! design:
//!
//! - offsets are indices, not addresses: the buffer can grow while the
//!   package coerces, and the sealed form is position-independent — one
//!   pagable blob per package instead of one allocation per value;
//! - an `AttrWord` is a shared handle: value content is stored once (the
//!   builder dedups on encode) and referenced from any number of parent
//!   records; refs within a record are fixed-stride, so they stay
//!   indexable;
//! - refs only ever point at already-encoded values (children are encoded
//!   before their parents), so a ref cannot be observed half-built and
//!   deltas are never negative.

mod arg;
pub(crate) mod builder;
pub(crate) mod fields;
mod owned;
mod query_expr;
pub(crate) mod schema;
pub(crate) mod storage;
#[cfg(test)]
mod tests;
mod view;

pub use arg::ArgMacroView;
pub use arg::ArgPartView;
pub use arg::ArgQueryView;
pub use builder::CoercedAttrArenaBuilder;
pub use owned::OwnedCoercedAttr;
pub use query_expr::QueryExprRef;
pub use storage::AttrToken;
pub use storage::AttrWord;
pub use storage::CoercedAttrArena;
pub use storage::CoercedAttrHash;
pub use view::ArgParts;
pub use view::AttrRefSelectorEntries;
pub use view::CoercedAttrRef;
pub use view::Pairs;
pub use view::RefItem;
pub use view::Refs;
pub use view::Values;
