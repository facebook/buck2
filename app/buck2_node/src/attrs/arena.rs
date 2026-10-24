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
//! An attribute value is a tagged record in a per-package byte buffer; an
//! [`AttrWord`] is the byte offset of the record's tag.
//!
//! ```text
//! AttrWord: [ 1 | kind:5 | payload:26 ]      inline leaf: None, bools,
//!                                            small ints, table indices
//!           [ 0 | record offset:31 ]         everything else
//!
//! record:   tag:u8 = [ width:2 | kind:6 ]   head fields...   tail
//!
//!   head    the record's immediate content (counts, table indices, split
//!           ints) as `width + 1`-byte little-endian fields; how many, and
//!           what follows, is the kind's layout (`AttrKind::layout`);
//!   tail    text bytes (length in the first head field), or child refs,
//!           `width + 1` bytes each: bit 0 set = an inline leaf, kind in the
//!           next five bits and payload above; bit 0 clear = a record ref,
//!           the backward delta from the record's own offset above; a zero
//!           field marks an absent value. The width is chosen per record
//!           from its largest head field or ref.
//! ```
//!
//! A value's kind costs at most one byte read: inline leaves carry it in
//! the word, records in their tag, so a traversal over deps opens only
//! containers.
//! Each kind's representation is documented on its `AttrKind` variant.
//! Worked example — `{"srcs": ["a.c"]}`, a dict of one pair whose value is
//! a one-element list of a source file:
//!
//! ```text
//! bytes:  [String  4 "srcs"]   [List  1  SourceFile#0]   [Dict  1  ->"srcs" ->list]
//!          ^ S                  ^ L                      ^ D = the value's own ref
//!
//! the list's one ref is the inline source leaf (path table index 0); the
//! dict's refs are deltas D - S and D - L, all one byte wide here.
//! ```
//!
//! One [`CoercedAttrArena`] holds every attribute value coerced for one package:
//! the byte buffer plus typed side tables — shares of globally interned
//! labels and of the package listing's paths, and tables of the whole-value
//! leaves (visibility, within_view, metadata, modifiers), one entry per
//! distinct value. An arena is *sealed* when its package finishes
//! evaluating: writes end, reads begin (distinct from starlark *freezing*,
//! which happens earlier, to the values attrs are coerced from). Three
//! properties carry the design:
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
mod builder;
mod fields;
mod owned;
mod query_expr;
mod schema;
mod semantics;
mod storage;
#[cfg(test)]
mod tests;
mod view;

pub use arg::ArgMacroView;
pub use arg::ArgPartView;
pub use arg::ArgQueryView;
pub use builder::CoercedAttrArenaBuilder;
pub use owned::OwnedCoercedAttr;
pub use query_expr::QueryView;
pub use storage::AttrToken;
pub use storage::AttrWord;
pub use storage::CoercedAttrArena;
pub use storage::CoercedAttrHash;
pub use view::ArgParts;
pub use view::AttrRefSelectorEntries;
pub use view::AttrView;
pub use view::CoercedAttrRef;
pub use view::CoercedAttrRefKind;
pub use view::Pairs;
pub use view::RefItem;
pub use view::Refs;
pub use view::Values;
