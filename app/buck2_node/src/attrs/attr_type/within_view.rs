/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use dupe::Dupe;
use pagable::Pagable;
use serde::Serialize;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(
    Debug, Pagable, Eq, PartialEq, Hash, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct WithinViewAttrType;

impl_semantics!(WithinView {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) { ResultMaybeCompatible::Compatible(ConfiguredAttr::WithinView(s.clone())) }
    traverse_configured(v, (s,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (s,), ctx) { Ok(s.to_json()) }
    fmt(v, (s,), ctx, f) { write!(f, "{}", s) }
    any_matches(v, (s,), filter) { s.any_matches(filter) }
    serialize<S>(v, (s,), ctx, ser) { s.to_json().serialize(ser) }
});
