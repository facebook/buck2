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
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(
    Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct IntAttrType;

impl_semantics!(Int {
    traverse(v, i, t, pkg, tr) { Ok(()) }
    configure(v, i, t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Int(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Int(i))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, i, t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, i, ctx) { Ok(to_value(i)?) }
    fmt(v, i, ctx, f) { write!(f, "{i}") }
    any_matches(v, i, filter) { filter(&i.to_string()) }
    serialize<S>(v, i, ctx, ser) { ser.serialize_i64(i) }
});
