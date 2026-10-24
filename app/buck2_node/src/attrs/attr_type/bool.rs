/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use dupe::Dupe;
use pagable::Pagable;
use serde::Deserialize;
use serde::Serialize;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(
    Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct BoolAttrType;

#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    StrongHash,
    Allocative,
    Serialize,
    Deserialize,
    Pagable
)]
#[serde(transparent)]
pub struct BoolLiteral(pub bool);

impl Display for BoolLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0 {
            write!(f, "True")
        } else {
            write!(f, "False")
        }
    }
}

impl AnyMatches for BoolLiteral {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        Ok(filter(if self.0 { "True" } else { "False" })?
            || filter(if self.0 { "true" } else { "false" })?)
    }
}

impl_semantics!(BoolFalse {
    traverse(v, (), t, pkg, tr) { Ok(()) }
    configure(v, (), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Bool(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Bool(BoolLiteral(false)))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (), ctx) { Ok(to_value(false)?) }
    fmt(v, (), ctx, f) { write!(f, "{}", BoolLiteral(false)) }
    any_matches(v, (), filter) { BoolLiteral(false).any_matches(filter) }
    serialize<S>(v, (), ctx, ser) { ser.serialize_bool(false) }
});

impl_semantics!(BoolTrue {
    traverse(v, (), t, pkg, tr) { Ok(()) }
    configure(v, (), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Bool(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Bool(BoolLiteral(true)))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (), ctx) { Ok(to_value(true)?) }
    fmt(v, (), ctx, f) { write!(f, "{}", BoolLiteral(true)) }
    any_matches(v, (), filter) { BoolLiteral(true).any_matches(filter) }
    serialize<S>(v, (), ctx, ser) { ser.serialize_bool(true) }
});
