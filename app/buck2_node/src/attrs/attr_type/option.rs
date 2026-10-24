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

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::attr_type::AttrType;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative, StrongHash)]
pub struct OptionAttrType {
    pub inner: AttrType,
}

impl OptionAttrType {
    pub fn new(inner: AttrType) -> Self {
        Self { inner }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.option({}{})", self.inner, arg)
    }
}

impl_semantics!(None {
    traverse(v, (), t, pkg, tr) { Ok(()) }
    configure(v, (), t, ctx, name) { ResultMaybeCompatible::Compatible(ConfiguredAttr::None) }
    traverse_configured(v, (), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (), ctx) { Ok(serde_json::Value::Null) }
    fmt(v, (), ctx, f) { write!(f, "None") }
    any_matches(v, (), filter) { Ok(false) }
    serialize<S>(v, (), ctx, ser) { ser.serialize_unit() }
});
