/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::plugins::PluginKind;
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
    Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, Dupe, StrongHash
)]
pub struct PluginDepAttrType {
    kind: PluginKind,
}

impl PluginDepAttrType {
    pub fn new(kind: PluginKind) -> Self {
        Self { kind }
    }

    pub fn kind(&self) -> &PluginKind {
        &self.kind
    }
}

impl_semantics!(PluginDep {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::PluginDep(ty) => tr.plugin_dep(l, ty.kind()),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::PluginDep(ty) => ResultMaybeCompatible::Compatible(ConfiguredAttr::PluginDep(
                l.dupe(),
                ty.kind().dupe(),
            )),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (l,), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::PluginDep(ty) => {
                if let Err(e) = tr.plugin_dep(l, ty.kind()) {
                    return e.into();
                }
            }
            _ => return mismatch(v, t).into(),
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (l,), ctx) { Ok(to_value(l.to_string())?) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});
