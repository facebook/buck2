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
use buck2_core::provider::label::ProvidersLabel;
use dupe::Dupe;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::json_label;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(
    Debug, Pagable, Eq, PartialEq, Hash, Clone, Copy, Dupe, Allocative, StrongHash
)]
pub struct LabelAttrType;

impl LabelAttrType {
    pub(crate) fn configure(
        ctx: &dyn AttrConfigurationContext,
        label: &ProvidersLabel,
    ) -> buck2_error::Result<ConfiguredAttr> {
        Ok(ConfiguredAttr::Label(ctx.configure_target(label)))
    }
}

impl_semantics!(Label {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Label(_) => tr.label(l),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Label(_) => ResultMaybeCompatible::Compatible(LabelAttrType::configure(ctx, l)?),
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});
