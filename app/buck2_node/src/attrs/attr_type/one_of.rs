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
use buck2_error::BuckErrorOptionContext;
use buck2_error::internal_error;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::serialize::AttrSerializeWithContext;

#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative, StrongHash)]
pub struct OneOfAttrType {
    pub xs: Vec<AttrType>,
}

impl OneOfAttrType {
    pub fn new(xs: Vec<AttrType>) -> OneOfAttrType {
        Self { xs }
    }

    pub fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.one_of(")?;
        for (i, x) in self.xs.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{x}")?;
        }
        write!(f, "{arg})")
    }

    pub(crate) fn any_supports_concat(&self) -> bool {
        self.xs.iter().any(AttrType::supports_concat)
    }

    pub(crate) fn get(&self, i: u32) -> buck2_error::Result<&AttrType> {
        self.xs
            .get(i as usize)
            .with_internal_error(|| format!("Oneof index ({i}) out of bounds (internal error)"))
    }
}

impl_semantics!(OneOf {
    traverse(v, (idx, inner), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::OneOf(ty) => {
                let item_t = ty
                    .xs
                    .get(idx as usize)
                    .ok_or_else(|| internal_error!("invalid oneof index"))?;
                inner.traverse(item_t, pkg, tr)
            }
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (idx, inner), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::OneOf(ty) => {
                let item_t = match ty.xs.get(idx as usize) {
                    Some(ty) => ty,
                    None => return internal_error!("invalid oneof index").into(),
                };
                let configured = inner.configure(item_t, ctx, name)?;
                ResultMaybeCompatible::Compatible(ConfiguredAttr::OneOf(Box::new(configured), idx))
            }
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (idx, inner), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => ResultMaybeCompatible::Compatible(()),
            AttrTypeInner::OneOf(ty) => {
                let item_t = match ty.xs.get(idx as usize) {
                    Some(ty) => ty,
                    None => return internal_error!("invalid oneof index").into(),
                };
                inner.traverse_configured(item_t, pkg, ctx, tr, name)
            }
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (idx, inner), ctx) { inner.to_json(ctx) }
    fmt(v, (idx, inner), ctx, f) { AttrDisplayWithContext::fmt(&inner, ctx, f) }
    any_matches(v, (idx, inner), filter) { inner.any_matches(filter) }
    json_key(v, (idx, inner), ctx) { inner.json_key(ctx) }
    serialize<S>(v, (idx, inner), ctx, ser) { inner.serialize_with_ctx(ctx, ser) }
});
