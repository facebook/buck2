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
use buck2_core::package::source_path::SourcePathRef;
use buck2_error::internal_error;
use dupe::Dupe;
use pagable::Pagable;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::json_label;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::attr_config::source_file_display;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(
    Debug, Pagable, Eq, PartialEq, Hash, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct SourceAttrType {
    pub allow_directory: bool,
}

impl_semantics!(SourceLabel {
    traverse(v, (l,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => tr.dep(l),
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (l,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => ResultMaybeCompatible::Compatible(ConfiguredAttr::SourceLabel(
                l.configure_pair(ctx.cfg().cfg_pair().dupe()),
            )),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (l,), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::Source(_) => {
                let configured = l.configure_pair(ctx.cfg().cfg_pair().dupe());
                if let Err(e) = tr.dep(&configured) {
                    return e.into();
                }
            }
            _ => return mismatch(v, t).into(),
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (l,), ctx) { json_label(l) }
    fmt(v, (l,), ctx, f) { write!(f, "\"{l}\"") }
    any_matches(v, (l,), filter) { filter(&l.to_string()) }
    json_key(v, (l,), ctx) { Ok(Cow::Owned(l.to_string())) }
    serialize<S>(v, (l,), ctx, ser) { ser.serialize_str(&l.to_string()) }
});

impl_semantics!(SourceFile {
    traverse(v, (p,), t, pkg, tr) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => {
                let pkg = pkg.ok_or_else(|| {
                    internal_error!("Expected a package when traversing coerced source attribute")
                })?;
                for x in p.inputs() {
                    tr.input(SourcePathRef::new(pkg, x))?;
                }
                Ok(())
            }
            _ => Err(mismatch(v, t)),
        }
    }
    configure(v, (p,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Source(_) => ResultMaybeCompatible::Compatible(ConfiguredAttr::SourceFile(p.clone())),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (p,), t, pkg, ctx, tr, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::Source(_) => {
                for x in p.inputs() {
                    if let Err(e) = tr.input(SourcePathRef::new(pkg, x)) {
                        return e.into();
                    }
                }
            }
            _ => return mismatch(v, t).into(),
        }
        ResultMaybeCompatible::Compatible(())
    }
    json(v, (p,), ctx) { Ok(to_value(source_file_display(ctx, p).to_string())?) }
    fmt(v, (p,), ctx, f) { write!(f, "\"{}\"", source_file_display(ctx, p)) }
    any_matches(v, (p,), filter) { filter(&p.path().to_string()) }
    json_key(v, (p,), ctx) { Ok(Cow::Owned(source_file_display(ctx, p).to_string())) }
    serialize<S>(v, (p,), ctx, ser) { ser.serialize_str(&source_file_display(ctx, p).to_string()) }
});
