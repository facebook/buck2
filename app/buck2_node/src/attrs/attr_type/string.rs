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
use std::fmt;
use std::ops::Deref;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use pagable::Pagable;
use serde::Serialize;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::fmt_text;
use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::fmt_context::AttrFmtContext;

#[derive(
    Debug, Pagable, Eq, PartialEq, Hash, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct StringAttrType;

#[derive(
    Default, Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Serialize, Pagable, StrongHash
)]
#[serde(transparent)]
pub struct StringLiteral(pub ArcStr);

impl Deref for StringLiteral {
    type Target = ArcStr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AttrDisplayWithContext for StringLiteral {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if ctx.options.exclude_quotes {
            f.write_str(self.0.as_str())
        } else {
            write!(f, "\"{}\"", self.0)
        }
    }
}

impl_semantics!(String: concat "string" {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::String(_) | AttrTypeInner::Any(_) => ResultMaybeCompatible::Compatible(
                ConfiguredAttr::String(StringLiteral(ArcStr::from(s))),
            ),
            _ => mismatch(v, t).into(),
        }
    }
    traverse_configured(v, (s,), t, pkg, ctx, tr, name) { ResultMaybeCompatible::Compatible(()) }
    json(v, (s,), ctx) { Ok(to_value(s)?) }
    fmt(v, (s,), ctx, f) { fmt_text(s, ctx, f) }
    any_matches(v, (s,), filter) { filter(s) }
    json_key(v, (s,), ctx) { Ok(Cow::Borrowed(s)) }
    serialize<S>(v, (s,), ctx, ser) { ser.serialize_str(s) }
});
