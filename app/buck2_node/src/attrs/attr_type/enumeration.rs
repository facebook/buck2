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
use std::hash::Hash;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_hash::BuckFixedInsertionSet;
use buck2_hash::BuckMutInsertionSet;
use buck2_util::arc_str::ArcStr;
use pagable::Pagable;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::fmt_text;
use crate::attrs::arena::semantics::impl_semantics;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(Debug, Pagable, Eq, PartialEq, Hash, Allocative)]
pub struct EnumAttrType {
    pub variants: BuckFixedInsertionSet<ArcStr>,
}

impl StrongHash for EnumAttrType {
    fn strong_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.variants.len().strong_hash(state);
        for v in self.variants.iter() {
            v.strong_hash(state);
        }
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum EnumAttrError {
    #[error("enum.attr() variant names must all be lowercase, got `{0}`")]
    NotLowercase(String),
    #[error("enum.attr() variant names must all be distinct, got repeated `{0}`")]
    DuplicateVariant(String),
}

impl EnumAttrType {
    pub fn new(variants: Vec<String>) -> buck2_error::Result<Self> {
        let mut result = BuckMutInsertionSet::with_capacity(variants.len());
        for x in variants {
            if x != x.to_lowercase() {
                return Err(EnumAttrError::NotLowercase(x).into());
            }
            if result.contains(x.as_str()) {
                return Err(EnumAttrError::DuplicateVariant(x).into());
            }
            result.insert(ArcStr::from(x));
        }
        Ok(Self {
            variants: result.freeze(),
        })
    }

    pub fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.enum([")?;
        for (i, x) in self.variants.iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "{x:?}")?;
        }
        write!(f, "]{arg})")
    }
}

impl_semantics!(EnumVariant {
    traverse(v, (s,), t, pkg, tr) { Ok(()) }
    configure(v, (s,), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::Enum(_) => ResultMaybeCompatible::Compatible(
                ConfiguredAttr::EnumVariant(StringLiteral(ArcStr::from(s))),
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
