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
use serde::Serializer;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::AttrToken;
use crate::attrs::arena::CoercedAttrArenaBuilder;
use crate::attrs::arena::CoercedAttrRef;
use crate::attrs::arena::CoercedAttrRefKind;
use crate::attrs::arena::fields::Text;
use crate::attrs::arena::kinds;
use crate::attrs::arena::schema::AttrKind;
use crate::attrs::arena::schema::SchemaKind;
use crate::attrs::arena::schema::encode_kind;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::JsonKey;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::TraverseConfigured;
use crate::attrs::arena::semantics::fmt_text;
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

impl SchemaKind for kinds::String {
    const KIND: AttrKind = AttrKind::String;
    type Fields = Text;

    fn wrap<'a>(_v: CoercedAttrRef<'a>, s: &'a str) -> CoercedAttrRefKind<'a> {
        CoercedAttrRefKind::String(s)
    }
}

impl CoercedAttrArenaBuilder {
    pub fn encode_string(&mut self, s: &str) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::String>(self, s)
    }
}

impl Semantics for kinds::String {
    const CONCAT: Option<&'static str> = Some("string");

    fn traverse<'a>(_: Self::Parts<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        s: Self::Parts<'_>,
        Configure { v, t, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &inner_type(t).0.inner {
            AttrTypeInner::String(_) | AttrTypeInner::Any(_) => ResultMaybeCompatible::Compatible(
                ConfiguredAttr::String(StringLiteral(ArcStr::from(s))),
            ),
            _ => mismatch(v, t).into(),
        }
    }

    fn traverse_configured(
        _: Self::Parts<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json(s: Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value(s)?)
    }

    fn fmt(s: Self::Parts<'_>, Fmt { ctx, f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        fmt_text(s, ctx, f)
    }

    fn any_matches(
        s: Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        filter(s)
    }

    fn json_key<'a>(s: Self::Parts<'a>, _: JsonKey<'_, 'a>) -> buck2_error::Result<Cow<'a, str>> {
        Ok(Cow::Borrowed(s))
    }

    fn serialize<S: Serializer>(
        s: Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_str(s)
    }
}
