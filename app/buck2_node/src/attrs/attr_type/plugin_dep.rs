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
use serde::Serializer;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::kinds;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::JsonKey;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::TraverseConfigured;
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

impl Semantics for kinds::PluginDep {
    fn traverse<'a>(
        (l,): Self::Parts<'a>,
        Traverse { v, t, tr, .. }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        match &inner_type(t).0.inner {
            AttrTypeInner::PluginDep(ty) => tr.plugin_dep(l, ty.kind()),
            _ => Err(mismatch(v, t)),
        }
    }

    fn configure(
        (l,): Self::Parts<'_>,
        Configure { v, t, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &inner_type(t).0.inner {
            AttrTypeInner::PluginDep(ty) => ResultMaybeCompatible::Compatible(
                ConfiguredAttr::PluginDep(l.dupe(), ty.kind().dupe()),
            ),
            _ => mismatch(v, t).into(),
        }
    }

    fn traverse_configured(
        (l,): Self::Parts<'_>,
        TraverseConfigured { v, t, tr, .. }: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
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

    fn to_json((l,): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value(l.to_string())?)
    }

    fn fmt((l,): Self::Parts<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "\"{l}\"")
    }

    fn any_matches(
        (l,): Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        filter(&l.to_string())
    }

    fn json_key<'a>(
        (l,): Self::Parts<'a>,
        _: JsonKey<'_, 'a>,
    ) -> buck2_error::Result<Cow<'a, str>> {
        Ok(Cow::Owned(l.to_string()))
    }

    fn serialize<S: Serializer>(
        (l,): Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&l.to_string())
    }
}
