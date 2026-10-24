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
use serde::Serializer;
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
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::json_label;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;

/// Describes where a configuration dep appears
#[repr(u8)]
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, Hash, Pagable, Allocative, Dupe, StrongHash
)]
pub enum ConfigurationDepKind {
    SelectKey,
    CompatibilityAttribute,
    ConfiguredDepPlatform,
    Transition,
    DefaultTargetPlatform,
}

/// A configuration dep attribute accepts a target as a value. This is different from
/// a dep in that the values themselves never undergo configuration and appear as bare
/// unconfigured labels even in the configured node. While the values aren't configured,
/// the attribute still is and so selects are still resolved and other values in the
/// attribute could be configured (for example, a `attrs.dict(attrs.dep(), attrs.configuration_dep())`
/// would have the keys configured).
///
/// This is generally used for things that refer to configuration nodes (like platforms or constraints)
/// in attributes like `target_compatible_with` or `exec_compatible_with`.
///
/// They resolve to just the string form of the target and so aren't particularly useful to UDR
/// directly (they are used by the framework).
#[derive(
    Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct ConfigurationDepAttrType(pub ConfigurationDepKind);

impl ConfigurationDepAttrType {
    pub(crate) fn configure(
        _ctx: &dyn AttrConfigurationContext,
        label: &ProvidersLabel,
    ) -> buck2_error::Result<ConfiguredAttr> {
        Ok(ConfiguredAttr::ConfigurationDep(label.dupe()))
    }
}

impl Semantics for kinds::ConfigurationDep {
    fn traverse<'a>(
        (l,): Self::Parts<'a>,
        Traverse { v, t, tr, .. }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfigurationDep(ty) => tr.configuration_dep(l, ty.0),
            _ => Err(mismatch(v, t)),
        }
    }

    fn configure(
        (l,): Self::Parts<'_>,
        Configure { v, t, ctx, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfigurationDep(_) => {
                ResultMaybeCompatible::Compatible(ConfigurationDepAttrType::configure(ctx, l)?)
            }
            _ => mismatch(v, t).into(),
        }
    }

    fn to_json((l,): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        json_label(l)
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
