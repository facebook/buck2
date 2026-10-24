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
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use dupe::Dupe;
use pagable::Pagable;
use serde::Deserialize;
use serde::Serialize;
use serde::Serializer;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::kinds;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::TraverseConfigured;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(
    Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct BoolAttrType;

#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    StrongHash,
    Allocative,
    Serialize,
    Deserialize,
    Pagable
)]
#[serde(transparent)]
pub struct BoolLiteral(pub bool);

impl Display for BoolLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0 {
            write!(f, "True")
        } else {
            write!(f, "False")
        }
    }
}

impl AnyMatches for BoolLiteral {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        Ok(filter(if self.0 { "True" } else { "False" })?
            || filter(if self.0 { "true" } else { "false" })?)
    }
}

impl Semantics for kinds::BoolFalse {
    fn traverse<'a>((): Self::Parts<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        (): Self::Parts<'_>,
        Configure { v, t, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &inner_type(t).0.inner {
            AttrTypeInner::Bool(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Bool(BoolLiteral(false)))
            }
            _ => mismatch(v, t).into(),
        }
    }

    fn traverse_configured(
        (): Self::Parts<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json((): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value(false)?)
    }

    fn fmt((): Self::Parts<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "{}", BoolLiteral(false))
    }

    fn any_matches(
        (): Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        BoolLiteral(false).any_matches(filter)
    }

    fn serialize<S: Serializer>(
        (): Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_bool(false)
    }
}

impl Semantics for kinds::BoolTrue {
    fn traverse<'a>((): Self::Parts<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        (): Self::Parts<'_>,
        Configure { v, t, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &inner_type(t).0.inner {
            AttrTypeInner::Bool(_) | AttrTypeInner::Any(_) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Bool(BoolLiteral(true)))
            }
            _ => mismatch(v, t).into(),
        }
    }

    fn traverse_configured(
        (): Self::Parts<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json((): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value(true)?)
    }

    fn fmt((): Self::Parts<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "{}", BoolLiteral(true))
    }

    fn any_matches(
        (): Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        BoolLiteral(true).any_matches(filter)
    }

    fn serialize<S: Serializer>(
        (): Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_bool(true)
    }
}
