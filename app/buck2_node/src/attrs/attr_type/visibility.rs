/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use dupe::Dupe;
use pagable::Pagable;
use serde::Serialize;
use serde::Serializer;
use strong_hash::StrongHash;

use crate::attrs::arena::AttrToken;
use crate::attrs::arena::CoercedAttrArenaBuilder;
use crate::attrs::arena::CoercedAttrRef;
use crate::attrs::arena::CoercedAttrRefKind;
use crate::attrs::arena::fields::Idx;
use crate::attrs::arena::kinds;
use crate::attrs::arena::schema::AttrKind;
use crate::attrs::arena::schema::SchemaKind;
use crate::attrs::arena::schema::encode_kind;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::TraverseConfigured;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::visibility::VisibilitySpecification;

#[derive(
    Debug, Pagable, Eq, PartialEq, Hash, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct VisibilityAttrType;

impl SchemaKind for kinds::Visibility {
    const KIND: AttrKind = AttrKind::Visibility;
    type Fields = (Idx<VisibilitySpecification>,);

    fn wrap<'a>(
        _v: CoercedAttrRef<'a>,
        (v,): (&'a VisibilitySpecification,),
    ) -> CoercedAttrRefKind<'a> {
        CoercedAttrRefKind::Visibility(v)
    }
}

impl CoercedAttrArenaBuilder {
    pub fn encode_visibility(
        &mut self,
        v: &VisibilitySpecification,
    ) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::Visibility>(self, (v,))
    }
}

impl Semantics for kinds::Visibility {
    fn traverse<'a>(_: Self::Parts<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        (s,): Self::Parts<'_>,
        _: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        ResultMaybeCompatible::Compatible(ConfiguredAttr::Visibility(s.clone()))
    }

    fn traverse_configured(
        _: Self::Parts<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json((s,): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        Ok(s.to_json())
    }

    fn fmt((s,): Self::Parts<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "{}", s)
    }

    fn any_matches(
        (s,): Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        s.any_matches(filter)
    }

    fn serialize<S: Serializer>(
        (s,): Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        s.to_json().serialize(ser)
    }
}
