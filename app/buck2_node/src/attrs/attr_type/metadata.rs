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
use crate::attrs::arena::fields::Idx;
use crate::attrs::arena::kinds;
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
use crate::metadata::map::MetadataMap;

#[derive(
    Debug, Pagable, Eq, PartialEq, Hash, Allocative, Clone, Copy, Dupe, StrongHash
)]
pub struct MetadataAttrType;

impl SchemaKind for kinds::Metadata {
    type Fields = Idx<MetadataMap>;
}

impl CoercedAttrArenaBuilder {
    pub fn encode_metadata(&mut self, m: &MetadataMap) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::Metadata>(self, m)
    }
}

impl Semantics for kinds::Metadata {
    fn traverse<'a>(_: Self::View<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(m: Self::View<'_>, _: Configure<'_, '_>) -> ResultMaybeCompatible<ConfiguredAttr> {
        ResultMaybeCompatible::Compatible(ConfiguredAttr::Metadata(m.clone()))
    }

    fn traverse_configured(
        _: Self::View<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json(m: Self::View<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        Ok(m.to_value())
    }

    fn fmt(m: Self::View<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "{}", m)
    }

    fn any_matches(
        m: Self::View<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        m.any_matches(filter)
    }

    fn serialize<S: Serializer>(
        m: Self::View<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        m.to_value().serialize(ser)
    }
}
