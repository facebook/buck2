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
use pagable::Pagable;
use serde::Serializer;
use strong_hash::StrongHash;

use crate::attrs::arena::AttrToken;
use crate::attrs::arena::CoercedAttrArenaBuilder;
use crate::attrs::arena::CoercedAttrRef;
use crate::attrs::arena::CoercedAttrRefKind;
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
use crate::attrs::attr_type::AttrType;
use crate::attrs::configured_attr::ConfiguredAttr;

#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative, StrongHash)]
pub struct OptionAttrType {
    pub inner: AttrType,
}

impl OptionAttrType {
    pub fn new(inner: AttrType) -> Self {
        Self { inner }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.option({}{})", self.inner, arg)
    }
}

impl SchemaKind for kinds::None {
    const KIND: AttrKind = AttrKind::None;
    type Fields = ();

    fn wrap<'a>(_v: CoercedAttrRef<'a>, (): ()) -> CoercedAttrRefKind<'a> {
        CoercedAttrRefKind::None
    }
}

impl CoercedAttrArenaBuilder {
    pub fn encode_none(&mut self) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::None>(self, ())
    }
}

impl Semantics for kinds::None {
    fn traverse<'a>((): Self::Parts<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        (): Self::Parts<'_>,
        _: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        ResultMaybeCompatible::Compatible(ConfiguredAttr::None)
    }

    fn traverse_configured(
        (): Self::Parts<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json((): Self::Parts<'_>, _: ToJson<'_, '_>) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::Value::Null)
    }

    fn fmt((): Self::Parts<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "None")
    }

    fn any_matches((): Self::Parts<'_>, _: MatchesAny<'_, '_>) -> buck2_error::Result<bool> {
        Ok(false)
    }

    fn serialize<S: Serializer>(
        (): Self::Parts<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_unit()
    }
}
