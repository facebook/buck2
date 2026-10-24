/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use dupe::Dupe;
use pagable::Pagable;
use serde::Serialize;
use serde::Serializer;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::AttrToken;
use crate::attrs::arena::CoercedAttrArenaBuilder;
use crate::attrs::arena::fields::Label;
use crate::attrs::arena::fields::Target;
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
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::strong_hash_without_config::StrongHashWithoutConfig;
use crate::provider_id_set::ProviderIdSet;

/// Represents attrs.configured_dep()
#[derive(
    Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable, StrongHash
)]
pub struct ExplicitConfiguredDepAttrType {
    pub required_providers: ProviderIdSet,
}

impl ExplicitConfiguredDepAttrType {
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
        label: &ProvidersLabel,
        platform: TargetLabel,
    ) -> buck2_error::Result<ConfiguredAttr> {
        let configuration = ctx.platform_cfg(&platform)?;
        Ok(ConfiguredAttr::ExplicitConfiguredDep(Box::new(
            ConfiguredExplicitConfiguredDep::new(self.dupe(), label.configure(configuration)),
        )))
    }
}

/// Represents the value of an `attrs.configured_dep()`
/// in its unconfigured form.
#[derive(
    derive_more::Display,
    Debug,
    Hash,
    StrongHash,
    PartialEq,
    Eq,
    Clone,
    Allocative,
    Pagable
)]
#[display("({}, {})", label, platform)]
pub struct UnconfiguredExplicitConfiguredDep {
    pub label: ProvidersLabel,
    pub platform: TargetLabel,
}

/// Represents the value of an `attrs.configured_dep()`
/// in its configured form.
#[derive(
    derive_more::Display,
    Hash,
    StrongHash,
    PartialEq,
    Eq,
    Debug,
    Clone,
    Allocative,
    Pagable
)]
#[display("{}", label)]
pub struct ConfiguredExplicitConfiguredDep {
    pub attr_type: ExplicitConfiguredDepAttrType,
    pub label: ConfiguredProvidersLabel,
}

impl ConfiguredExplicitConfiguredDep {
    pub fn new(attr_type: ExplicitConfiguredDepAttrType, label: ConfiguredProvidersLabel) -> Self {
        Self { attr_type, label }
    }
}

impl StrongHashWithoutConfig for ConfiguredExplicitConfiguredDep {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        let Self { attr_type, label } = self;
        attr_type.strong_hash(state);
        label.strong_hash_without_config(state);
    }
}

impl ConfiguredExplicitConfiguredDep {
    pub(crate) fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::to_value(self.to_string())?)
    }

    pub(crate) fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        filter(&self.to_string())
    }

    pub(crate) fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        traversal.dep(&self.label)
    }
}

impl SchemaKind for kinds::ExplicitConfiguredDep {
    type Fields = (Label, Target);
}

impl CoercedAttrArenaBuilder {
    pub fn encode_explicit_configured_dep(
        &mut self,
        label: &ProvidersLabel,
        platform: &TargetLabel,
    ) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::ExplicitConfiguredDep>(self, (label, platform))
    }
}

impl Semantics for kinds::ExplicitConfiguredDep {
    fn traverse<'a>(
        (label, platform): Self::View<'a>,
        Traverse { v, t, tr, .. }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfiguredDep(_) => {
                tr.dep(label)?;
                tr.configuration_dep(
                    &ProvidersLabel::default_for(platform.dupe()),
                    ConfigurationDepKind::ConfiguredDepPlatform,
                )
            }
            _ => Err(mismatch(v, t)),
        }
    }

    fn configure(
        (label, platform): Self::View<'_>,
        Configure { v, t, ctx, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfiguredDep(ty) => {
                ResultMaybeCompatible::Compatible(ty.configure(ctx, label, platform.dupe())?)
            }
            _ => mismatch(v, t).into(),
        }
    }

    fn to_json(
        (label, platform): Self::View<'_>,
        _: ToJson<'_, '_>,
    ) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value([label.to_string(), platform.to_string()])?)
    }

    fn fmt((label, platform): Self::View<'_>, Fmt { f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        write!(f, "({label}, {platform})")
    }

    fn any_matches(
        (label, platform): Self::View<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        filter(&format!("({label}, {platform})"))
    }

    fn serialize<S: Serializer>(
        (label, platform): Self::View<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        [label.to_string(), platform.to_string()].serialize(ser)
    }
}
