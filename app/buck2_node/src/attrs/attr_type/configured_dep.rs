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
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::semantics::impl_semantics;
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

impl_semantics!(ExplicitConfiguredDep {
    traverse(v, (label, platform), t, pkg, tr) {
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
    configure(v, (label, platform), t, ctx, name) {
        match &inner_type(t).0.inner {
            AttrTypeInner::ConfiguredDep(ty) => {
                ResultMaybeCompatible::Compatible(ty.configure(ctx, label, platform.dupe())?)
            }
            _ => mismatch(v, t).into(),
        }
    }
    json(v, (label, platform), ctx) { Ok(to_value([label.to_string(), platform.to_string()])?) }
    fmt(v, (label, platform), ctx, f) { write!(f, "({label}, {platform})") }
    any_matches(v, (label, platform), filter) { filter(&format!("({label}, {platform})")) }
    serialize<S>(v, (label, platform), ctx, ser) {
        [label.to_string(), platform.to_string()].serialize(ser)
    }
});
