/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use dupe::Dupe;

use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::attr_type::dep::ExplicitConfiguredDepMaybeConfigured;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

/// Represents attrs.configured_dep()
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative)]
pub struct ExplicitConfiguredDepAttrType {
    pub required_providers: ProviderIdSet,
}

impl ExplicitConfiguredDepAttrType {
    pub(crate) fn configure(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &UnconfiguredExplicitConfiguredDep,
    ) -> buck2_error::Result<ConfiguredAttr> {
        let configured = Self::configure_target_with_platform(ctx, dep_attr)?;
        Ok(ConfiguredAttr::ExplicitConfiguredDep(Box::new(configured)))
    }

    fn configure_target_with_platform(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &UnconfiguredExplicitConfiguredDep,
    ) -> buck2_error::Result<ConfiguredExplicitConfiguredDep> {
        let configuration = ctx.platform_cfg(&dep_attr.platform)?;
        let configured_label = dep_attr.label.configure(configuration.dupe());
        Ok(ConfiguredExplicitConfiguredDep::new(
            dep_attr.attr_type.dupe(),
            configured_label,
        ))
    }
}

/// Represents the value of an `attrs.configured_dep()`
/// in its unconfigured form.
#[derive(derive_more::Display, Debug, Hash, PartialEq, Eq, Clone, Allocative)]
#[display("({}, {})", label, platform)]
pub struct UnconfiguredExplicitConfiguredDep {
    pub attr_type: ExplicitConfiguredDepAttrType,
    pub label: ProvidersLabel,
    pub platform: TargetLabel,
}

/// Represents the value of an `attrs.configured_dep()`
/// in its configured form.
#[derive(derive_more::Display, Hash, PartialEq, Eq, Debug, Clone, Allocative)]
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

impl UnconfiguredExplicitConfiguredDep {
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        traversal.dep(&self.label)?;
        let label = ProvidersLabel::default_for(self.platform.dupe());
        traversal.configuration_dep(&label, ConfigurationDepKind::ConfiguredDepPlatform)
    }
}

impl ExplicitConfiguredDepMaybeConfigured for ConfiguredExplicitConfiguredDep {
    fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::to_value(self.to_string())?)
    }

    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        filter(&self.to_string())
    }
}

impl ExplicitConfiguredDepMaybeConfigured for UnconfiguredExplicitConfiguredDep {
    fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::to_value([
            self.label.to_string(),
            self.platform.to_string(),
        ])?)
    }

    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        filter(&self.to_string())
    }
}
