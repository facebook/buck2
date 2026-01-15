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
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use dupe::Dupe;
use pagable::Pagable;

use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

/// Represents attrs.configured_dep()
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
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
#[derive(
    derive_more::Display,
    Debug,
    Hash,
    PartialEq,
    Eq,
    Clone,
    Allocative,
    Pagable
)]
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

impl UnconfiguredExplicitConfiguredDep {
    pub(crate) fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::to_value([
            self.label.to_string(),
            self.platform.to_string(),
        ])?)
    }

    pub(crate) fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        filter(&self.to_string())
    }

    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        traversal.dep(&self.label)?;
        let label = ProvidersLabel::default_for(self.platform.dupe());
        traversal.configuration_dep(&label, ConfigurationDepKind::ConfiguredDepPlatform)
    }
}
