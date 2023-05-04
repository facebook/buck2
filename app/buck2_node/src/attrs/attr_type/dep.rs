/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use dupe::Dupe;
use static_assertions::assert_eq_size;

use super::attr_config::ConfiguredAttrExtraTypes;
use crate::attrs::attr_type::attr_like::AttrLike;
use crate::attrs::attr_type::configured_dep::ConfiguredExplicitConfiguredDep;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

// Just a placeholder for what a label should resolve to.
#[derive(Debug)]
pub struct DefaultProvider {}

/// How configuration is changed when configuring a dep.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative)]
pub enum DepAttrTransition {
    // No transition.
    Identity,
    // Transition to execution platform.
    Exec,
    // Transition to toolchain.
    Toolchain,
    // Transition dependency using given transition function.
    Transition(Arc<TransitionId>),
}

/// A dep attribute accepts a target label and will resolve to the provider collection from that label's analysis.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative)]
pub struct DepAttrType {
    /// The set of providers that are required to be available, during attr resolution we'll verify that these
    /// are present on each attribute value.
    pub required_providers: ProviderIdSet,
    pub transition: DepAttrTransition,
}

assert_eq_size!(DepAttrType, [usize; 3]);

#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative)]
pub struct DepAttr<T: ProvidersLabelMaybeConfigured + AttrLike> {
    pub attr_type: DepAttrType,
    pub label: T,
}

impl<T: ProvidersLabelMaybeConfigured + AttrLike> Display for DepAttr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.label, f)
    }
}

impl DepAttr<ConfiguredProvidersLabel> {
    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> anyhow::Result<()> {
        match &self.attr_type.transition {
            DepAttrTransition::Identity => traversal.dep(&self.label),
            DepAttrTransition::Exec => traversal.exec_dep(&self.label),
            DepAttrTransition::Toolchain => traversal.toolchain_dep(&self.label),
            DepAttrTransition::Transition(..) => traversal.dep(&self.label),
        }
    }
}

impl DepAttr<ProvidersLabel> {
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match &self.attr_type.transition {
            DepAttrTransition::Identity => traversal.dep(self.label.target()),
            DepAttrTransition::Exec => traversal.exec_dep(self.label.target()),
            DepAttrTransition::Toolchain => traversal.toolchain_dep(self.label.target()),
            DepAttrTransition::Transition(tr) => traversal.transition_dep(self.label.target(), tr),
        }
    }
}

impl DepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: DepAttrTransition) -> Self {
        Self {
            required_providers,
            transition,
        }
    }

    pub(crate) fn configure(
        ctx: &dyn AttrConfigurationContext,
        dep_attr: &DepAttr<ProvidersLabel>,
    ) -> anyhow::Result<ConfiguredAttr> {
        let label = &dep_attr.label;
        let configured_label = match &dep_attr.attr_type.transition {
            DepAttrTransition::Identity => ctx.configure_target(label),
            DepAttrTransition::Exec => ctx.configure_exec_target(label),
            DepAttrTransition::Toolchain => ctx.configure_toolchain_target(label),
            DepAttrTransition::Transition(tr) => ctx.configure_transition_target(label, tr)?,
        };
        Ok(ConfiguredAttr::Extra(ConfiguredAttrExtraTypes::Dep(
            Box::new(DepAttr {
                attr_type: dep_attr.attr_type.dupe(),
                label: configured_label,
            }),
        )))
    }
}

/// Represents both configured and unconfigured forms.
pub trait ExplicitConfiguredDepMaybeConfigured: Display + Allocative {
    fn to_json(&self) -> anyhow::Result<serde_json::Value>;
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}

impl ConfiguredExplicitConfiguredDep {
    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> anyhow::Result<()> {
        traversal.dep(&self.label)
    }
}
