/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Display;

use allocative::Allocative;
use buck2_core::plugins::PluginKindSet;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use dupe::Dupe;
use pagable::Pagable;
use static_assertions::assert_eq_size;
use strong_hash::StrongHash;

use crate::attrs::attr_type::attr_like::AttrLike;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

/// How configuration is changed when configuring a dep.
#[derive(
    Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable, StrongHash
)]
pub enum DepAttrTransition {
    /// No transition.
    ///
    /// May participate in plugin propagation
    Identity(PluginKindSet),
    /// Transition to execution platform.
    Exec,
    /// Transition to toolchain.
    Toolchain,
}

/// A dep attribute accepts a target label and will resolve to the provider collection from that label's analysis.
#[derive(
    Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable, StrongHash
)]
pub struct DepAttrType {
    /// The set of providers that are required to be available, during attr resolution we'll verify that these
    /// are present on each attribute value.
    pub required_providers: ProviderIdSet,
    pub transition: DepAttrTransition,
}

assert_eq_size!(DepAttrType, [usize; 3]);

#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative, Pagable, StrongHash)]
pub struct DepAttr<T: ProvidersLabelMaybeConfigured + AttrLike> {
    // FIXME(JakobDegen): Storing this on every dep - and then having to box this value as a result
    // - is a pretty sad waste of memory
    pub attr_type: DepAttrType,
    pub label: T,
}

impl<T: ProvidersLabelMaybeConfigured + AttrLike> Display for DepAttr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.label, f)
    }
}

impl DepAttr<ConfiguredProvidersLabel> {
    pub(crate) fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        match &self.attr_type.transition {
            DepAttrTransition::Identity(plugins) if plugins.is_empty() => {
                traversal.dep(&self.label)
            }
            DepAttrTransition::Identity(plugins) => {
                traversal.dep_with_plugins(&self.label, plugins)
            }
            DepAttrTransition::Exec => traversal.exec_dep(&self.label),
            DepAttrTransition::Toolchain => traversal.toolchain_dep(&self.label),
        }
    }
}

impl DepAttr<ProvidersLabel> {
    pub fn traverse<'a>(
        label: &'a ProvidersLabel,
        attr_type: &DepAttrType,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        match &attr_type.transition {
            DepAttrTransition::Identity(..) => traversal.dep(label),
            DepAttrTransition::Exec => traversal.exec_dep(label),
            DepAttrTransition::Toolchain => traversal.toolchain_dep(label),
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
        &self,
        label: &ProvidersLabel,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredAttr> {
        let configured_label = match &self.transition {
            DepAttrTransition::Identity(..) => ctx.configure_target(label),
            DepAttrTransition::Exec => ctx.configure_exec_target(label)?,
            DepAttrTransition::Toolchain => ctx.configure_toolchain_target(label),
        };
        Ok(ConfiguredAttr::Dep(Box::new(DepAttr {
            attr_type: self.dupe(),
            label: configured_label,
        })))
    }
}
