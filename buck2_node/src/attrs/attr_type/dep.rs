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

use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::id::ProviderId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use gazebo::dupe::Dupe;

use crate::attrs::attr_type::attr_like::AttrLike;
use crate::attrs::traversal::CoercedAttrTraversal;

// Just a placeholder for what a label should resolve to.
#[derive(Debug)]
pub struct DefaultProvider {}

pub type ProviderIdSet = Vec<Arc<ProviderId>>;

/// How configuration is changed when configuring a dep.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub enum DepAttrTransition {
    // No transition.
    Identity,
    // Transition to execution platform.
    Exec,
    // Transition dependency using given transition function.
    Transition(Arc<TransitionId>),
}

/// A dep attribute accepts a target label and will resolve to the provider collection from that label's analysis.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub struct DepAttrType {
    /// The set of providers that are required to be available, during attr resolution we'll verify that these
    /// are present on each attribute value.
    ///
    /// Both None and Some(Arc([])) represent that no specific providers are required.
    pub required_providers: Option<Arc<ProviderIdSet>>,
    pub transition: DepAttrTransition,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DepAttr<T: ProvidersLabelMaybeConfigured + AttrLike> {
    pub attr_type: DepAttrType,
    pub label: T,
}

impl<T: ProvidersLabelMaybeConfigured + AttrLike> DepAttr<T> {
    pub fn new(attr_type: DepAttrType, label: T) -> Self {
        Self { attr_type, label }
    }

    pub fn label(&self) -> &T {
        &self.label
    }

    pub fn into_label(self) -> T {
        self.label
    }
}

impl<T: ProvidersLabelMaybeConfigured + AttrLike> Display for DepAttr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.label, f)
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
            DepAttrTransition::Transition(tr) => traversal.transition_dep(self.label.target(), tr),
        }
    }
}

impl DepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: DepAttrTransition) -> Self {
        let required_providers = if required_providers.is_empty() {
            None
        } else {
            Some(Arc::new(required_providers))
        };
        Self {
            required_providers,
            transition,
        }
    }
}

/// Represents both configured and unconfigured forms.
pub trait ExplicitConfiguredDepMaybeConfigured {
    fn to_json(&self) -> anyhow::Result<serde_json::Value>;
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}
