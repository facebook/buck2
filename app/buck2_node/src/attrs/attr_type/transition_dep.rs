/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use dupe::Dupe;

use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

#[derive(Debug, PartialEq, Eq, Hash, Allocative)]
pub struct TransitionDepAttrType {
    pub required_providers: ProviderIdSet,
    pub transition: Arc<TransitionId>,
}

impl TransitionDepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: Arc<TransitionId>) -> Self {
        TransitionDepAttrType {
            required_providers,
            transition,
        }
    }

    pub(crate) fn configure(
        &self,
        attr: &CoercedTransitionDep,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredAttr> {
        Ok(ConfiguredAttr::TransitionDep(Box::new(
            ConfiguredTransitionDep {
                dep: ctx.configure_transition_target(&attr.dep, &self.transition)?,
                required_providers: self.required_providers.dupe(),
            },
        )))
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Allocative)]
pub struct ConfiguredTransitionDep {
    pub dep: ConfiguredProvidersLabel,
    pub required_providers: ProviderIdSet,
}

impl Display for ConfiguredTransitionDep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.dep, f)
    }
}

impl ConfiguredTransitionDep {
    pub(crate) fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::to_value(self.dep.to_string())?)
    }

    pub(crate) fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        filter(&self.dep.to_string())
    }

    pub(crate) fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
    ) -> buck2_error::Result<()> {
        traversal.dep(&self.dep)
    }
}

#[derive(derive_more::Display, Debug, Hash, PartialEq, Eq, Clone, Allocative)]
#[display("{}", dep)]
pub struct CoercedTransitionDep {
    pub dep: ProvidersLabel,
}

impl CoercedTransitionDep {
    pub(crate) fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        Ok(serde_json::to_value(self.dep.to_string())?)
    }

    pub(crate) fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        filter(&self.dep.to_string())
    }

    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
        t: &TransitionDepAttrType,
    ) -> buck2_error::Result<()> {
        match &*t.transition {
            TransitionId::MagicObject { .. } => (),
            TransitionId::Target(label) => {
                traversal.configuration_dep(label, ConfigurationDepKind::Transition)?
            }
        };
        traversal.transition_dep(&self.dep, &t.transition)
    }
}
