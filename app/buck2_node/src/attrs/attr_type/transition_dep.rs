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
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use dupe::Dupe;
use pagable::Pagable;

use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::provider_id_set::ProviderIdSet;

#[derive(Debug, Pagable, PartialEq, Eq, Hash, Allocative)]
pub struct TransitionDepAttrType {
    pub required_providers: ProviderIdSet,
    pub transition: Option<Arc<TransitionId>>,
}

impl TransitionDepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: Option<Arc<TransitionId>>) -> Self {
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
                dep: ctx.configure_transition_target(&attr.dep, self.get_transition(attr))?,
                required_providers: self.required_providers.dupe(),
            },
        )))
    }

    pub(crate) fn get_transition<'a>(
        &'a self,
        attr: &'a CoercedTransitionDep,
    ) -> &'a Arc<TransitionId> {
        match self.transition.as_ref() {
            Some(t) => t,
            None => attr.transition.as_ref().unwrap(),
        }
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
#[display("{}", dep)]
pub struct CoercedTransitionDep {
    pub dep: ProvidersLabel,
    /// `Some` iff the transition in the attr type is `None`
    ///
    /// Stored as a `TransitionId`, but always a `TransitionId::Target` if set
    pub transition: Option<Arc<TransitionId>>,
}

impl CoercedTransitionDep {
    pub(crate) fn to_json(&self) -> buck2_error::Result<serde_json::Value> {
        match self.get_dynamic_transition() {
            Some(tr) => Ok(serde_json::to_value([
                self.dep.to_string(),
                tr.to_string(),
            ])?),
            None => Ok(serde_json::to_value(self.dep.to_string())?),
        }
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
        let transition = t.get_transition(self);
        match &**transition {
            TransitionId::MagicObject { .. } => (),
            TransitionId::Target(label) => {
                traversal.configuration_dep(label, ConfigurationDepKind::Transition)?
            }
        };
        traversal.transition_dep(&self.dep, &transition)
    }

    /// If there's a dynamic transition, return the target
    pub fn get_dynamic_transition(&self) -> Option<&ProvidersLabel> {
        match &**self.transition.as_ref()? {
            TransitionId::Target(t) => Some(t),
            TransitionId::MagicObject { .. } => unreachable!(),
        }
    }
}
