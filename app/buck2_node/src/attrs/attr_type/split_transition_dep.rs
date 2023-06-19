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
use buck2_util::collections::sorted_map::SortedMap;
use dupe::Dupe;

use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::provider_id_set::ProviderIdSet;

#[derive(Debug, PartialEq, Eq, Hash, Allocative)]
pub struct SplitTransitionDepAttrType {
    pub required_providers: ProviderIdSet,
    pub transition: Arc<TransitionId>,
}

impl SplitTransitionDepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: Arc<TransitionId>) -> Self {
        SplitTransitionDepAttrType {
            required_providers,
            transition,
        }
    }

    pub(crate) fn configure(
        &self,
        dep: &ProvidersLabel,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredAttr> {
        let configured_providers = ctx.configure_split_transition_target(dep, &self.transition)?;
        Ok(ConfiguredAttr::SplitTransitionDep(Box::new(
            ConfiguredSplitTransitionDep {
                deps: configured_providers,
                required_providers: self.required_providers.dupe(),
            },
        )))
    }
}

/// Configured or unconfigured.
pub trait SplitTransitionDepMaybeConfigured: Display + Allocative {
    fn to_json(&self) -> anyhow::Result<serde_json::Value>;
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool>;
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Allocative)]
pub struct ConfiguredSplitTransitionDep {
    pub deps: SortedMap<String, ConfiguredProvidersLabel>,
    pub required_providers: ProviderIdSet,
}

impl Display for ConfiguredSplitTransitionDep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, dep) in self.deps.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}: {}", dep.0, dep.1)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl SplitTransitionDepMaybeConfigured for ConfiguredSplitTransitionDep {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        let mut map = serde_json::Map::with_capacity(self.deps.len());
        for (label, target) in &self.deps {
            map.insert(label.clone(), serde_json::to_value(target.to_string())?);
        }
        Ok(serde_json::Value::Object(map))
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        for (label, target) in &self.deps {
            if filter(label)? || filter(&target.to_string())? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

#[derive(derive_more::Display, Debug, Hash, PartialEq, Eq, Clone, Allocative)]
#[display(fmt = "{}", label)]
pub struct SplitTransitionDep {
    pub label: ProvidersLabel,
    pub transition: Arc<TransitionId>,
    pub required_providers: ProviderIdSet,
}

impl SplitTransitionDepMaybeConfigured for SplitTransitionDep {
    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        Ok(serde_json::to_value(self.to_string())?)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        filter(&self.to_string())
    }
}
