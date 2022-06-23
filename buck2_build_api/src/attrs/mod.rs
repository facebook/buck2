/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module provides support for implementing and consuming attr
//! types.
//!
//! Support for implementing new attribute types is in the `val` module.
//!
//! Attribute values are present in 4 different states:
//!   1. initial starlark value - the value passed to the rule function
//!   2. coerced value - the value captured after processing the build file. At this point, the type has
//!      been checked (i.e. for an attr.list(attr.string()), we've confirmed that (1) was an iterable of strings).
//!      This is done when invoking a rule function, so it has no access to configuration or information from
//!      other build files.
//!   3. configured value - this is roughly (2) with a specific configuration attached to all configurable
//!      values. For example a dep or a source (where the source is an output) are configurable (they both are basically
//!      targets).
//!   4. resolved value - this is the "resolved" attribute once again as a starlark value and as provided to a rule
//!      implementation function.
//!
//! Attribute coercion happens immediately on declaring a build target (via
//! invoking a rule function). It will validate the types of the
//! attribute values and do some simple conversions (ex. parse strings to
//! target labels).
//!
//! Attribute configuration happens when the configuration for a target
//! becomes available. There are two primary operations that happen
//! here: select resolution and target configuration.
//!
//! Attribute resolution happens just before invoking the rule
//! implementation. At this point, the context has access to all the
//! providers that are needed (based on inspection of the configured value)
//! and anything that requires a provider can be resolved to its final
//! value.
//!
//! Generally an attribute type is specified as its "resolved" type that the
//! rule implementation requests. For example, a rule that wants a list of files
//! for its sources would specify `attr.list(attr.file())` and would
//! receive a list of files in the implementation. The intermediate form of that
//! may be strings or targets or some other thing (e.g. a lazy glob, maybe).

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::anyhow;
use attr_type::attr_literal::AttrConfig;
use attr_type::attr_literal::AttrLiteral;
use attr_type::bool;
use attr_type::dep::ConfiguredExplicitConfiguredDep;
use attr_type::dep::UnconfiguredExplicitConfiguredDep;
use attr_type::split_transition_dep::ConfiguredSplitTransitionDep;
use attr_type::split_transition_dep::SplitTransitionDep;
use buck2_core::buck_path::BuckPath;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::result::ToSharedResultExt;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use either::Either;
use gazebo::prelude::*;
use starlark::collections::small_map;
use starlark::collections::SmallMap;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configured_attr::ConfiguredAttr;

pub(crate) mod analysis;
pub mod attr_type;
pub mod coerced_attr;
pub mod configurable;
pub mod configured_attr;

#[cfg(test)]
pub(crate) mod testing;
#[cfg(test)]
mod tests;

/// A simple map that maintains insertion order.
pub type OrderedMap<K, V> = SmallMap<K, V>;
pub type OrderedMapEntry<'a, K, V> = small_map::Entry<'a, K, V>;
pub type OrderedMapOccupiedEntry<'a, K, V> = small_map::OccupiedEntry<'a, K, V>;
pub type OrderedMapVacantEntry<'a, K, V> = small_map::VacantEntry<'a, K, V>;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum CoercedPath {
    File(BuckPath),
    Directory(BuckPath, Vec<BuckPath>),
}

impl CoercedPath {
    pub fn path(&self) -> &BuckPath {
        match self {
            CoercedPath::File(x) => x,
            CoercedPath::Directory(x, _) => x,
        }
    }

    pub fn inputs(&self) -> impl Iterator<Item = &BuckPath> {
        match self {
            CoercedPath::File(x) => Either::Left(std::iter::once(x)),
            CoercedPath::Directory(_, xs) => Either::Right(xs.iter()),
        }
    }
}

/// The context for attribute coercion. Mostly just contains information about
/// the current package (to support things like parsing targets from strings).
pub trait AttrCoercionContext {
    fn coerce_target(&self, value: &str) -> anyhow::Result<TargetLabel> {
        let label = self.coerce_label(value)?;
        if let ProvidersName::Named(_) = label.name() {
            return Err(anyhow!(CoercionError::unexpected_providers_name(value)));
        }
        Ok(label.into_parts().0)
    }

    /// Attempt to convert a string into a label
    fn coerce_label(&self, value: &str) -> anyhow::Result<ProvidersLabel>;

    /// Attempt to convert a string into a BuckPath
    fn coerce_path(&self, value: &str, allow_directory: bool) -> anyhow::Result<CoercedPath>;
}

/// The context for attribute configuration. Contains information about the
/// configuration.
pub(crate) trait AttrConfigurationContext {
    /// Return the content of the resolved `config_setting` on match.
    fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigurationData>;

    fn cfg(&self) -> &Configuration;

    fn exec_cfg(&self) -> &Configuration;

    fn platform_cfg(&self, label: &TargetLabel) -> anyhow::Result<&Configuration>;

    /// Map of transition ids resolved to configurations
    /// using current node configuration as input.
    fn resolved_transitions(&self) -> &SmallMap<Arc<TransitionId>, Arc<TransitionApplied>>;

    fn configure_target(&self, label: &ProvidersLabel) -> ConfiguredProvidersLabel {
        label.configure(self.cfg().dupe())
    }

    fn configure_exec_target(&self, label: &ProvidersLabel) -> ConfiguredProvidersLabel {
        label.configure(self.exec_cfg().dupe())
    }

    /// Configure a transition target.
    fn configure_transition_target(
        &self,
        label: &ProvidersLabel,
        tr: &TransitionId,
    ) -> anyhow::Result<ConfiguredProvidersLabel> {
        let cfg = self
            .resolved_transitions()
            .get(tr)
            .expect("internal error: no resolved transition");
        Ok(label.configure(cfg.single().shared_error()?.dupe()))
    }

    fn configure_split_transition_target(
        &self,
        label: &ProvidersLabel,
        tr: &TransitionId,
    ) -> anyhow::Result<BTreeMap<String, ConfiguredProvidersLabel>> {
        let cfg = self
            .resolved_transitions()
            .get(tr)
            .expect("internal error: no resolved transition");
        let split = cfg.split()?;
        Ok(split
            .iter()
            .map(|(k, v)| (k.to_owned(), label.configure(v.dupe())))
            .collect())
    }
}

pub(crate) trait CoercedAttrTraversal<'a> {
    fn dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn exec_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &'a Arc<TransitionId>,
    ) -> anyhow::Result<()>;
    fn split_transition_dep(
        &mut self,
        dep: &'a TargetLabel,
        tr: &'a Arc<TransitionId>,
    ) -> anyhow::Result<()>;
    fn configuration_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn platform_dep(&mut self, dep: &'a TargetLabel) -> anyhow::Result<()>;
    fn input(&mut self, input: &'a BuckPath) -> anyhow::Result<()>;
    fn label(&mut self, _label: &'a ProvidersLabel) -> anyhow::Result<()> {
        Ok(())
    }
}

impl AttrConfig for CoercedAttr {
    type TargetType = TargetLabel;
    type ProvidersType = ProvidersLabel;
    type SplitTransitionDepType = SplitTransitionDep;
    type ExplicitConfiguredDepType = UnconfiguredExplicitConfiguredDep;

    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        CoercedAttr::to_json(self)
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        CoercedAttr::any_matches(self, filter)
    }
}

impl AttrConfig for ConfiguredAttr {
    type TargetType = ConfiguredTargetLabel;
    type ProvidersType = ConfiguredProvidersLabel;
    type SplitTransitionDepType = ConfiguredSplitTransitionDep;
    type ExplicitConfiguredDepType = ConfiguredExplicitConfiguredDep;

    fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        self.0.to_json()
    }

    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        self.0.any_matches(filter)
    }
}
