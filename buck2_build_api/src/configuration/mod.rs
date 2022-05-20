/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::BTreeMap,
    hash::{Hash, Hasher},
    sync::Arc,
};

use async_trait::async_trait;
use buck2_core::{
    cells::CellName,
    configuration::{Configuration, ConfigurationData},
    result::SharedResult,
    target::TargetLabel,
};
use gazebo::prelude::*;
use indexmap::{Equivalent, IndexMap, IndexSet};
use starlark::collections::SmallMap;
use thiserror::Error;

use crate::{
    attrs::AttrConfigurationContext,
    configuration::execution::{ExecutionPlatform, ExecutionPlatformResolution},
    interpreter::rule_defs::transition::{applied::TransitionApplied, id::TransitionId},
};

pub mod calculation;
pub mod execution;
pub mod target_platform_detector;

pub type ExecutionPlatforms = Arc<Vec<Arc<ExecutionPlatform>>>;

#[async_trait]
pub trait ConfigurationCalculation {
    async fn get_default_platform(&self, target: &TargetLabel) -> SharedResult<Configuration>;

    async fn get_platform_configuration(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<Configuration>;

    async fn get_resolved_configuration<'a, T: Iterator<Item = &'a TargetLabel> + Send>(
        &self,
        target_cfg: &Configuration,
        target_node_cell: &CellName,
        configuration_deps: T,
    ) -> SharedResult<ResolvedConfiguration>;

    async fn get_configuration_node(
        &self,
        target_cfg: &Configuration,
        target_cell: &CellName,
        cfg_target: &TargetLabel,
    ) -> SharedResult<ConfigurationNode>;

    /// Returns a list of the configured execution platforms. This looks up the providers on the target
    /// configured **in the root cell's buckconfig** with key `build.execution_platforms`. If there's no
    /// value configured, it will return `None` which indicates we should fallback to the legacy execution
    /// platform behavior.
    async fn get_execution_platforms(&self) -> SharedResult<Option<ExecutionPlatforms>>;

    /// Gets the compatible execution platforms for a give list of compatible_with constraints and execution deps.
    ///
    /// We do this as a sort of monolithic computation (rather than checking things one-by-one or separating
    /// compatible_with and exec deps) because we expect those values to be common across many nodes (for example,
    /// all c++ targets targeting a specific platform are likely to share compatible_with and exec_deps except
    /// for some rare exceptions). By having a monolithic key like `(Vec<TargetLabel>, Vec<TargetLabel>)` allows all
    /// those nodes to just have a single dice dep. This approach has the downside that it is less incremental, but
    /// we expect these things to change rarely.
    async fn resolve_execution_platform(
        &self,
        target_node_cell: &CellName,
        compatible_with: Vec<TargetLabel>,
        exec_deps: IndexSet<TargetLabel>,
    ) -> SharedResult<ExecutionPlatformResolution>;
}

#[derive(Debug, Eq)]
pub struct ConfigurationSettingKey(pub(crate) TargetLabel);

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ConfigurationSettingKeyRef<'a>(pub(crate) &'a TargetLabel);

impl Equivalent<ConfigurationSettingKey> for ConfigurationSettingKeyRef<'_> {
    fn equivalent(&self, key: &ConfigurationSettingKey) -> bool {
        self == &key.as_ref()
    }
}

impl ConfigurationSettingKey {
    fn as_ref(&self) -> ConfigurationSettingKeyRef {
        ConfigurationSettingKeyRef(&self.0)
    }
}

impl PartialEq for ConfigurationSettingKey {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Hash for ConfigurationSettingKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

/// For a target, some of the configuration comes down the graph from its dependents, other
/// parts of the configuration (those not specified by the target platform or transitions)
/// comes from the global configuration state.
///
/// The ResolvedConfiguration represents the full configuration state that is available to
/// a target for analysis.
#[derive(Clone, Dupe, Debug, Eq, PartialEq)]
pub struct ResolvedConfiguration(Arc<ResolvedConfigurationData>);

#[derive(Debug, Eq, PartialEq)]
struct ResolvedConfigurationData {
    cfg: Configuration,
    settings: IndexMap<ConfigurationSettingKey, ConfigurationNode>,
}

#[allow(clippy::derive_hash_xor_eq)] // IndexMap doesn't implement Hash
impl Hash for ResolvedConfiguration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.cfg.hash(state);
        for (k, v) in &self.0.settings {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl ResolvedConfiguration {
    pub fn new(
        cfg: Configuration,
        settings: IndexMap<ConfigurationSettingKey, ConfigurationNode>,
    ) -> Self {
        Self(Arc::new(ResolvedConfigurationData { cfg, settings }))
    }

    pub fn cfg(&self) -> &Configuration {
        &self.0.cfg
    }

    pub fn setting_matches(&self, key: ConfigurationSettingKeyRef) -> Option<&ConfigurationData> {
        let configuration_node = self.0.settings.get(&key).expect(
            "framework should've ensured all necessary configuration setting keys are present",
        );
        if configuration_node.matches() {
            Some(configuration_node.configuration_data())
        } else {
            None
        }
    }

    pub fn matches(&self, label: &TargetLabel) -> Option<&ConfigurationData> {
        self.setting_matches(ConfigurationSettingKeyRef(label))
    }
}

#[derive(Debug, Error)]
pub enum PlatformConfigurationError {
    #[error("Could not find configuration for platform target `{0}`")]
    UnknownPlatformTarget(TargetLabel),
}

pub(crate) struct AttrConfigurationContextImpl<'b> {
    pub(crate) resolved_cfg: &'b ResolvedConfiguration,
    pub(crate) exec_cfg: &'b Configuration,
    pub(crate) resolved_transitions: &'b SmallMap<Arc<TransitionId>, Arc<TransitionApplied>>,
    pub(crate) platform_cfgs: &'b BTreeMap<TargetLabel, Configuration>,
}

impl<'b> AttrConfigurationContext for AttrConfigurationContextImpl<'b> {
    fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigurationData> {
        self.resolved_cfg
            .setting_matches(ConfigurationSettingKeyRef(label))
    }

    fn cfg(&self) -> &Configuration {
        self.resolved_cfg.cfg()
    }

    fn exec_cfg(&self) -> &Configuration {
        self.exec_cfg
    }

    fn platform_cfg(&self, label: &TargetLabel) -> anyhow::Result<&Configuration> {
        match self.platform_cfgs.get(label) {
            Some(configuration) => Ok(configuration),
            None => Err(anyhow::anyhow!(
                PlatformConfigurationError::UnknownPlatformTarget(label.dupe())
            )),
        }
    }

    fn resolved_transitions(&self) -> &SmallMap<Arc<TransitionId>, Arc<TransitionApplied>> {
        self.resolved_transitions
    }
}

/// A ConfigurationNode contains the information about a config_setting() or similar target in a certain configuration.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash)]
pub struct ConfigurationNode(Arc<ConfigurationNodeData>);

#[derive(Debug, Eq, PartialEq, Hash)]
struct ConfigurationNodeData {
    // This is stored as a split Configuration/TargetLabel (rather than a ConfiguredTargetLabel) because it's not
    // quite the same as what you would think of a ConfiguredTargetLabel. Importantly, we don't do analysis of the
    // target with this configuration, instead we interpret the results of the analysis of the target in the "unbound"
    // configuration within the context of this configuration.
    cfg: Configuration,

    label: TargetLabel,

    configuration_data: ConfigurationData,

    /// Indicates whether this node "matches" the configuration.
    ///
    /// For example, a configuration node that requires a list of constraints "matches" a configuration where all of those constraints are satisfied.
    matches: bool,
}

impl ConfigurationNode {
    pub fn new(
        cfg: Configuration,
        label: TargetLabel,
        configuration_data: ConfigurationData,
        matches: bool,
    ) -> Self {
        Self(Arc::new(ConfigurationNodeData {
            cfg,
            label,
            configuration_data,
            matches,
        }))
    }

    pub fn matches(&self) -> bool {
        self.0.matches
    }

    pub fn label(&self) -> &TargetLabel {
        &self.0.label
    }

    pub fn configuration_data(&self) -> &ConfigurationData {
        &self.0.configuration_data
    }
}
