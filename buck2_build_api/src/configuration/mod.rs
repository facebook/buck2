/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::cells::CellName;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::result::SharedResult;
use buck2_core::target::TargetLabel;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::configuration::resolved::ConfigurationNode;
use buck2_node::configuration::resolved::ConfigurationSettingKeyRef;
use buck2_node::configuration::resolved::ResolvedConfiguration;
use gazebo::prelude::*;
use indexmap::IndexMap;
use indexmap::IndexSet;
use thiserror::Error;

use crate::configuration::execution::ExecutionPlatform;
use crate::configuration::execution::ExecutionPlatformResolution;

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
        exec_compatible_with: Vec<TargetLabel>,
        exec_deps: IndexSet<TargetLabel>,
    ) -> SharedResult<ExecutionPlatformResolution>;
}

#[derive(Debug, Error)]
pub enum PlatformConfigurationError {
    #[error("Could not find configuration for platform target `{0}`")]
    UnknownPlatformTarget(TargetLabel),
}

pub(crate) struct AttrConfigurationContextImpl<'b> {
    pub(crate) resolved_cfg: &'b ResolvedConfiguration,
    pub(crate) exec_cfg: &'b Configuration,
    pub(crate) resolved_transitions: &'b IndexMap<Arc<TransitionId>, Arc<TransitionApplied>>,
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

    fn resolved_transitions(&self) -> &IndexMap<Arc<TransitionId>, Arc<TransitionApplied>> {
        self.resolved_transitions
    }
}
