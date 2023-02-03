/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::collections::sorted_map::SortedMap;
use buck2_core::configuration::pair::ConfigurationPairNoExec;
use buck2_core::configuration::pair::ConfigurationPairWithExec;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::ConfigurationData;
use buck2_core::configuration::ConfigurationDataData;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use dupe::Dupe;

use crate::configuration::resolved::ConfigurationSettingKeyRef;
use crate::configuration::resolved::ResolvedConfiguration;

#[derive(Debug, thiserror::Error)]
pub enum PlatformConfigurationError {
    #[error("Could not find configuration for platform target `{0}`")]
    UnknownPlatformTarget(TargetLabel),
}

/// The context for attribute configuration. Contains information about the
/// configuration.
pub trait AttrConfigurationContext {
    /// Return the content of the resolved `config_setting` on match.
    fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigurationDataData>;

    fn cfg(&self) -> ConfigurationPairNoExec;

    fn exec_cfg(&self) -> ConfigurationPairNoExec;

    /// Must be equal to `(cfg, Some(exec_cfg))`.
    fn toolchain_cfg(&self) -> ConfigurationPairWithExec;

    fn platform_cfg(&self, label: &TargetLabel) -> anyhow::Result<ConfigurationData>;

    /// Map of transition ids resolved to configurations
    /// using current node configuration as input.
    fn resolved_transitions(&self) -> &OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>;

    fn configure_target(&self, label: &ProvidersLabel) -> ConfiguredProvidersLabel {
        label.configure_pair(self.cfg().cfg_pair().dupe())
    }

    fn configure_exec_target(&self, label: &ProvidersLabel) -> ConfiguredProvidersLabel {
        label.configure_pair(self.exec_cfg().cfg_pair().dupe())
    }

    fn configure_toolchain_target(&self, label: &ProvidersLabel) -> ConfiguredProvidersLabel {
        // The toolchain dependency itself is always configured in the target configuration,
        // but its exec_deps are considered when picking an execution platform, and MUST
        // use the execution dependency of its parent.
        label.configure_pair(self.toolchain_cfg().cfg_pair().dupe())
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
            .context("internal error: no resolved transition")?;
        Ok(label.configure(cfg.single()?.dupe()))
    }

    fn configure_split_transition_target(
        &self,
        label: &ProvidersLabel,
        tr: &TransitionId,
    ) -> anyhow::Result<SortedMap<String, ConfiguredProvidersLabel>> {
        let cfg = self
            .resolved_transitions()
            .get(tr)
            .context("internal error: no resolved transition")?;
        let split = cfg.split()?;
        Ok(split
            .iter()
            .map(|(k, v)| (k.to_owned(), label.configure(v.dupe())))
            .collect())
    }
}

pub struct AttrConfigurationContextImpl<'b> {
    resolved_cfg: &'b ResolvedConfiguration,
    exec_cfg: ConfigurationPairNoExec,
    /// Must be equal to `(cfg, Some(exec_cfg))`.
    toolchain_cfg: ConfigurationPairWithExec,
    resolved_transitions: &'b OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
    platform_cfgs: &'b OrderedMap<TargetLabel, ConfigurationData>,
}

impl<'b> AttrConfigurationContextImpl<'b> {
    pub fn new(
        resolved_cfg: &'b ResolvedConfiguration,
        exec_cfg: ConfigurationPairNoExec,
        resolved_transitions: &'b OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
        platform_cfgs: &'b OrderedMap<TargetLabel, ConfigurationData>,
    ) -> AttrConfigurationContextImpl<'b> {
        AttrConfigurationContextImpl {
            resolved_cfg,
            toolchain_cfg: resolved_cfg.cfg().make_toolchain(&exec_cfg),
            exec_cfg,
            resolved_transitions,
            platform_cfgs,
        }
    }
}

impl<'b> AttrConfigurationContext for AttrConfigurationContextImpl<'b> {
    fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigurationDataData> {
        self.resolved_cfg
            .setting_matches(ConfigurationSettingKeyRef(label))
    }

    fn cfg(&self) -> ConfigurationPairNoExec {
        self.resolved_cfg.cfg().dupe()
    }

    fn exec_cfg(&self) -> ConfigurationPairNoExec {
        self.exec_cfg.dupe()
    }

    fn toolchain_cfg(&self) -> ConfigurationPairWithExec {
        self.toolchain_cfg.dupe()
    }

    fn platform_cfg(&self, label: &TargetLabel) -> anyhow::Result<ConfigurationData> {
        match self.platform_cfgs.get(label) {
            Some(configuration) => Ok(configuration.dupe()),
            None => Err(anyhow::anyhow!(
                PlatformConfigurationError::UnknownPlatformTarget(label.dupe())
            )),
        }
    }

    fn resolved_transitions(&self) -> &OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>> {
        self.resolved_transitions
    }
}
