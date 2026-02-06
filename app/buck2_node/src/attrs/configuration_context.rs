/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::configuration::pair::ConfigurationWithExec;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::internal_error;
use dupe::Dupe;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::sorted_map::SortedMap;

use crate::configuration::resolved::MatchedConfigurationSettingKeys;
use crate::configuration::resolved::MatchedConfigurationSettingKeysWithCfg;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
pub enum PlatformConfigurationError {
    #[error("Could not find configuration for platform target `{0}`")]
    UnknownPlatformTarget(TargetLabel),
}

/// The context for attribute configuration. Contains information about the
/// configuration.
pub trait AttrConfigurationContext {
    fn matched_cfg_keys(&self) -> &MatchedConfigurationSettingKeys;

    fn cfg(&self) -> ConfigurationNoExec;

    fn exec_cfg(&self) -> buck2_error::Result<ConfigurationNoExec>;

    /// Must be equal to `(cfg, Some(exec_cfg))`.
    fn toolchain_cfg(&self) -> ConfigurationWithExec;

    fn platform_cfg(&self, label: &TargetLabel) -> buck2_error::Result<ConfigurationData>;

    /// Map of transition ids resolved to configurations
    /// using current node configuration as input.
    fn resolved_transitions(
        &self,
    ) -> buck2_error::Result<&OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>>;

    fn configure_target(&self, label: &ProvidersLabel) -> ConfiguredProvidersLabel {
        label.configure_pair(self.cfg().cfg_pair().dupe())
    }

    fn configure_exec_target(
        &self,
        label: &ProvidersLabel,
    ) -> buck2_error::Result<ConfiguredProvidersLabel> {
        Ok(label.configure_pair(self.exec_cfg()?.cfg_pair().dupe()))
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
    ) -> buck2_error::Result<ConfiguredProvidersLabel> {
        let cfg = self
            .resolved_transitions()?
            .get(tr)
            .ok_or_else(|| internal_error!("internal error: no resolved transition"))?;
        Ok(label.configure(cfg.single()?.dupe()))
    }

    fn configure_split_transition_target(
        &self,
        label: &ProvidersLabel,
        tr: &TransitionId,
    ) -> buck2_error::Result<SortedMap<String, ConfiguredProvidersLabel>> {
        let cfg = self
            .resolved_transitions()?
            .get(tr)
            .ok_or_else(|| internal_error!("internal error: no resolved transition"))?;
        let split = cfg.split()?;
        Ok(split
            .iter()
            .map(|(k, v)| (k.to_owned(), label.configure(v.dupe())))
            .collect())
    }
}

pub struct AttrConfigurationContextImpl<'b> {
    resolved_cfg: &'b MatchedConfigurationSettingKeysWithCfg,
    exec_cfg: ConfigurationNoExec,
    /// Must be equal to `(cfg, Some(exec_cfg))`.
    toolchain_cfg: ConfigurationWithExec,
    resolved_transitions: &'b OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
    platform_cfgs: &'b OrderedMap<TargetLabel, ConfigurationData>,
}

impl<'b> AttrConfigurationContextImpl<'b> {
    pub fn new(
        resolved_cfg: &'b MatchedConfigurationSettingKeysWithCfg,
        exec_cfg: ConfigurationNoExec,
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

impl AttrConfigurationContext for AttrConfigurationContextImpl<'_> {
    fn matched_cfg_keys(&self) -> &MatchedConfigurationSettingKeys {
        self.resolved_cfg.settings()
    }

    fn cfg(&self) -> ConfigurationNoExec {
        self.resolved_cfg.cfg().dupe()
    }

    fn exec_cfg(&self) -> buck2_error::Result<ConfigurationNoExec> {
        Ok(self.exec_cfg.dupe())
    }

    fn toolchain_cfg(&self) -> ConfigurationWithExec {
        self.toolchain_cfg.dupe()
    }

    fn platform_cfg(&self, label: &TargetLabel) -> buck2_error::Result<ConfigurationData> {
        match self.platform_cfgs.get(label) {
            Some(configuration) => Ok(configuration.dupe()),
            None => Err(PlatformConfigurationError::UnknownPlatformTarget(label.dupe()).into()),
        }
    }

    fn resolved_transitions(
        &self,
    ) -> buck2_error::Result<&OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>> {
        Ok(self.resolved_transitions)
    }
}
