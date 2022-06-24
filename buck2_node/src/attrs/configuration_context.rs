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

use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::TargetLabel;
use gazebo::dupe::Dupe;
use indexmap::IndexMap;

/// The context for attribute configuration. Contains information about the
/// configuration.
pub trait AttrConfigurationContext {
    /// Return the content of the resolved `config_setting` on match.
    fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigurationData>;

    fn cfg(&self) -> &Configuration;

    fn exec_cfg(&self) -> &Configuration;

    fn platform_cfg(&self, label: &TargetLabel) -> anyhow::Result<&Configuration>;

    /// Map of transition ids resolved to configurations
    /// using current node configuration as input.
    fn resolved_transitions(&self) -> &IndexMap<Arc<TransitionId>, Arc<TransitionApplied>>;

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
        Ok(label.configure(cfg.single()?.dupe()))
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
