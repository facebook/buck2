/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use dupe::Dupe;
use starlark_map::unordered_map::UnorderedMap;

/// Key in `select` or an item in `target_compatible_with`.
/// Should point to `config_setting` target, or `constraint_value`.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Hash,
    Allocative,
    derive_more::Display,
    Clone,
    Dupe,
    Ord,
    PartialOrd
)]
pub struct ConfigurationSettingKey(pub ProvidersLabel);

impl ConfigurationSettingKey {
    pub fn testing_parse(label: &str) -> ConfigurationSettingKey {
        ConfigurationSettingKey(ProvidersLabel::default_for(TargetLabel::testing_parse(
            label,
        )))
    }
}

/// For a target, some of the configuration comes down the graph from its dependents, other
/// parts of the configuration (those not specified by the target platform or transitions)
/// comes from the global configuration state.
///
/// The ResolvedConfiguration represents the full configuration state that is available to
/// a target for analysis.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative)]
pub struct ResolvedConfiguration(Arc<ResolvedConfigurationData>);

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub(crate) struct ResolvedConfigurationData {
    cfg: ConfigurationNoExec,
    pub(crate) settings: ResolvedConfigurationSettings,
}

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct ResolvedConfigurationSettings {
    settings: UnorderedMap<ConfigurationSettingKey, ConfigurationNode>,
}

impl ResolvedConfiguration {
    pub fn new(cfg: ConfigurationNoExec, settings: ResolvedConfigurationSettings) -> Self {
        Self(Arc::new(ResolvedConfigurationData { cfg, settings }))
    }

    pub fn cfg(&self) -> &ConfigurationNoExec {
        &self.0.cfg
    }

    pub fn settings(&self) -> &ResolvedConfigurationSettings {
        &self.0.settings
    }
}

impl ResolvedConfigurationSettings {
    pub fn new(
        settings: UnorderedMap<ConfigurationSettingKey, ConfigurationNode>,
    ) -> ResolvedConfigurationSettings {
        ResolvedConfigurationSettings { settings }
    }

    pub fn empty() -> ResolvedConfigurationSettings {
        ResolvedConfigurationSettings::new(UnorderedMap::new())
    }

    pub fn setting_matches(&self, key: &ConfigurationSettingKey) -> Option<&ConfigSettingData> {
        let Some(configuration_node) = self.settings.get(key) else {
            panic!("unresolved configuration setting: `{key}`");
        };
        configuration_node.configuration_data()
    }
}

/// A ConfigurationNode contains the information about a config_setting() or similar target in a certain configuration.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative)]
pub struct ConfigurationNode(Arc<ConfigurationNodeData>);

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
struct ConfigurationNodeData {
    /// `None` when config settings does not match the configuration.
    config_setting: Option<ConfigSettingData>,
}

impl ConfigurationNode {
    pub fn new(config_setting: Option<ConfigSettingData>) -> Self {
        Self(Arc::new(ConfigurationNodeData { config_setting }))
    }

    pub fn configuration_data(&self) -> Option<&ConfigSettingData> {
        self.0.config_setting.as_ref()
    }
}
