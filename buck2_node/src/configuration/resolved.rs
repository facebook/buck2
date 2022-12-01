/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::collections::unordered_map::UnorderedMap;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::target::TargetLabel;
use gazebo::prelude::*;
use starlark_map::Equivalent;

#[derive(Debug, Eq, Allocative)]
pub struct ConfigurationSettingKey(pub TargetLabel);

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ConfigurationSettingKeyRef<'a>(pub &'a TargetLabel);

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
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative)]
pub struct ResolvedConfiguration(Arc<ResolvedConfigurationData>);

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
struct ResolvedConfigurationData {
    cfg: Configuration,
    settings: UnorderedMap<ConfigurationSettingKey, ConfigurationNode>,
}

impl ResolvedConfiguration {
    pub fn new(
        cfg: Configuration,
        settings: UnorderedMap<ConfigurationSettingKey, ConfigurationNode>,
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

/// A ConfigurationNode contains the information about a config_setting() or similar target in a certain configuration.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative)]
pub struct ConfigurationNode(Arc<ConfigurationNodeData>);

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
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
