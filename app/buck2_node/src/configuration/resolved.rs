/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::constraints::ConstraintKey;
use buck2_core::configuration::constraints::ConstraintValue;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::target::label::TargetLabel;
use dupe::Dupe;
use starlark_map::unordered_map::UnorderedMap;
use starlark_map::Equivalent;

/// Key in `select` or an item in `target_compatible_with`.
/// Should point to `config_setting` target, or `constraint_value`.
#[derive(
    Debug,
    Eq,
    Allocative,
    derive_more::Display,
    Clone,
    Dupe,
    Ord,
    PartialOrd
)]
pub struct ConfigurationSettingKey(pub TargetLabel);

#[derive(Debug, Hash, Eq, PartialEq, derive_more::Display)]
pub struct ConfigurationSettingKeyRef<'a>(pub &'a TargetLabel);

impl Equivalent<ConfigurationSettingKey> for ConfigurationSettingKeyRef<'_> {
    fn equivalent(&self, key: &ConfigurationSettingKey) -> bool {
        self == &key.as_ref()
    }
}

impl ConfigurationSettingKey {
    pub fn as_ref(&self) -> ConfigurationSettingKeyRef {
        ConfigurationSettingKeyRef(&self.0)
    }

    pub fn testing_parse(label: &str) -> ConfigurationSettingKey {
        ConfigurationSettingKey(TargetLabel::testing_parse(label))
    }
}

impl ConfigurationSettingKeyRef<'_> {
    pub fn to_owned(&self) -> ConfigurationSettingKey {
        ConfigurationSettingKey(self.0.dupe())
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

    pub fn setting_matches(&self, key: ConfigurationSettingKeyRef) -> Option<&ConfigSettingData> {
        let Some(configuration_node) = self.settings.get(&key) else {
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
    // This is stored as a split Configuration/TargetLabel (rather than a ConfiguredTargetLabel) because it's not
    // quite the same as what you would think of a ConfiguredTargetLabel. Importantly, we don't do analysis of the
    // target with this configuration, instead we interpret the results of the analysis of the target in the "unbound"
    // configuration within the context of this configuration.
    cfg: ConfigurationData,

    /// `None` when config settings does not match the configuration.
    config_setting: Option<ConfigSettingData>,
}

impl ConfigurationNode {
    pub fn new(cfg: ConfigurationData, config_setting: Option<ConfigSettingData>) -> Self {
        Self(Arc::new(ConfigurationNodeData {
            cfg,
            config_setting,
        }))
    }

    pub fn configuration_data(&self) -> Option<&ConfigSettingData> {
        self.0.config_setting.as_ref()
    }

    pub fn testing_new_constraints(
        constraints: BTreeMap<ConstraintKey, ConstraintValue>,
    ) -> ConfigurationNode {
        ConfigurationNode::new(
            ConfigurationData::testing_new(),
            Some(ConfigSettingData {
                constraints,
                buckconfigs: BTreeMap::new(),
            }),
        )
    }
}
